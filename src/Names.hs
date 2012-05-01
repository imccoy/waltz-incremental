module Names where

import qualified Data.List as List

import Module
import Name hiding (varName)
import Unique


inctimeName = mkModuleName "Inctime"
inctime = mkModule mainPackageId inctimeName



-- in The Real World, everything lives in a monad that lets you pluck new
-- uniques out of the unique supply. I didn't want to do that, so we
-- append a string according to the purpose that we want the unique for,
-- make the name with that string, and just use the unique from that new
-- name. Essentially we piggy-back on FastString's impure trickery.
mutantNameUnique oldName nameSpace suffix
  = setNameUnique oldName (nameUnique mutantName)
  where mutantName = mutantNameIntoSpace oldName nameSpace suffix

mutantNameUniqueLocal oldName suffix
  = mutantNameUnique oldName (occNameSpace $ nameOccName oldName) suffix

mutantName oldName = mutantNameIntoSpace oldName
                                         (occNameSpace $ nameOccName oldName)
                                         "incrementalised"

mutantNameIntoSpace :: Name -> NameSpace -> String -> Name
mutantNameIntoSpace oldName nameSpace suffix
  = setNameUnique (sameSortOfName oldName occName mod)
                  (getUnique occNameUnique)
  where 
    nameString = (adaptName oldName) ++ "_" ++ suffix
    occName = mkOccName nameSpace nameString
    mod = case nameModule_maybe oldName of
            Just mod' -> moduleNameString $ moduleName mod'
            otherwise -> ""
    occNameUnique = mkOccName nameSpace (mod ++ "." ++ nameString)

adaptName name = adaptName' $ occNameString $ nameOccName name
  where adaptName' "ds" = "ds" ++ (show $ getUnique name)
        adaptName' "+"  = "plus"
        adaptName' "."  = "compose"
        adaptName' "()" = "unit"
        adaptName' "[]" = "BuiltinList"
        adaptName' ":" = "BuiltinList"
        adaptName' s 
          | List.isPrefixOf "$c" s = s ++ (show $ getUnique name)
          | otherwise              = s

sameSortOfName oldName occName mod
  | isInternalName oldName = mkInternalName unique occName (nameSrcSpan oldName)
  | isExternalName oldName = mkExternalName unique (adaptModule $ 
                                                      nameModule oldName)
                                            occName (nameSrcSpan oldName)
  | isWiredInName oldName  = mkSystemName unique occName 
  | otherwise              = mkSystemName unique occName 
  where 
    unique = getUnique occName

adaptModule mod | modulePackageId mod == primPackageId = inctime
                | modulePackageId mod == rtsPackageId  = inctime
                | modulePackageId mod == basePackageId = inctime
                | otherwise                            = mod


