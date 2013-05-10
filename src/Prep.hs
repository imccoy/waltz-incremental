{-# LANGUAGE TupleSections #-}
module Prep (etaExpandVdefgs, lambdaLiftVdefgs) where

import Data.Map (Map)
import qualified Data.Map as Map

import Language
import qualified Language.Core.Core as C

getVdefgs (C.Module _ _ vdefgs) = vdefgs

etaExpandVdefgs vdefgs = map (etaExpandVdefg names) vdefgs
  where names = Map.unions $ map namesFrom vdefgs

namesFrom (NonRec vdef) = Map.fromList [vdefName vdef]
namesFrom (Rec vdefs) = Map.fromList $ map vdefName vdefs
vdefName (Vdef (qVar, ty, _)) = (qVar, ty)

etaExpandVdefg namesUsed (Rec vdefs) = Rec $ map (etaExpandVdef namesUsed) vdefs
etaExpandVdefg namesUsed (NonRec vdef) = NonRec $ etaExpandVdef namesUsed vdef

etaExpandVdef namesUsed (Vdef (n, ty, exp)) = Vdef (n
                                                   ,ty
                                                   ,etaExpandExp namesUsed
                                                                 argTys
                                                                 exp)
  where argTys = case splitFunTy_maybe ty of
                   Just (args, result) -> args
                   Nothing             -> []

etaExpandExp :: Map (Qual Var) Ty -> [Ty] -> Exp -> Exp
etaExpandExp _ _ exp@(Var _) = exp
etaExpandExp _ _ exp@(Dcon _) = exp
etaExpandExp _ _ exp@(Lit _) = exp
etaExpandExp namesUsed argTys (Lam interestingness binds exp)
  | length argTys == length binds = Lam interestingness
                                        binds
                                        (etaExpandExp namesUsed argTys exp)
  | length argTys > length binds = error $ "lambda takes more args " ++
                                           "then are in it's type."
  | length argTys < length binds = Lam interestingness
                                       (binds ++ newBindsNoQual)
                                       (etaExpandExp namesUsed'
                                                     argTys
                                                     (applyNewNames newNameExps
                                                                    exp))
  where newNames = makeNewNames namesUsed newNamesCount
        newNameExps = map Var newNames
        newNamesCount = length newBindTys
        newBindTys = drop (length binds) argTys
        newBinds = zip newNames newBindTys
        newBindsNoQual = map (Vb . mapFst snd) newBinds
        namesUsed' = Map.unions [bindVarNames newBinds
                                ,someBindVarNames binds
                                ,namesUsed]

etaExpandExp namesUsed argTys (App exp tyArgs args)
  = App (etaExpandExp namesUsed argTys exp)
        tyArgs
        (map (etaExpandExp namesUsed argTys) args)
etaExpandExp namesUsed argTys (Let vdefg exp)
  = Let (expandVdefg vdefg)
        (etaExpandExp namesUsed' argTys exp)
  where namesUsed' = namesUsed `Map.union` namesFrom vdefg
        expandVdefg (NonRec vdef) = NonRec $ etaExpandVdef namesUsed' vdef
        expandVdefg (Rec vdefs) = Rec $ map (etaExpandVdef namesUsed') vdefs
etaExpandExp namesUsed argTys (Case scrut vb ty alts)
  = Case (etaExpandExp namesUsed argTys scrut)
         vb
         ty
         (etaExpandAlts namesUsed argTys alts)
  where namesUsed' = mapFst C.unqual vb `addBind` namesUsed

etaExpandAlts ns as = map (etaExpandAlt ns as)
etaExpandAlt namesUsed argTys (AltDcon qDcon tbs vbs exp)
  = AltDcon qDcon tbs vbs (etaExpandExp namesUsed argTys exp)
etaExpandAlt namesUsed argTys (AltLit lit exp)
  = AltLit lit (etaExpandExp namesUsed argTys exp)
etaExpandAlt namesUsed argTys (AltDefault exp)
  = AltDefault (etaExpandExp namesUsed argTys exp)

applyNewNames names exp@(Var _) = App exp [] names
applyNewNames names exp@(Dcon _) = App exp [] names
applyNewNames names exp@(Lit _) = error "Can't add args to a literal"
applyNewNames names (Lam inte binds exp) = Lam inte
                                               binds
                                               (applyNewNames names exp)
applyNewNames names (App exp tyArgs args) = App exp tyArgs (args ++ names)
applyNewNames names (Let vdefg exp) = Let vdefg (applyNewNames names exp)
applyNewNames names (Case scrut vb ty alts) = Case scrut vb ty (map go alts)
  where go (AltDcon qDcon tbs vbs exp) = AltDcon qDcon
                                                 tbs
                                                 vbs
                                                 (applyNewNames names exp)
        go (AltLit lit exp) = AltLit lit (applyNewNames names exp)
        go (AltDefault exp) = AltDefault (applyNewNames names exp)

lambdaLiftVdefgs vdefgs = map (lambdaLiftVdefg ns) vdefgs
  where ns = Map.unions $ map namesFrom vdefgs

lambdaLiftVdefg ns (NonRec vdef) = NonRec $ lambdaLiftVdef ns vdef
lambdaLiftVdefg ns (Rec vdefs) = Rec $ map (lambdaLiftVdef ns) vdefs

lambdaLiftVdef ns (Vdef (qVar, ty, exp)) = Vdef (qVar, ty, lambdaLiftExp ns exp)

lambdaLiftExp :: Map (Qual Var) Ty -> Exp -> Exp
lambdaLiftExp _ exp@(Var _) = exp
lambdaLiftExp _ exp@(Dcon _) = exp
lambdaLiftExp _ exp@(Lit _) = exp
lambdaLiftExp ns (Lam interestingness binds exp)
 | freeVars == [] = exp
 | otherwise = Lam Boring binds $
                 App (Lam interestingness (binds ++ freeVarsBinds) exp')
                     (map (Tvar . fst) tbinds)
                     (map (Var . C.unqual . fst) vbinds ++ map Var freeVars)
 where exp' = lambdaLiftExp ns' exp
       freeVars :: [Qual Var]
       freeVars = findFreeVars ns' exp'
       freeVarsBinds :: [Bind]
       freeVarsBinds = map (Vb . (,undefined) . snd) freeVars
       (tbinds, vbinds) = splitBinds binds
       ns' = someBindVarNames binds `Map.union` ns

lambdaLiftExp ns (App exp tyargs args) = App (lambdaLiftExp ns exp)
                                             tyargs
                                             (map (lambdaLiftExp ns) args)
lambdaLiftExp ns (Let vdefg exp) = Let (expandVdefg vdefg)
                                       (lambdaLiftExp ns' exp)
  where expandVdefg (NonRec vdef) = NonRec $ lambdaLiftVdef ns' vdef
        expandVdefg (Rec vdefs) = Rec $ map (lambdaLiftVdef ns') vdefs
        ns' = ns `Map.union` namesFrom vdefg
lambdaLiftExp ns (Case scrut vb ty alts)
  = Case (lambdaLiftExp ns' scrut)
         vb
         ty
         (lambdaLiftAlts ns' alts)
  where ns' = mapFst C.unqual vb `addBind` ns 

lambdaLiftAlts ns = map (lambdaLiftAlt ns)
lambdaLiftAlt ns (AltDcon qDcon tbs vbs exp)
  = AltDcon qDcon tbs vbs (lambdaLiftExp ns' exp)
  where ns' = bindVarNames_unqual vbs `Map.union`  ns
lambdaLiftAlt ns (AltDefault exp) = AltDefault $ lambdaLiftExp ns exp
lambdaLiftAlt ns (AltLit lit exp) = AltLit lit $ lambdaLiftExp ns exp 

findFreeVars :: Map (Qual Var) Ty -> Exp -> [Qual Var]
findFreeVars ns (Var qVar)
  | qVar `Map.member` ns = []
  | otherwise            = [qVar]
findFreeVars _ (Dcon _) = []
findFreeVars _ (Lit _) = []
findFreeVars ns (Lam _ bind exp) = findFreeVars ns' exp
  where ns' = ns `Map.union` someBindVarNames bind
findFreeVars ns (App exp _ args) = concat ((findFreeVars ns exp):
                                           (map (findFreeVars ns) args))
findFreeVars ns (Let vdefg exp) = concat ((findFreeVars ns' exp):
                                          (map (findFreeVars ns')
                                               (vdefgExps vdefg)))
  where ns' = ns `Map.union` namesFrom vdefg
        vdefgExps (NonRec vdef) = [vdefExp vdef]
        vdefgExps (Rec vdefs) = map vdefExp vdefs
        vdefExp (Vdef (_, _, exp)) = exp
findFreeVars ns (Case scrut vb ty alts) = concat ((findFreeVars ns' scrut):
                                                  (map findFreeVarsAlt
                                                       alts))
  where findFreeVarsAlt (AltDefault exp) = findFreeVars ns' exp
        findFreeVarsAlt (AltLit lit exp) = findFreeVars ns' exp
        findFreeVarsAlt (AltDcon _ _ vbs exp)
          = findFreeVars (ns `Map.union` (bindVarNames_unqual vbs)) exp
        ns' = mapFst C.unqual vb `addBind` ns
        

makeNewNames namesUsed n = take n $ filter unused names
  where names = [(Nothing, "eta" ++ show n) | n <- [1..]]
        unused n = Map.member n namesUsed
aBindVarName = C.unqual . fst
bindVarNames :: [(Qual Var, Ty)] -> Map (Qual Var) Ty
bindVarNames = Map.fromList

bindVarNames_unqual :: [(Var, Ty)] -> Map (Qual Var) Ty
bindVarNames_unqual = Map.fromList . map (mapFst C.unqual)

someBindVarNames :: [Bind] -> Map (Qual Var) Ty
someBindVarNames = bindVarNames . map extractB . filter isVbind
  where isVbind (Vb _) = True
        isVbind _ = False
        extractB (Vb x) = mapFst C.unqual x

addBind (a,b) m = Map.insert a b m

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)


