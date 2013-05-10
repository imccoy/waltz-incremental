{-# LANGUAGE StandaloneDeriving #-}
module Language (Alt (..),
                 Bind (..),
                 Dcon (..),
                 Exp (..),
                 Interestingness (..),
                 Lit (..),
                 Qual (..),
                 Tbind (..),
                 Ty (..),
                 Var (..),
                 Vbind (..),
                 Vdef (..),
                 Vdefg (..),
                 convertVdefgs,
                 splitBinds,
                 splitFunTy_maybe) where

import qualified Language.Core.Core as C
import Language.Core.Core (Bind (..),
                           Dcon (..),
                           Lit (..),
                           Qual (..),
                           Tbind (..),
                           Ty (..),
                           Var (..),
                           Vbind (..))
import Language.Core.Parser()
import Language.Core.ParseGlue()
import Language.Core.CoreUtils (splitFunTy_maybe, splitBinds)

data Vdefg = Rec [Vdef]
           | NonRec Vdef
  deriving (Show)

newtype Vdef = Vdef (Qual Var, Ty, Exp)
  deriving (Show)

data Exp = Var (Qual Var)
         | Dcon (Qual Dcon)
         | Lit Lit
         | Lam Interestingness [Bind] Exp
         | App Exp [Ty] [Exp]
         | Let Vdefg Exp
         | Case Exp Vbind Ty [Alt]
  deriving (Show)

data Alt = AltDcon (Qual Dcon) [Tbind] [Vbind] Exp
         | AltLit Lit Exp
         | AltDefault Exp
  deriving (Show)
          
data Interestingness = Interesting | Boring
  deriving (Show)

deriving instance Show Bind

convertVdefgs = map convertVdefg
convertVdefg (C.Nonrec vdef) = NonRec $ convertVdef vdef
convertVdefg (C.Rec vdefs) = Rec $ map convertVdef vdefs

convertVdef (C.Vdef (qVar, ty, exp)) = Vdef (qVar, ty, convertExp exp)

convertExp (C.Var qVar) = Var qVar
convertExp (C.Dcon dcon) = Dcon dcon
convertExp (C.Lit lit) = Lit lit
convertExp exp@(C.Lam bind body) = Lam Interesting
                                       binds
                                       (convertExp body')
  where (binds, body') = collectLamArgs exp
convertExp exp@(C.App fun arg) = App (convertExp fun')
                                     []
                                     (map convertExp vbinds)
  where (vbinds, fun') = collectAppArgs exp
convertExp exp@(C.Appt fun arg) = App (convertExp fun')
                                      tbinds
                                      (map convertExp vbinds)
  where (tbinds, (vbinds, fun')) = collectApptArgs exp
convertExp (C.Let vdefg exp) = Let (convertVdefg vdefg) (convertExp exp)
convertExp (C.Case scrut vbind ty alts) = Case (convertExp scrut)
                                            vbind
                                            ty
                                            (convertAlts alts)
convertExp (C.Cast exp _) = convertExp exp
convertExp (C.Note _ exp) = convertExp exp

convertAlts alts = map convertAlt alts
convertAlt (C.Adefault exp) = AltDefault (convertExp exp)
convertAlt (C.Alit lit exp) = AltLit lit (convertExp exp)
convertAlt (C.Acon qDcon tbinds vbinds exp) = AltDcon qDcon
                                                   tbinds
                                                   vbinds
                                                   (convertExp exp)


collectLamArgs exp = go [] exp
  where go args (C.Lam b exp) = go (b:args) exp
        go args exp           = (args, exp)

collectAppArgs :: C.Exp -> ([C.Exp], C.Exp)
collectAppArgs exp = go [] exp
  where go args (C.App b exp) = go (b:args) exp
        go args exp           = (args, exp)

collectApptArgs :: C.Exp -> ([Ty], ([C.Exp], C.Exp))
collectApptArgs exp = go [] exp
  where go args (C.Appt exp t) = go (t:args) exp
        go args exp@(C.App _ _) = (args, collectAppArgs exp)
        -- next case is type args but no value args, which happens with
        -- calls to undefined
        go args exp               = (args, ([], exp))



