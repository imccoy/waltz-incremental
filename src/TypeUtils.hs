module TypeUtils where

import Control.Monad
import Data.Maybe (isJust)

import Type

splitArrowAppTy_maybe t = do -- maybe monad
  (arrowAndFirst, rest) <- splitAppTy_maybe t
  (arrow, first) <- splitAppTy_maybe arrowAndFirst
  (funTyConCandidate, _) <- splitTyConApp_maybe arrow
  if funTyConCandidate == funTyCon
    then return (first, rest)
    else mzero

isArrowAppTy = isJust . splitArrowAppTy_maybe

mkArrow a b = mkAppTy (mkAppTy (mkTyConApp funTyCon []) a) b
