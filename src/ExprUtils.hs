{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DoRec #-}
module ExprUtils where

import Data.List (partition)

import CoreSyn
import Literal (literalType)
import Type (Type, applyTy, splitFunTy_maybe, mkFunTy)
import Var (varType)

collectTypeArgs expr = go expr []
  where go (App expr arg@(isTypeArg -> True)) args = go expr (arg:args)
        go expr                               args = (expr, args)

collectTypeAndValArgs expr = let (expr', args) = collectArgs expr
                                 (typeArgs, valArgs) = partition isTypeArg args
                              in (expr', typeArgs, valArgs)

exprType :: Expr CoreBndr -> Type
exprType (Var id) = varType id
exprType (Lit lit) = literalType lit
exprType (App expr (Type arg)) = applyTy (exprType expr) arg
exprType (App expr arg) = case splitFunTy_maybe (exprType expr) of
                            Just (_, t) -> t
                            Nothing -> exprType expr
exprType (Lam id expr) = mkFunTy (varType id) (exprType expr)
exprType (Let bind expr) = exprType expr
exprType (Case expr id type_ alts) = type_
exprType (Cast expr coercion) = coercion
exprType (Type type_) = type_
exprType (Note note expr) = exprType expr


