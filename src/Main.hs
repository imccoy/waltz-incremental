{-# LANGUAGE ScopedTypeVariables, ViewPatterns, DoRec #-}
module Main where

import System.Environment
import System.IO
import System.Exit

import Data.Data (toConstr)
import Data.List (nub)
import Data.Maybe (isJust, fromJust)

import Safe

import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Graph.Inductive.Graphviz as Gr

import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue

data CoreNode = VarBindNode (Qual Var)
              | ExpNode Exp

instance Show CoreNode where
  show (ExpNode exp) = show $ toConstr exp
  show (VarBindNode (mod, name)) = "(" ++ show mod ++ "," ++ name ++ ")"

data CoreEdge = VarSet
              | VarReference
              | ExpAlias
              | LamArg
              | LamBody
              | AppFun
              | AppArg
              | LamApp
              | NthDconArg (Qual Dcon) Int
              | CaseEdge Alt

instance Show CoreEdge where
  show VarSet = "VarSet"
  show VarReference = "VarReference"
  show ExpAlias = "ExpAlias"
  show LamArg = "LamArg"
  show LamBody = "LamBody"
  show AppFun = "AppFun"
  show AppArg = "AppArg"
  show LamApp = "LamApp"
  show (NthDconArg (mod, name) n) = "(NthDconArg (" ++ show mod ++ "," ++ name 
                                      ++ ") " ++ show n ++ ")"
  show (CaseEdge (Adefault _)) = "CaseEdge Adefault"
  show (CaseEdge (Acon _ _ _ _)) = "CaseEdge Acon"
  show (CaseEdge (Alit _ _)) = "CaseEdge Alit"

type CoreGraph = Gr.Gr CoreNode CoreEdge

namesGraph :: [Qual Var] -> CoreGraph
namesGraph names = Gr.mkGraph (mkNameNodes names) []

mkNameNodes = zipWith (\n name -> (n, VarBindNode name)) [1..]

nameNode name gr = headNote ("Name node for name " ++ show name) $
                     filter hasName $ Gr.labNodes gr
  where hasName (_, (VarBindNode n)) = n == name
        hasName _ = False

lamNodes gr = filter isLamNode $ Gr.labNodes gr
lamVbNodes gr = filter isLamVbNode $ lamNodes gr

isLamNode (_, (ExpNode (Lam _ _))) = True
isLamNode _ = False

isAppNode (_, (ExpNode (App _ _))) = True
isAppNode _ = False

isLamVbNode (_, (ExpNode (Lam (Vb _) _))) = True
isLamVbNode _ = False

labelNode :: Gr.Graph gr => gr a b -> Gr.Node -> Gr.LNode a
labelNode gr node = (node, fromJust $ Gr.lab gr node)

allNames :: [Vdefg] -> [Qual Var]
allNames vdefgs = nub $ concatMap allNamesVdefg vdefgs
  where allNamesVdefg (Nonrec vdef) = allNamesVdef vdef
        allNamesVdefg (Rec vdefs) = concatMap allNamesVdef vdefs
        allNamesVdef (Vdef (qVar, _, exp)) = qVar:(allNamesExp exp)
        allNamesExp (Lam (Tb _) exp) = allNamesExp exp
        allNamesExp (Lam (Vb vbind) exp) = (nameVbind vbind):(allNamesExp exp)
        allNamesExp (App exp arg) = (allNamesExp exp) ++ (allNamesExp arg)
        allNamesExp (Appt exp ty) = allNamesExp exp
        allNamesExp (Let vdefg exp) = allNamesVdefg vdefg ++ allNamesExp exp
        allNamesExp (Cast exp ty) = allNamesExp exp
        allNamesExp (Case scrut vbind _ alts) = (nameVbind vbind):(
                                                         allNamesExp scrut ++
                                                         allNamesAlts alts)
        allNamesExp (Var qVar@(mod, _))
          | isJust mod && isNotMain (fromJust mod) = [qVar]
          | otherwise                              = []
        allNamesExp _ = []
        allNamesAlts = concatMap allNamesAlt
        allNamesAlt (Alit _ exp) = allNamesExp exp
        allNamesAlt (Adefault exp) = allNamesExp exp
        allNamesAlt (Acon _ _ vbinds exp) = allNamesExp exp ++ 
                                               map nameVbind vbinds
        nameVbind (var, _) = unqual var

buildGraph :: [Vdefg] -> CoreGraph
buildGraph vdefgs = addLamApps $
                      foldr vdefgNode (namesGraph $ allNames vdefgs') vdefgs'
  where vdefgs' = addFakeVdefs vdefgs

vdefgNode :: Vdefg -> CoreGraph -> CoreGraph
vdefgNode (Nonrec vdef) graph  = vdefNodeG vdef graph
vdefgNode (Rec vdefs) graph = foldr vdefNodeG graph vdefs

vdefNodeG v gr = fst $ vdefNode v gr

vdefNode :: Vdef -> CoreGraph -> (CoreGraph, Gr.Node)
vdefNode (Vdef (qVar, ty, exp)) gr = let node = nameNode qVar gr
                                         (gr', expNode) = expGraph gr exp
                                      in (Gr.insEdge (expNode,
                                                      fst node,
                                                      VarSet)
                                                    gr'
                                         ,expNode)

expGraph :: CoreGraph -> Exp -> (CoreGraph, Gr.Node)
expGraph gr exp = let [expNode] = Gr.newNodes 1 gr
                      expLNode = (expNode, (ExpNode exp))
                      grWithExpNode = Gr.insNode expLNode gr
                      grWithDeepExpNode = expGraphDeep grWithExpNode expNode exp
                   in (grWithDeepExpNode, expNode)

expGraphDeep :: CoreGraph -> Gr.Node -> Exp -> CoreGraph

expGraphDeep gr expNode (Var qVar)
  = let referentLNode = nameNode qVar gr
     in Gr.insEdge (fst referentLNode, expNode, VarReference) gr

expGraphDeep gr expNode (Lam (Tb _) exp) = expAlias gr expNode exp

expGraphDeep gr expNode (Lam (Vb bind) exp)
  = let argLNode = nameNode (unqual $ fst bind) gr
        gr' = Gr.insEdge (fst argLNode, expNode, LamArg) gr
        (gr'', bodyNode) = expGraph gr' exp
     in Gr.insEdge (bodyNode, expNode, LamBody) gr''

expGraphDeep gr expNode (App fun arg)
  = let (gr', funNode) = expGraph gr fun
        (gr'', argNode) = expGraph gr' arg
     in Gr.insEdge (funNode, expNode, AppFun) $
          Gr.insEdge (argNode, expNode, AppArg) gr''

expGraphDeep gr expNode (Appt exp _) = expAlias gr expNode exp

expGraphDeep gr expNode (Let vdefg exp)
  = let gr' = foldr vdefNodeG gr $ flattenBinds [vdefg] 
     in expAlias gr' expNode exp

expGraphDeep gr expNode (Case scrut vbind ty alts)
  = let (gr', scrutNode) = vdefNode (Vdef (unqual $ fst vbind
                                          , snd vbind
                                          , scrut))
                                    gr
     in foldr (altGraph scrutNode) gr' alts
  where altGraph :: Gr.Node -> Alt -> CoreGraph -> CoreGraph
        altGraph scrutNode alt@(Acon dcon _ vbinds altExp) gr
          = let dconEdge (var, _) (gr, n)
                  = let node = nameNode (unqual var) gr
                        val = NthDconArg dcon n
                     in (Gr.insEdge (scrutNode, fst node, val) gr, n+1)
                gr' = fst $ foldr dconEdge
                                  (gr, 0)
                                  vbinds
             in includingAltNode gr' alt altExp
        altGraph _ alt@(Alit lit altExp) gr
          = includingAltNode gr alt altExp 
        altGraph _ alt@(Adefault altExp) gr
          = includingAltNode gr alt altExp 
        includingAltNode gr alt altExp
          = let (gr', altExpNode) = expGraph gr altExp
             in Gr.insEdge (altExpNode, expNode, CaseEdge alt) gr'

expGraphDeep gr expNode (Cast exp _) = expAlias gr expNode exp
expGraphDeep gr expNode (Note _ exp) = expAlias gr expNode exp
expGraphDeep gr _ (Dcon _) = gr
expGraphDeep gr _ (Lit _) = gr

expAlias gr expNode exp
  = let (gr', innerExpNode) = expGraph gr exp
     in Gr.insEdge (innerExpNode, expNode, ExpAlias) gr'

addFakeVdefs :: [Vdefg] -> [Vdefg]
addFakeVdefs = (map Nonrec [ztFake, zmFake] ++)
  where zpFake = Vdef ((Just numModule, "zp")
                      ,Tvar "undefined"
                      ,Lam (Tb ("t", Kopen))
                         $ Lam (Vb ("tc", Tvar "T:Num"))
                           $ Lam (Vb ("a", Tvar "t"))
                             $ Lam (Vb ("b", Tvar "t"))
                               $ Var (Just baseModule, "undefined"))
        ztFake = Vdef ((Just numModule, "zt")
                      ,Tvar "undefined"
                      ,Lam (Tb ("t", Kopen))
                         $ Lam (Vb ("tc", Tvar "T:Num"))
                           $ Lam (Vb ("a", Tvar "t"))
                             $ Lam (Vb ("b", Tvar "t"))
                               $ Var (Just baseModule, "undefined"))
        zmFake = Vdef ((Just numModule, "zm")
                      ,Tvar "undefined"
                      ,Lam (Tb ("t", Kopen))
                         $ Lam (Vb ("tc", Tvar "T:Num"))
                           $ Lam (Vb ("a", Tvar "t"))
                             $ Lam (Vb ("b", Tvar "t"))
                               $ Var (Just baseModule, "undefined"))



addLamApps :: CoreGraph -> CoreGraph
addLamApps gr = Gr.insEdges [(fst lam, fst app, LamApp)
                              | lam <- lamVbNodes gr
                              , app <- appNodesForLam gr lam]
                            gr

appNodesForLam :: CoreGraph -> Gr.LNode CoreNode -> [Gr.LNode CoreNode]
appNodesForLam gr lam = go 0 lam
  where go d n = do next <- Gr.suc gr (fst n) -- list monad
                    consider d (labelNode gr next)
        consider d n
          | isAppNode n && d == 0 = [n]
          | isAppNode n           = go (d - 1) n
          | isLamVbNode n         = go (d + 1) n
          | otherwise             = go d n

moduleName (Module name _ _) = name

modulePname (M (pname, _, _)) = pname

isNotMain :: AnMname -> Bool
isNotMain = (/= P "main") . modulePname

numModule = M (P "base", ["GHC"], "Num")
baseModule = M (P "base", ["GHC"], "Base")

moduleById id mods = headNote ("Couldn't find module with id " ++ id) $
                       [ mod | mod <- mods, moduleId mod == id ]
moduleId (Module (M (_, _, id)) _ _) = id

module_vdefgs (Module _ _ vdefgs) = vdefgs

coreFileContents filename = do
  file <- openFile filename ReadMode
  contents <- hGetContents file
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
                    exitFailure
    (OkP e) -> return e

writeFileContents filename content = do
  file <- openFile filename WriteMode
  hPutStr file $ content
  hClose file

main = do
  mods <- mapM coreFileContents =<< getArgs
  let graph = buildGraph $ concatMap module_vdefgs mods
  putStrLn $ Gr.graphviz' graph
  return ()
