{-# language TypeFamilies, FlexibleContexts #-}
module Lib
       (MuRef, reifyGraph)
       where

import Control.Concurrent.MVar
import qualified System.Mem.StableName as SN
import qualified Data.IntMap as IM

type Unique = Int
data Graph e = Graph [(Unique, e Unique)] Unique

class MuRef a where
  type DeRef a :: * -> *
  mapDeRef :: Applicative f => (a -> f u) -> a -> f (DeRef a u)

reifyGraph :: MuRef a => a -> IO (Graph (DeRef a))
reifyGraph mr = do
  rt1 <- newMVar IM.empty
  rt2 <- newMVar []
  uVar <- newMVar 0
  root <- findNodes rt1 rt2 uVar mr
  pairs <- readMVar rt2
  return (Graph pairs root)
    
newUnique x = do
      v0 <- takeMVar x
      let v' = succ v0
      putMVar x v'
      return v'

-- mylookup :: Unique -> IM.IntMap [(Unique, a)] -> Maybe a             
mylookup h tab =
      case IM.lookup (SN.hashStableName h) tab of
        Just t2 -> lookup h t2
        Nothing -> Nothing

findNodes :: MuRef a =>
     MVar (IM.IntMap [(SN.StableName a, Unique)])
     -> MVar [(Unique, DeRef a Unique)] -> MVar Unique -> a -> IO Unique
findNodes rt1 rt2 uVar j
      | j `seq` True = do  -- force evaluation of j
          st <- SN.makeStableName j
          tab <- takeMVar rt1
          case mylookup st tab of
            Just v -> do
              putMVar rt1 tab
              return v
            Nothing -> do  
              var <- newUnique uVar
              putMVar rt1 $ IM.insertWith (++)
                                (SN.hashStableName st)
                                [(st, var)]
                                tab
              res <- mapDeRef (findNodes rt1 rt2 uVar) j
              tab' <- takeMVar rt2
              putMVar rt2 $ (var, res) : tab'
              return var
      | otherwise = error "findNodes : Failed strict evaluation"
        


-- --
  

type Name = String
data TreeAst = Var Name
             | Const Int
             | UnOp TreeAst
             | BinOp TreeAst TreeAst deriving (Eq, Show)

data GraphAst s = GVar Name
                | GConst Int
                | GUnOp s
                | GBinOp s s deriving (Eq, Show)           
               

instance MuRef TreeAst where
  type DeRef TreeAst = GraphAst
  mapDeRef _ (Var v) = pure $ GVar v
  mapDeRef _ (Const c) = pure $ GConst c
  mapDeRef f (UnOp p) = GUnOp <$> f p
  mapDeRef f (BinOp p1 p2) = GBinOp <$> f p1 <*> f p2





                                 
  


-- testing testing

t0 = BinOp (Const 2) (Var "Potato")
rt0 = reifyGraph t0
