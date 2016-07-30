{-# language TypeFamilies #-}
module Lib where


class MuRef a where
  type DeRef a :: * -> *
  mapDeRef :: Applicative f => (a -> f u) -> a -> f (DeRef a u)



-- --
  

type Name = String
data TreeAst = Var Name
             | Const Int
             | UnOp TreeAst
             | BinOp TreeAst TreeAst deriving (Eq, Show)

data GraphAst s = GVar Name
                | GConst Int
                | GUnOp s
                | GBinOp s s               
               

instance MuRef TreeAst where
  type DeRef TreeAst = GraphAst
  mapDeRef _ (Var v) = pure $ GVar v
  mapDeRef _ (Const c) = pure $ GConst c
  mapDeRef f (UnOp p) = GUnOp <$> f p
  mapDeRef f (BinOp p1 p2) = GBinOp <$> f p1 <*> f p2
  
