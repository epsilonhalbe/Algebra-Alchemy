module Data where

import Ratio

data Fun = Add|Sub|Mul|Div|Pow
    deriving (Eq,Ord)
instance Show Fun where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
--    show Pow = "^"

readsFun :: ReadS Fun
readsFun ('+':s) = [(Add,s)]
readsFun ('-':s) = [(Sub,s)]
readsFun ('*':s) = [(Mul,s)]
readsFun ('/':s) = [(Div,s)]
-- readsFun ('^':s) = [(Pow,s)]

readsExprTree :: ReadS (ExprTree Val)
readsExprTree t = [ (Node (0%1) f lb rb, tt) | ("(", t1) <- lex t,
                                               (lb , t2) <- readsExprTree t1,
                                               (f  , t3) <- readsFun t2,
                                               (rb , t4) <- readsExprTree t3,
                                               (")", tt) <- lex t4]
                ++
                  [ (Leaf (0%1) v, tt)       | (v  , tt) <- readsVal t]

readsVal :: ReadS Val
readsVal v = [(Val v1, vv) | (v1, vv) <- lex v]

type Label = Rational
data Side = L| R deriving (Eq, Ord, Show)

class Eval e where
    eval ::  e -> Rational

data Val = Val String deriving (Eq)
instance Show Val where
    show (Val a) = "_"++(show a)++"_"

instance Eval Val where
    eval (Val a) = read a :: Rational

data ExprTree a = Leaf {lab::Label, val::a}
                | Node {lab::Label, fun::Fun, lBranch::ExprTree a,
                                              rBranch::ExprTree a}
instance Functor ExprTree where
    fmap phi (Node l f lb rb) = Node l f (fmap phi lb) (fmap phi rb)
    fmap phi (Leaf l a) = Leaf l (phi a)

instance (Show a) => Show (ExprTree a) where
    show (Leaf l a) = show a
    show (Node l f lb rb ) = "("++(show lb)++"/"++(show f)++(show l)++(show f)++"\\"++(show rb)++")"



