{- | all datatypes for the algebraic trees and transformation
     the backstage stuff -}
module Data (
    -- * Classes
    Eval (eval),
    -- * Types
    Label,
    Fun ( Add,
          Sub,
          Mul,
          Div),
    Side (L, R),
    Symbol (Symbol),
    Algebraic (Alg),
    -- * Functors
    ExprTree (Node, Leaf, lab)
    )
    where

import Data.Ratio

type Label = Rational
{- ^ Labels are chosen to be Rational so i can renumber and insert branches very
     cheaply - by subtracting half the denominator on the left branch and adding
     on the right branch respectively. -}

data Fun = Add | Sub | Mul | Div {- Pow -} deriving (Eq, Ord)
{- ^ The Fun(ction) datatype will represent - the function in a Node of the
     @ExprTree@ expression tree -}

instance Show Fun where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
{-  show Pow = "^"
    show _ = error "no Fun to show" -}

instance Read Fun where
    readsPrec _ = _readsFun

_readsFun :: ReadS Fun
-- ^ internal helper function to construct the read instance of @Fun@
_readsFun ('+' : s) = [(Add, s)]
_readsFun ('-' : s) = [(Sub, s)]
_readsFun ('*' : s) = [(Mul, s)]
_readsFun ('/' : s) = [(Div, s)]
_readsFun (_ : s) = error "none of the functions +-/*"
-- _readsFun ('^' : s) = [(Pow, s)]

data Side = L | R deriving (Eq, Ord, Show)
-- ^ Denotes the side a branch will get inserted

data Symbol = Symbol (Maybe Rational, Algebraic) deriving (Show)
-- ^ the Symbol datatype is - well for symbolic calculations

data Algebraic = Alg String deriving (Eq)
{- ^ the Alg datatype is a helper for reading the String input
     to make @ExprTree@ -}
instance Show Algebraic where
    show (Alg a) = "_" ++ a ++ "_"

instance Eval Algebraic where
    eval (Alg a) = read a :: Rational

instance Read Algebraic where
        readsPrec _ = _readsAlg

_readsAlg :: ReadS Algebraic
{- ^ internal helper function to create the read instance on @Alg@
     does anyone know how to hide _readsAlg from showing up in the docu - i just
     want to have it in the sources. -}
_readsAlg v = [(Alg (v1 ++ "%" ++ v2), vv) | (v1,  vv1) <- lex v,
                                             ("%", vv2) <- lex vv1,
                                             (v2,  vv ) <- lex vv2 ] ++
              [(Alg v1, vv)                | (v1,  vv ) <- lex v]

{- | The standard Construction of a tree - with some extra flavour for
     expressions -}

data ExprTree a
  -- | Leaf has a label and a value -
                = Leaf {lab :: Label, val :: a}
  {- | Node has a label used as above and a function - which should be used on
       the branches -}
                | Node {lab :: Label, fun :: Fun, lBranch :: ExprTree a,
                                                  rBranch :: ExprTree a}
{- | ExprTree the main vehicle to do algebraic transformations
     i'll probably use ExprTree Algebraic -toSymbol-> ExprTree Symbol
                   and ExprTree Algebraic -eval->     ExprTree Rational -}
instance Functor ExprTree where
    fmap phi (Node l f lb rb) = Node l f (fmap phi lb) (fmap phi rb)
    fmap phi (Leaf l a) = Leaf l (phi a)

instance (Show a) => Show (ExprTree a) where
    show (Leaf l a) = show a
    show (Node l f lb rb ) = "(" ++ show lb ++ "/"  ++ show f  ++ show l
                                 ++ show f  ++ "\\" ++ show rb ++ ")"

instance (Eq a) => Eq (ExprTree a) where
    (Leaf l1 a1) == (Leaf l2 a2) = a1 == a2
    (Node l1 f1 lb1 rb1 ) == (Node l2 f2 lb2 rb2) =
        (f1 == f2) && (lb1 == lb2) && (rb1 == rb2)
    _ == _ = False

instance (Read a) => Read (ExprTree a) where
    readsPrec _ = _readsExprTree

_readsExprTree :: (Read a) => ReadS (ExprTree a)
-- ^ internal helper function to create the read instance on @ExprTree@
_readsExprTree t =
    [(Node (0 % 1) (read f :: Fun) lb rb, tt) | ("(", t1) <- lex t,
                                                (lb , t2) <- _readsExprTree t1,
                                                ( f , t3) <- lex t2,
                                                (rb , t4) <- _readsExprTree t3,
                                                (")", tt) <- lex t4] ++
    [(Leaf (0 % 1) v, tt)                     | (v  , tt) <- reads t]

class Eval e where
    eval ::  e -> Rational
{- ^ the class of Eval denotes all things that are evaluatable to @Rational@ -
     i used Rational as i want to be able to do algebraic transformations
     without rounding errors. -}
