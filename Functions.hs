-- | all internal Functions for manipulating ExprTrees also backstage fun
module Functions (
    -- * Functions
    insert,
    forkWith,
    relabel,
    relabelAt,
    commutate,
    substitute,
   _substitute,
    ($$),
    diag
    )
    where

import Data
import Ratio
import Control.Exception (assert)

insert :: ExprTree a -> Label -> Fun -> Side -> (ExprTree a) -> (ExprTree a)
-- ^ guess what - inserts a tree at label on given side into given tree
insert shrubbery here withfun onSide toTree
        | l == here = forkWith shrubbery withfun onSide toTree
        | here < l = (Node l f (insert shrubbery here withfun onSide lb) rb)
        | here > l = (Node l f lb (insert shrubbery here withfun onSide rb))
        | otherwise = error "label out of range"
        where (Node _ f lb rb) = toTree
              l = lab toTree

forkWith :: (ExprTree a) -> Fun -> Side -> (ExprTree a)-> (ExprTree a)
-- ^ Just an insert on top level
forkWith branch fun L trunk = relabelAt l (Node l fun branch trunk)
                             where l = lab trunk
forkWith branch fun R trunk = relabelAt l (Node l fun trunk branch)
                             where l = lab trunk

relabel :: (ExprTree a) -> (ExprTree a)
-- ^ relabels a tree on top level
relabel = relabelAt (1%1)

relabelAt :: Label -> (ExprTree a) -> (ExprTree a)
-- ^ relabels a tree startin on a given level
relabelAt init (Node l fun lb rb) = (Node init fun (relabelAt (init - (1%(2*d))) lb)
                                                 (relabelAt (init + (1%(2*d))) rb))
                                where d = denominator init

relabelAt init (Leaf l v) = (Leaf init v)

commutate :: (ExprTree a) -> (ExprTree a)
-- ^ should be commutation laws for addition and multiplication
commutate (Node l fun lb rb) = relabelAt l (Node l fun rb lb)
commutate e = id e

substitute :: [(Algebraic, Rational)] -> ExprTree Algebraic -> ExprTree Rational
-- ^ substitutes a whole ExprTree according to a list of substitution rules
substitute sRule = fmap (_substitute sRule)

_substitute :: [(Algebraic, Rational)] -> Algebraic -> Rational
-- ^ an internal function to apply a list of substitution rules to given
-- variables
--
-- >>> _substitute [(Alg "x1",2%1)] Alg "x1"
-- 2%1
-- >>> _substitute [(Alg "x2",2%1)] Alg "x1"
-- Alg "x2"

_substitute sRule (Alg v) | ('%' `elem` v) = read v::Rational
                          | otherwise    = assert (length _sRule == 1) (snd (head _sRule))
                          where _sRule = filter (isSubstitute (Alg v)) sRule

isSubstitute :: Algebraic -> (Algebraic,Rational) -> Bool
-- ^ checks if a given Algebraic expression can be substituted by Rational
--
isSubstitute v x = fst x == v

($$):: ((a1->a2),(b1->b2))->(a1,b1) -> (a2,b2)
-- ^ applicates a pair of functions to a pair of values
(f1,f2) $$ (x,y) = (f1 x, f2 y)

diag :: a -> (a,a)
-- ^ duplicates a value to a pair
diag x = (x,x)

