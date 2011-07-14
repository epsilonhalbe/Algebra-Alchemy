module Functions where

import Data
import Ratio
import Control.Exception (assert)

insert :: ExprTree a -> Label -> Fun -> Side -> (ExprTree a) -> (ExprTree a)
insert shrubbery here withfun onSide toTree
        | l == here = forkWith shrubbery withfun onSide toTree
        | here < l = (Node l f (insert shrubbery here withfun onSide lb) rb)
        | here > l = (Node l f rb (insert shrubbery here withfun onSide rb))
        | otherwise = error "label out of range"
        where (Node _ f lb rb) = toTree
              l = lab toTree

forkWith :: (ExprTree a) -> Fun -> Side -> (ExprTree a)-> (ExprTree a)
forkWith branch fun L trunk = relabelAt l (Node l fun branch trunk)
                             where l = lab trunk
forkWith branch fun R trunk = relabelAt l (Node l fun trunk branch)
                             where l = lab trunk

relabel :: (ExprTree a) -> (ExprTree a)
relabel = relabelAt (1%1)

relabelAt :: Label -> (ExprTree a) -> (ExprTree a)
relabelAt init (Node l fun lb rb) = (Node init fun (relabelAt (init - (1%(2*d))) lb)
                                                 (relabelAt (init + (1%(2*d))) rb))
                                where d = denominator init

relabelAt init (Leaf l v) = (Leaf init v)

commutate :: (ExprTree a) -> (ExprTree a)
commutate (Node l fun lb rb) = relabelAt l (Node l fun rb lb)
commutate e = id e

substitute :: [(Algebraic, Rational)] -> ExprTree Algebraic -> ExprTree Rational
substitute sRule= fmap (_substitute sRule)

_substitute :: [(Algebraic, Rational)] -> Algebraic -> Rational
_substitute sRule (Alg v) | '%' `elem` v = c%d
                          | otherwise    = assert (length _sRule == 1) (snd (head _sRule))
                          where _sRule = filter (isSubstitute (Alg v)) sRule
                                (c,d) = diag (\x->read x::Integer) $.$ break (=='%') v

isSubstitute :: Algebraic -> (Algebraic,Rational) -> Bool
isSubstitute v x = fst x == v

($.$):: ((a1->a2),(b1->b2))->(a1,b1) -> (a2,b2)
(f1,f2) $.$ (x,y) = (f1 x, f2 y)

diag :: a -> (a,a)
diag x = (x,x)

