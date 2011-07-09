module Functions where

import Data
import Ratio
import Control.Exception (assert)

insert ::(Num a) => (ExprTree a) -> Label -> Fun -> Side -> (ExprTree a) -> (ExprTree a)
insert shrubbery here withfun onSide toTree
        | l == here = forkWith shrubbery withfun onSide toTree
        | here < l = (Node l f (insert shrubbery here withfun onSide lb) rb)
        | here > l = (Node l f rb (insert shrubbery here withfun onSide rb))
        | otherwise = error "label out of range"
        where (Node _ f lb rb) = toTree
              l = lab toTree

forkWith ::(Num a) => (ExprTree a) -> Fun -> Side -> (ExprTree a)-> (ExprTree a)
forkWith branch fun L trunk = relabelAt l (Node l fun branch trunk)
                             where l = lab trunk
forkWith branch fun R trunk = relabelAt l (Node l fun trunk branch)
                             where l = lab trunk

relabel :: (Num a) => (ExprTree a) -> (ExprTree a)
relabel = relabelAt (1%1)

relabelAt :: (Num a) => Label -> (ExprTree a) -> (ExprTree a)
relabelAt init (Node l fun lb rb) = (Node init fun (relabelAt (init - (1%(2*d))) lb)
                                                 (relabelAt (init + (1%(2*d))) rb))
                                where d = denominator init

relabelAt init (Leaf l v) = (Leaf init v)

commute :: (Num a) => (ExprTree a) -> (ExprTree a)
commute (Node l fun lb rb) = relabelAt l (Node l fun rb lb)
commute e = id e

substitute :: [(Val, Rational)] -> ExprTree Val -> ExprTree Rational
substitute sRule= fmap (_substitute sRule)

_substitute :: [(Val, Rational)] -> Val -> Rational
_substitute sRule (Val v) | '%' `elem` v = c%d
                          | otherwise    = assert (length _sRule == 1) (snd (head _sRule))
                          where _sRule = filter (isSubstitute (Val v)) sRule
                                (c,d) = diag (\x->read x::Integer) $.$ break (=='%') v

isSubstitute :: Val -> (Val,Rational) -> Bool
isSubstitute v x = fst x == v

($.$):: ((a1->a2),(b1->b2))->(a1,b1) -> (a2,b2)
(f1,f2) $.$ (x,y) = (f1 x, f2 y)

diag :: a -> (a,a)
diag x = (x,x)

