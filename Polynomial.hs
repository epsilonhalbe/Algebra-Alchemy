{- | all datatypes for the algebraic trees and transformation
     the backstage stuff -}
module Polynomial (
    -- * Classes
    Rev (reverse),
    -- * Types
    Sort ( C2N,
           N2C),
    Polynomial (Poly),
    -- * Functions
    eval,
    scalePoly,
    norm1,
    norm2,
    snorm
    )
    where

import Prelude hiding (reverse)
import qualified Prelude as P (reverse)

-- | denotes all reversable things
class Rev a where
    -- | reverse should have the property /reverse.reverse = id/
    reverse :: a -> a

instance Rev [a] where
    reverse = P.reverse

-- | Sort denotes the sorting of a polynomial
data Sort = C2N -- const term to leading coefficient
          | N2C -- leading coefficient to const term
          deriving (Eq, Show)

instance Rev (Sort) where
    reverse C2N = N2C
    reverse N2C = C2N

-- | Poly sp p generates a polynomial
data Polynomial a = Poly { sort :: Sort -- mostly denoted /sp/: the sorting of p
                         , coeff :: [a] -- and p the list of coefficients
                         } deriving (Show)

instance Functor (Polynomial) where
    fmap f (Poly sp p) = Poly sp (fmap f p)

instance Rev (Polynomial a) where
    reverse (Poly C2N p) = Poly N2C (reverse p)
    reverse (Poly N2C p) = Poly C2N (reverse p)

instance (Eq a) => Eq (Polynomial a) where
    Poly sp p == Poly sq q = sp == sq && p == q

instance (Num a) => Num (Polynomial a) where
-- (+)
    Poly C2N p + Poly C2N q = Poly C2N (zipWith_ (+)          p           q)
    Poly N2C p + Poly C2N q = Poly C2N (zipWith_ (+) (reverse p)          q)
    Poly C2N p + Poly N2C q = Poly C2N (zipWith_ (+)          p  (reverse q))
    Poly N2C p + Poly N2C q = Poly C2N (zipWith_ (+) (reverse p) (reverse q))
-- (-)
    Poly C2N p - Poly C2N q = Poly C2N (zipWith_ (-)          p           q)
    Poly N2C p - Poly C2N q = Poly C2N (zipWith_ (-) (reverse p)          q)
    Poly C2N p - Poly N2C q = Poly C2N (zipWith_ (-)          p  (reverse q))
    Poly N2C p - Poly N2C q = Poly C2N (zipWith_ (-) (reverse p) (reverse q))
-- (*)
    Poly C2N p * Poly C2N [] = Poly C2N []
    Poly C2N p * Poly C2N (q1 : qs) = Poly C2N (foldr mul [] p)
        where mul 0 bs = 0 : bs
              mul a bs = (a * q1) : zipWith_ (+) (map (a *) qs) bs

    p * Poly C2N q = reverse p * Poly C2N q
    Poly C2N p * q = Poly C2N p * reverse q
    p * q = reverse p * reverse q
-- negate
    negate = fmap negate
-- abs - abs of the leading coefficient
    abs p = p * signum p
-- fromInteger
    fromInteger i = Poly C2N [fromInteger i]
-- signum
    signum (Poly _ []) = Poly C2N []
    signum (Poly N2C (p1 : ps)) = Poly C2N [ signum p1 ]
    signum p = signum $ reverse p

scalePoly :: (Num a) => a -> Polynomial a -> Polynomial a
-- ^ is scalar multiplication - within the vector space of polynomials
scalePoly t = fmap (* t )

norm1 :: Num a => Polynomial a -> a
-- ^ the 1-Norm = $\sum_{i=1}^n |a_i|$
norm1 (Poly _ p) = sum $ map abs p

norm2 :: (Floating a, Num a) => Polynomial a -> a
-- ^ the traditional euclidian norm
norm2 (Poly _ p) = sqrt $ sum $ map (^ 2) p

snorm :: (Ord a, Num a) => Polynomial a -> a
-- ^ supreme/maximum norm - also handy sometime
snorm (Poly _ p) = maximum $ map (abs) p

eval :: Num a => a -> Polynomial a -> a
-- ^ evaluates an /a/-polynomial to an /a/-value
eval n (Poly N2C p) = _eval 0 n p
                    where _eval acc _ (x : []) = acc + x
                          _eval acc n (x : xs) = _eval (acc * n + x) n xs
eval n p = eval n (reverse p)

zipWith_ :: (a -> a -> a) -> [a] -> [a] -> [a]
-- ^ internal helper - like zipWith but appends the longer tail
zipWith_ fun = _zipWith_ fun []


_zipWith_ :: (a -> a -> a) -> [a] -> [a] -> [a] -> [a]
_zipWith_ _ tmp p [] = tmp ++ p
_zipWith_ _ tmp [] q = tmp ++ q
_zipWith_ fun tmp (p1 : ps) (q1 : qs) = _zipWith_ fun _tmp ps qs
                           where _tmp = tmp ++ [ fun p1 q1 ]





