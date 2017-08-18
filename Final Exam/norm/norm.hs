{-
 - Author:  Aditya Karanjkar, 902832282, akaranjkar2016@my.fit.edu
 - Course:  CSE 5400, Fall 2016
 - Project: Norm
 -}

import           Control.Applicative
import           Control.Monad
import           Test.QuickCheck

-- Type synonyms for truth and variables
type Truth = Bool

type Variable = Char

-- Data type for propositions
data Expr = T Truth
          | V Variable
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
  deriving (Show, Eq)

-- Code to make Expr datatype an instance of Aribtrary was borrowed and modified from the Hatt
-- package for Haskell
instance Arbitrary Expr where
  arbitrary = randomExpr

randomExpr :: Gen Expr
randomExpr = sized randomExpr'

randomExpr' :: Int -> Gen Expr
randomExpr' n
  | n > 0 = oneof [randomNeg boundedExpr, randomBin boundedExpr]
  | otherwise = randomVar
  where
    boundedExpr = randomExpr' (n `div` 4)

randomBin :: Gen Expr -> Gen Expr
randomBin rExp = oneof . map (\c -> liftM2 c rExp rExp) $ [And, Or]

randomNeg :: Gen Expr -> Gen Expr
randomNeg rExp = Not <$> rExp

randomVar :: Gen Expr
randomVar = V <$> arbitrary

-- Function to convert a proposition to negation normal form
norm :: Expr -> Expr
norm (T t) = T t
norm (Not (T t)) = Not (T t)
norm (V v) = V v
norm (Not (V v)) = Not (V v)
norm (Not (Not a)) = norm a
norm (a `And` b) = (norm a) `And` (norm b)
norm (Not (a `And` b)) = norm ((Not a) `Or` (Not b))
norm (a `Or` b) = (norm a) `Or` (norm b)
norm (Not (a `Or` b)) = norm ((Not a) `And` (Not b))

-- QuickCheck all properties
test1 :: IO ()
test1 = quickCheck prop_NNV

test2 :: IO ()
test2 = quickCheck prop_NAV

test3 :: IO ()
test3 = quickCheck prop_NOV

test4 :: IO ()
test4 = quickCheck prop_NN

test5 :: IO ()
test5 = quickCheck prop_NA

test6 :: IO ()
test6 = quickCheck prop_NO

-- Properties for double negation, not and, not or, for variables and expressions
prop_NNV :: Variable -> Bool
prop_NNV x = norm (Not (Not (V x))) == norm (V x)

prop_NAV :: Variable -> Variable -> Bool
prop_NAV x y = norm (Not ((V x) `And` (V y))) == (Not (V x)) `Or` (Not (V y))

prop_NOV :: Variable -> Variable -> Bool
prop_NOV x y = norm (Not ((V x) `Or` (V y))) == (Not (V x)) `And` (Not (V y))

prop_NN :: Expr -> Bool
prop_NN x = norm (Not (Not x)) == norm x

prop_NA :: Expr -> Expr -> Bool
prop_NA x y = norm (Not (x `And` y)) == norm ((Not x) `Or` (Not y))

prop_NO :: Expr -> Expr -> Bool
prop_NO x y = norm (Not (x `Or` y)) == norm ((Not x) `And` (Not y))
