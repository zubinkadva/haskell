{-
Author: Zubin Kadva, 902772316, zkadva2016@my.fit.edu
Course: CSE 5400, Fall 2016
Project: Norm
Example run:
*Main> +++ OK, passed 100 tests.
-}

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Prelude hiding (True, False, negate)

-- Testing the program
-- Simple checks
test1 :: IO()
test1 = quickCheck doubleNegation

test2 :: IO()
test2 = quickCheck doubleNotConjunction

test3 :: IO()
test3 = quickCheck doubleNotDisjunction

-- Checks with variables
test4 :: IO()
test4 = quickCheck doubleVNegation

test5 :: IO()
test5 = quickCheck doubleVNotConjunction

test6 :: IO()
test6 = quickCheck doubleVNotDisjunction

-- Convert any proposition to one in negation normal form
-- 1. Handle True / False / Variables
norm :: Proposition -> Proposition
norm (Variable v) = Variable v
norm (True t)     = True t
norm (False f)    = False f

-- 2. Handle x && y and x || y
norm (x `Conjunction` y) = (norm x) `Conjunction` (norm y)
norm (x `Disjunction` y) = (norm x) `Disjunction` (norm y)

-- 3. Handle negation of True / False / Variables / Negation
norm (Negation (Variable v)) = Negation (Variable v)
norm (Negation (True t))     = Negation (True t)
norm (Negation (False f))    = Negation (False f)
norm (Negation (Negation x)) = norm x

-- 4. Handle ~(x && y) and ~(x || y)
norm (Negation (x `Conjunction` y)) = norm ((Negation x) `Disjunction` (Negation y))
norm (Negation (x `Disjunction` y)) = norm ((Negation x) `Conjunction` (Negation y))

-- Datatype representing propositions
data Proposition 
    = Variable Char
    | True Bool
    | False Bool    
    | Negation Proposition
    | Conjunction Proposition Proposition
    | Disjunction Proposition Proposition
    deriving (Show, Eq)

-- Create instance of class Arbitary
instance Arbitrary Proposition where
  arbitrary = sized randomPropositions where
    randomPropositions n 
      | n > 0     = oneof [negate $ randomPropositions (n `div` 4), randomBin $ randomPropositions (n `div` 4)]
      | otherwise = Variable <$> arbitrary 
      where
        negate x       = Negation <$> x
        randomBin rExp = oneof . map (\c -> liftM2 c rExp rExp) $ [Conjunction, Disjunction]

-- 1. Simple double negation
doubleNegation :: Proposition -> Bool
doubleNegation x = norm (Negation (Negation x)) == norm x

-- 2. Simple not conjunction
doubleNotConjunction :: Proposition -> Proposition -> Bool
doubleNotConjunction x y = norm (Negation (x `Conjunction` y)) == norm ((Negation x) `Disjunction` (Negation y))

-- 3. Simple not disjunction
doubleNotDisjunction :: Proposition -> Proposition -> Bool
doubleNotDisjunction x y = norm (Negation (x `Disjunction` y)) == norm ((Negation x) `Conjunction` (Negation y))

-- 4. Variable double negation
doubleVNegation :: Char -> Bool
doubleVNegation x = norm (Negation (Negation (Variable x))) == norm (Variable x)

-- 5. Variable not conjunction
doubleVNotConjunction :: Char -> Char -> Bool
doubleVNotConjunction x y = norm (Negation ((Variable x) `Conjunction` (Variable y))) == (Negation (Variable x)) `Disjunction` (Negation (Variable y))

-- 6. Variable not disjunction
doubleVNotDisjunction :: Char -> Char -> Bool
doubleVNotDisjunction x y = norm (Negation ((Variable x) `Disjunction` (Variable y))) == (Negation (Variable x)) `Conjunction` (Negation (Variable y))