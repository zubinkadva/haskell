{-
 - Author:  Zubin Kadva, 902772316, zkadva2016@my.fit.edu
 - Author:  Aditya Karanjkar, 902832282, akaranjkar2016@my.fit.edu
 - Course:  CSE 5400, Fall 2016
 - Project: Unify
 -}

import qualified Data.Map as Map

type X = Char

type F = Char

-- Abstract grammar
data Expr = Var X
          | Fun F [Expr]
  deriving (Eq, Ord, Show, Read)

-- Examples expressions
x = Var 'x'   -- "x"
y = Var 'y'
z = Var 'z'
xa = Fun 'a' []
xb = Fun 'b' []
fx = Fun 'f' [x]
fy = Fun 'f' [y]
fa = Fun 'f' [xa]
fab = Fun 'f' [xa, xb]
fax = Fun 'f' [xa, x]
fxy = Fun 'f' [x, y]
fxb = Fun 'f' [x, xb]
fyy = Fun 'f' [y, y]
fyz = Fun 'f' [y, z]
fya = Fun 'f' [y, xa]  -- "f(y,a)"
gy = Fun 'g' [y]
ga = Fun 'g' [xa]
fgx = Fun 'f' [Fun 'g' [x]]
fgxx = Fun 'f' [Fun 'g' [x], x]

type Subst = [(X, Expr)]

-- apply a substitution to an expression
apply :: Subst -> Expr -> Expr
apply [] expr = expr
apply subst (Fun f []) =
  Fun f []
apply subst (Fun f x) = Fun f (map (apply subst) x)
apply subst (Var x) =
  case mlkp x of
    Just a  -> apply subst a
    Nothing -> Var x
  where
    substmap = Map.fromList subst
    mlkp y = Map.lookup y substmap

{-
        Two terms unify if 
   (apply sigma t1) == (apply sigma t2)
-}

-- find the unifying substitution, if any, of two expressions
unify :: Expr -> Expr -> Maybe Subst
unify x y = subst
  where
    (_, _, subst) = tryUnify (x, y, Just [])

----------------------
-- Auxillary functions
----------------------

-- Occurs function for expressions
occurs :: Expr -> Expr -> Bool
occurs (Var x) (Var y) =
  x == y
occurs (Var x) (Fun f a) =
  or (map (occurs (Var x)) a)
occurs _ _ = False

-- Mapping already exists
exists :: Expr -> Subst -> Bool
exists (Var x) [] = False
exists (Var x) subst =
  case mlkp x of
    Just a  -> True
    Nothing -> False
  where
    substmap = Map.fromList subst
    mlkp y = Map.lookup y substmap
exists (Fun _ _) _ = False

-- Substitute and add a mapping to a substitution list
substAndAdd :: (X, Expr) -> Subst -> Subst
substAndAdd (x, e) subst =
  if null (filter (\(v, t) -> occurs (Var x) t) subst)
    then (x, apply subst e) : subst
    else (x, e) : (Map.toList (Map.map (substitute (x, e)) substmap))
  where
    substmap = Map.fromList subst

-- Substitute term in other mappings in substitution list
substitute :: (X, Expr) -> Expr -> Expr
substitute (x, e) (Var y)
  | x == y = e
  | otherwise = (Var y)
substitute (x, e) (Fun f a) =
  Fun f (map (substitute (x, e)) a)

-- Attempt to unify two expressions
tryUnify :: (Expr, Expr, Maybe Subst) -> (Expr, Expr, Maybe Subst)
tryUnify (x, y, Nothing) =
  (x, y, Nothing)
tryUnify (Var x, Var y, Just subst)
  | x == y = (Var x, Var y, Just subst)
  | otherwise = if (exists (Var x) subst)
                  then (Var x, Var y, Nothing)
                  else let newsubst = substAndAdd (x, Var y) subst
                           e1 = apply newsubst (Var x)
                           e2 = apply newsubst (Var y)
                       in (e1, e2, Just newsubst)
tryUnify (Var x, Fun f a, Just subst) =
  if (not (exists (Var x) subst) && not (occurs (Var x) (Fun f a)))
    then let newsubst = substAndAdd (x, Fun f a) subst
             e1 = apply newsubst (Var x)
             e2 = apply newsubst (Fun f a)
         in (e1, e2, Just newsubst)
    else (Var x, Fun f a, Nothing)
tryUnify (Fun f a, Var y, Just subst) =
  if (not (exists (Var y) subst) && not (occurs (Var y) (Fun f a)))
    then let newsubst = substAndAdd (y, Fun f a) subst
             e1 = apply newsubst (Fun f a)
             e2 = apply newsubst (Var y)
         in (e1, e2, Just newsubst)
    else (Fun f a, Var y, Nothing)
tryUnify (Fun f [], Fun g [], Just subst)
  | f == g = (Fun f [], Fun g [], Just subst)
  | otherwise = (Fun f [], Fun g [], Nothing)
tryUnify (Fun f [], Fun g b, Just subst) =
  (Fun f [], Fun g b, Nothing)
tryUnify (Fun f a, Fun g [], Just subst) =
  (Fun f a, Fun g [], Nothing)
tryUnify (Fun f (a:as), Fun g (b:bs), Just subst)
  | f == g = if (a : as) == (b : bs)
               then (Fun f (a : as), Fun g (b : bs), Just subst)
               else if (length (a : as) == length (b : bs))
                      then let (_, _, newsubst) = tryUnify (a, b, Just subst)
                           in case newsubst of
                             Just ns -> tryUnify
                                          (apply ns (Fun f (as)), apply ns (Fun g (bs)), newsubst)
                             Nothing -> (Fun f (a : as), Fun g (b : bs), Nothing)
                      else (Fun f (a : as), Fun g (b : bs), Nothing)
  | otherwise = (Fun f (a : as), Fun g (b : bs), Nothing)
