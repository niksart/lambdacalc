module Lambda where

import Prelude hiding (succ)
import Data.Set
-- import Data.List

-- Type definition for the implementation of Lambda Calculus
type Name = String
data Expr
  = Var Name      -- ^ Variable
  | Lam Name Expr -- ^ Abstraction
  | App Expr Expr -- ^ Application
  deriving (Show, Eq)

-- | Function that returns the "body" of the lambda expression
viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x         = x

{-
   To avoid capture, you have to know the free and binded variables.
   This example explain why:
   (\x.xy) [y/x] => that means "substitute in the first expression the xs with a y"
   In this case, to avoid variable capture, I have to rename "y" because it is
   part of the free variables of (\x.xy)
-}

{-
-- Implementation with Data.List

bindV :: Expr -> [Name]
bindV (Lam v a) = v : bindV a
bindV _ = []

allV' :: Expr -> [Name]
allV' (Var v) = v : []
allV' (Lam v e) = v : (allV' e)
allV' (App e1 e2) = (allV' e1) ++ (allV' e2)

allV :: Expr -> [Name]
allV e = nub $ allV' e

freeV :: Expr -> [Name]
freeV e = (allV e) \\ (bindV e)
-}

-- Implementation with Data.Set (preferable) --

-- | Function that returns a Set Name that contains the binded variables
bindV :: Expr -> Set Name
bindV (Lam v a) = insert v (bindV a)
bindV         _ = empty

-- | Function that returns a Set Name that contains all the variables (free and binded)
allV :: Expr -> Set Name
allV (Var v)     = singleton v
allV (Lam v e)   = insert v (allV e)
allV (App e1 e2) = allV e1 `union` allV e2

-- | Function that returns a Set Name that contains the free variables (allV e - bindV e)
freeV :: Expr -> Set Name
freeV e = allV e \\ bindV e

--                                            --

-- | Step by step reduction using the beta reduction rule
reduce :: Expr -> Expr
reduce (Var n) = Var n
reduce (Lam n e) = Lam n $ reduce e
reduce (App (Var n) x) = App (Var n) $ reduce x
reduce (App (Lam n e) x) = {-reduce $-} subst n x e --REDEX
reduce (App a b) = App (reduce a) (reduce b)

-- | One step reduction of a lambda expression
reducerec :: Expr -> Expr
reducerec x | r == x = x
            | otherwise = reducerec r
  where r = reduce x

subst :: Name -- ^ substitute this name (in a lambda expression)
      -> Expr -- ^ with this expression
      -> Expr -- ^ and find it (the name) in this expression
      -> Expr -- ^ resulting expression
subst from to (Var n)   = if n == from then to else Var n
subst from to (App a b) = App (subst from to a) (subst from to b)
subst from to (Lam n e) | from == n = Lam n e
                        | otherwise = Lam n $ subst from (preventCapture to e) e

-- | This function avoid variable capture. In renaming this function adds "'" at the end of the variable.
preventCapture to@(Var v) a = if member v (freeV a) then Var (v++"'") else to -- if list implementation use elem instead of member
preventCapture (App b c) a = App (preventCapture b a) (preventCapture c a)
preventCapture (Lam n e) a = Lam n' $ preventCapture e a
  where n' = if member n (freeV a) then n ++ "'" else n

-- | Function that returns a string in the style of lambda calculus. (Lam "x" (Var "x")) => (\x.x). It is an auxiliary function for lprint
printex :: Expr -> String
printex (Var n) = n
printex l@(Lam _ _) = "(\\"++ f l
  where f (Lam n e) = n ++ f e
        f e = "." ++ printex e ++ ")"
printex (App a b) = "(" ++ printex a ++ printex b ++ ")"

-- | Function that prints the string result of printex
lprint :: Expr -> IO ()
lprint = putStrLn . printex

-- DEFINITION of useful lambda expressions

succ :: Expr
succ = Lam "w"
     $ Lam "x"
     $ Lam "y"
       $ App (Var "x")
           $ App (App (Var "w")
                      (Var "x"))
                 (Var "y")

zero :: Expr
zero = Lam "s" $ Lam "z" $ Var "z"

peano :: (Ord t, Num t) => t -> Expr
peano 0 = zero
peano n | n < 0 = error "Please, only natural numbers."
        | otherwise = reducerec $ App succ $ peano (n-1)

mult :: Expr
mult = Lam "x" $ Lam "y" $ Lam "f" $ App (Var "x") $ App (Var "y") (Var "f")

pow :: Expr
pow = Lam "x" $ Lam "y" $ App (Var "y") (Var "x")

yComb :: Expr
yComb = Lam "f" $ App (Lam "x" $ App (Var "f") $ App (Var "x") (Var "x")) (Lam "x" $ App (Var "f") $ App (Var "x") (Var "x"))
