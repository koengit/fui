module Expr where

import Data.Char
import Test.QuickCheck

--------------------------------------------------------------------------
-- A.

data Expr
  = Val Double
  | Mul Expr Expr
  | Add Expr Expr
  | Sin Expr
  | Cos Expr
  | X
 deriving ( Eq, Read )

--------------------------------------------------------------------------
-- B.

-- it is not so important to get the exact minimal number of parentheses
-- in their solutions...

showExpr :: Expr -> String
showExpr (Val d)   = show d
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Sin a)   = "sin " ++ showArg a
showExpr (Cos a)   = "cos " ++ showArg a
showExpr X         = "x"

showFactor :: Expr -> String
showFactor (Add a b) = showParens (Add a b)
showFactor a         = showExpr a

showArg :: Expr -> String
showArg (Add a b) = showParens (Add a b)
showArg (Mul a b) = showParens (Mul a b)
showArg a         = showExpr a

showParens :: Expr -> String
showParens a = "(" ++ showExpr a ++ ")"

instance Show Expr where
  show a = showExpr a

--------------------------------------------------------------------------
-- C.

eval :: Expr -> Double -> Double
eval (Val d)   x = d
eval (Add a b) x = eval a x + eval b x
eval (Mul a b) x = eval a x * eval b x
eval (Sin a)   x = sin (eval a x)
eval (Cos a)   x = cos (eval a x)
eval X         x = x

--------------------------------------------------------------------------
-- D.

readExpr :: String -> Maybe Expr
readExpr s =
  case expr [ c | c <- s, not (isSpace c) ] of
    Just (a,s) -> Just a
    _          -> Nothing

type Parser a = String -> Maybe (a, String)

factor :: Parser Expr
factor ('(':s) =
  case expr s of
    Just (a,')':s') -> Just (a,s')
    _               -> Nothing

factor ('x':s) =
  Just (X,s)

factor ('s':'i':'n':s) =
  case factor s of
    Just (x,s') -> Just (Sin x,s')
    Nothing     -> Nothing

factor ('c':'o':'s':s) =
  case factor s of
    Just (x,s') -> Just (Cos x,s')
    Nothing     -> Nothing

-- this only parses positive numbers, and that's OK
factor s@(c:_) | isDigit c =
  Just (Val d,s')
 where
  ((d,s'):_) = reads s

factor _ =
  Nothing

term = chain '*' factor Mul
expr = chain '+' term   Add

chain :: Char -> Parser a -> (a -> a -> a) -> Parser a
chain op p f s =
  case p s of
    Just (a,s1) ->
      case s1 of
        c:s2 | c == op -> case chain op p f s2 of
                            Just (b,s3) -> Just (f a b,s3)
                            Nothing     -> Nothing
        _      -> Just (a,s1)
    Nothing -> Nothing

--------------------------------------------------------------------------
-- E.

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr a =
  case readExpr (showExpr a) of
    Just a' -> a' == assoc a
    Nothing -> False
 where
  assoc (Add (Add a b) c) = assoc (Add a (Add b c))
  assoc (Add a b)         = Add (assoc a) (assoc b)
  assoc (Mul (Mul a b) c) = assoc (Mul a (Mul b c))
  assoc (Mul a b)         = Mul (assoc a) (assoc b)
  assoc (Sin a)           = Sin (assoc a)
  assoc (Cos a)           = Cos (assoc a)
  assoc a                 = a

prop_ShowReadExpr2 :: Expr -> Double -> Bool
prop_ShowReadExpr2 a x =
  case readExpr (showExpr a) of
    Just a' -> eval a' x ~= eval a x
    Nothing -> False
 where
  x ~= y = abs (y-x) <= 0.001

instance Arbitrary Expr where
  arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr
arbExpr n =
  frequency
  [ (1, do d <- arbitrary
           return (Val (abs d)))
  , (1, do return X)
  , (n, do a <- arbExpr (n`div`2)
           b <- arbExpr (n`div`2)
           return (Add a b))
  , (n, do a <- arbExpr (n`div`2)
           b <- arbExpr (n`div`2)
           return (Mul a b))
  , (n, do a <- arbExpr (n-1)
           return (Sin a))
  , (n, do a <- arbExpr (n-1)
           return (Cos a))
  ]

--------------------------------------------------------------------------
           
           
