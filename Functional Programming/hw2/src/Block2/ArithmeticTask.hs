{-# LANGUAGE InstanceSigs #-}

-- | Contains arithmetic expression classes and evaluation logic.
module Block2.ArithmeticTask (ArithmeticError (..), Expr (..), eval) where

-- | Arithmetic expressions representation
data Expr
  = Const Int      -- ^ Represents an integer constant
  | Add Expr Expr  -- ^ Represents a + b
  | Sub Expr Expr  -- ^ Represents a - b
  | Mul Expr Expr  -- ^ Represents a * b
  | Div Expr Expr  -- ^ Represents a / b
  | Pow Expr Expr  -- ^ Represents a ^ b
  deriving (Show)

instance Eq Expr where
  (==) :: Expr -> Expr -> Bool
  (==) (Const a) (Const b) = a == b
  (==) (Add a b) (Add c d) = a == c && b == d
  (==) (Sub a b) (Sub c d) = a == c && b == d
  (==) (Mul a b) (Mul c d) = a == c && b == d
  (==) (Div a b) (Div c d) = a == c && b == d
  (==) (Pow a b) (Pow c d) = a == c && b == d
  (==) _ _                 = False

-- | Represents an error in an arithmetic expression
data ArithmeticError
  = DivisionByZero    -- ^ Occurs, when division by zero occurs
  | NegativeExponent  -- ^ Occurs, when an expression is raised to a negative power
  deriving (Show)

instance Eq ArithmeticError where
  (==) :: ArithmeticError -> ArithmeticError -> Bool
  (==) DivisionByZero DivisionByZero     = True
  (==) NegativeExponent NegativeExponent = True
  (==) _ _                               = False

-- | Evaluates the given expression and returns the result,
-- or an ArithmeticError if an error occurs.
eval :: Expr -> Either ArithmeticError Int
eval (Const a) = return a
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = case eval b of
  Right c ->
    if c == 0
      then Left DivisionByZero
      else div <$> eval a <*> eval b
  Left s -> Left s
eval (Pow a b) = case eval b of
  Right c ->
    if c < 0
      then Left NegativeExponent
      else (^) <$> eval a <*> Right c
  Left s -> Left s

