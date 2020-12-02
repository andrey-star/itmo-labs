{-# LANGUAGE InstanceSigs #-}

-- | Contains natural number data type implementation
-- and related methods.
module Block1.NatTask
  ( Nat (..)
  , natToInteger
  , isEven
  , natDiv
  , natMod
  ) where

-- | Natural number representation
data Nat
  = Z      -- ^ Represents 0
  | S Nat  -- ^ Represents the following number
  deriving (Show)

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z = True
  (==) (S n1) (S n2) = n1 == n2
  (==) _ _ = False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) Z _ = True
  (<=) (S n1) (S n2) = n1 <= n2
  (<=) _ _ = False

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) Z n = n
  (+) (S n1) n2 = S $ n1 + n2

  (-) :: Nat -> Nat -> Nat
  (-) Z _ = Z
  (-) n Z = n
  (-) (S n1) (S n2) = n1 - n2

  (*) :: Nat -> Nat -> Nat
  (*) _ Z = Z
  (*) n1 (S n2) = n1 * n2 + n1

  abs :: Nat -> Nat
  abs n = n

  signum :: Nat -> Nat
  signum Z = Z
  signum _ = S 0

  fromInteger :: Integer -> Nat
  fromInteger n
    | n < 0 = error "Out of bounds"
    | n == 0 = Z
    | otherwise = S $ fromInteger $ n - 1

-- | Converts 'Nat' to 'Integer'
natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger (S n) = natToInteger (n :: Nat) + 1

-- | Returns true, if this 'Nat' is even, false otherwise
isEven :: Nat -> Bool
isEven Z = True
isEven (S n) = not $ isEven n
  
-- | Returns the division quotient of the passed 'Nat's
natDiv :: Nat -> Nat -> Nat
natDiv _ Z = error "Division by zero"
natDiv n1 n2
  | n1 < n2 = Z
  | otherwise = S (natDiv (n1 - n2) n2)

-- | Returns division remainder of the passed 'Nat's
natMod :: Nat -> Nat -> Nat
natMod n1 n2 = n1 - (natDiv n1 n2 * n2)
