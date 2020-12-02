{-# LANGUAGE InstanceSigs #-}

-- | Contains instances for parser combinator.
module Block3.ParserCombinatorTask (Parser (..)) where

import Control.Applicative (Alternative, empty, (<|>))

-- | Parser representation.
-- runParser accepts a stream of data and returns the parse result
-- along with the rest of the unparsed data stream.
newtype Parser s a = Parser {runParser :: [s] -> Maybe (a, [s])}

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) = Parser $ \input -> do
    (a, r) <- runParser (Parser p) input
    return (f a, r)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> return (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser pf) (Parser pa)  = Parser $ \s -> do
    (f, fr) <- runParser (Parser pf) s
    (a, ar) <- runParser (Parser pa) fr
    return (f a, ar)

instance Monad (Parser s) where
  return :: a -> Parser s a
  return a = Parser $ \s -> return (a, s)

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser pa) f  = Parser $ \s -> do
    (a, r) <- runParser (Parser pa) s
    runParser (f a) r

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser pa) (Parser pb) = Parser $ \s -> case pa s of
    Just (a, r) -> Just (a, r)
    Nothing -> pb s
