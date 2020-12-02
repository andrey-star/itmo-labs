-- | Contains parser for bracket sequences and integers.
module Block3.SimpleParsersTask (balancedBrackets, int) where

import Block3.BasicCombinatorsTask (element, eof, satisfy)
import Block3.ParserCombinatorTask (Parser (..))
import Control.Applicative (many, some, (<|>))
import Data.Char (digitToInt, isDigit)

-- | Accepts correct bracket sequences.
balancedBrackets :: Parser Char ()
balancedBrackets = balancedBrackets' *> eof
  where
    balancedBrackets' = many (element '(' *> balancedBrackets' *> element ')')

-- | Accepts integers, optionally starting with '+'/'-'.
int :: Parser Char Int
int = nonNeg <|> (sign <*> nonNeg)
  where
    digit :: Parser Char Int
    digit = digitToInt <$> satisfy isDigit

    sign :: Parser Char (Int -> Int)
    sign = negate <$ element '-' <|> id <$ element '+'

    nonNeg :: Parser Char Int
    nonNeg = foldl (\s d -> s * 10 + d) 0 <$> some digit
