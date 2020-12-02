-- | Contains a parser for lists of lists of integers.
module Block3.NotSimpleParsersTask (listlistParser) where

import Block3.BasicCombinatorsTask (element, eof, satisfy)
import Block3.ParserCombinatorTask (Parser (..))
import Block3.SimpleParsersTask (int)
import Control.Applicative (empty, many)
import Data.Char (isSpace)

-- | Accepts a comma separated list of integers
ints :: Parser Char [Int]
ints = (:) <$> spacedInt <*> fmap concat (many (element ',' *> ints))
  where
    spacedInt :: Parser Char Int
    spacedInt = spaces *> int <* spaces
    spaces :: Parser Char String
    spaces = many (satisfy isSpace)

-- | Accepts a list of lists, described as a comma searated list
-- of integers and list sizes.
-- e.g. [1, 1, 2, 1, 2, 3, 1, 2, 3] -> [[1], [1, 2], [1, 2, 3]]
-- first number denotes the amount if ints in this list.
listlistParser :: Parser Char [[Int]]
listlistParser =
  ints >>= \flat ->
    eof *> maybe empty return (convert flat)
  where
    convert [] = Just []
    convert (n : xs) =
      if n < 0 || length xs < n
        then Nothing
        else do
          let (a, r) = splitAt n xs
          s <- convert r
          return (a : s)
