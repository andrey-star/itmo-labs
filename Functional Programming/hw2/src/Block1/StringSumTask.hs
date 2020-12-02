-- | Contains a function for summing integers in a string
module Block1.StringSumTask
  ( stringSum
  ) where

import Text.Read (readMaybe)

-- | Returns the sum of integers in the passed string,
-- or Nothing if a parse error occurs
-- e.g. stringSum "1 2 3 4 5" produces Just 15
-- e.g. stringSum "1 2 a 4 5" produces Nothing
stringSum :: String -> Maybe Int
stringSum s = sum <$> traverse readMaybe (words s)
