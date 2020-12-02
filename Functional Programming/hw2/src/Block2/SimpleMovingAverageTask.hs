-- | Contains an implementation of the Simple Moving Average algorithm. 
module Block2.SimpleMovingAverageTask (moving) where

import Control.Monad.State.Lazy
import Numeric.Natural (Natural)

-- | Returns the simple moving average of the provided array
-- with given window size.
moving :: Natural -> [Int] -> [Float]
moving n as = evalState (getState 0) (as, as, 0)
  where
    getState :: Natural -> State ([Int], [Int], Int) [Float]
    getState i = do
      (s, r, sum_) <- get
      case r of
        [] -> return []
        (rr : rs) ->
          if i < n
            then do
              let newSum = sum_ + rr
              put (s, rs, newSum)
              avs <- getState (i + 1)
              return (divIntNat newSum (i + 1) : avs)
            else do
              let newSum = sum_ + rr - head s
              put (tail s, rs, newSum)
              avs <- getState (i + 1)
              return (divIntNat newSum n : avs)
    divIntNat :: Int -> Natural -> Float
    divIntNat a b = realToFrac a / realToFrac b
