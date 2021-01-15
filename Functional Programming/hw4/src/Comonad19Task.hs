-- | This module contains a virus spread simulation
-- on a 2D grid using ListZipper.
module Comonad19Task
  ( evolve
  , showGrid
  , initVirus
  ) where

import Control.Comonad (duplicate, extend, extract)
import Control.Monad (liftM2)
import Grid (Grid (..), ListZipper (..), down, gridWrite, left, right, toList,
             up)
import System.Random (StdGen, mkStdGen, randomR)

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where
    horizontals = [left, right]
    verticals = [up, down]

data Stage
  = Healthy StdGen
  | Infected StdGen Int
  | Sick StdGen Int
  | Immune StdGen Int

instance Show Stage where
  show (Healthy _)      = " "
  show (Infected _ _) = "."
  show (Sick _ _)       = "#"
  show (Immune _ _)     = "@"

data Config = Config
  { incubationPeriod :: Int,
    sicknessPeriod   :: Int,
    immunityPeriod   :: Int,
    infectionProb    :: Double
  }

config :: Config
config =
  Config
    { incubationPeriod = 7,
      sicknessPeriod = 7,
      immunityPeriod = 30,
      infectionProb = 0.75
    }

allNeighbours :: Grid Stage -> [Stage]
allNeighbours g = map (\direction -> extract $ direction g) neighbours

next :: Grid Stage -> Stage
next g = case extract g of
  Healthy gen -> case getsSick gen (allNeighbours g) of
    (True, newGen)  -> Infected newGen 0
    (False, newGen) -> Healthy newGen
  Infected gen days -> if days == incubationPeriod config then Sick gen 0 else Infected gen (days + 1)
  Sick gen days -> if days == sicknessPeriod config then Immune gen 0 else Sick gen (days + 1)
  Immune gen days -> if days == immunityPeriod config then Healthy gen else Immune gen (days + 1)
  where
    getsSick :: StdGen -> [Stage] -> (Bool, StdGen)
    getsSick gen (cur : rest) = do
      let (prob, nextGen) = randomR (0, 1) gen
      let (nextSick, lastGen) = getsSick nextGen rest
      (getsSick' cur prob || nextSick, lastGen)
    getsSick gen [] = (False, gen)

    getsSick' :: Stage -> Double -> Bool
    getsSick' (Healthy _) _         = False
    getsSick' (Infected _ _) prob = prob < infectionProb config
    getsSick' (Sick _ _) prob       = prob < infectionProb config
    getsSick' (Immune _ _) _        = False

-- | Display the current state of the simulation grid of provided size.
showGrid :: Grid Stage -> Int -> String
showGrid (Grid g) n = unlines (map (>>= show) $ toList (fmap (`toList` n) g) n)

-- | Generate a start grid with one infected.
initVirus :: Grid Stage
initVirus = do
  let grid = Grid (duplicate (fmap (Healthy . mkStdGen) (LZ [-1, -2 ..] 0 [1, 2 ..])))
  let i_like_eating_bats_for_dinner = Infected (mkStdGen 0) 0
  gridWrite i_like_eating_bats_for_dinner grid

-- | Emulate a step in the simulation
evolve :: Grid Stage -> Grid Stage
evolve = extend next