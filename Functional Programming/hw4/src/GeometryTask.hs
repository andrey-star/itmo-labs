{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains a 'Point' data type
-- and a number of geometric functions.
module GeometryTask
  ( Point (..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  , dist
  ) where

-- | Represents a point on a 2D plane.
data Point = Point !Int !Int deriving (Show)

instance Eq Point where
  (==) :: Point -> Point -> Bool
  (==) (Point x1 y1) (Point x2 y2) = (x1 == x2) && (y1 == y2)

-- | Adds two points by each coordinate.
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Subtracts two points by each coordinate.
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Calculates the scalar product of two points.
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | Calculates the cross product of two points.
crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | Calculates the distance between two points.
dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt (fromIntegral ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)))

-- | Calculates the perimeter of a closed polygon.
perimeter :: [Point] -> Double
perimeter = geomFold dist

-- | Calculates the double area of a closed polygon.
doubleArea :: [Point] -> Int
doubleArea = abs . geomFold crossProduct

geomFold :: (Num a) => (Point -> Point -> a) -> [Point] -> a
geomFold f ps = go 0 ps
  where
    go _ []                 = 0
    go !acc [last_p]        = acc + f last_p (head ps)
    go !acc (p1 : p2 : pps) = go (acc + f p1 p2) (p2 : pps)
