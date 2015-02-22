-- |

module Boggle where

import Data.List
import Data.Maybe

type Letter = Char
type Dimensions = (Int, Int)
type Coordinate = (Int, Int)
type Tray = (Dimensions, (Coordinate -> Maybe Letter))

-- | Create an empty tray with the specified dimensions.
--
-- Dimensions should be positive, but this constraint isn't checked.
emptyTray :: Dimensions -> Tray
emptyTray dims = (dims, (\_ -> Nothing))

-- | List of tray coordinates with no assigned letters.
--
-- >>> emptyPositions $ emptyTray (1,2)
-- [(0,0),(0,1)]
emptyPositions :: Tray -> [Coordinate]
emptyPositions ((w,h), at) =
  [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)], isNothing $ at (x,y)]

-- | Check if every valid coordinate on the tray has been assigned a
-- letter.
--
-- >>> isFull $ emptyTray (0,0)
-- True
--
-- >>> isFull $ emptyTray (1,1)
-- False
--
-- >>> isFull $ safeInsertLetter (emptyTray (1,1)) (0,0) 'x'
-- True
isFull :: Tray -> Bool
isFull = null . emptyPositions

-- | Check whether coordinate is within the bounds of the specified
-- dimensions.
--
-- >>> validCoordinate (3,5) (3,5)
-- False
--
-- >>> validCoordinate (3,5) (2,4)
-- True
--
-- >>> validCoordinate (3,5) (0,0)
-- True
--
-- >>> validCoordinate (3,5) (-1,2)
-- False
validCoordinate :: Dimensions -> Coordinate -> Bool
validCoordinate (w,h) (x,y) =
  x >= 0 && x < w &&
  y >= 0 && y < h

-- | Insert letter into tray at the specified coordinates. If the
-- coordinates are outside the tray's dimensions or another letter
-- already occupies that position, return the original tray.
safeInsertLetter :: Tray -> Coordinate -> Letter -> Tray
safeInsertLetter tray@((w,h), at) (x,y) letter =
  if validCoordinate (w,h) (x,y) && (isNothing $ at (x,y))
  then ((w,h), (\coord -> if coord == (x,y)
                          then Just letter
                          else at coord))
  else tray

-- | String representation of a Tray.
--
-- >>> trayString '*' $ emptyTray (3, 2)
-- "***\n***"
--
-- >>> trayString '_' $ emptyTray (4, 4)
-- "____\n____\n____\n____"
--
-- >>> trayString '_' (safeInsertLetter (safeInsertLetter (emptyTray (4, 4)) (2,0) 'x') (1,1) 'y')
-- "__x_\n_y__\n____\n____"
trayString :: Char -> Tray -> String
trayString emptyChar ((w,h), at) =
  intercalate "\n" rows
  where
    displayLetter = fromMaybe emptyChar
    row y = [displayLetter $ at (x,y) | x <- [0..(w-1)]]
    rows = [row y | y <- [0..(h-1)]]
