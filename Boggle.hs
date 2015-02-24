module Boggle where

import Data.List
import Data.Maybe
import System.Random

type Letter = Char
type Dimensions = (Int, Int)
type Coordinate = (Int, Int)
type Tray = (Dimensions, Coordinate -> Maybe Letter)
type WordPath = [Coordinate]

-- | Create an empty tray with the specified dimensions.
--
-- Dimensions should be positive, but this constraint isn't checked.
emptyTray :: Dimensions -> Tray
emptyTray dims = (dims, const Nothing)

-- | List of tray coordinates with no assigned letters.
--
-- >>> emptyPositions $ emptyTray (1,2)
-- [(0,0),(0,1)]
emptyPositions :: Tray -> [Coordinate]
emptyPositions ((w,h), at) =
  [(x,y) | y <- [0..(h-1)], x <- [0..(w-1)], isNothing $ at (x,y)]

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
  if validCoordinate (w,h) (x,y) && isNothing (at (x,y))
  then ((w,h), \coord -> if coord == (x,y)
                         then Just letter
                         else at coord)
  else tray

-- | Sequentially load letters into the tray until it's either full or
-- there are no more letters.
--
-- The process for selecting the next position to fill is
-- deterministic. Shuffle the list of letters beforehand if the goal
-- is to "shake" the tray.
--
-- >>> trayString '_' $ loadTray (emptyTray (2,3)) ['a'..]
-- "ab\ncd\nef"
loadTray :: Tray -> [Letter] -> Tray
loadTray tray letters =
  case (emptyPositions tray, letters) of
    ([], _)     -> tray
    (_, [])     -> tray
    (p:_, c:cs) -> loadTray newTray cs
      where
        newTray = safeInsertLetter tray p c

alphabet :: [Letter]
alphabet = ['a'..'z']

-- | Randomly select an element from the list.
--
-- Returns Nothing if the list is empty:
--
-- >>> pickRandomly []
-- Nothing
pickRandomly :: [a] -> IO (Maybe a)
pickRandomly elts =
  case elts of
    [] -> return Nothing
    _  -> do
      index <- randomRIO (0, (length elts)-1)
      return $ Just $ elts !! index

pickManyRandomly :: Int -> [a] -> IO [a]
pickManyRandomly n xs
  | n <= 0 || null xs = return []
  | otherwise         = do
    p <- pickRandomly xs
    ps <- pickManyRandomly (n-1) xs
    case p of
      Just p' -> do return (p':ps)
      _       -> do return ps

-- |
shuffledTray :: Dimensions -> IO Tray
shuffledTray (w,h) = do
  randomLetters <- pickManyRandomly (w*h) alphabet
  return $ loadTray (emptyTray (w,h)) randomLetters

-- | All valid coordinates neighboring the provided coordinate. There
-- can be up to 8 neighbors (4 sides and 4 diagonals).
--
-- >>> neighbors (2,2) (0,0)
-- [(1,0),(0,1),(1,1)]
--
-- >>> neighbors (3,3) (1,1)
-- [(0,0),(1,0),(2,0),(0,1),(2,1),(0,2),(1,2),(2,2)]
--
-- >>> neighbors (1,1) (0,0)
-- []
--
-- There are no more than 8 neighbors:
-- prop> length (neighbors d c) <= 8
--
-- No coordinate neighbors itself:
-- prop> not $ c `elem` (neighbors d c)
--
-- No neighbors fall outside the space defined by the dimensions parameter:
-- prop> not $ any (\(x,y) -> x < 0 || y < 0 || x >= w || y >= h) (neighbors (w,h) c)
--
-- Returned list of neighbors has no duplicates:
-- prop> neighbors d c == nub (neighbors d c)
neighbors :: Dimensions -> Coordinate -> [Coordinate]
neighbors (w,h) (x,y) =
  filter (validCoordinate (w,h)) [(x+i,y+j) | j <- [-1..1], i <- [-1..1], (i,j) /= (0,0)]

-- | Locate a letter on the tray. Returns a list of coordinates for
-- all matches.
--
-- >>> findLetter (emptyTray (1,1)) 'x'
-- []
--
-- >>> findLetter (loadTray (emptyTray (2,2)) "wxyz") 'x'
-- [(1,0)]
--
-- >>> findLetter (loadTray (emptyTray (2,2)) "xxxx") 'x'
-- [(0,0),(0,1),(1,0),(1,1)]
findLetter :: Tray -> Letter -> [Coordinate]
findLetter ((w,h), at) letter =
  [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)], at (x,y) == Just letter]

-- | Locate neighbors matching the provided letter.
--
-- >>> findNeighboringLetter (loadTray (emptyTray (2,2)) "wxyz") (0,0) 'z'
-- [(1,1)]
findNeighboringLetter :: Tray -> Coordinate -> Letter -> [Coordinate]
findNeighboringLetter tray@(dims,_) fromCoord letter =
  filter (`elem` neighbors dims fromCoord) letterCoords
  where
    letterCoords = findLetter tray letter

-- | Find all paths on the Boggle tray which spell out the specified
-- word. No coordinate may be used multiple times in the same path.
--
-- >>> findWord (loadTray (emptyTray (1,1)) "x") "x"
-- [[(0,0)]]
--
-- >>> findWord (loadTray (emptyTray (1,1)) "A") "x"
-- []
--
-- >>> findWord (loadTray (emptyTray (2,2)) "x--x") "x"
-- [[(0,0)],[(1,1)]]
--
-- >>> findWord (loadTray (emptyTray (2,2)) "cat-") "cat"
-- [[(0,0),(1,0),(0,1)]]
--
-- >>> findWord (loadTray (emptyTray (2,2)) "patp") "pat"
-- [[(0,0),(1,0),(0,1)],[(1,1),(1,0),(0,1)]]
--
-- There are no words on an empty tray:
-- prop> findWord (emptyTray (w,h)) word == []
findWord :: Tray -> [Letter] -> [WordPath]
findWord tray word =
  case word of
    [] -> []
    (w:ws) -> map reverse $ findWord' ws $ initialPaths w
  where
    initialPaths letter = map (: []) $ findLetter tray letter
    findWordFromPath word path =
      case (word, path) of
        ("", _) -> [path]
        (w:ws, c:cs) -> findWord' ws [m:(c:cs) | m <- findNeighboringLetter tray c w, m `notElem` (c:cs)]
    findWord' w =
      concatMap (findWordFromPath w)

-- | Search tray for the provided words and return all that are found.
--
-- There are no words on an empty tray:
-- prop> wordsOnTray (emptyTray (w,h)) ws == []
wordsOnTray :: Tray -> [[Letter]] -> [[Letter]]
wordsOnTray tray = filter (not . null . findWord tray)

-- | String representation of a Tray.
--
-- >>> putStr $ trayString '*' $ emptyTray (3, 2)
-- ***
-- ***
--
-- >>> putStr $ trayString '_' $ emptyTray (4, 4)
-- ____
-- ____
-- ____
-- ____
--
-- >>> putStr $ trayString '_' (safeInsertLetter (safeInsertLetter (emptyTray (4, 4)) (2,0) 'x') (1,1) 'y')
-- __x_
-- _y__
-- ____
-- ____
trayString :: Char -> Tray -> String
trayString emptyChar ((w,h), at) =
  intercalate "\n" rows
  where
    displayLetter = fromMaybe emptyChar
    row y = [displayLetter $ at (x,y) | x <- [0..(w-1)]]
    rows = [row y | y <- [0..(h-1)]]
