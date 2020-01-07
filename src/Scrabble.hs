-- | Scrabble is the board and related functions (applying a move, validating a move, scoring a move)

module Scrabble where

import Data.List.Split
import Data.List
import Control.Lens

import qualified Data.Vector as V

import Prelude hiding (Word)

-- Determines if a potential move is legal
isMoveLegal :: Dictionary -> Grid -> Move -> Bool
isMoveLegal dict grid move = canPlaceTiles && allWordsLegal
  where
    canPlaceTiles = canPlaceAllTiles grid move
    allWordsLegal = all (isLegalWord dict) (allWords grid)
  
-- Applies a move to board, assuming that the move is legal
applyMove :: Move -> Grid -> Grid
applyMove move grid = grid V.// (map toUpdate move)

-- Returns the score of the move, if applied
-- Find all new words
-- Each new word has a score
--   The score of a new word is the sum of the scores of all letters, multiplied by any multiples
--   A multiple is applied iff one of the tiles of the move is is on the multiplier, and it applies to all new words that contain the tile

newWords :: Grid -> Move -> [Word]
newWords grid move = undefined

allWordsLine :: String -> [String]
allWordsLine = filter (\x -> length x > 1) . words

allWords :: Grid -> [String]
allWords grid = allHorizontalWords ++ allVerticalWords
  where
    allHorizontalWords = concatMap allWordsLine (getRows grid)
    allVerticalWords = concatMap allWordsLine (getCols grid)

getRows :: Grid -> [String]
getRows grid = map (map (char . (grid V.!))) allRowIxs

getCols :: Grid -> [String]
getCols grid = map (map (char . (grid V.!))) allColIxs

isWhiteSpaceAt :: [[Char]] -> (Int, Int) -> Bool
isWhiteSpaceAt grid (i, j) = (grid !! i) !! j == ' '

-- To support blank tiles, a letter has two attributes: a character and a point value
data Letter = Letter {
  char :: Char,
  points :: Int
  } deriving (Eq)

instance Show Letter where
  show = show . char
  
data Tile = Tile {
  letter :: Letter,
  position :: (Int, Int)
  }

type Move = [Tile]

type Word = [Tile]

type Grid = V.Vector Letter

type CellIxs = [Int]
type Dictionary = [String]

fromXY :: (Int, Int) -> Int
fromXY (x, y) = x * 15 + y

emptyGrid :: Grid
emptyGrid = V.replicate 225 (Letter ' ' 0)

allRowIxs, allColIxs :: [CellIxs]
allRowIxs = [getRow i | i <- [0..14]]
  where getRow n = [ fromXY (n, i) | i <- [0..14] ]
allColIxs = [getCol i | i <- [0..14]]
  where getCol n = [ fromXY (i, n) | i <- [0..14] ]

showGrid :: Grid -> String
showGrid grid = unlines . map (unwords . map (show . (grid V.!))) $ allRowIxs

replaceLetter :: Int -> Letter -> Grid -> Grid
replaceLetter i l g = g V.// [(i, l)]

toPos :: Tile -> Int
toPos (Tile _ pos) = fromXY pos

toUpdate :: Tile -> (Int, Letter)
toUpdate (Tile l pos) = (fromXY pos, l)

canPlaceAllTiles :: Grid -> Move -> Bool
canPlaceAllTiles grid = all (== ' ') . map (char . (grid V.!) . toPos)

isLegalWord :: Dictionary -> String -> Bool
isLegalWord dictionary word = word `elem` dictionary

data Multiplier = Multiplier {
  letterMult :: Int -> Int,
  wordMult :: Int -> Int
  }

instance Show Multiplier where
  show _ = ""

type Cell = (Letter, Multiplier)
type ScoredWord = [Cell]

-- Main Data Type
-- The scrabble board is represented with a 1D vector of Cells. A cell is a combination of a letter
--   and a multiplier.
-- The score of the board is a function of all letters and multipliers on the board. When a move is
--  applied, it changes the letters on the board. After a move is scored, the used multipliers are
--  removed.
-- Algorithm Flow:
--   0) Get Move M
--   1) Apply M to Board B, getting B'
--   2) If B' is valid, the score of M is score B' - score B
--   2b) If B' is invalid, try again
--   3) Filter B' to remove used multipliers
type ScrabbleBoard = V.Vector Cell

turn :: ScrabbleBoard -> Move -> (ScrabbleBoard, Int)
turn board move = case isLegalMove board move of
                    (Just boardWithMove) -> (removeMultiplier boardWithMove, scoreBoard boardWithMove)
                    Nothing              -> (board, 0)

-- Validating That A Move Is Legal
isLegalMove :: Dictionary -> ScrabbleBoard -> Move -> Maybe ScrabbleBoard
isLegalMove dictionary board move = if inALine && allTilesEmpty && allWordsLegal then Just boardWithMove else Nothing
  where
    boardWithMove = applyScrabbleMove board move
    allTilesEmpty = undefined
    inALine = undefined
    allWordsLegal = all $ map (isLegalWord dictionary) allStrings
    allWords = allWords boardWithMove
    allStrings = map scoredWordToString allWords
               
allTilesConnected :: ScrabbleBoard -> Move -> Bool
allTilesConnected board move = undefined

allTilesInALine :: Move -> Bool
allTilesInALine move = undefined

-- Scoring a legal move
scoreBoard :: ScrabbleBoard -> Int
scoreBoard = sum . map wordScore . allWords

                        
allWords :: ScrabbleWord -> [ScoredWord]
allWords board = allRowWords ++ allColWords
  where
    allRowWords = getWordsInRow board
    allColWords = getWordsInCol board

emptyBoard :: ScrabbleBoard
emptyBoard = V.replicate 225 (Letter ' ' 0, Multiplier (\x -> x) (\x -> x))

-- Sets the inital multipliers for a scrabble board
initialMultiplier :: ScrabbleBoard -> ScrabbleBoard
initialMultiplier board = board V.// []

showBoard :: ScrabbleBoard -> String
showBoard scrabble = unlines . map (unwords . map (show . fst . (scrabble V.!))) $ allRowIxs

getWordsInRow :: ScrabbleBoard -> [ScoredWord]
getWordsInRow scrabble = concatMap (scoredWords . map (scrabble V.!)) allRowIxs

getWordsInCol :: ScrabbleBoard -> [ScoredWord]
getWordsInCol scrabble = concatMap (scoredWords . map (scrabble V.!)) allColIxs

-- Applies a move by overwriting all letters in the the appropriate cells
applyScrabbleMove :: Move -> ScrabbleBoard -> ScrabbleBoard
applyScrabbleMove move board = board V.// (map toUpdate move)
  where
    toUpdate (Tile l pos) = let idx = fromXY pos in (idx, (l, snd (board V.! idx)))

-- After a move has been scored, all special tiles are no longer useable
removeMultiplier :: ScrabbleBoard -> ScrabbleBoard
removeMultiplier = V.map (\cell -> if isSpace cell then cell else removeMult cell)
  where removeMult (letter, _) = (letter, idMult)

-- Prelude.words, but for cell instead of char
scoredWords :: [Cell] -> [ScoredWord]
scoredWords [] = [[]]
scoredWords (cell:rest) = if isSpace cell
                          then [] : (scoredWords rest)
                          else let (word:words) = scoredWords rest
                               in (cell : word):words

move = [Tile (Letter 'a' 1) (1,1), Tile (Letter 'b' 3) (1,3), Tile (Letter 'c' 4) (1,4)]

idMult = Multiplier id id
m = Multiplier id id
cells = [(Letter 'a' 1, m), (Letter ' ' 2, m), (Letter 'b' 3, Multiplier (*2) (*3)), (Letter 'c' 4, m)]

-- The score of a word is each letter multiplied by any per letter multplier summed and then
-- multiplied by any per word multipliers.
wordScore :: ScoredWord -> Int
wordScore = finalScore . foldl accumScores (0, id) . map scoreCell

-- The final score of a word is the sum of the individual weights words multiplied by the word multiplier
finalScore :: (Int, Int -> Int) -> Int
finalScore (score, mult) = mult score

-- Given two scored cells comprised of the weighted letter and the multiplier for the entire word, the
--   accumulation of them is the sum of the weighted scores and the composition of the multiplying functions.
accumScores :: (Int, Int -> Int) -> (Int, Int -> Int) -> (Int, Int -> Int)
accumScores (x, f) (y, g) = (x + y, f . g)

-- The contribution of a single cell in scrabble is the score of the letter (the letter handles blank tiles)
--   multiplied by the per letter multiplier for that tile location. Additionally, the cell might have a
--   multiplier for the entire word which is carried along till the end.
scoreCell :: Cell -> (Int, Int -> Int)
scoreCell (letter, mult) = (letterScore, wordMult mult)
  where letterScore = letterMult mult . points $ letter

isSpace :: Cell -> Bool
isSpace = (==) ' ' . char . fst

scoredWordToString :: ScoredWord -> String
scoredWordToString = map (char . fst)
