--------------------------------------------------------------------------------
--  Nam Nguyen
--  30/08/2017
--  COMP30020 - Declarative Programming
--  Project 1 - Chord Probe, a chord guessing game


-- this file supplies the functions that make a guess based on response received
-- from the main testing controller, in order to correctly guess the chord
-- in as few guesses as possible
--------------------------------------------------------------------------------

module Proj1 (initialGuess, nextGuess, GameState)
where

import Data.List
import Data.Char

-- type definitions

-- representing possible notes
data Note = A | B | C | D | E | F | G
    deriving (Eq, Show)

-- octave is represented as {1,2,3}, Chord then is a list of (note, octave)
type Chord = [(Note, Int)]

-- GameState stores a list of possible targets
type GameState = [Chord]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- main functions

-- always guess A1, B2, C3
-- initialize GameState with all 1330 possible targets
initialGuess :: ([String], GameState)
initialGuess = (["A1","B2","C3"], allPossibleTargets)
    where
        allPossiblePitch = generatePitches [A,B,C,D,E,F,G] [1,2,3]
        allPossibleTargets = generateTargets 3 allPossiblePitch

-- at every step, remove targets inconsistent with received response
-- then choose a guess based on the number of possible targets it can eliminate
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (lastGuess, gs) response = (newGuess, gs')
    where
        gs' = removeInconsistent gs response lastGuess
        newGuess = pitchToString (findGuess gs')

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- helper functions

-- generate Pitch given a list of notes and a list of octaves
generatePitches :: [Note] -> [Int] -> [(Note, Int)]
generatePitches [] [] = []
generatePitches _ [] = []
generatePitches [] _ = []
generatePitches [x] (y:ys) = (x,y) : generatePitches [x] ys
generatePitches (x:xs) octaves = generatePitches [x] octaves ++ generatePitches xs octaves


-- generate targets given a list of pitches
generateTargets :: Int -> [(Note, Int)] -> [Chord]
-- generate all subsequences of list, then only choose sequences that have 3 elements
-- & nub to eliminate possible duplicates
generateTargets n xs = filter ((n==).length.nub) (subsequences (xs))

-- calculate response to a guess given the target
getResponse :: [String] -> Chord -> (Int, Int, Int)
getResponse guess target = (cPitch, cNote, cOctave)
    where
        guess' = nub (toChord guess)
        cPitch = length (guess' `intersect` target)
        cNote = numCorrect (getNote guess') (getNote target) - cPitch
        cOctave = numCorrect (getOctave guess') (getOctave target) - cPitch

-- function to get number of correct octave/note
numCorrect :: Eq a => [a] -> [a] -> Int
numCorrect [] [] = 0
numCorrect _ [] = 0
numCorrect [] _ = 0
numCorrect x y = 3 - length (deleteFirstsBy (==) x y)

-- get a list of notes from Chord
getNote :: Chord -> [Note]
getNote [(p1,_), (p2,_), (p3,_)] = [p1, p2, p3]

-- get a list of octaves from Chord
getOctave :: Chord -> [Int]
getOctave [(_,o1), (_, o2), (_, o3)] = [o1, o2, o3]

-- translate from String to Chord
toChord :: [String] -> Chord
toChord [c1,c2,c3] = [stringToPitch c1, stringToPitch c2, stringToPitch c3]

-- translate from String to a pitch (Note, Int)
stringToPitch :: String -> (Note, Int)
stringToPitch [x,y]
    | x == 'A' = (A, octave)
    | x == 'B' = (B, octave)
    | x == 'C' = (C, octave)
    | x == 'D' = (D, octave)
    | x == 'E' = (E, octave)
    | x == 'F' = (F, octave)
    | x == 'G' = (G, octave)
    where
        octave = digitToInt y

-- translate from Chord to String
pitchToString :: Chord -> [String]
pitchToString [(n1,o1),(n2,o2),(n3,o3)] = [show n1 ++ show o1, show n2 ++ show o2, show n3 ++ show o3]


-- remove all targets that are not consistent with the response received
removeInconsistent :: [Chord] -> (Int, Int, Int) -> [String] -> [Chord]
removeInconsistent [] _ _  = []
removeInconsistent (x:xs) response guess
    | response' /= response = removeInconsistent xs response guess
    | otherwise = x : removeInconsistent xs response guess
    where
        response' = getResponse guess x


-- get a list of responses for guess x with every possible target
listResponse :: Chord -> [Chord] -> [(Int,Int,Int)]
listResponse _ [] = []
listResponse x (y:ys) = getResponse x' y : listResponse x ys
    where
        x' = pitchToString x

-- get maximum number of targets left for a guess, given a response list
getMaxSize :: [(Int, Int, Int)] -> Int
getMaxSize x = maximum sizes
    where
        -- group list by sorted answer
        x' = groupBy (==) (sort x)
        sizes = map length x'

-- go through list of possible targets and compute list of responses for every guess
allListResponse :: [Chord] -> [Chord] -> [[(Int,Int,Int)]]
allListResponse [] [] = []
allListResponse _ [] = []
allListResponse [] _ = []
allListResponse allTargets (x:xs) = listResponse x allTargets : allListResponse allTargets xs

-- find the guess that eliminates the most targets by choosing guess that has
-- smallest number of max targets remaining
-- maxTargetLeft :: [[(Int,Int,Int)]] -> [Int]
-- maxTargetLeft x = map getMaxSize x
findGuess :: [Chord] -> Chord
findGuess x = x!!index
    where
        responseList = allListResponse x x
        maxTargetList = map getMaxSize responseList
        minNum = minimum maxTargetList
        index = head (elemIndices minNum maxTargetList)

--------------------------------------------------------------------------------
