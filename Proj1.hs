module Proj1 (initialGuess, nextGuess, GameState)
where

import Data.List
import Data.Char

-- type definitions

data Note = A | B | C | D | E | F | G
    deriving (Eq, Show)

data Octave = 1 | 2 | 3
    deriving (Eq, Show)

type Pitch = (Note,Octave)
    deriving (Eq, Show)

type Chord = [Pitch]
    deriving (Eq, Show)

type Answer = [Int]
    deriving (Eq, Ord, Show)

-- GameState stores a list of target Pitch correctly guessed & previous guess
data GameState = ([Pitch], Chord)

-- helper functions

getNote :: String -> Note
getNote x = x!!0

getOctave :: String -> Octave
getOctave x = x!!1

-- main functions

-- always guess A1, B2, C3
initialGuess :: ([String], GameState)
initialGuess = (["A1","B2","C3"], ([],[]))

nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess ([g1, g2, g3], (targets, lastguess)) (cPitch, cNote, cOctave) =
    
