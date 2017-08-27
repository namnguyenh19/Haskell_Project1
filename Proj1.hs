module Proj1 (initialGuess, nextGuess, GameState)
where

import Data.List
import Data.Char

-- type definitions

data Note = A | B | C | D | E | F | G
    deriving (Eq, Show)

type Pitch = (Note, Int)

type Chord = [Pitch]

type Answer = [Int]

-- GameState stores a list of target Pitch correctly guessed & previous guess
type GameState = ([Pitch], [Chord], [Answer])

-- helper functions

getNote :: String -> Note
getNote = head

getOctave :: String -> Octave
getOctave x = x!!1

remainNotes :: 

-- main functions

-- always guess A1, B2, C3
initialGuess :: ([String], GameState)
initialGuess = (["A1","B2","C3"], [(A,1), (B,2), (C,3)], [] )

nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess ([g1, g2, g3], (foundTargets, guesses, results) (cPitch, cNote, cOctave)
  | cPitch == 0 && cNote == 0 && cOctave == 0
