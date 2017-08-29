module Proj1 (initialGuess, nextGuess, GameState)
where

import Data.List
import Data.Char

-- type definitions

data Note = A | B | C | D | E | F | G
    deriving (Eq, Show)

type Pitch = (Note, Int)

type Chord = (Pitch, Pitch, Pitch)

type Answer = (Int, Int, Int)

-- GameState stores a list of possible notes, possible sets of Octaves, previous guesses and according answers
type GameState = ([Note], [(Int, Int, Int)], [Chord], [Answer])

-- helper functions

listToTuple :: [Int] -> (Int, Int, Int)
listToTuple [x,y,z] = (x,y,z)

tupleToList :: (Int, Int, Int) -> [Int]
tupleToList (x,y,z) = [x,y,z]

tuplePermutations :: (Int, Int, Int) -> [(Int,Int,Int)]
tuplePermutations x = map listToTuple (permutations (tupleToList x))

getNote :: Chord -> [Note]
getNote ((p1,o1), (p2, o2), (p3, o3)) = [p1, p2, p3]

getOctave :: Chord -> (Int, Int, Int)
getOctave ((p1,o1), (p2, o2), (p3, o3)) = (o1, o2, o3)

remainNotes :: GameState -> (Int,Int,Int) -> GameState
remainNotes (notes, setOctaves, guesses, answers) (cPitch, cNote, cOctave)
    | correctNotes == 3 = (guessedNotes, setOctaves, guesses, answers)
    | correctNotes == 0 = (filter (`notElem` guessedNotes) notes, setOctaves, guesses, answers)
    | correctNotes == 1 && null answers = 
    where
        correctNotes = cPitch + cNote
        guessedNotes = getNote (last guesses)

remainOctaves :: GameState -> (Int, Int, Int) -> GameState
remainOctaves (notes, setOctaves, guesses, answers) (cPitch, cNote, cOctave)
    | correctOctaves == 3 = (notes, guessedOctaves, guesses, answers)
    | correctOctaves == 0 = (notes, filter (`notElem` guessedOctaves) setOctaves, guesses, answers)
    -- only apply for the first guess
    | correctOctaves == 1 &&  null answers = (notes, [(1,1,1), (2,2,2), (3,3,3)] , guesses, answers)
    | correctOctaves == 2 &&  null answers = (notes, oneDuplicates , guesses, answers)
    where
        correctOctaves = cPitch + cOctave
        guessedOctaves = tuplePermutations (getOctave (last guesses))
        oneDuplicates = tuplePermutations (1,1,2) ++ tuplePermutations (1,1,3)
            ++ tuplePermutations (2,2,1) ++ tuplePermutations (2,2,3)
            ++ tuplePermutations (3,3,1) ++ tuplePermutations (3,3,2)


-- main functions

-- always guess A1, B2, C3
initialGuess :: ([String], GameState)
initialGuess = (["A1","B2","C3"],
                ([A, B, C, D, E, F, G],
                 [(1,1,1), (1,1,2), (1,1,3), (1,2,1), (1,2,2), (1,2,3), (1,3,1), (1,3,2), (1,3,3),
                  (2,1,1), (2,1,2), (2,1,3), (2,2,1), (2,2,2), (2,2,3), (2,3,1), (2,3,2), (2,3,3),
                  (3,1,1), (3,1,2), (3,1,3), (3,2,1), (3,2,2), (3,2,3), (3,3,1), (3,3,2), (3,3,3)
                  ], [((A,1), (B,2), (C,3))], []
                 )
                )

-- nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
-- nextGuess ([g1, g2, g3], (foundTargets, guesses, results) (cPitch, cNote, cOctave)
--   | cPitch == 0 && cNote == 0 && cOctave == 0
