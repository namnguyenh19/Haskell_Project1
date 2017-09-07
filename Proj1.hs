module Proj1 (initialGuess, nextGuess, GameState)
where

import Data.List
import Data.Char

-- type definitions

data Note = A | B | C | D | E | F | G
    deriving (Eq, Show)

type Chord = ((Note, Int), (Note, Int), (Note, Int))

-- GameState stores a list of possible notes, possible sets of Octaves, previous guesses and according answers
type GameState = ([Note], [(Int, Int, Int)], [Chord], [(Int, Int, Int)])

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- main functions

-- always guess A1, B2, C3
initialGuess :: ([String], GameState)
initialGuess = (["A1","B2","C3"],
                ([A, B, C, D, E, F, G],
                 [(1,1,1), (1,1,2), (1,1,3), (1,2,1), (1,2,2), (1,2,3), (1,3,1), (1,3,2), (1,3,3),
                  (2,1,1), (2,1,2), (2,1,3), (2,2,1), (2,2,2), (2,2,3), (2,3,1), (2,3,2), (2,3,3),
                  (3,1,1), (3,1,2), (3,1,3), (3,2,1), (3,2,2), (3,2,3), (3,3,1), (3,3,2), (3,3,3)
                  ], [], []
                 )
                )

nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (lastGuess, gs) (cPitch, cNote, cOctave) = (newGuess, newGameState)
  where
      newGameState = updateGameState gs (cPitch, cNote, cOctave) lastGuess
      nextNoteGuess = findNote newGameState (cPitch, cNote, cOctave)
      nextOctaveGuess = chooseOctave newGameState
      newGuess = generateGuess nextNoteGuess nextOctaveGuess
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- general helper functions

listToTuple :: [Int] -> (Int, Int, Int)
listToTuple [x,y,z] = (x,y,z)

tupleToList :: (Int, Int, Int) -> [Int]
tupleToList (x,y,z) = [x,y,z]

tuplePermutations :: (Int, Int, Int) -> [(Int,Int,Int)]
tuplePermutations x = map listToTuple (permutations (tupleToList x))

updateGameState :: GameState -> (Int, Int, Int) -> [String] -> GameState
updateGameState (notes, setOctaves, guesses, answers) lastAnswer lastGuess = gs'
    where
        guessAdded = addGuess (notes, setOctaves, guesses, answers) (stringToChord lastGuess)
        answerAdded = addAnswer guessAdded lastAnswer
        notesUpdated = remainNotes answerAdded  lastAnswer
        octaveUpdated = remainOctaves notesUpdated lastAnswer
        gs' = findOctave octaveUpdated lastAnswer

addAnswer :: GameState -> (Int, Int, Int) -> GameState
addAnswer (notes, setOctaves, guesses, answers) lastAnswer = (notes, setOctaves, guesses, answers ++ [lastAnswer])

addGuess :: GameState -> Chord -> GameState
addGuess (notes, setOctaves, guesses, answers) lastGuess = (notes, setOctaves, guesses ++ [lastGuess], answers)

stringToChord :: [String] -> Chord
stringToChord [c1,c2,c3] = (stringToPitch c1, stringToPitch c2, stringToPitch c3)

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

getNote :: Chord -> [Note]
getNote ((p1,_), (p2,_), (p3,_)) = [p1, p2, p3]

getOctave :: Chord -> (Int, Int, Int)
getOctave ((_,o1), (_, o2), (_, o3)) = (o1, o2, o3)

chooseOctave :: GameState -> (Int, Int, Int)
chooseOctave (_, setOctaves, _, _) = head setOctaves

remainNotes :: GameState -> (Int,Int,Int) -> GameState
remainNotes (notes, setOctaves, guesses, answers) (cPitch, cNote, cOctave)
    | correctNotes == 3 = (guessedNotes, setOctaves, guesses, answers)
    | correctNotes == 0 = (filter (`notElem` guessedNotes) notes, setOctaves, guesses, answers)
    | correctNotes == 2 && not improved && length answers > 1 = removeNote (notes, setOctaves, guesses, answers) guessedNotes
    | correctNotes == 2 && not improved && length answers <= 1 = (notes, setOctaves, guesses, answers)
    | correctNotes == 2 && improved = (notes, setOctaves, guesses, answers)
    | correctNotes == 1 && elem G guessedNotes = removeTwoNotes (notes, setOctaves, guesses, answers) guessedNotes
    | correctNotes == 1 && notElem G guessedNotes = (notes, setOctaves, guesses, answers)
    where
        correctNotes = cPitch + cNote
        guessedNotes = getNote (last guesses)
        improved = isImproved answers (cPitch, cNote, cOctave)

-- check if number of correct notes has isImproved
isImproved :: [(Int, Int, Int)] -> (Int, Int, Int) -> Bool
isImproved answers lastAnswer
    | numCorrectNote (last answers) < numCorrectNote lastAnswer = True
    | otherwise = False

removeTwoNotes :: GameState -> [Note] -> GameState
removeTwoNotes (notes, setOctaves, guesses, answers) thisGuess = (notes', setOctaves, guesses, answers)
    where
        toRemove = filter isG thisGuess
        notes' = delete (toRemove!!1) (delete (toRemove!!0) notes)

isG :: Note -> Bool
isG x
    | x == G = True
    | otherwise = False

penultimate :: [a] -> a
penultimate [x,_] = x
penultimate x = head (drop (length x - 2) x)

-- remove a guessed note from possible notes set && update GameState
removeNote :: GameState -> [Note] -> GameState
removeNote (notes, setOctaves, guesses, answers) thisGuess = (notes', setOctaves, guesses, answers)
    where
        lastGuess = getNote (penultimate guesses)
        sharedNotes = thisGuess `intersect` lastGuess
        wrongNote = head (filter (`notElem` sharedNotes) thisGuess)
        notes' = delete wrongNote notes

remainOctaves :: GameState -> (Int, Int, Int) -> GameState
remainOctaves (notes, setOctaves, guesses, answers) (cPitch, _, cOctave)
    -- only apply for the first guess
    | length answers == 1 && correctOctaves == 1 = (notes, [(1,1,1), (2,2,2), (3,3,3)] , guesses, answers)
    | length answers == 1 && correctOctaves == 2 = (notes, oneDuplicates , guesses, answers)
    -- otherwise for other guesses
    -- if all octaves are correct, keep all permutations of them
    | correctOctaves == 3 = (notes, guessedOctaves, guesses, answers)
    -- else remove all permutations of them
    | otherwise = (notes, filter (`notElem` guessedOctaves) setOctaves, guesses, answers)
    where
        correctOctaves = cPitch + cOctave
        guessedOctaves = tuplePermutations (getOctave (last guesses))
        oneDuplicates = tuplePermutations (1,1,2) ++ tuplePermutations (1,1,3)
            ++ tuplePermutations (2,2,1) ++ tuplePermutations (2,2,3)
            ++ tuplePermutations (3,3,1) ++ tuplePermutations (3,3,2)

generateGuess :: [Note] -> (Int, Int, Int) -> [String]
generateGuess [n1, n2, n3] (o1, o2, o3) = [show n1 ++ show o1, show n2 ++ show o2, show n3 ++ show o3]

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- find next set of notes to guess
findNote :: GameState -> (Int, Int, Int) -> [Note]
findNote (notes, setOctaves, guesses, answers) (cPitch, cNote, _)
    | correctNotes == 0 = newNoteSet notes
    | correctNotes == 1 = findOneNotes (notes, setOctaves, guesses, answers)
    | correctNotes == 2 = findTwoNotes (notes, setOctaves, guesses, answers)
    | correctNotes == 3 = lastNotes
    where
        lastNotes = getNote (last guesses)
        correctNotes = cPitch + cNote

--------------------------------------------------------------------------------
-- Handle case where all guessed notes are wrong

-- takes set of all possible notes
-- returns new guesses
newNoteSet :: [Note] -> [Note]
newNoteSet [] = []
newNoteSet (a:[]) = [a,a,a]
newNoteSet notes
    | numNotes == 3 = notes
    | numNotes > 3 = [notes!!0, notes!!1, notes!!2]
    | numNotes < 3 = fillNoteSet notes
    where
        numNotes = length notes

-- used only when sets of remaining notes has less than 3 elements
-- when there are less than 3 distinct notes remaining, duplicate the first one
fillNoteSet :: [Note] -> [Note]
fillNoteSet notes
    | numNotes == 2 = [notes!!0, notes!!0, notes!!1]
    | numNotes == 1 = [notes!!0, notes!!0, notes!!0]
    where
        numNotes = length notes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Handle case where only one note is right

-- takes GameState, last guessed notes
-- returns new set of notes
findOneNotes :: GameState -> [Note]
findOneNotes (notes, _, guesses, answers)
-- if this is 2nd guess, guess a new set of notes
    | length guesses == 1 = [D, E, F]
-- after 2nd guess and we have obtained better results before (have guessed 2 notes right)
    | length guesses >= 2 && isWorse = chooseGuess guesses twoCombinations  ++ findUnguessed notes guesses
-- after 2nd guess and not obtained better results, then one note has to be G
    | length guesses >= 2 && not isWorse = [G, notes'!!0, notes'!!1]
    where
        lastNotes = getNote (last guesses)
        isWorse = worseResult answers
        notes' = delete G notes
        betterGuess = findBetter guesses answers
        twoCombinations = combineGuess betterGuess lastNotes


worseResult :: [(Int, Int, Int)] -> Bool
worseResult [] = False
worseResult (a:as)
    | numCorrectNote a <= 1 = False && worseResult as
    | otherwise = True

numCorrectNote :: (Int, Int, Int) -> Int
numCorrectNote (cPitch, cNote, _) = cPitch + cNote

-- look into past better guesses to find the right pair of notes, then combine
-- with an unguessed note

-- (A,B,C) got 2 notes right, last guess is (A,B,D) got one, this function
-- try (B,C,E)

-- find an unguessed note
-- takes possible notes, past guesses
-- returns unguessed notes
findUnguessed :: [Note] -> [Chord] -> [Note]
findUnguessed [] _ = []
findUnguessed notes guesses = filter (`notElem` allUnique) notes
    where
        allGuessed = mergeGuesses guesses
        allUnique = nub allGuessed

-- find all guessed notes
mergeGuesses :: [Chord] -> [Note]
mergeGuesses [] = []
mergeGuesses (x:xs) = getNote x ++ mergeGuesses xs

-- find the first better guess
findBetter :: [Chord] -> [(Int, Int, Int)] -> [Note]
findBetter (g:gs) (a:as)
    | numCorrectNote a > 1 = getNote g
    | otherwise = findBetter gs as


-- combine past guesses
-- takes the better guess, last guess
-- return 2-note combinations not tried
combineGuess :: [Note] -> [Note] -> [[Note]]
combineGuess [x,y,z] lastGuess
    | elem x lastGuess && elem y lastGuess = [[x,z],[y,z]]
    | elem y lastGuess && elem z lastGuess = [[x,y],[x,z]]
    | elem x lastGuess && elem z lastGuess = [[x,y],[y,z]]
    | otherwise = []

-- looks into all past guesses, and possible 2-note combinations
-- return a new set of notes not tried before
chooseGuess :: [Chord] -> [[Note]] -> [Note]
chooseGuess _ [] = []
chooseGuess _ [c] = c
chooseGuess (x:xs) [c1, c2]
    | containNotes x c1 && containNotes x c2 = chooseGuess xs [c1,c2]
    | containNotes x c1 = chooseGuess xs [c2]
    | containNotes x c2 = chooseGuess xs [c1]
    | otherwise = chooseGuess xs [c1, c2]


containNotes :: Chord -> [Note] -> Bool
containNotes guess combo = elem (combo!!0) guessedNotes && elem (combo!!1) guessedNotes
    where
        guessedNotes = getNote guess
--------------------------------------------------------------------------------
-- Handle case where two notes is right

-- takes GameState, last guessed notes
-- returns new set of notes
findTwoNotes :: GameState -> [Note]
findTwoNotes (notes, _, guesses, _)
    -- match 2 notes from previous guess with an unguessed note
    | length notes' > 0 = [lastNotes!!0, lastNotes!!1, notes'!!0]
    -- otherwise a note is repeated
    | otherwise = [lastNotes!!0, lastNotes!!0, lastNotes!!1]
    where
        notes' = filter (`notElem` lastNotes) notes
        lastNotes = getNote (last guesses)

------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- find next set of Octave to guess & update GameState
-- takes current GameState, current response
-- returns updated GameState
findOctave :: GameState -> (Int, Int, Int) -> GameState
findOctave (notes, setOctaves, guesses, answers) (cPitch, _, cOctave)
    | length answers == 1 = (notes, setOctaves, guesses, answers)
    | isOneDup lastOctave = newOneDup (notes, setOctaves, guesses, answers) lastOctave correctOctaves
    | isAllDup lastOctave = newAllDup (notes, setOctaves, guesses, answers) correctOctaves
    | otherwise = (notes, tuplePermutations (1,2,3), guesses, answers)
    where
        lastOctave = getOctave (last guesses)
        correctOctaves = cPitch + cOctave

--------------------------------------------------------------------------------
-- Handle all same octave case (referred to as all duplicates)

-- form new set of octave & update GameState
-- takes current GameState & number of correct octaves in last guesses
-- returns updated GameState
newAllDup :: GameState -> Int -> GameState
newAllDup (notes, setOctaves, guesses, answers) cOctave
    | cOctave == 3 = (notes, setOctaves, guesses, answers)
    | cOctave == 0 = (notes, findAllDup setOctaves, guesses, answers)

-- check if guess has all duplicated numbers
isAllDup :: (Int, Int, Int) -> Bool
isAllDup (x,y,z)
    | x == y && x == z  = True
    | otherwise = False

-- find remaining all duplicates sets
findAllDup :: [(Int, Int, Int)] -> [(Int, Int, Int)]
findAllDup (first:rest)
    | isAllDup first = [first] ++ findAllDup rest
    | otherwise = findAllDup rest
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Handle one duplicate cases

-- form new set of one duplicate from last guess's feedback & update GameState
-- takes current GameState, last guess, number of correct octaves in last guesses
-- returns updated GameState
newOneDup :: GameState -> (Int, Int, Int) -> Int -> GameState
newOneDup (notes, _, guesses, answers) (x,y,z) cOctave
    | cOctave == 3 = (notes, [(x,y,z)], guesses, answers)
    | cOctave == 2 = (notes, setOctaves', guesses, answers)
    | cOctave == 1 = (notes, setOctaves'', guesses, answers)
    where
        setOctaves' = twoCorrectSets (x,y,z)
        setOctaves'' = oneCorrectSets (x,y,z)

-- return the index of number which is not duplicated
whichNotDup :: (Int, Int, Int) -> Int
whichNotDup (x,y,z)
    | x == y = 2
    | y == z = 0
    | x == z = 1

-- return the number not used in the guess
remainOctave :: (Int, Int, Int) -> Int
remainOctave (x,y,z)
    | x /= 1 && y /= 1 && z /= 1 = 1
    | x /= 2 && y /= 2 && z /= 2 = 2
    | x /= 3 && y /= 3 && z /= 3 = 3

-- check if guess has duplicated numbers
isOneDup :: (Int, Int, Int) -> Bool
isOneDup (x,y,z)
    | x == y && x /= z  = True
    | x == z && x /= y  = True
    | y == z && y /= x  = True
    | otherwise = False

-- generate new possible sets of octaves when number of correct octaves is 2
twoCorrectSets :: (Int, Int, Int) -> [(Int, Int, Int)]
twoCorrectSets (x,y,z)
    | nonDupIndex == 0 = tuplePermutations (y,z, numberNotPicked)
                            ++ tuplePermutations (y, x, x)
    | nonDupIndex == 1 = tuplePermutations (x,z, numberNotPicked)
                            ++ tuplePermutations (x, y, y)
    | nonDupIndex == 2 = tuplePermutations (x,y, numberNotPicked)
                            ++ tuplePermutations (x, z, z)
    where
        nonDupIndex = whichNotDup (x,y,z)
        numberNotPicked = remainOctave (x,y,z)

-- generate new possible sets of octaves when number of correct octaves is 1
oneCorrectSets :: (Int, Int, Int) -> [(Int, Int, Int)]
oneCorrectSets (x,y,z)
    | nonDupIndex == 0 = tuplePermutations (y, numberNotPicked, numberNotPicked)
                            ++ tuplePermutations (x, numberNotPicked, numberNotPicked)
    | nonDupIndex == 1 = tuplePermutations (x, numberNotPicked, numberNotPicked)
                            ++ tuplePermutations (y, numberNotPicked, numberNotPicked)
    | nonDupIndex == 2 = tuplePermutations (z, numberNotPicked, numberNotPicked)
                            ++ tuplePermutations (x, numberNotPicked, numberNotPicked)
    where
        nonDupIndex = whichNotDup (x,y,z)
        numberNotPicked = remainOctave (x,y,z)
--------------------------------------------------------------------------------
