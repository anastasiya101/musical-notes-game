--  Author   : Anastasiya Bogoslovskaya <abogoslovska@student.unimelb.edu.au>
--  Purpose  : Implement a two player game where one person the 'composer'
--             selects a a three-pitch musical chord which becomes our target
--             and one is the 'performer'who repeatedly chooses a chord as a 
--             guess and recieves feedback from the 'composer' on three 
--             criteria how close his guess is to the target chord.

-- A pitch [A1] comprises of a note and an octave.

-- note: A, B, C, D, E, F, or G, 
-- octave: 1, 2, or 3. 

-- A chord [A1, B2, C3] comprises of 3 pitches

-- The criteria :
-- 1. how many pitches in the guess are included in the target 
--    aka. correct pitches

-- 2. how many pitches have the right note but the wrong octave 
--    aka. correct notes

-- 3. how many pitches have the right octave but the wrong note 
--    aka. correct octaves

-- Some things to note
-- 1. The order of pitches in the target is irrelevant. 
-- 2. No pitch may appear more than once. 
-- 3. No more or less than three notes may be included in the target.
-- 4. Multiple occurrences in the guess are only counted as correct if they 
--    also appear repeatedly in the target.
-- 5. If the pitch is correct its note and octave are not recorded in the other
--    feedback criteria.

-- The approach was to create a list of potential matches to the target.
-- From this list selecting the match with the lowest number of posibilities 
-- that give the target thus getting the next guess we can use feedback most
-- effectively to get the solution.

 
module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where

-- list library which allows me to manipulate lists with inbuilt functions
-- like grouping, sorting and creating sequences
import Data.List
import Data.Ord (comparing)

-- my pitch is defined as two character values, 
-- first one for the note and second one for the octave

data Pitch = Pitch Char Char
    deriving Eq

instance Show Pitch where
    show (Pitch note octave) = [note, octave]

-- chord consists of a list of three pitches
type Chord = [Pitch]

-- feedback is a three integer value 
-- which gives us the information about our guesss
type Feedback = (Int, Int, Int)

-- a list of pitches that could be a potential match
type GameState = [[Pitch]] 

-- getNote : takes the pitch and returns the note component
getNote :: Pitch -> Char
getNote (Pitch note octave) = note

-- getOctave : takes the pitch and returns the octave component
getOctave :: Pitch -> Char
getOctave (Pitch note octave) = octave

-------

-- toPitch : takes a string and returns a pitch if it's a valid pitch as 
-- described above, otherwise the string is not a valid pitch type 
-- and we return nothing

-------

toPitch :: String -> Maybe Pitch
toPitch [note, octave]
    | elem note "ABCDEFG" && elem octave "123" = Just $ Pitch note octave
toPitch _ = Nothing

-------

-- feedback : takes the target Pitch and our guess and 
-- returns how we match the required criteria in the form :-
-- (no. of correct pitches, no. of correct notes, no. of correct octaves)
-- we do this by comparing the guess to the target

-------

feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback targetPitch guessPitch = (correctPitches, correctNotes, correctOctaves)
    where
        len = length guessPitch
        correctPitches = len - length (targetPitch \\ guessPitch)
        correctNotes = len - correctPitches 
            - length(map getNote targetPitch \\ map getNote guessPitch)
        correctOctaves = len - correctPitches 
            - length(map getOctave targetPitch \\ map getOctave guessPitch)

-------

-- initialGuess : doesn't take any inputs but returns our first/initial guess 
-- and a list of all the possible solutions
-- the first guess is [A1, B2, C2]
-- after running test and drawing differen't paths and scenarios of the
-- pitches, i came to a conclusion it was best to use three different notes 
-- and two different octaves to make feedback as effective as possible and
-- eliminate the most amount of pitches faster.

-------

initialGuess :: ([Pitch], GameState)
initialGuess = (firstGuess, startState)
    where 
    firstGuess = ([Pitch 'A' '1', Pitch 'B' '2', Pitch 'C' '2'])
    pitches = [Pitch note octave 
        | note <- ['A', 'B', 'C', 'D', 'E', 'F', 'G'], octave <- ['1', '2', '3']]
    startState = [chord 
        | chord <- subsequences pitches, length chord == 3]
    

-------

-- nextGuess : takes the guess, our list of potential matches and the feedback 
-- returns our next guess and and updated list of potential matches
-- which includes only the possibilties that gave our feedback
-- first we 

-------

nextGuess :: ([Pitch], GameState) -> (Int,Int,Int) -> ([Pitch], GameState)
nextGuess (myGuess, currState) myFeedback = (newGuess, newState)
    where
        newState = delete myGuess [ chord 
                    | chord <- currState, feedback chord myGuess == myFeedback ]
        remainingGuesses = [ (state, potGuess)
                    | potGuess <- newState
                    , let state = score potGuess (newState \\ [potGuess])]
                    -- a1 -> a1 -> Ordering
        newGuess = snd $ head $ sortBy (comparing (\(x,y)->x)) remainingGuesses
       
               
-------  

-- score : takes a potential match and the current game state
-- and generates a score for a guess by counting the number of targets that 
-- would give that answer. 

-------

score :: [Pitch] -> GameState -> Double
score myGuess currState = sum [(outcomes / totalOutcomes) * outcomes
                               | myGroup <- groups
                               , let outcomes = fromIntegral $ length myGroup]
                           where
                               myScores = [scores | newGuess <- currState, let scores = feedback myGuess newGuess]
                               groups = group $ sort myScores
                               totalOutcomes = fromIntegral $ length myScores
      