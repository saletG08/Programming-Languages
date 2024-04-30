module RandomNameGenerator where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text


type StudentName = String
type HouseCounter = [(SchoolHouse, Int)]

data Student = Student{
    lastName::String,
    name::StudentName
    } deriving (Show, Eq,Ord)

data SchoolHouse = Griffindor
                    | Hufflepuff
                    | Ravenclaw 
                    | Slytherin 
                    deriving (Eq, Show, Ord, Read)

assignSchoolHouse :: Int -> HouseCounter -> IO (SchoolHouse, HouseCounter)
assignSchoolHouse maxStudentsPerHouse counters = do
    let houses = [house | (house, count) <- counters, count < maxStudentsPerHouse]
    if null houses
        then error "All houses reached their maximum capacity"
        else do
            house <- pickRandom houses
            newCounters <- updateCounter maxStudentsPerHouse house counters
            return (house, newCounters)

updateCounter :: Int -> SchoolHouse -> HouseCounter -> IO HouseCounter
updateCounter maxStudentsPerHouse house counters = do
    let newCounters = map (\(h, c) -> if h == house then (h, c+1) else (h, c)) counters
    if any (\(_, c) -> c > maxStudentsPerHouse) newCounters
        then error "The counter of a house exceeded the maximum"
        else return newCounters

studentsList :: Int -> [Student] -> HouseCounter -> IO ()
studentsList _ [] _ = return ()
studentsList maxStudentsPerHouse (x:xs) counters = do 
    (schoolHouse, newCounters) <- assignSchoolHouse maxStudentsPerHouse counters
    putStrLn $ show (x, schoolHouse)
    studentsList maxStudentsPerHouse xs newCounters

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1) 
    return (xs !! idx) 

split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y)
             where (x,y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student{lastName=lastName, name=name}

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = map parseLine (lines contents)
    return students

main :: IO()
main = do
    records <- readFileToList "list.txt"
    let numHouses = length [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
    let numStudents = length records
    let maxStudentsPerHouse = numStudents `div` numHouses + if numStudents `mod` numHouses > 0 then 1 else 0
    putStrLn $ "Total number of students: " ++ show numStudents
    putStrLn $ "Total number of groups: " ++ show numHouses
    putStrLn $ "Maximum number of students in each group: " ++ show maxStudentsPerHouse
    let initialCounters = [(Griffindor, 0), (Hufflepuff, 0), (Ravenclaw, 0), (Slytherin, 0)]
    studentsList maxStudentsPerHouse records initialCounters