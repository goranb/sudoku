module Main where

import Data.List
import Control.Monad.Random

random9 :: IO [Int]
random9 = liftM (take 9 . nub) $ getRandomRs (1, 9)

random81 :: IO [[Int]]
random81 = sequence (replicate 9 random9)

-- prettyPrint :: [[Int]] -> IO ()
-- prettyPrint s = sequence (map print s) >> return ()

main :: IO ()
main = do
  r81 <- random81
  print $ conform r81

conform :: [[Int]] -> [[Int]] -> [[Int]]
conform []     o = o
conform (r:rs) o = conform rs $ o : sortRow r o

column :: Int -> [[Int]] -> [Int]
column c [] = []
column c o  = map (\r -> r !! c) o

sortRow :: [Int] -> [[Int]] -> [Int]
sortRow r [] = i
sortRow r o  = map (\c -> ) columns
  where
  columns = map (\x -> column x o) [1..9]



sort_ rest column




  -- print $ place r9 [[],[],[],[],[],[],[],[],[]]


  -- s <- random81
  -- if conforms s then
  --   prettyPrint s
  -- else do
  --   main


-- conforms :: [[Int]] -> Bool
-- conforms s = map nub ff == ff
--   where
--     f c = map (\x -> (s !! x) !! c) [0..8]
--     ff = map f [0..8]


-- place :: Int -> [[Int]] -> [[Int]]
-- place num grid = newGrid where
--   index = findIndex (\row -> length row < 9) grid

-- getRow :: Maybe Int -> [[Int]] -> [Int]
-- getRow (Just num) grid = grid !! num

-- placeInRow :: Maybe Int -> [Int] -> [Int]
-- placeInRow (Just num) row =
--   if num `elem` row then row
--   else row ++ [num]
-- placeInRow Nothing row = row

