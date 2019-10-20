module Main where

import Data.List
import Control.Monad.Random



random9 :: IO [Int]
random9 = liftM (take 9 . nub) $ getRandomRs (1, 9)

random81 :: IO [[Int]]
random81 = sequence (replicate 9 random9)

prettyPrint :: [[Int]] -> IO ()
prettyPrint s = sequence (map print s) >> return ()

main :: IO ()
main = do
  r81 <- random81
  prettyPrint $ conform $ index r81


conform :: [[(Int, Int)]] -> [[Int]]
conform o = [ [ q | c <- r,
                    let n = snd c,
                    let i = fst c,
                    let q = n * 1000 + i,
                    let x = i `mod` 9,
                    let y = i `div` 9,
                    let row = map (\c -> snd c) $ o !! y,
                    let col = column x o

                    ] | r <- o ]
                    -- i <- r
                    -- ]]
column :: Int -> [[(Int, Int)]] -> [Int]
column _ [] = []
column c o  = map (\r -> snd $ r !! c) o


index :: [[Int]] -> [[(Int, Int)]]
index o =  map (\r -> map (\i -> (i, o !! (i `div` 9) !! (i `mod` 8))) [(0 + r*9 :: Int)..(8 + r*9 :: Int)]) [0..8]

-- conform :: [[Int]] -> [[Int]] -> [[Int]]
-- conform []     o = o
-- conform (r:rs) o = conform rs $ o ++ [sortRow r o]

-- column :: Int -> [[Int]] -> [Int]
-- column _ [] = []
-- column c o  = map (\r -> r !! c) o

-- sortRow :: [Int] -> [[Int]] -> [Int]
-- sortRow r [] = map (\i -> sortCell i r [] []) [0..8]
-- sortRow r o  = map (\(i, c) -> sortCell i r c o) columns
--   where
--   columns = map (\n -> (n, column n o)) [0..8]

-- sortCell :: Int -> [Int] -> [Int] -> [[Int]] -> Int
-- sortCell i r c  _  = r !! i * 1000000 + div (sum c) 1000000





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