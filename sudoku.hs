module Main where

-- import Data.List

import Control.Monad.Random

random9 :: IO [Int]
random9 = getRandomRs (1, 9)

-- random9 :: IO [Int]
-- random9 = liftM (take 9 . nub) $ getRandomRs (1, 9)

-- random81 :: IO [[Int]]
-- random81 = sequence (replicate 9 random9)

prettyPrint :: [[Int]] -> IO ()
prettyPrint s = sequence (map print s) >> return ()

main :: IO ()
main = do
  r9 <- random9
  let con = conform 0 0 r9 []
  prettyPrint $ group 9 $ con
  -- print $ column 8 con
  -- print $ row 8 con
  -- print $ quadrant 2 2 con


conform :: Int -> Int -> [Int] -> [Int] -> [Int]
conform _     81 _      o = o
conform 1000000 _  _      o = o
conform z     i  (s:ss) o = if s `elem` r || s `elem` c || s `elem` q then conform (z+1) ii ss ooo
                      else conform (z+1) (i+1) ss oo
                      where
                        x = i `mod` 9
                        y = i `div` 9
                        r = row y o
                        c = column x o
                        q = quadrant (x `div` 3) (y `div` 3) o -- it's not a `quadrant` :P
                        oo = o ++ [s]
                        ooo = if z `mod` 1000 == 0 then [] else o
                        ii = if z `mod` 1000 == 0 then 0 else i


                     -- if (s `elem` (row (i)))

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

row :: Int -> [a] -> [a]
row _ [] = []
row r o  = take 9 $ drop (r * 9) o

column :: Int -> [a] -> [a]
column _ [] = []
column c o = every 9 $ drop c o

every :: Int -> [a] -> [a]
every n [] = []
every n as  = head as : every n (drop n as)

quadrant :: Int -> Int -> [a] -> [a]
quadrant x y o = q1 ++ q2 ++ q3
  where
  q1 = take 3 $ drop (x*3) $ row (y*3) o
  q2 = take 3 $ drop (x*3) $ row (y*3 + 1) o
  q3 = take 3 $ drop (x*3) $ row (y*3 + 2) o

-- every n = everyf n . drop (n-1)


-- index :: [[Int]] -> [[(Int, Int)]]
-- index o =  map (\r -> map (\i -> (i, o !! (i `div` 9) !! (i `mod` 8))) [(0 + r*9 :: Int)..(8 + r*9 :: Int)]) [0..8]

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