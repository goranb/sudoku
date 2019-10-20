module Main where

import Control.Monad.Random

random9 :: IO [Int]
random9 = getRandomRs (1, 9)

grp :: Int -> [a] -> [[a]]
grp _ [] = []
grp n l
  | n > 0 = (take n l) : (grp n (drop n l))
  | otherwise = error "Negative or zero n"

prettyPrint :: [Int] -> IO ()
prettyPrint s = sequence (map print $ grp 9 s) >> return ()

main :: IO ()
main = do
  r9 <- random9
  let con = conform 0 r9 []
  prettyPrint con

conform :: Int -> [Int] -> [Int] -> [Int]
conform 81 _      o = o
conform i  (s:ss) o = if s `elem` r || s `elem` c || s `elem` q then skip else append
                      where
                      x = i `mod` 9
                      y = i `div` 9
                      r = row y o
                      c = column x o
                      q = quad (x `div` 3) (y `div` 3) o
                      candidates = any (\z -> not (z `elem` r || z `elem` c || z `elem` q)) [1..9]
                      continue = conform i ss o
                      reset = conform 0 ss []
                      skip = if candidates then continue else reset
                      append = conform (i+1) ss $ o ++ [s]
conform _  _     o = o -- exhaustive pattern matching

row :: Int -> [a] -> [a]
row _ [] = []
row r o  = take 9 $ drop (r * 9) o

column :: Int -> [a] -> [a]
column _ [] = []
column c o = every 9 $ drop c o

every :: Int -> [a] -> [a]
every _ [] = []
every n as  = head as : every n (drop n as)

quad :: Int -> Int -> [a] -> [a]
quad x y o = q1 ++ q2 ++ q3
  where
  q1 = take 3 $ drop (x*3) $ row (y*3) o
  q2 = take 3 $ drop (x*3) $ row (y*3 + 1) o
  q3 = take 3 $ drop (x*3) $ row (y*3 + 2) o
