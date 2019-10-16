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
  s <- random81
  if conforms s then
    prettyPrint s
  else do
    -- print $ nub $ map (\x -> (s !! x) !! 0) [0..8]
    -- prettyPrint s
    -- putChar '.'
    main

conforms :: [[Int]] -> Bool
conforms s = map nub ff == ff
  where
    f c = map (\x -> (s !! x) !! c) [0..8]
    ff = map f [0..8]



