module Main where

import System.Random
import Data.List
import Control.Monad

main :: IO ()
main = do
  r99 <- random99
  putStr . show $ r99

randomDigits :: IO [Int]
randomDigits = do
  gen <- newStdGen
  return $ randomRs (1,9) gen

random9 :: IO [Int]
random9 = do
  digits <- randomDigits
  return $ take 9 $ nub digits

random99 :: IO [[Int]]
random99 = replicate9 9 random9

replicate9 :: Int -> IO [Int] -> IO [[Int]]
replicate9 0 _ = return []
replicate9 x y = do
  r9 <- random9
  (:) r9 <$> replicate9 (x - 1) y

-- prettyPrint :: IO [[Int]] -> IO ()
-- prettyPrint p = do
--   map (\l -> fmap (\d -> print d) l) p
