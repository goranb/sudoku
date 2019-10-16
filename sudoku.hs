module Main where

import Data.List
import Control.Monad.Random

main :: IO ()
main = random9x9 >>= print

random9 :: IO [Int]
random9 = liftM (take 9 . nub) $ getRandomRs (1, 9)

random9x9 :: IO [[Int]]
random9x9 = sequence (replicate 9 random9)
