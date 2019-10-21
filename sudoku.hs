module Main where

import Control.Monad.Random
import Data.List
import System.Environment

data Format = Lines | Plain | OneLiner
  deriving (Eq)
type Padding = Int

random9 :: IO [Int]
random9 = getRandomRs (1, 9)

grp :: Int -> [String] -> [[String]]
grp _ [] = []
grp n l
  | n > 0 = (take n l) : (grp n (drop n l))
  | otherwise = error "Negative or zero n"

strRows :: [Int] -> [[String]]
strRows o = grp 9 $ map (\c -> show c) o

format :: Format -> Padding -> [Int] -> String
format OneLiner _ o = join . join . strRows $ o
format f        p o
  | f == Plain      = join $ intersperse (vpad ++ "\n") $ map (\r -> join $ intersperse hpad r) $ strRows o
  | otherwise       = join $ intersperse "\n" $ (if p > 1 then intersperse inbetweenPad else id) $ borders $ rows $ strRows o
  where
    vpad = replicate (p `div` 2) '\n'
    hpad = replicate p ' '
    borders rs = [topcap] ++ inbetweens rs ++ [bottomcap]
    rows rs = map (\r -> rline '┃' '│' '┃' ' ' '┃' r) rs
    inbetweenThin = rline '┃' '┼' '╂' '─' '┃' $ line '─'
    inbetweenThick = rline '┣' '┿' '╋' '━' '┫' $ line '━'
    inbetweenPad = join $ intersperse "\n" $ replicate (p `div` 2) $ rline '┃' '│' '┃' ' ' '┃' $ line ' '
    inbetweens rs = join $ intersperse [inbetweenThick] $ grp 5 $ join . (map (\r3 -> intersperse inbetweenThin r3)) $ grp 3 rs
    topcap = rline '┏' '┯' '┳' '━' '┓' $ line '━'
    bottomcap = rline '┗' '┷' '┻' '━' '┛' $ line '━'
    rline ll cl cb m rl r = [ll] ++ (join $ intersperse [cb] $ map (\r3 -> join $ intersperse [cl] $ map (\c -> replicate p m ++ c ++ replicate p m) r3) $ grp 3 r) ++ [rl]
    line c = replicate 9 [c]

main :: IO ()
main = do
  args <- getArgs
  r9 <- random9
  let con = conform r9 []
  let f = formatArg args
  let p = paddingArg args
  putStr $ format f p con
  putStr "\n"

formatArg :: [String] -> Format
formatArg args
  | "plain" `elem` args     = Plain
  | "oneliner" `elem` args  = OneLiner
  | otherwise               = Lines

paddingArg :: [String] -> Padding
paddingArg args = findmax $ map (\a -> do
  case readMaybe a :: Maybe Int of
    Just i -> i
    Nothing -> 0
  ) args

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
              [(val, "")] -> Just val
              _           -> Nothing

findmax :: [Int] -> Int
findmax xs = foldl (\a x -> max a x) 0 xs


conform :: [Int] -> [Int] -> [Int]
conform [] o = o -- not used because the random number list is infinite
conform (s:ss) o
  | length o == 81  = o
  | otherwise       = if placeable s then append else skip
                      where
                        x = (length o) `mod` 9
                        y = (length o) `div` 9
                        r = row y o
                        c = column x o
                        q = quad (x `div` 3) (y `div` 3) o
                        placeable z = not $ z `elem` r || z `elem` c || z `elem` q
                        append = conform ss $ o ++ [s]
                        skip = if candidates then continue else reset
                        candidates = any placeable [1..9]
                        continue = conform ss o
                        reset = conform ss []

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
