module Main where

import Control.Monad.Random
import Data.List
import System.Environment

data Format = Lines | Plain | OneLiner
  deriving (Eq, Read, Show)

type Padding = Int
type Solution = Bool
type Rank = Bool

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n l
  | n > 0 = (take n l) : (groups n (drop n l))
  | otherwise = error "Negative or zero n"

strRowsD :: String -> [Int] -> [[String]]
strRowsD d o = groups 9 $ map (\c -> if c > 0 then show c else d) o

strRows :: [Int] -> [[String]]
strRows o = strRowsD " " o

strRows_ :: [Int] -> [[String]]
strRows_ o = strRowsD "_" o

format :: Format -> Padding -> [Int] -> String
format OneLiner _ o = join . join . strRows_ $ o
format f        p o
  | f == Plain      = join $ intersperse (vpad ++ "\n") $ map (\r -> join $ intersperse hpad r) $ strRows_ o
  | otherwise       = join $ intersperse "\n" $ (if p > 1 then intersperse inbetweenPad else id) $ borders $ rows $ strRows o
  where
    vpad = replicate (p `div` 2) '\n'
    hpad = replicate p ' '
    borders rs = [topcap] ++ inbetweens rs ++ [bottomcap]
    rows rs = map (\r -> line '┃' '│' '┃' ' ' '┃' r) rs
    inbetweenThin = line '┠' '┼' '╂' '─' '┨' $ row9 '─'
    inbetweenThick = line '┣' '┿' '╋' '━' '┫' $ row9 '━'
    inbetweenPad = join $ intersperse "\n" $ replicate (p `div` 2) $ line '┃' '│' '┃' ' ' '┃' $ row9 ' '
    inbetweens rs = join $ intersperse [inbetweenThick] $ groups 5 $ join . (map (\r3 -> intersperse inbetweenThin r3)) $ groups 3 rs
    topcap = line '┏' '┯' '┳' '━' '┓' $ row9 '━'
    bottomcap = line '┗' '┷' '┻' '━' '┛' $ row9 '━'
    line ll cl cb m rl r = [ll] ++ (join $ intersperse [cb] $ map (\r3 -> join $ intersperse [cl] $ map (\c -> replicate p m ++ c ++ replicate p m) r3) $ groups 3 r) ++ [rl]
    row9 c = replicate 9 [c]

formatArg :: [String] -> Format
formatArg args
  | "plain" `elem` args     = Plain
  | "oneliner" `elem` args  = OneLiner
  | otherwise               = Lines

paddingArg :: [String] -> Padding
paddingArg args = if p > -1 then p else 1
  where
    p = findmax $ map (\a -> case reads a of
        [(val, "")] -> val
        _           -> (-1)
      ) args

solutionArg :: [String] -> Solution
solutionArg args
  | "solution" `elem` args  = True
  | otherwise               = False

rankArg :: [String] -> Rank
rankArg args
  | "rank" `elem` args  = True
  | otherwise           = False

findmax :: [Int] -> Int
findmax xs = foldl (\a x -> max a x) (-1) xs

placeable :: [Int] -> Int -> Int -> Int -> Bool
placeable sudoku x y n = not $ n `elem` r || n `elem` c || n `elem` q
  where
    r = row y sudoku
    c = column x sudoku
    q = quad (x `div` 3) (y `div` 3) sudoku

conform :: [Int] -> [Int] -> [Int]
conform []     sudoku = sudoku -- not used because the random number list is infinite
conform (n:ns) sudoku
  | length sudoku == 81 = sudoku
  | otherwise           = if placeable sudoku x y n then append else skip
                            where
                              x = (length sudoku) `mod` 9
                              y = (length sudoku) `div` 9
                              append = conform ns $ sudoku ++ [n]
                              skip = if candidates then continue else reset
                              candidates = any (placeable sudoku x y) [1..9]
                              continue = conform ns sudoku
                              reset = conform ns []

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
quad x y o = join $ map (\r -> take 3 $ drop (x*3) $ row (y*3 + r) o) [0..2]

problematize :: [Int] -> [Int] -> [Int]
problematize []   sudoku = sudoku
problematize (u:us) sudoku = if length (solve problematized) > 1 then sudoku else problematize us problematized
  where
    problematized = replace sudoku u 0

solve :: [Int] -> [[Int]]
solve []     = []
solve sudoku
  | not $ 0 `elem` sudoku = [sudoku]
  | otherwise = join $ map solve potential
    where
      potential = case elemIndex 0 sudoku of
        Nothing -> [sudoku]
        Just n  -> map (\p -> replace sudoku n p) [ p |
          p <- [1..9],
          placeable sudoku x y p
          ]
          where
            x = n `mod` 9
            y = n `div` 9

solved :: [Int] -> Bool
solved sudoku = case elemIndex 0 sudoku of -- = elem 0 sudoku -- does not work?
  Nothing -> True
  Just _  -> False

replace :: [Int] -> Int -> Int -> [Int]
replace l i n = take i l ++ [n] ++ drop (i + 1) l

main :: IO ()
main = do
  -- arguments
  args <- getArgs
  let layout = formatArg args
  let solution = solutionArg args
  let padding = paddingArg args
  let rank = rankArg args
  -- generate sudoku
  r9 <- getRandomRs (1, 9)
  let sudoku = conform r9 []
  r81 <- getRandomRs (0, 80)
  let u81 = nub r81
  let problem = problematize u81 sudoku
  let holes = length $ filter (\x -> x == 0) problem
  let numbers = length $ filter (\x -> x /= 0) problem
  -- pretty print
  -- sudoku
  putStr $ format layout padding problem
  if solution then do
    -- solution
    putStr $ terminator layout
    putStr $ format layout padding sudoku
  else
    return ()
  if rank then do
    -- rank
    putStr $ terminator layout
    putStr $ if layout /= OneLiner then replicate holes '▓' ++ replicate numbers '░' else show numbers
  else
    return ()
  putStr "\n"

  -- putStr "Ooh-λa-λa!\n"
  if numbers < 30 then return () else main -- loop it, baby

terminator :: Format -> String
terminator f = case f of
    OneLiner  -> ","
    Plain     -> "\n\n"
    Lines     -> "\n"