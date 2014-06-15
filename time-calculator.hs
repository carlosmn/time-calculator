-- Copyright (c) 2014 Carlos Martïn Nieto <cmn@dwim.me>

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

import System.IO (hFlush, stdout)

type Time = (Integer, Integer)

-- Make sure we have hhmm or hh:mm format
validateInput :: [Char] -> Maybe [Char]
validateInput xs
  | length xs == 4 = Just xs
  | length xs == 5 = Just xs
  | otherwise      = Nothing

validateTime :: Time -> Maybe Time
validateTime time@(hours, minutes)
  | elem hours [0..23] &&
    elem minutes [0..59]  = Just time
  | otherwise             = Nothing

parseMinutes :: [Char] -> Integer
parseMinutes (':':xs) = parseMinutes xs
parseMinutes m@(m1:m2:[]) = read m

parseTime' :: [Char] -> Maybe Time
parseTime' (h1:h2:xs) = validateTime (hours, parseMinutes xs)
  where
    hours = read [h1, h2] :: Integer

parseTime :: [Char] -> Maybe Time
parseTime t = validateInput t >>= parseTime'

-- how many minutes into the day we are
toMinutes (hours, minutes) = hours * 60 + minutes

-- |Difference between two times. If the second time is smaller than
-- the first, it is considered to lie in the next day
timeDifference :: Time -> Time -> Integer
timeDifference t1 t2@(h2, m2)
  | ts2 < ts1 = timeDifference t1 (h2 + 24, m2)
  | otherwise = ts2 - ts1
  where
    ts2 = toMinutes t2
    ts1 = toMinutes t1

-- |Split a time difference from minutes to hours + minutes
toTime :: Integer -> Time
toTime t = divMod t 60

reportElapsed' (h, m) = "Time elapsed: " ++ show h ++ "h" ++ show m ++ "m"
reportElapsed t1 t2 = reportElapsed' $ toTime elapsed
  where
    elapsed = timeDifference t1 t2

printReport (Just s) = s
printReport Nothing = "There was some invalid input"

-- fmap for two args
fmap2 f (Just a) (Just b) = Just (f a b)
fmap2 _ _ _               = Nothing

getInput :: String -> IO String
getInput s = do
  putStr s
  hFlush stdout
  getLine

main :: IO ()
main = do
  s1 <- getInput "> "
  let t1 = parseTime s1
  s2 <- getInput "> "
  let t2 = parseTime s2
  putStrLn $ printReport $ fmap2 reportElapsed t1 t2
  main
