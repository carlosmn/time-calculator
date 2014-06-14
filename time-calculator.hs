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
  | otherwise    = Nothing

validateTime :: Time -> Maybe Time
validateTime time@(hours, minutes)
  | elem hours [0..23] &&
    elem minutes [0..59]  = Just time
  | otherwise             = Nothing

parseMinutes :: [Char] -> Integer
parseMinutes (':':m1:m2:[]) = parseMinutes [m1, m2]
parseMinutes (m1:m2:[]) = read [m1, m2]

parseTime' :: [Char] -> Maybe Time
parseTime' (h1:h2:xs) = validateTime (hours, parseMinutes xs)
  where
    hours = read [h1, h2] :: Integer

parseTime :: [Char] -> Maybe Time
parseTime t = validateInput t >>= parseTime'

-- how many minutes into the day we are
toMinutes (hours, minutes) = hours * 60 + minutes

oneDay = toMinutes (24, 0)

timeDifference' t1 t2
  | t2 < t1 = timeDifference' t1 (t2 + oneDay)
  | otherwise = t2 - t1

-- |Difference between two times. If the second time is smaller than
-- the first, it is considered to lie in the next day
timeDifference :: Maybe Time -> Maybe Time -> Maybe Integer
timeDifference (Just t1) (Just t2) = Just (timeDifference' ts1 ts2)
  where
    ts2 = toMinutes t2
    ts1 = toMinutes t1
timeDifference _ _ = Nothing

-- |Split a time difference from minutes to hours + minutes
toTime :: Integer -> Time
toTime t = divMod t 60

reportElapsed' (h, m) = "Time elapsed: " ++ show h ++ "h" ++ show m ++ "m"
reportElapsed t1 t2 = fmap (reportElapsed' . toTime) elapsed
  where
    elapsed = timeDifference t1 t2

printReport (Just s) = s
printReport Nothing = "There was some invalid input"

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
  putStrLn $ printReport $ t1 >> t2 >> reportElapsed t1 t2
  main
