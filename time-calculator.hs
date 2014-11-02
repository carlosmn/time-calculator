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
import Text.Printf (printf)

type Time = (Integer, Integer)

-- |Make sure we have hhmm or hh:mm format
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

parseDigits num@(a:b:[])
  | isDigit a && isDigit b = Just (read num)
  | otherwise              = Nothing

isDigit x = elem x ['0'..'9']

parseMinutes :: [Char] -> Maybe Integer
parseMinutes (':':xs) = parseMinutes xs
parseMinutes xs       = parseDigits xs

parseTime' :: [Char] -> Maybe Time
parseTime' xs = do
  let (h, m) = splitAt 2 xs
  hours   <- parseDigits h
  minutes <- parseMinutes m
  validateTime (hours, minutes)

parseTime :: [Char] -> Maybe Time
parseTime t = validateInput t >>= parseTime'

-- |Difference between two times. If the second time is smaller than
-- the first, it is considered to lie in the next day
timeDifference :: Time -> Time -> Time
timeDifference t1 t2@(h2, m2)
  | ts2 < ts1 = timeDifference t1 (h2 + 24, m2)
  | otherwise = toTime (ts2 - ts1)
  where
    ts2 = toMinutes t2
    ts1 = toMinutes t1
    toMinutes (h, m) = h * 60 + m -- how many minutes into the day we are
    toTime t = divMod t 60 -- minutes back to hours + minutes

reportElapsed :: Time -> Time -> String
reportElapsed t1 t2 = printf "Time elapsed: %02dh %02dm" h m
  where (h, m) = timeDifference t1 t2

formatReport Nothing _ = "Invalid start time"
formatReport _ Nothing = "Invalid end time"
formatReport (Just t1) (Just t2) = reportElapsed t1 t2

getInput :: String -> IO String
getInput s = do
  putStr s
  hFlush stdout
  getLine

-- |If given Nothing, restart the main loop
restartOnError Nothing = do
  putStrLn "Invalid input"
  main
restartOnError _ = return ()

main :: IO ()
main = do
  s1 <- getInput "> "
  let t1 = parseTime s1
  restartOnError t1
  s2 <- getInput "→ "
  let t2 = parseTime s2
  restartOnError t2
  putStrLn $ formatReport t1 t2
  main
