
main = do lines <- getLines
          putStrLn $ show $ sum $ map scoreCalc lines

getLines :: IO ([String])
getLines = do x <- getLine
              if x == ""
                 then return []
                 else do xs <- getLines
                         return (x:xs)

scoreCalc :: String -> Int
scoreCalc [a,_,b] = (winnerScore a b) + (selectedScore b)

winnerScore :: Char -> Char -> Int
winnerScore a b | diff == -2 = 0
                | diff == -1 = 6
                | diff == 0 = 3
                | diff == 1 = 0
                | diff == 2 = 6
 where diff = a' - b'
       a' = selectedScore a
       b' = selectedScore b

selectedScore :: Char -> Int
selectedScore 'A' = 1
selectedScore 'B' = 2
selectedScore 'C' = 3
selectedScore 'X' = 1
selectedScore 'Y' = 2
selectedScore 'Z' = 3