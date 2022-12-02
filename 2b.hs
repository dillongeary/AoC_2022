
main = do lines <- getLines
          putStrLn $ show $ sum $ map scoreCalc lines

getLines :: IO ([String])
getLines = do x <- getLine
              if x == ""
                 then return []
                 else do xs <- getLines
                         return (x:xs)

scoreCalc :: String -> Int
scoreCalc [a,_,b] = (winnerScore a b) + ((selectedScore b) * 3)

winnerScore :: Char -> Char -> Int
winnerScore a b = (mod (a' + b' + 1) 3) + 1
 where a' = selectedScore a
       b' = selectedScore b

selectedScore :: Char -> Int
selectedScore 'A' = 1
selectedScore 'B' = 2
selectedScore 'C' = 3
selectedScore 'X' = 0
selectedScore 'Y' = 1
selectedScore 'Z' = 2