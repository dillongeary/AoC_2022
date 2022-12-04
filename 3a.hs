main = do lines <- getLines
          putStrLn $ show $ sum $ map (char2int "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 1 . findWrong . compartments) lines

getLines :: IO ([String])
getLines = do x <- getLine
              if x == ""
                 then return []
                 else do xs <- getLines
                         return (x:xs)

compartments :: String -> (String,String)
compartments xs = (take n xs,drop n xs)
  where n = (length xs) `div` 2

findWrong :: (String,String) -> Char
findWrong ([],bs) = 'a'
findWrong ((a:as),bs) | a `elem` bs = a
                      | otherwise = findWrong (as,bs)

char2int :: String -> Int -> Char -> Int
char2int (x:xs) i a | a == x = i
                    | otherwise = char2int xs (i+1) a