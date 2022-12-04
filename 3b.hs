main = do lines <- getLines
          putStrLn $ show $ sum $ map (priorities . findBadge) $ intoThrees lines

getLines :: IO ([String])
getLines = do x <- getLine
              if x == ""
                 then return []
                 else do xs <- getLines
                         return (x:xs)

intoThrees :: [String] -> [(String,String,String)]
intoThrees [] = []
intoThrees (a:b:c:ds) = (a,b,c) : intoThrees ds

findBadge :: (String,String,String) -> Char
findBadge ([],bs,cs) = 'a'
findBadge (a:as,bs,cs) | a `elem` bs && a `elem` cs = a
                       | otherwise = findBadge (as,bs,cs)

priorities :: Char -> Int
priorities = char2int "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 1
  where char2int :: String -> Int -> Char -> Int
        char2int (x:xs) i a | a == x = i
                            | otherwise = char2int xs (i+1) a