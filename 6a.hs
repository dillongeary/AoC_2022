import Data.List

main = do line <- getLine
          putStrLn $ show $ counter line 4

counter :: String -> Int -> Int
counter (a:b:c:d:es) n | areUnique = n
                       | otherwise = counter (b:c:d:es) (n+1)
  where areUnique = (length $ group $ sort [a,b,c,d]) == 4