import Data.List

main = do line <- getLine
          putStrLn $ show $ counter line 4 4

counter :: String -> Int -> Int -> Int
counter xs n m | areUnique = n
               | otherwise = counter (tail xs) (n+1)
  where areUnique = (length $ group $ sort $ take m xs) == m