import Data.List

main = do line <- getLine
          putStrLn $ show $ counter line 14

counter :: String -> Int -> Int
counter xs n | areUnique = n
             | otherwise = counter (tail xs) (n+1)
  where areUnique = (length $ group $ sort $ take 14 xs) == 14