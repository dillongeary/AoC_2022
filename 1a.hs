import Data.List

main = do lines <- getLines
          putStrLn $ show $ maximum $ totals lines

totals :: [String] -> [Int]
totals xs = map total something
 where something = groupBy (\a b -> b /= "") xs
       total :: [String] -> Int
       total [] = 0
       total ("":xs) = total xs
       total (x:xs) = read x + total xs


getLines :: IO ([String])
getLines = do x <- getLine
              if x == "END"
                 then return []
                 else do xs <- getLines
                         return (x:xs)