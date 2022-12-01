import Data.List

main = do lines <- getLines
          putStrLn $ show $ sum $ lastN 3 $ sort $ totals lines

totals :: [String] -> [Int]
totals xs = map total something
 where something = groupBy (\a b -> b /= "") xs
       total :: [String] -> Int
       total [] = 0
       total ("":xs) = total xs
       total (x:xs) = read x + total xs

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

getLines :: IO ([String])
getLines = do x <- getLine
              if x == "END"
                 then return []
                 else do xs <- getLines
                         return (x:xs)