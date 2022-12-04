import Data.List

main = do lines <- getLines
          putStrLn $ show $ length $ filter (filterFunc) $ map (format . splitFunc) lines

getLines :: IO ([String])
getLines = do x <- getLine
              if x == ""
                 then return []
                 else do xs <- getLines
                         return (x:xs)

format :: [String] -> ((Int,Int),(Int,Int))
format [a,b,c,d] = ((read a, read b),(read c,read d))

filterFunc :: ((Int,Int),(Int,Int)) -> Bool
filterFunc ((a,b),(c,d)) | a <= c && b >= d = True
                         | c <= a && d >= b = True
                         | otherwise        = False

splitFunc :: String -> [String]
splitFunc xs = splitFunc' xs []
  where splitFunc' :: String -> String -> [String]
        splitFunc' [] as = [as]
        splitFunc' ('-':xs) as = as : splitFunc' xs []
        splitFunc' (',':xs) as = as : splitFunc' xs []
        splitFunc' (x:xs) as = splitFunc' xs (as ++ [x])