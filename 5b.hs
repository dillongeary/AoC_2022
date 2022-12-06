import Data.List

main = do lines <- getLines
          let crateList = map (filter (/= ' ')) $ transpose $ map (getCrates) $ filter (elem '[') lines
          let instructionList = map (getInstructions) $ filter (isInfixOf "move") lines
          putStrLn $ show $ map head $ applyInstructions instructionList crateList


getLines :: IO ([String])
getLines = do x <- getLine
              if x == "end"
                 then return []
                 else do xs <- getLines
                         return (x:xs)

getCrates :: String -> String
getCrates [] = []
getCrates ('[':a:']':as) = a : getCrates1 as
getCrates (' ':' ':' ':as) = ' ' : getCrates1 as

getCrates1 [] = []
getCrates1 (' ':as) = getCrates as

applyInstructions :: [(Int,Int,Int)] -> [String] -> [String]
applyInstructions [] is = is
applyInstructions ( (n,f,t) :xs) is = applyInstructions xs os
  where fromStack = last $ take f is
        toStack = last $ take t is
        movedCrates = take n fromStack
        fromStack2 = drop n fromStack
        toStack2 = movedCrates ++ toStack
        os1 = (take (f-1) is) ++ [fromStack2] ++ (drop f is)
        os = (take (t-1) os1) ++ [toStack2] ++ (drop t os1)

getInstructions :: String -> (Int,Int,Int)
getInstructions xs = (a,b,c)
  where (a:b:c:ds) = getInstructions' xs
        getInstructions' :: String -> [Int]
        getInstructions' ('m':'o':'v':'e':' ':xs) = getNumeral xs ""
        getInstructions' ('f':'r':'o':'m':' ':xs) = getNumeral xs ""
        getInstructions' ('t':'o':' ':xs) = getNumeral xs ""
        getInstructions' [] = []

        getNumeral :: String -> String -> [Int]
        getNumeral [] o = read o : getInstructions' xs
        getNumeral (' ':xs) o = read o : getInstructions' xs
        getNumeral (x:xs) o = getNumeral xs (o ++ [x])