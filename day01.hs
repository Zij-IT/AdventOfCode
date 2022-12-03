import Data.List(lines, groupBy, sortBy)

clean :: String -> [[Int]]
clean = map (map read . filter (not . null)) . groupBy (\x y -> not $ null y) . lines

partOne :: [[Int]] -> Int
partOne = maximum . map sum

partTwo :: [[Int]] -> Int
partTwo = sum . take 3 . sortBy (flip compare) . map sum

main :: IO ()
main = do
  input <- readFile "./input/day01.txt"
  let cleanInput = clean input in do
    print $ partOne cleanInput
    print $ partTwo cleanInput