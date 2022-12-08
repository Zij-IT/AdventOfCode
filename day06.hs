import Data.List(lines, nub)

clean :: String -> String
clean = head  . lines

partX :: Int -> String -> Int
partX i =  go 0
  where
    go :: Int -> String -> Int
    go n ls = if (==i) . length . nub $ take i ls then n + i
              else go (n + 1) (tail ls)

partOne :: String -> Int
partOne = partX 4

partTwo :: String -> Int
partTwo = partX 14

main :: IO ()
main = do
  input <- readFile "./input/day06.txt"
  let cleanInput = clean input in do
    putStrLn . ("Part 1:" ++) . show $ partOne cleanInput
    putStrLn . ("Part 2:" ++) . show $ partTwo cleanInput