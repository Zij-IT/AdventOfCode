import Data.List (intersect)
import Data.Char (isUpper)

type Backpack = String

priority :: Char -> Int
priority c = if isUpper c then fromEnum c - 65 + 27 else fromEnum c - 97 + 1

clean :: String -> [(Backpack, Backpack)]
clean = map halve . lines
  where
    halve :: String -> (String, String)
    halve s = splitAt half s
      where half = length s `div` 2
  
partOne :: [(Backpack, Backpack)] -> Int
partOne = sum . map (priority . head . uncurry intersect)

partTwo :: [(Backpack, Backpack)] -> Int
partTwo [] = 0
partTwo (x:y:z:ls) = priority (getBadge [x,y,z]) + partTwo ls 
  where 
    getBadge :: [(Backpack, Backpack)] -> Char
    getBadge = head . foldl1 intersect . map (uncurry (++))

main :: IO ()
main = do
  input <- readFile "./input/day03.txt"
  let cleanInput = clean input in do
    print $ partOne cleanInput
    print $ partTwo cleanInput  
