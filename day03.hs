import qualified Data.Map.Strict as M
import Data.List (group, sort)
import Data.Bifunctor (bimap)
import Data.Tuple (fst)
import Data.Bits (xor, (.&.))

type Backpack = M.Map Char Int

priority :: Char -> Int
priority = (\x -> x - fromEnum 'A' + 1 - if wasUpper x then 6 else 0) . twiddleCase . fromEnum
  where
    wasUpper :: Int -> Bool
    wasUpper i = i .&. 32 == 32
    
    twiddleCase :: Int -> Int
    twiddleCase = xor 32

clean :: String -> [(Backpack, Backpack)]
clean = map toBackpacks . lines
  where
    toBackpacks :: String -> (Backpack, Backpack)
    toBackpacks = bimap toBackpack toBackpack . halve

    toBackpack :: String -> Backpack
    toBackpack = M.fromAscList . map (\x -> (head x, length x)) . group . sort     

    halve :: String -> (String, String)
    halve s = splitAt half s
      where half = (`div` 2) . length $ s
  
partOne :: [(Backpack, Backpack)] -> Int
partOne = sum . concatMap (map (priority. fst) . M.toList . uncurry M.intersection)

partTwo :: [(Backpack, Backpack)] -> Int
partTwo [] = 0
partTwo (x:y:z:ls) = priority (getBadge [x,y,z]) + partTwo ls 
  where 
    getBadge :: [(Backpack, Backpack)] -> Char
    getBadge = fst . head . M.toList . foldl1 M.intersection . map (uncurry M.union)

main :: IO ()
main = do
  input <- readFile "./input/day03.txt"
  let cleanInput = clean input in do
    print $ partOne cleanInput
    print $ partTwo cleanInput  
