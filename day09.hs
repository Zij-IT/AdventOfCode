import qualified Data.Set as S
import Data.List(lines, foldl')

type State = (S.Set (Int, Int), (Int, Int), [(Int, Int)])

clean :: String -> [(Char, Int)]
clean = map split_ . filter (not . null) . lines
  where split_ (c:_:rs) = (c, read rs)
  
move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) c = case c of
  'U' -> (x, y + 1)
  'D' -> (x, y - 1)
  'L' -> (x - 1, y)
  'R' -> (x + 1, y)
  
follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (hx, hy) (tx, ty) = case (hx - tx, hy - ty) of
  ( 2,  2) -> (tx + 1, ty + 1)
  (-2,  2) -> (tx - 1, ty + 1)
  ( 2, -2) -> (tx + 1, ty - 1)
  (-2, -2) -> (tx - 1, ty - 1)
  ( 2,  _) -> (tx + 1, hy)
  (-2,  _) -> (tx - 1, hy)
  ( _,  2) -> (hx, ty + 1)
  ( _, -2) -> (hx, ty - 1)
  _        -> (tx, ty)
 
partX :: Int -> [(Char, Int)] -> Int
partX knots = S.size . getSet . foldl' moveN (S.singleton (0, 0), (0, 0), [(0, 0) | _ <- [1..knots]])
  where
    getSet :: State -> S.Set (Int, Int)
    getSet (x, _, _) = x
  
    moveN :: State ->  (Char, Int) -> State
    moveN state (c, i) = (!! i) $ iterate (`move1` c) state
                        
    move1 :: State -> Char -> State
    move1 (s, h, t) c = let nh = move h c
                            nt = tail $ scanl follow nh t
                        in (S.insert (last nt) s, nh, nt)

partOne :: [(Char, Int)] -> Int
partOne = partX 1

partTwo :: [(Char, Int)] -> Int
partTwo = partX 9

main :: IO ()
main = do
  input <- clean <$> readFile "./input/day09.txt"
  putStrLn . ("Part 1: " ++) . show $ partOne input
  putStrLn . ("Part 2: " ++) . show $ partTwo input