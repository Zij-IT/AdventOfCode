import qualified Data.Set as S
import Data.List(lines, foldl')
import Data.Tuple(fst, snd)
import Debug.Trace

newtype Register = Register Int
  deriving Show

newtype Cycle = Cycle Int
  deriving Show

data Instr = Add Int | NoOp
  deriving Show

type State = (Register, Cycle, [Instr])

clean :: String -> [Instr]
clean = map parseInstr . filter (not . null) . lines  
  where parseInstr s = case words s of
                          ["noop"]    -> NoOp
                          ["addx", rs] -> Add (read rs)
                          c -> error $ "Invalid words: " ++ show c
                          
-- if cycleNumber +- 1 == RegX then -> '#' else '.'
  
cpuCycle :: State -> State
cpuCycle state = case state of
  (Register x, Cycle 1, (Add i):is) -> (Register (x + i), Cycle 0      ,         is)
  (r         , Cycle 0,    NoOp:is) -> (r               , Cycle 0      ,         is)
  (r         , Cycle n,         is) -> (r               , Cycle (n + 1),         is)
  
getRegisterVal :: State -> Int
getRegisterVal (Register r, _, _) = r
  
runNCycles :: Int -> State -> [State]
runNCycles n = take n . iterate cpuCycle

-- Explanation for zipWith (curry draw) [0..]
--   The problem states that the register change is applied at the end of a cycle. Thus follows that
--   the value at the end of cycle N is equivalent to the value at beginning of cycle N + 1. Thus,
--   the zipped values [(1, 1), (2, 1), (3, 16), (4, 16), (5, 5), ...] are to be understood as
--   "BeginingCycleN with RegisterValue" pairs
partOne :: [Instr] -> Int
partOne is = sum . map (uncurry (*)) . filter (isDesiredSignal . fst) . zip [1..] . map getRegisterVal $ runNCycles 220 (Register 1, Cycle 0, is)
  where
    isDesiredSignal :: Int -> Bool
    isDesiredSignal x = x == 20 || x == 60 || x == 100 || x == 140 || x == 180 || x == 220

-- Explanation for zipWith (curry draw) [0..]
--   The positions start with position 0 and end at 239. Thus instead of starting 1 like in P1 we start
--   at 0
partTwo :: [Instr] -> String
partTwo is = zipWith (curry draw) [0..] . map getRegisterVal $ runNCycles 240 (Register 1, Cycle 0, is)
  where
    draw :: (Int, Int) -> Char
    draw (cc, x) = if abs ((cc `mod` 40) - x) <= 1 then '#' else '.'

main :: IO ()
main = do
  input <- clean <$> readFile "./test_input/day10.txt"
  putStrLn . ("Part 1: "  ++) . show $ partOne input
  putStrLn . ("Part 2:\n" ++) $ partTwo input