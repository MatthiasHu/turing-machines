import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad


newtype Tape = Tape (S.Set Int)

emptyTape :: Tape
emptyTape = Tape S.empty

getTapeBit :: Int -> Tape -> Bit
getTapeBit n (Tape t) = if n `S.member` t then I else O

setTapeBit :: Int -> Bit -> Tape -> Tape
setTapeBit n I (Tape t) = Tape (S.insert n t)
setTapeBit n O (Tape t) = Tape (S.delete n t)

data Bit = O | I
  deriving (Show, Eq, Ord)

newtype Machine = Machine (M.Map StateName (Bit -> StateCode))

type StateName = String

data Movement = L | R

movement :: Movement -> Int
movement L = -1
movement R = 1

data StateCode = StateCode Bit Movement StateName

step :: Machine -> (StateName, Tape, Int) -> (StateName, Tape, Int)
step (Machine m) (sn, t, pos) =
  let StateCode b mov sn' = m M.! sn $ getTapeBit pos t
  in (sn', setTapeBit pos b t, pos + movement mov)

showExecution :: Machine -> StateName -> Tape -> IO ()
showExecution m sn t = showExecution' m (sn, t, 0)

showExecution' :: Machine -> (StateName, Tape, Int) -> IO ()
showExecution' m s@(sn, t, pos) = do
  showTape t pos
  putStrLn sn
  _ <- getLine
  showExecution' m (step m s)

showTape :: Tape -> Int -> IO ()
showTape t pos = do
  when (pos >= l) $ putStr (replicate (2*(pos-l)) ' ' ++ " |")
  putStrLn ""
  forM_ [l..r] $ \n -> (putStr $ " " ++ show (getTapeBit n t))
  putStrLn ""
  where
    l = -10
    r = 20
