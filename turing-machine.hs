{-# LANGUAGE LambdaCase #-}

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

stopStateName :: StateName
stopStateName = "stop"

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
  when (sn /= stopStateName) $
    showExecution' m (step m s)

showTape :: Tape -> Int -> IO ()
showTape t pos = do
  when (pos >= l) $ putStr (replicate (2*(pos-l)) ' ' ++ " |")
  putStrLn ""
  forM_ [l..r] $ \n -> (putStr $ " " ++ show (getTapeBit n t))
  putStrLn ""
  where
    l = -10
    r = 28


-- example machines

machineTimesTwo :: Machine
machineTimesTwo = Machine . M.fromList $
  [ ( "start"
    , \case O -> StateCode O R "stop"
            I -> StateCode O R "skip_right_two"
    )
  , ( "skip_right_two"
    , \case O -> StateCode O R "skip_right_one"
            I -> StateCode I R "skip_right_two"
    )
  , ( "skip_right_one"
    , \case O -> StateCode I R "write_two_1"
            I -> StateCode I R "skip_right_one"
    )
  , ( "write_two_1"
    , \case _ -> StateCode I L "skip_left_two"
    )
  , ( "skip_left_two"
    , \case O -> StateCode O L "skip_left_one"
            I -> StateCode I L "skip_left_two"
    )
  , ( "skip_left_one"
    , \case O -> StateCode O R "start"
            I -> StateCode I L "skip_left_one"
    )
  ]

-- Writes four 1s and halts.
busy2 :: Machine
busy2 = Machine . M.fromList $
  [ ( "A"
    , \case O -> StateCode I R "B"
            I -> StateCode I L "B"
    )
  , ( "B"
    , \case O -> StateCode I L "A"
            I -> StateCode I L "stop"
    )
  ]

-- Writes six 1s and halts.
busy3 :: Machine
busy3 = Machine . M.fromList $
  [ ( "A"
    , \case O -> StateCode I R "B"
            I -> StateCode I L "C"
    )
  , ( "B"
    , \case O -> StateCode I L "A"
            I -> StateCode I R "B"
    )
  , ( "C"
    , \case O -> StateCode I L "B"
            I -> StateCode I L "stop"
    )
  ]
