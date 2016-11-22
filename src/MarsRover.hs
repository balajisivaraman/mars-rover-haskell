module MarsRover
    ( marsMain
    ) where

import DataTypes
import Control.Lens (over, view)
import Data.Char (digitToInt)

leftOf :: Direction -> Direction
leftOf North = West
leftOf South = East
leftOf East = North
leftOf West = South

rightOf :: Direction -> Direction
rightOf North = East
rightOf South = West
rightOf East = South
rightOf West = North

turnLeft :: MarsRover -> MarsRover
turnLeft = over direction leftOf

turnRight :: MarsRover -> MarsRover
turnRight = over direction rightOf

forwardOf :: Direction -> MarsRover -> MarsRover
forwardOf North = over (position . y) (+ 1)
forwardOf South = over (position . y) (subtract 1)
forwardOf East  = over (position . x) (+ 1)
forwardOf West  = over (position . x) (subtract 1)

moveForward :: MarsRover -> MarsRover
moveForward rover = forwardOf (view direction rover) rover

parseDirection :: String -> Direction
parseDirection "N" = North
parseDirection "S" = South
parseDirection "E" = East
parseDirection "W" = West

parseInstruction :: Char -> Instruction
parseInstruction 'M' = MoveForward
parseInstruction 'L' = TurnLeft
parseInstruction 'R' = TurnRight

runInstruction :: Instruction -> MarsRover -> MarsRover
runInstruction MoveForward = moveForward
runInstruction TurnRight   = turnRight
runInstruction TurnLeft    = turnLeft

getInstructions :: String -> [Instruction]
getInstructions = map parseInstruction

marsMain :: IO ()
marsMain = do
  putStrLn "Enter the starting direction"
  direction <- parseDirection <$> getLine
  putStrLn "Enter the starting X coordinate"
  x <- read <$> getLine
  putStrLn "Enter the starting Y coordinate"
  y <- read <$> getLine
  let position = Position x y
  let rover = MarsRover position direction
  putStrLn "Enter the sequence of instructions"
  instructions <- getInstructions <$> getLine
  let finalRover = foldr runInstruction rover instructions
  putStrLn (show finalRover)
