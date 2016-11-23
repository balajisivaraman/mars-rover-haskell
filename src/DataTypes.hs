{-# LANGUAGE TemplateHaskell #-}
module DataTypes where

import Control.Lens (over,view,makeLenses)

data Direction = North
               | South
               | East
               | West
               deriving Show

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

data Instruction = MoveForward
                 | TurnLeft
                 | TurnRight
                 deriving Show

data Position = Position { _x :: Int, _y :: Int } deriving (Show)
makeLenses ''Position

data MarsRover = MarsRover { _position :: Position, _direction :: Direction }
makeLenses ''MarsRover


forwardOf :: Direction -> MarsRover -> MarsRover
forwardOf North = over (position . y) (+ 1)
forwardOf South = over (position . y) (subtract 1)
forwardOf East  = over (position . x) (+ 1)
forwardOf West  = over (position . x) (subtract 1)

instance Show MarsRover where
  show rover = (show (view (position . x) rover)) ++ " " ++
               (show (view (position . y) rover)) ++ " " ++
               (show (view (direction) rover))
