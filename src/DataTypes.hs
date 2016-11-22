{-# LANGUAGE TemplateHaskell #-}
module DataTypes where

import Control.Lens (view,makeLenses)

data Direction = North
               | South
               | East
               | West
               deriving Show

data Instruction = MoveForward
                 | TurnLeft
                 | TurnRight
                 deriving Show

data Position = Position { _x :: Int, _y :: Int } deriving (Show)
makeLenses ''Position

data MarsRover = MarsRover { _position :: Position, _direction :: Direction }
makeLenses ''MarsRover

instance Show MarsRover where
  show rover = (show (view (position . x) rover)) ++ " " ++
               (show (view (position . y) rover)) ++ " " ++
               (show (view (direction) rover))
