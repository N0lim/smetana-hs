module AST where

data Smetana = Smetana Step Smetana | End deriving(Show)
data Step = Step Int Command deriving(Show)
data Command = Swap Int Int | GoTo Int deriving(Show)