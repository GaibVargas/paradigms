module TypeModule where

type Row x = [x]
type Matrix x = [Row x]
type PossibleChoices = [Int]
type Board = Matrix (PossibleChoices, Int)
