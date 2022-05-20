module Types where

import           Data.Map

newtype Board = Board {
    grid :: Map Pos Piece
}

newtype Piece = Piece {
    mover :: Board -> [Move] -> [Move]
}

newtype Move = Move [Atom]
newtype Pos = Pos [Int]
newtype Vec = Vec [Int]

data Atom = MoveTo Vec | CaptureAt Vec
