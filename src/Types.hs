module Types where

import           Data.Map
import           Vec

newtype Board = Board {
    grid :: Map Pos Piece
}

newtype Piece = Piece {
    mover :: Board -> [Move] -> [Move]
}

newtype Move = Move [Atom]
newtype Pos = Pos [Int]

data Atom = MoveTo Vec | CaptureAt Vec
