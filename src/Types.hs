{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Control.Lens.TH
import           Data.Map
import           Vec

data Atom = MoveTo Vec | CaptureAt Vec
newtype Move = Move [Atom]
newtype Pos = Pos [Int]

newtype Board = Board {
    _grid :: Map Pos Piece
}

newtype Piece = Piece {
    _mover :: Board -> [Move] -> [Move]
}

makeLenses ''Board
makeLenses ''Piece
