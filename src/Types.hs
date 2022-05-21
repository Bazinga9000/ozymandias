{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.RWS
import           Data.Map
import           Vec

data AtomType = MoveTo | CaptureAt

data Atom = Atom Vec AtomType

type MoveMachine = RWS Pos () [Move]
newtype Mover = MkMover {runMover :: Board -> MoveMachine [Move]}

newtype Move = Move [Atom]

concatMoves :: Move -> Move -> Move
concatMoves (Move a) (Move b) = Move $ a ++ b

newtype Pos = Pos [Int]

newtype Board = Board {
    _grid :: Map Pos Piece
}

newtype Piece = Piece {
    _mover :: Mover
}

makeLenses ''Board
makeLenses ''Piece
