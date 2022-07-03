{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.RWS
import qualified Data.Map          as M
import           Rose
import           Vec

data AtomType = MoveTo | CaptureAt deriving Eq

data Atom = Atom AtomType Vec deriving Eq

retypeAtom :: AtomType -> Atom -> Atom
retypeAtom t (Atom _ v) = Atom t v

transformAtom :: [Vec] -> Atom -> Atom
transformAtom m (Atom t v) = Atom t (Vec.transform m v)

type UnresolvedMoves = [Rose (Maybe Atom)]

type MoveMachine = RWS Pos () UnresolvedMoves
newtype Mover = MkMover {runMover :: Board -> MoveMachine ()}

fromP :: Pos -> Vec
fromP (Pos p) = Vec p

toP :: Vec -> Pos
toP (Vec p) = Pos p

addP :: Pos -> Vec -> Pos
addP p v = toP $ fromP p .+ v

newtype Pos = Pos [Int] deriving (Eq, Ord)

newtype Square = Square {
    _occupant :: Maybe Piece
}

data Board = Board {
    _grid        :: M.Map Pos Square,
    _boardFilter :: [Board] -> [Board]
}

newtype Piece = Piece {
    _mover :: Mover
}

makeLenses ''Board
makeLenses ''Piece
makeLenses ''Square
