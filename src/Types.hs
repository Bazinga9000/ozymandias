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

newtype Move = Move [Atom] deriving Eq

mapMove :: (Atom -> Atom) -> Move -> Move
mapMove f (Move as) = Move $ map f as

retypeMove :: AtomType -> Move -> Move
retypeMove t = mapMove $ retypeAtom t

transformMove :: [Vec] -> Move -> Move
transformMove m = mapMove (transformAtom m)

concatMoves :: Move -> Move -> Move
concatMoves (Move a) (Move b) = Move $ a ++ b

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
    _boardFilter :: [Move] -> [Move]
}

newtype Piece = Piece {
    _mover :: Mover
}

makeLenses ''Board
makeLenses ''Piece
makeLenses ''Square
