{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.RWS
import qualified Data.Map          as M
import           Vec

data AtomType = MoveTo | CaptureAt deriving Eq

data Atom = Atom Vec AtomType deriving Eq

retypeAtom :: AtomType -> Atom -> Atom
retypeAtom t (Atom v _) = Atom v t

transformAtom :: [Vec] -> Atom -> Atom
transformAtom m (Atom v t) = Atom (Vec.transform m v) t

type MoveMachine = RWS Pos () [Move]
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

singleton :: Vec -> Move
singleton v = Move [Atom v MoveTo]

fromP :: Pos -> Vec
fromP (Pos p) = Vec p

newtype Pos = Pos [Int]

data Board = Board {
    _grid        :: M.Map Pos Piece,
    _boardFilter :: [Move] -> [Move]
}

newtype Piece = Piece {
    _mover :: Mover
}

makeLenses ''Board
makeLenses ''Piece
