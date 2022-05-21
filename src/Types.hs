{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.RWS
import qualified Data.Map          as M
import           Vec

data AtomType = MoveTo | CaptureAt

data Atom = Atom Vec AtomType

retypeAtom :: AtomType -> Atom -> Atom
retypeAtom t (Atom v _) = Atom v t

type MoveMachine = RWS Pos () [Move]
newtype Mover = MkMover {runMover :: Board -> MoveMachine ()}

newtype Move = Move [Atom]

retypeMove :: AtomType -> Move -> Move
retypeMove t (Move as) = Move $ map (retypeAtom t) as

concatMoves :: Move -> Move -> Move
concatMoves (Move a) (Move b) = Move $ a ++ b

newtype Pos = Pos [Int]

newtype Board = Board {
    _grid :: M.Map Pos Piece
}

newtype Piece = Piece {
    _mover :: Mover
}

makeLenses ''Board
makeLenses ''Piece
