{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.RWS
import qualified Data.Map          as M
import           Rose
import           Vec

newtype Square = Square {
    _occupant :: Maybe Piece
}

data Board = Board {
    _grid        :: M.Map Pos Square,
    _boardFilter :: [Board] -> [Board],
    _globalState :: M.Map String StateValue
}

data StateValue =
      SInt Int
    | SBool Bool
    | SString String
    | SPosition Pos
    | SList [StateValue]
    | SPiece Piece

data UnaryOperator = Negate | Not

data BinaryOperator =
      Plus | Minus | Times | Divide | Modulo
    | Equal | NotEqual | Greater | Less | GreaterEqual | LessEqual
    | And | Or | Xor
    | BitAnd | BitOr | BitXor

data StateExpr =
      Constant StateValue
    | Get String
    | GetGlobal String
    | Let String StateExpr StateExpr
    | UnaryOp UnaryOperator StateExpr StateExpr
    | BinOp BinaryOperator StateExpr StateExpr


data StatusDuration = Finite Int | Infinite deriving (Eq, Ord, Show)

data Piece = Piece {
    _mover         :: Mover,
    _name          :: String,
    _symbol        :: String,
    _movesMade     :: Int,
    _capturesMade  :: Int,
    _team          :: Int,
    _color         :: Int,
    _statusEffects :: M.Map String StatusDuration,
    _pieceState    :: M.Map String StateValue
}

data PieceType = OnEmpty | OnOccupied | OnAlly | OnEnemy | OnTeam Int | OnColor Int | OnOr PieceType PieceType

data Atom =
      Move Vec
    | On PieceType Atom
    | ChangePieceState String StateValue
    | ChangeGlobalState String StateValue
    | Endow Vec Atom

type UnresolvedMoves = [Rose (Maybe Atom)]

type MoveMachine = RWS Pos () UnresolvedMoves
newtype Mover = MkMover {runMover :: Board -> MoveMachine ()}

makeLenses ''Board
makeLenses ''Piece
makeLenses ''Square
