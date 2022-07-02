module Resolver where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import           Mover
import           Rose
import           Types
import           Vec

isInBounds :: Board -> Pos -> Bool
isInBounds board pos = getSquareState board pos /= Void

data SquareState = Void | Unoccupied | Occupied deriving Eq

getSquareAt :: Board -> Pos -> Maybe Square
getSquareAt b p = M.lookup p (b ^. grid)

getSquareState :: Board -> Pos -> SquareState
getSquareState board pos = case getSquareAt board pos of
    Nothing       -> Void
    Just s        -> case s ^. occupant of
        Nothing -> Unoccupied
        Just _  -> Occupied


pieceAt :: Board -> Pos -> Maybe Piece
pieceAt board pos = do
    square <- getSquareAt board pos
    square ^. occupant

isOccupied :: Board -> Pos -> Bool
isOccupied b p = getSquareState b p == Occupied

spawnOn :: Board -> Pos -> Piece -> Board
spawnOn board pos piece = over grid spawnPiece board where
    spawnPiece = M.adjust (set occupant (Just piece)) pos

movePiece :: Board -> Pos -> Pos -> Board
movePiece board pos newPos = set grid newGrid board where
    piece = pieceAt board pos
    oldGrid = board ^. grid
    intermediateGrid = M.adjust (set occupant Nothing) pos oldGrid
    newGrid = M.adjust (set occupant piece) newPos intermediateGrid

executeAtom :: Board -> Atom -> State Pos (Maybe Board)
executeAtom board (Atom MoveTo v) = do
    pos <- get
    let pos' = addP pos v
    if getSquareState board pos' /= Unoccupied then
        return Nothing
    else do
        put pos'
        return $ Just $ movePiece board pos pos'

executeAtom board (Atom CaptureAt v) = do
    pos <- get
    let pos' = addP pos v
    if getSquareState board pos' /= Occupied then
        return Nothing
    else do
        put pos'
        return $ Just $ movePiece board pos pos'

forkResolution :: Board -> [Rose (Maybe Atom)] -> State Pos [Board]
forkResolution board roses = do
    pos <- get
    let propogate rose = do
        x <- executeRose board rose
        put pos
        return x

    newboards <- mapM propogate roses
    return $ concat newboards

executeRose :: Board -> Rose (Maybe Atom) -> State Pos [Board]
executeRose board (Rose Nothing _) = return [board]
executeRose board (Rose (Just atom) roses) = do
    result <- executeAtom board atom
    case result of
        Nothing       -> return [board]
        Just newboard -> forkResolution newboard roses


resolveMoves :: Board -> UnresolvedMoves -> Pos -> [Board]
resolveMoves board roses = evalState (forkResolution board roses)
