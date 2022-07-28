module Resolver where

import           Atom
import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import           Mover
import           Rose
import           Types
import           Vec

-- Determines whether a square exists on a given board at a given position
isInBounds :: Board -> Pos -> Bool
isInBounds board pos = getSquareState board pos /= Void

data SquareState = Void | Unoccupied | Occupied deriving Eq

-- Gets the square at the given position, if one exists
getSquareAt :: Board -> Pos -> Maybe Square
getSquareAt b p = M.lookup p (b ^. grid)

-- Checks the state of a given position as to whether or not a square exists, and,
-- if one does exist, whether or not a piece is present there
getSquareState :: Board -> Pos -> SquareState
getSquareState board pos = case getSquareAt board pos of
    Nothing       -> Void
    Just s        -> case s ^. occupant of
        Nothing -> Unoccupied
        Just _  -> Occupied

-- gets the piece at a given square, if one exists
pieceAt :: Board -> Pos -> Maybe Piece
pieceAt board pos = do
    square <- getSquareAt board pos
    square ^. occupant

-- Determines whether a square is occupied
isOccupied :: Board -> Pos -> Bool
isOccupied b p = getSquareState b p == Occupied

-- Directly spawns a piece onto a given square. This function does **not** trigger
-- any moves of any pieces. Used mostly internally to modify pieces
spawnOn :: Board -> Pos -> Piece -> Board
spawnOn board pos piece = over grid spawnPiece board where
    spawnPiece = M.adjust (set occupant (Just piece)) pos

-- Moves whatever exists at one square to another. If the source square is
-- void or empty, the target will be cleared.
-- TODO: CAPTURE LOGIC
movePiece :: Board -> Pos -> Pos -> Board
movePiece board pos newPos = set grid newGrid board where
    piece = pieceAt board pos
    oldGrid = board ^. grid
    intermediateGrid = M.adjust (set occupant Nothing) pos oldGrid
    newGrid = M.adjust (set occupant piece) newPos intermediateGrid

-- Returns all possible intermediate game states (a move was executed but things
-- such as post-turn automatic triggers and the turn counter are not updated)
-- reachable after an atom is executed by the piece at the state's position
executeAtom :: Board -> Atom -> State Pos [Board]
executeAtom board (Move v) = do
    pos <- get
    let pos' = addP pos v
    put pos'
    return [movePiece board pos pos']

executeAtom board (On t a) = do
    case getPrimaryVectorOf a of
        Nothing -> executeAtom board a
        Just v -> do
            pos <- get
            let pos' = addP pos v
            let currentPiece = pieceAt board pos
            let testPiece = pieceAt board pos'
            if satisfiesPieceType currentPiece testPiece t then
                executeAtom board a
            else
                return []

executeAtom board (ChangePieceState s v) = do
    pos <- get
    case pieceAt board pos of
        Nothing -> return []
        Just p -> do
            let p' = over pieceState (M.insert s v) p
            return [spawnOn board pos p']

executeAtom board (ChangeGlobalState s v) =
    return [over globalState (M.insert s v) board]


executeAtom board (Endow v a) = do
    pos <- get
    put $ addP pos v
    moves <- executeAtom board a
    put pos
    return moves

-- determines whether or not a piece type restriction is satisfied by the second
-- piece. If comparison must be made, the first piece is used for such
satisfiesPieceType :: Maybe Piece -> Maybe Piece -> PieceType -> Bool
satisfiesPieceType _ mp2 OnEmpty = isNothing mp2
satisfiesPieceType _ mp2 OnOccupied = isJust mp2
satisfiesPieceType mp1 mp2 OnAlly = sPT' mp1 mp2 (\x y -> (x ^. team) == (y ^. team))
satisfiesPieceType mp1 mp2 OnEnemy = sPT' mp1 mp2 (\x y -> (x ^. team) /= (y ^. team))
satisfiesPieceType _ Nothing (OnTeam _) = False
satisfiesPieceType _ (Just p2) (OnTeam t) = p2 ^. team == t
satisfiesPieceType _ Nothing (OnColor _) = False
satisfiesPieceType _ (Just p2) (OnColor c) = p2 ^. color == c
satisfiesPieceType p1 p2 (OnOr a b) = satisfiesPieceType p1 p2 a || satisfiesPieceType p1 p2 b

sPT' :: Maybe Piece -> Maybe Piece -> (Piece -> Piece -> Bool) -> Bool
sPT' mp1 mp2 f = fromMaybe False (liftM2 f mp1 mp2)

-- execute every Rose Tree on all given board states and return all possible
-- results
forkResolution :: [Board] -> [Rose (Maybe Atom)] -> State Pos [Board]
forkResolution boards roses = do
    pos <- get
    let propogate board rose = do {
        x <- executeRose board rose;
        put pos;
        return x
    }

    let paths = [(b, r) | b <- boards, r <- roses]
    newboards <- mapM (uncurry propogate) paths
    return $ concat newboards

-- Return all posible boards given by executing a single Rose Tree on a single
-- board
executeRose :: Board -> Rose (Maybe Atom) -> State Pos [Board]
executeRose board (Rose Nothing _) = return []
executeRose board (Rose (Just atom) roses) = do
    result <- executeAtom board atom
    case result of
        []        -> return []
        newboards -> forkResolution newboards roses

-- Execute an UnresolvedMoves (a list of Atom Trees) on a given board with the
-- piece at the given position
resolveMoves :: Board -> UnresolvedMoves -> Pos -> [Board]
resolveMoves board roses = evalState (forkResolution [board] roses)

--new boards given by moving the piece at some position using some arbitrary mover
resolveAtWithMover :: Board -> Pos -> Mover -> [Board]
resolveAtWithMover board pos mover = resolveMoves board moves pos where
    moves = fst $ execRWS (runMover mover board) pos []

--new boards given by moving the piece at some position using its own mover
resolveAt :: Board -> Pos -> Maybe [Board]
resolveAt board pos = do
    piece <- pieceAt board pos
    return $ resolveAtWithMover board pos (piece ^. mover)
