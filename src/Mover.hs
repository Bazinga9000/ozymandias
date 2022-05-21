module Mover where

import           Control.Monad.RWS
import           Types
import           Vec

--no moves
nullMover :: Mover
nullMover = MkMover {runMover = \_ -> put []}

--only move is a move with no atoms
emptyMover :: Mover
emptyMover = MkMover {runMover = \_ -> put [Move []]}

--only move is the zero move
zeroMover :: Mover
zeroMover = leaper vzero

--mover with a single direction of movement
leaper :: Vec -> Mover
leaper v = MkMover {runMover = \_ -> put [Move [Atom v MoveTo]]}

--union
(|+|) :: Mover -> Mover -> Mover
x |+| y = MkMover {runMover = \b -> do
    m <- liftM2 (++) (runMover x b >> get) (runMover y b >> get)
    put m
    }

--mandatory compose, that is "do x then do y"
(|.|) :: Mover -> Mover -> Mover
x |.| y = MkMover {runMover = \b -> do
    xs <- runMover x b >> get
    ys <- runMover x b >> get
    put [concatMoves xm ym | xm <- xs, ym <- ys]
    }

--optional compose, that is "do x and then you may or may not do y"
(|.?|) :: Mover -> Mover -> Mover
a |.?| b = a |.| (emptyMover |+| b)

--filter out moves by some predicate
filterMv :: Mover -> (Move -> Bool) -> Mover
filterMv m f = MkMover {runMover = \b -> do
    mvs <- runMover m b >> get
    put $ filter f mvs
    }

--change the type of all moves
retypeMv :: Mover -> AtomType -> Mover
retypeMv m t = MkMover {runMover = \b -> do
    mvs <- runMover m b >> get
    put $ map (retypeMove t) mvs
    }

--repeat a move up to n times
freeN :: Int -> Mover -> Mover
freeN n m
    | n < 0 = error "Negative value passed to freeN"
    | n == 0 = nullMover
    | n > 0 = m |.?| freeN (n-1) m
