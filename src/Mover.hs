module Mover where

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.Map          as M
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

--mover that can go anywhere
universalMover :: Mover
universalMover = MkMover {runMover = \b ->
    put $ map (singleton . fromP) (M.keys $ b ^. grid)
    }

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
    put $ liftM2 concatMoves xs ys
    }

--optional compose, that is "do x and then you may or may not do y"
(|.?|) :: Mover -> Mover -> Mover
a |.?| b = a |.| (emptyMover |+| b)

--filter out moves by some predicate
filterMv :: Mover -> (Move -> Bool) -> Mover
filterMv m f = MkMover {runMover = \b -> do
    runMover m b
    modify $ filter f
    }

--change the type of all moves
retypeMv :: Mover -> AtomType -> Mover
retypeMv m t = MkMover {runMover = \b -> do
    runMover m b
    modify $ map (retypeMove t)
    }
