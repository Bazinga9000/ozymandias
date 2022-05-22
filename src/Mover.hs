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
leaper v = MkMover {runMover = \_ -> put [singleton v]}

--runs two movers with the same state, then merges their outputs
--TODO: there is no way in hell that this is the best way to write this
fork :: ([Move] -> [Move] -> [Move]) -> Mover -> Mover -> Mover
fork f x y = MkMover {runMover = \b -> do
    cs <- get
    xs <- runMover x b >> get
    put cs
    ys <- runMover y b >> get
    put $ f xs ys
    }

--union
(|+|) :: Mover -> Mover -> Mover
x |+| y = fork (++) x y

--mandatory compose, that is "do x then do y"
(|.|) :: Mover -> Mover -> Mover
x |.| y = MkMover {runMover = \b -> do
    xs <- runMover x b >> get
    ys <- runMover y b >> get
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

--iteratively apply the function f n times
iterN :: (Mover -> Mover -> Mover) -> Int -> Mover -> Mover
iterN f n m
    | n < 0 = error "Negative N given to iterN"
    | n == 0 = nullMover
    | otherwise = f m (iterN f (n-1) m)

--repeat a mover up to n times
freeN :: Int -> Mover -> Mover
freeN = iterN (|.?|)

--repeat a mover exactly n times
freeNStrict :: Int -> Mover -> Mover
freeNStrict = iterN (|.|)
