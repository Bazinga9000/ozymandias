module Mover where

import           Control.Lens
import           Control.Monad
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

multiUnion :: [Mover] -> Mover
multiUnion []     = nullMover
multiUnion (m:ms) = foldr (|+|) m ms

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
iterN f n m = iterate (f m) nullMover !! n

--repeat a mover up to n times
freeN :: Int -> Mover -> Mover
freeN = iterN (|.?|)

--repeat a mover exactly n times
freeNStrict :: Int -> Mover -> Mover
freeNStrict = iterN (|.|)

--repeat a mover until it does not change the list of moves
free :: Mover -> Mover
free m = MkMover {runMover = \b -> put [] >> runMover go b} where
    go = MkMover {runMover = \b -> do
        old <- get
        runMover m b
        new <- get
        let filtered = (b ^. boardFilter) (old ++ new)
        put filtered
        when (filtered /= old) $ runMover go b
    }

transformMv :: [Vec] -> Mover -> Mover
transformMv m mv = MkMover {runMover = \b -> do
    runMover mv b
    modify $ map $ transformMove m
    }

multiTransformMv :: [[Vec]] -> Mover -> Mover
multiTransformMv ms mv = multiUnion $ map (`transformMv` mv) ms
