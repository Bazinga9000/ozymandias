module Mover where

import           Atom
import           Control.Lens
import           Control.Monad
import           Control.Monad.RWS
import qualified Data.Map          as M
import           Rose
import           Types
import           Vec

{----------------------------------------------
PRIMITIVE MOVERS
----------------------------------------------}
--no moves
nullMover :: Mover
nullMover = MkMover {runMover = \_ -> put []}

--only move is a move with no atoms
emptyMover :: Mover
emptyMover = MkMover {runMover = \_ -> put [return Nothing]}

--only move is the zero move
zeroMover :: Mover
zeroMover = leaper vzero

--mover that can go anywhere
universalMover :: Mover
universalMover = MkMover {runMover = \b ->
    put $ map (moveByVec . fromP) (M.keys $ b ^. grid)
    }

moveByVec :: Vec -> Rose (Maybe Atom)
moveByVec v = return $ Just $ Move v

--mover with a single direction of movement
leaper :: Vec -> Mover
leaper v = MkMover {runMover = \_ -> put [moveByVec v]}

{----------------------------------------------
MOVER OPERATIONS
----------------------------------------------}

--runs two movers with the same state, then merges their outputs
--TODO: there is no way in hell that this is the best way to write this
fork :: (UnresolvedMoves -> UnresolvedMoves -> UnresolvedMoves) -> Mover -> Mover -> Mover
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
    put $ map (`composeMany` ys) xs
    }

--optional compose, that is "do x and then you may or may not do y"
(|.?|) :: Mover -> Mover -> Mover
a |.?| b = a |.| (emptyMover |+| b)

{-
--filter out moves by some predicate
filterMv :: Mover -> (Move -> Bool) -> Mover
filterMv m f = MkMover {runMover = \b -> do
    runMover m b
    modify $ filter f
    }
-}

{----------------------------------------------
ATOM MODIFICATION
----------------------------------------------}

changeAllAtoms :: (Atom -> Atom) -> Mover -> Mover
changeAllAtoms f m = MkMover {runMover = \b -> do
    runMover m b
    modify $ map $ fmap (fmap f)
}

--transform the vectors of a move as described by a given matrix
transformMv :: [Vec] -> Mover -> Mover
transformMv m = changeAllAtoms (transformAtom m)

--union of transforms from different matrices
multiTransformMv :: [[Vec]] -> Mover -> Mover
multiTransformMv ms mv = multiUnion $ map (`transformMv` mv) ms

{----------------------------------------------
MOVER ITERATION
----------------------------------------------}

--iteratively apply the function f n times
iterN :: (Mover -> Mover -> Mover) -> Int -> Mover -> Mover
iterN f n m = iterate (f m) nullMover !! n

--repeat a mover up to n times
freeN :: Int -> Mover -> Mover
freeN = iterN (|.?|)

--repeat a mover exactly n times
freeNStrict :: Int -> Mover -> Mover
freeNStrict = iterN (|.|)

--repeat a mover n times for all n
free :: Mover -> Mover
free m = m |.?| free m
