module Mover where

import           Control.Monad.RWS
import           Types
import           Vec

--no moves
nullMover :: Mover
nullMover = MkMover {runMover = \_ -> return []}

--only move is a move with no atoms
emptyMover :: Mover
emptyMover = MkMover {runMover = \_ -> return [Move []]}

--only move is the zero move
zeroMover :: Mover
zeroMover = leaper vzero

--mover with a single direction of movement
leaper :: Vec -> Mover
leaper v = MkMover {runMover = \_ -> return [Move [Atom v MoveTo]]}

--union
(|+|) :: Mover -> Mover -> Mover
x |+| y = MkMover {runMover = \b -> liftM2 (++) (runMover x b) (runMover y b)}

--mandatory compose, that is "do x then do y"
(|.|) :: Mover -> Mover -> Mover
x |.| y = MkMover {runMover = \b -> do
    xs <- runMover x b
    put xs
    ys <- runMover x b
    return [concatMoves xm ym | xm <- xs, ym <- ys]
    }

--optional compose, that is "do x and then you may or may not do y"
(|.?|) :: Mover -> Mover -> Mover
a |.?| b = a |.| (emptyMover |+| b)
