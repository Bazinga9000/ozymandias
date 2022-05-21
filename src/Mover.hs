module Mover where

import           Control.Monad.Reader
import           Types
import           Vec

--no moves
nullMover :: Mover
nullMover = MkMover {runMover = \_ _ -> return []}

--only move is the zero move
zeroMover :: Mover
zeroMover = leaper vzero

--mover with a single direction of movement
leaper :: Vec -> Mover
leaper v = MkMover {runMover = \_ _ -> return [Move [Atom v MoveTo]]}

--union
(|+|) :: Mover -> Mover -> Mover
x |+| y = MkMover {runMover = \b m -> liftM2 (++) (runMover x b m) (runMover y b m)}

--mandatory compose, that is "do x then do y"
(|.|) :: Mover -> Mover -> Mover
x |.| y = MkMover {runMover = \b m -> do
    xs <- runMover x b m
    Move atoms <- runMover x b m
    }
