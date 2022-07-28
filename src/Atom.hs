module Atom where

import           Types
import           Vec

transformAtom :: [Vec] -> Atom -> Atom
transformAtom m (Move v) = Move $ transform m v
transformAtom m (On p a) = On p (transformAtom m a)
transformAtom _ a@(ChangePieceState _ _) = a
transformAtom _ a@(ChangeGlobalState _ _) = a
transformAtom m (Endow v a) = Endow (transform m v) (transformAtom m a)

getPrimaryVectorOf :: Atom -> Maybe Vec
getPrimaryVectorOf (Move v)                = Just v
getPrimaryVectorOf (On _ a)                = getPrimaryVectorOf a
getPrimaryVectorOf (ChangePieceState _ _)  = Nothing
getPrimaryVectorOf (ChangeGlobalState _ _) = Nothing
getPrimaryVectorOf (Endow v _)             = Just v
