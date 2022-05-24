module Rose where

data Rose a = Rose a [Rose a] deriving (Eq, Show)

instance Functor Rose where
    fmap f (Rose a as) = Rose (f a) (map (fmap f) as)

instance Applicative Rose where
    pure a = Rose a []
    (Rose f fs) <*> ra@(Rose a as) =
        Rose (f a) (map (f <$>) as ++ map (<*> ra) fs)

instance Monad Rose where
    return = pure
    (>>=) = join .: flip fmap where
        (.:) = (.).(.)
        join (Rose (Rose value roses) roseroses) = Rose value (roses ++ map join roseroses)


compose :: Rose a -> Rose a -> Rose a
compose (Rose a as) b = Rose a (b : map (`compose` b) as)

flaggedUnion :: Eq a => a -> Rose a -> Rose a -> Rose a
flaggedUnion flag ra@(Rose a as) rb@(Rose b bs)
    | a == flag && b == flag = Rose flag (as ++ bs)
    | a == flag = Rose flag (rb : as)
    | b == flag = Rose flag (ra : bs)
    | otherwise = Rose flag [ra,rb]
