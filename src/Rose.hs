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
compose a b = composeMany a [b]

composeMany :: Rose a -> [Rose a] -> Rose a
composeMany (Rose a as) bs = Rose a (bs ++ map (`composeMany` bs) as)
