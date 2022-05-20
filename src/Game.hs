{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Game where

class Game b p m c | b -> c, p -> m, m -> p where
    getPieceAt :: b -> c -> p
    applyMove :: m -> b -> b
    getMoves :: b -> [m]
    
