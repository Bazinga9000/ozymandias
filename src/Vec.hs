module Vec where

newtype Vec = Vec [Int] deriving Eq

vmap :: (Int -> Int -> Int) -> Vec -> Vec -> Vec
vmap f (Vec a) (Vec b) = Vec $ vzipwith f a b where
    vzipwith f [] []         = []
    vzipwith f [] (b:bs)     = f 0 b : vzipwith f [] bs
    vzipwith f (a:as) []     = f a 0 : vzipwith f as []
    vzipwith f (a:as) (b:bs) = f a b : vzipwith f as bs

(.+) :: Vec -> Vec -> Vec
(.+) = vmap (+)

(.-) :: Vec -> Vec -> Vec
(.-) = vmap (-)

(>*) :: Int -> Vec -> Vec
n >* Vec a = Vec $ map (*n) a

vzero :: Vec
vzero = Vec []

vsum :: [Vec] -> Vec
vsum = foldr (.+) vzero

transform :: [Vec] -> Vec -> Vec
transform vs (Vec x) = vsum $ zipWith (>*) x vs

newtype Pos = Pos [Int] deriving (Eq, Ord, Show)

fromP :: Pos -> Vec
fromP (Pos p) = Vec p

toP :: Vec -> Pos
toP (Vec p) = Pos p

addP :: Pos -> Vec -> Pos
addP p v = toP $ fromP p .+ v
