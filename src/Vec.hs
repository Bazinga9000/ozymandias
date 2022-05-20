module Vec where

newtype Vec = Vec [Int]

vmap :: (Int -> Int -> Int) -> Vec -> Vec -> Vec
vmap f (Vec a) (Vec b) = Vec $ zipWith f a b

(.+) :: Vec -> Vec -> Vec
(.+) = vmap (+)

(.-) :: Vec -> Vec -> Vec
(.-) = vmap (-)

(>*) :: Int -> Vec -> Vec
n >* Vec a = Vec $ map (*n) a
