{-******************************************
  *     File Name: MapNest.hs
  *        Author: neouniverse
  * Last Modified: 2014/07/04 15:55:56
  *******************************************-}

mapWith :: (a -> b -> c) -> [a] -> [b] -> [[c]]
mapWith f [] _ = []
mapWith f (x:xs) ys = map (f x) ys : mapWith f xs ys

mapZipWith :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [[d]]
mapZipWith f [] _ _ = []
mapZipWith f (x:xs) ys zs = zipWith (f x) ys zs : mapZipWith f xs ys zs

mapWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [[d]]
mapWith3 f [] _ _ = []
mapWith3 f _ [] _ = []
mapWith3 f (x:xs) (y:ys) zs = map (f x y) zs : mapWith3 f xs ys zs

