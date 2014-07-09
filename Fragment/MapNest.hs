{-******************************************
  *     File Name: MapNest.hs
  *        Author: neouniverse
  * Last Modified: 2014/07/09 14:36:24
  *******************************************-}

module HaskellGit.Fragment.MapNest (
   mapWith
  ,mapZipWith
  ,mapWith3
) where


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

