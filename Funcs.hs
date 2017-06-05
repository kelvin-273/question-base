module Funcs
  ( (.:)
  , apply
  ) where

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

apply :: (a -> a) -> Int -> [a] -> [a]
apply _ _ [] = []
apply f n (x:xs)
    | n == 0 = f x : xs
    | n > 0 = x : apply f (n - 1) xs
    | n < 0 = x:xs

main :: IO ()
main = let
    range n = [0..n-1]
    drange x n = const x <$> range n
    test = (drange .: drange . range) 4 3 2
    in print $ (((apply .: apply) .: apply) (*3) 3 2 0) test
