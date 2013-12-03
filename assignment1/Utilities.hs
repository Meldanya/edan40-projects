module Utilities where

-- | Applies a pair of functions on a pair of arguments, respectivly.
--   The resulting value is a pair.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- | Constucts a Maybe instance from the result of applying f to x.
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- | Take the first argument that is not Nothing. Could be chained.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- | Return the result of a function application if result not Nothing,
--   otherwise the parameter.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- | Returns the value which the function f converges towards.
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- | Indexes a list by fraction.
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
