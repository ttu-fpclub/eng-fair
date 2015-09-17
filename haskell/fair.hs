import Control.Monad

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = less ++ [x] ++ more
    where less = filter (< x) xs
          more = filter (>= x) xs

isPrime :: Integral a => a -> Bool
isPrime n = all ((/= 0) . (n `rem`)) [2 .. n - 1]

primes :: Integral a => [a]
primes = filter isPrime [2..]

gcd' :: Integral a => a -> a -> a
gcd' n 0 = n
gcd' n m = gcd' m (n `mod` m)

powerSet :: [a] -> [[a]]
powerSet = filterM $ const [True, False]

