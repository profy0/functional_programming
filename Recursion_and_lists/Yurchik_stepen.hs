power :: Int -> Int -> Int
power x 0 = 1
power x n
    | even n    = power (x * x) (n `div` 2)
    | otherwise = x * power (x * x) ((n - 1) `div` 2)
main = print (power 2 10)