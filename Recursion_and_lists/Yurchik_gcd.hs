gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

main = print (gcd' 56 98) -- выведет 14
