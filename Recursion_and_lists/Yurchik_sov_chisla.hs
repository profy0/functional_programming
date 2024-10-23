isPerfect :: Integer -> Bool
isPerfect n = (sum (divisors n) - n) == n
  where
    divisors x = [d | d <- [1..x], x `mod` d == 0]
main = print (isPerfect 28)