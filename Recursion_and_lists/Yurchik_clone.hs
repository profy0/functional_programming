clone :: Int -> [a] -> [a]
clone n = concatMap (replicate n)
main = do
  print (clone 3 [1, 2, 3])
  print (clone 1 [1, 2, 3])
  print (clone 0 [1, 2, 3])