-- Тип "двоичное дерево"
data Tree a = Empty                   -- Пустое дерево
            | Node a (Tree a) (Tree a) -- Узел с левым и правым поддеревом
            deriving (Show, Eq)

-- Функция map для дерева
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x left right) =
  Node (f x) (treeMap f left) (treeMap f right)

-- Подсчёт элементов дерева
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) =
  1 + treeSize left + treeSize right

-- Обход в глубину (depth-first traversal)
treeTraverseD :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseD _ acc Empty = acc
treeTraverseD f acc (Node x left right) =
  let accLeft = treeTraverseD f acc left
      accRoot = f x accLeft
  in treeTraverseD f accRoot right

-- Обход в ширину (breadth-first traversal)
treeTraverseW :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseW f acc tree = go f acc [tree]
  where
    go _ acc [] = acc
    go f acc (Empty : xs) = go f acc xs
    go f acc (Node x left right : xs) =
      let accRoot = f x acc
      in go f accRoot (xs ++ [left, right])

-- Примеры использования
exampleTree :: Tree Int
exampleTree =
  Node 1
    (Node 2
      (Node 4 Empty Empty)
      (Node 5 Empty Empty))
    (Node 3
      Empty
      (Node 6 Empty Empty))

-- Пример преобразования дерева с помощью treeMap
exampleMap :: Tree Int
exampleMap = treeMap (*2) exampleTree -- Удвоение всех значений

-- Пример подсчёта элементов
exampleSize :: Int
exampleSize = treeSize exampleTree

-- Пример обхода в глубину с аккумуляцией суммы
exampleSumD :: Int
exampleSumD = treeTraverseD (+) 0 exampleTree

-- Пример обхода в ширину с аккумуляцией суммы
exampleSumW :: Int
exampleSumW = treeTraverseW (+) 0 exampleTree

main :: IO ()
main = do
  putStrLn "Original tree:"
  print exampleTree
  putStrLn "\nTree after map (*2):"
  print exampleMap
  putStrLn "\nSize of tree:"
  print exampleSize
  putStrLn "\nSum of elements (depth-first traversal):"
  print exampleSumD
  putStrLn "\nSum of elements (breadth-first traversal):"
  print exampleSumW
