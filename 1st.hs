fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac(n-1)

hello :: IO()
hello = putStrLn "Hello World"

fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)

data Tree a = Node (Tree a) a (Tree a)
            | Empty
      deriving (Show)

test_tree :: Tree Int
test_tree = Node (Node Empty 1 Empty) 2 (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty))

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right
