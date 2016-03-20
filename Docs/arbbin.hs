data Tree a = Node a (Tree a) (Tree a)
            | Empty 
            deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node x fillE fillD) = 1 + size fillE + size fillD

height :: Tree a -> Int
height Empty = 0
height (Node x fillE fillD) = 1 + max (height fillE) (height fillD)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node x xE xD) (Node y yE yD) = (x == y) && (equal xE yE) && (equal xD yD)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node x xE xD) (Node y yE yD) = (x == y) && (((isomorphic xE yE) && (isomorphic xD yD)) || ((isomorphic xE yD) && (isomorphic xD yE)))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x xE xD) = x:((preOrder xE) ++ (preOrder xD))

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x xE xD) = ((postOrder xE) ++ (postOrder xD)) ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x xE xD) = (inOrder xE) ++ (x:(inOrder xD))

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst t = bfs [t]
  where 
    bfs :: [Tree a] -> [a]
    bfs [] = []
    bfs (Empty:xs) = bfs xs
    bfs ((Node x xE xD):xs) = x:(bfs (xs ++ [xE,xD]))

build :: Eq a => [a] -> [a] -> Tree a
build xs ys = t
  where 
    (as, bs, t) = buildr xs ys
    
    buildr :: Eq a => [a] -> [a] -> ([a], [a], Tree a)
    buildr _ [] = ([], [], Empty)
    buildr [] _ = ([], [], Empty)
    buildr (x:p) (y:i)
      | x == y      = (p, i, (Node x Empty Empty))
      | otherwise   = (p2, i2, (Node x t1 t2))
      where  
        i' = [n | n<-(y:i), n /= x]
        (p1, i1, t1) = buildr p i'
        (p2, i2, t2) = buildr p1 i1
        
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap f Empty t = t
overlap f t Empty = t
overlap f (Node x xE xD) (Node y yE yD) = Node (f x y) (overlap f xE yE) (overlap f xD yD)