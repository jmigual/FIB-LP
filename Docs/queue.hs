data Queue a = Queue [a] [a]
              deriving (Show)
              
instance Eq a => Eq (Queue a)
  where (==) = equals

equals :: Eq a => Queue a -> Queue a -> Bool
equals (Queue x []) (Queue y []) = x == y
equals (Queue x xs) (Queue y []) = equals (Queue (x ++ (reverse xs)) []) (Queue y [])
equals q1 (Queue y ys) = equals q1 (Queue (y ++ (reverse ys)) [])
  
  
 
create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue ys xs) = Queue ys (x:xs)

pop :: Queue a -> Queue a
pop (Queue [] []) = create
pop (Queue [] xs) = pop (Queue (reverse xs) [])
pop (Queue (x:xs) ys) = Queue xs ys

top :: Queue a -> a
top (Queue [] xs) = last xs
top (Queue (x:xs) _) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty q = False