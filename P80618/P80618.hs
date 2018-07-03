-- import GFUENTES97 

data Queue a = Queue [a] [a]
    deriving (Show)

create:: Queue a
create = Queue [] []

push:: a -> Queue a -> Queue a
push x (Queue l r) = Queue l (x:r)

pop::Queue a -> Queue a
pop (Queue [] [])   = (Queue [] [])
pop (Queue [] r)    = pop $ shift (Queue [] r)
pop (Queue l r)     = Queue (tail l) r

top::Queue a -> a
top (Queue [] r) = top $ shift (Queue [] r)
top (Queue l r) = head l

empty::Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

len::Queue a -> Int
len (Queue l r) = length(l) + length(r)

shift :: Queue a -> Queue a
shift (Queue l r) = Queue (reverse r) l

instance Eq a => Eq (Queue a) where
    q1@(Queue ar al) == q2@(Queue br bl) 
        | len(q1) /= len(q2) = False
        | otherwise  = all (==True) $ zipWith (==) (ar ++ (reverse al)) (br ++ (reverse bl)) 

{-- 
 - 
 -  last update: 
 - 
 -  03/07/2018 12:59:10
 - 
 --}