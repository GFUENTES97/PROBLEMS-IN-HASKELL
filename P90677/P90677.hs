myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ x [] = x
myFoldl f x (n:ns) = myFoldl f (f x n) ns

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr f x (n:ns) = f n $ myFoldr f x ns

myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : myIterate f (f n)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil b f n 
    | b n         = n
    | otherwise   = myUntil b f (f n)

myMap :: (a -> b) -> [a] -> [b]
myMap f l = myFoldr (\x xs -> (f x):xs) [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = myFoldr (\x xs -> if f x then x:xs else xs) [] xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll f l = and (myMap f l)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = or (myMap f l)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] j = []
myZip l [] = []
myZip (x:xs) (n:ns) = (x,n):myZip xs ns

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = myFoldr (\x xs -> (f (fst x) (snd x)):xs) [] $ myZip a b