isort::[Int] -> [Int]
isort []     = []
isort (x:xs) = insert (isort xs) x 	

insert::[Int] -> Int -> [Int]
insert [] x = [x]
insert (x:xs) n
	| x > n		= [n] ++ [x] ++ xs
	| otherwise = x : insert xs n

ssort::[Int] -> [Int]
ssort [] = []
ssort xs = [m] ++ ssort (remove xs m)
	where m = minimum xs

remove::[Int] -> Int -> [Int]
remove [] _ = []
remove (x:xs) n
	| x == n	= xs
	| otherwise = x:remove xs n

msort::[Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort a) (msort b)
	where a = take n xs; b = drop n xs; n = (div (length xs) 2)

merge::[Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	| x < y      = x : merge xs (y:ys)
	| y <= x     = y : merge (x:xs) ys

qsort::[Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort a) ++ [x] ++ (qsort b)
	where a = (filter (<=x)) xs; b = (filter (>x)) xs

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x : xs) = (genQsort a) ++ [x] ++ (genQsort b)
  where (a,b) = (filter (<=x) xs, filter (>x) xs)