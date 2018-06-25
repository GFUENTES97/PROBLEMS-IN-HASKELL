countIf::(Int -> Bool) -> [Int] -> Int
countIf f l = foldr (\x y -> if f x then (y + 1) else y) 0 l

pam::[Int] -> [Int -> Int] -> [[Int]]
pam l f = map (\fx -> map fx l) f

pam2::[Int] -> [Int -> Int] -> [[Int]]
pam2 l f = map (\x -> map ($ x) f) l

filterFoldl::(Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl c f x l = foldl f x $ filter c l

insert::(Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f l x = takeWhile (not.f x) l ++ [x] ++ dropWhile (not.f x) l

insertionSort:: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort c l = foldr (\x xs -> insert c xs x) [] l