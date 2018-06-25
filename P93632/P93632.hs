eql :: [Int] -> [Int] -> Bool
eql x y = (length x) == (length y) && all (==True) (zipWith (==) x y)

prod:: [Integer] -> Integer
prod xs = foldl (\acc n -> acc*n) 1 xs

prodOfEvens:: [Integer] -> Integer
prodOfEvens xs = prod (filter (even) xs)
    
powersOf2:: [Integer]
powersOf2 = iterate (2*) 1

scalarProduct:: [Float] -> [Float] -> Float
scalarProduct xs ns = foldl (\acc n -> acc + n) 0 (zipWith (*) xs ns)
