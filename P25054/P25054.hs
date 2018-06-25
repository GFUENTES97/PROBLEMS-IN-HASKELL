import Data.List


myMaximum:: [Integer] -> Integer
myMaximum [x] = x
myMaximum (x:xs)
    | m > x     = m
    | otherwise = x
    where m = myMaximum xs
     
myLength:: [Integer] -> Integer
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs
     
sumatori::[Integer] -> Integer
sumatori [x] = x
sumatori (x:xs) = x + sumatori xs


average:: [Integer] -> Float
average l = fromIntegral (foldr (+) 0 l) / fromIntegral(myLength l)

reverseList:: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]


buildPalindrome:: [Integer] -> [Integer]
buildPalindrome (xs) = (ys)
	where ys = (reverseList xs) ++ xs

flatten::[[Int]] -> [Int]
flatten [] = []
flatten (xs) = foldl (++) [] xs

remove::[Int] -> [Int] -> [Int]
remove [] _ = []
remove (x:xs) n
	| elem x n  = remove xs n
	| otherwise = x:remove xs n

odds::[Integer] -> [Integer]
odds [] = []
odds (x:xs)
	| even x 	= odds xs
	| otherwise = x: odds xs

evens::[Integer] -> [Integer]
evens [] = []
evens (x:xs)
	| even x 	= x:evens xs
	| otherwise = evens xs

oddsNevens::[Integer] -> ([Integer],[Integer])
oddsNevens [] = ([],[])
oddsNevens xs = ((odds xs),(evens xs))

primeDivisors:: Integer -> [Integer]
primeDivisors 1 = []
primeDivisors x
	| d == [] = [x]
	| otherwise = nub (d ++ primeDivisors(div x (head d)))
	where d = take 1 $ filter (\y -> (mod x y) == 0) [2 .. (x-1)]