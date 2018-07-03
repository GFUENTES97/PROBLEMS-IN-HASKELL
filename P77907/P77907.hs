-- import GFUENTES97 

absValue :: Int -> Int
absValue n = 
    if n < 0 then n*(-1)
    else n
    
power:: Integer -> Integer -> Integer
power x n 
    | n == 0    = 1
    | even n    = y * y
    | otherwise = y * y * x
    where y = power x (div n 2)
          
busca:: Integer -> Integer -> Bool
busca n m
    | m > (div n 2)        = True
    | mod n m > 0          = busca n (m + 1)
    | otherwise = False
          
isPrime:: Integer -> Bool
isPrime n
    | n == 0    = False
    | n == 1    = False
    | otherwise = busca n 2
    
slowFib:: Integer -> Integer
slowFib n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise   = slowFib (n-1) + slowFib(n-2)

suma:: (Integer, Integer) -> Integer -> Integer
suma (a,b) n
    | n == 2    = (a + b)
    | otherwise = suma(b, (b+a)) (n-1)

    
quickFib:: Integer -> Integer
quickFib n 
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = suma (0,1) n
          
    
    
    
    


{-- 
 - 
 -  last update: 
 - 
 -  03/07/2018 14:05:30
 - 
 --}