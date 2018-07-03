-- import GFUENTES97 

main = do
    name <- getLine
    putStrLn $ saluda name

saluda x
    | length(x) > 1  = saluda $ tail x
    | length(x) == 1 && (x == "a" || x == "A")   = "Hola maca!"
    | otherwise                                  = "Hola maco!"

{-- 
 - 
 -  last update: 
 - 
 -  03/07/2018 12:59:10
 - 
 --}