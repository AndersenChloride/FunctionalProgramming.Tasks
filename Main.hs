module Main (main) where

main :: IO ()
main = do 
    print "Solver of the quadratic equation. Enter a"
    a <- getLine
    print "Enter b"
    b <- getLine
    print "Enter c"
    c <- getLine
    print (output (quad (read a :: Double) (read b :: Double) (read c :: Double)))


data Custom  = Exist {ans1, ans2 :: Double}| Undef

quad :: Double -> Double -> Double -> Custom
quad a b c 
    | disk < 0 = Undef 
    | disk > 0 = Exist {ans1=x1, ans2=x2}
    where  disk = b^2 - 4 * a * c
           x1 = (- b - sqrt disk) / (2 * a)
           x2 = (- b + sqrt disk) / (2 * a)


output :: Custom -> String
output (Exist ans1 ans2) = if ans1 == ans2 then ("There is one root: x=" ++ show ans1) else ("There is two roots: x1=" ++ show ans1 ++ " x2=" ++ show ans2)
output (Undef) = "No roots"