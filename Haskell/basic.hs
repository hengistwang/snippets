-- |

module Basic where

-- variable (immutable)
one :: Int
one = 1

two :: Int
two = 2

double :: Integer -> Integer
double x = x + x

quadruple :: Integer -> Integer
quadruple = double .double

distance x1 y1 x2 y2 = sqrt (square diffx + square diffy)
  where
    square x = x * x
    diffx = x1 - x2
    diffy = y1 - y2

isZero 0 = True
isZero _ = False

sumTo 1 = 1
sumTo n = n + sumTo (n - 1)

power n 0 = 1
power n k = n * power n (k - 1)

ilog3 0 = 0
ilog3 n = 1 + ilog3 (div n 3)

-- Lists
years = [2000,2024]

takeFinal n xs = reverse (take n (reverse xs))
updateAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

substring i j s = take (j-1) (drop i s)

isPalindrome s = s == reverse s

myGcd 0 y = y
myGcd x 0 = x
myGcd x y
  | x < y = myGcd x (y-x)
  |otherwise = myGcd (x-y) y

smallestDivisor n = go n 2
  where
    go n k = if mod n k == 0 then k else go n (k + 1)

isPrime 0 = False
isPrime 1 = False
isPrime n = smallestDivisor n == n

biggestPrimeAtMost n = if isPrime n then n else biggestPrimeAtMost (n-1)
