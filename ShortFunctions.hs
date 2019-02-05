--applying a function twice to a parameter
twice :: (a -> a) -> a -> a
twice f x = f (f x)

--reminder of data types
data Tree = Branch Tree Tree | Tip Int

--method for generating triangular numbers
triangular :: [Integer]
triangular = [ div (x*y) 2 | (x,y) <- pairs]
nats = [0..]
pairs = zip nats (tail nats)

--getting n number of primes
takePrimes :: Int -> [Int]
takePrimes x = take x primes

--getting all primes under less than a number
getPrimesLessThan :: Int -> [Int]
getPrimesLessThan x = takeWhile (<x) primes

--producing all the primes
primes :: [Int]
primes = sieve [2..]

--making a list of primes given a starting number
sieve :: [Int] -> [Int]
sieve (x:xs) = x : sieve [a | a <- xs, mod a x /= 0]

--rewritting '++' in terms of ':'
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

--rewritting ':' as '£' in terms of '++'
(£) :: a -> [a] -> [a]
x £ xs = [x] ++ xs

--defining sum with foldr instead of using recursion
sum' :: [Int] -> Int
sum' = foldr (+) 0

--a function to zip multiple lists together using list comprehension
multiZip :: [[a]] -> [[a]]
multiZip [] = []
multiZip ([]:xss) = multiZip xss
multiZip ((x:xs): xss) = (x : [y|(y:_) <- xss]) : multiZip (xs:[ z | (_:z) <- xss])
