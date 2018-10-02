-- Solutions in Haskell

-- https://projecteuler.net/problem=1
threeAndFive = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

-- https://projecteuler.net/problem=2
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
evenFibs = [x | x <- fibs, (not (odd x))]
sumEvenFibs x = sum (takeWhile (<x) evenFibs)

-- https://projecteuler.net/problem=3
primeFactors n = [x | x <- [1..(isqrt n)], mod n x == 0, isPrime x]
isPrime n = prime n 2
  where
    prime n i
      | n <= 1       = False
      | n == 2       = True
      | mod n i == 0 = False
      | i * i > n    = True
      | otherwise    = prime n (succ i)
isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral

-- https://projecteuler.net/problem=4
isPalindrome x = show x == reverse (show x)
is3 x = any (\n -> func1 n && func2 n) [100..999]
  where func1 n = mod x n == 0
        func2 n = length (show (div x n)) == 3 
largestPali = head [x | x <- [999^2, 999^2-2..], isPalindrome x, is3 x]

-- https://projecteuler.net/problem=5
divBy20 x = all (\n -> mod x n == 0) [1..20]
oneThroughTwenty = head [x | x <- [20, 40..10000000000], divBy20 x]

-- https://projecteuler.net/problem=6
sumSquares = sum [x^2 | x <- [1..100]]
squareSum = (sum [x | x <- [1..100]])^2
difference = squareSum - sumSquares

-- https://projecteuler.net/problem=7
largePrime = [n | n <- [2, 3..150000], isPrime n] !! 10000