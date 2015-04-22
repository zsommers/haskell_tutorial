

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

listLength :: [a] -> Integer
listLength []      = 0
listLength (x:xs)  = 1 + listLength xs

hailstone :: Integer -> Integer
hailstone n
  | isEven n   = n `div` 2
  | otherwise  = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

hailstoneLen :: Integer -> Integer
hailstoneLen n = listLength (hailstoneSeq n) - 1

