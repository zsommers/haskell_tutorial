
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0     = []
  | otherwise  = (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOther' (reverse x))

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' []          = []
doubleEveryOther' (x:[])      = [x]
doubleEveryOther' (x:(y:xs))  = x : (2*y) : (doubleEveryOther' xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10     = x + (sumDigits xs)
  | otherwise  = (sumDigits (toDigits x)) + (sumDigits xs)

goodCard, badCard :: Integer
goodCard = 4012888888881881
badCard = 4012888888881882

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

lenHanoi :: (Integer -> [Move]) -> Integer -> Integer
lenHanoi f n  = fromIntegral $ length $ f n

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _  = []
hanoi n a b c  = (hanoi (n-1) a c b) ++ ((a, b) : (hanoi (n-1) c b a))

defaultHanoi :: Integer -> [Move]
defaultHanoi n  = hanoi n "a" "b" "c"

hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 _ _ _ _  = []
hanoi2 1 a b _ _  = [(a, b)]
hanoi2 n a b c d  = (hanoi2 (n `quot` 2) a d b c) ++
                    (hanoi (n - (n `quot` 2)) a b c) ++
                    (hanoi2 (n `quot` 2) d b a c)

defaultHanoi2 :: Integer -> [Move]
defaultHanoi2 n  = hanoi2 n "a" "b" "c" "d"

hanoiR :: Integer -> [Peg] -> (Integer -> Integer) -> [Move]
hanoiR 0 _ _ = []
hanoiR 1 (a : b : rest) _ = [(a, b)]
hanoiR n (a : b : c : rest) fk =
    hanoiR k (a : c : b : rest) fk ++  -- peg c now unavailable
    hanoiR (n - k) (a : b : rest) fk ++  -- move lower (n-k) discs to target (b)
    hanoiR k (c : b : a : rest) fk where
    k | null rest  = n - 1
      | otherwise  = fk n -- `quot` 2
