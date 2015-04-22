import Data.List

listLen :: Integral b => [a] -> b
listLen [] = 0
listLen (x:xs) = 1 + (listLen xs)

listMean l = (sum l) / (fromIntegral $ listLen l)

makePal l = l ++ (reverse l)

checkPal [] = True
checkPal l
  | odd $ listLen l = (tail back) == reverse front
  | otherwise       = back == reverse front
  where n = listLen l `div` 2
        front = take n l
        back = drop n l

sortLen l = sortBy (\x y -> compare (listLen x) (listLen y)) l

intersp :: a -> [[a]] -> [a]
intersp _ []     = []
intersp _ (x:[]) = x
intersp s (x:xs) = x ++ [s] ++ intersp s xs

data MaybeTree a = MaybeNode a (Maybe (MaybeTree a)) (Maybe (MaybeTree a))
                   deriving (Show)
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + (max (treeHeight l) (treeHeight r))

insertT Empty x = Node x Empty Empty
insertT (Node v l r) x
  | x < v     = Node v (insertT l x) r
  | otherwise = Node v l (insertT r x)

a = foldr (\x t-> insertT t x) Empty [1,4,2,6,7,9,3,2]
b = foldl insertT Empty [1,4,2,6,7,9,3,2]

data Direction = LeftTurn
               | RightTurn
               | Straight
               deriving (Show)
data Point = Point Double Double
             deriving (Eq, Show)

getDir :: Point -> Point -> Point -> Direction
getDir (Point ax ay) (Point bx by) (Point cx cy)
  | ccw > 0   = LeftTurn
  | ccw < 0   = RightTurn
  | otherwise = Straight
  where ccw = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

getDirList :: [Point] -> [Direction]
getDirList (a:b:c:[]) = [getDir a b c]
getDirList (a:b:c:ps) = (getDir a b c) : getDirList (b:c:ps)
getDirList _          = []

p1 = Point 1.2 1.4
p2 = Point 2.0 7.2
p3 = Point 7.2 7.2
p4 = Point 8.0 7.2
p5 = Point 9.0 15.3
