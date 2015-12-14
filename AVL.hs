module AVL
( Tree
, newTree
, left
, right
, value
, btmin
, depth
, inorder
, listToTree
, balancedFactor
, search
, getElem
, leftLeft
, leftRight
, rightLeft
, rightRight
, insert
, delete
) where

data Tree = EmptyTree | Node Int Tree Tree deriving (Show, Eq, Ord)

-- Creates a new Tree with the passed Int as the root
newTree :: Int -> Tree
newTree x = Node x EmptyTree EmptyTree

left (Node _ t _) = t
right (Node _ _ u) = u
value (Node v _ _) = v

btmin = head . inorder

-- Takes a Tree and returns it's maximum depth
depth :: Tree -> Int
depth EmptyTree = 0
depth (Node a left right) = 1 + (max (depth left) (depth right))

-- This will take a Tree and return it in the form of a list in order
inorder :: Tree -> [Int]
inorder EmptyTree = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

-- Takes a list and turns it into a Tree from the right
listToTree :: [Int] -> Tree
listToTree x = foldr insert EmptyTree x

balancedFactor :: Tree -> Tree -> Int
balancedFactor x y = (depth x) - (depth y)

-- This function will take a Tree and an Int and return a list
-- that will give you a path to that Int if it's in the list
-- where 0 means left and 1 means right. Nothing means it's Not
-- in the list
search :: Tree -> Int -> Maybe [Int]
search EmptyTree x = Nothing
search (Node i left right) x
	| i == x										  = Just []
	| (search left x) /= Nothing  = fmap((:) 0) (search left x)
 	| (search right x) /= Nothing = fmap ((:) 1) (search right x)
	| otherwise 									= Nothing

-- The list of Int's in this function should be the path created
-- by the search function. Will return the element at that location
getElem :: Tree -> [Int] -> Maybe Int
getElem EmptyTree _ = Nothing
getElem (Node x left right) [] = Just x
getElem (Node x left right) (y:ys)
	| y == 0  	= getElem left ys
	| otherwise = getElem right ys

-- 4 different types of balances
leftLeft (Node value (Node subValue subLeft subRight) right) = (Node subValue subLeft (Node value subRight right))

leftRight (Node x (Node a ileft (Node b gleft gright)) right) = (Node b (Node a ileft gleft)(Node x gright right))

rightLeft (Node value left (Node subValue (Node subSubValue subSubLeft subSubRight) subSubRig)) = (Node subSubValue (Node value left subSubLeft) (Node subValue subSubRight subSubRig))

rightRight (Node value left (Node subValue subLeft subRight)) = (Node subValue (Node value left subLeft) subRight)

-- Balanced insert
insert :: Int -> Tree -> Tree
insert i EmptyTree = (Node i EmptyTree EmptyTree)
insert i (Node v t u)
    | i == v = (Node v t u)
    | i < v && (balancedFactor ti u) ==  2 && i < value t = leftLeft (Node v ti u)
    | i < v && (balancedFactor ti u) ==  2 && i > value t = leftRight (Node v ti u)
    | i > v && (balancedFactor t ui) == -2 && i < value u = rightLeft (Node v t ui)
    | i > v && (balancedFactor t ui) == -2 && i > value u = rightRight (Node v t ui)
    | i < v  = (Node v ti u)
    | i > v  = (Node v t ui)
        where ti = insert i t
              ui = insert i u

-- Balanced delete
delete :: Tree -> Int -> Tree
delete EmptyTree d = EmptyTree
delete (Node v EmptyTree EmptyTree) d = if v == d then EmptyTree else (Node v EmptyTree EmptyTree)
delete (Node v t EmptyTree) d = if v == d then t else (Node v t EmptyTree)
delete (Node v EmptyTree u) d = if v == d then u else (Node v EmptyTree u)
delete (Node v t u) d
    | v == d                            = (Node mu t dmin)
    | v > d && abs (balancedFactor dt u) < 2 = (Node v dt u)
    | v < d && abs (balancedFactor t du) < 2 = (Node v t du)
    | v > d && (balancedFactor (left u) (right u)) < 0 = rightRight (Node v dt u)
    | v < d && (balancedFactor (left t) (right t)) > 0 = leftLeft (Node v t du)
    | v > d                                       = rightLeft (Node v dt u)
    | v < d                                       = leftRight (Node v t du)
        where dmin = delete u mu
              dt   = delete t d
              du   = delete u d
              mu   = btmin u
