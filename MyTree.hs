module MyTree 
  where
  
import DataTypes
import Data.Tree
import Data.Maybe
import Data.List
import Data.Ord

-- this is an implementation without monads:
myTreeMap :: (a -> b) -> Tree a -> Tree b
myTreeMap f (Node node subTree)  = Node (f node) (myTreeMap' f subTree)
			where myTreeMap' f []           = []
			      myTreeMap' f (first:rest) = (myTreeMap f first) : (myTreeMap' f rest)

-- this is an implementation with monads:
--myTreeMap :: (a -> b) -> Tree a -> Tree b
--myTreeMap f tree = do
--		    move <- tree
--		    return (f move)

-- regular zipper, except that during execution, if a node finds that it has won or lost,
-- further generation of the tree stops. We need this so that we dont get weird results when
-- we use minimax.
myCustomTreeZip :: Tree Float -> Tree TableMove -> Tree (Float,TableMove)
myCustomTreeZip (Node n1 s1) (Node n2 s2) = if n1 == 1/0 || n1 == -1/0 then Node (n1,n2) [] else Node (n1,n2) (myTreeZip' s1 s2)
			where myTreeZip' [] _ = []
			      myTreeZip' _ [] = []
			      myTreeZip' (f1:r1) (f2:r2) = (myCustomTreeZip f1 f2) : (myTreeZip' r1 r2)

gameTree :: Tree TableMove -> Tree TableMove
gameTree p = reptree moves p 

-- initialize a non-empty board
makeTableTree :: Board -> CurrentMove -> Tree TableMove
makeTableTree board currentMove = Node (toTableMove board (findHistory board) currentMove) []

-- given a board, returns the history of moves already taken
findHistory :: Board -> [Integer]
findHistory board = foldr (\(p,n) xL -> if p == (Just O) || p == (Just X) then n:xL else xL) [] (zip (concat board) [0..]) 

reptree :: (TableMove -> [Tree TableMove]) -> Tree TableMove -> Tree TableMove
reptree f (Node node []) =
				Node node (map (reptree f) (f node))

-- function responsible for generating a tree and making the input the parent 
-- of all the other subtrees. It does this by getting each possibility of the input
-- excluding the ones in the history that have already been processed. It then proceeds 
-- to remove the symmetries from the generated possibilities, after which it it converts 
-- the TableMove to a Tree of TableMove. For each input, it generates all the possible moves 
-- that the node can make in a list.
moves :: TableMove -> [Tree TableMove]
moves thisNode = map (makeLeafs) (removeSymmetries (map (possibilities thisNode) ([0..8] \\ (accessHistory thisNode))))

-- makes a TableMove into a Tree of TableMove with empty leafs
makeLeafs :: TableMove -> Tree TableMove
makeLeafs (xs) = Node xs [] 

-- a container that interfaces with the possibly function, converting
-- the data into its appropriate type. The Integer is the move placeholder 
-- that we want to place a X or O.
possibilities :: TableMove -> Integer -> TableMove
possibilities node n = toTableMove (possibly (accessMaybeMove node) n (accessCurrentMove node)) (n : (accessHistory node)) (getOtherMove (accessCurrentMove node))

-- breaks down the current TableMove into three rows,
-- with x being the first, y being the second, and z being the third.
-- xs is an empty list. Note: I could probably simplify this to a lot 
-- less code using the chop function.
possibly :: [[Maybe Move]] -> Integer -> Maybe Move -> [[Maybe Move]]
possibly (x:y:z:xs) n currentMove 
		  | n >= 0 && n <= 2 = possibly' x (n `mod` 3) : y : z : xs
		  | n >= 3 && n <= 5 = x : possibly' y (n `mod` 3) : z : xs
		  | n >= 6 && n <= 8 = x : y : possibly' z (n `mod` 3) : xs
		  | otherwise        = x : y : z : xs
                where trulyNothing elem  = if isNothing elem then (getOtherMove currentMove) else elem
		      possibly' (x:y:z:xs) m
		        | m == 0 = trulyNothing x : y : z : xs
		        | m == 1 = x : trulyNothing y : z : xs
		        | m == 2 = x : y : trulyNothing z : xs
		        | otherwise = x : y : z : xs

removeSymmetries :: [TableMove] -> [TableMove]
removeSymmetries []     = []
removeSymmetries (x:xL) = x : removeSymmetries (filter (/= x) xL)
