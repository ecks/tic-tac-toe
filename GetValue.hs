module GetValue
  where

import Data.List
import Data.Tree
import DataTypes
import MyTree

-- since for each evaluation of a tree, we need to find the value in terms of one moves, 
-- so that the moves do not alternate between each other. Kinda ugly but works.
myTreeValue :: Tree TableMove -> Tree Float
myTreeValue (Node node subTree) = myTreeMap (getValue ((getOtherMove.accessCurrentMove) node)) (Node node subTree)

maximise :: (Ord a) => Tree (a,TableMove) -> (a,TableMove)
maximise = maximum . maximise'

-- this is called alpha-beta pruning...
maximise' :: (Ord a) => Tree (a,TableMove) -> [(a,TableMove)]
maximise' (Node node []) = [node]
maximise' (Node n l)     = mapmin (map minimise' l)
		where mapmin (nums:rest) = (minimum nums) : (omit1 (minimum nums) rest)

omit1 :: (Ord a) => a -> [[a]] -> [a]
omit1 pot [] = []
omit1 pot (nums:rest) = if minleq nums pot then omit1 pot rest 
					   else (minimum nums) : (omit1 (minimum nums) rest)

minleq :: (Ord a) => [a] -> a -> Bool
minleq [] pot = False
minleq (num:rest) pot = if num <= pot then True
				      else minleq rest pot


minimise :: (Ord a) => Tree (a,TableMove) -> (a,TableMove)
minimise = minimum . minimise'

minimise' :: (Ord a) => Tree (a,TableMove) -> [(a,TableMove)]
minimise' (Node node []) = [node]
minimise' (Node n l)     = mapmax (map maximise' l)
		where mapmax (nums:rest) = (maximum nums) : (omit2 (maximum nums) rest)

omit2 :: (Ord a) => a -> [[a]] -> [a]
omit2 pot [] = []
omit2 pot (nums:rest) = if maxleq nums pot then omit2 pot rest
					   else (maximum nums) : (omit2 (maximum nums) rest)

maxleq :: (Ord a) => [a] -> a -> Bool
maxleq [] pot = False
maxleq (num:rest) pot = if num >= pot then True
				      else maxleq rest pot


prune :: Integer -> Tree TableMove -> Tree TableMove
prune 0 (Node node x) = Node node []
prune n (Node node x) = Node node (map (prune (n-1)) x)

-- this function evaluates a position and returns a value, based on how good it is
-- The function first takes the sum of the rows columns or diagonals it can form based on 
-- the current move. It then takes the sum of the rows, columns or diagonals the opponent 
-- can form, and subtracts that from its original sum, which it returns. If the function finds
-- that it has formed a full row, column, or diagonal, it returns 1/0 (infinity), or if it finds 
-- the same condition for its opponent (it can't be both), it returns -1/0 (negative infinity).
-- If there is no more open spaces that it can use, the function returns 0.
getValue :: CurrentMove -> TableMove -> Float
getValue curr t = (getValueOfMove curr (accessMaybeMove t)) - (getValueOfMove (getOtherMove curr) (accessMaybeMove t))
	where getValueOfMove c l = (rowValues c l) + (rowValues c (transpose l)) + (diagValues c l) + (diagValues c (map reverse l))
				
rowValues :: Maybe Move -> [[Maybe Move]] -> Float
rowValues cMove table = if (any (==True) (findEndGame cMove table)) then 1/0 else foldr (\x y -> if x then (1+y) else (0+y)) 0 (findStatic cMove table)
	where findEndGame c m = map (all (\x -> x == c)) m
              findStatic  c m = map (all (\x -> (x == c) || (x == Nothing))) m

diagValues :: Maybe Move -> [[Maybe Move]] -> Float 
diagValues cMove xL = if (all (==cMove) (diag xL)) then 1/0 else if (all (\x -> (x == cMove) || (x == Nothing)) (diag xL)) then 1 else 0
