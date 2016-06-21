module GetAgentMove
  where

import MyTree
import GetValue
import DataTypes
import Data.Tree

-- This is the main driver function of the AI. It uses a composition of functions.
ans :: Board -> CurrentMove -> Integer -> TableMove
ans board currentMove diff = backtrackWith (findHistory board) $ maximise $ 
			myCustomTreeZip (myTreeValue $ prune diff $ gameTree (makeTableTree board currentMove)) (prune diff $ gameTree (makeTableTree board currentMove))

--test board currentMove = maximise $ myCustomTreeZip (myTreeValue $ prune 4 $ gameTree (makeTableTree board currentMove)) (prune 4 $ gameTree (makeTableTree board currentMove))

backtrackWith :: [Integer] -> (Float,TableMove) -> TableMove
backtrackWith h (m,tableMove) = remove h tableMove

remove :: [Integer] -> TableMove -> TableMove
remove h tableMove = if tail (accessHistory tableMove) == h then tableMove 
	  	                                            else (remove h (toTableMove (chop size (xs ++ (Nothing : ys))) 
							           (tail (accessHistory tableMove)) ((getOtherMove . accessCurrentMove) tableMove)))
		     where (xs,y:ys) = splitAt (fromInteger $ head (accessHistory tableMove)) (concat (accessMaybeMove tableMove))
