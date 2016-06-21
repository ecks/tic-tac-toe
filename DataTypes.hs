module DataTypes
  where
 
import Data.List

data TableMove = TableMove Board History CurrentMove deriving (Show,Ord)
data Move = X | O deriving (Eq, Show, Enum, Ord)

type History = [Integer]
type CurrentMove = Maybe Move 
type Board = [[Maybe Move]]

size :: Int
size = 3

empty :: Board
empty =  replicate size (replicate size Nothing)

-- make use of symmetries
-- we try to see if there is a symmetry if the move we are about to make 
-- has across from it, either horizontally, vertically, or diagonally, a Nothing slot.
-- The only position that may change is the position of the move we are about to make.
-- If the symmetrical move is already included in the current ply, we may take it out.
instance Eq TableMove where
	(TableMove m1 h1 c1) == (TableMove m2 h2 c2) =
	  (horizontalSym m1 == m2) || (leftDiagonalSym m1 == m2) || (verticalSym m1 == m2) || (rightDiagonalSym m1 == m2)

horizontalSym = reverse
leftDiagonalSym = transpose
verticalSym = transpose.reverse.transpose
rightDiagonalSym = verticalSym.reverse.transpose

-- these are our accessors
accessMaybeMove :: TableMove -> [[Maybe Move]]
accessMaybeMove (TableMove m _ _) = m

accessHistory :: TableMove -> [Integer]
accessHistory (TableMove _ h _) = h

accessCurrentMove :: TableMove -> CurrentMove
accessCurrentMove (TableMove _ _ c) = c

getOtherMove :: CurrentMove -> CurrentMove 
getOtherMove (Just X) = Just (succ X)
getOtherMove (Just O) = Just (pred O)
getOtherMove Nothing  = Nothing

toTableMove :: [[Maybe Move]] -> History -> CurrentMove -> TableMove
toTableMove m h c = TableMove m h c

chop      :: Int -> [a] -> [[a]]
chop n [] =  []
chop n xs =  take n xs : chop n (drop n xs)

infinity :: Float
infinity = 1/0

-- gives us a list of the diagonal elements
diag 	 :: [[a]] -> [a]
diag xss =  [xss !! n !! n | n <- [0 .. length xss - 1]]
