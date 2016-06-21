-- Copyright (c) 2006, Wouter Swierstra
-- All rights reserved.
-- This code is released under the BSD license
--   included in this distribution

-- Imports

import IO
import Maybe
import Data.List
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Data.IORef
import Control.Monad

-- my own imports
import GetAgentMove
import DataTypes
import MyTree

-- With n being the square that we want to move to, p being the 
-- actual move, and b being the current board, we return 
-- a board with the move placed on the square
move       :: Int -> CurrentMove -> Board -> Maybe Board
move n p b = case y of
	       Nothing     -> Just (chop size (xs ++ (p : ys)))
	       _         -> Nothing
	       where
                 (xs,y:ys) = splitAt n (concat b)

full   :: Board -> Bool
full b =  all (all (/= Nothing)) b

wins	 ::  CurrentMove -> Board -> Bool
wins p b =  any (all (== p)) b
	    || any (all (== p)) (transpose b)
	    || all (== p) (diag b)
	    || all (== p) (diag (reverse b))

won   :: Board -> Bool
won b =  wins (Just O) b || wins (Just X) b

-- The state and GUI

data State = State {
  board :: Board,
  turn :: CurrentMove,
  difficulty :: Integer
}

data GUI = GUI {
  disableBoard :: IO (),
  resetBoard :: IO (),
  setSquare :: Int -> CurrentMove -> IO (),
  setStatus :: String -> IO ()
}

-- reset the game
reset gui (State board turn difficulty) = do
  setStatus gui "Player Cross: make your move."
  resetBoard gui
  return (State empty (Just X) difficulty)

-- when a square is clicked on, try to make a move.
--   if the square is already occupied, nothing happens
--   otherwise, update the board, let the next player make his move,
--     and check whether someone has won or the board is full.
occupy gui square st@(State board currentMove difficulty) = do
  case move square currentMove board of
    Nothing -> return st
    Just newBoard -> do
      setSquare gui square currentMove
      handleMove gui newBoard currentMove
      return (State newBoard currentMove difficulty)

-- main function responsible for controlling the AI of the game
-- if the previous move has resulted in a win or a full board,
-- then we return the State which was passed initially.
-- Otherwise we ask the agent for a move to return, and set its controls.
getAgentAns gui st@(State board currentMove difficulty) = do
  setSquare gui (fromInteger $ head $ accessHistory newBoard) (accessCurrentMove newBoard)
  handleMove gui (accessMaybeMove newBoard) (accessCurrentMove newBoard)
  return (State (accessMaybeMove newBoard) (getOtherMove (accessCurrentMove newBoard)) difficulty)
  where newBoard = if (wins currentMove board) || (full board) then toTableMove board (findHistory board) currentMove 
  							       else ans board currentMove difficulty
  

-- we set the difficulty into the State Monad
setDifficulty newDifficulty st@(State board currentMove difficulty) = do
  return (State board currentMove newDifficulty)

-- check whether a board is won or full
handleMove gui board currentMove
  | wins currentMove board = do
      setStatus gui ("Player "  ++ thisMove currentMove ++ " wins!")
      disableBoard gui
  | full board = do
      setStatus gui "It's a draw."
      disableBoard gui
  | otherwise = do
      setStatus gui ("Player " ++ thisMove (getOtherMove currentMove) ++ ": make your move")
  where thisMove (Just X) = "Cross"
        thisMove (Just O) = "Nought"

main = do
  initGUI

  -- Extract widgets from the glade xml file
  Just xml <- xmlNew "noughty.glade"

  window <- xmlGetWidget xml castToWindow "window"
  window `onDestroy` mainQuit

  newGame <- xmlGetWidget xml castToMenuItem "newGame"
  quit <- xmlGetWidget xml castToMenuItem "quit"

  easy <- xmlGetWidget xml castToMenuItem "easy"
  medium <- xmlGetWidget xml castToMenuItem "medium"
  hard <- xmlGetWidget xml castToMenuItem "hard"

  squares <- flip mapM [1..9] $ \n -> do
    square <- xmlGetWidget xml castToButton ("button" ++ show n)
    -- we set this in the glde file but it doesn't seem to work there.
    set square [ widgetCanFocus := False ]
    return square

  images <- flip mapM [1..9] $ \n -> do
    xmlGetWidget xml castToImage ("image" ++ show n)

  statusbar <- xmlGetWidget xml castToStatusbar "statusbar"
  ctx <- statusbarGetContextId statusbar "state"
  statusbarPush statusbar ctx "Player Cross: make your move."

  -- Construct the GUI actions that abstracts from the actual widgets
  gui <- guiActions squares images statusbar ctx

  -- Initialize the state
  state <- newIORef State { board = empty, turn = (Just X), difficulty = 1 }
  let modifyState f = readIORef state >>= f >>= writeIORef state

  -- Add action handlers
  onActivateLeaf quit mainQuit
  onActivateLeaf newGame $ modifyState $ reset gui

  onActivateLeaf easy $ modifyState $ setDifficulty 2 
  onActivateLeaf medium $ modifyState $ setDifficulty 4 
  onActivateLeaf hard $ modifyState $ setDifficulty 6 
  
  zipWithM_ (\square i ->
    onPressed square $ do
    			modifyState $ occupy gui i
                        modifyState $ getAgentAns gui)
    squares [0..8]

  widgetShowAll window
  mainGUI

guiActions buttons images statusbar ctx = do
  noughtPic <- pixbufNewFromFile "Nought.png"
  crossPic  <- pixbufNewFromFile "Cross.png"
  return GUI {
    disableBoard = mapM_ (flip widgetSetSensitivity False) buttons,
    resetBoard = do
      mapM_ (\i -> imageClear i >> widgetQueueDraw i) images
      mapM_ (flip widgetSetSensitivity True) buttons,
    setSquare = \ i player ->
      case player of
        Just X -> set (images !! i) [ imagePixbuf := crossPic ]
        Just O -> set (images !! i) [ imagePixbuf := noughtPic ],
    setStatus = \msg -> do
            statusbarPop statusbar ctx
            statusbarPush statusbar ctx msg
            return ()
   }
