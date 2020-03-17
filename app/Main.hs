module Main where

import           Mines
import           Solver

import           Data.IORef
import           Control.Monad
import           Graphics.UI.Threepenny.Core

import qualified Data.HashSet                  as HS
import qualified Data.HashMap.Lazy             as H
import qualified Control.Monad.State           as M
import qualified Graphics.UI.Threepenny        as UI

mode :: Difficulty
mode = Easy

canvasSize :: Int
canvasSize = 720

canvasSizeD :: Double
canvasSizeD = conv canvasSize

main :: IO ()
main = do
  (g, d) <- M.execStateT newMineField ((//), mode)
  r      <- newIORef (g, HS.empty, False)
  putStr $ renderGrid d g
  startGUI defaultConfig $ setup r

-- setup sets up threepenny-gui.
setup :: IORef Boards -> Window -> UI ()
setup ref window = do
  return window # set title "Minesweeper"

  -- Draw canvas for the minesweeper board.
  canvas <- UI.canvas # set UI.height canvasSize # set UI.width canvasSize # set
    style
    [("border", "solid black 1px"), ("background", "#eee")]

  -- Create banner to show game status (playing/win/lose).
  banner <- UI.paragraph #+ [string "Playing..."]
  banner # set' UI.style [("color", "white")]

  (g, f, _) <- liftIO $ readIORef ref
  case hasWonOrLost g f of
    Just x -> setBanner banner x
    _      -> pure ()

  -- Create text to show when the AI cannot make a safe move.
  ambiguous <- UI.small
  ambiguous # set' UI.style [("color", "white")]

  -- Create buttons for autoplay and new game.
  makeMove <- UI.button #+ [string "Make move"]
  newGame  <- UI.button #+ [string "New game"]

  getBody window
    #+ [ element banner
       , column [element canvas]
       , element newGame
       , element makeMove
       , element ambiguous
       ]
  getBody window # set UI.bgcolor "#121212"

  -- Draw required components for the game.
  drawGrid ref canvas
  drawText ref canvas

  -- Add event handlers.
  on UI.contextmenu canvas $ flagCell ref banner canvas ambiguous
  on UI.click newGame $ \_ -> drawNewGame ref banner canvas ambiguous
  on UI.click makeMove $ \_ -> nextMove ref banner canvas ambiguous
  on UI.mouseup canvas $ handleClick ref banner canvas ambiguous

-- unlessUI is unless but in the UI monad.
unlessUI :: Bool -> UI () -> UI ()
unlessUI b u = if b then pure () else u

-- flagCell flags a given cell as potentially having a mine.
flagCell :: IORef Boards -> Element -> Element -> Element -> Coord -> UI ()
flagCell r banner canvas ambiguous xy = do
  let c = mousePosToCoord xy
  (g, f, e) <- liftIO $ readIORef r
  setAmbiguousMove False ambiguous

  -- Only allow interaction if the game has not ended.
  unlessUI e
    $ let f' = case HS.member c f of
            False -> HS.insert c f
            _     -> HS.delete c f
          hidden = case c ?? g of
            Just (Mine x       ) -> x
            Just (Highlight _ x) -> x
            Just (Empty x      ) -> x
            Nothing              -> False
      in  liftIO $ when hidden $ writeIORef r (g, f', e)

  -- Redraw canvas.
  clearAndDraw canvas r
  (g', f', e') <- liftIO $ readIORef r

  -- Check ending conditions.
  case hasWonOrLost g' f' of
    Just x -> do
      setBanner banner x
      liftIO $ writeIORef r (g', f', True)
    _ -> pure ()

-- handleClick handles exposing a cell.
handleClick :: IORef Boards -> Element -> Element -> Element -> Coord -> UI ()
handleClick r banner canvas ambiguous xy = do
  let c = mousePosToCoord xy
  (g, f, e) <- liftIO $ readIORef r
  setAmbiguousMove False ambiguous

  -- Only allow interaction if the game has not ended.
  unlessUI e
    $ liftIO
    $ unless (HS.member c f)
    $ writeIORef r
    $ (reveal (size mode) c g, f, e)

  -- Redraw canvas.
  clearAndDraw canvas r
  (g', f', e') <- liftIO $ readIORef r

  -- Check ending conditions.
  case hasWonOrLost g' f' of
    Just x -> do
      setBanner banner x
      liftIO $ writeIORef r (g', f', True)
    _ -> pure ()

-- nextMove handles autoplay.
nextMove :: IORef Boards -> Element -> Element -> Element -> UI ()
nextMove r banner canvas ambiguous = do
  (g, f, e) <- liftIO $ readIORef r
  setAmbiguousMove False ambiguous

  case solve (size mode) (obfuscate g) f of
    Just (Mark c) ->
      unlessUI e
        $ let f' = case HS.member c f of
                False -> HS.insert c f
                _     -> HS.delete c f
              hidden = case c ?? g of
                Just (Mine x       ) -> x
                Just (Highlight _ x) -> x
                Just (Empty x      ) -> x
                Nothing              -> False
          in  liftIO $ when hidden $ writeIORef r (g, f', e)
    Just (Uncover c) ->
      unlessUI e
        $ liftIO
        $ unless (HS.member c f)
        $ writeIORef r
        $ (reveal (size mode) c g, f, e)
    _ -> setAmbiguousMove True ambiguous

  -- Redraw canvas.
  clearAndDraw canvas r
  (g', f', e') <- liftIO $ readIORef r

  -- Check ending conditions.
  case hasWonOrLost g' f' of
    Just x -> do
      setBanner banner x
      liftIO $ writeIORef r (g', f', True)
    _ -> pure ()

-- drawNewGame creates a new minefield.
drawNewGame :: IORef Boards -> Element -> Element -> Element -> UI ()
drawNewGame r banner canvas ambiguous = do
  (g, d) <- liftIO $ M.execStateT newMineField ((//), mode)
  liftIO $ writeIORef r (g, HS.empty, False)
  liftIO $ putStr $ renderGrid d g
  banner # set' UI.text "Playing..."
  setAmbiguousMove False ambiguous
  canvas # UI.clearCanvas
  drawGrid r canvas
  drawText r canvas

-- clearAndDraw redraws the canvas.
clearAndDraw :: Element -> IORef Boards -> UI ()
clearAndDraw canvas r = do
  canvas # UI.clearCanvas
  drawGrid r canvas
  drawText r canvas

-- drawText puts text on the canvas.
drawText :: IORef Boards -> Element -> UI ()
drawText r canvas = do
  b <- liftIO $ readIORef r
  pure canvas # set UI.textFont (show cellSize ++ "px monospace") # set
    UI.fillStyle
    (UI.htmlColor "black")
  forM_ (texts b) $ \(s, p) -> canvas # UI.fillText s p

-- drawGrid draws the minefield graphics.
drawGrid :: IORef Boards -> Element -> UI ()
drawGrid r canvas = do
  (g, _, _) <- liftIO $ readIORef r
  forM_ (cells g) $ \(x, y, w, h, c) -> do
    canvas # set' UI.fillStyle (UI.htmlColor c)
    canvas # UI.fillRect (x, y) w h
  forM_ gridLines $ \(a, b) -> do
    canvas # UI.beginPath
    canvas # UI.moveTo a
    canvas # UI.lineTo b
    canvas # UI.closePath
    canvas # UI.stroke

-- texts generate the cell texts for the minefield.
texts :: Boards -> [(String, UI.Point)]
texts (g, f, _) = map (stringify g) $ allCoords mode
 where
  stringify :: Grid -> Coord -> (String, UI.Point)
  stringify g c = case HS.member c f of
    True -> ("!", textCoordToPoint c)
    _    -> case c ?? g of
      Just x -> (hiddenText x, textCoordToPoint c)
      _      -> ("", (0, 0))

-- coordToPoint converts a coordinate to a point on the canvas.
coordToPoint :: Coord -> UI.Point
coordToPoint (x, y) = ((conv x) * cellSize, (conv y) * cellSize)

-- mousePosToCoord converts a mouse position to a cell on the minefield grid.
mousePosToCoord :: Coord -> Coord
mousePosToCoord (x, y) = (div x $ floor cellSize, div y $ floor cellSize)

-- textCoordToPoint converts a coordinate to a point on the canvas for the text.
textCoordToPoint :: Coord -> UI.Point
textCoordToPoint (x, y) = (((conv x)) * cellSize, ((conv y) + 1) * cellSize)

cellSize :: Double
cellSize = canvasSizeD / (conv $ size mode)

-- cells gives the square data for a cell for threepenny-gui to draw.
cells :: Grid -> [(Double, Double, Double, Double, String)]
cells g = map (draw g) $ allCoords mode
 where
  draw :: Grid -> (Int, Int) -> (Double, Double, Double, Double, String)
  draw g c = case c ?? g of
    Just (Empty b) ->
      let (x, y) = coordToPoint c
          l      = hiddenColour b
      in  (x, y, cellSize, cellSize, l)
    Just (Highlight _ b) ->
      let (x, y) = coordToPoint c
          l      = hiddenColour b
      in  (x, y, cellSize, cellSize, l)
    Just (Mine b) ->
      let (x, y) = coordToPoint c
          l      = hiddenColour b
      in  (x, y, cellSize, cellSize, l)

-- gridLines gives the line data for cell borders for threepenny-gui to draw.
gridLines :: [(UI.Point, UI.Point)]
gridLines = grid ((size mode) - 1) $ size mode - 1
 where
  grid :: Int -> Int -> [(UI.Point, UI.Point)]
  grid 0 0 = []
  grid x y
    | y == 0
    = let (a, _) = coordToPoint (x, y)
      in  ((0, a), (canvasSizeD, a)) : grid (x - 1) y
    | otherwise
    = let (_, a) = coordToPoint (x, y)
      in  ((a, 0), (a, canvasSizeD)) : grid x (y - 1)

conv :: Integral a => a -> Double
conv = fromIntegral

hiddenColour :: Bool -> String
hiddenColour True = "#aaa"
hiddenColour _    = "#eee"

hiddenText :: Cell -> String
hiddenText (Mine True       ) = ""
hiddenText (Mine{}          ) = "X"
hiddenText (Highlight x True) = ""
hiddenText (Highlight x _   ) = show x
hiddenText _                  = ""

setAmbiguousMove :: Bool -> Element -> UI ()
setAmbiguousMove True  h = h # set' UI.text "Unable to make unambiguous move"
setAmbiguousMove False h = h # set' UI.text ""

setBanner :: Element -> Ending -> UI ()
setBanner banner Win  = banner # set' UI.text "You win!"
setBanner banner Lose = banner # set' UI.text "You lose!"
