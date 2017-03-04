module Canvas.Event
  ( canvasClick
  , canvasDoubleClick
  , canvasUnclick
  , canvasDrag ) where

import Canvas.Monad
import Control.Monad.IO.Class
import Graphics.UI.WX.Events
import Graphics.UI.WX.Types
import Graphics.UI.WX.Window ()
import Graphics.UI.WXCore.WxcClassTypes (GLCanvas)

canvasClick :: Point -> Canvas ()
canvasClick x = do
  setMousePoint x
  setMouseClickPoint x

canvasDoubleClick :: Point -> Canvas ()
canvasDoubleClick = canvasClick

canvasUnclick :: Point -> Canvas ()
canvasUnclick (Point ax ay) = do
  Point bx by <- getMouseClickPoint
  Point x y <- getDisplayOffset
  setDisplayOffset   $ Point ((ax - bx) + x) ((by - ay) + y)
  setMousePoint      $ Point 0 0
  setMouseClickPoint $ Point 0 0

canvasDrag :: GLCanvas () -> Point -> Canvas ()
canvasDrag opengl_canvas x = do
  setMousePoint x
  liftIO $ repaint opengl_canvas