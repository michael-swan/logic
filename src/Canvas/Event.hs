module Canvas.Event
    ( canvasClick
    , canvasDoubleClick
    , canvasUnclick
    , canvasDrag
    , canvasAnyKey
    , canvasAnyKey' ) where

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

canvasDrag :: Point -> Canvas ()
canvasDrag x = do
  setMousePoint x
  canvasRepaint

canvasAnyKey :: EventKey -> Canvas ()
canvasAnyKey (EventKey key _ _) =  canvasAnyKey' key

canvasAnyKey' :: Key -> Canvas ()
canvasAnyKey' (KeyChar c) = getTextBuffer >>= setTextBuffer . (++ [c]) >> canvasRepaint
canvasAnyKey' KeyReturn   = canvasAnyKey' (KeyChar '\n')
canvasAnyKey' KeySpace    = canvasAnyKey' (KeyChar ' ')
canvasAnyKey' KeyBack     = do
    text_buffer <- getTextBuffer
    case text_buffer of
      "" ->  return ()
      _ -> setTextBuffer (init text_buffer) >> canvasRepaint
canvasAnyKey' _           = return ()

canvasRepaint :: Canvas ()
canvasRepaint = getGLCanvas >>= liftIO . repaint
