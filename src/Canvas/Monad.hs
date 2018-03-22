module Canvas.Monad
    ( Canvas
    , CanvasData
    , initCanvasData
    , runCanvas
    , getMousePoint
    , getMouseClickPoint
    , getDisplayOffset
    , getTextBuffer
    , getGLCanvas
    , setMousePoint
    , setMouseClickPoint
    , setDisplayOffset
    , setTextBuffer ) where

import Control.Concurrent.MVar
import Control.Monad.State
import Graphics.UI.WX.Types
import Graphics.UI.WXCore.WxcClassTypes (GLCanvas)

data CanvasData = CanvasData Point Point Point String (GLCanvas ())
type Canvas = StateT CanvasData IO

initCanvasData :: GLCanvas () -> IO (MVar CanvasData)
initCanvasData = newMVar . CanvasData (Point 0 0) (Point 0 0) (Point 0 0) "default"

-- Change to wrapping each individual field with a TVar
runCanvas :: MVar CanvasData -> Canvas a -> IO a
runCanvas state cm = do
    s <- readMVar state
    (r, s') <- runStateT cm s
    swapMVar state s'
    return r

getMousePoint :: Canvas Point
getMousePoint = do
    CanvasData mp _ _ _ _ <- get
    return mp

getMouseClickPoint :: Canvas Point
getMouseClickPoint = do
    CanvasData _ mcp _ _ _ <- get
    return mcp

getDisplayOffset :: Canvas Point
getDisplayOffset = do
    CanvasData _ _ off _ _ <- get
    return off

getTextBuffer :: Canvas String
getTextBuffer = do
    CanvasData _ _ _ txt _ <- get
    return txt

getGLCanvas :: Canvas (GLCanvas ())
getGLCanvas = do
    CanvasData _ _ _ _ opengl_canvas <- get
    return opengl_canvas

setMousePoint :: Point -> Canvas ()
setMousePoint mp = modify $ \(CanvasData _ b c d e) -> CanvasData mp b c d e

setMouseClickPoint :: Point -> Canvas ()
setMouseClickPoint mcp = modify $ \(CanvasData a _ c d e) -> CanvasData a mcp c d e

setDisplayOffset :: Point -> Canvas ()
setDisplayOffset off = modify $ \(CanvasData a b _ d e) -> CanvasData a b off d e

setTextBuffer :: String -> Canvas ()
setTextBuffer txt = modify $ \(CanvasData a b c _ e) -> CanvasData a b c txt e
