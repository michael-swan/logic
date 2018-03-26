{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
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
    , getCaretBlinkTimer
    , getCaretBlinkInterval
    , getCaretVisible
    , setMousePoint
    , setMouseClickPoint
    , setDisplayOffset
    , setTextBuffer
    , setCaretVisible
    , toggleCaretVisible
    , canvasRepaint
    , resetCaretBlinkTimer ) where

import Control.Concurrent.MVar
import Control.Monad.State
import Graphics.UI.WX.Types
import Graphics.UI.WX.Timer
import Graphics.UI.WXCore.WxcClassTypes (GLCanvas)
import Graphics.UI.WX as WX hiding (get)

data CanvasData = CanvasData { canvas_mouse_point :: Point
                             , canvas_mouse_click_point :: Point
                             , canvas_display_offset :: Point
                             , canvas_text_buffer :: String
                             , canvas_gl_canvas :: GLCanvas ()
                             , canvas_caret_blink_timer :: Timer
                             , canvas_caret_blink_interval :: Int
                             , canvas_caret_visible :: Bool
                             }
type Canvas = StateT CanvasData IO

#if defined(windows_HOST_OS)
defaultBlinkInterval :: IO Int
defaultBlinkInterval = fromIntegral <$> getCaretBlinkTime

foreign import ccall "GetCaretBlinkTime" getCaretBlinkTime :: IO CUint
#else
defaultBlinkInterval :: IO Int
defaultBlinkInterval = return 500
#endif

initCanvasData :: GLCanvas () -> IO (MVar CanvasData)
initCanvasData gl_canvas = do
    blink_interval <- defaultBlinkInterval
    t <- timer gl_canvas [interval := blink_interval]
    state <- newMVar $ CanvasData (Point 0 0) (Point 0 0) (Point 0 0) "Xample" gl_canvas t blink_interval False
    WX.set t [on command := runCanvas state $ toggleCaretVisible >> canvasRepaint]
    return state

-- TODO: Change to wrapping each individual field with a TVar
runCanvas :: MVar CanvasData -> Canvas a -> IO a
runCanvas state cm = do
    s <- readMVar state
    (r, s') <- runStateT cm s
    swapMVar state s'
    return r

getMousePoint :: Canvas Point
getMousePoint = do
    CanvasData{..} <- get
    return canvas_mouse_point

getMouseClickPoint :: Canvas Point
getMouseClickPoint = do
    CanvasData{..} <- get
    return canvas_mouse_click_point

getDisplayOffset :: Canvas Point
getDisplayOffset = do
    CanvasData{..} <- get
    return canvas_display_offset

getTextBuffer :: Canvas String
getTextBuffer = do
    CanvasData{..} <- get
    return canvas_text_buffer

getGLCanvas :: Canvas (GLCanvas ())
getGLCanvas = do
    CanvasData{..} <- get
    return canvas_gl_canvas

getCaretBlinkTimer :: Canvas Timer
getCaretBlinkTimer = do
    CanvasData{..} <- get
    return canvas_caret_blink_timer

getCaretBlinkInterval :: Canvas Int
getCaretBlinkInterval = do
    CanvasData{..} <- get
    return canvas_caret_blink_interval

getCaretVisible :: Canvas Bool
getCaretVisible = do
    CanvasData{..} <- get
    return canvas_caret_visible


setMousePoint :: Point -> Canvas ()
setMousePoint mp = modify $ \cd -> cd { canvas_mouse_point = mp }

setMouseClickPoint :: Point -> Canvas ()
setMouseClickPoint mcp = modify $ \cd -> cd { canvas_mouse_click_point = mcp }

setDisplayOffset :: Point -> Canvas ()
setDisplayOffset off = modify $ \cd -> cd { canvas_display_offset = off }

setTextBuffer :: String -> Canvas ()
setTextBuffer txt = modify $ \cd -> cd { canvas_text_buffer = txt }

setCaretVisible :: Bool -> Canvas ()
setCaretVisible cb = modify $ \cd@(CanvasData{..}) -> cd { canvas_caret_visible = cb }

toggleCaretVisible :: Canvas ()
toggleCaretVisible = modify $ \cd@(CanvasData{..}) -> cd { canvas_caret_visible = not canvas_caret_visible }

canvasRepaint :: Canvas ()
canvasRepaint = getGLCanvas >>= liftIO . repaint

resetCaretBlinkTimer :: Canvas ()
resetCaretBlinkTimer = do
    t <- getCaretBlinkTimer
    i <- getCaretBlinkInterval
    liftIO $ WX.set t [interval := i]
