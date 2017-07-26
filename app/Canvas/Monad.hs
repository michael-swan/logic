module Canvas.Monad
  ( Canvas
  , CanvasData
  , initCanvasData
  , runCanvas
  , getMousePoint
  , getMouseClickPoint
  , getDisplayOffset
  , setMousePoint
  , setMouseClickPoint
  , setDisplayOffset ) where

import Control.Concurrent.MVar
import Control.Monad.State
import Graphics.UI.WX.Types

data CanvasData = CanvasData Point Point Point
type Canvas = StateT CanvasData IO

initCanvasData :: IO (MVar CanvasData)
initCanvasData = newMVar $ CanvasData (Point 0 0) (Point 0 0) (Point 0 0)

runCanvas :: MVar CanvasData -> Canvas a -> IO a
runCanvas state cm = do
  s <- readMVar state
  (r, s') <- runStateT cm s
  swapMVar state s'
  return r

getMousePoint :: Canvas Point
getMousePoint = do
  CanvasData mp _ _ <- get
  return mp

getMouseClickPoint :: Canvas Point
getMouseClickPoint = do
  CanvasData _ mcp _ <- get
  return mcp

getDisplayOffset :: Canvas Point
getDisplayOffset = do
  CanvasData _ _ off <- get
  return off

setMousePoint :: Point -> Canvas ()
setMousePoint mp = modify $ \(CanvasData _ b c) -> CanvasData mp b c

setMouseClickPoint :: Point -> Canvas ()
setMouseClickPoint mcp = modify $ \(CanvasData a _ c) -> CanvasData a mcp c

setDisplayOffset :: Point -> Canvas ()
setDisplayOffset off = modify $ \(CanvasData a b _) -> CanvasData a b off
