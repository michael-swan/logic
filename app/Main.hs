{-# LANGUAGE TypeFamilies #-}
module Main
where

import Graphics.UI.WX hiding ((#))
import Graphics.UI.WXCore hiding ((#))
import Graphics.Rendering.OpenGL
import Control.Monad.IO.Class
import qualified Graphics.UI.WX as WX
import qualified Graphics.Rendering.OpenGL as GL
import Control.Concurrent
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import System.IO.Unsafe
import Control.Monad
import Graphics.Rendering.OpenGL.GL.VertexSpec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import GHC.Ptr
import Canvas.Monad
import Canvas.Event
import Data.Bits
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import Data.Binary
import Data.Binary.Put hiding (flush)
import qualified Data.StateVar as SV
import Data.Vector.Storable (unsafeWith)
import Codec.Picture.Png
import Codec.Picture.Types
import Graphics.Rendering.OpenGL.GLU.Errors
import Graphics.Rendering.OpenGL.GL.StringQueries
import System.Exit
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Graphics.GLUtil.Shaders
import Diagrams.Backend.Rasterific as R
import Diagrams.Size
import Diagrams.TwoD.Types
import Diagrams.Prelude ((#), strokeT, medium, lw, lc, darkred, rotateBy, Diagram, (<>), reflectX, reflectY)
import qualified Diagrams.Prelude as D
import Diagrams.Core.Points
import qualified Diagrams.Core.Compile as CD

type Dia = Diagram B

-- import Graphics.Rendering.OpenGL

main :: IO()
main = start gui

gui :: IO ()
gui = do
   -- Create window
   main_window <- frame [ text := "Logic" ]
   -- Setup menu bar
   menu_bar <- menuPane [text := "&File"]
   menu_close <- menuItem menu_bar [text := "&Close\tCtrl+C", help := "Close the document", on command := close main_window]
   -- Setup status bar
   status_bar <- statusField [statusWidth := 50, text := "Hello"]
   -- Setup main layout
   main_panel <- panel main_window []
   main_split <- splitterWindow main_panel []
   -- Setup OpenGL canvas
   opengl_canvas <- glCanvasCreateEx main_split 0 (Rect 0 0 800 600) 0 "GLCanvas" [GL_RGBA, GL_CORE_PROFILE, GL_MAJOR_VERSION 4, GL_MINOR_VERSION 3] nullPalette
   opengl_context <- glContextCreateFromNull opengl_canvas
   state <- initCanvasData
   glContextSetCurrent opengl_context opengl_canvas
   -- (vao, vbo, ebo)<- createShaderBuffers (floatsToBytes [0.0, 0.5, 0.5, -0.5, -0.5, -0.5]) (uintToBytes [0, 1, 2, 2, 3, 0])
   vao <- createShaderBuffers
   shader_program <- createShaderProgram
   transform_uniform <- SV.get $ uniformLocation shader_program "transform"
   let identity_matrix = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] :: [GLfloat]
   mat <- newMatrix ColumnMajor identity_matrix
   uniformGLMat4 transform_uniform $= mat
   WX.set opengl_canvas [ on paintRaw    := canvasPaint state opengl_canvas opengl_context shader_program vao transform_uniform
                        , on click       := runCanvas state . canvasClick
                        , on unclick     := runCanvas state . canvasUnclick
                        , on doubleClick := runCanvas state . canvasDoubleClick
                        , on drag        := runCanvas state . canvasDrag opengl_canvas ]
   -- Setup quit button (temporary)
   quit <- button main_split [text := "Quit", on command := close main_window]
   WX.set main_window [menuBar := [menu_bar]
                      , statusBar := [status_bar]
                      , layout := container main_panel $ fill $
                                    vsplit main_split 4 100
                                      -- Left Panel
                                      (widget quit)
                                      -- Right Panel
                                      (floatBottomRight $ widget opengl_canvas) ]

newTexture :: IO TextureObject
newTexture = do
  let opts = RasterificOptions $ mkSizeSpec (V2 (Just 100.0) (Just 100.0) :: V2 (Maybe Double))
  let Image w h d = CD.renderDia Rasterific opts ex
  texObj <- genObjectName :: IO TextureObject
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just texObj
  unsafeWith d $ \ptr ->
    texImage2D Texture2D NoProxy 0 RGBA'	(TextureSize2D (fromIntegral w) (fromIntegral h)) 0 $ PixelData RGBA UnsignedByte ptr
  return texObj
  where
    ex = atPoints (trailVertices $ regPoly 6 1) (repeat (circle 0.2 # fc green))
    hilbert 0 = mempty
    hilbert n = hilbert' (n-1) # reflectY <> D.vrule 1
             <> hilbert  (n-1) <> D.hrule 1
             <> hilbert  (n-1) <> D.vrule (-1)
             <> hilbert' (n-1) # reflectX
      where
        hilbert' m = hilbert m # rotateBy (1/4)

    example :: Dia
    example = D.frame 1 . lw medium . lc darkred
                      . strokeT $ hilbert 5

floatsToBytes :: [Float] -> BS.ByteString
floatsToBytes = BSL.toStrict . runPut . mapM_ put

uintToBytes :: [Word32] -> BS.ByteString
uintToBytes = BSL.toStrict . runPut . mapM_ putWord32le

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

createShaderBuffers :: IO VertexArrayObject
createShaderBuffers = do
  triangle <- genObjectName
  bindVertexArrayObject $= Just triangle
  let c = 256
  let vertices = [ Vertex2 (-c) ( c), Vertex2 (0.0) (0.0)
                 , Vertex2 ( c) ( c), Vertex2 (1.0) (0.0)
                 , Vertex2 ( c) (-c), Vertex2 (1.0) (1.0)
                 , Vertex2 (-c) (-c), Vertex2 (0.0) (1.0)
                 ] :: [Vertex2 GLfloat]
      elements = [Vertex3 0 1 2, Vertex3 2 3 0] :: [Vertex3 GLuint]
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral $ length vertices * sizeOf (head vertices)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  ebo <- genObjectName
  bindBuffer ElementArrayBuffer $= Just ebo
  withArray elements $ \ptr -> do
    let size = fromIntegral $ length elements * sizeOf (head elements)
    bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)

  let pos = AttribLocation 0
  vertexAttribPointer pos $= (ToFloat, VertexArrayDescriptor 2 Float (4*4) (bufferOffset 0))
  vertexAttribArray pos $= Enabled

  let tex = AttribLocation 1
  vertexAttribPointer tex $= (ToFloat, VertexArrayDescriptor 2 Float (4*4) (bufferOffset 8))
  vertexAttribArray tex $= Enabled
  newTexture
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  textureFilter Texture2D $= ((Linear', Nothing), Linear')

  return triangle

makeShader :: ShaderType -> BS.ByteString -> IO Shader
makeShader ty src = do
    s <- createShader ty
    shaderSourceBS s $= src
    compileShader s
    s'Ok <- SV.get $ compileStatus s
    unless s'Ok $ do
        slog <- SV.get $ shaderInfoLog s
        putStrLn $ "Log:" ++ slog
        exitFailure
    print =<< SV.get errors
    return s

makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
    p <- createProgram
    mapM_ (attachShader p) shaders
    mapM_ (\(name, loc) -> attribLocation p name $= loc) attributes
    bindFragDataLocation p "outColor" $= 0
    linkProgram p
    p'Ok <- SV.get $ linkStatus p
    validateProgram p
    status <- SV.get $ validateStatus p
    unless (p'Ok && status) $ do
        plog <- SV.get $ programInfoLog p
        putStrLn plog
        print =<< SV.get errors
        exitFailure
    return p

createShaderProgram :: IO Program
createShaderProgram = do
  vs_source <- BS.readFile "/home/mswan/proj/logic/shaders/vertex.glsl"
  fs_source <- BS.readFile "/home/mswan/proj/logic/shaders/fragment.glsl"
  vs <- makeShader VertexShader vs_source
  fs <- makeShader FragmentShader fs_source
  let pos = AttribLocation 0
      tex = AttribLocation 1
  p <- makeProgram [vs, fs] [("position", pos), ("texcoord", tex)]
  currentProgram $= Just p
  return p

canvasPaint :: MVar CanvasData -> GLCanvas a -> GLContext a -> Program -> VertexArrayObject -> UniformLocation -> DC c -> WX.Rect -> [WX.Rect]-> IO ()
canvasPaint canvas_state canvas context shader_program vao transform_uniform _ (WX.Rect _ _ w h) _ = do
   glContextSetCurrent context canvas
   currentProgram $= Just shader_program
   reshape $ GL.Size (fromIntegral w) (fromIntegral h)
   clearColor $= Color4 1 0 0 1
   clear [ColorBuffer]
   runCanvas canvas_state $ do
     x <- getMousePoint
     y <- getMouseClickPoint
     off <- getDisplayOffset
     let diff_x = 2 * fromIntegral (pointX off + pointX x - pointX y)
         diff_y = 2 * fromIntegral (pointY off + pointY y - pointY x)
     let ortho_matrix = [1 / fromIntegral w, 0, 0, diff_x / fromIntegral w, 0, 1 / fromIntegral h, 0, diff_y / fromIntegral h, 0, 0, 1, 0, 0, 0, 0, 1] :: [GLfloat]
     mat <- liftIO $ newMatrix ColumnMajor ortho_matrix
     uniformGLMat4 transform_uniform $= mat


   bindVertexArrayObject $= Just vao
   drawElements Triangles 6 UnsignedInt nullPtr
   -- drawArrays Triangles 0 6
   -- runCanvas canvas_state display
   glCanvasSwapBuffers canvas
   return ()

reshape size@(GL.Size w h) = do
   GL.viewport GL.$= (GL.Position 0 0, size)
   GL.matrixMode GL.$= GL.Projection
   GL.loadIdentity
   --GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
   let w' = 2 * fromIntegral w
       h' = 2 * fromIntegral h
   GL.ortho (-w') w' (-h') h' (-1.0) 1.0
   GL.matrixMode GL.$= GL.Modelview 0
   GL.loadIdentity
