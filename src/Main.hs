{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.WX hiding ((#), JoinMiter)
import Graphics.UI.WXCore hiding ((#), JoinMiter, Image)
import Graphics.Rendering.OpenGL
import Control.Monad.IO.Class
import qualified Graphics.UI.WX as WX
import qualified Graphics.Rendering.OpenGL as GL
import Control.Concurrent
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import GHC.Ptr
import Canvas.Monad
import Canvas.Event
import Data.Binary
import Data.Binary.Put hiding (flush)
import qualified Data.StateVar as SV
import Data.Vector.Storable (unsafeWith)
import qualified Data.Vector.Storable as V
import System.Directory
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
-- import qualified Graphics.GL.Core43 as Raw
import Data.List
import Font
import Graphics.Text.PCF
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Bits
import Data.Bool
import Compiler.Hoopl
import Canvas.Shader
import Graphics.GL.Core32 hiding (GL_RGBA, GL_MAJOR_VERSION, GL_MINOR_VERSION)
import Data.ByteString.Unsafe

main :: IO ()
main = start gui

gui :: IO ()
gui = do
   -- Create window
   main_window <- frame [ text := "Logic" ]
   -- Setup menu bar
   menu_bar <- menuPane [text := "&File"]
   menu_settings <- menuItem menu_bar [text := "&Settings\tCtrl+E", help := "Configure program settings", on command := close main_window]
   menu_close <- menuItem menu_bar [text := "&Close\tCtrl+Q", help := "Close the document", on command := proceedDialog main_window "Quit Application" "Are you sure you want to leave?" >>= bool (return ()) (close main_window)]
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
   glEnable GL_BLEND
   -- blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
   glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   (vao, vao_lines) <- createShaderBuffers
   Right (ShaderPrograms shader_program shader_program_lines) <- compileShaderPrograms
   transform_uniform <- unsafeUseAsCString "transform" $ glGetUniformLocation shader_program
   transform_uniform_lines <- unsafeUseAsCString "transform" $ glGetUniformLocation shader_program_lines
   orthoMatrix <- newArray [ 1, 0, 0, 0
                           , 0, 1, 0, 0
                           , 0, 0, 1, 0
                           , 0, 0, 0, 1 ]
   WX.set opengl_canvas [ on paintRaw    := canvasPaint state opengl_canvas opengl_context shader_program shader_program_lines vao vao_lines transform_uniform transform_uniform_lines orthoMatrix
                        , on click       := runCanvas state . canvasClick
                        , on unclick     := runCanvas state . canvasUnclick
                        , on doubleClick := runCanvas state . canvasDoubleClick
                        , on drag        := runCanvas state . canvasDrag opengl_canvas ]
   -- Setup quit button (temporary)
   -- quit <- button main_split [text := "Quit", on command := close main_window]
   list <- listView main_split ["Symbol", "Address"] (\(x, y) -> [x, show (y :: Int)]) -- [columns := [("Symbol", AlignLeft, 100)]]
   listViewSetItems list [("sub_400000", 1234), ("main", 5678)]
   WX.set (listViewCtrl list) [style := wxLC_VRULES .|. wxLC_HRULES .|. wxLC_REPORT]
   WX.set main_window [menuBar := [menu_bar]
                      , statusBar := [status_bar]
                      , layout := container main_panel $ WX.fill $
                                    vsplit main_split 4 300
                                      -- Left Panel
                                      (widget $ listViewCtrl list) -- quit)
                                      -- Right Panel
                                      (floatBottomRight $ widget opengl_canvas) ]

-- | Texture atlas of a monospaced font.
data TextureAtlas = TextureAtlas { textureAtlasObject :: TextureObject
                                 -- ^ Texture object containing the font's rendered texture atlas
                                 , textureAtlasMap :: Map Char (Int, Int)
                                 -- ^ Map from character to its horizontal offset into our texture and its width
                                 , textureAtlasWidth :: Int
                                 -- ^ Width of each character
                                 , textureAtlasHeight :: Int
                                 -- ^ Height of each character
                                 }

newTextureAtlas :: PCFText -> IO TextureAtlas -- (TextureObject, Map Char (Int, PCFGlyph)) -- TextureObject
newTextureAtlas !(PCFText gs w h !font_img) = do
  texObj <- genObjectName
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just texObj
  unsafeWith (V.map (\px -> shiftL (fromIntegral px :: Word32) 24) font_img) $ \ptr ->
      texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 $ PixelData RGBA UnsignedByte ptr
  max_texture_size <- alloca $ \ptr -> do
      glGetIntegerv GL_MAX_TEXTURE_SIZE ptr
      peek ptr
  let m = M.fromList $ snd $ foldl' (\(p, xs) g -> (p+glyph_width g, (glyph_char g, (p, glyph_width g)):xs)) (0, []) gs
  print =<< SV.get errors
  return $ TextureAtlas texObj m w h

floatsToBytes :: [Float] -> BS.ByteString
floatsToBytes = BSL.toStrict . runPut . mapM_ put

uintToBytes :: [Word32] -> BS.ByteString
uintToBytes = BSL.toStrict . runPut . mapM_ putWord32le

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

createShaderBuffers :: IO (GLuint, GLuint)
createShaderBuffers = do
  -- Allocate VAO's and BO's
  [boxes, lines] <- allocaArray 2 $ \vaos -> do
      glGenVertexArrays 2 vaos
      peekArray 2 vaos
  [arrayBuffer, ebo, arrayBuffer'] <- allocaArray 3 $ \buffers -> do
    glGenBuffers 3 buffers
    peekArray 3 buffers
  -- BOXES --
  glBindVertexArray boxes
  let (w, h) = (760, 14)
  let vertices = [ Vertex2 (-w-1.0) ( h-1.0), Vertex2 (0.0) (0.0)
                 , Vertex2 ( w-1.0) ( h-1.0), Vertex2 (1.0) (0.0)
                 , Vertex2 ( w-1.0) (-h-1.0), Vertex2 (1.0) (1.0)
                 , Vertex2 (-w-1.0) (-h-1.0), Vertex2 (0.0) (1.0)
                 ] :: [Vertex2 GLfloat]
      elements = [Vertex3 0 1 2, Vertex3 2 3 0] :: [Vertex3 GLuint]
  -- arrayBuffer <- genObjectName
  -- bindBuffer ArrayBuffer $= Just arrayBuffer
  glBindBuffer GL_ARRAY_BUFFER arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral $ length vertices * sizeOf (head vertices)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  -- ebo <- genObjectName
  -- bindBuffer ElementArrayBuffer $= Just ebo
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  withArray elements $ \ptr -> do
    let size = fromIntegral $ length elements * sizeOf (head elements)
    bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)

  let pos = AttribLocation 0
  vertexAttribPointer pos $= (ToFloat, VertexArrayDescriptor 2 Float (4*4) (bufferOffset 0))
  vertexAttribArray pos $= Enabled

  let tex = AttribLocation 1
  vertexAttribPointer tex $= (ToFloat, VertexArrayDescriptor 2 Float (4*4) (bufferOffset 8))
  vertexAttribArray tex $= Enabled
  atlas_obj <- newTextureAtlas =<< gohuFont
  putStrLn "After newTexture"
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  textureFilter Texture2D $= ((Linear', Nothing), Linear')

  -- LINES --
  -- lines <- genObjectName
  -- bindVertexArrayObject $= Just lines
  glBindVertexArray lines

  -- arrayBuffer <- genObjectName
  -- bindBuffer ArrayBuffer $= Just arrayBuffer'
  glBindBuffer GL_ARRAY_BUFFER arrayBuffer'

  let vertices = [ Vertex2 0 0
                 , Vertex2 50 100
                 , Vertex2 100 100
                 ] :: [Vertex2 GLfloat]
  
  withArray vertices $ \ptr -> do
    let size = fromIntegral $ length vertices * sizeOf (head vertices)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let pos = AttribLocation 0
  vertexAttribPointer pos $= (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
  vertexAttribArray pos $= Enabled

  return (boxes, lines)

canvasPaint :: MVar CanvasData -> GLCanvas a -> GLContext a -> GLuint -> GLuint -> GLuint -> GLuint -> GLint -> GLint -> Ptr GLfloat -> DC c -> WX.Rect -> [WX.Rect] -> IO ()
canvasPaint canvas_state canvas context shader_program shader_program_lines vao vao_lines transform_uniform transform_uniform_lines orthoMatrix _ (WX.Rect _ _ w h) _ = do
   glContextSetCurrent context canvas
   -- Adjust viewport size
   glViewport 0 0 (fromIntegral w) (fromIntegral h)
   -- Clear screen and fill with background color
   let bg = 0xF0 / 0xFF
   glClearColor bg bg bg 1
   clear [ColorBuffer]
   -- Calculate orthographic projection matrix
   runCanvas canvas_state $ do
     x <- getMousePoint
     y <- getMouseClickPoint
     off <- getDisplayOffset
     let diff_x = 2 * fromIntegral (pointX off + pointX x - pointX y)
         diff_y = 2 * fromIntegral (pointY off + pointY y - pointY x)
         w' = fromIntegral w
         h' = fromIntegral h
     liftIO $ do
         pokeElemOff orthoMatrix 0 (1 / w')
         pokeElemOff orthoMatrix 3 (diff_x / w')
         pokeElemOff orthoMatrix 5 (1 / h')
         pokeElemOff orthoMatrix 7 (diff_y / h')
   -- Render text
   glUseProgram shader_program
   glUniformMatrix4fv transform_uniform 1 GL_TRUE orthoMatrix
   glBindVertexArray vao
   drawElements Triangles 6 UnsignedInt nullPtr
   -- Render lines
   glUseProgram shader_program_lines
   glUniformMatrix4fv transform_uniform_lines 1 GL_TRUE orthoMatrix
   glBindVertexArray vao_lines
   drawArrays LineStrip 0 3
   -- Swap framebuffers
   glCanvasSwapBuffers canvas
   return ()
