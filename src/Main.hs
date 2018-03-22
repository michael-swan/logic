{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.UI.WX hiding ((#), JoinMiter)
import Graphics.UI.WXCore hiding ((#), JoinMiter, Image, GL_RGBA)
import qualified Graphics.UI.WXCore as WXCore
import Control.Monad.IO.Class
import Control.Monad
import qualified Graphics.UI.WX as WX
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
import Graphics.GL.Core32
import Graphics.GL.Types
import Data.ByteString.Unsafe
import Data.Maybe
import GL

main :: IO ()
main = start $ do
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
    -- Setup list
    list <- listView main_split ["Symbol", "Address"] (\(x, y) -> [x, show (y :: Int)])
    listViewSetItems list [("sub_400000", 1234), ("main", 5678)]
    WX.set (listViewCtrl list) [style := wxLC_VRULES .|. wxLC_HRULES .|. wxLC_REPORT]
    -- Setup OpenGL canvas
    opengl_canvas <- glCanvasCreateEx main_split 0 (Rect 0 0 800 600) 0 "GLCanvas" [WXCore.GL_RGBA, GL_CORE_PROFILE, WXCore.GL_MAJOR_VERSION 4, WXCore.GL_MINOR_VERSION 3] nullPalette
    opengl_context <- glContextCreateFromNull opengl_canvas
    glContextSetCurrent opengl_context opengl_canvas
    -- Initialize OpenGL vertex array objects (VAO's), buffer objects (BO's), uniforms, and shader programs
    (vao, vao_text, vao_lines, tex_atlas) <- createShaderBuffers
    Right (ShaderPrograms shader_program shader_program_lines shader_program_text) <- compileShaderPrograms
    transform_uniform <- unsafeUseAsCString "transform" $ glGetUniformLocation shader_program
    transform_uniform_text <- unsafeUseAsCString "transform" $ glGetUniformLocation shader_program_text
    transform_uniform_lines <- unsafeUseAsCString "transform" $ glGetUniformLocation shader_program_lines
    orthoMatrix <- newArray [ 1, 0, 0, 0
                            , 0, 1, 0, 0
                            , 0, 0, 1, 0
                            , 0, 0, 0, 1 ]
    -- Configure blending
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    -- Setup OpenGL canvas event handlers
    state <- initCanvasData
    WX.set opengl_canvas [ on paintRaw    := canvasPaint state opengl_canvas opengl_context shader_program shader_program_text shader_program_lines vao vao_text vao_lines transform_uniform transform_uniform_text transform_uniform_lines orthoMatrix tex_atlas
                         , on click       := runCanvas state . canvasClick
                         , on unclick     := runCanvas state . canvasUnclick
                         , on doubleClick := runCanvas state . canvasDoubleClick
                         , on drag        := runCanvas state . canvasDrag opengl_canvas ]
    WX.set main_window [menuBar := [menu_bar]
                       , statusBar := [status_bar]
                       , layout := container main_panel $ WX.fill $
                                     vsplit main_split 4 300
                                       -- Left Panel
                                       (widget $ listViewCtrl list) -- quit)
                                       -- Right Panel
                                       (floatBottomRight $ widget opengl_canvas) ]

-- | Texture atlas of a monospaced font.
data TextureAtlas = TextureAtlas { textureAtlasObject :: GLuint
                                 -- ^ Texture object containing the font's rendered texture atlas
                                 , textureAtlasMap :: Map Char (Int, PCFGlyph)
                                 -- ^ Map from character to its horizontal offset into our texture and its width
                                 , textureAtlasWidth :: Int
                                 -- ^ Width of each character
                                 , textureAtlasHeight :: Int
                                 -- ^ Height of each character
                                 }

newTextureAtlas :: PCFText -> IO TextureAtlas -- (TextureObject, Map Char (Int, PCFGlyph)) -- TextureObject
newTextureAtlas !(PCFText gs w h !font_img) = do
    -- Allocate Texture Object
    texture_object <- alloca $ \texture_object_ptr -> do
        glGenTextures 1 texture_object_ptr
        peek texture_object_ptr
    -- Bind texture object to first texture slot
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D texture_object
    -- Load texture data into OpenGL
    unsafeWith (V.map (\px -> shiftL (fromIntegral px :: Word32) 24) font_img) $
        glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral w) (fromIntegral h) 0 GL_RGBA GL_UNSIGNED_BYTE . castPtr
    -- Configure texture
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    -- Generate texture atlas map
    -- let atlas_map = M.fromList $ snd $ foldl' (\(p, xs) g -> (p+glyph_width g, (glyph_char g, (p, g)):xs)) (0, []) gs
    let atlas_map = M.fromList $ zip (map glyph_char gs) $ zip (reverse $ foldl' (\(x:xs) g -> (x + glyph_width g):x:xs) [0] gs) gs
    return $ TextureAtlas texture_object atlas_map w h

floatsToBytes :: [Float] -> BS.ByteString
floatsToBytes = BSL.toStrict . runPut . mapM_ put

uintToBytes :: [Word32] -> BS.ByteString
uintToBytes = BSL.toStrict . runPut . mapM_ putWord32le

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

createShaderBuffers :: IO (GLuint, GLuint, GLuint, TextureAtlas)
createShaderBuffers = do
    -- Allocate VAO's and BO's
    [boxes, text, lines] <- allocaArray 3 $ \vaos -> do
        glGenVertexArrays 3 vaos
        peekArray 3 vaos
    [arrayBuffer, ebo, arrayBuffer', textArray] <- allocaArray 4 $ \buffers -> do
        glGenBuffers 4 buffers
        peekArray 4 buffers
    -- BOXES --
    glBindVertexArray boxes
    let (w, h) = (760, 14)
    let (w', h') = (w/2, h/2)
    let vertices = [ (-w'+0.25), ( 0+0.25),  0, 0
                   , ( w'+0.25), ( 0+0.25),  1, 0
                   , ( w'+0.25), (-h+0.25),  1, 1
                   , (-w'+0.25), (-h+0.25),  0, 1 ] :: [GLfloat]
        elements = [ 0, 1, 2
                   , 2, 3, 0 ] :: [GLuint]
    glBindBuffer GL_ARRAY_BUFFER arrayBuffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral $ length vertices * sizeOf (head vertices)
        glBufferData GL_ARRAY_BUFFER size (castPtr ptr) GL_STATIC_DRAW

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
    withArray elements $ \ptr -> do
        let size = fromIntegral $ length elements * sizeOf (head elements)
        glBufferData GL_ELEMENT_ARRAY_BUFFER size (castPtr ptr) GL_STATIC_DRAW

    let pos = 0
    glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 16 $ bufferOffset 0
    glEnableVertexAttribArray pos

    let tex = 1
    glVertexAttribPointer tex 2 GL_FLOAT GL_FALSE 16 $ bufferOffset 8
    glEnableVertexAttribArray tex

    -- TEXT --
    atlas_obj <- newTextureAtlas =<< gohuFont
    glBindVertexArray text
    glBindBuffer GL_ARRAY_BUFFER textArray
    bufferTextVertices atlas_obj "Abcd"

    flip mapM [0, 1, 2] $ \i -> do
        glVertexAttribPointer i 1 GL_FLOAT GL_FALSE 12 $ bufferOffset (i*4)
        glEnableVertexAttribArray i

    -- LINES --
    glBindVertexArray lines
    glBindBuffer GL_ARRAY_BUFFER arrayBuffer'

    let vertices = [ 0, 0
                   , 50, 100
                   , 100, 100 ] :: [GLfloat]
    withArray vertices $ \ptr -> do
        let size = fromIntegral $ length vertices * sizeOf (head vertices)
        glBufferData GL_ARRAY_BUFFER size (castPtr ptr) GL_STATIC_DRAW

    let pos = 0
    glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 $ bufferOffset 0
    glEnableVertexAttribArray pos

    return (boxes, text, lines, atlas_obj)

canvasPaint :: MVar CanvasData -> GLCanvas a -> GLContext a -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLint -> GLint -> GLint -> Ptr GLfloat -> TextureAtlas -> DC c -> WX.Rect -> [WX.Rect] -> IO ()
canvasPaint canvas_state canvas context shader_program shader_program_text shader_program_lines vao vao_text vao_lines transform_uniform transform_uniform_text transform_uniform_lines orthoMatrix tex_atlas _ (WX.Rect _ _ w h) _ = runCanvas canvas_state $ do
    -- Adjust viewport size
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    -- Clear screen and fill with background color
    let bg = 0xF0 / 0xFF
    glClearColor bg bg bg 1
    glClear GL_COLOR_BUFFER_BIT
    -- Calculate orthographic projection matrix
    x <- getMousePoint
    y <- getMouseClickPoint
    off <- getDisplayOffset
    let diff_x = fromIntegral (pointX off + pointX x - pointX y)
        diff_y = fromIntegral (pointY off + pointY y - pointY x)
        w' = fromIntegral w / 2
        h' = fromIntegral h / 2
    liftIO $ do
        pokeElemOff orthoMatrix 0 (1 / w')
        pokeElemOff orthoMatrix 3 (diff_x / w')
        pokeElemOff orthoMatrix 5 (1 / h')
        pokeElemOff orthoMatrix 7 (diff_y / h')
    -- Render boxes
    glUseProgram shader_program
    glUniformMatrix4fv transform_uniform 1 GL_TRUE orthoMatrix
    glBindVertexArray vao
    -- glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
    -- Render text
    glUseProgram shader_program_text
    transform_atlas_height <- liftIO $ unsafeUseAsCString "atlas_height" $ glGetUniformLocation shader_program_text
    transform_atlas_width <- liftIO $ unsafeUseAsCString "atlas_width" $ glGetUniformLocation shader_program_text
    -- liftIO $ print $ textureAtlasHeight tex_atlas
    -- liftIO $ print $ textureAtlasWidth tex_atlas
    glUniformMatrix4fv transform_uniform_text 1 GL_TRUE orthoMatrix
    glUniform1f transform_atlas_height $ fromIntegral $ textureAtlasHeight tex_atlas
    glUniform1f transform_atlas_width $ fromIntegral $ textureAtlasWidth tex_atlas
    glBindVertexArray vao_text
    glDrawArrays GL_TRIANGLES 0 (6*4) -- GL_UNSIGNED_INT nullPtr
    -- liftIO . print =<< getErrors
    -- Render lines
    glUseProgram shader_program_lines
    glUniformMatrix4fv transform_uniform_lines 1 GL_TRUE orthoMatrix
    glBindVertexArray vao_lines
    -- glDrawArrays GL_LINE_STRIP 0 3
    -- Swap framebuffers
    liftIO $ glCanvasSwapBuffers canvas
    return ()


-- -- String input contains no line breaks

-- Rendering as triangle strip
bufferTextVertices :: TextureAtlas -> String -> IO ()
bufferTextVertices atlas str = do
    let (atlas_offsets, glyphs) = unzip $ mapMaybe (`M.lookup` textureAtlasMap atlas) str
        xs = reverse $ foldl' (\(x:xs) g -> (x + glyph_width g):x:xs) [0] glyphs
        buf_size = length str * 18 * sizeOf (undefined :: GLfloat)
    allocaBytes buf_size $ \buf -> do
        let f p (PCFGlyph{..}, pre_ox, pre_ax) = do
                let ox = fromIntegral pre_ox :: GLfloat
                    ax = fromIntegral pre_ax
                    height = fromIntegral glyph_height
                    width = fromIntegral glyph_width
                    ox' = ox + width
                    ax' = ax + width
                -- Top left vertex
                pokeElemOff p 0 ox      -- x offset (output)
                pokeElemOff p 1 0       -- y offset (output & atlas)
                pokeElemOff p 2 ax      -- x offset (atlas)
                -- Bottom left vertex
                pokeElemOff p 3 ox      -- x offset (output)
                pokeElemOff p 4 height  -- y offset (output & atlas)
                pokeElemOff p 5 ax      -- x offset (atlas)
                -- Bottom right vertex
                pokeElemOff p 6 ox'     -- x offset (output)
                pokeElemOff p 7 height  -- y offset (output & atlas)
                pokeElemOff p 8 ax'     -- x offset (atlas)

                -- Bottom right vertex
                pokeElemOff p 9  ox'     -- x offset (output)
                pokeElemOff p 10 height  -- y offset (output & atlas)
                pokeElemOff p 11 ax'     -- x offset (atlas)
                -- Top right vertex
                pokeElemOff p 12 ox'     -- x offset (output)
                pokeElemOff p 13 0       -- y offset (output & atlas)
                pokeElemOff p 14 ax'     -- x offset (atlas)
                -- Top left vertex
                pokeElemOff p 15 ox      -- x offset (output)
                pokeElemOff p 16 0       -- y offset (output & atlas)
                pokeElemOff p 17 ax      -- x offset (atlas)

                print (ox, ox', p)
                print (ax, ax', height)
                return $ advancePtr p 18
        foldM f buf (zip3 glyphs xs atlas_offsets)
        -- print buf_size
        -- print $ last xs
        print (xs, atlas_offsets)
        glBufferData GL_ARRAY_BUFFER (fromIntegral buf_size) (castPtr buf) GL_STATIC_DRAW
