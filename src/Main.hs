{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.WX hiding ((#), JoinMiter)
import Graphics.UI.WXCore hiding ((#), JoinMiter, Image, GL_RGBA)
import qualified Graphics.UI.WXCore as WXCore
import Control.Monad.IO.Class
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
    (vao, vao_lines) <- createShaderBuffers
    Right (ShaderPrograms shader_program shader_program_lines) <- compileShaderPrograms
    transform_uniform <- unsafeUseAsCString "transform" $ glGetUniformLocation shader_program
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
    WX.set opengl_canvas [ on paintRaw    := canvasPaint state opengl_canvas opengl_context shader_program shader_program_lines vao vao_lines transform_uniform transform_uniform_lines orthoMatrix
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
    let atlas_map = M.fromList $ zip (map glyph_char gs) $ zip (foldl' (\(x:xs) g -> (x + glyph_width g):x:xs) [0] gs) gs
    return $ TextureAtlas texture_object atlas_map w h

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

    atlas_obj <- newTextureAtlas =<< gohuFont
    print $ textVertices atlas_obj "Fuck"

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

    return (boxes, lines)

canvasPaint :: MVar CanvasData -> GLCanvas a -> GLContext a -> GLuint -> GLuint -> GLuint -> GLuint -> GLint -> GLint -> Ptr GLfloat -> DC c -> WX.Rect -> [WX.Rect] -> IO ()
canvasPaint canvas_state canvas context shader_program shader_program_lines vao vao_lines transform_uniform transform_uniform_lines orthoMatrix _ (WX.Rect _ _ w h) _ = runCanvas canvas_state $ do
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
    -- Render text
    glUseProgram shader_program
    glUniformMatrix4fv transform_uniform 1 GL_TRUE orthoMatrix
    glBindVertexArray vao
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
    -- Render lines
    glUseProgram shader_program_lines
    glUniformMatrix4fv transform_uniform_lines 1 GL_TRUE orthoMatrix
    glBindVertexArray vao_lines
    glDrawArrays GL_LINE_STRIP 0 3
    -- Swap framebuffers
    liftIO $ glCanvasSwapBuffers canvas
    return ()

-- renderText :: GLuint -> String -> IO GLuint
-- renderText vao str = do
--     [boxes, lines] <- allocaArray 2 $ \vaos -> do
--         glGenVertexArrays 2 vaos
--         peekArray 2 vaos
--     [arrayBuffer, ebo, arrayBuffer'] <- allocaArray 3 $ \buffers -> do
--         glGenBuffers 3 buffers
--         peekArray 3 buffers
--     -- BOXES --
--     glBindVertexArray boxes
--     let (w, h) = (760, 14)
--     let (w', h') = (w/2, h/2)
--     let f Nothing = f $ Just $ textureAtlasMap atlas M.! '?'
--         f (Just (off, g)) =
--             [ [, 0, off / textureAtlasWidth g]
--             , ]
--             -- ^ Shader should divide offset by width of atlas to get tex_coord
--     let vertices = concatMap (f . M.lookup . textureAtlasMap atlas) str
--     let vertices = [ Vertex2 (-w'+0.25) ( h'+0.25), Vertex2 (0.0) (0.0)
--                    , Vertex2 ( w'+0.25) ( h'+0.25), Vertex2 (1.0) (0.0)
--                    , Vertex2 ( w'+0.25) (0+0.25), Vertex2 (1.0) (1.0)
--                    , Vertex2 (-w'+0.25) (0+0.25), Vertex2 (0.0) (1.0)
--                    ] :: [Vertex2 GLfloat]
--         elements = [Vertex3 0 1 2, Vertex3 2 3 0] :: [Vertex3 GLuint]
--     glBindBuffer GL_ARRAY_BUFFER arrayBuffer
--     withArray vertices $ \ptr -> do
--         let size = fromIntegral $ length vertices * sizeOf (head vertices)
--         bufferData ArrayBuffer $= (size, ptr, StaticDraw)
-- 
--     glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
--     withArray elements $ \ptr -> do
--         let size = fromIntegral $ length elements * sizeOf (head elements)
--         bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)
-- 
--     -- let pos = AttribLocation 0
--     -- vertexAttribPointer pos $= (ToFloat, VertexArrayDescriptor 2 Float (4*4) (bufferOffset 0))
--     -- vertexAttribArray pos $= Enabled
-- 
--     let pos = 0
--     glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 16 $ bufferOffset 0
--     glEnableVertexAttribArray pos
-- 
--     let tex = 1
--     glVertexAttribPointer tex 2 GL_FLOAT GL_FALSE 16 $ bufferOffset 8
--     glEnableVertexAttribArray tex
--     return text_vao

type Vec3 a = (a, a, a)
type Vec4 a = (a, a, a, a)

-- String input contains no line breaks
textVertices :: TextureAtlas -> String -> Maybe [Vec4 (Vec3 GLfloat)]
textVertices atlas "" = Nothing
textVertices atlas str = do
    (atlas_offsets, glyphs) <- unzip <$> mapM (`M.lookup` textureAtlasMap atlas) str
    let xs = map fromIntegral $ foldl' (\(x:xs) g -> (x + glyph_width g):x:xs) [0] glyphs
        hs = map (fromIntegral . glyph_height) glyphs
        ws = map (fromIntegral . glyph_width) glyphs
    return $ zipWith4 charVertices xs hs ws $ map fromIntegral atlas_offsets
    where
        -- charVertices :: Int -> Char -> [(GLfloat, GLfloat, GLfloat, GLfloat)]
        charVertices :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Vec4 (Vec3 GLfloat)
        charVertices x height width offset =
            let top_left = (x, 0, offset)
                bot_left = (x, height, offset)
                bot_right = (x + width, height, offset + width)
                top_right = (x + width, 0, offset + width)
            in (top_left, bot_left, bot_right, top_right)
