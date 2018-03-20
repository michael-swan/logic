{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Canvas.Shader
    ( ShaderPrograms(..)
    , compileShaderPrograms ) where

import Text.RawString.QQ
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.StateVar
import Graphics.GL.Core32
import Graphics.GL.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String
import GL

data ShaderPrograms = ShaderPrograms { shader_default :: GLuint
                                     , shader_lines :: GLuint
                                     , shader_text :: GLuint }

compileShaderPrograms :: IO (Either [String] ShaderPrograms)
compileShaderPrograms = runExceptT $
    ShaderPrograms <$> compileShaderProgram defaultVertexShader defaultFragmentShader ["position", "texcoord"]
                   <*> compileShaderProgram lineVertexShader lineFragmentShader ["position", "texcoord"]
                   <*> compileShaderProgram textVertexShader textFragmentShader ["ox", "y", "ax"]

compileShaderProgram :: ByteString -> ByteString -> [ByteString] -> ExceptT [String] IO GLuint
compileShaderProgram vs_source fs_source attrs = do
    vs <- makeShader GL_VERTEX_SHADER vs_source
    fs <- makeShader GL_FRAGMENT_SHADER fs_source
    makeProgram [vs, fs] $ zip attrs [0..]

makeShader :: GLenum -> ByteString -> ExceptT [String] IO GLuint
makeShader shader_type shader_source = do
    shader <- glCreateShader shader_type
    liftIO $ alloca $ \source_ptr -> alloca $ \length_ptr ->
        unsafeUseAsCString shader_source $ \source -> do
            poke length_ptr $ fromIntegral $ BS.length shader_source
            poke source_ptr source
            glShaderSource shader 1 source_ptr length_ptr
    glCompileShader shader
    compile_status <- liftIO $ alloca $ \compile_status_ptr -> do
        glGetShaderiv shader GL_COMPILE_STATUS compile_status_ptr
        peek compile_status_ptr
    if compile_status == GL_FALSE then do
        shaderLog <- maybe [] (:[]) <$> getShaderInfoLog shader
        shaderErrors <- getErrors
        throwE $ shaderLog ++ map (("Error Code: " ++) . show) shaderErrors
    else
        return shader

makeProgram :: [GLuint] -> [(ByteString, GLuint)] -> ExceptT [String] IO GLuint
makeProgram shaders attributes = do
    program <- glCreateProgram
    mapM_ (glAttachShader program) shaders
    liftIO $ do
        mapM_ (\(name, loc) -> unsafeUseAsCString name $ glBindAttribLocation program loc) attributes
        unsafeUseAsCString "outColor" $ glBindFragDataLocation program 0
    glLinkProgram program
    glValidateProgram program
    link_status <- liftIO $ alloca $ \link_status_ptr -> do
        glGetProgramiv program GL_LINK_STATUS link_status_ptr
        peek link_status_ptr
    validate_status <- liftIO $ alloca $ \validate_status_ptr -> do
        glGetProgramiv program GL_VALIDATE_STATUS validate_status_ptr
        peek validate_status_ptr
    if link_status == 0 || validate_status == 0 then do
        programLog <- maybe [] (:[]) <$> getProgramInfoLog program
        programErrors <- getErrors
        throwE $ programLog ++ map (("Error Code: " ++) . show) programErrors
    else
        return program

defaultVertexShader :: ByteString
defaultVertexShader = [r|
#version 300 es
precision highp float;
in vec2 position;
in vec2 texcoord;
out vec2 Texcoord;
uniform mat4 transform;
void main()
{
    Texcoord = texcoord;
    gl_Position = transform * vec4(position, 0.0, 1.0);
}
|]

defaultFragmentShader :: ByteString
defaultFragmentShader = [r|
#version 300 es
precision highp float;
in vec2 Texcoord;
out vec4 outColor;
uniform sampler2D tex;
void main()
{
    outColor = texture(tex, Texcoord);
}
|]

lineVertexShader :: ByteString
lineVertexShader = [r|
#version 300 es
precision highp float;
in vec2 position;
uniform mat4 transform;
void main()
{
    gl_Position = transform * vec4(position, 1.0, 1.0);
}
|]

lineFragmentShader :: ByteString
lineFragmentShader = [r|
#version 300 es
precision highp float;
out vec4 outColor;
void main()
{
    outColor = vec4(1.0, 0.0, 0.0, 1.0);
}
|]

textVertexShader :: ByteString
textVertexShader = [r|
#version 300 es
precision highp float;
in float ox;
in float y;
in float ax;
out float out_y;
out float out_ax;
uniform mat4 transform;
uniform float atlas_height;
uniform float atlas_width;
void main()
{
    out_y = y / atlas_height;
    out_ax = ax / atlas_width;
    gl_Position = transform * vec4(ox, y, 0.0, 1.0);
}
|]

textFragmentShader :: ByteString
textFragmentShader = [r|
#version 300 es
precision highp float;
in float y;
in float ax;
out vec4 outColor;
uniform sampler2D texture_atlas;
void main()
{
    outColor = texture(texture_atlas, vec2(ax, y));
}
|]

getShaderInfoLog :: MonadIO m => GLuint -> m (Maybe String)
getShaderInfoLog = getInfoLog glGetShaderInfoLog glGetShaderiv

getProgramInfoLog :: MonadIO m => GLuint -> m (Maybe String)
getProgramInfoLog = getInfoLog glGetProgramInfoLog glGetProgramiv

getInfoLog :: MonadIO m => (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
                        -> (GLuint -> GLenum -> Ptr GLint -> IO ())
                        -> GLuint
                        -> m (Maybe String)
getInfoLog getInfoLog getVar object = do
    log_length <- liftIO $ alloca $ \log_length_ptr -> do
        getVar object GL_INFO_LOG_LENGTH log_length_ptr
        peek log_length_ptr
    if log_length /= 0 then
        liftIO $ allocaBytes (fromIntegral log_length) $ \log_ptr -> do
            getInfoLog object log_length nullPtr log_ptr 
            Just <$> peekCString log_ptr
    else
        return Nothing
