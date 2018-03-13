module GL (getErrors) where

import Control.Monad.IO.Class
import Graphics.GL.Core32
import Graphics.GL.Types

getErrors :: MonadIO m => m [GLuint]
getErrors = do
    error <- glGetError
    if error /= GL_NO_ERROR then
        (error:) <$> getErrors
    else
        return []
