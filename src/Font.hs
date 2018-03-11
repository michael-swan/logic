{-# LANGUAGE TemplateHaskell #-}
module Font (gohuFont) where

import Graphics.Text.PCF
import Graphics.Text.PCF.Embed
import System.IO.Unsafe
-- import Data.Vector.Storable
-- import Data.Word

gohuFont :: IO PCFText
gohuFont = $(embedPCFTextColor "fonts/gohufont-14.pcf.gz" 0xF0 0x00 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789`~!@#$%^&*()_-+=[]{}\\|;:'\",.<>/? ")
