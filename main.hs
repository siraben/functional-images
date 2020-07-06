{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign
import Foreign.C.Types

foreign import ccall "color.h" cMain :: IO CInt

main = cMain
