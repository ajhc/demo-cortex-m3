module Delay where
import Foreign.C.Types

foreign import ccall "c_extern.h wait_us" delayUs :: Int -> IO ()

delayMs :: Int -> IO ()
delayMs ms = delayUs $ ms * 1000
