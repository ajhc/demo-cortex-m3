module Delay where
import Foreign.C.Types

foreign import ccall "c_extern.h wait_us" delayUs :: CInt -> IO ()
