module Delay where
import Foreign.C.Types

foreign import ccall "c_extern.h delay" delay :: CInt -> IO ()
