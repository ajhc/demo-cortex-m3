import Control.Monad
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall "c_extern.h set_gpioa_13" c_set_gpioa_13 :: IO ()
foreign import ccall "c_extern.h set_gpioa_15" c_set_gpioa_15 :: IO ()

main :: IO ()
main = forever $ do
  c_set_gpioa_13
  c_set_gpioa_15
