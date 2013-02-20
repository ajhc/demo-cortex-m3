import Data.Word
import Data.Bits
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "c_extern.h gpio_ptr" c_gpioPtr :: Word32 -> IO ()

gpioOut :: [Word32] -> IO ()
gpioOut v = do
  let v' = foldl (.|.) 0 v
  c_gpioPtr v'

main :: IO ()
main = forever $ do
  gpioOut [1 `shiftL` 13]
  gpioOut [1 `shiftL` 13, 1 `shiftL` 15]
  gpioOut [1 `shiftL` 15]
  gpioOut []
