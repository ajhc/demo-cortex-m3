import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

import Intr

foreign import ccall "c_extern.h &jhc_zeroAddress" c_jhc_zeroAddress16 :: Ptr Word16

gpioPin9, led3 :: Word16
gpioPin9 = 0x0200
led3     = gpioPin9

brrPtr, bsrrPtr :: Ptr Word16
brrPtr  = c_jhc_zeroAddress16 `plusPtr` 0x48001028
bsrrPtr = c_jhc_zeroAddress16 `plusPtr` 0x48001018

ledOff, ledOn :: Word16 -> IO ()
ledOff = poke brrPtr
ledOn  = poke bsrrPtr

main :: IO ()
main = forever $ do
  ledOn led3
  delay 10
  ledOff led3
  delay 10
