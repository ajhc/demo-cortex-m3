import Data.Word
import Data.Bits
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "c_extern.h delay" c_delay :: Word32 -> IO ()
foreign import ccall "c_extern.h &jhc_zeroAddress" c_jhc_zeroAddress16 :: Ptr Word16

gpioPin8, gpioPin9, gpioPin10, gpioPin11, gpioPin12, gpioPin13, gpioPin14, gpioPin15, led3, led4, led5, led6, led7, led8, led9, led10 :: Word16
gpioPin8  = 0x0100
gpioPin9  = 0x0200
gpioPin10 = 0x0400
gpioPin11 = 0x0800
gpioPin12 = 0x1000
gpioPin13 = 0x2000
gpioPin14 = 0x4000
gpioPin15 = 0x8000
led3  = gpioPin9
led4  = gpioPin8
led5  = gpioPin10
led6  = gpioPin15
led7  = gpioPin11
led8  = gpioPin14
led9  = gpioPin12
led10 = gpioPin13

brrPtr, bsrrPtr :: Ptr Word16
brrPtr  = c_jhc_zeroAddress16 `plusPtr` 0x48001028
bsrrPtr = c_jhc_zeroAddress16 `plusPtr` 0x48001018

ledOff, ledOn :: Word16 -> IO ()
ledOff = poke brrPtr
ledOn  = poke bsrrPtr

main :: IO ()
main = return ()
