import Data.Word
import Data.Bits
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "c_extern.h &jhc_zeroAddress" c_jhc_zeroAddress32 :: Ptr Word32
foreign import ccall "c_extern.h delay"    c_delay    :: Word32 -> IO ()
foreign import ccall "c_extern.h led2_on"  c_led2_on  :: IO ()
foreign import ccall "c_extern.h led2_off" c_led2_off :: IO ()

lpcGpio0Fioset, lpcGpio0Fioclr :: Ptr Word32
lpcGpio0Fioset = c_jhc_zeroAddress32 `plusPtr` 0x2009c024
lpcGpio0Fioclr = c_jhc_zeroAddress32 `plusPtr` 0x2009c028

led2 :: Word32
led2 = 0x400000

ledOff, ledOn :: Word32 -> IO ()
{-
ledOff = poke lpcGpio0Fioclr
ledOn  = poke lpcGpio0Fioset
-}
ledOn = const c_led2_on
ledOff = const c_led2_off

main :: IO ()
main = forever $ do
  ledOn led2
  c_delay 200
  ledOff led2
  c_delay 200
