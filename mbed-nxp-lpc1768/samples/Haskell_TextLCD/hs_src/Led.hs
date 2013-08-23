module Led where
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "c_extern.h &jhc_zeroAddress" c_jhc_zeroAddress32 :: Ptr Word32

addr_LPC_GPIO_BASE, addr_LPC_GPIO1_BASE, addr_FIODIR, addr_FIOPIN :: Ptr Word32
addr_LPC_GPIO_BASE = c_jhc_zeroAddress32 `plusPtr` 0x2009C000
addr_LPC_GPIO1_BASE = addr_LPC_GPIO_BASE `plusPtr` 0x00020
addr_FIODIR = addr_LPC_GPIO1_BASE
addr_FIOPIN = addr_LPC_GPIO1_BASE `plusPtr` (4 + 4 * 3 + 4) -- see LPC_GPIO_TypeDef

-- LED1 = P1.18  LED2 = P1.20  LED3 = P1.21  LED4 = P1.23
led1, led2, led3, led4, ledAll :: Word32
led1 = 1 `shiftL` 18
led2 = 1 `shiftL` 20
led3 = 1 `shiftL` 21
led4 = 1 `shiftL` 23
ledAll = foldl (.|.) 0 [led1, led2, led3, led4]

initLeds :: IO ()
initLeds = poke addr_FIODIR ledAll

ledsOn :: [Word32] -> IO ()
ledsOn ls = poke addr_FIOPIN $ foldl (.|.) 0 ls
