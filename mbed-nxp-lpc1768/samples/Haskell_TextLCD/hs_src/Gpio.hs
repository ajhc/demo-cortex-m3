module Gpio where
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc

foreign import primitive "const.sizeof(gpio_t)" gpio_t_size  :: Int
foreign import primitive "const.LPC_GPIO0_BASE" addr_LPC_GPIO0_BASE :: CInt

foreign import ccall "c_extern.h gpio_init" c_gpio_init :: Ptr GpioT -> CInt -> CInt -> IO ()
foreign import ccall "c_extern.h gpio_write" c_gpio_write :: Ptr GpioT -> CInt -> IO ()

newtype {-# CTYPE "gpio_t" #-} GpioT = GpioT ()

pinInput, pinOutput :: CInt
pinInput  = 0
pinOutput = 1

-- P0_0=LPC_GPIO0_BASE, P0_1=P0_0+1 ... P0_31=P0_30+1, P1_0=P0_31+1 ...
pinName :: CInt -> CInt -> CInt
pinName major minor = addr_LPC_GPIO0_BASE + major * 32 + minor

-- void gpio_init(gpio_t *obj, PinName pin, PinDirection direction);
gpioInit :: CInt -> CInt -> IO (Ptr GpioT)
gpioInit name direction = do
  tag <- mallocBytes gpio_t_size
  c_gpio_init tag name direction
  return tag
  
-- static inline void gpio_write(gpio_t *obj, int value)
gpioWrite :: Ptr GpioT -> CInt -> IO ()
gpioWrite tag val = c_gpio_write tag val
