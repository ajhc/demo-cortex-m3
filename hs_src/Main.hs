import Data.Word
import Data.Bits
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

{--
lib/CMSIS/Core/CM3/stm32f10x.h
define PERIPH_BASE           ((uint32_t)0x40000000) /*!< SRAM base address in
define APB2PERIPH_BASE       (PERIPH_BASE + 0x10000)
define GPIOA_BASE            (APB2PERIPH_BASE + 0x0800)
typedef struct
{
  __IO uint32_t CRL;
  __IO uint32_t CRH;
  __IO uint32_t IDR;
  __IO uint32_t ODR;
  __IO uint32_t BSRR;
  __IO uint32_t BRR;
  __IO uint32_t LCKR;
} GPIO_TypeDef;
--}

foreign import ccall "c_extern.h Delay" c_delay :: Word32 -> IO ()

gpioPtr :: Ptr Word32
gpioPtr = odr
  where periphBase     = nullPtr        `plusPtr` 0x40000000
        arb2periphBase = periphBase     `plusPtr` 0x10000
        gpioaBase      = arb2periphBase `plusPtr` 0x0800
        odr            = gpioaBase      `plusPtr` 12

main :: IO ()
main = forever $ do
  poke gpioPtr (1 `shiftL` 13)
  c_delay 500000
  poke gpioPtr (1 `shiftL` 15)
  c_delay 500000
