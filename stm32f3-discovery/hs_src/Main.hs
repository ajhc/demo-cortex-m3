import Data.Word
import Data.Bits
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "c_extern.h Delay" c_delay :: Word32 -> IO ()

gpioPtr :: Ptr Word32
gpioPtr = odr
  where periphBase     = nullPtr        `plusPtr` 0x40000000
        arb2periphBase = periphBase     `plusPtr` 0x10000
        gpioaBase      = arb2periphBase `plusPtr` 0x0800
        odr            = gpioaBase      `plusPtr` 12

gpioOut :: Word32 -> IO ()
gpioOut v = do
  poke gpioPtr v
  c_delay 500000

main :: IO ()
main = forever $ mapM_ gpioOut pat
  where
    o13 = (1 `shiftL` 13)
    o15 = (1 `shiftL` 15)
    pat = [o13, o13 .|. o15, o15, 0]

{--
define LED3_PIN                         GPIO_Pin_9
const uint16_t GPIO_PIN[LEDn] = {LED3_PIN, LED4_PIN, LED5_PIN, LED6_PIN,
                                 LED7_PIN, LED8_PIN, LED9_PIN, LED10_PIN};
void STM_EVAL_LEDOn(Led_TypeDef Led)
{
  GPIO_PORT[Led]->BSRR = GPIO_PIN[Led];
}
void STM_EVAL_LEDOff(Led_TypeDef Led)
{
  GPIO_PORT[Led]->BRR = GPIO_PIN[Led];  
}
typedef enum 
{
  LED3 = 0,
  LED4 = 1,
  LED5 = 2,
  LED6 = 3,
  LED7 = 4,
  LED8 = 5,
  LED9 = 6,
  LED10 = 7
} Led_TypeDef;
STM_EVAL_LEDOff(LED3);
--}
