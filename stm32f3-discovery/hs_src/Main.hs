import Data.Word
import Data.Bits
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "c_extern.h Delay" c_delay :: Word32 -> IO ()

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

brrPtr :: Ptr Word16
brrPtr = nullPtr `plusPtr` 0x48001028

bsrrPtr :: Ptr Word16
bsrrPtr = nullPtr `plusPtr` 0x48001018

ledOff :: Word16 -> IO ()
ledOff n = do
  poke brrPtr n

ledOn :: Word16 -> IO ()
ledOn n = do
  poke bsrrPtr n

main :: IO ()
main = forever $ do
  ledOn  led3
  ledOff led5
  ledOn  led7
  ledOff led9
  ledOn  led10
  ledOff led8
  ledOn  led6
  ledOff led4
  c_delay 50
  ledOff led3
  ledOn  led5
  ledOff led7
  ledOn  led9
  ledOff led10
  ledOn  led8
  ledOff led6
  ledOn  led4
  c_delay 50

{--
define GPIO_Pin_8                 ((uint16_t)0x0100)  /*!< Pin 8 selected    */
define GPIO_Pin_9                 ((uint16_t)0x0200)  /*!< Pin 9 selected    */
define GPIO_Pin_10                ((uint16_t)0x0400)  /*!< Pin 10 selected   */
define GPIO_Pin_11                ((uint16_t)0x0800)  /*!< Pin 11 selected   */
define GPIO_Pin_12                ((uint16_t)0x1000)  /*!< Pin 12 selected   */
define GPIO_Pin_13                ((uint16_t)0x2000)  /*!< Pin 13 selected   */
define GPIO_Pin_14                ((uint16_t)0x4000)  /*!< Pin 14 selected   */
define GPIO_Pin_15                ((uint16_t)0x8000)  /*!< Pin 15 selected   */
define LED3_PIN                         GPIO_Pin_9
define LED4_PIN                         GPIO_Pin_8
define LED5_PIN                         GPIO_Pin_10
define LED6_PIN                         GPIO_Pin_15
define LED7_PIN                         GPIO_Pin_11
define LED8_PIN                         GPIO_Pin_14
define LED9_PIN                         GPIO_Pin_12
define LED10_PIN                        GPIO_Pin_13
const uint16_t GPIO_PIN[LEDn] = {LED3_PIN, LED4_PIN, LED5_PIN, LED6_PIN,
                                 LED7_PIN, LED8_PIN, LED9_PIN, LED10_PIN};
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

void STM_EVAL_LEDOn(Led_TypeDef Led)
{
  GPIO_PORT[Led]->BSRR = GPIO_PIN[Led];
}
void STM_EVAL_LEDOff(Led_TypeDef Led)
{
  GPIO_PORT[Led]->BRR = GPIO_PIN[Led];
}

STM_EVAL_LEDOff(LED3);
 8001560:       2000            movs    r0, #0
 8001562:       f000 f879       bl      8001658 <STM_EVAL_LEDOff>

void STM_EVAL_LEDOff(Led_TypeDef Led)
{
  GPIO_PORT[Led]->BRR = GPIO_PIN[Led];
 8001658:       4b03            ldr     r3, [pc, #12]   ; (8001668 <STM_EVAL_LEDOff+0x10>) r3 = 0x20000034
 800165a:       4a04            ldr     r2, [pc, #16]   ; (800166c <STM_EVAL_LEDOff+0x14>) r2 = 0x0800185c
 800165c:       f853 3020       ldr.w   r3, [r3, r0, lsl #2]
 8001660:       f832 2010       ldrh.w  r2, [r2, r0, lsl #1]
 8001664:       851a            strh    r2, [r3, #40]   ; 0x28
 8001666:       4770            bx      lr
 8001668:       20000034        .word   0x20000034
 800166c:       0800185c        .word   0x0800185c

void STM_EVAL_LEDOn(Led_TypeDef Led)
{
  GPIO_PORT[Led]->BSRR = GPIO_PIN[Led];
 8001640:       4b03            ldr     r3, [pc, #12]   ; (8001650 <STM_EVAL_LEDOn+0x10>)
 8001642:       4a04            ldr     r2, [pc, #16]   ; (8001654 <STM_EVAL_LEDOn+0x14>)
 8001644:       f853 3020       ldr.w   r3, [r3, r0, lsl #2]
 8001648:       f832 2010       ldrh.w  r2, [r2, r0, lsl #1]
 800164c:       619a            str     r2, [r3, #24]
 800164e:       4770            bx      lr
 8001650:       20000034        .word   0x20000034
 8001654:       0800185c        .word   0x0800185c

(gdb) p &(GPIO_PORT[0]->BRR)
$26 = (volatile uint16_t *) 0x48001028
(gdb) p &(GPIO_PORT[0]->BSRR)
$31 = (volatile uint32_t *) 0x48001018

(gdb) p GPIO_PIN[0]
$16 = 512
(gdb) p GPIO_PIN[1]
$17 = 256
(gdb) p GPIO_PIN[2]
$18 = 1024
(gdb) p GPIO_PIN[3]
$19 = 32768
(gdb) p GPIO_PIN[4]
$20 = 2048
(gdb) p GPIO_PIN[5]
$21 = 16384
(gdb) p GPIO_PIN[6]
$22 = 4096
(gdb) p GPIO_PIN[7]
$23 = 8192
--}
