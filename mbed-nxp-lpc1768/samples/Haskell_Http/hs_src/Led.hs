module Led where
import Foreign.C.Types
import Foreign.Ptr

import Gpio

-- LED1 = P1.18  LED2 = P1.20  LED3 = P1.21  LED4 = P1.23
led1, led2, led3, led4 :: Int
led1 = pinName 1 18
led2 = pinName 1 20
led3 = pinName 1 21
led4 = pinName 1 23
ledAll :: [Int]
ledAll = [led1, led2, led3, led4]

initLed :: Int -> IO (Ptr GpioT)
initLed name = gpioInit name pinOutput

ledOn, ledOff :: Ptr GpioT -> IO ()
ledOn  l = gpioWrite l 1
ledOff l = gpioWrite l 0

ledsOn, ledsOff :: [Ptr GpioT] -> IO ()
ledsOn  ls = mapM_ ledOn ls
ledsOff ls = mapM_ ledOff ls
