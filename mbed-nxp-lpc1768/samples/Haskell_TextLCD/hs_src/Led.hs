module Led where

import Gpio

-- LED1 = P1.18  LED2 = P1.20  LED3 = P1.21  LED4 = P1.23
led1, led2, led3, led4 :: Int
led1 = 1 * 32 + 18
led2 = 1 * 32 + 20
led3 = 1 * 32 + 21
led4 = 1 * 32 + 23

data LedState = LedOn | LedOff

ledList :: [Int]
ledList = [led1, led2, led3, led4]

initLed :: Int -> IO GpioTag
initLed l = gpioInit l PinOutput  

ledCtrl :: GpioTag -> LedState -> IO ()
ledCtrl gt LedOn  = gpioWrite gt PinSet
ledCtrl gt LedOff = gpioWrite gt PinClr
