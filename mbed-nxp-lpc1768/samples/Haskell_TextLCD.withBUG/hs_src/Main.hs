import Control.Monad

import Led
import Gpio
import Delay

main :: IO ()
main = do
  gt1 <- initLed led1
  realmain gt1

realmain :: GpioTag -> IO ()
realmain gt1 = forever $ do
  ledCtrl gt1 LedOn
  delayUs 100000
  ledCtrl gt1 LedOff
  delayUs 100000
