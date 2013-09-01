import Control.Monad

import Led
import Delay
import Gpio
import qualified TextLCD as LCD

main :: IO ()
main = do
  ledList <- mapM initLed [led1, led2, led3, led4]
  let p24 = pinName 2 2
      p26 = pinName 2 0
      p27 = pinName 0 11
      p28 = pinName 0 10
      p29 = pinName 0 5
      p30 = pinName 0 4
  lcd <- LCD.initTextLCD p24 p26 (p27, p28, p29, p30) LCD.LCD16x2
  realmain ledList lcd

realmain ledList lcd = forever $ do
  let ledOnActs  = fmap (ledsOn  . (flip take $ ledList)) [0..4]
      ledOffActs = fmap (ledsOff . (flip take $ ledList)) [0..4]
      ledActs    = ledOnActs ++ ledOffActs
      lcdActs    = fmap (LCD.putc lcd) $ ['a'..'z'] ++ ['A'..'Z']
      delayActs  = replicate 20 $ delayUs 100000
  sequence_ $ zipWith3 (\a b c -> a >> b >> c) ledActs lcdActs delayActs
