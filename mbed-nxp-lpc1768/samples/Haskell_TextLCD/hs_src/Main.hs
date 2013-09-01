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
  ledsOn $ take 1 ledList
  LCD.putc lcd 'a'
  delayUs 100000
  ledsOn $ take 2 ledList
  LCD.putc lcd 'b'
  delayUs 100000
  ledsOn $ take 3 ledList
  LCD.putc lcd 'c'
  delayUs 100000
  ledsOn $ take 4 ledList
  LCD.putc lcd 'd'
  delayUs 100000
  ledsOff $ take 1 ledList
  LCD.putc lcd 'e'
  delayUs 100000
  ledsOff $ take 2 ledList
  LCD.putc lcd 'f'
  delayUs 100000
  ledsOff $ take 3 ledList
  LCD.putc lcd 'g'
  delayUs 100000
  ledsOff $ take 4 ledList
  delayUs 100000
