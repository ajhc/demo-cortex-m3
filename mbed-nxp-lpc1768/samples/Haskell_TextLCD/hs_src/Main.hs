import Control.Monad

import Led
import Delay
import Gpio
import qualified TextLCD as LCD


space :: String
space =  replicate 16 ' '

logo :: [String]
logo = replicate 2 space ++ [ "   _ _          "
                            , "  (_) |__   ___ "
                            , "  | | '_ \\ / __|"
                            , "  | | | | | (__ "
                            , " _/ |_| |_|\\___|"
                            , "|__/            " ] ++ replicate 2 space

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
  let ledOnActs  = fmap (ledsOn  . (flip take $ ledList)) [1..4]
      ledOffActs = fmap (ledsOff . (flip take $ ledList)) [1..4]
      ledActs    = ledOnActs ++ ledOffActs
      sliceLogo n = concat . take 2 . drop n $ logo
      lcdActs    = fmap (LCD.putstr lcd . sliceLogo) [0..(length logo - 2)]
      delayActs  = replicate 20 $ delayUs 150000
  sequence_ $ zipWith3 (\a b c -> a >> b >> c) ledActs lcdActs delayActs
