import Control.Monad

import Led
import Delay
import Gpio
import qualified TextLCD as LCD
import EthernetInterface
import TCPSocketConnection

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
  -- DHCP
  LCD.putstr lcd "start. "
  ethernetInitDhcp >>= LCD.putstr lcd . show
  ethernetConnect 12000 >>= LCD.putstr lcd . show
  LCD.putstr lcd "\nIP"
  ethernetGetIpAddress >>= LCD.putstr lcd
  -- TCP
  tcp <- tcpSocketConnection_connect "mbed.org" 80
  tcpSocketConnection_send_all tcp "GET /media/uploads/mbed_official/hello.txt HTTP/1.0\n\n"
  tcpSocketConnection_receive tcp >>= LCD.putstr lcd
  tcpSocketConnection_receive tcp >>= LCD.putstr lcd
  tcpSocketConnection_receive tcp >>= LCD.putstr lcd
  realmain ledList lcd

realmain ledList lcd = forever $ do
  let ledOnActs  = fmap (ledsOn  . (flip take $ ledList)) [1..4]
      ledOffActs = fmap (ledsOff . (flip take $ ledList)) [1..4]
      ledActs    = ledOnActs ++ ledOffActs
      delayActs  = replicate 20 $ delayUs 150000
  sequence_ $ zipWith (\a b -> a >> b) ledActs delayActs
