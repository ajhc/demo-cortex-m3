import Control.Monad
import System.IO.Unsafe

import Delay
import Gpio
import qualified TextLCD as LCD
import EthernetInterface
import TCPSocketConnection
import ParseRss

receiveAll tcp = unsafeInterleaveIO receiveAll' where
  receiveAll' = do
    s <- tcpSocketConnection_receive tcp
    if s == "" then return [] else do
      s' <- unsafeInterleaveIO receiveAll'
      return $ s ++ s'

main :: IO ()
main = do
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
  tcp <- tcpSocketConnection_connect "www.reddit.com" 80
  tcpSocketConnection_send_all tcp "GET http://www.reddit.com/r/haskell/.rss HTTP/1.0\n\n"
  r <- receiveAll tcp
  printTitle (LCD.putc lcd) r
