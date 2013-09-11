import Control.Monad
import System.IO.Unsafe
import Foreign.Ptr

import Delay
import Gpio
import Led
import qualified TextLCD as LCD
import EthernetInterface
import TCPSocketConnection
import ParseRss

ledBar :: [Ptr GpioT] -> Int -> IO ()
ledBar leds n = do sequence_ $ zipWith ($) acts leds
                   delayMs 100
  where acts = replicate n ledOn ++ replicate 4 ledOff

receiveAll :: Ptr TCPSocketConnectionT -> IO [String]
receiveAll tcp = unsafeInterleaveIO receiveAll' where
  receiveAll' = do
    s <- tcpSocketConnection_receive tcp
    if s == "" then return [] else do
      s' <- unsafeInterleaveIO receiveAll'
      return $ s:s'

slowPutstrDelay = delayMs 150

slowPutstr lcd str = do
  let col = LCD.columns lcd
  slowPutstr' lcd col str

slowPutstr' :: LCD.LCDState -> Int -> String -> IO ()
slowPutstr' lcd _ [] = do
  let col  = LCD.columns lcd
  slowPutstrDelay
  LCD.locate lcd 0 1
  LCD.putstr lcd $ replicate col ' '
slowPutstr' lcd 0 str@(x:xs) = do
  let col  = LCD.columns lcd
      str' = take col str
      rem  = col - length str'
  slowPutstrDelay
  LCD.locate lcd 0 1
  LCD.putstr lcd str'
  when (rem > 0) $ LCD.putstr lcd (replicate rem ' ')
  slowPutstr' lcd 0 xs
slowPutstr' lcd c str = do
  let col = LCD.columns lcd
  slowPutstrDelay
  LCD.locate lcd 0 1
  LCD.putstr lcd $ replicate c ' '
  LCD.putstr lcd $ take (col - c) str
  slowPutstr' lcd (c - 1) str

printRss lcd leds host (headline, url) = do
  ledBar leds 0
  tcp <- tcpSocketConnection_connect host 80
  ledBar leds 1
  tcpSocketConnection_send_all tcp $ "GET " ++ url ++ " HTTP/1.0\n\n"
  ledBar leds 2
  r <- receiveAll tcp
  ledBar leds 3
  LCD.cls lcd
  LCD.putstr lcd $ ">> " ++ headline
  printTitle (slowPutstr lcd) r
  tcpSocketConnection_close tcp
  ledBar leds 4
  delayMs 500
  return ()

tryDhcp lcd leds = do
  ledBar leds 1
  LCD.cls lcd
  LCD.putstr lcd "Start >>= "
--  ethernetDisconnect
  r1 <- ethernetInitDhcp
  ledBar leds 2
  LCD.putstr lcd . show $ r1
  r2 <- ethernetConnect 12000
  ledBar leds 3
  LCD.putstr lcd . show $ r2
  LCD.putstr lcd "\nIP"
  ip <- ethernetGetIpAddress
  ledBar leds 4
  LCD.putstr lcd ip
  return $ r1 == 0 && r2 == 0

main :: IO ()
main = do
  leds <- mapM initLed ledAll
  let p24 = pinName 2 2
      p26 = pinName 2 0
      p27 = pinName 0 11
      p28 = pinName 0 10
      p29 = pinName 0 5
      p30 = pinName 0 4
  lcd <- LCD.initTextLCD p24 p26 (p27, p28, p29, p30) LCD.LCD16x2
  tryDhcp lcd leds
  delayMs 500
  -- TCP
  forever $ mapM_ (printRss lcd leds "www.reddit.com")
                  [("Haskell", "http://www.reddit.com/r/haskell/.rss")
                  ,("OCaml", "http://www.reddit.com/r/ocaml/.rss")
                  ,("NetBSD", "http://www.reddit.com/r/NetBSD/.rss")
                  ,("Debian", "http://www.reddit.com/r/debian/.rss")
                  ,("Linux", "http://www.reddit.com/r/linux/.rss")
                  ]
