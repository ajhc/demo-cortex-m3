import Data.Word
import Control.Monad

import Intr
import Led

main :: IO ()
main = do mapM_ ledOff [led3, led4, led5, led6, led7, led8, led9, led10]
          forever $ sequence_ dos
          -- forever $! sequence_ dos -- will crash
  where
    delays = repeat $ delay 4
    leds = [led3, led4, led5, led6, led7, led8, led9] -- led10
    ledsOnOff = fmap ledOn leds ++ fmap ledOff leds
    dos = concat $ zipWith (\a b -> [a,b]) ledsOnOff delays
