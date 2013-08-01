import Data.Word
import Control.Monad

import Intr
import Led

main :: IO ()
main = do mapM_ ledOff [led3, led4, led5, led6, led7, led8, led9, led10]
          forever $ do
            ledOn led3
            delay 10
            ledOff led3
            delay 10
