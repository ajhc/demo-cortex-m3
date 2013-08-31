import Control.Monad
import Data.Word

import Led
import Delay

main :: IO ()
main = do
  initLeds
  realmain

ledList :: [Word32]
ledList = [led1, led2, led3, led4]

realmain :: IO ()
realmain = forever $ do
  ledsOn $ take 1 ledList
  delay 1000000
  ledsOn $ take 2 ledList
  delay 1000000
  ledsOn $ take 3 ledList
  delay 1000000
  ledsOn $ take 4 ledList
  delay 1000000
  ledsOn $ drop 1 ledList
  delay 1000000
  ledsOn $ drop 2 ledList
  delay 1000000
  ledsOn $ drop 3 ledList
  delay 1000000
  ledsOn $ drop 4 ledList
  delay 1000000
