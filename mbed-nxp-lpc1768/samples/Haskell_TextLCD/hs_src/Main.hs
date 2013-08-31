import Control.Monad

import Led
import Delay

main :: IO ()
main = do
  ledList <- mapM initLed [led1, led2, led3, led4]
  realmain ledList

realmain ledList = forever $ do
  ledsOn $ take 1 ledList
  delayUs 100000
  ledsOn $ take 2 ledList
  delayUs 100000
  ledsOn $ take 3 ledList
  delayUs 100000
  ledsOn $ take 4 ledList
  delayUs 100000
  ledsOff $ take 1 ledList
  delayUs 100000
  ledsOff $ take 2 ledList
  delayUs 100000
  ledsOff $ take 3 ledList
  delayUs 100000
  ledsOff $ take 4 ledList
  delayUs 100000
