module Intr (delay) where
import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

import Led

foreign import ccall "c_extern.h getTimingDelay" c_getTimingDelay :: IO (Ptr Word32)
foreign import ccall "c_extern.h getIntrCount" c_getIntCount :: IO (Ptr Word32)

timingDelayDecrement :: IO ()
timingDelayDecrement = do
  p <- c_getTimingDelay
  pc <- c_getIntCount
  i <- peek p
  c <- peek pc
  poke pc (c + 1)
  when (i >= 0) $ poke p (i - 1)
  blink c
    where blink c | odd (c `div` 32) && c `mod` 32 == 0 = ledOn led10
                  | odd (c `div` 32) && c `mod` 32 == 4 = ledOff led10
                  | otherwise = return ()

foreign export ccall "timingDelayDecrement" timingDelayDecrement :: IO ()

delay :: Word32 -> IO ()
delay nTime = do
  p <- c_getTimingDelay
  poke p nTime
  let while :: IO ()
      while = do
        p' <- c_getTimingDelay
        i <- peek p'
        if (i > 0) then while else return ()
  while
