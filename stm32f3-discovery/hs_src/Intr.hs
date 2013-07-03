module Intr (delay) where
import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "c_extern.h getTimingDelay" c_gettimingDelay :: IO (Ptr Word32)

timingDelayDecrement :: IO ()
timingDelayDecrement = do
  p <- c_gettimingDelay
  i <- peek p
  when (i >= 0) $ poke p (i - 1)

foreign export ccall "timingDelayDecrement" timingDelayDecrement :: IO ()

delay :: Word32 -> IO ()
delay nTime = do
  p <- c_gettimingDelay
  poke p nTime
  let while :: IO ()
      while = do
        p' <- c_gettimingDelay
        i <- peek p'
        if (i > 0) then while else return ()
  while
