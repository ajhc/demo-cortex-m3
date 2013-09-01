module TextLCD where
import Control.Monad
import Data.Char
import Data.Bits
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr

import Gpio
import Delay

data LCDType  = LCD16x2 | LCD16x2B | LCD20x2 | LCD20x4
data LCDState = LCDState { lcdGpioRs  :: Ptr GpioT
                         , lcdGpioE   :: Ptr GpioT
                         , lcdGpioBus :: [Ptr GpioT] -- Fixed 4 entry
                         , lcdType    :: LCDType
                         , lcdColumn  :: IORef Int
                         , lcdRow     :: IORef Int }

-- TextLCD lcd(p24, p26, p27, p28, p29, p30); // rs, e, d0-d3
--   p24=P2_2 p26=P2_0 p27=P0_11 p28=P0_10 p29=P0_5 p30=P0_4
initTextLCD :: Int -> Int -> (Int, Int, Int, Int) -> LCDType -> IO LCDState
initTextLCD rs e bus lcdt = do
  gRs  <- gpioInit rs pinOutput
  gE   <- gpioInit e pinOutput
  gBus <- mapTuple4 ((flip gpioInit) pinOutput) bus
  col  <- newIORef 0
  row  <- newIORef 0
  let lcd = LCDState { lcdGpioRs  = gRs
                     , lcdGpioE   = gE
                     , lcdGpioBus = gBus
                     , lcdType    = lcdt
                     , lcdColumn  = col
                     , lcdRow     = row }
  -- command mode
  gpioWrite gE 1 >> gpioWrite gRs 0
  delayMs 15
  replicateM_ 3 $ displaySetting lcd
  writeByte lcd 0x2
  delayUs 40
  writeCommand lcd 0x28
  writeCommand lcd 0x0c
  writeCommand lcd 0x6
  cls lcd
  return lcd
  where mapTuple4 f (a, b, c, d) = do
          mapM f [a, b, c, d] >>= return -- Fixed 4 entry
        displaySetting lcd = do
          writeByte lcd 0x3
          delayUs 16400

character :: LCDState -> Int -> Int -> Int -> IO ()
character ls c r char = do
  let a = address ls c r
  writeCommand ls a
  writeData ls char

cls :: LCDState -> IO ()
cls lcd = do
  writeCommand lcd 0x01
  delayUs 16400
  locate lcd 0 0

locate :: LCDState -> Int -> Int -> IO ()
locate (LCDState { lcdGpioRs  = gRs
                 , lcdGpioE   = gE
                 , lcdGpioBus = gBus
                 , lcdType    = lcdt
                 , lcdColumn  = col
                 , lcdRow     = row }) c r = do
  modifyIORef col $ const c
  modifyIORef row $ const r

putc :: LCDState -> Char -> IO ()
putc lcd@(LCDState { lcdGpioRs  = gRs
                   , lcdGpioE   = gE
                   , lcdGpioBus = gBus
                   , lcdType    = lcdt
                   , lcdColumn  = col
                   , lcdRow     = row }) = go
  where go '\n' = do modifyIORef col $ const 0
                     modifyIORef row (1 +)
                     r <- readIORef row
                     when (r >= rows lcd) $ modifyIORef row $ const 0
        go v    = do c <- readIORef col
                     r <- readIORef row
                     character lcd c r $ ord v
                     let c' = c + 1
                     modifyIORef col $ const c'
                     when (c' >= columns lcd) $ do modifyIORef col $ const 0
                                                   let r' = r + 1
                                                   modifyIORef row $ const r'
                                                   when (r' >= rows lcd) $ modifyIORef row $ const 0

writeByte :: LCDState -> Int -> IO ()
writeByte (LCDState { lcdGpioRs  = gRs
                    , lcdGpioE   = gE
                    , lcdGpioBus = gBus
                    , lcdType    = lcdt
                    , lcdColumn  = col
                    , lcdRow     = row }) val = do
  sequence_ $ zipWith gpioWrite gBus (bits (shiftR val 4))
  delayUs 40 >> gpioWrite gE 0 >> delayUs 40 >> gpioWrite gE 1
  sequence_ $ zipWith gpioWrite gBus (bits val)
  delayUs 40 >> gpioWrite gE 0 >> delayUs 40 >> gpioWrite gE 1
  return ()
  where bits :: Int -> [Int]
        bits v = fmap (.&. v) $ fmap bit [0..3]

writeCommand :: LCDState -> Int -> IO ()
writeCommand lcd@(LCDState { lcdGpioRs  = gRs
                           , lcdGpioE   = gE
                           , lcdGpioBus = gBus
                           , lcdType    = lcdt
                           , lcdColumn  = col
                           , lcdRow     = row }) cmd = do
  gpioWrite gRs 0
  writeByte lcd cmd

writeData :: LCDState -> Int -> IO ()
writeData lcd@(LCDState { lcdGpioRs  = gRs
                        , lcdGpioE   = gE
                        , lcdGpioBus = gBus
                        , lcdType    = lcdt
                        , lcdColumn  = col
                        , lcdRow     = row }) dat = do
  gpioWrite gRs 1
  writeByte lcd dat

address :: LCDState -> Int -> Int -> Int
address (LCDState { lcdGpioRs  = gRs
                  , lcdGpioE   = gE
                  , lcdGpioBus = gBus
                  , lcdType    = lcdt
                  , lcdColumn  = col
                  , lcdRow     = row }) c r = go lcdt
  where go LCD20x4 = case r of 0 -> 0x80 + c
                               1 -> 0xc0 + c
                               2 -> 0x94 + c
                               3 -> 0xd4 + c
        go LCD16x2B = 0x80 + r * 40 + c;
        go _ = 0x80 + r * 0x40 + c;

columns :: LCDState -> Int
columns (LCDState { lcdType = lcdt }) =
  case lcdt of LCD20x4   -> 20
               LCD20x2   -> 20
               otherwise -> 16

rows :: LCDState -> Int
rows (LCDState { lcdType = lcdt }) =
  case lcdt of LCD20x4   -> 4
               otherwise -> 2
