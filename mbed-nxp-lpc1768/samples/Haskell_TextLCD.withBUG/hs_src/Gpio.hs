module Gpio where
import Control.Monad
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

foreign import ccall "c_extern.h &jhc_zeroAddress" c_jhc_zeroAddress32 :: Ptr Word32

data GpioPinDesc = GpioPinDesc { gpIndex  :: CUInt
                               , gpOffset :: Word32
                               , gpMask   :: Word32
                               , gpFIOSET :: Ptr Word32
                               , gpFIOCLR :: Ptr Word32
                               , gpFIOPIN :: Ptr Word32
                               , gpFIODIR :: Ptr Word32}

addr_LPC_GPIO_BASE, gpioBase0, gpioBase1, gpioBase2, gpioBase3, gpioBase4 :: Int
addr_LPC_GPIO_BASE = 0x2009C000
gpioBase0 = addr_LPC_GPIO_BASE + 0x00000
gpioBase1 = addr_LPC_GPIO_BASE + 0x00020
gpioBase2 = addr_LPC_GPIO_BASE + 0x00040
gpioBase3 = addr_LPC_GPIO_BASE + 0x00060
gpioBase4 = addr_LPC_GPIO_BASE + 0x00080

ptr_LPC_APB0_BASE, ptr_LPC_PINCON_BASE, ptrPinconarrayPinselBase :: Ptr Word32
ptr_LPC_APB0_BASE           = c_jhc_zeroAddress32 `plusPtr` 0x40000000
ptr_LPC_PINCON_BASE         = ptr_LPC_APB0_BASE   `plusPtr` 0x2C000
ptrPinconarrayPinselBase    = ptr_LPC_PINCON_BASE
ptrPinconarrayPinModeBase   = ptr_LPC_PINCON_BASE `plusPtr` (4 * (11 + 5))
ptrPinconarrayPinModeOdBase = ptr_LPC_PINCON_BASE `plusPtr` (4 * (11 + 5 + 10))

pinModePullUp, pinModePullDown, pinModePullNone, pinModeOpenDrain :: Word32
pinModePullUp    = 0
pinModePullDown  = 3
pinModePullNone  = 2
pinModeOpenDrain = 4

pinFunction :: Int -> Word32 -> IO ()
pinFunction pinNumber func = do
  let index  = pinNumber `shiftR` 4
      offset = (pinNumber .&. 0xf) `shiftL` 1
      pinsel :: Ptr Word32
      pinsel = ptrPinconarrayPinselBase `plusPtr` (index * 4)
  v <- peek pinsel
  poke pinsel $ v .&. complement (0x3 `shiftL` offset)
  v' <- peek pinsel
  poke pinsel $ v' .|. (func `shiftL` offset)

pinMode :: Int -> Word32 -> IO ()
pinMode pinNumber mode = do
  let index  = pinNumber `shiftR` 5
      offset = pinNumber .&. 0x1f
      drain  = (mode .&. pinModeOpenDrain) `shiftR` 2
      pinModeOd :: Ptr Word32
      pinModeOd = ptrPinconarrayPinModeOdBase `plusPtr` (index * 4)
  vd <- peek pinModeOd
  poke pinModeOd $ vd .&. complement (drain `shiftL` offset)
  vd' <- peek pinModeOd
  poke pinModeOd $ vd' .|. (drain `shiftL` offset)
  when (drain /= 0) go
  where go = do let index  = pinNumber `shiftR` 4
                    offset = (pinNumber .&. 0xf) `shiftL` 1
                    pinM :: Ptr Word32
                    pinM = ptrPinconarrayPinModeBase `plusPtr` (index * 4)
                v <- peek pinM
                poke pinM $ v .&. complement (0x3 `shiftL` offset)
                v' <- peek pinM
                poke pinM $ v' .|. (mode `shiftL` offset)

data GpioTag = GpioTag { gpioTagPinNumber :: Int
                       , gpioTagMask   :: Word32
                       , gpioTagFIOSET :: Ptr Word32 
                       , gpioTagFIOCLR :: Ptr Word32
                       , gpioTagFIOPIN :: Ptr Word32
                       , gpioTagFIODIR :: Ptr Word32 }
data PinDirection = PinOutput | PinInput
data PinState = PinSet | PinClr

gpioSet :: Int -> IO Word32
gpioSet pinNumber = do
  pinFunction pinNumber 0
  return $ 1 `shiftL` (pinNumber .&. 0x1f)

gpioInit :: Int -> PinDirection -> IO GpioTag
gpioInit pinNumber direction = do
  mask <- gpioSet pinNumber
  let pin = pinNumber + addr_LPC_GPIO_BASE
      gt  = GpioTag { gpioTagPinNumber = pinNumber
                    , gpioTagMask      = mask
                    , gpioTagFIOSET    = c_jhc_zeroAddress32 `plusPtr` (pin + 4 * 6)
                    , gpioTagFIOCLR    = c_jhc_zeroAddress32 `plusPtr` (pin + 4 * 7)
                    , gpioTagFIOPIN    = c_jhc_zeroAddress32 `plusPtr` (pin + 4 * 5)
                    , gpioTagFIODIR    = c_jhc_zeroAddress32 `plusPtr` pin }
  gpioDir gt direction
  pm direction
  return gt
  where pm PinOutput = pinMode pinNumber pinModePullNone
        pm PinInput  = pinMode pinNumber pinModePullDown

gpioMode :: GpioTag -> Word32 -> IO ()
gpioMode gt mode = pinMode (gpioTagPinNumber gt) mode

gpioDir :: GpioTag -> PinDirection -> IO ()
gpioDir gt direction = go direction
  where reg  = gpioTagFIODIR gt
        mask = gpioTagMask gt
        go PinInput  = peek reg >>= poke reg . (.&. complement mask)
        go PinOutput = peek reg >>= poke reg . (.|. mask)

gpioWrite :: GpioTag -> PinState -> IO ()
gpioWrite gt = go
  where
    regset = gpioTagFIOSET gt
    regclr = gpioTagFIOCLR gt
    mask = gpioTagMask gt
    go PinSet = poke regset mask
    go PinClr = poke regclr mask
