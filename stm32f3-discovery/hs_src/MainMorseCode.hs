import Data.Maybe
import Data.Word
import Data.Bits
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Data.Map (Map)
import qualified Data.Map as Map

foreign import ccall "c_extern.h Delay" c_delay :: Word32 -> IO ()
foreign import ccall "c_extern.h &jhc_zeroAddress" c_jhc_zeroAddress16 :: Ptr Word16

--------
--- LED
--------
gpioPin8, gpioPin9, gpioPin10, gpioPin11, gpioPin12, gpioPin13, gpioPin14, gpioPin15, led3, led4, led5, led6, led7, led8, led9, led10 :: Word16
gpioPin8  = 0x0100
gpioPin9  = 0x0200
gpioPin10 = 0x0400
gpioPin11 = 0x0800
gpioPin12 = 0x1000
gpioPin13 = 0x2000
gpioPin14 = 0x4000
gpioPin15 = 0x8000
led3  = gpioPin9
led4  = gpioPin8
led5  = gpioPin10
led6  = gpioPin15
led7  = gpioPin11
led8  = gpioPin14
led9  = gpioPin12
led10 = gpioPin13

brrPtr, bsrrPtr :: Ptr Word16
brrPtr  = c_jhc_zeroAddress16 `plusPtr` 0x48001028
bsrrPtr = c_jhc_zeroAddress16 `plusPtr` 0x48001018

ledOff, ledOn :: Word16 -> IO ()
ledOff = poke brrPtr
ledOn  = poke bsrrPtr

---------------
--- Morse Code
---------------
data Morse = S | L | LetterSpace | WordSpace

-- http://en.wikipedia.org/wiki/Morse_code
morseCodes :: Map Char [Morse]
morseCodes = Map.fromList [('A', [S, L, LetterSpace]),
                           ('B', [L, S, S, S, LetterSpace]),
                           ('C', [L, S, L, S, LetterSpace]),
                           ('D', [L, S, S, LetterSpace]),
                           ('E', [S, LetterSpace]),
                           ('F', [S, S, L, S, LetterSpace]),
                           ('G', [L, L, S, LetterSpace]),
                           ('H', [S, S, S, S, LetterSpace]),
                           ('I', [S, S, LetterSpace]),
                           ('J', [S, L, L, L, LetterSpace]),
                           ('K', [L, S, L, LetterSpace]),
                           ('L', [S, L, S, S, LetterSpace]),
                           ('M', [L, L, LetterSpace]),
                           ('N', [L, S, LetterSpace]),
                           ('O', [L, L, L, LetterSpace]),
                           ('P', [S, L, L, S, LetterSpace]),
                           ('Q', [L, L, S, L, LetterSpace]),
                           ('R', [S, L, S, LetterSpace]),
                           ('S', [S, S, S, LetterSpace]),
                           ('T', [L, LetterSpace]),
                           ('U', [S, S, L, LetterSpace]),
                           ('V', [S, S, S, L, LetterSpace]),
                           ('W', [S, L, L, LetterSpace]),
                           ('X', [L, S, S, L, LetterSpace]),
                           ('Y', [L, S, L, L, LetterSpace]),
                           ('Z', [L, L, S, S, LetterSpace]),
                           ('1', [S, L, L, L, L, LetterSpace]),
                           ('2', [S, S, L, L, L, LetterSpace]),
                           ('3', [S, S, S, L, L, LetterSpace]),
                           ('4', [S, S, S, S, L, LetterSpace]),
                           ('5', [S, S, S, S, S, LetterSpace]),
                           ('6', [L, S, S, S, S, LetterSpace]),
                           ('7', [L, L, S, S, S, LetterSpace]),
                           ('8', [L, L, L, S, S, LetterSpace]),
                           ('9', [L, L, L, L, S, LetterSpace]),
                           ('0', [L, L, L, L, L, LetterSpace]),
                           (' ', [WordSpace])]

morseEncodeChar :: Char -> [Morse]
morseEncodeChar c = fromMaybe notFound $ Map.lookup c morseCodes
  where notFound = [L, L, L, L, L, L, L, L]

morseEncode :: String -> [Morse]
morseEncode = concatMap morseEncodeChar

sigOn, sigOff, clockOn, clockOff :: IO ()
sigOn    = ledOn  led4
sigOff   = ledOff led4
clockOn  = ledOn  led5
clockOff = ledOff led5

morseToIO :: Morse -> [IO ()]
morseToIO S           = [sigOn, sigOff]
morseToIO L           = [sigOn, sigOn, sigOn, sigOff]
morseToIO LetterSpace = [sigOff, sigOff]
morseToIO WordSpace   = [sigOff, sigOff, sigOff, sigOff]

morseEncodeIO :: String -> [IO ()]
morseEncodeIO = concatMap morseToIO . morseEncode

main :: IO ()
main = forever $ sequence_ dos
  where
    rawString = "HELLO WORLD"
    delays = repeat $ c_delay 20
    sigs = morseEncodeIO rawString
    clocks = cycle [clockOn, clockOff]
    dos = concat $ zipWith3 (\a b c -> [a,b,c]) sigs clocks delays
