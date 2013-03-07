import Data.Word
import Data.Bits
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "c_extern.h Delay" c_delay :: Word32 -> IO ()
foreign import ccall "c_extern.h &jhc_zeroAddress" c_jhc_zeroAddress16 :: Ptr Word16

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

main :: IO ()
main = forever $ sequence_ dos
  where
    delays = repeat $ c_delay 20
--    leds = [led3, led4, led5, led6, led7, led8, led9, led10] -- will crash!
    leds = [led3] -- will crash after a few moments
    ledsOnOff = fmap ledOn leds ++ fmap ledOff leds
    dos = concat $ zipWith (\a b -> [a,b]) ledsOnOff delays

{--
(gdb) c
Continuing.
^C
Program received signal SIGTRAP, Trace/breakpoint trap.
abort () at rts/rts_support.c:82
82      void abort() {
(gdb) bt
#0  abort () at rts/rts_support.c:82
#1  0x080015c0 in malloc (size=size@entry=4112) at rts/alloc.c:210
#2  0x080015f8 in realloc (ptr=<optimized out>, size=4112) at rts/alloc.c:269
#3  0x08000bde in stack_grow (s=s@entry=0x20007f54, grow=grow@entry=1028) at rts/gc_jgc.c:48
#4  0x08000dfc in stack_check (n=4, s=0x20007f54) at rts/gc_jgc.c:60
#5  gc_perform_gc (gc=0x20000160 <gc_stack_base_area+16>) at rts/gc_jgc.c:113
#6  0x0800101e in get_free_block (arena=0x200025bc <malloc_heapstart+4>, gc=<optimized out>) at rts/gc_jgc.c:331
#7  s_alloc (gc=gc@entry=0x20000160 <gc_stack_base_area+16>, sc=0x2000261c <malloc_heapstart+100>) at rts/gc_jgc.c:463
#8  0x08001c00 in fR$__fJhc_Basics_$pp (gc=gc@entry=0x20000150 <gc_stack_base_area>, v110947982=v110947982@entry=0x8001fcc <_c1>, 
    v29534740=v29534740@entry=0x8001fd4 <_c2>) at hs_main.c:403
#9  0x08001e78 in ftheMain (gc=0x20000150 <gc_stack_base_area>) at hs_main.c:539
#10 b__main (gc=0x20000150 <gc_stack_base_area>) at hs_main.c:236
#11 _amain () at hs_main.c:230
#12 0x08001892 in main () at src/main.c:172
(gdb)
--}
