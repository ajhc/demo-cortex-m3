# Set the PROJ variable before including this make file.
# PROJ = blinky
# BOARD = photonsaw-v1

# Default to version 1 of the PhotonSaw board
BOARD ?= photonsaw-v1

EXECNAME=$(PROJ).elf
MAPFILE=$(PROJ).map

-include $(HOME)/.makefile

FW = ../lpc1769
JHCRTS_SRC = jhc_rts

CSRC  = $(wildcard $(FW)/drivers/*.c)
CSRC += $(wildcard $(FW)/lpcusb/*.c)
CSRC += $(wildcard $(FW)/newlib/*.c)
CSRC += $(wildcard $(FW)/fat_sd/*.c)
CSRC += $(wildcard $(FW)/$(BOARD)/*.c)
CSRC += $(wildcard $(FW)/*.c)
CSRC += $(wildcard *.c)
CSRC += $(wildcard */*.c)
CSRC += jhc_rts/rts/gc_jgc.c jhc_rts/rts/jhc_rts.c jhc_rts/rts/stableptr.c
CSRC += jhc_rts/rts/rts_support.c hs_main.c


CPSRC  = $(wildcard $(FW)/drivers/*.cpp)
CPSRC += $(wildcard $(FW)/$(BOARD)/*.cpp)
CPSRC += $(wildcard $(FW)/*.cpp)
CPSRC += $(wildcard *.cpp)
CPSRC += $(wildcard */*.cpp)

OBJS = $(CSRC:%.c=%.o) $(CPSRC:%.cpp=%.o)

CC                      = arm-none-eabi-gcc
AS                      = arm-none-eabi-as
AR                      = arm-none-eabi-ar -r
LD                      = arm-none-eabi-gcc
NM                      = arm-none-eabi-nm
OBJDUMP                 = arm-none-eabi-objdump
OBJCOPY                 = arm-none-eabi-objcopy
READELF                 = arm-none-eabi-readelf
CODESIZE                = arm-none-eabi-size
GDB                     = arm-none-eabi-gdb

CFLAGS                  = 
CFLAGS			+= -std=gnu99
CFLAGS			+= -Werror
CFLAGS                  += -Wall 
CFLAGS                  += -mcpu=cortex-m3
CFLAGS                  += -mthumb 
CFLAGS                  += -O
CFLAGS                  += -mapcs-frame 
CFLAGS                  += -D__thumb2__=1 
CFLAGS                  += -D__RAM_MODE__=0
CFLAGS                  += -msoft-float 
CFLAGS                  += -gdwarf-2 
CFLAGS                  += -mno-sched-prolog 
CFLAGS                  += -fno-hosted 
CFLAGS                  += -mtune=cortex-m3 
CFLAGS                  += -march=armv7-m 
CFLAGS                  += -mfix-cortex-m3-ldrd  
CFLAGS                  += -ffunction-sections 
CFLAGS                  += -fdata-sections 
CFLAGS                  += -fno-asynchronous-unwind-tables -fno-unwind-tables
CFLAGS                  += -I$(FW) -I$(FW)/drivers -I$(FW)/$(BOARD)
CFLAGS                  += -Ijhc_rts -I. -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0
CFLAGS                  += -D_JHC_ARM_STAY_IN_THUMB_MODE -D_JHC_JGC_NAIVEGC
CFLAGS                  += -D_JHC_JGC_STACKGROW=128 -D_JHC_JGC_LIMITED_NUM_MEGABLOCK=1
CFLAGS                  += -D_JHC_JGC_BLOCK_SHIFT=9 -D_JHC_JGC_MEGABLOCK_SHIFT=14
CFLAGS                  += -D_JHC_JGC_GC_STACK_SHIFT=8 -D_JHC_JGC_LIMITED_NUM_GC_STACK=1
CFLAGS                  += -D_LITTLE_ENDIAN

LDFLAGS = -static -mcpu=cortex-m3 -mthumb -mthumb-interwork -Wl,--start-group 
#LDFLAGS += -L$(THUMB2GNULIB) -L$(THUMB2GNULIB2)
LDFLAGS += -lgcc -lm 
LDFLAGS += -lc -lg -lstdc++ -lsupc++ 
LDFLAGS += -Wl,--end-group 
LDFLAGS += -Wl,--gc-sections
#LDFLAGS += -Wl,--print-gc-sections 
LDFLAGS += -T $(FW)/lpc1769.ld
LDFLAGS += -Xlinker -Map -Xlinker $(PROJ).map
LDFLAGS += -Wl,--defsym,jhc_zeroAddress=0

.PHONY : all
all: hs_main.c $(EXECNAME)

$(EXECNAME): $(FW)/$(BOARD)/board-pins.h $(OBJS)
	$(LD) $(OBJS) $(LDFLAGS) -o $(EXECNAME)
#	$(OBJCOPY) -O ihex $(EXECNAME) $(PROJ).hex
	$(CODESIZE) -A $(EXECNAME)

$(FW)/$(BOARD)/board-pins.h: $(FW)/$(BOARD)/board-pins.h.pl
	 $(FW)/$(BOARD)/board-pins.h.pl

hs_main.c: Main.hs
	ajhc -fffi --tdir=$(JHCRTS_SRC) -C -o $@ $<

.PHONY : flash
flash: $(EXECNAME)
	$(OBJCOPY) -O binary $(EXECNAME) $(PROJ).bin
	$(FW)/fix-lpcchecksum $(PROJ).bin
#	openocd -f $(FW)/openocd.cfg -c 'flash write_image erase $(PROJ).bin' -c 'verify_image $(PROJ).bin' -c 'reset run'
	lpc21isp -bin $(PROJ).bin /dev/ttyUSB0 115200 100000

.PHONY : gdb
gdb:
	$(GDB) -x $(FW)/gdb.config $(EXECNAME)

.PHONY : clean
clean:
	rm -rf $(OBJS) $(EXECNAME) $(PROJ).hex $(PROJ).bin $(PROJ).map */*~ *~ $(FW)/$(BOARD)/board-pins.h hs_main.c
	rm -rf jhc_rts
