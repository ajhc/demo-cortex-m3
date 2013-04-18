# Set the PROJ variable before including this make file.
# PROJ = blinky
# BOARD = photonsaw-v1

# Default to version 1 of the PhotonSaw board
BOARD ?= photonsaw-v1

EXECNAME=$(PROJ).elf
MAPFILE=$(PROJ).map

-include $(HOME)/.makefile

FW = ../lpc1769

CSRC  = $(wildcard $(FW)/drivers/*.c)
CSRC += $(wildcard $(FW)/lpcusb/*.c)
CSRC += $(wildcard $(FW)/newlib/*.c)
CSRC += $(wildcard $(FW)/fat_sd/*.c)
CSRC += $(wildcard $(FW)/$(BOARD)/*.c)
CSRC += $(wildcard $(FW)/*.c)
CSRC += $(wildcard *.c)
CSRC += $(wildcard */*.c)

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
CFLAGS			+= -std=c99
CFLAGS			+= -Werror
CFLAGS                  += -Wall 
CFLAGS                  += -mcpu=cortex-m3
CFLAGS                  += -mthumb 
CFLAGS                  += -O2
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

LDFLAGS = -static -mcpu=cortex-m3 -mthumb -mthumb-interwork -Wl,--start-group 
#LDFLAGS += -L$(THUMB2GNULIB) -L$(THUMB2GNULIB2)
LDFLAGS += -lgcc -lm 
LDFLAGS += -lc -lg -lstdc++ -lsupc++ 
LDFLAGS += -Wl,--end-group 
LDFLAGS += -Wl,--gc-sections
#LDFLAGS += -Wl,--print-gc-sections 
LDFLAGS += -T $(FW)/lpc1769.ld
LDFLAGS += -Xlinker -Map -Xlinker $(PROJ).map

.PHONY : all
all: $(EXECNAME)

$(EXECNAME): $(FW)/$(BOARD)/board-pins.h $(OBJS)
	$(LD) $(OBJS) $(LDFLAGS) -o $(EXECNAME)
#	$(OBJCOPY) -O ihex $(EXECNAME) $(PROJ).hex
	$(CODESIZE) -A $(EXECNAME)

$(FW)/$(BOARD)/board-pins.h: $(FW)/$(BOARD)/board-pins.h.pl
	 $(FW)/$(BOARD)/board-pins.h.pl

.PHONY : flash
flash: $(EXECNAME)
	$(OBJCOPY) -O binary $(EXECNAME) $(PROJ).bin
	$(FW)/fix-lpcchecksum $(PROJ).bin
	openocd -f $(FW)/openocd.cfg -c 'flash write_image erase $(PROJ).bin' -c 'verify_image $(PROJ).bin' -c 'reset run'

.PHONY : gdb
gdb:
	$(GDB) -x $(FW)/gdb.config $(EXECNAME)

.PHONY : clean
clean:
	rm -r $(OBJS) $(EXECNAME) $(PROJ).hex $(PROJ).bin $(PROJ).map */*~ *~ $(FW)/$(BOARD)/board-pins.h
