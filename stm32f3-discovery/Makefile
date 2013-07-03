# put your *.o targets here, make should handle the rest!
SRCS = main.c errno.c stm32f3_discovery.c system_stm32f30x.c hs_main.c alloc.c dummy4jhc.c

# all the files will be generated with this name (main.elf, main.bin, main.hex, etc)
PROJ_NAME=main

# Location of the Libraries folder from the STM32F0xx Standard Peripheral Library
STD_PERIPH_LIB=Libraries
JHCRTS_LIB=jhc_custom/rts
JHCRTS_SRC=jhc_custom/rts/src

# Location of the linker scripts
LDSCRIPT_INC=Device/ldscripts

# location of OpenOCD Board .cfg files (only used with 'make program')
OPENOCD_BOARD_DIR=/home/matt/bin/openocd/share/openocd/scripts/board

# Configuration (cfg) file containing programming directives for OpenOCD
OPENOCD_PROC_FILE=extra/stm32f3-openocd.cfg

# that's it, no need to change anything below this line!

###################################################

CC=arm-none-eabi-gcc
OBJCOPY=arm-none-eabi-objcopy
OBJDUMP=arm-none-eabi-objdump
SIZE=arm-none-eabi-size
GDB=arm-none-eabi-gdb

CFLAGS  = -Wall -g -std=gnu99 -O
CFLAGS += -mlittle-endian -mcpu=cortex-m4  -march=armv7e-m -mthumb
CFLAGS += -mfpu=fpv4-sp-d16 -mfloat-abi=hard
CFLAGS += -ffunction-sections -fdata-sections
CFLAGS += -Wl,--gc-sections -Wl,-Map=$(PROJ_NAME).map
CFLAGS += -Wl,--defsym,jhc_zeroAddress=0

###################################################

vpath %.c src
vpath %.a $(STD_PERIPH_LIB)

ROOT=$(shell pwd)

CFLAGS += -I inc 
CFLAGS += -I $(STD_PERIPH_LIB) 
CFLAGS += -I $(STD_PERIPH_LIB)/CMSIS/Device/ST/STM32F30x/Include
CFLAGS += -I $(STD_PERIPH_LIB)/CMSIS/Include 
CFLAGS += -I $(STD_PERIPH_LIB)/STM32F30x_StdPeriph_Driver/inc
CFLAGS += -I $(STD_PERIPH_LIB)/STM32_USB-FS-Device_Driver/inc
CFLAGS += -include $(STD_PERIPH_LIB)/stm32f30x_conf.h
CFLAGS += -I . -I $(JHCRTS_SRC)
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0 -D_BIG_ENDIAN

SRCS += Device/startup_stm32f30x.s # add startup file to build

# need if you want to build with -DUSE_CMSIS 
#SRCS += stm32f0_discovery.c
#SRCS += stm32f0_discovery.c stm32f0xx_it.c

OBJS = $(SRCS:.c=.o)

###################################################

.PHONY: lib proj

all: lib jhcrts proj

lib:
	$(MAKE) -C $(STD_PERIPH_LIB)

jhcrts: hs_main.c
	$(MAKE) -C $(JHCRTS_LIB)

hs_main.c: hs_src/Main.hs
	ajhc -fffi -p containers --tdir=$(JHCRTS_SRC) -C --include=hs_src -o $@ $<

proj: 	$(PROJ_NAME).elf

$(PROJ_NAME).elf: $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@ -L$(STD_PERIPH_LIB) -lstm32f3 -L$(JHCRTS_LIB) -ljhcrts -L$(LDSCRIPT_INC) -Tstm32f3.ld
	$(OBJCOPY) -O ihex $(PROJ_NAME).elf $(PROJ_NAME).hex
	$(OBJCOPY) -O binary $(PROJ_NAME).elf $(PROJ_NAME).bin
	$(OBJDUMP) -St $(PROJ_NAME).elf >$(PROJ_NAME).lst
	$(SIZE) $(PROJ_NAME).elf

gdbwrite: all
	@echo '################################################'
	@echo '##### Use me after running "sudo st-util". #####'
	@echo '################################################'
	$(GDB) -x gdbwrite.boot $(PROJ_NAME).elf

gdbattach: all
	@echo '################################################'
	@echo '##### Use me after running "sudo st-util". #####'
	@echo '################################################'
	$(GDB) -x gdbattach.boot $(PROJ_NAME).elf

program: $(PROJ_NAME).bin
	openocd -f $(OPENOCD_BOARD_DIR)/stm32f3discovery.cfg -f $(OPENOCD_PROC_FILE) -c "stm_flash `pwd`/$(PROJ_NAME).bin" -c shutdown

clean:
	find ./ -name '*~' | xargs rm -f	
	rm -f *.o hs_main.c *.symlist
	rm -f $(PROJ_NAME).elf
	rm -f $(PROJ_NAME).hex
	rm -f $(PROJ_NAME).bin
	rm -f $(PROJ_NAME).map
	rm -f $(PROJ_NAME).lst
	$(MAKE) -C $(STD_PERIPH_LIB) clean
	$(MAKE) -C $(JHCRTS_LIB) clean

nohs.symlist: all
	rm -f all.symlist hs.symlist
	nm $(PROJ_NAME).elf | grep -v ' $$d' | grep -v ' $$t' | grep -v 'o:$$' | grep "^[0-9a-zA-Z]" > all.symlist
	nm alloc.o dummy4jhc.o hs_main.o jhc_custom/rts/libjhcrts.a | grep -v ' $$d' | grep -v ' $$t' | grep -v 'o:$$' | grep "^[0-9a-zA-Z]" > hs.symlist
	runghc symlist_noths.hs all.symlist hs.symlist > $@
