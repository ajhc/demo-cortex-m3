GCC_BIN = ../gcc-arm-none-eabi-4_7-2012q4/bin/
PROJECT = blink
OBJECTS = system_LPC17xx.o startup_LPC17xx.o main.o 
SYS_OBJECTS = 
INCLUDE_PATHS = -I. -I./LPC1768 
LIBRARY_PATHS = 
LIBRARIES = 
LINKER_SCRIPT = ./LPC1768/LPC1768.ld
BUILD_DIR = build/

############################################################################### 
AS      = $(GCC_BIN)arm-none-eabi-as
CC      = $(GCC_BIN)arm-none-eabi-gcc
CPP     = $(GCC_BIN)arm-none-eabi-g++
LD      = $(GCC_BIN)arm-none-eabi-gcc
OBJCOPY = $(GCC_BIN)arm-none-eabi-objcopy

CCLOCAL = gcc

CPU = -mcpu=cortex-m3 -mthumb
CC_FLAGS = $(CPU) -c -fno-common -fmessage-length=0 -Wall -fno-exceptions -ffunction-sections -fdata-sections -g 
CC_SYMBOLS = -DTARGET_LPC1769 -DTOOLCHAIN_GCC_ARM -DNDEBUG -D__CORTEX_M3

LD_FLAGS = -mcpu=cortex-m3 -mthumb -Wl,--gc-sections,-Map=$(PROJECT).map,--cref --specs=nano.specs
LD_SYS_LIBS = -lc -lgcc -lnosys

all: $(PROJECT).bin

clean:
	rm -f $(PROJECT).bin $(PROJECT).elf $(addprefix $(BUILD_DIR), $(OBJECTS)) $(PROJECT).map
	rmdir $(BUILD_DIR)

.s.o:
	mkdir -p $(BUILD_DIR)
	$(AS) $(CPU) -o $(addprefix $(BUILD_DIR), $@) $<

.c.o:
	mkdir -p $(BUILD_DIR)
	$(CC)  $(CC_FLAGS) $(CC_SYMBOLS) -std=gnu99   $(INCLUDE_PATHS) -o $(addprefix $(BUILD_DIR), $@) $<

.cpp.o:
	mkdir -p $(BUILD_DIR)
	$(CPP) $(CC_FLAGS) $(CC_SYMBOLS) -std=gnu++98 $(INCLUDE_PATHS) -o $(addprefix $(BUILD_DIR), $@) $<

# This is needed for NXP Cortex M devices
nxpsum:
	$(CCLOCAL) nxpsum.c -std=c99 -o nxpsum

$(PROJECT).elf: $(OBJECTS) $(SYS_OBJECTS)
	$(LD) $(LD_FLAGS) -T$(LINKER_SCRIPT) $(LIBRARY_PATHS) -o $@ $(addprefix $(BUILD_DIR), $^) $(LIBRARIES) $(LD_SYS_LIBS) $(LIBRARIES) $(LD_SYS_LIBS)

$(PROJECT).bin: $(PROJECT).elf nxpsum
	$(OBJCOPY) -O binary $< $@
	# Compute nxp checksum on .bin file here
	./nxpsum $@
