# Some defaults that the project makefile should override:
AVR               ?= atmega88
UART_BAUD         ?= 9600
F_CPU             ?= 8000000
SLEEP_10_MS_COUNT ?= 79

# These are the different programmers that I use with avrdude, to choose one, create a ~/.makefile 
# that sets the variable, that way different users can have different programmers

#AVR_PROGRAMMER = -c usbtiny -P usb
#AVR_PROGRAMMER = -c avrispv2 -P usb
AVR_PROGRAMMER ?= -c jtag2isp -P usb

-include $(HOME)/.makefile

FW = ../atmega8

CSRC  = $(wildcard $(FW)/*.c) $(wildcard $(FW)/*/*.c) $(wildcard *.c) $(wildcard */*.c)
CPSRC = $(wildcard $(FW)/*.cpp) $(wildcard $(FW)/*/*.cpp) $(wildcard *.cpp) $(wildcard */*.cpp)

OBJS = $(CSRC:%.c=%.o) $(CPSRC:%.cpp=%.o)

CC=avr-gcc
SIZE=avr-size


CFLAGS  = -Wall -pedantic-errors -Werror -mcall-prologues -mmcu=$(AVR) -std=c99 -g
CFLAGS  += -I$(FW)


# Optimization flags, :

CFLAGS += -Os
CFLAGS += -fdata-sections -ffunction-sections

CFLAGS += -funsigned-char
CFLAGS += -funsigned-bitfields
CFLAGS += -fpack-struct
CFLAGS += -fshort-enums 

CFLAGS += -finline-limit=3
CFLAGS += -fno-inline-small-functions 



# Configuration macros:

CFLAGS += -D SLEEP_10_MS_COUNT=$(SLEEP_10_MS_COUNT)
CFLAGS += -D UART_BAUD=$(UART_BAUD)
CFLAGS += -D F_CPU=$(F_CPU)


CPPFLAGS=-fno-exceptions

LDFLAGS = -lc -mmcu=$(AVR) -Wl,--gc-sections -Wl,-Map,$(PROJ).map -Wl,--print-gc-sections 
LDFLAGS += -Wl,--relax

#LDFLAGS += -L/usr/lib/avr/lib
#LDFLAGS += -lm

program : $(PROJ).hex
	avrdude $(AVR_PROGRAMMER) -p $(AVR) -B 1 -e -U flash:w:$(PROJ).hex

bin : $(PROJ).hex

# Notice: We must include this particular libm if if we want to use float at all: /usr/lib/avr/lib/libm.a 

# This uses whole-program optimization so there's only one compilation unit:
$(PROJ).hex : $(CSRC) $(CPSRC) $(INTERMEDIATE)
	$(CC) $(CFLAGS) --combine -fwhole-program $(CPFLAGS) $(LDFLAGS) $(CSRC) $(CPSRC) -o $(PROJ).elf
	avr-objcopy -O ihex $(PROJ).elf $(PROJ).hex
	$(SIZE) --format=berkeley $(PROJ).elf
	$(SIZE) --format=avr --mcu=$(AVR) $(PROJ).elf

# This builds each source file to an OBJ and then links it to a binary at the end, it produces slightly
# larger code with worse optimization, so I don't recommend using it.
#$(PROJ)-o.hex : $(OBJS) $(INTERMEDIATE)
#	$(CC) $(LDFLAGS) $(OBJS) -o $(PROJ)-o.elf
#	avr-objcopy -O ihex $(PROJ)-o.elf $(PROJ)-o.hex
#	$(SIZE) --format=berkeley $(PROJ)-o.elf
#	$(SIZE) --format=avr --mcu=$(AVR) $(PROJ)-o.elf

# Internal oscilator 8MHz, no prescaler, BOD=2.7V:
fuses8int:
	avrdude $(AVR_PROGRAMMER) -p $(AVR) -B 100 -U lfuse:w:0xe2:m -U hfuse:w:0xdc:m -U efuse:w:0xf9:m 

clean :
	rm -f $(PROJ).hex $(PROJ).elf $(PROJ).map $(OBJS) $(INTERMEDIATE)
