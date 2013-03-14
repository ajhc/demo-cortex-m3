CC=arm-none-eabi-gcc
AR=arm-none-eabi-ar

###########################################

JHCRTS_SRC = ../../../jhc_custom/rts

vpath %.c $(JHCRTS_SRC)/rts

CFLAGS  = -g -O -Wall
CFLAGS += -mlittle-endian -mthumb -mcpu=cortex-m4 -march=armv7e-m
CFLAGS += -mfpu=fpv4-sp-d16 -mfloat-abi=hard
CFLAGS += -ffreestanding -nostdlib
CFLAGS += -I$(JHCRTS_SRC)
CFLAGS += -std=gnu99
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0
CFLAGS += -D_LITTLE_ENDIAN
CFLAGS += -D_JHC_ARM_STAY_IN_THUMB_MODE
CFLAGS += -D_JHC_JGC_STACKGROW=128 -D_JHC_JGC_FIXED_MEGABLOCK
CFLAGS += -D_JHC_JGC_BLOCK_SHIFT=9 -D_JHC_JGC_MEGABLOCK_SHIFT=14
CFLAGS += -D_JHC_JGC_NAIVEGC

SRCS = gc_jgc.c jhc_rts.c stableptr.c rts_support.c

OBJS = $(SRCS:.c=.o)

.PHONY: libjhcrts.a

all: libjhcrts.a

%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $^

libjhcrts.a: $(OBJS)
	$(AR) -r $@ $(OBJS)

clean:
	rm -f $(OBJS) libjhcrts.a

