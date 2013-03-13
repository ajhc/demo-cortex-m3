CC=arm-none-eabi-gcc
AR=arm-none-eabi-ar

###########################################

vpath %.c rts

CFLAGS  = -g -O -Wall
CFLAGS += -mlittle-endian -mthumb -mcpu=cortex-m4 -march=armv7e-m
CFLAGS += -mfpu=fpv4-sp-d16 -mfloat-abi=hard
CFLAGS += -ffreestanding -nostdlib
CFLAGS += -I.
CFLAGS += -std=gnu99
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0
CFLAGS += -D_LITTLE_ENDIAN
# Below detail: http://communities.mentor.com/community/cs/archives/arm-gnu/msg01904.html
CFLAGS += -DARM_STAY_IN_THUMB_MODE -DJHC_TINY_RTS

SRCS = gc_jgc.c jhc_rts.c stableptr.c rts_support.c alloc.c

OBJS = $(SRCS:.c=.o)

.PHONY: libjhcrts.a

all: libjhcrts.a

%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $^

libjhcrts.a: $(OBJS)
	$(AR) -r $@ $(OBJS)

clean:
	rm -f $(OBJS) libjhcrts.a

