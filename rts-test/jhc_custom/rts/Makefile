JHCRTS_SRC = src

vpath %.c $(JHCRTS_SRC)/rts

TARGET = libjhcrts.a
SRC = gc_jgc.c jhc_rts.c stableptr.c rts_support.c profile.c
OBJ = $(SRC:.c=.o)
CFLAGS += -I$(JHCRTS_SRC)
CFLAGS += -g -std=gnu99
CFLAGS += -falign-functions=4 -ffast-math -fno-strict-aliasing
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0
CFLAGS += -D_LITTLE_ENDIAN
CFLAGS += -D_JHC_JGC_STACKGROW=128 -D_JHC_JGC_FIXED_MEGABLOCK
CFLAGS += -D_JHC_JGC_BLOCK_SHIFT=9 -D_JHC_JGC_MEGABLOCK_SHIFT=13
CFLAGS += -D_JHC_JGC_NAIVEGC

all: $(TARGET)

$(TARGET): $(OBJ)
	ar cr $(TARGET) $(OBJ)

clean:
	rm -f $(TARGET) $(OBJ) *~
	rm -rf $(JHCRTS_SRC)
