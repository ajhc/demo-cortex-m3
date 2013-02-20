TARGET = libjhcrts.a
SRC = rts/gc_jgc.c rts/jhc_rts.c rts/stableptr.c rts/rts_support.c
OBJ = $(SRC:.c=.o)
CFLAGS += -I.
CFLAGS += -std=gnu99
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0

all: $(TARGET)

$(TARGET): $(OBJ)
	ar cr $(TARGET) $(OBJ)

clean:
	rm -f $(TARGET) $(OBJ) *~
