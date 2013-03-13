JHCRTS_LIB=jhc_custom/rts
JHCRTS_SRC=../jhc_custom/rts

TARGET = rts-test
SRC = main.c hs_main.c
OBJ = $(SRC:.c=.o)
CFLAGS += -I. -I$(JHCRTS_SRC)
CFLAGS += -g -std=gnu99
CFLAGS += -falign-functions=4 -ffast-math -fno-strict-aliasing
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0
LDFLAGS = -Ljhc_custom/rts -ljhcrts
RTS = jhc_custom/rts/libjhcrts.a

all: $(TARGET)

$(TARGET): $(RTS) $(OBJ)
	gcc $(OBJ) -o $@ $(LDFLAGS)

$(RTS):
	make -C $(JHCRTS_LIB)

hs_main.c : hs_src/Main.hs
	ajhc -fffi -C -o $@ $<

mtrace.so: mtrace.c
	gcc -shared -fPIC -o mtrace.so mtrace.c -ldl

test: all
	./$(TARGET)

test_ldpreload: all mtrace.so
	LD_PRELOAD=./mtrace.so ./$(TARGET)

clean:
	make -C $(JHCRTS_LIB) clean
	rm -f $(TARGET) $(OBJ) mtrace.so hs_main.c *~

.PHONY: clean test
