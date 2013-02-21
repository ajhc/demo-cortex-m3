TARGET = rts-test
SRC = main.c hs_main.c
OBJ = $(SRC:.c=.o)
CFLAGS += -I. -Ijhc_custom/rts
CFLAGS += -std=gnu99
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0
LDFLAGS = -Ljhc_custom/rts -ljhcrts
RTS = jhc_custom/rts/libjhcrts.a

all: $(TARGET)

$(TARGET): $(RTS) $(OBJ)
	gcc $(OBJ) -o $@ $(LDFLAGS)

$(RTS):
	make -C jhc_custom/rts

hs_main.c : hs_src/Main.hs
	jhc -fffi -C -o $@ $<

mtrace.so: mtrace.c
	gcc -shared -fPIC -o mtrace.so mtrace.c -ldl

test: all
	./$(TARGET)

test_ldpreload: all mtrace.so
	LD_PRELOAD=./mtrace.so ./$(TARGET)

clean:
	make -C jhc_custom/rts clean
	rm -f $(TARGET) $(OBJ) mtrace.so hs_main.c *~

.PHONY: clean test
