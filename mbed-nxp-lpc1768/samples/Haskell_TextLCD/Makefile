TRGT  = arm-none-eabi-
GDB   = $(TRGT)gdb
HSDIR = hs_src
HSBUILD = build_haskell
HSSRC = $(wildcard $(HSDIR)/*.hs $(HSDIR)/*/*.hs $(HSDIR)/*/*/*.hs $(HSDIR)/*/*/*/*.hs $(HSDIR)/*/*/*/*/*.hs)

all: $(HSBUILD)/hs_main.c
	make -f forc.mk

$(HSBUILD)/hs_main.c: $(HSSRC)
	mkdir -p $(HSBUILD)
	ajhc -fffi -p containers --tdir=$(HSBUILD) -C --include=hs_src -o $@ hs_src/Main.hs
	rm -f $(HSBUILD)/rts/gc_none.c $(HSBUILD)/rts/profile.c $(HSBUILD)/rts/slub.c

clean:
	rm -rf build_haskell
	make -f forc.mk clean

gdbwrite gdbattach: all
	make -f forc.mk $@
