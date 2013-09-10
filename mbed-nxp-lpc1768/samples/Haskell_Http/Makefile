HSBUILD = build_haskell
TINI = $(HSBUILD)/targets.ini

all:: $(HSBUILD)/hs_main.c
	make -f forc.mk

include forc.mk

$(HSBUILD)/hs_main.c: $(HSSRC)
	mkdir -p $(HSBUILD)
	echo "[mbed]"               > $(TINI)
	echo "cc=arm-none-eabi-gcc" >> $(TINI)
	echo "byteorder=le"         >> $(TINI)
	echo "cflags=$(GCFLAGS)"    >> $(TINI)
	echo "cflags_debug="        >> $(TINI)
	echo "cflags_nodebug="      >> $(TINI)
	ajhc --targetsini=$(TINI) --cross -mmbed -fffi --tdir=$(HSBUILD) -C --include=hs_src -o $@ $(HSMAIN)
	rm -f $(HSBUILD)/rts/gc_none.c $(HSBUILD)/rts/profile.c $(HSBUILD)/rts/slub.c
