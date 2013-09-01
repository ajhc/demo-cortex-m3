SUBDIRS := rts-test stbee-mini stm32f3-discovery mbed-nxp-lpc1768

all clean:
	@for i in $(SUBDIRS); do \
		$(MAKE) -C $$i $@; \
	done

.PHONY: all clean $(SUBDIRS)
