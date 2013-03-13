SUBDIRS := rts-test stbee-mini stm32f3-discovery

all clean:
	@for i in $(SUBDIRS); do \
		$(MAKE) -C $$i $@; \
	done

.PHONY: all clean $(SUBDIRS)
