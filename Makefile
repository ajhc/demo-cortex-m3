all:
	echo "Only support \"clean\" target."

clean:
	make -C rts-test clean
	make -C stbee-mini distclean

