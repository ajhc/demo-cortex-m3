#include <stdio.h>
#include <unistd.h>
#include "jhc_rts_header.h"
#include "c_extern.h"

void
gpio_ptr(unsigned int v)
{
	printf("gpio_ptr: 0x%08x\n", v);
	usleep(100000);
}

int
main(int argc, char *argv[])
{
	hs_init(&argc,&argv);
	_amain();
	// hs_exit();
	return 0;
}
