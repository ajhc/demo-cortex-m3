#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

#include "HsFFI.h"
#include "rts/rts_support.h"
#include "rts/profile.h"
#include "rts/gc.h"

jmp_buf jhc_uncaught;
int jhc_argc;
char **jhc_argv;
char *jhc_progname;

#ifdef __WIN32__
A_UNUSED char *jhc_options_os =  "mingw32";
A_UNUSED char *jhc_options_arch = "i386";
#elif defined(__ARM_EABI__)
A_UNUSED char *jhc_options_os =  "nds";
A_UNUSED char *jhc_options_arch = "ARM";
#else
A_UNUSED char *jhc_options_os = "(unknown os)";
A_UNUSED char *jhc_options_arch = "(unknown arch)";
#endif

void
hs_set_argv(int argc, char *argv[])
{
        jhc_argc = argc - 1;
        jhc_argv = argv + 1;
        jhc_progname = argv[0];
}

void A_NORETURN A_UNUSED A_COLD
jhc_exit(int n) {
        abort();
}

void  A_NORETURN A_UNUSED  A_COLD
jhc_error(char *s) {
        abort();
}

void  A_NORETURN A_UNUSED  A_COLD
jhc_case_fell_off(int n) {
        abort();
}

void jhc_hs_init(void);

static int hs_init_count;
void
hs_init(int *argc, char **argv[])
{

        if(!hs_init_count++) {
                jhc_alloc_init();
                jhc_hs_init();
                hs_set_argv(*argc,*argv);
#if JHC_isPosix
                struct utsname jhc_utsname;
                if(!uname(&jhc_utsname)) {
                        jhc_options_arch = jhc_utsname.machine;
                        jhc_options_os   = jhc_utsname.sysname;
                }
#endif
        }
}

void
hs_exit(void)
{
        if(!hs_init_count) {
                abort();
        }
        if(!--hs_init_count) {
                jhc_alloc_fini();
		abort();
        }
}

void abort() {
	for (;;);
}

/* copy from http://ja.wikipedia.org/wiki/Memset */
void *memset(void *str, int c, size_t num)
{
	unsigned char *ptr = (unsigned char *)str;
	const unsigned char ch = c;

	while(num--)
		*ptr++ = ch;

	return str;
}
