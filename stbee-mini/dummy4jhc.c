#include "jhc_rts_header.h"

static struct _reent g_impure_ptr;
struct _reent *_impure_ptr = &g_impure_ptr;

void abort() {
	for (;;);
}

void exit(int status) {
	abort();
}

void *memalign(size_t __alignment, size_t __size) {
	abort();
}

char *setlocale(int category, const char *locale) {
	return NULL;
}

int fputs(const char *s, FILE *stream) {
	return 0;
}

int fputc(int c, FILE *stream) {
	return 0;
}

int fprintf(FILE *stream, const char *format, ...) {
	return 0;
}

int fflush(FILE* stream) {
	return 0;
}

void jhc_print_profile(void) {
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
