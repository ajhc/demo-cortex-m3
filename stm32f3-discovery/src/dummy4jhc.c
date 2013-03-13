#include "jhc_rts_header.h"

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

int fprintf(FILE *stream, const char *format, ...) {
	return 0;
}

int fflush(FILE* stream) {
	return 0;
}

void jhc_print_profile(void) {
}
