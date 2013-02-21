/* Copy from http://stackoverflow.com/questions/6083337/overriding-malloc-using-the-ld-preload-mechanism */
#include <stdio.h>

#define __USE_GNU
#include <dlfcn.h>

static void* (*real_malloc)(size_t)=NULL;

static void __mtrace_init(void)
{
	real_malloc = dlsym(RTLD_NEXT, "malloc");
	if (NULL == real_malloc) {
		fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
		return;
	}
}

void *malloc(size_t size)
{
	if(real_malloc==NULL)
		__mtrace_init();

	void *p = NULL;
	fprintf(stderr, "malloc(%d) = ", size);
	p = real_malloc(size);
	fprintf(stderr, "%p\n", p);
	return p;
}
