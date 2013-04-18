#include <sys/types.h>
#include <reent.h>
//#include <sys/sysdefs.h>

#define HEAPSIZE 8*1024
unsigned char heap[HEAPSIZE];


void * _sbrk_r (struct _reent *ptr, ptrdiff_t incr) {
// caddr_t _sbrk (int incr) {
  static unsigned char *heap_end;
  unsigned char *prev_heap_end;

  /* initialize */
  if( heap_end == 0 ) heap_end = heap;

  prev_heap_end = heap_end;

  if( heap_end + incr - heap > HEAPSIZE ) {
    
    /* heap overflow—announce on stderr, perhaps? */
    // write (2, "Heap overflow!\n", 15);
    return (void*)0;
  }

  heap_end += incr;
  
  return (caddr_t) prev_heap_end;
}

 
