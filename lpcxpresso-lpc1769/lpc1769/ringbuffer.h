#ifndef __RINGBUFFER_H__
#define __RINGBUFFER_H__

#include "LPC17xx.h"
#include "lpc_types.h"

typedef struct {
  unsigned int mask;  // The mask to and the index with to cause it wrap around.
  volatile int start; // Index of the oldest element (where the reader reads from)
  volatile int end;   // Index just beyond the newest element (where the writer can write to)
  volatile int endHidden; // Like end, but used for inserting hidden elements.
  volatile char lock;
} RingBufferControl;

// Really simple locking primitives, be sure to never hold the lock for longer that you need to
static inline void rbLock(RingBufferControl *rb) {
  __disable_irq();  
}

static inline void rbUnlock(RingBufferControl *rb) {
  __enable_irq();
}

inline void rbReset(RingBufferControl *rb) {
  rb->start = rb->end = rb->endHidden = 0;  
}

// Initializes a ring buffer to use an array with 1<<order elements
inline void rbInit(RingBufferControl *rb, int order) {  
  rb->mask = (1<<order) -1;
  rbReset(rb);
}

inline char rbIsFull(RingBufferControl *rb) {
  return ((rb->end + 1) & rb->mask) == rb->start;
}

inline char rbIsEmpty(RingBufferControl *rb) {
  return rb->end == rb->start;
}

inline int rbLength(RingBufferControl *rb) {
  return (rb->end-rb->start) & rb->mask;
}

// Returns the index of element to write to, will overwrite the oldest element in case of overflow
inline int rbOverWrite(RingBufferControl *rb) {
  int res = rb->end;
  rb->end = (rb->end + 1) & rb->mask;
  if (rb->end == rb->start) {
    rb->start = (rb->start + 1) & rb->mask; /* full, overwrite */
  }
  return res;
}

// Returns the index of element to write to, will return -1 if buffer is full
inline int rbWrite(RingBufferControl *rb) {
  int res = rb->end;
  int newEnd = (rb->end + 1) & rb->mask;
  if (newEnd == rb->start) {
    return -1;
  } else {
    rb->end = newEnd;
    return res;
  }
}

// Returns the index of the element to read from, will return -1 if buffer is empty.
inline int rbRead(RingBufferControl *rb) {
		if (rb->start == rb->end) {
			return -1;
		}

		int res = rb->start;
    rb->start = (rb->start + 1) & rb->mask;
    return res;
}


inline int rbWriteHidden(RingBufferControl *rb) {
  int res = rb->endHidden;
  int newEnd = (rb->endHidden + 1) & rb->mask;
  if (newEnd == rb->start) {
    return -1;
  } else {
    rb->endHidden = newEnd;
    return res;
  }
}

inline char rbIsFullHidden(RingBufferControl *rb) {
  return ((rb->endHidden + 1) & rb->mask) == rb->start;
}

inline char rbIsEmptyHidden(RingBufferControl *rb) {
    return rb->endHidden == rb->start;
}

inline void rbShowHidden(RingBufferControl *rb) {
  rb->end = rb->endHidden;
}

inline void rbRemoveHidden(RingBufferControl *rb) {
  rb->endHidden = rb->end;
}

inline int rbLengthHidden(RingBufferControl *rb) {
  return (rb->endHidden-rb->start) & rb->mask;
}

// Define the ring buffer control and the array and initialize the control structure
#define RING_BUFFER(ctrl, order, type) RingBufferControl ctrl; type ctrl ## Array[1<<(order)]
#define EXTERN_RING_BUFFER(ctrl, order, type) extern RingBufferControl ctrl; extern type ctrl ## Array[1<<(order)];

// Read an element (be sure to check rbIsEmpty first or you will crash!)
#define RB_READ(ctrl) ctrl ## Array[rbRead(&ctrl)]

// Writes an element (be sure to check rbIsFull first or you will crash!)
#define RB_WRITE(ctrl, value) ctrl ## Array[rbWrite(&ctrl)]=value;

// Writes an element, possibly overwriting the oldest element
#define RB_OVERWRITE(ctrl, value) ctrl ## Array[rbOverWrite(&ctrl)]=value;



#endif
