#ifndef __STATE_H__
#define __STATE_H__

#include "api.h"

/*
  Output all of the available state to the file
*/
void printState(FILE *file);
void printAlarmState(FILE *file);
void printBufferState(FILE *file);
void printMotionState(FILE *file);

#define WD_STATE_MAX 100
typedef struct {
  SYSTICK_TYPE timestamp;
  char msg[WD_STATE_MAX];  
  int bad;
} WatchdogState;

extern WatchdogState *wdState;

#endif
