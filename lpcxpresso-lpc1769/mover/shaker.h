#ifndef __SHAKER_H__
#define __SHAKER_H__

#include "axis.h"

/*
  This is the number of micro seconds between each stepper interrupt fires,
  it is also the maximum time the IRQ routine is allowed to take.
*/

#define STEPPER_TIMER_INTERVAL_US 20
#define MOVE_BUFFER_ORDER 12

void shakerInit();

extern unsigned int stepperIRQMax;
extern int stepperIRQAvg;

int motionActive();
unsigned int motionDuration();
unsigned int motionMoveID();
unsigned int motionMoveOffset();


// True if it's possible to add more move codes
char bufferIsFull();

// Add one move code to the buffer, it is not executed yet, though
void bufferMoveCode(unsigned int code);

// Release the buffered move for execution
void bufferCommit();

// Undo uncommitted writes to the buffer
void bufferRollback();

// Number of move codes available in the buffer
int bufferAvailable();

// The commited number of move codes ready for execution
int bufferInUse();

// True if there are no moves ready for execution
char bufferIsEmpty();

// Stop any action and purge the move buffer, do not call this from shaker itself or it will deadlock!
void shakerResetBuffer();

// Jog in the specified direction for one second.
void setJogSpeed(int *axes);

#define AXIS_X 0
#define AXIS_Y 1
#define AXIS_Z 2
#define AXIS_A 3

extern Axis axes[4];

#endif
