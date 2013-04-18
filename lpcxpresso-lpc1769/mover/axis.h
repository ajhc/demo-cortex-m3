#ifndef __AXIS_H__
#define __AXIS_H__

#include "stepper.h"
#include "move.h"
#include "api.h"

typedef struct {
  int volatile position;     // Current step position.
  Stepper stepper;

  int moveError;    // 0..ONE_STEP-1 
  int moveSpeed;    // Never negative.
  int moveDirection;// 1 if speed was negative 
  int moveAccel;    
  char name;
} Axis;

void axisInit(Axis *a,
	      const char *name,
	      const unsigned int stepPin, 
	      const unsigned int dirPin, 
	      const unsigned int enablePin, 
	      const unsigned int currentPin, 
	      const unsigned int usm0Pin, 
	      const unsigned int usm1Pin);

#define AXIS_INIT(a, name) axisInit(a, #name, IO_ ## name ## _STEP, IO_ ## name ## _DIR, IO_ ## name ## _ENABLE, IO_ ## name ## _CURRENT, IO_ ## name ## _USM0, IO_ ## name ## _USM1)

void inline axisNewMove(Axis *a) {
  a->moveError = a->moveSpeed = a->moveDirection = a->moveAccel = 0;
}

void inline axisSetSpeed(Axis *a, int speed) {
  a->moveSpeed = speed;
}

void inline axisSetAccel(Axis *a, int accel) {
  a->moveAccel = accel;
}

void inline axisPrepareMove(Axis *a) {  // Notice: update Move.java: getAxisLength if this is changed
  if (a->moveSpeed < 0 || (a->moveSpeed == 0 && a->moveAccel < 0)) {
    a->moveSpeed = -a->moveSpeed;
    a->moveAccel = -a->moveAccel;
    a->moveDirection = 1;
    GPIO_SET(a->stepper.dirPin);
    
  } else {
    GPIO_CLEAR(a->stepper.dirPin);
  }
}

void inline axisStartMove(Axis *a, int speed, int accel) { // Notice: update Move.java: getAxisLength if this is changed
  a->moveError = a->moveDirection = 0;
  a->moveSpeed = speed;
  a->moveAccel = accel;
  axisPrepareMove(a);
}

void inline axisTick(Axis *a) { // Notice: update Move.java: getAxisLength if this is changed
  a->moveError += a->moveSpeed;
  if (a->moveAccel) {
    a->moveSpeed += a->moveAccel;
  } 

  if (a->moveError >= ONE_STEP) {
    GPIO_SET(a->stepper.stepPin);
    a->moveError -= ONE_STEP;

    if (a->moveDirection) {
      a->position--;
    } else {
      a->position++;
    }
  }
}

void inline axisTock(Axis *a) {
  GPIO_CLEAR(a->stepper.stepPin);
}

void axisMotorEnable(Axis *a, unsigned int current, unsigned int usm);

#endif
