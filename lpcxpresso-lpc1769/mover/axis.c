#include "axis.h"

void axisInit(Axis *a, 
	      const char *name,
	      const unsigned int stepPin, 
	      const unsigned int dirPin, 
	      const unsigned int enablePin, 
	      const unsigned int currentPin, 
	      const unsigned int usm0Pin, 
	      const unsigned int usm1Pin) {

  stpInit(&a->stepper, stepPin, dirPin, enablePin, currentPin, usm0Pin, usm1Pin);
  a->position = a->moveError = a->moveSpeed = a->moveDirection = a->moveAccel = 0;
  a->name = *name;
}

void axisMotorEnable(Axis *a, unsigned int current, unsigned int usm) {
  if (current) {
    stpCurrent(&a->stepper, current);
    stpMicrostep(&a->stepper, usm);
    stpEnable(&a->stepper);
  } else {
    stpDisable(&a->stepper);
  }
}

