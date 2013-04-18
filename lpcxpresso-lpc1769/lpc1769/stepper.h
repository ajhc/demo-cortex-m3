#ifndef __STEPPER_H
#define __STEPPER_H

typedef struct {
  unsigned int stepPin;
  unsigned int dirPin;
  unsigned int enablePin;
  unsigned int currentPin;
  unsigned int usm0Pin;
  unsigned int usm1Pin;
} Stepper;

void stpInit(Stepper *s,
	     const unsigned int stepPin, 
	     const unsigned int dirPin, 
	     const unsigned int enablePin, 
	     const unsigned int currentPin, 
	     const unsigned int usm0Pin, 
	     const unsigned int usm1Pin);

void stpEnable(Stepper *s);
void stpDisable(Stepper *s);
void stpCurrent(Stepper *s, unsigned int ma);
void stpMicrostep(Stepper *s, unsigned int n);

// I = V / (8*R)
#define STEPPER_MAX_CURRENT (int)(VDD_MV/ (8*STEPPER_SHUNT))

#endif
