#include "stepper.h"
#include "pwm.h"
#include "board.h"

void stpInit(Stepper *s,
	     const unsigned int stepPin, 
	     const unsigned int dirPin, 
	     const unsigned int enablePin, 
	     const unsigned int currentPin, 
	     const unsigned int usm0Pin, 
	     const unsigned int usm1Pin) {

  s->stepPin = stepPin;
  s->dirPin = dirPin;
  s->enablePin = enablePin;
  s->currentPin = currentPin;
  s->usm0Pin = usm0Pin;
  s->usm1Pin = usm1Pin;

  configPin(s->stepPin);
  configPin(s->dirPin);
  configPin(s->enablePin);
  configPin(s->currentPin);
  configPin(s->usm0Pin);
  configPin(s->usm1Pin);
}

void stpEnable(Stepper *s) {
  GPIO_CLEAR(s->enablePin);
}

void stpDisable(Stepper *s) {
  GPIO_SET(s->enablePin);
}

/*
  I = V / (8*R)

  I: Chopper current
  V: Voltage at reference pin
  R: Shunt resistor size

  R = 0.22 Ohm
  V = 3.3*pwm/255

  pwm = (2047*R*I) / vdd
*/
void stpCurrent(Stepper *s, unsigned int ma) {
  /*
   Trick: We're using mA on top and mV on the bottom, so the calculation
   still works, even though it was made for A and V.
  */
  
  unsigned int pwm = (2047*STEPPER_SHUNT*ma) / VDD_MV;
  if (pwm > 255) {
    pwm = 255;
  }

  setPWM(IO_CHAN(s->currentPin), pwm);
}

void stpMicrostep(Stepper *s, unsigned int n) {
  if (n == 0) { // full step
    GPIO_CLEAR(s->usm0Pin);
    GPIO_CLEAR(s->usm1Pin);

  } else if (n == 1) { // half step
    GPIO_SET(s->usm0Pin);
    GPIO_CLEAR(s->usm1Pin);

  } else if (n == 2) { // quarter step
    GPIO_CLEAR(s->usm0Pin);
    GPIO_SET(s->usm1Pin);

  } else { // 1/8 step
    GPIO_SET(s->usm0Pin);
    GPIO_SET(s->usm1Pin);
  } 
}
