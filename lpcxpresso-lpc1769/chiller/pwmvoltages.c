#include <avr/io.h>

#include "pwmvoltages.h"
#include "adchelper.h"

// These are the PWM output limits.
const long MAX_OUTPUT = ((long)128)<<PID_Q;
const long MIN_OUTPUT = 0;

#define ADC_MAX 780

// 100% in ADC output
#define P 50000

struct PIDdata {
  long current;
  long output;
  long target;
};


struct PIDdata pidA;
struct PIDdata pidB;

void setPIDTarget(struct PIDdata *pid, int target) {
  pid->target = target;
  pid->target *= ADC_MAX;
  pid->target >>= 10;
  if (pid->target > 1023) {
    pid->target = 1023;
  } else if (pid->target < 0) {
    pid->target = 0;
  }
}

void updatePID(struct PIDdata *pid, int measurement) {

  pid->current = measurement;
  long error = pid->current - pid->target;
  pid->output -= error * P;

  if (pid->output > MAX_OUTPUT) {
    pid->output = MAX_OUTPUT;
      
  } else if (pid->output < MIN_OUTPUT) {
    pid->output = MIN_OUTPUT;
  }
}

void setAPWM(unsigned char value) {
  if (value) {    
    OCR1AL = value;
    TCCR1A |= _BV(COM1A1);
  } else {
    TCCR1A &=~ _BV(COM1A1);
  }
}

void setBPWM(unsigned char value) {
  if (value) {    
    OCR1BL = value;
    TCCR1A |= _BV(COM1B1);
  } else {
    TCCR1A &=~ _BV(COM1B1);
  }
}

void setCirculationSpeed(int speed) {
  setPIDTarget(&pidA, speed);
}

void setCoolingSpeed(int speed) {
  setPIDTarget(&pidB, speed);
}

int getCurrentCirculationSpeed() {
  return 1023*pidA.current / ADC_MAX;
}

int getCurrentCoolingSpeed() {
  return 1023*pidB.current / ADC_MAX;
}

void updatePWM() {
  updatePID(&pidA, getOsADC(2));
  updatePID(&pidB, getOsADC(3));
  setAPWM(pidA.output >> PID_Q);
  setBPWM(pidB.output >> PID_Q);
}

void initPWM() {
  DDRB  |= _BV(PB1);  // Circulation pump PWM
  DDRB  |= _BV(PB2);  // Cooling pump PWM

  // Set up timer 1 for fast PWM mode & the highest frequency available
  TCCR1A = _BV(WGM10);
  TCCR1B = _BV(WGM12) | _BV(CS10);
}
