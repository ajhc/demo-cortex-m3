#pragma once

// Initialize the PWM outputs
void initPWM();

// Call once per 10 ms cycle to update the pwm outputs.
void updatePWM();

// Sets the target speed of the circulation pump in percent (0=stopped, 1023=full speed)
void setCirculationSpeed(int speed);

// Sets the target speed of the cooling pump in percent (0=stopped, 1023=full speed)
void setCoolingSpeed(int speed);

// Gets the actual speed of the circulation pump
int getCurrentCirculationSpeed();

// Gets the actual speed of the cooling pump 
int getCurrentCoolingSpeed();

#define PUMP_SPEED_MAX 1023

// The number of fractional bits 
#define PID_Q 20



