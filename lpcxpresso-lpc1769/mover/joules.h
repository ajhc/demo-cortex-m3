#ifndef __JOULES_H__
#define __JOULES_H__

// This gets called at the highest possible frequency to detect the pulses
void joulesPollFlow();

// This gets called at a much slower speed to update all the totals
void joulesUpdateTotals100Hz();

// The total number of water flow pulses seen
unsigned int joulesRawFlowCount();

// The total mass of water, that has passed through the tube in kg 
double joulesTotalWaterMass();

// The total energy dissipated by the tube since poweron in joule
double joulesTotal();

// Average power currently being dissipated
double joulesCurrentPower();

// Average coolant flow in gram / second
double joulesWaterFlow();

// returns true if coolant is not within spec
unsigned int getCoolantAlarm();

double joulesLastInTemp();
double joulesLastOutTemp();
double joulesLastInternalTemp();
       
#endif
