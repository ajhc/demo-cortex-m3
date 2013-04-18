#ifndef __ADC_H
#define __ADC_H

void initADC();

// Gets the latest converted value from a channel
// See the READ_ADC macro in board.h if you have an IO_ constant
unsigned int readADC(const int channel);

// Gets the voltage at an ADC pin in mV
unsigned int readADCmv(const int channel);


// Gets the raw supply voltage, before regulation, in mv, nominally 24000 mV
unsigned int supplyVoltage();

// Gets the amount of airflow detected by the airflow sensor, very non-linear
unsigned int airflow();

// Gets the temperature of a temperature sensor
double readNTCcelcius(const int channel);
double readNTCres(const int channel);

#endif
