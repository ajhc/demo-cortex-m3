#include "joules.h"
#include "api.h"
#include "alarm.h"

#define COOLING_SMOOTH 0.02

// This gets called at the highest possible frequency to detect the pulses
signed char waterflowPinstate;
char waterflowArmed;
unsigned int waterflowPulses;

void joulesPollFlow() {

  if (GPIO_GET(IO_WATERFLOW)) {
    waterflowPinstate++;
  } else {
    waterflowPinstate--;
  }

  if (waterflowPinstate > 10) {
    waterflowPinstate = 10;

    if (waterflowArmed) {
      waterflowPulses++;
      waterflowArmed = 0;
    }    

  } else if (waterflowPinstate < -10) {
    waterflowPinstate = -10;
    waterflowArmed = 1;
  }

  /*
  if (waterflowPinstate && !GPIO_GET(IO_WATERFLOW)) {
    waterflowPinstate = 0;
  } else if (!waterflowPinstate && GPIO_GET(IO_WATERFLOW)) {
    waterflowPinstate = 1;
    waterflowPulses++;
  }
  */
}

// The total number of water flow pulses seen
unsigned int joulesRawFlowCount() {
  return waterflowPulses;
}

SYSTICK_TYPE lastTime;
double lastInTemp;
double lastOutTemp;
double lastInternalTemp;
unsigned int lastPulseCount;
double totalWaterMass;
double totalJoules;
double avgPower;
double avgWaterFlow;
unsigned int coolantAlarm;


int div20;
void joulesUpdateTotals100Hz() {

  // Divide down the 100 Hz to 5 Hz, so we don't go too crazy with the floating point cycles
  if (++div20 > 19) { 
    div20 = 0;    
  } else {
    return;
  }

  coolantAlarm = 0;
  
  double thisInternalTemp = readNTCcelcius(IO_CHAN(IO_TEMP_INTERNAL));
  if (lastTime) {
    lastInternalTemp += (thisInternalTemp-lastInternalTemp) * COOLING_SMOOTH;
  } else {
    lastInternalTemp = thisInternalTemp;
  }
  if (lastInternalTemp > 100) {
    coolantAlarm |= 1<<ALARM_MOTOR_DRIVER_OVERTEMP;
  }

  double it = readNTCcelcius(IO_CHAN(IO_TEMP_IN)); 
  double ot = readNTCcelcius(IO_CHAN(IO_TEMP_OUT));
  SYSTICK_TYPE t1 = systick;
  SYSTICK_TYPE deltaTime = systickInterval(lastTime, t1);

  if (it < 0 || ot < 0 || it > 50 || ot > 50) { // Ignore crazy values

    // But raise the alarm if the sensor problems keep up for a long time
    if (deltaTime > 2000) {
      coolantAlarm |= 1<<ALARM_COOLING_SENSORS;
    }
    return;
  }
  unsigned int p = waterflowPulses;
  
  double newIt = lastTime ? lastInTemp  + (it-lastInTemp) * COOLING_SMOOTH : it;
  double newOt = lastTime ? lastOutTemp + (ot-lastOutTemp)* COOLING_SMOOTH : ot;

  if (lastTime && deltaTime) {
    double mass = (p-lastPulseCount)*WATERFLOW_GRAMS_PER_PULSE; // grams of water since last update
    totalWaterMass += mass/1000; // Because total watermass is in kg
    double deltaTemp = newOt-newIt; // ot and it are in C, so the diff in K is the same as the diff in C.
    double j = WATER_HEAT_CAPACITY * mass * deltaTemp; 
    totalJoules += j;

    double power = j / (deltaTime / 1000.0); // deltaTime is in ms
    avgPower += (power-avgPower) * COOLING_SMOOTH;

    double waterFlow = (1000*mass/deltaTime); // gram / second
    avgWaterFlow += (waterFlow-avgWaterFlow) * COOLING_SMOOTH;

    if (avgWaterFlow < MINIMUM_WATER_FLOW_GS) {
      coolantAlarm |= 1<<ALARM_COOLANT_FLOW;
    }

    if (newOt < MINIMUM_WATER_TEMP || newIt < MINIMUM_WATER_TEMP) {
      coolantAlarm |= 1<<ALARM_COOLANT_TEMP_LOW;
    }

    if (newOt > MAXIMUM_WATER_TEMP || newIt > MAXIMUM_WATER_TEMP) { 
      coolantAlarm |= 1<<ALARM_COOLANT_TEMP_HIGH;
    }    
  }  
  
  lastPulseCount = p;
  lastTime = t1;
  lastOutTemp = newOt;
  lastInTemp = newIt;
}

// The total mass of water, that has passed through the tube in kg 
double joulesTotalWaterMass() {
  return totalWaterMass;
}

// The total energy dissipated by the tube since poweron in joule
double joulesTotal() {
  return totalJoules;
}

// Average power currently being dissipated
double joulesCurrentPower() {
  return avgPower;
}

double joulesWaterFlow() {
  return avgWaterFlow;
}

unsigned int getCoolantAlarm() {
  return coolantAlarm & (~alarmsIgnored);
}

double joulesLastInTemp() {
  return lastInTemp;
}

double joulesLastOutTemp() {
  return lastOutTemp;
}

double joulesLastInternalTemp() {
  return lastInternalTemp;
}
