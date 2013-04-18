#ifndef __ALARM_H__
#define __ALARM_H__

#include "board.h"

void alarmInit();

/*
  Checks the limit switches, emergency switch, watchdog ready output
  then sets an alarm if anything is wrong.  

  If any alarms are active this will always return 1 without checking anything else.
*/
unsigned int checkAlarmInputs();

// Alarm source bits, used in the switches field of each alarm.

// Limit switches
#define ALARM_SW_X_MIN 0
#define ALARM_SW_Y_MIN 1
#define ALARM_SW_Z_MIN 2
#define ALARM_SW_A_MIN 3

#define ALARM_SW_X_MAX 4
#define ALARM_SW_Y_MAX 5
#define ALARM_SW_Z_MAX 6
#define ALARM_SW_A_MAX 7

// Emergency stop was triggered
#define ALARM_SW_ESTOP 8

// Watchdog is unhappy
#define ALARM_WD    9

// The code was invalid
#define ALARM_CODE    10

// Coolant flow was too low and the laser was commanded on
#define ALARM_COOLANT_FLOW 11

// Coolant temperature too high and the laser was commanded on
#define ALARM_COOLANT_TEMP_HIGH 12

// The buffer is about to be reset
#define ALARM_RESET 13

// LASER is not ready
#define ALARM_LASER 14

// Motor drivers too hot.
#define ALARM_MOTOR_DRIVER_OVERTEMP 15

// Cooling sensors are giving invalid readings.
#define ALARM_COOLING_SENSORS 16

// Coolant temperature too low and the laser was commanded on
#define ALARM_COOLANT_TEMP_LOW 17


#define ALARM_MAX_LENGTH 80
typedef struct {
  SYSTICK_TYPE timestamp; // The milisecond systick value when the fault occured
  int active; // Never set or clear this manually always use alarmSet and alarmClear!
  unsigned int moveId;         // The Move ID active/being interpreted when the alarm occured
  unsigned int moveCodeOffset; // The number of the move code being interpreted
  unsigned int switches;
  char msg[ALARM_MAX_LENGTH];
} Alarm;

#define ALARMS 10
extern Alarm alarms[ALARMS] IN_IRAM1;
extern int alarmsActive;
extern unsigned int alarmsIgnored;


// Sets an alarm and returns the index
int alarmSet(unsigned int switches, char *message);

// Clears an alarm
int alarmClear(int index);

// Returns the number of non-cleared alarms
int inline alarmCount() {
  return alarmsActive;
}


#endif
