// Actual voltage measured on the board, this should probably sit somewhere in flash, programmed at runtime rather than in the code.
#define VDD_MV 3293

// The shunt resistor used on the stepper drivers.
#define STEPPER_SHUNT 0.22  

// Number of grams of water that flows though the waterflow sensor for each pulse, should probably be a runtime option as well
#define WATERFLOW_GRAMS_PER_PULSE 6.480605487228

// The temperature measurement NTCs:
#define NTC_PULLUP 10000
#define NTC_PULLUP_VOLTAGE 5

// Constants from the datasheet for the NTC used:
// ELFA:60-279-24 (1% 5k RH16 6D502)
#define NTC_B50 3936.0
#define NTC_R25 5000.0

// Constant taken from: https://en.wikipedia.org/wiki/Properties_of_water#Heat_capacity_and_heats_of_vaporization_and_fusion
// We assume the water temperature is around 20 degC
#define WATER_HEAT_CAPACITY 4.18

// The minimum water flow we tolerate through the tube before raising an alarm
#define MINIMUM_WATER_FLOW_GS 50
#define MAXIMUM_WATER_TEMP 30
#define MINIMUM_WATER_TEMP 15

