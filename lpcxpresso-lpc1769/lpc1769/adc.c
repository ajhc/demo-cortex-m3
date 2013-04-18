#include "board.h"
#include "lpc17xx_adc.h"
#include "lpc17xx_pinsel.h"
#include <math.h>


void configureADC(const uint32_t pin) {
  const int channel = IO_CHAN(pin);
  ADC_IntConfig( LPC_ADC, channel, DISABLE);
  ADC_ChannelCmd(LPC_ADC, channel, ENABLE);
  configPin(pin);
}

void initADC() {
  // Configure the ADCs
  ADC_Init(LPC_ADC, 1000);
  configureADC(IO_AIRFLOW); 
  
  configureADC(IO_TEMP_OUT);
  configureADC(IO_TEMP_IN);
  configureADC(IO_TEMP_INTERNAL);
  configureADC(IO_VOLTAGE);
  ADC_StartCmd(LPC_ADC, ADC_START_CONTINUOUS);
  ADC_BurstCmd(LPC_ADC, 1);
}

unsigned int readADC(const int channel) {
  /*  
  ADC_StartCmd(LPC_ADC, ADC_START_NOW);
  while (!(ADC_ChannelGetStatus(LPC_ADC, channel,ADC_DATA_DONE)));
  */
  return ADC_ChannelGetData(LPC_ADC, channel);
}

unsigned int readADCmv(const int channel) {
  unsigned int raw = readADC(channel);

  return (VDD_MV*raw) / (1<<12);
}

unsigned int supplyVoltage() {
  unsigned int mv = readADCmv(IO_CHAN(IO_VOLTAGE));

  return (mv * 92165) / 10000;
}

#define AF_ERROR 2000
#define AF_MAX 1300
#define AF_MIN 780

unsigned int airflow() {
  unsigned int raw = readADC(IO_CHAN(IO_AIRFLOW));

  if (raw < AF_MIN || raw > AF_ERROR) {
    return 0;

  } else if (raw > AF_MAX) {
    return 100;

  } else {
    return 100*(raw - AF_MIN) / (AF_MAX-AF_MIN);
  }
}


// See board-config.h for the constants
 
double readNTCres(const int channel) {
  double v = readADCmv(channel);
  v /= 1000;
  return -(NTC_PULLUP * v) / (v-NTC_PULLUP_VOLTAGE);
}

double r2c(double r) {
  return 1/(1/(25+273.15) + 1/NTC_B50 * log(r/NTC_R25))-273.15;
}

double readNTCcelcius(const int channel) {
  return r2c(readNTCres(channel));
}

