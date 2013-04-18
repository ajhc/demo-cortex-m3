#include "board.h"
#include "adc.h"
#include "pwm.h"
#include "stepper.h"

#include <string.h>
#include <stdio.h>

volatile unsigned long SysTickCnt;
void SysTick_Handler (void) {
  SysTickCnt++;

  if (SysTickCnt & 1 << 10) {
    GPIO_SET(IO_X_STEP);
    GPIO_CLEAR(IO_X_STEP);
  }

  if (SysTickCnt & 1 << 11) {
    GPIO_SET(IO_Y_STEP);
    GPIO_CLEAR(IO_Y_STEP);
  }

  if (SysTickCnt & 1 << 12) {
    GPIO_SET(IO_Z_STEP);
    GPIO_CLEAR(IO_Z_STEP);
  }
}

void Delay (unsigned long tick) {
  unsigned long systickcnt = SysTickCnt;
  while ((SysTickCnt - systickcnt) < tick);
}

int main(void) {
  iprintf("Power Up!\n\r");
  
  Stepper s = stpInit(IO_X_STEP, IO_X_DIR, IO_X_ENABLE, IO_X_CURRENT, IO_X_USM0, IO_X_USM1);
  stpCurrent(&s, 350);
  stpMicrostep(&s, 3);
  stpEnable(&s);
  
  Stepper sy = stpInit(IO_Y_STEP, IO_Y_DIR, IO_Y_ENABLE, IO_Y_CURRENT, IO_Y_USM0, IO_Y_USM1);
  stpCurrent(&sy, 300);
  stpMicrostep(&sy, 3);
  stpEnable(&sy);
  
  Stepper sz = stpInit(IO_Z_STEP, IO_Z_DIR, IO_Z_ENABLE, IO_Z_CURRENT, IO_Z_USM0, IO_Z_USM1);
  stpCurrent(&sz, 300);
  stpMicrostep(&sz, 3);
  stpEnable(&sz);
  
  SysTick_Config(SystemCoreClock/800 - 1);

  while (1) {
    GPIO_SET(IO_LED);
    Delay(500);
    
    GPIO_CLEAR(IO_LED);
    Delay(500);
  }
}
