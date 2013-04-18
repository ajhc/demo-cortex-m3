#define __FROM_BOARD_C

#include "board.h"
#include "adc.h"
#include "uarts.h"
#include "pwm.h"
#include "api.h"

#include "lpc17xx_pinsel.h"
#include "lpc17xx_timer.h"

void WEAK diskTick100Hz() {
  // Do nothing.
}

void WEAK joulesUpdateTotals100Hz() {
  // Do nothing here either
}
     
void TIMER3_IRQHandler(void) {
  TIM_ClearIntPending(LPC_TIM3, TIM_MR0_INT);
  diskTick100Hz();
  joulesUpdateTotals100Hz();
}

//volatile unsigned long systick;
volatile SYSTICK_TYPE systick;
void SysTick_Handler (void) {
  systick++;
}

void delay(SYSTICK_TYPE ms) {
  SYSTICK_TYPE endtick = ms+systick;

  if (endtick < systick) { // Wraparound
    while (systick); // Wait for 0 to roll around.
  }

  while (systick < endtick); 
}

SYSTICK_TYPE systickInterval(SYSTICK_TYPE t0, SYSTICK_TYPE t1) {
  return t1-t0;
}


// Does the basic GPIO/function configuration of a pin using the pin config constant.
void configPin(const uint32_t pin) {

  PINSEL_CFG_Type PinCfg;

  PinCfg.Pinnum = IO_PIN(pin);
  PinCfg.Portnum = IO_PORT(pin);
  PinCfg.Funcnum = IO_FUNC(pin);

  PinCfg.OpenDrain = 0;
  PinCfg.Pinmode = 0;

  PINSEL_ConfigPin(&PinCfg);

  GPIO_SetDir(IO_PORT(pin), 1<<IO_PIN(pin), IO_OUTPUT(pin) ? 1 : 0);
}

void boardInit() {
  SysTick_Config(SystemCoreClock/1000 - 1); 

  /*
    A note about interrupt priorities

    A smaller numeric priority means that the priority is higher, thus
    the most important interrupt has priority 0, the least import 31.

    If an interrupt arrives while servicing an interrupt with a lower
    priority, then the lesser interrupt is interrupted to service the
    high priority one, if the high priority interrupt is of a different
    preemption priority group.

    IOW: an IRQ in PP group 3 will be interrupted if an IRQ arrives with
    PP group 2,1 or 0.

    The sub priority is only used to order the interrupts at the same PE
    level.

    NVIC_SetPriorityGrouping is used here to divide the 32 levels of
    IRQ priorities into 8 preemption groups and 4 sub priorities.
  */
  NVIC_SetPriorityGrouping(4);   
  for (int i=0;i<35;i++) {
    NVIC_SetPriority(i, GROUP_PRIORITY_DEFAULT);    
  }
  NVIC_SetPriority(TIMER2_IRQn, GROUP_PRIORITY_STEPPER);
  NVIC_SetPriority(SysTick_IRQn, GROUP_PRIORITY_1000HZ); 
  NVIC_SetPriority(TIMER3_IRQn, GROUP_PRIORITY_100HZ);
  NVIC_SetPriority(USB_IRQn, GROUP_PRIORITY_USB);
  
  initUARTs();
  initADC();
  initPWM();
  
  // Motor drivers are active low, so let's disable all of them, until the drivers turn them on:
  GPIO_SET(IO_X_ENABLE);
  GPIO_SET(IO_Y_ENABLE);
  GPIO_SET(IO_Z_ENABLE);
  GPIO_SET(IO_A_ENABLE); 
   
  /*
    Set the simple I/O configuration for all the pins we use,    
    this will ensure that all pins have had its function selected
  */
  for (int i=0;i<ALL_PINS_SIZE;i++) {
    configPin(ALL_PINS[i]);
  }

  /*
    Set up timer3 to poke the "slow" 100Hz maintainance routine
   */    

  TIM_TIMERCFG_Type timerCfg;
  timerCfg.PrescaleOption = TIM_PRESCALE_USVAL;
  timerCfg.PrescaleValue  = 1000; // 1 ms interval
  TIM_Init(LPC_TIM3, TIM_TIMER_MODE, &timerCfg);

  TIM_MATCHCFG_Type timerMatch;
  timerMatch.MatchChannel = 0;
  timerMatch.IntOnMatch   = TRUE;
  timerMatch.ResetOnMatch = TRUE;
  timerMatch.StopOnMatch  = FALSE;
  timerMatch.ExtMatchOutputType = TIM_EXTMATCH_NOTHING;
  timerMatch.MatchValue   = 10-1;
  TIM_ConfigMatch(LPC_TIM3,&timerMatch);

  NVIC_EnableIRQ(TIMER3_IRQn);
  TIM_Cmd(LPC_TIM3,ENABLE);
  
  initAPI();
}
