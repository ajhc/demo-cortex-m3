#include "stm32f30x.h"
#include "stm32f3_discovery.h"
#include "jhc_rts_header.h"
#include "c_extern.h"

/* Private variables ---------------------------------------------------------*/
  RCC_ClocksTypeDef RCC_Clocks;
__IO uint32_t TimingDelay = 0;

uint32_t *getTimingDelay()
{
  return &TimingDelay;
}

/* Private function prototypes -----------------------------------------------*/
extern void timingDelayDecrement(void);

/* Private functions ---------------------------------------------------------*/

/**
  * @brief  This function handles SysTick Handler.
  * @param  None
  * @retval None
  */
void SysTick_Handler(void)
{
  timingDelayDecrement(); // Call Haskell code.
}

#if 1 /* Catch intr. */
/* void Reset_Handler(void) {for(;;);} ** Start program ***/
void NMI_Handler(void) {for(;;);}
void HardFault_Handler(void) {
    __asm volatile
    (
        " tst lr, #4                                                \n"
        " ite eq                                                    \n"
        " mrseq r0, msp                                             \n"
        " mrsne r0, psp                                             \n"
        " ldr r1, [r0, #24]                                         \n"
        " ldr r2, handler2_address_const                            \n"
        " bx r2                                                     \n"
        " handler2_address_const: .word prvGetRegistersFromStack    \n"
    );
}
void MemManage_Handler(void) {for(;;);}
void BusFault_Handler(void) {for(;;);}
void UsageFault_Handler(void) {for(;;);}
void SVC_Handler(void) {for(;;);}
void DebugMon_Handler(void) {for(;;);}
void PendSV_Handler(void) {for(;;);}
/* void SysTick_Handler(void) {for(;;);} */
void WWDG_IRQHandler(void) {for(;;);}
void PVD_IRQHandler(void) {for(;;);}
void TAMPER_STAMP_IRQHandler(void) {for(;;);}
void RTC_WKUP_IRQHandler(void) {for(;;);}
void FLASH_IRQHandler(void) {for(;;);}
void RCC_IRQHandler(void) {for(;;);}
void EXTI0_IRQHandler(void) {for(;;);}
void EXTI1_IRQHandler(void) {for(;;);}
void EXTI2_TS_IRQHandler(void) {for(;;);}
void EXTI3_IRQHandler(void) {for(;;);}
void EXTI4_IRQHandler(void) {for(;;);}
void DMA1_Channel1_IRQHandler(void) {for(;;);}
void DMA1_Channel2_IRQHandler(void) {for(;;);}
void DMA1_Channel3_IRQHandler(void) {for(;;);}
void DMA1_Channel4_IRQHandler(void) {for(;;);}
void DMA1_Channel5_IRQHandler(void) {for(;;);}
void DMA1_Channel6_IRQHandler(void) {for(;;);}
void DMA1_Channel7_IRQHandler(void) {for(;;);}
void ADC1_2_IRQHandler(void) {for(;;);}
void USB_HP_CAN1_TX_IRQHandler(void) {for(;;);}
void USB_LP_CAN1_RX0_IRQHandler(void) {for(;;);}
void CAN1_RX1_IRQHandler(void) {for(;;);}
void CAN1_SCE_IRQHandler(void) {for(;;);}
void EXTI9_5_IRQHandler(void) {for(;;);}
void TIM1_BRK_TIM15_IRQHandler(void) {for(;;);}
void TIM1_UP_TIM16_IRQHandler(void) {for(;;);}
void TIM1_TRG_COM_TIM17_IRQHandler(void) {for(;;);}
void TIM1_CC_IRQHandler(void) {for(;;);}
void TIM2_IRQHandler(void) {for(;;);}
void TIM3_IRQHandler(void) {for(;;);}
void TIM4_IRQHandler(void) {for(;;);}
void I2C1_EV_IRQHandler(void) {for(;;);}
void I2C1_ER_IRQHandler(void) {for(;;);}
void I2C2_EV_IRQHandler(void) {for(;;);}
void I2C2_ER_IRQHandler(void) {for(;;);}
void SPI1_IRQHandler(void) {for(;;);}
void SPI2_IRQHandler(void) {for(;;);}
void USART1_IRQHandler(void) {for(;;);}
void USART2_IRQHandler(void) {for(;;);}
void USART3_IRQHandler(void) {for(;;);}
void EXTI15_10_IRQHandler(void) {for(;;);}
void RTC_Alarm_IRQHandler(void) {for(;;);}
void USBWakeUp_IRQHandler(void) {for(;;);}
void TIM8_BRK_IRQHandler(void) {for(;;);}
void TIM8_UP_IRQHandler(void) {for(;;);}
void TIM8_TRG_COM_IRQHandler(void) {for(;;);}
void TIM8_CC_IRQHandler(void) {for(;;);}
void ADC3_IRQHandler(void) {for(;;);}
void SPI3_IRQHandler(void) {for(;;);}
void UART4_IRQHandler(void) {for(;;);}
void UART5_IRQHandler(void) {for(;;);}
void TIM6_DAC_IRQHandler(void) {for(;;);}
void TIM7_IRQHandler(void) {for(;;);}
void DMA2_Channel1_IRQHandler(void) {for(;;);}
void DMA2_Channel2_IRQHandler(void) {for(;;);}
void DMA2_Channel3_IRQHandler(void) {for(;;);}
void DMA2_Channel4_IRQHandler(void) {for(;;);}
void DMA2_Channel5_IRQHandler(void) {for(;;);}
void ADC4_IRQHandler(void) {for(;;);}
void COMP1_2_3_IRQHandler(void) {for(;;);}
void COMP4_5_6_IRQHandler(void) {for(;;);}
void COMP7_IRQHandler(void) {for(;;);}
void USB_HP_IRQHandler(void) {for(;;);}
void USB_LP_IRQHandler(void) {for(;;);}
void USBWakeUp_RMP_IRQHandler(void) {for(;;);}
void FPU_IRQHandler(void) {for(;;);}

/* These are volatile to try and prevent the compiler/linker optimising them
away as the variables never actually get used.  If the debugger won't show the
values of the variables, make them global my moving their declaration outside
of this function. */
volatile uint32_t prvGetr0;
volatile uint32_t prvGetr1;
volatile uint32_t prvGetr2;
volatile uint32_t prvGetr3;
volatile uint32_t prvGetr12;
volatile uint32_t prvGetlr; /* Link register. */
volatile uint32_t prvGetpc; /* Program counter. */
volatile uint32_t prvGetpsr;/* Program status register. */

void prvGetRegistersFromStack( uint32_t *pulFaultStackAddress )
{
    prvGetr0  = pulFaultStackAddress[ 0 ];
    prvGetr1  = pulFaultStackAddress[ 1 ];
    prvGetr2  = pulFaultStackAddress[ 2 ];
    prvGetr3  = pulFaultStackAddress[ 3 ];
    prvGetr12 = pulFaultStackAddress[ 4 ];
    prvGetlr  = pulFaultStackAddress[ 5 ];
    prvGetpc  = pulFaultStackAddress[ 6 ];
    prvGetpsr = pulFaultStackAddress[ 7 ];

    /* When the following line is hit, the variables contain the register values. */
    for( ;; );
}
#endif

/**
  * @brief  Main program.
  * @param  None 
  * @retval None
  */
int main(void)
{  
  /* SysTick end of count event each 10ms */
  RCC_GetClocksFreq(&RCC_Clocks);
  SysTick_Config(RCC_Clocks.HCLK_Frequency / 100);
  
  /* Initialize LEDs and User Button available on STM32F3-Discovery board */
  STM_EVAL_LEDInit(LED3);
  STM_EVAL_LEDInit(LED4);
  STM_EVAL_LEDInit(LED5);
  STM_EVAL_LEDInit(LED6);
  STM_EVAL_LEDInit(LED7);
  STM_EVAL_LEDInit(LED8);
  STM_EVAL_LEDInit(LED9);
  STM_EVAL_LEDInit(LED10);

  { /* Run Haskell code */
	  int hsargc = 1;
	  char *hsargv = "q";
	  char **hsargvp = &hsargv;

	  hs_init(&hsargc, &hsargvp);
	  _amain();
	  /* hs_exit(); */
  }

  for (;;);
  /* NOTREACHED */
}

#ifdef  USE_FULL_ASSERT

/**
  * @brief  Reports the name of the source file and the source line number
  *         where the assert_param error has occurred.
  * @param  file: pointer to the source file name
  * @param  line: assert_param error line source number
  * @retval None
  */
void assert_failed(uint8_t* file, uint32_t line)
{ 
  /* User can add his own implementation to report the file name and line number,
     ex: printf("Wrong parameters value: file %s on line %d\r\n", file, line) */

  /* Infinite loop */
  while (1)
  {
  }
}
#endif
