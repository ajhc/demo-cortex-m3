/*
 * This file is part of MicroCLI.
 *
 * Copyright (C) 2012 Christian Jann <christian.jann@ymail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Includes ------------------------------------------------------------------*/
#include "main.h"
#include "conf.h"
#include <stdio.h>
#include "usart.h"
#include <stdlib.h>
#include "conio.h"
#include "cli.h"
#include "wifi.h"
#include <string.h>

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
#define TIMEOUT_0_DEFAULT 50
#define LINEBUFFERSIZE 100

/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
GPIO_InitTypeDef GPIO_InitStructure;
__IO uint32_t TimingDelay;
__IO uint8_t timeout_0 = TIMEOUT_0_DEFAULT;
__IO uint16_t timeout_one_second = 1000;
__IO uint32_t uptime = 0;
__IO uint8_t UserButtonPressed = 0x00;

char line_buffer[LINEBUFFERSIZE];

/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/


int main(void)
{

    /* Initialize Leds mounted on STM32F0-discovery */
    STM_EVAL_LEDInit(LED3);
    STM_EVAL_LEDInit(LED4);

    /* Turn on LED3 and LED4 */
    STM_EVAL_LEDOn(LED3);
    STM_EVAL_LEDOn(LED4);
    
    /* Initialize other outputs and inputs */
    STM_EVAL_LEDInit(RELAIS1);
    STM_EVAL_LEDInit(RELAIS2);
    STM_EVAL_LEDInit(OUT_12V_1);
    STM_EVAL_LEDInit(OUT_12V_2);
    STM_EVAL_PBInit(IN_12V_1, BUTTON_MODE_PDOWN);
    STM_EVAL_PBInit(IN_12V_2, BUTTON_MODE_PDOWN);
    
    /* Initialize the User_Button */
    STM_EVAL_PBInit(BUTTON_USER, BUTTON_MODE_EXTI);

    /* Setup SysTick Timer for 1 msec interrupts. */
    if (SysTick_Config(SystemCoreClock / 1000))
    {
        /* Capture error */
        while (1);
    }

    usart_1_init();

    while (1)
    {
	 STM_EVAL_LEDOff(LED3);
         delay_ms(200);
	 STM_EVAL_LEDOff(LED4);
         delay_ms(200);
	 STM_EVAL_LEDOn(LED3);
         delay_ms(200);
	 STM_EVAL_LEDOn(LED4);
         delay_ms(200);
    }
}

/**
  * @brief  Inserts a delay time.
  * @param  nTime: specifies the delay time length, in milliseconds.
  * @retval None
  */
void delay_ms(__IO uint32_t nTime)
{
    TimingDelay = nTime;

    while (TimingDelay != 0);
}

/**
  * @brief  SysTick Timer callback with 1 msec interrupts
  * @param  None
  * @retval None
  */
void SystemTick(void)
{
    if (TimingDelay != 0)
    {
        TimingDelay--;
    }
    if (timeout_0 != 0)
    {
        timeout_0--;
    }
    if (--timeout_one_second == 0)
    {
        timeout_one_second = 1000;
        uptime++;
    }
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
