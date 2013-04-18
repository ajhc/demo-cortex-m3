/****************************************************************************//**
 * Notice: This is a modified version of the system_LPC17xx.c file shipped
 * by NXP as part of the lpc17xx driver library, the following is
 * the original copyright header:
 *
 * @file :    startup_LPC17xx.c
 * @brief : CMSIS Cortex-M3 Core Device Startup File
 * @version : V1.01
 * @date :    4. Feb. 2009
 *
 *----------------------------------------------------------------------------
 *
 * Copyright (C) 2009 ARM Limited. All rights reserved.
 *
 * ARM Limited (ARM) is supplying this software for use with Cortex-Mx
 * processor based microcontrollers.  This file can be freely distributed
 * within development tools that are supporting such ARM based processors.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS".  NO WARRANTIES, WHETHER EXPRESS, IMPLIED
 * OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE APPLY TO THIS SOFTWARE.
 * ARM SHALL NOT, IN ANY CIRCUMSTANCES, BE LIABLE FOR SPECIAL, INCIDENTAL, OR
 * CONSEQUENTIAL DAMAGES, FOR ANY REASON WHATSOEVER.
 *
 ******************************************************************************/

// Mod by nio for the .fastcode part

#include "cpu_def.h"
#include "LPC17xx.h"
#include "board.h"

#define WEAK __attribute__ ((weak))
//*****************************************************************************
//
// Forward declaration of the default fault handlers.
//
//*****************************************************************************
/* System exception vector handler */
void WEAK 		Reset_Handler(void);             /* Reset Handler */
void WEAK 		NMI_Handler(void);               /* NMI Handler */
void WEAK 		HardFault_Handler(void);         /* Hard Fault Handler */
void WEAK 		MemManage_Handler(void);         /* MPU Fault Handler */
void WEAK 		BusFault_Handler(void);          /* Bus Fault Handler */
void WEAK 		UsageFault_Handler(void);        /* Usage Fault Handler */
void WEAK 		SVC_Handler(void);               /* SVCall Handler */
void WEAK 		DebugMon_Handler(void);          /* Debug Monitor Handler */
void WEAK 		PendSV_Handler(void);            /* PendSV Handler */
void WEAK 		SysTick_Handler(void);           /* SysTick Handler */

/* External interrupt vector handler */
void WEAK      	WDT_IRQHandler(void);            /* Watchdog Timer */
void WEAK      	TIMER0_IRQHandler(void);         /* Timer0 */
void WEAK      	TIMER1_IRQHandler(void);         /* Timer1 */
void WEAK      	TIMER2_IRQHandler(void);         /* Timer2 */
void WEAK      	TIMER3_IRQHandler(void);         /* Timer3 */
void WEAK      	UART0_IRQHandler(void);          /* UART0 */
void WEAK      	UART1_IRQHandler(void);          /* UART1 */
void WEAK      	UART2_IRQHandler(void);          /* UART2 */
void WEAK      	UART3_IRQHandler(void);          /* UART3 */
void WEAK      	PWM1_IRQHandler(void);           /* PWM1 */
void WEAK      	I2C0_IRQHandler(void);           /* I2C0 */
void WEAK      	I2C1_IRQHandler(void);           /* I2C1 */
void WEAK      	I2C2_IRQHandler(void);           /* I2C2 */
void WEAK      	SPI_IRQHandler(void);            /* SPI */
void WEAK      	SSP0_IRQHandler(void);           /* SSP0 */
void WEAK      	SSP1_IRQHandler(void);           /* SSP1 */
void WEAK      	PLL0_IRQHandler(void);           /* PLL0 (Main PLL) */
void WEAK      	RTC_IRQHandler(void);            /* Real Time Clock */
void WEAK      	EINT0_IRQHandler(void);          /* External Interrupt 0 */
void WEAK      	EINT1_IRQHandler(void);          /* External Interrupt 1 */
void WEAK      	EINT2_IRQHandler(void);          /* External Interrupt 2 */
void WEAK      	EINT3_IRQHandler(void);          /* External Interrupt 3 */
void WEAK      	ADC_IRQHandler(void);            /* A/D Converter */
void WEAK      	BOD_IRQHandler(void);            /* Brown Out Detect */
void WEAK      	USB_IRQHandler(void);            /* USB */
void WEAK      	CAN_IRQHandler(void);            /* CAN */
void WEAK      	DMA_IRQHandler(void);            /* GP DMA */
void WEAK      	I2S_IRQHandler(void);            /* I2S */
void WEAK      	ENET_IRQHandler(void);           /* Ethernet */
void WEAK      	RIT_IRQHandler(void);            /* Repetitive Interrupt Timer */
void WEAK      	MCPWM_IRQHandler(void);          /* Motor Control PWM */
void WEAK      	QEI_IRQHandler(void);            /* Quadrature Encoder Interface */
void WEAK      	PLL1_IRQHandler(void);           /* PLL1 (USB PLL) */



/* Exported types --------------------------------------------------------------*/
/* Exported constants --------------------------------------------------------*/
extern unsigned long _etext;
extern unsigned long _sidata;		/* start address for the initialization values of the .data section. defined in linker script */
extern unsigned long _sdata;		/* start address for the .data section. defined in linker script */
extern unsigned long _edata;		/* end address for the .data section. defined in linker script */

extern unsigned long _sifastcode;		/* start address for the initialization values of the .fastcode section. defined in linker script */
extern unsigned long _sfastcode;		/* start address for the .fastcode section. defined in linker script */
extern unsigned long _efastcode;		/* end address for the .fastcode section. defined in linker script */

extern unsigned long _sbss;			/* start address for the .bss section. defined in linker script */
extern unsigned long _ebss;			/* end address for the .bss section. defined in linker script */

extern void _estack;		/* init value for the stack pointer. defined in linker script */


/* Private typedef -----------------------------------------------------------*/
/* function prototypes ------------------------------------------------------*/
void Reset_Handler(void) __attribute__((__interrupt__));
extern int main(void);

typedef void (*FNC)(void);
FNC fnc_entry;

/******************************************************************************
*
* The minimal vector table for a Cortex M3.  Note that the proper constructs
* must be placed on this to ensure that it ends up at physical address
* 0x0000.0000.
*
******************************************************************************/

extern unsigned long _stack;

__attribute__ ((section(".isr_vector")))
void (* const g_pfnVectors[])(void) =
{
        (void (*)(void))&_stack,   /* The initial stack pointer */
        Reset_Handler,             /* Reset Handler */
        NMI_Handler,               /* NMI Handler */
        HardFault_Handler,         /* Hard Fault Handler */
        MemManage_Handler,         /* MPU Fault Handler */
        BusFault_Handler,          /* Bus Fault Handler */
        UsageFault_Handler,        /* Usage Fault Handler */
        0,                         /* Reserved */
        0,                         /* Reserved */
        0,                         /* Reserved */
        0,                         /* Reserved */
        SVC_Handler,               /* SVCall Handler */
        DebugMon_Handler,          /* Debug Monitor Handler */
        0,                         /* Reserved */
        PendSV_Handler,            /* PendSV Handler */
        SysTick_Handler,           /* SysTick Handler */

		// External Interrupts
        WDT_IRQHandler,            /* Watchdog Timer */
        TIMER0_IRQHandler,         /* Timer0 */
        TIMER1_IRQHandler,         /* Timer1 */
        TIMER2_IRQHandler,         /* Timer2 */
        TIMER3_IRQHandler,         /* Timer3 */
        UART0_IRQHandler,          /* UART0 */
        UART1_IRQHandler,          /* UART1 */
        UART2_IRQHandler,          /* UART2 */
        UART3_IRQHandler,          /* UART3 */
        PWM1_IRQHandler,           /* PWM1 */
        I2C0_IRQHandler,           /* I2C0 */
        I2C1_IRQHandler,           /* I2C1 */
        I2C2_IRQHandler,           /* I2C2 */
        SPI_IRQHandler,            /* SPI */
        SSP0_IRQHandler,           /* SSP0 */
        SSP1_IRQHandler,           /* SSP1 */
        PLL0_IRQHandler,           /* PLL0 (Main PLL) */
        RTC_IRQHandler,            /* Real Time Clock */
        EINT0_IRQHandler,          /* External Interrupt 0 */
        EINT1_IRQHandler,          /* External Interrupt 1 */
        EINT2_IRQHandler,          /* External Interrupt 2 */
        EINT3_IRQHandler,          /* External Interrupt 3 */
        ADC_IRQHandler,            /* A/D Converter */
        BOD_IRQHandler,            /* Brown Out Detect */
        USB_IRQHandler,            /* USB */
        CAN_IRQHandler,            /* CAN */
        DMA_IRQHandler,            /* GP DMA */
        I2S_IRQHandler,            /* I2S */
        ENET_IRQHandler,           /* Ethernet */
        RIT_IRQHandler,            /* Repetitive Interrupt Timer */
        MCPWM_IRQHandler,          /* Motor Control PWM */
        QEI_IRQHandler,            /* Quadrature Encoder Interface */
        PLL1_IRQHandler,           /* PLL1 (USB PLL) */
};

/*
TODO: This is a terrible hack and should not be done this way, help!
*/
char __exidx_start[0];
char __exidx_end[0];


/*******************************************************************************
* Function Name  : Reset_Handler
* Description    : This is the code that gets called when the processor first starts execution
*		       following a reset event.  Only the absolutely necessary set is performed,
*		       after which the application supplied main() routine is called.
* Input          :
* Output         :
* Return         :
*******************************************************************************/

void Reset_Handler(void)
{
    unsigned long *pulDest;
    unsigned long *pulSrc;

    //
    // Copy the data segment initializers from flash to SRAM in ROM mode
    //

    if (&_sidata != &_sdata) {	// only if needed
		pulSrc = &_sidata;
		for(pulDest = &_sdata; pulDest < &_edata; ) {
			*(pulDest++) = *(pulSrc++);
		}
    }

    // Copy the .fastcode code from ROM to SRAM

    if (&_sifastcode != &_sfastcode) {	// only if needed
    	pulSrc = &_sifastcode;
		for(pulDest = &_sfastcode; pulDest < &_efastcode; ) {
			*(pulDest++) = *(pulSrc++);
		}
    }

    //
    // Zero fill the bss segment.
    //
    for(pulDest = &_sbss; pulDest < &_ebss; )
    {
        *(pulDest++) = 0;
    }

    // copy initial values and set irq table
    if(irq_handler_table && irq_table_size) {
        int i;
        pulSrc = (unsigned long*)g_pfnVectors;
        pulDest = (unsigned long*)irq_handler_table;
        for(i = irq_table_size; i--; ) {
            *(pulDest++) = *(pulSrc++);
        }
        /*SCB->VTOR=0x10000000;*/
        SCB->VTOR=(uint32_t)irq_handler_table;
    }

    // Start the CPU (PLL, FLASH)
    SystemInit();

    // Configure the rest of the board 
    boardInit();

    // Call the application's entry point.
    main();
    
    while (1) {}; // Wait for the sweet release of death by watchdog
}


#if (0)

//*****************************************************************************
// 
// Provide weak aliases for each Exception handler to the Default_Handler.
// As they are weak aliases, any function with the same name will override
// this definition.
//
//*****************************************************************************
#pragma weak NMI_Handler = Default_Handler       
#pragma weak HardFault_Handler= Default_Handler; //Default_Handler  

#pragma weak MemManage_Handler = Default_Handler          /* MPU Fault Handler */
#pragma weak BusFault_Handler = Default_Handler           /* Bus Fault Handler */
#pragma weak UsageFault_Handler = Default_Handler         /* Usage Fault Handler */
#pragma weak SVC_Handler = Default_Handler                /* SVCall Handler */
#pragma weak DebugMon_Handler = Default_Handler           /* Debug Monitor Handler */
#pragma weak PendSV_Handler = Default_Handler             /* PendSV Handler */
#pragma weak SysTick_Handler = Default_Handler            /* SysTick Handler */

/* External interrupt vector handler */
#pragma weak WDT_IRQHandler = Default_Handler            /* Watchdog Timer */
#pragma weak TIMER0_IRQHandler = Default_Handler         /* Timer0 */
#pragma weak TIMER1_IRQHandler = Default_Handler         /* Timer1 */
#pragma weak TIMER2_IRQHandler = Default_Handler         /* Timer2 */
#pragma weak TIMER3_IRQHandler = Default_Handler         /* Timer3 */
#pragma weak UART0_IRQHandler = Default_Handler          /* UART0 */
#pragma weak UART1_IRQHandler = Default_Handler          /* UART1 */
#pragma weak UART2_IRQHandler = Default_Handler          /* UART2 */
#pragma weak UART3_IRQHandler = Default_Handler          /* UART3 */
#pragma weak PWM1_IRQHandler = Default_Handler           /* PWM1 */
#pragma weak I2C0_IRQHandler = Default_Handler           /* I2C0 */
#pragma weak I2C1_IRQHandler = Default_Handler           /* I2C1 */
#pragma weak I2C2_IRQHandler = Default_Handler           /* I2C2 */
#pragma weak SPI_IRQHandler = Default_Handler            /* SPI */
#pragma weak SSP0_IRQHandler = Default_Handler           /* SSP0 */
#pragma weak SSP1_IRQHandler = Default_Handler           /* SSP1 */
#pragma weak PLL0_IRQHandler = Default_Handler           /* PLL0 (Main PLL) */
#pragma weak RTC_IRQHandler = Default_Handler            /* Real Time Clock */
#pragma weak EINT0_IRQHandler = Default_Handler          /* External Interrupt 0 */
#pragma weak EINT1_IRQHandler = Default_Handler          /* External Interrupt 1 */
#pragma weak EINT2_IRQHandler = Default_Handler          /* External Interrupt 2 */
#pragma weak EINT3_IRQHandler = Default_Handler          /* External Interrupt 3 */
#pragma weak ADC_IRQHandler = Default_Handler            /* A/D Converter */
#pragma weak BOD_IRQHandler = Default_Handler            /* Brown Out Detect */
#pragma weak USB_IRQHandler = Default_Handler            /* USB */
#pragma weak CAN_IRQHandler = Default_Handler            /* CAN */
#pragma weak DMA_IRQHandler = Default_Handler            /* GP DMA */
#pragma weak I2S_IRQHandler = Default_Handler            /* I2S */
#pragma weak ENET_IRQHandler = Default_Handler           /* Ethernet */
#pragma weak RIT_IRQHandler = Default_Handler            /* Repetitive Interrupt Timer */
#pragma weak MCPWM_IRQHandler = Default_Handler          /* Motor Control PWM */
#pragma weak QEI_IRQHandler = Default_Handler            /* Quadrature Encoder Interface */
#pragma weak PLL1_IRQHandler = Default_Handler           /* PLL1 (USB PLL) */

void Default_Handler(void) {
     while (1) { }
}

#else

#include "default-handlers.h"

#endif

#if (0)

/**
 * HardFault_HandlerAsm:
 * Alternative Hard Fault handler to help debug the reason for a fault.
 * To use, edit the vector table to reference this function in the HardFault vector
 * This code is suitable for Cortex-M3 and Cortex-M0 cores
 */

// Use the 'naked' attribute so that C stacking is not used.
__attribute__((naked))
void HardFault_HandlerAsm(void){
        /*
         * Get the appropriate stack pointer, depending on our mode,
         * and use it as the parameter to the C handler. This function
         * will never return
         */

        __asm(  ".syntax unified\n"
                        "MOVS   R0, #4  \n"
                        "MOV    R1, LR  \n"
                        "TST    R0, R1  \n"
                        "BEQ    _MSP    \n"
                        "MRS    R0, PSP \n"
                        "B      HardFault_HandlerC      \n"
                "_MSP:  \n"
                        "MRS    R0, MSP \n"
                        "B      HardFault_HandlerC      \n"
                ".syntax divided\n") ;
}

/**
 * HardFaultHandler_C:
 * This is called from the HardFault_HandlerAsm with a pointer the Fault stack
 * as the parameter. We can then read the values from the stack and place them
 * into local variables for ease of reading.
 * We then read the various Fault Status and Address Registers to help decode
 * cause of the fault.
 * The function ends with a BKPT instruction to force control back into the debugger
 */
void HardFault_HandlerC(unsigned long *hardfault_args){
        volatile unsigned long stacked_r0 __attribute__ ((unused));
        volatile unsigned long stacked_r1 __attribute__ ((unused));
        volatile unsigned long stacked_r2 __attribute__ ((unused));
        volatile unsigned long stacked_r3 __attribute__ ((unused));
        volatile unsigned long stacked_r12 __attribute__ ((unused));
        volatile unsigned long stacked_lr __attribute__ ((unused));
        volatile unsigned long stacked_pc __attribute__ ((unused));
        volatile unsigned long stacked_psr __attribute__ ((unused));
        volatile unsigned long _CFSR __attribute__ ((unused));
        volatile unsigned long _HFSR __attribute__ ((unused));
        volatile unsigned long _DFSR __attribute__ ((unused));
        volatile unsigned long _AFSR __attribute__ ((unused));
        volatile unsigned long _BFAR __attribute__ ((unused));
        volatile unsigned long _MMAR __attribute__ ((unused));

        stacked_r0 = ((unsigned long)hardfault_args[0]) ;
        stacked_r1 = ((unsigned long)hardfault_args[1]) ;
        stacked_r2 = ((unsigned long)hardfault_args[2]) ;
        stacked_r3 = ((unsigned long)hardfault_args[3]) ;
        stacked_r12 = ((unsigned long)hardfault_args[4]) ;
        stacked_lr = ((unsigned long)hardfault_args[5]) ;
        stacked_pc = ((unsigned long)hardfault_args[6]) ;
        stacked_psr = ((unsigned long)hardfault_args[7]) ;

        // Configurable Fault Status Register
        // Consists of MMSR, BFSR and UFSR
        _CFSR = (*((volatile unsigned long *)(0xE000ED28))) ;   
                                                                                        
        // Hard Fault Status Register
        _HFSR = (*((volatile unsigned long *)(0xE000ED2C))) ;

        // Debug Fault Status Register
        _DFSR = (*((volatile unsigned long *)(0xE000ED30))) ;

        // Auxiliary Fault Status Register
        _AFSR = (*((volatile unsigned long *)(0xE000ED3C))) ;

        // Read the Fault Address Registers. These may not contain valid values.
        // Check BFARVALID/MMARVALID to see if they are valid values
        // MemManage Fault Address Register
        _MMAR = (*((volatile unsigned long *)(0xE000ED34))) ;
        // Bus Fault Address Register
        _BFAR = (*((volatile unsigned long *)(0xE000ED38))) ;

        __asm("BKPT #0\n") ; // Break into the debugger

}


#endif
