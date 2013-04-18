#ifndef __SYSTEM_LPC17xx_H
#define __SYSTEM_LPC17xx_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

extern uint32_t SystemCoreClock; // System Clock Frequency (Core Clock)

// Updates the SystemCoreClock with current core Clock retrieved from cpu registers.
extern void SystemCoreClockUpdate (void);

// Set up all board specific hardware
extern void SystemInit();

#ifdef __cplusplus
}
#endif

/**
 * @}
 */

#endif /* __SYSTEM_LPC17xx_H */
