#include <string.h>
#include <stdio.h>

#include "api.h"
#include "shaker.h"
#include "console.h"
#include "usbconsole.h"


int main(void) {
  fiprintf(stderr, "\x1b[2JPower Up!\r\n");

  /*
    Everything happens in the IRQ service routines:

    commander.c   Parses the usb commands
    watchbone.c   Code the watchdog chews on
    mrchilly.c    Chiller interface
    console.c     Debug serial port interface 
    usbconsole.c  USB serial port interface
    shaker.c      Realtime control output
  */

  shakerInit();

  fiprintf(stderr, "Non-default IRQ priorities:\r\n");    
  for (int i=-2;i<35;i++) {
    unsigned int p = (unsigned int)NVIC_GetPriority(i);

    if (p != GROUP_PRIORITY_DEFAULT) {
      fiprintf(stderr, "  IRQ %d group:%d, sub:%d\r\n", i, p >> 2, p & 3);    
    }
  }

  int loop = 0;
  while (1) {
    SYSTICK_TYPE blinky = systick;
    while (!consolePending() && 
	   !usbPending() && 
	   systickInterval(blinky, systick) < 1000) ;

    if (usbPending()) {
      usbHandle();
    }

    if (consolePending()) {
      consoleHandle();
    }

    if (loop) {
      GPIO_SET(IO_LED);
      loop = 0;
    } else {
      GPIO_CLEAR(IO_LED);
      loop = 1;
    }
  }
}
