#include "api.h"

#include <string.h>
#include <stdio.h>

void usbLine(char *line, unsigned int lineSize) {
  fprintf(stderr, "Got line from USB: %s\n\r", line);
}

void handleUart0Line(const char *line, int lineLength) {
  fprintf(stderr, "Got line from UART0: %s\r\n", line); 
}

void handleUart1Line(const char *line, int lineLength) {
  fprintf(stderr, "Got line from UART1: %s\r\n", line); 
}

void handleUart2Line(const char *line, int lineLength) {
  fprintf(stderr, "Got line from UART2: %s\r\n", line); 
}

void handleUart3Line(const char *line, int lineLength) {
  if (lineLength) {
    fiprintf(stderr, "WD said: %s\n\r", line);
  }
}


const char TESTHEST[] = "Test hest\n\r";

int main(void) {
  fiprintf(stderr, "Power Up!\n\r");
  
  while (1) {
    GPIO_SET(IO_LED);
    GPIO_SET(IO_ASSIST_AIR);
    GPIO_CLEAR(IO_EXHAUST);
    GPIO_CLEAR(IO_LASER_FIRE);
    delay(500);

    GPIO_CLEAR(IO_LED);
    GPIO_CLEAR(IO_ASSIST_AIR);
    GPIO_SET(IO_EXHAUST);
    GPIO_SET(IO_LASER_FIRE);
    delay(500);

    fiprintf(stderr, "USB connected: %d\n\r", usbConnected());
    if (usbConnected()) {
      usbSendFlush(TESTHEST, sizeof(TESTHEST));
    }

    fiprintf(stderr, "Airflow: %d (%d %%)\n\r", READ_ADC(IO_AIRFLOW), airflow());

    fprintf(stderr, "T out:   %d (%f Ohm, %f degC)\n\r",
	   READ_ADC(IO_TEMP_OUT),
	   readNTCres(IO_CHAN(IO_TEMP_OUT)),
	   readNTCcelcius(IO_CHAN(IO_TEMP_OUT))
	   );

    fprintf(stderr, "T in:    %f degC\n\r", readNTCcelcius(IO_CHAN(IO_TEMP_IN)));
    fprintf(stderr, "T inter: %f degC\n\r", readNTCcelcius(IO_CHAN(IO_TEMP_INTERNAL)));
    fiprintf(stderr, "Supply:  %d mv\n\r", supplyVoltage());        
    
    unsigned int err0 = errorUART(IO_DEBUG_RX);
    if (err0) {
      fiprintf(stderr, "Debug UART Error: %x\n\r", err0);        
    }

    err0 = errorUART(IO_WATCHDOG_RX);
    if (err0) {
      fiprintf(stderr, "Watchdog UART Error: %x\n\r", err0);        
    }
    err0 = errorUART(IO_CHILLER_RX);
    if (err0) {
      fiprintf(stderr, "Chiller UART Error: %x\n\r", err0);        
    }
  }
}
