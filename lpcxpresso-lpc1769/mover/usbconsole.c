#include <stdio.h>
#include <string.h>

#include "api.h"
#include "usbconsole.h"
#include "commander.h"

volatile int pendingUsbCommand;
int usbPending() {
  return pendingUsbCommand;
}

char usbCommandLine[USB_LINE_BUFFER_SIZE] IN_IRAM1;
void usbLine(char *line, unsigned int lineSize) {
  if (!pendingUsbCommand) {
    strcpy(usbCommandLine, line);
    pendingUsbCommand = 1;
  } 
}

void usbHandle() {
  if (!pendingUsbCommand) {
    return;
  }

  commandRun(usbCommandLine, stdout);

  pendingUsbCommand = 0;
} 
