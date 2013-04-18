#ifndef __USBWRAPPER_H
#define __USBWRAPPER_H

#include "board.h"

// This is the maximum input line size
#define USB_LINE_BUFFER_SIZE 1<<12

// Callback called when a full line has been buffered by the USB CDC layer.
void usbLine(char *line, unsigned int lineSize);

// Function to send data via USB
void usbSend(const char *data, unsigned int dataSize);
void usbSendFlush(const char *data, unsigned int dataSize);
void usbFlush();

// Initializes the USB subsystem, called from initAPI()
void usbWrapperInit();

#endif
