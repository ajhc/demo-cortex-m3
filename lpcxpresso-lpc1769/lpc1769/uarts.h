#ifndef __UARTS_H
#define __UARTS_H

#include "board.h"

void initUARTs();

uint32_t recvUART(unsigned int rxPin, char *rxbuf, uint32_t buflen);
uint32_t sendUART(unsigned int txPin, char *txbuf, uint32_t buflen);
void flushUART(unsigned int txPin);

uint32_t errorUART(unsigned int rxPin);

#endif
