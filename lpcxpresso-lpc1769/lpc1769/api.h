#ifndef __API_H
#define __API_H

#include <stdio.h>

#include "board.h"
#include "adc.h"
#include "pwm.h"
#include "uarts.h"
#include "usbwrapper.h"

extern FILE *chiller;
extern FILE *watchdog;

void initAPI();

/*
// Allocate the largest 2^n sized buffer we can in IRAM1 for the move buffer.
#define MOVE_BUFFER_SIZE (1<<12)
extern unsigned int moves[MOVE_BUFFER_SIZE] IN_IRAM1;
*/

#endif
