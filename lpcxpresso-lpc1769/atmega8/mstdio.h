#pragma once

/*
  This is a very minimal implementation of the bits of stdio I needed for my atmega
  projects.

  It provides a character based interface to the uart and printf-like functions
  to format output with the following features:

  * Format string is always in FLASH, never RAM.
  * %s: String from RAM
  * %p: String from FLASH, declare using: PSTR("foo")
  * %d: integer from RAM 
  * %x: integer from RAM, in hex
  * %l: long from RAM
  * \n: \r\n
  * %%: %

  This code was written for the PhotonSaw project by Flemming Frandsen <http://dren.dk> in 2012.
  For details see: http://psaw.osaa.dk
*/

#include <stdarg.h>
#include <avr/pgmspace.h>

// Initialize the uart
void muartInit(void);

// True if a character is ready to be read from the uart
char mchready();

// A single character from the uart, will block if none is ready.
char mgetch();

// Send a single char via the uart
void mputchar(const char c);

// Send a string from RAM via the uart
void mputs(char *c);

// Prints a formatted string using a format stored in flash
void mprintf(PGM_P format, ...);

// like mprintf, but outputs to a string
void msprintf(char *out, PGM_P format, ...);

// Prints using a function pointer for output:
void mfprintf(void (*putchar)(const char), PGM_P format, ...);

// This is the underlying implementation, use it to build output to other channels.
void mvfprintf(void (*putchar)(const char), PGM_P format, va_list ap);
