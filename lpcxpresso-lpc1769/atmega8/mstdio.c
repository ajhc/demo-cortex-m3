#include "mstdio.h"
#include <stdlib.h>

void muartInit(void) {
#if F_CPU < 2000000UL && defined(U2X)
  UCSR0A = _BV(U2X);             /* improve baud rate error by using 2x clk */
  UBRR0L = (F_CPU / (8UL * UART_BAUD)) - 1;
#else
  UBRR0L = (F_CPU / (16UL * UART_BAUD)) - 1;
#endif
  UCSR0B = _BV(TXEN0) | _BV(RXEN0); /* tx/rx enable */
}

/*
 * Send character c down the UART Tx, wait until tx holding register
 * is empty.
 */
void mputchar(const char c) {
  loop_until_bit_is_set(UCSR0A, UDRE0);
  UDR0 = c;
  loop_until_bit_is_set(UCSR0A, UDRE0);
}  

void mputs(char *c) {
  while (*c) {
    mputchar(*(c++));
  }
}  

char mchready() {
  return UCSR0A & _BV(RXC0);
}

char mgetch() {
  while (!mchready()) { }
  return UDR0;
}


void mvfprintf(void (*putchar)(const char), PGM_P format, va_list ap) {

  char ch;
  while ((ch = pgm_read_byte(format))) {
    if (ch == '%') {
      char type = pgm_read_byte(++format);
      if (type == 'd' || type == 'x') { // An integer from ram
		int d = va_arg(ap, int);
		char db[10];
		db[0] = 0;
		itoa(d, db, type=='d' ? 10 : 16);
		char *s = db;
		while (*s) {
		  putchar(*(s++));
		}

      } else if (type == 'l') { // A long from ram
		long d = va_arg(ap, long);
		char db[10];
		db[0] = 0;
		itoa(d, db, 10);
		char *s = db;
		while (*s) {
		  putchar(*(s++));
		}

      } else if (type == 's') { // A string from ram
		char *s = va_arg(ap, char *);
		while (*s) {
		  putchar(*(s++));
		}

      } else if (type == 'p') { // A string from progmem
		PGM_P s = va_arg(ap, PGM_P);

		while ((ch = pgm_read_byte(s))) {
		  putchar(ch);
		  ++s;
		}

      } else { // Fall back is to print the formatting code (so %% works normally)
		putchar('%');
		putchar(ch);		
      }	  

    } else if (ch == '\n') { // This saves one byte of flash per line of output, so yay!
      putchar('\r');
      putchar(ch);

    } else {
      putchar(ch);
    }

    ++format;
  }

}

void mfprintf(void (*putchar)(const char), PGM_P format, ...) {
  va_list ap;
  va_start(ap, format);
  mvfprintf(putchar, format, ap);
  va_end(ap);
}

void mprintf(PGM_P format, ...) {
  va_list ap;
  va_start(ap, format);
  mvfprintf(mputchar, format, ap);
  va_end(ap);
}

char *sputCharPos;
void sputchar(char c) {
  *(sputCharPos++) = c;
}

void msprintf(char *out, PGM_P format, ...) {
  va_list ap;
  va_start(ap, format);
  sputCharPos = out;
  mvfprintf(sputchar, format, ap);
  *(sputCharPos++) = 0;
  va_end(ap);
}

