/*
 * ----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <joerg@FreeBSD.ORG> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.        Joerg Wunsch
 * ----------------------------------------------------------------------------
 *
 * Stdio demo, UART implementation
 *
 * $Id: uart.c,v 1.1.2.1 2005/12/28 22:35:08 joerg_wunsch Exp $
 */
#include <stdint.h>
#include <stdio.h>

#include <avr/io.h>
#include <util/delay.h>

#include "uart.h"


/*
 * Initialize the UART to 9600 Bd, tx/rx, 8N1.
 */
void uart_init(void) {
#if F_CPU < 2000000UL && defined(U2X)
  UCSR0A = _BV(U2X);             /* improve baud rate error by using 2x clk */
  UBRR0L = (F_CPU / (8UL * UART_BAUD)) - 1;
#else
  UBRR0L = (F_CPU / (16UL * UART_BAUD)) - 1;
#endif
  UCSR0B = _BV(TXEN0) | _BV(RXEN0); /* tx/rx enable */
  //UCSRB = _BV(TXEN); /* tx enable */
}

/*
 * Send character c down the UART Tx, wait until tx holding register
 * is empty.
 */
int uart_putchar(char c, FILE *stream){

  //  if (c == '\n')
  //uart_putchar('\r', stream);
  loop_until_bit_is_set(UCSR0A, UDRE0);
  UDR0 = c;
  loop_until_bit_is_set(UCSR0A, UDRE0);
  
  return 0;
}

int uart_getchar(FILE *stream) {

  if (UCSR0A & 1<<RXC0) {
	if (UCSR0A & _BV(FE0))
	  return _FDEV_EOF;
	if (UCSR0A & _BV(DOR0))
	  return _FDEV_ERR;
	
	return UDR0;
  } else {
	return -1000;
  }
}

