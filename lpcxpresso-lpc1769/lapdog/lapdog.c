#include <ctype.h>
#include <inttypes.h>

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <mstdio.h>

#include <avr/io.h>
#include <util/delay.h>
#include <avr/pgmspace.h>
#include <avr/sleep.h>

#include <avr/wdt.h> 
#include <avr/interrupt.h>
#include <avr/eeprom.h> 
#include <avr/pgmspace.h>

// We don't really care about unhandled interrupts.
EMPTY_INTERRUPT(__vector_default)

void led1(char on) {
  if (on) {
    PORTB |= _BV(PB6);   
  } else {
    PORTB &=~ _BV(PB6);   
  }
}

void led2(char on) {
  if (on) {
    PORTB |= _BV(PB7);   
  } else {
    PORTB &=~ _BV(PB7);   
  }
}

void stop() {
  PORTB &=~ _BV(PB2);
  PORTC &=~ _BV(PC1);
}

void run() {
  PORTB |= _BV(PB2);
  PORTC |= _BV(PC1);
}


int main(void) {
  DDRB  |= _BV(PB6) | _BV(PB7);  // LED outputs
  PORTB |= _BV(PB7) | _BV(PB6);

  DDRB  |= _BV(PB2);  // Enable Motors output
  DDRC  |= _BV(PC1);  // Enable LASER outout 

  stop(); // This pulls the motor driver reset
  
  wdt_enable(WDTO_4S);
  
  muartInit();
  mprintf(PSTR("#Power up!\n"));

  // Ensure that the motor drivers are properly initialized by holding them in reset while the power stabilizes.
  for (char i=0;i<10;i++) {
    _delay_ms(100);
  }
  run();
  
  mprintf(PSTR("#Motor drivers and laser armed!\n"));

  led1(0);
  led2(0);

  char frame = 0;
  while(1) {
    if (!(frame & 15)) {
      mprintf(PSTR("OK\n"));
    }

    if (frame & 8) {
      led1(!(frame & 1));  
    } else {
      led2(!(frame & 1));
    }
    
    _delay_ms(50);
    wdt_reset();
    frame++;
  }	
}
