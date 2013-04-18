#include "adchelper.h"
#include <avr/io.h>

void initADC() {
  ADCSRA |= 1<<ADEN; // Enable ADC
}

unsigned int getADC(unsigned char input) {
    ADMUX = (input & 15) | _BV(REFS0); // AVcc reference + external cap.
    ADCSRA |= _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0) | _BV(ADIE);

    ADCSRA |= 1<<ADSC;
    while(ADCSRA & 1<<ADSC) {}
    
    unsigned int result = ADCL | ADCH << 8;
    return result;
}

#define ADC_OVERSAMPLES 4
unsigned int getOsADC(unsigned char input) {
    unsigned int sum = 0;

    for (int i=0;i<_BV(ADC_OVERSAMPLES);i++) {
	sum += getADC(input);
    }
    
    return sum >> ADC_OVERSAMPLES;
}
