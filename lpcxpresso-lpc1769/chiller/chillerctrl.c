#include <ctype.h>
#include <inttypes.h>

#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include <avr/io.h>
#include <util/delay.h>
#include <avr/pgmspace.h>

#include <avr/wdt.h> 
#include <avr/interrupt.h>
#include <avr/eeprom.h> 

#include "pwmvoltages.h"
#include "adchelper.h"
#include "lcd.h"
#include "mstdio.h"

// We don't really care about unhandled interrupts.
EMPTY_INTERRUPT(__vector_default)

// A macro and function to store string constants in flash and only copy them to
// RAM when needed, note the limit on string length.

char stringBuffer[80];

const char *getString(PGM_P src) {
    strcpy_P(stringBuffer, src);
    return stringBuffer;
}

#define PROGSTR(s) getString(PSTR(s))

void led(char on) {
  if (on) {
    PORTB |= _BV(PB5);   
  } else {
    PORTB &=~ _BV(PB5);   
  }
}

char NL[] = "\r\n";


#define INPUT_BUFFER_SIZE 40
char inputBuffer[INPUT_BUFFER_SIZE];
char *inputBufferEnd;

void resetInputBuffer() {
  memset(inputBuffer, 0, INPUT_BUFFER_SIZE);
  inputBufferEnd = inputBuffer;
}

#include "parameterlist.h"

PGM_P getParameterNamePGM(int index) {
  return (PGM_P)pgm_read_word(&(PARAMETER_NAMES[index]));
}

char *getParameterName(int index) {
  strcpy_P(stringBuffer, getParameterNamePGM(index));
  return stringBuffer;
}


void printState() {  
  parameters[P_COOLING_PWM]         = getCurrentCoolingSpeed();
  parameters[P_CIRCULATION_PWM]     = getCurrentCirculationSpeed();

  for (int i=0;i<P_COUNT;i++) {
    mprintf(PSTR("p%d\t%d\t%p\n"), i, parameters[i], getParameterNamePGM(i));
  }
  mputs(NL);
}


#define STATE_OFF 0
#define STATE_ON 1
#define STATE_FAN_ON 2
#define STATE_COMPRESSOR_ON 3
#define STATE_WARMUP 4

void setState(char state) {
  parameters[P_CURRENT_STATE] = state;
  
  // Turn things off first (fall-through is intentional):
  switch (state) {
  case STATE_OFF:
    parameters[P_POWER] = 0;
    PORTB &=~ _BV(PB0);  // Tank circulation pump Relay
	parameters[P_TANK_RELAY] = 0;
    setCirculationSpeed(0);    
    setCoolingSpeed(0);    

  case STATE_ON:
    PORTD &=~ _BV(PD7);  // Fan Relay
	parameters[P_FAN_RELAY] = 0;

  case STATE_WARMUP:
  case STATE_FAN_ON:
    PORTC &=~ _BV(PC5);  // Compressor Relay	
	parameters[P_COMPRESSOR_RELAY] = 0;
  }


  // Turn things on last (fall-through is intentional):
  switch (state) {
  case STATE_COMPRESSOR_ON:
    PORTC |= _BV(PC5);  // Compressor Relay
	parameters[P_COMPRESSOR_RELAY] = 1;
	
  case STATE_WARMUP:
  case STATE_FAN_ON:
    PORTD |=  _BV(PD7);  // Fan Relay
	parameters[P_FAN_RELAY] = 1;

  case STATE_ON:
    PORTB |=  _BV(PB0);  // Tank circulation pump Relay
	parameters[P_TANK_RELAY] = 1;
    setCirculationSpeed(PUMP_SPEED_MAX);        
  }
}



char ledToggle;

void handleInputLine() {
  led(ledToggle); 
  ledToggle = ledToggle ? 0 : 1;

  char *value = strchr(inputBuffer, '=');
  if (value) {
    *value = 0; // Zero terminate the key.
    value++;    
    int val = atoi(value);

    for (int i=0;i<P_RW_COUNT;i++) {
      if (!strcmp_P(inputBuffer, getParameterNamePGM(i))) {
	  parameters[i] = val;
	  mprintf(PSTR("Ok: Set p%d = %d\n"), i, val);	
      }
    }
	    
  } else if (*inputBuffer) {
    mprintf(PSTR("Fail '%s'\n"), inputBuffer);
  } else {
    mprintf(PSTR("Status:\n"));	
  }
  
  printState();
}

void updateDisplay() {
  
  lcd_gotoxy(0,0);
  if (parameters[P_CURRENT_STATE] == STATE_OFF) {
    lcd_puts_p(PSTR("  pSaw Chiller   "));
    lcd_gotoxy(0,1);
    lcd_puts_p(PSTR("    Standby      "));
    return;

  } else if (parameters[P_CURRENT_STATE] == STATE_ON) {
    lcd_puts_p(PSTR(" Chiller online  "));

  } else if (parameters[P_CURRENT_STATE] == STATE_FAN_ON) {
    lcd_puts_p(PSTR("   Running fan   "));

  } else if (parameters[P_CURRENT_STATE] == STATE_WARMUP) {
    lcd_puts_p(PSTR("   Huuuurnggh!   "));

  } else if (parameters[P_CURRENT_STATE] == STATE_COMPRESSOR_ON) {
    lcd_puts_p(PSTR("   Compressing!  "));
  }
  
  lcd_gotoxy(0,1);
  lcd_printf(PSTR("s=%d \xdf" "C o=%d \xdf" "C"),
	     parameters[P_STORE_CURRENT]/10,
	     parameters[P_CIRCULATION_CURRENT]/10);
}

void pollInput() {
  
  if (mchready()) {
    char ch = mgetch();

    if (ch == '\r') {
      mputs(NL);

      *inputBufferEnd = 0;
      handleInputLine();
      resetInputBuffer();

    } else {
      *inputBufferEnd = ch;
      inputBufferEnd++;
      mputchar(ch);

      if (inputBufferEnd == inputBuffer + INPUT_BUFFER_SIZE -1) {	
	mprintf(PSTR("ERROR: linebuffer overflow\n"));
	resetInputBuffer();
      }      
    }  
  }
}

#define AVCC_MV 4650.0
#define NTC_PULLUP 10000
#define NTC_PULLUP_VOLTAGE (AVCC_MV/1000)

// Constants from the datasheet for the NTC used:
// ELFA:60-279-24 (1% 5k RH16 6D502)
#define NTC_B50 3936.0
#define NTC_R25 5000.0


/*
 These routines weigh in at 538 bytes of code in total, perhaps I should use a lookup table in stead
 http://appnote.avrportal.com/calculator/thermistor-ntc
*/
/*
unsigned int readADCmv(const int channel) {
  unsigned int raw = getOsADC(channel);

  return (AVCC_MV*raw) / (1<<10);
}

float readNTCres(const int channel) {
  float v = readADCmv(channel);
  v /= 1000;
  return -(NTC_PULLUP * v) / (v-NTC_PULLUP_VOLTAGE);
}

float r2c(float r) {
  return 1/(1/(25+273.15) + 1/NTC_B50 * log(r/NTC_R25))-273.15;
}

float readNTCcelcius(const int channel) {
  return r2c(readNTCres(channel));
}

int readNTC(const unsigned char channel) {
  return (int)floor(readNTCcelcius(channel)*10);
}

*/

// Calculated using: http://appnote.avrportal.com/calculator/thermistor-ntc
const unsigned int NTC_MINUS_20_TO_PLUS_30[] PROGMEM = { 860, 851, 842, 833, 823, 814, 804, 793, 783, 772, 761, 750, 738, 727, 715, 703, 691, 679, 666, 654, 641, 628, 616, 603, 590, 577, 564, 552, 539, 526, 514, 501, 489, 476, 464, 452, 440, 428, 417, 406, 394, 383, 373, 362, 352, 341, 331, 322, 312, 303, 294 };

int readNTCint(const unsigned char channel) {
  unsigned int raw = getOsADC(channel);
  
  signed char underIndex = -1;
  unsigned int underADC = 1024;
  unsigned int overADC = 0;
  while (underADC > raw && underIndex < 51) {
    ++underIndex;
    overADC = underADC;			    
    underADC = pgm_read_word(&NTC_MINUS_20_TO_PLUS_30[underIndex]);
  }
  
  if (underIndex < 1) {
    return -200;
  } else if (underIndex >= 51) {
    return 300;
  }

  return (underIndex-20)*10 - (raw-underADC)*10/(overADC-underADC);
}


#define P 10000

#define MIN_OUTPUT ((long)0)
#define MAX_OUTPUT ((long)1023<<PID_Q)
#define MIN_SPEED 512

long circOutput;

unsigned int longpwm = 0;
unsigned char timer = 0;

#define TICKS_PER_SECOND 59
#define LONG_PWM_TICK 5*TICKS_PER_SECOND

void updateStateMachine() {

  int thisStore = readNTCint(0);
  parameters[P_STORE_CURRENT] += (thisStore-parameters[P_STORE_CURRENT])/10;

  int thisCirculation = readNTCint(1);
  parameters[P_CIRCULATION_CURRENT] += (thisCirculation-parameters[P_CIRCULATION_CURRENT])/10;

  if (parameters[P_CURRENT_STATE] == STATE_OFF) {
    if (parameters[P_POWER]) {
      setState(STATE_ON);
    }

  } else {
	
    // This is the PID regulation of the circulation temperature
    long error = parameters[P_CIRCULATION_TEMP]-parameters[P_CIRCULATION_CURRENT];
    circOutput -= error * P;

    if (circOutput > MAX_OUTPUT) {
      circOutput = MAX_OUTPUT;
      
    } else if (circOutput < MIN_OUTPUT) {
      circOutput = MIN_OUTPUT;
    }
    
    int co = circOutput>>PID_Q;
    if (co < MIN_SPEED) {
      if (longpwm++ > LONG_PWM_TICK) {
	longpwm = 0;
      }
      
      if (longpwm < (LONG_PWM_TICK*co)/MIN_SPEED) {
	setCoolingSpeed(MIN_SPEED);
      } else {
	setCoolingSpeed(0);
      }      

    } else {
      setCoolingSpeed(co);
    }


    if (parameters[P_CURRENT_STATE] == STATE_ON) {

      if (parameters[P_POWER]) {
      
	if (parameters[P_STORE_CURRENT] > parameters[P_STORE_MAX_TEMP]) {
	  setState(STATE_WARMUP);
	  parameters[P_FAN_TIMER] = 4; // This starts the fan a little while before the compressor to reduce the surge.
	  timer = 0;
	}      
      
      } else {
	setState(STATE_OFF);
      }


    } else if (parameters[P_CURRENT_STATE] == STATE_WARMUP) {
      if (++timer >= TICKS_PER_SECOND) {
	timer = 0;
	parameters[P_FAN_TIMER]--;

	if (parameters[P_FAN_TIMER] <= 0) {
	  setState(STATE_COMPRESSOR_ON);
	}
      }


    } else if (parameters[P_CURRENT_STATE] == STATE_COMPRESSOR_ON) {

      if (parameters[P_STORE_CURRENT] < parameters[P_STORE_MIN_TEMP] || !parameters[P_POWER]) {
	setState(STATE_FAN_ON);
	parameters[P_FAN_TIMER] = parameters[P_FAN_POST_RUN];
	timer = 0;
      }      


    } else if (parameters[P_CURRENT_STATE] == STATE_FAN_ON) {

      if (++timer >= TICKS_PER_SECOND) {
	timer = 0;
	parameters[P_FAN_TIMER]--;

	if (parameters[P_STORE_CURRENT] > parameters[P_STORE_MAX_TEMP]) {
	  setState(STATE_COMPRESSOR_ON);

	} else if (parameters[P_FAN_TIMER] <= 0) {
	  setState(STATE_ON);
	}
      }
   
    }	
  }
}


int main(void) {
  wdt_enable(WDTO_4S); // We don't want to hang.

  // Set up the outputs:
  DDRB  |= _BV(PB5);  // LED output on SCK pin

  DDRC  |= _BV(PC5);  // Compressor Relay
  DDRD  |= _BV(PD7);  // Fan Relay
  DDRB  |= _BV(PB0);  // Tank circulation pump Relay

  led(1);

  initADC();
  initPWM();
  resetInputBuffer();

  muartInit();
  mprintf(PSTR("#Power up!\n"));

  lcd_init(LCD_DISP_ON);
  led(0);
  setState(STATE_OFF);
  memcpy_P(parameters, (PGM_VOID_P)DEFAULT_PARAMETERS, sizeof(DEFAULT_PARAMETERS));

  char frame = 0;
  updateDisplay();

  while(1) {
    led(ledToggle); 
    ledToggle = ledToggle ? 0 : 1;

    if ((frame & 15) == 0) {
      updateDisplay();
    } 

    pollInput();
    updateStateMachine();
    updatePWM();
    //_delay_ms(10);
    wdt_reset();
    frame++;
  }	
}
