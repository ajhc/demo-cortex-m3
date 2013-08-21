#include "mbed.h"
#include "TextLCD.h"
 
// TextLCD lcd(p15, p16, p17, p18, p19, p20); // rs, e, d4-d7
TextLCD lcd(p24, p26, p27, p28, p29, p30); // rs, e, d0-d3
 
int main() {
    lcd.printf("Hello World!\n");
}

