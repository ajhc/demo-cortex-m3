#include "lpc17xx_gpio.h"
#include "board.h"

void led2_init(void) {
	LPC_PINCON->PINSEL1 &= (~(3 << 12));
	LPC_GPIO0->FIODIR |= (1 << 22);
}

void led2_on(void) {
	LPC_GPIO0->FIOSET = (1 << 22);
}

void led2_off(void) {
	LPC_GPIO0->FIOCLR = (1 << 22);
}

int main(void) {
	led2_init();

	while (1) {
		led2_on();
		delay(200);
		led2_off();
		delay(200);
	}
}
