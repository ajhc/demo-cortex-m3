/*
 *	STBee Mini LED点灯サンプル
 *	2010/8 Strawberry Linux Co.,Ltd.
 *
 *	割り込みも何も使わない最もシンプルなLED点灯サンプルです。
 *
 *
 */


// I/Oなどの全ての#defineがあります。
#include "stm32f10x_conf.h"
#include "c_extern.h"


// 空ループでウェイトするルーチン
void Delay(unsigned long delay)
{
	volatile unsigned long delay_v = delay;
	while(delay_v) delay_v--;
}


/*************************************************************************
 * Function Name: main
 * Parameters: none
 * Return: Int32U
 *
 * Description: The main subroutine
 *
 *************************************************************************/
int main(void)
{
	GPIO_InitTypeDef GPIO_InitStructure;
	int i;
	
	// STM32の初期化 クロック設定
	SystemInit();
	NVIC_SetVectorTable(0x3000, 0);

	// JTAGを無効にします。
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_AFIO , ENABLE);
	AFIO->MAPR = _BV(26);
	
	// GPIO A, B, Cポートを有効にします
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOA, ENABLE);
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOB, ENABLE);
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOC, ENABLE);
	
	
	// ポートの初期化(PA13, PA15を出力に)
	GPIO_InitStructure.GPIO_Pin = _BV(13) | _BV(15);
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_Out_PP;
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
	GPIO_Init(GPIOA, &GPIO_InitStructure);

#if 1
	{ /* Run Haskell code */
		int hsargc = 1;
		char *hsargv = "q";
		char **hsargvp = &hsargv;

		hs_init(&hsargc, &hsargvp);
		_amain();
		/* hs_exit(); */
	}
#endif

#if 0
#endif
	for (;;);

}

// オンボードLEDを点滅させる

void
stopblink_wink()
{
	while(1){
		GPIOA->ODR = _BV(13);
		Delay(500000);
		GPIOA->ODR = _BV(15);
		Delay(500000);
	}
}

void
stopblink_both()
{
	while(1){
		GPIOA->ODR = _BV(13) | _BV(15);
		Delay(500000);
		GPIOA->ODR = 0;
		Delay(500000);
	}
}

void
stopblink_one()
{
	while(1){
		GPIOA->ODR = _BV(13);
		Delay(500000);
		GPIOA->ODR = 0;
		Delay(500000);
	}
}
