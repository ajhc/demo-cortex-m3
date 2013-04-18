PROJ=chiller
AVR = atmega88
INTERMEDIATE = parameterlist.h
UART_BAUD=9600
F_CPU=8000000

include ../atmega8/makefile

parameterlist.h: generate-parameters.pl
	./generate-parameters.pl
