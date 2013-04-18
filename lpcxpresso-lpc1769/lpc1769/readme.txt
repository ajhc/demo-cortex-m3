This directory contains the framework for writing applications for the lpc1769, 
specifically the photonsaw boards, but for the most part, the board specific parts
should be kept in a separate subdirectory, chosen by the application make file via
the BOARD variable.


Subdirs:
  drivers: Files from the lpc17xx driver library by NXP, including some very minor bugfixes.

  photonsaw-v1: Board specific routines and configuration for the first version of the PS board.

  newlib: Stubs that allow the needed functions of newlib to work, including file system support.

  usb: The USB-CDC sub system, use the functions in usbapi.h to access it.
  
  fat_sd: The SD card based FAT file system from: http://www.siwawi.arubi.uni-kl.de/avr_projects/arm_projects/arm_memcards/#chanfat_lpc_cm3


Files:

  lpc1769.ld: Linker script for the lpc1769 (and '68)

  startup.c: Boot strap code run before main, calls out to the board specific boardInit()
             and the application main()

  adc.h & adc.c: ADC "API" routines

  fix-lpcchecksum: Perl script used to make the interrupt vector checksum correct in a bin file.

  openocd.cfg: Configuration file used to start and configure openocd (use make flash)

  gdb.cfg: Configuration file loaded by gdb (use make gdb once openocd is started)

  makefile: Do not run this manually, include it from the application specific directories


Build environment:
set MAKEFLAGS=-j2
. /home/ff/projects/osaa/PhotonSaw/firmware/env
