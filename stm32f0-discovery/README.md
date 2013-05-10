#MicroCLI: A command-line interface (CLI) for the STM32F0-Discovery with WiFi and 2 Android Apps

##Getting started

This repository contains the source code for the STM32F0-Discovery board.
It is based on the [STM32F0-Discovery Application Template](https://github.com/szczys/stm32f0-discovery-basic-template), so have a look at its `Readme.md`.

The (German) project description with some more information is located [here](http://www.jann.cc/2012/08/13/microcli_befehlsinterpreter_auf_einem_stm32f0_discovery.html).

##Subfolders:

1. Libraries/
   * This folder contains the STM32F0xx_StdPeriph_Lib_V1.0.0 standard peripheral driver library produced by STM. This preserves the original structure which should make it easy to roll in library upgrades as they are published
   * libemb is a current git snapshot from https://github.com/wendlers/libemb

2. Device/
   * Folder contains device specific files:
   * **startup_stm32f0xx.s** is the startup file taken from the STM32F0-Discovery firmware package. It is found in the following directory:
      * Libraries/CMSIS/ST/STM32F0xx/Source/Templates/TrueSTUDIO/
   * Linker Scripts (Device/ldscripts)
      * These linker scripts are used instead of the stm32_flash.ld script which is included in the STM demo code. This is because the original file contains an unreasonably restrictive copyright assertion.

3. inc/
   * All include files for this particular project.

4. src/
   * All source files for this particular project (including main.c).

5. extra/
   * This contains a procedure file used to write the image to the board via OpenOCD

##Loading the image on the board

If you have OpenOCD installed `make program` can be used to flash the .bin file to the board. OpenOCD must be installed with stlink enabled. Clone [the git repository](http://sourceforge.net/p/openocd/code/) and use these commands to compile/install it:

    git clone git://git.code.sf.net/p/openocd/code openocd.git
    cd openocd.git
    ./bootstrap
    ./configure --prefix=/usr --enable-maintainer-mode --enable-stlink
    make 
    sudo make install

If there is an error finding the .cfg file, please double-check the OPENOCD_BOARD_DIR constant at the top of the Makefile (in this template directory, not in OpenOCD).

###UDEV Rule for the Discovery Board

If you are not able to communicate with the STM32F0-Discovery board without root privileges you should follow the step from [the stlink repo readme file](https://github.com/texane/stlink#readme) for adding a udev rule for this hardware.


##GCC ARM Embedded Toolchain

It might be best to use a precompiled toolchain.

* https://launchpad.net/gcc-arm-embedded/+download
    * Extract the archive and add it to the path, e.g. edit `~/.bashrc` and add a line with the path
      to the extracted archive `export PATH=$PATH:/home/chris/tools/gcc-arm-none-eabi-4_6-2012q2/bin`

* Alternatives
    * http://www.mentor.com/embedded-software/sourcery-tools/sourcery-codebench/editions/lite-edition/
    * https://github.com/esden/summon-arm-toolchain
