#STM32F3-Discovery Application Template
This package is for use when compiling programs for STM32F30x ARM microcontrollers using arm-none-eabi-gcc (I'm using the [gcc-arm-embedded](https://launchpad.net/gcc-arm-embedded) toolchain). The Makefile in the main directory will call the Make file in the Libraries directory, thereby automatically building the STM peripheral library. However, running 'make clean' will not affect the peripherals library (the same command can be run from the Libraries directory to do this).

This template will serve as a quick-start for those who do not wish to use an IDE, but rather develop in a text editor of choice and build from the command line. It is (indirectly) based on [an example template for the F4 Discovery board](http://jeremyherbert.net/get/stm32f4_getting_started) put together by Jeremy Herbert.  More recently, it is based on [an example template for the F0 Discover board](https://github.com/szczys/stm32f0-discovery-basic-template) by Mike Szczys.

##Subfolders:

1. Libraries/
   * This is the Libraries/ folder from the STM32F30x_StdPeriph_Driver standard peripheral driver library and STM32_USB-FS-Device_Driver produced by STM. This preserves the original structure (with some deletions to save space) which should make it easy to roll in library upgrades as they are published
   * **Makefile** is not part of the STM release, and must be copied over if the library is upgraded.
   * **stm32f30x_conf.h** is used to configure the peripheral library. It must be copied here if the library is upgraded. The file was file taken from the STM32F3-Discovery firmware package. It is found in the following directory:
      * Project/Demonstration/
   * **Abstracting the libraries:** You may place this folder anywhere you like in order to use it for multiple projects. Just change the path of the STD_PERIPH_LIB variable in the Makefile

2. Device/
   * Folder contains device specific files:
   * **startup_stm32f30x.s** is the startup file taken from the STM32F0-Discovery firmware package. It is found in the following directory:
      * Libraries/CMSIS/ST/STM32F0xx/Source/Templates/TrueSTUDIO/
   * Linker Scripts (Device/ldscripts)
      * These linker scripts are used instead of the stm32_flash.ld script which is included in the STM demo code. This is because the original file contains an unreasonably restrictive copyright assertion.
      * The STM32F303VCT6 used by the STM32F3-Discovery board has 256K of flash, and 48K or RAM...however, 8K of this RAM is special core-coupled SRAM, and seems to have a different position in the memory map.  I didn't bother figuring out exactly what is going on, so I set the linker script to only use 32K RAM just to be safe.  If anyone cares to refine this, please let me know.

3. inc/
   * All include files for this particular project.
   * Some of the peripheral source files also look for hw_config.h and usb_conf.h, so I've put them here.

4. src/
   * All source files for this particular project (including main.c).
   * **system_stm32f30x.c** can be generated using an XLS file developed by STM. This sets up the system clock values for the project. The file included in this repository is taken from the STM32F3-Discovery firmware package. It is found in the following directory:
      * Libraries/CMSIS/Device/ST/STM32F30x/Source/Templates/

5. extra/
   * This contains a procedure file used to write the image to the board via OpenOCD
   * **Abstracting the extra folder:** the .cfg file in the extra folder may be placed anywhere so that multiple projects can use one file. Just change the OPENOCD_PROC_FILE variable in the Make file to match the new location.

##Loading the image on the board

If you have OpenOCD (at least v0.6.1) installed 'make program' can be used to flash the .bin file to the board. OpenOCD must be installed with stlink enabled. Clone [the git repository](http://openocd.git.sourceforge.net/git/gitweb.cgi?p=openocd/openocd;a=summary) and use these commands to compile/install it:

    ./bootstrap
    ./configure --prefix=/usr --enable-maintainer-mode --enable-stlink
    make 
    sudo make install

If there is an error finding the .cfg file, please double-check the OPENOCD_BOARD_DIR constant at the top of the Makefile (in this template directory, not in OpenOCD).

###UDEV Rule for the Discovery Board

If you are not able to communicate with the STM32F3-Discovery board without root privileges you should follow the step from [the stlink repo readme file](https://github.com/texane/stlink#readme) for adding a udev rule for this hardware.

##Compiling your own toolchain
It might be best to use a precompiled toolchain like CodeSourcery G++: Lite Edition or gcc-arm-embedded. But if you would prefer to compile your own, give [this guide](http://www.kunen.org/uC/gnu_tool.html) a try. Just google for the source code to make sure you're using the most recent versions. GCC now comes with the core and g++ code all in one archive.

Note that the CodeSourcery G++: Lite Edition toochain doesn't support hardware floating-point, so you'll need to use gcc-arm-embedded or roll your own in order to take advantage of the FPU on this chip.

###Possible compiling errors:
   * You may encouter unfulfilled dependecies when it comes to GMP, MPFR and MPC. According to [the GCC installation Wiki](http://gcc.gnu.org/wiki/InstallingGCC) you should install the following packages: libgmp-dev libmpfr-dev libmpc-dev. If that doesn't work, read the linked Wiki for further options.
   * If you get the error: "configure: error: Link tests are not allowed after GCC_NO_EXECUTABLES" try adding the following flags when configuring GCC: "--with-system-zlib --disable-shared"
