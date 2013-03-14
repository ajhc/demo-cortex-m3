# Ajhc demo for Cortex-M3 board

First, you have to know there are several boards supported by the demo.
Second, the demo needs Ajhc 0.8.0.1 or later.

Using stm32f3-discovery is strongly recommend, so you can use gdb.

## [stm32f3-discovery](http://www.st.com/web/en/catalog/tools/FM116/SC959/SS1532/PF254044)

![](https://raw.github.com/ajhc/demo-cortex-m3/master/img/stm32f3-discovery.jpg)

* CPU: STM32F303VCT6
* ROM: 256kB
* RAM: 48kB

## [stbee-mini](http://strawberry-linux.com/catalog/items?code=32105)

![](https://raw.github.com/ajhc/demo-cortex-m3/master/img/stbee-mini-v2.jpg)

* CPU: STM32F103CBT6
* ROM: 128kB
* RAM: 20kB

## Common requirements

### Install summon-arm-toolchain

    $ git clone git://github.com/esden/summon-arm-toolchain.git
    $ apt-get install flex bison libgmp3-dev libmpfr-dev libncurses5-dev \
      libmpc-dev autoconf texinfo build-essential libftdi-dev zlib1g-dev \
      git zlib1g-dev python-yaml
    $ cd summon-arm-toolchain/
    $ ./summon-arm-toolchain
    $ export PATH=$HOME/sat/bin:$PATH

### Install Ajhc

    $ sudo apt-get install make locales autoconf libreadline-dev \
      libwww-perl libconfig-yaml-perl graphviz haskell-platform drift pandoc \
      libghc-readline-dev libghc-utf8-string-dev libghc-hssyck-dev libghc-pandoc-dev
    $ git clone git://github.com/ajhc/ajhc.git
    $ cd ajhc
    $ git checkout arafura
    $ autoreconf -i
    $ ./configure
    $ make
    $ sudo make install
    $ ajhc --version |head -1
    ajhc 0.8.0.1 (-0)

## If you use stm32f3-discovery

### Install stlink

    $ sudo apt-get install libsgutils2-dev libusb-1.0-0-dev
    $ git clone git://github.com/texane/stlink.git
    $ cd stlink/
    $ ./autogen.sh
    $ ./configure
    $ make
    $ sudo make install

### How to build

    $ git clone git://github.com/ajhc/demo-cortex-m3.git
    $ cd demo-cortex-m3/stm32f3-discovery/
    $ make

### Writing to flash

In one terminal, start the connection to the board.

    $ sudo st-util

In another terminal, connect to the debugger and flash program.

    $ make gdbwrite
    --snip--
    Loading section .isr_vector, size 0x188 lma 0x8000000
    Loading section .text, size 0x1ec4 lma 0x8000188
    Loading section .data, size 0x5c lma 0x800204c
    Start address 0x8001f89, load size 8360
    Transfer rate: 6 KB/sec, 2786 bytes/write.
    (gdb) c

## If you use stbee-mini

### Install dfu-util

    $ git clone git://gitorious.org/~tormod/unofficial-clones/dfuse-dfu-util.git
    $ cd dfuse-dfu-util/
    $ ./autogen.sh
    $ ./configure
    $ make
    $ sudo make install

### How to build

    $ git clone git://github.com/ajhc/demo-cortex-m3.git
    $ cd demo-cortex-m3/stbee-mini/
    $ make

### Writing to flash

Press and release the reset switch while holding down the user switch.
Then release the user switch.
The hardware will boot with DFU download mode.

Run below command on Linux box to flash program.

    $ make write2stbeemini

Press and release the reset switch to boot with normal mode.

## Original source code

This demo is based on below original source code.

* https://github.com/mblythe86/stm32f3-discovery-basic-template
* http://strawberry-linux.com/pub/mini-demo.zip
