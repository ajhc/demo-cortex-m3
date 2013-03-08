# Ajhc demo for Cortex-M3 board

First, you have to know there are several boards supported by the demo.

* [stm32f3-discovery](http://www.st.com/web/en/catalog/tools/FM116/SC959/SS1532/PF254044)
* [stbee-mini](http://strawberry-linux.com/catalog/items?code=32105)

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

## If you use stm32f3-discovery

### How to build

    $ sudo apt-get install libsgutils2-dev libusb-1.0-0-dev
    $ git clone git://github.com/texane/stlink.git
    $ cd stlink/
    $ ./autogen.sh
    $ ./configure
    $ make
    $ sudo make install
    $ cd ../stm32f3-discovery/
    $ make

### Writing to flash

In one terminal, start the connection to the board.

    $ sudo st-util

In another terminal, connect to the debugger and flash program.

    $ make gdbwrite

## If you use stbee-mini

### How to build

xxx Should install dfu-util.

    $ cd stbee-mini/
    $ make

### Writing to flash

xxx Explain to boot flash mode.

    $ make write2stbeemini

## Original source code

This demo is based on below original source code.

* https://github.com/mblythe86/stm32f3-discovery-basic-template
* http://strawberry-linux.com/pub/mini-demo.zip
