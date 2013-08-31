# Copyright 2011 Adam Green (http://mbed.org/users/AdamGreen/)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
PROJECT=HsTextLCD
GCC4MBED_DIR=../..
LIBS_PREFIX=
LIBS_SUFFIX=
NO_FLOAT_SCANF=1
NO_FLOAT_PRINTF=1
VERBOSE=1
CPLUSPLUS_ENABLE=0

HS_ENABLE = 1
GCFLAGS += -std=gnu99 -ffreestanding -nostdlib -falign-functions=4
GCFLAGS += -Wno-unused-parameter -fno-strict-aliasing -D_GNU_SOURCE
GCFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0 -D_JHC_USE_OWN_STDIO
GCFLAGS += -D_LITTLE_ENDIAN
GCFLAGS += -D_JHC_ARM_STAY_IN_THUMB_MODE
GCFLAGS += -D_JHC_JGC_STACKGROW=16 -D_JHC_JGC_LIMITED_NUM_MEGABLOCK=2
GCFLAGS += -D_JHC_JGC_BLOCK_SHIFT=8 -D_JHC_JGC_MEGABLOCK_SHIFT=13
GCFLAGS += -D_JHC_JGC_GC_STACK_SHIFT=8 -D_JHC_JGC_LIMITED_NUM_GC_STACK=1
GCFLAGS += -D_JHC_JGC_NAIVEGC -D_JHC_JGC_SAVING_MALLOC_HEAP
GCFLAGS += -D_JHC_CONC=_JHC_CONC_NONE

include ../../build/gcc4mbed.mk
