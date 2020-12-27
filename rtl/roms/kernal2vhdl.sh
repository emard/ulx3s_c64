#!/bin/sh
cat rom_kernal_head.vhd
cat basic_generic.rom kernal_generic.rom \
| hexdump -v -e '8/1 "x_%02X_, ""\n"' \
| sed -e 's/_/"/g'
cat rom_kernal_tail.vhd
