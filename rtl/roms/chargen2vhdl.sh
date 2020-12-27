#!/bin/sh
cat rom_chargen_head.vhd
cat chargen_openroms.rom \
| hexdump -v -e '8/1 "x_%02X_, ""\n"' \
| sed -e 's/_/"/g'
cat rom_chargen_tail.vhd
