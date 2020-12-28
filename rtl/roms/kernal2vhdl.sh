#!/bin/sh
cat rom_kernal_head.vhd
cat $* \
| hexdump -v -e '8/1 "x_%02X_, ""\n"' \
| sed -e 's/_/"/g' -e "$ s/,$//"
cat rom_kernal_tail.vhd
