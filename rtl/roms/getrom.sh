#!/bin/sh -e

# 4096 byte file
ROM_CHAR=characters.901225-01.bin
# 16384 byte file
ROM_KERNAL=64c.251913-01.bin

wget -O $ROM_CHAR ftp://www.zimmers.net/pub/cbm/firmware/computers/c64/$ROM_CHAR
./chargen2vhdl.sh $ROM_CHAR > rom_chargen_pack_orig.vhd
wget -O $ROM_KERNAL ftp://www.zimmers.net/pub/cbm/firmware/computers/c64/$ROM_KERNAL
./kernal2vhdl.sh $ROM_KERNAL > rom_kernal_pack_orig.vhd
