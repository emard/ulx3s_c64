#!/bin/sh -e
wget ftp://www.zimmers.net/pub/cbm/firmware/computers/c64/characters.901225-01.bin
./chargen2vhdl.sh characters.901225-01.bin > rom_chargen_pack_orig2.vhd
wget ftp://www.zimmers.net/pub/cbm/firmware/computers/c64/64c.251913-01.bin
./kernal2vhdl.sh 64c.251913-01.bin > rom_kernal_pack_orig2.vhd
