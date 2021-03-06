# ULX3S C64 attempts from mister

Attempt to borrow [C64_MiSTer](https://github.com/MiSTer-devel/C64_MiSTer)
code and compile it with opensource tools.

Boot picture with "READY." prompt and blinking cursor appears
with reported resolution 1616x300@51.6Hz on my lenovo monitor.

The video signal is not likely to be compatible with every
monitor but some should work.

This repo comes with free generic ROMs from
[MEGA65](https://github.com/MEGA65/open-roms/tree/master/bin) project.
Free ROM can load some PRG games like
[Gridrunner](http://www.zimmers.net/anonftp/pub/cbm/c64/games/Llamasoft/Gridrunner.prg),
[Galaga](https://www.planetemu.net/rom/commodore-c64-games-prg/galaga-1982-henrik-wening) or
[Elite-deutsch](http://www.zimmers.net/anonftp/pub/cbm/c64/games/Elite-deutsch.prg).
The script "getrom.sh" in "roms" directory will get real stuff for
small recompilation excercise.
Sound (SID6581) works. New SID8580 won't compile.

PS/2 Keyboard works.

# Compiling

From linux

    make clean
    # to program FPGA, will not persist after power off
    make prog
    # to program flash, will persist after power off
    make flash

With doublescan enabled in top module, there are difficulties
with compiling for 12F. Trellis and Diamond 3.7 won't compile for 12F
Diamond 3.11 will compile for 25F.

# ESP32 OSD loader

Download some PRG files and upload them to ESP32 flash or SD
directory which should contain "c64" string in the path so
the loader will use correct loader (to make it different from
VIC20 which also has PRG files).

To start ESP32 OSD service, type or put in main.py

    import osd

Onboard buttons act as joystick. To change joystick port 1/2 press BTN2.
LED D0 inidicates joystick port:

    port 1 : LED D0 OFF
    port 2 : LED D0 ON

Press all 4 onboard cursor buttons to open OSD window and select a PRG file,
navigate and press cursor right button at PRG file, it will be uploaded
and started.

For debugging RAM content, ESP32:

    osd.poke(9999,"ABCD")
    osd.peek(9999,4)
    ABCD

On C64 (65 = ASCII "A"):

    ?PEEK(9999)
    65

# Problems

[Raid on Bungeling Bay](https://www.planetemu.net/rom/commodore-c64-games-prg/raid-on-bungeling-bay-1984-broderbund-h-abc)
and few others probably, don't work.
It shows ABC color strips screen, after pressing space 
screen is filled with "@" and nothing more happens.
Mister runs it correctly.

