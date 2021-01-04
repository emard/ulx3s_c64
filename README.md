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

# ESP32 OSD loader

Download some PRG files and upload them to ESP32 flash or SD
directory which should contain "c64" string in the path so
the loader will use correct loader (to make it different from
VIC20 which also has PRG files).

To start ESP32 OSD service, type or put in main.py

    import osd

Onboard buttons act as joystick. To change joystick ports:

    osd.ctrl(0)
    osd.ctrl(4)

Press all 4 onboard cursor buttons to open OSD window and select a PRG file,
navigate and press cursor right button at PRG file, it will be uploaded
and started.
