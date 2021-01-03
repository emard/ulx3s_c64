# ULX3S C64 attempts from mister

Attempt to borrow [C64_MiSTer](https://github.com/MiSTer-devel/C64_MiSTer)
code and compile it with opensource tools.

Boot picture with "READY." prompt and blinking cursor appears
with reported resolution 1616x300@51.6Hz on my lenovo monitor.

The video signal is not likely to be compatible with every
monitor but some should work.

This repo comes with free generic ROMs from
[MEGA65](https://github.com/MEGA65/open-roms/tree/master/bin) project
and can load some games like
[Gridrunner](www.zimmers.net/anonftp/pub/cbm/c64/games/Llamasoft/Gridrunner.prg),
[Galaga](https://www.planetemu.net/rom/commodore-c64-games-prg/galaga-1982-henrik-wening) or
[Elite](www.zimmers.net/anonftp/pub/cbm/c64/games/Elite-deutsch.prg).
The script "getrom.sh" in "roms" directory will get real stuff for
small recompilation excercise.
Sound (SID6581) works. New SID8580 won't compile.

PS/2 Keyboard works.

ESP32 OSD PRG file loading works.
