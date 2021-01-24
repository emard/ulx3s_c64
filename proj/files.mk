VHDL_FILES = \
$(TOP_MODULE_FILE) \
../rtl/ecp5/ecp5pll.vhd \
../rtl/cpu_6510.vhd \
../rtl/t65/T65.vhd \
../rtl/t65/T65_Pack.vhd \
../rtl/t65/T65_ALU.vhd \
../rtl/t65/T65_MCode.vhd \
../rtl/fpga64_buslogic.vhd \
../rtl/bram_true2p_2clk.vhd \
../rtl/dprom_kernal.vhd \
../rtl/roms/rom_kernal_pack.vhd \
../rtl/dprom_chargen.vhd \
../rtl/roms/rom_chargen_pack.vhd \
../rtl/fpga64_keyboard.vhd \
../rtl/video_vicII_656x.vhd \
../rtl/video_2x_scan.vhd \
../rtl/fpga64_rgbcolor.vhd \
../rtl/osd/spi_ram_btn.vhd \
../rtl/osd/spi_osd.vhd \
../rtl/osd/osd_vhd.vhd \
../rtl/dvi/vga.vhd \
../rtl/dvi/vga2dvid.vhd \
../rtl/dvi/tmds_encoder.vhd \
../rtl/spi_display/spi_display.vhd \
../rtl/spi_display/spi_display_init_pack.vhd \
../rtl/spi_display/st7789_init_pack.vhd \
../rtl/sid6581/my_math_pkg.vhd \
../rtl/sid6581/mult_acc.vhd \
../rtl/sid6581/oscillator.vhd \
../rtl/sid6581/sid_filter.vhd \
../rtl/sid6581/Q_table.vhd \
../rtl/sid6581/sid_regs.vhd \
../rtl/sid6581/sid_top.vhd \
../rtl/sid6581/wave_map.vhd \
../rtl/sid6581/sid_ctrl.vhd \
../rtl/sid6581/sid_mixer.vhd \
../rtl/sid6581/adsr_multi.vhd \
../rtl/sid6581/sid_debug_pkg.vhd \
../rtl/sid8580/sid8580_vhd.vhd \
../rtl/dacpwm.vhd \
../rtl/spdif_tx.vhd \

VERILOG_FILES = \
../rtl/ps2.v \
../rtl/mos6526.v \
../rtl/osd/spi_ram_btn_v.v \
../rtl/osd/spi_osd_v.v \
../rtl/osd/osd.v \
../rtl/osd/spirw_slave_v.v \
../rtl/sid8580/sid8580.v \
../rtl/sid8580/sid_filters.v \
../rtl/sid8580/sid_envelope.v \
../rtl/sid8580/sid_voice.v \
../rtl/sid8580/sid_tables.v \

