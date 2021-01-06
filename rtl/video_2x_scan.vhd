--
-- A simulation model of Scramble hardware
-- Copyright (c) MikeJ - Feb 2007
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- You are responsible for any legal issues arising from your use of this code.
--
-- The latest version of this file can be found at: www.fpgaarcade.com
--
-- Email support@fpgaarcade.com
--
-- Revision list
--
-- version 001 initial release
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
--  use ieee.std_logic_arith.all;
  use ieee.numeric_std.all;

entity video_2x_scan is
generic
(
  xsize: integer range 160 to 960 := 256; -- blank: DVI picture size is 2x this value
  ysize: integer range 200 to 600 := 226; -- blank: DVI picture size is 2x this value
  -- x/y center (shifts blank signal relative to rising edge of hsync/vsync signals)
  xcenter: integer := 87; -- increase -> picture moves left
  ycenter: integer := 29; -- increase -> picture moves up
  -- sync pulse width
  hsync_width: integer range 0 to 80 := 15;
  vsync_width: integer range 0 to 25 := 14;
  -- color data bits
  color_bits: integer := 4
);
port
(
	I_COLOR           : in    unsigned(color_bits-1 downto 0);
	I_HSYNC           : in    std_logic;
	I_VSYNC           : in    std_logic;

	O_COLOR           : out   unsigned(color_bits-1 downto 0);
	O_HSYNC           : out   std_logic;
	O_VSYNC           : out   std_logic;
	O_BLANK           : out   std_logic;

	ENA_X2            : in    std_logic;
	ENA               : in    std_logic;
	CLK               : in    std_logic
);
end;

architecture RTL of video_2x_scan is

  signal ram_ena_x2  : std_logic;
  signal ram_ena     : std_logic;
  --
  -- input timing
  --
  signal hsync_in_t1 : std_logic;
  signal vsync_in_t1 : std_logic;
  signal hpos_i      : unsigned(8 downto 0) := (others => '0');    -- input capture postion
  signal hsize_i     : unsigned(8 downto 0);
  signal bank_i      : std_logic;
  signal rgb_in      : unsigned(color_bits-1 downto 0);
  --
  -- output timing
  --
  signal hpos_o      : unsigned(8 downto 0) := (others => '0');
  signal vpos_o      : unsigned(8 downto 0) := (others => '0');
  signal ohs         : std_logic;
  signal ohs_t1      : std_logic;
  signal ovs         : std_logic;
  signal ovs_t1      : std_logic;
  signal bank_o      : std_logic;
  --
  --signal vs_cnt      : unsigned(3 downto 0);
  signal rgb_out     : unsigned(color_bits-1 downto 0);
  
  signal vblank, hblank : std_logic;

  signal i_rising_h, i_rising_v, o_rising_h, o_rising_v: boolean;
begin

  i_rising_h <= (I_HSYNC = '1') and (hsync_in_t1 = '0');
  i_rising_v <= (I_VSYNC = '1') and (vsync_in_t1 = '0');

  p_input_timing : process
  begin
	wait until rising_edge (CLK);
	if (ENA = '1') then
	  hsync_in_t1 <= I_HSYNC;
	  vsync_in_t1 <= I_VSYNC;

	  if i_rising_v then
		bank_i <= '0';
	  elsif i_rising_h then
		bank_i <= not bank_i;
	  end if;

	  if i_rising_h then
		hpos_i <= (others => '0');
		hsize_i  <= hpos_i;
	  else
		hpos_i <= hpos_i + "1";
	  end if;
	end if;
  end process;

  rgb_in <= I_COLOR;

  u_ram: entity work.bram_true2p_2clk
    generic map
    (
      dual_port   => true,
      pass_thru_a => false,
      pass_thru_b => false,
      data_width  => rgb_in'length,
      addr_width  => 10
    )
    port map
    (
      -- write side, (delayed 1 clock)
      clk_a              => CLK,
      data_in_a          => rgb_in,
      data_out_a         => open,
      addr_a(9)          => bank_i,
      addr_a(8 downto 0) => hpos_i(8 downto 0),
      we_a               => ENA,
      -- read side
      clk_b              => CLK,
      data_in_b          => (others => '0'),
      data_out_b         => rgb_out,
      addr_b(9)          => bank_o,
      addr_b(8 downto 0) => hpos_o(8 downto 0),
      we_b               => '0'
    );

  o_rising_h <= (ohs = '1') and (ohs_t1 = '0');
  o_rising_v <= (ovs = '1') and (ovs_t1 = '0');

  p_output_timing : process
  begin
	wait until rising_edge (CLK);
	if  (ENA_X2 = '1') then

	  if o_rising_h or (hpos_o = hsize_i) then
		hpos_o <= (others => '0');
	  else
		hpos_o <= hpos_o + "1";
	  end if;

	  if o_rising_v then -- rising_v
		bank_o <= '1';
		vpos_o <= (others => '0');
	  elsif o_rising_h then
		bank_o <= not bank_o;
		vpos_o <= vpos_o + "1";
	  end if;

	  ohs <= I_HSYNC; -- reg on clk_12
	  ohs_t1 <= ohs;

	  ovs <= I_VSYNC; -- reg on clk_12
	  ovs_t1 <= ovs;
	end if;
  end process;

  p_op : process
  begin
	wait until rising_edge (CLK);
	if (ENA_X2 = '1') then
	  if hpos_o = to_unsigned(0,9) then
	    O_HSYNC <= '1';
	  elsif hpos_o = to_unsigned(hsync_width,9) then
	    O_HSYNC <= '0';
	  end if;

	  if hpos_o = to_unsigned(xcenter,9) then
	    hblank <= '0';
	  elsif hpos_o = to_unsigned(xcenter+xsize,9) then
	    hblank <= '1';
	  end if;

	  if vpos_o = to_unsigned(0,9) then
	    O_VSYNC <= '1';
	  elsif vpos_o = to_unsigned(vsync_width,9) then
	    O_VSYNC <= '0';
	  end if;

	  if vpos_o = to_unsigned(ycenter,9) then
	    vblank <= '0';
	  elsif vpos_o = to_unsigned(ycenter+ysize,9) then
	    vblank <= '1';
	  end if;
	  
	  O_BLANK <= hblank or vblank;
	  O_COLOR <= rgb_out;
	end if;
  end process;

end architecture RTL;
