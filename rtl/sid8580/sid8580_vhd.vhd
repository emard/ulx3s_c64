-- VHDL wrapper for sid8580.v

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity sid8580_vhd is
  port
  (
    reset           : in    std_logic;

    clk             : in    std_logic;
    ce_1m           : in    std_logic;

    we              : in    std_logic;
    addr            : in    std_logic_vector(4 downto 0);
    data_in         : in    std_logic_vector(7 downto 0);
    data_out        : out   std_logic_vector(7 downto 0);

    pot_x           : in    std_logic_vector(7 downto 0);
    pot_y           : in    std_logic_vector(7 downto 0);

    extfilter_en    : in    std_logic;
    audio_data      : out   std_logic_vector(17 downto 0)
  );
end;

architecture syn of sid8580_vhd is
  component sid8580 -- verilog name and its parameters
  port
  (
    reset           : in    std_logic;

    clk             : in    std_logic;
    ce_1m           : in    std_logic;

    we              : in    std_logic;
    addr            : in    std_logic_vector(4 downto 0);
    data_in         : in    std_logic_vector(7 downto 0);
    data_out        : out   std_logic_vector(7 downto 0);

    pot_x           : in    std_logic_vector(7 downto 0);
    pot_y           : in    std_logic_vector(7 downto 0);

    extfilter_en    : in    std_logic;
    audio_data      : out   std_logic_vector(17 downto 0)
  );
  end component;

begin
  sid8580_inst: sid8580
  port map
  (
    reset           => reset,

    clk             => clk,
    ce_1m           => ce_1m,

    we              => we,
    addr            => addr,
    data_in         => data_in,
    data_out        => data_out,

    pot_x           => pot_x,
    pot_y           => pot_y,

    extfilter_en    => extfilter_en,
    audio_data      => audio_data
  );
end syn;
