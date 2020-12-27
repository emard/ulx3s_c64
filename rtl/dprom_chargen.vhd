-- TODO: package with initial ROM content

library ieee;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

use work.rom_chargen_pack.all;

entity dprom_chargen is

	generic 
	(
		ADDR_WIDTH : natural := 12;
		DATA_WIDTH : natural := 8
	);

	port 
	(
		wrclock   : in  std_logic;
		wraddress : in  std_logic_vector((ADDR_WIDTH - 1) downto 0) := (others => '0');
		data	  : in  std_logic_vector((DATA_WIDTH - 1) downto 0) := (others => '0');
		wren      : in  std_logic := '0';

		rdclock   : in  std_logic;
		rdaddress : in  std_logic_vector((ADDR_WIDTH - 1) downto 0);
		q         : out std_logic_vector((DATA_WIDTH - 1) downto 0);
		cs        : in  std_logic := '1'
	);

end;

architecture rtl of dprom_chargen is

	--subtype word_t is std_logic_vector(q'range);
	--type memory_t is array(0 to 2**rdaddress'length-1) of word_t;

	shared variable ram : t_rom_chargen := rom_chargen;

	--attribute ram_init_file : string;
	--attribute ram_init_file of ram : variable is INIT_FILE;

	signal q0 : std_logic_vector(q'range);

begin

	q<= q0 when cs = '1' else (others => '1');

	-- WR Port
	process(wrclock) begin
		if(rising_edge(wrclock)) then 
			if(wren = '1') then
				ram(to_integer(unsigned(wraddress))) := data;
			end if;
		end if;
	end process;

	-- RD Port
	process(rdclock) begin
		if(rising_edge(rdclock)) then 
			 q0 <= ram(to_integer(unsigned(rdaddress)));
		end if;
	end process;

end rtl;
