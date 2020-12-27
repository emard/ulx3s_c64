library IEEE;
use IEEE.std_logic_1164.all;

package rom_chargen_pack is
type t_rom_chargen is array(0 to 4095) of std_logic_vector(7 downto 0);
constant rom_chargen : t_rom_chargen :=
(
