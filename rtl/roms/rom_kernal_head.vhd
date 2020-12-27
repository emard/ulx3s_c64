library IEEE;
use IEEE.std_logic_1164.all;

package rom_kernal_pack is
type t_rom_kernal is array(0 to 16383) of std_logic_vector(7 downto 0);
constant rom_kernal : t_rom_kernal :=
(
