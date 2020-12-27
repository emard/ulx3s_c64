-- -----------------------------------------------------------------------
--
--                                 FPGA 64
--
--     A fully functional commodore 64 implementation in a single FPGA
--
-- -----------------------------------------------------------------------
-- Copyright 2005-2008 by Peter Wendrich (pwsoft@syntiac.com)
-- http://www.syntiac.com/fpga64.html
-- -----------------------------------------------------------------------
--
-- C64 palette index to 24 bit RGB color
-- 
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

-- -----------------------------------------------------------------------

entity fpga64_rgbcolor is
	port (
		index: in unsigned(3 downto 0);
		r: out unsigned(7 downto 0);
		g: out unsigned(7 downto 0);
		b: out unsigned(7 downto 0)
	);
end fpga64_rgbcolor;

-- -----------------------------------------------------------------------

architecture Behavioral of fpga64_rgbcolor is
type t64_colors is array(0 to 15) of unsigned(23 downto 0);
constant c64_colors : t64_colors :=
(
x"000000", -- 0
x"FFFFFF", -- 1
x"68372B", -- 2
x"70A4B2", -- 3
x"6F3D86", -- 4
x"588D43", -- 5
x"352879", -- 6
x"B8C76F", -- 7
x"6F4F25", -- 8
x"433900", -- 9
x"9A6759", -- A
x"444444", -- B
x"6C6C6C", -- C
x"9AD284", -- D
x"6C5EB5", -- E
x"959595"  -- F
);
begin
	r <= c64_colors(to_integer(index))(23 downto 16);
	g <= c64_colors(to_integer(index))(15 downto  8);
	b <= c64_colors(to_integer(index))( 7 downto  0);
end Behavioral;
