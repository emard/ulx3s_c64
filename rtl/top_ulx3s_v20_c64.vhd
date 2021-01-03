-- (c)EMARD
-- License=BSD

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.numeric_std.ALL;

-- for diamond (not for opensource tools yosys/trellis)
--library ecp5u;
--use ecp5u.components.all;

entity top_ulx3s_v20_c64 is
  generic
  (
    sid_ver: std_logic := '0'; -- 0:6581, 1:8580
    victest: boolean := false -- true: fast compile VIC color test, false: normal C64 with CPU
  );
  port
  (
    clk_25mhz: in std_logic;  -- main clock input from 25MHz clock source

    -- Onboard blinky
    led: out std_logic_vector(7 downto 0);
    btn: in std_logic_vector(6 downto 0);

    usb_fpga_pu_dp, usb_fpga_pu_dn: out std_logic := '1';
    usb_fpga_bd_dp, usb_fpga_bd_dn: in std_logic;

    -- GPIO (some are shared with wifi and adc)
    gp, gn: in std_logic_vector(27 downto 0);

    ftdi_txd: in std_logic;
    ftdi_rxd: out std_logic;
    ftdi_nrts: in std_logic;
    ftdi_ndtr: in std_logic;

    -- WiFi additional signaling
    wifi_txd: in std_logic;
    wifi_rxd: out std_logic;
    wifi_gpio0  : out std_logic;
    wifi_gpio5  : in std_logic;
    wifi_gpio16 : inout std_logic;
    --wifi_gpio17 : inout std_logic;
    
    -- Audio
    audio_l, audio_r, audio_v: out std_logic_vector(3 downto 0);

    -- Digital Video (differential outputs)
    gpdi_dp: out std_logic_vector(3 downto 0)
  );
end;

architecture Behavioral of top_ulx3s_v20_c64 is

  signal clocks: std_logic_vector(3 downto 0);
  signal clk_pixel, clk_shift: std_logic;
  signal dvid_red, dvid_green, dvid_blue, dvid_clock: std_logic_vector(1 downto 0);
  
  component ODDRX1F
    port (D0, D1, SCLK, RST: in std_logic; Q: out std_logic);
  end component;

----------------------------------------------------------
constant resetCycles : integer := 4095;
signal clk32: std_logic;
signal clocks_c64: std_logic_vector(3 downto 0);

-- after OSD module 
signal osd_vga_r, osd_vga_g, osd_vga_b: std_logic_vector(7 downto 0);
signal osd_vga_hsync, osd_vga_vsync, osd_vga_blank: std_logic;
-- invert CS to get CSN
signal spi_irq, spi_csn, spi_miso, spi_mosi, spi_sck: std_logic;
signal spi_ram_wr, spi_ram_rd: std_logic;
signal spi_ram_wr_data: std_logic_vector(7 downto 0);
signal spi_ram_addr: std_logic_vector(31 downto 0); -- MSB for ROMs
signal R_cpu_control: std_logic_vector(7 downto 0);
signal R_btn_joy: std_logic_vector(btn'range);

signal bram_spi_cs, bram_spi_we, bram_mux_we: std_logic;
signal bram_mux_addr: unsigned(15 downto 0);
signal bram_mux_wr_data: unsigned(7 downto 0);

-------------------------------------
-- System state machine
type sysCycleDef is (
	CYCLE_IDLE0, CYCLE_IDLE1, CYCLE_IDLE2, CYCLE_IDLE3,
	CYCLE_IDLE4, CYCLE_IDLE5, CYCLE_IDLE6, CYCLE_IDLE7,
	CYCLE_IEC0,  CYCLE_IEC1,  CYCLE_IEC2,  CYCLE_IEC3,
	CYCLE_VIC0,  CYCLE_VIC1,  CYCLE_VIC2,  CYCLE_VIC3,
	CYCLE_CPU0,  CYCLE_CPU1,  CYCLE_CPU2,  CYCLE_CPU3,
	CYCLE_CPU4,  CYCLE_CPU5,  CYCLE_CPU6,  CYCLE_CPU7,
	CYCLE_CPU8,  CYCLE_CPU9,  CYCLE_CPUA,  CYCLE_CPUB,
	CYCLE_CPUC,  CYCLE_CPUD,  CYCLE_CPUE,  CYCLE_CPUF
);

signal sysCycle     : unsigned(4 downto 0) := (others => '0');
signal phi0_cpu     : std_logic;
signal cpuHasBus    : std_logic;

signal baLoc        : std_logic;
signal irqLoc       : std_logic;
signal nmiLoc       : std_logic;
signal aec          : std_logic;

signal enableCpu    : std_logic;
signal enableVic    : std_logic;
signal enablePixel  : std_logic;

signal irq_cia1     : std_logic;
signal irq_cia2     : std_logic;
signal irq_vic      : std_logic;

signal ps2_key      : std_logic_vector(10 downto 0);

signal systemWe     : std_logic;
signal pulseWrRam   : std_logic;
signal colorWe      : std_logic;
signal systemAddr   : unsigned(15 downto 0);
signal ramDataReg   : unsigned(7 downto 0);

-- external memory
signal ramAddr     : unsigned(15 downto 0);
signal ramDataIn   : unsigned(7 downto 0);
signal ramDataOut  : unsigned(7 downto 0);

signal ramCE       : std_logic;
signal ramWe       : std_logic;
signal ramWeCe     : std_logic;

signal io_cycle    : std_logic;
signal idle        : std_logic;

signal cs_vic       : std_logic;
signal cs_sid       : std_logic;
signal cs_color     : std_logic;
signal cs_cia1      : std_logic;
signal cs_cia2      : std_logic;
signal cs_ram       : std_logic;
signal cs_ioE       : std_logic;
signal cs_ioF       : std_logic;
signal cs_romL      : std_logic;
signal cs_romH      : std_logic;
signal cs_UMAXromH  : std_logic;							-- romH VIC II read flag
signal cpuWe        : std_logic;
signal cpuAddr      : unsigned(15 downto 0);
signal cpuDi        : unsigned(7 downto 0);
signal cpuDo        : unsigned(7 downto 0);
signal cpuIO        : unsigned(7 downto 0);
signal cpudiIO      : unsigned(7 downto 0);

signal reset        : std_logic := '1';
signal reset_cnt    : integer range 0 to resetCycles := 0;

-- CIA signals
signal enableCia_p  : std_logic;
signal enableCia_n  : std_logic;
signal cia1Do       : unsigned(7 downto 0);
signal cia2Do       : unsigned(7 downto 0);
signal cia1_pai     : unsigned(7 downto 0);
signal cia1_pao     : unsigned(7 downto 0);
signal cia1_pbi     : unsigned(7 downto 0);
signal cia1_pbo     : unsigned(7 downto 0);
signal cia2_pai     : unsigned(7 downto 0);
signal cia2_pao     : unsigned(7 downto 0);
signal cia2_pbi     : unsigned(7 downto 0);
signal cia2_pbo     : unsigned(7 downto 0);

signal todclk       : std_logic;
signal toddiv       : std_logic_vector(19 downto 0);
signal toddiv3      : std_logic_vector(1 downto 0);

-- video
constant ntscMode   : std_logic := '0';
signal vicColorIndex: unsigned(3 downto 0);
signal vicHSync     : std_logic;
signal vicVSync     : std_logic;
signal vicBlank     : std_logic;
signal vicBus       : unsigned(7 downto 0);
signal vicDi        : unsigned(7 downto 0);
signal vicDiAec     : unsigned(7 downto 0);
signal vicAddr      : unsigned(15 downto 0);
signal vicData      : unsigned(7 downto 0);
signal lastVicDi    : unsigned(7 downto 0);
signal vicAddr1514  : unsigned(1 downto 0);
signal colorQ       : unsigned(3 downto 0);
signal colorData    : unsigned(3 downto 0);
signal colorDataAec : unsigned(3 downto 0);

-- VGA/SCART interface
signal	vic_r       : unsigned(7 downto 0);
signal	vic_g       : unsigned(7 downto 0);
signal	vic_b       : unsigned(7 downto 0);

signal  vicydummy   : unsigned(15 downto 0); -- debug VIC filler

-- SID signals
signal sid_do       : std_logic_vector(7 downto 0);
signal sid_do6581   : std_logic_vector(7 downto 0);
signal sid_do8580   : std_logic_vector(7 downto 0);
signal sid_we       : std_logic;
signal sid_sel_int  : std_logic;
signal sid_wren     : std_logic;
signal audio_6581   : signed(17 downto 0);
signal pot_x1       : std_logic_vector(7 downto 0);
signal pot_y1       : std_logic_vector(7 downto 0);
signal pot_x2       : std_logic_vector(7 downto 0);
signal pot_y2       : std_logic_vector(7 downto 0);
signal sid_pot_x    : std_logic_vector(7 downto 0);
signal sid_pot_y    : std_logic_vector(7 downto 0);
signal audio_8580   : std_logic_vector(17 downto 0);
signal clk_1MHz     : std_logic_vector(31 downto 0);


-- "external" connections, in this project internal
-- cartridge port
signal	game        : std_logic := '1';
signal	exrom       : std_logic := '1';
signal	ioE_rom     : std_logic := '1';
signal	ioF_rom     : std_logic := '1';
signal	max_ram     : std_logic := '1';
signal	irq_n       : std_logic := '1';
signal	nmi_n       : std_logic := '1';
signal	nmi_ack     : std_logic := '1';
signal	ba          : std_logic := '1';
signal	romL	    : std_logic := '1'; -- cart signals LCA
signal	romH	    : std_logic := '1'; -- cart signals LCA
signal	UMAXromH    : std_logic := '1'; -- cart signals LCA
signal	IOE	    : std_logic := '1'; -- cart signals LCA
signal	IOF	    : std_logic := '1'; -- cart signals LCA
signal	CPU_hasbus  : std_logic := '1'; -- CPU has the bus STROBE
signal	freeze_key  : std_logic;

signal	ioF_ext     : std_logic;
signal	ioE_ext     : std_logic;
signal	io_data     : unsigned(7 downto 0);

-- joystick interface
signal	joyA        : std_logic_vector(6 downto 0) := (others => '0');
signal	joyB        : std_logic_vector(6 downto 0) := (others => '0');
signal	joyC        : std_logic_vector(6 downto 0) := (others => '0');
signal	joyD        : std_logic_vector(6 downto 0) := (others => '0');
signal	pot1        : std_logic_vector(7 downto 0);
signal	pot2        : std_logic_vector(7 downto 0);
signal	pot3        : std_logic_vector(7 downto 0);
signal	pot4        : std_logic_vector(7 downto 0);

-- Connector to the SID
signal	audio_data  : std_logic_vector(17 downto 0);
signal	extfilter_en: std_logic;
signal	sid_we_ext  : std_logic;
signal	sid_mode    : std_logic_vector(1 downto 0);

-- IEC
signal	iec_data_o  : std_logic;
signal	iec_data_i  : std_logic;
signal	iec_clk_o   : std_logic;
signal	iec_clk_i   : std_logic;
signal	iec_atn_o   : std_logic;

-- external (SPI) ROM update
signal	c64rom_addr : std_logic_vector(13 downto 0);
signal	c64rom_data : std_logic_vector(7 downto 0);
signal	c64rom_wr   : std_logic := '0';

-- cassette
signal	cass_motor  : std_logic;
signal	cass_write  : std_logic;
signal	cass_sense  : std_logic;
signal	cass_in     : std_logic;

signal	uart_enable : std_logic;

signal	uart_txd    : std_logic; -- CIA2, PortA(2) 
signal	uart_rts    : std_logic; -- CIA2, PortB(1)
signal	uart_dtr    : std_logic; -- CIA2, PortB(2)
signal	uart_ri_out : std_logic; -- CIA2, PortB(3)
signal	uart_dcd_out: std_logic; -- CIA2, PortB(4)

signal	uart_rxd    : std_logic; -- CIA2, PortB(0)
signal	uart_ri_in  : std_logic; -- CIA2, PortB(3)
signal	uart_dcd_in : std_logic; -- CIA2, PortB(4)
signal	uart_cts    : std_logic; -- CIA2, PortB(6)
signal	uart_dsr    : std_logic; -- CIA2, PortB(7)

-- verilog component
component mos6526
	port (
		clk           : in  std_logic;
		mode          : in  std_logic := '0'; -- 0 - 6526 "old", 1 - 8521 "new"
		phi2_p        : in  std_logic;
		phi2_n        : in  std_logic;
		res_n         : in  std_logic;
		cs_n          : in  std_logic;
		rw            : in  std_logic; -- '1' - read, '0' - write
		rs            : in  unsigned(3 downto 0);
		db_in         : in  unsigned(7 downto 0);
		db_out        : out unsigned(7 downto 0);
		pa_in         : in  unsigned(7 downto 0);
		pa_out        : out unsigned(7 downto 0);
		pb_in         : in  unsigned(7 downto 0);
		pb_out        : out unsigned(7 downto 0);
		flag_n        : in  std_logic;
		pc_n          : out std_logic;
		tod           : in  std_logic;
		sp_in         : in  std_logic;
		sp_out        : out std_logic;
		cnt_in        : in  std_logic;
		cnt_out       : out std_logic;
		irq_n         : out std_logic
	);
end component; 

component ps2
	port (
		clk           : in  std_logic;
		ps2_clk       : in  std_logic;
		ps2_data      : in  std_logic;
		ps2_key       : out std_logic_vector(10 downto 0)
	);
end component; 

----------------------------------------------------------
begin
  -- esp32 micropython console
  wifi_rxd <= ftdi_txd;
  ftdi_rxd <= wifi_txd;

  clk_c64_pll: entity work.ecp5pll
  generic map
  (
      in_Hz => natural(25.0e6),
    out0_Hz => natural(32.5e6)*5,
    out1_Hz => natural(32.5e6)
  )
  port map
  (
    clk_i => clk_25MHz,
    clk_o => clocks_c64
  );
  clk_shift <= clocks_c64(0);
  clk_pixel <= clocks_c64(1);
  clk32     <= clk_pixel; -- 32.5 MHz

-- -----------------------------------------------------------------------
-- System state machine, controls bus accesses
-- and triggers enables of other components
-- -----------------------------------------------------------------------
process(clk32)
begin
  if rising_edge(clk32) then
    --if sysCycle = to_unsigned(31,sysCycle'length) then
    --  syscycle <= to_unsigned(0,sysCycle'length); -- reset to idle cycle (0-2) to fit 50Hz refresh
    --else
      sysCycle <= sysCycle+1;
    --end if;
  end if;
end process;

-- PHI0/2-clock emulation
process(clk32)
begin
	if rising_edge(clk32) then
		if sysCycle = to_unsigned(15,sysCycle'length) then
			phi0_cpu <= '1';
			if baLoc = '1' or cpuWe = '1' then
				cpuHasBus <= '1';
			end if;
		end if;
		if sysCycle = to_unsigned(31,sysCycle'length) then
			phi0_cpu <= '0';
			cpuHasBus <= '0';
		end if;
	end if;
end process;

process(clk32)
begin
	if rising_edge(clk32) then
		enableCpu <= '0';
		enableVic <= '0';
		enableCia_n <= '0';
		enableCia_p <= '0';

		if sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_VIC2), sysCycle'length) then -- CYCLE_VIC2
			enableVic <= '1';
		elsif sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_CPUC), sysCycle'length) then -- CYCLE_CPUC
			enableCia_n <= '1';
		elsif sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_CPUE), sysCycle'length) then -- CYCLE_CPUE
			enableVic <= '1';
			enableCpu <= not R_cpu_control(1); -- halt
		elsif sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_CPUF), sysCycle'length) then -- CYCLE_CPUF
			enableCia_p <= '1';
		end if;
	end if;
end process;

-- Pixel timing
process(clk32)
begin
	if rising_edge(clk32) then
		enablePixel <= '0';
		if sysCycle(1 downto 0) = "10" then
			enablePixel <= '1';
                else
                        enablePixel <= '0';
		end if;
	end if;
end process;

-- -----------------------------------------------------------------------
-- Reset button
-- -----------------------------------------------------------------------
calcReset: process(clk32)
begin
	if rising_edge(clk32) then
		if sysCycle = to_unsigned(31,sysCycle'length) then
			if reset_cnt = resetCycles then
				reset <= '0';
			else
				reset <= '1';
				reset_cnt <= reset_cnt + 1;
			end if;
		end if;
		if btn(0) = '0' or R_cpu_control(0) = '1' then
			reset_cnt <= 0;
		end if;
	end if;
end process;

-- -----------------------------------------------------------------------
-- Keyboard
-- -----------------------------------------------------------------------
ps2recv: ps2
port map (
	clk      => clk32,
	ps2_clk  => usb_fpga_bd_dp,
	ps2_data => usb_fpga_bd_dn,
	ps2_key  => ps2_key
);

joyA <= '0' & btn(2) & btn(1) & btn(6) & btn(5) & btn(4) & btn(3) when R_cpu_control(2)='0' else (others => '0');
joyB <= '0' & btn(2) & btn(1) & btn(6) & btn(5) & btn(4) & btn(3) when R_cpu_control(2)='1' else (others => '0');

Keyboard: entity work.fpga64_keyboard
port map (
	clk => clk32,
	ps2_key => ps2_key,

	joyA => not unsigned(joyA(4 downto 0)),
	joyB => not unsigned(joyB(4 downto 0)),
	pai => cia1_pao,
	pbi => cia1_pbo,
	pao => cia1_pai,
	pbo => cia1_pbi,

	restore_key => freeze_key, -- freeze_key not connected to c64
	backwardsReadingEnabled => '1'
);

-- -----------------------------------------------------------------------
-- Local signal to outside world
-- -----------------------------------------------------------------------
ba <= baLoc;

io_cycle <= '1' when (sysCycle >= to_unsigned(sysCycleDef'pos(CYCLE_IDLE0),sysCycle'length)) and (sysCycle <= to_unsigned(sysCycleDef'pos(CYCLE_IEC3),sysCycle'length))
       else '0';

idle <= '1' when (sysCycle >= to_unsigned(sysCycleDef'pos(CYCLE_IDLE4),sysCycle'length)) and (sysCycle <= to_unsigned(sysCycleDef'pos(CYCLE_IDLE7),sysCycle'length))
   else '0';

iec_data_o <= not cia2_pao(5);
iec_clk_o <= not cia2_pao(4);
iec_atn_o <= not cia2_pao(3);
ramDataOut <= "00" & cia2_pao(5 downto 3) & "000" when sysCycle >= to_unsigned(sysCycleDef'pos(CYCLE_IEC0),sysCycle'length) and sysCycle <= to_unsigned(sysCycleDef'pos(CYCLE_IEC3),sysCycle'length) else cpuDo;
ramAddr <= systemAddr;
ramWe <= '0' when sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_IEC2),sysCycle'length) or sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_IEC3),sysCycle'length) else not systemWe;
-- CPU2...CPUE or VIC0..VIC3
ramCE <= '0' when ((sysCycle >= to_unsigned(sysCycleDef'pos(CYCLE_CPU2),sysCycle'length) and sysCycle <= to_unsigned(sysCycleDef'pos(CYCLE_CPUE),sysCycle'length))
               or  (sysCycle >= to_unsigned(sysCycleDef'pos(CYCLE_VIC0),sysCycle'length) and sysCycle <= to_unsigned(sysCycleDef'pos(CYCLE_VIC3),sysCycle'length)))
              and cs_ram = '1' else '1';
ramWeCE <= (not ramWe) and (not ramCE);

process(clk32)
begin
	if rising_edge(clk32) then
		if sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_CPUD),sysCycle'length)
		or sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_VIC2),sysCycle'length) then
			ramDataReg <= unsigned(ramDataIn);
		end if;
		if sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_VIC3),sysCycle'length) then
			lastVicDi <= vicDi;
		end if;
	end if;
end process;

-- dummy bus traffic generator for VIC to generate picture
dummy: if victest generate
process(clk32)
begin
	if rising_edge(clk32) then
		if sysCycle = to_unsigned(31,sysCycle'length) then
                  -- video addr/data bus traffic gnerator
                  -- use vertical sync to make stable picture
		  if vicVSync then
		    vicydummy <= (others => '0');
		  else
		    vicydummy <= vicydummy + 1;
		  end if;
                  vicDiAec <= vicydummy(15 downto 8);
                  colorDataAec <= vicydummy(13 downto 10);
		  -- write VIC registers to display something
		  cpuAddr <= vicydummy;
		  cpuDo <= vicydummy(7 downto 0);
                  cs_vic <= '1';
                  cpuWe <= '1';
		end if;
	end if;
end process;
end generate;

c64: if not victest generate
-- -----------------------------------------------------------------------
-- 6510 CPU
-- -----------------------------------------------------------------------
cpu: entity work.cpu_6510
port map (
	clk => clk32,
	reset => reset,
	enable => enableCpu,
	nmi_n => nmiLoc,
	nmi_ack => nmi_ack,
	irq_n => irqLoc,
	rdy => baLoc,

	di => cpuDi,
	addr => cpuAddr,
	do => cpuDo,
	we => cpuWe,

	diIO => cpudiIO,
	doIO => cpuIO
);
cpudiIO <= cpuIO(7) & cpuIO(6) & cpuIO(5) & cass_sense & cpuIO(3) & "111";

cass_motor <= cpuIO(5);
cass_write <= cpuIO(3);

-- -----------------------------------------------------------------------
-- Interrupt lines
-- -----------------------------------------------------------------------
irqLoc <= irq_cia1 and irq_vic and irq_n; 
nmiLoc <= irq_cia2 and nmi_n;

bram_spi_cs <= '1' when spi_ram_addr(31 downto 24) = x"00" else '0';
bram_spi_we <= bram_spi_cs and spi_ram_wr;
bram_mux_we <= '1' when bram_spi_we = '1' else ramWeCE;
bram_mux_addr <= unsigned(spi_ram_addr(15 downto 0)) when bram_spi_we = '1' else ramAddr;
bram_mux_wr_data <= unsigned(spi_ram_wr_data) when bram_spi_we = '1' else ramDataOut;

-- 64K RAM (BRAM)
ram64k: entity work.bram_true2p_2clk
generic map
(
	dual_port  => false,
	addr_width => 16,
	data_width => 8
)
port map
(
	clk_a      => clk32,
	--addr_a     => ramAddr,
	--we_a       => ramWeCE,
	--data_in_a  => ramDataOut,
	addr_a     => bram_mux_addr,
	we_a       => bram_mux_we,
	data_in_a  => bram_mux_wr_data,
	data_out_a => ramDataIn
);


process(clk32)
begin
  if rising_edge(clk32) then
     R_btn_joy <= btn;
  end if;
end process;

-- ESP32 -> FPGA
spi_csn <= not wifi_gpio5;
spi_sck <= gn(11); -- wifi_gpio25
spi_mosi <= gp(11); -- wifi_gpio26
-- FPGA -> ESP32
wifi_gpio16 <= spi_miso;
wifi_gpio0 <= not spi_irq; -- wifi_gpio0 IRQ active low

spi_slave_ram_btn: entity work.spi_ram_btn
generic map
(
  c_sclk_capable_pin => 0,
  c_addr_bits => 32
)
port map
(
  clk => clk32,
  csn => spi_csn,
  sclk => spi_sck,
  mosi => spi_mosi,
  miso => spi_miso,
  btn => R_btn_joy,
  irq => spi_irq,
  wr => spi_ram_wr,
  rd => spi_ram_rd,
  addr => spi_ram_addr,
  data_in => std_logic_vector(ramDataIn),
  data_out => spi_ram_wr_data
);

process(clk32)
begin
  if rising_edge(clk32) then
    if spi_ram_addr(31 downto 24) = x"FF" and spi_ram_wr = '1' then
      R_cpu_control <= spi_ram_wr_data;
    end if;
  end if;
end process;

-- -----------------------------------------------------------------------
-- Color RAM
-- -----------------------------------------------------------------------
colorram: entity work.bram_true2p_2clk
generic map (
	dual_port  => false,
	addr_width => 10,
	data_width => 4
)
port map (
	clk_a      => clk32,
	we_a       => colorWe,
	addr_a     => systemAddr(9 downto 0),
	data_in_a  => cpuDo(3 downto 0),
	data_out_a => colorQ,
	clk_b      => '0'
);

process(clk32)
begin
	if rising_edge(clk32) then
		colorWe <= (cs_color and pulseWrRam);
		colorData <= colorQ;
	end if;
end process;

-- -----------------------------------------------------------------------
-- PLA and bus-switches with ROM
-- -----------------------------------------------------------------------
buslogic: entity work.fpga64_buslogic
port map (
	clk => clk32,
	reset => reset,

	cpuHasBus => cpuHasBus,
	aec => aec,

	bankSwitch => cpuIO(2 downto 0),

	game => game,
	exrom => exrom,
	ioE_rom => ioE_rom,
	ioF_rom => ioF_rom,
	max_ram => max_ram,

	ramData => ramDataReg,

	ioF_ext => ioF_ext,
	ioE_ext => ioE_ext,
	io_data => io_data,

	cpuWe => cpuWe,
	cpuAddr => cpuAddr,
	cpuData => cpuDo,
	vicAddr => vicAddr,
	vicData => vicData,
	sidData => unsigned(sid_do),
	colorData => colorData,
	cia1Data => cia1Do,
	cia2Data => cia2Do,
	lastVicData => lastVicDi,

	systemWe => systemWe,
	systemAddr => systemAddr,
	dataToCpu => cpuDi,
	dataToVic => vicDi,

	cs_vic => cs_vic,
	cs_sid => cs_sid,
	cs_color => cs_color,
	cs_cia1 => cs_cia1,
	cs_cia2 => cs_cia2,
	cs_ram => cs_ram,
	cs_ioE => cs_ioE,
	cs_ioF => cs_ioF,
	cs_romL => cs_romL,
	cs_romH => cs_romH,
	cs_UMAXromH => cs_UMAXromH,

	c64rom_addr => c64rom_addr,
	c64rom_data => c64rom_data,
	c64rom_wr => c64rom_wr
);
end generate;

process(clk32)
begin
	if rising_edge(clk32) then
		pulseWrRam <= '0';
		if cpuWe = '1' then
			if sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_CPUC),sysCycle'length) then
				pulseWrRam <= '1';
			end if;
		end if;
	end if;
end process;

-- -----------------------------------------------------------------------
-- CIAs
-- -----------------------------------------------------------------------
cia1: mos6526
port map (
	clk => clk32,
	phi2_p => enableCia_p,
	phi2_n => enableCia_n,
	res_n => not reset,
	cs_n => not cs_cia1,
	rw => not cpuWe,

	rs => cpuAddr(3 downto 0),
	db_in => cpuDo,
	db_out => cia1Do,

	pa_in => cia1_pai,
	pa_out => cia1_pao,
	pb_in => cia1_pbi,
	pb_out => cia1_pbo,

	flag_n => cass_in,
	sp_in => '1',
	cnt_in => '1',

	tod => vicVSync, -- FIXME not exactly 50Hz

	irq_n => irq_cia1
);

cia2: mos6526
port map (
	clk => clk32,
	phi2_p => enableCia_p,
	phi2_n => enableCia_n,
	res_n => not reset,
	cs_n => not cs_cia2,
	rw => not cpuWe,

	rs => cpuAddr(3 downto 0),
	db_in => cpuDo,
	db_out => cia2Do,

	pa_in => cia2_pai,
	pa_out => cia2_pao,
	pb_in => cia2_pbi,
	pb_out => cia2_pbo,

	-- Looks like most of the old terminal programs use the FLAG_N input (and to PB0) on CIA2 to
	-- trigger an interrupt on the falling edge of the RXD input.
	-- (and they don't use the "SP" pin for some reason?) ElectronAsh.
	flag_n => uart_rxd,
	
	sp_in => uart_rxd,	-- Hooking up to the SP pin anyway, ready for the "UP9600" style serial.
	cnt_in => '1',

	tod => vicVSync, -- FIXME not exactly 50Hz

	irq_n => irq_cia2
);

-- generate TOD clock from stable 32 MHz
-- Can we simply use vicVSync?
process(clk32, reset)
begin
	if reset = '1' then
		todclk <= '0';
		toddiv <= (others => '0');
	elsif rising_edge(clk32) then
		toddiv <= toddiv + 1;
		if (ntscMode = '1' and toddiv = 27082 and toddiv3 = "00") or
			(ntscMode = '1' and toddiv = 27083 and toddiv3 /= "00") or
			toddiv = 324999 then
			toddiv <= (others => '0');
			todclk <= not todclk;
			toddiv3 <= toddiv3 + 1;
			if toddiv3 = "10" then toddiv3 <= "00"; end if;
		end if;
	end if;
end process;

--serialBus
serialBus: process(clk32)
begin
	if rising_edge(clk32) then
		if sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_IEC1),sysCycle'length) then
			cia2_pai(7) <= iec_data_i and not cia2_pao(5);
			cia2_pai(6) <= iec_clk_i and not cia2_pao(4);
		end if;
	end if;
end process;

cia2_pai(5 downto 0) <= cia2_pao(5 downto 0);

process(joyC, joyD, cia2_pbo, uart_rxd, uart_ri_in, uart_dcd_in, uart_cts, uart_dsr, uart_enable)
begin
	if uart_enable = '1' then
		cia2_pbi(0) <= uart_rxd;
		cia2_pbi(1) <= '1';
		cia2_pbi(2) <= '1';
		cia2_pbi(3) <= uart_ri_in;
		cia2_pbi(4) <= uart_dcd_in;
		cia2_pbi(5) <= '1';
		cia2_pbi(6) <= uart_cts;
		cia2_pbi(7) <= uart_dsr;
	else
		if cia2_pbo(7) = '1' then
			cia2_pbi(3 downto 0) <= not unsigned(joyC(3 downto 0));
		else
			cia2_pbi(3 downto 0) <= not unsigned(joyD(3 downto 0));
		end if;
		if joyC(6 downto 4) /= "000" then
			cia2_pbi(4) <= '0';
		else
			cia2_pbi(4) <= '1';
		end if;
		if joyD(6 downto 4) /= "000" then
			cia2_pbi(5) <= '0';
		else
			cia2_pbi(5) <= '1';
		end if;
		cia2_pbi(7 downto 6) <= cia2_pbo(7 downto 6);
	end if;
end process;

-- UART outputs... TODO connect to ftdi
uart_txd <= cia2_pao(2);
uart_rts <= cia2_pbo(1);
uart_dtr <= cia2_pbo(2);
uart_ri_out <= cia2_pbo(3);
uart_dcd_out <= cia2_pbo(4);

-- -----------------------------------------------------------------------
-- Cartridge port lines LCA
-- -----------------------------------------------------------------------
romL <= cs_romL;
romH <= cs_romH;
IOE <= cs_ioE;
IOF <= cs_ioF;
UMAXromH <= cs_UMAXromH;
CPU_hasbus <= cpuHasBus;


-- -----------------------------------------------------------------------
-- VIC-II video interface chip
-- -----------------------------------------------------------------------
process(clk32)
begin
	if rising_edge(clk32) then
		if phi0_cpu = '1' then
			if cpuWe = '1' and cs_vic = '1' then
				vicBus <= cpuDo;
			else
				vicBus <= x"FF";
			end if;
		end if;
	end if;
end process;

-- In the first three cycles after BA went low, the VIC reads
-- $ff as character pointers and
-- as color information the lower 4 bits of the opcode after the access to $d011.
vicDiAec <= vicBus when aec = '0' else vicDi;
colorDataAec <= cpuDi(3 downto 0) when aec = '0' else colorData;

-- -----------------------------------------------------------------------
-- VIC bank to address lines
-- -----------------------------------------------------------------------
-- The glue logic on a C64C will generate a glitch during 10 <-> 01
-- generating 00 (in other words, bank 3) for one cycle.
--
-- When using the data direction register to change a single bit 0->1
-- (in other words, decreasing the video bank number by 1 or 2),
-- the bank change is delayed by one cycle. This effect is unstable.
process(clk32)
begin
	if rising_edge(clk32) then
		if phi0_cpu = '0' and enableVic = '1' then
			vicAddr1514 <= not cia2_pao(1 downto 0);
		end if;
	end if;
end process;

-- emulate only the first glitch (enough for Undead from Emulamer)
vicAddr(15 downto 14) <= "11" when ((vicAddr1514 xor not cia2_pao(1 downto 0)) = "11") and (cia2_pao(0) /= cia2_pao(1)) else not unsigned(cia2_pao(1 downto 0));

vic: entity work.video_vicii_656x
generic map (
	registeredAddress => false,
	emulateRefresh    => true,
	emulateLightpen   => true,
	emulateGraphics   => true
)			
port map (
	clk => clk32,
	reset => reset,
	enaPixel => enablePixel,
	enaData => enableVic,
	phi => phi0_cpu,

	baSync => '0',
	ba => baLoc,

	mode6567old => '0', -- 60 Hz NTSC USA
	mode6567R8  => '0', -- 60 Hz NTSC USA
	mode6569    => '1', -- 50 Hz PAL-B Europe
	mode6572    => '0', -- 50 Hz PAL-N southern South America (not Brazil)

	-- CPU bus
	cs => cs_vic,
	we => cpuWe,
	aRegisters => cpuAddr(5 downto 0),
	diRegisters => cpuDo,

	-- video data bus
	di => vicDiAec,
	diColor => colorDataAec,
	do => vicData,
	vicAddr => vicAddr(13 downto 0),

	addrValid => aec,

	-- VGA
	hsync => vicHSync,
	vsync => vicVSync,
	blank => vicBlank,
	colorIndex => vicColorIndex,

	lp_n => cia1_pbi(4), -- light pen
	irq_n => irq_vic
);

c64colors: entity work.fpga64_rgbcolor
port map (
	index => vicColorIndex,
	r => vic_r,
	g => vic_g,
	b => vic_b
);

-- -----------------------------------------------------------------------
-- SID
-- -----------------------------------------------------------------------
div1m: process(clk32) -- this process devides 32 MHz to 1MHz (for the SID)
begin
	if (rising_edge(clk32)) then
		if (reset = '1') then
			clk_1MHz <= "00000000000000000000000000000001";
		else
			clk_1MHz(31 downto 1) <= clk_1MHz(30 downto 0);
			clk_1MHz(0)           <= clk_1MHz(31);
		end if;
	end if;
end process;

audio_data  <= std_logic_vector(audio_6581) when sid_ver='0' else audio_8580;
audio_l     <= audio_data(audio_data'high downto audio_data'high-3);
audio_r     <= audio_data(audio_data'high downto audio_data'high-3);
sid_we      <= pulseWrRam and phi0_cpu and cs_sid;
sid_sel_int <= not sid_mode(1) or (not sid_mode(0) and not cpuAddr(5)) or (sid_mode(0) and not cpuAddr(8));
sid_wren    <= sid_we and sid_sel_int;
sid_we_ext  <= sid_we and (not sid_mode(1) or not sid_sel_int);
sid_do      <= std_logic_vector(io_data) when sid_sel_int = '0' else sid_do6581 when sid_ver='0' else sid_do8580;

pot_x1 <= (others => '1' ) when cia1_pao(6) = '0' else not pot1;
pot_y1 <= (others => '1' ) when cia1_pao(6) = '0' else not pot2;
pot_x2 <= (others => '1' ) when cia1_pao(7) = '0' else not pot3;
pot_y2 <= (others => '1' ) when cia1_pao(7) = '0' else not pot4;
sid_pot_x <= pot_x1 and pot_x2;
sid_pot_y <= pot_y1 and pot_y2;

sound_6581: if sid_ver = '0' generate
sid_6581: entity work.sid_top
port map (
	clock => clk32,
	reset => reset,

	addr => "000" & cpuAddr(4 downto 0),
	wren => sid_wren,
	wdata => std_logic_vector(cpuDo),
	rdata => sid_do6581,

	potx => sid_pot_x,
	poty => sid_pot_y,

	comb_wave_l => '0',
	comb_wave_r => '0',

	extfilter_en => extfilter_en,

	start_iter => clk_1MHz(31),
	sample_left => audio_6581,
	sample_right => open
);
end generate;

-- compile will never finish, therefore commented
--sound_8580: if sid_ver = '1' generate
--sid_8580 : entity work.sid8580_vhd
--port map (
--	reset => reset,
--	clk => clk32,
--	ce_1m => clk_1MHz(31),
--	we => sid_wren,
--	addr => std_logic_vector(cpuAddr(4 downto 0)),
--	data_in => std_logic_vector(cpuDo),
--	data_out => sid_do8580,
--	pot_x => sid_pot_x,
--	pot_y => sid_pot_y,
--	audio_data => audio_8580,
--	extfilter_en => extfilter_en
--);
--end generate;

---------------------------------------------------------

  led <= spi_ram_addr(7 downto 0);

  -- SPI OSD pipeline
  spi_osd_inst: entity work.spi_osd
  generic map
  (
    c_start_x      =>128, c_start_y =>  6, -- xy centered
    c_char_bits_x  =>  6, c_chars_y => 18, -- xy size, slightly less than full screen
    c_bits_x       => 11, c_bits_y  =>  9, -- xy counters bits
    c_inverse      =>  1, -- 1:support inverse video 0:no inverse video
    c_transparency =>  1, -- 1:semi-tranparent 0:opaque
    c_init_on      =>  0, -- 1:OSD initially shown without any SPI init
    c_char_file    => "osd.mem", -- initial OSD content
    c_font_file    => "font_bizcat8x16.mem"
  )
  port map
  (
    clk_pixel => clk_pixel, clk_pixel_ena => sysCycle(0),
    i_r => std_logic_vector(vic_r(7 downto 0)),
    i_g => std_logic_vector(vic_g(7 downto 0)),
    i_b => std_logic_vector(vic_b(7 downto 0)),
    i_hsync => vicHSync, i_vsync => vicVSync, i_blank => vicBlank,
    i_csn => spi_csn, i_sclk => spi_sck, i_mosi => spi_mosi,
    o_r => osd_vga_r, o_g => osd_vga_g, o_b => osd_vga_b,
    o_hsync => osd_vga_hsync, o_vsync => osd_vga_vsync, o_blank => osd_vga_blank
  );

  vga2dvid_instance: entity work.vga2dvid
  generic map
  (
    C_ddr => '1',
    C_shift_clock_synchronizer => '0'
  )
  port map
  (
    clk_pixel => clk_pixel,
    clk_shift => clk_shift,

    in_red    => osd_vga_r,
    in_green  => osd_vga_g,
    in_blue   => osd_vga_b,
    in_hsync  => osd_vga_hsync,
    in_vsync  => osd_vga_vsync,
    in_blank  => osd_vga_blank,

    -- single-ended output ready for differential buffers
    out_red   => dvid_red,
    out_green => dvid_green,
    out_blue  => dvid_blue,
    out_clock => dvid_clock
  );

  -- vendor specific DDR modules
  -- convert SDR 2-bit input to DDR clocked 1-bit output (single-ended)
  ddr_clock: ODDRX1F port map (D0=>dvid_clock(0), D1=>dvid_clock(1), Q=>gpdi_dp(3), SCLK=>clk_shift, RST=>'0');
  ddr_red:   ODDRX1F port map (D0=>dvid_red(0),   D1=>dvid_red(1),   Q=>gpdi_dp(2), SCLK=>clk_shift, RST=>'0');
  ddr_green: ODDRX1F port map (D0=>dvid_green(0), D1=>dvid_green(1), Q=>gpdi_dp(1), SCLK=>clk_shift, RST=>'0');
  ddr_blue:  ODDRX1F port map (D0=>dvid_blue(0),  D1=>dvid_blue(1),  Q=>gpdi_dp(0), SCLK=>clk_shift, RST=>'0');

end Behavioral;
