-- (c)EMARD
-- License=BSD

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.numeric_std.ALL;

-- for diamond (not for opensource tools yosys/trellis)
--library ecp5u;
--use ecp5u.components.all;

entity top_bustest is
  generic
  (
    --
    bios     : std_logic_vector(1 downto 0) := "00";
    -- test picture generator
    x        : natural :=  720; -- pixels
    y        : natural :=  576; -- pixels
    f        : natural :=   50; -- Hz 60,50,30
    xadjustf : integer :=    0; -- adjust -3..3 if no picture
    yadjustf : integer :=    0; -- or to fine-tune f
    C_ddr    : natural :=    1  -- 0:SDR 1:DDR
  );
  port
  (
    clk_25mhz: in std_logic;  -- main clock input from 25MHz clock source

    -- Onboard blinky
    led: out std_logic_vector(7 downto 0);
    btn: in std_logic_vector(6 downto 0);

    -- GPIO (some are shared with wifi and adc)
    gp, gn: inout std_logic_vector(27 downto 0) := (others => 'Z');

    -- Digital Video (differential outputs)
    gpdi_dp: out std_logic_vector(3 downto 0)
  );
end;

architecture Behavioral of top_bustest is
  type T_video_timing is record
    x                  : natural;
    hsync_front_porch  : natural;
    hsync_pulse_width  : natural;
    hsync_back_porch   : natural;
    y                  : natural;
    vsync_front_porch  : natural;
    vsync_pulse_width  : natural;
    vsync_back_porch   : natural;
    f_pixel            : natural;
  end record T_video_timing;
  
  type T_possible_freqs is array (natural range <>) of natural;
  constant C_possible_freqs: T_possible_freqs :=
  (
    25000000,
    27000000,
    40000000,
    50000000,
    54000000,
    60000000,
    65000000,
    75000000,
    80000000,  -- overclock 400MHz
    100000000, -- overclock 500MHz
    108000000, -- overclock 540MHz
    120000000  -- overclock 600MHz
  );

  function F_find_next_f(f: natural)
    return natural is
      variable f0: natural := 0;
    begin
      for fx in C_possible_freqs'range loop
        if C_possible_freqs(fx)>f then
          f0 := C_possible_freqs(fx);
          exit;
        end if;
      end loop;
      return f0;
    end F_find_next_f;
  
  function F_video_timing(x,y,f: integer)
    return T_video_timing is
      variable video_timing : T_video_timing;
      variable xminblank   : natural := x/64; -- initial estimate
      variable yminblank   : natural := y/64; -- for minimal blank space
      variable min_pixel_f : natural := f*(x+xminblank)*(y+yminblank);
      variable pixel_f     : natural := F_find_next_f(min_pixel_f);
      variable yframe      : natural := y+yminblank;
      variable xframe      : natural := pixel_f/(f*yframe);
      variable xblank      : natural := xframe-x;
      variable yblank      : natural := yframe-y;
    begin
      video_timing.x                 := x;
      video_timing.hsync_front_porch := xblank/3;
      video_timing.hsync_pulse_width := xblank/3;
      video_timing.hsync_back_porch  := xblank-video_timing.hsync_pulse_width-video_timing.hsync_front_porch+xadjustf;
      video_timing.y                 := y;
      video_timing.vsync_front_porch := yblank/3;
      video_timing.vsync_pulse_width := yblank/3;
      video_timing.vsync_back_porch  := yblank-video_timing.vsync_pulse_width-video_timing.vsync_front_porch+yadjustf;
      video_timing.f_pixel           := pixel_f;

      return video_timing;
    end F_video_timing;
    
  constant video_timing : T_video_timing := F_video_timing(x,y,f);

  signal clocks: std_logic_vector(3 downto 0);
  signal clk_pixel, clk_shift: std_logic;
  signal vga_hsync, vga_vsync, vga_blank, vga_de: std_logic;
  signal vga_r, vga_g, vga_b: std_logic_vector(7 downto 0);
  signal dvid_red, dvid_green, dvid_blue, dvid_clock: std_logic_vector(1 downto 0);
  signal beam_x, beam_y: std_logic_vector(12 downto 0);
  
  signal R_slow_ena: std_logic_vector(10 downto 0);

  component ODDRX1F
    port (D0, D1, SCLK, RST: in std_logic; Q: out std_logic);
  end component;

----------------------------------------------------------
constant resetCycles : integer := 4095;
signal clk32: std_logic;
signal clocks_c64: std_logic_vector(3 downto 0);

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
signal audio_6581   : signed(17 downto 0);
signal pot_x1       : std_logic_vector(7 downto 0);
signal pot_y1       : std_logic_vector(7 downto 0);
signal pot_x2       : std_logic_vector(7 downto 0);
signal pot_y2       : std_logic_vector(7 downto 0);
signal audio_8580   : std_logic_vector(17 downto 0);

-- "external" connections, in this project internal
-- cartridge port
signal	game        : std_logic;
signal	exrom       : std_logic;
signal	ioE_rom     : std_logic;
signal	ioF_rom     : std_logic;
signal	max_ram     : std_logic;
signal	irq_n       : std_logic;
signal	nmi_n       : std_logic;
signal	nmi_ack     : std_logic;
signal	dma_n       : std_logic := '1';
signal	ba          : std_logic;
signal	romL	    : std_logic; -- cart signals LCA
signal	romH	    : std_logic; -- cart signals LCA
signal	UMAXromH    : std_logic; -- cart signals LCA
signal	IOE	    : std_logic; -- cart signals LCA
signal	IOF	    : std_logic; -- cart signals LCA
signal	CPU_hasbus  : std_logic; -- CPU has the bus STROBE
signal	freeze_key  : std_logic;

signal	ioF_ext     : std_logic;
signal	ioE_ext     : std_logic;
signal	io_data     : unsigned(7 downto 0);

-- joystick interface
signal	joyA        : std_logic_vector(6 downto 0);
signal	joyB        : std_logic_vector(6 downto 0);
signal	joyC        : std_logic_vector(6 downto 0);
signal	joyD        : std_logic_vector(6 downto 0);
signal	pot1        : std_logic_vector(7 downto 0);
signal	pot2        : std_logic_vector(7 downto 0);
signal	pot3        : std_logic_vector(7 downto 0);
signal	pot4        : std_logic_vector(7 downto 0);

-- Connector to the SID
signal	audio_data  : std_logic_vector(17 downto 0);
signal	extfilter_en: std_logic;
signal	sid_ver     : std_logic;
signal	sid_we_ext  : std_logic;
signal	sid_mode    : std_logic_vector(1 downto 0);

-- IEC
signal	iec_data_o  : std_logic;
signal	iec_data_i  : std_logic;
signal	iec_clk_o   : std_logic;
signal	iec_clk_i   : std_logic;
signal	iec_atn_o   : std_logic;
	
signal	c64rom_addr : std_logic_vector(13 downto 0);
signal	c64rom_data : std_logic_vector(7 downto 0);
signal	c64rom_wr   : std_logic;

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

----------------------------------------------------------
begin
  --clk_single_pll: entity work.ecp5pll
  --generic map
  --(
  --    in_Hz => natural(25.0e6),
  --  out0_Hz => video_timing.f_pixel*5,
  --  out1_Hz => video_timing.f_pixel
  --)
  --port map
  --(
  --  clk_i => clk_25MHz,
  --  clk_o => clocks
  --);
  --clk_shift <= clocks(0);
  --clk_pixel <= clocks(1);

---------------------------------------------------------
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

		if sysCycle = to_unsigned(14, sysCycle'length) then -- CYCLE_VIC2
			enableVic <= '1';
		elsif sysCycle = to_unsigned(28, sysCycle'length) then -- CYCLE_CPUC
			enableCia_n <= '1';
		elsif sysCycle = to_unsigned(30, sysCycle'length) then -- CYCLE_CPUE
			enableVic <= '1';
			enableCpu <= '1';
		elsif sysCycle = to_unsigned(31, sysCycle'length) then -- CYCLE_CPUF
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
		if btn(0) = '0' or dma_n = '0' then -- temp reset fix
			reset_cnt <= 0;
		end if;
	end if;
end process;

--iec_data_o <= not cia2_pao(5);
--iec_clk_o <= not cia2_pao(4);
--iec_atn_o <= not cia2_pao(3);
ramDataOut <= "00" & cia2_pao(5 downto 3) & "000" when sysCycle >= to_unsigned(sysCycleDef'pos(CYCLE_IEC0),sysCycle'length) and sysCycle <= to_unsigned(sysCycleDef'pos(CYCLE_IEC3),sysCycle'length) else cpuDo;
ramAddr <= systemAddr;
ramWe <= '0' when sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_IEC2),sysCycle'length) or sysCycle = to_unsigned(sysCycleDef'pos(CYCLE_IEC3),sysCycle'length) else not systemWe;
-- CPU2...CPUE or VIC0..VIC3
ramCE <= '0' when ((sysCycle >= to_unsigned(sysCycleDef'pos(CYCLE_CPU2),sysCycle'length) and sysCycle <= to_unsigned(sysCycleDef'pos(CYCLE_CPUE),sysCycle'length))
               or  (sysCycle >= to_unsigned(sysCycleDef'pos(CYCLE_VIC0),sysCycle'length) and sysCycle <= to_unsigned(sysCycleDef'pos(CYCLE_VIC3),sysCycle'length)))
              and cs_ram = '1' else '1';

-- dummy bus traffic generator for VIC to generate picture
dummy: if true generate
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

-- -----------------------------------------------------------------------
-- PLA and bus-switches with ROM
-- -----------------------------------------------------------------------
pla: if false generate
buslogic: entity work.fpga64_buslogic
port map (
	clk => clk32,
	reset => reset,
	bios => bios,

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

---------------------------------------------------------

  process(clk_pixel)
  begin
    if rising_edge(clk_pixel) then
      if R_slow_ena(R_slow_ena'high)='0' then
        R_slow_ena <= R_slow_ena + 1;
      else
        R_slow_ena <= (others => '0');
      end if;
    end if;
  end process;

  vga_instance: entity work.vga
  generic map
  (
    C_resolution_x      => video_timing.x,
    C_hsync_front_porch => video_timing.hsync_front_porch,
    C_hsync_pulse       => video_timing.hsync_pulse_width,
    C_hsync_back_porch  => video_timing.hsync_back_porch,
    C_resolution_y      => video_timing.y,
    C_vsync_front_porch => video_timing.vsync_front_porch,
    C_vsync_pulse       => video_timing.vsync_pulse_width,
    C_vsync_back_porch  => video_timing.vsync_back_porch,

    C_bits_x       =>  12,
    C_bits_y       =>  11
  )
  port map
  (
      clk_pixel  => clk_pixel,
      clk_pixel_ena => '1', -- R_slow_ena(R_slow_ena'high),
      test_picture => '1',
      --beam_x     => beam_x,
      --beam_y     => beam_y,
      vga_r      => vga_r,
      vga_g      => vga_g,
      vga_b      => vga_b,
      vga_hsync  => vga_hsync,
      vga_vsync  => vga_vsync,
      vga_blank  => vga_blank
      --vga_de     => vga_de
  );
  
  led(0) <= vichsync;
  led(1) <= vicvsync;
  led(2) <= reset;
  led(6 downto 3) <= (others => '0');
  led(7) <= vicblank;

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

--  debug test picture generator vga_instance
--    in_red    => vga_r,
--    in_green  => vga_g,
--    in_blue   => vga_b,
--    in_hsync  => vga_hsync,
--    in_vsync  => vga_vsync,
--    in_blank  => vga_blank,

    in_red    => std_logic_vector(vic_r),
    in_green  => std_logic_vector(vic_g),
    in_blue   => std_logic_vector(vic_b),
    in_hsync  => vicHSync,
    in_vsync  => vicVSync,
    in_blank  => vicBlank,

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
