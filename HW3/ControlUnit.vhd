----------------------------------------------------------------------------------------
-- Author: 			David Kornfeld and Bobby Abrahamson
-- Title:			ControlUnit
-- Description:  	This file implements the Control Unit for the AVR_2019 CPU designed by 
--						Bobby Abrahamson and David Kornfeld. It reads in instructions from the
--						program data bus and latches them into the instruction register. From
--						there, the IR bits are used (along with a finite state machine) to
--						generate all the control signals for the other modules in the CPU.
--						Additionally, offsets and immediate values are passed to the modules
--						that read them from the instruction register. 
--			
--
--	Parameters: (from header)
--		NUM_BITS				(integer range 2 to Infinity) - The number of bits used to 
--																			represent the numbers in the 
--																			Data bus.
--		INSTR_SIZE			(integer range 2 to Inifinty)	- The number of bits in the IR
--		DATA_OFFSET_SIZE 	(integer range 2 to NUM_BITS) - The size of offsets allowed on DataDB
--		PROG_OFFSET_SIZE 	(integer range 2 to INSTR_SIZE) - The size of offsets allowed on ProgDB
--
-- Inputs:
--		clock					(std_logic)											- System clock
--		SREG      			(std_logic_vector(NUM_FLAGS-1 downto 0))	- Status Register
--		ProgDB				(std_logic_vector(INSTR_SIZE-1 downto 0))	- Program Data Bus
--
--		IR						(std_logic_vector(INSTR_SIZE-1 downto 0) 	- **TESTING ONLY**, IR input
--		
-- Outputs: (Control Signals)
--		DataRd			(std_logic)												- Read data
--		DataWr			(std_logic)												- Write data
--		IOSel				(std_logic)												- Read/Write from IO space
--		RegInSel			(std_logic)												- Controls input to RegArray
--		OPBInSel			(std_logic)												- Controls Mux into ALU B Op
--		DBSel				(std_logic_vector(1 downto 0))					- Controls output to Data DB
--
--		RegArray Control Signals: ##########################################################
--		RegASel			(std_logic_vector(Log2(NUM_REG)-1 downto 0))	- Register A Select lines
--		RegBSel			(std_logic_vector(Log2(NUM_REG)-1 downto 0))	- Register B Select lines
--		RegWrSel			(std_logic_vector(Log2(NUM_REG)-1 downto 0))	- Reg Enable decoder lines
--		RegWr				(std_logic)												- Reg (write) Enable
--		AddrDataIn		(std_logic_vector(2*NUM_BITS-1 downto 0))		- Input data for address
--																							registers
--		AddrRegSel		(std_logic_vector(Log2(NUM_ADDR_REG)-1) downto 0)) - Address register 
--																								output select
--		AddrRegWrSel	(std_logic_vector(Log2(NUM_ADDR_REG)-1) downto 0)) - Address register 
--																								enable decoder lines
--								00 -> X
--								01 -> Y
--								10 -> Z
--								11 -> SP
--		AddrRegWr		(std_logic)												- Address Reg (write) En
--
--		ALU Control Signals: ###############################################################
--		N_AddMask		(std_logic) 								- Active low mask for Operand A
--		FSRControl		(std_logic_vector(3 downto 0))		- F block and shifter control lines
--		Subtract			(std_logic)									- Command subtraction from adder
--		CarryInControl	(std_logic_vector(1 downto 0))		- Mux lines for carry in to adder
--		ALUResultSel	(std_logic)									- Select between adder and SR
--		TBitSelect		(std_logic_vector(2 downto 0))		- Select which register bit
--																				for loading/storing T
--		TLoad				(std_logic)									- Indicate if loading from T flag
--
--		Program Memory Access Unit Control Signals: ########################################
--		PCUpdateEn		(std_logic) 								- Enable PC to update
--		N_PCLoad			(std_logic_vector(3 downto 0))		- Active low load control for PC
--		PCControl		(std_logic_vector(2 downto 0))		- Mux input to adder control
--		HiLoSel			(std_logic)									- Selects if loading high or low
--																				part of PC
--		Data Memory Access Unit Control Signals: ###########################################
--		N_Inc				(std_logic) 								- Active low increment select
--		N_OffsetMask	(std_logic) 								- Active low mask for offset inp.
--		PrePostSel		(std_logic) 								- Select between pre-post inc
--																				part of PC
--
-- Revision History:
-- 	01/24/19	David	Kornfeld		Initial Revision
--		01/31/19	David Kornfeld		Added clock input and test input for IR
--		01/31/19	David Kornfeld		Began first implementation of ALU controls and FSM
-----------------------------------------------------------------------------------------
library  ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AVR_2019_constants.all;
use work.opcodes.all;
-----------------------------------------------------------------------------------------
entity ControlUnit is
	generic	(
		NUM_BITS				:	integer 	:= NUM_BITS;
		INSTR_SIZE			:	integer	:= INSTR_SIZE;
		DATA_OFFSET_SIZE	:	integer	:= DATA_OFFSET_SIZE;
		PROG_OFFSET_SIZE	:	integer	:= PROG_OFFSET_SIZE
	);
	port 		(
		-- TESTING ONLY
		IR					:	in		std_logic_vector(INSTR_SIZE-1 downto 0);
	
		clock				:	in		std_logic;
		SREG      		:	in		std_logic_vector(NUM_FLAGS-1 downto 0);
		ProgDB			:	in		std_logic_vector(INSTR_SIZE-1 downto 0);
		DataRd			:	out	std_logic;
		DataWr			:	out	std_logic;
		IOSel				:	out	std_logic;
		RegInSel			:	out	std_logic;
		OPBInSel			:	out	std_logic;
		-- RegArray
		DBSel				:	out	std_logic_vector(1 downto 0);
		RegASel			:	out	std_logic_vector(4 downto 0);
		RegBSel			:	out	std_logic_vector(4 downto 0);
		RegWrSel			:	out	std_logic_vector(4 downto 0);
		RegWr				:	out	std_logic;
		AddrDataIn		:	out	std_logic_vector(2*NUM_BITS-1 downto 0);
		AddrRegSel		:	out	std_logic_vector(1 downto 0);
		AddrRegWrSel	:	out	std_logic_vector(1 downto 0);
		-- ALU
		N_AddMask		:	out	std_logic;
		FSRControl		:	out	std_logic_vector(3 downto 0);
		Subtract			:	out	std_logic;
		CarryInControl	:	out	std_logic_vector(1 downto 0);
		ALUResultSel	:	out	std_logic;
		CarryFlag		:	out	std_logic;
		TFlag				:	out	std_logic;
		TBitSelect		:	out	std_logic_vector(2 downto 0);
		TLoad				:	out	std_logic;
		FlagMask			:	out	std_logic_vector(NUM_FLAGS-1 downto 0);
		-- PMAU
		PCUpdateEn		:	out	std_logic;
		N_PCLoad			:	out	std_logic_vector(3 downto 0);
		PCControl		:	out	std_logic_vector(2 downto 0);
		HiLoSel			:	out	std_logic;
		-- DMAU
		N_Inc				:	out	std_logic;
		N_OffsetMask	:	out	std_logic;
		PrePostSel		:	out	std_logic
	);
end ControlUnit;
-----------------------------------------------------------------------------------------
architecture data_flow of ControlUnit is
	signal instr_cycle	:	std_logic_vector(MAX_INSTR_CLKS-1 downto 0); 	-- 1-hot cycle
																									-- counter
	signal reset_instr_counter	:	std_logic;
begin

	-- Instruction cycle counter logic
	process(clock)
	begin
		if rising_edge(clock) then
			if reset_instr_counter = '1' then
				-- Synchronous reset to 1 in the leftmost place
				instr_cycle <= std_logic_vector(to_unsigned(1, MAX_INSTR_CLKS));
			else
				-- Shift the bit left
				instr_cycle <= instr_cycle(MAX_INSTR_CLKS-2 downto 0) & '0';
			end if;
		end if;
	end process;
	
	-- Instruction decoding
	process(IR)
	begin
		-- Default, can assume that register selects follow Rd, Rr scheme from instr set
		RegASel <= IR(8 downto 4);
		RegBSel <= IR(9) & IR(3 downto 0);
		
		if std_match(IR, OpADC) then
			-- ALU
			N_AddMask		<= '1';		-- Adding normally
			FSRControl		<= "1010"; 	-- B
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "10"; 	-- Use carry flag
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpADD) then
			-- ALU
			N_AddMask		<= '1';		-- Adding normally
			FSRControl		<= "1010"; 	-- B
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "00"; 	-- No initial carry
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		--if std_match(IR, OpADIW) then
		--	-- ALU
		--	N_AddMask		<= '1';		-- Adding normally
		--	FSRControl		<= "1010"; 	-- B
		--	Subtract			<= '0'; 		-- Not subtracting
		--	CarryInControl	<= "10"; 	-- Use carry flag
		--	ALUResultSel	<= '0';		-- Adder
		--	TBitSelect		<= IR(2 downto 0); -- Don't care
		--	TLoad				<= '0';		-- Not loading from T
		--end if;
		
		if std_match(IR, OpAND) then
			-- ALU
			N_AddMask		<= '0';		-- Doing logical ops
			FSRControl		<= "1000"; 	-- A AND B
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "00"; 	-- No initial carry
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
	
		if std_match(IR, OpANDI) then
			-- ALU
			N_AddMask		<= '0';		-- Doing logical ops
			FSRControl		<= "1000"; 	-- A AND B
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "00"; 	-- No initial carry
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpASR) then
			-- ALU
			N_AddMask		<= '0';		-- Don't care
			FSRControl		<= "0101"; 	-- Doing ASR
			Subtract			<= '0'; 		-- Don't care
			CarryInControl	<= "00"; 	-- Don't care
			ALUResultSel	<= '1';		-- Shifter
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
	--	if std_match(IR, OpBCLR) then
	--		-- ALU
	--		N_AddMask		<= '1';		-- Adding normally
	--		FSRControl		<= "1010"; 	-- B
	--		Subtract			<= '0'; 		-- Not subtracting
	--		CarryInControl	<= "10"; 	-- Use carry flag
	--		ALUResultSel	<= '0';		-- Adder
	--		TBitSelect		<= IR(2 downto 0); -- Don't care
	--		TLoad				<= '0';		-- Not loading from T
	--	end if;
		
		if std_match(IR, OpBLD) then
			-- ALU
			N_AddMask		<= '0';		-- Hide Op A
			FSRControl		<= "1010"; 	-- B, passthrough
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "00"; 	-- No carry in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- from IR
			TLoad				<= '1';		-- Loading from T
		end if;
		
	--	if std_match(IR, OpBSET) then
	--		-- ALU
	--		N_AddMask		<= '1';		-- Adding normally
	--		FSRControl		<= "1010"; 	-- B
	--		Subtract			<= '0'; 		-- Not subtracting
	--		CarryInControl	<= "10"; 	-- Use carry flag
	--		ALUResultSel	<= '0';		-- Adder
	--		TBitSelect		<= IR(2 downto 0); -- Don't care
	--		TLoad				<= '0';		-- Not loading from T
	--	end if;
		
		if std_match(IR, OpBST) then
			-- ALU
			N_AddMask		<= '0';		-- Don't care
			FSRControl		<= "0000"; 	-- Don't care
			Subtract			<= '0'; 		-- Don't care
			CarryInControl	<= "00"; 	-- Don't care
			ALUResultSel	<= '0';		-- Don't care
			TBitSelect		<= IR(2 downto 0); -- from IR
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpCOM) then
			-- ALU
			N_AddMask		<= '0';		-- Logical operation
			FSRControl		<= "0101"; 	-- not B
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "00"; 	-- no carry
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpCP) then
			-- ALU
			N_AddMask		<= '1';		-- Subtracting normally
			FSRControl		<= "1010"; 	-- B
			Subtract			<= '1'; 		-- Subtracting
			CarryInControl	<= "01"; 	-- No borrow in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpCPC) then
			-- ALU
			N_AddMask		<= '1';		-- Subtracting normally
			FSRControl		<= "1010"; 	-- B
			Subtract			<= '1'; 		-- Subtracting
			CarryInControl	<= "11"; 	-- Use carry bar
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpCPI) then
			-- ALU
			N_AddMask		<= '1';		-- Subtracting normally
			FSRControl		<= "1010"; 	-- B
			Subtract			<= '1'; 		-- Subtracting
			CarryInControl	<= "01"; 	-- No borrow in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpDEC) then
			-- ALU
			N_AddMask		<= '1';		-- Subtracting normally
			FSRControl		<= "1111"; 	-- 11111111 (-1)
			Subtract			<= '1'; 		-- Subtracting
			CarryInControl	<= "01"; 	-- No borrow in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpEOR) then
			-- ALU
			N_AddMask		<= '0';		-- Logical
			FSRControl		<= "0110"; 	-- A xor B
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "00"; 	-- No carry in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpINC) then
			-- ALU
			N_AddMask		<= '1';		-- Adding normally
			FSRControl		<= "0000"; 	-- 00000000 (0)
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "01"; 	-- Add one
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpLSR) then
			-- ALU
			N_AddMask		<= '0';		-- Don't care
			FSRControl		<= "0110"; 	-- LSR
			Subtract			<= '0'; 		-- Don't care
			CarryInControl	<= "00"; 	-- Don't care
			ALUResultSel	<= '1';		-- Shifter
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpNEG) then
			-- ALU
			N_AddMask		<= '0';		-- Unary operation
			FSRControl		<= "0101"; 	-- not B
			Subtract			<= '0'; 		-- Not subtracting (TODO, check if this produces the right carry)
			CarryInControl	<= "01"; 	-- Add one
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpOR) then
			-- ALU
			N_AddMask		<= '0';		-- Logical
			FSRControl		<= "1110"; 	-- A or B
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "00"; 	-- No carry in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpORI) then
			-- ALU
			N_AddMask		<= '0';		-- Logical
			FSRControl		<= "1110"; 	-- A or B
			Subtract			<= '0'; 		-- Not subtracting
			CarryInControl	<= "00"; 	-- No carry in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpROR) then
			-- ALU
			N_AddMask		<= '0';		-- Don't care
			FSRControl		<= "0111"; 	-- ROR
			Subtract			<= '0'; 		-- Don't care
			CarryInControl	<= "00"; 	-- Shifter
			ALUResultSel	<= '1';		-- Shifter
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpSBC) then
			-- ALU
			N_AddMask		<= '1';		-- Subtracting normally
			FSRControl		<= "0101"; 	-- not B
			Subtract			<= '1'; 		-- Subtracting
			CarryInControl	<= "11"; 	-- Use carry bar
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpSBCI) then
			-- ALU
			N_AddMask		<= '1';		-- Subtracting normally
			FSRControl		<= "0101"; 	-- not B
			Subtract			<= '1'; 		-- Subtracting
			CarryInControl	<= "11"; 	-- Use carry bar
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
	--	if std_match(IR, OpSBIW) then
	--		-- ALU
	--		N_AddMask		<= '1';		-- Subtracting normally
	--		FSRControl		<= "0101"; 	-- not B
	--		Subtract			<= '1'; 		-- Subtracting
	--		CarryInControl	<= "11"; 	-- Use carry bar
	--		ALUResultSel	<= '0';		-- Adder
	--		TBitSelect		<= IR(2 downto 0); -- Don't care
	--		TLoad				<= '0';		-- Not loading from T
	--	end if;
		
		if std_match(IR, OpSUB) then
			-- ALU
			N_AddMask		<= '1';		-- Subtracting normally
			FSRControl		<= "0101"; 	-- not B
			Subtract			<= '1'; 		-- Subtracting
			CarryInControl	<= "01"; 	-- No borrow in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpSUBI) then
			-- ALU
			N_AddMask		<= '1';		-- Subtracting normally
			FSRControl		<= "0101"; 	-- not B
			Subtract			<= '1'; 		-- Subtracting
			CarryInControl	<= "01"; 	-- No borrow in
			ALUResultSel	<= '0';		-- Adder
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
		
		if std_match(IR, OpSWAP) then
			-- ALU
			N_AddMask		<= '0';		-- Don't care
			FSRControl		<= "0010"; 	-- SWAP
			Subtract			<= '0'; 		-- Don't care
			CarryInControl	<= "00"; 	-- Don't care
			ALUResultSel	<= '1';		-- Shifter
			TBitSelect		<= IR(2 downto 0); -- Don't care
			TLoad				<= '0';		-- Not loading from T
		end if;
	end process;
end architecture;