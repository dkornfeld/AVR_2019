-- Title: 	Test Bench for ALU
-- Author:	David Kornfeld and Bobby Abrahamson
--
-- TODO
--
--
-- Revision History:
--		01/31/19	David Kornfeld		Initial Revision
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AVR_2019_constants.all;
use work.opcodes.all;
----------------------------------------------------------------------------
entity ALU_tb is
end ALU_tb;
----------------------------------------------------------------------------
architecture TB_ARCHITECTURE of ALU_tb is

	-- Useful types for test vector #########################################
	constant test_tuple_length	:	integer := 
														(opcode_word'length + -- Instruction
														NUM_BITS + 				 -- OperandA
														NUM_BITS + 				 -- OperandB
														1 + 						 -- C Flag
														1 + 						 -- T Flag
														NUM_BITS + 				 -- Result
														NUM_FLAGS-1);			 -- Flags
	type test_tuple is array (natural range <>) of 
										std_logic_vector(test_tuple_length-1 downto 0);

	-- Constants ############################################################
	
	-- Test Vectors
	-- (Instruction, A, B, CarryFlagIn, TFlagIn, Result, ExpectedNewFlags)
	constant	TEST_VECTORS	:	test_tuple(1 to 49) :=	(
		OpADD 	& X"FF" & X"00" & '1' & '0' & X"FF" & "-------",
		OpADD 	& X"0F" & X"01" & '1' & '0' & X"10" & "-010100",
		OpADC 	& X"FF" & X"00" & '1' & '0' & X"00" & "-100000",
		OpADC 	& X"FF" & X"00" & '0' & '0' & X"FF" & "-100011",
		OpAND 	& X"FF" & X"55" & '0' & '0' & X"55" & "-010100",
		OpAND 	& X"FF" & X"00" & '0' & '0' & X"00" & "--0000-",
		OpANDI	& X"FF" & X"00" & '0' & '0' & X"00" & "--0001-",
		OpASR 	& X"AA" & X"00" & '0' & '0' & X"D5" & "--0001-",
		OpCOM 	& X"AA" & X"00" & '0' & '0' & X"55" & "--01100",
		OpCOM 	& X"FF" & X"00" & '0' & '0' & X"00" & "--00001",
		OpCP 		& X"00" & X"AA" & '0' & '0' & X"56" & "--00011",
		OpCPC 	& X"00" & X"AA" & '1' & '0' & X"55" & "-100001",
		OpCPC 	& X"00" & X"AA" & '0' & '0' & X"56" & "-100001",
		OpCPI 	& X"00" & X"AA" & '0' & '0' & X"56" & "-100001",
		OpSUB 	& X"00" & X"AA" & '0' & '0' & X"56" & "-100001",
		OpSBC 	& X"00" & X"AA" & '1' & '0' & X"55" & "-100001",
		OpSBC 	& X"00" & X"AA" & '0' & '0' & X"56" & "-100001",
		OpSUBI	& X"00" & X"AA" & '0' & '0' & X"56" & "-100001",
		OpDEC		& X"A5" & X"FF" & '1' & '0' & X"A4" & "-100001",
		OpDEC		& X"00" & X"FF" & '1' & '0' & X"FF" & "--1010-",
		OpEOR 	& X"AA" & X"55" & '0' & '0' & X"FF" & "--1010-",
		OpEOR 	& X"FF" & X"FF" & '0' & '0' & X"00" & "--1010-",
		OpINC		& X"A4" & X"FF" & '1' & '0' & X"A5" & "--0001-",
		OpINC		& X"FF" & X"FF" & '1' & '0' & X"00" & "--1010-",
		OpLSR 	& X"AA" & X"55" & '1' & '0' & X"55" & "--0001-",
		OpLSR 	& X"55" & X"55" & '1' & '0' & X"2A" & "--00000",
		OpNEG 	& X"FF" & X"55" & '1' & '0' & X"01" & "--11001",
		OpNEG 	& X"55" & X"FF" & '1' & '0' & X"AB" & "-100001",
		OpNEG 	& X"00" & X"FF" & '1' & '0' & X"00" & "-110101",
		OpOR  	& X"AA" & X"55" & '1' & '0' & X"FF" & "-000010",
		OpOR  	& X"00" & X"00" & '1' & '0' & X"00" & "--1010-",
		OpORI 	& X"AA" & X"55" & '1' & '0' & X"FF" & "--0001-",
		OpORI 	& X"00" & X"00" & '1' & '0' & X"00" & "--1010-",
		OpROR 	& X"AA" & X"00" & '0' & '0' & X"55" & "--0001-",
		OpROR 	& X"AA" & X"00" & '1' & '0' & X"D5" & "--00000",
		OpROR 	& X"55" & X"55" & '1' & '0' & X"AA" & "--01100",
		OpSWAP	& X"A3" & X"FF" & '1' & '0' & X"3A" & "--10101",
		OpSWAP	& X"00" & X"FF" & '1' & '0' & X"00" & "-------",
		"1001010000111000" & X"00" & X"A3" & '1' & '1' & X"08" & "-------", -- BSET 3
		"1001010011011000" & X"FF" & X"A3" & '1' & '1' & X"DF" & "-------", -- BCLR 5
		"1111101000000011" & X"08" & X"A3" & '1' & '0' & X"08" & "-------", -- BST 0x08 3
		"1111101000000100" & X"08" & X"A3" & '1' & '0' & X"08" & "1------", -- BST 0x08 4
		"1111100000000101" & X"00" & X"A3" & '1' & '1' & X"20" & "0------", -- BLD 0x00 5
		"1111100000000011" & X"08" & X"A3" & '1' & '0' & X"00" & "-------", -- BLD 0x08 3
		OpADIW	& X"FE" & X"02" & '1' & '1' & X"00" & "-------", -- ADIW 55FE, 2;
		OpADIW	& X"55" & X"02" & '1' & '1' & X"56" & "--00011", -- ADIW 55FE, 2;
		OpSBIW	& X"00" & X"3F" & '1' & '1' & X"C1" & "--00000", -- SBIW 0100, 3F;
		OpSBIW	& X"01" & X"3F" & '1' & '1' & X"00" & "--10101", -- SBIW 0100, 3F;
		"----------------" & "--------" & "--------" & '-' & '-' & "--------" & "--00010"
	);
	
	-- Timing Constants -----------------------------------------------------
	
	-- Clock period
	constant CLK_PERIOD_INT	:	integer	:= 20; -- ns
	constant CLK_PERIOD		:	time		:= CLK_PERIOD_INT * (1 ns);

	-- Component declaration of the tested unit #############################	
	component ALU  is
		generic	(
			NUM_BITS			:	integer := NUM_BITS -- Number of bits to use (from header)
		);
		port 		(
			OperandA      	:	in		std_logic_vector(NUM_BITS-1 downto 0);
			OperandB      	:	in		std_logic_vector(NUM_BITS-1 downto 0);
			CarryFlag		:	in		std_logic;
			TFlag				:	in		std_logic;
			N_AddMask		:	in		std_logic;
			FSRControl		:	in		std_logic_vector(3 downto 0);
			Subtract			:	in		std_logic;
			CarryInControl	:	in		std_logic_vector(1 downto 0);
			ALUResultSel	:	in		std_logic;
			TSCBitSelect	:	in		std_logic_vector(2 downto 0);
			TLoad				:	in		std_logic;
			BitSetClear		:	in		std_logic;
			SettingClearing:	in		std_logic;
			Result      	:	out	std_logic_vector(NUM_BITS-1 downto 0);
			NewFlags			:	out	std_logic_vector(NUM_FLAGS-2 downto 0)
		);
	end  component;
	
	component Registers  is
		generic	(
			NUM_BITS			:	integer := NUM_BITS   -- Number of bits to use for a GPR.
		);
		port (
			clock       : in std_logic;                             -- input clock
			  RegWr       : in std_logic;                             -- select write vs not write
			  RegWrSel    : in std_logic_vector(4 downto 0);          -- select register to write to
			  RegASel     : in std_logic_vector(4 downto 0);          -- select register A to read from
			  RegBSel     : in std_logic_vector(4 downto 0);          -- select register B to read from
			  RegIn       : in std_logic_vector(NUM_BITS-1 downto 0); -- Input data to write
			  
			  -- SREG, TODO: Move to ALU?
			  SFlag       : in std_logic;
			  FlagMask    : in std_logic_vector(NUM_BITS-1 downto 0);
			  NewFlags    : in std_logic_vector(NUM_FLAGS-2 downto 0);
			  
			  RegAOutput : out std_logic_vector(NUM_BITS-1 downto 0); -- Output register A
			  RegBOutput : out std_logic_vector(NUM_BITS-1 downto 0); -- Output register B
			SREG 	   : out std_logic_vector(NUM_BITS-1 downto 0)  -- Output SREG
		);
	end  component;
	
	component ControlUnit  is
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
			DBSel				:	out	std_logic_vector(1 downto 0);
			-- RegArray
			RegASel			:	out	std_logic_vector(4 downto 0);
			RegBSel			:	out	std_logic_vector(4 downto 0);
			RegWrSel			:	out	std_logic_vector(4 downto 0);
			RegWr				:	out	std_logic;
			AddrDataIn		:	out	std_logic_vector(2*NUM_BITS-1 downto 0);
			AddrRegSel		:	out	std_logic_vector(1 downto 0);
			AddrRegWrSel	:	out	std_logic_vector(1 downto 0);
			SFlag         	: 	out 	std_logic;
			FlagMask			:	out	std_logic_vector(NUM_FLAGS-1 downto 0);
			-- ALU
			N_AddMask		:	out	std_logic;
			FSRControl		:	out	std_logic_vector(3 downto 0);
			Subtract			:	out	std_logic;
			CarryInControl	:	out	std_logic_vector(1 downto 0);
			ALUResultSel	:	out	std_logic;
			TSCBitSelect	:	out	std_logic_vector(2 downto 0);
			TLoad				:	out	std_logic;
			BitSetClear		:	out	std_logic;
			SettingClearing:	out	std_logic;
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
	end  component;
	
	-- Unused Signals #######################################################
	-- ControlUnit
	-- General
	signal ProgDB			:	std_logic_vector(INSTR_SIZE-1 downto 0);
	signal DataRd			:	std_logic;
	signal DataWr			:	std_logic;
	signal IOSel			:	std_logic;
	signal RegInSel		:	std_logic;
	signal OPBInSel		:	std_logic;
	-- RegArray
	signal DBSel			:	std_logic_vector(1 downto 0);
	signal AddrDataIn		:	std_logic_vector(2*NUM_BITS-1 downto 0);
	signal AddrRegSel		:	std_logic_vector(1 downto 0);
	signal AddrRegWrSel	:	std_logic_vector(1 downto 0);
	-- PMAU
	signal PCUpdateEn		:	std_logic;
	signal N_PCLoad		:	std_logic_vector(3 downto 0);
	signal PCControl		:	std_logic_vector(2 downto 0);
	signal HiLoSel			:	std_logic;
	-- DMAU
	signal N_Inc			:	std_logic;
	signal N_OffsetMask	:	std_logic;
	signal PrePostSel		:	std_logic;
	-- Intermediate Signals #################################################
	
   -- Stimulus signals - signals mapped to the tested entity ports
	signal IR					:	std_logic_vector(INSTR_SIZE-1 downto 0);
	signal OperandA      	:	std_logic_vector(NUM_BITS-1 downto 0);
	signal OperandB      	:	std_logic_vector(NUM_BITS-1 downto 0);
	signal CarryFlagIn		:	std_logic;
	signal TFlagIn				:	std_logic;
	
	-- Intermediate signals - signals between tested components
	signal RegIn				:	std_logic_vector(NUM_BITS-1 downto 0);
	signal RegASel				:	std_logic_vector(4 downto 0);
	signal RegBSel				:	std_logic_vector(4 downto 0);
	signal RegWrSel			:	std_logic_vector(4 downto 0);
	signal RegWr				:	std_logic;
	signal N_AddMask			:	std_logic;
	signal FSRControl			:	std_logic_vector(3 downto 0);
	signal Subtract			:	std_logic;
	signal CarryInControl	:	std_logic_vector(1 downto 0);
	signal ALUResultSel		:	std_logic;
	signal TSCBitSelect		:	std_logic_vector(2 downto 0);
	signal TLoad				:	std_logic;
	signal BitSetClear		:	std_logic;
	signal SettingClearing	:	std_logic;
	signal SFlag       		: 	std_logic;
   signal FlagMask    		: 	std_logic_vector(NUM_BITS-1 downto 0);
   signal NewFlags    		: 	std_logic_vector(NUM_FLAGS-2 downto 0);
	
	
	-- Output signals - signals mapped to the tested entity ports
	signal Result      		:	std_logic_vector(NUM_BITS-1 downto 0);
	signal SREG					:	std_logic_vector(NUM_BITS-1 downto 0);
	
	-- Unused signals - signals mapped to the tested entity ports
	signal RegAOutput      	:	std_logic_vector(NUM_BITS-1 downto 0);
	signal RegBOutput			:	std_logic_vector(NUM_BITS-1 downto 0);
	
	-- System clock
	signal CLK     			:	std_logic;
	
   -- Signal used to stop clock signal generators
   signal END_SIM				:	BOOLEAN := FALSE;

begin -- ###################################################################
	-- Unit Under Test port map
	U_ALU : ALU
		port map(
			OperandA      	=> OperandA,
			OperandB      	=> OperandB,
			CarryFlag		=> CarryFlagIn,
			TFlag				=> TFlagIn,
			N_AddMask		=> N_AddMask,
			FSRControl		=> FSRControl,
			Subtract			=> Subtract,
			CarryInControl	=> CarryInControl,
			ALUResultSel	=> ALUResultSel,
			TSCBitSelect	=> TSCBitSelect,
			TLoad				=> TLoad,
			BitSetClear		=>	BitSetClear,
			SettingClearing=>	SettingClearing,
			Result      	=> Result,
			NewFlags			=> NewFlags
		);
		
	U_REG : Registers
		port map(
			clock 			=> CLK,
			RegWr 			=> RegWr,
			RegWrSel 		=> RegWrSel,
			RegASel 			=> RegASel,
			RegBSel 			=> RegBSel,
			RegIn 			=> RegIn,
			SFlag 			=> SFlag,
			FlagMask 		=> FlagMask,
			NewFlags 		=> NewFlags,
			RegAOutput 		=> RegAOutput,
			RegBOutput 		=> RegBOutput,
			SREG 				=> SREG
		);
	
	U_Control : ControlUnit
		port map(
			IR					=>	IR,			
			clock				=>	CLK,			
			SREG      		=> SREG,      		
			ProgDB			=> ProgDB,			
			DataRd			=> DataRd,			
			DataWr			=> DataWr,			
			IOSel				=> IOSel,		
			RegInSel			=> RegInSel,		
			OPBInSel			=> OPBInSel,	
			DBSel				=> DBSel,	
			-- RegArray             		
			RegASel			=> RegASel,			
			RegBSel			=> RegBSel,			
			RegWrSel			=> RegWrSel,		
			RegWr				=> RegWr,			
			AddrDataIn		=> AddrDataIn,		
			AddrRegSel		=> AddrRegSel,		
			AddrRegWrSel	=> AddrRegWrSel,
			SFlag				=> SFlag,
			FlagMask			=> FlagMask,
			-- ALU         
			N_AddMask		=> N_AddMask,		
			FSRControl		=> FSRControl,		
			Subtract			=> Subtract,		
			CarryInControl	=> CarryInControl,	
			ALUResultSel	=> ALUResultSel,
			TSCBitSelect	=> TSCBitSelect,		
			TLoad				=> TLoad,	
			BitSetClear		=>	BitSetClear,
			SettingClearing=>	SettingClearing,	
			-- PMAU     
			PCUpdateEn		=> PCUpdateEn,		
			N_PCLoad			=> N_PCLoad,		
			PCControl		=> PCControl,		
			HiLoSel			=> HiLoSel,			
			-- DMAU   
			N_Inc				=> N_Inc,			
			N_OffsetMask	=> N_OffsetMask,	
			PrePostSel		=> PrePostSel		
		);

   -- now generate the stimulus and test the design
	process
	
		-- some useful variables
		
		-- Expected Results
		variable ExpectedResult		:	std_logic_vector(NUM_BITS-1 downto 0);
		variable ExpectedSREG		:	std_logic_vector(NUM_FLAGS-2 downto 0);
		
		-- variable before assigning to IR
		variable IR_var				:	std_logic_vector(INSTR_SIZE-1 downto 0);
		
   begin  -- of stimulus process
			
		for i in 1 to TEST_VECTORS'length loop
			
			-- Retrieve test values from the vector
			IR_var 		:= TEST_VECTORS(i)(test_tuple_length-1 downto test_tuple_length-opcode_word'length);
			OperandA 	<= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-1 downto test_tuple_length-opcode_word'length-NUM_BITS);
			OperandB 	<= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-NUM_BITS-1 downto test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS);
			CarryFlagIn	<= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1);
			TFlagIn 		<= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1-1);
			
			ExpectedResult 	:= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1-1-1 downto test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1-1-NUM_BITS);
			ExpectedSREG		:= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1-1-NUM_BITS-1 downto 0);
			
			-- If not testing BLD/BST commands, manually input a destination
			-- bit so it's not U. (Also needed for undefined IRs)
			if std_match(IR_var(2 downto 0), "UUU") then
				IR_var(2 downto 0) := "000";
			end if;
			
			IR <= IR_var; -- Pass the fixed IR;
			wait for 1 ns;
			
			assert(std_match(result, ExpectedResult))
				report  "Result was wrong on test " & integer'image(i) & "."
				severity  ERROR;
				
			assert(std_match(SREG(SREG'length-2 downto 0), ExpectedSREG))
				report  "Flag computation was wrong from test " & integer'image(i-1) & "."
				severity  ERROR;
		
			wait for CLK_PERIOD - 1 ns; -- One computation per clock (for now)
			
		end loop;
			
      END_SIM <= TRUE;        -- end of stimulus events
		wait;                   -- wait for simulation to end
	end process; -- end of stimulus process

   CLOCK_CLK : process
   begin
		-- this process generates a clock with a CLK_PERIOD period and 50% 
		-- duty cycle. stop the clock when end of simulation is reached
      if END_SIM = FALSE then
          CLK <= '0';
          wait for (CLK_PERIOD/2);
      else
          wait;
      end if;

      if END_SIM = FALSE then
          CLK <= '1';
          wait for (CLK_PERIOD/2);
      else
          wait;
      end if;
	end process;

end TB_ARCHITECTURE;

configuration TESTBENCH_FOR_ALU_tb of ALU_tb is
    for TB_ARCHITECTURE
        for U_ALU : ALU
            use entity work.ALU(data_flow);
        end for;
    end for;
end TESTBENCH_FOR_ALU_tb;
