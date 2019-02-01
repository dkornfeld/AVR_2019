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
														NUM_FLAGS);				 -- Flags
	type test_tuple is array (natural range <>) of 
										std_logic_vector(test_tuple_length-1 downto 0);

	-- Constants ############################################################
	
	-- Test Vectors
	-- (Instruction, A, B, CarryFlagIn, TFlagIn, Result, ExpectedNewFlags)
	constant	TEST_VECTORS	:	test_tuple(1 to 2) :=	(
		"1111111111111111" & "11111111" & "11111111" & "00" & "11111111" & "00000000",
		"1111111111111111" & "11111111" & "11111111" & "00" & "11111111" & "00000000"
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
			FlagMask			:	in		std_logic_vector(NUM_FLAGS-1 downto 0);
			TBitSelect		:	in		std_logic_vector(2 downto 0);
			TLoad				:	in		std_logic;
			Result      	:	out	std_logic_vector(NUM_BITS-1 downto 0);
			NewFlags			:	out	std_logic_vector(NUM_FLAGS-2 downto 0)
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
	end  component;
	
	-- Unused Signals #######################################################
	-- ControlUnit
	-- General
	signal SREG      		:	std_logic_vector(NUM_FLAGS-1 downto 0);
	signal ProgDB			:	std_logic_vector(INSTR_SIZE-1 downto 0);
	signal DataRd			:	std_logic;
	signal DataWr			:	std_logic;
	signal IOSel			:	std_logic;
	signal RegInSel		:	std_logic;
	signal OPBInSel		:	std_logic;
	-- RegArray
	signal DBSel			:	std_logic_vector(1 downto 0);
	signal RegASel			:	std_logic_vector(4 downto 0);
	signal RegBSel			:	std_logic_vector(4 downto 0);
	signal RegWrSel		:	std_logic_vector(4 downto 0);
	signal RegWr			:	std_logic;
	signal AddrDataIn		:	std_logic_vector(2*NUM_BITS-1 downto 0);
	signal AddrRegSel		:	std_logic_vector(1 downto 0);
	signal AddrRegWrSel	:	std_logic_vector(1 downto 0);
	-- ALU
	signal CarryFlagUnused		:	std_logic;
	signal TFlagUnused			:	std_logic;
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
	signal N_AddMask			:	std_logic;
	signal FSRControl			:	std_logic_vector(3 downto 0);
	signal Subtract			:	std_logic;
	signal CarryInControl	:	std_logic_vector(1 downto 0);
	signal ALUResultSel		:	std_logic;
	signal TBitSelect			:	std_logic_vector(2 downto 0);
	signal TLoad				:	std_logic;
	signal FlagMask			:	std_logic_vector(NUM_FLAGS-1 downto 0);
	
	-- Output signals - signals mapped to the tested entity ports
	signal Result      		:	std_logic_vector(NUM_BITS-1 downto 0);
	signal NewFlags			:	std_logic_vector(NUM_FLAGS-2 downto 0);
	
	-- System clock
	signal CLK     			:	std_logic;
	
   -- Signal used to stop clock signal generators
   signal END_SIM				:	BOOLEAN := FALSE;

begin -- ###################################################################
	-- Unit Under Test port map
	UUT : ALU
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
			FlagMask			=> FlagMask,
			TBitSelect		=> TBitSelect,
			TLoad				=> TLoad,
			Result      	=> Result,
			NewFlags			=> NewFlags
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
			-- RegArray             
			DBSel				=> DBSel,			
			RegASel			=> RegASel,			
			RegBSel			=> RegBSel,			
			RegWrSel			=> RegWrSel,		
			RegWr				=> RegWr,			
			AddrDataIn		=> AddrDataIn,		
			AddrRegSel		=> AddrRegSel,		
			AddrRegWrSel	=> AddrRegWrSel,	
			-- ALU         
			N_AddMask		=> N_AddMask,		
			FSRControl		=> FSRControl,		
			Subtract			=> Subtract,		
			CarryInControl	=> CarryInControl,	
			ALUResultSel	=> ALUResultSel,
			CarryFlag		=> CarryFlagUnused,		
			TFlag				=> TFlagUnused,		
			TBitSelect		=> TBitSelect,		
			TLoad				=> TLoad,			
			FlagMask			=> FlagMask,		
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
		
		-- Integer forms of the test objects
		variable ExpectedResult		:	std_logic_vector(NUM_BITS-1 downto 0);
		variable ExpectedNewFlags	:	std_logic_vector(NUM_FLAGS-1 downto 0);
		
   begin  -- of stimulus process
			
		for i in 1 to TEST_VECTORS'length loop
			
			-- Retrieve test values from the vector
			IR		 		<= TEST_VECTORS(test_tuple_length-1 downto test_tuple_length-opcode_word'length);
			OperandA 	<= TEST_VECTORS(test_tuple_length-opcode_word'length-1 downto test_tuple_length-opcode_word'length-NUM_BITS);
			OperandB 	<= TEST_VECTORS(test_tuple_length-opcode_word'length-NUM_BITS-1 downto test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS);
			CarryFlagIn	<= TEST_VECTORS(test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1);
			TFlagIn 		<= TEST_VECTORS(test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1-1);
			
			ExpectedResult 	:= TEST_VECTORS(test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1-1-1 downto test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1-1-NUM_BITS);
			ExpectedNewFlags	:= TEST_VECTORS(test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS-1-1-NUM_BITS-1 downto 0);
			
			assert(std_match(result, ExpectedResult))
				report  "Result was wrong."
				severity  ERROR;
				
			assert(std_match(NewFlags, ExpectedNewFlags))
				report  "Flag computation was wrong."
				severity  ERROR;
		
			wait for CLK_PERIOD; -- One computation per clock (for now)
			
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

configuration TESTBENCH_FOR_ALU_tb of SD_tb is
    for TB_ARCHITECTURE
        for UUT : ALU
            use entity work.ALU(data_flow);
        end for;
    end for;
end TESTBENCH_FOR_ALU_tb;
