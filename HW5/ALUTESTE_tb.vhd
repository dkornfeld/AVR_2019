-- Title:   Test Bench for ALU and SREG
-- Author:  David Kornfeld and Bobby Abrahamson
--
-- This file serves as a testbench for the ALU unit as it is implemented for grading testing. The 
-- results are compared against a set of hand-written test-vectors. This uses the tests from the
-- ALU testbench, but delays the flag checks by a clock to check the functionality of the SREG. 
-- Additionally, all pre-set flag cases are converted to BSET/BCLR commands.
--
--
-- Revision History:
--      02/05/19    David Kornfeld      Copied from ALU_tb
--      02/05/19    David Kornfeld      Modified for registered flags
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AVR_2019_constants.all;
use work.opcodes.all;
----------------------------------------------------------------------------------------------------
entity ALUTESTE_tb is
end ALUTESTE_tb;
----------------------------------------------------------------------------------------------------
architecture TB_ARCHITECTURE of ALUTESTE_tb is

    -- Useful types for test vector ################################################################
    constant test_tuple_length  :   integer := 
                                                        (opcode_word'length +   -- Instruction
                                                        NUM_BITS +              -- OperandA
                                                        NUM_BITS +              -- OperandB
                                                        NUM_BITS +              -- Result
                                                        NUM_FLAGS-1);           -- Flags
    type test_tuple is array (natural range <>) of 
                                        std_logic_vector(test_tuple_length-1 downto 0);

    -- Constants ###################################################################################
    
    -- Test Vectors (copied directly from ALU_tb and modified with BSETs/BLCRs)
    -- (Instruction, A, B, Result, ExpectedNewFlags)
    constant    TEST_VECTORS    :   test_tuple(1 to 91) :=  (
        -- Quick toggle of the interrupt flag to see it working and give it a defined value
        "1001010001111000"  & X"00" & X"A3" & "--------"& "-------", -- BSET 7
        "1001010011111000"  & X"FF" & X"A3" & "--------"& "-------", -- BCLR 7
        -- Quick toggle of the T flag to see it working and give it a defined value
        "1001010001101000"  & X"00" & X"A3" & "--------"& "-------", -- BSET 6
        "1001010011101000"  & X"FF" & X"A3" & "--------"& "1------", -- BCLR 6
        OpADD               & X"FF" & X"00" & X"FF"     & "0------",
        OpADD               & X"0F" & X"01" & X"10"     & "-010100",
        "1001010000001000"  & X"00" & X"A3" & "--------"& "-100000", -- BSET 0
        OpADC               & X"FF" & X"00" & X"00"     & "-------",
        "1001010010001000"  & X"FF" & X"A3" & "--------"& "-100011", -- BCLR 0
        OpADC               & X"FF" & X"00" & X"FF"     & "-------",
        OpAND               & X"FF" & X"55" & X"55"     & "-010100",
        OpAND               & X"FF" & X"00" & X"00"     & "--0000-",
        OpANDI              & X"FF" & X"00" & X"00"     & "--0001-",
        OpASR               & X"AA" & X"00" & X"D5"     & "--0001-",
        OpCOM               & X"AA" & X"00" & X"55"     & "--01100",
        OpCOM               & X"FF" & X"00" & X"00"     & "--00001",
        OpCP                & X"00" & X"AA" & X"56"     & "--00011",
        "1001010000001000"  & X"00" & X"A3" & "--------"& "-100001", -- BSET 0
        OpCPC               & X"00" & X"AA" & X"55"     & "------1",
        "1001010010001000"  & X"FF" & X"A3" & "--------"& "-100001", -- BCLR 0
        OpCPC               & X"00" & X"AA" & X"56"     & "------0",
        OpCP                & X"AA" & X"00" & X"AA"     & "-100001",
        OPCPC               & X"FF" & X"FF" & X"00"     & "-010100",
        OpCPI               & X"00" & X"AA" & X"56"     & "-000000",
        OpSUB               & X"00" & X"AA" & X"56"     & "-100001",
        "1001010000001000"  & X"00" & X"A3" & "--------"& "-100001", -- BSET 0
        OpSBC               & X"00" & X"AA" & X"55"     & "------1",
        "1001010010001000"  & X"FF" & X"A3" & "--------"& "-100001", -- BCLR 0
        OpSBC               & X"00" & X"AA" & X"56"     & "------0",
        OpSUBI              & X"00" & X"AA" & X"56"     & "-100001",
        OpDEC               & X"A5" & X"FF" & X"A4"     & "-100001",
        OpDEC               & X"00" & X"FF" & X"FF"     & "--1010-",
        OpEOR               & X"AA" & X"55" & X"FF"     & "--1010-",
        OpEOR               & X"FF" & X"FF" & X"00"     & "--1010-",
        OpINC               & X"A4" & X"FF" & X"A5"     & "--0001-",
        OpINC               & X"FF" & X"FF" & X"00"     & "--1010-",
        OpLSR               & X"AA" & X"55" & X"55"     & "--0001-",
        OpLSR               & X"55" & X"55" & X"2A"     & "--00000",
        OpNEG               & X"FF" & X"55" & X"01"     & "--11001",
        OpNEG               & X"55" & X"FF" & X"AB"     & "-100001",
        OpNEG               & X"00" & X"FF" & X"00"     & "-110101",
        OpNEG               & X"80" & X"80" & X"80"     & "-000010",
        OpOR                & X"AA" & X"55" & X"FF"     & "-001101",
        OpOR                & X"00" & X"00" & X"00"     & "--1010-",
        OpORI               & X"AA" & X"55" & X"FF"     & "--0001-",
        OpORI               & X"00" & X"00" & X"00"     & "--1010-",
        "1001010010001000"  & X"FF" & X"A3" & "--------"& "--0001-", -- BCLR 0
        OpROR               & X"AA" & X"00" & X"55"     & "-------",
        "1001010000001000"  & X"00" & X"A3" & "--------"& "--00000", -- BSET 0
        OpROR               & X"AA" & X"00" & X"D5"     & "-------",
        "1001010000001000"  & X"00" & X"A3" & "--------"& "--01100", -- BSET 0
        OpROR               & X"55" & X"55" & X"AA"     & "-------",
        OpSWAP              & X"A3" & X"FF" & X"3A"     & "--10101",
        OpSWAP              & X"00" & X"FF" & X"00"     & "--10101",
        "1001010000111000"  & X"00" & X"A3" & "--------"& "--10101", -- BSET 3
        "1001010011011000"  & X"FF" & X"A3" & "--------"& "--11101", -- BCLR 5
        "1111101000000011"  & X"08" & X"A3" & X"08"     & "-011101", -- BST 0x08 3
        "1111101000000100"  & X"08" & X"A3" & X"08"     & "1------", -- BST 0x08 4
        "1001010001101000"  & X"00" & X"A3" & "--------"& "0------", -- BSET 6
        "1111100000000101"  & X"00" & X"A3" & X"20"     & "1------", -- BLD 0x00 5
        "1001010011101000"  & X"FF" & X"A3" & "--------"& "-------", -- BCLR 6
        "1111100000000011"  & X"08" & X"A3" & X"00"     & "0------", -- BLD 0x08 3
        OpADIW              & X"FE" & X"02" & X"00"     & "-------", -- ADIW 55FE, 2;
        OpADIW              & X"55" & X"02" & X"56"     & "--00011", -- ADIW 55FE, 2;
        OpSBIW              & X"00" & X"3F" & X"C1"     & "--00000", -- SBIW 0100, 3F;
        OpSBIW              & X"01" & X"3F" & X"00"     & "-------", -- SBIW 0100, 3F;
        OpSBIW              & X"FF" & X"3F" & X"C0"     & "--00000", -- SBIW FFFF, 3F;
        OpSBIW              & X"FF" & X"BB" & X"FF"     & "-010100", -- SBIW FFFF, 3F;
        OpSBIW              & X"00" & X"01" & X"FF"     & "-010100", -- SBIW 0000, 01;
        OpSBIW              & X"00" & X"AA" & X"FF"     & "-010101", -- SBIW 0000, 01;
        OpSBIW              & X"10" & X"10" & X"00"     & "-010101", -- SBIW 0010, 10;
        OpSBIW              & X"00" & X"AA" & X"00"     & "-000010", -- SBIW 0010, 10;
        OpSBIW              & X"10" & X"00" & X"10"     & "-000010", -- SBIW 0010, 00;
        OpSBIW              & X"00" & X"AA" & X"00"     & "-000000", -- SBIW 0010, 00;
        OpSBIW              & X"00" & X"00" & X"00"     & "-000000", -- SBIW 0000, 00;
        OpSBIW              & X"00" & X"00" & X"00"     & "-000010", -- SBIW 0000, 00;
        OpMUL               & X"02" & X"10" & X"20"     & "-000010", -- MUL 02, 10;
        OpMUL               & X"02" & X"10" & X"00"     & "-----00", -- MUL 02, 10;
        OpMUL               & X"FF" & X"FF" & X"01"     & "-----00", -- MUL FF, FF;
        OpMUL               & X"FF" & X"FF" & X"FE"     & "-----00", -- MUL FF, FF;
        OpMUL               & X"FF" & X"01" & X"FF"     & "-----01", -- MUL FF, 01;
        OpMUL               & X"FF" & X"01" & X"00"     & "-----01", -- MUL FF, 01;
        OpMUL               & X"00" & X"00" & X"00"     & "-----00", -- MUL 00, 00;
        OpMUL               & X"00" & X"00" & X"00"     & "-----10", -- MUL 00, 00;
        OpMUL               & X"FF" & X"00" & X"00"     & "-----10", -- MUL FF, 00;
        OpMUL               & X"FF" & X"00" & X"00"     & "-----10", -- MUL FF, 00;
        OpMUL               & X"5A" & X"02" & X"B4"     & "-----10", -- MUL 5A, 02;
        OpMUL               & X"5A" & X"02" & X"00"     & "-----01", -- MUL 5A, 02;
        OpMUL               & X"02" & X"80" & X"00"     & "-----00", -- MUL 02, 80;
        OpMUL               & X"02" & X"80" & X"01"     & "-----10", -- MUL 02, 80;
        "----------------"  & "--------" & "--------" & "--------" & "-----00" -- Check flags
    );
    
    -- Timing Constants ----------------------------------------------------------------------------
    
    -- Clock period
    constant CLK_PERIOD_INT :   integer := 20; -- ns
    constant CLK_PERIOD     :   time    := CLK_PERIOD_INT * (1 ns);

    -- Stimulus Signals
    signal IR               :   std_logic_vector(opcode_word'length-1 downto 0);
    signal OperandA         :   std_logic_vector(NUM_BITS-1 downto 0);
    signal OperandB         :   std_logic_vector(NUM_BITS-1 downto 0);

    -- Observed signals
    signal Result           :   std_logic_vector(NUM_BITS-1 downto 0);
    signal StatReg          :   std_logic_vector(NUM_FLAGS-1 downto 0);

    -- System clock
    signal CLK              :   std_logic;
    
   -- Signal used to stop clock signal generators
   signal END_SIM               :   BOOLEAN := FALSE;

begin -- ###########################################################################################
    -- Unit Under Test port map
    -- Component declaration of the tested unit #################################################### 
    UUT : entity work.ALU_TEST
    port map(
        IR        => IR,
        OperandA  => OperandA,
        OperandB  => OperandB, 
        clock     => CLK, 
        Result    => Result,
        StatReg   => StatReg
    );

   -- now generate the stimulus and test the design
    process
    
        -- some useful variables
        
        -- Expected Results
        variable ExpectedResult     :   std_logic_vector(NUM_BITS-1 downto 0);
        variable ExpectedSREG       :   std_logic_vector(NUM_FLAGS-2 downto 0);
        
        -- variable before assigning to IR
        variable IR_var             :   std_logic_vector(INSTR_SIZE-1 downto 0);
        
    begin  -- of stimulus process
        
        for i in 1 to TEST_VECTORS'length loop
            
            -- Retrieve test values from the vector (Gross arithmetic only used here. No need for
            -- constants)
            IR_var      := TEST_VECTORS(i)(test_tuple_length-1 downto test_tuple_length-
                            opcode_word'length);
            OperandA    <= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-1 downto 
                            test_tuple_length-opcode_word'length-NUM_BITS);
            OperandB    <= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-NUM_BITS-1 downto 
                            test_tuple_length-opcode_word'length-NUM_BITS-NUM_BITS);

            -- Expected outputs
            ExpectedResult  := TEST_VECTORS(i)(test_tuple_length-opcode_word'length-NUM_BITS-
                            NUM_BITS-1 downto test_tuple_length-opcode_word'length-NUM_BITS-
                            NUM_BITS-NUM_BITS);
            ExpectedSREG    := TEST_VECTORS(i)(test_tuple_length-opcode_word'length-NUM_BITS-
                            NUM_BITS-NUM_BITS-1 downto 0);
            
            -- If not testing BLD/BST commands, manually input a destination
            -- bit so it's not U. (Also needed for undefined IRs)
            if std_match(IR_var(2 downto 0), "UUU") then
                IR_var(2 downto 0) := "000";
            end if;
            
            IR <= IR_var; -- Pass the fixed IR;
            wait for 1 ns;
            
            assert(std_match(Result, ExpectedResult))
                report  "Result was wrong on test " & integer'image(i) & "."
                severity  ERROR;
                
            assert(std_match(StatReg(StatReg'length-2 downto 0), ExpectedSREG))
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
