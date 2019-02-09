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
entity MEMTESTE_tb is
end MEMTESTE_tb;
----------------------------------------------------------------------------------------------------
architecture TB_ARCHITECTURE of MEMTESTE_tb is

    -- Useful types for test vector ################################################################
    constant test_tuple_length  :   integer := 
                                                        (opcode_word'length +   -- Instruction
                                                        DATA_AB_SIZE +          -- ProgDB
                                                        NUM_BITS +              -- DataDB in
                                                        NUM_BITS +              -- DataDB out
                                                        DATA_AB_SIZE +          -- DataAB
                                                        1 +                     -- DataRd
                                                        1                       -- DataWr
                                                        );                    
    type test_tuple is array (natural range <>) of 
                                        std_logic_vector(test_tuple_length-1 downto 0);

    -- Constants ###################################################################################
    
    -- Test Vectors
    -- IR & ProgDB & DataDB in & DataDB out & DataAB & DataRd & DataWr
    constant    TEST_VECTORS    :   test_tuple(1 to 8) :=  (
        "1110011010100000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R26, 0x60
        "1110000010110000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R27, 0x00
        "1110011011000000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R28, 0x60
        "1110000011010001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R29, 0x01
        "1110011011100000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R30, 0x60
        "1110000011110010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R31, 0x02
        "1110101000000101" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R16, 0xa5
        "1110010100011010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1" -- LDI R17, 0x5a
    );
    
    -- Timing Constants ----------------------------------------------------------------------------
    
    -- Clock period
    constant CLK_PERIOD_INT :   integer := 20; -- ns
    constant CLK_PERIOD     :   time    := CLK_PERIOD_INT * (1 ns);

    -- Stimulus Signals
    signal IR               :   std_logic_vector(opcode_word'length-1 downto 0);
    signal ProgDB           :   std_logic_vector(opcode_word'length-1 downto 0);

    -- Observed signals
    signal DataAB           :   std_logic_vector(DATA_AB_SIZE-1 downto 0);
    signal DataRd           :   std_logic;
    signal DataWr           :   std_logic;

    -- Bi-directional
    signal DataDB           :   std_logic_vector(NUM_BITS-1 downto 0);

    -- System clock and reset
    signal CLK              :   std_logic;
    signal reset            :   std_logic;
    
    -- Signal used to stop clock signal generators
    signal END_SIM           :   BOOLEAN := FALSE;

begin -- ###########################################################################################
    -- Unit Under Test port map
    -- Component declaration of the tested unit #################################################### 
    UUT : entity work.MEM_TEST
    port map(
        IR      => IR,
        ProgDB  => ProgDB,
        clock   => CLK, 
        Reset   => reset,
        DataAB  => DataAB, 
        DataRd  => DataRd,
        DataWr  => DataWr
    );

   -- now generate the stimulus and test the design
    process
    
        -- some useful variables
        
        -- Expected Results
        variable ExpectedDataAB     :   std_logic_vector(DATA_AB_SIZE-1 downto 0);
        variable ExpectedDataDB     :   std_logic_vector(NUM_BITS-1 downto 0);
        variable ExpectedDataRd     :   std_logic;
        variable ExpectedDataWr     :   std_logic;
        
    begin  -- of stimulus process
        
        -- Reset the SP for the beginning
        reset <= '0';

        -- And give it a few clocks
        wait for 5 * CLK_PERIOD;

        -- Begin the tests
        reset <= '1';
        for i in 1 to TEST_VECTORS'length loop
            
            -- Retrieve test values from the vector (Gross arithmetic only used here. No need for
            -- constants)
            IR          <= TEST_VECTORS(i)(test_tuple_length-1 downto test_tuple_length-
                            opcode_word'length);
            ProgDB      <= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-1 downto 
                            test_tuple_length-opcode_word'length-DATA_AB_SIZE);
            DataDB      <= TEST_VECTORS(i)(test_tuple_length-opcode_word'length-DATA_AB_SIZE-1 
                            downto test_tuple_length-opcode_word'length-DATA_AB_SIZE-NUM_BITS);

            ExpectedDataDB  := TEST_VECTORS(i)(test_tuple_length-opcode_word'length-DATA_AB_SIZE-
                                NUM_BITS-1 downto test_tuple_length-opcode_word'length-DATA_AB_SIZE-
                                NUM_BITS-NUM_BITS);

            -- Expected outputs
            ExpectedDataAB  := TEST_VECTORS(i)(test_tuple_length-opcode_word'length-DATA_AB_SIZE-
                                NUM_BITS-NUM_BITS-1 downto test_tuple_length-opcode_word'length-
                                DATA_AB_SIZE-NUM_BITS-NUM_BITS-DATA_AB_SIZE);
            ExpectedDataRd  := TEST_VECTORS(i)(1);
            ExpectedDataWr  := TEST_VECTORS(i)(0);

            wait for 1 ns;
            
            assert(std_match(DataAB, ExpectedDataAB))
                report  "DataAB was wrong on test " & integer'image(i) & "."
                severity  ERROR;

            assert(std_match(DataDB, ExpectedDataDB))
                report  "DataDB was wrong on test " & integer'image(i) & "."
                severity  ERROR;

            assert(std_match(DataRd, ExpectedDataRd))
                report  "DataRd was wrong on test " & integer'image(i) & "."
                severity  ERROR;

            assert(std_match(DataWr, ExpectedDataWr))
                report  "DataWr was wrong on test " & integer'image(i) & "."
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
