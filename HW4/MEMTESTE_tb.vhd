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
    constant    TEST_VECTORS    :   test_tuple(1 to 125) :=  (
        "1110000000000000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R16, 0x00
        "1001001100000000" & X"CCCC" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STS R16 0x005F
        "1001001100000000" & X"005F" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STS R16 0x005F
        "1001001100000000" & X"CCCC" & "ZZZZZZZZ" & X"00"      & X"005F"            & "1" & "1",-- STS R16 0x005F
        "1110011010100000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R26, 0x60
        "1110000010110000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R27, 0x00
        "1110011011000000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R28, 0x60
        "1110000011010001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R29, 0x01
        "1110011011100000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R30, 0x60
        "1110000011110010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R31, 0x02
        "1110101000000101" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R16, 0xa5
        "1110010100011010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R17, 0x5a
        "1001001100001100" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R16 X
        "1001001100001100" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"0060"            & "1" & "0",-- ST R16 X
        "1001000100001100" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R16 X
        "1001000100001100" & X"0000" & X"A5"      & "--------" & X"0060"            & "0" & "1",-- LD R16 X
        "1001001100010000" & X"CCCC" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STS R17 0x0001
        "1001001100010000" & X"0001" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STS R17 0x0001
        "1001001100010000" & X"CCCC" & "ZZZZZZZZ" & X"5A"      & X"0001"            & "1" & "1",-- STS R17 0x0001
        "1001000000110000" & X"CCCC" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDS R3 0x001A
        "1001000000110000" & X"001A" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDS R3 0x001A
        "1001000000110000" & X"CCCC" & "ZZZZZZZZ" & X"60"      & X"001A"            & "1" & "1",-- LDS R3 0x001A
        "1001000001010000" & X"CCCC" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDS R5 0x0060
        "1001000001010000" & X"0060" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDS R5 0x0060
        "1001000001010000" & X"CCCC" & X"A5"      & "--------" & X"0060"            & "0" & "1",-- LDS R5 0x0060
        "1001001001010000" & X"CCCC" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STS R5 0x0061
        "1001001001010000" & X"0061" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STS R5 0x0061
        "1001001001010000" & X"CCCC" & "ZZZZZZZZ" & X"A5"      & X"0061"            & "1" & "0",-- STS R5 0x0061
        "0010111001111010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- MOV R7 R26
        "1001001100001111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- PUSH R16
        "1001001100001111" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"FFFF"            & "1" & "0",-- PUSH R16
        "1001001100011111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- PUSH R17
        "1001001100011111" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"FFFE"            & "1" & "0",-- PUSH R17
        "1001000100001111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- POP  R16
        "1001000100001111" & X"0000" & X"5A"      & "--------" & X"FFFE"            & "0" & "1",-- POP  R16
        "1001000100011111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- POP  R17
        "1001000100011111" & X"0000" & X"A5"      & "--------" & X"FFFF"            & "0" & "1",-- POP  R17
        "1001001000110000" & X"CCCC" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STS R3 0x0000
        "1001001000110000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STS R3 0x0000
        "1001001000110000" & X"CCCC" & "ZZZZZZZZ" & X"60"      & X"0000"            & "1" & "1",-- STS R3 0x0000
        "1001000100101111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- POP  R18
        "1001000100101111" & X"0000" & X"60"      & "--------" & X"0000"            & "1" & "1",-- POP  R18
        "1001001000111111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- PUSH R3
        "1001001000111111" & X"0000" & "ZZZZZZZZ" & X"60"      & X"0000"            & "1" & "1",-- PUSH R3
        "1001001100001101" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R16 X+
        "1001001100001101" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"0060"            & "1" & "0",-- ST R16 X+
        "1001001100011101" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R17 X+
        "1001001100011101" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"0061"            & "1" & "0",-- ST R17 X+
        "1001000100001110" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R16 -X
        "1001000100001110" & X"0000" & X"A5"      & "--------" & X"0061"            & "0" & "1",-- LD R16 -X
        "1001000100011110" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R17 -X
        "1001000100011110" & X"0000" & X"5A"      & "--------" & X"0060"            & "0" & "1",-- LD R17 -X
        "1001001100001001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R16 Y+
        "1001001100001001" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"0160"            & "1" & "0",-- ST R16 Y+
        "1001001100011001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R17 Y+
        "1001001100011001" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"0161"            & "1" & "0",-- ST R17 Y+
        "1001000100001010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R16 -Y
        "1001000100001010" & X"0000" & X"5A"      & "--------" & X"0161"            & "0" & "1",-- LD R16 -Y
        "1001000100011010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R17 -Y
        "1001000100011010" & X"0000" & X"A5"      & "--------" & X"0160"            & "0" & "1",-- LD R17 -Y
        "1001001100000001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R16 Z+
        "1001001100000001" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"0260"            & "1" & "0",-- ST R16 Z+
        "1001001100010001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R17 Z+
        "1001001100010001" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"0261"            & "1" & "0",-- ST R17 Z+
        "1001000100000010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R16 -Z
        "1001000100000010" & X"0000" & X"A5"      & "--------" & X"0261"            & "0" & "1",-- LD R16 -Z
        "1001000100010010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R17 -Z
        "1001000100010010" & X"0000" & X"5A"      & "--------" & X"0260"            & "0" & "1",-- LD R17 -Z
        "1000001100001000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STD R16 Y 0x00
        "1000001100001000" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"0160"            & "1" & "0",-- STD R16 Y 0x00
        "1010111100011111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STD R17 Y 0x3F
        "1010111100011111" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"019F"            & "1" & "0",-- STD R17 Y 0x3F
        "1010110100001111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDD R16 Y 0x3F
        "1010110100001111" & X"0000" & X"5A"      & "--------" & X"019F"            & "0" & "1",-- LDD R16 Y 0x3F
        "1000000100011000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDD R17 Y 0x00
        "1000000100011000" & X"0000" & X"A5"      & "--------" & X"0160"            & "0" & "1",-- LDD R17 Y 0x00
        "1000001100000000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STD R16 Z 0x00
        "1000001100000000" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"0260"            & "1" & "0",-- STD R16 Z 0x00
        "1010111100010111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STD R17 Z 0x3F
        "1010111100010111" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"029F"            & "1" & "0",-- STD R17 Z 0x3F
        "1010110100000111" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDD R16 Z 0x3F
        "1010110100000111" & X"0000" & X"A5"      & "--------" & X"029F"            & "0" & "1",-- LDD R16 Z 0x3F
        "1000000100010000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDD R17 Z 0x00
        "1000000100010000" & X"0000" & X"5A"      & "--------" & X"0260"            & "0" & "1",-- LDD R17 Z 0x00
        "1110000010101000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R26, 0x08
        "1110000010110000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R27, 0x00
        "1110000011001010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R28, 0x0A
        "1110000011010000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R29, 0x00
        "1110000011101110" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R30, 0x0E
        "1110000011110000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R31, 0x00
        "1001001100001100" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R16 X
        "1001001100001100" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"0008"            & "1" & "1",-- ST R16 X
        "1001000100001100" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R16 X
        "1001000100001100" & X"0000" & X"A5"      & "--------" & X"0008"            & "1" & "1",-- LD R16 X
        "1110101000010101" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R17, 0xa5
        "1110010100001010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R16, 0x5a
        "1001001100001101" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R16 X+
        "1001001100001101" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"0008"            & "1" & "1",-- ST R16 X+
        "1001001100011101" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R17 X+
        "1001001100011101" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"0009"            & "1" & "1",-- ST R17 X+
        "1001000100001110" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R16 -X
        "1001000100001110" & X"0000" & X"A5"      & "--------" & X"0009"            & "1" & "1",-- LD R16 -X
        "1001000100011110" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R17 -X
        "1001000100011110" & X"0000" & X"5A"      & "--------" & X"0008"            & "1" & "1",-- LD R17 -X
        "1001001100001001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R16 Y+
        "1001001100001001" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"000A"            & "1" & "1",-- ST R16 Y+
        "1001001100011001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R17 Y+
        "1001001100011001" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"000B"            & "1" & "1",-- ST R17 Y+
        "1001000100001010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R16 -Y
        "1001000100001010" & X"0000" & X"5A"      & "--------" & X"000B"            & "1" & "1",-- LD R16 -Y
        "1001000100011010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R17 -Y
        "1001000100011010" & X"0000" & X"A5"      & "--------" & X"000A"            & "1" & "1",-- LD R17 -Y
        "1001001100000001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R16 Z+
        "1001001100000001" & X"0000" & "ZZZZZZZZ" & X"5A"      & X"000E"            & "1" & "1",-- ST R16 Z+
        "1001001100010001" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- ST R17 Z+
        "1001001100010001" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"000F"            & "1" & "1",-- ST R17 Z+
        "1001000100000010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R16 -Z
        "1001000100000010" & X"0000" & X"A5"      & "--------" & X"000F"            & "1" & "1",-- LD R16 -Z
        "1001000100010010" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LD R17 -Z
        "1001000100010010" & X"0000" & X"5A"      & "--------" & X"000E"            & "1" & "1",-- LD R17 -Z
        "1110000001000000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDI R20, 0x00
        "1000101101001100" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- STD R20 Y 0x14
        "1000101101001100" & X"0000" & "ZZZZZZZZ" & X"00"      & X"001E"            & "1" & "1",-- SDD R20 Y 0x14
        "1000100101000000" & X"0000" & "ZZZZZZZZ" & "--------" & "----------------" & "1" & "1",-- LDD R20 Z 0x10
        "1000100101000000" & X"0000" & "ZZZZZZZZ" & X"A5"      & X"0010"            & "1" & "1" -- LDD R20 Z 0x10
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
        DataDB  => DataDB,
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
        for i in 1 to TEST_VECTORS'length loop

            wait for 1 ns;
            reset <= '1'; -- not resetting
            
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

            -- First, read and write should both be high

            assert(std_match(DataRd, '1'))
                report  "DataRd went low too early in test " & integer'image(i) & "."
                severity  ERROR;

            assert(std_match(DataWr, '1'))
                report  "DataWr went low too early in test " & integer'image(i) & "."
                severity  ERROR;

            -- Then, see what they should actually be doing
            wait for CLK_PERIOD/2;

            assert(std_match(DataRd, ExpectedDataRd))
                report  "DataRd was wrong on test " & integer'image(i) & "."
                severity  ERROR;

            assert(std_match(DataWr, ExpectedDataWr))
                report  "DataWr was wrong on test " & integer'image(i) & "."
                severity  ERROR;
        
            wait for CLK_PERIOD/2 -2 ns; -- One computation per clock (for now)
            
        end loop;
            
        END_SIM <= TRUE;        -- end of stimulus events
        wait;                   -- wait for simulation to end
    end process; -- end of stimulus process

    CLOCK_CLK : process
    begin
        -- this process generates a clock with a CLK_PERIOD period and 50% 
        -- duty cycle. stop the clock when end of simulation is reached
        if END_SIM = FALSE then
            CLK <= '1';
            wait for (CLK_PERIOD/2);
        else
            wait;
        end if;

        if END_SIM = FALSE then
            CLK <= '0';
            wait for (CLK_PERIOD/2);
        else
            wait;
        end if;
    end process;
end TB_ARCHITECTURE;
