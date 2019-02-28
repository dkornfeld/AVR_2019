-- Title:   AVR_2019_Constants
-- Author:  David Kornfeld and Bobby Abrahamson
--
-- This is a package file containing hardware constants for use with the
-- AVR_2019 processor designed by Bobby Abrahamson and David Kornfeld. It
-- includes all the necessary constants that are used to orient the CPU
-- modules with each other. 
--
-- Revision History:
--      01/24/19    David Kornfeld      Initial Revision
--      01/31/19    David Kornfeld      Added constants for ControlUnit FSM
--      02/08/19    David Kornfeld      Added constants for control in ControlUnit
--      02/08/19    David Kornfeld      Moved constants for ALU
--      02/09/19    David Kornfeld      Changed maximum instruction clocks and updated documentation
--      02/20/19    David Kornfeld      Fixed ProgMAU constants 
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package AVR_2019_constants is
    -- General Constants ###########################################################################
    constant NUM_BITS               :   integer := 8;   -- Width of Data Data bus
    
    -- ProgMAU Constants ###########################################################################
    constant PC_WIDTH               :   integer := 16;  -- Size of the PC
    
    -- DataMAU Constants ###########################################################################
    constant DATA_OFFSET_SIZE       :   integer := 6;   -- Size of offsets to DataAB
    constant DATA_AB_SIZE           :   integer := 16;  -- Size of the DataAB
    
    -- ControlUnit Constants #######################################################################
    constant INSTR_SIZE             :   integer := 16;  -- Size of instructions
    constant MAX_INSTR_CLKS         :   integer := 4;   -- The maximum number of clocks
                                                        -- for an instruction
    
    -- Register Constants ##########################################################################
    constant NUM_GPRS               :   integer := 32;  -- Number of general purpose registers
    constant NUM_IORS               :   integer := 64;  -- Number of IO registers
    constant NUM_REGS               :   integer := 96;  -- Number of registers
    constant NUM_FLAGS              :   integer := 8;   -- Size of the SREG

    constant ADDR_REG_SEL_X         :   std_logic_vector(1 downto 0) := "11"; -- Select X
    constant ADDR_REG_SEL_Y         :   std_logic_vector(1 downto 0) := "10"; -- Select Y
    constant ADDR_REG_SEL_Z         :   std_logic_vector(1 downto 0) := "00"; -- Select Z
    constant ADDR_REG_SEL_SP        :   std_logic_vector(1 downto 0) := "01"; -- Select SP

    -- Constants useful for flag masks (1 indicates flag is changed)
    constant FLAGS_ALL    : std_logic_vector(NUM_FLAGS-1 downto 0) := "01111111";
    constant FLAGS_ZCNVSH : std_logic_vector(NUM_FLAGS-1 downto 0) := "00111111";
    constant FLAGS_ZCNVS  : std_logic_vector(NUM_FLAGS-1 downto 0) := "00011111";
    constant FLAGS_ZNVS   : std_logic_vector(NUM_FLAGS-1 downto 0) := "00011110";
    constant FLAGS_T      : std_logic_vector(NUM_FLAGS-1 downto 0) := "01000000";
    constant FLAGS_C      : std_logic_vector(NUM_FLAGS-1 downto 0) := "00000001";
    constant FLAGS_ZC     : std_logic_vector(NUM_FLAGS-1 downto 0) := "00000011";
    constant FLAGS_NONE   : std_logic_vector(NUM_FLAGS-1 downto 0) := "00000000";

    -- ALU Constants ###############################################################################
    -- Logical FBLOCK constants
    constant ALU_FSR_ZEROS       :   std_logic_vector(3 downto 0) := "0000";
    constant ALU_FSR_A_NOR_B     :   std_logic_vector(3 downto 0) := "0001";
    constant ALU_FSR_A           :   std_logic_vector(3 downto 0) := "1100";
    constant ALU_FSR_B           :   std_logic_vector(3 downto 0) := "1010";
    constant ALU_FSR_NOT_A       :   std_logic_vector(3 downto 0) := "0011";
    constant ALU_FSR_NOT_B       :   std_logic_vector(3 downto 0) := "0101";
    constant ALU_FSR_A_XOR_B     :   std_logic_vector(3 downto 0) := "0110";
    constant ALU_FSR_A_AND_B     :   std_logic_vector(3 downto 0) := "1000";
    constant ALU_FSR_A_NAND_B    :   std_logic_vector(3 downto 0) := "0111";
    constant ALU_FSR_A_XNOR_B    :   std_logic_vector(3 downto 0) := "1001";
    constant ALU_FSR_A_OR_B      :   std_logic_vector(3 downto 0) := "1110";
    constant ALU_FSR_ONES        :   std_logic_vector(3 downto 0) := "1111";

    -- Carry in constants
    constant CARRY_IN_ZERO       :   std_logic_vector(1 downto 0) := "00";
    constant CARRY_IN_CFLAG      :   std_logic_vector(1 downto 0) := "10";
    constant CARRY_IN_NCFLAG     :   std_logic_vector(1 downto 0) := "11";
    constant CARRY_IN_ONE        :   std_logic_vector(1 downto 0) := "01";

    -- Shifter/Rotator constants
    constant ALU_FSR_ROR         :   std_logic_vector(3 downto 0) := ALU_FSR_A_NAND_B;
    constant ALU_FSR_SWAP        :   std_logic_vector(3 downto 0) := "0010";
    constant ALU_FSR_ASR         :   std_logic_vector(3 downto 0) := ALU_FSR_NOT_B;
    constant ALU_FSR_LSR         :   std_logic_vector(3 downto 0) := ALU_FSR_A_XOR_B;

    -- PC update select constants
    constant PC_UPDATE_ONE       :   std_logic_vector(2 downto 0) := "000";
    constant PC_UPDATE_TWO       :   std_logic_vector(2 downto 0) := "001";
    constant PC_UPDATE_OFFSET    :   std_logic_vector(2 downto 0) := "010";
    constant PC_UPDATE_PROGDB    :   std_logic_vector(2 downto 0) := "011";
    constant PC_UPDATE_DATADB    :   std_logic_vector(2 downto 0) := "100";
    constant PC_UPDATE_REGZ      :   std_logic_vector(2 downto 0) := "101";


    -- Constants for Addr Register readability
    constant X_LOW      :    integer := 26;
    constant X_HIGH     :    integer := 27;
    constant Y_LOW      :    integer := 28;
    constant Y_HIGH     :    integer := 29;
    constant Z_LOW      :    integer := 30;
    constant Z_HIGH     :    integer := 31;
    constant SP_LOW     :    integer := 93;
    constant SP_HIGH    :    integer := 94;
    
    -- Constant for specifying the SREG
    constant SREG_IDX     :    integer := 95;
    constant SREG_IDX_SEL :    std_logic_vector(6 downto 0) := std_logic_vector(to_unsigned(SREG_IDX, 7));

    -- Constants for Flags array readability
    constant FLAG_T    :    integer := 6;
    constant FLAG_H    :    integer := 5;
    constant FLAG_S    :    integer := 4;
    constant FLAG_V    :    integer := 3;
    constant FLAG_N    :    integer := 2;
    constant FLAG_Z    :    integer := 1;
    constant FLAG_C    :    integer := 0;

end package;