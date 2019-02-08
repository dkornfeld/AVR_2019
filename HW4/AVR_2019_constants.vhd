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

package AVR_2019_constants is
    -- General Constants
    constant NUM_BITS               :   integer := 8;   -- Width of Data Data bus
    
    -- ALU Constants
    constant NUM_FLAGS              :   integer := 8;   -- Size of the SREG

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

    -- non-combinational FBLOCK constants
    constant ALU_FSR_ROR         :   std_logic_vector(3 downto 0) := ALU_FSR_A_NAND_B;
    constant ALU_FSR_SWAP        :   std_logic_vector(3 downto 0) := "0010";
    constant ALU_FSR_ASR         :   std_logic_vector(3 downto 0) := ALU_FSR_NOT_B;
    constant ALU_FSR_LSR         :   std_logic_vector(3 downto 0) := ALU_FSR_A_XOR_B;

    
    -- ProgMAU Constants
    constant PC_WIDTH               :   integer := 16;  -- Size of the PC (TBD)
    constant PROG_OFFSET_SIZE       :   integer := 6;   -- Size of offsets to PC (TBD)
    
    -- DataMAU Constants
    constant DATA_OFFSET_SIZE       :   integer := 6;   -- Size of offsets to DataAB (TBD)
    constant DATA_AB_SIZE           :   integer := 16;  -- Size of the DataAB
    
    -- ControlUnit Constants
    constant INSTR_SIZE             :   integer := 16;  -- Size of instructions
    constant MAX_INSTR_CLKS         :   integer := 3;   -- The maximum number of clocks
                                                        -- for an instruction
    
    -- Registers Constants.
    constant NUM_GPRS               :   integer := 32;  -- Number of general purpose registers
    constant NUM_IORS               :   integer := 64;  -- Number of IO registers
    constant NUM_REGS               :   integer := 96;  -- Number of registers

    constant ADDR_REG_SEL_X         :   std_logic_vector(1 downto 0) := "11"; -- Select X
    constant ADDR_REG_SEL_Y         :   std_logic_vector(1 downto 0) := "10"; -- Select Y
    constant ADDR_REG_SEL_Z         :   std_logic_vector(1 downto 0) := "01"; -- Select Z
    constant ADDR_REG_SEL_SP        :   std_logic_vector(1 downto 0) := "00"; -- Select SP

end package;