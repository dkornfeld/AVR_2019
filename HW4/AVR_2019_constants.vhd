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
    
    -- ProgMAU Constants
    constant PC_WIDTH               :   integer := 16;  -- Size of the PC (TBD)
    constant PROG_OFFSET_SIZE       :   integer := 6;   -- Size of offsets to PC (TBD)
    
    -- DataMAU Constants
    constant DATA_OFFSET_SIZE       :   integer := 6;   -- Size of offsets to DataAB (TBD)
    constant DATA_AB_SIZE           :   integer := 16;  -- Size of the DataAB
    
    -- ControlUnit Constants
    constant INSTR_SIZE             :   integer := 16;  -- Size of instructions
    constant MAX_INSTR_CLKS         :   integer := 2;   -- The maximum number of clocks
                                                        -- for an instruction
    
end package;