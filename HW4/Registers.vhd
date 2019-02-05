----------------------------------------------------------------------------------------
-- Author:           David Kornfeld and Bobby Abrahamson
-- Title:            Registers
-- Description:      This file implements the Registers for the AVR_2019 CPU designed
--                   by Bobby Abrahamson and David Kornfeld.
--
--    Parameters:
--        NUM_BITS   (integer range 2 to Infinity) -  The number of bits used to represent the
--                                                                numbers. 
--
-- Inputs:
--        clock            (std_logic)                                  - Input clock
--        NewFlags         (std_logic_vector(NUM_FLAGS-2 downto 0))     - New flags to SREG
--        Control Signals: #################################################################
--        RegASel          (std_logic_vector(Log2(NUM_REG)-1 downto 0)) - Register A Select lines
--        RegBSel          (std_logic_vector(Log2(NUM_REG)-1 downto 0)) - Register B Select lines
--        RegWrSel         (std_logic_vector(Log2(NUM_REG)-1 downto 0)) - Reg Enable decoder lines
--        RegWr            (std_logic)                                  - Reg (write) Enable
--        RegIn            (std_logic_vector(NUM_BITS-1 downto 0))      - Input register data
--        SFlag            (std_logic)                                  - Indicate if directly writing SREG
--        FlagMask         (std_logic_vector(NUM_FLAGS-1 downto 0))     - Bitmask of flags to update
--        
-- Outputs:
--        RegAOutput       (std_logic_vector(NUM_BITS-1 downto 0)       - Output register A
--        RegBOutput       (std_logic_vector(NUM_BITS-1 downto 0)       - Output register B
--        SREG             (std_logic_vector(NUM_BITS-1 downto 0)       - Output SREG
--
-- Revision History:
--        01/30/19    Bobby Abrahamson        Initial Revision
--        01/31/19    Bobby Abrahamson        Implemented first code from block diagram
--        02/02/19    Bobby Abrahamson        Updated to output SREG for HW3
--        02/05/19    David Kornfeld          Fixed Interrupt flag sensitivity
-----------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.AVR_2019_constants.all;
-----------------------------------------------------------------------------------------
entity Registers is
    generic (
        NUM_BITS    : integer := NUM_BITS                        -- Number of bits to use for a GPR.
    );
    port (
        clock       : in std_logic;                              -- input clock
        RegWr       : in std_logic;                              -- select write vs not write
        RegWrSel    : in std_logic_vector(4 downto 0);           -- select register to write to
        RegASel     : in std_logic_vector(4 downto 0);           -- select register A to read from
        RegBSel     : in std_logic_vector(4 downto 0);           -- select register B to read from
        RegIn       : in std_logic_vector(NUM_BITS-1 downto 0);  -- Input data to write
        
        -- SREG
        SFlag       : in std_logic;                              -- Select direct overwrite of SREG
        FlagMask    : in std_logic_vector(NUM_BITS-1 downto 0);  -- Bitmask to update in SREG
        NewFlags    : in std_logic_vector(NUM_FLAGS-2 downto 0); -- New bits for SREG
        
        RegAOutput  : out std_logic_vector(NUM_BITS-1 downto 0); -- Output register A
        RegBOutput  : out std_logic_vector(NUM_BITS-1 downto 0); -- Output register B
        SREG        : out std_logic_vector(NUM_BITS-1 downto 0)  -- Output SREG
    );
end Registers;

architecture data_flow of Registers is
    -- Constants for Addr Register readability
    constant X_LOW      :    integer := 26;
    constant X_HIGH     :    integer := 27;
    constant Y_LOW      :    integer := 28;
    constant Y_HIGH     :    integer := 29;
    constant Z_LOW      :    integer := 30;
    constant Z_HIGH     :    integer := 31;
    
    -- Constant for specifying the SREG
    constant SREG_IDX   :    integer := 32;
    
    -- Utility constant, number of registers.
    constant NUM_GPRS   :    integer := 32; -- Number of general purpose registers
    constant NUM_IORS   :    integer := 64; -- Number of IO registers
    constant NUM_REGS   :    integer := 96; -- Number of registers
    
    --Define registers, signals.
    type REG_ARRAY is array (0 to NUM_REGS-1) of std_logic_vector(NUM_BITS-1 downto 0);
    signal RegData  :    REG_ARRAY;
    signal RegA     :    std_logic_vector(NUM_BITS-1 downto 0); -- used for intermediate computations, 
                                                                -- will be useful for future assignments
    signal RegB     :    std_logic_vector(NUM_BITS-1 downto 0); -- used for intermediate computations, 
                                                                -- will be useful for future assignments
begin
    -- Handle RegA
    RegA <= RegData(conv_integer(RegASel)) when (conv_integer(RegASel) < NUM_GPRS) else (others => 'X');
    RegAOutput <= RegA when SFlag = '0' else RegData(SREG_IDX);
    
    -- Handle RegB
    RegB <= RegData(conv_integer(RegBSel)) when (conv_integer(RegBSel) < NUM_GPRS) else (others => 'X');
    RegBOutput <= RegB;
    
    -- Output SREG
    SREG <= RegData(SREG_IDX);
            
    -- Write to a register when relevant
    process(clock, RegIn, NewFlags, FlagMask, RegData(SREG_IDX))
    begin
        if rising_edge(clock) then
            -- Write to register if requested
            if (RegWr = '1') then
                RegData(to_integer(unsigned(RegWrSel))) <= RegIn;
            end if;
            -- Set SREG with input if requested
            if (SFlag = '0') then
                for i in 0 to NUM_FLAGS-2 loop
                    -- Update bit if FlagMask is set, otherwise preserve old value
                    RegData(SREG_IDX)(i) <= (FlagMask(i) and NewFlags(i)) or ((not FlagMask(i)) and RegData(SREG_IDX)(i));
                end loop;
                -- Interrupt flag is never altered via ALU
                RegData(SREG_IDX)(NUM_FLAGS-1) <= RegData(SREG_IDX)(NUM_FLAGS-1);
            else -- (SFlag = '1')
                -- Directly write RegIn to SREG
                RegData(SREG_IDX) <= RegIn;
            end if;
        end if;
    end process;
end data_flow;
