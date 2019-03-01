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
--        reset            (std_logic)                                  - active low SP reset signal
--        NewFlags         (std_logic_vector(NUM_FLAGS-2 downto 0))     - New flags to SREG
--        RegIn            (std_logic_vector(NUM_BITS-1 downto 0))      - Input register data
--        AddrRegIn        (std_logic_vector(DATA_AB_SIZE-1 downto 0);) - Input Addr Register
--        Control Signals: #################################################################
--        RegASel          (std_logic_vector(Log2(NUM_REGS)-1 downto 0))- Register A Select lines
--        RegBSel          (std_logic_vector(Log2(NUM_REGS)-1 downto 0))- Register B Select lines
--        RegWrSel         (std_logic_vector(Log2(NUM_REGS)-1 downto 0))- Reg Enable decoder lines
--        RegWr            (std_logic)                                  - Reg (write) Enable
--        AddrRegSel       (std_logic_vector(1 downto 0))               - Select X vs Y vs Z vs SP
--        AddrRegWr        (std_logic)                                  - Enable write to Addr Reg
--        FlagMask         (std_logic_vector(NUM_FLAGS-1 downto 0))     - Bitmask of flags to update
--        
-- Outputs:
--        RegAOutput       (std_logic_vector(NUM_BITS-1 downto 0)       - Output register A
--        RegBOutput       (std_logic_vector(NUM_BITS-1 downto 0)       - Output register B
--        AddrRegOut       (std_logic_vector(DATA_AB_SIZE-1 downto 0))  - Output Addr Register
--        SREG             (std_logic_vector(NUM_BITS-1 downto 0)       - Output SREG
--        RegZ             (std_logic_vector(DATA_AB_SIZE-1 downto 0)   - Output Register Z
--
-- Revision History:
--        01/30/19    Bobby Abrahamson        Initial Revision
--        01/31/19    Bobby Abrahamson        Implemented first code from block diagram
--        02/02/19    Bobby Abrahamson        Updated to output SREG for HW3
--        02/05/19    David Kornfeld          Fixed Interrupt flag sensitivity
--        02/07/19    David Kornfeld          Added 16-bit register bus and reset on SP
--        02/08/19    Bobby Abrahamson        Increased select width, support for IO r/w
--        02/27/19    Bobby Abrahamson        Removed SFlag and redid IO register access logic
-----------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.AVR_2019_constants.all;
-----------------------------------------------------------------------------------------
entity Registers is
    generic (
        NUM_BITS    : integer := NUM_BITS   -- Number of bits to use for a GPR.
    );
    port (
        clock       : in std_logic;              
        reset       : in std_logic;                
        RegIn       : in std_logic_vector(NUM_BITS-1 downto 0);
        AddrRegIn   : in std_logic_vector(DATA_AB_SIZE-1 downto 0);
        RegWr       : in std_logic;                      
        RegWrSel    : in std_logic_vector(6 downto 0);      
        RegASel     : in std_logic_vector(6 downto 0);   
        RegBSel     : in std_logic_vector(6 downto 0);   
        FlagMask    : in std_logic_vector(NUM_BITS-1 downto 0); 
        NewFlags    : in std_logic_vector(NUM_FLAGS-2 downto 0); 
        AddrRegSel  : in std_logic_vector(1 downto 0);       
        AddrRegWr   : in std_logic;             

        RegAOutput  : out std_logic_vector(NUM_BITS-1 downto 0); 
        RegBOutput  : out std_logic_vector(NUM_BITS-1 downto 0);
        AddrRegOut  : out std_logic_vector(DATA_AB_SIZE-1 downto 0);
        SREG        : out std_logic_vector(NUM_BITS-1 downto 0);
        RegZ        : out std_logic_vector(DATA_AB_SIZE-1 downto 0)
    );
end Registers;

architecture data_flow of Registers is
    --Define registers, signals.
    type REG_ARRAY is array (0 to NUM_REGS-1) of std_logic_vector(NUM_BITS-1 downto 0);
    signal RegData  :    REG_ARRAY;--:= (others => (others => '0')); -- For simulation only, init to 0
begin
    -- Handle RegA
    RegAOutput <= RegData(conv_integer(RegASel));
    
    -- Handle RegB
    RegBOutput <= RegData(conv_integer(RegBSel));
    
    -- Output SREG
    SREG <= RegData(SREG_IDX);

    -- Output Register Z, for ProgMAU
    RegZ <= RegData(Z_HIGH) & RegData(Z_LOW);

    -- Output Address Register
    AddrRegOut <= RegData(X_HIGH)  & RegData(X_LOW)  when (AddrRegSel = ADDR_REG_SEL_X)  else -- X
                  RegData(Y_HIGH)  & RegData(Y_LOW)  when (AddrRegSel = ADDR_REG_SEL_Y)  else -- Y
                  RegData(Z_HIGH)  & RegData(Z_LOW)  when (AddrRegSel = ADDR_REG_SEL_Z)  else -- Z
                  RegData(SP_HIGH) & RegData(SP_LOW) when (AddrRegSel = ADDR_REG_SEL_SP) else -- SP
                  (others => 'X');
            
    -- Write to a register when relevant
    process(clock, RegIn, AddrRegIn, NewFlags, FlagMask, RegData(SREG_IDX))
    begin
        if rising_edge(clock) then
            if (Reset = '1') then
                -- Set SREG with flagmask input
                for i in 0 to NUM_FLAGS-2 loop
                    -- Update bit if FlagMask is set, otherwise preserve old value
                    RegData(SREG_IDX)(i) <= (FlagMask(i) and NewFlags(i)) or 
                                            ((not FlagMask(i)) and RegData(SREG_IDX)(i));
                end loop;
                -- Interrupt flag is never altered via ALU
                RegData(SREG_IDX)(NUM_FLAGS-1) <= RegData(SREG_IDX)(NUM_FLAGS-1);

                -- Write to register if requested
                if (RegWr = '1') then -- don't write to registers during reset bc sim
                    RegData(to_integer(unsigned(RegWrSel))) <= RegIn;
                end if;
            end if;
            -- Update Address registers as requested
            if (AddrRegWr = '1') then
                if (AddrRegSel = ADDR_REG_SEL_X) then
                    -- Write to X
                    RegData(X_LOW) <= AddrRegIn(NUM_BITS-1 downto 0);
                    RegData(X_HIGH) <= AddrRegIn(DATA_AB_SIZE-1 downto NUM_BITS);
                end if;
                if (AddrRegSel = ADDR_REG_SEL_Y) then
                    -- Write to Y
                    RegData(Y_LOW) <= AddrRegIn(NUM_BITS-1 downto 0);
                    RegData(Y_HIGH) <= AddrRegIn(DATA_AB_SIZE-1 downto NUM_BITS);
                end if;
                if (AddrRegSel = ADDR_REG_SEL_Z) then
                    -- Write to Z
                    RegData(Z_LOW) <= AddrRegIn(NUM_BITS-1 downto 0);
                    RegData(Z_HIGH) <= AddrRegIn(DATA_AB_SIZE-1 downto NUM_BITS);
                end if;
                if (AddrRegSel = ADDR_REG_SEL_SP) then
                    -- Write to SP
                    RegData(SP_LOW) <= AddrRegIn(NUM_BITS-1 downto 0);
                    RegData(SP_HIGH) <= AddrRegIn(DATA_AB_SIZE-1 downto NUM_BITS);
                end if;
            end if;
            -- Reset the stack pointer to all-ones when requested.
            if (reset = '0') then -- active low
                RegData(SP_LOW)  <= (others => '1');
                RegData(SP_HIGH) <= (others => '1');
            end if;
        end if;
    end process;
end data_flow;
