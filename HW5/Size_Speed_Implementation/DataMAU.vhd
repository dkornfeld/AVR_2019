----------------------------------------------------------------------------------------------------
-- Author:          David Kornfeld and Bobby Abrahamson
-- Title:           DataMAU
-- Description:     This file implements the Data Memory Access Unit for the AVR_2019 
--                  CPU designed by Bobby Abrahamson and David Kornfeld. It manages and 
--                  outputs to the  data address bus. These addresses can be sourced from the 
--                  address registers in the register bank, and it outputs the updated value
--                  for updating the address register. Additionally, offsets or immediates can 
--                  be added to (or replace entirely) any of the address registers before the 
--                  value is put on the bus.
--
-- Inputs:
--      clock           (std_logic)                                         - System clock
--      IR_Offset       (std_logic_vector(DATA_OFFSET_SIZE-1 downto 0))     - Address offset input
--      Immediate_Addr  (std_logic_vector(DATA_AB_SIZE-1 downto 0))         - Immediate address in
--      InpAddrData     (std_logic_vector(2*NUM_BITS-1 downto 0))           - Input address reg
--      Control Signals: ###########################################################################
--      N_Inc           (std_logic)                 - Active low increment select
--      N_OffsetMask    (std_logic)                 - Active low mask for offset inp.
--      PrePostSel      (std_logic)                 - Select between pre-post inc
--                                                    part of PC
--      OutputImmediate (std_logic)                 - Output the IR instead of the computed address
--      ImmediateAddrLatch (std_logic)              - Emables latching of the progdb
--      
-- Outputs:
--      DataAB          (std_logic_vector(DATA_AB_SIZE-1 downto 0)) - Computed data address
--      NewAddrData     (std_logic_vector(DATA_AB_SIZE-1 downto 0)) - Updated address reg
--
-- Revision History:
--      01/24/19    David Kornfeld   Initial Revision
--      02/05/19    David Kornfeld   Updated documentation and added input/output ports
--      02/05/19    David Kornfeld   Finished first draft and got to compile
--      02/07/19    David Kornfeld   Latched ProgDB input
--      02/08/19    Bobby Abrahamson Fixed error in PreIncrement calculation
--      02/09/19    David Kornfeld   Updated documentation

----------------------------------------------------------------------------------------------------
library  ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use work.AVR_2019_constants.all;
----------------------------------------------------------------------------------------------------
entity DataMAU is
    port (
        clock           :   in  std_logic;
        IR_Offset       :   in  std_logic_vector(DATA_OFFSET_SIZE-1 downto 0);
        Immediate_Addr  :   in  std_logic_vector(DATA_AB_SIZE-1 downto 0);
        InpAddrData     :   in  std_logic_vector(DATA_AB_SIZE-1 downto 0);
        N_Inc           :   in  std_logic;
        N_OffsetMask    :   in  std_logic;
        PrePostSel      :   in  std_logic;
        OutputImmediate :   in  std_logic;
        ImmediateAddrLatch: in  std_logic;
        DataAB          :   out std_logic_vector(DATA_AB_SIZE-1 downto 0);
        NewAddrData     :   out std_logic_vector(DATA_AB_SIZE-1 downto 0)
    );
end DataMAU;
----------------------------------------------------------------------------------------------------
architecture data_flow of DataMAU is
    -- The pre-incremented address register value
    signal PreIncremented   :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

    -- A masked version of the offset (for zeroing it out)
    signal MaskedOffset     :   std_logic_vector(DATA_OFFSET_SIZE-1 downto 0);

    -- Signals for the incrementer/decrementer adder
    signal IncCarries       :   std_logic_vector(DATA_AB_SIZE-1 downto 0);
    signal IncDecSummand    :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

    -- Inputs to the main adder for offset adding
    signal AdderInA         :   std_logic_vector(DATA_AB_SIZE-1 downto 0);
    signal AdderInB         :   std_logic_vector(DATA_AB_SIZE-1 downto 0);
    -- Carries for the main adder for offset adding
    signal MainCarries      :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

    -- The computed address from offsets (not Immediates)
    signal ComputedAddress  :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

    -- Latched version of the Immediate address for LDS/STS
    signal Latched_Immediate_Addr   :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

begin
    -- Latch the ProgDB into a register to hold it for LDS/STS #####################################
    process(clock)
    begin
        if falling_edge(clock) then -- Give the most time we possibly can
            if ImmediateAddrLatch = '1' then
                Latched_Immediate_Addr <= Immediate_Addr;
            end if;
        end if;
    end process;

    -- Compute the pre-incremented/decremented value ###############################################
    IncDecSummand(0)        <= '1'; -- low bit is always 1, whether incrementing or decrementing
    IncDecSummandGenerate: for i in 1 to DATA_AB_SIZE-1 generate
        IncDecSummand(i)    <= N_Inc; -- Becomes -1 if decrementing, otherwise, is 1
    end generate;

    -- Initialize the Incrementer/Decrementer Carries and results
    IncCarries(0)           <= InpAddrData(0) and IncDecSummand(0);
    PreIncremented(0)       <= InpAddrData(0) xor IncDecSummand(0);

    -- Generate the rest of the carries and bits
    IncDecGenerate: for i in 1 to DATA_AB_SIZE-1 generate
        PreIncremented(i)   <=  InpAddrData(i) xor IncDecSummand(i) xor IncCarries(i-1);
        IncCarries(i)       <=  (IncCarries(i-1) and (InpAddrData(i) or IncDecSummand(i))) or 
                                (InpAddrData(i) and IncDecSummand(i));
    end generate;

    -- Get the first input to the main adder #######################################################

    -- Multiplex between the preincremented and the actual value 
    AdderInA        <=  InpAddrData when PrePostSel = '0' else
                        PreIncremented; -- Select preinc/predec only when PrePostSel = '1'

    -- Get the second input to the main adder ######################################################

    -- Generate the masked offset (for setting it to 0)
    MaskedOffsetGenerate: for i in 0 to DATA_OFFSET_SIZE-1 generate
        MaskedOffset(i) <= IR_Offset(i) and N_OffsetMask;
    end generate;

    -- Then, resize the offset before going into the adder
    AdderInB        <=  std_logic_vector(resize(unsigned(MaskedOffset), DATA_AB_SIZE));

    -- Generate the main adder #####################################################################
    -- Initialize the Main Carries and results
    MainCarries(0)          <= AdderInA(0) and AdderInB(0);
    ComputedAddress(0)      <= AdderInA(0) xor AdderInB(0);

    -- Generate the rest of the carries and bits
    MainGenerate: for i in 1 to DATA_AB_SIZE-1 generate
        ComputedAddress(i)  <=  AdderInA(i) xor AdderInB(i) xor MainCarries(i-1);
        MainCarries(i)      <=  (MainCarries(i-1) and (AdderInA(i) or AdderInB(i))) or 
                                (AdderInA(i) and AdderInB(i));
    end generate;

    -- Multiplex the output between the computed value and the immediate ###########################
    DataAB                  <=  ComputedAddress when OutputImmediate = '0' else
                                Latched_Immediate_Addr; -- Only output value from ProgDB when
                                                        -- OutputImmediate = '1'

    -- Output back to the address registers ########################################################
    NewAddrData             <=  PreIncremented;

end architecture;