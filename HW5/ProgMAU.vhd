----------------------------------------------------------------------------------------------------
-- Author:          David Kornfeld and Bobby Abrahamson
-- Title:           ProgMAU
-- Description:     This file implements the Program Memory Access Unit for the AVR_2019 
--                  CPU designed by Bobby Abrahamson and David Kornfeld. It manages and 
--                  outputs the program counter as well as the program address bus. It
--                  allows for loading or offset-adding to the program counter, and it can
--                  skip multiple instructions if requested. For PUSH/POP instructions,
--                  the PC flows through the Data Data bus, whereas Offsets are expected
--                  to come from the instruction register. 
--          
--
-- Inputs:
--      clock           (std_logic)                                         - clock input
--      Offset          (std_logic_vector(PC_WIDTH-1 downto 0))     - ControlUnit-resized
--                                                                      word offset from
--                                                                      an instruction
--      ImmediateAddr   (std_logic_vector(PC_WIDTH-1 downto 0))     - Input from ProgDB for
--                                                                      word-loads
--      AddrDataIn      (std_logic_vector(PC_WIDTH-1 downto 0))     - Input from address registers
--      DataDB          (std_logic_vector(NUM_BITS-1 downto 0))     - Input from DataDB for RTS
--      Control Signals: ###########################################################################
--      PCUpdateEn      (std_logic)                         - Enable PC to update
--      N_PCLoad        (std_logic_vector)                  - Active low load control for PC
--      PCControl       (std_logic_vector(2 downto 0))      - Mux input to adder control
--      HiLoSel         (std_logic)                         - Selects if loading high or low
--                                                            part of PC
--      
-- Outputs:
--      ProgAB          (std_logic_vector(PC_WIDTH-1 downto 0))     - Computed next Prog 
--                                                                    address
--      PC              (std_logic_vector(PC_WIDTH-1 downto 0))     - Current PC
--
-- Revision History:
--  01/24/19    David Kornfeld      Initial Revision
--  02/21/19    David Kornfeld      Added functional code and updated IO ports/documentation
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use work.AVR_2019_constants.all;
----------------------------------------------------------------------------------------------------
entity ProgMAU is
    port        (
        clock           :   in  std_logic;
        Offset          :   in  std_logic_vector(PC_WIDTH-1 downto 0);
        ImmediateAddr   :   in  std_logic_vector(PC_WIDTH-1 downto 0);
        AddrDataIn      :   in  std_logic_vector(DATA_AB_SIZE-1 downto 0);
        DataDB          :   in  std_logic_vector(NUM_BITS-1 downto 0);
        PCUpdateEn      :   in  std_logic;
        N_PCLoad        :   in  std_logic;
        PCControl       :   in  std_logic_vector(2 downto 0);
        HiLoSel         :   in  std_logic;
        ProgAB          :   out std_logic_vector(PC_WIDTH-1 downto 0);
        PC              :   out std_logic_vector(PC_WIDTH-1 downto 0)
    );
end ProgMAU;
----------------------------------------------------------------------------------------------------
architecture data_flow of ProgMAU is
    -- For incrementing the PC
    constant ONE    :   std_logic_vector(PC_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(1, 
                                                                                    PC_WIDTH));
    -- For displacing the DataDB input for CALL/RTS
    constant ZEROS_8:   std_logic_vector(NUM_BITS-1 downto 0) := (others => '0');

    -- Latched version of the ProgDB for CALL and JMP
    signal Latched_Immediate_Addr   :   std_logic_vector(PC_WIDTH-1 downto 0);

    -- Shifted version of DataDB input for RTS recovery
    signal HiLoSelectedDataDB       :   std_logic_vector(PC_WIDTH-1 downto 0);

    -- Computed next address for ProgAB
    signal ComputedAddress  :   std_logic_vector(PC_WIDTH-1 downto 0);

    -- Signal version of the PC for internal use
    signal PrePC            :   std_logic_vector(PC_WIDTH-1 downto 0);

    -- Masked version of the PC for loading
    signal MaskedPC         :   std_logic_vector(PC_WIDTH-1 downto 0);

    -- Adder carrier signals (for clarity of equation writing)
    signal AdderInA         :   std_logic_vector(PC_WIDTH-1 downto 0);
    signal AdderInB         :   std_logic_vector(PC_WIDTH-1 downto 0);
    signal MainAdderCarries :   std_logic_vector(PC_WIDTH-1 downto 0);
begin

    -- Latch the ProgDB into a register to hold it for JMP/CALL ####################################
    process(clock)
    begin
        if falling_edge(clock) then -- Give the most time we possibly can
            Latched_Immediate_Addr <= ImmediateAddr;
        end if;
    end process;

    -- Latch the PC on the rising edge of clock ####################################################
    process(clock)
    begin
        if rising_edge(clock) then
            if PCUpdateEn='1' then -- Only if we're allowing the PC to update
                PrePC <= ComputedAddress;
            end if;
        end if;
    end process;

    -- Mux the DataDB to the proper position for RTS ###############################################
    HiLoSelectedDataDB  <=  (ZEROS_8 & DataDB)  when HiLoSel = '0' else
                            (DataDB & ZEROS_8); -- 0 -> load into low byte

    -- Mux the Non-PC input to the adder ###########################################################
    
    AdderInB    <=  ONE                     when PCControl = "000" else
                    Offset                  when PCControl = "001" else
                    AddrDataIn              when PCControl = "010" else
                    HiLoSelectedDataDB      when PCControl = "011" else
                    Latched_Immediate_Addr; -- when PCControl = "100" Same output for all others to
                                            -- save space

    -- Mask the PC's input to the adder for loads ##################################################
    MaskPCGenerate: for i in 0 to PC_WIDTH-1 generate
        MaskedPC(i) <= PrePC(i) and N_PCLoad;
    end generate;

    -- Generate the main adder #####################################################################
    
    AdderInA    <=  MaskedPC; -- A input is just the masked PC

    -- Initialize the Main Carries and results
    MainAdderCarries(0)     <= AdderInA(0) and AdderInB(0);
    ComputedAddress(0)      <= AdderInA(0) xor AdderInB(0);

    -- Generate the rest of the carries and bits
    MainAdderGenerate: for i in 1 to PC_WIDTH-1 generate
        ComputedAddress(i)  <=  AdderInA(i) xor AdderInB(i) xor MainAdderCarries(i-1);
        MainAdderCarries(i) <=  (MainAdderCarries(i-1) and (AdderInA(i) or AdderInB(i))) or 
                                (AdderInA(i) and AdderInB(i));
    end generate;

    -- Connect outputs to their ports ##############################################################
    PC      <=  PrePc;
    ProgAB  <=  ComputedAddress;

end architecture;