----------------------------------------------------------------------------
--
--  Atmel AVR Data Memory Test Entity Declaration
--
--  This is the entity declaration which must be used for building the data
--  memory access portion of the AVR design for testing.
--
--  Revision History:
--     24 Apr 98  Glen George       Initial revision.
--     25 Apr 00  Glen George       Fixed entity name and updated comments.
--      2 May 02  Glen George       Updated comments.
--      3 May 02  Glen George       Fixed Reset signal type.
--     23 Jan 06  Glen George       Updated comments.
--     21 Jan 08  Glen George       Updated comments..
--     07 Feb 19  Bobby Abrahamson  Implemented for EE119b.
--
----------------------------------------------------------------------------


--
--  MEM_TEST
--
--  This is the data memory access testing interface.  It just brings all
--  the important data memory access signals out for testing along with the
--  Instruction Register and Program Data Bus.
--
--  Inputs:
--    IR     - Instruction Register (16 bits)
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    clock  - the system clock
--
--  Outputs:
--    DataAB - data memory address bus (16 bits)
--    DataDB - data memory data bus (8 bits)
--    DataRd - data read (active low)
--    DataWr - data write (active low)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library opcodes;
use opcodes.opcodes.all;


entity  MEM_TEST  is

    port (
        IR      :  in     opcode_word;                      -- Instruction Register
        ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instruction
        Reset   :  in     std_logic;                        -- system reset signal (active low)
        clock   :  in     std_logic;                        -- system clock
        DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
        DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
        DataRd  :  out    std_logic;                        -- data read (active low)
        DataWr  :  out    std_logic                         -- data write (active low)
    );

end  MEM_TEST;

architecture data_flow of MEM_TEST is
    -- Intermediate signals between internal units

    -- Actual input to operand B of the ALU. Used for muxing below
    signal OperandBIn       :   std_logic_vector(7 downto 0);

begin

    -- Connect specific flags to the SREG
    CarryFlag   <= SREG(0);
    TFlag       <= SREG(6);

    -- Connect register to ALU
    OperandAIn  <=  RegAOutput;
    OperandBIn  <=  RegBOutput when OPBInSel = '0' else
                    IR_Immediate;

    -- Map our Control Unit
    ControlUnit : entity work.ControlUnit
    port map(
        -- Inputs

        -- ALU Control Signals

        -- Register Control Signals

        -- DMAU Control Signals
        
    );

    -- Map our Register Array
    ControlUnit : entity work.Registers
    port map(
        clock           => ,
        RegWr           => ,
        RegWrSel        => ,
        RegASel         => ,
        RegBSel         => ,
        SFlag           => ,
        FlagMask        => ,
        NewFlags        => ,
        AddrRegIn       => ,
        AddrRegOut      => ,
        AddrRegSel      => ,
        AddrRegWr       => ,
        DataDB          => ,
        RegDataOutSel   => ,
        reset           => ,
        RegAOutput      => ,
        RegBOutput      => ,
        SREG            => 
    );

    -- Map our DataMAU
    ControlUnit : entity work.DataMAU
    port map(
        IR_Offset       => ,
        Immediate_Addr  => ,
        InpAddrData     => ,
        N_Inc           => ,
        N_OffsetMask    => ,
        PrePostSel      => ,
        OutputImmediate => ,
        DataAddr        => ,
        NewAddrData     => 
    );

    -- Map our Control Unit
    ControlUnit : entity work.ALU
    port map(
        OperandA        => ,
        OperandB        => ,
        CarryFlag       => ,
        TFlag           => ,
        N_AddMask       => ,
        FSRControl      => ,
        Subtract        => ,
        CarryInControl  => ,
        ALUResultSel    => ,
        TSCBitSelect    => ,
        TLoad           => ,
        BitSetClear     => ,
        SettingClearing => ,
        Result          => ,
        NewFlags        => 
    );


end data_flow;