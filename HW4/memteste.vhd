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
    signal CarryFlag        :   std_logic;
    signal TFlag            :   std_logic;
    signal SFlag            :   std_logic;
    signal FlagMask         :   std_logic_vector(NUM_FLAGS-1 downto 0);
    signal SREG             :   std_logic_vector(7 downto 0);
    signal ProgDB           :   std_logic_vector(INSTR_SIZE-1 downto 0);
    signal NewFlags         :   std_logic_vector(6 downto 0);
    signal N_AddMask        :   std_logic;
    signal FSRControl       :   std_logic_vector(3 downto 0);
    signal Subtract         :   std_logic;
    signal CarryInControl   :   std_logic_vector(1 downto 0);
    signal ALUResultSel     :   std_logic;
    signal TSCBitSelect     :   std_logic_vector(2 downto 0);
    signal TLoad            :   std_logic;
    signal BitSetClear      :   std_logic;
    signal SettingClearing  :   std_logic;
    signal ALUResult        :   std_logic_vector(7 downto 0);
    signal RegAOutput       :   std_logic_vector(7 downto 0);

    -- Actual input to operand A of the ALU. Used for muxing below
    signal OperandAIn       :   std_logic_vector(7 downto 0);

    -- Actual input to operand B of the ALU. Used for muxing below
    signal OperandBIn       :   std_logic_vector(7 downto 0);

    -- Unused Signals
    signal RegWr            :   std_logic;                              
    signal RegWrSel         :   std_logic_vector(4 downto 0);           
    signal RegASel          :   std_logic_vector(4 downto 0);           
    signal RegBSel          :   std_logic_vector(4 downto 0);
begin

    -- Connect specific flags to the SREG
    CarryFlag   <= SREG(0);
    TFlag       <= SREG(6);

    -- Need to multiplex this, as normally Op A isn't open for input
    OperandAIn  <= OperandA when SFlag = '0' else
                   RegAOutput;

    OperandBIn <= OperandB when 

    -- Map our Control Unit
    ControlUnit : entity work.ControlUnit
    port map(
        -- Inputs
        IR              => IR,
        clock           => clock,
        SREG            => SREG,
        ProgDB          => ProgDB,

        -- ALU Control Signals
        N_AddMask       => N_AddMask,
        FSRControl      => FSRControl,
        Subtract        => Subtract,
        CarryInControl  => CarryInControl,
        ALUResultSel    => ALUResultSel,
        TSCBitSelect    => TSCBitSelect,
        TLoad           => TLoad,
        BitSetClear     => BitSetClear,
        SettingClearing => SettingClearing,

        -- Register Control Signals
        SFlag       => SFlag,
        FlagMask    => FlagMask
    );

    -- Map our Register array

    -- Map our DataMAU

    -- Map our ALU


end data_flow;