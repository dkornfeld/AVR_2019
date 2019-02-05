----------------------------------------------------------------------------------------------------
--
--  Atmel AVR ALU Test Entity Declaration
--
--  This is the entity declaration which must be used for building the ALU
--  portion of the AVR design for testing.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--     04 Feb 19  David Kornfeld    Integrated ALU and ControlUnit from AVR_2019
--
----------------------------------------------------------------------------------------------------


--
--  ALU_TEST
--
--  This is the ALU testing interface.  It just brings all the important
--  ALU signals out for testing along with the Instruction Register.
--
--  Inputs:
--    IR       - Instruction Register (16 bits)
--    OperandA - first operand to ALU (8 bits) - looks like the output
--               of the register array
--    OperandB - second operand to ALU (8 bits) - looks like the output
--               of the register array
--    clock    - the system clock
--
--  Outputs:
--    Result   - result of the ALU operation selected by the Instruction
--               Register (8 bits)
--    StatReg  - Status Register contents (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;
use work.AVR_2019_Constants.all;

----------------------------------------------------------------------------------------------------
entity  ALU_TEST  is

    port(
        IR        :  in  opcode_word;                       -- Instruction Register
        OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
        OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
        clock     :  in  std_logic;                         -- system clock
        Result    :  out std_logic_vector(7 downto 0);      -- ALU result
        StatReg   :  out std_logic_vector(7 downto 0)       -- status register
    );

end  ALU_TEST;
----------------------------------------------------------------------------------------------------
architecture data_flow of ALU_TEST is

    -- Intermediate signals between internal units
    signal CarryFlag        :   std_logic;
    signal TFlag            :   std_logic;
    signal SFlag            :   std_logic;
    signal FlagMask         :   std_logic_vector(NUM_FLAGS-1 downto 0);
    signal SREG             :   std_logic_vector(7 downto 0);
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

    -- Unused Signals
    signal RegWr            :   std_logic;                              
    signal RegWrSel         :   std_logic_vector(4 downto 0);           
    signal RegASel          :   std_logic_vector(4 downto 0);           
    signal RegBSel          :   std_logic_vector(4 downto 0);     

    signal UnusedSREG       :   std_logic_vector(NUM_FLAGS-1 downto 0);
    signal UnusedProgDB     :   std_logic_vector(INSTR_SIZE-1 downto 0);

begin

    -- Connect specific flags to the SREG
    CarryFlag   <= SREG(0);
    TFlag       <= SREG(6);

    -- Need to multiplex this, as normally Op A isn't open for input
    OperandAIn  <= OperandA when SFlag = '0' else
                    RegAOutput;

    ALU : entity work.ALU
    port map(
        OperandA        => OperandAIn,
        OperandB        => OperandB,
        CarryFlag       => CarryFlag,
        TFlag           => TFlag,
        N_AddMask       => N_AddMask,
        FSRControl      => FSRControl,
        Subtract        => Subtract,
        CarryInControl  => CarryInControl,
        ALUResultSel    => ALUResultSel,
        TSCBitSelect    => TSCBitSelect,
        TLoad           => TLoad,
        BitSetClear     => BitSetClear,
        SettingClearing => SettingClearing,

        -- Outputs
        Result          => ALUResult,
        NewFlags        => NewFlags       
    );

    Registers : entity work.Registers
    port map(
        clock       => clock,

        -- Unused Inputs
        RegWr       => RegWr,
        RegWrSel    => RegWrSel,
        RegASel     => RegASel,
        RegBSel     => RegBSel,
        
        --SREG
        SFlag       => SFlag,
        FlagMask    => FlagMask,
        SREG        => SREG,

        -- Input from ALU
        RegIn       => ALUResult, -- For direct loads of SREG or setting bits
        NewFlags    => NewFlags,

        -- Output (for writing SREG)
        RegAOutput  => RegAOutput
    );

    ControlUnit : entity work.ControlUnit
    port map(
        IR              => IR,
        clock           => clock,

        -- Unused Inputs
        SREG            => UnusedSREG,
        ProgDB          => UnusedProgDB,

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

    -- Connect to output port
    Result  <= ALUResult;
    StatReg <= SREG;

end architecture;
