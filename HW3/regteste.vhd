----------------------------------------------------------------------------
--
--  Atmel AVR Register Array Test Entity Declaration
--
--  This is the entity declaration which must be used for building the
--  register array portion of the AVR design for testing.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     22 Apr 02  Glen George       Updated comments.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--     04 Feb 19  Bobby Abrahamson  Implemented for EE119b.
--
----------------------------------------------------------------------------


--
--  REG_TEST
--
--  This is the register array testing interface.  It just brings all the
--  important register array signals out for testing along with the
--  Instruction Register.
--
--  Inputs:
--    IR      - Instruction Register (16 bits)
--    RegIn   - input to the register array (8 bits)
--    clock   - the system clock
--
--  Outputs:
--    RegAOut - register bus A output (8 bits), eventually will connect to ALU
--    RegBOut - register bus B output (8 bits), eventually will connect to ALU
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;
use work.AVR_2019_constants.all;


entity  REG_TEST  is

    port(
        IR       :  in  opcode_word;                        -- Instruction Register
        RegIn    :  in  std_logic_vector(7 downto 0);       -- input register bus
        clock    :  in  std_logic;                          -- system clock
        RegAOut  :  out std_logic_vector(7 downto 0);       -- register bus A out
        RegBOut  :  out std_logic_vector(7 downto 0)        -- register bus B out
    );

end  REG_TEST;

architecture data_flow of REG_TEST is
    -- Signals
    -- Intermediate signals - inbetween tested unit
    signal RegASel          :    std_logic_vector(4 downto 0);
    signal RegBSel          :    std_logic_vector(4 downto 0);
    signal RegWrSel         :    std_logic_vector(4 downto 0);
    signal RegWr            :    std_logic;
    
    signal SFlag            :    std_logic;
    signal FlagMask         :    std_logic_vector(NUM_BITS-1 downto 0);
    signal NewFlags         :    std_logic_vector(NUM_FLAGS-2 downto 0);

    signal ProgDB           :    std_logic_vector(INSTR_SIZE-1 downto 0);
    
    -- Output signals - signals mapped to the tested entity ports
    signal SREG             :    std_logic_vector(NUM_BITS-1 downto 0);
begin
    -- Map our Control Unit
    ControlUnit : entity work.ControlUnit
    port map (
        IR     => IR,
        clock  => clock,
        ProgDB => ProgDB,
        SREG   => SREG,
        RegASel => RegASel,
        RegBSel => RegBSel,
        RegWrSel => RegWrSel,
        RegWr => RegWr,
        SFlag => SFlag,
        FlagMask => FlagMask
    );

    -- Map our register array
    Registers : entity work.Registers
    port map (
        clock => clock,
        RegIn => RegIn,
        RegASel => RegASel,
        RegBSel => RegBSel,
        RegWrSel => RegWrSel,
        RegWr => RegWr,
        SFlag => SFlag,
        FlagMask => FlagMask,
        NewFlags => NewFlags,
        SREG => SREG,
        RegAOutput => RegAOut,
        RegBOutput => RegBOut
    );
end data_flow;