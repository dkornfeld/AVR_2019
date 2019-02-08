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
use ieee.numeric_std.all;
use work.AVR_2019_constants.all;
use work.opcodes.all;

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
    -- Intermediate signals between internal units #################################################
    -- Control Unit
    signal PreDataAB        std_logic_vector(DATA_AB_SIZE-1 downto 0);
    signal ProgDB           std_logic_vector(INSTR_SIZE-1 downto 0);
    signal IOSel            std_logic;
    signal RegInSel         std_logic;
    signal OPBInSel         std_logic;
    signal DBEnableOutput   std_logic;
    signal IR_Immediate     std_logic_vector(NUM_BITS-1 downto 0);

    -- ALU
    signal CarryFlag        std_logic;
    signal TFlag            std_logic;
    signal N_AddMask        std_logic;
    signal FSRControl       std_logic_vector(3 downto 0);
    signal Subtract         std_logic;
    signal CarryInControl   std_logic_vector(1 downto 0);
    signal ALUResultSel     std_logic;
    signal TSCBitSelect     std_logic_vector(2 downto 0);
    signal TLoad            std_logic;
    signal BitSetClear      std_logic;
    signal SettingClearing  std_logic;
    signal Result           std_logic_vector(NUM_BITS-1 downto 0);
    signal NewFlags         std_logic_vector(NUM_FLAGS-2 downto 0);

    -- Registers              
    signal AddrRegIn        std_logic_vector(DATA_AB_SIZE-1 downto 0);
    signal RegWr            std_logic;                      
    signal RegWrSel         std_logic_vector(6 downto 0);      
    signal RegASel          std_logic_vector(6 downto 0);   
    signal RegBSel          std_logic_vector(6 downto 0);   
    signal SFlag            std_logic;                        
    signal FlagMask         std_logic_vector(NUM_BITS-1 downto 0); 
    signal NewFlags         std_logic_vector(NUM_FLAGS-2 downto 0); 
    signal AddrRegSel       std_logic_vector(1 downto 0);       
    signal AddrRegWr        std_logic;             
    signal RegAOutput       std_logic_vector(NUM_BITS-1 downto 0); 
    signal RegBOutput       std_logic_vector(NUM_BITS-1 downto 0);
    signal AddrRegOut       std_logic_vector(DATA_AB_SIZES-1 downto 0);
    signal SREG             std_logic_vector(NUM_BITS-1 downto 0);

    -- DataMAU
    signal IR_Offset        std_logic_vector(DATA_OFFSET_SIZE-1 downto 0);
    signal N_Inc            std_logic;
    signal N_OffsetMask     std_logic;
    signal PrePostSel       std_logic;
    signal OutputImmediate  std_logic;

    -- Actual input to operand B of the ALU. Used for muxing below
    signal OperandBIn       :   std_logic_vector(7 downto 0);

begin

    -- Connect specific flags to the SREG for the ALU
    CarryFlag   <=  SREG(FLAGS_C);
    TFlag       <=  SREG(FLAGS_T);

    -- Mux the input to operand B (take immediate from ControlUnit when necessary)
    OperandBIn  <=  RegBOutput when OPBInSel = '0' else
                    IR_Immediate;

    -- Tri-state the DataDB mux when necessary
    DataDB      <=  Result when DBEnableOutput = '1' else -- Most of the time, output is ALU
                    (others => 'Z');

    -- Connect the output of the DMAU (since it also needs to go to control unit)
    DataAB      <=  PreDataAB;

    -- Map our Control Unit
    ControlUnit : entity work.ControlUnit
    port map(
        IR                          => IR                       ,

        -- Inputs
        clock                       => clock                    ,
        SREG                        => SREG                     ,
        DataAB                      => PreDataAB                ,
        ProgDB                      => ProgDB                   ,
        -- General Control Signals
        DataRd                      => DataRd                   ,
        DataWr                      => DataWr                   ,
        IOSel                       => IOSel                    ,
        RegInSel                    => RegInSel                 ,
        OPBInSel                    => OPBInSel                 ,
        DBSel                       => DBSel                    ,
        DBEnableOutput              => DBEnableOutput           ,
        -- Raw values                  
        IR_Immediate                => IR_Immediate             ,
        IR_Offset                   => IR_Offset                ,
        -- RegArray                         
        RegWr                       => RegWr                    ,
        RegWrSel                    => RegWrSel                 ,
        RegASel                     => RegASel                  ,
        RegBSel                     => RegBSel                  ,
        SFlag                       => SFlag                    ,
        FlagMask                    => FlagMask                 ,
        NewFlags                    => NewFlags                 ,
        AddrRegSel                  => AddrRegSel               ,
        AddrRegWr                   => AddrRegWr                ,
        -- ALU                                      
        N_AddMask                   => N_AddMask                ,
        FSRControl                  => FSRControl               ,
        Subtract                    => Subtract                 ,
        CarryInControl              => CarryInControl           ,
        ALUResultSel                => ALUResultSel             ,
        TSCBitSelect                => TSCBitSelect             ,
        TLoad                       => TLoad                    ,
        BitSetClear                 => BitSetClear              ,
        SettingClearing             => SettingClearing          ,
        -- PMAU                                   
        PCUpdateEn                  => PCUpdateEn               ,
        N_PCLoad                    => N_PCLoad                 ,
        PCControl                   => PCControl                ,
        HiLoSel                     => HiLoSel                  ,
        -- DMAU                                   
        N_Inc                       => N_Inc                    ,
        N_OffsetMask                => N_OffsetMask             ,
        PrePostSel                  => PrePostSel               ,
        OutputImmediate             => OutputImmediate           
    );

    -- Map our Register Array
    ControlUnit : entity work.Registers
    port map(
        clock                       => clock      ,
        reset                       => reset      ,
        RegIn                       => DataDB     ,
        AddrRegIn                   => AddrRegIn  ,

        RegWr                       => RegWr      ,
        RegWrSel                    => RegWrSel   ,
        RegASel                     => RegASel    ,
        RegBSel                     => RegBSel    ,
        SFlag                       => SFlag      ,
        FlagMask                    => FlagMask   ,
        NewFlags                    => NewFlags   ,
        AddrRegSel                  => AddrRegSel ,
        AddrRegWr                   => AddrRegWr  ,

        RegAOutput                  => RegAOutput ,
        RegBOutput                  => RegBOutput ,
        AddrRegOut                  => AddrRegOut ,
        SREG                        => SREG       
    );

    -- Map our DataMAU
    ControlUnit : entity work.DataMAU
    port map(                         
        clock                       => clock          ,  
        IR_Offset                   => IR_Offset      ,  
        Immediate_Addr              => ProgDB         ,  
        InpAddrData                 => AddrRegOut     , 

        N_Inc                       => N_Inc          ,  
        N_OffsetMask                => N_OffsetMask   ,  
        PrePostSel                  => PrePostSel     ,  
        OutputImmediate             => OutputImmediate, 

        DataAB                      => PreDataAB      ,  
        NewAddrData                 => AddrRegIn    
    );

    -- Map our ALU
    ControlUnit : entity work.ALU
    port map(
        OperandA                    => RegAOutput     ,
        OperandB                    => OperandBIn     ,
        CarryFlag                   => CarryFlag      ,
        TFlag                       => TFlag          ,

        N_AddMask                   => N_AddMask      ,
        FSRControl                  => FSRControl     ,
        Subtract                    => Subtract       ,
        CarryInControl              => CarryInControl ,
        ALUResultSel                => ALUResultSel   ,
        TSCBitSelect                => TSCBitSelect   ,
        TLoad                       => TLoad          ,
        BitSetClear                 => BitSetClear    ,
        SettingClearing             => SettingClearing,

        Result                      => Result         ,
        NewFlags                    => NewFlags       
    );

end data_flow;