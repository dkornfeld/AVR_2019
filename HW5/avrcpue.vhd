----------------------------------------------------------------------------------------------------
--
--  Atmel AVR CPU Entity Declaration
--
--  This is the entity declaration for the complete AVR CPU.  The design
--  should implement this entity to make testing possible.
--
--  Revision History:
--      11 May 98   Glen George     Initial revision.
--      9 May 00    Glen George     Updated comments.
--      7 May 02    Glen George     Updated comments.
--      21 Jan 08   Glen George     Updated comments.
--      2/28/19     David Kornfeld  Added our implementation
--
----------------------------------------------------------------------------------------------------


--
--  AVR_CPU
--
--  This is the complete entity declaration for the AVR CPU.  It is used to
--  test the complete design.
--
--  Inputs:
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    INT0   - active low interrupt
--    INT1   - active low interrupt
--    clock  - the system clock
--
--  Outputs:
--    ProgAB - program memory address bus (16 bits)
--    DataAB - data memory address bus (16 bits)
--    DataWr - data write signal
--    DataRd - data read signal
--
--  Inputs/Outputs:
--    DataDB - data memory data bus (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use work.AVR_2019_Constants.all;
use work.opcodes.all;
----------------------------------------------------------------------------------------------------
entity  AVR_CPU  is

    port (
        ProgDB  :  in     std_logic_vector(15 downto 0);   -- program memory data bus
        Reset   :  in     std_logic;                       -- reset signal (active low)
        INT0    :  in     std_logic;                       -- interrupt signal (active low)
        INT1    :  in     std_logic;                       -- interrupt signal (active low)
        clock   :  in     std_logic;                       -- system clock
        ProgAB  :  out    std_logic_vector(15 downto 0);   -- program memory address bus
        DataAB  :  out    std_logic_vector(15 downto 0);   -- data memory address bus
        DataWr  :  out    std_logic;                       -- data memory write enable (active low)
        DataRd  :  out    std_logic;                       -- data memory read enable (active low)
        DataDB  :  inout  std_logic_vector(7 downto 0)     -- data memory data bus
    );

end  AVR_CPU;
----------------------------------------------------------------------------------------------------
architecture data_flow of AVR_CPU is
    -- Intermediate signals between internal units #################################################
    -- Control Unit
    signal OPBInSel         :   std_logic;
    signal DBSel            :   std_logic;
    signal DBEnableOutput   :   std_logic;

    signal IR_Immediate     :   std_logic_vector(NUM_BITS-1 downto 0);
    signal IR_Offset        :   std_logic_vector(DATA_OFFSET_SIZE-1 downto 0);
    signal PCOffset         :   std_logic_vector(PC_WIDTH-1 downto 0);
                               
    -- ALU   
    signal OperandA         :   std_logic_vector(NUM_BITS-1 downto 0);
    signal OperandB         :   std_logic_vector(NUM_BITS-1 downto 0);

    signal N_AddMask        :   std_logic;
    signal FSRControl       :   std_logic_vector(3 downto 0);
    signal Subtract         :   std_logic;
    signal CarryInControl   :   std_logic_vector(1 downto 0);
    signal ALUResultSel     :   std_logic;
    signal TSCBitSelect     :   std_logic_vector(2 downto 0);
    signal TLoad            :   std_logic;
    signal BitSetClear      :   std_logic;
    signal SettingClearing  :   std_logic;
    signal DoubleZero       :   std_logic;
    signal MulSelect        :   std_logic;

    signal Result           :   std_logic_vector(NUM_BITS-1 downto 0);
    signal NewFlags         :   std_logic_vector(NUM_FLAGS-2 downto 0);
                               
    -- Registers          
    signal RegIn            :   std_logic_vector(NUM_BITS-1 downto 0);
    signal AddrRegIn        :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

    signal RegWr            :   std_logic;                      
    signal RegWrSel         :   std_logic_vector(6 downto 0);      
    signal RegASel          :   std_logic_vector(6 downto 0);   
    signal RegBSel          :   std_logic_vector(6 downto 0);    
    signal FlagMask         :   std_logic_vector(NUM_BITS-1 downto 0); 
    signal AddrRegSel       :   std_logic_vector(1 downto 0);       
    signal AddrRegWr        :   std_logic;             
    signal RegAOutput       :   std_logic_vector(NUM_BITS-1 downto 0); 
    signal RegBOutput       :   std_logic_vector(NUM_BITS-1 downto 0);

    signal AddrRegOut       :   std_logic_vector(DATA_AB_SIZE-1 downto 0);
    signal SREG             :   std_logic_vector(NUM_BITS-1 downto 0);
    signal RegZ             :   std_logic_vector(DATA_AB_SIZE-1 downto 0);
                               
    -- DataMAU                 
    signal Immediate_Addr   :   std_logic_vector(DATA_AB_SIZE-1 downto 0); -- ProgDB
    signal InpAddrData      :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

    signal N_Inc            :   std_logic;
    signal N_OffsetMask     :   std_logic;
    signal PrePostSel       :   std_logic;
    signal OutputImmediate  :   std_logic;
    signal ImmediateAddrLatch : std_logic;

    signal NewAddrData      :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

    -- ProgMAU
    signal Offset           :   std_logic_vector(PC_WIDTH-1 downto 0);
    signal AddrDataIn       :   std_logic_vector(DATA_AB_SIZE-1 downto 0);

    signal PCUpdateEn       :   std_logic;
    signal N_PCLoad         :   std_logic;
    signal PCControl        :   std_logic_vector(2 downto 0);
    signal HiLoSel          :   std_logic;
    signal PMAUProgDBLatch  :   std_logic;

    signal PC               :   std_logic_vector(PC_WIDTH-1 downto 0);

    -- For delaying the low byte of the PC for CALLs
    signal delayedPCLow     :   std_logic_vector((PC_WIDTH/2)-1 downto 0);

    -- For muxing from the PC
    signal HiLoSelectedPC   :   std_logic_vector(NUM_BITS-1 downto 0);

    -- For muxing the dataDB
    signal PreDataDB        :   std_logic_vector(NUM_BITS-1 downto 0);

    -- For connecting the DataAB multiple places
    signal PreDataAB        :   std_logic_vector(DATA_AB_SIZE-1 downto 0);
begin
    -- Mux the input to operand B (take immediate from ControlUnit when necessary) #################
    OperandB    <=  RegBOutput when OPBInSel = '0' else
                    IR_Immediate;

    -- Mux the DataDB ##############################################################################
    process(clock) 
    begin
        if rising_edge(clock) then
            delayedPCLow <= PC(NUM_BITS-1 downto 0);
        end if;
    end process;

    HiLoSelectedPC  <=  PC(PC_WIDTH-1 downto NUM_BITS)  when HiLoSel = '1' else
                        delayedPCLow;

    PreDataDB   <=  Result      when DBSel = '0' else   -- ALU Output
                    HiLoSelectedPC;                     -- PC output

    -- Tri-state the DataDB mux when necessary
    DataDB      <=  PreDataDB when DBEnableOutput = '1' else -- Most of the time, output is ALU
                    (others => 'Z');

    -- Connect the output of the DMAU (since it also needs to go to control unit)
    DataAB      <=  PreDataAB;


    -- Make the necessary connections
    OperandA        <= RegAOutput;
    RegIn           <= DataDB;
    Immediate_Addr  <= ProgDB;
    AddrRegIn       <= NewAddrData;
    InpAddrData     <= AddrRegOut;
    Offset          <= PCOffset;
    AddrDataIn      <= AddrRegOut;


    -- Map our Control Unit
    ControlUnit : entity work.ControlUnit
    port map(
        -- Inputs           => -- Inputs           ,
        clock               => clock               ,
        reset               => Reset               ,
        NewFlags            => NewFlags            ,
        SREG                => SREG                ,
        DataAB              => PreDataAB           ,
        ProgDB              => ProgDB              ,
        -- General Controls => -- General Controls ,
        DataRd              => DataRd              ,
        DataWr              => DataWr              ,
        OPBInSel            => OPBInSel            ,
        DBSel               => DBSel               ,
        DBEnableOutput      => DBEnableOutput      ,
        -- Raw values       => -- Raw values       ,
        IR_Immediate        => IR_Immediate        ,
        IR_Offset           => IR_Offset           ,
        PCOffset            => PCOffset            ,
        -- RegArray         => -- RegArray         ,
        RegWr               => RegWr               ,
        RegWrSel            => RegWrSel            ,
        RegASel             => RegASel             ,
        RegBSel             => RegBSel             ,
        FlagMask            => FlagMask            ,
        AddrRegSel          => AddrRegSel          ,
        AddrRegWr           => AddrRegWr           ,
        -- ALU              => -- ALU              ,
        N_AddMask           => N_AddMask           ,
        FSRControl          => FSRControl          ,
        Subtract            => Subtract            ,
        CarryInControl      => CarryInControl      ,
        ALUResultSel        => ALUResultSel        ,
        TSCBitSelect        => TSCBitSelect        ,
        TLoad               => TLoad               ,
        BitSetClear         => BitSetClear         ,
        SettingClearing     => SettingClearing     ,
        DoubleZero          => DoubleZero          ,
        MulSelect           => MulSelect           ,
        -- PMAU             => -- PMAU             ,
        PCUpdateEn          => PCUpdateEn          ,
        N_PCLoad            => N_PCLoad            ,
        PCControl           => PCControl           ,
        HiLoSel             => HiLoSel             ,
        PMAUProgDBLatch     => PMAUProgDBLatch     ,
        -- DMAU             => -- DMAU             ,
        N_Inc               => N_Inc               ,
        N_OffsetMask        => N_OffsetMask        ,
        PrePostSel          => PrePostSel          ,
        OutputImmediate     => OutputImmediate     ,
        ImmediateAddrLatch  => ImmediateAddrLatch  
    );

    -- Map our ALU
    ALU : entity work.ALU
    port map(
        OperandA        => OperandA         ,
        OperandB        => OperandB         ,
        SREG            => SREG             ,
        N_AddMask       => N_AddMask        ,
        FSRControl      => FSRControl       ,
        Subtract        => Subtract         ,
        CarryInControl  => CarryInControl   ,
        ALUResultSel    => ALUResultSel     ,
        TSCBitSelect    => TSCBitSelect     ,
        TLoad           => TLoad            ,
        BitSetClear     => BitSetClear      ,
        SettingClearing => SettingClearing  ,
        DoubleZero      => DoubleZero       ,
        MulSelect       => MulSelect        ,
        Result          => Result           ,
        NewFlags        => NewFlags         
    );

    -- Map our Registers
    Registers : entity work.Registers
    port map(
        clock       => clock       ,
        reset       => Reset       ,
        RegIn       => RegIn       ,
        AddrRegIn   => AddrRegIn   ,
        RegWr       => RegWr       ,
        RegWrSel    => RegWrSel    ,
        RegASel     => RegASel     ,
        RegBSel     => RegBSel     ,
        FlagMask    => FlagMask    ,
        NewFlags    => NewFlags    ,
        AddrRegSel  => AddrRegSel  ,
        AddrRegWr   => AddrRegWr   ,
        RegAOutput  => RegAOutput  ,
        RegBOutput  => RegBOutput  ,
        AddrRegOut  => AddrRegOut  ,
        SREG        => SREG        ,
        RegZ        => RegZ        
    );

    -- Map our DataMAU
    DataMAU : entity work.DataMAU
    port map(
        clock             => clock             ,
        IR_Offset         => IR_Offset         ,
        Immediate_Addr    => Immediate_Addr    ,
        InpAddrData       => InpAddrData       ,
        N_Inc             => N_Inc             ,
        N_OffsetMask      => N_OffsetMask      ,
        PrePostSel        => PrePostSel        ,
        OutputImmediate   => OutputImmediate   ,
        ImmediateAddrLatch=> ImmediateAddrLatch,
        DataAB            => PreDataAB         ,
        NewAddrData       => NewAddrData       
    );

    -- Map our ProgMAU
    ProgMAU : entity work.ProgMAU
    port map(
        clock           => clock           ,
        reset           => Reset           ,
        Offset          => Offset          ,
        RegZ            => RegZ            ,
        ProgDB          => ProgDB          ,
        DataDB          => DataDB          ,
        PCUpdateEn      => PCUpdateEn      ,
        N_PCLoad        => N_PCLoad        ,
        PCControl       => PCControl       ,
        HiLoSel         => HiLoSel         ,
        PMAUProgDBLatch => PMAUProgDBLatch ,
        ProgAB          => ProgAB          ,
        PC              => PC              
    );
end architecture;
