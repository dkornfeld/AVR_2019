----------------------------------------------------------------------------------------------------
-- Author:          David Kornfeld and Bobby Abrahamson
-- Title:           ControlUnit
-- Description:     This file implements the Control Unit for the AVR_2019 CPU designed by 
--                  Bobby Abrahamson and David Kornfeld. It reads in instructions from the
--                  program data bus and latches them into the instruction register. From
--                  there, the IR bits are used (along with a finite state machine) to
--                  generate all the control signals for the other modules in the CPU.
--                  Additionally, offsets and immediate values are passed to the modules
--                  that read them from the instruction register. 
--
-- Inputs:
--        clock                (std_logic)                               - System clock
--        reset                (std_logic)                               - active low reset signal
--        NewFlags             (std_logic_vector(NUM_FLAGS-2 downto 0))  - Flags from ALU
--        SREG                 (std_logic_vector(NUM_FLAGS-1 downto 0))  - Status Register
--        ProgDB               (std_logic_vector(INSTR_SIZE-1 downto 0)) - Program Data Bus
--
--        IR                   (std_logic_vector(INSTR_SIZE-1 downto 0)  - **TESTING ONLY**, IR input
--        
-- Outputs: (Control Signals)
--        DataRd               (std_logic)                          - Read data
--        DataWr               (std_logic)                          - Write data
--        OPBInSel             (std_logic)                          - Controls Mux into ALU B Op
--        DBSel                (std_logic)                          - Controls output to Data DB
--        DBEnableOutput       (std_logic)                          - If '0', tri-states the DB out
--
--        RegArray Control Signals: ################################################################
--        RegASel              (std_logic_vector(Log2(NUM_REG)-1 downto 0)) - Register A Select lines
--        RegBSel              (std_logic_vector(Log2(NUM_REG)-1 downto 0)) - Register B Select lines
--        RegWrSel             (std_logic_vector(Log2(NUM_REG)-1 downto 0)) - Reg Enable decoder lines
--        RegWr                (std_logic)                                  - Reg (write) Enable
--        AddrRegSel           (std_logic_vector(Log2(NUM_ADDR_REG)-1) downto 0)) - Address register 
--                                                                                  output select
--        AddrRegWr            (std_logic)                                - Address Reg (write) En
--        FlagMask             (std_logic_vector(NUM_FLAGS-1 downto 0))   - Indicate changed flags
--
--        ALU Control Signals: #####################################################################
--        N_AddMask       (std_logic)                         - Active low mask for Operand A
--        FSRControl      (std_logic_vector(3 downto 0))      - F block and shifter control lines
--                                                                (See AVR_2019_constants).
--        Subtract        (std_logic)                         - Command subtraction from adder
--        CarryInControl  (std_logic_vector(1 downto 0))      - Mux lines for carry in to adder
--                                                                (See AVR_2019_constants).
--        ALUResultSel    (std_logic)                         - Select between adder and SR
--        TSCBitSelect    (std_logic_vector(2 downto 0))      - Select which register bit
--                                                                for loading/storing T.
--                                                                Doubles for selecting which bit
--                                                                to set or clear
--        TLoad           (std_logic)                        - Indicate if loading from T flag
--        BitSetClear     (std_logic)                        - Indicates if we're setting or
--                                                                resetting the selected bit
--        SettingClearing (std_logic)                        - Indicates if we're changing a
--                                                                bit at all
--        DoubleZero      (std_logic)                        - Used to indicate double zero flag
--                                                                to the ALU.
--        MulSelect       (std_logic)                        - Indicates if a multiplication is 
--                                                                being done
--
--        Program Memory Access Unit Control Signals: ##############################################
--        PCUpdateEn           (std_logic)                      - Enable PC to update
--        N_PCLoad             (std_logic)                      - Active low load control for PC
--        PCControl            (std_logic_vector(2 downto 0))   - Mux input to adder control
--        HiLoSel              (std_logic)                      - Selects if loading high or lo
--        PMAUProgDBLatch      (std_logic)                     - Select whether the DataMAU should 
--                                                                    latch ProgDB on the current
--                                                                    clock (for use on next
--                                                                    cycles of CALL/JMP).
--        Data Memory Access Unit Control Signals: #################################################
--        N_Inc                (std_logic)                      - Active low increment select
--        N_OffsetMask         (std_logic)                      - Active low mask for offset inp.
--        PrePostSel           (std_logic)                      - Select between pre-post inc
--                                                                    part of PC
--        OutputImmediate      (std_logic)                      - Select whether to DataMAU should
--                                                                    output ProgDB immediate
--                                                                    instead of AddrReg
--        ImmediateAddrLatch   (std_logic)                      - Select whether the DataMAU should 
--                                                                    latch ProgDB on the current
--                                                                    clock (for use on next
--                                                                    cycle of STS/LDS).
--
-- Revision History:
--      01/24/19    David Kornfeld      Initial Revision
--      01/31/19    David Kornfeld      Added clock input and test input for IR
--      01/31/19    David Kornfeld      Began first implementation of ALU controls and FSM
--      01/31/19    Bobby Abrahamson    Integrated register unit
--      02/02/19    Bobby Abrahamson    Added flag masks as control outputs
--      02/04/19    David Kornfeld      Updated documentation
--      02/07/19    Bobby Abrahamson    Added constants for ALU controls and LD/ST instructions
--      02/08/19    David Kornfeld      Updated documentation and fixed glitches in LD/ST
--      02/08/19    David Kornfeld      Removed generics
--      02/08/19    David Kornfeld      Added immediate data routing
--      02/08/19    Bobby Abrahamson    Fixed errors in AddrRegSel decoding.
--      02/09/19    Bobby Abrahamson    Optimized, updated documentation.
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AVR_2019_constants.all;
use work.opcodes.all;
----------------------------------------------------------------------------------------------------
entity ControlUnit is
    port         (
        -- TEST TODO Remove This
        --IR                  :    in     std_logic_vector(INSTR_SIZE-1 downto 0);

        -- Inputs
        clock               :    in     std_logic;
        reset               :    in     std_logic;
        NewFlags            :    in     std_logic_vector(NUM_FLAGS-2 downto 0);
        SREG                :    in     std_logic_vector(NUM_FLAGS-1 downto 0);
        DataAB              :    in     std_logic_vector(DATA_AB_SIZE-1 downto 0);
        ProgDB              :    in     std_logic_vector(INSTR_SIZE-1 downto 0);
        IRQ                 :    in     std_logic;
        -- General Control Signals
        DataRd              :    out    std_logic;
        DataWr              :    out    std_logic;
        OPBInSel            :    out    std_logic;
        DBSel               :    out    std_logic;
        DBEnableOutput      :    out    std_logic;
        IRQClear            :    out    std_logic;
        -- Raw values
        IR_Immediate        :    out    std_logic_vector(NUM_BITS-1 downto 0);
        IR_Offset           :    out    std_logic_vector(DATA_OFFSET_SIZE-1 downto 0);
        -- RegArray
        RegWr               :    out    std_logic;                      
        RegWrSel            :    out    std_logic_vector(6 downto 0);      
        RegASel             :    out    std_logic_vector(6 downto 0);   
        RegBSel             :    out    std_logic_vector(6 downto 0);   
        FlagMask            :    out    std_logic_vector(NUM_BITS-1 downto 0); 
        AddrRegSel          :    out    std_logic_vector(1 downto 0);       
        AddrRegWr           :    out    std_logic;             
        -- ALU
        N_AddMask           :    out    std_logic;
        FSRControl          :    out    std_logic_vector(3 downto 0);
        Subtract            :    out    std_logic;
        CarryInControl      :    out    std_logic_vector(1 downto 0);
        ALUResultSel        :    out    std_logic;
        TSCBitSelect        :    out    std_logic_vector(2 downto 0);
        TLoad               :    out    std_logic;
        BitSetClear         :    out    std_logic;
        SettingClearing     :    out    std_logic;
        DoubleZero          :    out    std_logic;
        MulSelect           :    out    std_logic;
        -- PMAU
        PCUpdateEn          :    out    std_logic;
        N_PCLoad            :    out    std_logic;
        PCControl           :    out    std_logic_vector(2 downto 0);
        HiLoSel             :    out    std_logic;
        PCOffset            :    out    std_logic_vector(PC_WIDTH-1 downto 0);
        PMAUProgDBLatch     :    out    std_logic;
        -- DMAU
        N_Inc               :    out    std_logic;
        N_OffsetMask        :    out    std_logic;
        PrePostSel          :    out    std_logic;
        OutputImmediate     :    out    std_logic;
        ImmediateAddrLatch  :    out    std_logic
    );
end ControlUnit;
-----------------------------------------------------------------------------------------
architecture data_flow of ControlUnit is
    -- Current Instruction Register
    signal IR             :    std_logic_vector(INSTR_SIZE-1 downto 0);

    -- Storage for the current cycle count
    signal instr_cycle    :    std_logic_vector(MAX_INSTR_CLKS-1 downto 0); -- 1-hot cycle
                                                                            -- counter
    signal reset_instr_counter    :    std_logic;    -- Active high reset

    -- Storage for enabling selection of IO
    signal IOBSel : std_logic;
    signal IOWrSel : std_logic;
    -- Signal version of Register B/WR Select for internal use
    signal PreRegBSel : std_logic_vector(6 downto 0);
    signal PreRegWrSel : std_logic_vector(6 downto 0);

    -- Storage for whether the next instruction is two words
    signal NextInsnTwoWords : std_logic;

    -- Storage for whether we need to take an IRQ
    signal TakingIRQ : std_logic;

    -- DataMAU signals (reg vs mem)
    signal reg_access_enable : std_logic; -- 1 when load/store accesses registers instead of memory
    signal reg_index : std_logic_vector(6 downto 0); -- Index of register access via load/store
begin
    -- Instruction cycle counter logic
    process(clock, ProgDB, IRQ)
    begin
        if rising_edge(clock) then
            if (reset = '1') then
                if (reset_instr_counter = '1') then
                    -- Synchronous reset to 1 in the rightmost place
                    instr_cycle <= std_logic_vector(to_unsigned(1, MAX_INSTR_CLKS));

                    -- Update IR on start of new instruction
                    IR <= ProgDB;

                    -- Update whether we are taking an IRQ
                    --if (IRQ = '1') and (SREG(FLAG_I) = '1') then
                    --    -- Only take an IRQ when interrupt flag is set
                    --    TakingIRQ <= '1';
                    --    IR <= OpNOP;
                    --else
                    --    TakingIRQ <= '0';
                    --end if;
                else
                    -- Shift the bit left
                    instr_cycle <= instr_cycle(MAX_INSTR_CLKS-2 downto 0) & '0';
                end if;
            else
                -- Synchronous Reset
                instr_cycle <= std_logic_vector(to_unsigned(1, MAX_INSTR_CLKS));

                -- We're not taking an IRQ.
                --TakingIRQ <= '0';
            end if;
        end if;
    end process;

    -- Latch whether the next instruction is two words
    process(clock, ProgDB)
    begin
        if rising_edge(clock) then
            if (std_match(ProgDB, OpSTS) or std_match(ProgDB, OpLDS) or 
                std_match(ProgDB, OpJMP) or std_match(ProgDB, OpCALL)) then
                NextInsnTwoWords <= '1';
            else
                NextInsnTwoWords <= '0';
            end if;
        end if;
    end process;

    -- IO Select bits.
    --IOBSel <=   '1' when ((instr_cycle(0) = '1') and (std_match(IR, OpCBI) or std_match(IR, OpSBI) or
    --                                                    std_match(IR, OpIN))) else
    --            '0';
    --IOWrSel <=  '1' when ((instr_cycle(0) = '1') and (std_match(IR, OpCBI) or std_match(IR, OpSBI) or
    --                                                    std_match(IR, OpOUT))) else
    --            '0';

    -- Useful signals for memory-mapped access of registers. 
    reg_access_enable <= --'X' when is_x(DataAB) else
                         --'1' when (to_integer(unsigned(DataAB)) < NUM_REGS) else 
                         '0';

    reg_index <= DataAB(6 downto 0);

    -- And offsets get pulled out this way
    IR_Offset <= IR(13) & IR(11 downto 10) & IR(2 downto 0);

    -- We only want to latch ProgDB on cycle 2 for DMAU, cycle 1 for PMAU.
    PMAUProgDBLatch <= instr_cycle(0);
    ImmediateAddrLatch <= instr_cycle(1);
    
    -- Instruction decoding
    process(IR, clock, instr_cycle, ProgDB, DataAB, reg_access_enable, reg_index, NewFlags, SREG)
    begin        
        -- Default General Controls
        reset_instr_counter <= '1'; -- Assume instruction is 1 clock
        DataRd <= '1';              -- Neither read nor write (active low)
        DataWr <= '1';
        DBEnableOutput <= '1';       -- Assume we can write to the data DB
        OPBInSel <= '0';             -- Assume we're taking register B output most of the time

        -- By default immediate 8-bit values from instruction get pulled out this way
        IR_Immediate <= IR(11 downto 8) & IR(3 downto 0);

        -- Default Register Controls
        RegASel <= "00" & IR(8 downto 4);           -- Assume that register selects 
        PreRegBSel <= "00" & IR(9) & IR(3 downto 0);   -- follow Rd, Rr scheme from instr set
        RegWr   <= '1';                             -- Assume we're writing
        PreRegWrSel<= "00" & IR (8 downto 4);
        AddrRegWr <= '0';                           -- By default, don't write to address register.

        -- Default ALU Controls
        TSCBitSelect <= IR(2 downto 0);             -- Always use lower 3 bits to determine 
                                                    -- which bit in byte to alter with 
                                                    -- set/clear/T flag
        DoubleZero <= '0';           -- We are normally not in a double zero insn
        MulSelect <= '0';            -- We are normally not multiplying numbers

        -- By default, we are not ending taking an interrupt.
        IRQClear <= '0';

        -- Default DataMAU signals
        N_OffsetMask <= '0';    -- Mask the offset
        N_Inc <= '0';           -- Leave on Increment
        PrePostSel <= '0';      -- Select Pre
        OutputImmediate <= '0'; -- Don't output immediately from ProgDB

        -- Default ProgMAU signals
        PCUpdateEn <= '1';           -- Update to next PC
        PCControl <= PC_UPDATE_ONE;  -- Update PC = PC + 1
        N_PCLoad <= '1';             -- Don't load fixed PC
        HiLoSel <= '0';              -- Don't care
        PCOffset <= (others => '0'); -- Offset normally zero
        DBSel <= '0';                -- Usually take DataDB from ALU
        
        if std_match(IR, OpADC) then
            -- ALU
            N_AddMask       <= '1';             -- Adding normally
            FSRControl      <= ALU_FSR_B;       -- B
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_CFLAG;  -- Use carry flag
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H
        end if;
        
        if std_match(IR, OpADD) then
            -- ALU
            N_AddMask       <= '1';             -- Adding normally
            FSRControl      <= ALU_FSR_B;       -- B
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No initial carry
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H
        end if;
        
        if std_match(IR, OpADIW) then
            -- ALU
            N_AddMask       <= '1';             -- Adding normally
            Subtract        <= '0';             -- Not subtracting
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVS;     -- Z, C, N, V, S
            DoubleZero      <= instr_cycle(1);  -- Avoid double zero flag

            -- Immediate comes in through operand B
            OPBInSel        <= '1';

            -- Force top two bits of immediate to 0.
            IR_Immediate(NUM_BITS-1 downto NUM_BITS-4) <= "00" & IR(7 downto 6);
            
            -- The even registers for A and B are encoded in
            -- bits 5 and 4 and can easily be turned into
            -- corresponding 5-bit signals. If in the second
            -- cycle, move up one register and reset the
            -- cycle counter;
            RegASel     <= "0011" & IR(5 downto 4) & instr_cycle(1);
            PreRegWrSel    <= "0011" & IR(5 downto 4) & instr_cycle(1);
            reset_instr_counter <= instr_cycle(1);

            -- Update PC only on second cycle.
            PCUpdateEn <= instr_cycle(1);
            
            -- Now set the cycle-dependent controls
            if instr_cycle(0) = '1' then                -- 1/2, ADD B
                 FSRControl         <= ALU_FSR_B;       -- B
                 CarryInControl     <= CARRY_IN_ZERO;   -- No initial carry
            end if;
            if instr_cycle(1) = '1' then                -- 2/2, ADC to 0
                 FSRControl         <= ALU_FSR_ZEROS;   -- 0x00 (0)
                 CarryInControl     <= CARRY_IN_CFLAG;  -- Use carry flag
            end if;
        end if;
        
        if std_match(IR, OpAND) then
            -- ALU
            N_AddMask       <= '0';             -- Doing logical ops
            FSRControl      <= ALU_FSR_A_AND_B; -- A AND B
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No initial carry
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZNVS;      -- Z, N, V, S
        end if;
    
        if std_match(IR, OpANDI) then
            -- ALU
            N_AddMask       <= '0';             -- Doing logical ops
            FSRControl      <= ALU_FSR_A_AND_B; -- A AND B
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No initial carry
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZNVS;      -- Z, N, V, S

            -- Immediate comes in through operand B
            OPBInSel        <= '1';

            -- Registers
            -- Only allow writing to upper half of registers.
            RegASel <= "001" & IR(7 downto 4);
            PreRegWrSel <= "001" & IR(7 downto 4);
        end if;
        
        if std_match(IR, OpASR) then
            -- ALU
            N_AddMask       <= '0';             -- Don't care
            FSRControl      <= ALU_FSR_ASR;     -- Doing ASR
            Subtract        <= '0';             -- Don't care
            CarryInControl  <= CARRY_IN_ZERO;   -- Don't care
            ALUResultSel    <= '1';             -- Shifter
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVS;     -- Z, C, N, V, S
        end if;
        
        if std_match(IR, OpBCLR) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '1';             -- Change a bit
            BitSetClear     <= '0';             -- to 0
            -- Registers
            RegASel         <= SREG_IDX_SEL;    -- Directly write to SREG
            PreRegWrSel     <= SREG_IDX_SEL;
            TSCBitSelect    <= IR(6 downto 4);  -- Flag select
            FlagMask        <= FLAGS_ALL;       -- ALU can set any flag
        end if;
        
        if std_match(IR, OpBLD) then
            -- ALU
            N_AddMask       <= '0';             -- Hide Op A
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '1';             -- Loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_NONE;      -- No flags
        end if;
        
          if std_match(IR, OpBSET) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '1';             -- Change a bit
            BitSetClear     <= '1';             -- to 1
            -- Registers
            RegASel         <= SREG_IDX_SEL;    -- Directly write to SREG
            PreRegWrSel     <= SREG_IDX_SEL;
            TSCBitSelect    <= IR(6 downto 4);  -- Flag select
            FlagMask        <= FLAGS_ALL;       -- ALU can set any flag
        end if;
        
        if std_match(IR, OpBST) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_T;         -- Only T updates
        end if;
        
        if std_match(IR, OpCOM) then
            -- ALU
            N_AddMask       <= '0';             -- Logical operation
            FSRControl      <= ALU_FSR_NOT_A;   -- not A
            Subtract        <= '1';             -- Forces Carry=1
            CarryInControl  <= CARRY_IN_ZERO;   -- no carry
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVS;     -- Z, C, N, V, S
        end if;
        
        if std_match(IR, OpCP) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_NOT_B;          -- not B
            Subtract        <= '1';             -- Subtracting
            CarryInControl  <= CARRY_IN_ONE;    -- No borrow in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H

            -- Registers
            -- Don't write to a register.
            RegWr   <= '0';
        end if;
        
        if std_match(IR, OpCPC) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_NOT_B;   -- not B
            Subtract        <= '1';             -- Subtracting
            CarryInControl  <= CARRY_IN_NCFLAG; -- Use carry bar
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H
            DoubleZero      <= '1';             -- Avoid double zero flag

            -- Registers
            -- Don't write to a register.
            RegWr   <= '0';
        end if;
        
        if std_match(IR, OpCPI) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_NOT_B;   -- not B
            Subtract        <= '1';             -- Subtracting
            CarryInControl  <= CARRY_IN_ONE;    -- No borrow in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H

            -- Immediate comes in through operand B
            OPBInSel        <= '1';

            -- Registers
            -- Select upper half of registers, don't write.
            RegASel <= "001" & IR(7 downto 4);
            RegWr   <= '0';
        end if;
        
        if std_match(IR, OpDEC) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_ONES;    -- 11111111 (-1)
            Subtract        <= '0';             -- Adding (-1)
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZNVS;      -- Z, N, V, S
        end if;
        
        if std_match(IR, OpEOR) then
            -- ALU
            N_AddMask       <= '0';             -- Logical
            FSRControl      <= ALU_FSR_A_XOR_B; -- A xor B
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZNVS;      -- Z, N, V, S
        end if;
        
        if std_match(IR, OpINC) then
            -- ALU
            N_AddMask       <= '1';             -- Adding normally
            FSRControl      <= ALU_FSR_ZEROS;   -- 00000000 (0)
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ONE;    -- Add one
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZNVS;      -- Z, N, V, S
        end if;
        
        if std_match(IR, OpLSR) then
            -- ALU
            N_AddMask       <= '0';             -- Don't care
            FSRControl      <= ALU_FSR_LSR;     -- LSR
            Subtract        <= '0';             -- Don't care
            CarryInControl  <= CARRY_IN_ZERO;   -- Don't care
            ALUResultSel    <= '1';             -- Shifter
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVS;     -- Z, C, N, V, S
        end if;

        --if std_match(IR, OpMUL) then
        --    -- ALU
        --    reset_instr_counter <= instr_cycle(1); -- 2-cycle multiply
        --    DoubleZero          <= instr_cycle(1); -- For 2-byte zero computation
        --    MulSelect           <= '1';

        --    -- Update PC only on second cycle.
        --    PCUpdateEn <= instr_cycle(1);

        --    N_AddMask       <= '1';             -- Need both operands
        --    FSRControl      <= ALU_FSR_ONES;    -- Don't care
        --    Subtract        <= '0';             -- Need flags to act like addition
        --    CarryInControl  <= CARRY_IN_ZERO;   -- Don't care
        --    ALUResultSel    <= instr_cycle(1);  -- Low byte first, then high byte
        --    TLoad           <= '0';             -- Not loading from T
        --    SettingClearing <= '0';             -- Not setting/clearing
        --    BitSetClear     <= '0';             -- Don't care
        --    FlagMask        <= FLAGS_ZC;        -- Z, C

        --    -- Write to R0 on cycle 0, and R1 on cycle 1.
        --    PreRegWrSel <= "000000" & instr_cycle(1);
        --end if;
        
        if std_match(IR, OpNEG) then
            -- ALU
            N_AddMask       <= '0';             -- Unary operation
            FSRControl      <= ALU_FSR_NOT_A;   -- not A
            Subtract        <= '1';             -- Act like subtract
            CarryInControl  <= CARRY_IN_ONE;    -- Add one
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H
        end if;
        
        if std_match(IR, OpOR) then
            -- ALU
            N_AddMask       <= '0';             -- Logical
            FSRControl      <= ALU_FSR_A_OR_B;  -- A or B
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZNVS;      -- Z, N, V, S
        end if;
        
        if std_match(IR, OpORI) then
            -- ALU
            N_AddMask       <= '0';             -- Logical
            FSRControl      <= ALU_FSR_A_OR_B;  -- A or B
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZNVS;      -- Z, N, V, S

            -- Immediate comes in through operand B
            OPBInSel        <= '1';

            -- Registers
            -- Only allow writing to upper half of registers.
            RegASel <= "001" & IR(7 downto 4);
            PreRegWrSel <= "001" & IR(7 downto 4);
        end if;
        
        if std_match(IR, OpROR) then
            -- ALU
            N_AddMask       <= '0';             -- Don't care
            FSRControl      <= ALU_FSR_ROR;     -- ROR
            Subtract        <= '0';             -- Don't care
            CarryInControl  <= CARRY_IN_ZERO;   -- Shifter
            ALUResultSel    <= '1';             -- Shifter
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVS;     -- Z, C, N, V, S
        end if;
        
        if std_match(IR, OpSBC) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_NOT_B;   -- not B
            Subtract        <= '1';             -- Subtracting
            CarryInControl  <= CARRY_IN_NCFLAG; -- Use carry bar
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H

            -- Zero flag preserved if 0
            DoubleZero <= '1';
        end if;
        
        if std_match(IR, OpSBCI) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_NOT_B;   -- not B
            Subtract        <= '1';             -- Subtracting
            CarryInControl  <= CARRY_IN_NCFLAG; -- Use carry bar
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H

            -- Zero flag preserved if 0
            DoubleZero <= '1';

            -- Immediate comes in through operand B
            OPBInSel        <= '1';

            -- Registers
            -- Only allow writing to upper half of registers.
            RegASel <= "001" & IR(7 downto 4);
            PreRegWrSel <= "001" & IR(7 downto 4);
        end if;
        
        if std_match(IR, OpSBIW) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            Subtract        <= '1';             -- Subtracting
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVS;     -- Z, C, N, V, S
            DoubleZero      <= instr_cycle(1);  -- Avoid double zero flag

            -- Immediate comes in through operand B
            OPBInSel        <= '1';

            -- Force top two bits of immediate to 0.
            IR_Immediate(NUM_BITS-1 downto NUM_BITS-4) <= "00" & IR(7 downto 6);
            
            -- The even registers for A and B are encoded in
            -- bits 5 and 4 and can easily be turned into
            -- corresponding 5-bit signals. If in the second
            -- cycle, move up one register and reset the
            -- cycle counter;
            RegASel     <= "0011" & IR(5 downto 4) & instr_cycle(1);
            PreRegWrSel    <= "0011" & IR(5 downto 4) & instr_cycle(1);
            reset_instr_counter <= instr_cycle(1);

            -- Update PC only on second cycle.
            PCUpdateEn <= instr_cycle(1);
            
            -- Now set the cycle-dependent controls
            if instr_cycle(0) = '1' then            -- 1/2, SUB B
                FSRControl      <= ALU_FSR_NOT_B;   -- not B
                CarryInControl  <= CARRY_IN_ONE;    -- No initial borrow
            end if;
            if instr_cycle(1) = '1' then            -- 2/2, SBC w/ 0
                FSRControl      <= ALU_FSR_ONES;    -- 0xFF SBC 0
                CarryInControl  <= CARRY_IN_NCFLAG; -- Use carry bar
            end if;
        end if;
        
        if std_match(IR, OpSUB) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_NOT_B;   -- not B
            Subtract        <= '1';             -- Subtracting
            CarryInControl  <= CARRY_IN_ONE;    -- No borrow in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H
        end if;
        
        if std_match(IR, OpSUBI) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_NOT_B;   -- not B
            Subtract        <= '1';             -- Subtracting
            CarryInControl  <= CARRY_IN_ONE;    -- No borrow in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_ZCNVSH;    -- Z, C, N, V, S, H

            -- Immediate comes in through operand B
            OPBInSel        <= '1';

            -- Registers
            -- Only allow writing to upper half of registers.
            RegASel <= "001" & IR(7 downto 4);
            PreRegWrSel <= "001" & IR(7 downto 4);
        end if;
        
        if std_match(IR, OpSWAP) then
            -- ALU
            N_AddMask       <= '0';             -- Don't care
            FSRControl      <= ALU_FSR_SWAP;    -- SWAP
            Subtract        <= '0';             -- Don't care
            CarryInControl  <= CARRY_IN_ZERO;   -- Don't care
            ALUResultSel    <= '1';             -- Shifter
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_NONE;      -- No flags
        end if;

        if std_match(IR, OpMOV) then
            N_AddMask       <= '0';             -- Mask out A
            FSRControl      <= ALU_FSR_B;       -- B passes through
            Subtract        <= '0';             -- Not subtracting
            ALUResultSel    <= '0';             -- Adder
            CarryInControl  <= CARRY_IN_ZERO;   -- No Carry Influence
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_NONE;      -- Don't change flags
        end if;

        if std_match(IR, OpLDI) then
            N_AddMask       <= '0';             -- Mask out A op
            FSRControl      <= ALU_FSR_B;       -- B passes through
            Subtract        <= '0';             -- Not subtracting
            ALUResultSel    <= '0';             -- Adder
            CarryInControl  <= CARRY_IN_ZERO;   -- No Carry influence
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_NONE;      -- Don't change flags
            
            -- Only write to upper 16 registers. Only have to overwrite bit 8 to '1'
            PreRegWrSel        <= "001" & IR(7 downto 4);   

            -- Immediate comes in through operand B
            OPBInSel        <= '1';
        end if;

        if (std_match(IR, OpLDX) or std_match(IR, OpLDXI) or std_match(IR, OpLDXD) or
                                    std_match(IR, OpLDYI) or std_match(IR, OpLDYD) or
                                    std_match(IR, OpLDZI) or std_match(IR, OpLDZD) or
            std_match(IR, OpSTX) or std_match(IR, OpSTXI) or std_match(IR, OpSTXD) or
                                    std_match(IR, OpSTYI) or std_match(IR, OpSTYD) or
                                    std_match(IR, OpSTZI) or std_match(IR, OpSTZD) or
            std_match(IR, OpPUSH) or std_match(IR, OpPOP)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- don't care
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register by default.
            RegWr <= '0';

            -- Take two cycles
            reset_instr_counter <= instr_cycle(1);

            -- If we are loading/storing from registers
            -- we should update the selects to route correctly.
            if reg_access_enable = '1' then
                if IR(9) = '0' then
                    -- On a load, read from the correct register.
                    RegASel <= reg_index;
                else
                    -- On a store, write to the correct register.
                    PreRegWrSel <= reg_index;
                end if;
            end if;

            -- Select AddrReg
            if IR(3 downto 0) = "1111" then -- SP
                AddrRegSel <= ADDR_REG_SEL_SP;
            else
                AddrRegSel <= IR (3 downto 2);
            end if;

            -- Update PC only on second cycle.
            PCUpdateEn <= instr_cycle(1);

            -- Clock dependent selections
            if instr_cycle(0) = '1' then
                if IR(1 downto 0) = "10" then -- pre-decrement
                    PrePostSel <= '1'; -- Select Pre
                    AddrRegWr <= '1';  -- Update address register
                    N_Inc <= '1';      -- Select decrement
                end if;
                if IR(3 downto 0) = "1111" and IR(9) = '0' then -- pre-increment for pop
                    PrePostSel <= '1'; -- Select Pre
                    AddrRegWr <= '1';  -- Update address register
                                       -- Select increment by default
                end if;
            end if;
            if instr_cycle(1) = '1' then
                -- Output read/write if touching memory
                if reg_access_enable = '0' then
                    DataRd <= clock or IR(9);    -- Output read on low clock + load
                    DataWr <= clock or not IR(9);-- Output write on lock clock + store
                    DBEnableOutput <= IR(9);     -- Give up control of the DB if reading
                end if;

                -- On a load, or a store to register space, we must write to a register.
                if (IR(9) = '0') or ((IR(9) = '1') and reg_access_enable = '1') then
                    RegWr <= '1';
                end if;

                -- If we're doing a post increment, we want to update the address register.
                -- Increment is selected by default.
                if IR(1 downto 0) = "01" then -- post-increment
                    AddrRegWr <= '1';
                end if;

                -- If we're doing a push, we want to update the address register
                if IR(3 downto 0) = "1111" and IR(9) = '1' then -- post-decrement for push
                    N_Inc <= '1';     -- Do a decrement
                    AddrRegWr <= '1';
                end if;
            end if;
        end if;

        if (std_match(IR, OpLDDY) or std_match(IR, OpLDDZ) or 
            std_match(IR, OpSTDY) or std_match(IR, OpSTDZ)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- to 0
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register by default.
            RegWr <= '0';

            -- Take two cycles
            reset_instr_counter <= instr_cycle(1);

            -- Use the immediate offset
            N_OffsetMask <= '1';

            -- If we are loading/storing from registers
            -- we should update the selects to route correctly.
            if reg_access_enable = '1' then
                if IR(9) = '0' then
                    -- On a load, read from the correct register.
                    RegASel <= reg_index;
                else
                    -- On a store, write to the correct register.
                    PreRegWrSel <= reg_index;
                end if;
            end if;

            -- Select AddrReg
            if IR(3) = ADDR_REG_SEL_Y(1) then -- Y
                AddrRegSel <= ADDR_REG_SEL_Y;
            else                              -- Z
                AddrRegSel <= ADDR_REG_SEL_Z;
            end if;

            -- Update PC only on second cycle.
            PCUpdateEn <= instr_cycle(1);

            -- Clock dependent selections
            if instr_cycle(1) = '1' then
                -- Output read/write if touching memory
                if reg_access_enable = '0' then
                    DataRd <= clock or IR(9);    -- Output read on low clock + load
                    DataWr <= clock or not IR(9);-- Output write on lock clock + store
                    DBEnableOutput <= IR(9);     -- Give up control of the DB if reading
                end if;

                -- On a load, or a store to register space, we must write to a register.
                if (IR(9) = '0') or ((IR(9) = '1') and reg_access_enable = '1') then
                    RegWr <= '1';
                end if;
            end if;
        end if;

        if (std_match(IR, OpLDS) or std_match(IR, OpSTS)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- to 0
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register by default.
            RegWr <= '0';

            -- Take three cycles
            reset_instr_counter <= instr_cycle(2);

            -- Update PC only on second and third cycles.
            PCUpdateEn <= (not instr_cycle(0));

            -- Tell DataMAU to output immediate.
            OutputImmediate <= '1';

            -- If we are loading/storing from registers
            -- we should update the selects to route correctly.
            if reg_access_enable = '1' then
                if IR(9) = '0' then
                    -- On a load, read from the correct register.
                    RegASel <= reg_index;
                else
                    -- On a store, write to the correct register.
                    PreRegWrSel <= reg_index;
                end if;
            end if;

            -- Clock dependent selections
            if instr_cycle(2) = '1' then
                -- Output read/write if touching memory
                if reg_access_enable = '0' then
                    DataRd <= clock or IR(9);    -- Output read on low clock + load
                    DataWr <= clock or not IR(9);-- Output write on lock clock + store
                    DBEnableOutput <= IR(9);     -- Give up control of the DB if reading
                end if;

                -- On a load, or a store to register space, we must write to a register.
                if (IR(9) = '0') or ((IR(9) = '1') and reg_access_enable = '1') then
                    RegWr <= '1';
                end if;
            end if;
        end if;

        if (std_match(IR, OpJMP)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- to 0
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register.
            RegWr <= '0';

            -- Only update PC on instruction cycle 1.
            PCUpdateEn <= instr_cycle(1);

            -- Update PC on clock 2 with latched ProgDB.
            if instr_cycle(1) = '1' then
                PCControl <= PC_UPDATE_PROGDB;
                N_PCLoad <= '0';
            end if;
            if instr_cycle(2) = '1' then
                -- Fetch instruction without updating PC.
                PCControl <= PC_UPDATE_ZERO;
            end if;

            -- Take three cycles
            reset_instr_counter <= instr_cycle(2);
        end if;

        if (std_match(IR, OpRJMP) or std_match(IR, OpIJMP)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- to 0

            -- Don't update a register.
            RegWr <= '0';

            -- Update PC on clock 1 (and clock 2, if RJMP).
            PCUpdateEn <= instr_cycle(0) or IR(14);

            if instr_cycle(0) = '1' then
                if IR(14) = '1' then
                    -- RJMP
                    PCControl <= PC_UPDATE_OFFSET;
                    -- Set PCOffset = sign extended IR offset
                    PCOffset <= std_logic_vector(to_signed(to_integer(signed(IR(11 downto 0))), PCOffset'length));
                else
                    -- IJMP
                    PCControl <= PC_UPDATE_REGZ;
                    N_PCLoad <= '0';
                end if;
            end if;

            -- On cycle 2 of an IJMP, we want to lock the PC on ProgAB
            if (instr_cycle(1) = '1') and (IR(14) = '0') then
                PCControl <= PC_UPDATE_ZERO;
            end if;

            -- Take two cycles
            reset_instr_counter <= instr_cycle(1);
        end if;

        if (std_match(IR, OpCALL)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- to 0
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register.
            RegWr <= '0';

            -- Take four cycles
            reset_instr_counter <= instr_cycle(3);

            -- Update PC on clock 1, 2, 3.
            PCUpdateEn <= (not instr_cycle(3));

            -- Select AddrReg = SP
            AddrRegSel <= ADDR_REG_SEL_SP;

            -- Write PC high, then PC low
            HiLoSel    <= instr_cycle(2);
            DBSel      <= instr_cycle(2) or instr_cycle(3);

            -- If we are loading from registers
            -- we should update the selects to route correctly.
            if reg_access_enable = '1' then
                -- On a store, write to the correct register.
                PreRegWrSel <= reg_index;
            end if;

            -- Clock dependent selections
            if (instr_cycle(2) or instr_cycle(3)) = '1' then
                if reg_access_enable = '0' then
                    -- Output read/write if touching memory
                    DataWr <= clock;       -- Output write on lock clock + store
                    DBEnableOutput <= '1'; -- Don't give up control of the DB
                end if;

                -- If we're storing to register space, we must write to a register.
                RegWr <= reg_access_enable;

                -- We're pushing, so update the address register
                N_Inc <= '1';     -- Do a decrement
                AddrRegWr <= '1';
            end if;
            if instr_cycle(2) = '1' then
                -- Update PC with latched ProgDB
                PCControl  <= PC_UPDATE_PROGDB;
                N_PCLoad   <= '0';
            end if;
            if instr_cycle(3) = '1' then
                -- Hold PC locked in ProgAB
                PCControl <= PC_UPDATE_ZERO;
            end if;
        end if;

        if (std_match(IR, OpRCALL) or std_match(IR, OpICALL)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- to 0
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register.
            RegWr <= '0';

            -- Take three cycles
            reset_instr_counter <= instr_cycle(2);

            -- Update PC on clock 1/2.
            PCUpdateEn <= (not instr_cycle(2));

            -- Select AddrReg = SP
            AddrRegSel <= ADDR_REG_SEL_SP;

            -- Write PC high, then PC low
            HiLoSel    <= instr_cycle(1);
            DBSel      <= instr_cycle(1) or instr_cycle(2);

            -- If we are loading from registers
            -- we should update the selects to route correctly.
            if reg_access_enable = '1' then
                -- On a store, write to the correct register.
                PreRegWrSel <= reg_index;
            end if;

            -- Clock dependent selections
            if (instr_cycle(1) or instr_cycle(2)) = '1' then
                if reg_access_enable = '0' then
                    -- Output read/write if touching memory
                    DataWr <= clock;       -- Output write on lock clock + store
                    DBEnableOutput <= '1'; -- Don't give up control of the DB
                end if;

                -- If we're storing to register space, we must write to a register.
                RegWr <= reg_access_enable;

                -- We're pushing, so update the address register
                N_Inc <= '1';     -- Do a decrement
                AddrRegWr <= '1';
            end if;
            if instr_cycle(1) = '1' then
                if IR(14) = '1' then
                    -- RCALL
                    PCControl <= PC_UPDATE_OFFSET;
                    -- Set PCOffset = sign extended IR offset
                    PCOffset <= std_logic_vector(to_signed(to_integer(signed(IR(11 downto 0))), PCOffset'length));
                else
                    -- ICALL
                    PCControl <= PC_UPDATE_REGZ;
                    N_PCLoad <= '0';
                end if;
            end if;
             -- On cycle 3, we want to lock the PC on ProgAB
            if (instr_cycle(2) = '1') then
                PCControl <= PC_UPDATE_ZERO;
            end if;
        end if;

        if (std_match(IR, OpRET) or std_match(IR, OpRETI)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- to 0
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register by default.
            RegWr <= '0';

            -- Take four cycles
            reset_instr_counter <= instr_cycle(3);

            -- If we are loading from registers
            -- we should update the selects to route correctly.
            if reg_access_enable = '1' then
                -- On a load, read from the correct register.
                RegASel <= reg_index;
            end if;

            -- Select AddrReg = SP
            AddrRegSel <= ADDR_REG_SEL_SP;

            -- Update PC on clocks 2/3 with DataDB
            PCUpdateEn <= instr_cycle(1) or instr_cycle(2);
            N_PCLoad   <= (not instr_cycle(1));
            HiLoSel    <= instr_cycle(2); 

            -- Clock dependent selections
            if (instr_cycle(1) or instr_cycle(2)) = '1' then
                -- Load PC from DataDB (popped return address)
                PCControl <= PC_UPDATE_DATADB;

                -- Pre-increment for pop
                PrePostSel <= '1';  -- Select Pre
                AddrRegWr  <= '1';  -- Update address register
                                    -- Select increment by default

                -- Output read/write if touching memory
                if reg_access_enable = '0' then
                    DataRd <= clock;       -- Output read on low clock + load
                    DBEnableOutput <= '0'; -- Give up control of the DB
                end if;
            end if;
            if instr_cycle(3) = '1' then
                -- Hold PC locked in ProgAB
                PCControl <= PC_UPDATE_ZERO;
                -- If RETI, set interrupt flag.
                if IR(4) = '1' then
                    -- ALU
                    SettingClearing <= '1';             -- Change a bit
                    BitSetClear     <= '1';             -- to 1
                    -- Registers
                    RegASel         <= SREG_IDX_SEL;    -- Directly write to SREG
                    PreRegWrSel        <= SREG_IDX_SEL;
                    TSCBitSelect    <= "111";           -- Interrupt flag select
                    FlagMask        <= FLAGS_NONE;      -- ALU shouldn't modify other flags
                    RegWr           <= '1';             -- Write to a register
                end if;
            end if;
        end if;

        if (std_match(IR, OpBRBC) or std_match(IR, OpBRBS)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- to 0
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register by default.
            RegWr <= '0';

            -- By default, take 1 cycle.
            reset_instr_counter <= '1';

            -- Branch only if bit and condition match.
            if (instr_cycle(0) = '1') and ((IR(10) xor SREG(to_integer(unsigned(IR(2 downto 0))))) = '1') then
                -- On cycle 0, take a branch.
                reset_instr_counter <= '0';
                -- Relative Branch
                PCControl <= PC_UPDATE_OFFSET;
                -- Set PCOffset = sign extended IR offset
                PCOffset <= std_logic_vector(to_signed(to_integer(signed(IR(9 downto 3))), PCOffset'length));
            end if;
        end if;

        if (std_match(IR, OpSBRC) or std_match(IR, OpSBRS)) then
            -- ALU
            N_AddMask       <= '0';             -- Mask A op
            FSRControl      <= ALU_FSR_A;       -- A, passthrough
            Subtract        <= '0';             -- Not subtracting
            CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Don't update a register by default.
            RegWr <= '0';

            -- By default, take 1 cycle.
            reset_instr_counter <= '1';

            -- Branch only if bit and condition match. (bit passed through non-updated T flag)
            if (instr_cycle(0) = '1') and (IR(9) = NewFlags(FLAG_T)) then
                -- Don't update the instruction counter..
                reset_instr_counter <= '0';
            end if;
            if (instr_cycle(1) = '1') and (NextInsnTwoWords = '1') then
                -- Take three cycles if two-word instruction.
                    reset_instr_counter <= '0';
            end if;
        end if;

        if (std_match(IR, OpCPSE)) then
            -- ALU
            N_AddMask       <= '1';             -- Subtracting normally
            FSRControl      <= ALU_FSR_NOT_B;   -- not B
            Subtract        <= '1';             -- Subtracting
            CarryInControl  <= CARRY_IN_ONE;    -- No borrow in
            ALUResultSel    <= '0';             -- Adder
            TLoad           <= '0';             -- Not loading from T
            SettingClearing <= '0';             -- Not setting/clearing
            BitSetClear     <= '0';             -- Don't care
            FlagMask        <= FLAGS_NONE;      -- No flags update

            -- Don't write to a register.
            RegWr   <= '0';

            -- Default to next instruction.
            reset_instr_counter <= '1';

            -- Skip only if bit and condition match. (Z flag output from ALI)
            if (instr_cycle(0) = '1') and (NewFlags(FLAG_Z) = '1') then
                -- Don't update the instruction counter..
                reset_instr_counter <= '0';
            end if;
            if (instr_cycle(1) = '1') and (NextInsnTwoWords = '1') then
                -- Take three cycles if two-word instruction.
                    reset_instr_counter <= '0';
            end if;
        end if;

        --if (std_match(IR, OpNOP) or std_match(IR, OpSLP) or std_match(IR, OpWDR)) then
        --    -- ALU
        --    N_AddMask       <= '0';             -- Mask A op
        --    FSRControl      <= ALU_FSR_A;       -- A, passthrough
        --    Subtract        <= '0';             -- Not subtracting
        --    CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
        --    ALUResultSel    <= '0';             -- Adder
        --    TLoad           <= '0';             -- Not loading from T
        --    SettingClearing <= '0';             -- Not setting/clearing
        --    BitSetClear     <= '0';             -- to 0
        --    FlagMask        <= FLAGS_NONE;      -- Don't change flags

        --    -- Don't update a register by default.
        --    RegWr <= '0';
        --end if;

        --if (std_match(IR, OpIN)) then
        --    -- ALU
        --    N_AddMask       <= '0';             -- Mask out A
        --    FSRControl      <= ALU_FSR_B;       -- B passes through
        --    Subtract        <= '0';             -- Not subtracting
        --    ALUResultSel    <= '0';             -- Adder
        --    CarryInControl  <= CARRY_IN_ZERO;   -- No Carry Influence
        --    TLoad           <= '0';             -- Not loading from T
        --    SettingClearing <= '0';             -- Not setting/clearing
        --    BitSetClear     <= '0';             -- Don't care
        --    FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Registers
        --    PreRegBSel <= "0" & IR(10 downto 9) & IR(3 downto 0); 
        --end if;

        --if (std_match(IR, OpOut)) then
        --    -- ALU
        --    N_AddMask       <= '0';             -- Mask out A
        --    FSRControl      <= ALU_FSR_A;       -- A passes through
        --    Subtract        <= '0';             -- Not subtracting
        --    ALUResultSel    <= '0';             -- Adder
        --    CarryInControl  <= CARRY_IN_ZERO;   -- No Carry Influence
        --    TLoad           <= '0';             -- Not loading from T
        --    SettingClearing <= '0';             -- Not setting/clearing
        --    BitSetClear     <= '0';             -- Don't care
        --    FlagMask        <= FLAGS_NONE;      -- Don't change flags

            -- Registers
        --    PreRegWrSel <= "0" & IR(10 downto 9) & IR(3 downto 0); 
        --end if;

        --if (std_match(IR, OpSBI) or std_match(IR, OpCBI)) then
        --    -- ALU
        --    N_AddMask       <= '0';             -- Mask out A
        --    FSRControl      <= ALU_FSR_B;       -- B passes through
        --    Subtract        <= '0';             -- Not subtracting
        --    CarryInControl  <= CARRY_IN_ZERO;   -- No carry influence
        --    ALUResultSel    <= '0';             -- Adder
        --    TLoad           <= '0';             -- Not loading from T
        --    SettingClearing <= '1';             -- Changing a bit
        --    BitSetClear     <= IR(9);           -- to IR
        --    TSCBitSelect    <= IR(2 downto 0);  -- Flag select
        --    FlagMask        <= FLAGS_NONE;      -- Don't change flags

        --    -- Register select.
        --    PreRegBSel <= "0" & IR (8 downto 3);
        --    PreRegWrSel<= "0" & IR (8 downto 3);
        --end if;

        -- NOTE: This MUST be the last block in the process
        --if (TakingIRQ = '1') then
        --    -- By default (since IRQ has NOP in IR)
        --    -- We are not writing a register/are passing through A
        --    -- So we can choose to only overwrite a few signals. 

        --    -- Configure ALU to clear interrupt flag.
        --    SettingClearing <= '1';                   -- Change a bit
        --    TSCBitSelect    <= "111";                 -- Interrupt flag select
        --    BitSetClear     <= '0';                   -- Clear interrupt flag

        --    -- Take three cycles to complete
        --    reset_instr_counter <= instr_cycle(2);

        --    -- Update PC on clock 2.
        --    PCUpdateEn <= instr_cycle(1);

        --    -- Select AddrReg = SP
        --    AddrRegSel <= ADDR_REG_SEL_SP;

        --    -- Write PC high, then PC low
        --    HiLoSel    <= instr_cycle(0);
        --    DBSel      <= instr_cycle(0) or instr_cycle(1);

        --    -- If we are loading from registers
        --    -- we should update the selects to route correctly.
        --    if reg_access_enable = '1' then
        --        -- On a store, write to the correct register.
        --        PreRegWrSel <= reg_index;
        --    end if;

       --     -- Clock dependent selections
       --     if (instr_cycle(0) or instr_cycle(1)) = '1' then
       --         if reg_access_enable = '0' then
       --             -- Output read/write if touching memory
       --             DataWr <= clock;       -- Output write on lock clock + store
       --             DBEnableOutput <= '1'; -- Don't give up control of the DB
       --         end if;

        --        -- If we're storing to register space, we must write to a register.
        --        RegWr <= reg_access_enable;

        --        -- We're pushing, so update the address register
        --        N_Inc <= '1';     -- Do a decrement
        --        AddrRegWr <= '1';
        --    end if;
        --    if instr_cycle(1) = '1' then
        --        -- Update PC with the reset vector
        --        PCControl  <= PC_UPDATE_IRQ;
        --        N_PCLoad   <= '0';
        --    end if;
        --    if instr_cycle(2) = '1' then
        --        -- Clear the interrupt flag.
        --        RegASel         <= SREG_IDX_SEL;    -- Directly write to SREG
        --        PreRegWrSel     <= SREG_IDX_SEL;
        --        RegWr           <= '1';

        --        -- Hold PC locked in ProgAB
        --        PCControl <= PC_UPDATE_ZERO;

        --        IRQClear <= '1';
        --    end if;
        --end if;
    end process;

    -- Connect RegBSel/RegWRSel to relevant ports
    -- This implements an inline one-bit adder.
    RegBSel <= PreRegBSel; --(PreRegBSel(6) or (PreRegBSel(5) and IOBSel)) & (PreRegBSel(5) xor IOBSel)
               --& PreRegBSel(4 downto 0);
    RegWrSel <= PreRegWRSel; --(PreRegWRSel(6) or (PreRegWRSel(5) and IOWrSel)) & (PreRegWRSel(5) xor IOWrSel)
               --& PreRegWRSel(4 downto 0);
end architecture;
