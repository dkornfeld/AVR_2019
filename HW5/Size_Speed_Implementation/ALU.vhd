----------------------------------------------------------------------------------------------------
-- Author:      	David Kornfeld and Bobby Abrahamson
-- Title:        	ALU
-- Description:   	This file implements the ALU Array for the AVR_2019 CPU designed by Bobby
--					Abrahamson and David Kornfeld. It can perform any logical operation, any 
--					shift operation, and any arithmetic operation on 2 operands of NUM_BITS size. 
--					Once the result is computed, if the flags in the status register are commanded 
--					to update, they do so based on the computed result. The result is available as 
--					an output. The architecture for this system can be seen in the block diagram. 
--
-- Parameters:
-- 		NUM_BITS    (integer range 2 to Infinity) -     The number of bits used to represent the
--                                                		numbers. 
--
-- Inputs:
--  	OperandA  	(std_logic_vector(NUM_BITS-1 downto 0))        - Input A to ALU
--     	OperandB    (std_logic_vector(NUM_BITS-1 downto 0))        - Input A to ALU
--      SREG        (std_logic_vector(NUM_FLAGS-1 downto ))        - Status register from RegArray
--      Control Signals: ###########################################################################
--   	N_AddMask   	(std_logic)                     	- Active low mask for Operand A
--    	FSRControl   	(std_logic_vector(3 downto 0))    	- F block and shifter control lines
--    	Subtract      	(std_logic)                       	- Command subtraction from adder
--   	CarryInControl 	(std_logic_vector(1 downto 0))   	- Mux lines for carry in to adder
--  	ALUResultSel 	(std_logic)                      	- Select between adder and SR when not
--                                                              multiplying. Otherwise select low 
--                                                              (0) or high (1) byte of the result
--  	TSCBitSelect 	(std_logic_vector(2 downto 0)) 		- Select which register bit
--                                                        		for loading/storing T.
--                                                     			Doubles for selecting which bit
--                                                          	to set or clear
--      TLoad       	(std_logic)                        - Indicate if loading from T flag
--      BitSetClear 	(std_logic)                        - Indicates if we're setting or
--                                                     			resetting the selected bit
--      SettingClearing (std_logic)                        - Indicates if we're changing a
--                                                           	bit at all
--      DouleZero       (std_logic)                        - Indicates that zero flag should only be
--                                                              set if it was set previously
--      MulSelect       (std_logic)                        - Indicates if a multiply is done and
--                                                              changes the meaning of ALUResultSel
--        
-- Outputs:
--		Result     	(std_logic_vector(NUM_BITS-1 downto 0))  	- Output from ALU
--   	NewFlags   	(std_logic_vector(NUM_FLAGS-2 downto 0))  	- New flags to SREG
--                                                        			(minus I Flag)
--
-- Revision History:
--		01/24/19	David Kornfeld    	Initial Revision
--  	01/31/19    David Kornfeld   	Implemented first code from block diagram
--  	01/31/19    David Kornfeld   	Implemented SWAP
--   	01/31/19    David Kornfeld   	cleaned up ins and outs list and documentation
--   	02/02/19    David Kornfeld   	Changed T bit signals and added bit setting/clearing
--      02/04/19    David Kornfeld      Updated documentation
--      02/08/19    David Kornfeld      Updated documentation
--      02/24/19    David Kornfeld      Added whole SREG input and added double zero control signal
--      02/26/19    David Kornfeld      Added signal declarations and types/first equations for MUL
--      02/27/19    David Kornfeld      Debugged MUL and achieved full functionality w/ flags
--      02/28/19    David Kornfeld      Fixed F block behavior on undefined input
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.AVR_2019_constants.all;
----------------------------------------------------------------------------------------------------
entity ALU is
	generic    (
        NUM_BITS        :    integer := NUM_BITS -- Number of bits to use (from header)
    );
    port         (
        OperandA       	:    in   	std_logic_vector(NUM_BITS-1 downto 0);
        OperandB        :    in   	std_logic_vector(NUM_BITS-1 downto 0);
        SREG            :    in     std_logic_vector(NUM_FLAGS-1 downto 0);
        N_AddMask       :    in   	std_logic;
        FSRControl      :    in   	std_logic_vector(3 downto 0);
        Subtract        :    in   	std_logic;
        CarryInControl  :    in   	std_logic_vector(1 downto 0);
        ALUResultSel    :    in   	std_logic;
        TSCBitSelect    :    in   	std_logic_vector(2 downto 0);
        TLoad           :    in   	std_logic;
        BitSetClear     :    in   	std_logic;
        SettingClearing :    in   	std_logic;
        DoubleZero      :    in     std_logic;
        MulSelect       :    in     std_logic;
        Result          :    out    std_logic_vector(NUM_BITS-1 downto 0);
        NewFlags        :    out  	std_logic_vector(NUM_FLAGS-2 downto 0)
    );
end ALU;
----------------------------------------------------------------------------------------------------
architecture data_flow of ALU is
    
    -- Type needed for multiplier
    type std_logic_vector_array is array (0 to NUM_BITS-1) of 
                                    std_logic_vector(NUM_BITS-1 downto 0);
    -- Component needed for multiplier
    component FullAdder
        port (
            A       :   in  std_logic;
            B       :   in  std_logic;
            Cin     :   in  std_logic;
            Sum     :   out std_logic;
            Cout    :   out std_logic
        );
    end component;

    -- Utility/readability
    constant ZEROS     	:    std_logic_vector(NUM_BITS-1 downto 0) := 
                                std_logic_vector(to_unsigned(0, NUM_BITS));

    signal Fout       	:    std_logic_vector(NUM_BITS-1 downto 0);
    signal AdderInA    	:    std_logic_vector(NUM_BITS-1 downto 0);
    signal AdderInB    	:    std_logic_vector(NUM_BITS-1 downto 0);
    signal Sum       	:    std_logic_vector(NUM_BITS-1 downto 0);
    signal Carries      :    std_logic_vector(NUM_BITS downto 0);
    signal CarryIn     	:    std_logic;
    signal CarryOut    	:    std_logic;
    signal SRout        :    std_logic_vector(NUM_BITS-1 downto 0);
    signal MULCarries   :    std_logic_vector_array;
    signal MULSums      :    std_logic_vector_array;
    signal AandB        :    std_logic_vector_array;
    signal MULResult    :    std_logic_vector(2*NUM_BITS-1 downto 0);
    signal RegIndex    	:    integer := 0; 	-- Initialized purely for simulation reasons
                                         	-- Protects against out-of-range indexing
                                        	-- from undefineds
    signal CarryFlag    :    std_logic;     -- Flag from SREG
    signal TFlag        :    std_logic;     -- Flag from SREG
    
    signal pre_result 	:    std_logic_vector(NUM_BITS-1 downto 0);
    signal pre_flags  	:    std_logic_vector(NUM_FLAGS-2 downto 0);
begin
    -- Connect flag signals from SREG ##############################################################
    CarryFlag   <= SREG(FLAG_C);
    TFlag       <= SREG(FLAG_T);

    -- F Block #####################################################################################
    -- Bit-wise logical operatins depending on positions of FSRControl. Fout receives:
    --        0000 -> Zero Vector
    --        0001 -> A nor B
    --        1100 -> A
    --        1010 -> B
    --        0011 -> not A
    --        0101 -> not B
    --        0110 -> A xor B
    --        0111 -> A nand B
    --        1000 -> A and B
    --        1001 -> A xnor B
    --        1110 -> A or B
    --        1111 -> Ones vector
    FGenerate: for i in 0 to NUM_BITS-1 generate
        Fout(i)    <=   FSRControl(0)   when    (OperandA(i)='0') and (OperandB(i)='0') else
                        FSRControl(1)   when    (OperandA(i)='0') else -- (OperandB(i)='1')
                        FSRControl(2)   when    (OperandA(i)='1') and (OperandB(i)='0') else
                        FSRControl(3)   when    (OperandA(i)='1') else -- (OperandB(i)='1')
                        -- For cases when A is undefined, but B is
                        FSRControl(0)   when    (OperandB(i)='0') else
                        FSRControl(1)   when    (OperandB(i)='1');
    end generate;
    
    -- Full Adder ##################################################################################
    
    -- Inputs to adder are the selectively masked A Operand and the F block
    AdderInAGenerate: for i in 0 to NUM_BITS-1 generate
        AdderInA(i) <=  OperandA(i) when N_AddMask = '1' else
                        '0';
    end generate;
    
    AdderInB <= Fout;
    
    -- Multiplex the CarryIn
    CarryIn <=  '0'             when CarryInControl = CARRY_IN_ZERO     else -- Normal
                '1'             when CarryInControl = CARRY_IN_ONE      else -- Force + 1
                CarryFlag       when CarryInControl = CARRY_IN_CFLAG    else -- Use Carry Flag
                not CarryFlag   when CarryInControl = CARRY_IN_NCFLAG;       -- Use its inv.
    
    -- Initialize the generation loop
    Carries(0) <= CarryIn;
    
    -- Generate the adder and carry bits
    AdderGenerate: for i in 0 to NUM_BITS-1 generate
        Sum(i)          <=  AdderInA(i) xor AdderInB(i) xor Carries(i);
        Carries(i+1)    <=  (Carries(i) and (AdderInA(i) or AdderInB(i))) or 
                            (AdderInA(i) and AdderInB(i));
    end generate;
    
    -- CarryOut is just the last carry xor'd with subtract (for borrow)
    CarryOut <= Carries(NUM_BITS) xor Subtract;
    
    -- Shifter/Rotator #############################################################################
    
    -- All left shifts done with adder, so only need to worry about right shifts. Only
    -- uppermost bit is the one with any muxing. All others always just move down. This
    -- of course assumes we are not swapping, which is just a special case of shifting.
    process(FSRControl, OperandA, CarryFlag)
    begin
        -- First, assume not swapping
        SRout(NUM_BITS-2 downto 0) <= OperandA(NUM_BITS-1 downto 1);
        
        case FSRControl is
            when ALU_FSR_LSR     => SRout(NUM_BITS-1)    <= '0';                     -- LSR
            when ALU_FSR_ASR     => SRout(NUM_BITS-1)    <= OperandA(NUM_BITS-1);    -- ASR
            when ALU_FSR_ROR     => SRout(NUM_BITS-1)    <= CarryFlag;               -- ROR
            when ALU_FSR_SWAP    => SRout(NUM_BITS-1)    <= OperandA((NUM_BITS/2)-1);-- SWAP
            when others          => SRout(NUM_BITS-1)    <= '1';                     -- Others
        end case;
        
        -- But if we are swapping
        if std_match(FSRControl, ALU_FSR_SWAP) then -- Swap the "nibbles"
            SRout(NUM_BITS-2 downto NUM_BITS/2)     <= OperandA((NUM_BITS/2)-2 downto 0);
            SRout((NUM_BITS/2) - 1 downto 0)        <= OperandA(NUM_BITS-1 downto NUM_BITS/2);
        end if;
    end process;
    
    -- Multiplier ##################################################################################

    ---- Generate 'A and B' signals
    --AandBGenerate1: for i in 0 to NUM_BITS-1 generate
    --    AandBGenerate2: for j in 0 to NUM_BITS-1 generate
    --        AandB(i)(j) <= OperandA(i) and OperandB(j);
    --    end generate;
    --end generate;

    ---- Generate a NMM for N-bits
    --MULOuterGenerate: for i in 0 to NUM_BITS-1 generate
    --    MULInnerGenerate: for j in 1 to NUM_BITS-1 generate

            -- First row of the NMM
    --        lowest_row: if i=0 generate
    --            U0: FullAdder port map 
    --                (AandB(j)(i),
    --                AandB(j-1)(i+1), 
    --                '0', 
    --                MULSums(i)(j-1), 
    --                MULCarries(i)(j-1));
    --        end generate;

    --        -- All middle rows of NMM
    --        middle_rows: if (i>0 and i<NUM_BITS-1) generate

    --            -- Need to differentiate between A input on last adder
    --            middle_not_last_adder: if j<NUM_BITS-1 generate
    --            UX: FullAdder port map
    --                (MULSums(i-1)(j), 
    --                AandB(j-1)(i+1),
    --                MULCarries(i-1)(j-1),
    --                MULSums(i)(j-1),
    --                MULCarries(i)(j-1));
    --            end generate;

    --            -- Last adder gets non-sum signal as A input
    --            middle_last_adder: if j=NUM_BITS-1 generate
    --            UX: FullAdder port map
    --                (AandB(j)(i),
    --                AandB(j-1)(i+1),
    --                MULCarries(i-1)(j-1),
    --                MULSums(i)(j-1),
    --                MULCarries(i)(j-1));
    --            end generate;

    --        end generate;

    --        -- Lowest row has even more unique cases
    --        last_row: if (i=NUM_BITS-1) generate

    --            -- This one takes in zero for B
    --            first_last_adder: if j=1 generate
    --            UX: FullAdder port map
    --                (MULSums(i-1)(j), 
    --                '0',
    --                MULCarries(i-1)(j-1),
    --                MULSums(i)(j-1),
    --                MULCarries(i)(j-1));
    --            end generate;

    --            -- This one is the normal one
    --            middle_last_adder: if (j>1 and j<NUM_BITS-1) generate
    --            UX: FullAdder port map
    --                (MULSums(i-1)(j),
    --                MULCarries(i)(j-2),
    --                MULCarries(i-1)(j-1),
    --                MULSums(i)(j-1),
    --                MULCarries(i)(j-1));
    --            end generate;

    --            -- And this one takes in a special input for A
    --            last_last_adder: if j=NUM_BITS-1 generate
    --            UX: FullAdder port map
    --                (AandB(j)(i),
    --                MULCarries(i)(j-2),
    --                MULCarries(i-1)(j-1),
    --                MULSums(i)(j-1),
    --                MULCarries(i)(j-1));
    --            end generate;

    --        end generate;

    --    end generate;
    --end generate;

    ---- Pick out the result from the signals we generated
    --MULResult(0) <= AandB(0)(0);
    --MULResultFetch1: for i in 1 to (NUM_BITS-1) generate
    --    MULResult(i) <= MULSums(i-1)(0);
    --end generate;
    --MULResultFetch2: for i in 0 to (NUM_BITS-2) generate
    --    MULResult(i+NUM_BITS) <= MULSums(NUM_BITS-1)(i);
    --end generate;
    --MULResult(2*NUM_BITS-1) <= MULCarries(NUM_BITS-1)(NUM_BITS-2);

    -- Output selection and loading to/from bits ###################################################
    
    -- Decode the binary to an index we can use
    RegIndex <= conv_integer(TSCBitSelect);
    
    process(Sum, SRout, ALUResultSel, TLoad, TFlag, SettingClearing, 
            BitSetClear, RegIndex, MulSelect, MULResult)
    begin
        -- Result is either from the adder (through the F block) or the shifter/rotator
        if (ALUResultSel = '0') and (MulSelect = '0') then
            pre_result <= Sum;
        end if;
        if  (ALUResultSel = '1') and (MulSelect = '0') then
            pre_result <= SRout;
        end if;
        --if  (ALUResultSel = '0') and (MulSelect = '1') then
        --    pre_result <= MULResult(NUM_BITS-1 downto 0);
        --end if;
        --if  (ALUResultSel = '1') and (MulSelect = '1') then
        --    pre_result <= MULResult(2*NUM_BITS-1 downto NUM_BITS);
        --end if;
                            
        -- If doing a BLD instruction
        if TLoad = '1' then
            pre_result(RegIndex) <= TFlag;
        end if;
        
        -- If doing a bit set or clear instruction
        if SettingClearing = '1' then
            pre_result(RegIndex) <= BitSetClear;
        end if;
        
    end process;
    
    -- Flag computation ############################################################################
    
    -- Z Flag is just when result is all zeros. Also might need to take previous zero flag into 
    --  account for 2-clock instructions and CPC
    pre_flags(FLAG_Z) <=    '1' when (pre_result = ZEROS) and 
                                ((DoubleZero='0') or (SREG(FLAG_Z)='1')) else   -- See if previous
                                                                                -- Z Flag is needed
                            '0';
                                
    -- N Flag is just the high bit of the result
    pre_flags(FLAG_N) <=    pre_result(NUM_BITS-1);
    
    -- S Flag (Corrected sign bit) is just N xor V
    pre_flags(FLAG_S) <=    pre_flags(FLAG_N) xor pre_flags(FLAG_V);
    
    -- T Flag. Always assume we're doing BST, flag mask will make sure it's not loaded when it 
    --          needs to hold
    pre_flags(FLAG_T) <= OperandA(RegIndex);
    
    -- V Flag. Signed Overflow (carry into high bit != carry out of high bit), 
    --          is zero (0) for logical operations and N xor C for shift operations
    pre_flags(FLAG_V) <=    -- Logical
                            '0' when (N_AddMask='0' and Subtract='0' and ALUResultSel='0') else
                            -- Shift Operations
                            pre_flags(FLAG_N) xor pre_flags(FLAG_C) when ALUResultSel='1' else
                            -- Else we can use the carries. Xor is true when !=
                            (Carries(NUM_BITS-1) xor Carries(NUM_BITS));
                                
    -- H Flag. Half-carry. Set if there was a carry/borrow through middle bit. Cleared
    --          otherwise.
    pre_flags(FLAG_H) <= Carries(NUM_BITS/2) xor Subtract;
    
    -- C Flag. Carry. Follows normal behavior for all left shifts and arithmetic 
    --          operations. It's the low bit when rotating right and set when doing COM
    pre_flags(FLAG_C) <=    CarryOut    when (ALUResultSel='0' and MulSelect='0') else -- Arithmetic
                            OperandA(0) when (ALUResultSel='1' and MulSelect='0'); --else -- Shift/Rot
                            --pre_result(NUM_BITS-1);                                    -- MUL
                                
    -- Connect output of result/flags ##############################################################
    result      <= pre_result;
    NewFlags    <= pre_flags;

end architecture;