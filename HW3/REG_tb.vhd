-- Title:     Test Bench for Registers
-- Author:    David Kornfeld and Bobby Abrahamson
--
-- This is a testbench for the Registers entity for the AVR_2019 CPU designed by
-- Bobby Abrahamson and David Kornfeld. It validates that the contents of the register array
-- can be filled with arbitrary values, and that those values can be read out later. It also
-- tests that functionality to update the SREG both directly and indirectly (via a flags-to-update
-- bitmask) works as intended.
--
--
-- Revision History:
--        01/31/19    Bobby Abrahamson        Initial Revision
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AVR_2019_constants.all;
----------------------------------------------------------------------------
entity REG_tb is
end REG_tb;
----------------------------------------------------------------------------
architecture TB_ARCHITECTURE of REG_tb is

    -- Constants ------------------------------------------------------------

    constant NUM_GPRS          :    integer    := 32;
    
    -- Timing Constants -----------------------------------------------------
    
    -- Clock period
    constant CLK_PERIOD_INT    :    integer    := 20; -- ns
    constant CLK_PERIOD        :    time       := CLK_PERIOD_INT * (1 ns);

    -- Component declaration of the tested unit #############################    
    component Registers  is
        generic (
            NUM_BITS    :    integer := NUM_BITS -- Number of bits to use (from header)
        );
        port (
            clock       : in std_logic;                              -- input clock
            RegWr       : in std_logic;                              -- select write vs not write
            RegWrSel    : in std_logic_vector(4 downto 0);           -- select register to write to
            RegASel     : in std_logic_vector(4 downto 0);           -- select register A to read from
            RegBSel     : in std_logic_vector(4 downto 0);           -- select register B to read from
            RegIn       : in std_logic_vector(NUM_BITS-1 downto 0);  -- Input data to write
            
            -- SREG
            SFlag       : in std_logic;                              -- Select direct overwrite of SREG
            FlagMask    : in std_logic_vector(NUM_BITS-1 downto 0);  -- Bitmask to update in SREG
            NewFlags    : in std_logic_vector(NUM_FLAGS-2 downto 0); -- New bits for SREG
            
            RegAOutput  : out std_logic_vector(NUM_BITS-1 downto 0); -- Output register A
            RegBOutput  : out std_logic_vector(NUM_BITS-1 downto 0); -- Output register B
            SREG        : out std_logic_vector(NUM_BITS-1 downto 0)  -- Output SREG
        );
    end  component;
    
    -- Stimulus signals - input to the tested unit
    signal RegIn            :    std_logic_vector(NUM_BITS-1 downto 0);
    signal RegASel          :    std_logic_vector(4 downto 0);
    signal RegBSel          :    std_logic_vector(4 downto 0);
    signal RegWrSel         :    std_logic_vector(4 downto 0);
    signal RegWr            :    std_logic;
    
    signal SFlag            :     std_logic;
    signal FlagMask         : std_logic_vector(NUM_BITS-1 downto 0);
    signal NewFlags         : std_logic_vector(NUM_FLAGS-2 downto 0);
    
    -- Output signals - signals mapped to the tested entity ports
    signal RegAOutput       :    std_logic_vector(NUM_BITS-1 downto 0);
    signal RegBOutput       :    std_logic_vector(NUM_BITS-1 downto 0);
    signal SREG             :    std_logic_vector(NUM_BITS-1 downto 0);
    
    -- System clock
    signal CLK              :    std_logic;
    
   -- Signal used to stop clock signal generators
   signal END_SIM           :    BOOLEAN := FALSE;

begin -- ###################################################################
    -- Unit Under Test port map
    UUT : Registers
        port map(
            clock => CLK,
            RegWr => RegWr,
            RegWrSel => RegWrSel,
            RegASel => RegASel,
            RegBSel => RegBSel,
            RegIn => RegIn,
            SFlag => SFlag,
            FlagMask => FlagMask,
            NewFlags => NewFlags,
            RegAOutput => RegAOutput,
            RegBOutput => RegBOutput,
            SREG => SREG
        );
    
   -- now generate the stimulus and test the design
    process
        
        -- Integer forms of the test objects
        variable ExpectedRegAOutput    :    std_logic_vector(NUM_BITS-1 downto 0);
        variable ExpectedRegBOutput    :    std_logic_vector(NUM_BITS-1 downto 0);
        
   begin  -- of stimulus process

        -- Initialize all registers, GPR(i) takes value i.
        for i in 0 to NUM_GPRS-1 loop
            RegWr <= '1';
            SFlag <= '0';
            RegWrSel <= std_logic_vector(to_unsigned(i, RegWrSel'length));
            RegASel <= std_logic_vector(to_unsigned(0, RegASel'length)); -- Don't care
            RegBSel <= std_logic_vector(to_unsigned(0, RegBSel'length)); -- Don't care
            RegIn  <= std_logic_vector(to_unsigned(i, RegIn'length));
            wait for CLK_PERIOD; -- One operation per clock
        end loop;

        -- Verify we can read each register as output A.
        for i in 0 to NUM_GPRS-1 loop
            RegWr <= '0';
            SFlag <= '0';
            RegASel <= std_logic_vector(to_unsigned(i, RegASel'length));
            RegBSel <= std_logic_vector(to_unsigned(0, RegBSel'length)); -- Don't care
            RegWrSel <= std_logic_vector(to_unsigned(0, RegWrSel'length)); -- Don't care
            wait for 1 ns;
            assert(std_match(RegAOutput, std_logic_vector(to_unsigned(i, RegAOutput'length))))
                report "Register A was wrong."
                severity ERROR;
            wait for CLK_PERIOD - 1 ns;
        end loop;

        -- Verify we can read each register as output B.
        for i in 0 to NUM_GPRS-1 loop
            RegWr <= '0';
            SFlag <= '0';
            RegBSel <= std_logic_vector(to_unsigned(i, RegBSel'length));
            RegASel <= std_logic_vector(to_unsigned(0, RegASel'length)); -- Don't care
            RegWrSel <= std_logic_vector(to_unsigned(0, RegWrSel'length)); -- Don't care
            wait for 1 ns;
            assert(std_match(RegBOutput, std_logic_vector(to_unsigned(i, RegBOutput'length))))
                report "Register B was wrong."
                severity ERROR;
            wait for CLK_PERIOD - 1 ns;
        end loop;

        -- Test direct overwrite of SREG. We will set SFlag, and write i to RegIn.
        -- This will cause the SREG to take the value i directly,
        -- and be output over RegAOutput.
        for i in 0 to NUM_GPRS-1 loop
            RegWr <= '0';
            SFlag <= '1';
            RegIn  <= std_logic_vector(to_unsigned(i, RegIn'length));
            RegBSel <= std_logic_vector(to_unsigned(0, RegBSel'length)); -- Don't care
            RegASel <= std_logic_vector(to_unsigned(0, RegASel'length)); -- Don't care
            RegWrSel <= std_logic_vector(to_unsigned(0, RegWrSel'length)); -- Don't care
            wait for CLK_PERIOD;
            assert(std_match(SREG, std_logic_vector(to_unsigned(i, SREG'length))))
                report "SREG (via direct overwrite) was wrong."
                severity ERROR;
            wait for 1 ns;
            assert(std_match(RegAOutput, std_logic_vector(to_unsigned(i, RegAOutput'length))))
                report "SREG not output over RegAOutput after direct overwrite."
                severity ERROR;
            wait for 1 ns;
        end loop;
        
        -- Verify that we can update the SREG via normal flag updates.
        -- We will try setting the SREG to each value (0..NUM_GPRS-1),
        -- with an all ones bitmask and verify that it reads out correctly.
        -- We will then set the bitmask to zero, and set all input flags to 0.
        -- Because the bitmask is zero, SREG should preserve its value and not update anything
        -- and we will verify this.
        for i in 0 to NUM_GPRS-1 loop
            RegWr <= '0';
            SFlag <= '0';
            RegBSel <= std_logic_vector(to_unsigned(0, RegBSel'length)); -- Don't care
            RegASel <= std_logic_vector(to_unsigned(0, RegASel'length)); -- Don't care
            RegWrSel <= std_logic_vector(to_unsigned(0, RegWrSel'length)); -- Don't care
            RegIn <= std_logic_vector(to_unsigned(0, RegIn'length)); -- Don't care

            -- Test setting non-zero value
            NewFlags <= std_logic_vector(to_unsigned(i, NewFlags'length));
            FlagMask <= (others => '1');
            wait for CLK_PERIOD;
            
            assert(std_match(SREG, std_logic_vector(to_unsigned(i, SREG'length))))
                report "SREG (via flag mask update) was wrong."
                severity error;
                wait for CLK_PERIOD;

            -- Test flags preserved when empty mask
            NewFlags <= std_logic_vector(to_unsigned(0, NewFlags'length));
            FlagMask <= (others => '0');
            wait for CLK_PERIOD;
            
            assert(std_match(SREG, std_logic_vector(to_unsigned(i, SREG'length))))
                report "SREG (via flag mask update) was wrong."
                severity error;
                wait for CLK_PERIOD;
        end loop;
                        
        END_SIM <= TRUE;        -- end of stimulus events
        wait;                 -- wait for simulation to end
    end process; -- end of stimulus process

    CLOCK_CLK : process
    begin
        -- this process generates a clock with a CLK_PERIOD period and 50% 
        -- duty cycle. stop the clock when end of simulation is reached
        if END_SIM = FALSE then
            CLK <= '0';
            wait for (CLK_PERIOD/2);
        else
            wait;
        end if;

        if END_SIM = FALSE then
            CLK <= '1';
            wait for (CLK_PERIOD/2);
        else
            wait;
        end if;
    end process;

end TB_ARCHITECTURE;

configuration TESTBENCH_FOR_REG_tb of REG_tb is
    for TB_ARCHITECTURE
        for UUT : Registers
            use entity work.Registers(data_flow);
        end for;
    end for;
end TESTBENCH_FOR_REG_tb;
