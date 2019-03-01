-- Title:   Test Bench for AVR CPU
-- Author:  David Kornfeld and Bobby Abrahamson
--
-- This file serves as a testbench for the full AVR CPU as it is implemented for grading 
-- testing. A customized version of Glen's Test ROM is executed, testing in addition
-- to the baseline the correctness of the MUL, SBI/CBI, IN/OUT, and NOP/SLEEP/WDR instructions.
-- In addition, asserts an interrupt shortly into execution, validating both interrupt
-- vector correctness and the correctness of prioritization when multiple interrupts are
-- asserted. Shortly after, a RESET is asserted, ensuring both a clean state for the program
-- and verifying that the CPU can safely be reset in the middle of execution. These tests are
-- intended to demonstrate the reliability of the processor.
--
-- Revision History:
--      02/28/19    David Kornfeld      Initial Revision
--      02/28/19    Bobby Abrahamson    Update documentation, implement much of TB.
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AVR_2019_constants.all;
use work.opcodes.all;
----------------------------------------------------------------------------------------------------
entity AVRCPUE_tb is
end AVRCPUE_tb;
----------------------------------------------------------------------------------------------------
architecture TB_ARCHITECTURE of AVRCPUE_tb is    
    -- Timing Constants ----------------------------------------------------------------------------
    
    -- Clock period
    constant CLK_PERIOD_INT :   integer := 20; -- ns
    constant CLK_PERIOD     :   time    := CLK_PERIOD_INT * (1 ns);

    -- System clock and reset
    signal CLK              :   std_logic;
    signal Reset            :   std_logic;
    
    -- Signal used to stop clock signal generators
    signal END_SIM           :   BOOLEAN := FALSE;

    -- Intermediate signals used to connect components
    signal    ProgDB    :   std_logic_vector(15 downto 0);
    signal    ProgAB    :   std_logic_vector(15 downto 0);
    signal    DataAB    :   std_logic_vector(15 downto 0);
    signal    DataWr    :   std_logic;
    signal    DataRd    :   std_logic;
    signal    DataDB    :   std_logic_vector(7 downto 0);

    -- Interrupt Stimuli
    signal    INT0      :   std_logic;
    signal    INT1      :   std_logic;
    signal    T1CAP     :   std_logic;
    signal    T1CPA     :   std_logic;
    signal    T1CPB     :   std_logic;
    signal    T1OVF     :   std_logic;
    signal    T0OVF     :   std_logic;
    signal    IRQSPI    :   std_logic;
    signal    UARTRX    :   std_logic;
    signal    UARTRE    :   std_logic;
    signal    UARTTX    :   std_logic;
    signal    ANACMP    :   std_logic;

begin -- ###########################################################################################
    -- Unit Under Test port map
    -- Component declaration of the tested unit #################################################### 
    UUT : entity work.AVR_CPU
    port map(
        ProgDB => ProgDB ,  
        Reset  => Reset  , 
        INT0   => INT0  ,
        INT1   => INT1  ,
        T1CAP  => T1CAP ,
        T1CPA  => T1CPA ,
        T1CPB  => T1CPB ,
        T1OVF  => T1OVF ,
        T0OVF  => T0OVF ,
        IRQSPI => IRQSPI,
        UARTRX => UARTRX,
        UARTRE => UARTRE,
        UARTTX => UARTTX,
        ANACMP => ANACMP, 
        clock  => CLK    ,  
        ProgAB => ProgAB ,  
        DataAB => DataAB ,  
        DataWr => DataWr ,  
        DataRd => DataRd ,  
        DataDB => DataDB  
    );

    RAM : entity work.DATA_MEMORY
    port map(
        RE    => DataRd ,  
        WE    => DataWr ,  
        DataAB=> DataAB ,  
        DataDB=> DataDB 
    );

    ROM : entity work.PROG_MEMORY
    port map(
        ProgAB=> ProgAB ,  
        Reset => Reset  ,  
        ProgDB=> ProgDB 
    );

    process
        
    begin  -- of stimulus process
        
        -- Reset for a few clocks
        Reset <= '0';

        -- And give it a few clocks
        wait for 5 * CLK_PERIOD;

        -- Begin the tests
        Reset <= '1';
        for i in 1 to 5000 loop
        
            wait for CLK_PERIOD; -- run for a set number of clock cycles

            -- 10 clocks in, assert an interrupt for 1 NS.
            if (i = 10) then
                INT0 <= '0';
                wait for 1 ns;
                INT0 <= '1';
            end if;

            -- 2 clocks later, assert a different, lower priority
            -- interrupt for 1 NS. The first, higher priority
            -- interrupt should have its vector taken.
            if (i = 12) then
                INT1 <= '0';
                wait for 1 ns;
                INT1 <= '1';
            end if;

            -- 95 clocks in, assert reset for 5 clock periods (-2ns).
            -- This should realign us with the expected clock period,
            -- And also test for our ability to reset mid-execution.
            if (i = 95) then
                Reset <= '0';
                wait for (5 * CLK_PERIOD) - 2 ns;
                Reset <= '1';
            end if;
            
        end loop;
            
        END_SIM <= TRUE;        -- end of stimulus events
        wait;                   -- wait for simulation to end
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
