-- Title:   Test Bench for AVR CPU
-- Author:  David Kornfeld and Bobby Abrahamson
--
-- TODO
--
--
-- Revision History:
--      02/28/19    David Kornfeld      Initial Revision
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
    signal    INT0      :   std_logic;
    signal    INT1      :   std_logic;
    signal    ProgAB    :   std_logic_vector(15 downto 0);
    signal    DataAB    :   std_logic_vector(15 downto 0);
    signal    DataWr    :   std_logic;
    signal    DataRd    :   std_logic;
    signal    DataDB    :   std_logic_vector(7 downto 0);

begin -- ###########################################################################################
    -- Unit Under Test port map
    -- Component declaration of the tested unit #################################################### 
    UUT : entity work.AVR_CPU
    port map(
        ProgDB => ProgDB ,  
        Reset  => Reset  ,  
        INT0   => INT0   ,  
        INT1   => INT1   ,  
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
        for i in 1 to 1000 loop
        
            wait for CLK_PERIOD; -- run for a set number of clock cycles
            
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
