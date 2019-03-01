----------------------------------------------------------------------------------------------------
-- Author:          David Kornfeld and Bobby Abrahamson
-- Title:           Interrupt Vector Controller
-- Description:     This entity operates as the interrupt vector controller for the AVR_2019 CPU
--                  implementation. It takes in IRQ inputs from all peripherals and external pins
--                  and uses them to generate a master IRQ to the control unit. Additionally, it
--                  outputs the address of the required jump.
--
-- Inputs: (interrupt requests)
--      IRQClear        (std_logic)         - Clears the fact that an interrupt happened (taken)
--      RESET           (std_logic)         - hardware pin and watchdog restet  (active low)
--      INT0            (std_logic)         - external interrupt request 0      (active low)
--      INT1            (std_logic)         - external interrupt request 1      (active low)
--      T1CAP           (std_logic)         - timer 1 capture event             (active high)
---     T1CPA           (std_logic)         - timer 1 compare match A           (active high)
--      T1CPB           (std_logic)         - timer 1 compare match B           (active high)
--      T1OVF           (std_logic)         - timer 1 overflow                  (active high)
--      T0OVF           (std_logic)         - timer 0 overflow                  (active high)
--      IRQSPI          (std_logic)         - serial transfer complete          (active high)
--      UARTRX          (std_logic)         - UART receive complete             (active high)
--      UARTRE          (std_logic)         - UART data register empty          (active high)
--      UARTTX          (std_logic)         - UART transmit complete            (active high)
--      ANACMP          (std_logic)         - analog comparator                 (active high)
--        
-- Outputs:
--      IRQ             (std_logic)                             - Any interrupt requested
--      Vector_Address  (std_logic_vector(PC_WIDTH-1 downto 0)) - New execution address
--
-- Revision History:
--      02/25/19    David Kornfeld      Initial Revision
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.AVR_2019_constants.all;
----------------------------------------------------------------------------------------------------
entity IRQController is
    port (
        IRQClear    :   in std_logic;

        RESET       :   in std_logic;
        INT0        :   in std_logic;
        INT1        :   in std_logic;
        T1CAP       :   in std_logic;
        T1CPA       :   in std_logic;
        T1CPB       :   in std_logic;
        T1OVF       :   in std_logic;
        T0OVF       :   in std_logic;
        IRQSPI      :   in std_logic;
        UARTRX      :   in std_logic;
        UARTRE      :   in std_logic;
        UARTTX      :   in std_logic;
        ANACMP      :   in std_logic;

        IRQ             :   out std_logic;
        Vector_Address  :   out std_logic_vector(PC_WIDTH-1 downto 0)
    );
end IRQController;
----------------------------------------------------------------------------------------------------
architecture data_flow of IRQController is
    -- Constants for address vectors
    constant RESET_ADDRESS  :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0000";
    constant INT0_ADDRESS   :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0001";
    constant INT1_ADDRESS   :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0002";
    constant T1CAP_ADDRESS  :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0003";
    constant T1CPA_ADDRESS  :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0004";
    constant T1CPB_ADDRESS  :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0005";
    constant T1OVF_ADDRESS  :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0006";
    constant T0OVF_ADDRESS  :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0007";
    constant IRQSPI_ADDRESS :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0008";
    constant UARTRX_ADDRESS :   std_logic_vector(PC_WIDTH-1 downto 0) := X"0009";
    constant UARTRE_ADDRESS :   std_logic_vector(PC_WIDTH-1 downto 0) := X"000A";
    constant UARTTX_ADDRESS :   std_logic_vector(PC_WIDTH-1 downto 0) := X"000B";
    constant ANACMP_ADDRESS :   std_logic_vector(PC_WIDTH-1 downto 0) := X"000C";

    signal Pre_IRQ              :   std_logic;
    signal Pre_Vector_Address   :   std_logic_vector(PC_WIDTH-1 downto 0);
begin
    -- Generate the IRQ output (if any interrupt occured) ##########################################
    Pre_IRQ <=  (not INT0)  or  -- Active low
                (not INT1)  or  -- Active low
                T1CAP       or  -- Active high
                T1CPA       or  -- Active high
                T1CPB       or  -- Active high
                T1OVF       or  -- Active high
                T0OVF       or  -- Active high
                IRQSPI      or  -- Active high
                UARTRX      or  -- Active high
                UARTRE      or  -- Active high
                UARTTX      or  -- Active high
                ANACMP;         -- Active high
    process(Pre_IRQ, IRQClear)
    begin
        if RESET = '0' or IRQClear = '1' then
            IRQ <= '0';
        elsif rising_edge(Pre_IRQ) then
            IRQ <= '1';
        end if;
    end process;

    -- Output the proper address ###################################################################
    process (RESET, INT0, INT1, T1CAP, T1CPA, T1CPB, T1OVF, T0OVF, IRQSPI, UARTRX, UARTRE, UARTTX, ANACMP)
    begin
        if (INT0 = '0') then                        -- Priority given in this order
            Pre_Vector_Address <= INT0_ADDRESS;
        elsif (INT1 = '0') then
            Pre_Vector_Address <= INT1_ADDRESS;
        elsif (T1CAP = '1') then
            Pre_Vector_Address <= T1CAP_ADDRESS;
        elsif (T1CPA = '1') then
            Pre_Vector_Address <= T1CPA_ADDRESS;
        elsif (T1CPB = '1') then
            Pre_Vector_Address <= T1CPB_ADDRESS;
        elsif (T1OVF = '1') then
            Pre_Vector_Address <= T1OVF_ADDRESS;
        elsif (T0OVF = '1') then
            Pre_Vector_Address <= T0OVF_ADDRESS;
        elsif (IRQSPI = '1') then
            Pre_Vector_Address <= IRQSPI_ADDRESS;
        elsif (UARTRX = '1') then
            Pre_Vector_Address <= UARTRX_ADDRESS;
        elsif (UARTRE = '1') then
            Pre_Vector_Address <= UARTRE_ADDRESS;
        elsif (UARTTX = '1') then
            Pre_Vector_Address <= UARTTX_ADDRESS;
        elsif (ANACMP = '1') then
            Pre_Vector_Address <= ANACMP_ADDRESS;
        end if;
    end process;

    process(Pre_IRQ)
    begin
        if rising_edge(Pre_IRQ) then
            Vector_Address <= Pre_Vector_Address;
        end if;
    end process;
end architecture;