----------------------------------------------------------------------------------------
-- Author: 			David Kornfeld and Bobby Abrahamson
-- Title:			Registers
-- Description:  	This file implements the Registers for the AVR_2019 CPU designed
-- 					by Bobby Abrahamson and David Kornfeld.
--
--	Parameters:
--		NUM_BITS	(integer range 2 to Infinity) - 	The number of bits used to represent the
--																numbers. 
--
-- Inputs:
--		OperandA      	(std_logic_vector(NUM_BITS-1 downto 0))		- Input A to ALU
--		OperandB      	(std_logic_vector(NUM_BITS-1 downto 0))		- Input A to ALU
--		CarryFlag		(std_logic)									- Carry flag from the SREG
--		TFlag				(std_logic)									- T Flag from the SREG
--		Control Signals: ##################################################################
--		N_AddMask		(std_logic) 								- Active low mask for Operand A
--		FSRControl		(std_logic_vector(3 downto 0))		- F block and shifter control lines
--		Subtract			(std_logic)									- Command subtraction from adder
--		CarryInControl	(std_logic_vector(1 downto 0))		- Mux lines for carry in to adder
--		ALUResultSel	(std_logic)									- Select between adder and SR
--		TBitSelect		(std_logic_vector(2 downto 0))		- Select which register bit
--																				for loading/storing T
--		TLoad				(std_logic)									- Indicate if loading from T flag
--		
-- Outputs:
--		Result      	(std_logic_vector(NUM_BITS-1 downto 0))		- Output from ALU
--		NewFlags			(std_logic_vector(NUM_FLAGS-2 downto 0))		- New flags to SREG
--																						- (minus I Flag)
--
-- Revision History:
-- 	    01/30/19	Bobby Abrahamson		Initial Revision
--		01/31/19	Bobby Abrahamson		Implemented first code from block diagram
-----------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.AVR_2019_constants.all;
-----------------------------------------------------------------------------------------
entity Registers is
	generic	(
		NUM_BITS			:	integer := NUM_BITS   -- Number of bits to use for a GPR.
	);
	port (
		clock       : in std_logic;                             -- input clock
        RegWr       : in std_logic;                             -- select write vs not write
        RegWrSel    : in std_logic_vector(4 downto 0);          -- select register to write to
        RegASel     : in std_logic_vector(4 downto 0);          -- select register A to read from
        RegBSel     : in std_logic_vector(4 downto 0);          -- select register B to read from
        RegIn       : in std_logic_vector(NUM_BITS-1 downto 0); -- Input data to write
        
        -- SREG, TODO: Move to ALU?
        SFlag       : in std_logic;
        SMask    : in std_logic_vector(NUM_BITS-1 downto 0);
		  NewFlags : in std_logic_vector(NUM_FLAGS-2 downto 0);
        
        RegAOutput : out std_logic_vector(NUM_BITS-1 downto 0); -- Output register A
        RegBOutput : out std_logic_vector(NUM_BITS-1 downto 0) -- Output register B
	);
end Registers;

architecture data_flow of Registers is
	-- Constants for Addr Register readability
	constant X_LOW		:	integer := 26;
	constant X_HIGH	:	integer := 27;
	constant Y_LOW		:	integer := 28;
	constant Y_HIGH	:	integer := 29;
	constant Z_LOW		:	integer := 30;
	constant Z_HIGH	:	integer := 31;
    
    -- Constant for specifying the SREG
    constant SREG 	:	integer	:= 32;
    
    -- Utility constant, number of registers.
    constant NUM_GPRS : integer := 32;  -- Number of general purpose registers
    constant NUM_IORS : integer := 64; -- Number of IO registers
    constant NUM_REGS : integer := 96; -- Number of registers
    
    --Define registers, signals.
    type REG_ARRAY is array (0 to NUM_REGS-1) of std_logic_vector(NUM_BITS-1 downto 0);
    signal RegData : REG_ARRAY;
    signal RegA: std_logic_vector(NUM_BITS-1 downto 0); -- used for intermediate computations, will be useful for future assignments
    signal RegB: std_logic_vector(NUM_BITS-1 downto 0); -- used for intermediate computations, will be useful for future assignments
begin
    -- Handle RegA
    RegA <= RegData(conv_integer(RegASel)) when (conv_integer(RegASel) < NUM_GPRS) else (others => 'X');
    RegAOutput <= RegA;
    
    -- Handle RegB
	 RegB <= RegData(conv_integer(RegBSel)) when (conv_integer(RegBSel) < NUM_GPRS) else (others => 'X');
    RegBOutput <= RegB when SFlag = '0' else RegData(SREG);
            
    -- Write to a register when relevant
    process(clock, RegIn)
    begin
        if rising_edge(clock) then
            -- Write to register if requested
            if (RegWr = '1') then
                RegData(to_integer(unsigned(RegWrSel))) <= RegIn;
            end if;
            -- Set SREG with input if requested
            if (SFlag = '0') then
                for i in 0 to NUM_FLAGS-2 loop
                    RegData(SREG)(i) <= (SMask(i) and NewFlags(i)) or ((not SMask(i)) and RegData(SREG)(i));
                end loop;
            else -- (SFlag = '1')
					RegData(SREG) <= RegIn;
					
				end if;
        end if;
    end process;
end data_flow;