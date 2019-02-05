----------------------------------------------------------------------------------------
-- Author: 			David Kornfeld and Bobby Abrahamson
-- Title:			RegArray
-- Description:  	This file implements the Register Array for the AVR_2019 CPU designed
-- 					by Bobby Abrahamson and David Kornfeld. It contains a generic number
--						of general purpose registers (defined in a header file) as well as 
--						a generic number of registers that can be used for addressing. 
--				  		The Stack Pointer register is also contained in this register array
--						and it is treated as an Address Register. It is assumed that the
--						address bus is twice the width of the data bus. This dual-input dual-
--						output register array is controlled by signals from the control unit.
--						(IO functionality shown in diagram will be added later)
--
--	Parameters:
--		NUM_BITS	(integer range 2 to Infinity) - 	The number of bits used to represent the
--																numbers in the Data AB
--		DATA_OFFSET_WIDTH	(integer range 2 to NUM_BITS - The size of offsets allowed on Data DB
--		
--
-- Inputs:
--		clock				(std_logic)												- Clock input			
--		RegIn      		(std_logic_vector(NUM_BITS-1 downto 0))		- Input data to reg bank
--		Control Signals: ##################################################################
--		RegASel			(std_logic_vector(Log2(NUM_REG)-1 downto 0))	- Register A Select lines
--		RegBSel			(std_logic_vector(Log2(NUM_REG)-1 downto 0))	- Register B Select lines
--		RegWrSel			(std_logic_vector(Log2(NUM_REG)-1 downto 0))	- Reg Enable decoder lines
--		RegWr				(std_logic)												- Reg (write) Enable
--		AddrDataIn		(std_logic_vector(2*NUM_BITS-1 downto 0))		- Input data for address
--																							registers
--		AddrRegSel		(std_logic_vector(Log2(NUM_ADDR_REG)-1) downto 0)) - Address register 
--																								output select
--		AddrRegWrSel	(std_logic_vector(Log2(NUM_ADDR_REG)-1) downto 0)) - Address register 
--																								enable decoder lines
--								00 -> X
--								01 -> Y
--								10 -> Z
--								11 -> SP
--		AddrRegWr		(std_logic)												- Address Reg (write) En
--		
-- Outputs:
--		RegAOut			(std_logic_vector(NUM_BITS-1 downto 0))	- Output A
--		RegBOut			(std_logic_vector(NUM_BITS-1 downto 0))	- Output B
--		AddrDataOut		(std_logic_vector(2*NUM_BITS-1 downto 0))	- Address register output
--
-- Revision History:
-- 	01/24/19	David	Kornfeld		Initial Revision
-----------------------------------------------------------------------------------------
library  ieee;
-- Library instantiations
use	work.AVR_2019_constants.all;
-----------------------------------------------------------------------------------------
entity RegArray is
	generic	(
		NUM_BITS			:	integer := NUM_BITS -- Number of bits to use (from header)
	);
	port 		(
		clock				:	in		std_logic;
		RegIn      		:	in		std_logic_vector(NUM_BITS-1 downto 0);
		RegASel			:	in		std_logic_vector(4 downto 0);
		RegBSel			:	in		std_logic_vector(4 downto 0);
		RegWrSel			:	in		std_logic_vector(4 downto 0);
		RegWr				:	in		std_logic;
		AddrDataIn		:	in		std_logic_vector(2*NUM_BITS-1 downto 0);
		AddrRegSel		:	in		std_logic_vector(1 downto 0);
		AddrRegWrSel	:	in		std_logic_vector(1 downto 0);
		AddrRegWr		:	in		std_logic;
		RegAOut			:	out	std_logic_vector(NUM_BITS-1 downto 0);
		RegBOut			:	out	std_logic_vector(NUM_BITS-1 downto 0);
		AddrDataOut		:	out	std_logic_vector(2*NUM_BITS-1 downto 0)
	);
end RegArray;
-----------------------------------------------------------------------------------------