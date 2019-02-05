----------------------------------------------------------------------------------------
-- Author: 			David Kornfeld and Bobby Abrahamson
-- Title:			ALU
-- Description:  	This file implements the ALU Array for the AVR_2019 CPU designed
-- 					by Bobby Abrahamson and David Kornfeld. It can perform any logical
--						operation, any shift operation, and any arithmetic operation on
--						2 operands of NUM_BITS size. Once the result is computed, if the flags
--						in the status register are commanded to update, they do so based on the
--						computed result. The result is available as an output. The architecture
--						for this system can be seen in the block diagram. 
--
--	Parameters:
--		NUM_BITS	(integer range 2 to Infinity) - 	The number of bits used to represent the
--																numbers. 
--
-- Inputs:
--		clock				(std_logic)												- Clock input
--		OperandA      	(std_logic_vector(NUM_BITS-1 downto 0))		- Input A to ALU
--		OperandB      	(std_logic_vector(NUM_BITS-1 downto 0))		- Input A to ALU
--		Control Signals: ##################################################################
--		N_AddMask		(std_logic) 								- Active low mask for Operand A
--		FControl			(std_logic_vector(3 downto 0))		- F block control lines
--		Subtract			(std_logic)									- Command subtraction from adder
--		CarryInControl	(std_logic_vector(1 downto 0))		- Mux lines for carry in to adder
--		SRControl		(std_logic_vector(5 downto 0))		- Shifter/Rotator Control lines
--		ALUResultSel	(std_logic)									- Select between adder and SR
--		FlagMask			(std_logic_vector(NUM_FLAGS-1 downto 0))	- Mask for updating StatReg
--		
-- Outputs:
--		Result      	(std_logic_vector(NUM_BITS-1 downto 0))		- Output from ALU
--		StatReg      	(std_logic_vector(NUM_FLAGS-1 downto 0))		- Status register (flags)
--
-- Revision History:
-- 	01/24/19	David	Kornfeld		Initial Revision
-----------------------------------------------------------------------------------------
library  ieee;
-- Library instantiations
use	work.AVR_2019_constants.all;
-----------------------------------------------------------------------------------------
entity ALU is
	generic	(
		NUM_BITS			:	integer := NUM_BITS -- Number of bits to use (from header)
	);
	port 		(
		clock				:	in		std_logic;
		OperandA      	:	in		std_logic_vector(NUM_BITS-1 downto 0);
		OperandB      	:	in		std_logic_vector(NUM_BITS-1 downto 0);
		N_AddMask		:	in		std_logic;
		FControl			:	in		std_logic_vector(3 downto 0);
		Subtract			:	in		std_logic;
		CarryInControl	:	in		std_logic_vector(1 downto 0);
		SRControl		:	in		std_logic_vector(5 downto 0);
		ALUResultSel	:	in		std_logic;
		FlagMask			:	in		std_logic_vector(NUM_FLAGS-1 downto 0);
		Result      	:	out	std_logic_vector(NUM_BITS-1 downto 0);
		StatReg      	:	out	std_logic_vector(NUM_FLAGS-1 downto 0)
	);
end ALU;
-----------------------------------------------------------------------------------------