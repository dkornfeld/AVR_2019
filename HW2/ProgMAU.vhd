----------------------------------------------------------------------------------------
-- Author: 			David Kornfeld and Bobby Abrahamson
-- Title:			ProgMAU
-- Description:  	This file implements the Program Memory Access Unit for the AVR_2019 
--						CPU designed by Bobby Abrahamson and David Kornfeld. It manages and 
--						outputs the program counter as well as the program address bus. It
--						allows for loading or offset-adding to the program counter, and it can
--						skip multiple instructions if requested. For PUSH/POP instructions,
--						the PC flows through the Data Data bus, whereas Offsets are expected
--						to come from the instruction register. 
--			
--
--	Parameters: (from header)
--		PC_WIDTH				(integer range 2 to Infinity)	- Width of the Program Counter
--		NUM_BITS				(integer range 2 to Infinity) - The number of bits used to 
--																			represent the numbers in the 
--																			Data bus.
--		PROG_OFFSET_SIZE 	(integer range 2 to PC_WIDTH) - The size of offsets allowed
--
-- Inputs:
--		clock				(std_logic)														- clock input
--		Offset      	(std_logic_vector(PROG_OFFSET_SIZE-1 downto 0))		- Input A to ALU
--		Indirect      	(std_logic_vector(NUM_BITS-1 downto 0))				- Input A to ALU
--		Control Signals: ##################################################################
--		PCUpdateEn		(std_logic) 								- Enable PC to update
--		N_PCLoad			(std_logic_vector(3 downto 0))		- Active low load control for PC
--		PCControl		(std_logic_vector(2 downto 0))		- Mux input to adder control
--		HiLoSel			(std_logic)									- Selects if loading high or low
--																				part of PC
--		
-- Outputs:
--		ProgAB      	(std_logic_vector(PC_WIDTH-1 downto 0))		- Computed next Prog 
--																							address
--		PC      			(std_logic_vector(PC_WIDTH-1 downto 0))		- Current PC
--
-- Revision History:
-- 	01/24/19	David	Kornfeld		Initial Revision
-----------------------------------------------------------------------------------------
library  ieee;
-- Library instantiations
use	work.AVR_2019_constants.all;
-----------------------------------------------------------------------------------------
entity ProgMAU is
	generic	(
		PC_WIDTH				:	integer 	:= PC_WIDTH,
		NUM_BITS				:	integer 	:= NUM_BITS,
		PROG_OFFSET_SIZE	:	integer	:= PROG_OFFSET_SIZE
	);
	port 		(
		clock				std_logic;
		Offset      	std_logic_vector(PROG_OFFSET_SIZE-1 downto 0);
		Indirect      	std_logic_vector(NUM_BITS-1 downto 0);
		PCUpdateEn		std_logic;
		N_PCLoad			std_logic_vector(3 downto 0);
		PCControl		std_logic_vector(2 downto 0);
		HiLoSel			std_logic;
		ProgAB      	std_logic_vector(PC_WIDTH-1 downto 0);
		PC      			std_logic_vector(PC_WIDTH-1 downto 0)
	);
end ProgMAU;
-----------------------------------------------------------------------------------------