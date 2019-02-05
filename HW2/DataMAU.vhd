----------------------------------------------------------------------------------------
-- Author: 			David Kornfeld and Bobby Abrahamson
-- Title:			DataMAU
-- Description:  	This file implements the Data Memory Access Unit for the AVR_2019 
--						CPU designed by Bobby Abrahamson and David Kornfeld. It manages and 
--						outputs the data address bus. These can be sourced from the address
--						registers in the register bank, and it outputs the updated value
--						for updating the address register. Additionally, offsets can be added
--						to any of the address registers before the value is put on the bus.
--			
--
--	Parameters: (from header)
--		NUM_BITS				(integer range 2 to Infinity) - The number of bits used to 
--																			represent the numbers in the 
--																			Data bus.
--		DATA_OFFSET_SIZE 	(integer range 2 to NUM_BITS) - The size of offsets allowed
--
-- Inputs:
--		Offset      	(std_logic_vector(DATA_OFFSET_SIZE-1 downto 0))		- Input A to ALU
--		InpAddrData		(std_logic_vector(2*NUM_BITS-1 downto 0))				- Input address reg
--		Control Signals: ##################################################################
--		N_Inc				(std_logic) 								- Active low increment select
--		N_OffsetMask	(std_logic) 								- Active low mask for offset inp.
--		PrePostSel		(std_logic) 								- Select between pre-post inc
--																				part of PC
--		
-- Outputs:
--		DataAB      	(std_logic_vector(NUM_BITS-1 downto 0))		- Computed data address
--		CompAddrData	(std_logic_vector(2*NUM_BITS-1 downto 0))		- Updated address reg
--
-- Revision History:
-- 	01/24/19	David	Kornfeld		Initial Revision
-----------------------------------------------------------------------------------------
library  ieee;
-- Library instantiations
use	work.AVR_2019_constants.all;
-----------------------------------------------------------------------------------------
entity DataMAU is
	generic	(
		NUM_BITS				:	integer 	:= NUM_BITS,
		DATA_OFFSET_SIZE	:	integer	:= DATA_OFFSET_SIZE
	);
	port 		(
		Offset      	:	in		std_logic_vector(DATA_OFFSET_SIZE-1 downto 0);
		InpAddrData		:	in		std_logic_vector(2*NUM_BITS-1 downto 0);
		N_Inc				:	in		std_logic;
		N_OffsetMask	:	in		std_logic;
		PrePostSel		:	in		std_logic;
		DataAB      	:	out	std_logic_vector(NUM_BITS-1 downto 0);
		CompAddrData	:	out	std_logic_vector(2*NUM_BITS-1 downto 0)
	);
end DataMAU;
-----------------------------------------------------------------------------------------