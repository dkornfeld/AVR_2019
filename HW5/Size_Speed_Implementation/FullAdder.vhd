----------------------------------------------------------------------------------------------------
-- Author:          David Kornfeld and Bobby Abrahamson
-- Title:           FulLAdder
-- Description:     This file implements a simple one-bit full adder for use in the ALU multiplier.
--
-- Inputs:
--      A           (std_logic)     - Input 1
--      B           (std_logic)     - Input 2
--      Cin         (std_logic)     - CarryIn
--        
-- Outputs:
--      Sum         (std_logic)     - Sum bit
--      Cout        (std_logic)     - CarryOut
--
-- Revision History:
--      02/26/19    David Kornfeld      Initial Revision
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
----------------------------------------------------------------------------------------------------
entity FullAdder is
    port         (
        A       :   in  std_logic;
        B       :   in  std_logic;
        Cin     :   in  std_logic;
        Sum     :   out std_logic;
        Cout    :   out std_logic
    );
end FullAdder;
----------------------------------------------------------------------------------------------------
architecture data_flow of FullAdder is
begin
    -- Textbook formulas
    Sum     <= A xor B xor Cin;
    Cout    <= (A and B) or (Cin and (A xor B));
end architecture;