----------------------------------------------------------------------------
--
--  Atmel AVR Program Memory
--
--  This component describes a program for the AVR CPU.  It creates the 
--  program in a small (334 x 16) ROM.
--
--  Revision History:
--     11 May 00  Glen George       Initial revision (from 5/9/00 version of 
--                                  progmem.vhd).
--     28 Jul 00  Glen George       Added instructions and made memory return
--                                  NOP when not mapped.
--      7 Jun 02  Glen George       Updated commenting.
--     16 May 04  Glen George       Added more instructions for testing and
--                                  updated commenting.
--     21 Jan 08  Glen George       Updated commenting.
--     17 Jan 18  Glen George       Updated commenting.
--
----------------------------------------------------------------------------


--
--  PROG_MEMORY
--
--  This is the program memory component.  It is just a 334 word ROM with no
--  timing information.  It is meant to be connected to the AVR CPU.  The ROM
--  is always enabled and may be changed when Reset it active.
--
--  Inputs:
--    ProgAB - address bus (16 bits)
--    Reset  - system reset (active low)
--
--  Outputs:
--    ProgDB - program memory data bus (16 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity  PROG_MEMORY  is

    port (
        ProgAB  :  in   std_logic_vector(15 downto 0);  -- program address bus
        Reset   :  in   std_logic;                      -- system reset
        ProgDB  :  out  std_logic_vector(15 downto 0)   -- program data bus
    );

end  PROG_MEMORY;


architecture  ROM  of  PROG_MEMORY  is

    -- define the type for the ROM (an array)
    type  ROMtype  is array(0 to 413) of std_logic_vector(15 downto 0);

    -- define the actual ROM (initialized to a simple program)
    signal  ROMbits  :  ROMtype  :=  ( X"9488", X"94f8", X"94c8", X"94b8",
                                       X"9498", X"94d8", X"94a8", X"94e8",
                                       X"9448", X"9418", X"9478", X"9408",
                                       X"9428", X"9468", X"9458", X"9438",
                                       X"e000", X"2e00", X"2e10", X"2e20",
                                       X"2e30", X"2e40", X"2e50", X"2e60",
                                       X"2e70", X"f807", X"f813", X"f821",
                                       X"f836", X"f840", X"f855", X"f864",
                                       X"f872", X"ed4f", X"2e84", X"e044",
                                       X"2e94", X"e74f", X"2ea4", X"e041",
                                       X"2eb4", X"ef4d", X"2ec4", X"e440",
                                       X"2ed4", X"ef47", X"2ee4", X"e140",
                                       X"2ef4", X"fa85", X"fa92", X"faa7",
                                       X"fab0", X"fac1", X"fad6", X"fae3",
                                       X"faf4", X"ef0f", X"ef1f", X"e020",
                                       X"e730", X"e040", X"e75e", X"e76e",
                                       X"e870", X"e485", X"e890", X"efa0",
                                       X"efbf", X"e5c5", X"eada", X"e7e0",
                                       X"e3ff", X"1f01", X"1f02", X"1f21",
                                       X"1f95", X"1d60", X"0f95", X"0f01",
                                       X"0f24", X"0d70", X"9600", X"9650",
                                       X"231c", X"231d", X"20d0", X"7fcf",
                                       X"7f0f", X"7fdf", X"9505", X"95b5",
                                       X"95e5", X"9485", X"9500", X"9500",
                                       X"95c0", X"95c0", X"1710", X"175a",
                                       X"15f3", X"0710", X"0751", X"0715",
                                       X"0750", X"e4e0", X"e7ff", X"371f",
                                       X"37e0", X"3af0", X"2fe3", X"951a",
                                       X"940a", X"95ea", X"951a", X"2f1c",
                                       X"271d", X"271c", X"2721", X"2720",
                                       X"2788", X"9583", X"9563", X"9563",
                                       X"9403", X"95aa", X"e8f0", X"95a6",
                                       X"95e6", X"9546", X"95f6", X"9501",
                                       X"9561", X"9401", X"9551", X"2b21",
                                       X"2b2c", X"2b61", X"6f1f", X"679d",
                                       X"9517", X"9537", X"9507", X"9407",
                                       X"9507", X"e5f0", X"0b01", X"47f0",
                                       X"0aa4", X"e79f", X"e781", X"47af",
                                       X"0b91", X"4a80", X"e08d", X"e090",
                                       X"9740", X"9700", X"e7ef", X"e7ff",
                                       X"1b01", X"1be1", X"1bf4", X"e5e0",
                                       X"e7f1", X"574f", X"57e0", X"5af0",
                                       X"9552", X"94a2", X"95b2", X"93af",
                                       X"93bf", X"93cf", X"93df", X"93ef",
                                       X"93ff", X"900f", X"901f", X"efbf",
                                       X"efaf", X"efdf", X"ecc0", X"e0f0",
                                       X"e8e0", X"9200", X"5555", X"9210",
                                       X"aaaa", X"922c", X"923e", X"924d",
                                       X"925d", X"926c", X"9279", X"8288",
                                       X"929a", X"82a8", X"aebc", X"82ca",
                                       X"8ade", X"82e9", X"92f1", X"8300",
                                       X"9312", X"8320", X"8f36", X"8341",
                                       X"af57", X"a360", X"efdf", X"eec0",
                                       X"937a", X"9389", X"af9f", X"e0b0",
                                       X"e0a0", X"efdf", X"efcf", X"efff",
                                       X"ece0", X"9000", X"aaaa", X"9010",
                                       X"5555", X"907c", X"909e", X"914d",
                                       X"915c", X"9069", X"8178", X"916a",
                                       X"a0f8", X"9041", X"80d0", X"9022",
                                       X"ad14", X"940c", X"00f2", X"e56a",
                                       X"e57a", X"c002", X"ea85", X"cffb",
                                       X"efe9", X"e0f0", X"9409", X"e0b0",
                                       X"e0c0", X"940e", X"0146", X"d04a",
                                       X"e4e6", X"e0f1", X"9509", X"17cb",
                                       X"f010", X"940c", X"00ff", X"f3dc",
                                       X"f3d1", X"f411", X"940c", X"00ff",
                                       X"e659", X"0f55", X"f7a5", X"2bbb",
                                       X"f012", X"940c", X"00ff", X"2bcc",
                                       X"f372", X"f412", X"940c", X"00ff",
                                       X"2bbb", X"f74a", X"1bcb", X"f013",
                                       X"940c", X"00ff", X"95ca", X"f71b",
                                       X"30b1", X"f70c", X"94f8", X"f2ff",
                                       X"940e", X"014a", X"f6e7", X"fbe1",
                                       X"f6d6", X"fbe3", X"f2c6", X"0fee",
                                       X"f410", X"940c", X"00ff", X"0fee",
                                       X"f690", X"f015", X"940c", X"00ff",
                                       X"1367", X"cffe", X"1367", X"940c",
                                       X"0130", X"1368", X"e860", X"fd66",
                                       X"ef6f", X"fd63", X"940c", X"0130",
                                       X"fd67", X"ea65", X"ff60", X"e060",
                                       X"ff65", X"940c", X"0130", X"ff61",
                                       X"940c", X"0195", X"efbf", X"e7cf",
                                       X"e0d0", X"9508", X"ef9f", X"e7af",
                                       X"e6e6", X"9518", X"0000", X"9588",
                                       X"95a8", X"940c", X"0000", X"9800",
                                       X"b160", X"2f76", X"b971", X"980f",
                                       X"b161", X"2f76", X"b972", X"9814",
                                       X"b162", X"2f76", X"b973", X"981b",
                                       X"b163", X"2f76", X"b974", X"9821",
                                       X"b164", X"2f76", X"b975", X"982d",
                                       X"b165", X"2f76", X"b976", X"9832",
                                       X"b166", X"2f76", X"b977", X"983e",
                                       X"b167", X"2f76", X"b978", X"9a44",
                                       X"b168", X"2f76", X"b979", X"9a49",
                                       X"b169", X"2f76", X"b97a", X"9a57",
                                       X"b16a", X"2f76", X"b97b", X"9a58",
                                       X"b16b", X"2f76", X"b97c", X"9a62",
                                       X"b16c", X"2f76", X"b97d", X"9a6e",
                                       X"b16d", X"2f76", X"b97e", X"9a75",
                                       X"b16e", X"2f76", X"b97f", X"9a7b",
                                       X"b16f", X"2f76", X"bb7e", X"940c",
                                       X"014e", X"e860", X"e072", X"9f76",
                                       X"9f67", X"ef6f", X"ef7f", X"9f76",
                                       X"940c", X"0153"                    );


begin


    -- always read the value at the current address
    ProgDB <= ROMbits(CONV_INTEGER(ProgAB)) when (CONV_INTEGER(ProgAB) <= ROMbits'high)  else
              X"E0C0";    -- NOP instruction


    -- process to handle Reset
    process(Reset)
    begin

        -- check if Reset is low now
        if  (Reset = '0')  then
            -- reset is active - initialize the ROM (nothing for now)
        end if;

    end process;

end  ROM;
