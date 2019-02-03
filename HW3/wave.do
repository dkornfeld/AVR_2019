onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /alu_tb/U_Control/clock
add wave -noupdate /alu_tb/U_Control/IR
add wave -noupdate -radix hexadecimal /alu_tb/UUT/OperandA
add wave -noupdate -radix hexadecimal /alu_tb/UUT/OperandB
add wave -noupdate -radix hexadecimal /alu_tb/UUT/Result
add wave -noupdate -expand /alu_tb/UUT/NewFlags
add wave -noupdate /alu_tb/UUT/CarryFlag
add wave -noupdate /alu_tb/UUT/TFlag
add wave -noupdate /alu_tb/UUT/N_AddMask
add wave -noupdate /alu_tb/UUT/FSRControl
add wave -noupdate /alu_tb/UUT/Subtract
add wave -noupdate /alu_tb/UUT/CarryInControl
add wave -noupdate /alu_tb/UUT/ALUResultSel
add wave -noupdate /alu_tb/UUT/TSCBitSelect
add wave -noupdate /alu_tb/UUT/TLoad
add wave -noupdate /alu_tb/UUT/BitSetClear
add wave -noupdate /alu_tb/UUT/SettingClearing
add wave -noupdate /alu_tb/UUT/Fout
add wave -noupdate /alu_tb/UUT/AdderInA
add wave -noupdate /alu_tb/UUT/AdderInB
add wave -noupdate /alu_tb/UUT/Sum
add wave -noupdate /alu_tb/UUT/Carries
add wave -noupdate /alu_tb/UUT/CarryIn
add wave -noupdate /alu_tb/UUT/CarryOut
add wave -noupdate /alu_tb/UUT/SRout
add wave -noupdate /alu_tb/UUT/RegIndex
add wave -noupdate /alu_tb/UUT/pre_result
add wave -noupdate /alu_tb/UUT/pre_flags
add wave -noupdate /alu_tb/U_Control/SREG
add wave -noupdate /alu_tb/U_Control/ProgDB
add wave -noupdate /alu_tb/U_Control/DataRd
add wave -noupdate /alu_tb/U_Control/DataWr
add wave -noupdate /alu_tb/U_Control/IOSel
add wave -noupdate /alu_tb/U_Control/RegInSel
add wave -noupdate /alu_tb/U_Control/OPBInSel
add wave -noupdate /alu_tb/U_Control/DBSel
add wave -noupdate /alu_tb/U_Control/RegASel
add wave -noupdate /alu_tb/U_Control/RegBSel
add wave -noupdate /alu_tb/U_Control/RegWrSel
add wave -noupdate /alu_tb/U_Control/RegWr
add wave -noupdate /alu_tb/U_Control/AddrDataIn
add wave -noupdate /alu_tb/U_Control/AddrRegSel
add wave -noupdate /alu_tb/U_Control/AddrRegWrSel
add wave -noupdate /alu_tb/U_Control/SFlag
add wave -noupdate /alu_tb/U_Control/N_AddMask
add wave -noupdate /alu_tb/U_Control/FSRControl
add wave -noupdate /alu_tb/U_Control/Subtract
add wave -noupdate /alu_tb/U_Control/CarryInControl
add wave -noupdate /alu_tb/U_Control/ALUResultSel
add wave -noupdate /alu_tb/U_Control/TSCBitSelect
add wave -noupdate /alu_tb/U_Control/TLoad
add wave -noupdate /alu_tb/U_Control/BitSetClear
add wave -noupdate /alu_tb/U_Control/SettingClearing
add wave -noupdate /alu_tb/U_Control/PCUpdateEn
add wave -noupdate /alu_tb/U_Control/N_PCLoad
add wave -noupdate /alu_tb/U_Control/PCControl
add wave -noupdate /alu_tb/U_Control/HiLoSel
add wave -noupdate /alu_tb/U_Control/N_Inc
add wave -noupdate /alu_tb/U_Control/N_OffsetMask
add wave -noupdate /alu_tb/U_Control/PrePostSel
add wave -noupdate -radix unsigned /alu_tb/U_Control/instr_cycle
add wave -noupdate /alu_tb/U_Control/reset_instr_counter
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {930165 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 203
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {871501 ps} {960121 ps}
