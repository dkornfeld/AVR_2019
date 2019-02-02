onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /alu_tb/U_Control/IR
add wave -noupdate /alu_tb/U_Control/clock
add wave -noupdate /alu_tb/U_Control/instr_cycle
add wave -noupdate /alu_tb/U_Control/reset_instr_counter
add wave -noupdate -radix hexadecimal /alu_tb/UUT/OperandA
add wave -noupdate -radix hexadecimal /alu_tb/UUT/OperandB
add wave -noupdate /alu_tb/UUT/CarryFlag
add wave -noupdate /alu_tb/UUT/TFlag
add wave -noupdate -radix hexadecimal /alu_tb/UUT/Result
add wave -noupdate -expand /alu_tb/UUT/NewFlags
add wave -noupdate /alu_tb/UUT/N_AddMask
add wave -noupdate /alu_tb/UUT/FSRControl
add wave -noupdate /alu_tb/UUT/Subtract
add wave -noupdate /alu_tb/UUT/CarryInControl
add wave -noupdate /alu_tb/UUT/ALUResultSel
add wave -noupdate /alu_tb/UUT/FlagMask
add wave -noupdate /alu_tb/UUT/TBitSelect
add wave -noupdate /alu_tb/UUT/TLoad
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
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {702799 ps} 0}
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
WaveRestoreZoom {690317 ps} {805773 ps}
