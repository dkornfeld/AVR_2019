; Bobby Abrahamson, David Kornfeld
; AVR ISA Test assembly file

;  AVR ISA Test #############################################################

; This file contains linear tests intended to validate that the AVR processor
; implementation is working as intended.

; The public functions included are:
;    	None
;
; The local functions included are:
;    	None.
;
; Revision History:
;   1/22/18	Bobby Abrahamson	Began authoring tests
;   1/24/18	Bobby Abrahamson	Finished authoring tests

; We will start by defining helper macros, in order to simplify our lives later.
; These will each test whether a flag in SREG is set or unset.

; TESTEC will test whether the carry flag is set.
.MACRO          TESTEC
                BRCS	1
                NOP
                BRLO	1
                NOP
.ENDMACRO

; TESTLC will test whether the carry flag is clear.
.MACRO          TESTLC
                BRCC	1
                NOP
                BRSH	1
                NOP
.ENDMACRO

; TESTEN will test whether the negative flag is set.
.MACRO          TESTEN
                BRMI	1
                NOP
.ENDMACRO

; TESTLN will test whether the negative flag is clear.
.MACRO          TESTLN
                BRPL	1
                NOP
.ENDMACRO

; TESTEZ will test whether the zero flag is set.
.MACRO          TESTEZ
                BREQ	1
                NOP
.ENDMACRO

; TESTLZ will test whether the zero flag is clear.
.MACRO          TESTLZ
                BRNE	1
                NOP
.ENDMACRO

; TESTEV will test whether the overflow flag is set.
.MACRO          TESTEV
                BRVS	1
                NOP
.ENDMACRO

; TESTLV will test whether the overflow flag is clear.
.MACRO          TESTLV
                BRVS	1
                NOP
.ENDMACRO

; TESTEI will test whether the interrupt flag is set.
.MACRO          TESTEI
                BRIE	1
                NOP
.ENDMACRO

; TESTLI will test whether the interrupt flag is clear.
.MACRO          TESTLI
                BRID	1
                NOP
.ENDMACRO

; TESTEH will test whether the half-carry flag is set.
.MACRO          TESTEH
                BRHS	1
                NOP
.ENDMACRO

; TESTLH will test whether the half-carry flag is clear.
.MACRO          TESTLH
                BRHC	1
                NOP
.ENDMACRO

; TESTET will test whether the T bit is set.
.MACRO          TESTET
                BRTS	1
                NOP
.ENDMACRO

; TESTLT will test whether the T bit is clear.
.MACRO          TESTLT
                BRTC	1
                NOP
.ENDMACRO

; TESTES will test whether the signed flag (N xor V) is set.
.MACRO          TESTES
                BRLT	1
                NOP
.ENDMACRO

; TESTLS will test whether the signed flag (N xor V) is clear.
.MACRO          TESTLS
                BRGE	1
                NOP
.ENDMACRO

; TESTALU will test the flags Z, C, N, V, H, S in order, testing each for set and unset. 
; This will efficiently allow testing correct flag states following each arithmetic/logical operation.
.MACRO          TESTALU
                TESTEZ
                TESTLZ
                TESTEC
                TESTLC
                TESTEN
                TESTLN
                TESTEV
                TESTLV
                TESTEH
                TESTLH
                TESTES
                TESTLS
.ENDMACRO


; We will now test each SREG bit set/clear operation for all transitions.
; Clear carry for known good state
                CLC
; Set flag C, test transition 0 -> 1
                SEC
                TESTEC
                TESTLC
; Set flag C, test transition 1 -> 1
                SEC
                TESTEC
                TESTLC
; Clear flag C, test transition 1 -> 0
                CLC
                TESTEC
                TESTLC
; Clear flag C, test transition 0 -> 0
                CLC
                TESTEC
                TESTLC

; Clear interrupt for known good state
                CLI
; Set flag I, test transition 0 -> 1
                SEI
                TESTEI
                TESTLI
; Set flag I, test transition 1 -> 1
                SEI
                TESTEI
                TESTLI
; Clear flag I, test transition 1 -> 0
                CLI
                TESTEI
                TESTLI
; Clear flag I, test transition 0 -> 0
                CLI
                TESTEI
                TESTLI

; Clear half-carry for known good state
                CLH
; Set flag H, test transition 0 -> 1
                SEH
                TESTEH
                TESTLH
; Set flag H, test transition 1 -> 1
                SEH
                TESTEH
                TESTLH
; Clear flag H, test transition 1 -> 0
                CLH
                TESTEH
                TESTLH
; Clear flag H, test transition 0 -> 0
                CLH
                TESTEH
                TESTLH

; Clear negative for known good state
                CLN
; Set flag N, test transition 0 -> 1
                SEN
                TESTEN
                TESTLN
; Set flag N, test transition 1 -> 1
                SEN
                TESTEN
                TESTLN
; Clear flag N, test transition 1 -> 0
                CLN
                TESTEN
                TESTLN
; Clear flag N, test transition 0 -> 0
                CLN
                TESTEN
                TESTLN

; Clear signed for known good state
                CLS
; Set flag S, test transition 0 -> 1
                SES
                TESTES
                TESTLS
; Set flag S, test transition 1 -> 1
                SES
                TESTES
                TESTLS
; Clear flag S, test transition 1 -> 0
                CLS
                TESTES
                TESTLS
; Clear flag S, test transition 0 -> 0
                CLS
                TESTES
                TESTLS

; Clear T for known good state
                CLT
; Set flag T, test transition 0 -> 1
                SET
                TESTET
                TESTLT
; Set flag T, test transition 1 -> 1
                SET
                TESTET
                TESTLT
; Clear flag T, test transition 1 -> 0
                CLT
                TESTET
                TESTLT
; Clear flag T, test transition 0 -> 0
                CLT
                TESTET
                TESTLT

; Clear overflow for known good state
                CLV
; Set flag V, test transition 0 -> 1
                SEV
                TESTEV
                TESTLV
; Set flag V, test transition 1 -> 1
                SEV
                TESTEV
                TESTLV
; Clear flag V, test transition 1 -> 0
                CLV
                TESTEV
                TESTLV
; Clear flag V, test transition 0 -> 0
                CLV
                TESTEV
                TESTLV

; Clear zero for known good state
                SEZ
; Set flag Z, test transition 0 -> 1
                SEZ
                TESTEZ
                TESTLZ
; Set flag Z, test transition 1 -> 1
                SEZ
                TESTEZ
                TESTLZ
; Clear flag Z, test transition 1 -> 0
                CLZ
                TESTEZ
                TESTLZ
; Clear flag Z, test transition 0 -> 0
                CLZ
                TESTEZ
                TESTLZ

; Next we'll test the bit store instruction, and the swap instruction.
; In particular, we will store and validate every individual bit fron the pattern A5, and swap it to 5A.
; We will also check FF, and 00, which swap to themselves.
                LDI     R0, $A5
                BST     R0, 0 ; Set T = R0(0)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 1 ; Set T = R0(1)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 2 ; Set T = R0(2)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 3 ; Set T = R0(3)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 4 ; Set T = R0(4)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 5 ; Set T = R0(5)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 6 ; Set T = R0(6)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 7 ; Set T = R0(7)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                SWAP    R0    ; Swap nybbles, R0 = $5A
                BST     R0, 0 ; Set T = R0(0)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 1 ; Set T = R0(1)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 2 ; Set T = R0(2)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 3 ; Set T = R0(3)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 4 ; Set T = R0(4)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 5 ; Set T = R0(5)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 6 ; Set T = R0(6)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 7 ; Set T = R0(7)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                
                ; Test value FF
                LDI     R0, $FF
                BST     R0, 0 ; Set T = R0(0)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 1 ; Set T = R0(1)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 2 ; Set T = R0(2)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 3 ; Set T = R0(3)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 4 ; Set T = R0(4)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 5 ; Set T = R0(5)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 6 ; Set T = R0(6)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 7 ; Set T = R0(7)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                SWAP    R0    ; Swap nybbles, R0 = $FF
                BST     R0, 0 ; Set T = R0(0)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 1 ; Set T = R0(1)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 2 ; Set T = R0(2)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 3 ; Set T = R0(3)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 4 ; Set T = R0(4)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 5 ; Set T = R0(5)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 6 ; Set T = R0(6)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 7 ; Set T = R0(7)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                
                ; Test value $00
                LDI     R0, $00
                BST     R0, 0 ; Set T = R0(0)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 1 ; Set T = R0(1)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 2 ; Set T = R0(2)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 3 ; Set T = R0(3)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 4 ; Set T = R0(4)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 5 ; Set T = R0(5)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 6 ; Set T = R0(6)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 7 ; Set T = R0(7)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                SWAP    R0    ; Swap nybbles, R0 = $00
                BST     R0, 0 ; Set T = R0(0)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 1 ; Set T = R0(1)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 2 ; Set T = R0(2)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 3 ; Set T = R0(3)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 4 ; Set T = R0(4)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 5 ; Set T = R0(5)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 6 ; Set T = R0(6)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                BST     R0, 7 ; Set T = R0(7)
                TESTET        ; Test T set
                TESTLT        ; Test T clear
                
; Next, we will test the BLD instruction, and the CPI instruction
; We will use BLD and SET to load fixed patterns into registers,
; And then CPI to the expected immediates. We will check the zero
; flag to determine whether the result was what we expected.
; Test whether BLD can successfully give $A5
                CLR     R0
                SET ; Set T = ($A5 & $01) = 1
                BLD     R0, 0 ; Load T into R0(0)
                CLT ; Set T = ($A5 & $02) = 0
                BLD     R0, 1 ; Load T into R0(1)
                SET ; Set T = ($A5 & $04) = 1
                BLD     R0, 2 ; Load T into R0(2)
                CLT ; Set T = ($A5 & $08) = 0
                BLD     R0, 3 ; Load T into R0(3)
                CLT ; Set T = ($A5 & $10) = 0
                BLD     R0, 4 ; Load T into R0(4)
                SET ; Set T = ($A5 & $20) = 1
                BLD     R0, 5 ; Load T into R0(5)
                CLT ; Set T = ($A5 & $40) = 0
                BLD     R0, 6 ; Load T into R0(6)
                SET ; Set T = ($A5 & $80) = 1
                BLD     R0, 7 ; Load T into R0(7)
                CPI     R0, $A5 ; Compare to expected bit pattern $A5
                TESTEZ ; Test Z set
                TESTLZ ; Test Z unset

; Test whether BLD can successfully give $5A
                CLR     R1
                CLT ; Set T = ($5A & $01) = 0
                BLD     R1, 0 ; Load T into R1(0)
                SET ; Set T = ($5A & $02) = 1
                BLD     R1, 1 ; Load T into R1(1)
                CLT ; Set T = ($5A & $04) = 0
                BLD     R1, 2 ; Load T into R1(2)
                SET ; Set T = ($5A & $08) = 1
                BLD     R1, 3 ; Load T into R1(3)
                SET ; Set T = ($5A & $10) = 1
                BLD     R1, 4 ; Load T into R1(4)
                CLT ; Set T = ($5A & $20) = 0
                BLD     R1, 5 ; Load T into R1(5)
                SET ; Set T = ($5A & $40) = 1
                BLD     R1, 6 ; Load T into R1(6)
                CLT ; Set T = ($5A & $80) = 0
                BLD     R1, 7 ; Load T into R1(7)
                CPI     R1, $5A ; Compare to expected bit pattern $5A
                TESTEZ ; Test Z set
                TESTLZ ; Test Z unset

; Test whether BLD can successfully give $FF
                CLR     R2
                SET ; Set T = ($FF & $01) = 1
                BLD     R2, 0 ; Load T into R2(0)
                SET ; Set T = ($FF & $02) = 1
                BLD     R2, 1 ; Load T into R2(1)
                SET ; Set T = ($FF & $04) = 1
                BLD     R2, 2 ; Load T into R2(2)
                SET ; Set T = ($FF & $08) = 1
                BLD     R2, 3 ; Load T into R2(3)
                SET ; Set T = ($FF & $10) = 1
                BLD     R2, 4 ; Load T into R2(4)
                SET ; Set T = ($FF & $20) = 1
                BLD     R2, 5 ; Load T into R2(5)
                SET ; Set T = ($FF & $40) = 1
                BLD     R2, 6 ; Load T into R2(6)
                SET ; Set T = ($FF & $80) = 1
                BLD     R2, 7 ; Load T into R2(7)
                CPI     R2, $FF ; Compare to expected bit pattern $FF
                TESTEZ ; Test Z set
                TESTLZ ; Test Z unset

; Test whether BLD can successfully give $00
                CLR     R3
                CLT ; Set T = ($00 & $01) = 0
                BLD     R3, 0 ; Load T into R3(0)
                CLT ; Set T = ($00 & $02) = 0
                BLD     R3, 1 ; Load T into R3(1)
                CLT ; Set T = ($00 & $04) = 0
                BLD     R3, 2 ; Load T into R3(2)
                CLT ; Set T = ($00 & $08) = 0
                BLD     R3, 3 ; Load T into R3(3)
                CLT ; Set T = ($00 & $10) = 0
                BLD     R3, 4 ; Load T into R3(4)
                CLT ; Set T = ($00 & $20) = 0
                BLD     R3, 5 ; Load T into R3(5)
                CLT ; Set T = ($00 & $40) = 0
                BLD     R3, 6 ; Load T into R3(6)
                CLT ; Set T = ($00 & $80) = 0
                BLD     R3, 7 ; Load T into R3(7)
                CPI     R3, $00 ; Compare to expected bit pattern $00
                TESTEZ ; Test Z set
                TESTLZ ; Test Z unset
                
; Next, we will test CP, CPC, CPSE. We will do this by loading various bit patterns into registers, and then validating them.
; For CP, CPC we will use the TESTALU macro to check every flag.
; For CPSE, we will place a NOP following the instruction.
                LDI R0, $A5
                LDI R1, $5A
                LDI R2, $FF
                LDI R3, $00
                LDI R4, $A5
                
                ; Compare A5 == 5A, which are inverses of each other.
                CP  R0, R1
                TESTALU
                CPC R0, R1
                TESTALU
                CPSE R0, R1
                NOP
                
                ; Compare A5 == FF, not equal but Rr greater.
                CP  R0, R2
                TESTALU
                CPC R0, R2
                TESTALU
                CPSE R0, R2
                NOP
                
                ; Compare A5 == 00, not equal but Rr smaller.
                CP  R0, R3
                TESTALU
                CPC R0, R3
                TESTALU
                CPSE R0, R3
                NOP
                
                ; Compare A5 == A5, equal.
                CP  R0, R4
                TESTALU
                CPC R0, R4
                TESTALU
                CPSE R0, R4
                NOP
                
; Next we'll validate SBRC, and SBRS. We will use values $A5, $5A to test for each,
; These are inverse bit patterns and will allow us to test every bit for every value.
                ; Load bit pattern $A5 into R0
                LDI     R0, $A5
                SBRS    R0, 0 ; Skip if bit 0 is set in $A5 (it is)
                NOP
                SBRC    R0, 0 ; Skip if bit 0 is clear in $A5 (it is not)
                NOP
                SBRS    R0, 1 ; Skip if bit 1 is set in $A5 (it is not)
                NOP
                SBRC    R0, 1 ; Skip if bit 1 is clear in $A5 (it is)
                NOP
                SBRS    R0, 2 ; Skip if bit 2 is set in $A5 (it is)
                NOP
                SBRC    R0, 2 ; Skip if bit 2 is clear in $A5 (it is not)
                NOP
                SBRS    R0, 3 ; Skip if bit 3 is set in $A5 (it is not)
                NOP
                SBRC    R0, 3 ; Skip if bit 3 is clear in $A5 (it is)
                NOP
                SBRS    R0, 4 ; Skip if bit 4 is set in $A5 (it is not)
                NOP
                SBRC    R0, 4 ; Skip if bit 4 is clear in $A5 (it is)
                NOP
                SBRS    R0, 5 ; Skip if bit 5 is set in $A5 (it is)
                NOP
                SBRC    R0, 5 ; Skip if bit 5 is clear in $A5 (it is not)
                NOP
                SBRS    R0, 6 ; Skip if bit 6 is set in $A5 (it is not)
                NOP
                SBRC    R0, 6 ; Skip if bit 6 is clear in $A5 (it is)
                NOP
                SBRS    R0, 7 ; Skip if bit 7 is set in $A5 (it is)
                NOP
                SBRC    R0, 7 ; Skip if bit 7 is clear in $A5 (it is not)
                NOP
                
                ; Load bit pattern $5A into R1
                LDI     R1, $5A
                SBRS    R1, 0 ; Skip if bit 0 is set in $5A (it is not)
                NOP
                SBRC    R1, 0 ; Skip if bit 0 is clear in $5A (it is)
                NOP
                SBRS    R1, 1 ; Skip if bit 1 is set in $5A (it is)
                NOP
                SBRC    R1, 1 ; Skip if bit 1 is clear in $5A (it is not)
                NOP
                SBRS    R1, 2 ; Skip if bit 2 is set in $5A (it is not)
                NOP
                SBRC    R1, 2 ; Skip if bit 2 is clear in $5A (it is)
                NOP
                SBRS    R1, 3 ; Skip if bit 3 is set in $5A (it is)
                NOP
                SBRC    R1, 3 ; Skip if bit 3 is clear in $5A (it is not)
                NOP
                SBRS    R1, 4 ; Skip if bit 4 is set in $5A (it is)
                NOP
                SBRC    R1, 4 ; Skip if bit 4 is clear in $5A (it is not)
                NOP
                SBRS    R1, 5 ; Skip if bit 5 is set in $5A (it is not)
                NOP
                SBRC    R1, 5 ; Skip if bit 5 is clear in $5A (it is)
                NOP
                SBRS    R1, 6 ; Skip if bit 6 is set in $5A (it is)
                NOP
                SBRC    R1, 6 ; Skip if bit 6 is clear in $5A (it is not)
                NOP
                SBRS    R1, 7 ; Skip if bit 7 is set in $5A (it is not)
                NOP
                SBRC    R1, 7 ; Skip if bit 7 is clear in $5A (it is)
                NOP

; Test CLR, SER. These are straightforward, we will simply set and clear R0
; And check every ALU flag.
                ; First, Clear R0 to establish baseline value
                CLR R0
                ; Set R0, check all flags clear -> set.
                SER R0
                TESTALU
                ; Set R0, check all flags set -> set.
                SER R0
                TESTALU
                ; Clear R0, check all flags set -> clear
                CLR R0
                TESTALU
                ; Clear R0, check all flags clear -> clear
                TESTALU
                
; Test the TST instruction.
; We will simply load a few immediates, and check all flags.
                ; Check all-zero.
                LDI R0, $00
                TST R0
                TESTALU
                ; Check all-one.
                LDI R0, $FF
                TST R0
                TESTALU
                ; Check alternating bit pattern
                LDI R1, $A5
                TST R1
                TESTALU
                ; Check inverse alternating bit pattern
                LDI R2, $5A
                TST R2
                TESTALU
                
; Next, we will test INC and DEC.
; We will test INC by examining ALU flags for expected values in range [FE, 02] and [7E, 82]
; And test DEC by examining ALU flags for expected values in range [02, FE] and [82, 7E]
                ; Test a range that will include overflow, to validate flags work as expected
                LDI R0, $FE
                INC R0  ; R0 = FF
                TESTALU 
                INC R0  ; R0 = 00
                TESTALU
                INC R0  ; R0 = 01
                TESTALU
                INC R0  ; R0 = 02
                TESTALU
                DEC R0  ; R0 = 01
                TESTALU
                DEC R0  ; R0 = 00
                TESTALU
                DEC R0  ; R0 = FF
                TESTALU
                DEC R0  ; R0 = FE
                TESTALU
                
                ; Test range that wont' overflow (but would if signed) to validate flags for normal operation.
                LDI R0, $7E
                INC R0  ; R0 = 7F
                TESTALU 
                INC R0  ; R0 = 80
                TESTALU
                INC R0  ; R0 = 81
                TESTALU
                INC R0  ; R0 = 82
                TESTALU
                DEC R0  ; R0 = 81
                TESTALU
                DEC R0  ; R0 = 80
                TESTALU
                DEC R0  ; R0 = 7F
                TESTALU
                DEC R0  ; R0 = 7E
                TESTALU

                
; We'll now begin testing arithmetic operations.
; Validate addition works.
                ; Check that 0 + A5 = A5
                LDI R0, $00
                LDI R1, $A5
                ADD R0, R1
                TESTALU
                CP  R0, R1
                TESTALU
                ; Check that FF + A5 = A4, and overflow occurs
                LDI R0, $FF
                LDI R1, $A5
                LDI R2, $A4
                ADD R0, R1
                TESTALU
                CP  R0, R2
                TESTALU
                
                ; Perform similar validation for ADC.
                
                ; Check that 0 + A5 + 0 = A5
                CLC
                LDI R0, $00
                LDI R1, $A5
                ADC R0, R1
                TESTALU
                CP  R0, R1
                TESTALU
                ; Check that FF + A5 + 1 = A5, and overflow occurs
                SEC
                LDI R0, $FF
                LDI R1, $A5
                ADC R0, R1
                TESTALU
                CP  R0, R1
                TESTALU
            
; We'll now validate subtraction. We'll do the inverse of the operations from addition (A5 - 00 = A5, A5 - FF = A6)
; We will use the same subtractions for SUB, SUBI, SBC, SBCI
                ; Validate SUB
                ; Check that A5 - 0 = A5
                LDI R0, $A5
                LDI R1, $00
                LDI R2, $A5
                SUB R0, R1
                TESTALU
                CP  R0, R2
                TESTALU
                ; Check that A5 - FF = A6, and overflow occurs
                LDI R0, $A5
                LDI R1, $FF
                LDI R2, $A6
                SUB R0, R1
                TESTALU
                CP  R0, R2
                TESTALU
                
                ; Validate SUBI
                ; Check that A5 - 0 = A5
                LDI R0, $A5
                LDI R2, $A5
                SUB R0, $00
                TESTALU
                CP  R0, R2
                TESTALU
                ; Check that A5 - FF = A6, and overflow occurs
                LDI R0, $A5
                LDI R2, $A6
                SUB R0, $FF
                TESTALU
                CP  R0, R2
                TESTALU
                
                ; Validate SBC
                ; Check that A5 - 0 - 0 = A5
                CLC
                LDI R0, $A5
                LDI R1, $00
                LDI R2, $A5
                SUB R0, R1
                TESTALU
                CP  R0, R2
                TESTALU
                ; Check that A5 - FF - 1 = A5, and overflow occurs
                SEC
                LDI R0, $A5
                LDI R1, $FF
                LDI R2, $A5
                SUB R0, R1
                TESTALU
                CP  R0, R2
                TESTALU
                
                ; Validate SBCI
                ; Check that A5 - 0 - 0 = A5
                CLC
                LDI R0, $A5
                LDI R2, $A5
                SBC R0, $00
                TESTALU
                CP  R0, R2
                TESTALU
                ; Check that A5 - FF - 1 = A5, and overflow occurs
                SEC
                LDI R0, $A5
                LDI R2, $A5
                SUB R0, $FF
                TESTALU
                CP  R0, R2
                TESTALU
            
; Now, test AND and ANDI. We'll AND A5 with FF, A5, 5A, and 00.
; This should produce A5, A5, 00, 00, respectively.
                LDI R0, $A5
                LDI R1, $FF
                LDI R2, $A5
                
                AND R0, R1   ; Check A5 & FF = A5
                TESTALU
                CP  R0, R2
                TESTALU
                ANDI R0, $FF ; Check A5 & FF = A5
                TESTALU
                CP  R0, R2
                TESTALU
                
                LDI R1, $A5
                AND R0, R1   ; Check A5 & A5 = A5
                TESTALU
                CP  R0, R2
                TESTALU
                ANDI R0, $A5 ; Check A5 & A5 = A5
                TESTALU
                CP  R0, R2
                TESTALU
                
                LDI R0, $A5
                LDI R1, $5A
                CLR R2
                AND R0, R1   ; Check A5 & 5A = 00
                TESTALU
                CP  R0, R2
                TESTALU
                LDI R0, $A5
                ANDI R0, $5A ; Check A5 & 5A = 00
                TESTALU
                CP  R0, R2
                TESTALU
                
                LDI R0, $A5
                LDI R1, $00
                CLR R2
                AND R0, R1   ; Check A5 & 00 = 00
                TESTALU
                CP  R0, R2
                TESTALU
                LDI R0, $A5
                ANDI R0, $00 ; Check A5 & 00 = 00
                TESTALU
                CP  R0, R2
                TESTALU

; Now, test OR and ORI. We'll OR A5 with FF, A5, 5A, and 00.
; This should produce FF, A5, FF, A5, respectively.           
            LDI R0, $A5
            LDI R1, $FF
            LDI R2, $FF
            
            LDI R0, $A5
            OR R0, R1   ; Check A5 | FF = FF
            TESTALU
            CP  R0, R2
            TESTALU
            LDI R0, $A5
            ORI R0, $FF ; Check A5 | FF = FF
            TESTALU
            CP  R0, R2
            TESTALU
            
            LDI R0, $A5
            LDI R1, $A5
            LDI R2, $A5
            OR R0, R1   ; Check A5 | A5 = A5
            TESTALU
            CP  R0, R2
            TESTALU
            ORI R0, $A5 ; Check A5 | A5 = A5
            TESTALU
            CP  R0, R2
            TESTALU
            
            LDI R0, $A5
            LDI R1, $5A
            LDI R2, $FF
            OR R0, R1   ; Check A5 | 5A = FF
            TESTALU
            CP  R0, R2
            TESTALU
            LDI R0, $A5
            ORI R0, $5A ; Check A5 | 5A = FF
            TESTALU
            CP  R0, R2
            TESTALU
            
            LDI R0, $A5
            LDI R1, $00
            LDI R2, $A5
            OR R0, R1   ; Check A5 | 00 = A5
            TESTALU
            CP  R0, R2
            TESTALU
            ORI R0, $00 ; Check A5 | 00 = A5
            TESTALU
            CP  R0, R2
            TESTALU

; Now, test EOR. We will validate all the XORs of the group FF, A5, 5A, 00
            ; Load up registers. R0 will be our "work" register.
            LDI R0, $00
            LDI R1, $FF
            LDI R2, $A5
            LDI R3, $5A
            LDI R4, $00
            
            ; Validate 00 ^ FF = FF
            EOR R0, R1  
            TESTALU
            CP R0, R1
            TESTALU
            
            ; Validate FF ^ A5 = 5A
            EOR R0, R2
            TESTALU
            CP R0, R3
            TESTALU
            
            ; Validate 5A ^ A5 = FF
            EOR R0, R2
            TESTALU
            CP R0, R1
            TESTALU
            
            ; Validate FF ^ 00 = FF
            EOR R0, R4
            TESTALU
            CP R0, R1
            TESTALU
            
            ; Validate FF ^ FF = 00
            EOR R0, R1
            TESTALU
            CP R0, R4
            TESTALU

; Test NEG, COM. We will simply validate against expectations for the inverses of 00, A5, 5A, FF
            ; Validate COM 00 = FF, NEG 00 = 00
            LDI R0, $00
            LDI R1, $FF
            LDI R2, $00
            LDI R3, $00
            COM R0
            TESTALU
            CP R0, R1
            TESTALU
            NEG R2
            TESTALU
            CP R2, R3
            TESTALU
            
            ; Validate COM A5 = 5A, NEG A5 = 5B
            LDI R0, $A5
            LDI R1, $5A
            LDI R2, $A5
            LDI R3, $5B
            COM R0
            TESTALU
            CP R0, R1
            TESTALU
            NEG R2
            TESTALU
            CP R2, R3
            TESTALU
            
            ; Validate COM 5A = A5, NEG 5A = AB
            LDI R0, $5A
            LDI R1, $A5
            LDI R2, $5A
            LDI R3, $AB
            COM R0
            TESTALU
            CP R0, R1
            TESTALU
            NEG R2
            TESTALU
            CP R2, R3
            TESTALU
            
            ; Validate COM FF = 00, NEG FF = 01
            LDI R0, $FF
            LDI R1, $00
            LDI R2, $FF
            LDI R3, $01
            COM R0
            TESTALU
            CP R0, R1
            TESTALU
            NEG R2
            TESTALU
            CP R2, R3
            TESTALU
            
; Validate LSL, LSR instructions. These will both start with A5, and LSL/LSR 4 times, validating each result.
            LDI R0, $A5
            LDI R1, $A5
            
            ; Validate A5 >> 1 = 52
            LSR R0
            TESTALU
            CPI R0, $52
            TESTALU
            
            ; Validate A5 >> 2 = 29
            LSR R0
            TESTALU
            CPI R0, $29
            TESTALU
            
            ; Validate A5 >> 3 = 14
            LSR R0
            TESTALU
            CPI R0, $29
            TESTALU
            
            ; Validate A5 >> 4 = 0A
            LSR R0
            TESTALU
            CPI R0, $0A
            TESTALU
            
            ; Validate A5 << 1 = 4A
            LSL R1
            TESTALU
            CPI R1, $4A
            TESTALU
            
            ; Validate A5 << 2 = 94
            LSL R1
            TESTALU
            CPI R1, $94
            TESTALU
            
            ; Validate A5 << 3 = 28
            LSL R1
            TESTALU
            CPI R1, $28
            TESTALU
            
            ; Validate A5 << 4 = 50
            LSL R1
            TESTALU
            CPI R1, $50
            TESTALU
 
; Validate ASR works as expected. This will shift right A5 4 times, and 5A right four times.
            LDI R0, $A5
            
            ; Validate A5 >> 1 = D2
            ASR R0
            TESTALU
            CPI R0, $D2
            TESTALU
            
            ; Validate A5 >> 2 = E9
            ASR R0
            TESTALU
            CPI R0, $E9
            TESTALU
            
            ; Validate A5 >> 3 = F4
            ASR R0
            TESTALU
            CPI R0, $F4
            TESTALU
            
            ; Validate A5 >> 4 = FA
            ASR R0
            TESTALU
            CPI R0, $FA
            TESTALU
            
            LDI R0, $5A
            
            ; Validate 5A >> 1 = 2D
            ASR R0
            TESTALU
            CPI R0, $2D
            TESTALU
            
            ; Validate 5A >> 2 = 16
            ASR R0
            TESTALU
            CPI R0, $16
            TESTALU
            
            ; Validate 5A >> 3 = 0B
            ASR R0
            TESTALU
            CPI R0, $0B
            TESTALU
            
            ; Validate 5A >> 4 = 05
            ASR R0
            TESTALU
            CPI R0, $05
            TESTALU
            
; Validate ROR, ROL work as expected
; To do this, we will simply cycle through three right rotates of A5, and three left rotates of 5A.
            LDI R0, $A5
            LDI R1, $A5
            
            CLC ; Clear carry before beginning cycle
            ; Validate A5 >>> 1 = 52
            ROR R0
            TESTALU
            CPI R0, $52
            TESTALU
            SEC ; Carry should be 1.
            
            ; Validate A5 >>> 2 = A9
            ROR R0
            TESTALU
            CPI R0, $A9
            TESTALU
            CLC ; Carry should be 0
            
            ; Validate A5 >>> 3 = 54
            ROR R0
            TESTALU
            CPI R0, $54
            TESTALU
                        
            CLC ; Clear carry before beginning cycle
            ; Validate A5 <<< 1 = 4A
            ROL R1
            TESTALU
            CPI R1, $4A
            TESTALU
            SEC ; Carry should be 1
            
            ; Validate A5 <<< 2 = 95
            ROL R1
            TESTALU
            CPI R1, $95
            TESTALU
            CLC ; Carry should be 0
            
            ; Validate A5 <<< 3 = 2A
            ROL R1
            TESTALU
            CPI R1, $2A
            TESTALU
            
; We will only now test memory operations...
; We will write some number of patterns into SRAM (internal for XY, external for Z)
; In particular we will write the pattern A55A00FF
            ; Set X = $60
            LDI R26, $60
            LDI R27, $00
            ; Set Y = $60
            LDI R28, $60
            LDI R29, $01
            ; Set Z = $60
            LDI R28, $60
            LDI R29, $02
            
            ; Set R0, R1, R2, R3, R4, R5, R6, R7 to our pattern
            LDI R0, $A5
            LDI R1, $5A
            LDI R2, $00
            LDI R3, $FF
            LDI R4, $A5
            LDI R5, $5A
            LDI R6, $00
            LDI R7, $FF
            
            ; Begin writing pattern to memory.
            ST X, R0 ;W A5 0060
            INC R26 ; Advance X to 0061
            ST X, R1 ;W 5A 0061
            INC R26 ; Advance X to 0062
            ST X, R2 ;W 00 0062
            INC R26 ; Advance X to 0063
            ST X, R3 ;W FF 0063
            INC R26 ; Advance X to 0064
            ST Y, R0 ;W A5 0160
            INC R28 ; Advance Y to 0161
            ST Y, R1 ;W 5A 0161
            INC R28 ; Advance Y to 0162
            ST Y, R2 ;W 00 0162
            INC R28 ; Advance Y to 0163
            ST Y, R3 ;W FF 0163
            INC R28 ; Advance Y to 0164
            ST Z, R0 ;W A5 0260
            INC R30 ; Advance Z to 0261
            ST Z, R1 ;W 5A 0261
            INC R30 ; Advance Z to 0262
            ST Z, R2 ;W 00 0262
            INC R30 ; Advance Z to 0263
            ST Z, R3 ;W FF 0263
            INC R30 ; Advance Z to 0264
            
            ; Set X = $60
            LDI R26, $60
            LDI R27, $00
            ; Set Y = $60
            LDI R28, $60
            LDI R29, $01
            ; Set Z = $60
            LDI R28, $60
            LDI R29, $02
            
            ; Read pattern back from memory.
            LD R0, X ;R A5 0060
            INC R26 ; Advance X to 0061
            LD R1, X ;R 5A 0061
            INC R26 ; Advance X to 0062
            LD R2, X ;R 00 0062
            INC R26 ; Advance X to 0063
            LD R3, X ;R FF 0063
            INC R26 ; Advance X to 0064
            LD R0, Y ;R A5 0160
            INC R28 ; Advance Y to 0161
            LD R1, Y ;R 5A 0161
            INC R28 ; Advance Y to 0162
            LD R2, Y ;R 00 0162
            INC R28 ; Advance Y to 0163
            LD R3, Y ;R FF 0163
            INC R28 ; Advance Y to 0164
            LD R0, Z ;R A5 0260
            INC R30 ; Advance Z to 0261
            LD R1, Z ;R 5A 0261
            INC R30 ; Advance Z to 0262
            LD R2, Z ;R 00 0262
            INC R30 ; Advance Z to 0263
            LD R3, Z ;R FF 0263
            INC R30 ; Advance Z to 0264
            
            
            ; Set X = $60
            LDI R26, $60
            LDI R27, $00
            ; Set Y = $60
            LDI R28, $60
            LDI R29, $01
            ; Set Z = $60
            LDI R28, $60
            LDI R29, $02
            
            ; Write inverse pattern to memory
            ST X+, R3 ;W FF 0060
            ST X+, R2 ;W 00 0061
            ST X+, R1 ;W 5A 0062
            ST X+, R0 ;W A5 0063
            ST Y+, R3 ;W FF 0160
            ST Y+, R2 ;W 00 0161
            ST Y+, R1 ;W 5A 0162
            ST Y+, R0 ;W A5 0163
            ST Z+, R3 ;W FF 0260
            ST Z+, R2 ;W 00 0261
            ST Z+, R1 ;W 5A 0262
            ST Z+, R0 ;W A5 0263
            
            ; Read inverse pattern back from memory
            LD R0, -X ;R A5 0063
            LD R1, -X ;R 5A 0062
            LD R2, -X ;R 00 0061
            LD R3, -X ;R FF 0060
            LD R0, -Y ;R A5 0163
            LD R1, -Y ;R 5A 0162
            LD R2, -Y ;R 00 0161
            LD R3, -Y ;R FF 0160
            LD R0, -Z ;R A5 0263
            LD R1, -Z ;R 5A 0262
            LD R2, -Z ;R 00 0261
            LD R3, -Z ;R FF 0260
            
            ; Check LDS
            LDS R3, $60 ;R FF 0060
            LDS R2, $61 ;R 00 0061
            LDS R1, $62 ;R 5A 0062
            LDS R0, $63 ;R A5 0063
            
            ; Check STS
            STS R0, $60 ;W A5 0060
            STS R1, $61 ;W 5A 0061
            STS R2, $62 ;W 00 0062
            STS R3, $63 ;W FF 0063
            
            ; Write pattern with displacement
            STD X+8, R0 ;W A5 0068
            INC R26 ; Advance X to 0061
            STD X+8, R1 ;W 5A 0069
            INC R26 ; Advance X to 0062
            STD X+8, R2 ;W 00 006A
            INC R26 ; Advance X to 0063
            STD X+8, R3 ;W FF 006B
            INC R26 ; Advance X to 0064
            STD Y+8, R0 ;W A5 0168
            INC R28 ; Advance Y to 0161
            STD Y+8, R1 ;W 5A 0169
            INC R28 ; Advance Y to 0162
            STD Y+8, R2 ;W 00 016A
            INC R28 ; Advance Y to 0163
            STD Y+8, R3 ;W FF 016B
            INC R28 ; Advance Y to 0164
            STD Z+8, R0 ;W A5 0268
            INC R30 ; Advance Z to 0261
            STD Z+8, R1 ;W 5A 0269
            INC R30 ; Advance Z to 0262
            STD Z+8, R2 ;W 00 026A
            INC R30 ; Advance Z to 0263
            STD Z+8, R3 ;W FF 026B
            INC R30 ; Advance Z to 0264
            
            ; Read pattern with displacement
            LDD R0, X+8 ;R A5 0068
            INC R26 ; Advance X to 0061
            LDD R1, X+8 ;R 5A 0069
            INC R26 ; Advance X to 0062
            LDD R2, X+8 ;R 00 006A
            INC R26 ; Advance X to 0063
            LDD R3, X+8 ;R FF 006B
            INC R26 ; Advance X to 0064
            LDD R0, Y+8 ;R A5 0168
            INC R28 ; Advance Y to 0161
            LDD R1, Y+8 ;R 5A 0169
            INC R28 ; Advance Y to 0162
            LDD R2, Y+8 ;R 00 016A
            INC R28 ; Advance Y to 0163
            LDD R3, Y+8 ;R FF 016B
            INC R28 ; Advance Y to 0164
            LDD R0, Z+8 ;R A5 0268
            INC R30 ; Advance Z to 0261
            LDD R1, Z+8 ;R 5A 0269
            INC R30 ; Advance Z to 0262
            LDD R2, Z+8 ;R 00 026A
            INC R30 ; Advance Z to 0263
            LDD R3, Z+8 ;R FF 026B
            INC R30 ; Advance Z to 0264