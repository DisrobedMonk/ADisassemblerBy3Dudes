*-----------------------------------------------------------
* Title      :Diassembler OpCode File
* Written by :Torren,Doug,Jeremiah
* Date       :11/25/2018
* Description:
*-----------------------------------------------------------
END_LINE   EQU    $0A00  
F4_W_MASK  EQU    $0000F000
S4_W_MASK  EQU    $00000F00
 
START      ORG    $1500
*-------------Op Code Check Subroutine-------------
OP_CHECK
           MOVE.L (SP)+,D5

*-------------No Operand Section-------------
           CMP.L   #$00004E71,D6        ;Check if opcode is NOP
           BEQ     NOP_FOUND
   
           CMP.L   #$00004E75,D6        ;Check if opcode is RTS
           BEQ     RTS_FOUND
           
           MOVE.L  #F4_W_MASK,D1        ;Move first 4 mask bits of 16 bit word opcode
           AND.L   D6,D1                ;Mask first 4 bits of opcode to start identfieing the opcode
           CMP.L   #$00006000,D1        ;Compare to see if it is a branching instruction
           BEQ     BCC_FOUND
            
           BRA     DATA_FOUND



RTS_FOUND  
           MOVE.W  #END_LINE,-(SP)
           MOVE.W  #$530D,-(SP)
           MOVE.W  #$5452,-(SP)
           BRA     END_OP_SUB

NOP_FOUND
           MOVE.W  #END_LINE,-(SP)
           MOVE.W  #$500D,-(SP)
           MOVE.W  #$4E4F,-(SP)
           BRA     END_OP_SUB

BCC_FOUND
           MOVE.B  D6,D0            ;Move Lower order byte for branch offset into D0
           MOVE.L  A5,A0
           ADDA.L  D0,A0            ;A0 stores the offset address
           JSR     OFFSET_ADDR      ;Offset for address to branch back to is in the lower order byte
                                    ;For a branching instruction, two parts, determine address to branch back too
                                    ;and determine what kind of branching 
                                    ;TODO is calculation of the offset
           MOVE.W  #S4_W_MASK,D1    ;Need to determine which branch it is. Mask bits 4-7
           AND.L   D6,D1            ;of the lower word to determine
           CMP.L   #$00000000,D1
           BEQ     BRA_FOUND
           CMP.L   #$00008000,D1
           BEQ     BVC_FOUND
           CMP.L   #$0000D000,D1
           BEQ     BLT_FOUND,D1
           CMP.L   #$0000C000,D1
           BEQ     BGE_FOUND
           CMP.L   #$00005000,D1
           BEQ     BCS_FOUND
           CMP.L   #$00007000,D1
           BEQ     BEQ_FOUND
           CMP.L   #$0000E000,D1
           BEQ     BGT_FOUND
           CMP.L   #$0000F000,D1
           BEQ     BLE_FOUND,D1

BRA_FOUND
           MOVE.W   #END_LINE,-(SP)
           MOVE.W   #$410D, -(SP)
           MOVE.W   #$4252,-(SP)
           BRA      END_OP_SUB

BVC_FOUND  
           MOVE.W   #END_LINE,-(SP)
           MOVE.W   #$430D,-(SP)
           MOVE.W   #$4256,-(SP)
           BRA      END_OP_SUB

BLT_FOUND  
           MOVE.W   #END_LINE,-(SP)
           MOVE.W   #$540D,-(SP)
           MOVE.W   #$424C,-(SP)
           BRA      END_OP_SUB
           
BGE_FOUND  
           MOVE.W   #END_LINE,-(SP)
           MOVE.W   #$450D,-(SP)
           MOVE.W   #4247,-(SP)
           BRA      END_OP_SUB
           
BCS_FOUND  
           MOVE.W   #END_LINE,-(SP)           
           MOVE.W   #$530D,-(SP)
           MOVE.W   #$4243,-(SP)
           BRA      END_OP_SUB
           
BEQ_FOUND  
           MOVE.W   #END_LINE,-(SP)
           MOVE.W   #$510D,-(SP)
           MOVE.W   #$4245,-(SP)
           BRA      END_OP_SUB

BGT_FOUND
           MOVE.W   #END_LINE,-(SP)
           MOVE.W   #$540D,-(SP)
           MOVE.W   #$4247,-(SP)
           BRA      END_OP_SUB
            
BLE_FOUND  
           MOVE.W   #END_LINE,-(SP)
           MOVE.W   #$450D,-(SP)
           MOVE.W   #$4C42,-(SP)
           BRA      END_OP_SUB
           
DATA       
           MOVE.W  #END_LINE,-(SP)          
           MOVE.B  #$0D,-(SP)               ;CR
           MOVE.L  #$5758595A,-(SP)         ;WXYZ
           MOVE.W  #$0924,-(SP)             ;\t$
           MOVE.L  #$44415441,-(SP)         ;DATA
           BRA     END_OP_SUB



END_OP_SUB
           MOVE.L D5,-(SP)
           RTS    
        
           END     START
           
           
OFFSET_ADDR


*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
