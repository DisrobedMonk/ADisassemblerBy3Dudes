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

*--------------0000 Mask Table: ORI.B, ORI.W, ORI.L, BCLR, CMPI.B, CMPI.W, CMPI.L-----------*
maskTable0000
ORIB_mask       DC.W    %0000000000000000
ORIW_mask       DC.W    %0000000001000000
ORIL_mask       DC.W    %0000000010000000
BCLR_mask       DC.W    %0000000110000000
CMPIB_mask      DC.W    %0000110000000000
CMPIW_mask      DC.W    %0000110001000000
CMPIW_mask      DC.W    %0000110010000000
*-------------------------------------------------------------------------------------------*
*--------------0001 Mask Table: MOVE.B------------------------------------------------------*
maskTable0001
MOVEB_mask      DC.W    %0001000000000000
*-------------------------------------------------------------------------------------------*
*--------------0010 Mask Table: MOVE.L, MOVEA.L---------------------------------------------*
maskTable0010
MOVEL_mask      DC.W    %0010000000000000
MOVEAL_mask     DC.W    %0010000001000000
*-------------------------------------------------------------------------------------------*
*--------------0011 Mask Table: MOVE.W, MOVEA.W---------------------------------------------*
maskTable0011
MOVEW_mask      DC.W    %0011000000000000
MOVEAW_mask     DC.W    %0011000001000000
*-------------------------------------------------------------------------------------------*
*--------------0100 Mask Table: LEA, JSR, MOVEM.W, MOVEM.L----------------------------------*
maskTable0100
LEA_mask        DC.W    %0100000111000000
JSR_mask        DC.W    %0100111010000000
MOVEMW_mask     DC.W    %0100100010000000
MOVEML_mask     DC.W    %0100100011000000
*-------------------------------------------------------------------------------------------*
*--------------0101 Mask Table: SUBQ.B SUBQ.W, SUBQ.L---------------------------------------*
maskTable0101
SUBQB_mask      DC.W    %0101000100000000
SUBQW_mask      DC.W    %0101000101000000
SUBQL_mask      DC.W    %0101000110000000
*-------------------------------------------------------------------------------------------*
*--------------1000 Mask Table: DIVS, OR (EA OR Reg) .B, .W, .L, OR (Reg OR EA) .B, .W, .L--*
maskTable1000
DIVS_mask       DC.W    %1000000111000000
ORB_EAReg_mask  DC.W    %1000000000000000
ORW_EAReg_mask  DC.W    %1000000001000000
ORL_EAReg_mas   DC.W    %1000000010000000
ORB_RegEA_mask  DC.W    %1000000100000000
ORW_RegEA_mask  DC.W    %1000000101000000
ORL_RegEA_mask  DC.W    %1000000110000000
*-------------------------------------------------------------------------------------------*
*--------------1001 Mask Table: SUB (Reg -> EA) .B, .W, .L, SUB (EA -> REG) .B, .W, .L------*
maskTable1001
SUBB_RegEA_mask DC.W    %1001000000000000
SUBW_RegEA_mask DC.W    %1001000001000000
SUBL_RegEA_mask DC.W    %1001000010000000
SUBB_EAReg_mask DC.W    %1001000100000000
SUBW_EAReg_mask DC.W    %1001000101000000
SUBL_EAReg_mask DC.W    %1001000110000000
*-------------------------------------------------------------------------------------------*
*--------------1011 Mask Table: EOR.B, EOR.W, EOR.L, CMP.B, CMP.W, CMP.L--------------------*
maskTable1011
EORB_mask       DC.W    %1011000100000000
EORW_mask       DC.W    %1011000101000000
EORL_mask       DC.W    %1011000110000000
CMPB_mask       DC.W    %1011000000000000
CMPW_mask       DC.W    %1011000001000000
CMPL_mask       DC.W    %1011000010000000
*-------------------------------------------------------------------------------------------*
*--------------1100 Mask Table: MULS--------------------------------------------------------*
maskTable1100
MULS_mask       DC.W    %1100000111000000
*-------------------------------------------------------------------------------------------*
*--------------1101 Mask Table: ADD (EA + Reg) .B, .W, .L, ADD (Reg + EA) .B, .W, .L, ADDA--*
maskTable1101
ADDB_EAReg_mask DC.W    %1101000000000000
ADDW_EAReg_mask DC.W    %1101000001000000
ADDL_EAReg_mask DC.W    %1101000010000000
ADDB_RegEA_mask DC.W    %1101000100000000
ADDW_RegEA_mask DC.W    %1101000101000000
ADDL_RegEA_mask DC.W    %1101000110000000
ADDAW_mask      DC.W    %1101000011000000
ADDAL_mask      DC.W    %1101000111000000
*---------------------------------------------------------------------------------------*
*--------------1110 Mask Table: LSR Reg, LSR Mem, LSL Reg, LSL Mem----------------------*
*--------------1110 Mask Table: ASR Reg, ASR Mem, ASL Reg, ASR Mem----------------------*
*--------------1110 Mask Table: ROL Reg, ROL Mem, ROR Reg, ROR Mem----------------------*



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
                
           LEA     maskTable000, A4
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORIB_FOUND
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORIW_FOUND

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
   
ORIB_FOUND

ORIW_FOUND

ORIL_FOUND
           
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






*~Font name~Fixedsys~
*~Font size~9~
*~Tab type~1~
*~Tab size~4~
