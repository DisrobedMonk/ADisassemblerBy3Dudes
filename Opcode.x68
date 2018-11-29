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
CMPIL_mask      DC.W    %0000110010000000
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
                
*-------------Checking 0000 Mask Table---------------*
           LEA     maskTable0000, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORIB_FOUND           * ORI.B Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORIW_FOUND           * ORI.W Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORIL_FOUND           * ORI.L Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     BCLR_FOUND           * BCLR Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     CMPIB_FOUND          * CMPI.B Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     CMPIW_FOUND          * CMPI.W Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     CMPIL_FOUND          * CMPI.L Found
           
*-------------Checking 0001 Mask Table---------------*
           LEA     maskTable0001, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     MOVEB_FOUND          * MOVE.B Found
           
*-------------Checking 0010 Mask Table---------------*
           LEA     maskTable0010, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     MOVEL_FOUND          * MOVE.L Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     MOVEAL_FOUND         * MOVEA.L Found

*-------------Checking 0011 Mask Table----------------*
           LEA     maskTable0011, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     MOVEW_FOUND          * MOVE.W Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     MOVEAW_FOUND         * MOVEA.W Found
            
*-------------Checking 0100 Mask Table----------------*
           LEA     maskTable0100, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     LEA_FOUND            * LEA Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     JSR_FOUND            * JSR Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     MOVEMW_FOUND         * MOVEM.W Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     MOVEML_FOUND         * MOVEM.L Found
           
*-------------Checking 0101 Mask Table----------------*
           LEA     maskTable0101, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     SUBQB_FOUND          * SUBQ.B Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     SUBQW_FOUND          * SUBQ.W Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     SUBQL_FOUND          * SUBQ.L Found

*-------------Checking 1000 Mask Table----------------*
           LEA     maskTable1000, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     DIVS_FOUND           * DIVS Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORB_EAReg_FOUND      * OR.B <ea>, Dr Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORW_EAReg_FOUND      * OR.W <ea>, Dr Found
           
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORL_EAReg_FOUND      * OR.L <ea>, Dr Found
           
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORB_RegEA_FOUND      * OR.B Dr,<ea> Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ORW_RegEA_FOUND      * OR.W Dr,<ea> Found

           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     ORL_RegEA_FOUND      * OR.L Dr,<ea> Found


*-------------Checking 1001 Mask Table----------------*
           LEA     maskTable1001, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     SUBB_RegEA_FOUND      * SUB.B Dr,<ea> Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     SUBW_RegEA_FOUND      * SUB.W Dr,<ea> Found  
         
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     SUBL_RegEA_FOUND      * SUB.L Dr,<ea> Found
           
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     SUBB_EAReg_FOUND      * SUB.B <ea>,Dr Found
           
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     SUBW_EAReg_FOUND      * SUB.W <ea>,Dr Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     SUBL_EAReg_FOUND      * SUB.L <ea>,Dr Found

*-------------Checking 1011 Mask Table----------------*
           LEA     maskTable1011, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     EORB_FOUND             * EOR.B Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     EORW_FOUND             * EOR.W Found  
         
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     EORL_FOUND             * EOR.L Found
           
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     CMPB_FOUND              * CMP.B Found
           
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     CMPW_FOUND              * CMP.W Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     CMPL_FOUND              * CMP.L Found

*-------------Checking 1100 Mask Table----------------*
           LEA     maskTable1100, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     MULS_FOUND              * MULS Found
           
*-------------Checking 1101 Mask Table----------------*
           LEA     maskTable1101, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ADDB_EAReg_FOUND        * ADD.B <ea>,Dr Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ADDW_EAReg_FOUND        * ADD.W <ea>,Dr Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ADDL_EAReg_FOUND        * ADD.L <ea>,Dr Found           
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ADDB_RegEA_FOUND        * ADD.B Dr,<ea> Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ADDW_RegEA_FOUND        * ADD.W Dr,<ea> Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ADDL_RegEA_FOUND        * ADD.L Dr,<ea> Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+,D0
           BEQ     ADDAW_FOUND             * ADDA.W Found           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     ADDAL_FOUND             * ADDA.L Found    
            
*-------------Checking 1110 Mask Table----------------*
           
           
           
           
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
           BEQ     BLT_FOUND
           CMP.L   #$0000C000,D1
           BEQ     BGE_FOUND
           CMP.L   #$00005000,D1
           BEQ     BCS_FOUND
           CMP.L   #$00007000,D1
           BEQ     BEQ_FOUND
           CMP.L   #$0000E000,D1
           BEQ     BGT_FOUND
           CMP.L   #$0000F000,D1
           BEQ     BLE_FOUND
           
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
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ORI.B CODE HERE
           BRA      END_OP_SUB

ORIW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ORI.W CODE HERE
           BRA      END_OP_SUB
ORIL_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ORI.L CODE HERE
           BRA      END_OP_SUB
BCLR_FOUND 
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT BCLR CODE HERE
           BRA      END_OP_SUB
CMPIB_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT CMPI.B CODE HERE
           BRA      END_OP_SUB

CMPIW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT CMPI.W CODE HERE
           BRA      END_OP_SUB
CMPIL_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT CMPI.L CODE HERE
           BRA      END_OP_SUB
MOVEB_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT MOVE.B CODE HERE
           BRA      END_OP_SUB
MOVEL_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT MOVE.L CODE HERE
           BRA      END_OP_SUB
MOVEAL_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT MOVEA.L CODE HERE
           BRA      END_OP_SUB
           
MOVEW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT MOVE.W CODE HERE
           BRA      END_OP_SUB
           
MOVEAW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT MOVEA.W CODE HERE
           BRA      END_OP_SUB

LEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT LEACODE HERE
           BRA      END_OP_SUB
           
JSR_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT JSR CODE HERE
           BRA      END_OP_SUB
           
MOVEMW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT MOVEM.W CODE HERE
           BRA      END_OP_SUB
           
MOVEML_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT MOVEM.L CODE HERE
           BRA      END_OP_SUB
           
SUBQB_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUBQ.B CODE HERE
           BRA      END_OP_SUB

SUBQW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUBQ.W CODE HERE
           BRA      END_OP_SUB
           
SUBQL_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUBQ.L CODE HERE
           BRA      END_OP_SUB

DIVS_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT DIVS CODE HERE
           BRA      END_OP_SUB
           
ORB_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT OR.B CODE HERE
           BRA      END_OP_SUB
           
ORW_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT OR.W CODE HERE
           BRA      END_OP_SUB
           
ORL_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT OR.L CODE HERE
           BRA      END_OP_SUB

ORB_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT OR.B CODE HERE
           BRA      END_OP_SUB
           
ORW_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT OR.W CODE HERE
           BRA      END_OP_SUB
           
ORL_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT OR.L CODE HERE
           BRA      END_OP_SUB
           
SUBB_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUB.B CODE HERE
           BRA      END_OP_SUB
           
SUBW_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUB.W CODE HERE
           BRA      END_OP_SUB
           
SUBL_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUB.L CODE HERE
           BRA      END_OP_SUB
           
SUBB_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUB.B CODE HERE
           BRA      END_OP_SUB
           
SUBW_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUB.W CODE HERE
           BRA      END_OP_SUB

SUBL_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT SUB.L CODE HERE
           BRA      END_OP_SUB

EORB_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT EOR.B CODE HERE
           BRA      END_OP_SUB
           
EORW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT EOR.W CODE HERE
           BRA      END_OP_SUB
           
EORL_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT EOR.L CODE HERE
           BRA      END_OP_SUB
           
CMPB_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT CMP.B CODE HERE
           BRA      END_OP_SUB
           
CMPW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT CMP.W CODE HERE
           BRA      END_OP_SUB
           
CMPL_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT CMP.L CODE HERE
           BRA      END_OP_SUB
           
MULS_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT MULS CODE HERE
           BRA      END_OP_SUB

ADDB_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ADD.B CODE HERE
           BRA      END_OP_SUB
           
ADDW_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ADD.W CODE HERE
           BRA      END_OP_SUB
           
ADDL_EAReg_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ADD.L CODE HERE
           BRA      END_OP_SUB
           
ADDB_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ADD.B CODE HERE
           BRA      END_OP_SUB
           
ADDW_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ADD.W CODE HERE
           BRA      END_OP_SUB
           
ADDL_RegEA_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ADD.L CODE HERE
           BRA      END_OP_SUB
           
ADDAW_FOUND
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ADDA.W CODE HERE
           BRA      END_OP_SUB
           
ADDAL_FOUND     
           MOVE.W   #END_LINE,-(SP)
           ;JSR TO EA LABLE HERE
           ;PRINT ADDA.L CODE HERE
           BRA      END_OP_SUB
           
           
DATA_FOUND       
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
