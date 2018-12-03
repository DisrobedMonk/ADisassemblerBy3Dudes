*-----------------------------------------------------------
* Title      : Complete Disassembler File
* Written by : Torren,Doug,Jeremiah
* Date       : 11/23/2018
* Description: This is a disassembler which works through a user specified memory section taking a word of data at a time
*               in order to determine and print the OP code along with the necessary register/data information if applicable
*-----------------------------------------------------------
START           EQU     $1000
CR              EQU     $0D         * ASCII code for Carriage Return
LF              EQU     $0A         * ASCII code for Line Feed
STACK           EQU     $10000      * Defined location for the stack
LINE_CNT        EQU     $F          * Number of lines to print before pausing
EXIT            EQU     $2700

END_LINE        EQU     $200D0A00  
F4_W_MASK       EQU     $0000F000
S4_W_MASK       EQU     $00000F00



*   D0:  
*   D1:  
*   D2:  
*   D3:  
*   D4: 
*   !D5: OP_CHECK to I/O RTS holder
*   !D6: Current OP code read from latest Current Address position
*   !D7: The current line being printed (I/O line counter)

*   A0:  
*   A1:  
*   A2:  
*   A3:  
*   A4:  
*   !A5: Current Address*   DO NOT ALTER (Initialized via start up requests
*           - except for when in EA to get more information
*   !A6: End Address*       DO NOT ALTER (Initialized via start up requests)
*   !A7: STACK POINTER*     DO NOT ALTER
*           - except in adding or taking from stack






        ORG    START
        

*-----------------I/O code----------------

*---Positioning the stack---
        LEA     STACK,SP        * Position the stack
        
*-------Requesting the Starting Address---
STRT_ADDR_RQST
        LEA     STRT_RQST,A1    * Load/print start address request text
        MOVE.L  #14,D0
        TRAP    #15
        
        CLR.L   D1              * Clear any previously used Dr from looped requests
        CLR.L   D2
        CLR.L   D3
        CLR.L   D4
        
        MOVE.L  #2,D0           * Take in their start address
        MOVE.L  #0,A1           * Cleared A1 to not print over start address request in memory
        TRAP    #15
        
        CMP     #8,D1           * Make sure not longer than 8
        BGT     STRT_ADDR_RQST
     
*---Converting input to hex address------           
CNVRT_STRT
        CMP.L   D2,D1           * Check if all entered ASCII converted
        BEQ     SAVE_STRT       * Move on to save the start address into Current Address (A5)
            
        LSL.L   #4,D4           * Move D4 left to make room for next entry
        MOVE.B  (A1)+,D3        * Put first char into D3 for conversion
            
            
        CMP.B   #$39,D3         * Check if char is '9' or less in ASCII
        BLE     ITS_A_NUM_STRT  * Branch to number intake method
                        
        CMP.B   #$41,D3         * Check if char is 'A' or higher in ASCII
        BGE     ITS_A_CHR_STRT  * Branch to char intake mehtod

        BRA     STRT_ADDR_RQST  * Not in bounds of valid data, request again

ITS_A_NUM_STRT
        CMP.B   #$30,D3         * Check if char is '0' or higher in ASCII
        BLT     STRT_ADDR_RQST  * Not in bounds of valid data, request again
        
        SUB.L   #$30,D3         * Convert from ASCII number to hex number
        
        ADD.L   D3,D4           * Add newest converted char to entire hex
        
        ADD.B   #1,D2           * Increment counter
        
        BRA     CNVRT_STRT      * Loop back to convert next char
        
ITS_A_CHR_STRT
        CMP.B   #$46,D3         * Check if char is 'F' or less in ASCII
        BGT     STRT_ADDR_RQST  * Not in bounds of valid data, request again
        
        SUB.L   #$37,D3         * Convert from ASCII letter to hex 'letter'
        
        ADD.L   D3,D4           * Add newest converted char to entire hex
        
        ADD.B   #1,D2           * Increment counter
        
        BRA     CNVRT_STRT      * Loop back to convert next char
        
        
SAVE_STRT
        MOVE.L  D4,D3           * Double up the converted address for mutation
        AND.L   #$1,D3          * Bit-mask with 1 to check for odd/even
        CMP     #1,D3
        BEQ     STRT_ADDR_RQST  * Odd value not allowed, request again
        
        MOVE.L  D4,A5           * Save the given starting address in Current Address position (A5)
   
   
   
*-------Requesting the End Address-------
END_ADDR_RQST
        LEA     END_RQST,A1     * Load/print end address request text
        MOVE.L  #14,D0
        TRAP    #15
        
        CLR.L   D1              * Clear any previously used Dr from looped requests
        CLR.L   D2
        CLR.L   D3
        CLR.L   D4
        
        MOVE.L  #2,D0           * Take in their end address
        MOVE.L  #0,A1           * Cleared A1 to not print over start address request in memory
        TRAP    #15
        
        CMP     #8,D1           * Make sure not longer than 8
        BGT     END_ADDR_RQST   
        
        
*---Converting input to hex address------
CNVRT_END
        CMP.L   D2,D1           * Check if all entered ASCII converted
        BEQ     SAVE_END        * Move on to saving end address if done
            
        LSL.L   #4,D4           * Move D4 left to make room for next entry
        MOVE.B  (A1)+,D3        * Put first char into D3 for conversion
            
            
        CMP.B   #$39,D3         * Check if char is '9' or less in ASCII
        BLE     ITS_A_NUM_END   * Branch to number intake method
                        
        CMP.B   #$41,D3         * Check if char is 'A' or higher in ASCII
        BGE     ITS_A_CHR_END   * Branch to char intake mehtod

        BRA     END_ADDR_RQST

ITS_A_NUM_END
        CMP.B   #$30,D3         * Check if char is '0' or higher in ASCII
        BLT     END_ADDR_RQST   * Not in bounds of valid data, request again
        
        SUB.L   #$30,D3         * Convert from ASCII number to hex number
        
        ADD.L   D3,D4           * Add newest converted char to entire hex
        
        ADD.B   #1,D2           * Increment counter
        
        BRA     CNVRT_END       * Loop back to convert next char
        
ITS_A_CHR_END
        CMP.B   #$46,D3         * Check if char is 'F' or less in ASCII
        BGT     END_ADDR_RQST   * Not in bounds of valid data, request again
        
        SUB.L   #$37,D3         * Convert from ASCII letter to hex 'letter'
        
        ADD.L   D3,D4           * Add newest converted char to entire hex
        
        ADD.B   #1,D2           * Increment counter
        
        BRA     CNVRT_END       * Loop back to convert next char
        
        
SAVE_END
        MOVE.L  D4,A6           * Save the given/converted address in End Address (A6)

        CLR.L   D0              * Clear unnecessary data from Dr list
        CLR.L   D1
        CLR.L   D2
        CLR.L   D3
        CLR.L   D4
        CLR.L   D5
        CLR.L   D6
        CLR.L   D7
        
        
*-------Moving on to decoding* start with instructions-------        
BEGIN
        LEA     INSTRCTNS,A1    * Load instruction to inform user how disassembler will work
        MOVE.B  #14,D0
        TRAP    #15
        
        MOVE.L  #0,A1           * Clear A1 to ensure instructions not overwritten

*-------Pause and wait for user to continue------------------
WAIT
        MOVE.L  #5,D0           * Pause for 'enter' by user to move forward
        TRAP    #15             *   A char is actually taken in for D1
        
        CLR.L   D7              * Zero the line counter

*---Check if done or perform next line's disassembly---------
SEE_IF_DONE
        CMP     A5,A6           * Check if Current Address is past End Address
        BLT     DONE            * Branch to DONE if completed
        
        CMP.L   #15,D7          * Check if we have printed 15 lines
        BEQ     WAIT            * Await 'Enter' before continuing if so
        
        CLR.L   D0              * Clear unnecessary data from register list except for:
        CLR.L   D1              * (D7) line counter, (A5) current address, (A6) end address,
        CLR.L   D2              * and (A7/SP) stack pointer
        CLR.L   D3
        CLR.L   D4
        CLR.L   D5
        CLR.L   D6
        MOVE.L  #0,A0
        MOVE.L  #0,A1
        MOVE.L  #0,A2
        MOVE.L  #0,A3
        MOVE.L  #0,A4
        
        JSR     PRINT_CURRENT_ADDRESS

        
        MOVE.W  (A5)+,D6        * Take the next word of instruction into Current OPcode and increment Current address
        
        JSR     OP_CHECK        * Jump to the OP_CHECK code
                                * Sending:      A0-A4:  Empty
                                *               A5:     The Current Address (just after Current OPcode)
                                *                           Alter this only if more information deemed necessary via OPcode/EAcode
                                *               !A6:    The End Address (DO NOT TOUCH)
                                *               A7:     The current stack position
                                *                           Alter this in order to maintain RTS position and to prepare print statement for OPcode's line
                                *               D0-D5:  Empty
                                *               !D6:    The current OPcode being worked (DO NOT TOUCH)
                                *               !D7:    The current count of lines printed (DO NOT TOUCH)
                                *
                                * Need back:    A0-A4:  Garbage - anything is fine as these will be cleared
                                *               A5:     The Current Address from which the next OPcode will be read
                                *               !A6:    The End Address
                                *               A7:     The current stack position
                                *                           Should have the start of the OPcode's string to be printed and end with "'CR','LF',0"
                                *               D0-D5:  Garbage - anything is fine as these will be cleared
                                *               D6:     Unneeded / was the just completed OPcode
                                *               !D7:    The current count of lines printed
        
        
        MOVE.L  #14,D0          * Prep for printing the just disassembled OPcode
        
        MOVE.L  SP,A1           * Prep A1 as the top of the stack to print
        TRAP    #15
        
        LEA     STACK,SP        * Reposition stack back to its original position

        
        ADD.L   #1,D7           * Increment counter
        
        BRA     SEE_IF_DONE
        


*---------Print a hex address as a string-----
PRINT_CURRENT_ADDRESS
        MOVE.L  A5,D4
        LEA     HEX_to_ASCII,A4

PRNT_LOOP
        CMP     #4,D2           * Check if the 4 bytes have been printed
        BEQ     PRNT_DONE
        
        ROL.L   #8,D4           * Position the first/next byte
        
        MOVE.B  D4,D3           * Take in the byte
        LSR.L   #4,D3           * Only use first nibble

        MOVE.B  (0,A4,D3.W),D1  * Find the correct ASCII char

        MOVE.B  #6,D0           * Print the char
        TRAP    #15A

        MOVE.B  D4,D3           * Take in the byte again
        AND.B   #%00001111,D3   * Mask for second nibble

        MOVE.B  (0,A4,D3.W),D1  * Find the correct ASCII char
        TRAP    #15             * and print
        
        ADDI.B  #1,D2           * Increment counter
        BRA     PRNT_LOOP
        
PRNT_DONE
        CLR.L   D0              * Clear the Drs and Ar that were used
        CLR.L   D1
        CLR.L   D3
        CLR.L   D4
        CLR.L   D5
        MOVE.L  #0,A4

        RTS                     


*-------------------------End of I/O code--------------------





*-------------------------OP code section---------------------


*----Mask tables separated by leading bit differntiation------

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
*--------------1101 Mask Table: ADD (EA + Reg) .B, .W, .L, ADD (Reg + EA) .B, .W, .L, ADDA.W,.L-*
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
*--------------1110 Mask Table: ASR Reg, ASR Mem, ASL Reg, ASR Mem, (.B,W, .L)----------*
*--------------1110 Mask Table: LSR Reg, LSR Mem, LSL Reg, LSL Mem  (.B,W, .L)----------*
*--------------1110 Mask Table: ROR Reg, ROR Mem, ROL Reg, ROL Mem  (.B,W, .L)----------*
maskTable1110
ASRB_Reg_mask   DC.W    %1110000000000000
ASRW_Reg_mask   DC.W    %1110000001000000
ASRL_Reg_mask   DC.W    %1110000010000000
ASR_Mem_mask    DC.W    %1110000011000000
ASLB_Reg_mask   DC.W    %1110000100000000
ASLW_Reg_mask   DC.W    %1110000101000000
ASLL_Reg_mask   DC.W    %1110000110000000
ASL_Mem_mask    DC.W    %1110000111000000
LSRB_Reg_mask   DC.W    %1110000000001000
LSRW_Reg_mask   DC.W    %1110000001001000
LSRL_Reg_mask   DC.W    %1110000010001000
LSR_Mem_mask    DC.W    %1110001011000000
LSLB_Reg_mask   DC.W    %1110000100001000
LSLW_Reg_mask   DC.W    %1110000101001000
LSLL_Reg_mask   DC.W    %1110000110001000
LSL_Mem_mask    DC.W    %1110001111000000
RORB_Reg_mask   DC.W    %1110000000011000
RORW_Reg_mask   DC.W    %1110000001011000
RORL_Reg_mask   DC.W    %1110000010011000
ROR_Mem_mask    DC.W    %1110011011000000
ROLB_Reg_mask   DC.W    %1110000100011000
ROLW_Reg_mask   DC.W    %1110000101011000
ROLL_Reg_mask   DC.W    %1110000110011000
ROL_Mem_mask    DC.W    %1110011111000000


*-------------Op Code Check Subroutine (utilizes mask tables to determine current OP word's function)-------------
OP_CHECK
           MOVE.L (SP)+,D5              * Save the RTS value for putting back into place after EA text has been added to SP

*-------------No Operand Section-------------
           CMP.L   #$00004E71,D6        *Check if opcode is NOP
           BEQ     NOP_FOUND
   
           CMP.L   #$00004E75,D6        *Check if opcode is RTS
           BEQ     RTS_FOUND
           
           MOVE.L  #F4_W_MASK,D1        *Move first 4 mask bits of 16 bit word opcode
           AND.L   D6,D1                *Mask first 4 bits of opcode to start identfieing the opcode
           CMP.L   #$00006000,D1        *Compare to see if it is a branching instruction
           BEQ     BCC_FOUND



*-------------Mask table check logic-----------------
*       Load the table to be checked into A4
*       Move the first/next check into D0
*       Mask the current OP word into the mask we're looking for
*       Check if current mask being worked on is still the same as it was originally
*           and move on to next mask for checking
*       If found (equal), branch to the header for the specific OP code found
*           to perform specific EA operations and add the ASCII OP word to SP
*-----------------------------------------------------

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
           
           LEA     maskTable1110, A4     
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ASRB_Reg_FOUND          * ASR.B Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ASRW_Reg_FOUND          * ASR.W Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ASRL_Reg_FOUND          * ASR.L Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ASR_Mem_FOUND           * ASR Memory Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ASLB_Reg_FOUND          * ASL.B Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ASLW_Reg_FOUND          * ASL.W Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ASLL_Reg_FOUND          * ASL.L Register Found
           
            MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ASL_Mem_FOUND           * ASL Memory Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     LSRB_Reg_FOUND          * LSR.B Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     LSRW_Reg_FOUND          * LSR.W Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     LSRL_Reg_FOUND          * LSR.L Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     LSR_Mem_FOUND           * LSR Memory Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     LSLB_Reg_FOUND          * LSL.B Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     LSLW_Reg_FOUND          * LSL.W Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     LSLL_Reg_FOUND          * LSL.L Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     LSL_Mem_FOUND           * LSL Memory Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     RORB_Reg_FOUND          * ROR.B Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     RORW_Reg_FOUND          * ROR.W Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     RORL_Reg_FOUND          * ROR.L Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ROR_Mem_FOUND           * ROR Memory Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ROLB_Reg_FOUND          * ROL.B Register Found
           
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ROLW_Reg_FOUND          * ROL.W Register Found
                      
           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4)+, D0
           BEQ     ROLL_Reg_FOUND          * ROL.L Register Found

           MOVE.W  (A4),D0
           AND.W   D6,D0
           CMP.W   (A4),D0
           BEQ     ROL_Mem_FOUND           * ROL Memory Found           
           
           
           
*----------No known OP code found; take in as "data"------------------------------

           BRA     DATA_FOUND              * Did not discover the OP word we were given; must be data that needs printing



*-----Specific OP code found section (perform specific EA and add the ASCII to SP)-------

RTS_FOUND  
           MOVE.L  #END_LINE,-(SP)
           MOVE.L  #$52545309,-(SP)
           MOVE.W  #$2009,-(SP)
           BRA     END_OP_SUB

NOP_FOUND
           MOVE.L  #END_LINE,-(SP)
           MOVE.L  #$4E4F5009,-(SP)
           MOVE.W  #$2009,-(SP)
           BRA     END_OP_SUB

BCC_FOUND
           MOVE.B  D6,D0            *Move Lower order byte for branch offset into D0
           MOVE.L  A5,A0
           SUB.L   #2,A0
           ADDA.L  D0,A0            *A0 stores the offset address
           JSR     OFFSET_ADDR      *Offset for address to branch back to is in the lower order byte
                                    *For a branching instruction, two parts, determine address to branch back too
                                    *and determine what kind of branching 
                                    *TODO is calculation of the offset -        MUST PUT END LINE IN FIRST                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           MOVE.W  #S4_W_MASK,D1    *Need to determine which branch it is. Mask bits 4-7
           AND.L   D6,D1            *of the lower word to determine
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
           MOVE.L   #$42524109, -(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

BVC_FOUND  
           MOVE.L   #$42564309,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

BLT_FOUND  
           MOVE.L   #$424C5409,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
           
BGE_FOUND  
           MOVE.L   #$42474509,-(SP)
           MOVE.W   #2009,-(SP)
           BRA      END_OP_SUB
           
BCS_FOUND  
           MOVE.L   #$42435309,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
           
BEQ_FOUND  
           MOVE.L   #$42455109,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

BGT_FOUND
           MOVE.L   #$42475409,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
            
BLE_FOUND  
           MOVE.L   #$424C4509,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
   
ORIB_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$492E4209,-(SP)
           MOVE.L   #$20094F52,-(SP)
           BRA      END_OP_SUB

ORIW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$492E5709,-(SP)
           MOVE.L   #$20094F52,-(SP)
           BRA      END_OP_SUB
ORIL_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$492E4C09,-(SP)
           MOVE.L   #$20094F52,-(SP)
           BRA      END_OP_SUB
BCLR_FOUND 
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C435209,-(SP)
           MOVE.W   #$0942,-(SP)
           BRA      END_OP_SUB
CMPIB_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$492E4209,-(SP)
           MOVE.L   #$09434D50,-(SP)
           BRA      END_OP_SUB

CMPIW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$492E5709,-(SP)
           MOVE.L   #$09434D50,-(SP)
           BRA      END_OP_SUB
CMPIL_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$492E4C09,-(SP)
           MOVE.L   #$09434D50,-(SP)
           BRA      END_OP_SUB
MOVEB_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$452E4209,-(SP)
           MOVE.L   #$094D4F56,-(SP)
           BRA      END_OP_SUB
MOVEL_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$452E4C09,-(SP)
           MOVE.L   #$094D4F56,-(SP)
           BRA      END_OP_SUB
MOVEAL_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$412E4C09,-(SP)
           MOVE.L   #$4D4F5645,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
           
MOVEW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$452E5709,-(SP)
           MOVE.L   #$094D4F56,-(SP)
           BRA      END_OP_SUB
           
MOVEAW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$412E5709,-(SP)
           MOVE.L   #$4D4F5645,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

LEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C454109,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
           
JSR_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4A535209,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
           
MOVEMW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4D2E5709,-(SP)
           MOVE.L   #$4D4F5645,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
           
MOVEML_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4D2E4C09,-(SP)
           MOVE.L   #$4D4F5645,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
           
SUBQB_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$512E4209,-(SP)
           MOVE.L   #$09535542,-(SP)
           BRA      END_OP_SUB

SUBQW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$512E5709,-(SP)
           MOVE.L   #$09535542,-(SP)
           BRA      END_OP_SUB
           
SUBQL_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$512E4C09,-(SP)
           MOVE.L   #$09535542,-(SP)
           BRA      END_OP_SUB

DIVS_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$49565309,-(SP)
           MOVE.W   #$0944,-(SP)
           BRA      END_OP_SUB
           
ORB_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4209,-(SP)
           MOVE.W   #$094F,-(SP)
           BRA      END_OP_SUB
           
ORW_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E5709,-(SP)
           MOVE.W   #$094F,-(SP)
           BRA      END_OP_SUB
           
ORL_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4C09,-(SP)
           MOVE.W   #$094F,-(SP)
           BRA      END_OP_SUB

ORB_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4209,-(SP)
           MOVE.W   #$094F,-(SP)
           BRA      END_OP_SUB
           
ORW_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E5709,-(SP)
           MOVE.W   #$094F,-(SP)
           BRA      END_OP_SUB
           
ORL_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4C09,-(SP)
           MOVE.W   #$094F,-(SP)
           BRA      END_OP_SUB
           
SUBB_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$422E4209,-(SP)
           MOVE.L   #$20095355,-(SP)
           BRA      END_OP_SUB
           
SUBW_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$422E5709,-(SP)
           MOVE.L   #$20095355,-(SP)
           BRA      END_OP_SUB
           
SUBL_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$422E4C09,-(SP)
           MOVE.L   #$20095355,-(SP)
           BRA      END_OP_SUB
           
SUBB_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$422E4209,-(SP)
           MOVE.L   #$20095355,-(SP)
           BRA      END_OP_SUB
           
SUBW_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$422E5709,-(SP)
           MOVE.L   #$20095355,-(SP)
           BRA      END_OP_SUB

SUBL_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$422E4C09,-(SP)
           MOVE.L   #$20095355,-(SP)
           BRA      END_OP_SUB

EORB_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4209,-(SP)
           MOVE.L   #$2009454F,-(SP)
           BRA      END_OP_SUB
           
EORW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E5709,-(SP)
           MOVE.L   #$2009454F,-(SP)
           BRA      END_OP_SUB
           
EORL_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4C09,-(SP)
           MOVE.L   #$2009454F,-(SP)
           BRA      END_OP_SUB
           
CMPB_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$502E4209,-(SP)
           MOVE.L   #$2009434D,-(SP)
           BRA      END_OP_SUB
           
CMPW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$502E5709,-(SP)
           MOVE.L   #$2009434D,-(SP)
           BRA      END_OP_SUB
           
CMPL_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$502E4C09,-(SP)
           MOVE.L   #$2009434D,-(SP)
           BRA      END_OP_SUB
           
MULS_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$554C5309,-(SP)
           MOVE.W   #$094D,-(SP)
           BRA      END_OP_SUB

ADDB_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$442E4209,-(SP)
           MOVE.L   #$20094144,-(SP)
           BRA      END_OP_SUB
           
ADDW_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$442E5709,-(SP)
           MOVE.L   #$20094144,-(SP)
           BRA      END_OP_SUB
           
ADDL_EAReg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$442E4C09,-(SP)
           MOVE.L   #$20094144,-(SP)
           BRA      END_OP_SUB
           
ADDB_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$442E4209,-(SP)
           MOVE.L   #$20094144,-(SP)
           BRA      END_OP_SUB
           
ADDW_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$442E5709,-(SP)
           MOVE.L   #$20094144,-(SP)
           BRA      END_OP_SUB
           
ADDL_RegEA_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$442E4C09,-(SP)
           MOVE.L   #$20094144,-(SP)
           BRA      END_OP_SUB
           
ADDAW_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$412E5709,-(SP)
           MOVE.L   #$09414444,-(SP)
           BRA      END_OP_SUB
           
ADDAL_FOUND     
           *JSR TO EA LABLE HERE
           MOVE.L   #$412E4C09,-(SP)
           MOVE.L   #$09414444,-(SP)
           BRA      END_OP_SUB
           
ASRB_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4209,-(SP)
           MOVE.L   #$20094153,-(SP)
           BRA      END_OP_SUB

ASRW_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E5709,-(SP)
           MOVE.L   #$20094153,-(SP)
           BRA      END_OP_SUB

ASRL_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4C09,-(SP)
           MOVE.L   #$20094153,-(SP)
           BRA      END_OP_SUB

ASR_Mem_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$41535209,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

ASLB_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E4209,-(SP)
           MOVE.L   #$20094153,-(SP)
           BRA      END_OP_SUB

ASLW_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E5709,-(SP)
           MOVE.L   #$20094153,-(SP)
           BRA      END_OP_SUB

ASLL_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E5C09,-(SP)
           MOVE.L   #$20094153,-(SP)
           BRA      END_OP_SUB

ASL_Mem_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$41534C09,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

LSRB_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4209,-(SP)
           MOVE.L   #$20094C53,-(SP)
           BRA      END_OP_SUB

LSRW_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E5709,-(SP)
           MOVE.L   #$20094C53,-(SP)
           BRA      END_OP_SUB

LSRL_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4C09,-(SP)
           MOVE.L   #$20094C53,-(SP)
           BRA      END_OP_SUB

LSR_Mem_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C535209,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

LSLB_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E4209,-(SP)
           MOVE.L   #$20094C53,-(SP)
           BRA      END_OP_SUB

LSLW_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E5709,-(SP)
           MOVE.L   #$20094C53,-(SP)
           BRA      END_OP_SUB

LSLL_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E4C09,-(SP)
           MOVE.L   #$20094C53,-(SP)
           BRA      END_OP_SUB

LSL_Mem_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C534C09,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

RORB_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4209,-(SP)
           MOVE.L   #$2009524F,-(SP)
           BRA      END_OP_SUB

RORW_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E5709,-(SP)
           MOVE.L   #$2009524F,-(SP)
           BRA      END_OP_SUB

RORL_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$522E4C09,-(SP)
           MOVE.L   #$2009524F,-(SP)
           BRA      END_OP_SUB

ROR_Mem_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$524F5209,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB

ROLB_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E4209,-(SP)
           MOVE.L   #$2009524F,-(SP)
           BRA      END_OP_SUB

ROLW_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E5709,-(SP)
           MOVE.L   #$2009524F,-(SP)
           BRA      END_OP_SUB

ROLL_Reg_FOUND
           *JSR TO EA LABLE HERE
           MOVE.L   #$4C2E4C09,-(SP)
           MOVE.L   #$2009524F,-(SP)
           BRA      END_OP_SUB

ROL_Mem_FOUND           
           *JSR TO EA LABLE HERE
           MOVE.L   #$524F4C09,-(SP)
           MOVE.W   #$2009,-(SP)
           BRA      END_OP_SUB
           
DATA_FOUND       
           MOVE.L  #END_LINE,-(SP)          
           SUB.L   #$2,A5
           JSR     ADDW_OUT
           MOVE.W  #$0924,-(SP)             * tab and $ for formatting 
           MOVE.L  #$44415441,-(SP)         * DATA
           MOVE.W  #$2009,-(SP)

           BRA     END_OP_SUB



END_OP_SUB
           MOVE.L D5,-(SP)
           RTS

*-----------End of OP code section-------------------------------------







* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
* Placeholder position for where the EA code will go
    
    * SELF EXPLANATORY ADDITIONAL TESTING AREA
    * 
    * Replace the name and test your I/O -> OP_CHECK -> EA code here


* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *








*---------Branch to when Current Address past End Address-----
DONE
    STOP    #EXIT               * Done, thus exit













* Variables and defined constraints

STRT_RQST       DC.B    'Please provide a valid starting hex address for the code to be disassembled.',CR,LF
                DC.B    'Invalid entry will only lead to the question being repeated.',CR,LF
                DC.B    'Only numbers 0-9 and capitals A-F are accepted.',CR,LF
                DC.B    'Starting address must also be even.',CR,LF
                                                                          * There's a chance we also want to iron out and check to ensure it's not our own code?
                DC.B    'What will the starting address be?',CR,LF,0 
            
END_RQST        DC.B    'Thank you for the starting address.',CR,LF
                DC.B    'Please now provide an end address.',CR,LF
                DC.B    'Invalid entry will only lead to the question being repeated.',CR,LF
                DC.B    'Only numbers 0-9 and capitals A-F are accepted.',CR,LF
                                                                          * There's a chance we also want to iron out and check to ensure it's not our own code?
                DC.B    'What will the ending address be?',CR,LF,0
            
INSTRCTNS       DC.B    'We are ready to begin reading the code between the given starting and end addresses.',CR,LF
                DC.B    'While running, the program will pause every fifteen lines.',CR,LF
                DC.B    'To continue, press the "Enter" key.',CR,LF
                DC.B    'Do so now to begin.',CR,LF,0

HEX_to_ASCII    DC.B    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'


    END    START        * last line of source


