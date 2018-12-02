*-----------------------------------------------------------
* Title      :
* Written by :
* Date       :
* Description:
*-----------------------------------------------------------
END_LINE        EQU     $0A00

                ORG     $1000
START:   

LEA_ADDRESS     LEA    $14454567,A6
                LEA    LEA_ADDRESS,A5
                MOVE.W (A5)+,D6
                JSR    LEA_EA_FOUND               
                
                MOVE.L SP,A1
                MOVE.L #14,D0
                TRAP   #15
                BRA    DONE
                
*-----------------STATIC OPERAND----------------
*This section is composed of Instructions where one operand (source or destination
*is has a static addressing mode SOURCE OR OPERAND != EA
*-----------------STATIC OPERAND----------------
BCLR_EA_FOUND                           ;If the BCLR instruction is found
                MOVE.L  (SP)+,D5        ;Save Return Address
                CLR.L   DO
                CLR.L   D1
                CLR.L   D3
                CLR.L   D4              ;Clear Registers of any junk (we expect these to be cleared)
                MOVE.L  $#00000038,D0   ;Need to figure out the Destination EA mode, Grab bits 5,4,3
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     BCLR_DR
                CMP.L   #2,D0
                BEQ     BCLR_AR_IND
                CMP.L   #3,D0
                BEQ     BCLR_AR_POST    ;Address Register Indirection with Post Increment
                CMP.L   #4,D0       
                BEQ     BCLR_AR_PRE     ;Address Register Indirection with Pre Decrement
                CMP.L   #7,D0           ;Actual Address values
                BEQ     BCLR_ADD        

BCLR_DR
BCLR_AR_IND
BCLR_AR_POST
BCLR_AR_PRE
BCLR_ADD
                

LEA_EA_FOUND    
                MOVE.L  (SP)+,D5        ;Save the return address
                CLR.L   D0              ;Clear D0 of any junk for masking bits
                MOVE.L  #$00000E00,D0   ;Mask to get the destination Address register
                AND.L   D6,D0           ;And to get the address register Number
                LSR.L   #8,D0           ;Shift three bits to the end to get our address number
                LSR.L   #1,D0
                JSR     GET_ASCII_CHAR 
                MOVE.W  #END_LINE,-(SP) ;Begin the address hex string
                MOVE.B  D1,D0
                LSL.L   #8,D0           ;The address number shifted into high order byte of the word
                MOVE.B  #$0D,D0         ;Add carriage return for end of line
                MOVE.W  D0,-(SP)        
                MOVE.W  #$2C41,-(SP)    ;,A of destination address register
                MOVE.L  #$00000038,D0   ;Move masking for EA mode
                AND.L   D6,D0           
                LSR.L   #3,D0           ;Shift bits 3 to left to get mode number
                CMP.L   #2,D0
                BEQ     LEA_ADDR_IND
                CMP.L   #7,D0
                BEQ     LEA_ADD    
                    
LEA_ADDR_IND                            ;If the effective addressing mode is Address Register Indirect
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     GET_ASCII_CHAR  ;Get the ascii value of the hex digit for our address number
                LSL.L   #8,D1           ;Shift over the ascii number
                MOVE.B  #$29,D1
                MOVE.W  D1,-(SP)
                MOVE.W  #$2841,-(SP)    ;After this our addressing should be (AX),AX where X is the register number
                MOVE.L  D5,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
                
LEA_ADD                                 ;If the evective addressing mode is a straight address
                MOVE.L #$00000007,D0    ;Mask for last bits
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     LEA_ADDW
                BRA     LEA_ADDL        ;Only two options here for address size
                    
LEA_ADDW
                CLR     D0
                CLR     D1
                CLR     D2
                MOVE.W  (A5)+,D3         ;Move and increment current address (at this point current address should be on the next instruction to be disassembled)
                MOVE.B  #0,D2            ;We want to find 4 digits for the word address
DW_LOOP         MOVE.L  #$0000F000,D0    ;Mask for last 4 bits (DW = DigitWord)
                AND.L   D3,D0
                LSR.L   #8,D0
                LSR.L   #4,D0
                LSL.L   #4,D3            ;Shift to next digit
                JSR     GET_ASCII_CHAR   ;Ascii digit should be in D1
                CMP.L   #3,D2            ;if we've done this 4 times (starting loop counter from 0)
                BEQ     DW_DONE
                LSL.L   #8,D1
                ADD.L   #1,D2
                BRA     DW_LOOP          
DW_DONE
                MOVE.L  D1, -(SP)        ;D1 should hold the ascii hex digits
                MOVE.L  D5,-(SP)
                RTS

LEA_ADDL                                           
                MOVE.L  (A5)+,D2
                MOVE.B  #0,D3
                MOVE.B  #0,D4
LW_LOOP         MOVE.L  #$0000F000,D0   ;This loop will have to be performed twice, due to only being able to store 4 ascii charcters (2 bytes each) at a time
                AND.L   D2,D0
                LSR.L   #8,D0
                LSR.L   #4,D0
                ROL.L   #4,D2           ;Grab the next hex digit
                JSR     GET_ASCII_CHAR  
                CMP.L   #3,D3           ;check to see if this was the last iteration
                BEQ     LWI_DONE
                LSL.L   #8,D1           ;Open up space for the next ascii value
                ADD.L   #1,D3
                BRA     LW_LOOP      
LWI_DONE        MOVE.L  D1,-(SP)        
                CLR.L   D1
                CMP.L   #1,D4
                BEQ     LWO_DONE        ;If the outer loop is not done, clear the inner loop sentinel value and D1 and move on
                ADD.L   #1,D4
                CLR.L   D3
                BRA     LW_LOOP
LWO_DONE        MOVE.L  D5,-(SP)
                RTS      
                    
  
DONE    SIMHALT             

* Put variables and constants here
HEX_to_ASCII    DC.B    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'


       

*-----------------GET_ASCII_CHAR SUB----------------
* Description: Given a number between 0 and F in Hex in D0, returns the corresponding
* ascii value in D1
* Data Register D0: The hex value we want to find the corresponding ascii value for
* Data Register D1: The ascii value (in hex IE: 41 = 'A') of the hex digit
*-----------------GET_ASCII_CHAR_SUB----------------
GET_ASCII_CHAR
              LEA       HEX_to_ASCII,A1
              ADDA.W    D0,A1
              MOVE.B    (A1),D1
              RTS
     
       END    START              
              
              
*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
