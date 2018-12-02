*-----------------------------------------------------------
* Title      :  Effective Addressing
* Written by : Torren,Doug,Jeremiah
* Date       : 11/30/2018
* Description: A collection of subroutines that determine the Effective addressing for 
* the instructions we need to support
*-----------------------------------------------------------
END_LINE        EQU     $0A00       ;END_LINE and REST_OF Comprise the characters that always need to be at the end of a line. 
REST_OF         EQU     $200D       ;IE Carriage Return, Line Feed, and NULL terminator. 1B is added as this is the escape character
                                    ;and fills out the word for rest_of as this makes managing the stack easier.
COMMA           EQU     $2C 
                ORG     $1000
START:   

LEA_ADDRESS     BCLR   D2,$11112222
                LEA    LEA_ADDRESS,A5
                MOVE.W (A5)+,D6
                JSR    BCLR_EA_FOUND             
                
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
                CLR.L   D0
                CLR.L   D1
                CLR.L   D3
                CLR.L   D4              ;Clear Registers of any junk (we expect these to be cleared)
                MOVE.W  #END_LINE,-(SP) ;EA always ends the line of an instruction, so add this to stack for output (Provides LF and Null terminator)
                MOVE.W  #REST_OF,-(SP)  
                MOVE.L  #$00000038,D0   ;Need to figure out the Destination EA mode, Grab bits 5,4,3
                AND.L   D6,D0
                LSR.L   #3,D0           ;Shift so we are comparing the mode bits where they would be valued 0-7
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

BCLR_RET_FOR_SRC                        ;Need to return from determining the EA destination to determine Dn source
                               
                MOVE.B  #COMMA,D0       ;Move comma between source and destination onto stack for output this should complete word values to push onto stack
                LSL.L   #8,D0
                CMP.L   #0,D1
                BEQ     BCLR_EVEN
                MOVE.B  D1,D0
                BRA     BCLR_ODD
BCLR_EVEN       MOVE.B  #$20,D0
BCLR_ODD        MOVE.W  D0,-(SP)
                CLR.L   D0       
                MOVE.L  #$00000E00,D0   ;Mask for Source Dn number
                AND.L   D6,D0           
                LSR.L   #8,D0           ;Need to move over 9 bits for Register number
                LSR.L   #1,D0
                JSR     DR_DIRECT
                MOVE.L  D5,-(SP)        ;Move return address back onto the stack
                RTS                     ;Return back to opcheck
                

BCLR_DR                                 ;Looking at our destination being a Data register (Dn)
                MOVE.L  #$00000007,D0   ;Need to grab the lowest 3 bits
                AND.L   D6,D0
                JSR     DR_DIRECT       ;Get ascii output of Dn register
                BRA     BCLR_RET_FOR_SRC 
                
BCLR_AR_IND
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT     ;Get ascii output of (An) register
                BRA     BCLR_RET_FOR_SRC
                
BCLR_AR_POST                           
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                BRA     BCLR_RET_FOR_SRC
                

BCLR_AR_PRE
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                BRA     BCLR_RET_FOR_SRC

BCLR_ADD        
                MOVE.L  #$00000007,D0   ;Mask for last 3 bits. Register determines the size of the address we are looking for in this case (000 = Word/001 = Long)
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     BCLR_ADDW
                BRA     BCLR_ADDL
BCLR_ADDW
                JSR     ADDW_OUT
                BRA     BCLR_RET_FOR_SRC            
                
BCLR_ADDL       
                JSR     ADDL_OUT
                BRA     BCLR_RET_FOR_SRC    

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
                MOVE.W  #$2841,-(SP)    ;After this our addressing should be (An),An where n is the register number
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
LEA_DW_LOOP     MOVE.L  #$0000F000,D0    ;Mask for last 4 bits (DW = DigitWord)
                AND.L   D3,D0
                LSR.L   #8,D0
                LSR.L   #4,D0
                LSL.L   #4,D3            ;Shift to next digit
                JSR     GET_ASCII_CHAR   ;Ascii digit should be in D1
                CMP.L   #3,D2            ;if we've done this 4 times (starting loop counter from 0)
                BEQ     LEA_DW_DONE
                LSL.L   #8,D1
                ADD.L   #1,D2
                BRA     LEA_DW_LOOP          
LEA_DW_DONE
                MOVE.L  D1,-(SP)        ;D1 should hold the ascii hex digits
                MOVE.B  #$24,-(SP)
                MOVE.L  D5,-(SP)
                RTS

LEA_ADDL                                           
                MOVE.L  (A5)+,D2
                MOVE.B  #0,D3
                MOVE.B  #0,D4
LEA_LW_LOOP     MOVE.L  #$0000F000,D0   ;This loop will have to be performed twice, due to only being able to store 4 ascii charcters (2 bytes each) at a time
                AND.L   D2,D0
                LSR.L   #8,D0
                LSR.L   #4,D0
                ROL.L   #4,D2           ;Grab the next hex digit
                JSR     GET_ASCII_CHAR  
                CMP.L   #3,D3           ;check to see if this was the last iteration
                BEQ     LEA_LWI_DONE
                LSL.L   #8,D1           ;Open up space for the next ascii value
                ADD.L   #1,D3
                BRA     LEA_LW_LOOP      
LEA_LWI_DONE    MOVE.L  D1,-(SP)        
                CLR.L   D1
                CMP.L   #1,D4
                BEQ     LEA_LWO_DONE    ;If the outer loop is not done, clear the inner loop sentinel value and D1 and move on
                ADD.L   #1,D4
                CLR.L   D3
                BRA     LEA_LW_LOOP
LEA_LWO_DONE    MOVE.B  #$24,-(SP)
                MOVE.L  D5,-(SP)
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
              
*-----------------DR_DIRECT----------------
*Description: Given a register number in D0, generate the ascii representation of the Data register
*for the EA. (EX: Given 101 in D0, push 'D5' onto the stack
*Data Register D0: The value of D0 on entering the SUB is the n in Dn
*-----------------DR_DIRECT----------------             
DR_DIRECT
             MOVE.L     (SP)+,D4        ;Save the return address
             MOVE.B     #$44,D2
             LSL.L      #8,D2           ;Move 'D' into the upper byte of lower order word
             JSR        GET_ASCII_CHAR  
             MOVE.B     D1,D2           ;After return from GET_ASCII_CHAR ascii value of digit in D0 should be in D1
             MOVE.W     D2,-(SP)        ;Move 'Dn' onto stack for output
             MOVE.L     D4,-(SP)
             CLR.L      D1
             RTS

*-----------------AR_INDIRECT----------------
*Description: Given a register number in D0, generate the ascii representation of the Indirect Address Register
*for EA. (EX: Given 101 in D0, push '(A5)' onto the stack)
*Data Register D0: The value of D0 on entering SUB is the n in (An)
*-----------------AR_INDIRECT----------------            
AR_INDIRECT    
            MOVE.L      (SP)+,D4        ;Save return address
            JSR         GET_ASCII_CHAR
            LSL.L       #8,D1           ;Move ascii digit over
            MOVE.B      #$29,D1
            MOVE.W      D1,-(SP)        ;Move 'n)' onto the stack
            MOVE.W      #$2841,-(SP)    ;Move '(A' onto the stack
            MOVE.L      D4,-(SP)
            CLR         D1
            RTS
           
*-----------------AR_INDIRECT_POST----------------
*Description: Given a register number in D0 generate the ascii representation of the Indirect Address Register with 
*post increment for EA. (EX: Given 101 in D0, push '(A5)+' onto the stack)
*Data Register D0: The value of D0 on entering SUB is the n in (An)+
*-----------------AR_INDIRECT_POST----------------
AR_INDIRECT_POST
            MOVE.L      (SP)+,D4        ;Save return address
            JSR         GET_ASCII_CHAR
            MOVE.W      #$292B,-(SP)    ;Push ')+' onto stack
            MOVE.B      #$41,D0
            LSL.L       #8,D0
            MOVE.B      D1,D0           
            MOVE.W      D0,-(SP)        ;Push 'An' onto the stack
            MOVE.W      #$28,D1          ;Push '(' onto D1 and wait for another character to fill the word
            MOVE.L      D4,-(SP)
            RTS

*-----------------AR_INDIRECT_PRE----------------
*Description: Does the same as INDIRECT_POST but with pre decrementing instead. (EX: Given 101 in D0, push '-(A5)' onto the stack)
*Data Register D0: The value of D0 on entering SUB is the n in -(An)
*-----------------AR_INDIRECT_PRE---------------- 
AR_INDIRECT_PRE
            MOVE.L      (SP)+,D4
            JSR         GET_ASCII_CHAR  
            LSL.L       #8,D1
            MOVE.B      #$29,D1         
            MOVE.W      D1,-(SP)        ;Push 'n)' onto the stack
            MOVE.W      #$2841,-(SP)    ;Push '(A' onto the stack
            MOVE.W      #$2D,D1         ;Push '-' into D1 (need something else to complete word)
            MOVE.L      D4,-(SP)
            RTS                        

*-----------------ADDW_OUT----------------
*Descripton: Given the current address in A5, which should be after the opcode, determine the Word address and push it onto the stack for 
*output. 
*Data Register A5 the  current address
*Data Register A5 on done, is the next address or the address of the next instruction to be diassembled
*Data Register D1 holds '$' and must be combined with another character to fill out a word to be pushed onto stack
*-----------------ADDW_OUT----------------
ADDW_OUT
            MOVE.L      (SP)+,A2
            MOVE.W      (A5)+,D4        ;Move the word address into A1      
            CLR.L       D0
            CLR.L       D1
            CLR.L       D2
            CLR.L       D3
ADDW_LOOP   MOVE.L      #$0000000F,D0     ;Mask for last 4 bits
            AND.L       D4,D0             ;This loop grabs 2 ascii values at a time and stores them into d1
            LSR.L       #4,D4
            JSR         GET_ASCII_CHAR
            ROR.W       #8,D1
            CMP.L       #1,D2
            BEQ         ADDW_PUSH
            ADD.L       #1,D2
            BRA         ADDW_LOOP
ADDW_PUSH   MOVE.W      D1,-(SP)        ;Push 2 ascii values onto stack
            CMP.L       #1,D3           
            BEQ         ADDW_DONE
            ADD.L       #1,D3
            CLR.L       D2
            CLR.L       D1
            BRA         ADDW_LOOP
ADDW_DONE   MOVE.B      #$24,D1
            MOVE.L      A2,-(SP)
            RTS

*-----------------ADDL_OUT----------------
*Description: Does what ADDW does but for Long addresses
*Data Register A5 the  current address
*Data Register A5 on done, is the next address or the address of the next instruction to be diassembled
*Data Register D1 holds '$' and must be combined with another character to fill out a word to be pushed onto stack
*-----------------ADDL_OUT----------------
ADDL_OUT
            MOVE.L      (SP)+,A2
            MOVE.L      (A5)+,D4        ;Move the word address into A1      
            CLR.L       D0
            CLR.L       D1
            CLR.L       D2
            CLR.L       D3
ADDL_LOOP   MOVE.L      #$0000000F,D0     ;Mask for last 4 bits
            AND.L       D4,D0             ;This loop grabs 2 ascii values at a time and stores them into d1
            LSR.L       #4,D4
            JSR         GET_ASCII_CHAR
            ROR.W       #8,D1
            CMP.L       #1,D2
            BEQ         ADDL_PUSH
            ADD.L       #1,D2
            BRA         ADDL_LOOP
ADDL_PUSH   MOVE.W      D1,-(SP)        ;Push 2 ascii values onto stack
            CMP.L       #3,D3           ;Loops twice
            BEQ         ADDL_DONE
            ADD.L       #1,D3
            CLR.L       D2
            CLR.L       D1
            BRA         ADDL_LOOP
ADDL_DONE   MOVE.B      #$24,D1
            MOVE.L      A2,-(SP)
            RTS


            
       END    START              
              
              



*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
