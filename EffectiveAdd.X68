*-----------------------------------------------------------
* Title      :  Effective Addressing
* Written by : Torren,Doug,Jeremiah
* Date       : 11/30/2018
* Description: A collection of subroutines that determine the Effective addressing for 
* the instructions we need to support
*-----------------------------------------------------------
END_LINE        EQU     $200D0A00       ;END_LINE and REST_OF Comprise the characters that always need to be at the end of a line.                                     ;and fills out the word for rest_of as this makes managing the stack easier.
COMMA           EQU     $2C 
                ORG     $1000
START:   

TEST_INSTR      MOVE.L  $5000,$60005000
                LEA     TEST_INSTR,A5
                MOVE.W  (A5)+,D6
                JSR     MOVE_EA        
                
                MOVE.L SP,A1
                MOVE.L #14,D0
                TRAP   #15
                BRA    DONE
                
BCLR_EA                           ;If the BCLR instruction is found
                MOVE.L  (SP)+,A3        ;Save Return Address
                CLR.L   D0
                CLR.L   D1
                CLR.L   D3
                CLR.L   D4              ;Clear Registers of any junk (we expect these to be cleared)
                MOVE.L  #END_LINE,-(SP) ;EA always ends the line of an instruction, so add this to stack for output (Provides LF and Null terminator)  
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
                MOVE.L  A3,-(SP)        ;Move return address back onto the stack
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

LEA_EA    
                MOVE.L  (SP)+,A3        ;Save the return address
                MOVE.L  #END_LINE,-(SP)
                CLR.L   D0              ;Clear D0 of any junk for masking bits
                MOVE.L  #$00000E00,D0   ;Mask to get the destination Address register
                AND.L   D6,D0           ;And to get the address register Number
                LSR.L   #8,D0           ;Shift three bits to the end to get our address number
                LSR.L   #1,D0
                JSR     AR_DIRECT
                CLR.L   D1
                MOVE.B  #COMMA,D1
                LSL.L   #8,D1
                MOVE.B  #$20,D1
                MOVE.W  D1,-(SP)        ;', ' for output
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
                JSR     AR_INDIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
                
LEA_ADD                                 ;If the evective addressing mode is a straight address
                MOVE.L #$00000007,D0    ;Mask for last bits
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     LEA_ADDW
                BRA     LEA_ADDL        ;Only two options here for address size
                    
LEA_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0       ;Move space into D0, then move $
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Move the final ' $' of the address string onto the stack
                MOVE.L  A3,-(SP)
                RTS

LEA_ADDL        
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0       ;Move space into D0, then move $
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Move the final ' $' of the address string onto the stack                    
                MOVE.L  A3,-(SP)
                RTS      
JSR_EA
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                CLR.L   D0
                MOVE.L  #$00000038,D0   ;Mask for JSR Mode
                AND.L   D6,D0           
                LSR.L   #3,D0           ;Shift right 3 bits to get mode value
                CMP.L   #2,D0
                BEQ     JSR_AR_IND      ;Address register indirect
                BRA     JSR_ADD         ;Absolute Address
                               
JSR_AR_IND
                MOVE.L  #$00000007,D0   ;Mask for last 3 bits to determine address number
                AND.L   D6,D0           
                JSR     AR_DIRECT
                MOVE.L  A3,-(SP)
                RTS                     ;Only one EA for JSR
JSR_ADD
                MOVE.L  #$00000007,D0   ;
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     JSR_ADDW
                BRA     JSR_ADDL
JSR_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0       ;Move space into D0, then move $
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Move the final ' $' of the address string onto the stack
                MOVE.L  A3,-(SP)
                RTS

JSR_ADDL                  
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0       ;Move space into D0, then move $
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Move the final ' $' of the address string onto the stack
                MOVE.L  A3,-(SP)
                RTS

NEG_EA       
                MOVE.L  (SP)+,A3        ;Save return address
                MOVE.L  #END_LINE,-(SP)
                CLR.L   D0
                MOVE.L  #$00000038,D0    ;Mask for mode bits
                AND.L   D6,D0
                LSR.L   #3,D0           ;Shift for mode value
                CMP.L   #0,D0
                BEQ     NEG_DR
                CMP.L   #2,D0
                BEQ     NEG_AR_IND
                CMP.L   #3,D0
                BEQ     NEG_AR_IND_POST
                CMP.L   #4,D0
                BEQ     NEG_AR_IND_PRE
                BRA     NEG_ADD         ;Default value is Absolute Addressing 
                  
NEG_DR
                MOVE.L  #$00000007,D0    ;Mask for last 3 bits
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)
                RTS
NEG_AR_IND
                MOVE.L  #$00000007,D0    ;Mask for last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT     
                MOVE.L  A3,-(SP)
                RTS
                
NEG_AR_IND_POST
                MOVE.L  #$00000007,D0    ;Mask for last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                CLR.L   D0
                MOVE.B  #$20,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Have to fill out the word containing '(' so add a space ' ('
                MOVE.L  A3,-(SP)
                RTS
NEG_AR_IND_PRE
                MOVE.L  #$00000007,D0    ;Mask for last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                CLR.L   D0
                MOVE.B  #$20,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Have to fill out the word containing '-' so add a space ' -'
                MOVE.L  A3,-(SP)
                RTS

NEG_ADD
                MOVE.L  #$00000007,D0    ;Mask last 3 bits to find size
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     NEG_ADDW
                BRA     NEG_ADDL
NEG_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0           ;Move the '$' character into D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3,-(SP)
                RTS
NEG_ADDL
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0           ;Move the '$' character into D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3,-(SP)
                RTS

CMP_EA       
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000E00,D0       ;Mask for Dn register number
                AND.L   D6,D0
                JSR     DR_DIRECT           ;Data registers are always the destination
                CLR     D0
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)            ;Move operand separator
                MOVE.L  #$00000038,D0       ;Now to determine source operand 
                AND.L   D6,D0
                LSR.L   #3,D0               ;Shift for mode value
                CMP.L   #0,D0
                BEQ     CMP_DR
                CMP.L   #1,D0
                BEQ     CMP_AR
                CMP.L   #2,D0
                BEQ     CMP_AR_IND
                CMP.L   #3,D0
                BEQ     CMP_AR_IND_POST
                CMP.L   #4,D0
                BEQ     CMP_AR_IND_PRE
                BRA     CMP_IMM_ADD         ;Immediate and Absolute address have same mode register, branch to determine between the three (word,Long,immediate value)
CMP_DR
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Data register number
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.L  A3, -(SP)
                RTS
CMP_AR
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_DIRECT
                MOVE.L  A3, -(SP)
                RTS
     
CMP_AR_IND
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.L  A3, -(SP)
                RTS
                
CMP_AR_IND_POST
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

CMP_AR_IND_PRE
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

CMP_IMM_ADD
                MOVE.L  #$00000007,D0
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     CMP_ADDW
                CMP.L   #1,D0
                BEQ     CMP_ADDL
                BRA     CMP_IMM

CMP_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3,-(SP)
                RTS                
CMP_ADDL
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3,-(SP)
                RTS    
CMP_IMM
                MOVE.L  #$000001C0,D0   *Mask to determine opmode for immediate values
                AND.L   D6,D0
                LSR.L   #6,D0           
                CMP.L   #0,D0
                BEQ     CMP_IMMB
                CMP.L   #1,D0
                BEQ     CMP_IMMW
                BRA     CMP_IMML     
CMP_IMMB
                JSR     IMMB_OUT
                MOVE.L  A3,-(SP)
                RTS
CMP_IMMW
                JSR     IMMW_OUT
                MOVE.L  A3,-(SP)
                RTS
CMP_IMML    
                JSR     IMML_OUT
                MOVE.L  A3,-(SP)
                RTS
                
CMPI_EA         
                MOVE.L  (SP)+,A3        *Store return address 
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  A5,A4         ;Save the current address of the immediate value
                MOVEA.L #$0,A0
                MOVE.L  #$00000038,D0   *Mask for mode bits
                AND.L   D6,D0
                LSR.L   #3,D0           
                CMP.L   #0,D0
                BEQ     CMPI_DR
                CMP.L   #2,D0
                BEQ     CMPI_AR_IND
                CMP.L   #3,D0
                BEQ     CMPI_AR_IND_POST
                CMP.L   #4,D0
                BEQ     CMPI_AR_IND_PRE
                BRA     CMPI_ADD
CMPI_RET_FOR_SRC                      *Branch back to do Immediate value
                MOVE.L  A4,A5         *Move the address of immediate value back into A5
                MOVE.L  #$000000C0,D0   
                AND.L   D6,D0         *Mask for size
                LSR.L   #6,D0
                CMP.L   #0,D0
                BEQ     CMPI_IMMB
                CMP.L   #1,D0
                BEQ     CMPI_IMMW
                BRA     CMPI_IMML
CMPI_IMMB
                JSR     IMMB_OUT
                CMP.L   #0,A0
                BEQ     CMPI_NO_ADDB_CHANGE
                MOVE.L  A0,A5           *Need to check if we need to move the address at A5 (This could happen if we had to get an Address Value for the destination
CMPI_NO_ADDB_CHANGE  
                MOVE.L  A3,-(SP)
                RTS
                
CMPI_IMMW
                JSR     IMMW_OUT
                CMP.L   #0,A0
                BEQ     CMPI_NO_ADDW_CHANGE
                MOVE.L  A0,A5           *Need to check if we need to move the address at A5 (This could happen if we had to get an Address Value for the destination
CMPI_NO_ADDW_CHANGE   
                MOVE.L  A3,-(SP)
                RTS

CMPI_IMML
                JSR     IMML_OUT
                CMP.L   #0,A0
                BEQ     CMPI_NO_ADDL_CHANGE
                MOVE.L  A0,A5           *Need to check if we need to move the address at A5 (This could happen if we had to get an Address Value for the destination
CMPI_NO_ADDL_CHANGE   
                MOVE.L  A3,-(SP)
                RTS

                
CMPI_DR
                MOVE.L  #$00000007,D0   *Mask for 3 bits to get register number
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     CMPI_RET_FOR_SRC
                
CMPI_AR_IND
                MOVE.L  #$00000007,D0   *Mask 3 bits to get register number
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     CMPI_RET_FOR_SRC                
CMPI_AR_IND_POST
                MOVE.L  #$00000007,D0   *Mask 3 bits for register number
                CLR.L   D1
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     CMPI_RET_FOR_SRC 
                
CMPI_AR_IND_PRE
                MOVE.L  #$00000007,D0   *Mask 3 bits for register number
                CLR.L   D1
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     CMPI_RET_FOR_SRC 
CMPI_ADD        
                MOVE.L  #$000000C0,D0
                AND.L   D6,D0           *Need to mask the bits of size of the instruction to jump over immediate value and get the address
                LSR.L   #6,D0
                CMP.L   #1,D0
                BLE     CMPI_WORD       *Add appropriate offset to get to address (4 for longs, 2 for words and bytes)
                ADDA.L  #4,A5
                BRA     CMPI_CONTINUE_ADD
CMPI_WORD       ADDA.L  #2,A5
CMPI_CONTINUE_ADD    
                MOVE.L  #$00000007,D0   *mask 3 bits for absolute address size 
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     CMPI_ADDW
                BRA     CMPI_ADDL
CMPI_ADDW
                JSR     ADDW_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A5,A0           *Store the address of the next instruction in A0 to be returned to later
                BRA     CMPI_RET_FOR_SRC 
CMPI_ADDL       
                JSR     ADDL_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A5,A0           *Store the address of the next instruction in A0 to be returned to later
                BRA     CMPI_RET_FOR_SRC 
                   
ORI_EA          
                MOVE.L  (SP)+,A3        *Store return address 
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  A5,A4         ;Save the current address of the immediate value
                MOVEA.L #$0,A0        ;Clear A0, will determine if we need to move the Current address to the next instruction
                MOVE.L  #$00000038,D0   *Mask for mode bits
                AND.L   D6,D0
                LSR.L   #3,D0
                LEA     ADD_MODES,A1  *List for different addressing modes           
                CMP.W   (A1)+,D0
                BEQ     ORI_DR
                ADDA.W  #2,A1         *ORI doesn't support Address register direct
                CMP.W   (A1)+,D0
                BEQ     ORI_AR_IND
                CMP.W   (A1)+,D0
                BEQ     ORI_AR_IND_POST
                CMP.W   (A1)+,D0
                BEQ     ORI_AR_IND_PRE
                BRA     ORI_ADD
ORI_RET_FOR_SRC
                MOVE.L  A4,A5         *Move the address of immediate value back into A5
                MOVEA.L #$0,A1
                MOVE.L  #$000000C0,D0   *Branch back to do Immediate value
                AND.L   D6,D0           *Mask for size
                LSR.L   #6,D0
                CMP.L   #0,D0
                BEQ     ORI_IMMB
                CMP.L   #1,D0
                BEQ     ORI_IMMW
                BRA     ORI_IMML
ORI_IMMB
                JSR     IMMB_OUT
                CMP.L   #0,A0
                BEQ     ORI_NO_ADDB_CHANGE
                MOVE.L  A0,A5           *Need to check if we need to move the address at A5 (This could happen if we had to get an Address Value for the destination
ORI_NO_ADDB_CHANGE  
                MOVE.L  A3,-(SP)
                RTS
            
ORI_IMMW
                JSR     IMMW_OUT
                CMP.L   #0,A0
                BEQ     ORI_NO_ADDW_CHANGE
                MOVE.L  A0,A5           *Need to check if we need to move the address at A5 (This could happen if we had to get an Address Value for the destination
ORI_NO_ADDW_CHANGE  
                MOVE.L  A3,-(SP)
                RTS

ORI_IMML
                JSR     IMML_OUT
                CMP.L   #0,A0
                BEQ     ORI_NO_ADDL_CHANGE
                MOVE.L  A0,A5           *Need to check if we need to move the address at A5 (This could happen if we had to get an Address Value for the destination
ORI_NO_ADDL_CHANGE  
                MOVE.L  A3,-(SP)
                RTS
  
ORI_DR
                MOVE.L  #$00000007,D0   *Mask for 3 bits to get register number
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     ORI_RET_FOR_SRC
          
ORI_AR_IND
                MOVE.L  #$00000007,D0   *Mask 3 bits to get register number
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     ORI_RET_FOR_SRC      
                
ORI_AR_IND_POST
                MOVE.L  #$00000007,D0   *Mask 3 bits for register number
                CLR.L   D1
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     ORI_RET_FOR_SRC 
  
ORI_AR_IND_PRE
                MOVE.L  #$00000007,D0   *Mask 3 bits for register number
                CLR.L   D1
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     CMPI_RET_FOR_SRC 

ORI_ADD       
                MOVE.L  #$000000C0,D0
                AND.L   D6,D0           *Need to mask the bits of size of the instruction to jump over immediate value and get the address
                LSR.L   #6,D0
                CMP.L   #1,D0
                BLE     ORI_WORD       *Add appropriate offset to get to address (4 for longs, 2 for words and bytes)
                ADDA.L  #4,A5
                BRA     ORI_CONTINUE_ADD
ORI_WORD        ADDA.L  #2,A5
ORI_CONTINUE_ADD    
                MOVE.L  #$00000007,D0   *mask 3 bits for absolute address size 
                AND.L   D6,D0
                CMP.L   #0,D0
                BEQ     ORI_ADDW
                BRA     ORI_ADDL
ORI_ADDW
                JSR     ADDW_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A5,A0           *Store the address of the next instruction in A0 to be returned to later
                BRA     ORI_RET_FOR_SRC 
        
ORI_ADDL
                JSR     ADDL_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A5,A0           *Store the address of the next instruction in A0 to be returned to later
                BRA     ORI_RET_FOR_SRC 
             
ADDA_EA
                MOVE.L  (SP)+,A3        ;Save the return address
                MOVE.L  #END_LINE,-(SP)
                CLR.L   D0              ;Clear D0 of any junk for masking bits
                MOVE.L  #$00000E00,D0   ;Mask to get the destination Address register
                AND.L   D6,D0           ;And to get the address register Number
                LSR.L   #8,D0           ;Shift three bits to the end to get our address number
                LSR.L   #1,D0
                JSR     AR_DIRECT
                CLR.L   D0
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)        ;', ' for output
                MOVE.L  #$00000038,D0   ;Move masking for EA mode
                AND.L   D6,D0           
                LSR.L   #3,D0           ;Shift bits 3 to left to get mode number
                LEA     ADD_MODES,A1    ;Load addressing modes list for comparison
                CMP.W   (A1)+,D0
                BEQ     ADDA_DR
                CMP.W   (A1)+,D0
                BEQ     ADDA_AR
                CMP.W   (A1)+,D0
                BEQ     ADDA_AR_IND
                CMP.W   (A1)+,D0
                BEQ     ADDA_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     ADDA_AR_IND_PRE
                BRA     ADDA_ADD_IMM
ADDA_DR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
         
ADDA_AR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
ADDA_AR_IND
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck


ADDA_AR_IND_POST
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

ADDA_AR_IND_PRE
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

ADDA_ADD_IMM
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will let us know if we have Absolute addressing or Immediate  
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     ADDA_ADDW
                CMP.W   (A1)+,D0
                BEQ     ADDA_ADDL
                BRA     ADDA_IMM
ADDA_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS
     
ADDA_ADDL
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0           
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS
ADDA_IMM
                MOVE.L  #$000001C0,D0       *Mask for opmode which will give the size needed for the immediate value
                AND.L   D6,D0
                LSR.L   #6,D0
                CMP.L   #3,D0
                BEQ     ADDA_IMMW
                BRA     ADDA_IMML
ADDA_IMMW
                JSR     IMMW_OUT
                MOVE.L  A3,-(SP)
                RTS
ADDA_IMML
                JSR     IMML_OUT
                MOVE.L  A3,-(SP)
                RTS
                
MOVEA_EA        
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000E00,D0       ;Mask for address register number bits
                AND.L   D6,D0
                MOVE.L  #9,D1
                LSR.L   D1,D0
                CLR.L   D1
                JSR     AR_DIRECT       ;Only one kind of destination (Address register)
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                CLR.L   D0
                MOVE.L  #$00000038,D0   ;Move masking for EA mode
                AND.L   D6,D0           
                LSR.L   #3,D0           ;Shift bits 3 to left to get mode number
                LEA     ADD_MODES,A1    ;Load addressing modes list for comparison
                CMP.W   (A1)+,D0
                BEQ     MOVEA_DR
                CMP.W   (A1)+,D0
                BEQ     MOVEA_AR
                CMP.W   (A1)+,D0
                BEQ     MOVEA_AR_IND
                CMP.W   (A1)+,D0
                BEQ     MOVEA_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     MOVEA_AR_IND_PRE
                BRA     MOVEA_ADD_IMM
             
MOVEA_DR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck

MOVEA_AR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
  
MOVEA_AR_IND
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck

MOVEA_AR_IND_POST
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

MOVEA_AR_IND_PRE
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

MOVEA_ADD_IMM
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will let us know if we have Absolute addressing or Immediate  
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     MOVEA_ADDW
                CMP.W   (A1)+,D0
                BEQ     MOVEA_ADDL
                BRA     MOVEA_IMM
MOVEA_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS
   
MOVEA_ADDL
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS

MOVEA_IMM
                MOVE.L  #$00003000,D0       *Mask for opmode which will give the size needed for the immediate value
                AND.L   D6,D0
                LEA     MOVE_SIZES,A1
                ADDA.W  #2,A1
                CMP.W   (A1),D0
                BEQ     MOVEA_IMMW
                BRA     MOVEA_IMML
MOVEA_IMMW
                JSR     IMMW_OUT
                MOVE.L  A3,-(SP)
                RTS

MOVEA_IMML
                JSR     IMML_OUT
                MOVE.L  A3,-(SP)
                RTS

SUBQ_EA         
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000038,D0
                AND.L   D6,D0
                LSR.L   #3,D0
                LEA     ADD_MODES,A1
                CMP.W   (A1)+,D0
                BEQ     SUBQ_DR
                CMP.W   (A1)+,D0
                BEQ     SUBQ_AR
                CMP.W   (A1)+,D0
                BEQ     SUBQ_AR_IND
                CMP.W   (A1)+,D0
                BEQ     SUBQ_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     SUBQ_AR_IND_PRE
                BRA     SUBQ_ADD
                
SUBQ_DR
                MOVE.L  #$00000007,D0   *Mask for 3 bits to get register number
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     SUBQ_RET_FOR_SRC
               
SUBQ_AR
                MOVE.L  #$00000007,D0   *Mask 3 bits to get register number
                AND.L   D6,D0
                JSR     AR_DIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     SUBQ_RET_FOR_SRC    
SUBQ_AR_IND
                MOVE.L  #$00000007,D0   *Mask 3 bits to get register number
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     SUBQ_RET_FOR_SRC    
SUBQ_AR_IND_POST
                MOVE.L  #$00000007,D0   *Mask 3 bits for register number
                CLR.L   D1
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     SUBQ_RET_FOR_SRC 
 
SUBQ_AR_IND_PRE
                MOVE.L  #$00000007,D0   *Mask 3 bits for register number
                CLR.L   D1
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     SUBQ_RET_FOR_SRC 

SUBQ_ADD
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will let us know if we have Absolute addressing or Immediate  
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1),D0
                BEQ     SUBQ_ADDW
                BRA     SUBQ_ADDL
SUBQ_ADDW
                JSR     ADDW_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A5,A0           *Store the address of the next instruction in A0 to be returned to later
                BRA     SUBQ_RET_FOR_SRC
                
SUBQ_ADDL
                JSR     ADDL_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A5,A0           *Store the address of the next instruction in A0 to be returned to later
                BRA     SUBQ_RET_FOR_SRC
                
SUBQ_RET_FOR_SRC
                MOVE.L  #0,D2
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                LEA     SUBQ_IMM_VALUES,A1    
                CMP.W   (A1)+,D0            
                BEQ     SUBQ_VALUE_8        *If the value is equal to 0, it's actually equal to 8. Dope
SUBQ_LOOP       CMP.W   #8,D2             *Loop finds the proper immediate value between 1 and 7
                BEQ     SUBQ_BAD            
                CMP.W   (A1)+,D0    
                BEQ     SUBQ_LOOP_DONE
                ADD.L   #1,D2
                BRA     SUBQ_LOOP
SUBQ_VALUE_8
                MOVE.L  #8,D0
                JSR     GET_ASCII_CHAR
                MOVE.W  #$2400,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.W  #$2023,-(SP)
                BRA     SUBQ_IMM_DONE
SUBQ_LOOP_DONE
                JSR     GET_ASCII_CHAR
                MOVE.W  #$2400,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.W  #$2023,-(SP)
                BRA     SUBQ_IMM_DONE
                
SUBQ_BAD                                    *JUST in case really. Prints out BADV
                MOVE.W #$4456,-(SP)
                MOVE.W #$4241,-(SP)
                BRA     SUBQ_IMM_DONE              
SUBQ_IMM_DONE
                MOVE.L  A3,-(SP)
                RTS

ADD_SUB_EAREG_EA                            *Add and Sub have the same process for EA determination. ITS FREE CODE [https://www.youtube.com/watch?v=cd4-UnU8lWY]

                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000D00,D0      *Mask for Dn in destination
                AND.L   D6,D0
                MOVE.L  #9,D1              *Need to shift 9 bits for the Register number
                LSR.L   D1,D0
                CLR.L   D1
                JSR     DR_DIRECT
                MOVE.W  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)            *Push comma to stack to split operands
                MOVE.L  #$00000038,D0       *Move mask for mode bits int D0
                AND.L   D6,D0
                LSR.L   #3,D0
                LEA     ADD_MODES,A1    ;Load addressing modes list for comparison
                CMP.W   (A1)+,D0
                BEQ     ADD_ER_DR
                CMP.W   (A1)+,D0
                BEQ     ADD_ER_AR
                CMP.W   (A1)+,D0
                BEQ     ADD_ER_AR_IND
                CMP.W   (A1)+,D0
                BEQ     ADD_ER_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     ADD_ER_AR_IND_PRE
                BRA     ADD_ER_ADD_IMM

ADD_ER_DR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck

ADD_ER_AR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck

ADD_ER_AR_IND
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck

ADD_ER_AR_IND_POST
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

ADD_ER_AR_IND_PRE
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS
 
ADD_ER_ADD_IMM
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will let us know if we have Absolute addressing or Immediate  
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     ADD_ER_ADDW
                CMP.W   (A1)+,D0
                BEQ     ADD_ER_ADDL
                BRA     ADD_ER_IMM
ADD_ER_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS
   
ADD_ER_ADDL
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS

ADD_ER_IMM
                MOVE.L  #$00003000,D0       *Mask for opmode which will give the size needed for the immediate value
                AND.L   D6,D0
                LEA     ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     ADD_ER_IMMB
                CMP.W   (A1),D0
                BEQ     ADD_ER_IMMW
                BRA     ADD_ER_IMML
ADD_ER_IMMB
                JSR     IMMB_OUT
                MOVE.L  A3,-(SP)
                RTS
                
ADD_ER_IMMW
                JSR     IMMW_OUT
                MOVE.L  A3,-(SP)
                RTS

ADD_ER_IMML
                JSR     IMML_OUT
                MOVE.L  A3,-(SP)
                RTS
                
ADD_SUB_OR_REGEA_EA                         *ADD, SUB, OR have for REG->EA have same process for determining EA. ITS FREE REAL ESTATE 
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
EOR_ENTRY       MOVE.L  #$00000038,D0       *Mask for mode bits of destination. FOR EOR: If the EA mode is not DR direct, EOR has same process for determining EA
                AND.L   D6,D0
                LSR.L   #3,D0
                MOVE.B  #4,D1
                LEA     ADD_MODES,A1
                ADDA.L  D1,A1
                CLR.L   D1
                CMP.W   (A1)+,D0
                BEQ     ADD_RE_AR_IND
                CMP.W   (A1)+,D0
                BEQ     ADD_RE_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     ADD_RE_AR_IND_PRE
                BRA     ADD_RE_ADD
ADD_RE_AR_IND
                MOVE.L  #$00000007,D0       *Mask for register number
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     ADD_REGEA_SRC

ADD_RE_AR_IND_POST
                MOVE.L  #$00000007,D0       *Mask for register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     ADD_REGEA_SRC
                
ADD_RE_AR_IND_PRE
                MOVE.L  #$00000007,D0       *Mask for register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     ADD_REGEA_SRC

ADD_RE_ADD
                MOVE.L  #$00000007,D0       *Mask for register number
                AND.L   D6,D0               
                LEA     AR_ADD_SIZES,A1     *Need to determine if we are using a Word/Long address or Immediate value
                CMP.W   (A1),D0
                BEQ     ADD_RE_ADDW
                BRA     ADD_RE_ADDL
ADD_RE_ADDW
                JSR     ADDW_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     ADD_REGEA_SRC
                
ADD_RE_ADDL
                JSR     ADDL_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     ADD_REGEA_SRC
                
ADD_REGEA_SRC
                MOVE.L  #$00000D00,D0      *Mask for Dn in destination
                AND.L   D6,D0
                MOVE.L  #9,D1              *Need to shift 9 bits for the Register number
                LSR.L   D1,D0
                CLR.L   D1
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)
                RTS

MULS_DIVS_EA                               *MULS and DIVS use same process for determining EA
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000D00,D0      *Mask for Dn in destination
                AND.L   D6,D0
                MOVE.L  #9,D1              *Need to shift 9 bits for the Register number
                LSR.L   D1,D0
                CLR.L   D1
                JSR     DR_DIRECT
                MOVE.W  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)            *Push comma to stack to split operands
                MOVE.L  #$00000038,D0       *Move mask for mode bits int0 D0
                AND.L   D6,D0
                LSR.L   #3,D0
                LEA     ADD_MODES,A1    ;Load addressing modes list for comparison
                CMP.W   (A1)+,D0
                BEQ     MULS_DIVS_DR
                ADDA.W  #2,A1
                CMP.W   (A1)+,D0
                BEQ     MULS_DIVS_AR_IND
                CMP.W   (A1)+,D0
                BEQ     MULS_DIVS_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     MULS_DIVS_AR_IND_PRE
                BRA     MULS_DIVS_AR_ADD_IMM
                
MULS_DIVS_DR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
     
MULS_DIVS_AR_IND
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck

MULS_DIVS_AR_IND_POST
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

MULS_DIVS_AR_IND_PRE
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

MULS_DIVS_AR_ADD_IMM
                 MOVE.L  #$00000007,D0        *Mask for last 3 bits which will let us know if we have Absolute addressing or Immediate  
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     MULS_DIVS_ADDW
                CMP.W   (A1)+,D0
                BEQ     MULS_DIVS_ADDL
                BRA     MULS_DIVS_IMM
MULS_DIVS_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS
   
MULS_DIVS_ADDL
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS
                
MULS_DIVS_IMM     
                JSR     IMMW_OUT            *Only immediate size supported is Word, so just use word. 
                MOVE.L  A3,-(SP)
                RTS
 
OR_EAREG_EA
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000D00,D0      *Mask for Dn in destination
                AND.L   D6,D0
                MOVE.L  #9,D1              *Need to shift 9 bits for the Register number
                LSR.L   D1,D0
                CLR.L   D1
                JSR     DR_DIRECT
                MOVE.W  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)            *Push comma to stack to split operands
                MOVE.L  #$00000038,D0       *Move mask for mode bits int D0
                AND.L   D6,D0
                LSR.L   #3,D0
                LEA     ADD_MODES,A1    ;Load addressing modes list for comparison
                CMP.W   (A1)+,D0
                BEQ     OR_ER_DR
                ADDA.L  #2,A1
                CMP.W   (A1)+,D0
                BEQ     OR_ER_AR_IND
                CMP.W   (A1)+,D0
                BEQ     OR_ER_AR_IND_POST
                CMP.W   (A1)+,D0
                BEQ     OR_ER_AR_IND_PRE
                BRA     OR_ER_ADD_IMM
OR_ER_DR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
                
OR_ER_AR_IND
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
                
OR_ER_AR_IND_POST
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS
                
OR_ER_AR_IND_PRE
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

OR_ER_ADD_IMM
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will let us know if we have Absolute addressing or Immediate  
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     OR_ER_ADDW
                CMP.W   (A1)+,D0
                BEQ     OR_ER_ADDL
                BRA     OR_ER_IMM
OR_ER_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS
   
OR_ER_ADDL
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS

OR_ER_IMM
                MOVE.L  #$000001C0,D0       *Mask for opmode which will give the size needed for the immediate value
                AND.L   D6,D0
                LEA     ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     OR_ER_IMMB
                CMP.W   (A1),D0
                BEQ     OR_ER_IMMW
                BRA     OR_ER_IMML
OR_ER_IMMB
                JSR     IMMB_OUT
                MOVE.L  A3,-(SP)
                RTS
                
OR_ER_IMMW
                JSR     IMMW_OUT
                MOVE.L  A3,-(SP)
                RTS

OR_ER_IMML
                JSR     IMML_OUT
                MOVE.L  A3,-(SP)
                RTS
EOR_EA
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000038,D0
                AND.L   D6,D0
                LSR.L   #3,D0
                CMP.W   #0,D0
                BEQ     EOR_DR_DEST
                BRA     EOR_ENTRY          *Branch to ADD_SUB_OR_REGEA as that process is the same for determining EA in EOR when Destination is not DR direct
EOR_DR_DEST     MOVE.L  #$00000007,D0
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0      *Mask for source Data register 
                AND.L   D6,D0
                LSR.L   D1,D0
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)
                RTS

LSD_ASD_EA                                 *Logical shift and Arithmetic shift should both be the same process
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000007,D0      *mask for register number, this subroutine is specific to Dn being the destination
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                MOVE.L  #5,D1
                MOVE.L  #$00000020,D0      *Mask for the i/r bit to determine if we are dealing with a register or immediate shift count
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                CMP.L   #0,D0
                BEQ     LSD_ASD_IMM
                MOVE.L  #9,D1              *For data register shift count
                MOVE.L  #$00000E00,D0
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)
                RTS
LSD_ASD_IMM                                *For immediate shift count
                MOVE.L  #0,D2
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                LEA     SUBQ_IMM_VALUES,A1    
                CMP.W   (A1)+,D0            
                BEQ     SHIFT_VALUE_8        *If the value is equal to 0, it's actually equal to 8. Dope
SHIFT_LOOP       CMP.W   #8,D2             *Loop finds the proper immediate value between 1 and 7
                BEQ     SHIFT_BAD            
                CMP.W   (A1)+,D0    
                BEQ     SHIFT_LOOP_DONE
                ADD.L   #1,D2
                BRA     SHIFT_LOOP
SHIFT_VALUE_8
                MOVE.L  #8,D0
                JSR     GET_ASCII_CHAR
                MOVE.W  #$2400,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.W  #$2023,-(SP)
                BRA     SHIFT_IMM_DONE
SHIFT_LOOP_DONE
                JSR     GET_ASCII_CHAR
                MOVE.W  #$2400,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.W  #$2023,-(SP)
                BRA     SHIFT_IMM_DONE
                
SHIFT_BAD                                    *JUST in case really. Prints out BADV
                MOVE.W #$4456,-(SP)
                MOVE.W #$4241,-(SP)
                BRA     SHIFT_IMM_DONE              
SHIFT_IMM_DONE
                MOVE.L  A3,-(SP)
                RTS

LSD_ASD_MEM_EA
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$00000038,D0   *Mask to determine addressing mode for memory shift
                AND.L   D6,D0
                LSR.L   #3,D0
                LEA     ADD_MODES,A1
                ADDA.L  #4,A1
                CMP.W   (A1)+,D0
                BEQ     LSD_ASD_AR_IND
                CMP.W   (A1)+,D0
                BEQ     LSD_ASD_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     LSD_ASD_AR_IND_PRE
                BRA     LSD_ASD_ADD

LSD_ASD_AR_IND
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
  
LSD_ASD_AR_IND_POST
                MOVE.L  #$00000007,D0    ;Mask for last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                CLR.L   D0
                MOVE.B  #$20,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Have to fill out the word containing '(' so add a space ' ('
                MOVE.L  A3,-(SP)
                RTS

LSD_ASD_AR_IND_PRE
                MOVE.L  #$00000007,D0    ;Mask for last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                CLR.L   D0
                MOVE.B  #$20,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Have to fill out the word containing '(' so add a space ' ('
                MOVE.L  A3,-(SP)
                RTS

LSD_ASD_ADD
                MOVE.L #$00000007,D0    ;Mask for last bits
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1),D0
                BEQ     LSD_ASD_ADDW
                BRA     LSD_ASD_ADDL        ;Only two options here for address size
LSD_ASD_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0       ;Move space into D0, then move $
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Move the final ' $' of the address string onto the stack
                MOVE.L  A3,-(SP)
                RTS

LSD_ASD_ADDL 
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0       ;Move space into D0, then move $
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)        ;Move the final ' $' of the address string onto the stack
                MOVE.L  A3,-(SP)
                RTS
                
MOVE_EA         
                MOVE.L  #0,A0           *Clear A0 just in case             
                MOVE.L  (SP)+,A3
                MOVE.L  #END_LINE,-(SP)
                MOVE.L  #$000001C0,D0   *Mask for the destination mode
                AND.L   D6,D0
                LSR.L   #6,D0
                LEA     ADD_MODES,A1
                CMP.W   (A1)+,D0
                BEQ     MOVE_D_DR
                ADDA.W  #2,A1
                CMP.W   (A1)+,D0
                BEQ     MOVE_D_AR_IND
                CMP.W   (A1)+,D0
                BEQ     MOVE_D_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     MOVE_D_AR_IND_PRE
                BRA     MOVE_D_AR_ADD
MOVE_D_DR
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0   *Mask for 3 bits to get register number
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                JSR     DR_DIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     MOVE_SOURCE
                
MOVE_D_AR_IND
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0   *Mask 3 bits to get register number
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                JSR     AR_INDIRECT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  #$20,D0
                MOVE.W  D0,-(SP)
                BRA     MOVE_SOURCE
    
MOVE_D_AR_IND_POST
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0   *Mask 3 bits for register number
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                JSR     AR_INDIRECT_POST
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     MOVE_SOURCE 

MOVE_D_AR_IND_PRE
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0   *Mask 3 bits for register number
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                JSR     AR_INDIRECT_POST
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     MOVE_SOURCE 

MOVE_D_AR_ADD
                MOVE.L  #$0000003F,D0   *Need to determine source mode in case it is immediate or another address
                AND.L   D6,D0
                CMP.L   #$00000038,D0
                BEQ     MOVE_W
                CMP.L   #$00000039,D0
                BEQ     MOVE_L
                CMP.L   #$0000003C,D0
                BNE     MOVE_SOURCE_NOT_IMM_ADD            
                MOVE.L  #$00003000,D0   *Need the size of the operation
                AND.L   D6,D0
                LEA     MOVE_SIZES,A1
                CMP.W   (A1),D0
                BEQ     MOVE_B
                CMP.W   (A1),D0
                BEQ     MOVE_W
                BRA     MOVE_L
MOVE_B
MOVE_W
                MOVE.L  A5,A4           *Save the address to the immediate or absolute address source           
                ADDA.W  #2,A5
                BRA     D_ADD_CONT
MOVE_L
                MOVE.L  A5,A4           *Save the address to the immediate or absolute address source
                ADDA.W  #4,A5
D_ADD_CONT      
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1),D0
                BEQ     MOVE_IMMS_ADDW         *Specifically for when source is an immediate value
                BRA     MOVE_IMMS_ADDL
MOVE_IMMS_ADDW
                JSR     ADDW_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A5,A0           *Store the address of the next instruction in A0 to be returned to later
                MOVE.L  #2,D1
                BRA     MOVE_SOURCE                
MOVE_IMMS_ADDL
                JSR     ADDL_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A5,A0           *Store the address of the next instruction in A0 to be returned to later
                MOVE.L  #2,D1
                BRA     MOVE_SOURCE
                
MOVE_SOURCE_NOT_IMM_ADD
                MOVE.L  #9,D1
                MOVE.L  #$00000E00,D0
                AND.L   D6,D0
                LSR.L   D1,D0
                CLR.L   D1
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1),D0
                BEQ     MOVE_D_ADDW         *Specifically for when source is an immediate value
                BRA     MOVE_D_ADDL
MOVE_D_ADDW
                JSR     ADDW_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     MOVE_SOURCE

                
MOVE_D_ADDL
                JSR     ADDL_OUT
                MOVE.B  #COMMA,D0
                LSL.L   #8,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                BRA     MOVE_SOURCE

MOVE_SOURCE
                CMP.L   #2,D1
                BNE     MOVE_EASY          *Easy means destination is a register, or that destination is an abs address, but source is not an abs address or immediate value
                MOVE.L  A4,A5              *MOve address of source back into A5
                MOVE.L  #$00000007,D0
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     MOVE_HS_ADDW
                CMP.W   (A1),D0
                BEQ     MOVE_HS_ADDL
                BRA     MOVE_HS_IMM
MOVE_HS_ADDW    
                JSR     ADDW_OUT
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A0,A5               *Move the address of the next instruction back into A5
                MOVE.L  A3,-(SP)
                RTS
MOVE_HS_ADDL
                JSR     ADDL_OUT
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A0,A5               *Move the address of the next instruction back into A5
                MOVE.L  A3,-(SP)
                RTS
MOVE_HS_IMM     
                MOVE.L  #$00003000,D0
                AND.L   D6,D0
                LEA     MOVE_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     MOVE_HS_IMMB
                CMP.W   (A1),D0
                BEQ     MOVE_HS_IMMW
                BRA     MOVE_HS_IMML
MOVE_HS_IMMB                    
                JSR     IMMB_OUT
                MOVE.L  A0,A5
                MOVE.L  A3,-(SP)
                RTS
MOVE_HS_IMMW
                JSR     IMMW_OUT
                MOVE.L  A0,A5
                MOVE.L  A3,-(SP)
                RTS
MOVE_HS_IMML
                JSR     IMML_OUT
                MOVE.L  A0,A5
                MOVE.L  A3,-(SP)
                RTS
MOVE_EASY                                   *The "easier" sources
                CLR.L   D1
                MOVE.L  #$00000038,D0       *mask for source mode
                AND.L   D6,D0
                LSR.L   #3,D0
                LEA     ADD_MODES,A1
                CMP.W   (A1)+,D0
                BEQ     MOVE_ES_DR
                CMP.W   (A1)+,D0
                BEQ     MOVE_ES_AR
                CMP.W   (A1)+,D0
                BEQ     MOVE_ES_AR_IND
                CMP.W   (A1)+,D0
                BEQ     MOVE_ES_AR_IND_POST
                CMP.W   (A1),D0
                BEQ     MOVE_ES_AR_IND_PRE
                BRA     MOVE_ES_ADD_IMM
MOVE_ES_DR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     DR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
          
MOVE_ES_AR
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_DIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck

MOVE_ES_AR_IND
                MOVE.L #$00000007,D0    ;Mask for the last 3 bits
                AND.L   D6,D0
                JSR     AR_INDIRECT
                MOVE.L  A3,-(SP)        ;Return address that goes back to opcheck onto the stack
                RTS                     ;Return to Opcheck
                     
MOVE_ES_AR_IND_POST
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_POST
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

MOVE_ES_AR_IND_PRE
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will give us the Address register number
                AND.L   D6,D0
                JSR     AR_INDIRECT_PRE
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)
                MOVE.L  A3, -(SP)
                RTS

MOVE_ES_ADD_IMM
                MOVE.L  #$00000007,D0        *Mask for last 3 bits which will let us know if we have Absolute addressing or Immediate  
                AND.L   D6,D0
                LEA     AR_ADD_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     MOVE_ES_ADDW
                CMP.W   (A1)+,D0
                BEQ     MOVE_ES_ADDL
                BRA     MOVE_ES_IMM
MOVE_ES_ADDW
                JSR     ADDW_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS
   
MOVE_ES_ADDL
                JSR     ADDL_OUT
                CLR.L   D0
                MOVE.W  #$2000,D0
                MOVE.B  D1,D0
                MOVE.W  D0,-(SP)            *Push ' $' onto the stack for output
                MOVE.L  A3,-(SP)
                RTS

MOVE_ES_IMM
                MOVE.L  #$00003000,D0       *Mask for opmode which will give the size needed for the immediate value
                AND.L   D6,D0
                LEA     MOVE_SIZES,A1
                CMP.W   (A1)+,D0
                BEQ     MOVE_ES_IMMB
                CMP.W   (A1),D0
                BEQ     MOVE_ES_IMMW
                BRA     MOVE_ES_IMML
MOVE_ES_IMMB
                JSR     IMMB_OUT
                MOVE.L  A3,-(SP)
                RTS
MOVE_ES_IMMW
                JSR     IMMW_OUT
                MOVE.L  A3,-(SP)
                RTS

MOVE_ES_IMML
                JSR     IMML_OUT
                MOVE.L  A3,-(SP)
                RTS

                
                    
DONE    SIMHALT             

* Put variables and constants here
HEX_to_ASCII    DC.B    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
SUBQ_IMM_VALUES DC.W    0,1,2,3,4,5,6,7
ADD_MODES       DC.W    0,1,2,3,4
AR_ADD_SIZES    DC.W    0,1
MOVE_SIZES      DC.W    $1000,$3000,$2000
ADD_SIZES       DC.W    $0000,$0040,$0080,$0100,$0140,$0180


       

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
             MOVE.L     (SP)+,A2        ;Save the return address
             MOVE.B     #$44,D2
             LSL.L      #8,D2           ;Move 'D' into the upper byte of lower order word
             JSR        GET_ASCII_CHAR  
             MOVE.B     D1,D2           ;After return from GET_ASCII_CHAR ascii value of digit in D0 should be in D1
             MOVE.W     D2,-(SP)        ;Move 'Dn' onto stack for output
             MOVE.L     A2,-(SP)
             CLR.L      D1
             RTS

*-----------------AR_DIRECT----------------
*Description: Given a register number in D0, generate the ascii representation of the Address register
*for the EA. (EX: Given 101 in D0, push 'D5' onto the stack
*Data Register D0: The value of D0 on entering the SUB is the n in An
*-----------------AR_DIRECT----------------
AR_DIRECT
             MOVE.L     (SP)+,A2        ;Save the return address
             MOVE.B     #$41,D2
             LSL.L      #8,D2           ;Move 'A' into the upper byte of lower order word
             JSR        GET_ASCII_CHAR  
             MOVE.B     D1,D2           ;After return from GET_ASCII_CHAR ascii value of digit in D0 should be in D1
             MOVE.W     D2,-(SP)        ;Move 'An' onto stack for output
             MOVE.L     A2,-(SP)
             CLR.L      D1
             RTS

*-----------------AR_INDIRECT----------------
*Description: Given a register number in D0, generate the ascii representation of the Indirect Address Register
*for EA. (EX: Given 101 in D0, push '(A5)' onto the stack)
*Data Register D0: The value of D0 on entering SUB is the n in (An)
*-----------------AR_INDIRECT----------------            
AR_INDIRECT    
            MOVE.L      (SP)+,A2        ;Save return address
            JSR         GET_ASCII_CHAR
            LSL.L       #8,D1           ;Move ascii digit over
            MOVE.B      #$29,D1
            MOVE.W      D1,-(SP)        ;Move 'n)' onto the stack
            MOVE.W      #$2841,-(SP)    ;Move '(A' onto the stack
            MOVE.L      A2,-(SP)
            CLR         D1
            RTS
           
*-----------------AR_INDIRECT_POST----------------
*Description: Given a register number in D0 generate the ascii representation of the Indirect Address Register with 
*post increment for EA. (EX: Given 101 in D0, push '(A5)+' onto the stack)
*Data Register D0: The value of D0 on entering SUB is the n in (An)+
*-----------------AR_INDIRECT_POST----------------
AR_INDIRECT_POST
            MOVE.L      (SP)+,A2        ;Save return address
            JSR         GET_ASCII_CHAR
            MOVE.W      #$292B,-(SP)    ;Push ')+' onto stack
            MOVE.B      #$41,D0
            LSL.L       #8,D0
            MOVE.B      D1,D0           
            MOVE.W      D0,-(SP)        ;Push 'An' onto the stack
            MOVE.W      #$28,D1         ;Push '(' onto D1 and wait for another character to fill the word
            MOVE.L      A2,-(SP)
            RTS

*-----------------AR_INDIRECT_PRE----------------
*Description: Does the same as INDIRECT_POST but with pre decrementing instead. (EX: Given 101 in D0, push '-(A5)' onto the stack)
*Data Register D0: The value of D0 on entering SUB is the n in -(An)
*-----------------AR_INDIRECT_PRE---------------- 
AR_INDIRECT_PRE
            MOVE.L      (SP)+,A2
            JSR         GET_ASCII_CHAR  
            LSL.L       #8,D1
            MOVE.B      #$29,D1         
            MOVE.W      D1,-(SP)        ;Push 'n)' onto the stack
            MOVE.W      #$2841,-(SP)    ;Push '(A' onto the stack
            MOVE.W      #$2D,D1         ;Push '-' into D1 (need something else to complete word)
            MOVE.L      A2,-(SP)
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
ADDW_DONE   MOVE.B      #$24,D1          *Push '$' into D1 (need something else to complete word)

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
ADDL_DONE   MOVE.B      #$24,D1          *Push '$' into D1 (need something else to complete word)

            MOVE.L      A2,-(SP)
            RTS         

*-----------------IMMB_OUT----------------
*Description: Based on the word of data at the current address, determine the immediate byte value in ascii hex 
**Data Register A5 the  current address
*Data Register A5 on done, is the next address or the address of the next instruction to be diassembled
*-----------------IMMB_OUT----------------
IMMB_OUT
            MOVE.L      (SP)+,A2
            MOVE.W      (A5)+,D4      ;Move the value into D4 and increment our address to the next address or the next instruction
            CLR         D0
            CLR         D1
            CLR         D2
IMMB_LOOP   MOVE.L      #$0000000F,D0   ;Mask for last 4 bits
            AND.L       D4,D0
            LSR.L       #4,D4         
            JSR         GET_ASCII_CHAR
            ROR.W       #8,D1
            CMP.L       #1,D2
            BEQ         IMMB_PUSH
            ADD.L       #1,D2
            BRA         IMMB_LOOP
IMMB_PUSH    MOVE.W      D1,-(SP)
            CLR         D1
            CLR         D2
            MOVE.W      #$2324,-(SP)    ;Move #$ to mark an immediate hex value
            MOVE.L      A2,-(SP)
            RTS  

*-----------------IMMW_OUT----------------
*Description: Based on the word of data at the current address, determine the immediate word value in ascii hex 
**Data Register A5 the  current address
*Data Register A5 on done, is the next address or the address of the next instruction to be diassembled
*-----------------IMMW_OUT----------------
IMMW_OUT
            MOVE.L      (SP)+,A2
            MOVE.W      (A5)+,D4      ;Move the value into D4 and increment our address to the next address or the next instruction
            CLR         D0
            CLR         D1
            CLR         D2
            CLR         D3
IMMW_LOOP   MOVE.L      #$0000000F,D0   ;Mask for last 4 bits
            AND.L       D4,D0
            LSR.L       #4,D4           ;This needs to loop through twice to get the word values
            JSR         GET_ASCII_CHAR
            ROR.W       #8,D1
            CMP.L       #1,D2
            BEQ         IMMW_PUSH
            ADD.L       #1,D2
            BRA         IMMW_LOOP
IMMW_PUSH   MOVE.W      D1,-(SP)
            CMP.L       #1,D3
            BEQ         IMMW_DONE
            ADD.L       #1,D3
            CLR         D1
            CLR         D2
            BRA         IMMW_LOOP
IMMW_DONE   MOVE.W      #$2324,-(SP)    ;Move #$ to mark an immediate hex value
            MOVE.L      A2,-(SP)
            RTS
            
*-----------------IMML_OUT----------------
*Description: Based on the long of data at the current address, determine the immediate long value in ascii hex 
**Data Register A5 the  current address
*Data Register A5 on done, is the next address or the address of the next instruction to be diassembled
*-----------------IMML_OUT----------------
IMML_OUT
            MOVE.L      (SP)+,A2
            MOVE.L      (A5)+,D4      ;Move the value into D4 and increment our address to the next address or the next instruction
            CLR         D0
            CLR         D1
            CLR         D2
            CLR         D3
IMML_LOOP   MOVE.L      #$0000000F,D0   ;Mask for last 4 bits
            AND.L       D4,D0
            LSR.L       #4,D4           ;This needs to loop through twice to get the word values
            JSR         GET_ASCII_CHAR
            ROR.W       #8,D1
            CMP.L       #1,D2
            BEQ         IMML_PUSH
            ADD.L       #1,D2
            BRA         IMML_LOOP
IMML_PUSH   MOVE.W      D1,-(SP)
            CMP.L       #3,D3
            BEQ         IMML_DONE
            ADD.L       #1,D3
            CLR         D1
            CLR         D2
            BRA         IMML_LOOP
IMML_DONE   MOVE.W      #$2324,-(SP)    ;Move #$ to mark an immediate hex value
            MOVE.L      A2,-(SP)
            RTS

    


TEST_SUB
            RTS
            
       END    START              
              
              









*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
