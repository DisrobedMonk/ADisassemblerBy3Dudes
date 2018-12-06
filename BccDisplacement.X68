*-----------------------------------------------------------
* Title      :
* Written by :
* Date       :
* Description:
*-----------------------------------------------------------
    ORG    $1000
START:                  ; first instruction of program


OFFSET_ADDR                 * Used to determine the displacement of Bcc's and its address it is branching too
    MOVE.L (SP)+,D4         * Store RTS somewhere
    MOVEA.W A5, A0          * Store Address of current address so we can add displacement to it (This is already at the address right after the instruction)
    
    CMP.B   #$00, D6        * Are we working with 16 bit displacement?
    BEQ     BCC_16BIT_DIS   * Evaluate the 16 bit discplacement 
    
    CMP.B   #$FF, D6        * Are we working with 32 bit displacement?
    BEQ     BCC_32BIT_DIS   * Evaluate 32 bit displacement
    
*------------------8-Bit Displacement-----------------------*
    MOVE.B  D6, D0          * Isolate the displacement bits
    CMPI.B  #$80, D0        * We can compare the displacement byte to $70 (0111 1111) 
    BLT     Neg8Bit         * If it is greater than $80, then that means it is >= $80 (1000 0000) and therefore means the sign bit is set and this is negative displacement
    ADDA.W  D0, A0          * Add displacement to address 
    BRA     DISPLACEMENT_FINISHED 
    

Neg8Bit
    NEG.B   D0
    SUBA.W  D0, A0
    BRA     DISPLACEMENT_FINISHED
    
BCC_16BIT_DIS
    MOVE.W  (A5)+, D0       * Grab the next word (16 bits) which contains the displacement value
    CMPI.W  #$8000, D0
    BLT     Neg16Bit
    ADDA.W  D0,A0
    BRA     DISPLACEMENT_FINISHED

Neg16Bit
    NEG.W   D0
    SUBA.W  D0, A0
    BRA     DISPLACEMENT_FINISHED
    
BCC_32BIT_DIS
    MOVE.L  (A5)+, D0       * Grab the next long (32 bits) which contains the displacement value
    CMPI.L  #$80000000, D0
    BLT     Neg32Bit
    ADDA.L  D0, A0
    BRA DISPLACEMENT_FINISHED
    
Neg32Bit
    NEG.L   D0
    SUBA.L  D0, A0
    BRA DISPLACEMENT_FINISHED
    
DISPLACEMENT_FINISHED
     MOVE.B  #$0A,-(SP)     * New Line
     MOVE.L  A0, -(SP)      * Add displacement address onto the stack
     MOVE.L  D4, -(SP)      * Add RTS address back to stack
     RTS

EXIT    SIMHALT             ; halt simulator

* Put variables and constants here

    END    START        ; last line of source

*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
