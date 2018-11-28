*-----------------------------------------------------------
* Title      :
* Written by :
* Date       :
* Description:
*-----------------------------------------------------------
    ORG    $1000
START:                  ; first instruction of program
    
    MOVE.L  #$FFFFFFFF,A5           * our test address that should print out
    MOVE.L  A5,D5                   * a Dr we store it in
    LEA     HEX_to_ASCII,A4         * our 'list' of ASCII char in their proper order of 0-F at indexes of 0-15
    
    CLR.W   D4
PRNT_LOOP
    CMP     #4,D3                   * check if we've converted a total of 8 (this can change for Word or Byte printing - 2 and 1 respectively)
    BEQ     DONE                
    ROL.L   #8,D5                   * rotate the Dr so the starting values (12 in example) are at the end of the register (change the ROL to Word or Byte for that printing)
    MOVE.B  D5,D4                   * Put the byte into a Dr for our offset
    
    LSR.L   #4,D4                   * Shift the offset to the first digit only (1 instead of 12)
    
    
    * THIS IS WHERE IT'S BREAKING
    MOVE.B  (0,A4,D4.W),D1          * Move the char's word at the position given by offset into D1
    
    MOVE.L  #6,D0                   * trap to print the value in D1 as a char, print it
    TRAP    #15
    
    MOVE.B  D5,D4                   * Reset our offset to the original offset byte (12 for the first time through)
    AND.B   #%00001111,D4            * Mask out the first hex value (makes it only 2)
    
    
    * THUS, THIS IS ALSO WHERE IT WILL BREAK
    MOVE.B  (0,A4,D4.W),D1          * Move the char's word at the position given by offset into D1
    TRAP    #15
    
    ADDI.B  #1,D3                   * Increment counter
    BRA     PRNT_LOOP
    
* Put program code here


DONE
* Put variables and constants here
HEX_to_ASCII    DC.B    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
    END    START        ; last line of source
*~Font name~Courier New~
*~Font size~12~
*~Tab type~1~
*~Tab size~4~
