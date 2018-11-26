*-----------------------------------------------------------
* Title      :Diassembler OpCode File
* Written by :Torren,Doug,Jeremiah
* Date       :11/25/2018
* Description:
*-----------------------------------------------------------
END_LINE   EQU    $0A00  
 
START      ORG    $1500
*-------------Op Code Check Subroutine-------------
OP_CHECK
           MOVE.L (SP)+,D5

*-------------No Operand Section-------------
           CMP.L   #$00004E71,D6        ;Check if opcode is NOP
           BEQ     NOP_FOUND
   
           CMP.L   #$00004E75,D6        ;Check if opcode is RTS
           BEQ
            




RTS_FOUND  
           MOVE.W  #END_LINE,-(SP)
           MOVE.W  #$530D,-(SP)
           MOVE.W  #$5452,-(SP)

NOP_FOUND
           MOVE.W  #END_LINE,-(SP)
           MOVE.W  #$500D,-(SP)
           MOVE.W  #$4E4F,-(SP)
           BRA     END_OP_SUB
    
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

*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
