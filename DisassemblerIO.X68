*-----------------------------------------------------------
* Title      : Disassembler IO File
* Written by : Torren,Doug,Jeremiah
* Date       : 11/23/2018
* Description:
*-----------------------------------------------------------
START   EQU     $6000
END     EQU     $6500
STACK   EQU     $10000
PRINTLI EQU     #15        ;Number of read instructions before a print

        ORG    $400
START:                  ; first instruction of program
        LEA     #start,A1
        LEA     #end,A2
        MOVE.L  #start,D0
        ANDI.L  #1,D0
        CMP.L   #1,D0
        BEQ     START
        
        



* Put program code here

    SIMHALT             ; halt simulator

* Put variables and constants here

    END    START        ; last line of source

*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
