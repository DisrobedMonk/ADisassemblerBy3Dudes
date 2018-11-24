*-----------------------------------------------------------
* Title      : Disassembler IO File
* Written by : Torren,Doug,Jeremiah
* Date       : 11/23/2018
* Description:
*-----------------------------------------------------------
START           EQU     $1000
CR              EQU     $0D         * ASCII code for Carriage Return
LF              EQU     $0A         * ASCII code for Line Feed
STRT_ADDR       EQU     $6000       * Hard coded start address for now
END_ADDR        EQU     $6500       * Hard coded end address for now
STACK           EQU     $10000      * Defined location for the stack
LINE_CNT        EQU     $F          * Number of lines to print before pausing


*   D0:  
*   D1:  
*   D2:  
*   D3:  
*   D4:  
*   D5:  
*   D6:  Current OP code read from latest Current Address position
*   D7:  The current line being printed (I/O line counter)

*   A0:  
*   A1:  
*   A2:  
*   A3:  
*   A4:  
*   A5:  Current Address;   DO NOT ALTER (Initialized via start up requests)
*   A6:  End Address;       DO NOT ALTER (Initialized via start up requests)
*   A7:  STACK POINTER;     DO NOT ALTER



        ORG    START
        
*---!! Can use this for hard coded star/end !! ----
*        LEA     <starting address>,A5
*        LEA     <ending address>,A6
*        BRA     BEGIN



*-----------------I/O code----------------

*---Positioning the stack---
        LEA     STACK,SP
        
*-------Requesting the Starting Address---
STRT_ADDR_RQST
        LEA     STRT_RQST,A1
        MOVE.L  #14,D0
        TRAP    #15
        
        MOVE.L  #0,D1
        MOVE.L  #0,D2
        MOVE.L  #0,D3
        MOVE.L  #0,D4
        
        MOVE.L  #2,D0
        MOVE.L  #0,A1
        TRAP    #15
        
        CMP     #8,D1
        BGT     STRT_ADDR_RQST
     
*---Converting input to hex address------           
CNVRT_STRT
        CMP.L   D2,D1           * Check if all entered ASCII converted
        BEQ     SAVE_STRT       * Move on to MSB finding if so
            
        LSL.L   #4,D4           * Move D5 left to make room for next entry
        MOVE.B  (A1)+,D3        * Put first char into D5 for conversion
            
            
        CMP.B   #$39,D3         * Check if char is '9' or less in ASCII
        BLE     ITS_A_NUM_STRT  * Branch to number intake method
                        
        CMP.B   #$41,D3         * Check if char is 'A' or higher in ASCII
        BGE     ITS_A_CHR_STRT  * Branch to char intake mehtod

        BRA     STRT_ADDR_RQST

ITS_A_NUM_STRT
        CMP.B   #$30,D3
        BLT     STRT_ADDR_RQST
        
        SUB.L   #$30,D3
        
        ADD.L   D3,D4
        
        ADD.B   #1,D2
        
        BRA     CNVRT_STRT
        
ItS_A_CHR_STRT
        CMP.B   #$46,D3
        BGT     STRT_ADDR_RQST
        
        SUB.L   #$37,D3
        
        ADD.L   D3,D4
        
        ADD.B   #1,D2
        
        BRA     CNVRT_STRT
        
        
SAVE_STRT
        MOVE.L  D4,A5     
   
   
   
*-------Requesting the End Address-------
END_ADDR_RQST
        LEA     END_RQST,A1
        MOVE.L  #14,D0
        TRAP    #15
        
        MOVE.L  #0,D1
        MOVE.L  #0,D2
        MOVE.L  #0,D3
        MOVE.L  #0,D4
        
        MOVE.L  #2,D0
        MOVE.L  #0,A1
        TRAP    #15
        
        CMP     #8,D1
        BGT     END_ADDR_RQST
        
        BRA     CNVRT_END
        
*---Converting input to hex address------
CNVRT_END
        CMP.L   D2,D1           * Check if all entered ASCII converted
        BEQ     SAVE_END        * Move on to MSB finding if so
            
        LSL.L   #4,D4           * Move D5 left to make room for next entry
        MOVE.B  (A1)+,D3        * Put first char into D5 for conversion
            
            
        CMP.B   #$39,D3         * Check if char is '9' or less in ASCII
        BLE     ITS_A_NUM_END   * Branch to number intake method
                        
        CMP.B   #$41,D3         * Check if char is 'A' or higher in ASCII
        BGE     ITS_A_CHR_END   * Branch to char intake mehtod

        BRA     END_ADDR_RQST

ITS_A_NUM_END
        CMP.B   #$30,D3
        BLT     END_ADDR_RQST
        
        SUB.L   #$30,D3
        
        ADD.L   D3,D4
        
        ADD.B   #1,D2
        
        BRA     CNVRT_END
        
ItS_A_CHR_END
        CMP.B   #$46,D3
        BGT     END_ADDR_RQST
        
        SUB.L   #$37,D3
        
        ADD.L   D3,D4
        
        ADD.B   #1,D2
        
        BRA     CNVRT_END
        
        
SAVE_END
        MOVE.L  D4,A6
        
        
*-------Moving on to decoding; start with instructions-------        
BEGIN
        LEA     INSTRCTNS,A1
        MOVE.B  #14,D0
        TRAP    #15

*---Wait for user to continue---
WAIT
        MOVE.L  #0,D7
        * IN HERE, WE NEED A SIMPLE PAUSE TRAP WAITING FOR THEM TO PRESS ENTER                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*---Check if done or perform next line's disassembly---------
SEE_IF_DONE
        CMPA    A5,A6
        BGT     DONE
        
        MOVE.W  (A5)+,D5
        
        JSR     OP_CHECK
        
        * IN HERE, WE NEED CODE THAT WILL PRINT OUT THE STRING THAT WILL BE SAVED IN THE STACK                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        ADD.L   D7,#15
        BEQ     WAIT
        
        BRA     SEE_IF_DONE
        

* Put variables and constants here
STRT_RQST   DC.B    'Please provide a valid starting hex address for the code to be disassembled.',CR,LF
            DC.B    'Invalid entry will only lead to the question being repeated.',CR,LF
            DC.B    'Only numbers 0-9 and capitals A-F are accepted.',CR,LF
            DC.B    'Starting address must also be even.',CR,LF
                                                                          * There's a chance we also want to iron out and check to ensure it's not our own code?
            DC.B    'What will the starting address be?',CR,LF,0 
            
END_RQST    DC.B    'Thank you for the starting address.',CR,LF
            DC.B    'Please now provide an end address.',CR,LF
            DC.B    'Invalid entry will only lead to the question being repeated.',CR,LF
            DC.B    'Only numbers 0-9 and capitals A-F are accepted.',CR,LF
                                                                          * There's a chance we also want to iron out and check to ensure it's not our own code?
            DC.B    'What will the ending address be?',CR,LF,0
            
INSTRCTNS   DC.B    'We are ready to begin reading the code between the given starting and end addresses.',CR,LF
            DC.B    'While running, the program will pause every fifteen lines.',CR,LF
            DC.B    'To continue, press the "Enter" key.',CR,LF
            DC.B    'Do so now to begin.',CR,LF,0



    END    START        ; last line of source
