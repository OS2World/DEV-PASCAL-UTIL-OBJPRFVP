;******************************************************
;                   OPBM.ASM 1.30
;              String handling routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

        PUBLIC  BMMakeTable, BMSearch, BMSearchUC

        EXTRN   UpCasePrim : FAR

Upcase  MACRO                           ;UpCase character in AL
;        PUSH   BX                      ;!!.02 UpCasePrim saves BX now
        CALL   UpCasePrim
;        POP    BX                      ;!!.02 UpCasePrim saves BX now
        ENDM

;****************************************************** BMMakeTable

;  procedure BMMakeTable(MatchString : string; var BT : BTable);
;    Build Boyer-Moore link table
;    BTable is array[0..255] of byte;

MString EQU    DWORD PTR SS:[BX+8]
BTable  EQU    DWORD PTR SS:[BX+4]

BMMakeTable   PROC   FAR

        StackFrame
        MOV    DX,DS                    ;Save DS in DX
        CLD                             ;Go forward

        LDS    SI,MString               ;DS:SI => MatchString
        LES    DI,BTable                ;ES:DI => BY
        MOV    BX,DI                    ;Save DI in BX
        LODSB                           ;AL = length(MatchString)
        MOV    AH,AL                    ;Copy it to AH
        MOV    CX,128                   ;Number of words in BT
        REP    STOSW                    ;Fill BT with length(MatchString)
        CMP    AL,1                     ;Is length(MatchString) <= 1?
        JBE    MTDONE                   ;Yes, we're done

        MOV    DI,BX                    ;Restore base of table from BX
        MOV    BH,CH                    ;BH = 0
        MOV    CL,AL                    ;CX = length(MatchString)
        DEC    CX                       ;CX = length(MatchString)-1

MTnext: LODSB                           ;AL = MatchString[i]
        MOV    BL,AL                    ;BL = MatchString[i]
        MOV    ES:[BX+DI],CL            ;BTable[char] = length(MatchString)-i
        LOOP   MTnext                   ;Repeat for all characters in MatchString

MTDone: MOV    DS,DX                    ;Restore DS from DX
        RET    8

BMMakeTable   ENDP

;****************************************************** BMSearch

;  function BMSearch(var Buffer; BufLength : Word; BT : BTable; MatchString : string) : Word;
;    Search Buffer for MatchString
;    Return FFFF for failure
;    Else return number of bytes searched to find MatchString

MString EQU    DWORD PTR [BP+6]
BTable  EQU    DWORD PTR [BP+10]
BufSize EQU    WORD PTR [BP+14]
Buffer  EQU    DWORD PTR [BP+16]
BufOfs  EQU    WORD PTR [BP+16]

BMSearch   PROC   FAR

        StackFrameBP
        PUSH   DS                       ;Will wipe out DS
        PUSH   BP                       ;Will use BP for temp storage later

        MOV    CX,BufSize               ;CX = Buffer size
        LES    DI,Buffer                ;ES:DI => Buffer
        LDS    BX,BTable                ;DS:BX => BTable
        MOV    AX,DS                    ;Keep BTable segment in AX a moment
        LDS    SI,MString               ;DS:SI => MatchString
        MOV    BP,AX                    ;Keep BTable segment in BP

        XOR    AX,AX                    ;AX = 0
        MOV    DX,AX                    ;DX = 0
        MOV    DL,[SI]                  ;DL = length(MatchString)
        CMP    DL,1                     ;Check for trivial cases
        JA     BMSinit                  ;Do Boyer-Moore if longer than one char
        JB     BMSnotFound              ;Fail for empty string

        MOV    AL,[SI+1]                ;AL = one and only char to find
        MOV    BX,DI                    ;Save offset of Buffer
        CLD                             ;Forward
        REPNE  SCASB                    ;Scan Buffer for AL
        JNE    BMSnotFound              ;Char wasn't found
        MOV    AX,DI                    ;AX holds offset where char was found
        DEC    AX                       ;Back up one
        SUB    AX,BX                    ;Subtract base offset of Buffer
        JMP    BMSdone                  ;We're done

BMSinit:
        DEC    DL                       ;DX = length(MatchString)-1
        ADD    SI,DX                    ;DS:SI => MatchString[length(MatchString)-1]
        ADD    CX,DI                    ;CX = offset of last char in buffer
        ADD    DI,DX                    ;ES:DI => first position to search
        MOV    DH,[SI+1]                ;DH = MatchString[length(MatchString)]
        STD                             ;Go backwards
        JMP    SHORT BMScomp            ;Skip link table first time

BMSnext:
        PUSH   DS                       ;Save DS a moment
        MOV    DS,BP                    ;Get segment of link table
        XLAT                            ;Get size of link at DS:[BX+AL]
        POP    DS                       ;Restore DS
        ADD    DI,AX                    ;Compute next place to search

BMScomp:
        JC     BMSnotFound              ;Done if overflowed 64K !!.03
        CMP    DI,CX                    ;At end of buffer?
        JAE    BMSnotFound              ;Done if so
        MOV    AL,ES:[DI]               ;AL = next char to try
        CMP    DH,AL                    ;Does it match the end of MatchString?
        JNE    BMSnext                  ;If not same, go back and try again

        PUSH   CX                       ;Save end of buffer position
        DEC    DI                       ;Start comparing one character before
        MOV    CL,DL                    ;Compare length(MatchString)-1 characters
        MOV    CH,AH                    ;CH = 0
        REPE   CMPSB                    ;Compare backwards while matched
        JE     BMSfound                 ;Matched!

        MOV    AL,DL                    ;Restore SI,DI,AL
        SUB    AL,CL
        ADD    SI,AX
        ADD    DI,AX
        INC    DI
        MOV    AL,DH                    ;Put matched char back in AL
        POP    CX                       ;Restore end of buffer
        JMP    SHORT BMSnext            ;Try again

BMSfound:                               ;DI points to start of match
        INC    SP                       ;End of buffer off stack
        INC    SP
        POP    BP                       ;Get frame pointer back
        SUB    DI,BufOfs                ;Subtract buffer start address
        MOV    AX,DI
        INC    AX                       ;Return 0 if found in first byte
        JMP    SHORT BMSDone2           ;We're done

BMSnotFound:
        MOV    AX,0FFFFh                ;Result = FFFF
BMSDone:                                ;Result returned in AX
        POP    BP
BMSDone2:
        CLD
        POP    DS
        Exit_Code 14                    ;!!.13

BMSearch   ENDP

;***************************************************** BMSearchUC

;  function BMSearchUC(var Buffer;
;                      BufLength : Word;
;                      BT : BTable;
;                      MatchString : string) : Word;
;    Case-insensitive search of Buffer for MatchString
;    Return FFFF for failure
;    Else return number of bytes searched to find MatchString
;    Assumes MatchString is already raised to uppercase

BMSearchUC   PROC   FAR

        StackFrameBP
        PUSH   DS                       ;Will wipe out DS
        PUSH   BP                       ;Will use BP for temp storage later

        MOV    CX,BufSize               ;CX = Buffer size
        LES    DI,Buffer                ;ES:DI => Buffer
        LDS    BX,BTable                ;DS:BX => BTable
        MOV    AX,DS                    ;Keep BTable segment in AX a moment
        LDS    SI,MString               ;DS:SI => MatchString
        MOV    BP,AX                    ;Keep BTable segment in BP

        XOR    AX,AX                    ;AX = 0
        MOV    DX,AX                    ;DX = 0
        MOV    DL,[SI]                  ;DL = length(MatchString)
        OR     DL,DL                    ;Check for trivial case
        JZ     BMSUnotFound             ;Fail for empty string

BMSUinit:
        DEC    DL                       ;DX = length(MatchString)-1
        ADD    SI,DX                    ;DS:SI => MatchString[length(MatchString)-1]
        ADD    CX,DI                    ;CX = offset of last char in buffer
        ADD    DI,DX                    ;ES:DI => first position to search
        MOV    DH,[SI+1]                ;DH = MatchString[length(MatchString)]
        STD                             ;Go backwards
        JMP    SHORT BMSUcomp           ;Skip link table first time

BMSUnext:
        PUSH   DS                       ;Save DS a moment
        MOV    DS,BP                    ;Get segment of link table
        XLAT                            ;Get size of link at DS:[BX+AL]
        POP    DS                       ;Restore DS
        ADD    DI,AX                    ;Compute next place to search

BMSUcomp:
        JC     BMSUnotFound             ;Done if overflowed 64K !!.03
        CMP    DI,CX                    ;At end of buffer?
        JAE    BMSUnotFound             ;Done if so
        MOV    AL,ES:[DI]               ;AL = next char to try
        UpCase                          ;Raise it to uppercase
        CMP    DH,AL                    ;Does it match the end of MatchString?
        JNE    BMSUnext                 ;If not same, go back and try again

        PUSH   CX                       ;Save end of buffer position
        DEC    DI                       ;Start comparing one character before
        MOV    CL,DL                    ;Compare length(MatchString)-1 characters
        MOV    CH,AH                    ;CH = 0
        JCXZ   BMSUfound                ;Completely matched if CX = 0

BMSUcomp2:
        LODSB                           ;Next match character in AL
        MOV    AH,ES:[DI]               ;Next buffer character in AH
        DEC    DI                       ;Decrement buffer index
        XCHG   AL,AH                    ;Uppercase it
        UpCase
        CMP    AH,AL                    ;A match?
        LOOPE  BMSUcomp2                ;Loop while AH=AL and CX<>0
        JE     BMSUfound                ;Matched!

        XOR    AH,AH                    ;Restore SI,DI,AX
        MOV    AL,DL
        SUB    AL,CL
        ADD    SI,AX
        ADD    DI,AX
        INC    DI
        MOV    AL,DH                    ;Put matched char back in AL
        POP    CX                       ;Restore end of buffer
        JMP    SHORT BMSUnext           ;Try again

BMSUfound:                              ;DI points to start of match
        INC    SP                       ;End of buffer off stack
        INC    SP
        POP    BP                       ;Get frame pointer back
        SUB    DI,BufOfs                ;Subtract buffer start address
        MOV    AX,DI
        INC    AX                       ;Return 0 if found in first byte
        JMP    SHORT BMSUDone2          ;We're done

BMSUnotFound:
        MOV    AX,0FFFFh                ;Result = FFFF
BMSUDone:                               ;Result returned in AX
        POP    BP
BMSUDone2:
        CLD
        POP    DS
        Exit_Code 14                    ;!!.13

BMSearchUC   ENDP

CODE    ENDS

        END
