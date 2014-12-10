;******************************************************
;                   OPTAB.ASM 1.30
;              String handling routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

        PUBLIC  Entab, Detab

;****************************************************** Entab

;  function Entab(S : string; TabSize : Byte) : string;
;    {-Convert blanks in a string to tabs on spacing TabSize}

RS             EQU DWORD PTR [BP+12]    ;Result string
TS             EQU DWORD PTR [BP+8]     ;Input string
TabSize        EQU BYTE PTR  [BP+6]     ;TabSize

Entab   PROC   FAR

        StackFrameBP
        PUSH   DS
        CLD

        XOR    BX,BX                    ;Initial SpaceCount = 0
        XOR    CX,CX                    ;Default input length = 0
        XOR    DX,DX                    ;Default output length = 0 in DL
        MOV    DH,TabSize               ;DH will hold TabSize

        LDS    SI,TS                    ;DS:SI => input string
        LES    DI,RS                    ;ES:DI => output string
        LODSB                           ;Get input length
        OR     DH,DH                    ;TabSize = 0?
        JNZ    ETDefLength
        XOR    AL,AL                    ;Return zero length string if TabSize = 0
ETDefLength:
        MOV    CL,AL                    ;Store length in counter
        STOSB                           ;Store default output length
        JCXZ   ETdone                   ;Done if empty input string

        INC    CH                       ;Current input position=1

ETNext: OR     BL,BL                    ;Compare SpaceCount to 0
        JE     ETNoTab                  ;If SpaceCount=0 then no tab insert here
        MOV    AL,CH                    ;Ipos to AL
        XOR    AH,AH                    ;AX has Ipos
        DIV    DH                       ;Ipos DIV TabSize
        CMP    AH,1                     ;Ipos MOD TabSize = 1 ?
        JNE    ETNoTab                  ;If not, no tab insert here
        SUB    DL,BL                    ;Reduce Olen by SpaceCount
        SUB    DI,BX                    ;Remove unused characters from output string
        MOV    AL,09
        STOSB                           ;Store a tab
        INC    DL                       ;Add one to output length
        XOR    BL,BL                    ;Reset SpaceCount
ETNoTab:
        LODSB                           ;Get next input character
        CMP    CL, CH                   ;End of string?               !!.11
        JE     ETStore                  ;Yes, store character anyway  !!.11
        INC    BL                       ;Increment SpaceCount
        CMP    AL,32                    ;Is character a space?
        JZ     ETstore                  ;Yes, store it for now
        XOR    BL,BL                    ;Reset SpaceCount
        CMP    AL,39                    ;Is it a quote?
        JZ     ETquotes                 ;Yep, enter quote loop
        CMP    AL,34                    ;Is it a doublequote?
        JNZ    ETstore                  ;Nope, store it

ETquotes:
        MOV    AH,AL                    ;Save quote start
ETnextQ:
        STOSB                           ;Store quoted character
        INC    DL                       ;Increment output length
        LODSB                           ;Get next character
        INC    CH                       ;Increment Ipos
        CMP    CH,CL                    ;At end of line?
        JAE    ETstore                  ;If so, exit quote loop
        CMP    AL,AH                    ;Matching end quote?
        JNZ    ETnextQ                  ;Nope, stay in quote loop
        CMP    AL,39                    ;Single quote?
        JZ     ETstore                  ;Exit quote loop
        CMP    BYTE PTR [SI-2],'\'      ;Previous character an escape?
        JZ     ETnextQ                  ;Stay in if so

ETstore:
        STOSB                           ;Store last character
        INC    DL                       ;Increment output length
        INC    CH                       ;Increment input position
        JZ     ETstoreLen               ;Exit if past 255
        CMP    CH,CL                    ;Compare Ipos to Ilen
        JBE    ETNext                   ;Repeat while characters left

ETstoreLen:
        LES    DI,RS                    ;ES:DI => output string
        MOV    ES:[DI],DL               ;Store final length

ETdone:
        POP    DS
        Exit_Code 6                     ;!!.13

Entab   ENDP

;****************************************************** Detab

;  function Detab(S : string; TabSize : Byte) : string;
;    {-Expand tabs in a string to blanks on spacing TabSize}

Detab   PROC   FAR

        StackFrameBP
        PUSH   DS
        CLD

        XOR    CX,CX                    ;Default input length = 0
        XOR    DX,DX                    ;Default output length = 0 in DL
        MOV    DH,TabSize               ;DH will hold TabSize

        LDS    SI,TS                    ;DS:SI => input string
        LES    DI,RS                    ;ES:DI => output string
        LODSB                           ;Get input length
        OR     DH,DH                    ;TabSize = 0?
        JNZ    DTDefLength
        XOR    AL,AL                    ;Return zero length string if TabSize = 0
DTDefLength:
        MOV    CL,AL                    ;Store length in counter
        STOSB                           ;Store default output length
        JCXZ   DTdone                   ;Done if empty input string

        MOV    AH,09                    ;Store tab in AH
        MOV    BL,255                   ;Maximum length of output

DTNext: LODSB                           ;Next input character
        CMP    AL,AH                    ;Is it a tab?
        JE     DTTab                    ;Yes, compute next tab stop
        STOSB                           ;No, store to output
        INC    DL                       ;Increment output length
        CMP    DL,BL                    ;255 characters max
        LOOPNE DTNext                   ;Next character while Olen <= 255
        JMP SHORT DTStoreLen            ;Loop termination

DTTab:  MOV    BH,CL                    ;Save input counter
        MOV    AL,DL                    ;Current output length in AL
        XOR    AH,AH                    ;Clear top byte
        DIV    DH                       ;OLen DIV TabSize in AL
        INC    AL                       ;Round up to next tab position
        MUL    DH                       ;Next tab position in AX
        OR     AH,AH                    ;AX > 255?
        JNE    DTStoreLen               ;Can't store it
        SUB    AL,DL                    ;Count of blanks to insert
        ADD    DL,AL                    ;New output length in DL
        MOV    CL,AL                    ;Loop counter for blanks
        MOV    AX,0920h                 ;Tab in AH, Blank in AL
        REP    STOSB                    ;Store blanks
        MOV    CL,BH                    ;Restore input position
        LOOP   DTNext                   ;Back for next input

DTStoreLen:
        LES    DI,RS                    ;ES:DI => output string
        MOV    ES:[DI],DL               ;Store final length

DTDone: POP    DS
        Exit_Code 6                     ;!!.13

Detab   ENDP


CODE    ENDS

        END
