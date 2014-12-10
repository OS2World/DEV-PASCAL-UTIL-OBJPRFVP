;******************************************************
;                   OPTAB.ASM 1.30
;              String handling routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************
;       Modification for Virtual Pascal for OS/2
;           Copyright (c) fPrint UK Ltd 1995
;******************************************************

        INCLUDE VPCOMMON.ASM

;****************************************************** Code

CODE32  SEGMENT DWORD PUBLIC USE32 'CODE'

        ASSUME  CS:CODE32

        PUBLIC  Entab, Detab

;****************************************************** Entab

;  function Entab(S : string; TabSize : Byte) : string;
;    {-Convert blanks in a string to tabs on spacing TabSize}

RS             EQU DWORD PTR [EBP+10h]    ;Result string
TS             EQU DWORD PTR [EBP+0Ch]    ;Input string
TabSize        EQU BYTE PTR  [EBP+08h]    ;TabSize

Entab   PROC

        VPStackFrameBP
        PUSH   EBX
        PUSH   ESI
        PUSH   EDI
        CLD

        XOR    EBX,EBX                  ;Initial SpaceCount = 0
        XOR    ECX,ECX                  ;Default input length = 0
        XOR    EDX,EDX                  ;Default output length = 0 in DL
        MOV    DH,TabSize               ;DH will hold TabSize

        MOV    ESI,TS                   ;DS:SI => input string
        MOV    EDI,RS                   ;ES:DI => output string
        LODSB                           ;Get input length
        OR     DH,DH                    ;TabSize = 0?
        JNZ    SHORT ETDefLength
        XOR    AL,AL                    ;Return zero length string if TabSize = 0
ETDefLength:
        MOV    CL,AL                    ;Store length in counter
        STOSB                           ;Store default output length
        JCXZ   ETdone                   ;Done if empty input string

        INC    CH                       ;Current input position=1

ETNext: OR     BL,BL                    ;Compare SpaceCount to 0
        JE     SHORT ETNoTab            ;If SpaceCount=0 then no tab insert here
        MOV    AL,CH                    ;Ipos to AL
        MOVZX  EAX,AL                   ;AX has Ipos
        DIV    DH                       ;Ipos DIV TabSize
        CMP    AH,1                     ;Ipos MOD TabSize = 1 ?
        JNE    SHORT ETNoTab            ;If not, no tab insert here
        SUB    DL,BL                    ;Reduce Olen by SpaceCount
        SUB    EDI,EBX                  ;Remove unused characters from output string
        MOV    AL,09
        STOSB                           ;Store a tab
        INC    DL                       ;Add one to output length
        XOR    BL,BL                    ;Reset SpaceCount
ETNoTab:
        LODSB                           ;Get next input character
        CMP    CL, CH                   ;End of string?               !!.11
        JE     SHORT ETStore            ;Yes, store character anyway  !!.11
        INC    BL                       ;Increment SpaceCount
        CMP    AL,32                    ;Is character a space?
        JZ     SHORT ETstore            ;Yes, store it for now
        XOR    BL,BL                    ;Reset SpaceCount
        CMP    AL,39                    ;Is it a quote?
        JZ     SHORT ETquotes           ;Yep, enter quote loop
        CMP    AL,34                    ;Is it a doublequote?
        JNZ    SHORT ETstore            ;Nope, store it

ETquotes:
        MOV    AH,AL                    ;Save quote start
ETnextQ:
        STOSB                           ;Store quoted character
        INC    DL                       ;Increment output length
        LODSB                           ;Get next character
        INC    CH                       ;Increment Ipos
        CMP    CH,CL                    ;At end of line?
        JAE    SHORT ETstore            ;If so, exit quote loop
        CMP    AL,AH                    ;Matching end quote?
        JNZ    SHORT ETnextQ            ;Nope, stay in quote loop
        CMP    AL,39                    ;Single quote?
        JZ     SHORT ETstore            ;Exit quote loop
        CMP    BYTE PTR [ESI-2],'\'     ;Previous character an escape?
        JZ     SHORT ETnextQ            ;Stay in if so

ETstore:
        STOSB                           ;Store last character
        INC    DL                       ;Increment output length
        INC    CH                       ;Increment input position
        JZ     ETstoreLen         ;Exit if past 255
        CMP    CH,CL                    ;Compare Ipos to Ilen
        JBE    ETNext                   ;Repeat while characters left

ETstoreLen:
        MOV    EDI,RS                   ;ES:DI => output string
        MOV    [EDI],DL                 ;Store final length

ETdone:
        POP    EDI
        POP    ESI
        POP    EBX
        VPExit_Code 08h

Entab   ENDP

;****************************************************** Detab

;  function Detab(S : string; TabSize : Byte) : string;
;    {-Expand tabs in a string to blanks on spacing TabSize}

Detab   PROC

        VPStackFrameBP
        PUSH   EBX
        PUSH   ESI
        PUSH   EDI
        CLD

        XOR    ECX,ECX                  ;Default input length = 0
        XOR    EDX,EDX                  ;Default output length = 0 in DL
        MOV    DH,TabSize               ;DH will hold TabSize

        MOV    ESI,TS                   ;DS:SI => input string
        MOV    EDI,RS                   ;ES:DI => output string
        LODSB                           ;Get input length
        OR     DH,DH                    ;TabSize = 0?
        JNZ    SHORT DTDefLength
        XOR    AL,AL                    ;Return zero length string if TabSize = 0
DTDefLength:
        MOV    CL,AL                    ;Store length in counter
        STOSB                           ;Store default output length
        JCXZ   DTdone                   ;Done if empty input string

        MOV    AH,09                    ;Store tab in AH
        MOV    BL,255                   ;Maximum length of output

DTNext: LODSB                           ;Next input character
        CMP    AL,AH                    ;Is it a tab?
        JE     SHORT DTTab                    ;Yes, compute next tab stop
        STOSB                           ;No, store to output
        INC    DL                       ;Increment output length
        CMP    DL,BL                    ;255 characters max
        LOOPNE DTNext                   ;Next character while Olen <= 255
        JMP SHORT DTStoreLen            ;Loop termination

DTTab:  MOV    BH,CL                    ;Save input counter
        MOV    AL,DL                    ;Current output length in AL
        MOVZX  EAX,AL
        DIV    DH                       ;OLen DIV TabSize in AL
        INC    AL                       ;Round up to next tab position
        MUL    DH                       ;Next tab position in AX
        OR     AH,AH                    ;AX > 255?
        JNE    SHORT DTStoreLen               ;Can't store it
        SUB    AL,DL                    ;Count of blanks to insert
        ADD    DL,AL                    ;New output length in DL
        MOV    CL,AL                    ;Loop counter for blanks
        MOV    AX,0920h                 ;Tab in AH, Blank in AL
        REP    STOSB                    ;Store blanks
        MOV    CL,BH                    ;Restore input position
        LOOP   DTNext                   ;Back for next input

DTStoreLen:
        MOV    EDI,RS                   ;ES:DI => output string
        MOV    [EDI],DL                 ;Store final length

DTDone:
        POP    EDI
        POP    ESI
        POP    EBX
        VPExit_Code 08h                 ;!!.13

Detab   ENDP


CODE32  ENDS

        END
