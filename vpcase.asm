;******************************************************
;                  OPCASE.ASM 1.30
;              String handling routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************
;       Modification for Virtual Pascal for OS/2
;           Copyright (c) fPrint UK Ltd 1995
;******************************************************

;!!.20 numerous changes for handling of international upcase in pmode

        INCLUDE VPCOMMON.ASM

;****************************************************** Code

DATA32  SEGMENT DWORD PUBLIC USE32 'DATA'

        ;Pascal variables

        EXTRN   DpmiInUse : BYTE                                    ;!!.20
        EXTRN   UpcaseFunc : DWORD                                  ;!!.20
        EXTRN   UCTable : BYTE                                      ;!!.20
        EXTRN   LCTable : BYTE                                      ;!!.20

DATA32  ENDS

CODE32  SEGMENT DWORD PUBLIC USE32 'CODE'

        ASSUME  CS:CODE32, DS:DATA32

        PUBLIC  Upcase, Locase
        PUBLIC  StUpcase, StLocase
;        PUBLIC  SetInternationalUpcasePrim                 ;!!.02  ;!!.20
        PUBLIC  UpCasePrim, LoCasePrim

;UpcaseFunc Pointer <>               ;Address of upcase function  !!.02 !!.20

;************************************************ SetInternationalUpcasePrim
;!!.02 new routine
;procedure SetInternationalUpcase;
;Activate DOS international uppercase function for current code page

;SetInternationalUpcasePrim PROC    NEAR ;!!.20
;        VPStackFrameBP
;        SUB     SP,34                   ;Space for CountryInfo {34} !!.20
;        MOV     AH,30h                  ;Get DOS version
;        INT     21h
;        CMP     AL,2
;        JBE     SetDone                 ;Get out for DOS 2.x or earlier
;        MOV     AX,3800h                ;Get DOS country info
;        MOV     DX,SP                   ;DX offset of CountryInfo space
;        PUSH    DS
;        PUSH    SS
;        POP     DS                      ;DS=SS
;        INT     21h
;        POP     DS
;        JC      SetDone                 ;Done if function failed
;        MOV     AX,[BP-14]
;        MOV     UpcaseFunc.Segm,AX      ;Save DOS function address
;        MOV     AX,[BP-16]
;        MOV     UpcaseFunc.Ofst,AX
;SetDone:
;        Exit_Code 0                     ;!!.13
;SetInternationalUpcasePrim ENDP

;****************************************************** UpcasePrim
;!!.02 modified for generalized international uppercasing function
;Entry : character to upcase in AL
;Exit  : uppercase in AL

UpcasePrim PROC
UpcasePrimNear:                         ;!!.20
        PUSH    EBX                     ;!!.21 - preserve BX
        CMP     AL,128
        JB      Short NotExtended
        CMP     AL,165
        JA      Short UpCaseDone
        SUB     AL,128                  ;Reduce to range of map table
        MOV     EBX,OFFSET UCTable
;        LEA     EBX,UCTable
        XLAT    [EBX]
        JMP     SHORT UpCaseDone
NotExtended:
        CMP     AL,'a'
        JB      SHORT UpCaseDone              ;Done if AL < 'a'
        CMP     AL,'z'
        JA      SHORT UpCaseDone              ;Done if AL > 'z'
        SUB     AL,32                   ;Convert to uppercase
UpCaseDone:
        POP     EBX                      ;!!.21
        RETN
UpcasePrim ENDP

;****************************************************** Upcase

;function UpCase(Ch : Char) : Char;
;Return uppercase of char, with international character support

UpCase  PROC
;        MOV     EBX,ESP
        MOV     AL,[ESP+4]            ;AL = input character
        CALL    UpcasePrimNear
        RETN    4
UpCase  ENDP

;****************************************************** LocasePrim

LoCasePrim PROC
LoCasePrimNear:                         ;!!.20
        PUSH    EBX                     ;!!.21
        CMP     AL,128
        JB      SHORT NotExtendedLo
        CMP     AL,165
        JA      SHORT LoCaseDone
        SUB     AL,128                  ;Reduce to range of map table
;        LEA     EBX,LCTable
        MOV     EBX, OFFSET LCTable
        XLAT    [EBX]
        JMP     SHORT LoCaseDone
NotExtendedLo:
        CMP     AL,'A'
        JB      SHORT LoCaseDone              ;Done if AL < 'A'
        CMP     AL,'Z'
        JA      SHORT LoCaseDone              ;Done if AL > 'A'
        ADD     AL,32                   ;Convert to lowercase
LoCaseDone:
        POP     EBX                     ;!!.21
        RET
LoCasePrim ENDP

;****************************************************** Locase

;function Locase(Ch : Char) : Char;
;Return lowercase of char, with international character support

LoCase  PROC
        MOV     EBX,ESP
        MOV     AL,[EBX+4]              ;AL = input character
        CALL    LoCasePrimNear          ;!!.02
        RET     4
LoCase  ENDP

;****************************************************** StLocase

;function StLocase(S : string) : string;
;Convert upper case letters in string to lower case

StLocase PROC
         LEA     EDX,LocasePrim
         JMP     SHORT StCaseNear
StLocase ENDP

;****************************************************** StUpcase

;function StUpcase(S : string) : string;
;Convert lower case letters in string to upper case

StUpcase PROC
         LEA     EDX,UpcasePrim
         ; falls through into StCase
StUpcase ENDP

;****************************************************** StCase
;Convert string to one case or another, depending on DX
StCase  PROC
StCaseNear:
        VPStackFrame
        PUSH    ESI
        PUSH    EDI
        CLD                             ;go forward
        MOV     ESI,[EBX+08h]           ;DS:SI => S
        MOV     EDI,[EBX+0Ch]           ;ES:DI => function result
        LODSB                           ;AL = Length(S)
        STOSB                           ;Set length of result
        SetZero ECX                     ;ECX = 0
        MOV     CL,AL                   ;ECX = Length(S)
        JCXZ    SUDone                  ;Done if CX is 0
SUNext:
        LODSB                           ;Next char into AL
        CALL    EDX                     ;Upper/lower-case it
        STOSB                           ;Store char in result
        LOOP    SUNext                  ;repeat
SUDone:
        POP     EDI
        POP     ESI
        POP     EBX
        RET     4
StCase  ENDP


CODE32  ENDS

        END
