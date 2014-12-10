;******************************************************
;                  OPCASE.ASM 1.30
;              String handling routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

;!!.20 numerous changes for handling of international upcase in pmode

        INCLUDE OPCOMMON.ASM

;****************************************************** Code

DATA    SEGMENT BYTE PUBLIC

        ;Pascal variables

        EXTRN   DpmiInUse : BYTE                                    ;!!.20
        EXTRN   UCTable : BYTE                                      ;!!.20
        EXTRN   LCTable : BYTE                                      ;!!.20
        EXTRN   UpcaseFunc : DWORD                                  ;!!.20

DATA    ENDS
CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA                                    ;!!.20

        PUBLIC  Upcase, Locase
        PUBLIC  StUpcase, StLocase
        PUBLIC  SetInternationalUpcasePrim                 ;!!.02  ;!!.20
        PUBLIC  UpCasePrim, LoCasePrim
        PUBLIC  MoveFast

;UpcaseFunc Pointer <>               ;Address of upcase function  !!.02 !!.20

;************************************************ SetInternationalUpcasePrim
;!!.02 new routine
;procedure SetInternationalUpcase;
;Activate DOS international uppercase function for current code page

SetInternationalUpcasePrim PROC    NEAR ;!!.20
        StackFrameBP
        SUB     SP,34                   ;Space for CountryInfo {34} !!.20
        MOV     AH,30h                  ;Get DOS version
        INT     21h
        CMP     AL,2
        JBE     SetDone                 ;Get out for DOS 2.x or earlier
        MOV     AX,3800h                ;Get DOS country info
        MOV     DX,SP                   ;DX offset of CountryInfo space
        PUSH    DS
        PUSH    SS
        POP     DS                      ;DS=SS
        INT     21h
        POP     DS
        JC      SetDone                 ;Done if function failed
        MOV     AX,[BP-14]
        MOV     UpcaseFunc.Segm,AX      ;Save DOS function address
        MOV     AX,[BP-16]
        MOV     UpcaseFunc.Ofst,AX
SetDone:
        Exit_Code 0                     ;!!.13
SetInternationalUpcasePrim ENDP

;****************************************************** UpcasePrim
;!!.02 modified for generalized international uppercasing function
;Entry : character to upcase in AL
;Exit  : uppercase in AL

UpcasePrim PROC FAR
UpcasePrimNear:                         ;!!.20
        PUSH    BX                      ;!!.21 - preserve BX
        CMP     AL,128
        JB      NotExtended
        CMP     AL,165
        JA      UpCaseDone
        SUB     AL,128                  ;Reduce to range of map table
        LEA     BX,UCTable
        PUSH    DS
        PUSH    AX
        MOV     AX,SEG DATA
        MOV     DS,AX
        POP     AX
        XLAT    DS:[BX]
        POP     DS
        JMP     SHORT UpCaseDone
NotExtended:
        CMP     AL,'a'
        JB      UpCaseDone              ;Done if AL < 'a'
        CMP     AL,'z'
        JA      UpCaseDone              ;Done if AL > 'z'
        SUB     AL,32                   ;Convert to uppercase
UpCaseDone:
        POP     BX                      ;!!.21
        RET
UpcasePrim ENDP

;****************************************************** Upcase

;function UpCase(Ch : Char) : Char;
;Return uppercase of char, with international character support

UpCase  PROC    FAR
        MOV     BX,SP
        MOV     AL,SS:[BX+4]            ;AL = input character
        PUSH    CS                      ;!!.02
        CALL    UpcasePrimNear          ;!!.02
        RET     2
UpCase  ENDP

;****************************************************** LocasePrim

LoCasePrim PROC FAR
LoCasePrimNear:                         ;!!.20
        PUSH    BX                      ;!!.21
        CMP     AL,128
        JB      NotExtendedLo
        CMP     AL,165
        JA      LoCaseDone
        SUB     AL,128                  ;Reduce to range of map table
        LEA     BX,LCTable
        PUSH    DS
        PUSH    AX
        MOV     AX,SEG DATA
        MOV     DS,AX
        POP     AX
        XLAT    DS:[BX]
        POP     DS
        JMP     SHORT LoCaseDone
NotExtendedLo:
        CMP     AL,'A'
        JB      LoCaseDone              ;Done if AL < 'A'
        CMP     AL,'Z'
        JA      LoCaseDone              ;Done if AL > 'A'
        ADD     AL,32                   ;Convert to lowercase
LoCaseDone:
        POP     BX                      ;!!.21
        RET
LoCasePrim ENDP

;****************************************************** Locase

;function Locase(Ch : Char) : Char;
;Return lowercase of char, with international character support

LoCase  PROC    FAR
        MOV     BX,SP
        MOV     AL,SS:[BX+4]            ;AL = input character
        PUSH    CS                      ;!!.02
        CALL    LoCasePrimNear          ;!!.02
        RET     2
LoCase  ENDP

;****************************************************** StLocase

;function StLocase(S : string) : string;
;Convert upper case letters in string to lower case

StLocase PROC FAR
         MOV     DX,OFFSET LocasePrim
         JMP     SHORT StCaseNear
StLocase ENDP

;****************************************************** StUpcase

;function StUpcase(S : string) : string;
;Convert lower case letters in string to upper case

StUpcase PROC FAR
         MOV     DX,OFFSET UpcasePrim
         ; falls through into StCase
StUpcase ENDP

;****************************************************** StCase
;Convert string to one case or another, depending on DX
StCase  PROC    FAR
StCaseNear:
        StackFrame
        PUSH    DS
        CLD                             ;go forward
        LDS     SI,SS:[BX+4]            ;DS:SI => S
        LES     DI,SS:[BX+8]            ;ES:DI => function result
        LODSB                           ;AL = Length(S)
        STOSB                           ;Set length of result
        SetZero CH                      ;CH = 0
        MOV     CL,AL                   ;CX = Length(S)
        JCXZ    SUDone                  ;Done if CX is 0
SUNext:
        LODSB                           ;Next char into AL
        PUSH    CS                      ;Fake a FAR CALL
        CALL    DX                      ;Uppercase it
        STOSB                           ;Store char in result
        LOOP    SUNext                  ;repeat
SUDone:
        POP     DS
        RET     4
StCase  ENDP

;****************************************************** MoveFast

; procedure MoveFast(var Src, Dest; Count : Word);
; Move Count bytes from Src to Dest

; Thanks to Pat Ritchey for portions of this routine

Normalize       MACRO   Sgm, Ofs
                ;Note: Destroys CX and AX
                MOV     AX,Ofs          ;AX = Ofs
                MOV     CL,4            ;CL = 4
                SHR     AX,CL           ;AX = Ofs shr 4
                ADD     Sgm,AX          ;Add result to Sgm
                AND     Ofs,000Fh       ;Ofs = Ofs and $000F
                ENDM

mfSrc   EQU     DWORD PTR [BP+12]
mfDest  EQU     DWORD PTR [BP+8]
mfCount EQU     WORD PTR [BP+6]

MoveFast        PROC FAR                ;!!.11 rewritten

        PUSH    BP                      ;set up stack frame
        MOV     BP,SP
        PUSH    DS                      ;save DS
        MOV     AL,DpmiInUse            ;save DpmiInUse                ;!!.20
        LDS     SI,mfSrc                ;DS:SI => Src
        LES     DI,mfDest               ;ES:DI => Dest
        MOV     CX,mfCount              ;CX    =  Count
        JCXZ    mfDone                  ;Do nothing if CX = 0
        MOV     DX,DS                   ;DX = DS
        MOV     BX,ES                   ;BX = ES

        CMP     AL,1                    ;PMode?                        ;!!.20
        JB      NoDpmi                  ;no, do normalization          ;!!.20
        CMP     BX,DX                   ;Are they equal?               ;!!.20
        JE      mfChkOfs                ;If so, compare offsets only   ;!!.20
        JMP     SHORT mfForward         ;else go forward               ;!!.20
NoDpmi:                                                                ;!!.20
        CMP     BX,DX                   ;Are they equal?
        JE      mfChkOfs                ;If so, compare offsets only
        PUSH    CX                      ;Save CX
        Normalize DX,SI                 ;Normalize DX:SI
        Normalize BX,DI                 ;Normalize BX,DI
        POP     CX                      ;Restore CX
        MOV     DS,DX                   ;Update DS and ES
        MOV     ES,BX
        CMP     DX,BX                   ;Is source above dest?
        JA      mfForward               ;If so, go forward
        JB      mfReverse               ;If below, go backward
mfChkOfs:                               ;Check offsets
        CMP     SI,DI                   ;Is source above dest?
        JA      mfForward               ;If so, go forward
mfReverse:
        STD                             ;go backward
        ADD     SI,CX                   ;point just beyond the ends
        ADD     DI,CX
        DEC     SI                      ;point to the actual ends
        DEC     DI
        TEST    DI,1                    ;is destination offset odd?
        JNZ     mfRevWord
        MOVSB                           ;move the odd byte
        DEC     CX                      ;decrement loop count
mfRevWord:
        DEC     SI                      ;point to the beginning of the word
        DEC     DI
        SHR     CX,1                    ;CX = words to move
        REP     MOVSW
        JNC     mfDone                  ;is there an odd byte to move?
        INC     SI                      ;point to the next byte
        INC     DI
        JMP     SHORT mfOddByte
mfForward:
        CLD                             ;go forward
        TEST    DI,1                    ;is destination offset odd?
        JZ      mfForWord
        MOVSB                           ;move the odd byte
        DEC     CX                      ;decrement loop count
mfForWord:
        SHR     CX,1                    ;CX = words to move
        REP     MOVSW
        JNC     mfDone                  ;is there an odd byte to move?
mfOddByte:
        MOVSB                           ;move the odd byte
mfDone:
        POP     DS                      ;restore DS
        POP     BP                      ;restore BP
        RET     10

MoveFast        ENDP

CODE    ENDS

        END
