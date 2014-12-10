;******************************************************
;                  VPCOMP.ASM 1.30
;              String handling routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************
;       Modification for Virtual Pascal for OS/2
;           Copyright (c) fPrint UK Ltd 1995
;******************************************************

        INCLUDE VPCOMMON.ASM

        EXTRN   UpCasePrim : PROC

;****************************************************** Data

DATA32  SEGMENT DWORD PUBLIC USE32 'DATA'

        EXTRN   LetterValues : BYTE     ;Table of letter values

DATA32  ENDS

;****************************************************** Code

CODE32  SEGMENT DWORD PUBLIC USE32 'CODE'

        ASSUME  CS:CODE32,DS:DATA32

        PUBLIC  CompString, CompUCString, CompStruct
        PUBLIC  Soundex, MakeLetterSet, CompareLetterSets

Upcase  MACRO                           ;UpCase character in AL
        CALL    UpCasePrim
        ENDM

;****************************************************** CompString

;  function CompString(s1, s2 : string) : CompareType;
;    {-Return 0, 1, 2 if s1<s2, s1=s2, or s1>s2}

CompString  PROC

        VPStackFrame
        PUSH    ESI
        PUSH    EDI
        CLD                             ;Go forward

        MOV     EDI,[EBX+08h]           ;EDI points to S2
        MOV     ESI,[EBX+0Ch]           ;ESI points to S1

        MOV     AH,[EDI]                ;AH = Length(S2)
        INC     DI                      ;DI points to S2[1]
        LODSB                           ;AL = Length(S1)
                                        ;SI points to S1[1]

        XOR     EBX,EBX                 ;BX holds temporary result
        XOR     ECX,ECX                 ;CX holds count of chars to compare

        MOV     CL,AL                   ;Length(S1) in CL
        CMP     AL,AH                   ;Compare lengths
        JE      EqLen                   ;Lengths equal?
        JB      Comp                    ;Jump if S1 shorter than S1

        INC     EBX                     ;S1 longer than S2
        MOV     CL,AH                   ;Length(S2) in CL

EqLen:  INC     EBX                     ;Equal or greater

Comp:   JCXZ    Done                    ;Done if either is empty

        REPE    CMPSB                   ;Compare until no match or CX = 0
        JE      Done                    ;If Equal, result ready based on length

        MOV     BL,2
        JA      Done                    ;S1 Greater? Return 2
        XOR     EBX,EBX                 ;Else S1 Less, Return 0

Done:
        POP     EDI
        POP     ESI
        MOV     EAX,EBX                 ;Result into AX
        POP     EBX
        RETN    8

CompString      ENDP

;****************************************************** CompUCString

;  function CompUCString(s1, s2 : string) : CompareType;
;    {-Return 0, 1, 2 if s1<s2, s1=s2, or s1>s2}
;    {-Comparison is done in uppercase}

CompUCString  PROC FAR

        VPStackFrame
        PUSH    ESI
        PUSH    EDI
        CLD                             ;Go forward

        MOV     EDI,[EBX+08h]             ;ES:DI points to S2
        MOV     ESI,[EBX+0Ch]             ;DS:SI points to S1

        MOV     AH,[EDI]                ;AH = Length(S2)
        INC     EDI                     ;DI points to S2[1]
        LODSB                           ;AL = Length(S1)
                                        ;SI points to S1[1]

        XOR     EBX,EBX                 ;BX holds temporary result
        XOR     ECX,ECX                 ;CX holds count of chars to compare

        MOV     CL,AL                   ;Length(S1) in CL
        CMP     AL,AH                   ;Compare lengths
        JE      SHORT UcEqLen           ;Lengths equal?
        JB      SHORT UcComp            ;Jump if S1 shorter than S1

        INC     EBX                     ;S1 longer than S2
        MOV     CL,AH                   ;Shorter length in CL

UcEqLen:INC     EBX                     ;Equal or greater

UcComp: JCXZ    UcDone                  ;UcDone if lesser string is empty

Start:  LODSB                           ;S1[?] into AL
        Call    UpcasePrim              ;convert to upper case
        MOV     AH,[EDI]                ;S2[?] into AH
        INC     EDI                     ;Point ES:DI to next char in S2
        XCHG    AL,AH
        Call    UpcasePrim              ;convert to upper case
        CMP     AH,AL                   ;Compare until no match
        LOOPE   Start

        JE      UcDone                  ;If Equal, result ready based on length

        MOV     BL,2
        JA      UcDone                  ;S1 Greater? Return 2
        XOR     EBX,EBX                 ;Else S1 Less, Return 0

UcDone:
        POP     EDI
        POP     ESI
        MOV     EAX,EBX                 ;Result into AX
        POP     EBX
        RETN    8

CompUCString    ENDP


;****************************************************** CompStruct

;  function CompStruct(var s1, s2; size : word) : CompareType;
;    {-Compare two fixed size structures}

CompStruct  PROC

        VPStackFrame
        PUSH    ESI
        PUSH    EDI
        MOV     EAX,1                   ;temporary result (Equal)

        MOV     ECX,[EBX+08h]            ;Size in CX
        JCXZ    CSDone                  ;Make sure size isn't zero

        MOV     EDI,[EBX+0Ch]           ;ES:DI points to S2
        MOV     ESI,[EBX+10h]           ;DS:SI points to S1
        CLD                             ;Go forward

        REPE    CMPSB                   ;Compare until no match or CX = 0
        JE      CSDone                  ;If Equal, result ready based on length

        INC     EAX                     ;Prepare for Greater
        JA      CSDone                  ;S1 Greater? Return 2
        XOR     EAX,EAX                 ;Else S1 Less, Return 0

CSDone:
        POP     EDI
        POP     ESI
        POP     EBX
        RET     0Ch

CompStruct      ENDP

;****************************************************** Soundex

;  function Soundex(s : string) : string;
;    {-Return 4 character soundex of input string}

;256 byte lookup table ASCII ==> soundex code
SoundExTable label Byte
    db   65 dup(0)
;        A  B   C   D  E  F   G  H I  J   K   L   M   N  O  P   Q   R   S   T  U  V  W  X  Y  Z
    db   0,'1','2','3',0,'1','2',0,0,'2','2','4','5','5',0,'1','2','6','2','3',0,'1',0,'2',0,'2'
    db   6 dup(0)
;        a  b   c   d  e  f   g  h i  j   k   l   m   n  o  p   q   r   s   t  u  v  w  x  y  z
    db   0,'1','2','3',0,'1','2',0,0,'2','2','4','5','5',0,'1','2','6','2','3',0,'1',0,'2',0,'2'
    db   133 dup(0)


;Parameter and function result
    Result EQU DWORD PTR [EBP+0Ch]
    Input  EQU DWORD PTR [EBP+08h]

Soundex PROC

        VPStackFrameBP
        PUSH   EBX
        PUSH   ESI
        PUSH   EDI
        CLD
        MOV    EDI,Result               ;ES:DI => function result
        MOV    AL,4
        STOSB                           ;Result will be 4 characters long
        MOV    EBX,EDI                  ;Store output position in BX
        MOV    AL,'0'                   ;Store four '0's in output
        MOV    ECX,4
        REP    STOSB                    ;Initialize to zeros
        MOV    EDI,EBX                  ;Reset output position

        MOV    ESI,Input                ;DS:SI => Input string
        LODSB                           ;Length byte into AL
        MOV    CL,AL                    ;Length into CX
        JCXZ   SXDone                   ;We're done if null string
        LODSB                           ;Get first character of input
        UpCase                          ;Uppercase it
        STOSB                           ;Store first output character
        DEC    ECX                      ;One input character used
        JCXZ   SXDone                   ;Done if one character string

        MOV    AH,AL                    ;Save previous character
        MOV    EDX,0401h                ;DL has output length, DH max output length

SXNext:
        XOR    EBX,EBX
        LODSB                           ;Next character into AL
        MOV    BL,AL                    ;Set up base register
        ADD    EBX,Offset SoundExTable  ;Add offset of SoundExTable to EBX
        MOV    AL,[EBX]                 ;Get SoundEx code
;       MOV    AL,SoundexTable[EBX]     <- This does not work! Why???
        OR     AL,AL                    ;Null soundex code?
        JZ     SHORT SXNoStore                ;Don't store it
        CMP    AH,AL                    ;Code same as previous output?
        JZ     SHORT SXNoStore                ;Don't store it
        STOSB                           ;Store to output
        INC    DL                       ;Output length increased by one
        CMP    DL,DH                    ;Check output length
        JAE    SHORT SXDone                   ;Stop at four chars of output
        MOV    AH,AL                    ;Store previous output character

SXNoStore:
        LOOP   SXNext

SXDone:
        POP    EBX
        POP    EDI
        POP    ESI
        VPExit_Code 4                     ;!!.13

Soundex ENDP

;****************************************************** MakeLetterSet

;function MakeLetterSet(S : string) : LongInt;
;Return a bit-mapped long storing the individual letters contained in S.

MLSstr  EQU     DWORD PTR [EBX+8]

MakeLetterSet   PROC

        VPStackFrame                            ;Set up stackframe
        PUSH    EBP                             ;Save BP
        PUSH    ESI
        PUSH    EDI

        SetZero EDI                             ;EDI = 0
        MOV     EAX,EDI                         ;EAX = 0
        CLD                                     ;Go forward
        MOV     ESI,MLSstr                      ;ESI => string
        LODSB                                   ;AX = Length(S)
        MOV     ECX,EAX                         ;ECX = Length(S)
        MOV     EBX,EDI                         ;EBX = 0
        JCXZ    MLSexit                         ;Done if CX is 0

MLSnext:
        SetZero AH                              ;AH = 0
        LODSB                                   ;AL has next char in S
        Upcase                                  ;Convert to upper case
        SUB     EAX,'A'                         ;Convert to bit number
        CMP     EAX,'Z'-'A'                     ;Was char in range 'A'..'Z'?
        JA      MLSskip                         ;Skip it if not

        XCHG    ECX,EAX                         ;CX = bit #, AX = loop count
        SetZero EDX                             ;DX:AX = 1
        MOV     EBP,1
        JCXZ    MLSnoShift                      ;don't shift if CX is 0

MLSshift:                                       ;DX:BP = 1 shl BitNumber
        SHL     EBP,1                           ;shift low word
        RCL     EDX,1                           ;shift high word
        LOOP    MLSshift                        ;repeat

MLSnoshift:
        OR      EDI,EDX                         ;DI:BX = DI:BX or DX:BP
        OR      EBX,EBP
        MOV     ECX,EAX                         ;Restore CX from AX

MLSskip:
        LOOP    MLSnext                         ;Get next character

MLSexit:
        MOV     EDX,EDI                         ;DX:AX = DI:BX
        MOV     EAX,EBX
        POP     EDI
        POP     ESI
        POP     EBP                             ;Restore BP
        POP     EBX
        RETN    4

MakeLetterSet   ENDP

;****************************************************** CompareLetterSets

;function CompareLetterSets(Set1, Set2 : LongInt) : Word;
;Returns the sum of the values of the letters common to Set1 and Set2.

Set1    EQU     DWORD PTR [EBX+08h]                 ;!!.20
Set2    EQU     DWORD PTR [EBX+0Ch]

CompareLetterSets       PROC

        VPStackFrame
        PUSH    EBP                             ;Save EBP
        PUSH    ESI
        PUSH    EDI

        MOV     EDI,Set1                        ;Set1 in EDI
        MOV     ESI,Set2
        AND     EDI,ESI                         ;Set1 AND Set2 in EDI

        SetZero EBP                             ;EBP = 0
        MOV     ECX,('Z'-'A')+1                 ;Loop count

CLSnext:
        MOV     EBX,ECX                         ;save ECX in EBX
        MOV     EAX,1                           ;EAX = 1
        SUB     ECX,EAX                         ;subtract 1 to get bit number
        JZ      CLSnoShift                      ;don't shift if CX is 0

CLSshift:                                       ;EAX = 1 shl BitNumber
        SHL     EAX,1                           ;shift left
        LOOP    CLSshift                        ;repeat

CLSnoshift:
        MOV     ECX,EBX                         ;restore CX from BX
        AND     EAX,EDI                         ;EAX = EAX and EDI
        OR      EAX,EAX                         ;EAX = 0?
        JNZ     CLSadd                          ;if not, add letter value
        LOOP    CLSnext                         ;else, next element
        JMP     SHORT CLSexit                   ;done

CLSadd:
        SetZero AH                              ;AX has value of the letter
        MOV     EAX,ECX                         ;AL = loop count
        DEC     AL                              ;convert to index into table
        MOV     EBX,Offset LetterValues         ;EBX points to LetterValues
        XLAT                                    ;AL has value of the letter
        ADD     EBP,EAX                         ;add to result
        LOOP    CLSnext                         ;next element

CLSexit:
        MOV     EAX,EBP                         ;Function result into AX
        POP     EDI
        POP     ESI
        POP     EBP                             ;Restore BP
        POP     EBX
        RET     8

CompareLetterSets       ENDP


CODE32  ENDS

        END
