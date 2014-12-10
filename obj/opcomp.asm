;******************************************************
;                  OPCOMP.ASM 1.30
;              String handling routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT WORD PUBLIC

        EXTRN   LetterValues : BYTE     ;Table of letter values

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  CompString, CompUCString, CompStruct
        PUBLIC  Soundex, MakeLetterSet, CompareLetterSets

        EXTRN   UpCasePrim : FAR

Upcase  MACRO                           ;UpCase character in AL
;        PUSH   BX                      ;!!.02 UpCasePrim saves BX now
        CALL   UpCasePrim
;        POP    BX                      ;!!.02 UpCasePrim saves BX now
        ENDM

;****************************************************** CompString

;  function CompString(s1, s2 : string) : CompareType;
;    {-Return 0, 1, 2 if s1<s2, s1=s2, or s1>s2}

CompString  PROC FAR

        StackFrame
        MOV     DX,DS                   ;Save DS
        CLD                             ;Go forward

        LES     DI,SS:[BX+4]            ;ES:DI points to S2
        LDS     SI,SS:[BX+8]            ;DS:SI points to S1

        MOV     AH,ES:[DI]              ;AH = Length(S2)
        INC     DI                      ;DI points to S2[1]
        LODSB                           ;AL = Length(S1)
                                        ;SI points to S1[1]

        XOR     BX,BX                   ;BX holds temporary result
        XOR     CX,CX                   ;CX holds count of chars to compare

        MOV     CL,AL                   ;Length(S1) in CL
        CMP     AL,AH                   ;Compare lengths
        JE      EqLen                   ;Lengths equal?
        JB      Comp                    ;Jump if S1 shorter than S1

        INC     BX                      ;S1 longer than S2
        MOV     CL,AH                   ;Length(S2) in CL

EqLen:  INC     BX                      ;Equal or greater

Comp:   JCXZ    Done                    ;Done if either is empty

        REPE    CMPSB                   ;Compare until no match or CX = 0
        JE      Done                    ;If Equal, result ready based on length

        MOV     BL,2
        JA      Done                    ;S1 Greater? Return 2
        XOR     BX,BX                   ;Else S1 Less, Return 0

Done:   MOV     AX,BX                   ;Result into AX
        MOV     DS,DX                   ;Restore DS
        RET     8

CompString      ENDP

;****************************************************** CompUCString

;  function CompUCString(s1, s2 : string) : CompareType;
;    {-Return 0, 1, 2 if s1<s2, s1=s2, or s1>s2}
;    {-Comparison is done in uppercase}

CompUCString  PROC FAR

        StackFrame
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward

        LES     DI,SS:[BX+4]            ;ES:DI points to S2
        LDS     SI,SS:[BX+8]            ;DS:SI points to S1

        MOV     AH,ES:[DI]              ;AH = Length(S2)
        INC     DI                      ;DI points to S2[1]
        LODSB                           ;AL = Length(S1)
                                        ;SI points to S1[1]

        XOR     BX,BX                   ;BX holds temporary result
        XOR     CX,CX                   ;CX holds count of chars to compare

        MOV     CL,AL                   ;Length(S1) in CL
        CMP     AL,AH                   ;Compare lengths
        JE      UcEqLen                 ;Lengths equal?
        JB      UcComp                  ;Jump if S1 shorter than S1

        INC     BX                      ;S1 longer than S2
        MOV     CL,AH                   ;Shorter length in CL

UcEqLen: INC    BX                      ;Equal or greater

UcComp: JCXZ    UcDone                  ;UcDone if lesser string is empty

Start:  LODSB                           ;S1[?] into AL
        Upcase                          ;convert to upper case
        MOV     AH,ES:[DI]              ;S2[?] into AH
        INC     DI                      ;Point ES:DI to next char in S2
        XCHG    AL,AH
        Upcase                          ;convert to upper case
        CMP     AH,AL                   ;Compare until no match
        LOOPE   Start

        JE      UcDone                    ;If Equal, result ready based on length

        MOV     BL,2
        JA      UcDone                  ;S1 Greater? Return 2
        XOR     BX,BX                   ;Else S1 Less, Return 0

UcDone: MOV     AX,BX                   ;Result into AX
        POP     DS                      ;Restore DS
        RET     8

CompUCString    ENDP


;****************************************************** CompStruct

;  function CompStruct(var s1, s2; size : word) : CompareType;
;    {-Compare two fixed size structures}

CompStruct  PROC FAR

        StackFrame
        MOV     DX,DS                   ;Save DS
        MOV     AX,1                    ;BX holds temporary result (Equal)

        MOV     CX,SS:[BX+4]            ;Size in CX
        JCXZ    CSDone                  ;Make sure size isn't zero

        LES     DI,SS:[BX+6]            ;ES:DI points to S2
        LDS     SI,SS:[BX+10]           ;DS:SI points to S1
        CLD                             ;Go forward

        REPE    CMPSB                   ;Compare until no match or CX = 0
        JE      CSDone                  ;If Equal, result ready based on length

        INC     AX                      ;Prepare for Greater
        JA      CSDone                  ;S1 Greater? Return 2
        XOR     AX,AX                   ;Else S1 Less, Return 0

CSDone: MOV     DS,DX                   ;Restore DS
        RET     10

CompStruct      ENDP

;****************************************************** Soundex

;  function Soundex(s : string) : string;
;    {-Return 4 character soundex of input string}

;256 byte lookup table ASCII ==> soundex code
SoundExTable label byte
    db   65 dup(0)
;        A  B   C   D  E  F   G  H I  J   K   L   M   N  O  P   Q   R   S   T  U  V  W  X  Y  Z
    db   0,'1','2','3',0,'1','2',0,0,'2','2','4','5','5',0,'1','2','6','2','3',0,'1',0,'2',0,'2'
    db   6 dup(0)
;        a  b   c   d  e  f   g  h i  j   k   l   m   n  o  p   q   r   s   t  u  v  w  x  y  z
    db   0,'1','2','3',0,'1','2',0,0,'2','2','4','5','5',0,'1','2','6','2','3',0,'1',0,'2',0,'2'
    db   133 dup(0)

;Parameter and function result
    Result EQU DWORD PTR [BP+10]
    Input  EQU DWORD PTR [BP+6]

Soundex PROC FAR

        StackFrameBP
        PUSH   DS
        CLD
        LES    DI,Result                ;ES:DI => function result
        MOV    AL,4
        STOSB                           ;Result will be 4 characters long
        MOV    BX,DI                    ;Store output position in BX
        MOV    AL,'0'                   ;Store four '0's in output
        MOV    CX,4
        REP    STOSB                    ;Initialize to zeros
        MOV    DI,BX                    ;Reset output position

        LDS    SI,Input                 ;DS:SI => Input string
        LODSB                           ;Length byte into AL
        MOV    CL,AL                    ;Length into CX
        JCXZ   SXDone                   ;We're done if null string
        LODSB                           ;Get first character of input
        UpCase                          ;Uppercase it
        STOSB                           ;Store first output character
        DEC    CX                       ;One input character used
        JCXZ   SXDone                   ;Done if one character string

        MOV    AH,AL                    ;Save previous character
        MOV    DX,0401h                 ;DL has output length, DH max output length
        XOR    BH,BH                    ;Prepare BX for indexing

SXNext:
        LODSB                           ;Next character into AL
        MOV    BL,AL                    ;Set up base register
        MOV    AL,CS:SoundexTable[BX]   ;Get soundex code into AL
        OR     AL,AL                    ;Null soundex code?
        JZ     SXNoStore                ;Don't store it
        CMP    AH,AL                    ;Code same as previous output?
        JZ     SXNoStore                ;Don't store it
        STOSB                           ;Store to output
        INC    DL                       ;Output length increased by one
        CMP    DL,DH                    ;Check output length
        JAE    SXDone                   ;Stop at four chars of output
        MOV    AH,AL                    ;Store previous output character

SXNoStore:
        LOOP   SXNext

SXDone:
        POP    DS
        Exit_Code 4                     ;!!.13

Soundex ENDP

;****************************************************** MakeLetterSet

;function MakeLetterSet(S : string) : LongInt;
;Return a bit-mapped long storing the individual letters contained in S.

MLSstr  EQU     DWORD PTR SS:[BX+4]

MakeLetterSet   PROC FAR

        StackFrame                              ;Set up stackframe
        PUSH    BP                              ;Save BP
        PUSH    DS                              ;Save DS
        SetZero DI                              ;DI = 0
        MOV     AX,DI                           ;AX = 0
        CLD                                     ;Go forward
        LDS     SI,MLSstr                       ;DS:SI => string
        LODSB                                   ;AX = Length(S)
        MOV     CX,AX                           ;CX = Length(S)
        MOV     BX,DI                           ;DI:BX = 0
        JCXZ    MLSexit                         ;Done if CX is 0

MLSnext:
        SetZero AH                              ;AH = 0
        LODSB                                   ;AL has next char in S
        Upcase                                  ;Convert to upper case
        SUB     AX,'A'                          ;Convert to bit number
        CMP     AX,'Z'-'A'                      ;Was char in range 'A'..'Z'?
        JA      MLSskip                         ;Skip it if not

        XCHG    CX,AX                           ;CX = bit #, AX = loop count
        SetZero DX                              ;DX:AX = 1
        MOV     BP,1
        JCXZ    MLSnoShift                      ;don't shift if CX is 0

MLSshift:                                       ;DX:BP = 1 shl BitNumber
        SHL     BP,1                            ;shift low word
        RCL     DX,1                            ;shift high word
        LOOP    MLSshift                        ;repeat

MLSnoshift:
        OR      DI,DX                           ;DI:BX = DI:BX or DX:BP
        OR      BX,BP
        MOV     CX,AX                           ;Restore CX from AX

MLSskip:
        LOOP    MLSnext                         ;Get next character

MLSexit:
        MOV     DX,DI                           ;DX:AX = DI:BX
        MOV     AX,BX
        POP     DS                              ;Restore DS
        POP     BP                              ;Restore BP
        RET     4

MakeLetterSet   ENDP

;****************************************************** CompareLetterSets

;function CompareLetterSets(Set1, Set2 : LongInt) : Word;
;Returns the sum of the values of the letters common to Set1 and Set2.

Set1Lo  EQU     WORD PTR SS:[BX+4]                 ;!!.20
Set1Hi  EQU     WORD PTR SS:[BX+6]                 ;!!.20
Set2Hi  EQU     WORD PTR SS:[BX+10]
Set2Lo  EQU     WORD PTR SS:[BX+8]

CompareLetterSets       PROC FAR

        StackFrame
        PUSH    BP                              ;Save BP

;!!.20  LES     DI,Set1                         ;Set1 in ES:DI
        MOV     DI,Set1Lo                       ;Set1 in SI:DI !!.20
        MOV     SI,Set1Hi                       ;              !!.20
;!!.20  MOV     SI,ES                           ;Set1 in SI:DI
        AND     DI,Set2Lo                       ;SI:DI = Set1 and Set2
        AND     SI,Set2Hi

        SetZero BP                              ;BP = 0
        MOV     CX,('Z'-'A')+1                  ;Loop count

CLSnext:
        MOV     BX,CX                           ;save CX in BX
        SetZero DX                              ;DX:AX = 1
        MOV     AX,1
        SUB     CX,AX                           ;subtract 1 to get bit number
        JZ      CLSnoShift                      ;don't shift if CX is 0

CLSshift:                                       ;DX:AX = 1 shl BitNumber
        SHL     AX,1                            ;shift low word
        RCL     DX,1                            ;shift high word
        LOOP    CLSshift                        ;repeat

CLSnoshift:
        MOV     CX,BX                           ;restore CX from BX
        AND     AX,DI                           ;DX:AX = DX:AX and SI:DI
        AND     DX,SI
        OR      AX,DX                           ;DX:AX = 0?
        JNZ     CLSadd                          ;if not, add letter value
        LOOP    CLSnext                         ;else, next element
        JMP     SHORT CLSexit                   ;done

CLSadd:
        SetZero AH                              ;AX has value of the letter
        MOV     AX,CX                           ;AL = loop count
        DEC     AL                              ;convert to index into table
        MOV     BX,Offset LetterValues          ;DS:BX points to LetterValues
        XLAT                                    ;AL has value of the letter
        ADD     BP,AX                           ;add to result
        LOOP    CLSnext                         ;next element

CLSexit:
        MOV     AX,BP                           ;Function result into AX
        POP     BP                              ;Restore BP
        RET     8

CompareLetterSets       ENDP


CODE    ENDS

        END
