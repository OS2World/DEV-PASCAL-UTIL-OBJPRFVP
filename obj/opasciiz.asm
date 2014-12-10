;******************************************************
;                  OPASCIIZ.ASM 1.30
;             ASCIIZ string manipulation
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;externals in other segments !!.03

        EXTRN   UpCasePrim : FAR
        EXTRN   LoCasePrim : FAR

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

        PUBLIC  Search
        PUBLIC  AscUpcase
        PUBLIC  AscLocase
        PUBLIC  CompAsc
        PUBLIC  CompUCAsc

Upcase  MACRO                           ;UpCase character in AL
;        PUSH   BX                      ;!!.02 UpCasePrim saves BX now
        CALL   UpCasePrim
;        POP    BX                      ;!!.02 UpCasePrim saves BX now
        ENDM

Locase  MACRO                           ;LoCase character in AL
        PUSH   BX
        CALL   LoCasePrim
        POP    BX
        ENDM

;****************************************************** Search

;  function Search(var Buffer; BufLength : Word;
;                  var Match;  MatLength : Word) : Word; external;
;Search through Buffer for Match.
;BufLength is length of range to search.
;MatLength is length of string to match
;Returns number of bytes searched to find St, FFFF if not found

;equates for parameters:
MatLength       EQU     WORD PTR [BP+6]
Match           EQU     DWORD PTR [BP+8]
BufLength       EQU     WORD PTR  [BP+0Ch]
Buffer          EQU     DWORD PTR [BP+0Eh]

Search  PROC FAR

        StackFrameBP
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward

        LES     DI,Buffer               ;ES:DI => Buffer
        MOV     BX,DI                   ;BX = Ofs(Buffer)

        MOV     CX,BufLength            ;CX = Length of range to scan
        MOV     DX,MatLength            ;DX = Length of match string

        TEST    DX,DX                   ;Length(Match) = 0?
        JZ      Error                   ;If so, we're done

        LDS     SI,Match                ;DS:SI => Match buffer
        LODSB                           ;AL = Match[1]; DS:SI => Match[2]
        DEC     DX                      ;DX = MatLength-1
        SUB     CX,DX                   ;CX = BufLength-(MatLength-1)
        JBE     Error                   ;Error if BufLength is less

;Search for first character in St
Next:   REPNE   SCASB                   ;Search forward for Match[1]
        JNE     Error                   ;Done if not found
        TEST    DX,DX                   ;If Length = 1 (DX = 0) ...
        JZ      Found                   ; the "string" was found

        ;Search for remainder of St

        PUSH    CX                      ;Save CX
        PUSH    DI                      ;Save DI
        PUSH    SI                      ;Save SI

        MOV     CX,DX                   ;CX = Length(St) - 1
        REPE    CMPSB                   ;Does rest of string match?

        POP     SI                      ;Restore SI
        POP     DI                      ;Restore DI
        POP     CX                      ;Restore CX

        JNE     Next                    ;Try again if no match

;Calculate number of bytes searched and return in St
Found:  DEC     DI                      ;DX = Offset where found
        MOV     AX,DI                   ;AX = Offset where found
        SUB     AX,BX                   ;Subtract starting offset
        JMP     Short Done              ;Done

;Match was not found
Error:  XOR     AX,AX                   ;Return
        DEC     AX                      ;Return FFFF

Done:   POP     DS                      ;Restore DS
        Exit_Code 12                    ;!!.10 !!.13

Search  ENDP

;****************************************************** AscUpcase

; procedure AscUpcase{(var a, b : asciiz)};
;  {-Uppercase the Asciiz in a, returning b}

;equates for parameters:
A       EQU     DWORD PTR [BP+10]
B       EQU     DWORD PTR [BP+6]

AscUpcase       PROC FAR

        StackFrameBP
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward

        LES     DI,B                    ;ES:DI => B
        LDS     SI,A                    ;DS:SI => A

AUPNext:
        LODSB
        OR      AL,AL                   ;Termination of string?
        JZ      AUPDone
        Upcase                          ;convert to uppercase
        STOSB                           ;Store the converted character
        JMP     AUPNext                 ;Get the next character

AUPDone:
        STOSB                           ;Terminate output string
        POP     DS                      ;Restore DS
        Exit_Code 8                     ;!!.13

AscUpcase  ENDP

;****************************************************** AscLocase

;  procedure AscLocase{(var a, b : asciiz)};
;    {-Lowercase the Asciiz in a, returning b}

;equates for parameters:
A    EQU DWORD PTR [BP+10]
B    EQU DWORD PTR [BP+6]

AscLocase  PROC FAR

        StackFrameBP
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward

        LES     DI,B                    ;ES:DI => B
        LDS     SI,A                    ;DS:SI => A

ALONext:
        LODSB
        OR      AL,AL                   ;Termination of string?
        JZ      ALODone
        Locase                          ;convert to lower case
        STOSB                           ;Store the converted character
        JMP     ALONext                 ;Get the next character

ALODone:
        STOSB                           ;Terminate output string

        POP     DS                      ;Restore DS
        Exit_Code 8                     ;!!.13

AscLocase  ENDP

;****************************************************** CompAsc

;  function CompAsc {(var a1, a2 : Asciiz) : AscCompareType} ;
;    {-Return 0, 1, 2 if a1<a2, a1=a2, or a1>a2}

CompAsc  PROC FAR

        StackFrameBP
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward

        MOV     AL,0                    ;look for null

        LES     DI,[BP+6]               ;ES:DI points to A2
        MOV     BX,DI                   ;store initial offset
        MOV     CX,0FFFFh               ;check maximum length
        REPNE   SCASB                   ;scan while equal
        SUB     DI,BX                   ;get the number of bytes scanned
        MOV     DX,DI                   ;lenasc(A2) in dx
        DEC     DX                      ;null doesn't count

        LES     DI,[BP+10]              ;ES:DI points to A1
        MOV     BX,DI                   ;store initial offset
        MOV     CX,0FFFFh               ;check maximum length
        REPNE   SCASB                   ;scan while equal
        SUB     DI,BX                   ;get the number of bytes scanned
        MOV     CX,DI                   ;lenasc(A1) in cx
        DEC     CX                      ;null doesn't count

        LES     DI,[BP+6]               ;ES:DI points to A2
        LDS     SI,[BP+10]              ;DS:SI points to A1

        XOR     AX,AX                   ;AX holds result

        CMP     CX,DX                   ;Which string is longer?
        JE      EqLen                   ;Lengths equal
        JB      Comp                    ;Jump if A1 shorter than A2

        INC     AX                      ;A1 longer than A2
        MOV     CX,DX                   ;Shorter length in CX

EqLen:  INC     AX                      ;Equal or greater

Comp:   JCXZ    CDone                   ;Done if either is empty
        REPE    CMPSB                   ;Compare until no match or CX = 0
        JE      CDone                   ;If Equal, result ready based on length

        MOV     AL,2
        JA      CDone                   ;A1 Greater? Return 2
        XOR     AX,AX                   ;Else A1 Less, Return 0

CDone:  POP     DS                      ;Restore DS
        Exit_Code 8                     ;!!.13

CompAsc      ENDP

;****************************************************** CompUCAsc

;  function CompUCAsc {(var a1, a2 : Asciiz) : AscCompareType} ;
;    {-Return 0, 1, 2 if a1<a2, a1=a2, or a1>a2}
;    {-Comparison is done in uppercase}

CompUCAsc       PROC FAR

        StackFrameBP
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward

        MOV     AL,0                    ;look for null

        LES     DI,[BP+6]               ;ES:DI points to A2
        MOV     BX,DI                   ;store initial offset
        MOV     CX,0FFFFh               ;check maximum length
        REPNE SCASB                     ;scan while equal
        SUB     DI,BX                   ;get the number of bytes scanned
        MOV     DX,DI                   ;lenasc(A2) in dx
        DEC     DX                      ;null doesn't count

        LES     DI,[BP+10]              ;ES:DI points to A1
        MOV     BX,DI                   ;store initial offset
        MOV     CX,0FFFFh               ;check maximum length
        REPNE SCASB                     ;scan while equal
        SUB     DI,BX                   ;get the number of bytes scanned
        MOV     CX,DI                   ;lenasc(A1) in cx
        DEC     CX                      ;null doesn't count

        LES     DI,[BP+6]               ;ES:DI points to A2
        LDS     SI,[BP+10]              ;DS:SI points to A1

        XOR     BX,BX                   ;BX holds result

        CMP     CX,DX                   ;Which string is longer?
        JE      UcEqLen                 ;Lengths equal
        JB      UcComp                  ;Jump if A1 shorter than A2

        INC     BX                      ;A1 longer than A2
        MOV     CX,DX                   ;Shorter length in CX

UcEqLen:INC     BX                      ;Equal or greater

UcComp: JCXZ    UcDone                  ;Done if either is empty

Start:  LODSB                           ;S1[?] into AL
        Upcase                          ;convert to upper case
        MOV     AH,ES:[DI]              ;S2[?] into AH
        INC     DI                      ;Point ES:DI to next char in S2
        XCHG    AH,AL                   ;convert to upper case
        Upcase
        CMP     AH,AL                   ;Compare until no match
        LOOPE   Start

        JE      UcDone                  ;If Equal, result ready based on length

        MOV     BL,2
        JA      UcDone                  ;A1 Greater? Return 2
        XOR     BX,BX                   ;Else A1 Less, Return 0

UcDone: MOV     AX,BX                   ;Final result into AX
        POP     DS                      ;Restore DS
        Exit_Code 8                     ;!!.13

CompUCAsc       ENDP

CODE    ENDS

        END
