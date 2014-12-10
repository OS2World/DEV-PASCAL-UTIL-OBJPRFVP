;******************************************************
;                   OPBM.ASM 1.30
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

        PUBLIC  BMMakeTable, BMSearch, BMSearchUC

        EXTRN   UpCasePrim : PROC

Upcase  MACRO                           ;UpCase character in AL
        CALL   UpCasePrim
        ENDM

;****************************************************** BMMakeTable

;  procedure BMMakeTable(MatchString : string; var BT : BTable);
;    Build Boyer-Moore link table
;    BTable is array[0..255] of byte;

MString EQU    DWORD PTR [EBX+0ch]
BTable  EQU    DWORD PTR [EBX+08h]

BMMakeTable   PROC

        VPStackFrame
        PUSH   ESI
        PUSH   EDI
        CLD                             ;Go forward

        MOV    ESI,MString              ;ESI => MatchString
        MOV    EDI,BTable               ;EDI => BY
        MOV    EBX,EDI                  ;Save DI in EBX
        LODSB                           ;AL = length(MatchString)
        MOV    AH,AL                    ;Copy it to AH
        MOV    ECX,128                  ;Number of words in BT
        REP    STOSW                    ;Fill BT with length(MatchString)
        CMP    AL,1                     ;Is length(MatchString) <= 1?
        JBE    MTDONE                   ;Yes, we're done

        MOV    EDI,EBX                  ;Restore base of table from EBX
        XOR    EBX,EBX                  ;BH = 0
        MOV    CL,AL                    ;ECX = length(MatchString)
        DEC    ECX                      ;ECX = length(MatchString)-1

MTnext: LODSB                           ;AL = MatchString[i]
        MOV    BL,AL                    ;BL = MatchString[i]
        MOV    [EBX+EDI],CL             ;BTable[char] = length(MatchString)-i
        LOOP   MTnext                   ;Repeat for all characters in MatchString

MTDone:
        POP    EDI
        POP    ESI
        POP    EBX
        RET    8

BMMakeTable   ENDP

;****************************************************** BMSearch

;  function BMSearch(var Buffer; BufLength : Word; BT : BTable; MatchString : string) : Word;
;    Search Buffer for MatchString
;    Return FFFF for failure
;    Else return number of bytes searched to find MatchString

MString EQU    DWORD PTR [EBP+08h]
BTable  EQU    DWORD PTR [EBP+0Ch]
BufSize EQU    DWORD PTR [EBP+10h]
Buffer  EQU    DWORD PTR [EBP+14h]

BMSearch   PROC

        VPStackFrameBP
        PUSH   ESI
        PUSH   EDI
        PUSH   EBX
        PUSH   EBP                      ;Will use EBP for temp storage later

        MOV    ECX,BufSize              ;ECX = Buffer size
        MOV    EDI,Buffer               ;EDI => Buffer
        MOV    EBX,BTable               ;EBX => BTable
        MOV    ESI,MString              ;ESI => MatchString
        MOV    EBP,EAX                  ;Keep BTable segment in BP

        XOR    EAX,EAX                  ;AX = 0
        MOV    EDX,EAX                  ;DX = 0
        MOV    DL,[ESI]                 ;DL = length(MatchString)
        CMP    DL,1                     ;Check for trivial cases
        JA     SHORT BMSinit            ;Do Boyer-Moore if longer than one char
        JB     SHORT BMSnotFound        ;Fail for empty string

        MOV    AL,[ESI+1]               ;AL = one and only char to find
        MOV    EBX,EDI                  ;Save offset of Buffer
        CLD                             ;Forward
        REPNE  SCASB                    ;Scan Buffer for AL
        JNE    SHORT BMSnotFound        ;Char wasn't found
        MOV    EAX,EDI                  ;EAX holds offset where char was found
        DEC    EAX                      ;Back up one
        SUB    EAX,EBX                  ;Subtract base offset of Buffer
        JMP    SHORT BMSdone            ;We're done

BMSinit:
        DEC    DL                       ;DX = length(MatchString)-1
        ADD    ESI,EDX                  ;ESI => MatchString[length(MatchString)-1]
        ADD    ECX,EDI                  ;ECX = offset of last char in buffer
        ADD    EDI,EDX                  ;DI => first position to search
        MOV    DH,[ESI+1]               ;DH = MatchString[length(MatchString)]
        STD                             ;Go backwards
        JMP    SHORT BMScomp            ;Skip link table first time

BMSnext:
        XLAT                            ;Get size of link at [EBX+AL]
        ADD    EDI,EAX                  ;Compute next place to search

BMScomp:
        JC     SHORT BMSnotFound        ;Done if overflowed 64K !!.03
        CMP    EDI,ECX                  ;At end of buffer?
        JAE    SHORT BMSnotFound        ;Done if so
        MOV    AL,[EDI]                 ;AL = next char to try
        CMP    DH,AL                    ;Does it match the end of MatchString?
        JNE    SHORT BMSnext            ;If not same, go back and try again

        PUSH   ECX                      ;Save end of buffer position
        DEC    EDI                      ;Start comparing one character before
        MOVZX  ECX,DL                   ;Compare length(MatchString)-1 characters
        REPE   CMPSB                    ;Compare backwards while matched
        JE     SHORT BMSfound           ;Matched!

        MOV    AL,DL                    ;Restore ESI,EDI,AL
        SUB    AL,CL
        ADD    ESI,EAX
        ADD    EDI,EAX
        INC    DI
        MOV    AL,DH                    ;Put matched char back in AL
        POP    ECX                      ;Restore end of buffer
        JMP    SHORT BMSnext            ;Try again

BMSfound:                               ;EDI points to start of match
        POP    EBP                      ;Remove end of buffer from stack
        POP    EBP                      ;Get frame pointer back
        SUB    EDI,Offset Buffer        ;Subtract buffer start address
        MOV    EAX,EDI
        INC    EAX                      ;Return 0 if found in first byte
        JMP    SHORT BMSDone2           ;We're done

BMSnotFound:
        XOR    EAX,EAX
        DEC    EAX                      ;Result = FFFF
BMSDone:                                ;Result returned in AX
        POP    EBP
BMSDone2:
        POP    EBX
        POP    EDI
        POP    ESI
        CLD
        VPExit_Code 10h                   ;!!.13

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

BMSearchUC   PROC

        VPStackFrameBP
        PUSH   ESI
        PUSH   EDI
        PUSH   EBX
        PUSH   EBP                       ;Will use BP for temp storage later

        MOV    ECX,BufSize               ;CX = Buffer size
        MOV    EDI,Buffer                ;ES:DI => Buffer
        MOV    EBX,BTable                ;DS:BX => BTable
        MOV    ESI,MString               ;DS:SI => MatchString

        XOR    EAX,EAX                    ;AX = 0
        MOV    EDX,EAX                    ;DX = 0
        MOV    DL,[ESI]                  ;DL = length(MatchString)
        OR     DL,DL                    ;Check for trivial case
        JZ     SHORT BMSUNotFound             ;Fail for empty string

BMSUinit:
        DEC    DL                       ;DX = length(MatchString)-1
        ADD    ESI,EDX                    ;DS:SI => MatchString[length(MatchString)-1]
        ADD    ECX,EDI                    ;CX = offset of last char in buffer
        ADD    EDI,EDX                    ;ES:DI => first position to search
        MOV    DH,[ESI+1]                ;DH = MatchString[length(MatchString)]
        STD                             ;Go backwards
        JMP    SHORT BMSUcomp           ;Skip link table first time

BMSUnext:
        XLAT                            ;Get size of link at DS:[BX+AL]
        ADD    EDI,EAX                   ;Compute next place to search

BMSUcomp:
        JC     SHORT BMSUnotFound             ;Done if overflowed 64K !!.03
        CMP    EDI,ECX                    ;At end of buffer?
        JAE    SHORT BMSUnotFound             ;Done if so
        MOV    AL,[EDI]               ;AL = next char to try
        UpCase                          ;Raise it to uppercase
        CMP    DH,AL                    ;Does it match the end of MatchString?
        JNE    SHORT BMSUnext                 ;If not same, go back and try again

        PUSH   ECX                       ;Save end of buffer position
        DEC    DI                       ;Start comparing one character before
        MOVZX  ECX,DL                    ;Compare length(MatchString)-1 characters
        JCXZ   BMSUfound                ;Completely matched if CX = 0

BMSUcomp2:
        LODSB                           ;Next match character in AL
        MOV    AH,[EDI]               ;Next buffer character in AH
        DEC    EDI                       ;Decrement buffer index
        XCHG   AL,AH                    ;Uppercase it
        UpCase
        CMP    AH,AL                    ;A match?
        LOOPE  BMSUcomp2                ;Loop while AH=AL and CX<>0
        JE     BMSUfound                ;Matched!

        XOR    AH,AH                    ;Restore SI,DI,AX
        MOV    AL,DL
        SUB    AL,CL
        ADD    ESI,EAX
        ADD    EDI,EAX
        INC    EDI
        MOV    AL,DH                    ;Put matched char back in AL
        POP    ECX                       ;Restore end of buffer
        JMP    SHORT BMSUnext           ;Try again

BMSUfound:                              ;DI points to start of match
        POP    EBP                       ;End of buffer off stack
        POP    EBP                       ;Get frame pointer back
        SUB    EDI,Offset Buffer        ;Subtract buffer start address
        MOV    EAX,EDI
        INC    EAX                       ;Return 0 if found in first byte
        JMP    SHORT BMSUDone2          ;We're done

BMSUnotFound:
        XOR    EAX,EAX
        DEC    EAX
BMSUDone:                               ;Result returned in AX
        POP    EBP
BMSUDone2:
        POP    EBX
        POP    EDI
        POP    ESI
        CLD
        VpExit_Code 10h

BMSearchUC   ENDP

CODE32  ENDS

        END
