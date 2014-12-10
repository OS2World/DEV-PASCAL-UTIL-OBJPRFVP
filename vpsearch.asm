;******************************************************
;                  OPSEARCH.ASM 1.30
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

        PUBLIC  Search, SearchUC
        PUBLIC  ReplacePrim             ;!!.30

        EXTRN   UpCasePrim : PROC

UpcaseAL        MACRO                   ;UpCase character in AL
                CALL   UpCasePrim
                ENDM

UpcaseAH        MACRO                   ;UpCase character in AL
                XCHG    AL,AH
                UpcaseAL
                XCHG    AH,AL
                ENDM


;****************************************************** Search

;function Search(var Buffer; BufLength : Word;
;                var Match;  MatLength : Word) : Word;

;Search through Buffer for Match.
;BufLength is length of range to search.
;MatLength is length of string to match.
;Returns number of bytes searched to find Match, 0FFFFh if not found.

;equates for parameters:
MatLength       EQU     DWORD PTR [EBP+08h]
Match           EQU     DWORD PTR [EBP+0Ch]
BufLength       EQU     DWORD PTR [EBP+10h]
Buffer          EQU     DWORD PTR [EBP+14h]

Search  PROC

        VPStackFrameBP
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    ECX
        PUSH    EDX
        CLD                             ;Go forward

        MOV     EDI,Buffer              ;EDI => Buffer
        MOV     EBX,EDI                 ;EBX => Buffer

        MOV     ECX,BufLength           ;CX = Length of range to scan
        MOV     EDX,MatLength           ;DX = Length of match string

        TEST    EDX,EDX                 ;Length(Match) = 0?
        JZ      SHORT Error                   ;If so, we're done

        MOV     ESI,Match               ;ESI => Match buffer
        LODSB                           ;AL = Match[1]; DS:SI => Match[2]
        DEC     EDX                     ;DX = MatLength-1
        SUB     ECX,EDX                 ;CX = BufLength-(MatLength-1)
        JBE     SHORT Error             ;Error if BufLength is less

;Search for first character in Match
Next:   REPNE   SCASB                   ;Search forward for Match[1]
        JNE     SHORT Error             ;Done if not found
        TEST    EDX,EDX                 ;If Length = 1 (DX = 0) ...
        JZ      SHORT Found             ; the "string" was found

        ;Search for remainder of Match

        PUSH    ECX                     ;Save CX
        PUSH    EDI                     ;Save DI
        PUSH    ESI                     ;Save SI

        MOV     ECX,EDX                 ;CX = Length(Match) - 1
        REPE    CMPSB                   ;Does rest of string match?

        POP     ESI                     ;Restore ESI
        POP     EDI                     ;Restore EDI
        POP     ECX                     ;Restore ECX

        JNE     SHORT Next                    ;Try again if no match

;Calculate number of bytes searched and return
Found:  DEC     EDI                     ;EDX = Offset where found
        MOV     EAX,EDI                 ;EAX = Offset where found
        SUB     EAX,EBX                 ;Subtract starting offset
        JMP     SHORT SDone             ;Done

;Match was not found
Error:  XOR     EAX,EAX                 ;Return
        DEC     EAX                     ;Return FFFF

SDone:
        POP     EDX
        POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
        VPExit_Code 10h                   ;!!.13

Search  ENDP

;****************************************************** SearchUC

;function SearchUC(var Buffer; BufLength : Word;
;                  var Match;  MatLength : Word) : Word;

;Search through Buffer for Match (CASE-INSENSITIVE)
;BufLength is length of range to search.
;MatLength is length of string to match.
;Returns number of bytes searched to find Match, 0FFFFh if not found.

SearchUC  PROC

        VPStackFrameBP
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    ECX
        PUSH    EDX
        CLD                             ;Go forward

        MOV     EDI,Buffer               ;ES:DI => Buffer
        MOV     EBX,EDI                   ;BX = Ofs(Buffer)

        MOV     ECX,BufLength            ;CX = Length of range to scan
        MOV     EDX,MatLength            ;DX = Length of match string

        TEST    EDX,EDX                   ;Length(Match) = 0?
        JZ      SHORT SUCError                ;If so, we're done

        MOV     ESI,Match                ;DS:SI => Match buffer
        LODSB                           ;AL = Match[1]; DS:SI => Match[2]
        UpcaseAL                        ;Uppercase it
        DEC     EDX                      ;DX = MatLength-1
        SUB     ECX,EDX                   ;CX = BufLength-(MatLength-1)
        JBE     SHORT SUCError                ;No match if BufLength is less

;Search for first character in Match
SUCNext:
        JCXZ    SHORT SUCError                ;done if CX is 0
        MOV     AH,[EDI]              ;Get next character of buffer
        INC     EDI                      ;To next position
        UpcaseAH                        ;Uppercase it
        CMP     AH,AL                   ;A match?
        LOOPNE  SUCNext                 ;Loop while CX<>0 and AH<>AL
        JNE     SHORT SUCError                ;Done if not found
        OR      EDX,EDX                   ;If Length = 1 (DX = 0) ...
        JZ      SHORT SUCFound                ; the "string" was found

        ;Search for remainder of Match

        PUSH    EAX                      ;Save AX
        PUSH    ECX                      ;Save CX
        PUSH    EDI                      ;Save DI
        PUSH    ESI                      ;Save SI

        MOV     ECX,EDX                   ;CX = Length(Match) - 1
SUCNextM:
        LODSB                           ;Next match character in AL
        UpcaseAL                        ;Uppercase it
        MOV     AH,[EDI]              ;Next buffer character in AH
        INC     EDI                      ;Increment index
        UpcaseAH                        ;Uppercase it
        CMP     AH,AL                   ;A match?
        LOOPE   SUCNextM                ;Loop while AH=AL and CX<>0

        POP     ESI                      ;Restore SI
        POP     EDI                      ;Restore DI
        POP     ECX                      ;Restore CX
        POP     EAX                      ;Restore AX

        JNE     SHORT SUCNext                 ;Try again if no match

;Calculate number of bytes searched and return
SUCFound:
        DEC     EDI                      ;DX = Offset where found
        MOV     EAX,EDI                   ;AX = Offset where found
        SUB     EAX,EBX                   ;Subtract starting offset
        JMP     SHORT SUCDone           ;Done

;Match was not found
SUCError:
        XOR     EAX,EAX
        DEC     EAX                      ;Return FFFF

SUCDone:
        POP     EDX
        POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
        VPExit_Code 10h                   ;!!.13

SearchUC  ENDP

;New - !!.30
;****************************************************** SearchUC

;procedure ReplacePrim(var Buffer; BufLength, Posn, SrcLen : Word;
;                      var Repl  ; RepLength : Word);

; Replace the SrcLen bytes of Buffer at Posn with the RepLength
; bytes in Repl.

; equates for parameters:
rpRepLength     EQU     DWORD PTR [EBP+08h]
rpRepl          EQU     DWORD PTR [EBP+0Ch]
rpSrcLen        EQU     DWORD PTR [EBP+10h]
rpPosn          EQU     DWORD PTR [EBP+14h]
rpBufLength     EQU     DWORD PTR [EBP+18h]
rpBuffer        EQU     DWORD PTR [EBP+1Ch]

ReplacePrim     PROC
        VPStackFrameBP
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    ECX
        PUSH    EDX
        CLD                             ;go forward by default--may change

        MOV     EDX,rpSrcLen             ;DX=# of bytes we're replacing
        OR      EDX,EDX                   ;replacement length 0?
        JZ      SHORT rpDone                  ;it's an error if so
        MOV     EAX,rpRepLength          ;AX=Length of replacement buffer

        CMP     EDX,EAX                   ;source length > replacement length?
        JA      SHORT rpDeleteChars           ;if so, delete trailing bytes
        JE      SHORT rpMoveChars             ;if equal, just move replacement bytes
                                        ;otherwise, expand the buffer

        ;expand the buffer
        STD                             ;go backward
        MOV     EDI,rpBuffer             ;ES:DI->last byte in buffer
        ADD     EDI,rpBufLength
        DEC     EDI
        MOV     ESI,EDI
        MOV     EBX,EAX                   ;BX=# of bytes to expand by
        SUB     EBX,EDX                   ;(RepLength - SrcLen)
;        SUB     ESI,EBX                   ;DS:SI->first byte to move
        ADD     EDI,EBX                  ;BUGFIX!
        MOV     ECX,rpBufLength          ;# of bytes to move=
        SUB     ECX,rpPosn               ;BufLength-Posn-Bytes to delte
;        SUB     ECX,EBX                   ;put # of bytes to move in CX
        REP     MOVSB                   ;move bytes for expansion
        CLD
        JMP     SHORT rpMoveChars

rpDeleteChars:
        ;delete characters from the buffer
        MOV     EBX,EDX                   ;calculate difference btw Src and Rep
        SUB     EBX,EAX

        MOV     ECX,rpBufLength          ;figure out # of bytes to move
        SUB     ECX,rpPosn
        SUB     ECX,EAX
        SUB     ECX,EBX

        MOV     EDI,rpBuffer             ;ES:DI->first byte to delete
        ADD     EDI,rpPosn
        ADD     EDI,EAX

        MOV     ESI,EDI
        ADD     ESI,EBX

        REP     MOVSB                   ;Move bytes, deleteing

rpMoveChars:
        OR      EAX,EAX                   ;any bytes to replace?
        JZ      SHORT rpDone                  ;no, we're done

        MOV     EDI,rpBuffer             ;ES:DI->replacement destination
        ADD     EDI,rpPosn
        MOV     ESI,rpRepl               ;DS:SI->replacement bytes
        MOV     ECX,EAX                   ;CX=replacement count
        REP     MOVSB                   ;move replacement bytes

rpDone:
        POP     EDX
        POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
        VPExit_Code 18h

ReplacePrim     ENDP

CODE32  ENDS

        END
