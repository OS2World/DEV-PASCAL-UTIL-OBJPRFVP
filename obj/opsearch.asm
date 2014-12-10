;******************************************************
;                  OPSEARCH.ASM 1.30
;              String handling routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

        PUBLIC  Search, SearchUC
        PUBLIC  ReplacePrim             ;!!.30

        EXTRN   UpCasePrim : FAR

UpcaseAL        MACRO                   ;UpCase character in AL
;               PUSH   BX               ;!!.02 UpCasePrim saves BX now
                CALL   UpCasePrim
;               POP    BX               ;!!.02 UpCasePrim saves BX now
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
MatLength       EQU     WORD PTR [BP+6]
Match           EQU     DWORD PTR [BP+8]
BufLength       EQU     WORD PTR  [BP+12]
Buffer          EQU     DWORD PTR [BP+14]

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

;Search for first character in Match
Next:   REPNE   SCASB                   ;Search forward for Match[1]
        JNE     Error                   ;Done if not found
        TEST    DX,DX                   ;If Length = 1 (DX = 0) ...
        JZ      Found                   ; the "string" was found

        ;Search for remainder of Match

        PUSH    CX                      ;Save CX
        PUSH    DI                      ;Save DI
        PUSH    SI                      ;Save SI

        MOV     CX,DX                   ;CX = Length(Match) - 1
        REPE    CMPSB                   ;Does rest of string match?

        POP     SI                      ;Restore SI
        POP     DI                      ;Restore DI
        POP     CX                      ;Restore CX

        JNE     Next                    ;Try again if no match

;Calculate number of bytes searched and return
Found:  DEC     DI                      ;DX = Offset where found
        MOV     AX,DI                   ;AX = Offset where found
        SUB     AX,BX                   ;Subtract starting offset
        JMP     SHORT SDone             ;Done

;Match was not found
Error:  XOR     AX,AX                   ;Return
        DEC     AX                      ;Return FFFF

SDone:  POP     DS                      ;Restore DS
        Exit_Code 12                    ;!!.13

Search  ENDP

;****************************************************** SearchUC

;function SearchUC(var Buffer; BufLength : Word;
;                  var Match;  MatLength : Word) : Word;

;Search through Buffer for Match (CASE-INSENSITIVE)
;BufLength is length of range to search.
;MatLength is length of string to match.
;Returns number of bytes searched to find Match, 0FFFFh if not found.

SearchUC  PROC FAR

        StackFrameBP
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward

        LES     DI,Buffer               ;ES:DI => Buffer
        MOV     BX,DI                   ;BX = Ofs(Buffer)

        MOV     CX,BufLength            ;CX = Length of range to scan
        MOV     DX,MatLength            ;DX = Length of match string

        TEST    DX,DX                   ;Length(Match) = 0?
        JZ      SUCError                ;If so, we're done

        LDS     SI,Match                ;DS:SI => Match buffer
        LODSB                           ;AL = Match[1]; DS:SI => Match[2]
        UpcaseAL                        ;Uppercase it
        DEC     DX                      ;DX = MatLength-1
        SUB     CX,DX                   ;CX = BufLength-(MatLength-1)
        JBE     SUCError                ;No match if BufLength is less

;Search for first character in Match
SUCNext:
        JCXZ    SUCError                ;done if CX is 0
        MOV     AH,ES:[DI]              ;Get next character of buffer
        INC     DI                      ;To next position
        UpcaseAH                        ;Uppercase it
        CMP     AH,AL                   ;A match?
        LOOPNE  SUCNext                 ;Loop while CX<>0 and AH<>AL
        JNE     SUCError                ;Done if not found
        OR      DX,DX                   ;If Length = 1 (DX = 0) ...
        JZ      SUCFound                ; the "string" was found

        ;Search for remainder of Match

        PUSH    AX                      ;Save AX
        PUSH    CX                      ;Save CX
        PUSH    DI                      ;Save DI
        PUSH    SI                      ;Save SI

        MOV     CX,DX                   ;CX = Length(Match) - 1
SUCNextM:
        LODSB                           ;Next match character in AL
        UpcaseAL                        ;Uppercase it
        MOV     AH,ES:[DI]              ;Next buffer character in AH
        INC     DI                      ;Increment index
        UpcaseAH                        ;Uppercase it
        CMP     AH,AL                   ;A match?
        LOOPE   SUCNextM                ;Loop while AH=AL and CX<>0

        POP     SI                      ;Restore SI
        POP     DI                      ;Restore DI
        POP     CX                      ;Restore CX
        POP     AX                      ;Restore AX

        JNE     SUCNext                 ;Try again if no match

;Calculate number of bytes searched and return
SUCFound: DEC   DI                      ;DX = Offset where found
        MOV     AX,DI                   ;AX = Offset where found
        SUB     AX,BX                   ;Subtract starting offset
        JMP     SHORT SUCDone           ;Done

;Match was not found
SUCError: XOR   AX,AX
        DEC     AX                      ;Return FFFF

SUCDone:POP     DS                      ;Restore DS
        Exit_Code 12                    ;!!.13

SearchUC  ENDP

;New - !!.30
;****************************************************** SearchUC

;procedure ReplacePrim(var Buffer; BufLength, Posn, SrcLen : Word;
;                      var Repl  ; RepLength : Word);

; Replace the SrcLen bytes of Buffer at Posn with the RepLength
; bytes in Repl.

; equates for parameters:
rpRepLength     EQU     WORD PTR  [BP+4 ]
rpRepl          EQU     DWORD PTR [BP+6 ]
rpSrcLen        EQU     WORD PTR  [BP+10]
rpPosn          EQU     WORD PTR  [BP+12]
rpBufLength     EQU     WORD PTR  [BP+14]
rpBuffer        EQU     DWORD PTR [BP+16]
rpBufferSeg     EQU     WORD PTR  [BP+18]

ReplacePrim     PROC NEAR
        StackFrameBP
        PUSH    DS                      ;save DS
        CLD                             ;go forward by default--may change

        MOV     DX,rpSrcLen             ;DX=# of bytes we're replacing
        OR      DX,DX                   ;replacement length 0?
        JZ      rpDone                  ;it's an error if so
        MOV     AX,rpRepLength          ;AX=Length of replacement buffer

        CMP     DX,AX                   ;source length > replacement length?
        JA      rpDeleteChars           ;if so, delete trailing bytes
        JE      rpMoveChars             ;if equal, just move replacement bytes
                                        ;otherwise, expand the buffer

        ;expand the buffer
        STD                             ;go backward
        LES     DI,rpBuffer             ;ES:DI->last byte in buffer
        ADD     DI,rpBufLength
        DEC     DI
        MOV     DS,rpBufferSeg          ;DS:SI->last byte in buffer
        MOV     SI,DI
        MOV     BX,AX                   ;BX=# of bytes to expand by
        SUB     BX,DX                   ;(RepLength - SrcLen)
        SUB     SI,BX                   ;DS:SI->first byte to move
        MOV     CX,rpBufLength          ;# of bytes to move=
        SUB     CX,rpPosn               ;BufLength-Posn-Bytes to delte
        SUB     CX,BX                   ;put # of bytes to move in CX
        REP     MOVSB                   ;move bytes for expansion
        CLD
        JMP     SHORT rpMoveChars

rpDeleteChars:
        ;delete characters from the buffer
        MOV     BX,DX                   ;calculate difference btw Src and Rep
        SUB     BX,AX

        MOV     CX,rpBufLength          ;figure out # of bytes to move
        SUB     CX,rpPosn
        SUB     CX,AX
        SUB     CX,BX

        LES     DI,rpBuffer             ;ES:DI->first byte to delete
        ADD     DI,rpPosn
        ADD     DI,AX

        MOV     DS,rpBufferSeg          ;DS:SI->first byte to move
        MOV     SI,DI
        ADD     SI,BX

        REP     MOVSB                   ;Move bytes, deleteing

rpMoveChars:
        OR      AX,AX                   ;any bytes to replace?
        JZ      rpDone                  ;no, we're done

        LES     DI,rpBuffer             ;ES:DI->replacement destination
        ADD     DI,rpPosn
        LDS     SI,rpRepl               ;DS:SI->replacement bytes
        MOV     CX,AX                   ;CX=replacement count
        REP     MOVSB                   ;move replacement bytes

rpDone:
        POP     DS                      ;Restore DS
        Exit_Code

ReplacePrim     ENDP

CODE    ENDS

        END
