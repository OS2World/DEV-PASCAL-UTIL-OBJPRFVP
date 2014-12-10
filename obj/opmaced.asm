;******************************************************
;                   OPMACED.ASM 1.30
;                     Macro editor
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT WORD PUBLIC

        ;Pascal variables

        EXTRN   TempMacro : BYTE                ;temp macro used by editor
        EXTRN   LoCol : WORD                    ;dimensions for the editing
        EXTRN   HiCol : WORD                    ; window
        EXTRN   LoRow : WORD
        EXTRN   HiRow : WORD
        EXTRN   KnownKeyPtr : WORD              ;last Key whose location was
                                                ; calculated
        EXTRN   KnownRow : WORD                 ;row for KnownKeyPtr
        EXTRN   KnownCol : WORD                 ;column for KnownKeyPtr
        EXTRN   RegKeyLength : BYTE             ;length table for regular keys
        EXTRN   AuxKeyLength : BYTE             ;length table for aux. keys


DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  ComputeScreenPos

;****************************************************** ComputeScreenPos

;procedure ComputeScreenPos(KeyPtr : Word; var Row, Col : Word);

;Compute the Row and Col positions for KeyPtr

KeyPtr          EQU     WORD PTR [BP+12]
Row             EQU     DWORD PTR [BP+8]
Col             EQU     DWORD PTR [BP+4]

ComputeScreenPos        PROC NEAR               ;!!.11 rewritten

        StackFrameBP
        MOV     SI,KeyPtr                       ;SI = KeyPtr
        MOV     DI,KnownKeyPtr                  ;DI has KnownKeyPtr
        CMP     SI,DI                           ;KeyPtr < KnownKeyPtr?
        JB      FirstKey                        ;If so, start with 1st key
        MOV     DX,KnownRow                     ;DX has KnownRow
        MOV     CX,KnownCol                     ;CX has KnownCol
        JMP     NextKey                         ;Start searching

FirstKey:
        MOV     DI,1                            ;KnownKeyPtr = 1
        MOV     DX,LoRow                        ;KnownRow = LoRow
        MOV     CX,LoCol                        ;KnownCol = LoCol

NextKey:
        CMP     DI,SI                           ;KnownKeyPtr = KeyPtr?
        JE      Done                            ;If so, done

        ;get the length of the next key in the macro

        SHL     DI,1                            ;Key array is array of words
        MOV     BX,WP TempMacro[DI]             ;Key := TempMacro.KeyArray[KnownKeyPtr]
                                                ;Note: we're treating NumKeys
                                                ;as the 0th element of the array
        SHR     DI,1                            ;undo the shift left
        CMP     BL,0E0h                         ;Is it an enhanced keyboard key?
        JE      NotRegular                      ;If so, not regular
        OR      BL,BL                           ;Lo(Key) = 0?
        JNZ     RegularKey                      ;if not, regular key
NotRegular:
        MOV     BL,BH                           ;BL = scan code
        SetZero BH                              ;BX = scan code
        MOV     BL,AuxKeyLength[BX]             ;BL = length of string
        JMP     SHORT ComputeRowCol             ;compute next row and column

RegularKey:                                     ;!!.11 this section rewritten
        CMP     BL,1Bh                          ;is it a ctrl char?
        JA      RK5                             ;if not, skip ahead
        CMP     BX,0E08h                        ;is it BkSp?
        JNE     RK1
        MOV     BL,6
        JMP     SHORT ComputeRowCol
RK1:    CMP     BX,0F09h                        ;is it Tab?
        JNE     RK2
        MOV     BL,5
        JMP     SHORT ComputeRowCol
RK2:    CMP     BX,1C0Ah                        ;is it ^Enter?
        JNE     RK3
        MOV     BL,8
        JMP     SHORT ComputeRowCol
RK3:    CMP     BX,1C0Dh                        ;is it Enter?
        JNE     RK4
        MOV     BL,7
        JMP     SHORT ComputeRowCol
RK4:    CMP     BX,011Bh                        ;is it Esc?
        JNE     RK5
        MOV     BL,5
        JMP     SHORT ComputeRowCol
RK5:    SetZero BH                              ;BH = 0
        MOV     BL,RegKeyLength[BX]             ;BL = length of string

ComputeRowCol:
        INC     DI                              ;increment KnownKeyPtr
        SetZero BH                              ;BH = 0
        MOV     AX,CX                           ;AX = KnownCol
        ADD     AX,BX                           ;AX = KnownCol + Len
        CMP     AX,HiCol                        ;AX > HiCol?
        JA      NewRow                          ;If so, new row

        ADD     CX,BX                           ;KnownCol = KnownCol + length
        JMP     SHORT NextKey                   ;next key

NewRow:
        INC     DX                              ;Increment KnownRow
        MOV     CX,LoCol                        ;KnownCol = LoCol
        ADD     CX,BX                           ;KnownCol = LoCol + length
        JMP     SHORT NextKey                   ;next key

Done:
        MOV     KnownKeyPtr,DI                  ;Save KnownKeyPtr
        MOV     KnownCol,CX                     ;Save KnownCol
        MOV     KnownRow,DX                     ;Save KnownRow
        LES     DI,Col                          ;ES:DI => Col
        MOV     ES:[DI],CX                      ;Col = KnownCol
        LES     DI,Row                          ;ES:DI => Row
        MOV     ES:[DI],DX                      ;Row = KnownRow
        Exit_Code 10                            ;!!.13

ComputeScreenPos        ENDP

CODE    ENDS

        END
