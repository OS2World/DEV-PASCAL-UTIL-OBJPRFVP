;******************************************************
;                 OPSCAN.ASM 1.30
;     Copyright (c) TurboPower Software 1988, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE VPCOMMON.ASM

;****************************************************** Code

CODE32  SEGMENT DWORD PUBLIC 'CODE'

        ASSUME  CS:CODE32

        PUBLIC  GetScanCodeName

;****************************************************** Scan code table

;packed table of key names for scan codes

Scans LABEL BYTE
      db   3, "Esc"                 ;1
      db   1, "1"                   ;2
      db   1, "2"                   ;3
      db   1, "3"                   ;4
      db   1, "4"                   ;5
      db   1, "5"                   ;6
      db   1, "6"                   ;7
      db   1, "7"                   ;8
      db   1, "8"                   ;9
      db   1, "9"                   ;10
      db   1, "0"                   ;11
      db   1, "-"                   ;12
      db   1, "="                   ;13
      db   4, "Bksp"                ;14
      db   3, "Tab"                 ;15
      db   1, "Q"                   ;16
      db   1, "W"                   ;17
      db   1, "E"                   ;18
      db   1, "R"                   ;19
      db   1, "T"                   ;20
      db   1, "Y"                   ;21
      db   1, "U"                   ;22
      db   1, "I"                   ;23
      db   1, "O"                   ;24
      db   1, "P"                   ;25
      db   1, "["                   ;26
      db   1, "]"                   ;27
      db   5, "Enter"               ;28
      db   4, "Ctrl"                ;29
      db   1, "A"                   ;30
      db   1, "S"                   ;31
      db   1, "D"                   ;32
      db   1, "F"                   ;33
      db   1, "G"                   ;34
      db   1, "H"                   ;35
      db   1, "J"                   ;36
      db   1, "K"                   ;37
      db   1, "L"                   ;38
      db   1, ";"                   ;39
      db   1, "'"                   ;40
      db   1, "`"                   ;41
      db   7, "LtShift"             ;42
      db   1, "\"                   ;43
      db   1, "Z"                   ;44
      db   1, "X"                   ;45
      db   1, "C"                   ;46
      db   1, "V"                   ;47
      db   1, "B"                   ;48
      db   1, "N"                   ;49
      db   1, "M"                   ;50
      db   1, ","                   ;51
      db   1, "."                   ;52
      db   1, "/"                   ;53
      db   7, "RtShift"             ;54
      db   5, "PrtSc"               ;55
      db   3, "Alt"                 ;56
      db   5, "Space"               ;57
      db   7, "CapLock"             ;58
      db   2, "F1"                  ;59
      db   2, "F2"                  ;60
      db   2, "F3"                  ;61
      db   2, "F4"                  ;62
      db   2, "F5"                  ;63
      db   2, "F6"                  ;64
      db   2, "F7"                  ;65
      db   2, "F8"                  ;66
      db   2, "F9"                  ;67
      db   3, "F10"                 ;68
      db   7, "NumLock"             ;69
      db   7, "ScrLock"             ;70
      db   4, "Home"                ;71
      db   2, "Up"                  ;72
      db   4, "PgUp"                ;73
      db   5, "Minus"               ;74
      db   4, "Left"                ;75
      db   6, "Center"              ;76
      db   5, "Right"               ;77
      db   4, "Plus"                ;78
      db   3, "End"                 ;79
      db   4, "Down"                ;80
      db   4, "PgDn"                ;81
      db   3, "Ins"                 ;82
      db   3, "Del"                 ;83
      db   6, "SysReq"              ;84
      db   0                        ;85
      db   0                        ;86
      db   3, "F11"                 ;87
      db   3, "F12"                 ;88

Null    LABEL BYTE
      db   0

;*************************************************** GetScanCodeName

;function GetScanCodeName(B : Byte) : StringPointer;

;Return a pointer to a text string representing scan code B

ScanCode        EQU     BYTE PTR [EBX+8]

GetScanCodeName PROC

        VPStackFrame
        PUSH    ESI

        XOR     ECX,ECX
        MOV     CL,ScanCode             ;Get character to seek

        MOV     ESI,offset Scans        ;ESI points to Scans
        JECXZ   gscBad
        CMP     CL,88                   ;CL <= 84?
        JBE     gscSearch
gscBad:
        MOV     ESI,offset Null         ;ESI points to Null
        JMP     SHORT gscDone

gscSearch:
        DEC     ECX                     ;our array is 0-based
        JCXZ    gscDone                 ;done if CX = 0 now

        CLD                             ;Forward direction
        XOR     EAX,EAX                 ;Clear top half of length word

gscNext:
        LODSB                           ;Length byte into AL
        ADD     ESI,EAX                 ;Skip over string
        LOOP    gscNext                 ;Repeat for number of characters

gscDone:
        MOV     EAX,ESI                 ;ESI points to length byte of string
        POP     ESI
        POP     EBX
        RET     4

GetScanCodeName ENDP

CODE32  ENDS

        END

