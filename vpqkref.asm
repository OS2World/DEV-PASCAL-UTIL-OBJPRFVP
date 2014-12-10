;******************************************************
;                 OPQKREF.ASM 1.30
;     Copyright (c) TurboPower Software 1988, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE VPCOMMON.ASM

;****************************************************** Code

CODE32  SEGMENT DWORD PUBLIC 'CODE'

        ASSUME  CS:CODE32

        PUBLIC  EscapeSequence

;****************************************************** Key table

;packed table of key names for IBM extended keystroke sequences

Keys    LABEL BYTE
        db   6, "^Break"              ;0
        db   6, "AltEsc"              ;1
Null    LABEL BYTE
        db   0                        ;2
        db   2, "^@"                  ;3
        db   0                        ;4
        db   0                        ;5
        db   0                        ;6
        db   0                        ;7
        db   0                        ;8
        db   0                        ;9
        db   0                        ;10
        db   0                        ;11
        db   0                        ;12
        db   0                        ;13
        db   7, "AltBksp"             ;14
        db   5, "ShTab"               ;15
        db   4, "AltQ"                ;16
        db   4, "AltW"                ;17
        db   4, "AltE"                ;18
        db   4, "AltR"                ;19
        db   4, "AltT"                ;20
        db   4, "AltY"                ;21
        db   4, "AltU"                ;22
        db   4, "AltI"                ;23
        db   4, "AltO"                ;24
        db   4, "AltP"                ;25
        db   4, "Alt["                ;26
        db   4, "Alt]"                ;27
        db   8, "AltEnter"            ;28
        db   0                        ;29
        db   4, "AltA"                ;30
        db   4, "AltS"                ;31
        db   4, "AltD"                ;32
        db   4, "AltF"                ;33
        db   4, "AltG"                ;34
        db   4, "AltH"                ;35
        db   4, "AltJ"                ;36
        db   4, "AltK"                ;37
        db   4, "AltL"                ;38
        db   4, "Alt;"                ;39
        db   4, "Alt'"                ;40
        db   4, "Alt`"                ;41
        db   0                        ;42
        db   4, "Alt\"                ;43
        db   4, "AltZ"                ;44
        db   4, "AltX"                ;45
        db   4, "AltC"                ;46
        db   4, "AltV"                ;47
        db   4, "AltB"                ;48
        db   4, "AltN"                ;49
        db   4, "AltM"                ;50
        db   4, "Alt,"                ;51
        db   4, "Alt."                ;52
        db   4, "Alt/"                ;53
        db   0                        ;54
        db   0                        ;55
        db   0                        ;56
        db   0                        ;57
        db   0                        ;58
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
        db   0                        ;69
        db   0                        ;70
        db   4, "Home"                ;71
        db   2, "Up"                  ;72
        db   4, "PgUp"                ;73
        db   0                        ;74
        db   4, "Left"                ;75
        db   6, "Center"              ;76
        db   5, "Right"               ;77
        db   0                        ;78
        db   3, "End"                 ;79
        db   4, "Down"                ;80
        db   4, "PgDn"                ;81
        db   3, "Ins"                 ;82
        db   3, "Del"                 ;83
        db   4, "ShF1"                ;84
        db   4, "ShF2"                ;85
        db   4, "ShF3"                ;86
        db   4, "ShF4"                ;87
        db   4, "ShF5"                ;88
        db   4, "ShF6"                ;89
        db   4, "ShF7"                ;90
        db   4, "ShF8"                ;91
        db   4, "ShF9"                ;92
        db   5, "ShF10"               ;93
        db   3, "^F1"                 ;94
        db   3, "^F2"                 ;95
        db   3, "^F3"                 ;96
        db   3, "^F4"                 ;97
        db   3, "^F5"                 ;98
        db   3, "^F6"                 ;99
        db   3, "^F7"                 ;100
        db   3, "^F8"                 ;101
        db   3, "^F9"                 ;102
        db   4, "^F10"                ;103
        db   5, "AltF1"               ;104
        db   5, "AltF2"               ;105
        db   5, "AltF3"               ;106
        db   5, "AltF4"               ;107
        db   5, "AltF5"               ;108
        db   5, "AltF6"               ;109
        db   5, "AltF7"               ;110
        db   5, "AltF8"               ;111
        db   5, "AltF9"               ;112
        db   6, "AltF10"              ;113
        db   6, "^PrtSc"              ;114
        db   5, "^Left"               ;115
        db   6, "^Right"              ;116
        db   4, "^End"                ;117
        db   5, "^PgDn"               ;118
        db   5, "^Home"               ;119
        db   4, "Alt1"                ;120
        db   4, "Alt2"                ;121
        db   4, "Alt3"                ;122
        db   4, "Alt4"                ;123
        db   4, "Alt5"                ;124
        db   4, "Alt6"                ;125
        db   4, "Alt7"                ;126
        db   4, "Alt8"                ;127
        db   4, "Alt9"                ;128
        db   4, "Alt0"                ;129
        db   4, "Alt-"                ;130
        db   4, "Alt="                ;131
        db   5, "^PgUp"               ;132
        db   3, "F11"                 ;133
        db   3, "F12"                 ;134
        db   5, "ShF11"               ;135
        db   5, "ShF12"               ;136
        db   4, "^F11"                ;137
        db   4, "^F12"                ;138
        db   6, "AltF11"              ;139
        db   6, "AltF12"              ;140
        db   3, "^Up"                 ;141
        db   6, "^Minus"              ;142
        db   7, "^Center"             ;143
        db   5, "^Plus"               ;144
        db   5, "^Down"               ;145
        db   4, "^Ins"                ;146
        db   4, "^Del"                ;147
        db   4, "^Tab"                ;148
        db   0                        ;149
        db   0                        ;150
        db   7, "AltHome"             ;151
        db   5, "AltUp"               ;152
        db   7, "AltPgUp"             ;153
        db   0                        ;154
        db   7, "AltLeft"             ;155
        db   9, "AltCenter"           ;156
        db   8, "AltRight"            ;157
        db   0                        ;158
        db   6, "AltEnd"              ;159
        db   7, "AltDown"             ;160
        db   7, "AltPgDn"             ;161
        db   6, "AltIns"              ;162
        db   6, "AltDel"              ;163
        db   0                        ;164
        db   6, "AltTab"              ;165
        db   0

;Pseudo-scan codes for TPMOUSE
Mouse LABEL BYTE
        db   8, "ClkThree"            ;$E9
        db   8, "ClkRtCtr"            ;$EA
        db   8, "ClkLtCtr"            ;$EB
        db   9, "ClkCenter"           ;$EC
        db   7, "ClkBoth"             ;$ED
        db   8, "ClkRight"            ;$EE
        db   7, "ClkLeft"             ;$EF
        db   0

;*************************************************** GetEscapeSequence

;function EscapeSequence(B : Byte) : StringPointer;

;Return a pointer to a text string representing extended scan code B

ScanCode        EQU     BYTE PTR [EBX+8]

EscapeSequence  PROC

        VPStackFrame
        PUSH    ESI

        MOVZX   ECX,ScanCode
        MOV     ESI,offset Keys         ;ESI points to Keys
        CMP     CL,165                  ;CL <= 165?
        JBE     Short EsSearch
        CMP     CL,0E9h                 ;CL >= $E9?
        JB      Short EsNull
        CMP     CL,0EFh                 ;CL <= $EF?
        JA      Short EsNull
        MOV     ESI,offset Mouse        ;point to mouse table
        SUB     CL,0E9h                 ;make CL relative to $E9
        JMP     SHORT EsSearch          ;now do the search

EsNull:
        MOV     ESI,offset Null         ;ESI points to Null
        JMP     SHORT EsDone

EsSearch:
        JECXZ    EsDone                 ;Character #0? We're done
        CLD                             ;Forward direction
        XOR      EAX,EAX                ;Clear length word

EsNext:
        LODSB                           ;Length byte into AL
        ADD     ESI,EAX                 ;Skip over string
        LOOP    EsNext                  ;Repeat for number of characters

EsDone:
        MOV     EAX,ESI                 ;SI points to length byte of string
        POP     ESI
        POP     EBX
        RET     4

EscapeSequence  ENDP

CODE32  ENDS

        END

