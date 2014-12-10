;******************************************************
;                 OPMACRO.ASM 1.30
;            Object Professional Macro engine
;     Copyright (c) TurboPower Software 1987, 1992.
; Portions Copyright (c) Sunny Hill Software 1985, 1986
;     and used under license to TurboPower Software
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Equates

ZF              =       0000000001000000b       ;Zero flag mask
NotZF           =       1111111110111111b       ;Mask to clear ZF
EndOfMacro      =       0FFFFh                  ;Signals the end of a macro

;****************************************************** Data

DATA    SEGMENT WORD PUBLIC

        ;Pascal variables

        EXTRN   CurrentMacro : DWORD            ;Pointer to a macro
        EXTRN   MacroInProgress : BYTE          ;True if in a macro
        EXTRN   DefinedKeys : WORD              ;Array of defined keys
        EXTRN   OurMaxMacros : WORD             ;MaxMacros
        EXTRN   OurMaxKeys : WORD               ;MaxKeysInMacro
        EXTRN   MacroPointers : DWORD           ;Array of pointers to macros
        EXTRN   MacrosAreOn : BYTE              ;True if macros enabled
        EXTRN   MacroRecording : BYTE           ;True if recording a macro
        EXTRN   ScrapMacro : WORD               ;Used while recording
        EXTRN   PlaybackDelay : WORD            ;Delay factor during playback
        EXTRN   EndOfMacroProc : DWORD          ;Proc to call at end of macro
        EXTRN   CSToDSInc : WORD                ;!!.20
        EXTRN   WaitCount : BYTE                ;!!.20
        EXTRN   WaitFlag : BYTE                 ;!!.20

        ;Internal variables

        OneMS                   DW      ?       ;Loop count for 1 ms delay

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  Int16, Int16Init, EscapeSequence
        PUBLIC  SetWaitCount                    ;!!.20

SaveInt16       Pointer <>                      ;Saved INT $16 vector
OurDS           DW      DATA                    ;Our data segment

;****************************************************** DelayMS

;Delay one millisecond

DelayMS PROC NEAR

        PUSH    CX
        MOV     CX,OneMS                ;Loop count into CX
DelayMsLoop:
        LOOP    DelayMsLoop             ;Use up a millisecond
        POP     CX
        RET

DelayMS ENDP

;****************************************************** InitOneMS

;Initialize OneMs

InitOneMS       PROC NEAR

        ;set up delay count
        STI                             ;Force interrupts on
        MOV     AX,40h                  ;AX = $40
        MOV     ES,AX                   ;ES = $40
        MOV     DI,6Ch                  ;ES:DI => low word of BIOS timer count
        MOV     OneMS,55                ;Initial value for OneMS
        XOR     DX,DX                   ;DX = 0
        MOV     AX,ES:[DI]              ;AX = low word of timer

InitDelay1:
        CMP     AX,ES:[DI]              ;Keep looking at low word of timer
        JE      InitDelay1              ; until its value changes
        MOV     AX,ES:[DI]              ;Then save it

InitDelay2:
        CALL    DelayMs                 ;Delay for a count of OneMS (55)
        INC     DX                      ;Increment loop counter
        CMP     AX,ES:[DI]              ;Keep looping until the low word
        JE      InitDelay2              ; of the timer count changes again
        MOV     OneMS,DX                ;DX has new OneMS
        RET

InitOneMS       ENDP

;****************************************************** Int16

;procedure Int16;

COMMENT |
  This procedure is the macro engine. It takes over for BIOS INT $16.
  Currently, the engine does a simple replace.
|

OldFlags        EQU     WORD PTR [BP+24]        ;these are for referencing
OldCS           EQU     WORD PTR [BP+22]        ;saved registers
OldIp           EQU     WORD PTR [BP+20]
Flags           EQU     WORD PTR [BP+18]
ESreg           EQU     WORD PTR [BP+16]
DSreg           EQU     WORD PTR [BP+14]
DIreg           EQU     WORD PTR [BP+12]
SIreg           EQU     WORD PTR [BP+10]
BPreg           EQU     WORD PTR [BP+8]
DXreg           EQU     WORD PTR [BP+6]
CXreg           EQU     WORD PTR [BP+4]
BXreg           EQU     WORD PTR [BP+2]
AXreg           EQU     WORD PTR [BP+0]
AHreg           EQU     BYTE PTR [BP+1]
ALreg           EQU     BYTE PTR [BP+0]
ExtraAX         EQU     WORD PTR [BP-2]
ExtraAH         EQU     BYTE PTR [BP-1]
FuncType        EQU     BYTE PTR [BP-3]

ReadNext        =       0
CharReady       =       1

Int16   PROC FAR

        CLI                             ;Make sure interrupts are off
        PUSHF                           ;Save flags
        CMP     AH,10h                  ;Is it 10h?
        JE      Setup                   ;If so, set up
        CMP     AH,11h                  ;Is it 11h?
        JE      Setup                   ;If so, set up
        CMP     AH,CharReady            ;Function 0 or 1?
        JBE     Setup                   ;If so, set up
        FakePOPF                        ;Else restore flags...
        JmpFar  CS:SaveInt16            ;And chain to old ISR

Setup:
        SaveAllNoFlags                  ;Save all registers but Flags
        MOV     BP,SP                   ;Set up stack frame
        PUSH    AX                      ;Push extra copy of AX

        MOV     BX,AX                   ;BX = AX
        AND     BH,1                    ;BH = 0 or 1
        PUSH    BX                      ;Save this

        MOV     DS,CS:OurDS             ;DS = our DS

StartCheck:
        CMP     MacroInProgress,True    ;Are we in a macro?
        JNE     NotInMacro              ;If not, skip ahead

        LES     DI,CurrentMacro         ;ES:DI points to next word in macro
        MOV     AX,ES:[DI]              ;AX = next word
        CMP     AX,EndOfMacro           ;At end of macro?
        JE      AtEndOfMacro            ;If so, stop the macro
        MOV     AXreg,AX                ;Put the macro character in AXreg
        JMP     DoneCheck               ;Done with check

AtEndOfMacro:
        MOV     MacroInProgress,False   ;Stop the macro
        MOV     AX,EndOfMacroProc.Segm  ;Is there a user proc to call?
        OR      AX,EndOfMacroProc.Ofst
        JZ      NotInMacro
        CallFar EndOfMacroProc          ;Call it
        STI                             ;Turn on interrupts
        JMP     SHORT StartCheck        ;Check again for MacroInProgress

NotInMacro:
        MOV     AH,ExtraAH              ;Call character ready service
        OR      AH,1
        PUSHF                           ;Push flags
        CLI                             ;Interrupts off
        CallFar CS:SaveInt16            ;Emulate interrupt
        JNZ     HaveChar                ;If ZF not set, we have a char to return

        CMP     FuncType,CharReady      ;Processing char ready function?
        JNE     StartCheck              ;If not, check again
        OR      OldFlags,ZF             ;Else, set ZF to indicate failure
        JMP     Int16Exit               ;And return

HaveChar:
        MOV     AXreg,AX                ;Put the character in place
        SetZero SI                      ;SI = number of the macro we match
        CMP     MacrosAreOn,True        ;Are macros on?
        JNE     EndMacroSearch          ;If not, skip the search
        CMP     AX,EndOfMacro           ;Does AX = FFFF?
        JE      EndMacroSearch          ;If so, skip the search

        ;set up for the search
        MOV     BX,DS                   ;BX = DS
        MOV     ES,BX                   ;ES = DS
        MOV     DI,Offset DefinedKeys   ;ES:DI points to DefinedKeys
        MOV     CX,OurMaxMacros         ;CX = MaxMacros
        JCXZ    EndMacroSearch          ;if 0, don't check further
        MOV     BX,CX                   ;BX = MaxMacros
        DEC     BX                      ;DefinedKeys is 1-based
        SHL     BX,1                    ;DefinedKeys is array of words
        ADD     DI,BX                   ;ES:DI => DefinedKeys[MaxMacros]

        ;search for the macro corresponding to AX
        STD                             ;search backward
        REPNE   SCASW                   ;find macro
        CLD                             ;clear the direction flag
        JNE     EndMacroSearch          ;if not found, Match = 0
        INC     CX                      ;previous value of CX was the index
        MOV     SI,CX                   ;Match = CX

EndMacroSearch:
        OR      SI,SI                   ;Match = 0?
        JNZ     FoundMatch              ;If not continue
        MOV     AX,ExtraAX              ;Else, get back old AX
        MOV     AXreg,AX                ;Put it back
        CMP     FuncType,CharReady      ;Is this a character ready function?
        JE      ChainInt16              ;If so, don't record
        CMP     MacroRecording,True     ;Are we recording?
        JE      EmulateInt16            ;If so, emulate an INT $16

ChainInt16:
        STI                             ;Turn on interrupts
        MOV     SP,BP                   ;Restore SP
        RestoreAllRegs                  ;Restore all registers
        JmpFar  CS:SaveInt16            ;And chain to old ISR

EmulateInt16:
        PUSHF                           ;Push flags...
        CLI                             ;Interrupts off
        CallFar CS:SaveInt16            ;and call old ISR
        MOV     AXreg,AX                ;Return the character in AX
        JMP     SHORT RecordChar        ;And record it

FoundMatch:
        ;get rid of character that caused the macro
        MOV     AH,ExtraAH              ;Get next char function
        AND     AH,11111110b
        PUSHF                           ;Push flags
        CLI                             ;Interrupts off
        CallFar CS:SaveInt16            ;and call old ISR

        ;now start the macro
        DEC     SI                      ;SI back to 0-based format
        SHL     SI,1                    ;MacroPointers is array of double words
        SHL     SI,1
        LES     DI,MacroPointers[SI]    ;ES:DI => MacroPointers[Match]
        INC     DI                      ;Skip the length word
        INC     DI
        SetPtr  CurrentMacro, ES, DI    ;Set CurrentMacro
        MOV     MacroInProgress,True    ;Turn this macro on
        STI                             ;Turn interrupts on
        JMP     StartCheck              ;Process the first word in the macro

DoneCheck:
        ;if we get here, the character to return is in AX and AXreg
        CMP     FuncType,CharReady      ;Was this a Character Ready function?
        JE      DoneCharReady           ;If so, finish it up

DoneReadNext:
        ;A macro is in progress here, remove the character from the macro
        ADD     WP CurrentMacro,2       ;Increment CurrentMacro offset

        ;delay for PlaybackDelay milliseconds, if non-0
        MOV     CX,PlaybackDelay        ;CX = PlaybackDelay
        JCXZ    DontDelay               ;Don't delay if CX is 0
DelayLoop:
        CALL    DelayMS                 ;Delay one millisecond
        LOOP    DelayLoop               ;Repeat
DontDelay:

        ;if recording is on, save the character
        CMP     MacroRecording,True     ;Are we recording?
        JNE     Int16Exit               ;If not, we're done

RecordChar:
        MOV     DI,Offset ScrapMacro    ;DS:DI points to ScrapMacro
        MOV     BX,[DI]                 ;Get the length word
        CMP     BX,OurMaxKeys           ;Is the macro full?
        JAE     Int16Exit               ;If so, don't add to it
        INC     BX                      ;Else, increment the length word
        MOV     [DI],BX                 ;And save it
        SHL     BX,1                    ;BX is index into the array of words
        ADD     DI,BX                   ;                    !!.20
        MOV     [DI],AX                 ;Store the character !!.20
        MOV     AX,EndOfMacro           ;Terminate the macro !!.20
        MOV     [DI+2],AX               ;                    !!.20
        JMP     SHORT Int16Exit         ;And exit

DoneCharReady:
;!!.20 begin
        CMP     BYTE PTR WaitCount,0
        JE      DoIt
        PUSH    BX
        MOV     BL,WaitFlag
        INC     BL
        MOV     WaitFlag,BL
        CMP     BL,WaitCount
        POP     BX
        JBE     SkipIt
        MOV     WaitFlag,0
DoIt:   ;We have a character. Return with no zero flag and the char in AXreg
        AND     OldFlags,NotZF
        JMP     SHORT Int16Exit
SkipIt: ;Wait before pasting the current character
        OR      OldFlags,ZF
        MOV     AX,ExtraAX
        MOV     AXReg,AX
;!!.20 end

Int16Exit:
        STI                             ;Turn on interrupts
        MOV     SP,BP                   ;Restore SP
        RestoreAllRegs                  ;Restore all registers
        IRET                            ;Return from interrupt

Int16   ENDP

;****************************************************** Int16Init

;procedure Int16Init;

;Initialization routine for Int16
;!!.20 modified for pmode

Int16Init       PROC NEAR

        PUSH    DS
        MOV     AX,CS
        ADD     AX,CStoDSInc
        MOV     DS,AX
        GetVector 16h, DS:SaveInt16     ;Save INT 16 vector
        POP     DS
        CALL    InitOneMS               ;Initialize OneMS
        RET

Int16Init       ENDP

;****************************************************** Key table

;packed table of key names for IBM extended keystroke sequences

Keys  LABEL BYTE
      db   6, "^Break"              ;0
      db   6, "AltEsc"              ;1
QMark LABEL BYTE
      db   1, "?"                   ;2
      db   2, "^@"                  ;3
      db   1, "?"                   ;4
      db   1, "?"                   ;5
      db   1, "?"                   ;6
      db   1, "?"                   ;7
      db   1, "?"                   ;8
      db   1, "?"                   ;9
      db   1, "?"                   ;10
      db   1, "?"                   ;11
      db   1, "?"                   ;12
      db   1, "?"                   ;13
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
      db   4, "Ctrl"                ;29
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
      db   6, "LShift"              ;42
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
      db   6, "RShift"              ;54
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
      db   1, "?"                   ;149
      db   1, "?"                   ;150
      db   7, "AltHome"             ;151
      db   5, "AltUp"               ;152
      db   7, "AltPgUp"             ;153
      db   1, "?"                   ;154
      db   7, "AltLeft"             ;155
      db   9, "AltCenter"           ;156
      db   8, "AltRight"            ;157
      db   1, "?"                   ;158
      db   6, "AltEnd"              ;159
      db   7, "AltDown"             ;160
      db   7, "AltPgDn"             ;161
      db   6, "AltIns"              ;162
      db   6, "AltDel"              ;163
      db   1, "?"                   ;164
      db   6, "AltTab"              ;165
      db   0

;*************************************************** GetEscapeSequence

;function EscapeSequence(B : Byte) : StringPointer;

;Return a pointer to a text string representing extended scan code B

ScanCode        EQU     BYTE PTR SS:[BX+4]

EscapeSequence  PROC FAR

        StackFrame
        MOV     DX,DS                   ;Save DS

        SetZero CX
        MOV     CL,ScanCode             ;Get character to seek

        MOV     SI,offset Keys          ;CS:SI points to Keys
        JCXZ    EsDone                  ;Character #0? We're done

        CMP     CL,165                  ;CL <= 165?
        JBE     EsSearch
        MOV     SI,offset QMark         ;CS:SI points to QMark
        JMP     SHORT EsDone

EsSearch:
        MOV     AX,CS
        MOV     DS,AX                   ;DS:SI points to Keys
        CLD                             ;Forward direction
        SetZero AX                      ;Clear top half of length word

EsNext:
        LODSB                           ;Length byte into AL
        ADD     SI,AX                   ;Skip over string
        LOOP    EsNext                  ;Repeat for number of characters

EsDone:
        MOV     DS,DX                   ;Restore DS
        MOV     DX,CS                   ;Pointer Segment is current CS
        MOV     AX,SI                   ;SI points to length byte of string
        RET     2

EscapeSequence  ENDP

;****************************************************** SetWaitCount
;!!.20 added

; procedure SetWaitCount(Count : Byte);

UserCount       EQU     BYTE PTR [BP+6]

SetWaitCount            PROC FAR
        PUSH    BP
        MOV     BP,SP
        MOV     AL,UserCount
        CMP     AL,255
        JB      WaitOK
        DEC     AL      ;255 reduced to 254 to avoid byte overflow
WaitOK: MOV     WaitCount,AL
        POP     BP
        RET     2
SetWaitCount            ENDP

CODE    ENDS

        END

