;******************************************************
;                 OPREPLAY.ASM 1.30
;     Copyright (c) TurboPower Software 1989, 1992.
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

        EXTRN   CurrentMacroPtr : DWORD
        EXTRN   MacroInProgressPtr : DWORD
        EXTRN   CSToDSInc : WORD                ;!!.20

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  Int16, Int16Init, SetEndOfMacroProc
        PUBLIC  SetWaitCount                    ;!!.20

SaveInt16       Pointer <>                      ;Saved INT $16 vector

MacroInProgress DB      0                       ;True if in a macro

WaitCount       DB      0                       ;!!.13
WaitFlag        DB      0                       ;!!.13

CurrentMacro    Pointer <>                      ;Pointer to a macro
EndOfMacroProc  Pointer <>                      ;Proc to call at end of macro

CSToDSIncCS     DW      0                       ;!!.20

;Set DS to a writeable alias for the current code segment !!.20
AliasCStoDS     MACRO
                PUSH    AX
                MOV     AX,CS
                ADD     AX,CS:CSToDSIncCS
                MOV     DS,AX
                POP     AX
                ENDM

;****************************************************** Int16

;procedure Int16;       ;!!.02  almost completely rewritten for 1.02

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

ReadNext        =       0
CharReady       =       1

Int16   PROC FAR

        CLI                             ;Make sure interrupts are off
        PUSHF                           ;Save flags
        TEST    AH,11101110b            ;Is the function 0, 1, 10, or 11?
        JZ      Setup                   ;If so, get set up
        FakePOPF                        ;Else restore flags...
        JmpFar  CS:SaveInt16            ;And chain to old ISR

Setup:
        SaveAllNoFlags                  ;Save all registers but Flags
        MOV     BP,SP                   ;Set up stack frame
        PUSH    AX                      ;Save AX (ExtraAX)

StartCheck0:                            ;!!.20
        AliasCStoDS                     ;!!.20
        assume  DS:Nothing              ;!!.20 (DS = Code segment alias)

StartCheck:
        STI                             ;Turn interrupts on
        CMP     CS:MacroInProgress,True ;Are we in a macro?
        JE      GetNextKey              ;If so, return the next key

        MOV     AX,ExtraAX              ;Call character ready service
        OR      AH,1
        PUSHF                           ;Push flags
        CLI                             ;Interrupts off
        CallFar CS:SaveInt16            ;Emulate interrupt
        JNZ     HaveKey                 ;If ZF not set, we have a key to return
        TEST    ExtraAH,1               ;Was this a char ready function?
        JZ      StartCheck              ;If not, check again
        OR      OldFlags,ZF             ;Else, set ZF to indicate failure
        JMP     Int16Exit               ;And return

GetNextKey:
        LES     DI,CS:CurrentMacro      ;ES:DI points to next word in macro
        MOV     BX,2                    ;BX = 2
        MOV     AX,ES:[DI]              ;AX = next word
        CMP     AX,EndOfMacro           ;At end of macro?
        JNE     HaveMacro               ;If not, return the key

        MOV     DS:MacroInProgress,False;Else, stop the macro !!.20
        MOV     AX,CS:EndOfMacroProc.Segm  ;Is there a user proc to call?
        OR      AX,CS:EndOfMacroProc.Ofst
        JZ      StartCheck              ;If not, start over

        STI                             ;Turn on interrupts
        MOV     AX,DATA                 ;Set up our DS
        MOV     DS,AX

        assume  DS:DATA                 ;!!.20

        CallFar CS:EndOfMacroProc       ;Call the user proc

        JMP     SHORT StartCheck0       ;Start over !!.20

        assume  DS:Nothing              ;!!.20 (DS = Code segment alias)

HaveKey:
        TEST    ExtraAH,1               ;Was this a char ready function?
        JNZ     NoEmulateInt            ;If so, don't emulate int

        MOV     AX,ExtraAX              ;Load AX with proper function
        PUSHF                           ;Push flags...
        CLI                             ;Interrupts off
        CallFar CS:SaveInt16            ;and call old ISR

NoEmulateInt:
        SetZero BX                      ;BX = 0
HaveMacro:
        MOV     AXreg,AX                ;Put the key to return in AXreg
        TEST    ExtraAH,1               ;Was this a char ready function?
        JNZ     DoneCharReady           ;If so, finish it up
        ADD     DS:CurrentMacro.Ofst,BX ;Else, remove the key from the macro !!.20
        JMP     SHORT Int16Exit         ;And return

DoneCharReady:
;!!.13 !!.20 begin
        CMP     CS:MacroInProgress,True
        JNE     DoIt
        CMP     BYTE PTR CS:WaitCount,0
        JE      DoIt
        PUSH    BX
        MOV     BL,CS:WaitFlag
        INC     BL
        MOV     DS:WaitFlag,BL
        CMP     BL,CS:WaitCount
        POP     BX
        JBE     SkipIt
        MOV     DS:WaitFlag,0
DoIt:   ;Have a character. Return with no zero flag and the char in AXreg
        AND     OldFlags,NotZF          ;Clear the zero flag
        JMP     SHORT Int16Exit
SkipIt: ;Have a character, but don't want to return it yet
        OR      OldFlags,ZF
        MOV     AX,ExtraAX
        MOV     AXReg,AX
;!!.13 !!.20 end

Int16Exit:
        STI                             ;Turn on interrupts
        MOV     SP,BP                   ;Restore SP
        RestoreAllRegs                  ;Restore all registers
        IRET                            ;Return from interrupt

        assume  DS:DATA

Int16   ENDP

;****************************************************** Int16Init

;procedure Int16Init;

;Initialization routine for Int16
;!!.20 modified substantially for pmode

Int16Init       PROC NEAR

        MOV     BX,CStoDSInc
        PUSH    DS
        MOV     CX,CS
        ADD     CX,BX
        MOV     DS,CX
        MOV     DS:CStoDSIncCS,BX
        GetVector       16h, DS:SaveInt16       ;Save INT 16 vector
        POP     DS
        SetPtrByOfst    CurrentMacroPtr, CX, CurrentMacro
        SetPtrByOfst    MacroInProgressPtr, CX, MacroInProgress
        RET

Int16Init       ENDP

;****************************************************** SetEndOfMacroProc

;procedure SetEndOfMacroProc(P : Pointer);

;Set pointer to procedure to call when end of macro reached
;!!.20 modified substantially for pmode

UserProc        EQU     DWORD PTR SS:[BX+4]

SetEndOfMacroProc       PROC FAR
        StackFrame
        LES     DI,UserProc                     ;Point to the procedure
        PUSH    DS
        AliasCStoDS
        MOV     DS:EndOfMacroProc.Ofst,DI       ;!!.10
        MOV     DS:EndOfMacroProc.Segm,ES
        POP     DS
        RET     4
SetEndOfMacroProc       ENDP

;****************************************************** SetWaitCount
;!!.20 moved to assembly language

; procedure SetWaitCount(Count : Byte);

UserCount       EQU     BYTE PTR [BP+6]

SetWaitCount            PROC FAR
        PUSH    BP
        MOV     BP,SP
        MOV     BL,UserCount
        CMP     BL,255
        JB      WaitOK
        DEC     BL
WaitOK: PUSH    DS
        AliasCStoDS
        MOV     DS:WaitCount,BL
        POP     DS
        POP     BP
        RET     2
SetWaitCount            ENDP

CODE    ENDS

        END
