;******************************************************
;                   PHTIMER.ASM 1.30
;               ASM routines for POPHELP
;   Copyright (c) TurboPower Software 1989, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

        PUBLIC  GetMoreBytes, WaitInit

;externals in other segments !!.03
        EXTRN   SetPopTicker : FAR

DATA    SEGMENT BYTE PUBLIC             ;!!.03
        EXTRN   BufferReadyPtr : DWORD
        EXTRN   ResidentMacroPtr : DWORD
DATA    ENDS                            ;!!.03

CODE    SEGMENT WORD PUBLIC
        ASSUME  CS:CODE, DS:DATA        ;!!.03

;----------------------------------------------------------------------------
;Config equates
ResidentMacroSize = 512                 ;Size of Resident macro buffer
TryTics           = 72                  ;SetTicker parm for try count

;CS vars
OneMSec         DW ?                    ;Loop count value for one millsecond
WaitCnt         DW ?                    ;Wait time for TSR to swap in
BufferReady     DB ?                    ;BufferReady semaphore
ResidentMacro   DW ResidentMacroSize+2 DUP (?)  ;Local macro storage
UserDataPtr     Pointer <>              ;Ptr to ThisIfc.UserData

;----------------------------------------------------------------------------
;WaitInit(OneMsec, WaitCnt : Word; UserDataPtr : Pointer)
;  - Sets the delay vars (from OpCrt)
;  - Sets the address of the BufferReady semaphore
;
OneMsecVal      EQU     WORD PTR [BP+12]
WaitCntVal      EQU     WORD PTR [BP+10]
UserDataSeg     EQU     WORD PTR [BP+8]
UserDataOfs     EQU     WORD PTR [BP+6]

WaitInit        PROC FAR
        PUSH    BP
        MOV     BP,SP

;Store the OneMsec and WaitCnt values in the code segement
        MOV     AX, OneMsecVal
        MOV     OneMsec, AX
        MOV     AX, WaitCntVal
        MOV     WaitCnt, AX

;Store the address of ThisIfc.UserData
        MOV     AX, UserDataSeg
        MOV     UserDataPtr.Segm, AX
        MOV     AX, UserDataOfs
        MOV     UserDataPtr.Ofst, AX

;Set the pascal pointer vars to BufferReady and ResidentMacro
        SetPtrByOfst    BufferReadyPtr, CS, BufferReady
        SetPtrByOfst    ResidentMacroPtr, CS, ResidentMacro

        MOV     SP,BP
        POP     BP
        RET     8                                                      ;!!.13
WaitInit ENDP

;----------------------------------------------------------------------------
;GetMoreBytes
; - Calls TSR back in to fill another macro buffer
;
GetMoreBytes PROC FAR

;Tell ExternalIfc that this is a paste command
        LDS     DI, UserDataPtr         ;Load ptr to ThisIfc.UserData
        MOV     WORD PTR [DI], 0        ;Set UserData to zero
        MOV     WORD PTR [DI+2], 0      ; (PasteBlock command)

;Request POPHELP to swap back in and refill buffer
        MOV     AX, TryTics             ;Try for TryTics to swap in POPHELP
        PUSH    AX                      ;Pass to SetPopTicker
        CALL    SetPopTicker            ;Request TSR to swap back in
        STI                             ;Turn interrupts back on

;Get ready to wait for POPHELP...
        MOV     BufferReady, 0          ;Clear semaphore
        MOV     DX, WaitCnt             ;Load iteration count
        XOR     DI, DI                  ;ES:DI is a dummy address that
        MOV     ES, DI                  ; never changes
        MOV     AL, ES:[DI]             ;AL is the value in the dummy addr
Waits:  MOV     CX, OneMsec             ;Loop for one msec

;Delay for one millisecond (duplicates code in OpCrt)
OneMs:  MOV     BX, 4
OMDec:  DEC     BX
        JNZ     OMDec
        CMP     AL, ES:[DI]
        LOOPE   OneMs

;Wait for WaitCnt milliseconds or semaphore set
        CMP     BufferReady, 1          ;Quit if semaphore set
        JE      JustLeave               ;...
        INT     028h                    ;Give POPHELP a chance to swap in
        DEC     DX                      ;Else keep looping
        JNZ     Waits

;Whether we have a new macro or not, we will just exit here.  If a new macro
;was started, OpReplay.Int16 will return the first character of that macro and
;then keep going.  If a new macro was not started, OpReplay.Int16 will just
;return empty from the int16 in progress now and no more macro characters
;will be returned

JustLeave:
        CLI                             ;Turn interrupts back off
        RET

GetMoreBytes    ENDP

;-----------------------------------------------------------------------------
CODE    ENDS
        END
