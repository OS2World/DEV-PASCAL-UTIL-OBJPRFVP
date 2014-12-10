;******************************************************
;                   OPSWAPI.ASM 1.30
;           Include File for OPSWAP routines
;     Copyright (c) TurboPower Software 1987, 1992.
; Portions Copyright (c) Sunny Hill Software 1985, 1986
;     and used under license to TurboPower Software
;                All rights reserved.
;******************************************************

PopAdd          Pointer <>
TempBX          DW ?

PrepareToPop    PROC NEAR
        SetZero BX                      ;Zero BX
        MOV     BL,CS:PopupToCall       ;Get index of popup
        DEC     BX                      ;PopupAddrs is 1-based
        MOV     CS:TempBX,BX

        SHL     BX,1                    ;PopupAddrs is array of pointers
        SHL     BX,1                    ; so divide by 4

        MOV     AX,CS:PopupAddrs[BX].Segm   ; store in CS based pointer
        MOV     CS:PopAdd.Segm,AX
        MOV     AX,CS:PopupAddrs[BX].Ofst
        MOV     CS:PopAdd.Ofst,AX
        MOV     CS:PopupStackIndex,BX
        RET
PrepareToPop    ENDP

;****************************************************** TryPop
COMMENT |
  This procedure checks to see if the system is OK and, if so, calls the
  current popup.  It preserves all registers except the flags.

  On entry, the stack should look exactly as it did after the interrupt
  occurred, with no other data PUSHed, and the interrupt flag should be
  disabled.

  TryPop must not be CALLed; it must be jumped to. When finished, TryPop
  executes an IRET instruction.

  TryPop also contains the label DosOkToPop, which the interrupt $28 filter
  jumps to when it needs to pop up.
|

TryPop  PROC NEAR

        CLI                             ;No interrupts between chk and zero
        CMP     CS:PopupsEnabled,0      ;make sure popups are enabled
        JZ      SysNotOK
        CMP     CS:PopTicker,0          ;check this so we can be called without
                                        ;knowing
        JZ      SysNotOK                ;if not waiting, just return
        CMP     StateFlags,0            ;Check system state
        JE      SysOkTryPop             ;if not OK, pass on

SysNotOK:
        IRET                            ;return with non-zero flags

SysOkTryPop:
        MOV     CS:PopupsEnabled,0      ;no popups now
        STI                             ;Interrupts on
        PUSH    DS                      ;Save DS
        PUSH    BX                      ;Save BX
        PUSH    AX                      ;Save AX

        LDS     BX,CS:DosInUsePtr       ;DOS in use pointer in DS:BX
        MOV     AL,[BX]                 ;DOS byte in AL
        LDS     BX,CS:DosCriticalPtr    ;Check DOS critical flag
        OR      AL,[BX]                 ;they must both be zero
        POP     AX                      ;restore used regs
        POP     BX
        POP     DS
        JZ     DosOkToPop

ExitEnabled:
        MOV     CS:PopupsEnabled,1      ;reenable popups
        IRET

;If we get here then the DISK and DOS are fine.
DosOkToPop:
        MOV     CS:PopTicker,0          ;stop checking period
        STI                             ;interrupts on
        SaveAllRegs                     ;Save all registers

        CALL    PrepareToPop


        CALL    SetDosTraps             ;Set the traps and call pop-up

        MOV     BX,CS:TempBX            ;index value in BX

        MOV     CS:PopupInUse[BX],False    ;Popup is not in use


        RestoreAllRegs                  ;restore all registers
        MOV     CS:PopupsEnabled,1
        IRET                            ;return to caller

TryPop  ENDP

;****************************************************** DecPopTicker

;Check to see if we've timed out while waiting to pop up

DecPopTicker    PROC NEAR
        DEC     CS:PopTicker            ;Decrement ticker
        JNZ     DecDone

        PUSH    BX
        SetZero BH                      ;zero BH
        MOV     BL,CS:PopupToCall       ;get number of popup
        DEC     BX                      ;array has base of 1, convert to 0 base
        MOV     CS:PopupInUse[BX],False    ;Popup is not in use
        POP     BX                      ;restore registers
DecDone:
        STI                             ;interrupts on
        RET

DecPopTicker    ENDP

;****************************************************** Int8

COMMENT |
  Clock interrupt handler.
  ------------------------
  Traps the clock tick to see if it should activate a routine. If it can, it
  clears the clock tick, then calls the appropriate popup. If it can't, it
  chains to the previous INT $8 handler.
|

TempInt08       Pointer <>              ;Temporary address

Int8    PROC NEAR

        CMP     CS:PopTicker,0          ;Check to see if we're waiting to pop up
        JE      Int8Pass                ;if not, pass on interrupt

CheckPopClk:
        TEST    StateFlags,InInt08      ;check if we're in use
        JZ      Int8Try                 ;if not, try to pop up
        CALL    DecPopTicker            ;else, see if we timed out

Int8Pass:
        CLI                             ;interrupts off
        JmpFar  CS:OldInt08             ;pass on interrupt

Int8Try:
        OR      StateFlags,InInt08      ;now we are in use
        STI                             ;interrupts on
        POP     CS:TempInt08.Ofst       ;caller's offset
        POP     CS:TempInt08.Segm       ;caller's segment
        CLI                             ;interrupts off
        CALL    CS:OldInt08             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        STI                             ;interrupts on
        PUSHF                           ;caller's flags on stack
        PUSH    CS:TempInt08.Segm       ;caller's segment
        PUSH    CS:TempInt08.Ofst       ;caller's offset on stack

        CLI                             ;interrupts off
        CMP     CS:PopTicker,0          ;check again in case it expired
        JZ      NoTimeOut               ;return

        AND     StateFlags,NotIn08      ;clear clock-in-use bit
        JNZ     CheckTimeOut            ;not OK, pass on
        JMP     TryPop                  ;try to pop-up

CheckTimeOut:
        CALL    DecPopTicker            ;see if we've timed out

NoTimeOut:
        AND     StateFlags,NotIn08      ;clear clock-in-use bit
        IRET                            ;return to caller

Int8    ENDP

;****************************************************** Int9

COMMENT |
  Keyboard interrupt handler.
  ---------------------------
  Intercepts the keyboard hardware interrupt (INT 9) to determine when we should
  pop up. If the proper mask is active and one of the defined keys is struck,
  then this routine does the following:

  - sets PopupToCall to the array index of the key that was struck
  - sets PopTicker to the timeout value
  - resets the keyboard and returns

  Int8, Int16, and Int28 take over from there.
|

TempInt09       Pointer <>
RoutineNum      DB      0
BiosDataSeg     DW      40h

KbdData         =       60h
BiosShiftFlags  =       17h

Int9    PROC NEAR

        STI                             ;interrupts on
        PUSH    DS                      ;Save DS
        PUSH    AX                      ;Save AX
        PUSH    BX                      ;Save BX
        MOV     DS,CS:BiosDataSeg       ;Check BIOS data area
        MOV     AH,DS:[BiosShiftFlags]  ;BIOS status byte into AH
        AND     AH,00001111b            ;If none of these flags is set, can't
                                        ;be a hot key...
;!!.01  JZ      PassKbdInt              ;So exit

        IN      AL,KbdData              ;Get key struck
        CMP     AL,MaxScanCode          ;see if it's in our range
        JA      PassKbdInt              ;no, pass on

        MOV     DS,CS:OurDS             ;Reset DS
        SetZero BH                      ;BH = 0
        MOV     BL,AL                   ;BX has index into arrays of bytes
                                        ;based on scan codes
        MOV     AL,CS:PopupKeys[BX]        ;get popup handle in AL
        OR      AL,AL                   ;check for 0
        JZ      PassKbdInt              ;if 0, not a hot key
        CMP     AH,CS:ShiftKeys[BX]        ;Shift keys match?
        JE      AttemptPop              ;if so, try to POP up

PassKbdInt:
        POP     BX                      ;Restore BX
        POP     AX                      ;Restore AX
        POP     DS                      ;Restore DS
        TEST    StateFlags,InInt09      ;Check if we're in use
        JZ      TrackKbd                ;if no, set variable
        CLI                             ;interrupts off
        JmpFar  CS:OldInt09             ;else, just pass this on

TrackKbd:
        OR      StateFlags,InInt09      ;Set our bit
        POP     CS:TempInt09.Ofst       ;Offset of caller
        POP     CS:TempInt09.Segm       ;Segment of caller
        CLI                             ;interrupts off
        CallFar CS:OldInt09             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;Save flags
        CLI                             ;Ints off
        PUSH    CS:TempInt09.Segm       ;Segment of caller
        PUSH    CS:TempInt09.Ofst       ;Offset of caller

        AND     StateFlags,NotIn09      ;Reset our bit
        IRET                            ;return to caller

AttemptPop:
        SetZero AH                      ;zero in AH
        MOV     BX,AX                   ;get routine handle IN BX
        DEC     BX                      ;array has base of 1

        ;make sure it's all right to try to pop up

        CLI                             ;Ints off
        CMP     CS:PopupsEnabled,True   ;Are popups enabled?
        JNE     DontPop                 ;No? Eat the hotkey
        CMP     CS:PopupInUse[BX],AH       ;Popup already in use?
        JNE     DontPop                 ;Yes? Eat the hotkey
        CMP     CS:PopTicker,0          ;Something else waiting to pop up?
        JNE     DontPop                 ;Yes? Eat the hotkey

        ;checks went OK, we can set the pop ticker

        MOV     CS:PopupInUse[BX],True     ;This popup is in use now
        INC     BX                      ;Popup to call in BX
        MOV     CS:PopupToCall,BL       ;In PopupToCall
        MOV     AX,CS:PopTimeOut        ;timeout value in AX
        MOV     CS:PopTicker,AX         ;set PopTicker
DontPop:
        STI                             ;interrupts on
        ResetKbd                        ;reset keyboard (trashes AX)
        NullJump                        ;delay
        ResetPIC                        ;reset PIC port (trashes AX)
        POP     BX                      ;Restore BX
        POP     AX                      ;Restore AX
        POP     DS                      ;Restore DS
        IRET

Int9    ENDP

;****************************************************** Int16

COMMENT |
  BIOS keyboard interrupt handler.
  --------------------------------
  Intercepts the BIOS keyboard interrupt (INT 16) to allow popups to be
  activated while waiting for keyboard input.

  Also used to communicate data between TSR's written with Object Professional.
|

TempInt16       Pointer <>              ;local
IntraApp        =       00F0h           ;offset of intraapplication comm area

Int16   PROC NEAR
        STI                             ;Interrupts on
        CMP     AX,IfcSignature         ;See if this is a request for info
        JNE     NotIfcCheck1            ;If not, continue
        CMP     CS:IfcInstalled,True       ;Are we in charge of the interface?
        JNE     JumpOld16               ;If not, chain to old ISR

        NOT     AX                      ;Flip the bits in AX
        MovSeg  ES,CS                   ;ES = DS

        MOV     DI,Offset CS:ThisIfc    ;ES:DI points to ThisIfc
        MOV     WP ES:[DI].NextIfc,0    ;If we're answering this, we're the end
        MOV     WP ES:[DI].NextIfc+2,0  ; of the line, so NextIfc is nil
        IRET

NotIfcCheck1:
        CMP     AX,IfcSignature2        ;Is this a secondary request?
        JNE     NotIfcCheck
        CMP     CS:IfcInstalled,True       ;Are we in charge of the interface?
        JNE     JumpOld16               ;If not, chain to old ISR
        NOT     AX                      ;Flip the bits in AX
        PUSH    ES                      ;save registers
        PUSH    DI
        PUSH    BX
        PUSH    CX
        MOV     CX,BiosDataSeg          ;ES:DI => intra-app comm area
        MOV     ES,CX
        MOV     DI,IntraApp
        MOV     CX,CS                   ;CX:BX => ThisIfc
        MOV     BX,Offset ThisIfc
        MOV     ES:[DI].Ofst,BX         ;store @ThisIfc at Ptr(ES,DI)^
        MOV     ES:[DI].Segm,CX
        MOV     ES,CX                   ;ES:BX points to ThisIfc
        MOV     WP ES:[BX].NextIfc,0    ;If we're answering this, we're the end
        MOV     WP ES:[BX].NextIfc+2,0  ; of the line, so NextIfc is nil
        POP     CX
        POP     BX
        POP     DI
        POP     ES
        IRET

NotIfcCheck:
        TEST    StateFlags,InInt16      ;Check if we're in use
        JZ      TrackInt16              ;if not, set variable

JumpOld16:
        CLI                             ;Interrupts off
        JmpFar  CS:OldInt16             ;else, pass this on

TrackInt16:
        CMP     AH,10h                  ;Is it function 10h?
        JE      ChkEnhKbd               ;If so, use enhanced keyboard code
        OR      AH,AH                   ;check for function 0 (read next char)
        JNZ     TrackRaw16              ;if AH <> 0, track INT 16 raw

ChkKbdLoop:
        ;loop until a key is pressed, alternately checking for keyboard
        ;input and trying to pop up

        MOV     AH,1                    ;execute check for keypress function
        OR      StateFlags,InInt16      ;Set our bit

        POP     CS:TempInt16.Ofst       ;Offset of caller
        POP     CS:TempInt16.Segm       ;Segment of caller

        CLI                             ;interrupts off to emulate interrupt
        CallFar CS:OldInt16             ;Call original routine

        PUSHF                           ;save flags
        STI                             ;interrupts on
        PUSH    CS:TempInt16.Segm       ;Segment of caller
        PUSH    CS:TempInt16.Ofst       ;Offset of caller

        LAHF                            ;save return flags
        AND     StateFlags,NotIn16      ;Reset our bit
        JNZ     ChkKbdLoopNext          ;if state isn't clear, don't try

        PUSHF                           ;get set for IRET
        PUSH    CS                      ;push CS (next call is near)
        CALL    ChkKbdTryPop            ;push return offset too

        ;IRET in TryPop returns to here
        JMP     SHORT ChkKbdLoopNext    ;check again

ChkKbdTryPop:
        JMP     TryPop                  ;try to pop up

ChkKbdLoopNext:
        SAHF                            ;restore flags
        JZ      ChkKbdLoop              ;if no key waiting, loop
        SetZero AH                      ;switch back to function 0, get key

TrackRaw16:
        OR      StateFlags,InInt16      ;Set our bit
        POP     CS:TempInt16.Ofst       ;Offset of caller
        POP     CS:TempInt16.Segm       ;Segment of caller
        CLI                             ;interrupts off to emulate interrupt
        CallFar CS:OldInt16             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        STI                             ;interrupts on
        PUSH    CS:TempInt16.Segm       ;Segment of caller
        PUSH    CS:TempInt16.Ofst       ;Offset of caller

        ;reset our in-interrupt flag and return
        AND     StateFlags,NotIn16      ;Reset our bit
        JNZ     RetFromI16              ;if that didn't make state 0, return

        JMP     TryPop                  ;try to pop up

RetFromI16:
        IRET

        ;the remainder of the ISR allows us to pop up during function $10
        ;calls (the enhanced keyboard version of function 0, read next char)

ChkEnhKbd:
        CMP     CS:IsEnhanced,1         ;Is it an enhanced keyboard?
        JNE     TrackRaw16              ;If not, don't use the following code!

ChkEnhKbdLoop:
        ;loop until a key is pressed, alternately checking for keyboard
        ;input and trying to pop up

        MOV     AH,11h                  ;execute check for keypress function
        OR      StateFlags,InInt16      ;Set our bit

        POP     CS:TempInt16.Ofst       ;Offset of caller
        POP     CS:TempInt16.Segm       ;Segment of caller

        CLI                             ;interrupts off to emulate interrupt
        CallFar CS:OldInt16             ;Call original routine

        PUSHF                           ;save flags
        STI                             ;interrupts on
        PUSH    CS:TempInt16.Segm       ;Segment of caller
        PUSH    CS:TempInt16.Ofst       ;Offset of caller

        LAHF                            ;save return flags
        AND     StateFlags,NotIn16      ;Reset our bit
        JNZ     ChkEnhKbdLoopNext       ;if state isn't clear, don't try

        PUSHF                           ;get set for IRET
        PUSH    CS                      ;push CS (next call is near)
        CALL    ChkEnhKbdTryPop         ;push return offset too

        ;IRET in TryPop returns to here
        JMP     SHORT ChkEnhKbdLoopNext ;check again

ChkEnhKbdTryPop:
        JMP     TryPop                  ;try to pop up

ChkEnhKbdLoopNext:
        SAHF                            ;restore flags
        JZ      ChkEnhKbdLoop           ;if no key waiting, loop
        MOV     AH,10h                  ;switch back to function 10h, get key
        JMP     SHORT TrackRaw16        ;and track it

Int16   ENDP

;****************************************************** Int28

COMMENT |
  DOS multitasking interrupt handler.
  -----------------------------------
  Handles the DOS multitasking interrupt, which is called continuously at
  the DOS prompt. If PopTicker > 0, this interrupt will call the appropriate
  popup before chaining to the previous INT $28 handler.
|

TempInt28       Pointer <>              ;Temporary address

Int28   PROC NEAR
        CLI                             ;ints off to make sure
        CMP     CS:PopTicker,0          ;check pop ticker before doing anything
        JZ      I28Pass                 ;pass if not set

        TEST    StateFlags,InInt28      ;check if we're in use
        JNZ     I28Pass                 ;if so, pass

        ;following needed for Compaq DOS 3.25--calls itself to check for ^Break
        ;during file operations

        STI                             ;interrupts on
        PUSH    DS                      ;Save DS
        PUSH    BX                      ;Save BX
        LDS     BX,CS:DosInUsePtr       ;DS:BX => DosInUse flag
        CMP     BYTE PTR [BX],2         ;BX <= 2?
        JAE     Passing                 ;pass if DosInUse >= 2
        CMP     CS:HaveDosCritical,1    ;Do we know where DOS critical flag is?
        JNE     NotPassing              ;If not, don't check it here
        LDS     BX,CS:DosCriticalPtr    ;Check DOS critical flag
        CMP     BYTE PTR [BX],0         ;is it 0?
        JE      NotPassing              ;continue if it's 0
Passing:
        POP     BX                      ;Restore DS
        POP     DS                      ;Restore BX
        JMP     SHORT I28Pass

NotPassing:
        POP     BX                      ;Restore DS
        POP     DS                      ;Restore BX

        OR      StateFlags,InInt28      ;now we're in use
        POP     CS:TempInt28.Ofst       ;caller's offset
        POP     CS:TempInt28.Segm       ;caller's segment
        CLI                             ;interrupts off
        CallFar CS:OldInt28             ;call original routine

        PUSHF                           ;caller's flags on stack
        STI                             ;interrupts on
        PUSH    CS:TempInt28.Segm       ;caller's segment
        PUSH    CS:TempInt28.Ofst       ;caller's offset on stack

        CMP     CS:PopTicker,0          ;check if we're waiting to pop up
        JE      I28NoPop                ;if so, do it

        TEST    StateFlags,InSetTr
        JZ      Go28

I28NoPop:
        AND     StateFlags,NotIn28      ;now we're not in use
        IRET

I28Pass:
        CLI                             ;interrupts off
        JmpFar  CS:OldInt28             ;pass to old int 28 handler

Go28:
        MOV     CS:PopupsEnabled,0      ;disable popups
        AND     StateFlags,NotIn28      ;now we're not in use
        JMP     DosOkToPop              ;call routine which won't return
        IRET                            ;put this here anyhow

Int28   ENDP

COMMENT |
  The remainder of these interrupt handlers are simple filters that prevent
  us from popping up when any one of these interrupts is in progress by setting
  a bit in the system state flag. Their overhead is minimal.
|

;****************************************************** Int5

COMMENT |
  PrtSc interrupt handler.
  ------------------------
  Intercepts the PrtSc interrupt to contend with programs that generate the
  interrupt themselves to do screen dumps.
|

TempInt05      Pointer <>              ;local

Int5    PROC NEAR

        CLI                             ;Just in case it wasn't called properly
        TEST    StateFlags,InInt05      ;Check if we're in use
        JZ      TrackInt5               ;if not, set variable
        JmpFar  CS:OldInt05             ;yes, pass this on

TrackInt5:
        OR      StateFlags,InInt05      ;Set our bit
        POP     CS:TempInt05.Ofst       ;Offset of caller
        POP     CS:TempInt05.Segm       ;Segment of caller
        CallFar CS:OldInt05             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        CLI                             ;interrupts off
        PUSH    CS:TempInt05.Segm       ;Segment of caller
        PUSH    CS:TempInt05.Ofst       ;Offset of caller

        ;reset our in-interrupt flag and return
        AND     StateFlags,NotIn05      ;Reset our bit
        IRET

Int5    ENDP

;****************************************************** Int10

COMMENT |
  BIOS video interrupt handler.
  -----------------------------
  Intercepts the BIOS video interrupt to prevent problems when running in
  the OS/2 compatibility box. Not captured if running under DOS 2.x or 3.x.
|

TempInt10       Pointer <>              ;local

Int10   PROC NEAR

        CLI                             ;Just in case it wasn't called properly
        TEST    StateFlags,InInt10      ;Check if we're in use
        JZ      TrackInt10              ;if not, set variable
        JmpFar  CS:OldInt10             ;yes, pass this on

TrackInt10:
        OR      StateFlags,InInt10      ;Set our bit
        POP     CS:TempInt10.Ofst       ;Offset of caller
        POP     CS:TempInt10.Segm       ;Segment of caller
        CallFar CS:OldInt10             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        CLI                             ;interrupts off
        PUSH    CS:TempInt10.Segm       ;Segment of caller
        PUSH    CS:TempInt10.Ofst       ;Offset of caller

        AND     StateFlags,NotIn10      ;reset our in-interrupt flag and return
        IRET

Int10   ENDP

;****************************************************** Int13

COMMENT |
  BIOS disk interrupt handler.
  ----------------------------
  Intercepts the BIOS disk interrupt to contend with programs that bypass DOS
  and access the disk directly.
|

TempInt13       Pointer <>              ;local

Int13   PROC NEAR

        CLI                             ;Just in case it wasn't called properly
        TEST    StateFlags,InInt13      ;Check if we're in use
        JZ      TrackInt13              ;if not, set variable
        JmpFar  CS:OldInt13             ;yes, pass this on

TrackInt13:
        OR      StateFlags,InInt13      ;Set our bit
        POP     CS:TempInt13.Ofst       ;Offset of caller
        POP     CS:TempInt13.Segm       ;Segment of caller
        CallFar CS:OldInt13             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        CLI                             ;interrupts off
        PUSH    CS:TempInt13.Segm       ;Segment of caller
        PUSH    CS:TempInt13.Ofst       ;Offset of caller

        AND     StateFlags,NotIn13      ;reset our in-interrupt flag and return
        IRET

Int13   ENDP

;****************************************************** Int14

COMMENT |
  BIOS communications interrupt handler.
  --------------------------------------
  Intercepts the BIOS communications interrupt to prevent problems when running
  in the OS/2 compatibility box. Not captured if running under DOS 2.x or 3.x.
|

TempInt14       Pointer <>              ;local

Int14   PROC NEAR

        CLI                             ;Just in case it wasn't called properly
        TEST    StateFlags,InInt14      ;Check if we're in use
        JZ      TrackInt14              ;if not, set variable
        JmpFar  CS:OldInt14             ;yes, pass this on

TrackInt14:
        OR      StateFlags,InInt14      ;Set our bit
        POP     CS:TempInt14.Ofst       ;Offset of caller
        POP     CS:TempInt14.Segm       ;Segment of caller
        CallFar CS:OldInt14             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        CLI                             ;interrupts off
        PUSH    CS:TempInt14.Segm       ;Segment of caller
        PUSH    CS:TempInt14.Ofst       ;Offset of caller

        AND     StateFlags,NotIn14      ;reset our in-interrupt flag and return
        IRET

Int14   ENDP

;****************************************************** Int17

COMMENT |
  BIOS printer interrupt handler.
  ------------------------------
  Intercepts the BIOS printer interrupt to prevent problems when running in the
  OS/2 compatibility box. Not captured if running under DOS 2.x or 3.x.
|

TempInt17       Pointer <>              ;local

Int17   PROC NEAR

        CLI                             ;Just in case it wasn't called properly
        TEST    StateFlags,InInt17      ;Check if we're in use
        JZ      TrackInt17              ;if not, set variable
        JmpFar  CS:OldInt17             ;yes, pass this on

TrackInt17:
        OR      StateFlags,InInt17      ;Set our bit
        POP     CS:TempInt17.Ofst       ;Offset of caller
        POP     CS:TempInt17.Segm       ;Segment of caller
        CallFar CS:OldInt17             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        CLI                             ;interrupts off
        PUSH    CS:TempInt17.Segm       ;Segment of caller
        PUSH    CS:TempInt17.Ofst       ;Offset of caller

        AND     StateFlags,NotIn17      ;reset our in-interrupt flag and return
        IRET

Int17   ENDP

;****************************************************** Int24

;procedure Int24
;Interrupt handler for DOS critical errors

FailCode        = 3
IgnoreCode      = 0

Int24   PROC NEAR
        MOV     AX,DI                   ;DI has error code on entry
        AND     AX,01Fh                 ;convert error code to 150..181
        ADD     AX,150
        MOV     CS:Int24ErrCode,AL      ;Store error code for later
        MOV     AL,FailCode             ;Fail the DOS call
        CMP     CS:Dos3Plus,True        ;DOS 3.x or higher?
        JE      Int24Exit               ;If so, done
        MOV     AL,IgnoreCode           ;else, tell DOS to I)gnore error
Int24Exit:
        IRET

Int24   ENDP

;****************************************************** Int25

COMMENT |
  DOS absolute disk read interrupt handler.
  -----------------------------------------
  Intercepts the absolute disk read interrupt to contend with programs
  that use this interrupt.
|

TempInt25       Pointer <>              ;local

Int25   PROC NEAR

        CLI                             ;Just in case it wasn't called properly
        TEST    StateFlags,InInt25      ;Check if we're in use
        JZ      TrackInt25              ;if not, set variable
        JmpFar  CS:OldInt25             ;yes, pass this on

TrackInt25:
        OR      StateFlags,InInt25      ;Set our bit
        POP     CS:TempInt25.Ofst       ;Offset of caller
        POP     CS:TempInt25.Segm       ;Segment of caller
        CallFar CS:OldInt25             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        CLI                             ;interrupts off
        PUSH    CS:TempInt25.Segm       ;Segment of caller
        PUSH    CS:TempInt25.Ofst       ;Offset of caller

        AND     StateFlags,NotIn25      ;reset our in-interrupt flag and return
        IRET

Int25   ENDP

;****************************************************** Int26

COMMENT |
  DOS absolute disk write interrupt handler.
  -----------------------------------------
  Intercepts the absolute disk write interrupt to contend with programs
  that use this interrupt.
|

TempInt26       Pointer <>              ;local

Int26   PROC NEAR

        CLI                             ;Just in case it wasn't called properly
        TEST    StateFlags,InInt26      ;Check if we're in use
        JZ      TrackInt26              ;if not, set variable
        JmpFar  CS:OldInt26             ;yes, pass this on

TrackInt26:
        OR      StateFlags,InInt26      ;Set our bit
        POP     CS:TempInt26.Ofst       ;Offset of caller
        POP     CS:TempInt26.Segm       ;Segment of caller
        CallFar CS:OldInt26             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        CLI                             ;interrupts off
        PUSH    CS:TempInt26.Segm       ;Segment of caller
        PUSH    CS:TempInt26.Ofst       ;Offset of caller

        AND     StateFlags,NotIn26      ;reset our in-interrupt flag and return
        IRET

Int26   ENDP

;****************************************************** NopISR

NopISR  PROC NEAR

        IRET                            ;For dummy ISR's

NopISR  ENDP

;****************************************************** UseCritical

;Force DOS 2.x to use DOS critical stack when getting/setting PSP

UseCritical     PROC NEAR

        CMP     CS:Dos3Plus,True        ;Is this DOS 3.x?
        JE      UCexit                  ;if so, do nothing
        LES     DI,CS:DosCriticalPtr    ;ES:DI => DosCriticalPtr
        MOV     BYTE PTR ES:[DI],-1     ;Tell DOS to use critical stack
UCexit:
        RET

UseCritical     ENDP

;****************************************************** NotCritical

;Reset DOS 2.x for non-critical stack

NotCritical     PROC NEAR

        CMP     CS:Dos3Plus,True        ;Is this DOS 3.x?
        JE      NCexit                  ;if so, do nothing
        LES     DI,CS:DosCriticalPtr    ;ES:DI => DosCriticalPtr
        MOV     BYTE PTR ES:[DI],0      ;Tell DOS to use non-critical stack
NCexit:
        RET

NotCritical     ENDP

CheckForSwapFile PROC NEAR                  ;!!.03 begin
        MOV      DX,OFFSET CS:Swap1FileCS
        MOV      AX,4300h
        INT      21h
        RET                                 ;check for exist
CheckForSwapFile ENDP

SwapErrorHandler PROC NEAR
        PUSH     AX
        PUSH     BX
        PUSH     CX
        PUSH     DX
        PUSH     DS

        MOV      AH,40h
        MOV      BX,1
        MOV      CX,1
        PUSH     CS
        POP      DS
        MOV      DX,OFFSET SEH_Bell
        INT      21h

        POP      DS
        POP      DX
        POP      CX
        POP      BX
        POP      AX
        RET
SEH_Bell:
        DB       7
SwapErrorHandler ENDP

ReadyForSwap    PROC NEAR
        MOV     CS:Status,0                 ; !!.22
        TEST    CS:SwapFlags,EmsFlag        ; using EMS?
        JNZ     RFS_NotDisk
IFDEF SupportXms
        TEST    CS:SwapFlags,XmsFlag        ; using EMS?
        JNZ     RFS_NotDisk
ENDIF
        CMP     CS:SwapEnabled,0
        JE      RFS_NotDisk
        PUSH    DX
        PUSH    AX
;        MOV     CS:Status,0                ; !!.22
        CALL    CheckForSwapFile
        JC      RFS_Error
RFS_Exit:
        POP     AX
        POP     DX
RFS_NotDisk:
        RET
RFS_Error:
        MOV     CS:Status,AX                ; !!.22
        JMP     SHORT RFS_Exit
ReadyForSwap    ENDP                        ;!!.03 end

;****************************************************** SetDosTraps

COMMENT |

  SetDosTraps
  -----------
  Makes preparations to insure that DOS is safe to use during a popup:

  -- Saves and restores current DTA and PSP, switching to ours in between
  -- Prevents ^Break/^C problems by taking over dangerous interrupts
     and changing the DOS BREAK level, restoring them after the call
  -- Sets up DOS critical error handler for the popup

  After these preparations have been made, the popup is called. Reentrancy
  problems are avoided by means of the DosTrapsSet flag, which prohibits
  this code from being executed twice before the first session is over.
|

SaveBreak       DB      0               ;Saved ^Break state
SavePSP         DW      0               ;Saved PSP segment
SaveDTA         Pointer <>              ;Saved DTA

SDTSaveSP       DW      ?
SDTSaveSS       DW      ?

SetDosTraps     PROC NEAR
        ; check to make sure we're not attempting to swap out netware
        CMP     NetWareUnderneath,1
        JE      SDT_OK                  ; if NetWare is below us all is well

        CALL    NetWareLoaded           ; determine whether NetWare is on top
                                        ; of us
        OR      AL,AL
        JZ      SDT_OK                  ; no, so all is well
        JMP     SDT_Exit                ; NetWare on top, can't safely popup
SDT_OK:
        OR      StateFlags,InSetTR      ;setting DOS traps
        MOV     DosTrapsSet,True        ;next time sys ok, DOS traps are set

        MOV     AX,CS                   ;AX = CS
        MOV     DS,AX                   ;DS = CS

        assume  DS:CODE                 ;Tell MASM that DS = CS

        MOV     SDTSaveSP,SP            ; save SS:SP
        MOV     SDTSaveSS,SS

        SetTempStack                    ; switch to temp stack

        ;save current PSP and switch to ours
        CALL    UseCritical             ;Switch to critical stack in DOS 2.x
        DosCallAX       5100h           ;Get current PSP
        MOV     CS:SavePSP,BX           ;Save PSP returned in BX
        MOV     BX,CS:OurPSP            ;Get our PSP
        DosCallAX       5000h           ;Switch to our PSP
        CALL    NotCritical             ;Critical stack no longer needed

        CALL    ReadyForSwap            ;see if we're ready to swap !!.03
        CMP     CS:Status,0             ;check for error   !!.03
        JE      SDT_NoSwapErr           ;!!.03
        CALL    SwapErrorHandler        ;!!.03
        JMP     SDT_ErrorReentry        ;!!.03
SDT_NoSwapErr:                          ;!!.03

        CALL    SwapProgIn              ;swap in our application

        GetVector 21h, CS:SaveInt21     ;Get current INT 21 vector

        MovSeg  DS,CS                   ;DS = CS
        assume  DS:CODE

        DosCallAX       3300h           ;Get current BREAK level
        MOV     SaveBreak,DL            ;Save current level, returned in DL
        SetZero DL                      ;0 means relax break checking
        DosCallAX       3301h           ;Set BREAK value in DL

        ;Restore Turbo's interrupt handlers
        LDS     DX,CS:NewInt02
        DosCallAX       2502h
        LDS     DX,CS:NewInt34
        DosCallAX       2534h
        LDS     DX,CS:NewInt35
        DosCallAX       2535h
        LDS     DX,CS:NewInt36
        DosCallAX       2536h
        LDS     DX,CS:NewInt37
        DosCallAX       2537h
        LDS     DX,CS:NewInt38
        DosCallAX       2538h
        LDS     DX,CS:NewInt39
        DosCallAX       2539h
        LDS     DX,CS:NewInt3A
        DosCallAX       253Ah
        LDS     DX,CS:NewInt3B
        DosCallAX       253Bh
        LDS     DX,CS:NewInt3C
        DosCallAX       253Ch
        LDS     DX,CS:NewInt3D
        DosCallAX       253Dh
        LDS     DX,CS:NewInt3E
        DosCallAX       253Eh
        LDS     DX,CS:NewInt3F
        DosCallAX       253Fh
        LDS     DX,CS:NewInt75
        DosCallAX       2575h

        PUSH    CS                      ;Reset DS to CS
        POP     DS
        assume  DS:CODE

        ;switch to special INT 21 handler
        MOV     DX,Offset Int21
        DosCallAX       2521h

        ;save current DTA and switch to ours
        DosCall 2Fh                     ;Get current DTA
        SetPtr  SaveDTA, ES, BX         ;Save current DTA (in ES:BX)
        LDS     DX,OurDTA               ;DS:DX points to our DTA
        DosCall 1Ah                     ;Set DTA

        ;reset DS to DATA
        MOV     DS,CS:OurDS             ;Restore our DS
        assume  DS:DATA                 ;Tell MASM that DS = DATA

        ;Save info for emergency exit and call the popup
        MOV     LastEntrySS,SS          ;Save SS
        MOV     LastEntrySP,SP          ;Save SP
        MOV     LastEntryIP,Offset SDTReentry   ;Save reentry offset
        MOV     LastEntryCS,CS          ; save this CS

        TEST    CS:SwapFlags,1        ; test low bit of Mouse flags
        JZ      SDTSkipMouse1          ; if zero, no mouse installed
        MOV     AX,02h                 ; turn off mouse cursor
        INT     033h
SDTSkipMouse1:

        ;make popup look like main block to the overlay manager
        XOR     BP,BP                   ;BP = 0

        CallFar PopAdd                  ;Call the popup

SDTReentry:

        ;restore previous INT 21 handler
        SetVector 21h, CS:SaveInt21

        SetTempStack                    ; set up temp stack again
        CALL    SwapProgOut             ; swap our program out
        TEST    CS:SwapFlags,1        ; test low bit of Mouse flags
        JZ      SDTSkipMouse2          ; if zero, no mouse installed
        MOV     AX,01h                 ; turn on mouse cursor
        INT     033h
SDTSkipMouse2:

        ;reset DS to CS and PUSH our return address back on the stack
        MOV     AX,CS                   ;Set DS to CS
        MOV     DS,AX
        assume  DS:CODE                 ;Tell MASM that DS = CS

        ;restore saved DTA
        LDS     DX,SaveDTA              ;DS:DX points to saved DTA
        DosCall 1Ah                     ;Set DTA function

        assume  DS:NOTHING              ;Tell MASM that DS = ?

        ;restore DOS BREAK level
        MOV     AX,CS                   ;Reset DS to CS
        MOV     DS,AX
        assume  DS:CODE                 ;Tell MASM that DS = CS

        MOV     DL,SaveBreak            ;Restore saved break state
        DosCallAX       3301h           ;Set break check level

SDT_ErrorReentry:                       ;!!.03
        ;restore saved PSP
        CALL    UseCritical             ;Switch to critical stack in DOS 2.x
        MOV     BX,SavePSP              ;BX = saved PSP
        DosCallAX       5000h           ;Set PSP function
        CALL    NotCritical             ;Critical stack no longer needed

SetTrapExit:
        CLI
        MOV     SS,SDTSaveSS            ; put stack back the way we found it
        MOV     SP,SDTSaveSP
        STI

        MOV     DS,CS:OurDS             ;Restore our DS
        assume  DS:DATA                 ;Tell MASM that DS = DATA

        MOV     DosTrapsSet,False       ;DOS traps are not set now
        AND     StateFlags,NotSetTR     ;not setting DOS traps now

        CMP     CS:Disable,0            ;see if a Disable has been requested
        JE      SDT_Exit
        CALL    UnloadTSR               ;disable requested so try to unload

SDT_Exit:
        RET

SetDosTraps     ENDP

;the following code was moved from OPSWAP.ASM as of 1.21     !!.03

;DX old name
;DI new name
DeleteAndRename PROC NEAR
        PUSH    CX
        MovSeg  DS,CS
        MovSeg  ES,CS
        XOR     CX,CX
        DosCallAX 4301h
        DosCallAh 41h
        XCHG    DX,DI

        XOR     CX,CX
        DosCallAX 4301h
        DosCallAh 56h
        MOV     CX,FileAttr

        xchg    DX,DI             ;!!!!!!!

        DosCallAX 4301h

        POP     CX
        RET
DeleteAndRename ENDP

; request and reply packets for the netware function call
NWRequest:      DW   1
                DB   46h

NWReply:        DW   5
                DB   0
                DD   0

; returns AL = 0 if NetWare is not loaded, AL = 1 if it is loaded
NetWareLoaded   PROC NEAR
        PUSH    DS
        PUSH    ES
        PUSH    SI
        PUSH    DI
        PUSH    BX
        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        MOV     AH,0E3h                ; NetWare function call number
        MOV     SI,OFFSET NWRequest    ; DS:SI points to request
        MOV     DI,OFFSET NWReply      ; ES:DI points to reply
        MOV     BX,DI
        MOV     WORD PTR [BX+3],0
        MOV     WORD PTR [BX+5],0

        INT     21h                    ; make the function call
        OR      AL,AL                  ; if AL not zero than no netware
        JNZ     NWL_NotLoaded
        MOV     AX,CS:[BX+3]           ; if BX+5:BX+3 NIL than no netware
        OR      AX,CS:[BX+5]
        JZ      NWL_NotLoaded

;NetWare is Loaded
        MOV     AL,1                   ; indicate true and exit
        JMP     SHORT NWL_Exit

NWL_NotLoaded:
        XOR     AL,AL                  ; indicate false and exit
NWL_Exit:
        POP     BX
        POP     DI
        POP     SI
        POP     ES
        POP     DS
        RET
NetWareLoaded   ENDP

; Saves the entire interrupt vector table
SaveVectorTable PROC NEAR
        PUSH    DS
        PUSH    AX
IFDEF Lantastic5                       ; !!.22
;Code for dealing with Lantastic 5.0
        MOV     AX,3500h
        MOV     DI,OFFSET Vectors
        CLD
SAVT0:  MOV     CX,AX                  ; save AX
        INT     21h                    ; get vector
        MOV     DX,ES                  ; save vector segment
        MOV     AX,CS
        MOV     ES,AX                  ; ES = CS
        MOV     AX,BX
        STOSW                          ; store vector offset in table
        MOV     AX,DX
        STOSW                          ; store vector segment in table
        MOV     AX,CX                  ; restore AX
        CMP     AL,255                 ; are we done?
        JAE     SAVT1                  ; jump if so
        INC     AL                     ; next vector
        JMP     SAVT0
SAVT1:

ELSE
        XOR     AX,AX
        MOV     DS,AX                  ; DS = 0
        MOV     SI,AX                  ; SI = 0
        MOV     AX,CS
        MOV     ES,AX                  ; ES = CS
        MOV     DI,OFFSET Vectors      ; offset of data area to store vectors
        MOV     CX,VectorSize/2        ; move (256*SizeOf(Pointer)) div 2 words
        CLD                            ; forward string ops     !!.02
        REP     MOVSW                  ; copy the vector table
ENDIF
        POP     AX
        POP     DS
        RET
SaveVectorTable ENDP

SwapVectorTable PROC NEAR
        PUSH    DS
        PUSH    ES
        PUSH    SI
        PUSH    DI
        PUSH    DX
        PUSH    CX
        PUSH    BX
        PUSH    AX

IFDEF Lantastic5                       ; !!.22
;Code for dealing with Lantastic 5.0
        XOR     AL,AL                  ; AL = vector number
        MOV     DI,OFFSET Vectors
SWVT0:  MOV     AH,35h
        INT     21h                    ; get DOS vector to ES:BX
        MOV     DX,CS:[DI]
        MOV     DS,CS:[DI+2]           ; get stored vector to DS:DX
        MOV     AH,25h
        INT     21h                    ; set DOS vector from DS:DX
        MOV     CS:[DI],BX
        MOV     CS:[DI+2],ES           ; set stored vector to ES:BX
        CMP     AL,255                 ; are we done?
        JAE     SWVT1                  ; jump if so
        INC     AL                     ; next vector
        ADD     DI,4                   ; next table entry
        JMP     SWVT0
SWVT1:

ELSE
        XOR     AX,AX
        MOV     DS,AX                  ; DS = 0
        MOV     SI,AX                  ; SI = 0
        MOV     AX,CS
        MOV     ES,AX                  ; ES = CS
        MOV     DI,OFFSET Vectors      ; offset of data area where vectors are
        MOV     CX,VectorSize/2        ; move (256*SizeOf(Pointer)) div 2 words
        CLI
        CLD                            ; !!.02
SVT0:
        MOV     BX,SI                  ; swap ds:si with es:di
        MOV     AX,ES:[DI]
        MOVSW
        MOV     [BX],AX

        LOOP    SVT0
        STI
ENDIF

        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     DI
        POP     SI
        POP     ES
        POP     DS
        RET
SwapVectorTable ENDP

;new for !!.13
;saves UART interrupt state and disables UART interrupts
DisableComPorts PROC NEAR
        PUSH    DS
        PUSH    DX
        PUSH    BX
        PUSH    AX
        MOV     BX,0                ;first UART

;Get next UART baseaddr from BIOS
DCP0:
        MOV     AX,40H
        MOV     DS,AX
        MOV     DX,[BX]             ;DX = base address of UART
        OR      DX,0                ;UART here?
        JZ      DCP1                ;no, go check for next UART
        CMP     CS:UartInts[BX]+1,0FFh ;Ok to use?
        JE      DCP1                ;no, go check for next UART
        INC     DX                  ;DX = interrupt enable register
        IN      AL,DX               ;AL = interrupt enable mask
        MOV     BYTE PTR CS:UartInts[BX],AL  ;save interrupt mask
        OR      AL,0                ;any interrupts active?
        JZ      DCP1                ;no, skip this UART

;UART has active interrupts, turn them off
        XOR     AX,AX
        OUT     DX,AL               ;turn off interrupts
        DEC     DX                  ;DX = base address
        IN      AL,DX               ;clear receive interrupt
        ADD     DX,2                ;DX = interrupt ID register
        IN      AL,DX               ;clear THRE interrupt condition
        ADD     DX,3                ;DX = line status register
        IN      AL,DX               ;clear line interrupt condition
        INC     DX                  ;DX = modem status register
        IN      AL,DX               ;clear modem interrupt condition

;check for more UARTs to process
DCP1:   ADD     BX,2                ;point to next BIOS slot
        CMP     BX,8               ;no more than four ports
        JNE     DCP0                ;go look for next port

;no more UARTs
        POP     AX
        POP     BX
        POP     DX
        POP     DS
        RET
DisableComPorts ENDP

;new for !!.13
;restores UART interrupt state
EnableComPorts PROC NEAR
        PUSH    DS
        PUSH    DX
        PUSH    BX
        PUSH    AX
        MOV     BX,0                ;first saved interrupt mask

;Point DS to BIOS data area
        MOV     AX,40H
        MOV     DS,AX

;Check next stored interrupt mask
ECP0:
        MOV     AL,BYTE PTR CS:UartInts[BX] ;AX = first stored mask
        OR      AL,0                ;interrupts active?
        JZ      ECP1                ;no, go check next mask
        MOV     DX,[BX]             ;DX = base address of UART
        INC     DX                  ;DX = interrupt enable address
        OUT     DX,AL               ;enable interrupts

ECP1:   ADD     BX,2                ;point to next mask
        CMP     BX,8               ;no more than four ports
        JNE     DCP0                ;go check next mask

;no more UARTs
        POP     AX
        POP     BX
        POP     DX
        POP     DS
        RET
EnableComPorts  ENDP

; DS:SI points to source
; ES:DI points to destination
; CX    number of bytes to move
; Moves CX bytes from source into destination while moving the same number
; of bytes from destination to source.
TwoWayMoveEMS   PROC NEAR
        PUSH    AX
        PUSH    BX
        SHR CX,1                       ; divide number of bytes by 2 to get words
        CLD                            ; !!.20
TWMEMS0:
        MOV     BX,SI                  ; swap ds:si with es:di
        MOV     AX,ES:[DI]
        MOVSW
        MOV     [BX],AX

        LOOP    TWMEMS0                ; next word

        RCL     CX,1                   ; rotate carry left for odd byte
        JCXZ    TWMEMSExit             ; if no odd byte, then exit
        MOV     AL,BYTE PTR [SI]       ; exchange final byte
        XCHG    BYTE PTR ES:[DI],AL
        MOV     BYTE PTR [SI],AL

TWMEMSExit:
        POP     BX
        POP     AX
        RET
TwoWayMoveEMS   ENDP

TwoWayMoveDisk  PROC NEAR
        MOV     CS:Status,0            ;zero swap status              !!.22
        TEST    CS:SwapFlags,SingleFlag;see if using single swap file !!.02 !!.13
        JZ      TWMD_DualSwapFile      ;using 2 swap files            !!.02
        JMP     OneWayMoveDisk         ;jump to single file swap      !!.02

TWMD_DualSwapFile:                     ;                              !!.02
        MovSeg  DS,CS                  ;DS = CS
        InitSwapCount                  ;init the swap count

        MOV     CX,FileAttr
        MOV     DX,OFFSET Swap2FileCS  ; **************

        DosCallAH 3Ch                  ;create new file
        JNC     TWMD4
        JMP     TWMDDiskError          ;Disk error, so exit
TWMD4:
        MOV     BX,AX                  ;put new handle in BX
        MOV     FileHandleCS,AX        ;save handle in CS relative storage

TWMD0:  SetSwapCount FileBlockSize      ;CX = bytes to write
        MOV     DX,OFFSET FirstToSave   ;DS:DX -> start of region to save
        DosCallAH 40h                   ;File write
        JNC     TWMD5
        JMP     TWMDDiskError           ; write error, so exit
TWMD5:
        CMP     AX,CX                   ;All bytes written?
        JZ      TWMD2                   ;Jump if so
        JMP     TWMDDiskError
TWMD2:  NextBlock DS,FileBlockSize      ;Point DS to next block to write
        JNZ     TWMD0                   ;Loop if bytes left to write

        MovSeg  DS,CS

        FileClose                       ;*********************

        MOV     DX,OFFSET Swap1FileCS
        XOR     AL,AL
        DosCallAH 3Dh                   ; open the Swapfile1
        JNC     TWMD3
        JMP     TWMDDiskError           ; error opening file, so exit

TWMD3:
        MOV     BX,AX                   ; handle in BX
        MOV     FileHandleCS,AX         ; save handle in CS relative storage
        InitSwapCount

TWMD10: SetSwapCount FileBlockSize      ;CX = bytes to read
        MOV     DX,OFFSET FirstToSave   ;DS:DX -> start of region to save
        DosCallAH 3Fh                   ;File read
        JC      TWMDDiskError           ;Jump if read error
        CMP     AX,CX                   ;All bytes read?
        JZ      TWMD12                  ;Jump if so
        JMP     SHORT TWMDDiskError

TWMD12: NextBlock DS,FileBlockSize      ;Point DS to next block to write
        JNZ     TWMD10                  ;Loop if bytes left to write
        MovSeg  DS,CS
        FileClose
        MOV     DX,OFFSET Swap1FileCS   ;*********************
        MOV     DI,OFFSET Swap2FileCS
        CALL    DeleteAndRename         ; nothing we can do if an error here

        JMP     SHORT TWMDExit
TWMDDiskError:
        MOV     CS:Status,DiskError     ; indicate disk error !!.22
        CALL    HaltWithMessage
OneWayMoveDisk:                         ; !!.02
        CALL    SingleFileSwap          ; !!.02
TWMDExit:
        MovSeg  DS,CS                   ; DS = CS again
        RET
TwoWayMoveDisk  ENDP

SwapMoveDisk    PROC NEAR
        MovSeg  DS,CS                   ; DS = CS
        CALL    TwoWayMoveDisk          ; move to/from disk
        RET
SwapMoveDisk    ENDP

SwapMoveEMS     PROC NEAR
        MOV     ES,FrameSegCS           ; put frame segment in ES
        MOV     DX,EmsHandleCS          ; EMS handle in DX

        EmsCall 47h                     ; save mapping context
        JNZ     SMEMSError

        InitSwapCount
        XOR     BX,BX

SMEMS0:
        SetSwapCount EmsPageSize        ; Swap an entire EMS page
        MOV     SI,OFFSET FirstToSave   ; offset in this CS to start saving
        XOR     DI,DI

        XOR     AL,AL                   ; map page 0
        EmsCall 44h                     ; ems map memory call
        JZ      SMEMS1
        MOV     CS:Status,EMSError      ; !!.22
        JMP     SHORT SMEMSExit

SMEMS1:
        CALL    TwoWayMoveEMS

        INC     BX                      ; next page
        NextBlock DS,EmsPageSize        ; inc DS to segment of next page
        JNZ     SMEMS0                  ; if not zero then there's more

SMEMSExit:
        MOV     DX,EmsHandleCS          ;DX = handle of our EMS block
        EmsCall 48h
        RET
SMEMSError:
        CALL    HaltWithMessage
SwapMoveEMS     ENDP

IFDEF SupportXms
; DX:AX rounded up to even
RoundUpEven     PROC NEAR
        TEST    AX,1
        JZ      RUE_Exit
        ADD     AX,1
        ADC     DX,0
RUE_Exit:
        RET
RoundUpEven     ENDP

TwoWayMoveXms   PROC NEAR
        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        MOV     BX,OFFSET FirstToSave
        MOV     DI,OFFSET CS:XmsMoveBlock
        MOV     SI,DI
        CLD
        MOV     AX,CS:BytesSwappedCS.Lo
        PUSH    AX
        MOV     DX,CS:BytesSwappedCS.Hi
        CALL    RoundUpEven
        STOSW
        PUSH    DX
        MOV     AX,DX
        STOSW
        MOV     CX,DI
        TEST    CS:SwapFlags,XmsDirection
        JNZ     TWMXOut1

        XOR     AX,AX
        STOSW
        MOV     AX,BX
        STOSW
        MOV     AX,CS
        STOSW
        MOV     AX,CS:EmsHandleCS
        STOSW
        POP     DX
        POP     AX
        ADD     AX,1
        ADC     DX,0
        STOSW
        MOV     AX,DX
        STOSW
        JMP     SHORT TWMX1
TWMXOut1:
        XOR     AX,AX
        STOSW
        MOV     AX,BX
        STOSW
        MOV     AX,CS
        STOSW
        MOV     AX,CS:EmsHandleCS
        STOSW
        XOR     AX,AX
        STOSW
        STOSW

TWMX1:
        MOV     AH,0Bh
        PUSH    BX                     ;account for DR-DOS XMS Bug !!.13
        CALL    DWORD PTR CS:[XmsControlCS]
        POP     BX                     ;account for DR-DOS XMS Bug !!.13
        TEST    AX,1
        JZ      TWMXError
        MOV     DI,CX

        TEST    CS:SwapFlags,XmsDirection
        JNZ     TWMXOut2
        OR      CS:SwapFlags,XmsDirection
        MOV     AX,CS:EmsHandleCS
        STOSW
        XOR     AX,AX
        STOSW
        STOSW
        STOSW
        MOV     AX,BX
        STOSW
        MOV     AX,CS
        STOSW
        JMP     SHORT TWMX2
TWMXOut2:
        AND     CS:SwapFlags,not XmsDirection
        MOV     AX,CS:EmsHandleCS
        STOSW
        POP     DX
        POP     AX
        ADD     AX,1
        ADC     DX,0
        STOSW
        MOV     AX,DX
        STOSW
        XOR     AX,AX
        STOSW
        MOV     AX,BX
        STOSW
        MOV     AX,CS
        STOSW
TWMX2:
        MOV     AH,0Bh
        PUSH    BX                     ;account for DR-DOS XMS Bug !!.13
        CALL    DWORD PTR CS:[XmsControlCS]
        POP     BX                     ;account for DR-DOS XMS Bug !!.13
        TEST    AX,1
        JZ      TWMXError
        RET

TWMXError:
        CALL    HaltWithMessage
TwoWayMoveXms   ENDP

SwapMoveXms     PROC NEAR
        TEST    CS:SwapFlags,SingleFlag
        JZ      SMX2Way
SMX0:
        CALL    SingleXmsSwap
        JMP     SHORT SMXExit
SMX2Way:
        CALL    TwoWayMoveXms

SMXExit:
        RET
SMXError:
        CALL    HaltWithMessage
SwapMoveXms     ENDP

ENDIF

