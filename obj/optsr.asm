;******************************************************
;                  OPTSR.ASM 1.30
;              TSR Management routines
;     Copyright (c) TurboPower Software 1987, 1992.
; Portions Copyright (c) Sunny Hill Software 1985, 1986
;     and used under license to TurboPower Software
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;************************************************* Equates

MaxScanCode     =       58h             ;Do not change
IfcSignature    =       0F0F0h          ;Do not change
IfcSignature2   =       0E0E0h          ;Do not change
NextIfc         =       14              ;Offset of NextIfc field in IFC record

;************************************************* Data


DATA    SEGMENT BYTE PUBLIC

        ;Pascal variables

        EXTRN   PopupAddrs : BYTE               ;Addresses of popup routines
        EXTRN   PopupStacks : BYTE              ;Stacks for popup routines
        EXTRN   PopupInUse : BYTE               ;Flags for popups in use
        EXTRN   PopupKeys : BYTE                ;Trigger keys for popups
        EXTRN   ShiftKeys : BYTE                ;Shift keys for popups
        EXTRN   DosWaitFlags : BYTE             ;Flags popups that need DOS
        EXTRN   PopTickerPtr : DWORD            ;Points to PopTicker
        EXTRN   PopupsEnabledPtr : DWORD        ;Points to PopupsEnabled
        EXTRN   PopupToCallPtr : DWORD          ;Points to PopupToCall
        EXTRN   PrefixSeg : WORD                ;Our PSP segment
        EXTRN   DosVersion : WORD               ;Version of DOS
        EXTRN   ThisIfc : BYTE                  ;Standard interface record
        EXTRN   IfcInstalled : BYTE             ;True if we're in charge of IFC

        ;internal variables

        DosTrapsSet     DB      ?               ;True if special trapping of DOS
                                                ;INT vectors, etc. has been done
        LastEntrySS     DW      ?               ;Data needed for EmergencyExit
        LastEntrySP     DW      ?
        LastEntryIP     DW      ?

DATA    ENDS

;************************************************* Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  Int5, Int8, Int9, Int10, Int13, Int14, Int16, Int17, Int25,
        PUBLIC  Int26, Int28, InitTsrPtrs, EmergencyExit
        PUBLIC  Save3Fvector, DosBusyFlag, DosCriticalFlag
        PUBLIC  Int8grab, EnableInt8Grab                  ;!!.01
        PUBLIC  PrivateVectors
        PUBLIC  CSMouseFlag                               ;!!.13

;Note that these variables are all in the code segment

;When both of these variables point to zero, DOS may be called
DosInUsePtr     Pointer <>              ;Non-0 if DOS in use
DosCriticalPtr  Pointer <>              ;Non-0 if DOS critical
HaveDosCritical DB      1               ;Set to 1 if DosCriticalPtr is reliable

;Old interrupt vectors
OldInt05        Pointer <>              ;Old INT $05 (PrtSc)
OldInt08        Pointer <>              ;Old INT $08 (clock)
OldInt09        Pointer <>              ;Old INT $09 (keyboard)
OldInt09rg      Pointer <>              ;Old INT $09 when regrabbed !!.01
OldInt10        Pointer <>              ;Old INT $10 (video)
OldInt13        Pointer <>              ;Old INT $13 (disk)
OldInt14        Pointer <>              ;Old INT $14 (comm.)
OldInt16        Pointer <>              ;Old INT $16 (keyboard)
OldInt17        Pointer <>              ;Old INT $17 (printer)
OldInt25        Pointer <>              ;Old INT $25 (abs. disk read)
OldInt26        Pointer <>              ;Old INT $26 (abs. disk write)
OldInt28        Pointer <>              ;Old INT $28 (DOS multitasking)

NewInt34        Pointer <>              ;for emulation
NewInt35        Pointer <>
NewInt36        Pointer <>
NewInt37        Pointer <>
NewInt38        Pointer <>
NewInt39        Pointer <>
NewInt3A        Pointer <>
NewInt3B        Pointer <>
NewInt3C        Pointer <>
NewInt3D        Pointer <>
NewInt3E        Pointer <>
NewInt3F        Pointer <>              ;for overlays

Int24ErrCode    DB      0               ;critical error code

;Following needed for TSR's that use the 8087
NewInt02        Pointer <>              ;NMI interrupt
NewInt75        Pointer <>              ;8087 exceptions

PopupsActive    DB      0               ;# of popups active !!.01

PopupToCall     DB      0               ;Index number of a pending popup
PopupsEnabled   DB      0               ;Boolean, determines if we react to pop keys
PopTimeOut      DW      36              ;Default timeout number of clock ticks
                                        ;(2 seconds on PC)
PopTicker       DW      0               ;Decremented as the clock ticker
OurDS           DW      DATA            ;Value of DS -- init'd by EXE loader
OurDTA          Pointer <>              ;Our DTA
OurPSP          DW      0               ;Our PSP
TempTrapFlag    DB      0               ;Flag to indicate if traps need setting
IsEnhanced      DB      0               ;1 if it's an enhanced keyboard

CSMouseFlag:                            ;!!.13
HaveMouse       DB      0               ;1 if mouse is installed

Dos3Plus        DB      True            ;Boolean - True if running DOS 3.x or
                                        ; higher

SystemState     DW      0               ;see comment below
StateFlags      EQU     WP CS:SystemState ;for convenience

COMMENT @
 When SystemState is zero, popping up is OK.  Each interrupt handler has its
 own bit which is set on entry and reset on exit from the interrupt.  One
 bit used is set when we are setting/removing the DOS traps.

 The bit flags are as follows:

   F E D C B A 9 8 7 6 5 4 3 2 1 0  Flag indicates we're inside this INT:
   -------------------------------- -------------------------------------
   | | | | | | | | | | | | | | | +- $05  PrtSc
   | | | | | | | | | | | | | | +--- $08  Clock tick (hardware)
   | | | | | | | | | | | | | +----- $09  Keyboard (hardware)
   | | | | | | | | | | | | +------- $10  BIOS video interrupt
   | | | | | | | | | | | +--------- $13  BIOS disk read/write
   | | | | | | | | | | +----------- $14  BIOS communications
   | | | | | | | | | +------------- $16  BIOS keyboard
   | | | | | | | | +--------------- $17  BIOS printer
   | | | | | | | +----------------- $25  DOS absolute disk read
   | | | | | | +------------------- $26  DOS absolute disk write
   | | | | | +--------------------- $28  DOS multitasking
   | | | | +----------------------- --  Setting DOS traps
   +-+-+-+------------------------- xxx  Bits $C-$F are reserved
@

;************************************************* Bit masks

InInt05         =       0000000000000001b       ;Set when in INT $05
InInt08         =       0000000000000010b       ;Set when in INT $08
InInt09         =       0000000000000100b       ;Set when in INT $09
InInt10         =       0000000000001000b       ;Set when in INT $10
InInt13         =       0000000000010000b       ;Set when in INT $13
InInt14         =       0000000000100000b       ;Set when in INT $14
InInt16         =       0000000001000000b       ;Set when in INT $16
InInt17         =       0000000010000000b       ;Set when in INT $17
InInt25         =       0000000100000000b       ;Set when in INT $25
InInt26         =       0000001000000000b       ;Set when in INT $26
InInt28         =       0000010000000000b       ;Set when in INT $28
InSetTR         =       0000100000000000b       ;Set when we set traps

NotIn05         =       1111111111111110b       ;Clears INT $05 flag
NotIn08         =       1111111111111101b       ;Clears INT $08 flag
NotIn09         =       1111111111111011b       ;Clears INT $09 flag
NotIn10         =       1111111111110111b       ;Clears INT $10 flag
NotIn13         =       1111111111101111b       ;Clears INT $13 flag
NotIn14         =       1111111111011111b       ;Clears INT $14 flag
NotIn16         =       1111111110111111b       ;Clears INT $16 flag
NotIn17         =       1111111101111111b       ;Clears INT $17 flag
NotIn25         =       1111111011111111b       ;Clears INT $25 flag
NotIn26         =       1111110111111111b       ;Clears INT $26 flag
NotIn28         =       1111101111111111b       ;Clears INT $28 flag
NotSetTR        =       1111011111111111b       ;Clears setting traps flag

;************************************************* TryPop

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
        STI
        PUSH    DS                      ;Save DS
        PUSH    BX                      ;Save BX
        PUSH    AX                      ;Save AX

        MOV     DS,CS:OurDS             ;get data seg
        SetZero AX                      ;zero AX
        MOV     AL,CS:PopupToCall       ;Popup index in AL
        DEC     AX                      ;less 1 for base of 1
        MOV     BX,Offset DosWaitFlags  ;offset of DosWaitFlags array
        XLAT                            ;Get value. If not 0, check DOS
        MOV     BX,AX                   ;Store state of the flag in BX
        SUB     BL,DosTrapsSet          ;BL = (1-0), (1-1), (0-0), or (0-1)
        MOV     CS:TempTrapFlag,BL      ;Save the result
        OR      AX,AX                   ;check for 0
        JNZ     CheckDosOkToPop         ;if not 0, check DOS

        POP     AX                      ;restore used regs
        POP     BX
        POP     DS
        JMP     SHORT DosOkToPop        ;not using DOS, all's well

CheckDosOkToPop:
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
        STI
        SaveAllRegs                     ;Save all registers
        MOV     DS,CS:OurDS             ;Get data segment

        SetZero BX                      ;Zero BX
        MOV     BL,CS:PopupToCall       ;Get index of popup
        DEC     BX                      ;PopupAddrs is 1-based
        MOV     SI,BX                   ;save index in SI for now
        SHL     BX,1                    ;PopupAddrs is array of pointers
        SHL     BX,1

        PUSH    WP PopupAddrs[BX].Segm  ;Push address of popup
        PUSH    WP PopupAddrs[BX].Ofst

        ;switch stacks

        MOV     AX,SS                   ;save current SS in AX
        MOV     ES,AX                   ;ES = current SS
        MOV     DX,WP PopupStacks[BX].Segm ;DX = new SS
        MOV     BX,WP PopupStacks[BX].Ofst ;BX = new SP
        CLI
        MOV     SS,DX                   ;new SS in SS
        XCHG    BX,SP                   ;swap new & old SP, old in ES:BX
        STI

        ADD     BX,4                    ;ES:BX points to Regs,
                                        ;ES:[BX-4] has pointer to popup
        PUSH    ES                      ;save top of regs segment
        PUSH    BX                      ;and offset
        PUSH    SI                      ;save index of popup

        PUSH    LastEntrySS             ;Save any previous routine's SS
        PUSH    LastEntrySP             ;SP
        PUSH    LastEntryIP             ;and reentry offset

        PUSH    ES                      ;pass top of regs segment
        PUSH    BX                      ;and offset as VAR parameter

        CMP     CS:HaveMouse,True       ;do we have a mouse?
        JNE     NoMouse1
        PUSH    AX
        MOV     AX,02h                  ;hide the mouse cursor
        INT     33h
        POP     AX

NoMouse1:
        INC     CS:PopupsActive         ;!!.01
        CMP     CS:TempTrapFlag,True    ;see if traps need to be set
        JE      SetTrapsToPop           ;If so, use alternate routine

        ;Else, save info for emergency exit and call popup
        MOV     LastEntrySS,SS          ;Save SS
        MOV     LastEntrySP,SP          ;Save SP
        MOV     LastEntryIP,Offset BackInTryPop ;Save reentry offset

        MOV     CS:PopupsEnabled,1      ;reenable popups
        CallFar ES:[BX-4]               ;CALL popup, which will get rid
                                        ;of the last two PUSH's
        JMP     SHORT BackInTryPop      ;We're back, clean up

SetTrapsToPop:
        CALL    SetDosTraps             ;Set the traps and call pop-up

BackInTryPop:
        POP     LastEntryIP             ;restore previous reentry offset
        POP     LastEntrySP             ;SP
        POP     LastEntrySS             ;and previous routine's SS
        POP     BX                      ;index value in BX

        CMP     CS:HaveMouse,True       ;do we have a mouse?
        JNE     NoMouse2
        PUSH    AX
        MOV     AX,01h                  ;show the mouse cursor
        INT     033h
        POP     AX

NoMouse2:
        MOV     PopupInUse[BX],False    ;Popup is not in use
        DEC     CS:PopupsActive         ;!!.01
        POP     BX                      ;old SP in BX
        POP     AX                      ;old SS in AX
        CLI
        MOV     SS,AX                   ;restore SS
        MOV     SP,BX                   ;restore SP
        STI
        RestoreAllRegs                  ;restore all registers
        IRET                            ;return to caller

TryPop  ENDP

;************************************************* DecPopTicker

;Check to see if we've timed out while waiting to pop up

DecPopTicker    PROC NEAR

        DEC     CS:PopTicker            ;Decrement ticker
        JNZ     DecDone

        PUSH    DS                      ;Timed out, use these regs
        PUSH    BX
        MOV     DS,CS:OurDS             ;get data segment
        SetZero BH                      ;zero BH
        MOV     BL,CS:PopupToCall       ;get number of popup
        DEC     BX                      ;array has base of 1, convert to 0 base
        MOV     PopupInUse[BX],False    ;Popup is not in use
        POP     BX
        POP     DS

DecDone:
        STI
        RET

DecPopTicker    ENDP

;************************************************* EnableInt8Grab !!.01

GrabbedOnce     DB      False

EnableInt8Grab  PROC NEAR

        PUSH    DS                      ;Save DS
        PUSH    CS                      ;DS = CS
        POP     DS

        ASSUME  DS:CODE

        MOV     DX,Offset Int8grab      ;DS:DX to points to Int8grab
        DosCallAX       2508h           ;Change timer tick ISR
        MOV     CS:GrabbedOnce,False    ;Initialize GrabbedOnce
        POP     DS                      ;Restore DS

        ASSUME  DS:DATA

        RET

EnableInt8Grab  ENDP

;************************************************* Int8grab !!.01

Int9ofs = 09h*04h

Int8grab:
        CLI                             ;Make sure interrupts are off
        PUSH    DS                      ;Save registers
        PUSH    SI
        PUSH    AX
        PUSH    BX

        SetZero AX                      ;DS:SI => INT 9 vector
        MOV     DS,AX
        MOV     SI,Int9ofs

        MOV     AX,CS                   ;AX = CS
        CMP     [SI].Segm,AX            ;Is vector still pointing to us?
        JNE     GrabTry                 ;If not, try to regrab it

        CMP     CS:GrabbedOnce,True     ;Is grab flag set?
        JNE     GrabDone                ;If not, we're done
        CMP     [SI].Ofst,Offset Int9   ;Does vector point to Int9?
        JNE     GrabDone                ;If not, we're done
        MOV     CS:GrabbedOnce,False    ;Else, clear the flag
        JMP     SHORT GrabDone          ;Now we're done

GrabTry:
        CMP     CS:GrabbedOnce,True     ;Have we grabbed it once?
        JE      GrabDone                ;If so, don't do it again !!.20

        MOV     BX,[SI].Segm            ;Save current vector in OldInt09rg
        MOV     OldInt09rg.Segm,BX
        MOV     BX,[SI].Ofst
        MOV     OldInt09rg.Ofst,BX

        MOV     [SI].Segm,AX            ;Point INT 9 to Int9grab
        MOV     [SI].Ofst,Offset Int9grab

        MOV     CS:GrabbedOnce,True     ;We've grabbed it

GrabDone:
        POP     BX
        POP     AX
        POP     SI
        POP     DS
        JMP     SHORT Int8              ;Jump to ISR proper

;************************************************* Int8

COMMENT |
  Clock interrupt handler.
  ------------------------
  Traps the clock tick to see if it should activate a routine. If it can, it
  clears the clock tick, then calls the appropriate popup. If it can't, it
  chains to the previous INT $8 handler.
|

TempInt08       Pointer <>

Int8    PROC NEAR

        CMP     CS:PopTicker,0          ;Check to see if we're waiting to pop up
        JE      Int8Pass                ;if not, pass on interrupt

CheckPopClk:
        TEST    StateFlags,InInt08      ;check if we're in use
        JZ      Int8Try                 ;if not, try to pop up
        CALL    DecPopTicker            ;else, see if we timed out

Int8Pass:
        CLI
        JmpFar  CS:OldInt08             ;pass on interrupt

Int8Try:
        OR      StateFlags,InInt08      ;now we are in use
        STI
        POP     CS:TempInt08.Ofst       ;caller's offset
        POP     CS:TempInt08.Segm       ;caller's segment
        CLI
        CALL    CS:OldInt08             ;Call original routine

        ;Push flags and address on the stack so we can use IRET to return
        STI
        PUSHF                           ;caller's flags on stack
        PUSH    CS:TempInt08.Segm       ;caller's segment
        PUSH    CS:TempInt08.Ofst       ;caller's offset on stack

        CLI
        CMP     CS:PopTicker,0          ;check again in case it expired
        JZ      NoTimeOut               ;return

        AND     StateFlags,NotIn08      ;clear clock-in-use bit
        JNZ     CheckTimeOut            ;not OK, pass on
        JMP     TryPop                  ;try to pop-up

CheckTimeOut:
        CALL    DecPopTicker            ;see if we've timed out

NoTimeOut:
        AND     StateFlags,NotIn08      ;clear clock-in-use bit
        IRET

Int8    ENDP

;************************************************* Int9grab !!.01

;Stub ISR

UseInt09rg      DB 0

Int9grab:
        CMP     CS:PopupsActive,0       ;are any popups active?
        JA      Int9                    ;if so, bypass later ISR's
        MOV     CS:UseInt09rg,True      ;use OldInt09rg
        JMP     SHORT Int9prim

;************************************************* Int9

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
; RoutineNum      DB      0             ;removed in !!.01
BiosDataSeg     DW      40h

KbdData         =       60h
BiosShiftFlags  =       17h

Int9    PROC NEAR

        MOV     CS:UseInt09rg,False     ;!!.01

Int9prim:                               ;!!.01
        STI
        PUSH    DS
        PUSH    AX
        PUSH    BX
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
        MOV     AL,PopupKeys[BX]        ;get popup handle in AL
        OR      AL,AL                   ;check for 0
        JZ      PassKbdInt              ;if 0, not a hot key
        CMP     AH,ShiftKeys[BX]        ;Shift keys match?
        JE      AttemptPop              ;if so, try to POP up

PassKbdInt:
        POP     BX
        POP     AX
        POP     DS
        TEST    StateFlags,InInt09      ;Check if we're in use
        JZ      TrackKbd                ;if no, set variable
        CLI
        CMP     CS:UseInt09rg,True      ;Have we regrabbed the vector? !!.01
        JNE     DoJump                  ;If not, use OldInt09          !!.01
        JmpFar  CS:OldInt09rg           ;Else, use OldInt09rg          !!.01
DoJump:                                 ;                              !!.01
        JmpFar  CS:OldInt09

TrackKbd:
        OR      StateFlags,InInt09      ;Set our bit
        POP     CS:TempInt09.Ofst       ;Offset of caller
        POP     CS:TempInt09.Segm       ;Segment of caller
        CLI
        CMP     CS:UseInt09rg,True      ;Have we regrabbed the vector? !!.01
        JNE     DoCall                  ;If not, use OldInt09          !!.01
        CallFar CS:OldInt09rg           ;Else, use OldInt09rg          !!.01
        JMP     SHORT SkipCall          ;Skip over the next call       !!.01
DoCall:                                 ;                              !!.01
        CallFar CS:OldInt09             ;Call original routine
SkipCall:                               ;                              !!.01

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

        CLI
        CMP     CS:PopupsEnabled,True   ;Are popups enabled?
        JNE     DontPop                 ;No? Eat the hotkey
        CMP     PopupInUse[BX],AH       ;Popup already in use?
        JNE     DontPop                 ;Yes? Eat the hotkey
        CMP     CS:PopTicker,0          ;Something else waiting to pop up?
        JNE     DontPop                 ;Yes? Eat the hotkey

        ;checks went OK, we can set the pop ticker

        MOV     PopupInUse[BX],True     ;This popup is in use now
        INC     BX                      ;Popup to call in BX
        MOV     CS:PopupToCall,BL       ;In PopupToCall
        MOV     AX,CS:PopTimeOut        ;timeout value in AX
        MOV     CS:PopTicker,AX         ;set PopTicker
DontPop:
        STI
        ResetKbd                        ;reset keyboard (trashes AX)
        NullJump                        ;delay
        ResetPIC                        ;reset PIC port (trashes AX)
        POP     BX
        POP     AX
        POP     DS
        IRET

Int9    ENDP

;************************************************* Int16

COMMENT |
  BIOS keyboard interrupt handler.
  --------------------------------
  Intercepts the BIOS keyboard interrupt (INT 16) to allow popups to be
  activated while waiting for keyboard input.

  Also used to communicate data between TSR's written with Object Professional.
|

TempInt16       Pointer <>
IntraApp        =       00F0h           ;offset of intraapplication comm area

Int16   PROC NEAR

        STI
        CMP     AX,IfcSignature         ;See if this is a request for info
        JNE     NotIfcCheck1            ;If not, continue
        PUSH    DS                      ;Save DS
        MOV     DS,CS:OurDs             ;DS = OurDS
        CMP     IfcInstalled,True       ;Are we in charge of the interface?
        POP     DS                      ;Restore DS
        JNE     JumpOld16               ;If not, chain to old ISR
        NOT     AX                      ;Flip the bits in AX
        MOV     ES,CS:OurDs             ;ES = DS
        MOV     DI,Offset ThisIfc       ;ES:DI points to ThisIfc
        MOV     WP ES:[DI].NextIfc,0    ;If we're answering this, we're the end
        MOV     WP ES:[DI].NextIfc+2,0  ; of the line, so NextIfc is nil
        IRET

NotIfcCheck1:
        CMP     AX,IfcSignature2        ;Is this a secondary request?
        JNE     NotIfcCheck
        PUSH    DS                      ;Save DS
        MOV     DS,CS:OurDs             ;DS = OurDS
        CMP     IfcInstalled,True       ;Are we in charge of the interface?
        POP     DS                      ;Restore DS
        JNE     JumpOld16               ;If not, chain to old ISR
        NOT     AX                      ;Flip the bits in AX
        PUSH    ES
        PUSH    DI
        PUSH    BX
        PUSH    CX
        MOV     CX,BiosDataSeg          ;ES:DI => intra-app comm area
        MOV     ES,CX
        MOV     DI,IntraApp
        MOV     CX,CS:OurDs             ;CX:BX => ThisIfc
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
        CLI
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

        CLI                             ;emulate interrupt
        CallFar CS:OldInt16

        PUSHF                           ;save flags
        STI
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
        CLI                             ;emulate interrupt
        CallFar CS:OldInt16

        ;Push flags and address on the stack so we can use IRET to return
        PUSHF                           ;save flags
        STI
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

        CLI                             ;emulate interrupt
        CallFar CS:OldInt16

        PUSHF
        STI
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

;************************************************* Int28

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

        STI
        PUSH    DS
        PUSH    BX
        LDS     BX,CS:DosInUsePtr       ;DS:BX => DosInUse flag
        CMP     BYTE PTR [BX],2         ;BX <= 2?
        JAE     Passing                 ;pass if DosInUse >= 2
        CMP     CS:HaveDosCritical,1    ;Do we know where DOS critical flag is?
        JNE     NotPassing              ;If not, don't check it here
        LDS     BX,CS:DosCriticalPtr    ;Check DOS critical flag
        CMP     BYTE PTR [BX],0         ;is it 0?
        JE      NotPassing              ;continue if it's 0
Passing:
        POP     BX
        POP     DS
        JMP     SHORT I28Pass

NotPassing:
        POP     BX
        POP     DS

        OR      StateFlags,InInt28      ;now we're in use
        POP     CS:TempInt28.Ofst       ;caller's offset
        POP     CS:TempInt28.Segm       ;caller's segment
        CLI
        CallFar CS:OldInt28             ;call original routine

        PUSHF                           ;caller's flags on stack
        STI
        PUSH    CS:TempInt28.Segm       ;caller's segment
        PUSH    CS:TempInt28.Ofst       ;caller's offset on stack

        CMP     CS:PopTicker,0          ;check if we're waiting to pop up
        JNE     Go28                    ;if so, do it
        AND     StateFlags,NotIn28      ;now we're not in use
        IRET

I28Pass:
        CLI
        JmpFar  CS:OldInt28             ;pass to old int 28 handler

Go28:
        PUSH    DS
        PUSH    BX
        PUSH    AX

        MOV     PopupsEnabled,0         ;disable popups
        MOV     DS,CS:OurDS             ;get data seg
        SetZero AX                      ;zero AX
        MOV     AL,CS:PopupToCall       ;Popup index in AL
        DEC     AX                      ;less 1 for base of 1
        MOV     BX,Offset DosWaitFlags  ;offset of DosWaitFlags array
        XLAT                            ;Get value. If not 0, check DOS
        SUB     AL,DosTrapsSet          ;AL = (1-0), (1-1), (0-0), or (0-1)
        MOV     CS:TempTrapFlag,AL      ;Save the result

        POP     AX                      ;restore used regs
        POP     BX
        POP     DS
        AND     StateFlags,NotIn28      ;now we're not in use
        JMP     DosOkToPop              ;call routine which won't return
        IRET                            ;put this here anyhow

Int28   ENDP

COMMENT |
  The remainder of these interrupt handlers are simple filters that prevent
  us from popping up when any one of these interrupts is in progress by setting
  a bit in the system state flag. Their overhead is minimal.
|

;************************************************* Int5

COMMENT |
  PrtSc interrupt handler.
  ------------------------
  Intercepts the PrtSc interrupt to contend with programs that generate the
  interrupt themselves to do screen dumps.
|

TempInt05      Pointer <>

Int5    PROC NEAR

        CLI
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
        CLI
        PUSH    CS:TempInt05.Segm       ;Segment of caller
        PUSH    CS:TempInt05.Ofst       ;Offset of caller

        ;reset our in-interrupt flag and return
        AND     StateFlags,NotIn05      ;Reset our bit
        IRET

Int5    ENDP

;************************************************* Int10

COMMENT |
  BIOS video interrupt handler.
  -----------------------------
  Intercepts the BIOS video interrupt to prevent problems when running in
  the OS/2 compatibility box. Not captured if running under DOS 2.x or 3.x.
|

TempInt10       Pointer <>

Int10   PROC NEAR

        CLI
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
        CLI
        PUSH    CS:TempInt10.Segm       ;Segment of caller
        PUSH    CS:TempInt10.Ofst       ;Offset of caller

        AND     StateFlags,NotIn10      ;reset our in-interrupt flag and return
        IRET

Int10   ENDP

;************************************************* Int13

COMMENT |
  BIOS disk interrupt handler.
  ----------------------------
  Intercepts the BIOS disk interrupt to contend with programs that bypass DOS
  and access the disk directly.
|

TempInt13       Pointer <>

Int13   PROC NEAR

        CLI
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
        CLI
        PUSH    CS:TempInt13.Segm       ;Segment of caller
        PUSH    CS:TempInt13.Ofst       ;Offset of caller

        AND     StateFlags,NotIn13      ;reset our in-interrupt flag
        IRET

Int13   ENDP

;************************************************* Int14

COMMENT |
  BIOS communications interrupt handler.
  --------------------------------------
  Intercepts the BIOS communications interrupt to prevent problems when running
  in the OS/2 compatibility box. Not captured if running under DOS 2.x or 3.x.
|

TempInt14       Pointer <>

Int14   PROC NEAR

        CLI
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
        CLI
        PUSH    CS:TempInt14.Segm       ;Segment of caller
        PUSH    CS:TempInt14.Ofst       ;Offset of caller

        AND     StateFlags,NotIn14      ;reset our in-interrupt flag and return
        IRET

Int14   ENDP

;************************************************* Int17

COMMENT |
  BIOS printer interrupt handler.
  ------------------------------
  Intercepts the BIOS printer interrupt to prevent problems when running in the
  OS/2 compatibility box. Not captured if running under DOS 2.x or 3.x.
|

TempInt17       Pointer <>

Int17   PROC NEAR

        CLI
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
        CLI
        PUSH    CS:TempInt17.Segm       ;Segment of caller
        PUSH    CS:TempInt17.Ofst       ;Offset of caller

        AND     StateFlags,NotIn17      ;reset our in-interrupt flag and return
        IRET

Int17   ENDP

;************************************************* Int25

COMMENT |
  DOS absolute disk read interrupt handler.
  -----------------------------------------
  Intercepts the absolute disk read interrupt to contend with programs
  that use this interrupt.
|

TempInt25       Pointer <>

Int25   PROC NEAR

        CLI
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
        CLI
        PUSH    CS:TempInt25.Segm       ;Segment of caller
        PUSH    CS:TempInt25.Ofst       ;Offset of caller

        AND     StateFlags,NotIn25      ;reset our in-interrupt flag and return
        IRET

Int25   ENDP

;************************************************* Int26

COMMENT |
  DOS absolute disk write interrupt handler.
  -----------------------------------------
  Intercepts the absolute disk write interrupt to contend with programs
  that use this interrupt.
|

TempInt26       Pointer <>

Int26   PROC NEAR

        CLI
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
        CLI
        PUSH    CS:TempInt26.Segm       ;Segment of caller
        PUSH    CS:TempInt26.Ofst       ;Offset of caller

        AND     StateFlags,NotIn26      ;reset our in-interrupt flag and return
        IRET

Int26   ENDP

;************************************************* Save3Fvector

Save3Fvector    PROC NEAR

        GetVector       3Fh, CS:NewInt3F        ;for overlays
        RET

Save3Fvector    ENDP

;************************************************* Int21

CF      =       0000000000000001b       ;Carry flag mask
NotCF   =       1111111111111110b       ;Mask to clear CF
Flags21 EQU     WORD PTR [BP+6]         ;pushed flags

Int21           PROC FAR

        MOV     CS:Int24ErrCode,0       ;clear critical error code

        PUSHF                           ;emulate interrupt
        CallFar CS:SaveInt21

        PUSH    BP                      ;set up stack frame
        MOV     BP,SP

        JNC     Int21check              ;carry flag set?

        CMP     CS:Int24ErrCode,0       ;carry flag set--check critical error
        JNE     Int21error              ;if critical error, return special code
        JMP     SHORT Int21setCarry     ;else just set carry flag

Int21check:
        CMP     CS:Int24ErrCode,0       ;is there an INT 24 error pending?
        JNE     Int21error              ;if so, return special error code

        AND     Flags21,NotCF           ;else, clear carry flag
        POP     BP
        IRET

Int21error:
        MOV     AH,0                    ;error code in AX
        MOV     AL,CS:Int24ErrCode
        MOV     CS:Int24ErrCode,AH      ;clear critical error code

Int21setCarry:
        OR      Flags21,CF              ;set carry flag
        POP     BP
        IRET

Int21           ENDP

;************************************************* Int24

;procedure Int24
;Interrupt handler for DOS critical errors

FailCode        = 3
IgnoreCode      = 0

Int24           PROC NEAR

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

Int24           ENDP

;************************************************* InitTsrPtrs

;procedure InitTsrPtrs;

;Initializes pointers to hidden variables and pointers that indicate when
;DOS is active.

InitTsrPtrs     PROC NEAR

        ;initialization
        MOV     DosTrapsSet,False       ;DOS traps not set
        DosCall 2Fh                     ;Get current DTA
        SetPtr  CS:OurDTA, ES, BX       ;Save our DTA (in ES:BX)
        MOV     AX,PrefixSeg            ;AX = our PSP
        MOV     CS:OurPSP,AX            ;Save in CS-relative storage

        ;check to see if it's an enhanced keyboard
        MOV     AX,40h                  ;check bit 4 of byte at $40:$96
        MOV     ES,AX
        MOV     DI,96h
        TEST    BYTE PTR ES:[DI],00010000b
        JZ      EnhDone                 ;if bit is set, it's enhanced
        MOV     CS:IsEnhanced,True      ;Store CS-relative variable
EnhDone:

        ;check for mouse
        MOV     CS:HaveMouse,0          ;assume we don't have a mouse
        MOV     AX,3533h                ;get INT 33 vector
        INT     21h
        MOV     AX,ES                   ;is vector nil?
        OR      BX,AX
        JZ      NoMouse                 ;if so, no mouse

        SetZero AX                      ;call mouse installed function
        INT     33h
        CMP     AX,-1                   ;returns 0 if not installed, else -1
        JNE     NoMouse
        MOV     CS:HaveMouse,1          ;we have a mouse

NoMouse:
        ;allow Pascal routines access to CS-relative data
        SetPtrByOfst    PopTickerPtr, CS, PopTicker
        SetPtrByOfst    PopupsEnabledPtr, CS, PopupsEnabled
        SetPtrByOfst    PopupToCallPtr, CS, PopupToCall

        ;Get current interrupt vectors

        GetVector       02h, CS:NewInt02        ;NMI
        GetVector       05h, CS:OldInt05        ;PrtSc
        GetVector       08h, CS:OldInt08        ;Clock tick
        GetVector       09h, CS:OldInt09        ;Keyboard
        GetVector       10h, CS:OldInt10        ;Video
        GetVector       13h, CS:OldInt13        ;Disk
        GetVector       14h, CS:OldInt14        ;Comm.
        GetVector       16h, CS:OldInt16        ;Keyboard
        GetVector       17h, CS:OldInt17        ;Printer
        GetVector       25h, CS:OldInt25        ;Disk read
        GetVector       26h, CS:OldInt26        ;Disk write
        GetVector       28h, CS:OldInt28        ;DOS multitasking
        GetVector       34h, CS:NewInt34        ;emulator
        GetVector       35h, CS:NewInt35
        GetVector       36h, CS:NewInt36
        GetVector       37h, CS:NewInt37
        GetVector       38h, CS:NewInt38
        GetVector       39h, CS:NewInt39
        GetVector       3Ah, CS:NewInt3A
        GetVector       3Bh, CS:NewInt3B
        GetVector       3Ch, CS:NewInt3C
        GetVector       3Dh, CS:NewInt3D
        GetVector       3Eh, CS:NewInt3E
        GetVector       75h, CS:NewInt75        ;8087 exception

        ;Get the DOS version

        DosCall 30h                     ;Get DOS version
        XCHG    AL,AH                   ;Major version # in AH, minor in AL
        MOV     DosVersion,AX           ;Save for the Pascal code

        ;Get address of the In-DOS flag

        PUSH    DS
        DosCall 34h                     ;Undocumented call to get pointer to
                                        ;In-DOS flag -- returned in ES:BX
        POP     DS                      ;Restore DS
        SetPtr  CS:DosInUsePtr, ES, BX  ;Set DOS-in-use pointer

        ;Determine the address of the DOS critical pointer based on DOS version

        SetZero CX                      ;Assume failure
        MOV     AX,DosVersion           ;Get DosVersion back into AX
        CMP     AX,0200h                ;Check for range 2.00 - 2.$FF
        JB      InitDosExit             ;Exit with error if < 2.00
        CMP     AX,0300h                ;Check for 3.00
        JA      Dos3x                   ;If higher, need extra checks
        JB      Dos2                    ;If less, it's DOS 2.x

        CMP     BX,019Ch                ;Is this Compaq DOS 3.0?
        JE      DecOffset               ;If so, critical flag below InDos
        SUB     BX,01AAh                ;Else, this is MS/PC-DOS 3.0
        JMP     SHORT SetCriticalPtr    ;Ready

Dos2:
        MOV     CS:Dos3Plus,False       ;Not running DOS 3.x or above
        INC     BX                      ;Critical pointer after in-use pointer
        JMP     SHORT SetCriticalPtr    ;Ready

Dos3x:
        CMP     AX,030Ah                ;Check for 3.10 or higher
        JB      DosCriticalUnknown      ;This shouldn't happen
        CMP     AX,0963h                ;If <=, DOS version is 3.10-9.99 !!.30
        JA      DosCriticalUnknown      ;Higher version -- presumably OS/2

DecOffset:
        DEC     BX                      ;Critical pointer before in-use pointer
        JMP     SHORT SetCriticalPtr

DosCriticalUnknown:
        MOV     CS:HaveDosCritical,0    ;set DosCriticalPtr = DosInUsePtr

SetCriticalPtr:
        INC     CX                      ;Set success flag
        SetPtr  CS:DosCriticalPtr, ES, BX       ;Set DOS critical pointer

InitDosExit:
        MOV     AX,CX                   ;Result into AX
        RET

InitTsrPtrs     ENDP

;************************************************* NopISR

;For dummy ISR's

NopISR  PROC NEAR

        IRET

NopISR  ENDP

;************************************************* UseCritical

;Force DOS 2.x to use DOS critical stack when getting/setting PSP

UseCritical     PROC NEAR

        CMP     CS:Dos3Plus,True        ;Is this DOS 3.x?
        JE      UCexit                  ;if so, do nothing
        LES     DI,CS:DosCriticalPtr    ;ES:DI => DosCriticalPtr
        MOV     BYTE PTR ES:[DI],-1     ;Tell DOS to use critical stack
UCexit:
        RET

UseCritical     ENDP

;************************************************* NotCritical

;Reset DOS 2.x for non-critical stack

NotCritical     PROC NEAR

        CMP     CS:Dos3Plus,True        ;Is this DOS 3.x?
        JE      NCexit                  ;if so, do nothing
        LES     DI,CS:DosCriticalPtr    ;ES:DI => DosCriticalPtr
        MOV     BYTE PTR ES:[DI],0      ;Tell DOS to use non-critical stack
NCexit:
        RET

NotCritical     ENDP

;************************************************* SetDosTraps

COMMENT |

  SetDosTraps
  -----------
  Makes preparations to insure that DOS is safe to use during a popup:

  -- Saves and restores current DTA and PSP, switching to ours in between
  -- Prevents ^Break/^C problems by taking over dangerous interrupts
     and changing the DOS BREAK level, restoring them after the call
  -- Sets up DOS critical error handler for the popup
  -- Saves and restores the DOS int21 saved SS:SP in the PSP !!.30

  After these preparations have been made, the popup is called. Reentrancy
  problems are avoided by means of the DosTrapsSet flag, which prohibits
  this code from being executed twice before the first session is over.
|

SaveBreak       DB      0               ;Saved ^Break state
SavePSP         DW      0               ;Saved PSP segment

PrivateVectors:
SaveInt02       Pointer <>              ;Saved vectors
SaveInt1B       Pointer <>
SaveInt21       Pointer <>              ;Old INT $21      !!.03 moved
SaveInt23       Pointer <>
SaveInt24       Pointer <>
SaveInt34       Pointer <>
SaveInt35       Pointer <>
SaveInt36       Pointer <>
SaveInt37       Pointer <>
SaveInt38       Pointer <>
SaveInt39       Pointer <>
SaveInt3A       Pointer <>
SaveInt3B       Pointer <>
SaveInt3C       Pointer <>
SaveInt3D       Pointer <>
SaveInt3E       Pointer <>
SaveInt3F       Pointer <>
SaveInt75       Pointer <>
SaveDTA         Pointer <>              ;Saved DTA
OurReturn       DW      0               ;Our return address
OurCall         Pointer <>              ;The address we're supposed to call
SaveSSSP        Pointer <>              ;Saved PSP SS:SP pointer !!.30

SetDosTraps     PROC NEAR

        OR      StateFlags,InSetTR      ;setting DOS traps
        MOV     DosTrapsSet,True        ;next time sys ok, DOS traps are set
        MOV     CS:PopupsEnabled,1      ;reenable popups

        MOV     AX,CS                   ;DS = CS
        MOV     DS,AX

        assume  DS:CODE                 ;Tell MASM that DS = CS

        POP     OurReturn               ;POP our return address off the stack

        ;save the address we're supposed to call (at ES:[BX-4])
        LES     BX,ES:[BX-4]            ;Get the address
        SetPtr  OurCall, ES, BX         ;And save it

        DosCallAX       3300h           ;Get current BREAK level
        MOV     SaveBreak,DL            ;Save current level, returned in DL
        SetZero DL                      ;0 means relax break checking
        DosCallAX       3301h           ;Set BREAK value in DL

        ;save interrupt vectors we're taking over
        GetVector 02h, SaveInt02
        GetVector 1Bh, SaveInt1B
        GetVector 21h, SaveInt21
        GetVector 23h, SaveInt23
        GetVector 24h, SaveInt24
        GetVector 34h, SaveInt34
        GetVector 35h, SaveInt35
        GetVector 36h, SaveInt36
        GetVector 37h, SaveInt37
        GetVector 38h, SaveInt38
        GetVector 39h, SaveInt39
        GetVector 3Ah, SaveInt3A
        GetVector 3Bh, SaveInt3B
        GetVector 3Ch, SaveInt3C
        GetVector 3Dh, SaveInt3D
        GetVector 3Eh, SaveInt3E
        GetVector 3Fh, SaveInt3F
        GetVector 75h, SaveInt75

        ;grab control of potentially dangerous interrupts
        MOV     DX,Offset NopIsr        ;DS:DX to points to IRET
        DosCallAX       251Bh           ;BIOS ^Break handler
        DosCallAX       2523h           ;DOS ^C handler

        MOV     DX,Offset Int24
        DosCallAX       2524h           ;DOS critical error handler

        MOV     DX,Offset Int21
        DosCallAX       2521h           ;DOS

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
        MOV     AX,CS                   ;Reset DS to CS
        MOV     DS,AX

        ;save current DTA and switch to ours
        DosCall 2Fh                     ;Get current DTA
        SetPtr  SaveDTA, ES, BX         ;Save current DTA (in ES:BX)
        LDS     DX,OurDTA               ;DS:DX points to our DTA
        DosCall 1Ah                     ;Set DTA

        assume  DS:NOTHING              ;we don't know what ds is

        ;save current PSP and switch to ours
        CALL    UseCritical             ;Switch to critical stack in DOS 2.x
        DosCallAX       5100h           ;Get current PSP
        MOV     CS:SavePSP,BX           ;Save PSP returned in BX
        MOV     BX,CS:OurPSP            ;Get our PSP

        MOV     DS,BX                   ;DS points to our PSP           !!.30
        MOV     AX,DS:[002Eh]           ;Get DOS saved SS:SP            !!.30
        MOV     DX,DS:[0030h]           ;                               !!.30
        SetPtr  SaveSSSP, DX, AX        ;Save it in our code segment    !!.30

        DosCallAX       5000h           ;Switch to our PSP
        CALL    NotCritical             ;Critical stack no longer needed

        ;reset DS to DATA
        MOV     DS,CS:OurDS             ;Restore our DS
        assume  DS:DATA                 ;Tell MASM that DS = DATA

        ;Save info for emergency exit and call the popup
        MOV     LastEntrySS,SS          ;Save SS
        MOV     LastEntrySP,SP          ;Save SP
        MOV     LastEntryIP,Offset SDTReentry   ;Save reentry offset

        ;make popup look like main block to the overlay manager
        XOR     BP,BP                   ;BP = 0

        AND     StateFlags,NotSetTR     ;done setting DOS traps
        CallFar OurCall                 ;Call the popup
        OR      StateFlags,InSetTR      ;resetting DOS traps

SDTReentry:
        ;reset DS to CS and PUSH our return address back on the stack
        MOV     AX,CS                   ;Set DS to CS
        MOV     DS,AX
        assume  DS:CODE                 ;Tell MASM that DS = CS

        PUSH    OurReturn               ;PUSH the return address back up

        ;restore saved PSP
        CALL    UseCritical             ;Switch to critical stack in DOS 2.x

        MOV     BX,OurPSP               ;                               !!.30
        MOV     ES,BX                   ;ES points to our PSP           !!.30
        LDS     DX,SaveSSSP             ;DS:DX contains SaveSSSP        !!.30
        assume  DS:NOTHING              ;we don't know what ds is       !!.30
        MOV     ES:[002Eh],DX           ;                               !!.30
        MOV     ES:[0030h],DS           ;Restore DOS saved SS:SP        !!.30

        MOV     BX,SavePSP              ;BX = saved PSP
        DosCallAX       5000h           ;Set PSP function
        CALL    NotCritical             ;Critical stack no longer needed

        ;restore saved DTA
        LDS     DX,SaveDTA              ;DS:DX points to saved DTA
        DosCall 1Ah                     ;Set DTA function

        assume  DS:NOTHING              ;Tell MASM that DS = ?

        ;restore saved interrupt vectors
        LDS     DX,CS:SaveInt02
        DosCallAX       2502h
        LDS     DX,CS:SaveInt1B
        DosCallAX       251Bh
        LDS     DX,CS:SaveInt21
        DosCallAX       2521h
        LDS     DX,CS:SaveInt23
        DosCallAX       2523h
        LDS     DX,CS:SaveInt24
        DosCallAX       2524h
        LDS     DX,CS:SaveInt34
        DosCallAX       2534h
        LDS     DX,CS:SaveInt35
        DosCallAX       2535h
        LDS     DX,CS:SaveInt36
        DosCallAX       2536h
        LDS     DX,CS:SaveInt37
        DosCallAX       2537h
        LDS     DX,CS:SaveInt38
        DosCallAX       2538h
        LDS     DX,CS:SaveInt39
        DosCallAX       2539h
        LDS     DX,CS:SaveInt3A
        DosCallAX       253Ah
        LDS     DX,CS:SaveInt3B
        DosCallAX       253Bh
        LDS     DX,CS:SaveInt3C
        DosCallAX       253Ch
        LDS     DX,CS:SaveInt3D
        DosCallAX       253Dh
        LDS     DX,CS:SaveInt3E
        DosCallAX       253Eh
        LDS     DX,CS:SaveInt3F
        DosCallAX       253Fh
        LDS     DX,CS:SaveInt75
        DosCallAX       2575h

        ;restore DOS BREAK level
        MOV     AX,CS                   ;Reset DS to CS
        MOV     DS,AX
        assume  DS:CODE                 ;Tell MASM that DS = CS

        MOV     DL,SaveBreak            ;Restore saved break state
        DosCallAX       3301h           ;Set break check level

SetTrapExit:
        MOV     DS,CS:OurDS             ;Restore our DS
        assume  DS:DATA                 ;Tell MASM that DS = DATA

        MOV     DosTrapsSet,False       ;DOS traps are not set now
        AND     StateFlags,NotSetTR     ;not setting DOS traps now

        RET

SetDosTraps     ENDP

;************************************************* EmergencyExit

;Called by exit/error handler in case of runtime error while popped up

EmergencyExit   PROC NEAR

        CLI
        MOV     SS,LastEntrySS          ;Switch stacks
        MOV     SP,LastEntrySP
        STI
        ADD     SP,4                    ;Get rid of the parameter to the popup
        MOV     BX,LastEntryIP          ;Jump to re-entry point
        JMP     BX

EmergencyExit   ENDP

;************************************************* DosBusyFlag

;function DosBusyFlag : Byte;
;Returns current value of DOS busy flag

DosBusyFlag     PROC FAR

        GetPtr  CS:DosInUsePtr          ;ES:DI => DosInUsePtr
        MOV     AL,ES:[DI]              ;value into AL
        RET

DosBusyFlag     ENDP

;************************************************* DosCriticalFlag

;function DosCriticalFlag : Byte;
;Returns current value of DOS critical flag

DosCriticalFlag PROC FAR

        GetPtr  CS:DosCriticalPtr       ;ES:DI => DosCriticalPtr
        MOV     AL,ES:[DI]              ;value into AL
        RET

DosCriticalFlag ENDP

CODE    ENDS

        END
