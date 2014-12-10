;******************************************************
;                   OPSWAP.ASM 1.30
;               ASM routines for OPSWAP
;     Copyright (c) TurboPower Software 1987, 1992.
; Portions Copyright (c) Sunny Hill Software 1985, 1986
;     and used under license to TurboPower Software
;                All rights reserved.
;******************************************************

; Remove the semi-colon on the next line to enable XMS support
SupportXms      =       1

; Remove the semi-colon on the next line to enable Lantastic 5.0 support !!.22
;Lantastic5      =       1                                              ;!!.22

        INCLUDE OPCOMMON.ASM

DATA    SEGMENT WORD PUBLIC
        EXTRN   PrefixSeg : WORD                ;Our PSP segment
DATA    ENDS

;-----------------------------------------------------------------------------
CODE    SEGMENT WORD PUBLIC
        ASSUME  CS:CODE,DS:DATA
        PUBLIC  SwapOut
        PUBLIC  CSRelData
        PUBLIC  Intercom
        PUBLIC  SwappedFlag, SwappedOutFlag
        PUBLIC  SwapProgIn
        PUBLIC  SwapProgOut

        PUBLIC  Int5, Int8, Int9, Int10, Int13, Int14, Int16, Int17, Int25,
        PUBLIC  Int26, Int28
        PUBLIC  NopISR
        PUBLIC  InitCSPops
;the following is used to insure that the assembly and pascal files are
;matched for the conditional define SupportXms
IFDEF SupportXms
        PUBLIC  UsingXms
UsingXms:
ELSE
        PUBLIC  NotUsingXms
NotUsingXms:
ENDIF
;-----------------------------------------------------------------------------
MaxScanCode     =       58h             ;Do not change !!
MaxScanCode2    =       58h

IfcSignature    =       0F0F0h          ;Do not change
IfcSignature2   =       0E0E0h          ;Do not change
NextIfc         =       14              ;Offset of NextIfc field in IFC record


HiddenFileAttr  EQU     6               ;Swap file attribute (hidden+system)
NormalFileAttr  EQU     0               ;normal file attribute
EmsPageSize     EQU     16384           ;Size of EMS page
FileBlockSize   EQU     32768           ;Size of a file block
XmsBlockMax     EQU     40000h          ;256k largest XMS block !!.02
VectorSize      EQU     1024

DiskError       EQU     5
EMSError        EQU     4

StkSize         EQU     256             ;Bytes in temporary stack

;the BufSize parameter must be evenly divisable by 16
BufSize         EQU     0800h           ;2k buffer for single swap file !!.02
BufParas        EQU     BufSize shr 4   ;number of paragraphs in buffer !!.02

lo              EQU     (WORD PTR 0)    ;Convenient typecasts
hi              EQU     (WORD PTR 2)

MSQuickFix      EQU 00000010b           ; bit for MS Quick bug    !!.01
;-----------------------------------------------------------------------------
;Variables in CS
Status          DW      0               ;Swap status code
LeftToSwap      DD      0               ;Bytes left to move
SaveSP          DW      0               ;Original stack pointer
SaveSS          DW      0               ;Original stack segment

SMSaveSS        DW      0
SMSaveSP        DW      0
SMSaveAX        DW      0

SaveBP          DW      0
ParasWeHave     DW      0               ;Paragraphs allocated to process

UserRoutine     DD      0
ReturnAddress   DD      0


TempStack       DB      StkSize DUP(0)  ;Temporary stack
StackTop        LABEL   WORD            ;Initial top of stack

NewStackTop     DW      OFFSET StackTop

SwappedFlag     LABEL   BYTE
SwappedIn       DB      0
SwappedOutFlag  LABEL   BYTE
SwappedOut      DB      0
NetWareUnderneath DB    0

HaveDosCritical DB      1               ;Set to 1 if DosCriticalPtr is reliable

;Old interrupt vectors
OldInt05        Pointer <>              ;Old INT $05 handler (PrtSc)
OldInt08        Pointer <>              ;Old INT $08 handler (clock)
OldInt09        Pointer <>              ;Old INT $09 handler (keyboard)
OldInt10        Pointer <>              ;Old INT $10 handler (video)
OldInt13        Pointer <>              ;Old INT $13 handler (disk)
OldInt14        Pointer <>              ;Old INT $14 handler (comm.)
OldInt16        Pointer <>              ;Old INT $16 handler (keyboard)
OldInt17        Pointer <>              ;Old INT $17 handler (printer)
OldInt25        Pointer <>              ;Old INT $25 handler (abs. disk read)
OldInt26        Pointer <>              ;Old INT $26 handler (abs. disk write)
OldInt28        Pointer <>              ;Old INT $28 handler (DOS multitasking)

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

;Following needed for TSR's that use the 8087
NewInt02        Pointer <>              ;NMI interrupt
NewInt75        Pointer <>              ;8087 exceptions

;following needed to set up temporary interrupt handlers
SaveInt21       Pointer <>
Int24ErrCode    DB      0               ;critical error code

OurDS           DW      DATA            ;Value of DS -- init'd by EXE loader
OurDTA          Pointer <>              ;Our DTA
OurPSP          DW      0               ;Our PSP
OurMCB          DW      0               ;Our memory control block
;TempTrapFlag    DB      0               ;Flag to indicate if traps need setting
IsEnhanced      DB      0               ;1 if it's an enhanced keyboard

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
   | | | | +-----------------------  --  Setting DOS traps
   +-+-+-+------------------------- xxx  Bits $C-$F are reserved
@

;****************************************************** Bit masks

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

MouseFlag       =       0000000000000001b       ;use special mouse code !!.02
MsQuickFlag     =       0000000000000010b       ;special MS fix         !!.02
SingleFlag      =       0000000000000100b       ;use single swap file   !!.02
EmsFlag         =       0000000000001000b       ;using EMS              !!.02
XmsFlag         =       0000000000010000b       ;using XMS              !!.02
XmsDirection    =       0100000000000000b       ;internal XMS direction flag!!.02
SaveUarts       =       0000000000100000b       ;save UART interrupts   !!.13

CommandMask     =       1000000000000000b       ;used to indicate COMMAND.COM active !!.02
NoCommandMask   =       0111111111111111b       ;used to indicate COMMAND.COM not active !!.02

MaxPopups       EQU     8

; This is the start of the CS relative data needed by TSRSwap1
CSRelData       LABEL   BYTE
PopupAddrs      DD      MaxPopups DUP (0)       ;Addresses of popup routines
PopupStacks     DD      MaxPopups DUP (0)       ;Stacks for popup routines
PopupInUse      DB      MaxPopups DUP (0)       ;Flags for popups in use
PopupKeys       DB      MaxScanCode2+1 DUP (0)  ;Trigger keys for popups
ShiftKeys       DB      MaxScanCode2+1 DUP (0)  ;Shift keys for popups

; fields in the ISR_Record
        IntNum  =       0
        OrigAddr =      1
        NewAddr =       5
        Captured =      9

MaxISRs         EQU     15
SizeOfISRRecord EQU     10

ISRArray        DB      SizeOfISRRecord*MaxISRs DUP (0)
PopTicker       DW      0               ;Decremented as the clock ticker
PopupToCall     DB      0               ;Index number of a pending popup
PopupsEnabled   DB      0               ;Boolean, determines if we react to pop keys

DosVersion      DW      0               ;Version of DOS

PagesInEMSCS    DW      0
BytesSwappedCS  DD      0               ;Bytes to move during a swap
EmsHandleCS     DW      0               ;EMS handle
FrameSegCS      DW      0               ;Segment of EMS page window
FileHandleCS    DW      0               ;DOS file handle
NewPrefixSegCS  DW      0

EMSAllocated    DB      0
FileAllocated   DB      0
SwapMessageOn   DB      1
SwapMessageAttr DB      70h
SwapMessageRow  DB      24

ThisIFC:
                DB      26 DUP (0)              ;Standard interface record
ModuleName      DB      21 DUP (0)
IfcInstalled    DB      0                       ;True if we're in charge of IFC
Disable         DB      0                       ;True if unload requested
SwapEnabled     DB      0

SwapFileMaxLen  EQU     68
Swap1FileCS:                                    ; pathname for swap file 1
                DB      SwapFileMaxLen DUP (0)
Swap2FileCS:                                    ; pathname for swap file 2
                DB      SwapFileMaxLen DUP (0)

ISRTable        LABEL   BYTE                    ; array of ISRs
                DW      OFFSET Int5             ;1
                DW      OFFSET Int8             ;2
                DW      OFFSET Int9             ;3
                DW      OFFSET Int13            ;4
                DW      OFFSET Int16            ;5
                DW      OFFSET Int25            ;6
                DW      OFFSET Int26            ;7
                DW      OFFSET Int28            ;8
                DW      OFFSET Int10            ;9
                DW      OFFSET Int14            ;10
                DW      OFFSET Int17            ;11
                DW      OFFSET NopISR           ;12
                DW      OFFSET SwapCmdEntry     ;13

        ;internal variables

Vectors         DB      1024 DUP (0)            ; used to save entire ISR
                                                ; vector table
UartInts        DW      4 DUP (0)               ; used to save UART int states

CmdEntryHandle  DB      0

UserExitProc    Pointer <>                      ; pointer to user exit proc

LastEntrySS     DW      ?                       ;Data needed for EmergencyExit
LastEntrySP     DW      ?
LastEntryIP     DW      ?
LastEntryCS     DW      ?

NewInt3F        Pointer <>                      ; used to store int 3fh vector

SwapFlags       DW      0                       ; !!.02
MouseSavePtr    Pointer <>
MouseSaveFlags  DW      0
FileAttr        DW      HiddenFileAttr          ;file attribute for swap files

DosInUsePtr     Pointer <>                      ;Non-0 if DOS is in use
AllOfMem        DW      0                       ;All memory used by our program
StartMCB        DW      0                       ;First DOS MCB
XmsControlCS    Pointer <>                      ;Xms Control function

DosCriticalPtr  Pointer <>                      ;Non-0 if DOS is critical

PopUpStk        Pointer <>                      ; far pointer to a popup stack
PopupStackIndex DW      ?
PopTimeOut      DW      36              ;Default timeout number of clock ticks
                                        ;(2 seconds on PC)

DosTrapsSet     DB      ?                 ;True if special trapping of DOS
                                          ;INT vectors, etc. has been done

IFDEF SupportXms

ExtMemMoveBlock STRUC
        emLen           DD  ?
        emSrcHandle     DW  ?
        emSrcOfs        DD  ?
        emDestHandle    DW  ?
        emDestOfs       DD  ?
ExtMemMoveBlock ENDS

XmsMoveBlock    ExtMemMoveBlock <>

ENDIF

SwapInMsg       DB      'Swapping in...',0
SwapOutMsg      DB      'Swapping out...',0
FatalSwapMsg    DB      'Fatal error swapping between file and memory. Restart with <Ctrl>,<Alt><Del>.',0

;-----------------------------------------------------------------------------
;Macros
MovSeg          MACRO Dest,Src          ;Set one segment register to another
        PUSH    Src
        POP     Dest
                ENDM

MovMem          MACRO Dest,Src          ;Move from memory to memory via AX
        MOV     AX,Src
        MOV     Dest,AX
                ENDM

InitSwapCount   MACRO                   ;Initialize counter for bytes to swap
        MovMem  LeftToSwap.lo,BytesSwappedCS.lo
        MovMem  LeftToSwap.hi,BytesSwappedCS.hi
                ENDM

SetSwapCount    MACRO BlkSize           ;Return CX = bytes to move this block
        LOCAL   FullBlk                 ;...and reduce total bytes left to move
        MOV     CX,BlkSize              ;Assume we'll write a full block
        CMP     LeftToSwap.hi,0         ;Is high word still non-zero?
        JNZ     FullBlk                 ;Jump if so
        CMP     LeftToSwap.lo,BlkSize   ;Low word still a block or more?
        JAE     FullBlk                 ;Jump if so
        MOV     CX,LeftToSwap.lo        ;Otherwise, move what's left
FullBlk:SUB     LeftToSwap.lo,CX        ;Reduce number left to move
        SBB     LeftToSwap.hi,0
                ENDM

NextBlock       MACRO SegReg, BlkSize   ;Point SegReg to next block to move
        MOV     AX,SegReg
        ADD     AX,BlkSize/16           ;Add paragraphs to next segment
        MOV     SegReg,AX               ;Next block to move
        MOV     AX,LeftToSwap.lo
        OR      AX,LeftToSwap.hi        ;Bytes left to move?
                ENDM

VideoInt        MACRO FuncAH
        MOV     AH,FuncAH
        PUSH    BP
        INT     10h
        POP     BP
                ENDM

EmsCall         MACRO FuncAH            ;Call EMM and prepare to check result
        MOV     AH,FuncAH               ;Set up function
        INT     67h
        OR      AH,AH                   ;Error code in AH
                ENDM

DosCallAH       MACRO FuncAH            ;Call DOS subfunction AH
        MOV     AH,FuncAH
        INT     21h
                ENDM

DosCallAX       MACRO FuncAX            ;Call DOS subfunction AX
        MOV     AX,FuncAX
        INT     21h
                ENDM

Minimum         MACRO Mem1,Mem2
        LOCAL   MinExit                 ; return minimum of two items
        CMP     Mem1,Mem2
        JB      MinExit                 ; !!
        MOV     Mem1,Mem2
MinExit:
                ENDM


MoveFast        MACRO                   ;Move CX bytes from DS:SI to ES:DI
        CLD                             ;Forward
        SHR     CX,1                    ;Convert to words
        REP     MOVSW                   ;Move the words
        RCL     CX,1                    ;Get the odd byte, if any
        REP     MOVSB                   ;Move it
                ENDM

RestoreTPStack  MACRO                   ; macro to restore Turbo Pascal's stack
        CLI
        MOV     SS,SaveSS
        MOV     SP,SaveSP
        STI
                ENDM

SetTempStack    MACRO                   ;Switch to temporary stack
        MOV     AX,NewStackTop          ;Point to top of stack
        MOV     BX,CS                   ;Temporary stack in this code segment
        CLI                             ;Interrupts off
        MOV     SS,BX                   ;Change stack
        MOV     SP,AX
        STI                             ;Interrupts on
                ENDM

InitSwapFile    MACRO                   ; macro to seek start of swap file
        MOV     BX,FileHandleCS         ;BX = handle of swap file
        XOR     CX,CX
        XOR     DX,DX                   ;Start of file
        DosCallAX 4200h                 ;DOS file seek
                ENDM

FileClose       MACRO                   ; macro to close a swapfile
        MOV     BX,FileHandleCS
        DosCallAH 3Eh
                ENDM


; DX has filename offset
DeleteOutFile   MACRO
        XOR     CX,CX                   ;Normal attribute
        DosCallAX 4301h                 ;Set file attribute
        DosCallAH 41h
                ENDM

CallDOSAndCheckError MACRO FuncAH  ; Call DOS subfunction AH and abort if
              LOCAL Done           ;   an error is reported in carry flag
        MOV     AH,FuncAH
        INT     21h
        JNC     Done
        Call    HaltWithMessage
Done:
ENDM

INCLUDE OPSWAPI.ASM

;some code moved from here into OPSWAPI.ASM as of 1.21    !!.03

;perform the swap
SwapIt          PROC NEAR
        MovSeg  DS,CS
        TEST    CS:SwapFlags,EmsFlag   ; using ems?        !!.02 !!.13
        JZ      SINotEms
        CALL    SwapMoveEMS            ; perform swap to EMS
        JMP     SHORT SIExit
SINotEMS:

IFDEF SupportXms                       ; !!.02 begin
        TEST    CS:SwapFlags,XmsFlag   ; using XMS? !!.13
        JZ      SINotXms
        CALL    SwapMoveXms            ; perform swap to XMS
        JMP     SHORT SIExit
SINotXms:
ENDIF                                  ; !!.02 end

        CALL   SwapMoveDisk            ; perform swap to disk

SIExit:
        MOV     AX,SEG DATA
        MOV     DS,AX                  ;Restore DS to Turbo DS
        RET

SwapIt          ENDP

UnLoadTSR       PROC NEAR

        MOV     AH,49h
        MOV     ES,OurPSP
        MOV     BX,2Ch
        MOV     BX,WORD PTR ES:[BX]    ; get our environment memory segment
        MOV     ES,BX
        INT     21h                    ; release memory

        CMP     SwapEnabled,0
        JE      UTSR_Memory

        TEST    CS:SwapFlags,EmsFlag   ; using ems?  !!.02 !!.13
        JZ      UTSR_DiskFile          ;             !!.02

        MOV     DX,CS:EmsHandleCS
        EmsCall 45h                    ; release EMS handle and memory
        JMP     SHORT UTSR_Memory

UTSR_DiskFile:
IFDEF SupportXms
        TEST    CS:SwapFlags, XmsFlag  ;are we using XMS !!.02 !!.13
        JNZ     UTSR_Xms
ENDIF
        PUSH    DS
        MOV     CX,CS
        MOV     DS,CX                  ; ds = cs
        XOR     CX,CX                  ;Normal attribute
        MOV     DX,OFFSET Swap1FileCS  ;DS:DX -> ASCIIZ swap name

        DosCallAX 4301h                ;Set file attribute
        DosCallAH 41h                  ;Delete file

        MOV     DX,OFFSET Swap2FileCS  ;DS:DX -> ASCIIZ swap name
        DosCallAX 4301h                ;Set file attribute
        DosCallAH 41h                  ;Delete file
        POP     DS
IFDEF SupportXms                       ; !!.02 begin
        JMP     SHORT UTSR_Memory
UTSR_XMS:
        MOV     DX,CS:EmsHandleCS
        MOV     AH,0Ah
        PUSH    BX                     ;account for DR-DOS XMS Bug !!.13
        CALL    DWORD PTR CS:[XmsControlCS]
        POP     BX                     ;account for DR-DOS XMS Bug !!.13
ENDIF                                  ; !!.02 end
UTSR_Memory:
        ; release our memory
        MOV     AH,49h
        MOV     ES,OurPSP              ; release program's memory
        INT     21h

        RET

UnLoadTSR       ENDP


DSWCurrentWidth db ?
DSWCurrentMode  db ?
DSWCurrentPage  db ?
DSWSwapMsgRow   db ?
DSWSaveCursor   dw ?
ScreenBuffer    DB  80*2 DUP(0)

; SI has OFFSET of message to display
DisplaySwapMsg  PROC NEAR
        PUSH    BP
        PUSH    DS
        PUSH    ES
        PUSH    DI
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX

        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        CLD
        VideoInt 0Fh

        MOV     DSWCurrentPage,BH
        Minimum AH,80

        MOV     DSWCurrentWidth,AH
        MOV     DSWCurrentMode,AL

        CMP     AL,3
        JBE     DSM_ModeOK          ;!!.12
        CMP     AL,7
        JE      DSM_ModeOK
        JMP     DSM_ExitP

DSM_ModeOK:
        MOV     DL,SwapMessageRow    ; DSWSwapMsgRow = SwapMessageRow
        MOV     DSWSwapMsgRow,DL

        CMP     DL,0FEh              ; see if SwapMessageRow is 255-1
        JNE     DSM_DoIt             ; no, so jump forward
        PUSH    DS                   ; get current screen height from
        PUSH    SI                   ; BIOS data area
        MOV     SI,40h
        MOV     DS,SI                ; Height at 0040h:0084h
        MOV     SI,84h
        MOV     DL,BYTE PTR [SI]     ; Current height now in DL
        CMP     DL,24                ; Make sure it is at least 25-1
        JGE     DSM_HeightOK         ; if so, OK
        MOV     DL,24                ; otherwise, set equal to 25-1

DSM_HeightOK:
        MOV     DSWSwapMsgRow,DL     ; set DSWSwapMsgRow to value in DL

        POP     SI
        POP     DS

DSM_DoIt:
        VideoInt 03h
        MOV     DSWSaveCursor,DX
        MOV     DH,DSWSwapMsgRow     ; y = SwapMessageRow
        MOV     DL,00h               ; x = 0
        XOR     CH,CH
        MOV     CL,DSWCurrentWidth
        MOV     BL,SwapMessageAttr
        MOV     DI,OFFSET CS:ScreenBuffer
        PUSH    DX
DSM_Loop:                             ; save the contents of the status line
        VideoInt 02h                  ; position the cursor
        VideoInt 08h                  ; get the character and attribute
        STOSW                         ; save it to screen buffer
        INC     DL                    ; next column (y)
        LOOP    DSM_Loop

        POP     DX                    ; blank out status line

        VideoInt 02h
        MOV     CL,DSWCurrentWidth
        MOV     AL,' '
        VideoInt 09h

        MOV     DL,2
        MOV     CX,1
DSM_Message:
        LODSB
        CMP     AL,0
        JE      DSM_RestoreCursor
        PUSH    AX
        VideoInt 02h
        POP     AX
        VideoInt 09h
        INC     DL
        JMP     SHORT DSM_Message

DSM_RestoreCursor:
        MOV     DX,DSWSaveCursor      ; restore original cursor position
        VideoInt 02h
DSM_ExitP:
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        POP     DI
        POP     ES
        POP     DS
        POP     BP
        RET

DisplaySwapMsg  ENDP


ClearSwapMsg    PROC NEAR
        PUSH    DS
        PUSH    SI
        PUSH    DI
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX

        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        MOV     AL,DSWCurrentMode
        CMP     AL,3
        JBE     CSM_ModeOK           ;!!.12
        CMP     AL,7
        JE      CSM_ModeOK
        JMP     SHORT CSM_ExitP

CSM_ModeOK:

        MOV     BH,DSWCurrentPage
        MOV     DL,00h               ; x = 0
        MOV     DH,DSWSwapMsgRow     ; y = DSWSwapMsgRow
        XOR     CH,CH
        MOV     CL,DSWCurrentWidth
        MOV     SI,OFFSET CS:ScreenBuffer

CSM_Loop:                          ; restore the bottom line of the screen
        VideoInt 02h
        CLD
        LODSW
        MOV     BL,AH
        MOV     DI,CX
        MOV     CX,1
        VideoInt 09h
        MOV     CX,DI
        INC     DL
        LOOP    CSM_Loop

        MOV     DX,DSWSaveCursor
        VideoInt 02h

CSM_ExitP:
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        POP     DI
        POP     SI
        POP     DS

        RET
ClearSwapMsg    ENDP

HaltWithMessage PROC NEAR
        MOV     SI,offset CS:FatalSwapMsg
        CALL    DisplaySwapMsg
HangIt:
        JMP     SHORT HangIt        ; hang the machine
HaltWithMessage     ENDP

SaveMcbId       DB 0
SaveMcbLen      DW 0

SetTempMCB      PROC NEAR
        PUSH    DS
        PUSH    AX
        PUSH    BX
        MOV     AX,OurMCB
        MOV     DS,AX
        XOR     BX,BX
        MOV     AL,BYTE PTR [BX]
        MOV     CS:SaveMcbID,AL
        MOV     BYTE PTR [BX],'Z'
        ADD     BX,3
        MOV     AX,WORD PTR [BX]
        MOV     CS:SaveMcbLen,AX
        MOV     AX,CS:AllOfMem
        MOV     WORD PTR [BX],AX
        POP     BX
        POP     AX
        POP     DS
        RET
SetTempMCB      ENDP

RestoreMCB      PROC NEAR
        PUSH    DS
        PUSH    AX
        PUSH    BX
        MOV     AX,OurMCB
        MOV     DS,AX
        MOV     AL,CS:SaveMcbID
        XOR     BX,BX

        MOV     BYTE PTR [BX],AL
        ADD     BX,3
        MOV     AX,CS:SaveMcbLen
        MOV     WORD PTR [BX],AX
        POP     BX
        POP     AX
        POP     DS
        RET
RestoreMCB      ENDP

SwapProgOut     PROC NEAR
        CMP     SwapEnabled,1          ; set SwapEnabled flag
        JNE     SPO_NoSwap             ; !!.01

        CMP     SwappedIn,0            ; if not swapped in, then can't swap out
        JE      SPO_Exit

        CMP     SwapMessageOn,0
        JE      SPO_NoMessage
        PUSH    SI
        MOV     SI,OFFSET SwapOutMsg
        CALL    DisplaySwapMsg
        POP     SI
SPO_NoMessage:

        CALL    SetTempVectors         ; set up temp ISRs
        CALL    SwapIt                 ; perform the swap
        CALL    SwapVectorTable        ; swap the vector table
        CALL    RestoreMcb             ;!!!!!!

        TEST    CS:SwapFlags,SaveUarts ;save UART interrupts?           ;!!.13
        JZ      SPO_NoUartSave         ;no, skip the save               ;!!.13
        CALL    EnableComPorts         ;turn UART interrupts back on    ;!!.13

SPO_NoUartSave:

        CMP     SwapMessageOn,0
        JE      SPO_NoMessage2
        CALL    ClearSwapMsg
SPO_NoMessage2:

        NOT     SwappedIn              ; flip flag

        TEST    CS:SwapFlags,MouseFlag ; if low bit zero, then no mouse !!.02
        JZ      SPO_Exit

        PUSH    AX                     ; restore mouse user-interrupt
        PUSH    CX
        PUSH    DX
        PUSH    ES
        MOV     AX,014h
        MOV     CX,MouseSaveFlags
        MOV     DX,MouseSavePtr.Segm
        MOV     ES,DX
        MOV     DX,MouseSavePtr.Ofst
        INT     033h
        POP     ES
        POP     DX
        POP     CX
        POP     AX

SPO_Exit:
        RET
SPO_NoSwap:
        TEST    CS:SwapFlags,MSQuickFix
        JZ      SPO_ResTemp            ; !!.02
        CALL    SwapVectorTable
        RET
SPO_ResTemp:                           ; !!.02
        CALL    RestTempVectors        ; !!.02
        RET
SwapProgOut     ENDP

SetTempVectors  PROC NEAR

        ;grab control of potentially dangerous interrupts
        MovSeg  DS,CS
        MOV     DX,Offset NopIsr        ;DS:DX to points to IRET
        DosCallAX       251Bh           ;BIOS ^Break handler
        DosCallAX       2523h           ;DOS ^C handler

        ;set up temporary Int24 handler
        MOV     DX,Offset Int24         ;DS:DX points to Int 24 handler
        DosCallAX       2524h           ;Set critical error handler

        RET

SetTempVectors  ENDP

CommandActive   PROC NEAR                       ; !!.02
        PUSH    DS
        PUSH    ES
        PUSH    BX
        PUSH    CX
        MOV     AX,StartMCB                     ;AX = StartMCB
        MOV     DS,AX                           ;DS = mcb segment
NextMCB:
        INC     AX                              ;AX = mcb+1
        MOV     BX,DS:[0001h]                   ;BX = psp segment of mcb
        MOV     ES,BX                           ;ES = psp segment
        CMP     BX,ES:[0016h]                   ;psp = parent segment?
        JNZ     NotCmd                          ;jump if not
        CMP     AX,BX                           ;mcb+1 = psp?
        JNZ     NotCmd                          ;jump if not
        MOV     CX,BX                           ;CX = psp of last COMMAND.COM
NotCmd:
        ADD     AX,DS:[0003h]                   ;AX = next mcb segment
        MOV     DS,AX                           ;DS = mcb segment
        CMP     BYTE PTR DS:[0000h],'Z'         ;last mcb?
        JNZ     NextMCB                         ;loop if not

;!!.12 modified to return accurate result for 4DOS
        AND     CS:SwapFlags,NoCommandMask      ;assume command not active !!.13
        CMP     CX,SavePSP                      ;last cmd = active PSP?
        JNZ     CADone                          ;jump if not
        OR      CS:SwapFlags,CommandMask        ;command is active !!.13
;!!.12 end of modifications
CADone:
        POP     CX
        POP     BX
        POP     ES
        POP     DS
        RET
CommandActive   ENDP

SwapProgIn      PROC NEAR
        POP     ReturnAddress.Ofst     ; get near return address
        CALL    CommandActive          ; determine whether COMMAND.COM !!.02
                                       ; is active

        CMP     SwapEnabled,1          ; is swapping enabled?
        JE      SPI_PerformSwap
        JMP     SPI_NoSwap

SPI_PerformSwap:
        CMP     SwappedOut,1           ; Swapped out?
        JE      SPI_GoAhead
        JMP     SPIExit                ; nope, so exit
SPI_GoAhead:
        CMP     SwappedIn,0            ; not already swapped in?
        JE      SPIOK                  ; do it
        JMP     SPIExit                ; exit
SPIOK:
        TEST    CS:SwapFlags,1        ; test low bit of Mouse flags
        JZ      SPISkipMouse           ; if zero, no mouse installed

        PUSH    AX
        PUSH    CX
        PUSH    DX
        PUSH    ES
        MOV     AX,014h
        XOR     CX,CX
        MOV     ES,CX
        MOV     DX,CX
        INT     033h                   ; save mouse user-interrupt
        MOV     MouseSaveFlags,CX
        MOV     MouseSavePtr.Segm,ES
        MOV     MouseSavePtr.Ofst,DX
        POP     ES
        POP     DX
        POP     CX
        POP     AX

SPISkipMouse:

        CMP     SwapMessageOn,0
        JE      SPI_NoMessage
        PUSH    SI
        MOV     SI,OFFSET SwapInMsg
        CALL    DisplaySwapMsg
        POP     SI
SPI_NoMessage:

        TEST    CS:SwapFlags,SaveUarts ;save UART interrupts?           ;!!.13
        JZ      SPI_NoUartSave         ;no, skip the save               ;!!.13
        CALL    DisableComPorts        ;!!.13                           ;!!.13

SPI_NoUartSave:

        NOT     SwappedIn              ; flip flag
        CALL    SetTempMCB             ;!!!!!! set up a "fake" MCB
        CALL    SwapVectorTable        ; swap vector table

        CALL    SetTempVectors         ; set temp ISRs

        MOV     NewStackTop,SP         ; save current SP in NewStackTop
        CALL    SwapIt                 ; perform the swap

        CMP     SwapMessageOn,0
        JE      SPI_NoMessage2
        CALL    ClearSwapMsg

SPI_NoMessage2:

        JMP     SHORT SPI_StackSwitch

SPI_NoSwap:
        TEST    CS:SwapFlags,MSQuickFix
        JZ      SPI_NoQuick
        CALL    SwapVectorTable
SPI_NoQuick:
        CMP     CS:Disable,0
        JNE     SPI_Unloading          ; !!.01
        CALL    SaveTempVectors        ; !!.02
        CALL    SetTempVectors         ; set temp ISRs !!.01

SPI_Unloading:                         ; !!.01
        MOV     NewStackTop,SP         ; save current SP in NewStackTop

SPI_StackSwitch:
        MOV     BX,PopupStackIndex     ; Popup stack index into BX
        CLI                            ; ints off
        MOV     AX,CS:PopupStacks[BX].Segm ; get stack segment
        MOV     SS,AX
        MOV     SP,CS:PopupStacks[BX].Ofst ; get stack pointer
        STI
SPIExit:
        JMP     WORD PTR [ReturnAddress.Ofst] ; return
SwapProgIn      ENDP


SwapCmdEntry    PROC FAR
        CMP     CS:CmdEntryHandle,0    ; is handle 0?
        JE      SCE_Exit               ; yes, so exit

        SaveAllRegs
        MOV     AL,CS:CmdEntryHandle    ;cmdEntryHandle into AL
        MOV     BYTE PTR PopUpToCall,AL ; PopUpToCall = CmdEntryHandle

        CALL    PrepareToPop           ; prepare to popup

        CALL    SetDosTraps            ; set the dos traps

        RestoreAllRegs

SCE_Exit:
        RET
SwapCmdEntry    ENDP

;* begin !!.02
;*************************************************************************
;*     The following code does not remain resident after swap if in
;*     double swap file or EMS swapping mode
;*************************************************************************

even
FirstToSave:

FPos            dw   0                 ; position within file
BytesRead       dw   0                 ; # of bytes read
MSeg            dw   0                 ; pointer to current memory segment

SingleFileSwap  PROC NEAR
    mov   ax,cs
    mov   ds,ax
    mov   dx,OFFSET CS:Swap1FileCS     ; get file name
    mov   al,2                         ; open for read and write
    CallDOSAndCheckError 3Dh           ; open file
    mov   bx,ax                        ; put file handle in bx

    mov    CS:FPos,0                   ; initialize file position marker to 0
    mov    ax,cs                       ;
    mov    CS:MSeg,ax                  ; store code segment in mseg

SSF_Loop:
    mov   dx,OFFSET CS:SingleSwapBuf   ; buffer to read into
    mov   cx,BufSize                   ; # bytes to read
    CallDOSAndCheckError 3Fh           ; read file
    mov   CS:BytesRead,ax              ; store the # of bytes actually read
    cmp   ax,0
    jnz   SSF_Continue
    jmp   SSF_Done                     ; If no bytes left, then get out.

SSF_Continue:
    mov   dx,CS:FPos                   ; current seek position, in paragraphs
    mov   cl,4
    shl   dx,cl                        ; convert to bytes (mult by 16)
    mov   ax,CS:FPos                   ; current seek position, in paragraphs
    mov   cl,12
    shr   ax,cl                        ; get that part of it that's over FFFFh
    mov   cx,ax                        ; cx,dx dword is input to DOS seek function
    mov   al,0                         ; relative to start of file
    CallDOSAndCheckError 42h           ; file seek
    mov   cx,CS:BytesRead              ; # bytes actually read
    mov   dx,OFFSET FirstToSaveSingle  ; the start of data to write
    push  ds                           ; put the real data segment on the stack
    mov   ax,CS:MSeg                   ; load the segment for current pointer
    mov   ds,ax
    CallDOSAndCheckError 40h           ; file write
    pop   ds                           ; the real data segment returns.
                                       ;!!!!!check bytes written in AX???
    mov   si,OFFSET CS:SingleSwapBuf   ; buffer to read from
    mov   di,OFFSET FirstToSaveSingle  ; destination offset
    mov   ax,CS:MSeg
    mov   es,ax                        ; es:di points to placeholder in memory being swapped
    mov   ax,CS:BytesRead              ; how many we need to copy
    cld                                ; copying forward in memory
    and   ax,1                         ; if odd, copy one byte first
    jz    SSF_Even                     ; even so move by words
    movsb                              ; copy a single byte

SSF_Even:
    mov   cx,CS:BytesRead              ; how many we need to copy
    shr   cx,1                         ; divide by two to get words
    rep   movsw                        ; copy the memory


    mov   ax,BufParas                  ; retrieve # of paragraphs per read
    add   CS:MSeg,ax                   ; move pointer along memory being swapped
    add   CS:FPos,ax                   ; move file pointer

    cmp   CS:BytesRead, BufSize        ; cmp num requested with num read
    jne   SSF_Done                     ; if not equal then all done
    jmp   SSF_Loop                     ; get next block
SSF_Done:

      CallDOSAndCheckError 3Eh         ; close file
      RET
SingleFileSwap  ENDP

SingleSwapBuf   db BufSize dup (0)     ; single swap file buffer

IFDEF SupportXms
SingleXmsSwap   PROC NEAR
      MOV       AX,CS
      MOV       DS,AX
      MOV       ES,AX
      InitSwapCount
      CLD
      XOR       DX,DX
      MOV       BX,DX
SXS_0:
      MOV       SI,OFFSET FirstToSaveSingle
      SetSwapCount BufSize             ;CX has size to move
      MOV       DI,OFFSET SingleSwapBuf
      PUSH      DS
      PUSH      SI
      TEST      CX,1
      JZ        SXS_Even
      INC       CX
SXS_Even:
      PUSH      CX
      MoveFast
      POP       CX
      MOV       DI,OFFSET CS:XmsMoveBlock
      MOV       SI,DI
      MOV       AX,CX
      STOSW
      XOR       AX,AX
      STOSW
      MOV       AX,CS:EmsHandleCS
      STOSW
      MOV       AX,BX
      STOSW
      MOV       AX,DX
      STOSW
      XOR       AX,AX
      STOSW
      POP       AX                     ;get offset into normal mem from stack
      STOSW
      POP       AX                     ;get segment into normal mem from stack
      STOSW
      PUSH      DS
      MOV       AX,CS
      MOV       DS,AX
      MOV       AH,0Bh
      PUSH      BX                     ;account for DR-DOS XMS Bug !!.13
      CALL      DWORD PTR CS:[XmsControlCS]
      POP       BX                     ;account for DR-DOS XMS Bug !!.13
      POP       DS
      TEST      AX,1
      JZ        SXS_Error

      MOV       DI,OFFSET CS:XmsMoveBlock
      ADD       DI,4
      XOR       AX,AX
      STOSW
      MOV       AX,OFFSET SingleSwapBuf
      STOSW
      MOV       AX,CS
      STOSW
      MOV       AX,CS:EmsHandleCS
      STOSW
      MOV       AX,BX
      STOSW
      MOV       AX,DX
      STOSW
      PUSH      DS
      MOV       AX,CS
      MOV       DS,AX
      MOV       AH,0Bh
      PUSH      BX                     ;account for DR-DOS XMS Bug !!.13
      CALL      DWORD PTR CS:[XmsControlCS]
      POP       BX                     ;account for DR-DOS XMS Bug !!.13
      POP       DS
      TEST      AX,1
      JZ        SXS_Error
      ADD       BX,BufSize
      ADC       DX,0
      NextBlock DS,BufSize
      JZ        SXS_Done
      JMP       SXS_0
SXS_Done:
      RET
SXS_Error:
      CALL      HaltWithMessage
SingleXmsSwap   ENDP
ENDIF

;*************************************************************************
;*     The following code does not remain resident after swap
;*     if in single swap file mode.
;*************************************************************************

even
FirstToSaveSingle:
;* end !!.02

; the following two routines are only called when in non-swapping mode !!.02

; save the temp vectors for int 1Bh, 23h, 24h, and 3Fh
Saved1B         Pointer <>
Saved23         Pointer <>
Saved24         Pointer <>
Saved3F         Pointer <>
SaveTempVectors PROC NEAR
        PUSH    AX
        PUSH    BX
        PUSH    ES
        GetVector       1Bh, CS:Saved1B
        GetVector       23h, CS:Saved23
        GetVector       24h, CS:Saved24
        GetVector       3Fh, CS:Saved3F
        POP     ES
        POP     BX
        POP     AX
        RET
SaveTempVectors ENDP

; restore the temp vectors for int 1Bh, 23h, 24h, and 3Fh
RestTempVectors PROC NEAR
        PUSH    AX
        PUSH    DX
        SetVector       1Bh, CS:Saved1B
        SetVector       23h, CS:Saved23
        SetVector       24h, CS:Saved24
        SetVector       3Fh, CS:Saved3F
        POP     DX
        POP     AX
        RET
RestTempVectors ENDP

InitCSPops      PROC FAR                     ;!!.03
        PUSH    DS

        CALL    NetWareLoaded
        MOV     NetWareUnderneath,AL

        CALL    SwapMouseLoaded
        XOR     AH,AH
        OR      CS:SwapFlags,AX        ; !!.13

        CALL    InitMCB                ; get starting MCB
        MOV     UserExitProc.lo,0      ; set userexitproc to NIL
        MOV     UserExitProc.hi,0

;Initializes pointers to hidden variables and pointers that indicate when
;DOS is active.

        ;initialization
        MOV     DosTrapsSet,False       ;DOS traps not set
        DosCall 2Fh                     ;Get current DTA
        SetPtr  CS:OurDTA, ES, BX       ;Save our DTA (in ES:BX)
        MOV     AX,PrefixSeg            ;AX = our PSP
        MOV     CS:OurPSP,AX            ;Save in CS-relative storage
        MOV     BX,AX
        DEC     AX
        MOV     CS:OurMCB,AX            ;save our MCB
        PUSH    DS
        MOV     DS,BX                   ;DS is now PSP
        MOV     DI,2
        MOV     AX,[DI]                 ;get top of memory
        SUB     AX,BX                   ;subtract our segment
        MOV     CS:AllOfMem,AX          ;save for later use
        POP     DS
        ;check to see if it's an enhanced keyboard
        MOV     AX,40h                  ;check bit 4 of byte at $40:$96
        MOV     ES,AX
        MOV     DI,96h
        TEST    BYTE PTR ES:[DI],00010000b
        JZ      EnhDone                 ;if bit is set, it's enhanced
        MOV     CS:IsEnhanced,True      ;Store CS-relative variable
EnhDone:

        ;Get current interrupt vectors

        GetVector       02h, CS:NewInt02        ;NMI interrupt
        GetVector       05h, CS:OldInt05        ;PrtSc interrupt
        GetVector       08h, CS:OldInt08        ;Clock tick interrupt
        GetVector       09h, CS:OldInt09        ;Keyboard interrupt
        GetVector       10h, CS:OldInt10        ;Video interrupt
        GetVector       13h, CS:OldInt13        ;Disk interrupt
        GetVector       14h, CS:OldInt14        ;Comm. interrupt
        GetVector       16h, CS:OldInt16        ;Keyboard interrupt
        GetVector       17h, CS:OldInt17        ;Printer interrupt
        GetVector       25h, CS:OldInt25        ;Disk read interrupt
        GetVector       26h, CS:OldInt26        ;Disk write interrupt
        GetVector       28h, CS:OldInt28        ;DOS multitasking interrupt
        GetVector       34h, CS:NewInt34        ;for emulation
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
        GetVector       75h, CS:NewInt75        ;8087 exception ???

        ;Get the DOS version

        DosCall 30h                     ;Get DOS version
        XCHG    AL,AH                   ;Major version # in AH, minor in AL
        MOV     DosVersion,AX           ;Save for the Pascal code

        ;Get address of the In-DOS flag

        PUSH    DS                      ;Save DS
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
        POP     DS
        RET

InitCSPops      ENDP

;-----------------------------------------------------------------------------
InitSwapVars    PROC NEAR
;Move variables to CS where we can easily access them later
        InitSwapCount                   ;Initialize bytes LeftToSwap
        RET
InitSwapVars    ENDP

;-----------------------------------------------------------------------------
;function SwapOut : Boolean;

SwapOut         PROC FAR
        POP     ReturnAddress.Ofst      ; pop return address off stack
        POP     ReturnAddress.Segm
        PUSH    DS
        CALL    InitSwapVars            ; init key variables
        MOV     SaveSP,SP               ;Save stack position
        MOV     SaveSS,SS
;Check for swapping to EMS or file
        CMP     EmsAllocated,0          ;Check flag for EMS method
        JZ      NotEms                  ;Jump if EMS not used
        JMP     WriteE                  ;Swap to EMS
NotEms:
IFDEF SupportXms                        ;                     !!.02
        TEST    CS:SwapFlags,XmsFlag    ;are we using XMS?    !!.02 !!.13

        JZ      WriteF                  ;                     !!.02
        JMP     WriteX
ENDIF                                   ;                     !!.02
        CMP     FileAllocated,0         ;Check flag for swap file method
        JNZ     WriteF                  ;Swap to file
        JMP     EXError                 ;Exit if no swapping method set

;Write to swap file
WriteF: MovSeg  DS,CS                   ;DS = CS
        MOV     DX,OFFSET Swap2FileCS   ;DS:DX -> ASCIIZ swap name
        XOR     CX,CX
        DosCallAX 4301h                 ;Set file attribute

        MOV     DX,OFFSET Swap1FileCS   ;DS:DX -> ASCIIZ swap name
        DosCallAX 4301h                 ;Set file attribute

        MOV     CX,FileAttr             ;Attribute for swap file
        DosCallAH 3Ch                   ;Create file
        MOV     FileHandleCS,AX           ;Save handle assuming success
        MOV     BX,AX
        JNC     EF0                     ;Failed if carry set
        JMP     EXError
EF0:    SetSwapCount FileBlockSize      ;CX = bytes to write
        TEST    CS:SwapFlags,SingleFlag ;in single swap mode?
        JZ      EF0L0
        MOV     DX,OFFSET FirstToSaveSingle
        JMP     SHORT EF0L1
EF0L0:
        MOV     DX,OFFSET FirstToSave   ;DS:DX -> start of region to save
EF0L1:
        DosCallAH 40h                   ;File write
        JC      EF1                     ;Jump if write error
        CMP     AX,CX                   ;All bytes written?
        JZ      EF2                     ;Jump if so
EF1:    JMP     EXError                 ;Exit if error
EF2:    NextBlock DS,FileBlockSize      ;Point DS to next block to write
        JNZ     EF0                     ;Loop if bytes left to write
        FileClose
        JMP     EXExitTrue              ;Done swapping out

;Write to EMS
WriteE:
        MOV     ES,FrameSegCS           ;ES -> page window
        MOV     DX,EmsHandleCS          ;DX = handle of our EMS block

        EmsCall 47h                     ; save EMS mapping context
        JZ      EE00
        JMP     ExError
EE00:
        XOR     BX,BX                   ;BX = initial logical page
        MovSeg  DS,CS                   ;DS = CS
EE0:    XOR     AL,AL                   ;Physical page 0
        EmsCall 44h                     ;Map physical page
        JZ      EE1                     ;Jump if success
        JMP     EXError                 ;Exit if error
EE1:    SetSwapCount EmsPageSize        ;CX = Bytes to move
        XOR     DI,DI                   ;ES:DI -> base of EMS page
        MOV     SI,OFFSET FirstToSave   ;DS:SI -> region to save

        MoveFast                        ;Move CX bytes from DS:SI to ES:DI
        INC     BX                      ;Next logical page
        NextBlock DS,EmsPageSize        ;Point DS to next page to move
        JNZ     EE0                     ;Loop if bytes left to move
        MOV     DX,EmsHandleCS          ;DX = handle of our EMS block


        EmsCall 48h                     ; restore EMS mapping context

        JMP     SHORT EXExitTrue

IFDEF SupportXms

WriteX:         ;write to XMS
        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        TEST    CS:SwapFlags,SingleFlag ;in single swap mode?
        JZ      WX0
        MOV     BX,OFFSET CS:FirstToSaveSingle
        JMP     SHORT WX1
WX0:
        MOV     BX,OFFSET CS:FirstToSave   ;DS:SI -> start of region to save
WX1:
        MOV     DI,OFFSET CS:XmsMoveBlock
        MOV     SI,DI
        CLD
        MOV     AX,CS:BytesSwappedCS.Lo
        MOV     DX,CS:BytesSwappedCS.Hi
        CALL    RoundUpEven
        STOSW
        MOV     AX,DX
        STOSW
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
        MOV     AH,0Bh
        PUSH    BX                     ;account for DR-DOS XMS Bug !!.13
        CALL    DWORD PTR CS:[XmsControlCS]
        POP     BX                     ;account for DR-DOS XMS Bug !!.13
        TEST    AX,1
        JZ      EXError
        JMP     SHORT EXExitTrue
ENDIF

EXError:
        XOR     AL,AL                   ; return False to indicate error
        JMP     SHORT EXExit

EXExitTrue:
        MOV     AL,1
        MOV     SwapEnabled,AL
        MOV     SwappedOutFlag,AL
EXExit:
        CALL    SaveVectorTable
        POP     DS

        JMP     DWORD PTR [ReturnAddress]
SwapOut         endp

;-----------------------------------------------------------------------------

; procedure GoResident(ExitCode : Byte);
GoResident      PROC FAR
        MOV     BX,SP
        TEST    CS:SwapFlags,SingleFlag
        JZ      GRL0
        MOV     AX,OFFSET FirstToSaveSingle+15
        JMP     SHORT GRL1
GRL0:
        MOV     AX,OFFSET FirstToSave+15
GRL1:
        MOV     CL,4
        SHR     AX,CL                   ;Convert offset to paragraphs
        MOV     DX,CS
        ADD     DX,AX
        SUB     DX,CS:OurPSP            ;BX = new paragraphs to keep

        MOV     AH,31h                  ; Dos terminate and stay resident
        MOV     AL,BYTE PTR SS:[BX+4]   ; AL gets the ExitCode
        INT     21h
        RET     2

GoResident      ENDP

GoResidentNoSwap PROC FAR
        CALL    SaveVectorTable
        MOV     BX,SP
        MOV     AH,31h
        MOV     AL,BYTE PTR SS:[BX+4]
        MOV     DX,WORD PTR SS:[BX+6]
        INT     21h
        RET     4
GoResidentNoSwap ENDP

;****************************************************** Int21

CF      =       0000000000000001b       ;Carry flag mask
NotCF   =       1111111111111110b       ;Mask to clear CF
Flags21 EQU     WORD PTR [BP+6]         ;pushed flags

Int21           PROC FAR

        MOV     CS:Int24ErrCode,0       ;clear critical error code

        PUSHF                           ;push flags
        CallFar SaveInt21               ;call old ISR

        PUSH    BP                      ;save BP
        MOV     BP,SP                   ;set up stack frame

        JNC     Int21check              ;carry flag set?

        CMP     CS:Int24ErrCode,0       ;carry flag set--check critical error
        JNE     Int21error              ;if critical error, return special code
        JMP     SHORT Int21setCarry     ;else just set carry flag

Int21check:
        CMP     CS:Int24ErrCode,0       ;is there an INT 24 error pending?
        JNE     Int21error              ;if so, return special error code

        AND     Flags21,NotCF           ;else, clear carry flag
        POP     BP                      ;restore BP
        IRET

Int21error:
        MOV     AH,0                    ;error code in AX
        MOV     AL,CS:Int24ErrCode
        MOV     CS:Int24ErrCode,AH      ;clear critical error code

Int21setCarry:
        OR      Flags21,CF              ;set carry flag
        POP     BP                      ;restore BP
        IRET

Int21           ENDP

; function Intercom(Func : Byte) : Pointer;
; func 0 - return pointer to GoResident
; func 1 - return pointer to SwapOut
; func 2 - return FirstToSave
; func 3 - return offset of GoResidentNoSwap
Intercom        PROC FAR
        MOV     BX,SP
        MOV     DX,CS
        MOV     AX,WORD PTR SS:[BX+4]
        CMP     AL,1
        JE      I_F1
        JA      I_F2

        MOV     AX,OFFSET GoResident
        JMP     SHORT I_Exit

I_F1:
        MOV     AX,OFFSET SwapOut
        JMP     SHORT I_Exit
I_F2:
        CMP     AL,2
        JA      I_F3
        TEST    CS:SwapFlags,SingleFlag
        JZ      I_F2L0
        MOV     AX,OFFSET FirstToSaveSingle

        JMP     SHORT I_Exit

I_F2L0:
        MOV     AX,OFFSET FirstToSave
        JMP     SHORT I_Exit

I_F3:   CMP     AL,3
        JA      I_Exit
        MOV     AX,OFFSET GoResidentNoSwap

I_Exit:
        RET     2
Intercom        ENDP

; set al to 1 if mouse installed
SwapMouseLoaded PROC NEAR
        MOV     AX,3533h       ;Get INT 33 vector
        INT     21h            ;call DOS
        MOV     AX,ES          ;is vector nil?
        OR      BX,AX
        JZ      SML_NoMouse    ;if so, no mouse

        XOR     AX,AX          ;Mouse Installed Flag and Reset function
        INT     033h
        CMP     AX,-1          ;function returns 0 if not installed, else -1
        JNE     SML_NoMouse

        MOV     AL,1
        JMP     SHORT SML_Exit
SML_NoMouse:
        XOR     AL,AL
SML_Exit:
        RET
SwapMouseLoaded ENDP

;procedure InitMCB;

InitMCB PROC NEAR
        MOV     AH,52h                          ;DOS get internal info
        INT     21h                             ;Call DOS
        MOV     AX,ES:[BX-2]                    ;AX = segment of start MCB
        MOV     StartMCB,AX                     ;Store it
        RET
InitMCB ENDP

;-----------------------------------------------------------------------------
CODE    ENDS
        END
