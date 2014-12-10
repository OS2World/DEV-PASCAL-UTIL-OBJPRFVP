;******************************************************
;                   OPSWAP1.ASM 1.30
;               ASM Routines for OPSWAP1
;     Copyright (c) TurboPower Software 1987, 1992.
; Portions Copyright (c) Sunny Hill Software 1985, 1986
;     and used under license to TurboPower Software
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

DATA    SEGMENT WORD PUBLIC

DATA    ENDS

CODE    SEGMENT WORD PUBLIC
        ASSUME  CS:CODE,DS:DATA

        PUBLIC  DeallocateSwapFile
        PUBLIC  DefaultDrive,DiskFree
        PUBLIC  EmsInstalled,EmsPageFrame,EmsPagesAvail
        PUBLIC  AllocateEmsPages,DeallocateEmsHandle
        PUBLIC  EmulateInt, EmergencyExit

EmsErrorCode    =       0FFFFh

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

EmsCall          MACRO FuncAH            ;Call EMM and prepare to check result
        MOV     AH,FuncAH               ;Set up function
        INT     67h
        OR      AH,AH                   ;Error code in AH
                ENDM

EmsWordResult   MACRO RegName
                LOCAL   EWRexit
                OR      AH,AH           ;AH = 0 means success
                MOV     AX,RegName      ;assume success
                JZ      EWRexit         ;Done if not 0
                MOV     AX,EmsErrorCode ;$FFFF for failure
        EWRExit:
                ENDM

DosCallAH       MACRO FuncAH            ;Call DOS subfunction AH
        MOV     AH,FuncAH
        INT     21h
                ENDM

DosCallAX       MACRO FuncAX            ;Call DOS subfunction AX
        MOV     AX,FuncAX
        INT     21h
                ENDM

Minimum         MACRO Mem1,Mem2         ; returns the minimum of two values
        LOCAL   MinExit
        CMP     Mem1,Mem2
        JAE     MinExit
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

;-----------------------------------------------------------------------------
;procedure DeallocateSwapFile(Handle1 : Word; var Swap1,Swap2 : SwapFileName);
;
DeallocateSwapFile PROC NEAR
        PUSH    BP
        MOV     BP,SP
        PUSH    DS

        MOV     BX,[BP+12]              ;Handle of swap file
        DosCallAH 3Eh                   ;Close file
        XOR     CX,CX                   ;Normal attribute

                                        ;Delete SwapFile1
        LDS     DX,DWORD PTR [BP+8]     ;DS:DX --> AsciiZ string
        DosCallAX 4301h                 ;Set file attribute
        DosCallAH 41h                   ;Delete file

                                        ;Delete SwapFile2
        LDS     DX,DWORD PTR [BP+4]     ;DS:DX --> AsciiZ string
        DosCallAX 4301h                 ;Set file attribute
        DosCallAH 41h                   ;Delete file

        POP     DS
        POP     BP
        RET     10
DeallocateSwapFile ENDP
;-----------------------------------------------------------------------------
;function EmsInstalled : Boolean;

;Return true if Ems driver is installed
;!might crash if current drive is a remote network device!

EmsDevice       DB      'EMMXXXX0',0

EmsInstalled    PROC FAR

        MOV     AX,3567h                ;Get INT 67 vector
        INT     21h                     ;call DOS
        MOV     AX,ES                   ;is vector nil?
        OR      AX,BX
        JZ      EFail                   ;if so, fail          !!.03
        CMP     BYTE PTR ES:[BX],0CFh   ;pointing to IRET?    !!.03
        JNE     EInstChk                ;no, so continue check!!.03
EFail:                                  ;                     !!.03
        XOR     AL,AL                   ;else return False
        RET

EInstChk:
        PUSH    DS
        PUSH    CS
        POP     DS
        MOV     DX,OFFSET EmsDevice
        MOV     AX,3D02h                ;Open for read/write
        INT     21h
        POP     DS
        MOV     BX,AX                   ;Save handle in case one returned
        MOV     AL,0                    ;Assume false
        JC      EInstDone               ;Failed if carry flag set

        MOV     AX,4400h                ;make sure it is a character device
        INT     21h
        JC      EInstFail               ;Failed if carry flag set
        AND     DX,80h                  ;Bit 7 = 1 if character device
        JZ      EInstFail

        MOV     AX,4407h                ;make sure device is ready for output
        INT     21h
        JC      EInstFail               ;Failed if carry flag set
        OR      AL,AL                   ;if AL = 0 EMM is not available
        JZ      EInstFail

        PUSH    BX                      ;                  !!.03
        MOV     AH,30h                  ;get dos version   !!.03
        INT     21h                     ;                  !!.03
        POP     BX                      ;                  !!.03
        XCHG    AH,AL                   ;                  !!.03
        CMP     AX,030Ah                ;DOS 3.1?          !!.03
        JB      ENotDos31               ;<3.1, so all done !!.03
        MOV     AX,440Ah                ;IOCTRL subfunc A - Is Handle Local!!.03
        INT     21h                     ;                  !!.03
        JC      ENotDos31               ;                  !!.03
        AND     DX,8000h                ;local file if bit 15 of DX clear !!.03
        JNZ     EInstFail               ;network device so fail !!.03
ENotDos31:                              ;                  !!.03
        MOV     AL,1                    ;Return true
        JMP     SHORT EInstClose
EInstFail:
        XOR     AL,AL                   ;Return false
EInstClose:
        PUSH    AX                      ;Save function result
        MOV     AH,3Eh                  ;Close file to recover handle
        INT     21h
        POP     AX                      ;Restore function result
EInstDone:
        RET

EmsInstalled    ENDP

;-----------------------------------------------------------------------------
;function EmsPagesAvail : Word;
;Returns the number of available pages from the expanded memory manager,
; or EmsErrorCode in case of error.
EmsPagesAvail   PROC FAR
        EmsCall 42h                     ;Get number of pages function
        EmsWordResult   BX              ;If successful, return value in BX
        RET
EmsPagesAvail   ENDP
;function EmsPageFrame : Word;
EmsPageFrame    PROC FAR
        EmsCall 41h                     ;Get page frame
        OR      AH,AH                   ;Check for error           !!.12
        MOV     AX,BX                   ;AX = segment
        JZ      EPDone                  ;Done if Error = 0
        XOR     AX,AX                   ;Else segment = 0
EPDone: RET
EmsPageFrame    ENDP
;-----------------------------------------------------------------------------
;function AllocateEmsPages(NumPages : Word) : Word;
AllocateEmsPages PROC FAR
        MOV     BX,SP                   ;Set up stack frame
        MOV     BX,SS:[BX+4]            ;BX = NumPages
        EmsCall 43h                     ;Allocate EMS
        MOV     AX,DX                   ;Assume success
        JZ      APDone                  ;Done if not 0
        MOV     AX,0FFFFh               ;$FFFF for failure
APDone: RET     2                       ;Remove parameter and return
AllocateEmsPages ENDP
;-----------------------------------------------------------------------------
;procedure DeallocateEmsHandle(Handle : Word);
DeallocateEmsHandle PROC NEAR
        MOV     BX,SP                   ;Set up stack frame
        MOV     DX,SS:[BX+2]            ;DX = Handle
        EmsCall 45h                     ;Deallocate EMS
        RET     2                       ;Remove parameter and return
DeallocateEmsHandle ENDP
;-----------------------------------------------------------------------------
;function DefaultDrive : Char;
DefaultDrive    PROC FAR
        DosCallAH 19h                   ;Get default drive
        ADD     AL,'A'                  ;Convert to character
        RET
DefaultDrive    ENDP
;-----------------------------------------------------------------------------
;function DiskFree(Drive : Byte) : LongInt;
DiskFree        PROC FAR
        MOV     BX,SP                   ;Set up stack frame
        MOV     DL,SS:[BX+4]            ;DL = Drive to check
        DosCallAH 36h                   ;Get disk space
        MOV     DX,AX                   ;Return 0FFFFFFFFh for failure
        CMP     AX,0FFFFh               ;Bad drive number?
        JZ      DFDone                  ;Jump if so
        MUL     CX                      ;AX = bytes/cluster
        MUL     BX                      ;DX:AX = bytes free
DFDone: RET     2                       ;Remove parameter and return
DiskFree        ENDP

;Fields in an IntRegisters variable

        BpR     =       0
        EsR     =       2
        DsR     =       4
        DiR     =       6
        SiR     =       8
        DxR     =       10
        CxR     =       12
        BxR     =       14
        AxR     =       16
        Flags   =       22


;****************************************************** EmulateInt

;procedure EmulateInt(var Regs : IntRegisters; IntAddr : Pointer);

;Emulates an interrupt by filling the CPU registers with the values in Regs,
;clearing interrupts, pushing the flags, and calling far to IntAddr.

;Equates for parameters

IntAddr EQU     DWORD PTR [BP+6]
Regs    EQU     DWORD PTR [BP+10]
Regs2   EQU     DWORD PTR [BP+16]       ;Regs after the 'interrupt' - pushed
                                        ;BP, DS, and flags figure into
                                        ;equation the second time around

;Temporary variable stored in the code segment

        IntAddress      Pointer <>

EmulateInt      PROC FAR

        PUSH    BP                              ;Save BP
        MOV     BP,SP                           ;Set up stack frame
        PUSHF                                   ;Save flags
        PUSH    DS                              ;Save DS

        ;Load registers with contents of Regs

        LDS     DI,Regs                         ;DS:DI points to Regs
        MOV     AH,[DI].Flags                   ;Get new flags into AH
        SAHF                                    ;Load flags from AH
        MOV     AX,[DI].AxR                     ;Load AX
        MOV     BX,[DI].BxR                     ;Load BX
        MOV     CX,[DI].CxR                     ;Load CX
        MOV     DX,[DI].DxR                     ;Load DX

        ;Set up for the far call with interrupts off -- this section of code
        ;is not re-entrant

        LES     SI,IntAddr                      ;ES:SI points to IntAddr
        CLI                                     ;Interrupts off
        MOV     CS:IntAddress.Ofst,SI           ;Save offset of IntAddr
       MOV     CS:IntAddress.Segm,ES           ;Save segment
        MOV     BP,[DI].BpR                     ;Load BP
        MOV     SI,[DI].SiR                     ;Load SI
        MOV     ES,[DI].EsR                     ;Load ES
        PUSH    [DI].DsR                        ;PUSH new DS
        MOV     DI,[DI].DiR                     ;Load DI
        POP     DS                              ;POP new DS

        ;Emulate interrupt

        PUSHF                                   ;PUSH flags
        CALL    DWORD PTR CS:IntAddress         ;Call IntAddr

        ;Get ready to load Regs

        PUSH    BP                              ;Save BP
        MOV     BP,SP                           ;Set up stack frame
        PUSHF                                   ;Save Flags
        PUSH    ES                              ;Save ES
        PUSH    DI                              ;Save DI

        ;Load Regs with new values

        LES     DI,Regs2                        ;ES:DI points to Regs
        ADD     DI,AxR                          ;ES:DI points to Regs.AX
        STD                                     ;Go backward
        STOSW                                   ;Store AX
        MOV     AX,BX                           ;Get BX into AX and store it
        STOSW
        MOV     AX,CX                           ;Get CX into AX and store it
        STOSW
        MOV     AX,DX                           ;Get DX into AX and store it
        STOSW
        MOV     AX,SI                           ;Get SI into AX and store it
        STOSW
        POP     AX                              ;POP saved DI into AX
        STOSW                                   ;Store DX
        MOV     AX,DS                           ;Get DS into AX and store it
        STOSW
        POP     AX                              ;POP saved ES into AX
        STOSW                                   ;Store DS
        POP     ES:[DI].Flags                   ;POP saved Flags into place
        POP     AX                              ;POP saved BP into AX
                                                ;DS:DI now points to Regs.BP
        MOV     ES:[DI],AX                      ;Store BP
        CLD                                     ;Clear direction flag

        ;Clean up and return

        POP     DS                              ;Restore DS
        POPF                                    ;Restore flags
        POP     BP                              ;Restore BP
        RET     8

EmulateInt      ENDP


;****************************************************** EmergencyExit

;Called by exit/error handler in case of runtime error while popped up
; EmergencyExit(LastSS,LastSP,LastEntryCS,LastIP : Word)

EmergencyExit   PROC FAR
        POP     AX                     ; remove return address
        POP     AX
        POP     BX                     ; get LastEntryIP
        POP     CX
        POP     DX                     ; get LastEntrySP
        POP     AX                     ; get LastEntrySS
        CLI
        MOV     SS,AX
        MOV     SP,DX
        STI
        PUSH    CX
        PUSH    BX
        RET
EmergencyExit   ENDP
;-----------------------------------------------------------------------------
CODE    ENDS
        END
