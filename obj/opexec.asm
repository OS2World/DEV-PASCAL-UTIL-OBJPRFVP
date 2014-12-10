;******************************************************
;                   OPEXEC.ASM 1.30
;     Copyright (c) TurboPower Software 1988, 1992.
;                 All rights reserved.
;******************************************************

; Thanks to Chris Franzen of O.K.SOFT Software, West Germany
; who added the swap-to-disk capability to our EMSEXEC unit

; OPEXEC.ASM
; Routines to manage EMS and swap process to EMS or disk for shell

; Remove the semi-colon on the next line to enable XMS support
SupportXms      =       1                               ;!!.03

DATA    SEGMENT BYTE PUBLIC
        EXTRN   NoOfBytesSwapped:DWORD
        EXTRN   EmsAllocated:BYTE
        EXTRN   SwapFileAllocated:BYTE
        EXTRN   XmsAllocated:BYTE                        ;!!.03
        EXTRN   EmsHandle:WORD
        EXTRN   FrameSeg:WORD
        EXTRN   SwapFileHandle:WORD
        EXTRN   PrefixSeg:WORD
        EXTRN   DosError:WORD
        IFDEF SupportXms                                 ;!!.03
        EXTRN   XmsControl:DWORD                         ;!!.03
        ENDIF                                            ;!!.03
DATA    ENDS

CODE    SEGMENT WORD PUBLIC

        ASSUME  CS:CODE, DS:DATA

        PUBLIC ExecWithSwap,FirstToSave
        PUBLIC SetSwapMsgOn
        PUBLIC PtrDiff,EmsInstalled
        PUBLIC EmsPageFrame,AllocateEmsPages,DeallocateEmsHandle
        IFDEF SupportXms                                 ;!!.10
          PUBLIC XmsSupport                              ;!!.10
          XmsSupport:                                    ;!!.10
        ELSE                                             ;!!.03 !!.10
          PUBLIC NoXmsSupport                            ;!!.03
          NoXmsSupport:                                  ;!!.03
        ENDIF                                            ;!!.03
;****************************************************** ExecWithSwap
;procedure ExecWithSwap(Path, CmdLine : string);
;DOS shell with swapping to EMS or disk

;local values
EmsPageSize     EQU     16384           ;Size of EMS page frame, must match ExecSwap.EmsPageSize
SwapFileBlockSize EQU   32768           ;Size of largest block possible with DOS func 40h/41h
                                        ;  must be multiple of 16
StackSize       EQU     512             ;Bytes in temporary stack
lo              EQU     (WORD PTR 0)
hi              EQU     (WORD PTR 2)
ofst            EQU     (WORD PTR 0)
segm            EQU     (WORD PTR 2)

;local variables
NoOfBytesSwappedCS DD   ?
EmsHandleCS     DW      ?
FrameSegCS      DW      ?
SwapFileHandleCS DW     ?
PrefixSegCS     DW      ?
DosErrorCS      DW      ?
LeftToCopy      DD      ?
XYSave          DW      ?
UsedEms         DB      ?
UsedXms         DB      0                     ;!!.03
IFDEF SupportXms                              ;!!.03
XmsControlCS    DD      ?                     ;!!.03
ENDIF                                         ;!!.03
IRetOp          DB      0CFh
SaveDS          DW      ?
SaveSP          DW      ?
SaveSS          DW      ?
PathPtr         DD      ?
CmdPtr          DD      ?
ParasWeHave     DW      ?
Old1B           DD      ?
Old23           DD      ?
SaveDTA         DD      ?                     ;!!.10
TempStack       DB      StackSize DUP (?)

IFDEF SupportXms                       ;begin !!.03
ExtMemMoveBlock STRUC                  ;a structure for XMS move block
        emLen           DD  ?
        emSrcHandle     DW  ?
        emSrcOfs        DD  ?
        emDestHandle    DW  ?
        emDestOfs       DD  ?
ExtMemMoveBlock ENDS

XmsMoveBlock    ExtMemMoveBlock <>
ENDIF                                  ;end   !!.03

;local constants and values for prompts

TxtHandle       EQU     2               ;Prompts are written to StdErr
ToDiskTxt       DB      'Swapping out...'
ToDiskTxtLen    EQU     $-OFFSET ToDiskTxt
ToDiskClrTxt    DB      0Dh,'               ',0Dh
ToDiskClrLen    EQU     $-OFFSET ToDiskClrTxt
FromDiskTxt     DB      0Dh,0Ah,'Swapping in...'
FromDiskTxtLen  EQU     $-OFFSET FromDiskTxt
FromDiskClrTxt  DB      0Dh,'              ',0Dh
FromDiskClrLen  EQU     $-OFFSET FromDiskClrTxt
SwapMsgOn       DB      1               ;Swap message on by default

;Locals on temporary stack
FileSeg2        EQU     WORD PTR [BP-2]
FileOfs2        EQU     WORD PTR [BP-4]
FileSeg1        EQU     WORD PTR [BP-6]
FileOfs1        EQU     WORD PTR [BP-8]
CmdLineSeg      EQU     WORD PTR [BP-10]
CmdLineOfs      EQU     WORD PTR [BP-12]
EnvironSeg      EQU     WORD PTR [BP-14]
FileBlock1      EQU     BYTE PTR [BP-30]
FileBlock2      EQU     BYTE PTR [BP-46]
Path            EQU     BYTE PTR [BP-110]
CmdLine         EQU     BYTE PTR [BP-238] ;!!.03

ExecWithSwap    PROC    FAR

        PUSH    BP
        MOV     BP,SP                   ;Set up stack frame

;Move variables to CS where we can access them later
        LES     DI,[BP+6]
        MOV     CmdPtr.ofst,DI
        MOV     CmdPtr.segm,ES          ;CmdPtr -> command line string
        LES     DI,[BP+10]
        MOV     PathPtr.ofst,DI
        MOV     PathPtr.segm,ES         ;PathPtr -> path to execute
        MOV     SaveSP,SP
        MOV     SaveSS,SS               ;Stack position
        MOV     SaveDS,DS               ;DS
        MOV     AX,NoOfBytesSwapped.lo
        MOV     NoOfBytesSwappedCS.lo,AX
        MOV     AX,NoOfBytesSwapped.hi
        MOV     NoOfBytesSwappedCS.hi,AX ;Bytes to swapped
        MOV     AX,EmsHandle
        MOV     EmsHandleCS,AX          ;Handle of EMS memory block
        MOV     AX,FrameSeg
        MOV     FrameSegCS,AX           ;Segment of EMS page window
        MOV     AX,SwapFileHandle
        MOV     SwapFileHandleCS,AX     ;Handle of swap file
        MOV     AX,PrefixSeg
        MOV     PrefixSegCS,AX          ;Program prefix segment

        MOV     DosErrorCS,1            ;Assume failure

;Point break vectors somewhere harmless
        PUSH    DS
        PUSH    CS
        POP     DS
        ASSUME  DS:CODE

        MOV     AX,351Bh                ;Save old vectors
        INT     21h
        MOV     Old1B.ofst,BX
        MOV     Old1B.segm,ES
        MOV     AX,3523h
        INT     21h
        MOV     Old23.ofst,BX
        MOV     Old23.segm,ES

        MOV     AX,251Bh                ;Force breaks to IRET
        MOV     DX,OFFSET IretOp
        INT     21h
        MOV     AL,23h
        INT     21h

;Display "swapping out" message
        CMP     SwapMsgOn,1
        JNZ     OutDone
        MOV     AH,40h                  ;DOS write file handle func#
        MOV     BX,TxtHandle
        MOV     CX,ToDiskTxtLen
        MOV     DX,OFFSET ToDiskTxt
        INT     21h
OutDone:POP     DS
        ASSUME  DS:DATA

        MOV     AX,NoOfBytesSwapped.lo     ;LeftToCopy = NoOfBytesSwapped !!.03
        MOV     LeftToCopy.lo,AX           ;!!.03
        MOV     AX,NoOfBytesSwapped.hi     ;!!.03
        MOV     LeftToCopy.hi,AX           ;!!.03
        MOV     UsedEms,0                  ;set to zero !!.03
        MOV     UsedXms,0                  ;set to zero !!.03
;Check if swapping to EMS or swap file
        CMP     EmsAllocated,0          ;Check flag for EMS method
        JZ      NotEms
        JMP     UseEms
NotEms:
IFDEF SupportXms
        CMP     XmsAllocated,0          ;check flag for XMS method !!.03
        JZ      NotXms
        JMP     UseXms
NotXms:
ENDIF
        CMP     SwapFileAllocated,0     ;Check flag for swap file method
        JNZ     UseFile
        JMP     EE10                    ;Exit if no swapping method set from caller

;Write FirstToSave -> LastToSave to swap file
UseFile:
;!!.03        MOV     AX,NoOfBytesSwapped.lo
;!!.03        MOV     LeftToCopy.lo,AX
;!!.03        MOV     AX,NoOfBytesSwapped.hi
;!!.03        MOV     LeftToCopy.hi,AX

        PUSH    CS
        POP     DS                      ;DS = CS
        ASSUME  DS:NOTHING

        MOV     BX,SwapFileHandleCS     ;BX = handle of our swap file
                                        ;Prepare for writing by doing a seek
        MOV     AX,4200h                ;DOS file handle seek func#
        XOR     CX,CX
        XOR     DX,DX                   ;To file start
        INT     21h
        JNC     EF0                     ;Jump if we succeeded
        JMP     EE10

EF0:    MOV     CX,SwapFileBlockSize    ;Assume we'll write a full block
        CMP     LeftToCopy.hi,0         ;Is high word still non-zero?
        JNZ     EF2                     ;Jump if so
        CMP     LeftToCopy.lo,SwapFileBlockSize ;Low word still a page or more?
        JAE     EF2                     ;Jump if so
        MOV     CX,LeftToCopy.lo        ;Otherwise, write what's left
                                        ;CX=size of next block to write
EF2:    SUB     LeftToCopy.lo,CX        ;Reduce number left to write
        SBB     LeftToCopy.hi,0
        MOV     DX,OFFSET FirstToSave   ;DS:DX -> start of region to save
        MOV     AH,40h                  ;DOS write to file handle func#
        INT     21h
        JC      EF1                     ;Jump if error
        CMP     AX,CX
        JE      EF3                     ;Jump if full block written (no error)
EF1:    JMP     EE10

EF3:    MOV     AX,DS
        ADD     AX,SwapFileBlockSize/16
        MOV     DS,AX                   ;Next block region to save
        MOV     AX,LeftToCopy.lo
        OR      AX,LeftToCopy.hi        ;Bytes left to write?
        JNZ     EF0                     ;Jump if so

;!!.03        MOV     UsedEms,0               ;Flag we used swap file for swapping

;Flush swap file                                                     ;!!.10
;(Prevents lost clusters in case of reboot while in shell)           ;!!.10
        MOV     AH,45h                  ;DUP handle                  ;!!.10
        INT     21h                                                  ;!!.10
        JC      CannotDup               ;Skip it if error            ;!!.10
        MOV     BX,AX                   ;Get the new handle          ;!!.10
        MOV     AH,3Eh                  ;Close handle                ;!!.10
        INT     21h                                                  ;!!.10
CannotDup:                                                           ;!!.10

        JMP     SwapDone

;Copy FirstToSave -> LastToSave to EMS
UseEms:
        ASSUME  DS:DATA
;!!.03        MOV     AX,NoOfBytesSwapped.lo
;!!.03        MOV     LeftToCopy.lo,AX
;!!.03        MOV     AX,NoOfBytesSwapped.hi
;!!.03        MOV     LeftToCopy.hi,AX
        MOV     ES,FrameSeg             ;ES -> page window
        MOV     DX,EmsHandle            ;DX = handle of our EMS block
        MOV     AH,47h
        INT     67h                     ;Save EMS context
        XOR     BX,BX                   ;BX = logical page

        MOV     AX,CS
        MOV     DS,AX                   ;DS = CS
        ASSUME  DS:NOTHING

EE0:    MOV     AX,4400h                ;Map physical page 0
        INT     67h
        OR      AH,AH                   ;Success?
        JZ      EE1                     ;Jump if so
        JMP     EE10                    ;Exit if not

EE1:    MOV     CX,EmsPageSize          ;Assume we'll copy a full page
        CMP     LeftToCopy.hi,0         ;Is high word still non-zero?
        JNZ     EE2                     ;Jump if so
        CMP     LeftToCopy.lo,EmsPageSize ;Low word still a page or more?
        JAE     EE2                     ;Jump if so
        MOV     CX,LeftToCopy.lo        ;Otherwise, copy what's left

EE2:    SUB     LeftToCopy.lo,CX        ;Reduce number left to copy
        SBB     LeftToCopy.hi,0
        XOR     DI,DI                   ;Start at base of EMS page
        MOV     SI,OFFSET FirstToSave   ;DS:SI -> start of region to save
        CLD                             ;Forward
        SHR     CX,1                    ;Convert to words
        JNC     EE3                     ;Jump if even
        MOVSB                           ;Move odd byte
EE3:    REP     MOVSW                   ;Move the rest of the words

        INC     BX                      ;Next logical page
        MOV     AX,DS
        ADD     AX,EmsPageSize/16
        MOV     DS,AX                   ;Next page region to save
        MOV     AX,LeftToCopy.lo
        OR      AX,LeftToCopy.hi        ;Bytes left to move?
        JNZ     EE0                     ;Jump if so

;!!.13        MOV     AH,48h
;!!.13        INT     67h                     ;Restore EMS context
        MOV     UsedEms,1               ;Flag we used EMS for swapping
IFDEF SupportXms
        JMP     SwapDone                ;!!.03
UseXms:
        ASSUME  DS:DATA
        MOV     AX,XmsControl.Ofst      ;XmsControlCS = XmsControl
        MOV     XmsControlCS.Ofst,AX
        MOV     AX,XmsControl.Segm
        MOV     XmsControlCS.Segm,AX
        MOV     AX,CS
        MOV     ES,AX
        MOV     BX,OFFSET FirstToSave   ;BX = FirstToSave
        MOV     DI,OFFSET XmsMoveBlock  ;SI and DI point to XmsMoveBlock
        MOV     SI,DI
        CLD                             ;forward string ops
        MOV     AX,LeftToCopy.lo        ;get number of bytes to swap in DX:AX
        MOV     DX,LeftToCopy.hi
        TEST    AX,1                    ;is it odd
        JZ      EXMS0                   ;no, so nothing to do
        ADD     AX,1                    ;yes, so round up to even
        ADC     DX,0
EXMS0:                                  ;begin filling in XmsMoveBlock
        STOSW                           ;first word is length to move
        MOV     AX,DX                   ;msb of length to move
        STOSW
        XOR     AX,AX                   ;source is normal memory (handle 0)
        STOSW
        MOV     AX,BX                   ;offset of memory to swap
        STOSW
        MOV     AX,CS                   ;segment of memory to swap
        STOSW
        MOV     AX,EmsHandleCS          ;XMS handle stored in EmsHandleCS
        STOSW
        XOR     AX,AX                   ;offset in XMS is 0
        STOSW
        STOSW
        MOV     AH,0Bh                  ;prepare for XMS move memory function
        PUSH    DS                      ;save DS
        PUSH    CS
        POP     DS                      ;DS = CS
        CALL    DWORD PTR CS:[XmsControlCS] ;call XMS manager
        POP     DS                      ;restore DS
        MOV     UsedXms,1               ;set UsedXms to true
ENDIF
SwapDone:
;Clear "swapping to disk" prompt
        PUSH    CS
        POP     DS
        ASSUME  DS:CODE
        CMP     SwapMsgOn,1
        JNZ     OutClrDone
        MOV     AH,40h                  ;DOS write file handle func#
        MOV     BX,TxtHandle
        MOV     CX,ToDiskClrLen
        MOV     DX,OFFSET ToDiskClrTxt
        INT     21h
OutClrDone:

;Save DTA in case child changes it                    !!.10
        MOV     AH,2Fh                  ;             !!.10
        INT     21H                     ;Get DTA      !!.10
        MOV     SaveDTA.ofst,BX         ;Save it      !!.10
        MOV     SaveDTA.segm,ES         ;             !!.10

;Save amount of memory currently allocated
        MOV     AX,PrefixSegCS
        MOV     ES,AX                   ;ES = segment of the memory block
        DEC     AX
        MOV     DS,AX
        ASSUME  DS:NOTHING
        MOV     CX,DS:[0003h]           ;Get current paragraphs from DOS MCB
        MOV     ParasWeHave,CX

;Switch to temporary stack
        MOV     AX,StackSize+OFFSET TempStack
        MOV     BX,CS
        CLI
        MOV     SS,BX
        MOV     SP,AX
        STI
        MOV     BP,SP                   ;Set up new stack frame
        SUB     SP,238                  ;Space for locals !!.03

;Shrink memory block to just above FirstToSave
        MOV     AX,OFFSET FirstToSave+15
        MOV     CL,4
        SHR     AX,CL
        ADD     BX,AX
        SUB     BX,PrefixSegCS
        MOV     AH,4Ah                  ;SETBLOCK
        INT     21h
        JNC     EX0                     ;Jump if successful
        JMP     EX4                     ;Restore stack and get out

;Set up parameters and call DOS Exec
EX0:    MOV     AX,ES:[002Ch]           ;Get environment segment
        MOV     EnvironSeg,AX
        PUSH    SS
        POP     ES
        CLD
        LDS     SI,PathPtr              ;DS:SI -> Path to execute
        LEA     DI,Path                 ;ES:DI -> Local ASCIIZ copy
        LODSB
        CMP     AL,63                   ;Truncate path if needed
        JB      EX1
        MOV     AL,63
EX1:    MOV     CL,AL
        XOR     CH,CH
        REP     MOVSB
        XOR     AL,AL
        STOSB                           ;ASCIIZ terminate
        LDS     SI,CmdPtr               ;DS:SI -> Command line to pass
        LEA     DI,CmdLine              ;ES:DI -> Local terminated copy
        LODSB
        CMP     AL,126                  ;Truncate command if needed
        JB      EX2
        MOV     AL,126
EX2:    STOSB
        MOV     CL,AL
        XOR     CH,CH
        REP     MOVSB
        MOV     AL,0DH                  ;Terminate with ^M
        STOSB
        PUSH    SS
        POP     DS
        LEA     SI,CmdLine
        MOV     CmdLineOfs,SI
        MOV     CmdLineSeg,DS           ;Store pointer to command line
        INC     SI
        LEA     DI,FileBlock1
        MOV     FileOfs1,DI
        MOV     FileSeg1,ES             ;Store pointer to filename 1, if any
        MOV     AX,2901H                ;PARSE FCB 1
        INT     21H
        LEA     DI,FileBlock2
        MOV     FileOfs2,DI
        MOV     FileSeg2,ES             ;Store pointer to filename 2, if any
        MOV     AX,2901H                ;PARSE FCB 2
        INT     21H
        LEA     DX,Path
        LEA     BX,EnvironSeg
        MOV     AX,4B00H                ;EXEC
        INT     21H
        JC      EX3                     ;Jump if error in DOS call
        XOR     AX,AX
EX3:    MOV     DosErrorCS,AX           ;Save DOS error code

;Set up temporary stack and reallocate memory block
        MOV     AX,StackSize+OFFSET TempStack
        MOV     BX,CS
        CLI
        MOV     SS,BX
        MOV     SP,AX
        STI
        MOV     ES,PrefixSegCS
        MOV     BX,ParasWeHave
        MOV     AH,4Ah                  ;SETBLOCK
        INT     21h
        JNC     EX4                     ;Jump if no error
        MOV     AX,4C01h
        INT     21h                     ;!! Halt if failure here

EX4:
;Restore DTA saved before exec                        !!.10
        MOV     DX,SaveDTA.ofst         ;             !!.10
        MOV     DS,SaveDTA.segm         ;             !!.10
        MOV     AH,1Ah                  ;             !!.10
        INT     21H                     ;Set DTA      !!.10

;Point break vectors somewhere harmless
        PUSH    CS
        POP     DS
        ASSUME  DS:CODE
        MOV     AX,251Bh                ;Force breaks to IRET
        MOV     DX,OFFSET IretOp
        INT     21h
        MOV     AL,23h
        INT     21h

;Display "swapping from disk" prompt
        CMP     SwapMsgOn,1
        JNZ     InDone
        XOR     BH,BH
        MOV     AH,3
        INT     10H
        MOV     XYSave,DX               ;Save cursor position
        MOV     AH,40h                  ;DOS write file handle func#
        MOV     BX,TxtHandle
        MOV     CX,FromDiskTxtLen
        MOV     DX,OFFSET FromDiskTxt
        INT     21h
InDone:
        MOV     AX,NoOfBytesSwappedCS.lo         ;!!.03
        MOV     LeftToCopy.lo,AX                 ;!!.03
        MOV     AX,NoOfBytesSwappedCS.hi         ;!!.03
        MOV     LeftToCopy.hi,AX                 ;!!.03

;Check which swap method is in use
        CMP     UsedEms,0
        JZ      NotEms0                 ;!!.03
        JMP     CopyEms
NotEms0:                                ;begin !!.03
IFDEF SupportXms
        CMP     UsedXms,0               ;using XMS?
        JZ      ReadSwapFile            ;no, so disk file
        JMP     CopyXms                 ;yes, so jump to CopyXms
ENDIF                                   ;end !!.03

;We'll read back from swap file
ReadSwapFile:
        PUSH    CS
        POP     DS
        ASSUME  DS:NOTHING

;Read back from swap file
;!!.03        MOV     AX,NoOfBytesSwappedCS.lo
;!!.03        MOV     LeftToCopy.lo,AX
;!!.03        MOV     AX,NoOfBytesSwappedCS.hi
;!!.03        MOV     LeftToCopy.hi,AX
        MOV     BX,SwapFileHandleCS     ;BX = handle of our swap file
                                        ;Prepare for reading by doing a seek
        MOV     AX,4200h                ;DOS file handle seek func#
        XOR     CX,CX
        XOR     DX,DX                   ;to file start
        INT     21h
        JNC     EF6                     ;Jump if we succeeded
        MOV     AX,4C01h
        INT     21h                     ;!! Halt if failure here

EF6:    MOV     CX,SwapFileBlockSize    ;Assume we'll copy a full block
        CMP     LeftToCopy.hi,0         ;Is high word still non-zero?
        JNZ     EF8                     ;Jump if so
        CMP     LeftToCopy.lo,SwapFileBlockSize ;Low word still a block or more?
        JAE     EF8                     ;Jump if so
        MOV     CX,LeftToCopy.lo        ;Otherwise, copy what's left
                                        ;CX=size of next block to read
EF8:    SUB     LeftToCopy.lo,CX        ;Reduce number left to read
        SBB     LeftToCopy.hi,0
        MOV     DX,OFFSET FirstToSave   ;DS:DX -> start of region to restore
        MOV     AH,3Fh                  ;DOS read from file handle func#
        INT     21h
        JNC     EF9                     ;Jump if no error
        MOV     AX,4C01h
        INT     21h                     ;!! Halt if we fail here
EF9:    CMP     AX,CX
        JE      EF5                     ;Jump if full block read
        MOV     AX,4C01h
        INT     21h                     ;!! Halt if we fail here

EF5:    MOV     AX,DS
        ADD     AX,SwapFileBlockSize/16
        MOV     DS,AX                   ;Next block region to restore
        MOV     AX,LeftToCopy.lo
        OR      AX,LeftToCopy.hi        ;Bytes left to read?
        JNZ     EF6                     ;Jump if so

        JMP     EE10                    ;We're done

;Copy back from EMS
CopyEms:
;!!.03        MOV     AX,NoOfBytesSwappedCS.lo
;!!.03        MOV     LeftToCopy.lo,AX
;!!.03        MOV     AX,NoOfBytesSwappedCS.hi
;!!.03        MOV     LeftToCopy.hi,AX

        MOV     DS,FrameSegCS           ;DS -> page window
        MOV     DX,EmsHandleCS          ;DX = handle of our EMS block
;!!.13        MOV     AH,47h
;!!.13        INT     67h                     ;Save EMS context

        MOV     AX,CS
        MOV     ES,AX                   ;ES = CS
        XOR     BX,BX                   ;BX = logical page

EE6:    MOV     AX,4400h                ;Map physical page 0
        INT     67h
        OR      AH,AH                   ;Success?
        JZ      EE7                     ;Jump if so
        MOV     AX,4C01h
        INT     21h                     ;!! Halt if not

EE7:    MOV     CX,EmsPageSize          ;Assume we'll copy a full page
        CMP     LeftToCopy.hi,0         ;Is high word still non-zero?
        JNZ     EE8                     ;Jump if so
        CMP     LeftToCopy.lo,EmsPageSize ;Low word still a page or more?
        JAE     EE8                     ;Jump if so
        MOV     CX,LeftToCopy.lo        ;Otherwise, copy what's left

EE8:    SUB     LeftToCopy.lo,CX        ;Reduce number left to copy
        SBB     LeftToCopy.hi,0
        XOR     SI,SI                   ;Start at base of EMS page
        MOV     DI,OFFSET FirstToSave   ;ES:DI -> start of region to restore
        CLD                             ;Forward
        SHR     CX,1                    ;Convert to words
        JNC     EE9                     ;Jump if even
        MOVSB                           ;Move odd byte
EE9:    REP     MOVSW                   ;Move the rest of the words

        INC     BX                      ;Next logical page
        MOV     AX,ES
        ADD     AX,EmsPageSize/16
        MOV     ES,AX                   ;Next page region to restore
        MOV     AX,LeftToCopy.lo
        OR      AX,LeftToCopy.hi        ;Bytes left to move?
        JNZ     EE6                     ;Jump if so

        MOV     AH,48h
        INT     67h                     ;Restore EMS context
IFDEF SupportXms                        ;begin !!.03
        JMP     EE10                    ;We're done

CopyXms:
        ASSUME  DS:NOTHING
        MOV     AX,CS
        MOV     ES,AX                   ;ES = CS
        MOV     BX,OFFSET FirstToSave   ;BX = FirstToSave
        MOV     DI,OFFSET XmsMoveBlock  ;DI and SI point to XmsMoveBlock
        MOV     SI,DI
        CLD                             ;forward string ops
        MOV     AX,LeftToCopy.lo        ;number of bytes to swap in DX:AX
        MOV     DX,LeftToCopy.hi
        TEST    AX,1                    ;is it odd
        JZ      EXMS1                   ;no, so nothing to do
        ADD     AX,1                    ;yes, so round up to even
        ADC     DX,0
EXMS1:                                  ;begin filling in XmsMoveBlock
        STOSW                           ;lsb of number to swap
        MOV     AX,DX                   ;msb of number to swap
        STOSW
        MOV     AX,EmsHandleCS          ;XMS handle
        STOSW
        XOR     AX,AX                   ;XMS offset is 0
        STOSW
        STOSW
        XOR     AX,AX                   ;destination handle is 0 (normal mem)
        STOSW
        MOV     AX,BX                   ;offset of memory to swap to
        STOSW
        MOV     AX,CS                   ;segment of memory to swap to
        STOSW
        MOV     AH,0Bh                  ;prepare for XMS Move function
        PUSH    DS                      ;save DS
        PUSH    CS
        POP     DS                      ;DS = CS
        CALL    DWORD PTR CS:[XmsControlCS] ;Call XMS
        POP     DS                      ;restore DS

ENDIF                                   ;end !!.03
EE10:
;Clear "swapping from disk" prompt
        PUSH    CS
        POP     DS
        ASSUME  DS:CODE
        CMP     SwapMsgOn,1
        JNZ     InClrDone
        MOV     AH,40h                  ;DOS write file handle func#
        MOV     BX,TxtHandle
        MOV     CX,FromDiskClrLen
        MOV     DX,OFFSET FromDiskClrTxt
        INT     21h
        XOR     BH,BH
        MOV     AH,2
        MOV     DX,XYSave               ;Restore cursor position
        INT     10H
InClrDone:

        CLI                             ;Switch back to original stack
        MOV     SS,SaveSS
        MOV     SP,SaveSP
        STI

        ASSUME  DS:NOTHING
        MOV     AX,251Bh                ;Restore break vectors
        MOV     DX,Old1B.ofst
        MOV     DS,Old1B.segm
        INT     21h
        MOV     AX,2523h                ;Restore break vectors
        MOV     DX,Old23.ofst
        MOV     DS,Old23.segm
        INT     21h

        MOV     DS,SaveDS               ;Restore DS
        ASSUME  DS:DATA
        MOV     AX,DosErrorCS
        MOV     DosError,AX             ;Save DosError
        POP     BP
        RET     8
ExecWithSwap    ENDP

;****************************************************** FirstToSave
;Dummy routine to mark location to save
        EVEN
FirstToSave:

;****************************************************** SetSwapMsgOn
;procedure SetSwapMsgOn(On : Boolean);
;Turns Swap message on or off

SetSwapMsgOn    PROC FAR
        MOV     BX,SP
        MOV     AL,SS:[BX+4]
        MOV     SwapMsgOn,AL
        RET     2                  ;!!.!!
SetSwapMsgOn    ENDP

;****************************************************** EmsInstalled
;function EmsInstalled : Boolean;
;Return true if Ems driver is installed
;!might crash if current drive is a remote network device!
EmsDevice       DB      'EMMXXXX0',0

EmsInstalled    PROC FAR

        MOV     AX,3567h                ;Get INT 67 vector
        INT     21h                     ;call DOS
        MOV     AX,ES                   ;is vector nil?
        OR      AX,BX
        JNZ     EInstChk                ;if not, check for the driver
        CMP     BYTE PTR ES:[BX],0CFh   ;pointing to IRET?    !!.03
        JNE     EInstChk                ;no, so continue check!!.03
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

;****************************************************** PtrDiff
;function PtrDiff(H, L : Pointer) : LongInt; external;
;Return the number of bytes between H^ and L^. H is the higher address}
PtrDiff PROC NEAR
        MOV     BX,SP                   ;Set up stack frame

        MOV     AX,SS:[BX+4]            ;AX = seg(L^)
        XOR     DX,DX
        MOV     CX,4                    ;Shift counter
PD1:    SHL     AX,1                    ;Shift longint
        RCL     DX,1
        LOOP    PD1
        ADD     AX,SS:[BX+2]            ;Add offset
        ADC     DX,0                    ;Carry

        PUSH    DX                      ;Save intermediate result
        PUSH    AX

        MOV     AX,SS:[BX+8]            ;AX = seg(H^)
        XOR     DX,DX
        MOV     CX,4                    ;Shift counter
PD2:    SHL     AX,1                    ;Shift longint
        RCL     DX,1
        LOOP    PD2
        ADD     AX,SS:[BX+6]            ;Add offset
        ADC     DX,0                    ;Carry

        POP     BX
        POP     CX
        SUB     AX,BX
        SBB     DX,CX                   ;Return difference in DX:AX

        RET     8
PtrDiff ENDP

;****************************************************** EmsPageFramePtr
;function EmsPageFrame : Word;
;Returns segment of the page frame used by the EMM, 0 if error
EmsPageFrame PROC FAR
        MOV     AH,41h                  ;function code into AH
        INT     67h                     ;call the EMM
        OR      AH,AH                   ;Check for error
        MOV     AX,BX                   ;Segment from BX to AX
        JZ      FramePtrExit            ;Done if Error = 0
        XOR     AX,AX                   ;Else AX = 0
FramePtrExit:
        RET
EmsPageFrame ENDP

;****************************************************** AllocateEmsPages
;function AllocateEmsPages(NumPages : Word) : Word
;Allocates the indicated number of pages and returns a handle.
;Returns FFFF in case of error.
AllocateEmsPages        PROC FAR
        MOV     BX,SP                   ;Set up stack frame
        MOV     BX,SS:[BX+4]            ;BX = NumPages
        MOV     AH,43h                  ;function code into AH
        INT     67h                     ;call the EMM
        OR      AH,AH                   ;AH = 0 means success
        MOV     AX,DX                   ;assume success
        JZ      AEPExit                 ;Done if not 0
        MOV     AX,0FFFFh               ;$FFFF for failure
AEPExit:
        RET     2                       ;Remove parameter and return
AllocateEmsPages        ENDP

;****************************************************** DeallocateEmsHandle
;procedure DeallocateEmsHandle(Handle : Word);
;Deallocates the indicated handle and the memory associated with it.
DeallocateEmsHandle     PROC FAR
        MOV     BX,SP                   ;Set up stack frame
        MOV     DX,SS:[BX+4]            ;DX = Handle
        MOV     AH,45h                  ;function code into AH
        INT     67h                     ;call the EMM
        RET     2                       ;Remove parameter and return
DeallocateEmsHandle     ENDP

CODE    ENDS
        END

