;******************************************************
;                 OPERRHAN.ASM 1.30
;              Runtime error recovery
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

;!!.20 numerous changes for DOS pmode operation

        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT BYTE PUBLIC

        ;Pascal variables

        EXTRN   SystemContinue : DWORD  ;Continuation of system error handler
        EXTRN   UserHandler : DWORD     ;Address of user error handler
        EXTRN   ErrorCode : WORD        ;Stores error code !!.13
        EXTRN   ErrorAddr : DWORD       ;Stores error location
        EXTRN   PrefixSeg : WORD        ;Stores PSP segment of program
        EXTRN   ExitProc : DWORD        ;Address of exit handler
        EXTRN   OvrLoadList : WORD      ;Start of list of overlay segments !!.10
        EXTRN   CSToDSInc : WORD        ;To get DS alias for CS
        EXTRN   PMode : BYTE            ;Flag indicating real or prot mode
        EXTRN   FloatError : BYTE       ;1 if floating point error occurred

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  Handler, ErrHanInt0, Trap8087errors

        SaveInt02       Pointer <>      ;Old INT 2 handler
        SaveInt75       Pointer <>      ;Old INT 75 handler

lo      EQU     (WORD PTR 0)
hi      EQU     (WORD PTR 2)

;****************************************************** NewInt02
NewInt02:
        PUSH    AX
        PUSH    DS
        MOV     AX,SEG DATA
        MOV     DS,AX
        MOV     FloatError,1
        POP     DS
        POP     AX
        JmpFar  CS:SaveInt02

;****************************************************** NewInt75
NewInt75:
        PUSH    AX
        PUSH    DS
        MOV     AX,SEG DATA
        MOV     DS,AX
        MOV     FloatError,1
        POP     DS
        POP     AX
        JmpFar  CS:SaveInt75

;****************************************************** Trap8087errors

;Install INT 2 and INT 75 handlers

Trap8087errors  PROC NEAR

        MOV     AX,CS
        ADD     AX,CSToDSInc            ;Get alias data selector for CS
        PUSH    DS                      ;Note, DS=CS for real mode
        ASSUME  DS:CODE                 ;!!.21
        MOV     DS,AX
        GetVector 02h, DS:SaveInt02     ;NMI interrupt
        GetVector 75h, DS:SaveInt75     ;8087 exception
        PUSH    CS                      ;DS = CS
        POP     DS
        MOV     DX,OFFSET NewInt02      ;DX = Ofs(NewInt02)
        DosCallAX 2502h                 ;Set new NMI vector
        MOV     DX,OFFSET NewInt75      ;DX = Ofs(NewInt75)
        DosCallAX 2575h                 ;Set INT $75 vector
        POP     DS                      ;Restore DS
        ASSUME  DS:DATA                 ;!!.21
        RET

Trap8087errors  ENDP

;****************************************************** ErrHanInt0

;interrupt 0 entry point

ErrHanInt0      PROC FAR

        STI                             ;Enable interrupts
        MOV     AX,00C8h                ;Error code for divide by zero
        POP     CX                      ;Return address into BX:CX
        POP     BX
        POP     DX                      ;Take flags off stack
                                        ;Control continues in Handler
ErrHanInt0      ENDP

;****************************************************** Handler

;procedure Handler
;recover from Turbo runtime errors
;on entry BX:CX has address to return to, more or less
;AX has error number

Handler PROC FAR

        MOV     DX,SEG DATA
        MOV     DS,DX                   ;Restore DS

        MOV     DX,BX                   ;Check for Ctrl-Break or normal exit
        OR      DX,CX                   ;Is return address nil?
        JNZ     CheckErrorClass         ;No, check error class
        JMP     NoRecover               ;Yes, should not recover

CheckErrorClass:                        ;Check for classes of run time errors
        CMP     FloatError,1            ;Is it an 8087 error?
        JZ      CantRecover             ;Yes, can't recover
        CMP     AX,200                  ;Divide by zero error?
        JZ      CheckInstrClass         ;Yes, check for special cases
        CMP     AX,202                  ;Stack overflow error?
        JZ      CantRecover             ;Yes, can't recover
        CMP     AX,203                  ;Heap overflow error?
        JZ      CantRecover             ;Yes, can't recover
        CMP     AX,215                  ;Arithmetic error?              !!.21
        JZ      Recover                 ;Yes, recover immediately       !!.21
        CMP     AX,208                  ;Overlay error (208, 209)?
        JAE     CantRecover             ;Yes, can't recover
        CMP     AX,204                  ;Floating point error?
        JA      CheckInstrClass         ;Yes, check for special cases
        JMP     SHORT Recover           ;Else recover immediately

CheckInstrClass:                        ;Check for classes of erring instructions
        MOV     ES,BX                   ;Error segment
        MOV     DI,CX                   ;Error offset
        MOV     DX,ES:[DI]              ;Get word at error
        MOV     SI,2                    ;Minimum two bytes for integer DIV
        CMP     DL,0F7h                 ;Was it a DIV or IDIV?
        JZ      InstrLen                ;Yes, must fix up return address
        CMP     DL,09Bh                 ;Was it a WAIT?
        JZ      CantRecover             ;Yes, can't recover, 8087 wiped out
        CMP     DL,0D8h                 ;Was it a floating point instruction?
        JB      Recover
        CMP     DL,0DFh
        JA      Recover
CantRecover:
        JMP     SHORT NoRecover         ;Can't recover, 8087 stack wiped out

InstrLen:
        MOV     DL,DH                   ;Copy second byte into DL
        PUSH    CX
        MOV     CL,6
        SHR     DL,CL                   ;Get Mod field
        POP     CX
        CMP     DL,3                    ;Is it register addressing?
        JZ      Fixup                   ;Yes, no displacement bytes
        OR      DL,DL                   ;Is it direct addressing?
        JZ      Direct                  ;Yes, look further
        XOR     DH,DH
        ADD     SI,DX                   ;Add displacement bytes
        JMP     SHORT Fixup
Direct:
        AND     DH,7                    ;Get r/m field
        CMP     DH,6                    ;Direct addressing?
        JNZ     Fixup                   ;No, two byte instruction
        ADD     SI,2                    ;Else two address bytes
Fixup:
        ADD     CX,SI                   ;Add length of instruction

Recover:
        MOV     ErrorCode,AX            ;Save error code !!.13
        PUSH    BX                      ;Return address on stack
        PUSH    CX
        CMP     PMode,0                 ;Running in protected mode?
        JNE     PModeAddr               ;Jump if so

;!!.10  Support overlays
        MOV     AX,OvrLoadList          ;Get start of overlay segment list
ChkOverlay:
        OR      AX,AX                   ;Any overlays left to check?
        JE      OvrSegEnd               ;Jump if not
        MOV     ES,AX                   ;Static overlay segment into ES
        CMP     BX,ES:[0010h]           ;Error segment matches overlay segment?
        JE      StoreOvrSeg             ;Jump if so
        MOV     AX,ES:[0014h]           ;Next link in segment list
        JMP     ChkOverlay
StoreOvrSeg:
        MOV     BX,ES                   ;Update error segment to match stub seg
OvrSegEnd:
        SUB     BX,PrefixSeg
        SUB     BX,0010h
        JMP     StoreLogicalAddr

PModeAddr:
        MOV     ES,BX
        MOV     BX,ES:[0]               ;Get logical segment number

StoreLogicalAddr:
        MOV     ErrorAddr.hi,BX
        MOV     ErrorAddr.lo,CX

        LES     DI,UserHandler          ;Get address of user handler
        MOV     AX,ES
        OR      AX,DI
        JZ      Done                    ;Skip call if pointer nil
        CALL    DWORD PTR UserHandler   ;Else call user pointer
        OR      AL,AL                   ;Did UserHandler return true?
        JNZ     Done                    ;Yes, return to program

        POP     CX                      ;Get error address back
        POP     BX
        MOV     AX,ErrorCode            ;Get exitcode back !!.13
        JMP     SHORT NoRecover         ;Back to system handler
Done:
        RET                             ;Return beyond error

NoRecover:
        JMP     DWORD PTR [SystemContinue]      ;Transfer back to system handler

Handler ENDP

CODE    ENDS
        END
