;******************************************************
;                   OPINT.ASM 1.30
;                 Interrupt routines
;     Copyright (c) TurboPower Software 1987, 1992.
; Portions Copyright (c) Sunny Hill Software 1985, 1986
;     and used under license to TurboPower Software
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Equates

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

;****************************************************** Data   ;!!.20

DATA    SEGMENT BYTE PUBLIC                                    ;!!.20

        EXTRN   CStoDSInc : WORD                               ;!!.20

DATA    ENDS                                                   ;!!.20

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

        PUBLIC  EmulateInt

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
        MOV     BX,[DI].BxR                     ;Load BX
        MOV     CX,[DI].CxR                     ;Load CX
        MOV     DX,[DI].DxR                     ;Load DX

        ;Set up for the far call with interrupts off -- this section of code
        ;is not re-entrant

        LES     SI,IntAddr                      ;ES:SI points to IntAddr
        CLI                                     ;Interrupts off
        PUSH    DS                                                     ;!!.20
        PUSHF                                                          ;!!.21
        MOV     AX,SEG DATA                                            ;!!.20
        MOV     DS,AX                                                  ;!!.20
        MOV     AX,CS                                                  ;!!.20
        ADD     AX,CStoDSInc                                           ;!!.20
        MOV     DS,AX                                                  ;!!.20
        MOV     DS:IntAddress.Ofst,SI           ;Save offset of IntAddr !!.20
        MOV     DS:IntAddress.Segm,ES           ;Save segment           !!.20
        POPF                                                           ;!!.21
        POP     DS                                                     ;!!.20
        MOV     AX,[DI].AxR                     ;Load AX               ;!!.20
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
        STOSW
        MOV     AX,DS                           ;Get DS into AX and store it
        STOSW
        POP     AX                              ;POP saved ES into AX
        STOSW
                                                ;ES:DI now points to Regs.BP
        POP     AX                              ;POP saved Flags into AX
        POP     ES:[DI]                         ;POP saved BP into place
        MOV     ES:[DI].Flags,AL                ;Store low byte of flags
        CLD                                     ;Clear direction flag

        ;Clean up and return

        POP     DS                              ;Restore DS
        POPF                                    ;Restore flags
        POP     BP                              ;Restore BP
        RET     8

EmulateInt      ENDP

CODE    ENDS

        END
