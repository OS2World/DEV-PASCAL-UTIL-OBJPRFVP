;******************************************************
;                  OP8087e.ASM 1.30
;               80x87 support routines
;     Copyright (c) TurboPower Software 1987, 1992.
; Portions copyright (c) Sunny Hill Software 1985, 1986
;     and used under license to TurboPower Software
;                All rights reserved.
;******************************************************

;NOTE:  This file must be assembled with Turbo Assembler!!

        INCLUDE OPCOMMON.ASM

EMUL                                    ;activate emulation

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

        PUBLIC  Save8087, Restore8087, Exceptions8087, Error8087

;****************************************************** Save8087

;procedure Save8087(var SaveBuf : SaveBuffer8087);
;Saves the 8087 registers in the save buffer.

SaveBuf EQU     DWORD PTR SS:[BX+4]

Save8087        PROC FAR

        StackFrame
        LES     DI,SaveBuf              ;Point to save buffer
        FSAVE   ES:[DI]                 ;Save contents of 8087 registers
        WAIT
        RET     4                       ;Remove parameter and return

Save8087        ENDP

;****************************************************** Restore8087

;procedure Restore8087(var SaveBuf : SaveBuffer8087);
;Restores the 8087 registers from the save buffer.

Restore8087     PROC FAR

        StackFrame
        LES     DI,SaveBuf              ;Point to save buffer
        FRSTOR  ES:[DI]                 ;Restore contents of 8087 registers
        WAIT
        RET     4                       ;Remove parameter and return

Restore8087     ENDP

;****************************************************** Exceptions8087

;procedure Exceptions8087(On : boolean);
;Turn exception interrupts on or off

OnOff           EQU BYTE PTR [BP+6]
CtrlWord        EQU WORD PTR [BP-2]

Exceptions8087  PROC FAR

        StackFrameBP
        DEC     SP                        ;make room for control word
        DEC     SP                        ;**
        MOV     AL,OnOff
        OR      AL,AL
        JZ      ExceptionsOff

        MOV     CtrlWord,0372H            ;Unmask IM,ZM,OM
        JMP     SHORT ExceptionsDone

ExceptionsOff:
        FSTCW   CtrlWord                  ;Get current control word
        OR      CtrlWord,00FFh            ;Mask all exceptions

ExceptionsDone:
        FLDCW   CtrlWord                  ;Change 8087 control word
        Exit_Code 2                       ;!!.13

Exceptions8087  ENDP

;****************************************************** Error8087

;function Error8087 : Word;
;Return the Error status of the 8087

StatWord        EQU WORD PTR [BP-2]

Error8087       PROC FAR

        StackFrameBP
        DEC     SP                        ;make room for status word
        DEC     SP                        ;**
        FSTSW   StatWord                  ;Get current status word
        MOV     AX,StatWord               ;Return in AX
        AND     AX,03Fh                   ;Just the exception indicators
        FCLEX                             ;Clear exception indicators
        Exit_Code 0                       ;!!.13

Error8087       ENDP

CODE    ENDS

        END
