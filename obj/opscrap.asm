;******************************************************
;                  OPSCRAP.ASM 1.30
;     Copyright (c) TurboPower Software 1989, 1992.
; Portions Copyright (c) Sunny Hill Software 1985, 1986
;     and used under license to TurboPower Software
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Equates

ScrapSize       =       127                     ;Size of scrap macro

;****************************************************** Data

DATA    SEGMENT WORD PUBLIC

        ;Pascal variables

        EXTRN   ScrapMacroPtr : DWORD
        EXTRN   ScrapMacroSize : WORD
        EXTRN   CSToDSInc : WORD                ;!!.20

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  InitScrapMacroPtr

ScrapMacro      DW  ScrapSize+2  DUP (?)        ;Set aside space

;****************************************************** InitScrapMacroPtr

;procedure InitScrapMacroPtr;

;Initialize ScrapMacroPtr
;!!.20 rewritten for pmode

InitScrapMacroPtr       PROC FAR

        MOV     ScrapMacroSize,ScrapSize
        MOV     AX,CS
        ADD     AX,CSToDSInc              ;AX is writeable alias for CS
        MOV     ScrapMacroPtr.Segm,AX
        MOV     AX,Offset ScrapMacro
        MOV     ScrapMacroPtr.Ofst,AX
        RET

InitScrapMacroPtr       ENDP

CODE    ENDS

        END
