;******************************************************
;                 OPBCDLOW.ASM 1.30
;        Macros and equates used by BCD routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

;****************************************************** Macros

CopyReal        MACRO   R2,R1                   ;Copies var R1 to var R2
                MOV     SI,offset R1
                MOV     DI,offset R2
                CALL    CopyTemptoTemp
                ENDM

CopyConst       MACRO   R1,C1                   ;Copies const C1 to var R1
                MOV     BX,DS                   ;Save DS
                MOV     SI,CS
                MOV     DS,SI                   ;Set DS=CS
                MOV     SI,offset C1
                MOV     DI,offset R1
                CALL    CopyTemptoTemp          ;Copy
                MOV     DS,BX                   ;Restore DS
                ENDM

PushReal        MACRO   R1                      ;Push address of var R1 on stack
                MOV     DI,offset R1
                PUSH    DS
                PUSH    DI
                ENDM

PushConst       MACRO   C1                      ;Push address of const C1 on stack
                MOV     DI,offset C1
                PUSH    CS
                PUSH    DI
                ENDM

NegReal         MACRO   R1                      ;Invert sign of var R1
                LOCAL   IsZero
                MOV     AL,R1
                OR      AL,AL
                JZ      IsZero
                XOR     AL,SignBit              ;Flip sign bit
                MOV     R1,AL
IsZero:
                ENDM

AbsReal         MACRO   R1                      ;Return absolute value of var R1
                AND     BYTE PTR R1,NoSignBit
                ENDM

;****************************************************** Equates

;Note: BCDlength MUST be an EVEN number >= 10

BCDlength       =       10                      ;# of bytes in packed BCD
MantissaLength  EQU     BCDlength - 1           ;size of packed mantissa
MantissaDigits  EQU     MantissaLength shl 1    ;# of digits in unpacked mantissa
SigDigits       EQU     MantissaDigits + 1      ;# of significant digits for addition, etc.
UnpLength       EQU     BCDlength shl 1         ;# of bytes in unpacked BCD
WordsInBCD      EQU     BCDlength shr 1         ;# of words in packed BCD
MSDoffset       EQU     Offset TempReal2 + MantissaDigits   ;offset in DS for MSD of TempReal2
OverflowInt     =       0                       ;INT generated when overflow
                                                ; occurs
DivZeroInt      =       0                       ;INT generated for divide by 0
SignBit         =       10000000b               ;bit mask for 7th bit
NoSignBit       =       01111111b               ;bit mask for bits 0-6
ExponentsOnly   =       0111111101111111b
SignsOnly       =       1000000010000000b
HighNibble      =       11110000b               ;bit mask for high nibble
LowNibble       =       00001111b               ;bit mask for low nibble
AsciizLength    =       127
