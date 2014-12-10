;******************************************************
;                  OPBCD.ASM 1.30
;             Binary Coded Decimal routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

COMMENT @
   Format of Turbo 3.x style BCD reals:
   ------------------------------------
   BCD reals are packed into 10 bytes, as follows:

     LSB                      MSB (most significant byte at end)
      |<------ Mantissa ------>|
    1  2  3  4  5  6  7  8  9 10  <- Byte #
   sE ML ML ML ML ML ML ML ML ML
    ^                         ^^--- Least significant digit
    |                         |---- Most significant digit
    |
    v
    7 6 5 4 3 2 1 0 <-- Bit # (in Byte 1)
    s E E E E E E E
    ^ <--exponent->
    |       |
    |       |--- exponent has offset of $3F (eg, $41 means 10^2 = 100)
    |----------- sign bit (0 = positive, 1 = negative)

   Unpacked BCD format
   -------------------
   Many of the routines that follow work with these reals in an unpacked
   format. That is, before an arithmetic operation is performed, the mantissas
   are expanded (unpacked) so that there is one digit per byte. After unpacking,
   the reals look like this:

     LSB                                                MSB
      |<------------------ mantissa --------------------->|
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
   sE 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 00
                                                         ^^
                                                         ||---- Digit
                                                         |----- 0
   Byte 1 is unchanged.
   Bytes 2-19 contain the digits in the mantissa, LSB first. The high
     nibble of each byte is 0, and the low nibble contains the digit.
   Byte 20, sometimes used to keep track of overflow, is set to 0.

   When an operation is performed on two unpacked BCD's, the result
   ends up in the second one. This real is repacked before being moved
   into a user's variable.
@

;****************************************************** Macros/equates

        INCLUDE OPCOMMON.ASM    ;basic macros/equates
        INCLUDE OPBCDLOW.ASM    ;shared macros, equates, etc. for BCD routines

;****************************************************** Data

DATA    SEGMENT WORD PUBLIC

;Temporary reals in unpacked formats
EXTRN   TempReal1 : BYTE
EXTRN   TempReal2 : BYTE

;Double length for multiply/divide intermediates
EXTRN   TempReal3 : BYTE

;Half length for transcendental intermediates
EXTRN   TempReal4 : BYTE
EXTRN   TempReal5 : BYTE
EXTRN   TempReal6 : BYTE

;used by ValBCD
SignMask        DB      ?
ExpSigned       DB      ?
Asciiz          DB      (AsciizLength +1) DUP (?)

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        ;in BCDARITH.ASM
        PUBLIC  AddBCD, SubBCD, MultBCD, DivBCD

        ;in BCDCOMP.ASM
        PUBLIC  EqualBCD, NotEqualBCD, GreaterBCD, GreaterEqualBCD
        PUBLIC  LessBCD, LessEqualBCD

        ;in BCDCONV.ASM
        PUBLIC  LongintToBCD, RoundBCD, TruncBCD, AbsBCD, IntBCD, FracBCD
        PUBLIC  RealToBCD, BCDtoReal, StrBCD, StrExpBCD, ValBCD

        ;routines needed by BCDTRANS
        PUBLIC  LoadTempReal2, CopyTempToTemp, SetUsersReal, ActualIntBCD
        PUBLIC  AddPrimitive, DivPrimitive, MultPrimitive, CopyUsersTo2

        ;Routines to call system library STR and VAL procedures
        EXTRN   SystemStr : FAR
        EXTRN   SystemVal : FAR

;Table with powers of ten:

Power1  DD      1
Power2  DD      10
Power3  DD      100
Power4  DD      1000
Power5  DD      10000
Power6  DD      100000
Power7  DD      1000000
Power8  DD      10000000
Power9  DD      100000000
Power10 DD      1000000000

;***************************************************** UnpackTempReal

;On entry, DS:SI points to end of packed real, ES:DI points to end
;  of unpacked real buffer, direction flag is reset, CX has MantissaLength
;On exit, the 9-byte mantissa has been expanded to 18 bytes (2-19), and
;  the 20th byte has been initialized to 0.
;AX, CX, SI, DI destroyed.

UnpackTempReal PROC NEAR

        MOV     [DI+1],CH                       ;Initialize last byte to 0
        DEC     DI

UnpackByte1:
        LODSB                                   ;Next byte into AL
        MOV     AH,AL                           ;Store in AH
        AND     AL,LowNibble                    ;Isolate LSD in AL
        SHR     AH,1                            ;Isolate MSD in AH
        SHR     AH,1
        SHR     AH,1
        SHR     AH,1
        STOSW                                   ;Store unpacked digits
        LOOP    UnpackByte1                     ;Back for next byte
        RET

UnpackTempReal ENDP

;***************************************************** UnpackTempReals

;On entry, DS:SI and DS:DI point to two 10-byte BCD reals, with their
;  mantissas packed into bytes 2-10. (ES = DS)
;On exit, the 9-byte mantissa of each has been expanded to 18 bytes (2-19), and
;  the 20th byte has been initialized to 0.
;AX, BX, CX, and DX destroyed. SI and DI preserved.

UnpackTempReals PROC NEAR

        MOV     BX,SI                           ;Save SI in BX
        MOV     DX,DI                           ;Save DI in DX
        STD                                     ;Go backward

        ;unpack real at DS:SI
        MOV     CX,MantissaLength               ;CX has # of bytes in mantissa
        ADD     SI,CX                           ;Point to end of first mantissa
        MOV     DI,SI
        ADD     DI,CX                           ;Point DI to end of unpacked mantissa
        CALL    UnpackTempReal

        ;unpack real that was at DS:DI
        MOV     CX,MantissaLength               ;CX has # of bytes in mantissa
        MOV     SI,DX                           ;Get second real stored in DX
        ADD     SI,CX                           ;Point to end of packed
        MOV     DI,SI
        ADD     DI,CX                           ;Point to end of unpacked
        CALL    UnpackTempReal

        MOV     SI,BX                           ;Restore SI
        MOV     DI,DX                           ;Restore DI
        CLD                                     ;Clear direction flag
        RET                                     ;Return

UnpackTempReals ENDP

;***************************************************** PackTempReal

;On entry, DS:SI points to an unpacked BCD real. (ES = DS)
;On exit, its mantissa has been packed back into 9 bytes.
;AX, BX, CX, and DX destroyed. SI preserved.

PackTempReal    PROC NEAR

        MOV     BX,SI                           ;Save SI in BX
        INC     SI                              ;Point to mantissa
        MOV     DI,SI                           ;Point to output
        CLD                                     ;Go forward
        MOV     CX,MantissaLength               ;CX has # of bytes in mantissa

PackByte1:
        LODSW                                   ;Load LSD into AL, MSD into AH
        SHL     AH,1                            ;Shift MSD to top nibble
        SHL     AH,1
        SHL     AH,1
        SHL     AH,1
        OR      AL,AH                           ;Merge LSD into AL
        STOSB                                   ;And store
        LOOP    PackByte1                       ;Back for next byte

        MOV     SI,BX                           ;Restore SI
        RET                                     ;Return

PackTempReal    ENDP

;****************************************************** CopyUsersTo2
;Copy Real at [BP+6] to TempReal2
;Entry: ES=DS=DSEG

CopyUsersTo2 PROC NEAR
        MOV     BX,DS                           ;Save DS
        LDS     SI,DWORD PTR [BP+6]
        CLD
        MOV     DI,Offset TempReal2
        MOV     CX,WordsInBCD
        REP MOVSW
        MOV     DS,BX                           ;Restore DS
        RET
CopyUsersTo2 ENDP

;****************************************************** CopyTempToTemp
;Copy one temp real to another
;Entry: ES=DS=DSEG, SI=Source, DI=Destination
;Exit: SI,DI,CX destroyed

CopyTempToTemp PROC NEAR

        CLD
        MOV     CX,WordsInBCD
        REP MOVSW
        RET

CopyTempToTemp ENDP

;****************************************************** LoadTempReal2

;On entry, it is assumed that SS:[BP+10] points to real to load into
;  TempReal2
;On exit, TempReal2 is loaded

LoadTempReal2   PROC NEAR

        MOV     BX,DS                           ;Save DS
        CLD                                     ;Go forward
        MOV     ES,BX                           ;ES = DS
        MOV     DI,Offset TempReal2             ;ES:DI => TempReal2
        MOV     CX,WordsInBCD                   ;CX = words to move
        LDS     SI,DWORD PTR [BP+10]            ;Point to B2
        REP     MOVSW                           ;Move the real
        MOV     DS,BX                           ;Restore DS
        RET                                     ;Return

LoadTempReal2   ENDP

;****************************************************** LoadTempReal1

;On entry, it is assumed that SS:[BP+14] points to real to load into
;  TempReal1
;On exit, TempReal1 is loaded

LoadTempReal1   PROC NEAR

        MOV     BX,DS                           ;Save DS
        CLD                                     ;Go forward
        MOV     ES,BX                           ;ES = DS
        MOV     DI,Offset TempReal1             ;ES:DI => TempReal1
        MOV     CX,WordsInBCD                   ;CX = words to move
        LDS     SI,DWORD PTR [BP+14]            ;Point to B2
        REP     MOVSW                           ;Move the real
        MOV     DS,BX                           ;Restore DS
        RET                                     ;Return

LoadTempReal1   ENDP

;****************************************************** LoadTempReals

;On entry, it is assumed that SS:[BP+14] points to one real, and SS:[BP+10] to
;  another.
;On exit, these reals have been loaded into the first ten bytes of TempReal1
;  and TempReal2, respectively

LoadTempReals   PROC NEAR

        CALL    LoadTempReal1
        CALL    LoadTempReal2
        RET

LoadTempReals   ENDP

;****************************************************** SetUsersReal

;On entry, it is assumed that TempReal2 has the result of an arithmetic
;  operation, and that SS:[BP+6] points to a BCD real. This routine loads
;  that real with TempReal2

SetUsersReal    PROC NEAR

        CLD                                     ;Go forward
        PUSH    ES
        MOV     SI,Offset TempReal2             ;DS:SI => TempReal2
        LES     DI,DWORD PTR [BP+6]             ;ES:DI => B3
        MOV     CX,WordsInBCD                   ;CX = words to move
        REP     MOVSW                           ;Move the real
        POP     ES
        RET                                     ;Return

SetUsersReal    ENDP

;***************************************************** RoundUnpackedReal

;On entry, DS:DI points to 19 digit unpacked real, whose first byte is to be
;  rounded. ES=DS=DSEG and CX has maximum number of digits to round.
;On exit, same unpacked real is rounded
;  AX, CX, DI destroyed.

RoundUnpackedReal PROC NEAR

        CMP     BYTE PTR [DI],5                 ;Compare to 5
        CMC                                     ;Prepare carry flag for rounding
        JNC     RoundDone                       ;Done if less

        INC     DI                              ;Skip lowest digit
        MOV     CX,MantissaDigits               ;18 digits to check
        CLD

RoundNext:
        MOV     AL,[DI]                         ;Get next byte
        ADC     AL,0                            ;Add just the carry from previous
        AAA                                     ;Adjust for unpacked BCD
        STOSB                                   ;Store result
        JNC     RoundDone                       ;Done if no carry
        LOOP    RoundNext
        MOV     BYTE PTR [DI],1                 ;Set last digit if we got here

RoundDone:
        RET                                     ;Return

RoundUnpackedReal  ENDP

;****************************************************** ShiftDigitsLeft

;On entry, SI points to the mantissa that is to have its digits shifted
;  backward, and the direction flag is clear.
;CX, AX, DI destroyed; SI preserved.

ShiftDigitsLeft PROC NEAR

        MOV     AX,SI                           ;save SI in AX
        INC     SI                              ;SI points to first digit
        MOV     DI,SI                           ;DI = SI
        INC     SI                              ;SI points to second digit
        MOV     CX,MantissaLength               ;CX has words to move
        REP     MOVSW                           ;Move digits back
        MOV     SI,AX                           ;restore SI
        RET                                     ;return

ShiftDigitsLeft ENDP

;****************************************************** ShiftDigitsRight

;On entry, SI points to the mantissa that is to have its digits shifted
;  forward
;CX, AX, DI destroyed; SI preserved; Direction flag returned forward

ShiftDigitsRight PROC NEAR

        MOV     AX,SI                           ;save SI in AX
        ADD     SI,MantissaDigits-2             ;first word to move
        MOV     DI,SI                           ;DI = SI
        INC     DI                              ;DI points to last word
        MOV     CX,MantissaLength               ;CX has words to move
        STD                                     ;backward
        REP     MOVSW                           ;Move digits
        MOV     SI,AX                           ;restore SI
        CLD                                     ;forward
        RET                                     ;return

ShiftDigitsRight ENDP

;****************************************************** AddPrimitive

;On entry, TempReal1 and TempReal2 contain two packed BCD reals to be added.
;  Direction flag is clear, ES = DS.
;On exit, TempReal2 contains the result in packed BCD format.

AddPrimitive    PROC NEAR

        MOV     SI,Offset TempReal1             ;DS:SI => TempReal1
        MOV     AH,[SI]                         ;AH has sign/exp of TempReal1
        OR      AH,AH                           ;Is it 0?
        JZ      APzeroDone                      ;If not, check TempReal2 for 0

APchkZero:
        MOV     DI,Offset TempReal2             ;ES:DI and DS:DI => TempReal2
        MOV     AL,[DI]                         ;AL has sign/exp of TempReal2
        OR      AL,AL                           ;Is it 0?
        JNZ     APHaveToAdd                     ;If not, have to add
        MOV     CX,WordsInBCD                   ;CX = words to move
        REP     MOVSW                           ;TempReal2 = TempReal1
APzeroDone:
        RET                                     ;Return

APHaveToAdd:
        PUSH    AX                              ;Save AX
        CALL    UnpackTempReals                 ;unpack the mantissas
        POP     AX                              ;Restore AX
        SetZero BL                              ;BL = 0
        MOV     [DI],BL                         ;Set first byte of each to 0
        MOV     [SI],BL
        MOV     BX,AX                           ;Save signs/exponents in BX
        AND     BX,ExponentsOnly                ;Clear sign bits
        MOV     DH,AL
        AND     DH,SignBit                      ;DH has sign of TempReal2
        XOR     AL,AH                           ;merge signs/exponents (set SF)
        PUSHF                                   ;save flags
        CALL    AcctForExponents                ;adjust to account for exponents
        MOV     DL,BL                           ;DL has sign/exp of result
        FakePOPF                                ;restore flags
        CLC                                     ;clear carry flag
        MOV     BX,DI                           ;BX points to result's mantissa
        MOV     CX,SigDigits                    ;CX has loop count
        JS      APsubMantissas                  ;was SF was set when signs/exps
                                                ;were merged? if so, subtract

APaddMantissas:
        LODSB                                   ;AL has next byte in TempReal1
        ADC     AL,[DI]                         ;Add to the byte in TempReal2
        AAA                                     ;Adjust for addition
        STOSB                                   ;Store the result
        LOOP    APaddMantissas                  ;Repeat
        JNC     AddNoOV                         ;Ready to normalize if carry
                                                ;not set

        INC     BYTE PTR [DI]                   ;increment overflow byte (was 0, now 1)
        INC     DL                              ;increment the exponent
AddNoOV:
        MOV     DI,BX
        JMP     SHORT NormalizeMantissa         ;Now normalize the mantissa

APsubMantissas:
        LODSB                                   ;AL has next byte in TempReal1
        MOV     AH,AL                           ;store in AH
        MOV     AL,[DI]                         ;AL has next byte in TempReal2
        SBB     AL,AH                           ;subtract the two
        AAS                                     ;account for subtraction
        STOSB                                   ;Store the result
        LOOP    APsubMantissas                  ;repeat
        MOV     DI,BX                           ;restore DI from BX
        JC      APnegate                        ;Negate result if CF set
        JMP     SHORT NormalizeMantissa         ;Normalize the mantissa

APnegate:
        CLC                                     ;clear carry flag
        MOV     CX,SigDigits                    ;CX = # of significant digits
APnegMantissa:
        MOV     AL,CH                           ;AL = 0
        SBB     AL,[DI]                         ;AL = -[DI]
        AAS                                     ;adjust for subtraction
        STOSB                                   ;store the result
        LOOP    APnegMantissa                   ;repeat
        MOV     DI,BX                           ;restore DI
        XOR     DH,SignBit                      ;flip sign of result and
                                                ;normalize mantissa
NormalizeMantissa:
        SetZero BX                              ;exponent delta = 0
        MOV     SI,DI                           ;SI points to result's mantissa
        MOV     AL,[SI+SigDigits]               ;Get value of overflow byte
        OR      AL,AL                           ;Is it zero?
        JZ      APCheckMSB                      ;If so, check the the MSB
        INC     DI                              ;start with first byte in mantissa
        CALL    RoundUnpackedReal               ;round the result
        CALL    ShiftDigitsLeft                 ;and shift digits to the left
        JMP     SHORT APNormalized              ;normalized

APCheckMSB:
        MOV     AL,[SI+MantissaDigits]          ;AL has MSB of mantissa
        OR      AL,AL                           ;is MSB 0?
        JZ      APshift                         ;if so, need to shift
        CALL    RoundUnpackedReal               ;round the result
        MOV     AL,[SI+SigDigits]    ;!!.03     ;Get overflow byte again
        OR      AL,AL                ;!!.03     ;Is it still zero?
        JZ      APNormalized         ;!!.03     ;If so, it's normalized
        CALL    ShiftDigitsLeft      ;!!.03     ;Otherwise, must shift to left
        INC     DL                   ;!!.03     ;Increment exponent for shift
        JMP     SHORT APNormalized              ;now normalized

APshift:
        STD                                     ;go backward
        MOV     DI,SI                           ;DI = SI
        ADD     DI,MantissaDigits               ;point DI to MSB
        SetZero AX                              ;AX = 0
        MOV     CX,SigDigits                    ;scan all 19 significant digits
        REPE    SCASB                           ;scan for non-0 bytes
        JZ      APzero                          ;all zeros, so set exponent to 0

        INC     CX                              ;add 1 to get # of digits before t
                                                ;the first 0 (from end)
        INC     DI                              ;point DI to first non-0 digit
        PUSH    SI                              ;Save SI
        XCHG    SI,DI                           ;DI points to mantissa, SI
                                                ;to first non-0 digit
        ADD     DI,MantissaDigits               ;DI points to MSB
        MOV     BX,DI                           ;BX points to MSB
        SUB     BX,SI                           ;BX = # of non-zero
        REP     MOVSB                           ;move the significant digits
        MOV     CX,BX                           ;# of bytes to fill with 0
        REP     STOSB                           ;fill with zeros
        POP     SI                              ;restore SI
        CLD                                     ;Clear direction flag

APNormalized:
;!!.03  CLC                                     ;clear carry flag
;!!.03  ADC     DL,0                            ;sets SF if exponent > $7F
        SUB     DL,BL                           ;subtract delta in BL
        JBE     APzero                          ;result is zero if DL <= BL
        JNS     APok                            ;result OK if SF not set
        STC                                     ;else indicate overflow
        CALL    PackTempReal                    ;Re-pack the real anyway
        RET                                     ;and return

APzero: SetZero DX                              ;DX = 0
APok:   OR      DL,DH                           ;set sign bit (stored in DH)
        MOV     [SI],DL                         ;set sign/exponent (in DL)
        CLD                                     ;Clear direction flag
        CALL    PackTempReal                    ;Re-pack the real
        RET                                     ;return

AddPrimitive    ENDP

;****************************************************** MultPrimitive
;Actually multiply two BCD numbers
;Entry: TempReal1 and TempReal2 loaded with packed reals to multiply
;Exit: packed result in TempReal2

MultPrimitive PROC NEAR
        MOV     SI,offset TempReal1             ;Point SI to TempReal1
        MOV     DI,offset TempReal2             ;Point DI to TempReal2

;See if either input is zero
        XOR     AX,AX
        ADD     AH,[DI]                         ;Exponent of TempReal2 in AH
        MOV     [DI],AL                         ;Clear exponent of TempReal2
        JZ      MultPrimDone                    ;If zero, we're done
        ADD     AL,[SI]                         ;TempReal1 exponent in AL
        JZ      MultPrimDone                    ;If zero, we're done
        PUSH    AX                              ;Store Exponent/Sign word
        CALL    UnpackTempReals                 ;Unpack the two inputs
        CALL    MultBcdMant                     ;Multiply the mantissas
        POP     AX                              ;Restore Exponent/Sign word
        CALL    MultGetExponent                 ;Compute exponent and sign
        MOV     SI,offset TempReal2
        CALL    PackTempReal                    ;Pack the result
MultPrimDone:                                   ;DS:SI points to packed result
        RET
MultPrimitive ENDP

;********************************************************** DivPrimitive
;Divide TempReal2 by TempReal1 and return result in TempReal2
;Entry: DS=ES=DSEG

DivPrimitive PROC NEAR
        MOV     SI,offset TempReal1             ;Point SI to divisor
        MOV     DI,offset TempReal2             ;Point DI to numerator
        CMP     BYTE PTR [SI],0
        JNZ     DivisorOK
        INT     DivZeroInt                      ;Divide by zero error
        RET                                     ;Return in case of error recovery
DivisorOK:
        CMP     BYTE PTR [DI],0                 ;Check for zero numerator
        JZ      DivPrimDone                     ;We're done if so
        MOV     AH,[DI]                         ;Exponent of Numerator in AH
        MOV     AL,[SI]                         ;Divisor exponent in AL
        PUSH    AX                              ;Save word for later
        CALL    UnpackTempReals                 ;Unpack the two inputs
        CALL    DivBcdMant                      ;Divide the mantissas, result in TempReal2
        POP     AX                              ;Restore exponent/sign word
        MOV     DI,offset TempReal2             ;Point DI to base of answer
        CALL    DivGetExponent                  ;Compute the exponent and sign
        MOV     SI,DI                           ;Point SI to base of answer
        CALL    PackTempReal                    ;Pack the result
DivPrimDone:
        RET

DivPrimitive ENDP

;****************************************************** Includes

        INCLUDE BCDARITH.ASM    ;Basic arithmetic routines
        INCLUDE BCDCOMP.ASM     ;Real number comparison
        INCLUDE BCDCONV.ASM     ;Conversion routines

CODE    ENDS

        END
