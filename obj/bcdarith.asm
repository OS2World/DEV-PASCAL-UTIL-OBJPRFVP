;******************************************************
;                 BCDARITH.ASM 1.30
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

;****************************************************** AcctForExponents

;Adjusts mantissas to account for exponents before adding two reals.

;On entry, DS:DI points to mantissa of TempReal2, DS:SI points to
; mantissa of TempReal1, BH has exponent of TempReal1, and BL has
; exponent of TempReal2
;On exit, BL has sign/exponent of result, and mantissas are ready to be added.

AcctForExponents        PROC NEAR

        MOV     AX,BX                           ;exponents into AX
        SUB     AL,AH                           ;AL has difference in exponents
        JNZ     AFEdifferent                    ;adjust if AL <> 0
        RET

AFEdifferent:
        CLD                                     ;Go forward
        PUSH    SI                              ;Save SI and DI
        PUSH    DI
        JG      AFEchkDigits                    ;skip ahead if TempReal2's
                                                ;exponent was greater
        XCHG    BL,BH                           ;else, switch exponents
        XCHG    SI,DI                           ;reverse effects of next XCHG
                                                ;so TempReal1 gets set to 0 (or whatever)
        NEG     AL                              ;reverse the sign of AL

AFEchkDigits:
        XCHG    SI,DI                           ;point DI to the other real
        CMP     AL,MantissaDigits               ;Is difference <= # of
                                                ;significant digits?
        JNA      AFEdiffOK                      ;if not, we need to fill one of
                                                ;the two reals with 0
        SetZero AX                              ;AX = 0
        MOV     CX,SigDigits                    ;CX has loop count
        REP     STOSB                           ;fill the mantissa with 0's
        POP     DI                              ;Restore DI and SI
        POP     SI
        RET

AFEdiffOK:
        SetZero AH                              ;AH = 0
        MOV     CX,SigDigits                    ;CX = # of significant digits
        SUB     CX,AX                           ;Subtract difference in digits
                                                ;between the two exponents
        MOV     SI,DI                           ;SI points to real to change
        ADD     SI,AX                           ;SI => first byte to move
        REP     MOVSB                           ;Move what's left of the mantissa
        XCHG    CX,AX                           ;AX = 0, CX has loop count
        REP     STOSB                           ;Fill the rest with 0's
        POP     DI                              ;Restore DI and SI
        POP     SI
        RET

AcctForExponents        ENDP

;****************************************************** AddBCD

;procedure AddBCD(B1, B2 : BCD; var B3 : BCD);
;Add B1 to B2 and put result in B3.
;Divide by 0 interrupt generated if result overflows.

AddBCD  PROC FAR

        StackFrameBP
        CALL    LoadTempReals                   ;Move B1 and B2 into temporary reals
        CALL    AddPrimitive                    ;Add B1 and B2
        JNC     ARdone                          ;OK if carry flag not set
        INT     OverflowInt                     ;Use divide by zero to signal
                                                ;overflow
ARdone:
        CALL    SetUsersReal                    ;Move TempReal2 to user's result
        Exit_Code 12                            ;!!.13

AddBCD  ENDP

;****************************************************** SubBCD

;procedure SubBCD(B1, B2 : BCD; var B3 : BCD);
;Subtract B2 from B1 and put result in B3
;Divide by 0 interrupt generated if result overflows.

SubBCD  PROC FAR

        StackFrameBP
        CALL    LoadTempReals                   ;Move B1 and B2 into temporary reals
        MOV     AL,TempReal2                    ;AL has sign/exp of real #2
        OR      AL,AL                           ;AL = 0?
        JZ      SBnoFlip                        ;If so, don't flip sign bit
        XOR     AL,SignBit                      ;Flip the sign
        MOV     TempReal2,AL                    ;And store AL
SBnoFlip:
        CALL    AddPrimitive                    ;Add B1 and -B2
        JNC     SRdone                          ;OK if carry flag not set
        INT     OverflowInt                     ;Use divide by zero to signal
                                                ;overflow
SRdone:
        CALL    SetUsersReal                    ;Move TempReal2 to user's result
        Exit_Code 12                            ;!!.13

SubBCD  ENDP

;****************************************************** MultBcdMant
;Multiply two unpacked Bcd mantissas
;entry-
;  Multiplicand unpacked in TempReal1
;  Multiplier unpacked in TempReal2
;  DI points to 18 byte multiplier
;  ES=DS=SEG DATA
;exit-
;  Result in TempReal2
;  DI points to exponent of TempReal2
;  DX = 0 or DX = -1 for exponent correction

MultBcdMant  PROC NEAR

        CLD                                     ;Forward
        XOR     AX,AX                           ;To clear result
        MOV     BX,DI                           ;Multiplier address in BX
        MOV     DI,offset TempReal3             ;Result address in DI
        MOV     DX,DI                           ;Save it in DX too
        MOV     CX,UnpLength                    ;20 words to clear
        REP STOSW                               ;Zero the result buffer

        MOV     CX,MantissaDigits               ;18 digits to multiply
        MOV     DI,DX                           ;DI points to final result

NextMult:
        INC     BX                              ;Move to next multiplier digit
        INC     DI                              ;Move to next digit in final result

        MOV     AL,[BX]                         ;Get multiplier digit
        OR      AL,AL                           ;Is it zero?
        JZ      MultNextLoop                    ;Yes, skip all this

;Multiply [SI] by AL, result to [DI]
        PUSH    CX                              ;Save counter
        MOV     DL,AL                           ;Multiplier in DL
        MOV     SI,offset TempReal1+1           ;Point SI to TempReal1 mantissa
        MOV     CX,MantissaDigits               ;18 digits to multiply
        XOR     DH,DH                           ;Assure overflow zero for first digit

NextMD:
        LODSB                                   ;Next digit of multiplicand
        MUL     DL                              ; times multiplier, overflow in AH
        AAM                                     ;ASCII adjust
        ADD     AL,[DI]                         ;Add in previous result
        AAA                                     ;ASCII adjust
        ADD     AL,DH                           ;Add in previous overflow
        AAA
        STOSB                                   ;Store in intermediate result
        MOV     DH,AH                           ;Save overflow for next digit
        LOOP    NextMD                          ;Back for next digit

        MOV     [DI],DH                         ;Store last overflow in top digit
        SUB     DI,MantissaDigits               ;Restore to current location in intermediate
        POP     CX                              ;Restore digit counter

MultNextLoop:
        LOOP    NextMult                        ;Back for next digit

MultNormalize:
        XOR     DX,DX                           ;DX will return exponent correction
        MOV     SI,offset TempReal3+2*MantissaDigits ;Point to last byte of result
        CMP     BYTE PTR [SI],0                 ;Is last digit zero?
        JNZ     MultDoRounding
        DEC     DX                              ;Set DX to -1 to correct exponent
        DEC     SI                              ;Move to last non-zero digit

MultDoRounding:
        MOV     DI,SI                           ;Use DI for rounding pointer
        MOV     CX,MantissaDigits               ;18 digits to check
        SUB     DI,CX                           ;Point DI to start of 19 digit sequence
        CALL    RoundUnpackedReal               ;Round the least significant digit
        JNC     StoreMantResult                 ;If no carry, last digit was unchanged
        INC     DX                              ;Increase exponent
        INC     SI                              ;Increase last digit position

StoreMantResult:
        MOV     DI,offset TempReal2+MantissaDigits ;Point to last byte of result
        STD                                     ;Go backward
        MOV     CX,MantissaDigits               ;Take 18 digits to allow rounding
        REP MOVSB                               ;Copy to TempReal2

        RET

MultBcdMant  ENDP

;****************************************************** MultGetExponent
;Compute exponent of result
;entry
;  AX has combined sign/exponent word
;  DL has exponent correction
;  DI points to result exponent

MultGetExponent PROC NEAR

        MOV     BX,AX                           ;Copy to AX
        AND     AX,ExponentsOnly                ;AX has exponent part
        AND     BX,SignsOnly                    ;BX has sign part
        ADD     AL,AH                           ;Add exponents
        ADD     AL,DL                           ;Add exponent correction from mantissa
        SUB     AL,3Fh                          ;Rebias exponent
        JBE     MultExpUnderflow                ;Check for underflow
        JS      MultExpOverflow                 ;Check for overflow
        XOR     AL,BH                           ;Add in sign bit
        XOR     AL,BL                           ; from both terms
        MOV     [DI],AL                         ;Store to result
        RET

MultExpUnderFlow:
        MOV     BYTE PTR [DI],0                 ;Return zero
        RET

MultExpOverFlow:
        INT     OverflowInt                     ;Activate runtime error
        RET                                     ;Return in case of error recovery

MultGetExponent ENDP

;****************************************************** MultBcd
;Multiply two BCD numbers
;
; procedure MultBcd(a,b:Bcd; var c:Bcd);

MultBcd  PROC FAR

        StackFrameBP
        CALL    LoadTempReals                   ;Load temporary reals
        CALL    MultPrimitive                   ;Do the work
        CALL    SetUsersReal                    ;Return result
        Exit_Code 12                            ;!!.13

MultBcd  ENDP

;****************************************************** DivBcdMant
;Divide two unpacked Bcd mantissas
;entry-
;  Divisor unpacked in TempReal1
;  Numerator unpacked in TempReal2
;  DI points to numerator
;  ES=DS=SEG DATA
;exit-
;  Result in TempReal2
;  DI points to exponent of TempReal2
;  DX = 0 or DX = 1 for exponent correction

DivBcdMant  PROC NEAR

;Move numerator into working numerator (TempReal3)
        CLD                                     ;Forward
        MOV     SI,DI                           ;Now SI points to numerator
        INC     SI                              ;Skip over exponent byte
        MOV     DI,offset TempReal3
        MOV     CX,BcdLength
        XOR     AX,AX                           ;Clear lower 20 bytes
        REP STOSW
        MOV     CX,MantissaLength               ;Move 19 bytes of unpacked numerator
        REP MOVSW
        MOVSB

;Count significant digits in divisor
        MOV     DI,offset TempReal1+1           ;LSD of divisor
        MOV     BX,DI                           ;Store it for a moment
        MOV     CX,MantissaDigits+1             ;Scan 19 digits at most
        REPE SCASB                              ;Scan while equals zero
        MOV     DH,CL                           ;Store in DH

;Outer loop initialization
        MOV     CX,MantissaDigits+1             ;19 digits in result
        MOV     DI,offset TempReal2+MantissaDigits    ;Current digit of result
        MOV     SI,offset TempReal3+2*MantissaDigits+1  ;Current digit of numerator
        STD                                     ;Work from MSD to LSD

NextDivResult:
        PUSH    CX                              ;Save result counter
        PUSH    DI                              ;Save result position
        MOV     BX,SI                           ;Save numerator position

;Determine number of times divisor fits into current numerator
        XOR     DL,DL                           ;Clear number of times

NextTimesInto:
        CMP     BYTE PTR [SI+1],0               ;Check for overflow digit in numerator
        JNZ     DivStillFits                    ;If there, divisor is guaranteed to fit

        XOR     CX,CX
        MOV     CL,DH                           ;Number of divisor digits in CX
        MOV     DI,offset TempReal1+MantissaDigits ;Point to last digit of divisor

NextDivFitCheck:
        CMPSB
        JA      DivStillFits                    ;If divisor digit < numerator digit, it fits
        JB      DivDoesntFit                    ;If numerator > divisor, it doesn't fit
        LOOP    NextDivFitCheck                 ;Try again if digits are equal

DivStillFits:                                   ;Divisor still fits into numerator
        INC     DL                              ;Increment number of times it fits

;Subtract divisor from numerator
        MOV     DI,BX                           ;Restore numerator position
        XOR     CX,CX
        MOV     CL,DH                           ;Number of divisor digits in CX
        MOV     SI,offset TempReal1+MantissaDigits ;Point to last digit of divisor
        DEC     CX                              ;Reduce CX for a moment
        SUB     SI,CX                           ;Point to first sig digit of divisor
        SUB     DI,CX                           ;Point to first active digit of numerator
        INC     CX                              ;Restore CX
        CLC                                     ;Clear carry flag first time
        CLD                                     ;Forward

DivNextSub:
        LODSB                                   ;Get digit from divisor
        MOV     AH,AL                           ;Store momentarily
        MOV     AL,[DI]                         ;Get digit from numerator
        SBB     AL,AH                           ;Subtract divisor from numerator
        AAS                                     ;Adjust for unpacked Bcd
        STOSB                                   ;Store back to numerator
        LOOP    DivNextSub

        JNC     DivSubDone                      ;If no borrow left over, we're done
        DEC     BYTE PTR [DI]                   ;Reduce last digit for borrow
DivSubDone:
        STD                                     ;Backward again

;Try to fit it in again
        MOV     SI,BX                           ;Restore numerator position
        JMP     NextTimesInto                   ;Try again

DivDoesntFit:
        MOV     SI,BX                           ;Restore numerator position
        POP     DI                              ;Restore result position
        POP     CX                              ;Restore result counter

        MOV     [DI],DL                         ;Store times divisor went into numerator
        DEC     DI                              ;Next result digit
        DEC     SI                              ;Next numerator digit

        LOOP    NextDivResult                   ;Loop through result digits

;Scale and round result
        CLD                                     ;Forward again
        MOV     DX,1                            ;Assume exponent correction
        MOV     SI,offset TempReal2             ;Point to base of result
        CMP     BYTE PTR [SI+MantissaDigits],0  ;Is last digit zero?
        JNZ     DivRound                        ;If not, must round

        CALL    ShiftDigitsRight                ;Shift to fill MSD
        RET

DivRound:
        DEC     DX                              ;Won't need to correct exponent
        MOV     DI,SI                           ;Use DI for rounding pointer
        CALL    RoundUnpackedReal               ;Round the least significant digit
        RET

DivBcdMant  ENDP

;****************************************************** DivGetExponent
;Compute exponent of divided result
;entry
;  AX has combined sign/exponent word
;  DL has exponent correction
;  DI points to result exponent
;exit
;  result exponent stored in [DI]

DivGetExponent PROC NEAR
        MOV     BX,AX                           ;Copy to AX
        AND     AX,ExponentsOnly                ;AX has exponent part
        AND     BX,SignsOnly                    ;BX has sign part
        ADD     AH,40h                          ;Rebias exponent
        SUB     AH,AL                           ;Subtract divisor exp from numerator
        JBE     DivExpUnderflow                 ;Check for underflow  ***
        JS      DivExpOverflow                  ;Check for overflow   ***
        SUB     AH,DL                           ;Subtract exponent correction
        JBE     DivExpUnderflow                 ;Check for underflow
        JS      DivExpOverflow                  ;Check for overflow
        XOR     AH,BH                           ;Add in sign bit
        XOR     AH,BL                           ; from both terms
        MOV     [DI],AH                         ;Store to result
        RET

DivExpUnderFlow:
        MOV     BYTE PTR [DI],0                 ;Return zero
        RET

DivExpOverFlow:
        INT     OverflowInt                     ;Activate runtime error
        RET                                     ;Return in case of error recovery

DivGetExponent ENDP

;****************************************************** DivBcd
;Divide two BCD numbers
;
; procedure DivBcd(Numerator, Divisor : Bcd; var Result : Bcd);
;

DivBcd  PROC FAR

        StackFrameBP

;Load temporary reals
        MOV     BX,DS                           ;Save DS
        MOV     ES,BX                           ;Set ES=DS
        CLD                                     ;Forward
        LDS     SI,[BP+10]                      ;DS:SI => divisor
        MOV     DI,offset TempReal1
        MOV     CX,WordsInBcd
        REP MOVSW                               ;Divisor in TempReal1
        LDS     SI,[BP+14]                      ;DS:SI => numerator
        MOV     DI,offset TempReal2
        MOV     CX,WordsInBcd
        REP MOVSW                               ;Numerator in TempReal2
        MOV     DS,BX                           ;Set DS back to original again

        CALL    DivPrimitive                    ;Divide and return result in TempReal2
        CALL    SetUsersReal                    ;Return result

        Exit_Code 12                            ;!!.13

DivBcd  ENDP

