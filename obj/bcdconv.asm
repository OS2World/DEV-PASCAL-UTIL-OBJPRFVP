;******************************************************
;                  BCDCONV.ASM 1.30
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

;****************************************************** LongintToBCD

;procedure LongintToBCD(L : LongInt; var B : BCD);
;Convert a Longint to a BCD

LTBreal EQU     DWORD PTR [BP+6]
LTBhigh EQU     WORD PTR [BP+12]
LTBlow  EQU     WORD PTR [BP+10]

LongintToBCD    PROC FAR

        StackFrameBP
        MOV     AX,DS                           ;AX = DS
        MOV     ES,AX                           ;ES = DS
        MOV     DI,Offset TempReal2             ;ES:DI => TempReal2
        SetZero AX                              ;AX = 0
        CLD                                     ;Go forward
        MOV     CX,BCDlength                    ;# of words to fill
        REP     STOSW                           ;Fill with 0's
        MOV     DX,LTBhigh                      ;high word of L into DX
        MOV     BX,LTBlow                       ;low word into BX
        MOV     AX,DX                           ;AX = DX
        OR      AX,BX                           ;DX and BX both 0?
        JZ      LTBdone                         ;we're done if they are

        MOV     AL,4Ah                          ;AL has initial value of s/e
        OR      DX,DX                           ;see if number is negative
        JNS     LTBpositive                     ;skip this if it isn't
        MOV     AL,0CAh                         ;AL has initial value of s/e
        NOT     BX                              ;Flip bits in low word
        NOT     DX                              ;Flip bits in high word
        ADD     BX,1                            ;Increment the low word
        ADC     DX,0                            ;Increment high word if CF set

LTBpositive:
        MOV     SI,Offset Power10               ;Point to table of powers of ten
        MOV     CX,9                            ;CX has digit counter

LTBgetExp:
        DEC     AL                              ;Decrement exponent
        CMP     DX,CS:[SI+2]                    ;Compare high words
        JB      LTBnext                         ;Skip to next power if lower
        JA      LTBsetExp                       ;Done if higher
        CMP     BX,CS:[SI]                      ;High words equal - comp low words
        JAE     LTBsetExp                       ;Done if BX is greater or equal

LTBnext:
        SUB     SI,4                            ;Point to previous table entry
        DEC     CX                              ;decrement CX
        JMP     SHORT LTBGetExp                 ;and repeat

LTBsetExp:
        MOV     DI,Offset TempReal2             ;DI => TempReal2
        MOV     [DI],AL                         ;Store exponent
        ADD     DI,MantissaDigits               ;Point DI to MSB
        STD                                     ;Go backward

LTBnextDigit:
        MOV     AL,-1                           ;Initialize digit

LTBincDigit:
        INC     AL                              ;Increment digit
        SUB     BX,CS:[SI]                      ;subtract this power of ten
        SBB     DX,CS:[SI+02]                   ; from L
        JAE     LTBincDigit                     ;keep going while L >= 0
        ADD     BX,CS:[SI]                      ;undo the last subtraction
        ADC     DX,CS:[SI+02]
        STOSB                                   ;store the digit
        SUB     SI,4                            ;adjust pointer to powers table
        DEC     CX                              ;decrement digit counter
        JGE     LTBnextDigit                    ;more digits to do if CX >= 0

LTBdone:
        CLD                                     ;Go forward
        MOV     SI,Offset TempReal2             ;Point SI to the real
        CALL    PackTempReal                    ;Pack it
        CALL    SetUsersReal                    ;Store it in user's real
        Exit_Code 8                             ;!!.13

LongintToBCD    ENDP

;****************************************************** RealToBCD

;procedure RealToBCD(R : Real; var B : BCD);

;Convert a real to a BCD


RealToBCD       PROC FAR

        StackFrameBP
        PUSH    [BP+14]                         ;Push the real on the stack
        PUSH    [BP+12]                         ;  for the CallStr routine
        PUSH    [BP+10]
        PUSH    DS
        MOV     AX,offset TempReal1             ;use TempReal1 to hold string
        PUSH    AX                              ;Push the address of the string
        CALL    SystemStr                       ;Call the system STR routine

        MOV     AX,DS
        MOV     ES,AX                           ;ES=DS
        MOV     SI,offset TempReal1             ;Point to base of string
        INC     SI                              ;Skip length byte
        CMP     BYTE PTR [SI+2],'.'             ;Is decimal point where we expect?
        JZ      RTBNoBlank                      ;Missing Turbo 3 blank
        INC     SI                              ;Skip blank byte
RTBNoBlank:
        MOV     DI,SI                           ;Use DI for output
        MOV     BX,SI                           ;Save in BX
        MOV     CX,17                           ;17 characters to fix
        MOV     AH,30h                          ;Conversion to ascii
        CLD                                     ;Forward
        CMP     BYTE PTR [BX+1],'0'             ;Is whole number 0?
        JNZ     RTBNextShiftAscii               ;No, go ahead and process
        MOV     TempReal2,0                     ;Store sign/exponent byte
        JMP     SHORT RTBDone                   ;We're done

RTBNextShiftAscii:
        LODSB                                   ;Get character from string
        SUB     AL,AH                           ;Shift ASCII to decimal digit
        STOSB                                   ;Store it back
        LOOP    RTBNextShiftAscii

        MOV     AL,[BX+15]                      ;Tens digit of exponent
        SHL     AL,1
        MOV     AH,AL                           ;2*digit in AH
        SHL     AL,1
        SHL     AL,1                            ;8*digit in AL
        ADD     AL,AH                           ;10*digit in AL
        ADD     AL,[BX+16]                      ;Complete exponent in AL
        INC     AL                              ;Add 1 for mantissa decimal shift
        MOV     AH,3Fh                          ;Bias for exponent
        CMP     BYTE PTR [BX+14],'-'-30h        ;Is exponent negative?
        JNZ     RTBBiasExp
        DEC     AL
        DEC     AL                              ;Correct for mantissa decimal shift
        NEG     AL                              ;Reverse sign of exponent
RTBBiasExp:
        ADD     AH,AL                           ;AH has biased exponent
        CMP     BYTE PTR [BX], '-'-30h          ;Is mantissa negative?
        JNZ     RTBMantPositive
        OR      AH,SignBit                      ;Set negative sign bit
RTBMantPositive:
        MOV     TempReal2,AH                    ;Store sign/exponent byte

        MOV     DI,offset TempReal2+MantissaDigits ;Point to end of result
        STD                                     ;Go backwards
        MOV     CX,11                           ;11 digits in 6 byte real

RTBNextDigit:
        INC     BX                              ;Point to next digit of mantissa
        MOV     AL,[BX]
        CMP     AL,'.'-30h                      ;Skip decimal point
        JZ      RTBNextDigit
        STOSB                                   ;Store to unpacked TempReal2
        LOOP    RTBNextDigit                    ;Get 11 digits

        XOR     AL,AL
        MOV     CX,MantissaDigits-11
        REP STOSB                               ;Clear the remaining digits of TempReal2

        MOV     SI,DI                           ;Point SI to base of result
        CALL    PackTempReal                    ;Pack the result
RTBDone:
        CALL    SetUsersReal                    ;Return result
        Exit_Code 10                            ;!!.13

RealToBCD       ENDP

;****************************************************** BCDtoReal

;function BCDtoReal(B : BCD) : Real;

;Convert a BCD to a real

BTRStoreChar PROC NEAR
        INC     BX                              ;Point to next character
        MOV     [BX],AL                         ;Store what's in AL
        RET
BTRStoreChar ENDP

BCDtoReal       PROC FAR

        StackFrameBP
        MOV     BX,DS                           ;Save DS
        MOV     ES,BX                           ;Set ES=DS
        CALL    CopyUsersTo2                    ;Put B into TempReal2

        MOV     SI,offset TempReal2             ;Unpack TempReal2
        STD                                     ;Go backward
        MOV     CX,MantissaLength               ;CX has # of bytes in mantissa
        ADD     SI,CX                           ;Point to end of mantissa
        MOV     DI,SI
        ADD     DI,CX                           ;Point DI to end of unpacked mantissa
        CALL    UnpackTempReal                  ;Unpack it

        MOV     BX,offset TempReal1             ;TempReal1 will hold string
        MOV     AL,18                           ;Length of string if positive BCD
        MOV     [BX],AL                         ;Store default length
        MOV     AH,TempReal2                    ;Get exponent/sign
        TEST    AH,SignBit
        JZ      BTRPositive                     ;Jump if positive number
        INC     AL                              ;Increase string length by 1
        MOV     [BX],AL                         ;Store length with '-'
        MOV     AL,'-'
        CALL    BTRStoreChar                    ;Store '-'

BTRPositive:
        AND     AH,NoSignBit                    ;Get exponent
        MOV     AL,'0'
        CALL    BTRStoreChar
        MOV     AL,'.'
        CALL    BTRSToreChar                    ;Store '0.'

        MOV     SI,offset TempReal2+MantissaDigits ;Point to MSD of mantissa
        MOV     CX,12                           ;Take 12 digits

BTRNextDigit:
        LODSB                                   ;Next digit
        ADD     AL,30h                          ;Convert to ASCII
        CALL    BTRStoreChar                    ;Store it in string
        LOOP    BTRNextDigit                    ;Get 12 of them

        MOV     AL,'E'
        CALL    BTRStoreChar                    ;Store exponent indicator
        MOV     AL,'+'                          ;Assume positive exponent
        SUB     AH,3Fh                          ;Debias exponent
        JNS     BTRPosExponent                  ;Jump if exponent positive
        MOV     AL,'-'                          ;Negative exponent
        NEG     AH                              ;Convert exponent to positive
BTRPosExponent:
        CALL    BTRStoreChar                    ;Store exponent sign
        MOV     AL,'0'-1                        ;Prepare for counting tens
BTRExpTens:
        INC     AL                              ;Count tens in exponent
        SUB     AH,10
        JNB     BTRExpTens
        CALL    BTRStoreChar                    ;Store tens count
        ADD     AH,'9'+1                        ;Convert remainder to character
        MOV     AL,AH
        CALL    BTRStoreChar                    ;Store ones count

        PushReal TempReal1                      ;Push address of string
        PushReal TempReal2                      ;Push address of real
        PushReal TempReal3                      ;Push address of code
        CALL    SystemVal                       ;Let system convert string

        MOV     SI,offset TempReal2
        MOV     AX,[SI]
        MOV     BX,[SI+2]
        MOV     DX,[SI+4]                       ;Return real in registers
        Exit_Code 4                             ;!!.13

BCDtoReal       ENDP

;****************************************************** IsDigit

;Checks character in AL to see if it's in the range '0'..'9'
;On exit, the carry flag is set if it is not

IsDigit PROC NEAR

        CMP     AL,'0'                          ;AL < '0'?
        JB      IDexit                          ;Exit if it is
        CMP     AL,'9'+1                        ;AL < ('9' + 1)?
        CMC                                     ;flip the carry flag
IDexit: RET

IsDigit ENDP

;****************************************************** AddDigit

AddDigit        PROC NEAR

        PUSH    BX                              ;Save BX
        AND     AX,0Fh                          ;Convert '0' to 0, etc.
        MOV     BX,DI                           ;BX = DI
        SUB     BX,AX                           ;Subtract value of digit
        CMP     BX,MSDoffset                    ;these will be equal if no
                                                ;digits have been added and this
                                                ;one is '0'
        JE      ADexit                          ;exit if they are
        CMP     DI,Offset TempReal2             ;Is the real full?
        JBE     ADexit                          ;If so, don't add the digit
        MOV     [DI],AL                         ;Put in the digit
        DEC     DI                              ;point to previous slot
ADexit:
        POP     BX                              ;restore BX
        RET

AddDigit        ENDP


;****************************************************** ValPrimitive

;This routine does most of the work of Val. In case of error, it returns
;control with the carry flag set.

ValPrimitive    PROC NEAR

        MOV     DI,MSDoffset                    ;point DI to MSB
        SetZero AX                              ;Current char = 0
        SetZero DX                              ;Exponent = 0
        SetZero CX                              ;# of digits before '.' = 0
                                                ;if number is 0.0n, CX holds
                                                ;# of 0's after the '.' + 1
        MOV     SignMask,AL                     ;SignMask = 0
        MOV     ExpSigned,AL                    ;ExpSigned = False

        MOV     AL,[BX]                         ;get next digit
        CMP     AL,'+'                          ;Is it a plus?
        JE      VPsignFound                     ;if so, ignore it
        CMP     AL,'-'                          ;Is it a minus?
        JNE     VPfirstDigit                    ;if not, skip ahead
        MOV     SignMask,SignBit                ;else, initialize
VPsignFound:
        INC     BX                              ;skip the sign

VPfirstDigit:
        MOV     AL,[BX]                         ;AL has next char
        CALL    IsDigit                         ;See if it's a digit
        JNC     VPeatZeros                      ;if so, OK
        JMP     VPerrExit                       ;else exit with error

VPeatZeros:
        CMP     BYTE PTR [BX],'0'               ;Is first char a '0'?
        JNE     VPpreDot                        ;If not proceed
        INC     BX                              ;else, eat it
        JMP     SHORT VPeatZeros                ;repeat

VPpreDot:
        MOV     AL,[BX]                         ;AL has next char
        CALL    IsDigit                         ;See if it's a digit
        JC      VPdoDot                         ;if not, check for a period
;VPfirstOK:
        CALL    AddDigit                        ;add the digit
        INC     BX                              ;Increment string pointer
        INC     CX                              ;Increment count of digits
        JMP     SHORT VPpreDot                  ;repeat

VPdoDot:
        CMP     AL,'.'                          ;Is next char a '.'?
        JNE     VPdoExponent                    ;If not, skip to exponent section
        INC     BX                              ;else, pass over the period
        OR      CX,CX                           ;Is digit count 0?
        JNZ     VPpostDot                       ;If so, 0's are now significant
        INC     CX                              ;Counteract DEC CX to follow

VPeatZeros2:
        DEC     CX                              ;Decrement digit count
        CMP     BYTE PTR [BX],'0'               ;Is next char '0'?
        JNE     VPpostDot                       ;If not, continue
        INC     BX                              ;else, skip over it
        JMP     SHORT VPeatZeros2               ;repeat

VPpostDot:
        MOV     AL,[BX]                         ;AL has next char
        CALL    IsDigit                         ;Is it a digit?
        JC      VPdoExponent                    ;If not, check for 'Exx'
        CALL    AddDigit                        ;else, add the digit
        INC     BX                              ;increment string pointer
        JMP     SHORT VPpostDot                 ;repeat

VPdoExponent:
        CMP     AL,'e'                          ;Is next char an 'e'?
        JE      VPskipE                         ;if so, skip it
        CMP     AL,'E'                          ;Is next char an 'E'?
        JNE     VPsetExponent                   ;set exponent if it isn't
VPskipE:
        INC     BX                              ;skip over the E
        MOV     AL,[BX]                         ;AL has next char
        CMP     AL,'+'                          ;Is it a '+'?
        JE      VPskipSign                      ;If so, skip it
        CMP     AL,'-'                          ;Is it a '-'?
        JNE     VPexpDigitTest                  ;If not, check for a digit
        MOV     ExpSigned,True                  ;else, set signed flag
VPskipSign:
        INC     BX                              ;Point to next char

VPexpDigitTest:
        MOV     AL,[BX]                         ;AL = char
        CALL    IsDigit                         ;Check for a digit
        JC      VPerrExit                       ;exit if there isn't one
VPexpDigit:
        MOV     AL,[BX]                         ;AL = char
        CALL    IsDigit                         ;Check for a digit
        JC      VPsetExponent                   ;set the exponent if it isn't
        AND     AL,0Fh                          ;Convert to digit
        PUSH    AX                              ;Save AX
        MOV     AX,DX                           ;AX =  1 * DX
        SHL     DX,1                            ;DX =  2 * DX
        SHL     DX,1                            ;DX =  4 * DX
        ADD     DX,AX                           ;DX =  5 * DX
        SHL     DX,1                            ;DX = 10 * DX
        POP     AX                              ;Restore AX
        ADD     DX,AX                           ;DX = (10 * DX) + AX
        INC     BX                              ;Point to next char
        JMP     SHORT VPexpDigit                ;Repeat

VPsetExponent:
        CMP     ExpSigned,AH                    ;Is ExpSigned 0?
        JE      VPexpPositive                   ;exponent positive if it is
        NEG     DX                              ;else, negate the exponent
VPexpPositive:
        ADD     DX,CX                           ;Add to the exponent the # of
                                                ;digits before the period
        ADD     DX,3Fh                          ;Add the offset
        JS      VPerrExit                       ;Exit with error if SF set
        CMP     DX,7Fh                          ;Is exponent > $7F?
        JA      VPerrExit                       ;Exit with error if it is
        OR      DL,SignMask                     ;Merge in the sign bit

        ;check to see if real is all zeros

        MOV     SI,Offset TempReal2             ;Point SI to TempReal2
        MOV     DI,SI                           ;DI too
        SetZero AX                              ;AX = 0
        MOV     CX,BCDlength                    ;CX = Words to check
        REPZ    SCASW                           ;Scan while 0
        JZ      VPexit                          ;if ZF set, all 0's
        MOV     [SI],DL                         ;else, set exponent in TempReal2
VPexit:
        CLC                                     ;clear carry flag
        RET                                     ;return
VPerrExit:
        STC                                     ;set carry flag
        RET

ValPrimitive    ENDP

;****************************************************** ValBCD

;procedure ValBCD(S : string; var B : BCD; var Code : Word);
;Convert a string to a BCD

ValS    EQU     DWORD PTR [BP+14]
ValB    EQU     DWORD PTR [BP+10]
ValCode EQU     DWORD PTR [BP+6]

ValBCD  PROC FAR

        StackFrameBP
        CLD                                     ;go forward
        MOV     AX,DS                           ;AX = DS
        MOV     ES,AX                           ;ES = DS

        SetZero AX                              ;AX = 0
        MOV     CX,BCDlength                    ;CX has words to fill
        MOV     DI,Offset TempReal2             ;ES:DI points to TempReal2
        REP     STOSW                           ;fill with 0's

        MOV     BX,Offset Asciiz                ;BX = Ofs(Asciiz)
        MOV     DI,BX                           ;ES:DI points to Asciiz
        LDS     SI,ValS                         ;DS:SI points to string
        MOV     CL,[SI]                         ;CX has length of S
        INC     SI                              ;point to 1st char in S
        JCXZ    VBaddNull                       ;nothing to do if length is 0
        CMP     CL,126                          ;Is string too long?       !!.10
        JNA     VBlengthOK                      ;If not, go ahead and move it
        MOV     CL,126                          ;else, cut off the end     !!.10
VBlengthOK:
        CMP     BYTE PTR [SI],'.'               ;String starts with dot?   !!.10
        JNE     VBmakeAscZ                      ;Jump if not               !!.10
        MOV     BYTE PTR ES:[DI],'0'            ;Start with zero           !!.10
        INC     DI                              ;                          !!.10
VBmakeAscZ:                                     ;                          !!.10
        REP     MOVSB                           ;Move the string
VBaddNull:
        STOSB                                   ;put null at end
        MOV     CX,ES                           ;restore DS
        MOV     DS,CX
VBskipSpace:
        CMP     BYTE PTR [BX],' '               ;Is next char a space?
        JNE     VBNoSpace                       ;If not, OK
        INC     BX                              ;else, advance pointer
        JMP     SHORT VBskipSpace               ;and check again
VBNoSpace:
        CMP     [BX],AL                         ;1st char a null?
        JE      VBerr                           ;if so, it's an error
        CALL    ValPrimitive                    ;else, call
        JC      VBerr                           ;error if carry flag set

        SetZero AX                              ;AX = 0
        CMP     [BX],AL                         ;Does BX point to a null
        JE      VBok                            ;If so, OK
VBerr:
        SUB     BX,(Offset Asciiz - 1)          ;else, subtract starting offset - 1
        MOV     AX,BX                           ;move result into AX
VBok:
        LES     DI,ValCode                      ;Point to Code parameter
        MOV     ES:[DI],AX                      ;set Code

        MOV     AX,DS                           ;ES = DS
        MOV     ES,AX
        MOV     SI,Offset TempReal2
        CALL    PackTempReal                    ;Pack the result
        LES     DI,ValB                         ;Point ES:DI to user's real
        MOV     CX,WordsInBCD                   ;CX has # of words to move
        REP     MOVSW                           ;Move the real
        Exit_Code 12                            ;!!.13

ValBCD  ENDP

;****************************************************** StrBCD

;function StrBCD(B : BCD; Width, Places : Byte) : string;
;Return a BCD as a string

SBresult        EQU     DWORD PTR [BP+14]
SBreal          EQU     DWORD PTR [BP+10]
SBwidth         EQU     BYTE PTR [BP+8]
SBplaces        EQU     BYTE PTR [BP+6]

StrBCD  PROC FAR

        StackFrameBP

        ;move user's real to TempReal2
        CLD                                     ;go forward
        MOV     AX,DS                           ;AX = DS
        LDS     SI,SBreal                       ;DS:SI points to user's real
        MOV     ES,AX                           ;ES = DS
        MOV     DI,Offset TempReal2             ;ES:DI => TempReal2
        MOV     CX,WordsInBCD                   ;CX = 5
        REP     MOVSW                           ;Move the real into TempReal2
        MOV     DS,AX                           ;Restore DS

        ;unpack TempReal2
        MOV     CX,MantissaLength               ;CX has # of bytes in mantissa
        MOV     SI,Offset TempReal2             ;DS:SI => TempReal2
        ADD     SI,CX                           ;Point to end of first mantissa
        MOV     DI,SI
        ADD     DI,CX                           ;Point DI to end of unpacked mantissa
        STD                                     ;Go backward
        CALL    UnpackTempReal                  ;Unpack the real

        CLD                                     ;go forward
        MOV     SI,Offset TempReal2             ;point SI to TempReal2
        MOV     BL,[SI]                         ;BL = sign/exponent
        MOV     BYTE PTR [SI],0                 ;set sign/exponent to 0
        MOV     BH,BL                           ;BH = sign/exponent
        AND     BH,SignBit                      ;BH = sign
        OR      BL,BL                           ;is exponent 0?
        JNZ     SBgo                            ;Go ahead if it isn't
        MOV     DI,SI                           ;else, point DI to TempReal2
        MOV     CX,BcdLength                    ;CX has words to fill
        SetZero AX                              ;AX = 0
        REP     STOSW                           ;fill TempReal2 with 0's

SBgo:
        MOV     DX,1                            ;DX = 1
        AND     BL,NoSignBit                    ;clear the sign bit of the exponent
        SUB     BL,40h                          ;subtract the offset
        JLE     SBjustOne                       ;jump if exponent <= 0
        ADD     DL,BL                           ;else, DL = Exponent + 1

SBjustOne:
        SetZero CH                              ;CH = 0
        MOV     CL,SBPlaces                     ;CX = Places
        ADD     DX,CX                           ;Add decimal places to string length
        MOV     AX,DX                           ;Number of mantissa digits in AX
        JCXZ    SBnoPlaces                      ;If no decimal places, skip ahead
        INC     DX                              ;Add 1 for the period

SBnoPlaces:
        MOV     DI,SI                           ;DI = offset TempReal2
        ADD     DI,MantissaDigits               ;point DI to MSB of mantissa
        CMP     BL,0                            ;is exponent >= 0?
        JGE     SBaxOK                          ;Mantissa digits OK if it is
        SetZero AH                              ;AH = 0
        MOV     AL,BL                           ;AL = exponent
        CBW                                     ;extend the sign
        ADD     AX,DX                           ;we want to see if DX-1 >
        DEC     AX                              ; Abs(Exp)
        JGE     SBaxOK                          ;if so, OK
        SetZero AX                              ;else, mantissa digits to use = 0
SBaxOK:
        MOV     CX,AX                           ;digits to use in CX
        CMP     CX,MantissaDigits
        JAE     SBallDigits                     ;no need to round if >total digits

        ;get ready to round

        PUSH    CX                              ;Save CX
        SUB     DI,CX                           ;subtract # of digits in CX
        MOV     AX,DS                           ;AX = DS
        MOV     ES,AX                           ;ES = DS
        CALL    RoundUnpackedReal               ;round the real
        POP     CX                              ;restore CX
        CMP     BYTE PTR [SI+SigDigits],0       ;is overflow byte 0?
        JZ      SBrounded                       ;if so, proceed
        INC     BL                              ;else, increment exponent
        OR      BL,BL                           ;Compare to 0
        JG      SBIncLength                     ;If greater, increment overall length
        OR      CX,CX                           ;Is number of digits zero?
        JNE     SBRounded                       ;If so,
        INC     CX                              ; add one
        JMP SHORT SBrounded

SBIncLength:
        INC     DX                              ;increase string length
        JMP SHORT SBrounded

SBallDigits:
        MOV     CX,MantissaDigits               ;do all digits in the mantissa

SBrounded:
        PUSH    CX                              ;save # of digits to do
        PUSH    BX                              ;save exponent

        LES     DI,SBresult                     ;point ES:DI to function result
        MOV     SI,DI                           ;SI = DI
        INC     DI                              ;DI points to 1st char in string
        OR      BH,BH                           ;Is number negative?
        JZ      SBnotNegative                   ;If not, DX is fine
        INC     DX                              ;else, increment DX to account
                                                ;for minus sign

SBnotNegative:
        SetZero AH                              ;AH = 0
        MOV     AL,SBwidth                      ;AX = width
        MOV     CX,AX                           ;CX = width
        SUB     CX,DX                           ;CX = width - DX
        JG      SBfillBlanks                    ;jump if width > DX
        MOV     AX,DX                           ;width = DX
        JMP     SHORT SBnoBlanks                ;don't fill with blanks

SBfillBlanks:
        MOV     BL,AL                           ;save AL in BL
        MOV     AL,' '                          ;AL = ' '
        REP     STOSB                           ;fill with blanks
        MOV     AL,BL                           ;restore AL

SBnoBlanks:
        OR      BH,BH                           ;is number negative?
        JZ      SBnoMinus                       ;if not, no minus sign
        MOV     BL,AL                           ;save AL in BL
        MOV     AL,'-'                          ;AL = '-'
        STOSB                                   ;store it
        MOV     AL,BL                           ;restore AL

SBnoMinus:
        CMP     AX,00FFh                        ;width > 255
        JNA     SBwidthOK                       ;if not, ok
        MOV     AX,00FFh                        ;else, width = 255

SBwidthOK:
        MOV     ES:[SI],AL                      ;set length byte of string
        MOV     BX,SI                           ;offset of string
        POP     DX                              ;recover exponent
        ADD     BX,AX                           ;BX = BX + Width
        PUSH    BX                              ;save target offset for later
        MOV     AL,SBplaces                     ;AX = Places
        OR      AL,AL                           ;is it 0?
        JZ      SBzeroPlaces                    ;jump if it is
        SetZero AH                              ;AH = 0
        SUB     BX,AX                           ;else, BX = BX - Places
        JMP     SHORT SBzeroPad                 ;ready to pad with 0's

SBzeroPlaces:
        MOV     BX,SI                           ;back up BX

SBzeroPad:
        SetZero AH                              ;AH = 0
        MOV     AL,DL                           ;AL = exponent
        CBW                                     ;extend sign into AH
        OR      AH,AH                           ;is high byte 0?
        JZ      SBchkOverflow                   ;nothing to do if it is
        MOV     DX,AX                           ;exponent into DX
        NEG     DX                              ;get absolute value
        XCHG    CX,DX                           ;switch CX and DX
        MOV     AL,'0'                          ;AL = '0'
        STOSB                                   ;store it
        DEC     CX                              ;decrement CX
        JZ      SBzerosDone                     ;done if zero
        MOV     AL,'.'                          ;AL = '.'
        STOSB                                   ;store it
        MOV     AL,'0'                          ;AL = '0'
        REP     STOSB                           ;fill

SBzerosDone:
        MOV     CX,DX                           ;restore CX

SBchkOverflow:
        POP     DX                              ;DX = target offset
        INC     DX                              ;target offset is actually one
                                                ;beyond the end of the string
        POP     CX                              ;restore # of digits to do
        JCXZ    SBdigitsDone                    ;skip ahead if 0
        MOV     AH,30h                          ;for converting to ASCII
        MOV     SI,Offset TempReal2             ;SI points to TempReal2
        ADD     SI,MantissaDigits               ;SI points to MSD
        CMP     BYTE PTR [SI+1],0               ;is overflow byte 0?
        JZ      SBnext                          ;if so, proceed
        INC     SI                              ;else, point SI to it

SBnext:
        ;does DI point to position of period (in BX)?
        CMP     DI,BX                           ;DI point to position of '.'
        JNE     SBnoPeriod                      ;if not, continue
        MOV     AL,'.'                          ;else, AL = '.'
        STOSB                                   ;store it

SBnoPeriod:
        MOV     AL,[SI]                         ;get next digit
        DEC     SI                              ;decrement SI
        ADD     AL,AH                           ;convert to ASCII
        STOSB                                   ;store the digit
        LOOP    SBnext                          ;repeat

SBdigitsDone:
        MOV     CX,DX                           ;CX = target offset
        SUB     CX,DI                           ;subtract current offset
        JNA     SBdone                          ;done if target not greater
        MOV     AL,'0'                          ;AL = '0'
SBpadEnd:
        CMP     DI,BX                           ;DI point to position of '.'?
        JNE     SBnoPeriod2                     ;if not, continue
        MOV     AL,'.'                          ;else, AL = '.'
        STOSB                                   ;store it
        MOV     AL,'0'                          ;reload AL with '0'
SBnoPeriod2:
        STOSB                                   ;store the 0
        LOOP    SBpadEnd                        ;repeat

SBdone:
        Exit_Code 8                             ;!!.13

StrBCD  ENDP

;****************************************************** StrExpBCD

;function StrExpBCD(B : BCD; Width : Byte ) : string;
;Return B as a string in exponential format

SEBwidth        EQU     BYTE PTR [BP+6]
SEBreal         EQU     DWORD PTR [BP+8]
SEBresult       EQU     DWORD PTR [BP+12]

StrExpBCD       PROC FAR

        StackFrameBP

        ;move user's real to TempReal2

        CLD                                     ;go forward
        MOV     AX,DS                           ;AX = DS
        LDS     SI,SEBreal                      ;DS:SI points to user's real
        MOV     ES,AX                           ;ES = DS
        MOV     DI,Offset TempReal2             ;ES:DI => TempReal2
        MOV     CX,WordsInBCD                   ;CX = 5
        REP     MOVSW                           ;Move the real into TempReal2
        MOV     DS,AX                           ;Restore DS

        ;unpack TempReal2

        MOV     CX,MantissaLength               ;CX has # of bytes in mantissa
        MOV     SI,Offset TempReal2             ;DS:SI => TempReal2
        ADD     SI,CX                           ;Point to end of first mantissa
        MOV     DI,SI
        ADD     DI,CX                           ;Point DI to end of unpacked mantissa
        STD                                     ;Go backward
        CALL    UnpackTempReal                  ;Unpack the real

        ;validate the width parameter

        MOV     DL,SEBwidth                     ;Get width in DL
        MOV     DH,24                           ;Full accuracy width
        OR      DL,DL                           ;Is input zero?
        JZ      SEBgetSign                      ;Yes, use full accuracy length
        MOV     DH,8
        CMP     DL,8                            ;Compare to minimum width
        JBE     SEBgetSign
        MOV     DH,DL

;DH has output width to use

SEBgetSign:
        CLD                                     ;go forward
        MOV     SI,Offset TempReal2             ;point SI to TempReal2
        MOV     BL,[SI]                         ;BL = sign/exponent
        MOV     BH,BL                           ;BH = sign/exponent
        AND     BH,SignBit                      ;BH = sign

        OR      BL,BL                           ;is exponent 0?
        JNZ     SEBgo                           ;Go ahead if it isn't
        MOV     DI,SI                           ;else, point DI to TempReal2
        MOV     CX,BcdLength                    ;CX has words to fill
        SetZero AX                              ;AX = 0
        REP     STOSW                           ;fill TempReal2 with 0's
        MOV     BL,40h                          ;make exponent print as zero
SEBgo:
        LES     DI,SEBresult                    ;point ES:DI to function result

        MOV     AL,DH                           ;Output length in AL
        STOSB                                   ;set length byte

        MOV     AL,' '                          ;AL = ' '
        CMP     DH,24                           ;Compare length to full accuracy
        JBE     SEBshowSign                     ;No need to padding
        XOR     CH,CH
        MOV     CL,DH
        MOV     DH,24                           ;Remaining length will be 24
        SUB     CL,DH                           ;Number of blanks
        REP     STOSB                           ;Store padding

SEBshowSign:
        OR      BH,BH                           ;BH = 0?
        JZ      SEBsign                         ;if so, plug in the blank
        MOV     AL,'-'                          ;else insert a minus sign
SEBsign:
        STOSB                                   ;add blank or minus sign

        ADD     SI,MantissaDigits               ;Point SI to MSD
        MOV     CX,17                           ;Default decimal places
        MOV     DL,24
        CMP     DH,DL                           ;Full accuracy?
        JAE     SEBmantissa                     ;Yes, go do it

        PUSH    DI                              ;Save string position
        MOV     DI,offset TempReal2             ;Base position for rounding
        SUB     DL,DH                           ;Number of positions to round
        XOR     DH,DH                           ;DH no longer needed
        ADD     DI,DX                           ;Start position for rounding
        SUB     CX,DX                           ;Decimal places to display
        PUSH    CX                              ;Save decimal places
        MOV     CX,MantissaDigits               ;18 digits in all
        SUB     CX,DX                           ;Remainder to check

        CMP     BYTE PTR [DI],5                 ;Compare to 5
        CMC                                     ;Prepare carry flag for rounding
        JNC     SEBroundDone                    ;Done if less
        INC     DI                              ;Skip lowest digit

SEBroundNext:
        MOV     AL,[DI]                         ;Get next byte
        ADC     AL,0                            ;Add just the carry from previous
        AAA                                     ;Adjust for unpacked BCD
        MOV     [DI],AL                         ;Store rounded digit
        INC     DI                              ;Next position
        JNC     SEBroundDone                    ;Done if no carry
        LOOP    SEBroundNext

        MOV     BYTE PTR [DI],1                 ;Set last digit if we got here
        INC     SI                              ;MSD is in next position now
        INC     BL                              ;Increase exponent

SEBroundDone:
        POP     CX
        POP     DI                              ;Restore string position

SEBmantissa:
        MOV     AL,[SI]                         ;AL = MSD
        DEC     SI                              ;decrement digit pointer
        ADD     AL,30h                          ;convert to ASCII
        STOSB                                   ;store the digit
        MOV     AL,'.'                          ;AL = '.'
        STOSB                                   ;store it

SEBnext:
        MOV     AL,[SI]                         ;AL has next digit
        DEC     SI                              ;decrement digit pointer
        ADD     AL,30h                          ;convert to ASCII
        STOSB                                   ;store the digit
        LOOP    SEBnext                         ;repeat

        MOV     AL,'E'                          ;AL = 'E'
        STOSB                                   ;insert the 'E'
        MOV     AL,'+'                          ;assume positive
        AND     BL,NoSignBit                    ;clear the sign bit of the exponent
        SUB     BL,40h                          ;subtract the offset
        JNC     SEBsign2                        ;ready if carry not set
        MOV     AL,'-'                          ;else, change AL to '-'
        NEG     BL                              ;and make the exponent positive
SEBsign2:
        STOSB                                   ;store the sign
        SetZero AH                              ;AH = 0
        MOV     AL,BL                           ;AX has exponent
        MOV     BL,10                           ;BL = 10
        DIV     BL                              ;divide exponent by 10
        ADD     AH,30h                          ;convert AH to ASCII
        ADD     AL,30h                          ;convert AL to ASCII
        STOSW                                   ;store AX
        Exit_Code 6                             ;!!.13

StrExpBCD       ENDP

;****************************************************** AbsBCD

;procedure AbsBCD(B1 : BCD; var B2 : BCD);

;Returns absolute value of B1 in B2


AbsBCD  PROC FAR

        StackFrameBP
        MOV     BX,DS                           ;Save DS

        CLD
        LDS     SI,[BP+10]                      ;Source is B1
        LES     DI,[BP+6]                       ;Destination is B2
        LODSW                                   ;Get first word
        AND     AL,NoSignBit                    ;Clear the sign bit
        STOSW                                   ;Store the first word
        MOV     CX,WordsInBcd-1
        REP MOVSW                               ;Store the remaining words

        MOV     DS,BX                           ;Restore DS
        Exit_Code 8                             ;!!.13

AbsBCD  ENDP

;****************************************************** ActualIntBCD
;Entry: Packed real in TempReal2
;Exit: Packed result int(input) in TempReal2

ActualIntBCD PROC NEAR
        CLD
        MOV     SI,offset TempReal2
        MOV     AL,[SI]                         ;Exponent into AL
        AND     AL,NoSignBit                    ;Exponent only
        JZ      IntBCDDone                      ;If input is zero, we're done
        CMP     AL,51h
        JNB     IntBCDDone                      ;If all digits are integer part, we're done
        CMP     AL,40h
        JNB     IntBCDWork                      ;If not all fraction, do some work
        MOV     BYTE PTR [SI],0                 ;Else return zero
        JMP SHORT IntBCDDone

IntBCDWork:
        MOV     BL,AL                           ;Store exponent
        MOV     CX,MantissaLength
        ADD     SI,CX                           ;Point to end of TempReal2
        MOV     DI,SI
        ADD     DI,CX
        STD                                     ;Backward for this operation
        CALL    UnpackTempReal                  ;Unpack TempReal2
                                                ;Leaves SI pointing to TempReal2
        CLD                                     ;Forward again
        MOV     CL,51h                          ;Magic number
        SUB     CL,BL
        XOR     CH,CH                           ;CX has digits to clear
        XOR     AL,AL
        MOV     DI,SI
        INC     DI                              ;Skip over exponent
        REP STOSB                               ;Clear the fractional part
        CALL    PackTempReal                    ;Pack it up again
IntBCDDone:
        RET
ActualIntBCD ENDP

;****************************************************** IntBCD

;procedure IntBCD(B1 : BCD; var B2 : BCD);

;Returns the integer part of B1 in B2


IntBCD  PROC FAR

        StackFrameBP                            ;Set up stack frame
        CALL    LoadTempReal2                   ;Load TempReal2 with B1
        CALL    ActualIntBCD                    ;Leave the integer part in TempReal2
        CALL    SetUsersReal                    ;Store result
        Exit_Code 8                             ;!!.13

IntBCD  ENDP

;****************************************************** FracBCD

;procedure FracBCD(B1 : BCD; var B2 : BCD);

;Returns the fractional part of B1 in B2


FracBCD PROC FAR

        StackFrameBP
        CALL    LoadTempReal2                   ;Load TempReal2 with B1
        CopyReal TempReal1,TempReal2            ;Also load TempReal1 with B1
        CALL    ActualIntBCD                    ;Leave the integer part in TempReal2
        NegReal TempReal2                       ;Reverse its sign
        CALL    AddPrimitive                    ;Add TempReal1 and -TempReal2
        CALL    SetUsersReal                    ;Move TempReal2 to user's result
        Exit_Code 8                             ;!!.13

FracBCD ENDP

;****************************************************** TruncPrimitive

;Primitive routine used by Trunc and Round.
;On entry, SS:[BP+6] points to a real to be truncated to a long integer.
;On exit, DX:AX contains the long integer, and DI has the last digit added
; to it (needed by Round)

TruncPrimitive  PROC NEAR

        LES     SI,DWORD PTR [BP+6]             ;point to real
        MOV     CL,ES:[SI]                      ;get exponent
        MOV     CH,CL                           ;save in CH
        AND     CL,NoSignBit                    ;clear sign bit
        OR      CL,CL                           ;is it 0?
        JNZ     TPNotZero                       ;done if it is
        JMP     TPZero
TPNotZero:
        SUB     CL,3Fh                          ;subtract exponent offset
        JS      TPZero                          ;done if SF set

        CLD                                     ;go forward
        MOV     DX,CX                           ;save CX in DX
        MOV     AX,DS                           ;AX = DS
        MOV     BX,ES                           ;BX = ES
        MOV     DS,BX                           ;DS = ES
        MOV     ES,AX                           ;ES = DS
        MOV     DI,Offset TempReal2             ;ES:DI => TempReal2
        MOV     CX,WordsInBCD                   ;CX = 5
        REP     MOVSW                           ;Move the real into TempReal2
        MOV     DS,AX                           ;Restore DS

        MOV     CX,MantissaLength               ;CX has # of bytes in mantissa
        MOV     SI,Offset TempReal2             ;DS:SI => TempReal2
        ADD     SI,CX                           ;Point to end of first mantissa
        MOV     DI,SI
        ADD     DI,CX                           ;Point DI to end of unpacked mantissa
        MOV     BX,DI                           ;Save in BX
        STD                                     ;Go backward
        CALL    UnpackTempReal                  ;Unpack the real
        CLD                                     ;clear direction flag
        MOV     CX,DX                           ;Restore CX
        MOV     SI,BX                           ;SI points to end of unpacked mantissa

        SetZero AX                              ;assume result is 0
        SetZero DI

StartLoop:
        DEC     CL                              ;Decrement exponent
        JS      TPDone                          ;Done if SF set

        ;if DI:AX is already >= 214748365 [= Succ(MaxLongInt div 10)], then the
        ;next multiplication by 10 will cause an overflow

        CMP     DI,0CCCh                        ;check high word for overflow
                                                ; $0CCCCCCD = 214748365
        JG      TPoverflow                      ;overflow if DI > $CC
        JL      TPok                            ;OK if DI < $CC
        CMP     AX,0CCCDh                       ;if DI = $CC, check low word
        JBE     TPok                            ;OK if less than $CCCC
TPoverflow:
        INT     OverflowInt                     ;overflow
        RET                                     ;Return in case of error recovery

TPok:   ;multiply DI:AX by 10

        MOV     DX,DI                           ;DX = DI
        MOV     BX,AX                           ;BX = AX
        CLC                                     ;clear carry flag
        RCL     AX,1                            ;multiply by 2 (2X)
        RCL     DI,1
        RCL     AX,1                            ;multiply by 2 again (4X)
        RCL     DI,1
        ADD     AX,BX                           ;Add in original value of DI:AX (5X)
        ADC     DI,DX
        RCL     AX,1                            ;multiply by 2 again (10X)
        RCL     DI,1

        ;add the next digit

        SetZero DH                              ;DH = 0
        MOV     DL,[SI]                         ;DL = digit
        DEC     SI                              ;decrement index
        ADD     AX,DX                           ;Add digit to low word
        ADC     DI,0                            ;Add carry into high word
        JNS     StartLoop                       ;repeat if SF not set, else the
        INT     OverflowInt                     ; addition caused an overflow
        RET                                     ;Return in case of error recovery

TPDone:
        SetZero DH                              ;DH = 0
        MOV     DL,[SI]                         ;DL = next digit
        XCHG    DX,DI                           ;high word into DX from DI
                                                ;last digit into DI
        AND     CH,SignBit                      ;see if sign bit set in the real
        JZ      TPallDone                       ;if not, we're done
        NOT     AX                              ;else, negate the result
        NOT     DX
        ADD     AX,0001
        ADC     DX,0
TPallDone:
        RET
TPZero:
        SetZero AX                              ;result is 0
        SetZero DX
        SetZero DI                              ;DI = 0
        RET

TruncPrimitive  ENDP

;****************************************************** RoundBCD

;function RoundBCD(B1 : BCD) : Longint;
;Returns the value of B1 rounded to the nearest long integer

RoundBCD        PROC FAR

        StackFrameBP
        CALL TruncPrimitive                     ;call primitive routine
        CMP     DI,5                            ;DI >= 5
        JB      RBdone                          ;Done if it isn't
        TEST    CH,SignBit                      ;Check for negative number
        JNZ     RBnegative
        ADD     AX,0001                         ;Increment the low word
        ADC     DX,0                            ;Add any carry into DX
        JMP     SHORT RBdone
RBnegative:
        SUB     AX,0001
        SBB     DX,0
RBdone:
        Exit_Code 4                             ;!!.13

RoundBCD        ENDP

;****************************************************** TruncBCD

;function TruncBCD(B1 : BCD) : Longint;
;Returns the greatest long integer less than or equal to B1

TruncBCD        PROC FAR

        StackFrameBP
        CALL TruncPrimitive                     ;call primitive routine
        Exit_Code 4                             ;!!.13

TruncBCD        ENDP

