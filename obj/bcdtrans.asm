;******************************************************
;                  BCDTRANS.ASM 1.30
;              Transcendental BCD routines
;     Copyright (c) TurboPower Software 1987, 1992
;                All rights reserved.
;******************************************************

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

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        ;BCDTRANS.ASM
        PUBLIC  ExpBCD, SqrtBCD, SqrBCD, LnBCD, ArcTanBCD, CosBCD, SinBCD

        ;Low level routines in TPBCD.OBJ
        EXTRN   LoadTempReal2 : NEAR
        EXTRN   CopyTempToTemp : NEAR
        EXTRN   CopyUsersTo2 : NEAR
        EXTRN   SetUsersReal : NEAR
        EXTRN   MultPrimitive : NEAR
        EXTRN   DivPrimitive : NEAR
        EXTRN   AddPrimitive : NEAR
        EXTRN   GreaterBCD : FAR
        EXTRN   LessBCD : FAR
        EXTRN   LessEqualBCD : FAR
        EXTRN   RoundBCD : FAR
        EXTRN   LongIntToBCD : FAR
        EXTRN   ActualIntBCD : NEAR

        ;Routines to call system library STR and VAL procedures
        EXTRN   SystemStr : FAR
        EXTRN   SystemVal : FAR

;****************************************************** SqrBCD

;procedure SqrBCD(B1 : BCD; var B2 : BCD);

;Returns the square of B1 in B2


SqrBCD  PROC FAR

        StackFrameBP                            ;Set up stack frame
        CALL    LoadTempReal2                   ;Load TempReal2 with B1
        CopyReal TempReal1,TempReal2            ;Put TempReal2 into TempReal1
        CALL    MultPrimitive                   ;Do the work
        CALL    SetUsersReal                    ;Return result
        Exit_Code 8                             ;!!.13

SqrBCD  ENDP

;******************************************************* PartialSums

;Compute a quantity of the form:
;  x:= (((t2+c0)*t5+c1)*t5+c2)*t5...
;  on entry, t2 has initial value
;            cx has times to loop
;            si points beyond end of c0 (a table of constants)
;            t5 holds repeated multiplier
;  on exit,  t2 holds result

PartialSums PROC NEAR
PartialSumLoop:
        SUB     SI,BcdLength                    ;To next position in constant table
        PUSH    CX                              ;Save counter
        PUSH    SI                              ;Save constant table index
        PUSH    DS                              ;Save DS
        MOV     AX,CS
        MOV     DS,AX                           ;Set DS=CS
        MOV     DI,offset TempReal1
        CALL    CopyTemptoTemp                  ;Copy constant to TR1
        POP     DS                              ;Restore DS
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal5            ;TR1 <= TR5
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        POP     SI                              ;Restore constant table index
        POP     CX                              ;Restore counter
        LOOP    PartialSumLoop                  ;Loop through terms
        RET
PartialSums ENDP

;****************************************************** ExpBCD

;procedure ExpBCD(B1 : BCD; var B2 : BCD);

;Returns the exponential of B1 in B2
;Valid range for B1 is -145.06 to 147.36
;Smaller values will return 0.0
;Large values will cause an overflow error
;At least 17 digits are accurate. For a broad range
;of inputs, ExpBCD matches the extended result of an
;80287 within 3 digits in the 18th place
;Method:

;constants for Exponent calculation
MaxX db 042h,  23h,89h,61h,51h,59h,44h,65h,73h,14h  ; 0.147365445951618923e+3
MinX db 0C2h,  78h,48h,62h,58h,08h,86h,62h,50h,14h  ;-0.145062860858624878e+3
a1   db 03Fh,  55h,36h,50h,06h,38h,96h,88h,85h,86h  ; 0.868588963806503655e+0
a2   db 040h,  33h,79h,83h,16h,60h,76h,27h,62h,31h  ; 0.316227766016837933e+1
a3   db 03Fh,  00h,00h,00h,00h,00h,00h,00h,00h,50h  ; 0.500000000000000000e+0
a4   db 040h,  00h,00h,00h,00h,00h,00h,00h,00h,20h  ; 0.200000000000000000e+1
c1   db 040h,  00h,00h,00h,00h,00h,00h,00h,51h,11h  ; 0.115100000000000000e+1
c2   db 03Ch,  09h,20h,84h,22h,70h,49h,46h,25h,29h  ; 0.292546497022842009e-3
p0   db 045h,  11h,16h,80h,26h,92h,02h,67h,32h,33h  ; 0.333267029226801611e+6
p1   db 044h,  18h,39h,27h,24h,87h,14h,74h,09h,10h  ; 0.100974148724273918e+5
p2   db 041h,  15h,03h,45h,37h,81h,26h,14h,04h,42h  ; 0.420414268137450315e+2
q0   db 045h,  23h,32h,60h,53h,84h,05h,34h,65h,66h  ; 0.666534058453603223e+6
q1   db 044h,  44h,34h,88h,59h,61h,34h,93h,73h,75h  ; 0.757393346159883444e+5
q2   db 042h,  45h,45h,15h,14h,45h,58h,43h,12h,84h  ; 0.841243584514154545e+3

ExpBCD  PROC FAR

        StackFrameBP                            ;Set up stack frame
        CALL    LoadTempReal2                   ;Put argument into TR2

;Range checking
        PushReal TempReal2
        PushConst MaxX
        CALL    GreaterBCD                      ;Is TR2>MaxX?
        JZ      ExpNotTooLarge
        INT     OverflowInt                     ;Overflow
        Exit_Code 8                             ;!!.13

ExpNotTooLarge:
        PushReal TempReal2
        PushConst MinX
        CALL    LessBCD                         ;Is TR2<MinX?
        JZ      ExpNotTooSmall
        MOV     TempReal2,0                     ;Return a zero
        JMP     ExpDone                         ;We're done

ExpNotTooSmall:
        MOV     BX,DS
        MOV     ES,BX                           ;Set ES=DS again
        CopyConst TempReal1,a1                  ;Put A1 into TR1
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        PushReal TempReal2
        CALL    RoundBCD                        ;Round(TempReal2)
                                                ;DX:AX has result, DX is unimportant
        PUSH    AX                              ;Save AX value
        PUSH    DX                              ;Push parameter to LongintToBCD
        PUSH    AX
        PushReal TempReal4                      ;Result will come back in TR4
        CALL    LongIntToBCD                    ;LongIntToBCD(n,TR4)
        MOV     BX,DS
        MOV     ES,BX                           ;Set ES=DS again
        CopyReal TempReal2,TempReal4            ;TR2 <= TR4
        CopyConst TempReal1,c1                  ;TR1 <= c1
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        NegReal TempReal2                       ;TR2 <= -TR2
        CopyReal TempReal1,TempReal2            ;TR1 <= TR2
        CALL    LoadTempReal2                   ;Put argument into TR2
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2
        CopyReal TempReal2,TempReal4            ;TR2 <= TR4
        CopyConst TempReal1,c2                  ;TR1 <= c2
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        NegReal TempReal2                       ;TR2 <= -TR2
        CopyReal TempReal1,TempReal5            ;TR1 <= TR5
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal4,TempReal2            ;TR4 <= TR2
        CopyReal TempReal1,TempReal2            ;TR1 <= TR2
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2

        CopyConst TempReal1,p2                  ;TR1 <= p2
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyConst TempReal1,p1                  ;TR1 <= p1
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal5            ;TR1 <= TR5
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyConst TempReal1,p0                  ;TR1 <= p0
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal4            ;TR1 <= TR4
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyReal TempReal4,TempReal2            ;TR4 <= TR2
        CopyReal TempReal2,TempReal5            ;TR2 <= TR5

        MOV     CX,2                            ;Two loops of partial sums
        MOV     SI,offset q2+BcdLength          ;Constant table ends at q2+BcdLength !!.20
        CALL    PartialSums

        CopyConst TempReal1,q0                  ;TR1 <= q0
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal4            ;TR1 <= TR4
        NegReal TempReal1                       ;TR1 <= -TR1
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal2            ;TR1 <= TR2
        CopyReal TempReal2,TempReal4            ;TR2 <= TR4
        CALL    DivPrimitive                    ;TR2 <= TR2/TR1
        CopyConst TempReal1,a3                  ;TR1 <= a3
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyConst TempReal1,a4                  ;TR1 <= a4
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2

        POP     AX                              ;Get integer power back
        TEST    AX,1                            ;Was exponent odd?
        PUSH    AX                              ;Save AX again
        JZ      ExpFixExponent
        CopyConst TempReal1,a2                  ;TR1 <= a2
        OR      AL,AL
        JS      ExpExpNegative
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        JMP     SHORT ExpFixExponent

ExpExpNegative:
        CALL    DivPrimitive                    ;TR2 <= TR2/TR1

ExpFixExponent:
        POP     AX                              ;Restore exponent again
        MOV     CX,2                            ;Prepare to divide by 2
        CWD
        IDIV    CX                              ;Divide exponent by 2
        ADD     AL,TempReal2                    ;Add to existing exponent
        MOV     TempReal2,AL                    ;Store back

ExpDone:
        CALL    SetUsersReal                    ;Return result from TR2
        Exit_Code 8                             ;!!.13

ExpBCD  ENDP

;****************************************************** LnBCD

;procedure LnBCD(B1 : BCD; var B2 : BCD);

;Returns the natural log of B1 in B2
;At least 17 digits are accurate. For a broad range
;of inputs, LnBCD matches the extended result of an
;80287 within 2 digits in the 18th place
;
;Method:
;   Let X = +/-f * ( 10 ** e ), 1/10 <= f < 1.
;
;   Then calculate a range reduction of the form
;
;      f = g * ( 10 ** (-n) ), 1/sqrt(10) <= g < sqrt(10).
;
;   From this
;
;      log10( X ) = e - n + log10( g ).
;
;   Log10( g ) is calculated from a minimax rational approximation
;   of the form
;
;      R(s**2) = w * A( w ) / B( w ),
;      s = ( f - 1 ) / ( f + 1 ),
;
;   with weights derived by Cody and Waite.

;constants for Ln calculation
d0      db 0C1h, 17h,31h,03h,81h,78h,07h,52h,95h,89h  ;-0.899552077881033117E+2
d1      db 042h, 48h,93h,48h,68h,88h,61h,47h,53h,24h  ; 0.245347618868489348E+3
d2      db 0C2h, 42h,95h,82h,41h,53h,03h,03h,43h,24h  ;-0.244303035341829542E+3
d3      db 042h, 09h,80h,66h,15h,91h,78h,09h,71h,10h  ; 0.107109789115668009E+3
d4      db 0C1h, 86h,47h,85h,32h,58h,34h,32h,37h,19h  ;-0.193732345832854786E+2
e0      db 0C1h, 36h,76h,55h,05h,24h,00h,47h,04h,26h  ;-0.260447002405557636E+2
e1      db 041h, 31h,59h,20h,41h,20h,91h,85h,40h,55h  ; 0.554085912041205931E+2
e2      db 0C1h, 50h,62h,15h,03h,02h,41h,37h,27h,39h  ;-0.392737410203156250E+2
e3      db 041h, 65h,38h,79h,14h,15h,57h,38h,33h,10h  ; 0.103338571514793865E+2
e4      db 0BFh, 39h,92h,91h,61h,41h,78h,10h,10h,74h  ;-0.741010784161919239E+0
f0      db 03Fh, 33h,79h,83h,16h,60h,76h,27h,62h,31h  ; 0.316227766016837933E+0
f1      db 03Fh, 55h,36h,50h,06h,38h,96h,88h,85h,86h  ; 0.868588963806503655E+0
f2      db 040h, 68h,45h,40h,99h,92h,50h,58h,02h,23h  ; 0.230258509299404568E+1
f3      db 040h, 00h,00h,00h,00h,00h,00h,00h,00h,10h  ; 0.100000000000000000E+1

LnBCD   PROC FAR

        StackFrameBP                            ;Set up stack frame
        CALL    LoadTempReal2                   ;Put argument into TR2
        MOV     AL,TempReal2                    ;Get sign/exponent into AL
        OR      AL,AL                           ;Is argument zero?
        JG      LnArgPositive                   ;Jump if greater than zero
        INT     OverflowInt                     ;Error if zero or negative
        Exit_Code 8                             ;!!.13

LnArgPositive:
        XOR     AH,AH                           ;Set top byte to zero
        SUB     AX,003Fh                        ;Unbias exponent
        PUSH    AX                              ;Save exponent momentarily
        MOV     TempReal2,3Fh                   ;Normalize TempReal2 in 0.1 to 0.9999
        PushReal TempReal2
        PushConst f0
        CALL    LessBCD                         ;Is TR2<f0?
        MOV     BX,DS
        MOV     ES,BX                           ;Set ES=DS again
        JZ      LnCompute                       ;Jump if TR2>=f0
        POP     AX
        DEC     AX                              ;Renormalize in 0.316 to 3.16
        INC     TempReal2                       ;Correct working real
        PUSH    AX                              ;Save exponent for later

LnCompute:
        CopyReal TempReal4,TempReal2            ;TR4 <= TR2
        CopyConst TempReal1,f3                  ;TR1 <= f3 (=1.0)
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2 (f+1.0 in TR5)

        CopyConst TempReal1,a3                  ;TR1 <= a3 (=0.5)
        NegReal TempReal1                       ;TR1 <= -a3
        CopyReal TempReal2,TempReal4            ;TR2 <= TR4
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyConst TempReal1,a3                  ;TR1 <= a3 (=0.5)
        NegReal TempReal1                       ;TR1 <= -a3
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2 (f-1.0 in TR2)

        CopyReal TempReal1,TempReal5            ;TR1 <= TR5
        CALL    DivPrimitive                    ;TR2 <= TR2/TR1 (s)

        CopyReal TempReal4,TempReal2            ;TR4 <= TR2 (save s in TR4)
        CopyReal TempReal1,TempReal2            ;TR1 <= TR2
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2 (w)
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2 (save s = w**2 in TR5)

        CopyConst TempReal1,e4                  ;TR1 <= e4
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        MOV     CX,3                            ;3 times through loop
        MOV     SI,offset e4                    ;Starting pos in constant table !!.20
        CALL    PartialSums                     ;Compute partial sums
        CopyConst TempReal1,e0                  ;TR1 <= e0
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal6,TempReal2            ;TR6 <= TR2 (aw)

        CopyReal TempReal2,TempReal5            ;TR2 <= TR5
        MOV     CX,4                            ;4 times through loop
        MOV     SI,offset d4+BcdLength          ;Starting pos in constant table !!.20
        CALL    PartialSums                     ;Compute partial sums
        CopyConst TempReal1,d0                  ;TR1 <= d0
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal2            ;TR1 <= TR2 (bw)

        CopyReal TempReal2,TempReal6            ;TR2 <= TR6
        CALL    DivPrimitive                    ;TR2 <= TR2/TR1
        CopyReal TempReal1,TempReal5            ;TR1 <= TR5
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2 (r(w))

        CopyConst TempReal1,f1                  ;TR1 <= f1
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal4            ;TR1 <= TR4
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyReal TempReal4,TempReal2            ;TR4 <= TR2 (rs)

        POP     AX                              ;Get exponent back
        CWD                                     ;Initialize DX of longint
        PUSH    DX                              ;Push parameter to LongintToBCD
        PUSH    AX
        PushReal TempReal2                      ;Result will come back in TR2
        CALL    LongintToBCD                    ;LongIntToBCD(n,TR4)
        MOV     BX,DS
        MOV     ES,BX                           ;Set ES=DS again
        CopyReal TempReal1,TempReal4            ;TR1 <= TR4
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyConst TempReal1,f2                  ;TR1 <= f2
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2

        CALL    SetUsersReal                    ;Return result from TR2
        Exit_Code 8                             ;!!.13

LnBCD   ENDP

;****************************************************** SqrtBCD

;procedure SqrtBCD(B1 : BCD; var B2 : BCD);

;Returns the square root of B1 in B2
;Uses 4 iterations of Heron's Method after range reduction to 0.1-0.999...
;All 18 digits are accurate

s0   db 03Fh,  00h, 00h, 00h, 00h, 00h, 00h, 07h, 36h, 22h ; 0.223607
s1   db 03Fh,  00h, 00h, 00h, 00h, 00h, 00h, 70h, 44h, 89h ; 0.894470
s2   db 03Fh,  33h, 79h, 83h, 16h, 60h, 76h, 27h, 62h, 31h ; 0.316227766016837933 (SQRT(0.1))

SqrtBCD PROC FAR

        StackFrameBP                            ;Set up stack frame
        CALL    LoadTempReal2                   ;Input to TempReal2

;Error check
        MOV     BL,TempReal2                    ;Get exponent/sign
        OR      BL,BL
        JNZ     SqrtCheckSign
        JMP     SqrtDone                        ;Done for input of zero

SqrtCheckSign:
        TEST    BL,SignBit
        JZ      SqrtOK                          ;OK for positive numbers
        INT     DivZeroInt                      ;Error otherwise! Treat as divide by 0
        Exit_Code 8                             ;!!.13

;Shift input into range 0.1 to 0.999...
SqrtOK:
        SUB     BL,3Fh                          ;Debias exponent
        MOV     AL,BL
        CBW                                     ;Sign extend
        PUSH    AX                              ;Store for later
        MOV BYTE PTR TempReal2,3Fh              ;Normalize input

        CopyReal TempReal4,TempReal2            ;Save input in TR4
        CopyConst TempReal1,s1                  ;TR1 <= s1
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyConst TempReal1,s0                  ;TR1 <= s0
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2, initial approx;

        MOV     CX,3                            ;Iterate three times
SqrtIter:
        PUSH    CX                              ;Save loop counter
        CopyReal TempReal5,TempReal2            ;Save previous iteration in TR5
        CopyReal TempReal1,TempReal5            ;Previous iteration in TR1
        CopyReal TempReal2,TempReal4            ;Original input in TR2
        CALL    DivPrimitive                    ;TR2 <= TR2/TR1
        CopyReal TempReal1,TempReal5            ;Previous iteration in TR1
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyConst TempReal1,a3                  ;TR1 <= 0.5
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        POP     CX                              ;Restore loop counter
        LOOP    SqrtIter

        CopyReal TempReal5,TempReal2            ;Save previous iteration in TR5
        CopyReal TempReal1,TempReal5            ;Previous iteration in TR1
        CopyReal TempReal2,TempReal4            ;Original input in TR2
        CALL    DivPrimitive                    ;TR2 <= TR2/TR1
        CopyReal TempReal1,TempReal5            ;Previous iteration in TR1
        NegReal TempReal1                       ;TR1 <= -TR1
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyConst TempReal1,a3                  ;TR1 <= 0.5
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyReal TempReal1,TempReal5            ;Previous iteration in TR1
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2

        POP     AX                              ;Get exponent correction back
        TEST    AX,1                            ;See if odd
        JZ      SqrtExpEven                     ;Nope, it's even
        PUSH    AX                              ;Save AX a moment
        CopyConst TempReal1,s2                  ;TR1 <= s2
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        POP     AX                              ;Restore AX
        INC     AX                              ;Make exponent even

SqrtExpEven:
        SHR     AX,1                            ;Divide exponent by two
        ADD     TempReal2,AL                    ;Correct result

SqrtDone:
        CALL    SetUsersReal                    ;Pass back answer
        Exit_Code 8                             ;!!.13

SqrtBCD ENDP

;****************************************************** SinCosPrimitive
;Returns the sine or cosine in TempReal2
;Entry:
;      argument in TempReal2
;      CosWanted:Boolean in TempReal6[0]
;      Negate:Boolean in TempReal6[1]
;      DS=ES=DSEG

MaxY  db 049h, 00h,00h,00h,00h,54h,26h,59h,41h,31h  ; 0.314159265400000000e+10
onepi db 03Fh, 72h,06h,79h,83h,61h,88h,09h,83h,31h  ; 0.318309886183790672e+0
b1    db 040h, 00h,00h,00h,00h,00h,00h,00h,41h,31h  ; 0.314100000000000000e+1
b2    db 03Ch, 63h,84h,23h,93h,97h,58h,53h,26h,59h  ; 0.592653589793238463e-3
eps   db 037h, 00h,00h,00h,00h,00h,00h,00h,00h,10h  ; 0.100000000000000000e-8
r1    db 0BFh, 51h,66h,66h,66h,66h,66h,66h,66h,16h  ;-0.166666666666666651e+0
r2    db 03Dh, 03h,65h,31h,33h,33h,33h,33h,33h,83h  ; 0.833333333333316503e-2
r3    db 0BCh, 05h,84h,01h,12h,84h,69h,12h,84h,19h  ;-0.198412698412018405E-3
r4    db 03Ah, 61h,75h,52h,01h,21h,19h,73h,55h,27h  ; 0.275573192101527561E-5
r5    db 0B8h, 45h,58h,74h,82h,79h,06h,21h,05h,25h  ;-0.250521067982745845E-7
r6    db 036h, 91h,58h,71h,03h,49h,36h,89h,05h,16h  ; 0.160589364903715891E-9
r7    db 0B3h, 77h,46h,10h,89h,06h,78h,91h,42h,76h  ;-0.764291780689104677E-12
r8    db 031h, 62h,84h,88h,78h,95h,90h,47h,20h,27h  ; 0.272047909578888462E-14

SinCosPrimitive  PROC NEAR

        AbsReal TempReal2                       ;Make TempReal2 positive
        PushReal TempReal2
        PushConst MaxY
        CALL    GreaterBCD                      ;Is TR2>MaxY?
        JZ      SinCosArgOK                     ;No, compute it
        INT     OverflowInt                     ;Overflow
        RET                                     ;Return in case of error recovery

SinCosArgOK:
        MOV     BX,DS
        MOV     ES,BX                           ;Set ES=DS again
        CopyReal TempReal4,TempReal2            ;Save TR2
        CopyConst TempReal1,OnePi               ;TR1 <= OnePi
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyConst TempReal1,a3                  ;TR1 <= a3 (=0.5)
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2

        CMP     TempReal6,0                     ;Is Cos being computed?
        JZ      SinNormalize                    ;Jump if Sin
        CopyConst TempReal1,a3                  ;TR1 <= a3
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2

SinNormalize:
        CALL    ActualIntBCD                    ;TR2 <= int(TR2)
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2
        CopyConst TempReal1,a3                  ;TR1 <= a3
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyReal TempReal1,TempReal2            ;TR1 <= TR2
        CALL    ActualIntBCD                    ;TR2 <= int(TR2)
        NegReal TempReal2                       ;TR2 <= -TR2
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2

        PushReal TempReal2
        PushConst eps
        CALL    GreaterBCD                      ;Is TR2>eps?
        JZ      XnNotOdd                        ;If so, xn was odd

        MOV     SI,offset TempReal6
        XOR     BYTE PTR [SI+1],1               ;Toggle Negate boolean

XnNotOdd:
        MOV     BX,DS
        MOV     ES,BX                           ;Set ES=DS again
        CMP     TempReal6,0                     ;Is Cos being computed?
        JZ      ComputeNormalPart               ;Jump if Sin
        CopyReal TempReal2,TempReal5            ;TR2 <= xn
        CopyConst TempReal1,a3                  ;TR1 <= a3
        NegReal TempReal1                       ;TR1 <= -TR1
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2

ComputeNormalPart:
        CopyReal TempReal2,TempReal5            ;TR2 <= xn
        CopyConst TempReal1,b1                  ;TR1 <= b1
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        NegReal TempReal2                       ;TR2 <= -TR2
        CopyReal TempReal1,TempReal4            ;TR1 <= y
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal4,TempReal2            ;Save TR2
        CopyReal TempReal2,TempReal5            ;TR2 <= xn
        CopyConst TempReal1,b2                  ;TR1 <= b2
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        NegReal TempReal2                       ;TR2 <= -TR2
        CopyReal TempReal1,TempReal4            ;TR1 <= y
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal4,TempReal2            ;Save TR2 (f)
        AbsReal TempReal2
        PushReal TempReal2
        PushConst Eps
        CALL    LessEqualBCD                    ;Is TR2<=eps?
        MOV     BX,DS
        MOV     ES,BX                           ;Set ES=DS again
        JZ      ComputeSinCos                   ;If not, compute the long way
        JMP     SinCosSetSign

ComputeSinCos:
        CopyReal TempReal1,TempReal2            ;TR1 <= TR2 (f)
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2 (g)
        CopyConst TempReal1,r8                  ;TR1 <= r8
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2

        MOV     CX,7                            ;7 times through loop
        MOV     SI,offset r8                    ;Starting pos in constant table !!.20
        CALL    PartialSums                     ;Compute partial sums

        CopyConst TempReal1,f3                  ;TR1 <= f3 (1.0)
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal4            ;TR1 <= f
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2

SinCosSetSign:
        MOV     SI,offset TempReal6
        TEST    BYTE PTR [SI+1],1               ;Is Negate set?
        JZ      SinCosDone                      ;No, we're done
        NegReal TempReal2                       ;Negate result

SinCosDone:
        RET
SinCosPrimitive ENDP

;****************************************************** CosBCD

;procedure CosBCD(B1 : BCD; var B2 : BCD);

;Returns cosine of B1 in B2


CosBCD  PROC FAR
        StackFrameBP                            ;Set up stack frame
        CALL    LoadTempReal2                   ;Load TempReal2 with B1
        MOV     SI,offset TempReal6
        MOV     BYTE PTR [SI],1                 ;CosWanted is True
        MOV     BYTE PTR [SI+1],0               ;Negate is False
        CALL    SinCosPrimitive                 ;Compute the answer
        CALL    SetUsersReal                    ;Return result
        Exit_Code 8                             ;!!.13
CosBCD  ENDP

;****************************************************** SinBCD

;procedure SinBCD(B1 : BCD; var B2 : BCD);

;Returns sin of B1 in B2


SinBCD  PROC FAR
        StackFrameBP                            ;Set up stack frame
        CALL    LoadTempReal2                   ;Load TempReal2 with B1
        MOV     SI,offset TempReal6
        XOR     AH,AH                           ;Assume Negate False
        MOV     [SI],AH                         ;CosWanted is False
        MOV     AL,TempReal2                    ;Get sign/exponent into AL
        OR      AL,AL                           ;Check sign of argument
        JGE     SetSinNegate                    ;Jump if greater or equal zero
        INC     AH                              ;Set Negate true
SetSinNegate:
        MOV     [SI+1],AH                       ;Set negate
        CALL    SinCosPrimitive                 ;Compute the answer
        CALL    SetUsersReal                    ;Return result
        Exit_Code 8                             ;!!.13
SinBCD  ENDP

;****************************************************** ArcTanBCD

;procedure ArcTanBCD(B1 : BCD; var B2 : BCD);

;Returns arc tangent of B1 in B2

pi4   db 03Fh, 10h,83h,44h,97h,33h,16h,98h,53h,78h  ;0.785398163397448310e+0
g0    db 043h, 92h,03h,44h,40h,87h,75h,75h,57h,50h  ;0.505775758740440392e+4
g1    db 044h, 73h,33h,32h,78h,31h,72h,77h,98h,14h  ;0.149877723178323373e+5
g2    db 044h, 12h,09h,25h,08h,33h,11h,11h,03h,17h  ;0.170311113308250912e+5
g3    db 043h, 40h,53h,28h,00h,94h,79h,59h,09h,93h  ;0.930959799400285340e+4
g4    db 043h, 11h,41h,17h,88h,24h,63h,23h,13h,25h  ;0.251323632488174111e+4
g5    db 042h, 77h,15h,67h,33h,97h,91h,55h,73h,30h  ;0.307355919733671577e+3
g6    db 041h, 93h,63h,86h,60h,65h,10h,50h,20h,13h  ;0.132050106560866393e+2
g7    db 03Eh, 75h,68h,33h,93h,51h,74h,86h,97h,84h  ;0.849786745193336875e-1
h0    db 043h, 92h,03h,44h,40h,87h,75h,75h,57h,50h  ;0.505775758740440392e+4
h1    db 044h, 53h,80h,33h,36h,51h,91h,36h,67h,16h  ;0.166736915136338053e+5
h2    db 044h, 26h,48h,55h,45h,98h,56h,74h,57h,21h  ;0.215774569845554826e+5
h3    db 044h, 41h,06h,95h,09h,15h,82h,98h,88h,13h  ;0.138898821509950641e+5
h4    db 043h, 45h,26h,42h,22h,10h,85h,68h,47h,46h  ;0.464768851022422645e+4
h5    db 042h, 46h,14h,03h,42h,17h,60h,66h,82h,76h  ;0.768266601742031446e+3
h6    db 041h, 51h,81h,84h,13h,09h,96h,16h,26h,53h  ;0.532616960913848151e+2
h7    db 040h, 00h,00h,00h,00h,00h,00h,00h,00h,10h  ;0.100000000000000000e+1

ArcTanBCD       PROC FAR

        StackFrameBP                            ;Set up stack frame
        CALL    LoadTempReal2                   ;Load TempReal2 with B1
        MOV     AL,TempReal2                    ;Get sign/exponent into AL
        OR      AL,AL                           ;Check sign/exponent of argument
        JNZ     ArcTanSaveSign
        JMP     ArcTanDone                      ;Jump if equal zero

ArcTanSaveSign:
        PUSH    AX                              ;Save sign for later

        AbsReal TempReal2                       ;Work with absolute value
        CopyReal TempReal4,TempReal2            ;TR4 <= TR2
        MOV TempReal6,0                         ;TR6 <= 0
        PushReal TempReal2
        PushConst f3                            ;F3 is 1.0
        CALL    GreaterBCD                      ;Is TR2>1.0?
        MOV     BX,DS
        MOV     ES,BX                           ;Set ES=DS again
        JNZ     ArcTanNoSkip                    ;Skip following if <= 1.0
        JMP     ArcTanComputeY

ArcTanNoSkip:
        CopyConst TempReal6,Pi4                 ;TR6 <= Pi4
        CopyReal TempReal2,TempReal4            ;TR2 <= TR4
        CopyConst TempReal1,f3                  ;TR1 <= 1.0
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2
        CopyReal TempReal2,TempReal4            ;TR2 <= TR4
        CopyConst TempReal1,f3                  ;TR1 <= 1.0
        NegReal TempReal1                       ;TR1 <= -TR1
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CopyReal TempReal1,TempReal5            ;TR1 <= TR5
        CALL    DivPrimitive                    ;TR2 <= TR2/TR1
        CopyReal TempReal4,TempReal2            ;TR4 <= TR2

ArcTanComputeY:
        CopyReal TempReal1,TempReal4            ;TR1 <= TR4
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyReal TempReal5,TempReal2            ;TR5 <= TR2 (ysq)

        CopyConst TempReal1,g7                  ;TR1 <= g7
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2

        MOV     CX,6                            ;6 times through loop
        MOV     SI,offset g7                    ;Starting pos in constant table !!.20
        CALL    PartialSums                     ;Compute partial sums

        CopyConst TempReal1,g0                  ;TR1 <= g0
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2
        CALL    SetUsersReal                    ;Store result in output real

        CopyReal TempReal2,TempReal5            ;TR2 <= ysq
        CopyConst TempReal1,h7                  ;TR1 <= h7
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2

        MOV     CX,6                            ;6 times through loop
        MOV     SI,offset h7                    ;Starting pos in constant table !!.20
        CALL    PartialSums                     ;Compute partial sums

        CopyConst TempReal1,h0                  ;TR1 <= h0
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2

        CopyReal TempReal1,TempReal2            ;TR1 <= TR2
        CALL    CopyUsersTo2                    ;Reload p from output
        CALL    DivPrimitive                    ;TR2 <= TR2/TR1
        CopyReal TempReal1,TempReal4            ;TR1 <= TR4
        CALL    MultPrimitive                   ;TR2 <= TR1*TR2
        CopyReal TempReal1,TempReal6            ;TR1 <= TR6
        CALL    AddPrimitive                    ;TR2 <= TR1+TR2 (t+y*(p/q))

        POP     AX                              ;Restore sign
        OR      AL,AL
        JGE     ArcTanDone                      ;Jump if non-negative
        NegReal TempReal2                       ;Flip sign of result

ArcTanDone:
        CALL    SetUsersReal                    ;Return result
        Exit_Code 8                             ;!!.13

ArcTanBCD       ENDP

CODE    ENDS

        END
