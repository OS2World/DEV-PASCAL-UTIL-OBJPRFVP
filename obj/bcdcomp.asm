;******************************************************
;                  BCDCOMP.ASM 1.30
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

;****************************************************** CompReals

;Primitive routine to compare two BCD reals, B1 and B2.
;On entry, pointer to B1 is at BP+10, pointer to B2 at BP+6.
;On exit, ZF and CF indicate the result of the comparison.

CompB1  EQU     DWORD PTR [BP+10]
CompB2  EQU     DWORD PTR [BP+6]

CompReals       PROC NEAR

        MOV     BX,DS                           ;Save DS in BX
        LES     DI,CompB2                       ;ES:DI => Real2
        LDS     SI,CompB1                       ;DS:SI => Real1
        MOV     AH,ES:[DI]                      ;AH has sign/exp of Real2
        MOV     AL,[SI]                         ;AL has sign/exp of Real1

        MOV     CX,AX                           ;Save AX in CX
        XOR     AH,AL                           ;Merge sign bits/exponents
                                                ;SF set if 1 of the 2 is signed
                                                ;ZF set if both are 0
        MOV     AX,CX                           ;Restore AX from CX

        JNS     CRsignsMatch                    ;Continue if signs match
        RCL     AL,1                            ;Move B1's sign bit into CF
        JMP     SHORT CompDone                  ;Done -- if CF is set, Real1
                                                ;is less than Real2

CRsignsMatch:
        TEST    AH,SignBit                      ;If Real2 is neg, both are neg
        JZ      CRcmpExponents                  ;If not, proceed, else...

        ;both #'s are negative, so reverse the comparison. One with the smaller
        ;mantissa or exponent is actually larger (eg, 32/100 > 72/100)

        XCHG    AH,AL                           ;switch sign/exponents
        XCHG    SI,DI                           ;switch offsets
        MOV     CX,DS                           ;CX = Seg(Real1)
        MOV     DX,ES                           ;DX = Seg(Real2)
        MOV     DS,DX                           ;DS:SI => Real2
        MOV     ES,CX                           ;ES:DI => Real1

CRcmpExponents:
        AND     AX,ExponentsOnly                ;Clear the sign bits
        JZ      CompDone                        ;Done if exponents are both 0
        CMP     AL,AH                           ;Real1's exp = Real2's exp?
        JNZ     CompDone                        ;Done if one's exp is larger

CRcmpMantissas:
        STD                                     ;Compare end to front
        MOV     CX,MantissaLength               ;CX has loop count
        ADD     DI,CX                           ;point to end of Real2
        ADD     SI,CX                           ;point to end of Real1
        REPE    CMPSB                           ;compare while equal

CompDone:
        MOV     DS,BX                           ;Restore DS from BX
        CLD                                     ;Clear direction flag
        RET

CompReals       ENDP

;****************************************************** EqualBCD

;function EqualBCD(B1, B2 : BCD) : Boolean;

;Returns true if B1 = B2


EqualBCD        PROC FAR

        StackFrameBP
        CALL    CompReals                       ;Compare the two reals
        MOV     AX,True                         ;Assume true
        JZ      EBdone                          ;Done if ZF set
        DEC     AX                              ;Else AX = 0
EBDone: Exit_Code 8                             ;!!.13

EqualBCD        ENDP

;****************************************************** NotEqualBCD

;function NotEqualBCD(B1, B2 : BCD) : Boolean;

;Returns true if B1 <> B2


NotEqualBCD     PROC FAR

        StackFrameBP
        CALL    CompReals                       ;Compare the two reals
        MOV     AX,True                         ;Assume true
        JNZ     NEBdone                         ;Done if ZF not set
        DEC     AX                              ;Else AX = 0
NEBDone:
        Exit_Code 8                             ;!!.13

NotEqualBCD     ENDP

;****************************************************** GreaterBCD

;function GreaterBCD(B1, B2 : BCD) : Boolean;

;Returns true if B1 > B2


GreaterBCD      PROC FAR

        StackFrameBP
        CALL    CompReals                       ;Compare the two reals
        MOV     AX,True                         ;Assume true
        JA      GBdone                          ;Done if ZF and CF not set
        DEC     AX                              ;Else AX = 0
GBDone: Exit_Code 8                             ;!!.13

GreaterBCD      ENDP

;****************************************************** GreaterEqualBCD

;function GreaterEqualBCD(B1, B2 : BCD) : Boolean;

;Returns true if B1 >= B2


GreaterEqualBCD PROC FAR

        StackFrameBP
        CALL    CompReals                       ;Compare the two reals
        MOV     AX,True                         ;Assume true
        JNC     GEBdone                         ;Done if CF not set
        DEC     AX                              ;Else AX = 0
GEBDone:
        Exit_Code 8                             ;!!.13

GreaterEqualBCD ENDP

;****************************************************** LessBCD

;function LessBCD(B1, B2 : BCD) : Boolean;

;Returns true if B1 < B2


LessBCD PROC FAR

        StackFrameBP
        CALL    CompReals                       ;Compare the two reals
        MOV     AX,True                         ;Assume true
        JC      LBdone                          ;Done if CF set
        DEC     AX                              ;Else AX = 0
LBDone: Exit_Code 8                             ;!!.13

LessBCD ENDP

;****************************************************** LessEqualBCD

;function LessEqualBCD(B1, B2 : BCD) : Boolean;

;Returns true if B1 <= B2


LessEqualBCD    PROC FAR

        StackFrameBP
        CALL    CompReals                       ;Compare the two reals
        MOV     AX,True                         ;Assume true
        JNA     LEBdone                         ;Done if CF or ZF set
        DEC     AX                              ;Else AX = 0
LEBDone:
        Exit_Code 8                             ;!!.13

LessEqualBCD    ENDP

