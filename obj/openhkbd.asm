;******************************************************
;                 OPENHKBD.ASM 1.30
;              Enhanced keyboard routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

;!!.11 substantially rewritten to solve recurring problems when
;      used on some actual enhanced keyboards

;!!.20 major modifications to support Protected mode

        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT BYTE PUBLIC

        EXTRN   DpmiInUse : BYTE                                       ;!!.20
        EXTRN   BiosDataSele : WORD                                    ;!!.20
        EXTRN   PrevInt09 : DWORD                                      ;!!.20
        EXTRN   PrevInt16 : DWORD                                      ;!!.20
        EXTRN   HasEnhancedKbd : BYTE
        EXTRN   EnableEnhanced : BYTE
        EXTRN   FiltersEnhanced : BYTE

        ShiftReleased   DB      0       ;!!.20 - Moved up from CS

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

        PUBLIC  EnhancedKbd
        PUBLIC  FiltersEnhancedKeys
        PUBLIC  NewInt09, EnhInt09, NewInt16
        PUBLIC  FixOldBios

;Equates for BIOS data area
        BiosShiftFlags  EQU BYTE PTR 17h
        BufferHead      EQU WORD PTR 1Ah
        BufferTail      EQU WORD PTR 1Ch
        BufferStart     EQU WORD PTR 80h
        BufferEnd       EQU WORD PTR 82h


;*********************************************************** EnhancedKbd

;See if enhanced keyboard BIOS installed
;function EnhancedKbd : Boolean;

EnhancedKbd     PROC NEAR

        MOV     AX,BiosDataSele         ;check bit 4 of byte at $40:$96  ;!!.20
        MOV     ES,AX
        MOV     DI,96h
        XOR     AL,AL                   ;assume false
        TEST    BYTE PTR ES:[DI],00010000b
        JZ      EnhDone                 ;if bit is set, it's enhanced
        INC     AL
EnhDone:
        RET

EnhancedKbd     ENDP

;*********************************************************** FiltersEnhancedKeys
;function FiltersEnhancedKeys : Boolean;
FiltersEnhancedKeys PROC NEAR
        PUSH    DS
        MOV     BX,BiosDataSele
        MOV     DS,BX

        CLI
        MOV     BX,DS:BufferStart       ;Get address of buffer start
        MOV     DS:BufferHead,BX        ;Start clearing buffer
        MOV     WORD PTR [BX],08500h    ;Put one enhanced key in buffer
        ADD     BX,2
        MOV     DS:BufferTail,BX        ;One key is in buffer
        STI

        MOV     AH,1                    ;See if BIOS thinks key is there
        INT     16h
        MOV     AX,1                    ;Assume True
        JZ      FiltersDone             ;Jump if no key is there
        DEC     AX                      ;Return False

        CLI
        MOV     DS:BufferHead,BX        ;Clear buffer
        MOV     DS:BufferTail,BX
        STI

FiltersDone:
        POP     DS
        RET
FiltersEnhancedKeys ENDP

IndexTable        LABEL BYTE            ;Table of indexes into WordTable
        DB       0FFh   ;00h
        DB        00    ;01h - Esc
        DB       0FFh   ;02h
        DB       0FFh   ;03h
        DB       0FFh   ;04h
        DB       0FFh   ;05h
        DB       0FFh   ;06h
        DB       0FFh   ;07h
        DB       0FFh   ;08h
        DB       0FFh   ;09h
        DB       0FFh   ;0Ah
        DB       0FFh   ;0Bh
        DB       0FFh   ;0Ch
        DB       0FFh   ;0Dh
        DB        01    ;0Eh - Backspace
        DB        02    ;0Fh - Tab
        DB       0FFh   ;10h
        DB       0FFh   ;11h
        DB       0FFh   ;12h
        DB       0FFh   ;13h
        DB       0FFh   ;14h
        DB       0FFh   ;15h
        DB       0FFh   ;16h
        DB       0FFh   ;17h
        DB       0FFh   ;18h
        DB       0FFh   ;19h
        DB        03    ;1Ah - Left Brack
        DB        04    ;1Bh - Right Brack
        DB        05    ;1Ch - Enter
        DB       0FFh   ;1Dh
        DB       0FFh   ;1Eh
        DB       0FFh   ;1Fh
        DB       0FFh   ;20h
        DB       0FFh   ;21h
        DB       0FFh   ;22h
        DB       0FFh   ;23h
        DB       0FFh   ;24h
        DB       0FFh   ;25h
        DB       0FFh   ;26h
        DB        06    ;27h - Semicolon
        DB        07    ;28h - Quote
        DB        08    ;29h - Backquote
        DB       0FFh   ;2Ah
        DB        09    ;2Bh - Backslash
        DB       0FFh   ;2Ch
        DB       0FFh   ;2Dh
        DB       0FFh   ;2Eh
        DB       0FFh   ;2Fh
        DB       0FFh   ;30h
        DB       0FFh   ;31h
        DB       0FFh   ;32h
        DB        10    ;33h - Comma
        DB        11    ;34h - Period
        DB        12    ;35h - Slash
        DB       0FFh   ;36h
        DB        13    ;37h - Pad-Asterisk
        DB       0FFh   ;38h
        DB       0FFh   ;39h
        DB       0FFh   ;3Ah
        DB       0FFh   ;3Bh
        DB       0FFh   ;3Ch
        DB       0FFh   ;3Dh
        DB       0FFh   ;3Eh
        DB       0FFh   ;3Fh
        DB       0FFh   ;40h
        DB       0FFh   ;41h
        DB       0FFh   ;42h
        DB       0FFh   ;43h
        DB       0FFh   ;44h
        DB       0FFh   ;45h
        DB       0FFh   ;46h
        DB        14    ;47h - Home
        DB        15    ;48h - Up
        DB        16    ;49h - PgUp
        DB        17    ;4Ah - Pad-Minus
        DB        18    ;4Bh - Left
        DB        19    ;4Ch - Pad-5
        DB        20    ;4Dh - Right
        DB        21    ;4Eh - Pad-Plus
        DB        22    ;4Fh - End
        DB        23    ;50h - Down
        DB        24    ;51h - PgDn
        DB        25    ;52h - Insert
        DB        26    ;53h - Del

WordTable         LABEL WORD            ;Table of Scan words to return
        ;         Control   Alt
        DW       0FFFFh,    0100h  ;Esc
        DW       0FFFFh,    0E00h  ;Backspace
        DW        9400h,   0A500h  ;Tab
        DW       0FFFFh,    1A00h  ;Left Brack
        DW       0FFFFh,    1B00h  ;Right Brack
        DW       0FFFFh,    1C00h  ;Enter
        DW       0FFFFh,    2700h  ;Semicolon
        DW       0FFFFh,    2800h  ;Quote
        DW       0FFFFh,    2900h  ;Backquote
        DW       0FFFFh,    2B00h  ;Backslash
        DW       0FFFFh,    3300h  ;Comma
        DW       0FFFFh,    3400h  ;Period
        DW       0FFFFh,    3500h  ;Slash
        DW       0FFFFh,    3700h  ;Pad-Asterisk
        DW       0FFFFh,    9700h  ;Home
        DW        8D00h,    9800h  ;Up
        DW       0FFFFh,    9900h  ;PgUp
        DW        8E00h,    4A00h  ;Pad-minus
        DW       0FFFFh,    9B00h  ;Left
        DW        8F00h,    9C00h  ;Pad-5
        DW       0FFFFh,    9D00h  ;Right
        DW        9000h,    4E00h  ;Pad-Plus
        DW       0FFFFh,    9F00h  ;End
        DW        9100h,   0A000h  ;Down
        DW       0FFFFh,   0A100h  ;PgDn
        DW        9200h,   0A200h  ;Insert
        DW        9300h,   0A300h  ;Del

;put scan word in AX into keyboard buffer
;input:    AX = key
;changes:  AX,BX,CX
;          ints turned off and left off
StoreKeyAX      PROC NEAR
        PUSH    DS
        MOV     BX,BiosDataSele
        MOV     DS,BX
        AND     BYTE PTR DS:[0096h],0FDh;Clear E0h bit !!.30
        MOV     CX,AX                   ;Save scan word in CX
        CLI                             ;Stop CPU interrupts
        MOV     BX,DS:BufferTail        ;Point to end of keyboard buffer
        MOV     AX,BX                   ;Transfer to AX
        ADD     AX,0002                 ;Advance to next position
        CMP     AX,DS:BufferEnd         ;Wrap around if needed
        JNE     CheckFull               ;No need to wrap
        MOV     AX,DS:BufferStart       ;Else to beginning of buffer
CheckFull:
        CMP     AX,DS:BufferHead        ;Bumping into start?
        JE      StoreKeyAxDone          ;Exit if full
        MOV     WORD PTR [BX],CX        ;Store keystroke
        MOV     DS:BufferTail,AX        ;Advance tail
StoreKeyAXDone:
        POP     DS
        RET
StoreKeyAX      ENDP

;*********************************************************** NewInt09
;Handle hardware keyboard interrupts on a non-enhanced keyboard

NewInt09        PROC FAR

        STI                             ;Interrupts on
        PUSH    AX                      ;Save registers we use
        PUSH    BX
        PUSH    CX
        PUSH    DS

        MOV     AX,SEG DATA
        MOV     DS,AX                   ;Point to Turbo data area
        CMP     EnableEnhanced,0        ;Enhanced functions enabled?
        JZ      Int09Orig               ;No, get out

        PUSH    DS                                                      ;!!.20
        MOV     AX,BiosDataSele                                         ;!!.20
        MOV     DS,AX                   ;Point to BIOS data area
        MOV     AL,DS:BiosShiftFlags    ;Shift status in AL
        POP     DS                                                      ;!!.20

        TEST    AL,00001111b            ;Any shift bits set?
        JNZ     SkipReleased            ;Skip released check if so
        MOV     ShiftReleased,1         ;All shift keys released
SkipReleased:                           ;
        CMP     ShiftReleased,1         ;Have all shifts been released?
        JNZ     Int09Orig               ;Get out if not

        MOV     CL,AL                   ;Save shift state in CL
        TEST    AL,00001100b            ;Either Control or Alt depressed?
        JZ      TestPad5                ;No, check special case of Pad-5

        AND     AL,00001100b            ;Just Ctrl-Alt bits
        CMP     AL,00001100b            ;Both Ctrl and Alt depressed?
        JE      Int09Orig               ;Get out in case of Ctrl-Alt-Del

        MOV     AL,CL                   ;Restore shift state
        AND     AL,00001111b            ;All shift bits
        CMP     AL,00001010b            ;Just Alt-LeftShift?
        JE      Int09Orig               ;Get out so Alt-Keypad works

        IN      AL,60h                  ;Read scan code
        CMP     AL,53h                  ;Is it in range 0..53h?
        JA      Int09Orig               ;If not, pass on to BIOS int 09 handler

        MOV     BX,offset IndexTable    ;Point to index table
        XLAT    BYTE PTR CS:[0]         ;Get index
        CMP     AL,0FFh                 ;AL = FFh?
        JNZ     MatchedScan             ;Special case if AL <> FFh

Int09Orig:                              ;Let BIOS int 09 handler take care of it
        PUSHF                                                            ;!!.20
        CLI                                                              ;!!.20
        CALL    DWORD PTR DS:PrevInt09  ;Transfer to old interrupt 09    ;!!.20
        POP     DS                      ;Restore registers               ;!!.20
        POP     CX                                                       ;!!.20
        POP     BX                                                       ;!!.20
        POP     AX                                                       ;!!.20
        IRET                                                             ;!!.20

TestPad5:
        IN      AL,60h                  ;Read scan code
        CMP     AL,4Ch                  ;Pad-5 key?
        JNZ     Int09Orig               ;No, get out
        MOV     AX,4C00h                ;Set up scan code to return
        TEST    CL,00100000b            ;Is NumLock set?
        JZ      NoNumLock               ;No, it's not
NumLockSet:
        TEST    CL,00000011b            ;Is a shift key depressed?
        JNZ     StoreScanWord           ;Yes, we want to store 4C00 in buffer
        JMP SHORT Int09Orig             ;No, let original int 9 do it
NoNumLock:
        TEST    CL,00000011b            ;Is a shift key depressed?
        JZ      StoreScanWord           ;No, store scan word
        JMP SHORT Int09Orig             ;Yes, let original int 9 do it

MatchedScan:
        XOR     AH,AH                   ;make sure AH is 0               !!.20
        SHL     AX,1                    ;Multiply scan index by 2
        SHL     AX,1                    ;Multiply by 2 again
        MOV     BX,AX                   ;BX = Index * 4
        TEST    CL,00001000b            ;Alt depressed?
        JZ      NoAltKey                ;No, use first column of table
        ADD     BX,2                    ;Yes, use second column of table
NoAltKey:
        MOV     AX,CS:WordTable[BX]     ;Get the scan word
        CMP     AX,0FFFFh               ;Case handled by BIOS?
        JZ      Int09Orig               ;Yes, get out of here

StoreScanWord:
        CALL    StoreKeyAX              ;Put AX into keyboard buffer

NewInt09Done:
        IN      AL,61h                  ;Read control port value
        MOV     AH,AL                   ;Save in AH
        OR      AL,80h                  ;Set high bit
        OUT     61h,AL                  ;Reset keyboard
        MOV     AL,AH                   ;Retrieve original value
        OUT     61h,AL                  ;Enable keyboard
        CLI                             ;Assure ints off before signaling EOI
        MOV     AL,20h                  ;End of interrupt
        OUT     20h,AL                  ;To the interrupt controller
        POP     DS                      ;Restore registers
        POP     CX
        POP     BX
        POP     AX
        IRET                            ;Return to caller

NewInt09        ENDP

;*********************************************************** EnhInt09
;Handle hardware keyboard interrupts on an enhanced keyboard

EnhInt09        PROC FAR

        STI                             ;Interrupts on
        PUSH    AX                      ;Save registers we use
        PUSH    BX
        PUSH    CX
        PUSH    DS

        MOV     AX,SEG DATA
        MOV     DS,AX                   ;Point to Turbo data area
        CMP     EnableEnhanced,0        ;Enhanced functions enabled?
        JZ      Enh09Orig               ;No, get out

        PUSH    DS                                                      ;!!.20
        MOV     AX,BiosDataSele                                         ;!!.20
        MOV     DS,AX                   ;Point to BIOS data area
        MOV     AL,DS:BiosShiftFlags    ;Shift status in AL
        POP     DS                                                      ;!!.20

        TEST    AL,00001111b            ;Any shift bits set?
        JNZ     EnhSkipReleased         ;Skip released check if so
        MOV     ShiftReleased,1         ;All shift keys released
EnhSkipReleased:                        ;
        CMP     ShiftReleased,1         ;Have all shifts been released?
        JNZ     Enh09Orig               ;Get out if not

        AND     AL,00001111b            ;Clear all but shift bits !!.13
        CMP     AL,00001000b            ;Only Alt set?
        JNZ     Enh09Orig               ;No, get out

        IN      AL,60h                  ;Read scan code
        CMP     AL,53h                  ;Is it in range 37h..53h?
        JA      Enh09Orig               ;If not, pass on to BIOS int 09 handler
        CMP     AL,37h
        JB      Enh09Orig               ;If not, pass on to BIOS int 09 handler

        MOV     BX,offset IndexTable    ;Point to index table
        XLAT    BYTE PTR CS:[0]         ;Get index
        CMP     AL,0FFh                 ;AL = FFh?
        JNZ     EnhMatchedScan          ;Special case if AL <> FFh

Enh09Orig:                              ;Let BIOS int 09 handler take care of it
        PUSHF                                                             ;!!.20
        CLI                                                               ;!!.20
        CALL    DWORD PTR DS:PrevInt09  ;Transfer to old interrupt 09     ;!!.20
        POP     DS                      ;Restore registers                ;!!.20
        POP     CX                                                        ;!!.20
        POP     BX                                                        ;!!.20
        POP     AX                                                        ;!!.20
        IRET                                                              ;!!.20

EnhMatchedScan:
        XOR     AH,AH                   ;Make sure AH is 0                 !!.20
        SHL     AX,1                    ;Multiply scan index by 2
        SHL     AX,1                    ;Multiply by 2 again
        INC     AX
        INC     AX                      ;Use second table column (Alt pressed)
        MOV     BX,AX                   ;BX = Index * 4
        MOV     AX,CS:WordTable[BX]     ;Get the scan word
        CMP     AX,0FFFFh               ;Case handled by BIOS?
        JZ      Enh09Orig               ;Yes, get out of here

        CALL    StoreKeyAX              ;Put AX into keyboard buffer
        JMP     NewInt09Done            ;Gobble scan and return

EnhInt09        ENDP

;*********************************************************** NewInt16

;Handle software keyboard interrupts
ZFMask  EQU     0040h                   ;Bit for ZF in flags register

NewInt16        PROC FAR

        PUSH    BX                      ;Save registers we use
        PUSH    DS
        MOV     BX,SEG DATA
        MOV     DS,BX                   ;Assure DS points to Turbo data
        MOV     BX,SP                   ;Set up stack frame
        STI                             ;Allow interrupts

        CMP     EnableEnhanced,0        ;Enhanced functions enabled?
        JZ      Int16Orig               ;No, pass to previous int 16
        CMP     HasEnhancedKbd,0        ;Is enhanced keyboard installed?
        JNZ     UseThisInt16            ;Yes, use remainder of this int 16
        CMP     FiltersEnhanced,0       ;Will BIOS try to filter extra keys?
        JZ      Int16Orig               ;No, leave call alone

UseThisInt16:
        OR      AH,AH                   ;AH=0?
        JNZ     TryAH1                  ;No, try AH=1
AH0:
        OR      AH,10h                  ;Use enhanced BIOS call instead
        PUSHF
        CALL    DS:PrevInt16            ;Call previous int 16
FixAL:
        OR      AH,AH                   ;AH=0?
        JZ      AH0Done                 ;Yes, done
        CMP     AL,0E0h                 ;AL=E0h?
        JNZ     AH0Done                 ;No, leave it alone
        XOR     AL,AL                   ;Clear low byte of scan word
AH0Done:
        POP     DS                      ;Restore registers
        POP     BX
        IRET                            ;Return to caller

TryAH1:
        CMP     AH,1                    ;AH=1?
        JNZ     Int16Orig               ;No, let original int 16 do it
AH1:
        OR      WORD PTR SS:[BX+8],ZFMask ;Assume no key, set ZF in flags
        OR      AH,10h                  ;Use enhanced BIOS call instead
        PUSHF
        CALL    DS:PrevInt16            ;Call previous int 16
        JZ      AH0Done                 ;Just return if no key available
        AND     WORD PTR SS:[BX+8],not ZFMask ;Clear ZF in flags
        JMP     FixAL                   ;Fix up special codes and return

Int16Orig:
        PUSHF                                                          ;!!.20
        CALL    PrevInt16               ;Let old int16 handle the rest ;!!.20
        JZ      ZFSet                   ;Transfer ZF into returned flags ;!!.21
        AND     WORD PTR SS:[BX+8],not ZFMask                          ;!!.21
        JMP     AH0Done                                                ;!!.21
ZFSet:  OR      WORD PTR SS:[BX+8],ZFMask                              ;!!.21
        JMP     AH0Done                                                ;!!.20

NewInt16        ENDP

;*********************************************************** FixOldBios
;Correct invalid values of BufferStart and BufferEnd
;which aren't initialized by some old BIOS versions
;!!.11 rewritten

FixOldBios proc far
        push    ds                      ;Save DS
        mov     ax,BiosDataSele                                      ;!!.20
        mov     ds,ax                   ;DS = BIOS data segment
        mov     cx,DS:BufferStart       ;CX = BufferStart
        mov     dx,DS:BufferEnd         ;DX = BufferEnd
        cmp     cx,dx                   ;Start < End?
        jae     FixIt                   ;Fix if not
        mov     ax,dx
        sub     ax,cx                   ;AX = size of buffer
        cmp     ax,1000                 ;Size <= 1000?
        ja      FixIt                   ;Fix if not
        mov     ax,DS:BufferTail        ;AX = BufferTail
        mov     bx,DS:BufferHead        ;BX = BufferHead
        cmp     cx,ax                   ;Start <= Tail?
        ja      FixIt                   ;Fix if not
        cmp     cx,bx                   ;Start <= Head?
        ja      FixIt
        cmp     dx,ax                   ;End >= Tail?
        jb      FixIt                   ;Fix if not
        cmp     dx,bx                   ;End >= Head?
        jae     FxDone                  ;Done if so
FixIt:  mov     ax,001Eh                ;Reset all keyboard offsets to defaults
        mov     DS:BufferStart,ax
        mov     DS:BufferTail,ax
        mov     DS:BufferHead,ax
        mov     DS:BufferEnd,003Eh
FxDone: pop     ds
        ret
FixOldBios endp

CODE    ENDS
        END
