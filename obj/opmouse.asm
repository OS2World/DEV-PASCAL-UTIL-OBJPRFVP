;******************************************************
;                  OPMOUSE.ASM 1.30
;              Mouse support routines
;     Copyright (c) TurboPower Software 1988, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT BYTE PUBLIC

        ;Pascal variables

        EXTRN   ScreenHeight : BYTE     ;Current height of display
        EXTRN   ScreenWidth : BYTE      ;Current width of display

        EXTRN   MouseInstalled : BYTE   ;set to True if mouse installed
        EXTRN   MouseCursorOn : BYTE    ;keeps tracks of mouse cursor visibility
        EXTRN   MouseButtons : BYTE     ;number of mouse buttons

        EXTRN   MouseXLo : BYTE         ;coordinates for mouse window
        EXTRN   MouseXHi : BYTE
        EXTRN   MouseYLo : BYTE
        EXTRN   MouseYHi : BYTE

        EXTRN   MouseRoutine : DWORD    ;user's event handler
        EXTRN   MouseRoutineEvent : BYTE
        EXTRN   EventHandlerInstalled : BYTE

        EXTRN   MouseEvent : BYTE       ;last mouse event
        EXTRN   MouseStatus : BYTE      ;last button status
        EXTRN   MouseLastX : BYTE       ;last horizontal coordinate
        EXTRN   MouseLastY : BYTE       ;last vertical coordinate

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  InitializeMouse, ShowMouseLow, HideMouseLow, MouseWindow
        PUBLIC  GetStorageSize, SaveMouseStatePrim, RestoreMouseStatePrim
        PUBLIC  ScaleUpY, ScaleUpX, ScaleDownY, ScaleDownX

;****************************************************** Macros

MouseCall       MACRO   Function
                MOV     AX,Function     ;function code into AX
                INT     33h             ;call the mouse driver
                ENDM

CheckMouse      MACRO   PopCount
                LOCAL OK
                CMP     MouseInstalled,True
                JE      OK
                XOR     AX,AX           ;return 0 in AX
                RET     PopCount        ;return
        OK:
                ENDM

;****************************************************** InitializeMouse

;procedure InitializeMouse;

;Reinitializes mouse and sets MouseInstalled.

InitializeMouse PROC FAR

        MOV     MouseCursorOn,False     ;reset cursor flag
        MOV     MouseInstalled,False    ;assume failure
        MOV     EventHandlerInstalled,False     ;no event handler installed
        MOV     MouseButtons,0          ;0 mouse buttons if not installed
        MOV     MouseRoutine.Segm,0     ;reset our event variables
        MOV     MouseRoutine.Ofst,0
        MOV     MouseRoutineEvent,0
        MOV     MouseEvent,0
        MOV     MouseStatus,0

        MOV     AX,3533h                ;Get INT 33 vector
        INT     21h                     ;call DOS
        MOV     AX,ES                   ;is vector nil?
        OR      BX,AX
        JZ      MIexit                  ;if so, no mouse

        MouseCall 0                     ;Mouse Installed Flag and Reset function
        CMP     AX,-1                   ;function returns 0 if not installed, else -1
        JNE     MIexit

        INC     MouseInstalled          ;it's installed
        MOV     MouseButtons,BL         ;button count in BL

MIexit:
        RET

InitializeMouse ENDP

;****************************************************** ShowMouseLow

;procedure ShowMouseLow;
;Show the mouse cursor.

ShowMouseLow    PROC FAR

        CheckMouse 0            ;make sure mouse is installed
        MOV     MouseCursorOn,True
        MouseCall 1             ;Show Cursor function
        RET

ShowMouseLow    ENDP

;****************************************************** HideMouseLow

;procedure HideMouseLow;
;Hide the mouse cursor.

HideMouseLow    PROC FAR

        MOV     MouseCursorOn,False
        CheckMouse 0            ;make sure mouse is installed
        MouseCall 2             ;Hide Cursor function
        RET

HideMouseLow    ENDP

;****************************************************** ScaleUpY

;on entry, BL has a screen row coordinate to be scaled into the mouse's
;  coordinate system
;on exit, AX has the result

ScaleUpY        PROC NEAR

        DEC     BL              ;convert to 0-based number
        MOV     AL,8            ;get scaling factor into AL
        MUL     BL              ;multiply by coordinate in BL
        RET

ScaleUpY        ENDP

;****************************************************** ScaleUpX

;on entry, BL has a screen column coordinate to be scaled into the mouse's
;  coordinate system
;on exit, AX has the result

ScaleUpX        PROC NEAR

        DEC     BL              ;convert to 0-based number
        MOV     AL,8            ;get scaling factor into AL
        CMP     ScreenWidth,80
        JAE     suxGo
        MOV     AL,16
suxGo:
        MUL     BL              ;multiply by coordinate in BL
        RET

        RET

ScaleUpX        ENDP

;****************************************************** ScaleDownY

;on entry, AX has a number in the mouse's coordinate system that needs to be
;  scaled down to a screen row coordinate
;on exit, AL has the result

ScaleDownY      PROC NEAR

        PUSH    BX              ;save BX
        CMP     AX,0            ;check for > 0
        JG      sdyOK
        MOV     AL,MouseYLo     ;force it to a valid range
        JMP     SHORT sdyExit
sdyOK:
        MOV     BL,8            ;get scaling factor into BL
        DIV     BL              ;divide coordinate by scaling factor
sdyExit:
        SUB     AL,MouseYLo     ;make it relative to current window
        INC     AL              ;convert to 1-based number
        POP     BX              ;restore BX
        RET

ScaleDownY      ENDP

;****************************************************** ScaleDownX

;on entry, AX has a number in the mouse's coordinate system that needs to be
;  scaled down to a screen column coordinate
;on exit, AL has the result

ScaleDownX      PROC NEAR

        PUSH    BX              ;save BX
        CMP     AX,0            ;check for > 0
        JG      sdxOK
        MOV     AL,MouseXLo     ;force it to a valid range
        JMP     SHORT sdxExit
sdxOK:
        MOV     BL,8            ;get scaling factor into BL
        CMP     ScreenWidth,80
        JAE     sdxGo
        MOV     BL,16
sdxGo:
        DIV     BL              ;divide coordinate by scaling factor
sdxExit:
        SUB     AL,MouseXLo     ;make it relative to current window
        INC     AL              ;convert to 1-based number
        POP     BX              ;restore BX
        RET

ScaleDownX      ENDP

;****************************************************** MouseWindow

;procedure MouseWindow(XLow, YLow, XHigh, YHigh : Byte);
;Sets window coordinates to be observed by the mouse

XLo     EQU BYTE PTR [BP+12]
YLo     EQU BYTE PTR [BP+10]
XHi     EQU BYTE PTR [BP+8]
YHi     EQU BYTE PTR [BP+6]

MouseWindow     PROC FAR

        CheckMouse 8            ;make sure mouse is installed
        StackFrameBP            ;set up stack frame

        ;validate all parameters BEFORE setting any window coordinates
        MOV     BL,XLo          ;BL = XLo-1
        DEC     BL
        MOV     BH,XHi          ;BH = XHi-1
        DEC     BH
        CMP     BL,BH           ;XLo > XHi?
        JA      mwDone
        CMP     BH,ScreenWidth  ;XHi > ScreenWidth?
        JAE     mwDone
        MOV     AL,YLo          ;AL = YLo-1
        DEC     AL
        MOV     AH,YHi          ;AH = YHi-1
        DEC     AH
        CMP     AL,AH           ;YLo > YHi?
        JA      mwDone
        CMP     AH,ScreenHeight ;YHi > ScreenHeight?
        JAE     mwDone

        MOV     MouseXLo,BL     ;save 0-based coordinates for mouse window
        MOV     MouseYLo,AL

        INC     BH              ;save 1-based coordinates for mouse window
        MOV     MouseXHi,BH
        INC     AH
        MOV     MouseYHi,AH

        INC     BL              ;BL = XLo
        CALL    ScaleUpX        ;scale the XLow param (in BL) and put in CX
        MOV     CX,AX
        MOV     BL,XHi          ;scale the XHigh param and put in DX
        CALL    ScaleUpX
        MOV     DX,AX
        MouseCall 7             ;Set Minimum and Maximum Horizontal Position

        MOV     BL,YLo          ;scale the YLow param and put in CX
        CALL    ScaleUpY
        MOV     CX,AX
        MOV     BL,YHi          ;scale the YHigh param and put in DX
        CALL    ScaleUpY
        MOV     DX,AX
        MouseCall 8             ;Set Minimum and Maximum Vertical Position
mwDone:
        Exit_Code 8             ;!!.13

MouseWindow     ENDP

;****************************************************** GetStorageSize

;function GetStorageSize : Word;
;Returns amount of memory needed to save state of mouse driver

GetStorageSize  PROC FAR

        CheckMouse 0            ;make sure mouse is installed
        SetZero BX              ;set BX to 0
        MouseCall 21            ;Query Save-State Storage Size
        MOV     AX,BX           ;size returned in BX
        RET

GetStorageSize  ENDP

;****************************************************** SaveMouseStatePrim

;procedure SaveMouseStatePrim(var Buffer);

;Save mouse state in Buffer

Buffer  EQU DWORD PTR [BP+6]

SaveMouseStatePrim      PROC FAR

        CheckMouse 4            ;make sure mouse is installed
        StackFrameBP            ;set up stack frame
        LES     DX,Buffer       ;ES:DX points to buffer
        MouseCall 22            ;Save Mouse Driver State
        Exit_Code 4             ;!!.13

SaveMouseStatePrim      ENDP

;****************************************************** RestoreMouseStatePrim

;procedure RestoreMouseStatePrim(var Buffer);

;Restore mouse state from Buffer

RestoreMouseStatePrim   PROC FAR

        CheckMouse 4            ;make sure mouse is installed
        StackFrameBP            ;set up stack frame
        LES     DX,Buffer       ;ES:DX points to buffer
        MouseCall 23            ;Restore Mouse Driver State
        Exit_Code 4             ;!!.13

RestoreMouseStatePrim   ENDP

CODE    ENDS
        END
