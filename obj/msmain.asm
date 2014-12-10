;******************************************************
;                  MSMAIN.ASM 1.30
;     Copyright (c) TurboPower Software 1989, 1992.
;                All rights reserved.
;******************************************************

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

        PUBLIC  GetPicture

;****************************************************** Pictures

;packed table of sample picture masks

PictureCount    =       29

Pictures        LABEL BYTE

;      Picture     Characters Allowed
db 25,"XXXXXXXXXX  Any character"
db 43,"!!!!!!!!!!  Any character, force upper case"
db 43,"LLLLLLLLLL  Any character, force lower case"
db 43,"xxxxxxxxxx  Any character, force mixed case"
db 66,"aaaaaaaaaa  Alphas only (international alphas, ' ', '-', '.', ',')"
db 35,"AAAAAAAAAA  Alphas only, upper case"
db 35,"llllllllll  Alphas only, lower case"
db 35,"9999999999  Numbers ('0'..'9', ' ')"
db 44,"##########  Digits ('0'..'9', ' ', '-', '.')"
db 63," #####.##   Spaces on sides, use for 'Parens with minus' option"
db 57,"$######.##  Floating dollar, fixed decimal ('$123456.78')"
db 47,"c######.##  Fixed currency, left ('$   123.45')"
db 48,"#####.##CC  Fixed currency, right ('12345,67 F')"
db 60,"$##,###.##  Discretionary comma ('$12,345.67', '   $123.45')"
db 63,"EEEEEEEEEE  Scientific ('0'..'9', ' ', '-', '+', '.', 'e', 'E')"
db 59,"KKKKKKKKKK  Hexadecimal ('0'..'9', ' ', 'A'..'F', 'a'..'f')"
db 37,"B           Boolean ('T','t','F','f')"
db 36,"Y           Yes-no ('Y','y','N','n')"
db 44,"mm/dd/yyyy  Date, zero padded ('04/01/1989')"
db 45,"MM/DD/yyyy  Date, blank padded (' 4/ 1/1989')"
db 43,"dd/nnn/yy   Date, named month ('01/Apr/89')"
db 55,"dd/NNN/yy   Date, named month, upper case ('01/APR/89')"
db 42,"hh:mm:ss    Time, zero padded ('06:10:20')"
db 43,"HH:mm:ss    Time, blank padded (' 6:10:20')"
db 41,"hh:mmt      Time, am/pm format ('12:00p')"
db 43,"hh:mm te    Time, am/pm format ('12:00 pm')"
db 59,"0000000000  User-defined picture mask characters ('0'..'8')"
db 06,"   ..."
db 10,"8888888888"

;*************************************************** GetPicture

;function GetPicture(B : Byte) : StringPtr;

PicCode         EQU     BYTE PTR SS:[BX+4]

GetPicture      PROC FAR

        MOV     BX,SP                   ;Set BX to point to stack
        MOV     DX,DS                   ;Save DS

        MOV     SI,offset Pictures      ;CS:SI points to Pictures
        XOR     CH,CH
        MOV     CL,PicCode              ;Get picture mask code

        DEC     CL                      ;Decrement index
        JCXZ    gpDone                  ;Done if zero

        MOV     AX,CS
        MOV     DS,AX                   ;DS:SI points to Keys
        CLD                             ;Forward direction
        XOR     AH,AH                   ;Clear top half of length word

gpNext:
        LODSB                           ;Length byte into AL
        ADD     SI,AX                   ;Skip over string
        LOOP    gpNext                  ;Repeat for number of characters

gpDone:
        MOV     DS,DX                   ;Restore DS
        MOV     DX,CS                   ;Pointer Segment is current CS
        MOV     AX,SI                   ;SI points to length byte of string
        RET     2

GetPicture      ENDP

CODE    ENDS

        END
