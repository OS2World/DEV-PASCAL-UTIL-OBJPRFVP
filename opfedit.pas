{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPFEDIT.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1988,1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpFEdit;
  {-Field editors }

interface

uses
  Use32,
  Dpmi,       {!!.20}
  OpConst,    {!!.20}
  {$IFDEF VIRTUALPASCAL}
  VpSysLow,
  VPUtils,
  {$ENDIF}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  {$IFDEF UseDates}
  OpDate,
  {$ENDIF}
  {$IFDEF UseBcd}
  OpBcd,
  {$ENDIF}
  OpCmd,
  OpSelect;  {!!.30}

type
  ClearFirstFunc = function(var Cmd, Key : Word) : Boolean;

{.Z+}

procedure StringEditor(var Field; Row, Col : Word; FA, CA : Byte;
                       PasswordChar : Char; PosCode : Byte; ReadOnly : Boolean;
                       var CC : Word; var ChWord : Word;
                       var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                       UnitCode : Byte; var CP : CommandProcessor);
 {-For editing strings}

procedure NumberEditor(var Field; Row, Col : Word; FA, CA : Byte;
                       PasswordChar : Char; PosCode : Byte; ReadOnly : Boolean;
                       var CC : Word; var ChWord : Word;
                       var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                       UnitCode : Byte; var CP : CommandProcessor);
 {-For editing numbers (right to left)}

procedure CharEditor(var Field; Row, Col : Word; FA, CA : Byte;
                     PasswordChar : Char; PosCode : Byte; ReadOnly : Boolean;
                     var CC : Word; var ChWord : Word;
                     var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                     UnitCode : Byte; var CP : CommandProcessor);
 {-For editing character fields}

procedure SimpleStringEditor(var Field; Row, Col : Word; FA, CA : Byte;
                             PasswordChar : Char; PosCode : Byte; ReadOnly : Boolean;
                             var CC : Word; var ChWord : Word;
                             var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                             UnitCode : Byte; var CP : CommandProcessor);
  {-Simple string editor -- ignores picture mask}

procedure SimpleCharEditor(var Field; Row, Col : Word; FA, CA : Byte;
                           PasswordChar : Char; PosCode : Byte; ReadOnly : Boolean;
                           var CC : Word; var ChWord : Word;
                           var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                           UnitCode : Byte; var CP : CommandProcessor);
  {-Simpler char editor -- ignores picture mask}

{$IFNDEF UseCalcEdit} {!!.13}
  procedure CalcEditor(var Field; Row, Col : Word; FA, CA : Byte;
                       PasswordChar : Char; PosCode : Byte;
                       ReadOnly : Boolean; var CC, ChWord : Word;
                       var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                       UnitCode : Byte; var CP : CommandProcessor);
    {-Edit a string in calculator fashion}
{$ENDIF}

{--- miscellaneous routines intended for internal use ---}

procedure ToggleInsertMode(var InsertMode : Boolean);
  {-Toggle between insert and overtype mode, keeping BIOS keyboard flag up
    to date}

function DefClearFirstFunc(var Cmd, Key : Word) : Boolean;
  {-Default ClearFirstFunc}

var
  feClearFirstFunc : ClearFirstFunc;

{.Z-}

  {==========================================================================}

implementation

uses
  OpAbsFld,
  OpField;

const
  NumLockBit = $20;
  InsertBit  = $80;

{$IFNDEF VirtualPascal}
var
  KeyboardFlags : ^Word; {absolute $40 : $17;}                        {!!.20}
{$ENDIF}

  procedure ToggleInsertMode(var InsertMode : Boolean);
    {-Toggle between insert and overtype mode, keeping BIOS keyboard flag up
      to date}
  begin
    {toggle insert flag}
    InsertMode := not InsertMode;

    {use fat cursor if inserting}
    if InsertMode then begin
      SetCursorType(InsertCursor);
      {$IFDEF VIRTUALPASCAL}
      SetKeyboardState( kbd_Insert, True );
      {$ELSE}
      SetFlag(KeyboardFlags^, InsertBit);                              {!!.20}
      {$ENDIF}
    end
    else begin
      SetCursorType(OvertypeCursor);
      {$IFDEF VIRTUALPASCAL}
      SetKeyboardState( kbd_Insert, False );
      {$ELSE}
      ClearFlag(KeyboardFlags^, InsertBit);                            {!!.20}
      {$ENDIF}
    end;
  end;

  function DefClearFirstFunc(var Cmd, Key : Word) : Boolean;
    {-Default ClearFirstFunc}
  begin
    DefClearFirstFunc := (Cmd = ccChar);
  end;

  procedure SwapStrings(var S1, S2 : string);
    {-Swap two strings}
  begin
    ExchangeStructs(S1, S2, MaxWord(Length(S1), Length(S2))+1);
  end;

  {$I OPFEDIT.IN1}  {simple editors}

{$IFDEF UseCalcEdit} {!!.13}
  procedure NumberEditor(
{$ELSE}
  procedure CalcEditor(
{$ENDIF}
                         var Field; Row, Col : Word; FA, CA : Byte;
                         PasswordChar : Char; PosCode : Byte;
                         ReadOnly : Boolean; var CC, ChWord : Word;
                         var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                         UnitCode : Byte; var CP : CommandProcessor);
    {-Edit a string in calculator fashion}    {!!.13} {new}
  var
    EF : EntryField absolute Field;
    CursorSL : Word;
    SaveBreak : Boolean;
    SaveInsertMode : Boolean;
    SaveNumLock : Boolean;
    ErrCode : Word;
    ErrorMsg : StringPtr;
    FirstChar : Boolean;
    Finished : Boolean;
    PicChar : Char;
    Ch : Char absolute ChWord;
    StBgn, StEnd : Byte;
    St : string;
    StLen : Byte absolute St;
    StPtr : StringPtr;
    SaveEditSt : string;
    Tmp : string;
    SP : Byte;
    MaxLen, MaxDigits : Word;
    DotPos : Byte;
    Places : Word;
    HaveMinus : Boolean;
    MinusPos : Byte absolute HaveMinus;
    PFlags : PictureFlags;
    FirstTime : Boolean;

    procedure InitVars;
      {-Do all one-time initialization stuff}
    var
      I : Word;
    begin
      with EF do begin
        HaveMinus := False;
        DotPos := Pos(DecimalPt, efPicture^);
        CalcWidthAndPlaces(MaxLen, Places);
        MaxDigits := MaxLen;
        if Places <> 0 then
          Dec(MaxDigits, Places+1);

        {set up flags array}
        InitPictureFlags(PFlags);
        for I := 1 to Length(efPicture^) do
          if efPicture^[I] = FloatDollar then
            PFlags[I] := True;

        StBgn := 0;
        StEnd := 0;
        for I := 1 to Length(efPicture^) do
          if PFlags[I] then begin
            if StBgn = 0 then
              StBgn := I;
            StEnd := I;
          end;
        if DotPos <> 0 then
          PFlags[DotPos] := True;
      end;
    end;

    procedure FixString(var St : string);
      {-Fix up the string we're editing to make it look right}
    var
      StLen : Byte absolute St;
    begin
      {trim blanks}
      TrimSpacesPrim(St);

      {if string is empty, put in a 0}
      if StLen = 0 then begin
        StLen := 1;
        St[1] := '0';
      end
      else if (St[1] = '.') and (StLen < MaxLen) then {!!.22}
        Insert('0', St, 1);

      {prepend the minus sign}
      if HaveMinus then
        Insert('-', St, 1);
    end;

    procedure Adjust;
      {-Adjust display string to show correct number of decimal places}
    var
      StLen : Byte absolute St;
      Delta, ActPlaces : Integer;
      DotPosSt : Word;
      ExDec : string;
    begin
      ActPlaces := 0;
      DotPosSt := Pos(DecimalPt, St);
      if DotPosSt = 0 then
        Delta := Places+1
      else begin
        ActPlaces := StLen-DotPosSt;
        Delta := Places-ActPlaces;
      end;

      if Delta = 0 then
        Exit;

      if Delta > 0 then begin
        Delete(Tmp, StEnd-Pred(Delta), Delta);
        Insert(CharStr(' ', Delta), Tmp, StBgn);
      end
      else begin
        Delta := -Delta;
        ExDec := Copy(St, DotPosSt+Places+1, Delta);
        Delete(Tmp, StBgn, Delta);
        Insert(ExDec, Tmp, StEnd-Pred(Delta));
      end;
    end;

    procedure Redraw;
      {-Position cursor and redraw string}
    begin
      with EF do begin
        {merge the string we're editing with the picture}
        Tmp := St;
        FixString(Tmp);
        MergePicture(Tmp, Tmp);

        {adjust display string to show correct number of decimal places}
        if DotPos <> 0 then
          Adjust;

        {draw the result and position the cursor}
        Draw(Tmp, Row, Col, FA, FA, 0, PasswordChar, PFlags);
        GoToXYabs(Col+Pred(StEnd), Row);
      end;
    end;

    procedure ReInit(Reload : Boolean);
      {-Load the default string for editing}
    var
      I, J : Word;
      NeedToTrim : Boolean;
    begin
      {strip the picture}
      if Reload then
        EF.StripPicture(EF.efEditSt^, St)
      else
        EF.StripPicture(St, St);
      TrimSpacesPrim(St);

      {remove the minus sign if there is one}
      HaveMinus := Pos('-', St) = 1;
      if HaveMinus then
        Delete(St, 1, 1);

      {we want a blank string if it's a zero}
      if (StLen = 1) and (St[1] = '0') then
        StLen := 0;

      {SP will have first available space for new characters}
      SP := MaxLen-StLen;

      {pad the left side with blanks}
      St := LeftPad(St, MaxLen);
    end;

    procedure CheckAutoAdvance;
      {-See if we need to auto-advance to next/previous field}
    begin
      if not Finished then
        {advance if the string is full}
        if (SP = MinusPos) and LongFlagIsSet(EF.sfOptions, efAutoAdvCharEnd) then begin
          CC := ccAutoAdvance;
          Finished := True;
        end;
    end;

    procedure ClearString;
      {-Clear the input string}
    begin
      FillChar(St[1], MaxLen, ' ');
      SP := MaxLen;
      HaveMinus := False;
    end;

    function DigitCount : Word;
      {-Return number of digits to left of decimal place in St}
    var
      S : string;
      SLen : Byte absolute S;
      I : Word;
    begin
      S := St;
      TrimSpacesPrim(S);
      I := Pos(DecimalPt, S);
      if I <> 0 then
        SLen := Pred(I);
      DigitCount := SLen+Ord(HaveMinus);
    end;

    function InsertChar : Boolean;
      {-Insert Ch}
    var
      DP : Word;
    begin
      InsertChar := False;

      {make sure it's OK}
      if (Ch = ' ') then
        Exit;

      {OK to add decimal point?}
      {if FirstChar then}          {!!.22}
        {DP := 0}                  {!!.22}
      {else}                       {!!.22}
      DP := Pos(DecimalPt, St);
      if Ch = DecimalChar then     {!!.20}
        Ch := DecimalPt;           {!!.20}
      if not FirstChar and (Ch = DecimalPt) then {!!.30}
        if (DotPos = 0) or (DP <> 0) then
          Exit;

      {clear the string if it's the first valid char}
      if FirstChar then begin
        FirstChar := False;
        if LongFlagIsSet(EF.sfOptions, efClearFirstChar) then
          {clear the input string}
          ClearString;
      end;

      if (Ch = '-') then begin
        {minus sign treated as toggle}
        if HaveMinus then
          HaveMinus := False
        else
          HaveMinus := (DigitCount < MaxDigits) and (SP > 0);
      end
      else if (SP > MinusPos) then begin
        {don't allow initial zeros except for hex strings}
        if (Ch = '0') and (SP = MaxLen) then
          if not LongFlagIsSet(EF.sfFlags, ifHexadecimal) then begin
            InsertChar := True;
            Exit;
          end;

        {check for too many digits to left of decimal point}
        if (DotPos <> 0) and (Ch <> DecimalPt) then
          if (DP = 0) and (DigitCount >= MaxDigits) then
            Exit;

        {move everything to the left and append the char}
        Delete(St, 1, 1);
        Inc(StLen);
        St[StLen] := Ch;
        Dec(SP);
      end
      else if (MaxLen = 1) then
        if (Ch <> DecimalPt) then begin
          {overwrite the last character}
          St[MaxLen] := Ch;
          SP := 0;
        end
        else
          Exit
      else
        Exit;

      CheckAutoAdvance;
      InsertChar := True;
    end;

    procedure RoundResult(var S : string);
      {-Round the result as necessary}
    var
      SLen : Byte absolute S;
      I, DP : Word;

      function IncDigit(var Ch : Char) : Boolean;
      begin
        IncDigit := (Ch = '9');
        if Ch = '9' then
          Ch := '0'
        else
          Inc(Ch);
      end;

    begin
      DP := Pos(DecimalPt, S);
      I := DP+Places;
      if (DotPos = 0) or (DP = 0) or (I >= SLen) then
        Exit;

      if S[I+1] >= '5' then begin
        if HaveMinus then begin
          Delete(S, 1, 1);
          Dec(I);
        end;
        while (I > 0) and IncDigit(S[I]) do begin
          Dec(I);
          if I = 0 then
            Insert('1', S, 1)
          else if (S[I] = DecimalPt) then
            Dec(I);
        end;
        if HaveMinus then
          Insert('-', S, 1);
      end;
    end;

  begin

    with EF do begin
      {we're editing}
      SetLongFlag(sfFlags, ifEditing);
      StPtr := @Tmp;

      {save copy of efEditSt^}
      SaveEditSt := efEditSt^;

      {store cursor shape}
      CursorSL := CursorTypeSL;

      {save NumLock state and force it on}
      if LongFlagIsSet(sfOptions, efAutoNumLock) then begin
        {$IFDEF VIRTUALPASCAL}
        SaveNumLock := GetKeyboardState( kbd_Numlock );
        SetKeyboardState( kbd_Numlock, True );
        {$ELSE}
        SaveNumLock := FlagIsSet(KeyboardFlags^, NumLockBit);          {!!.20}
        SetFlag(KeyboardFlags^, NumLockBit);                           {!!.20}
        {$ENDIF}
      end;

      {save break checking state}
      SaveBreak := CheckBreak;
      CheckBreak := False;

      {initialize}
      InitVars;
      ReInit(True);
      PicChar := efPicture^[StEnd];
      sfFCPos := StEnd;

      {set insert mode}
      if LongFlagIsSet(sfOptions, efForceMode) then begin
        SaveInsertMode := InsertMode;
        InsertMode := LongFlagIsSet(sfOptions, efForceOvertype);
      end
      else
        InsertMode := not InsertMode;
      ToggleInsertMode(InsertMode);

      {clear modified flag}
      ClearLongFlag(sfFlags, ifModified);

      {loop reading keys}
      Finished := False;
      FirstChar := True;
      FirstTime := (CC <> ccNone);
      repeat
        {redraw string}
        Redraw;

        {get next command and validate it}
        if FirstTime then
          FirstTime := False
        else begin
          ExchangeLongInts(LongInt(StPtr), LongInt(efEditSt));
          CC := CP.GetCommand(ChWord);
          ExchangeLongInts(LongInt(StPtr), LongInt(efEditSt));
        end;

        {don't allow ccIncChoice, ccDecChoice, or ccToggle}
        case CC of
          ccToggle, ccIncChoice, ccDecChoice :
            if Lo(ChWord) = 0 then
              CC := ccNone
            else
              CC := ccChar;
        end;

        if ReadOnly then begin
          if (CC <= 255) and not(CC in ReadOnlyCommands) then
            CC := ccNone;
        end
        else if (CC <= 255) and not(CC in NumberCommands) then
          CC := ccNone;

        {allow editing of the existing string}
        if FirstChar then
          FirstChar := feClearFirstFunc(CC, ChWord);
        case CC of
          ccNone :
            if LongFlagIsSet(sfOptions, efBeepOnError) then
              RingBell;

          ccChar :
            begin
              {if Ch = DecimalChar then}         {!!.20}
                {Ch := DecimalPt;}               {!!.20}
              if not CharOK(PicChar, Ch, #255, True) then begin
                FirstChar := False;
                if LongFlagIsSet(sfOptions, efBeepOnError) then
                  RingBell;
              end
              else if not InsertChar then
                if LongFlagIsSet(sfOptions, efBeepOnError) then
                  RingBell;
            end;

          ccMouseAuto, ccAltKey, ccMouseDown, ccTab, ccWordRight, ccRight,
          ccLeft, ccWordLeft, ccBackTab, ccNextField, ccAutoAdvance, ccSelect,
          ccPrevField, ccUp, ccDown, ccNextRec, ccPrevRec, ccFirstFld,
          ccMouseSel, ccLastFld, ccPageUp, ccPageDn, ccNested, ccDone,
          ccUser0..ccUser65335 :
            Finished := True;

          ccRestore, ccQuit :
            begin
              ReInit(True);
              Finished := (CC = ccQuit);
            end;

          ccDelLine :
            ClearString;

          ccDel, ccBack :
            if (SP < MaxLen) then begin
              {remove the last character}
              Dec(StLen);
              Insert(' ', St, 1);
              Inc(SP);

              {if all that's left is a 0, remove it}
              if (SP = MaxLen-1) and (St[MaxLen] = '0') then begin
                St[MaxLen] := ' ';
                SP := MaxLen;
              end;
            end
            else
              {delete the minus sign, if there is one}
              HaveMinus := False;

          ccIns :
            if not LongFlagIsSet(sfOptions, efForceMode) then
              ToggleInsertMode(InsertMode);

          ccHelp :
            CP.cpGetHelp(UnitCode, nil, sfHelpIndex);
        end;

        {make sure it's OK to go to next/previous field}
        if Finished then
          Finished := efOKtoAdvance(CC);

        {validate the entry if done}
        if Finished then begin
          {make sure the screen is up to date}
          Redraw;

          if (CC <> ccQuit) and (not ReadOnly) then begin
            {swap strings for validation purposes}
            SwapStrings(St, efEditSt^);
            FixString(efEditSt^);
            RoundResult(efEditSt^);
            MergePicture(efEditSt^, efEditSt^);

            ErrorMsg := @emNullError;
            if not Validate(ErrCode, ErrorMsg) then begin
              {not done yet--swap back}
              Finished := False;
              SwapStrings(St, efEditSt^);
              Reinit(False);
              FirstChar := True;

              {display error message from validation routine}
              Inc(ErrCode, epWarning);
              ErrorRoutine(UnitCode, ErrCode, ErrorMsg^);
            end;
          end;
        end;
      until Finished;

      {set modified flag}
      if (CC <> ccQuit) and (efEditSt^ <> SaveEditSt) then
        SetLongFlag(sfFlags, ifModified);

      {restore insert mode if it was forced one way or the other}
      if LongFlagIsSet(sfOptions, efForceMode) then
        InsertMode := SaveInsertMode;

      {restore break checking status}
      CheckBreak := SaveBreak;

      {restore cursor shape}
      SetCursorSize(Hi(CursorSL), Lo(CursorSL));

      if LongFlagIsSet(sfOptions, efAutoNumLock) then
        {restore previous NumLock state}
        if SaveNumLock then
          {$IFDEF VIRTUALPASCAL}
          SetKeyboardState( kbd_Numlock, True )
          {$ELSE}
          SetFlag(KeyboardFlags^, NumLockBit)                         {!!.20}
          {$ENDIF}
        else
          {$IFDEF VIRTUALPASCAL}
          SetKeyboardState( kbd_Numlock, False );
          {$ELSE}
          ClearFlag(KeyboardFlags^, NumLockBit);                      {!!.20}
          {$ENDIF}

      {we're not editing}
      ClearLongFlag(sfFlags, ifEditing);
    end;
  end;

{$IFNDEF UseCalcEdit} {!!.13}

  procedure NumberEditor(var Field; Row, Col : Word; FA, CA : Byte;
                         PasswordChar : Char; PosCode : Byte;
                         ReadOnly : Boolean; var CC, ChWord : Word;
                         var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                         UnitCode : Byte; var CP : CommandProcessor);
    {-Edit a string}
  var
    EF : EntryField absolute Field;
    CursorSL : Word;
    SaveBreak : Boolean;
    SaveInsertMode : Boolean;
    SaveNumLock : Boolean;
    ErrCode : Word;
    ErrorMsg : StringPtr;
    FirstChar : Boolean;
    Finished : Boolean;
    PicChar : Char;
    Ch : Char absolute ChWord;
    StEnd : Byte;
    St : string;
    StLen : Byte absolute St;
    StPtr : StringPtr;
    SaveEditSt : string;
    Tmp : string;          {!!.11}
    SP : Byte;
    MaxLen : Word;
    DotPos : Byte;
    Places : Word;
    HaveMinus : Boolean;
    MinusPos : Byte absolute HaveMinus;
    PFlags : PictureFlags;
    FirstTime : Boolean;

    procedure InitVars;
      {-Do all one-time initialization stuff}
    var
      I : Word;
    begin
      with EF do begin
        HaveMinus := False;
        DotPos := Pos(DecimalPt, efPicture^);
        CalcWidthAndPlaces(MaxLen, Places);
        if DotPos <> 0 then
          Dec(MaxLen);

        {set up flags array}
        InitPictureFlags(PFlags);
        {if DotPos <> 0 then}                 {!!.13}
          {PFlags[DotPos] := True;}           {!!.13}
        StEnd := 0;
        for I := 1 to Length(efPicture^) do
          if PFlags[I] then
            StEnd := I;
        if DotPos <> 0 then                   {!!.13}
          PFlags[DotPos] := True;             {!!.13}
      end;
    end;

    procedure FixString(var St : string);
      {-Fix up the string we're editing to make it look right}
    var
      StLen : Byte absolute St;
      I, J : Word;
    begin
      if (DotPos <> 0) then begin
        {fill in 0's as necessary}
        J := StLen;
        for I := 1 to Succ(Places) do begin
          if St[J] = ' ' then
            St[J] := '0';
          Dec(J);
        end;

        {insert the decimal point}
        Insert('.', St, StLen-Pred(Places));
      end;

      {trim blanks}
      TrimSpacesPrim(St);

      {if string is empty, put in a 0}
      if StLen = 0 then begin
        StLen := 1;
        St[1] := '0';
      end;

      {prepend the minus sign}
      if HaveMinus then
        Insert('-', St, 1);
    end;

    procedure Redraw;
      {-Position cursor and redraw string}
    {var}                         {!!.11}
      {Tmp : String;}             {!!.11}
    begin
      with EF do begin
        {merge the string we're editing with the picture}
        Tmp := St;
        FixString(Tmp);
        MergePicture(Tmp, Tmp);

        {draw the result and position the cursor}
        Draw(Tmp, Row, Col, FA, FA, 0, PasswordChar, PFlags);
        GoToXYabs(Col+Pred(StEnd), Row);
      end;
    end;

    procedure ReInit(Reload : Boolean);
      {-Load the default string for editing}
    var
      I, J : Word;
      NeedToTrim : Boolean;
    begin
      {strip the picture}
      if Reload then
        EF.StripPicture(EF.efEditSt^, St)
      else
        EF.StripPicture(St, St);
      TrimSpacesPrim(St);

      {trim trailing zeros if the value is 0}
      I := Pos('.', St);
      if I <> 0 then begin
        NeedToTrim := True;
        for J := Succ(I) to Length(St) do
          case St[J] of
            '1'..'9' : NeedToTrim := False;
          end;
        for J := Pred(I) downto 1 do
          case St[J] of
            '1'..'9' : NeedToTrim := False;
          end;
        if NeedToTrim then
          TrimTrailingZeros(St);
      end
      else if LongFlagIsSet(EF.sfFlags, ifHexadecimal) then
        TrimLeadingZeros(St);

      {remove the minus sign if there is one}
      HaveMinus := Pos('-', St) = 1;
      if HaveMinus then
        Delete(St, 1, 1);

      {remove the decimal point if there is one}
      if DotPos <> 0 then begin
        I := Pos('.', St);
        if I <> 0 then
          Delete(St, I, 1);
      end;

      {we want a blank string if it's a zero}
      if (StLen = 1) and (St[1] = '0') then
        StLen := 0;

      {trim leading zeroes}                   {!!.11}
      while (StLen > 0) and (St[1] = '0') do  {!!.11}
        Delete(St, 1, 1);                     {!!.11}

      {SP will have first available space for new characters}
      SP := MaxLen-StLen;

      {pad the left side with blanks}
      St := LeftPad(St, MaxLen);
    end;

    procedure CheckAutoAdvance;
      {-See if we need to auto-advance to next/previous field}
    begin
      if not Finished then
        {advance if the string is full}
        if (SP = MinusPos) and LongFlagIsSet(EF.sfOptions, efAutoAdvCharEnd) then begin
          CC := ccAutoAdvance;
          Finished := True;
        end;
    end;

    procedure ClearString;
      {-Clear the input string}
    begin
      FillChar(St[1], MaxLen, ' ');
      SP := MaxLen;
      HaveMinus := False;
    end;

    function InsertChar : Boolean;
      {-Insert Ch}
    begin
      InsertChar := False;

      {make sure it's OK}
      if (Ch = ' ') then
        Exit;

      {clear the string if it's the first valid char}
      if FirstChar then begin
        FirstChar := False;
        if LongFlagIsSet(EF.sfOptions, efClearFirstChar) then
          {clear the input string}
          ClearString;
      end;

      if (Ch = '-') then begin
        {minus sign treated as toggle}
        if HaveMinus then
          HaveMinus := False
        else
          HaveMinus := (SP > 0);
      end
      else if (SP > MinusPos) then begin
        {don't add initial zeros}
        if (Ch = '0') then
          if (SP = MaxLen) then begin
            InsertChar := True;
            Exit;
          end;

        {decimal point is special case}
        if (Ch = DecimalChar) then
          {eat the decimal point}
          Exit;

        {move everything to the left and append the char}
        Delete(St, 1, 1);
        Inc(StLen);
        St[StLen] := Ch;
        Dec(SP);
      end
      else if (MaxLen = 1) then
        if (Ch <> '0') and (Ch <> '.') then begin
          {overwrite the last character}
          St[MaxLen] := Ch;
          if MaxLen = 1 then
            SP := 0;
        end
        else
          Exit
      else
        Exit;

      CheckAutoAdvance;
      InsertChar := True;
    end;

  begin

    with EF do begin
      {we're editing}
      SetLongFlag(sfFlags, ifEditing);
      StPtr := @Tmp {@St};                  {!!.11}

      {save copy of efEditSt^}
      SaveEditSt := efEditSt^;

      {store cursor shape}
      CursorSL := CursorTypeSL;

      {save NumLock state and force it on}
      if LongFlagIsSet(sfOptions, efAutoNumLock) then begin
        {$IFDEF VIRTUALPASCAL}
        SaveNumLock := GetKeyboardState( kbd_Numlock );
        SetKeyboardState( kbd_Numlock, True );
        {$ELSE}
        SaveNumLock := FlagIsSet(KeyboardFlags^, NumLockBit);           {!!.20}
        SetFlag(KeyboardFlags^, NumLockBit);                            {!!.20}
        {$ENDIF}
      end;

      {save break checking state}
      SaveBreak := CheckBreak;
      CheckBreak := False;

      {initialize}
      InitVars;
      ReInit(True);
      PicChar := efPicture^[StEnd];
      sfFCPos := StEnd;

      {set insert mode}
      if LongFlagIsSet(sfOptions, efForceMode) then begin
        SaveInsertMode := InsertMode;
        InsertMode := LongFlagIsSet(sfOptions, efForceOvertype);
      end
      else
        InsertMode := not InsertMode;
      ToggleInsertMode(InsertMode);

      {clear modified flag}
      ClearLongFlag(sfFlags, ifModified);

      {loop reading keys}
      Finished := False;
      FirstChar := True;
      FirstTime := (CC <> ccNone);
      repeat
        {redraw string}
        Redraw;

        {get next command and validate it}
        if FirstTime then
          FirstTime := False
        else begin
          ExchangeLongInts(LongInt(StPtr), LongInt(efEditSt));
          CC := CP.GetCommand(ChWord);
          ExchangeLongInts(LongInt(StPtr), LongInt(efEditSt));
        end;

        {don't allow ccIncChoice, ccDecChoice, or ccToggle}
        case CC of
          ccToggle,            {!!.03}
          ccIncChoice,
          ccDecChoice :
            if Lo(ChWord) = 0 then
              CC := ccNone
            else
              CC := ccChar;
        end;

        if ReadOnly then begin
          if (CC <= 255) and not(CC in ReadOnlyCommands) then
            CC := ccNone;
        end
        else if (CC <= 255) and not(CC in NumberCommands) then
          CC := ccNone;

        {allow editing of the existing string}
        if FirstChar then
          FirstChar := feClearFirstFunc(CC, ChWord);
        case CC of
          ccNone :
            if LongFlagIsSet(sfOptions, efBeepOnError) then
              RingBell;

          ccChar :
            if CharOK(PicChar, Ch, #255, True) then begin
              if not InsertChar and LongFlagIsSet(sfOptions, efBeepOnError) then
                RingBell;
            end
            else begin
              FirstChar := False;
              if LongFlagIsSet(sfOptions, efBeepOnError) then
                RingBell;
            end;

          ccMouseAuto,           {!!.11}
          ccAltKey, ccMouseDown, {!!.03}
          ccTab, ccWordRight, ccRight, ccLeft, ccWordLeft, ccBackTab,
          ccNextField, ccAutoAdvance, ccSelect, ccPrevField, ccUp, ccDown,
          ccNextRec, ccPrevRec, ccFirstFld, ccMouseSel, ccLastFld, ccPageUp,
          ccPageDn, ccNested, ccDone, ccUser0..ccUser65335 :
            Finished := True;

          ccRestore, ccQuit :
            begin
              ReInit(True);
              Finished := (CC = ccQuit);
            end;

          ccDelLine :
            ClearString;

          ccDel, ccBack :
            if (SP < MaxLen) then begin
              {remove the last character}
              Dec(StLen);
              Insert(' ', St, 1);
              Inc(SP);

              {if all that's left is a 0, remove it}
              if (SP = MaxLen-1) and (St[MaxLen] = '0') then begin
                St[MaxLen] := ' ';
                SP := MaxLen;
              end;
            end
            else
              {delete the minus sign, if there is one}
              HaveMinus := False;

          ccIns :
            if not LongFlagIsSet(sfOptions, efForceMode) then
              ToggleInsertMode(InsertMode);

          ccHelp :
            CP.cpGetHelp(UnitCode, nil, sfHelpIndex);
        end;

        {make sure it's OK to go to next/previous field}
        if Finished then
          Finished := efOKtoAdvance(CC);

        {validate the entry if done}
        if Finished then begin
          {make sure the screen is up to date}
          Redraw;

          if (CC <> ccQuit) and (not ReadOnly) then begin
            {swap strings for validation purposes}
            SwapStrings(St, efEditSt^);
            FixString(efEditSt^);
            MergePicture(efEditSt^, efEditSt^);

            ErrorMsg := @emNullError;
            if not Validate(ErrCode, ErrorMsg) then begin
              {not done yet--swap back}
              Finished := False;
              SwapStrings(St, efEditSt^);
              Reinit(False);
              FirstChar := True;

              {display error message from validation routine}
              Inc(ErrCode, epWarning);
              ErrorRoutine(UnitCode, ErrCode, ErrorMsg^);
            end;
          end;
        end;
      until Finished;

      {set modified flag}
      if (CC <> ccQuit) and (efEditSt^ <> SaveEditSt) then
        SetLongFlag(sfFlags, ifModified);

      {restore insert mode if it was forced one way or the other}
      if LongFlagIsSet(sfOptions, efForceMode) then
        InsertMode := SaveInsertMode;

      {restore break checking status}
      CheckBreak := SaveBreak;

      {restore cursor shape}
      SetCursorSize(Hi(CursorSL), Lo(CursorSL));

      if LongFlagIsSet(sfOptions, efAutoNumLock) then
        {restore previous NumLock state}
        if SaveNumLock then
          {$IFDEF VIRTUALPASCAL}
          SetKeyboardState( kbd_Numlock, True )
          {$ELSE}
          SetFlag(KeyboardFlags^, NumLockBit)                         {!!.20}
          {$ENDIF}
        else
          {$IFDEF VIRTUALPASCAL}
          SetKeyboardState( kbd_Numlock, False );
          {$ELSE}
          ClearFlag(KeyboardFlags^, NumLockBit);                      {!!.20}
          {$ENDIF}

      {we're not editing}
      ClearLongFlag(sfFlags, ifEditing);
    end;
  end;

{$ENDIF} {!!.13}

  procedure StringEditor(var Field; Row, Col : Word; FA, CA : Byte;
                         PasswordChar : Char; PosCode : Byte;
                         ReadOnly : Boolean; var CC, ChWord : Word;
                         var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                         UnitCode : Byte; var CP : CommandProcessor);
    {-Edit a string}
  var
    EF : EntryField absolute Field;
    PFlags : PictureFlags;
    Ch : Char absolute ChWord;
    St : string;
    STmp : string;
    StPtr : StringPtr;
    StLen : Byte;
    StBgn : Byte;
    StEnd : Byte;
    CursorSL : Word;
    {POffset : Integer;} {!!.20}
    Delta : Integer;
    SP, I : Byte;
    LastSP : Byte;
    FirstChar : Boolean;
    SaveBreak : Boolean;
    Finished : Boolean;
    MaxLen : Byte;
    SaveInsertMode : Boolean;
    DotPos : Byte;
    ErrCode : Word;
    SaveNumLock : Boolean;
    ErrorMsg : StringPtr;
    IsNumber : Boolean;
    DefPic : Char;
    IsNested : Boolean;
    FirstTime : Boolean;
    SaveModified : Boolean; {!!.11}

    procedure CalcBeginEnd;
      {-Calculate values for StBgn/StEnd}
    var
      I, J : Word;
    begin
      IsNumber := DotPos <> 0;
      Delta := 0;
      for I := 1 to MaxLen do
        case EF.efPicture^[I] of
          FloatDollar :
            begin
              PFlags[I] := True;
              IsNumber := True;
              Inc(Delta);
            end;
          Comma :
            begin
              PFlags[I] := True;
              IsNumber := True;
              Inc(Delta);
            end;
          else
            DefPic := EF.efPicture^[I];
        end;
      StBgn := 0;
      StEnd := 0;
      for I := 1 to MaxLen do
        if PFlags[I] then begin
          if StBgn = 0 then
            StBgn := I;
          StEnd := I;
        end;
      Inc(StBgn, Delta);
    end;

    procedure CheckAutoAdvance;
      {-See if we need to auto-advance to next/previous field}
    begin
      if not Finished then
        if (SP < StBgn) and LongFlagIsSet(EF.sfOptions, efAutoAdvCurBegin) then
          Finished := True
        else if (SP > StEnd) then
          if (CC = ccChar) and LongFlagIsSet(EF.sfOptions, efAutoAdvCharEnd) then begin
            CC := ccAutoAdvance;
            Finished := True;
          end
          else if (CC <> ccChar) and LongFlagIsSet(EF.sfOptions, efAutoAdvCurEnd) then
            Finished := True;
    end;

    procedure CalcStLen;
      {-Calculate length of St}
    var
      I : Word;
    begin
      I := StEnd;
      while ((St[I] = ' ') or not PFlags[I]) and (I <> 0) do
        Dec(I);
      StLen := I;
    end;

    procedure HomeCommand;
      {-Move the cursor to the start of the line}
    begin
      EF.efHOffset := 0;   {!!.20}
      SP := StBgn;
    end;

    procedure EndCommand;
      {-Move the cursor to the end of the line}
    begin
      SP := Succ(StLen);
      if SP > StEnd then
        SP := StEnd;
      while (not PFlags[SP]) and (SP < StEnd) do
        Inc(SP);
    end;

    procedure RePad;
      {-Pad to the end of the string with blanks}
    var
      I : Word;
    begin
      for I := SP to StEnd do
        if PFlags[I] then
          St[I] := ' ';
      CalcStLen;
    end;

    procedure ClearString;
      {-Clear the string being edited}
    begin
      HomeCommand;
      LastSP := SP;  {!!.02}
      RePad;
    end;

    procedure AdvanceCursor;
      {-Advance cursor one position}
    begin
      Inc(SP);
      if SP < StEnd then
        while not PFlags[SP] do
          Inc(SP);
    end;

    function StartOfSubField : Byte;
      {-Find the start of the current subfield}
    var
      I : Word;
    begin
      I := SP;
      while (I > StBgn) and PFlags[I-1] do
        Dec(I);
      StartOfSubField := I;
    end;

    function EndOfSubField : Byte;
      {-Find the end of the current subfield}
    var
      I : Word;
    begin
      I := SP;
      while (I < MaxLen) and PFlags[I+1] do
        Inc(I);
      EndOfSubField := I;
    end;

    procedure InsertChar;
      {-Insert Ch at St[SP]}
    begin
      if not LongFlagIsSet(EF.sfOptions, efInsertPushes) then
        if St[EndOfSubField] <> ' ' then begin
          if LongFlagIsSet(EF.sfOptions, efBeepOnError) then
            RingBell;
          Exit;
        end;

      if PFlags[SP+1] then begin
        Delete(St, EndOfSubField, 1);
        Insert(Ch, St, SP);
      end
      else
        St[SP] := Ch;
      CalcStLen;
      AdvanceCursor;
    end;

    procedure DeleteChar;
      {-Delete char at St[SP]}
    begin
      Delete(St, SP, 1);
      Insert(' ', St, EndOfSubField);
      CalcStLen;
    end;

    procedure DeleteWord;
      {-Delete the word to the right of the cursor}
    var
      I, J : Word;
    begin
      {delete all of the current word, if any}
      J := EndOfSubField;
      while St[SP] <> ' ' do begin
        Delete(St, SP, 1);
        Insert(' ', St, J);
      end;

      {delete any spaces prior to the next word, if any}
      I := SP;
      while (I <= J) and (St[I] = ' ') do
        Inc(I);
      if I < J then
        while St[SP] = ' ' do begin
          Delete(St, SP, 1);
          Insert(' ', St, J);
        end;

      CalcStLen;
    end;

    procedure WordRight;
      {-Cursor right one word}
    var
      I : Word;
    begin
      if (SP < StLen) then begin
        Inc(SP);
        I := EndOfSubField;
        while (SP <= I) and (St[SP] <> ' ') do
          Inc(SP);
        while (SP <= I) and (St[SP] = ' ') do
          Inc(SP);
        if SP < StEnd then begin
          while not PFlags[SP] do
            Inc(SP);
        end
        else
          SP := StLen+1;
      end
      else begin
        SP := Succ(EndOfSubField);
        if (SP > StEnd) then begin
          if LongFlagIsSet(EF.sfOptions, efAutoAdvCurEnd) then
            CheckAutoAdvance;
        end
        else
          while not PFlags[SP] do
            Inc(SP);
      end;
    end;

    procedure WordLeft;
      {-Cursor left one word}
    var
      I : Word;
    begin
      if (SP > StBgn) then begin
        Dec(SP);
        if not PFlags[SP] then
          while not PFlags[SP] do
            Dec(SP);
        I := StartOfSubField;
        while (SP >= I) and ((SP > StLen) or (St[SP] = ' ')) do
          Dec(SP);
        while (SP >= I) and (St[SP] <> ' ') do
          Dec(SP);
        Inc(SP);
      end
      else if LongFlagIsSet(EF.sfOptions, efAutoAdvCurBegin) then begin
        SP := 0;
        CheckAutoAdvance;
      end;
    end;

    procedure DeleteToEnd;
      {-Delete from cursor to end of subfield}
    var
      I : Word;
    begin
      I := EndOfSubField;
      if SP = I then
        Exit;
      Delete(St, SP, Succ(I-SP));
      Insert(CharStr(' ', Succ(I-SP)), St, SP);
      CalcStLen;
    end;

    procedure DeleteToBegin;
      {-Delete from beginning of subfield to the cursor}
    var
      I, J : Word;
    begin
      I := StartOfSubField;
      if SP = I then
        Exit;
      J := EndOfSubField;
      Delete(St, I, SP-I);
      Insert(CharStr(' ', SP-I), St, J-Pred(SP-I));
      SP := I;
      CalcStLen;
    end;

    procedure DoTab;
      {-Move cursor to start of next subfield}
    var
      I : Word;
    begin
      I := EndOfSubField;
      if I = StEnd then begin
        SP := I;
        Finished := EF.efOKtoAdvance(ccTab);
      end
      else begin
        SP := I+1;
        while not PFlags[SP] do
          Inc(SP);
      end;
    end;

    procedure DoBackTab;
      {-Move cursor to start of previous subfield}
    var
      I : Word;
    begin
      I := StartOfSubField;
      if I = StBgn then begin
        SP := I;
        Finished := True;
      end
      else begin
        SP := I-1;
        while not PFlags[SP] do
          Dec(SP);
        SP := StartOfSubField;
      end;
    end;

    procedure FixPOffset;
      {-Adjust SP and efHOffset if necessary} {!!.20}
    begin
      if (SP > StEnd) then
        SP := StEnd
      else if (SP < StBgn) then
        SP := StBgn;
      if (SP > EF.sfFWidth+EF.efHOffset) then    {!!.20}
        EF.efHOffset := Integer(SP)-EF.sfFWidth  {!!.20}
      else if (SP < Succ(EF.efHOffset)) then     {!!.20}
        EF.efHOffset := Integer(SP)-1;           {!!.20}
      if EF.efHOffset < 0 then                   {!!.20}
        EF.efHOffset := 0;                       {!!.20}
    end;

    procedure Redraw(MoveCursor : Boolean);
      {-Position cursor and redraw string}
    begin
      with EF do begin
        FixPOffset;
        Draw(St, Row, Col, FA, CA, efHOffset, PasswordChar, PFlags); {!!.20}
        if MoveCursor then
          GoToXYAbs(Col+Pred(SP)-efHOffset, Row);                    {!!.20}
        sfFCPos := SP-efHOffset;                                     {!!.20}
      end;
    end;

    procedure FixNumber(FirstHalf, SecondHalf : Boolean);
      {-Fix the first and/or second half of a numeric field}
    var
      I, J, K : Word;
      SaveSP : Byte;
    begin
      {Ch := ' ';} {!!.10}
      SaveSP := SP;
      if FirstHalf then begin
        {bring numbers to left of decimal flush right}
        if DotPos = 0 then
          SP := StEnd
        else
          SP := DotPos-1;
        K := EndOfSubField;
        J := StartOfSubField-Delta;
        I := J;
        while St[I] = ' ' do
          Inc(I);
        while I <= K do begin
          if St[I] = ' ' then begin
            Delete(St, I, 1);
            Insert(' ', St, J);
          end;
          Inc(I);
        end;

        {make sure it isn't all blanks to left of decimal}
        if St[K] = ' ' then
          St[K] := '0';
      end;

      if (DotPos <> 0) and SecondHalf then begin
        SP := DotPos+1;
        {bring numbers to right of decimal flush left}
        J := EndOfSubField;
        if SP <= J then begin   {!!.11}
          K := J;               {!!.11}
          J := StartOfSubField;
          I := K;
          while St[I] = ' ' do begin
            St[I] := '0';
            Dec(I);
          end;
          while I >= J do begin
            if St[I] = ' ' then begin
              Delete(St, I, 1);
              Insert('0', St, K);
            end;
            Dec(I);
          end;
        end;
      end;

      SP := SaveSP;
      CalcStLen;
    end;

    procedure CheckFirstChar;
      {-Check first char flag}
    begin
      if FirstChar then begin
        FirstChar := False;
        if LongFlagIsSet(EF.sfOptions, efClearFirstChar) then
          {clear the input string}
          ClearString;
      end;
    end;

    procedure ReloadEditSt;
      {-Reload the original string}
    var
      I : Word;
      Ch : Char;
    begin
      St := EF.efEditSt^;
      if IsNumber then begin
        if Pos(FloatDollar, EF.efPicture^) <> 0 then begin
          I := Pos(CurrencyLtStr, St);
          if (I <> 0) and PFlags[I] then
            FillChar(St[I], Length(CurrencyLtStr), ' ');
        end;
        for I := 1 to Length(St) do
          if (St[I] = CommaChar) then
            St[I] := ' ';
        FixNumber(True, True);
      end
      else
        CalcStLen;
      STmp := St;
    end;

    function CharIsOK : Boolean;
      {-Return true if Ch can be added to the string}
    var
      PicChar : Char;
      PrevCh : Char;
    begin
      PicChar := EF.efPicture^[SP];
      case PicChar of
        Comma,
        FloatDollar : PicChar := DefPic;
      end;
      if SP = StartOfSubField then
        PrevCh := ' '
      else if FirstChar and LongFlagIsSet(EF.sfOptions, efClearFirstChar) then {!!.03}
        PrevCh := ' '                                                          {!!.03}
      else                                                                     {!!.03}
        PrevCh := St[Sp-1];
      CharIsOK := CharOK(PicChar, Ch, PrevCh, True);
    end;

    function LeaveBlank : Boolean; {!!.02} {new routine}
      {-Return True}
    var
      SLB : string;
    begin
      LeaveBlank := False;
      with EF do
        if DotPos <> 0 then begin
          StripPicture(St, SLB);
          FixRealPrim(SLB, False);
          LeaveBlank := (SLB = '');
        end;
    end;

  begin
    with EF do begin
      {we're editing}
      SetLongFlag(sfFlags, ifEditing);
      StPtr := @St;

      {get maximum length of string}
      MaxLen := Length(efPicture^);

      {get position of decimal point, if any}
      DotPos := Pos(DecimalPt, efPicture^);

      {store cursor shape}
      CursorSL := CursorTypeSL;

      {save NumLock state and force it on}
      if LongFlagIsSet(sfOptions, efAutoNumLock) then begin
        {$IFDEF VIRTUALPASCAL}
        SaveNumLock := GetKeyboardState( kbd_Numlock );
        SetKeyboardState( kbd_Numlock, True );
        {$ELSE}
        SaveNumLock := FlagIsSet(KeyboardFlags^, NumLockBit);          {!!.20}
        SetFlag(KeyboardFlags^, NumLockBit);                           {!!.20}
        {$ENDIF}
      end;

      {save break checking state}
      SaveBreak := CheckBreak;
      CheckBreak := False;

      {initialize}
      if PosCode <> 4 then                                             {!!.20}
        efHOffset := 0;                                                {!!.20}
      InitPictureFlags(PFlags);
      IsNested := LongFlagIsSet(sfFlags, ifNested);

      {get the default string and calculate begin/end of line}
      CalcBeginEnd;
      ReloadEditSt;

      {position the cursor}
      case PosCode of
        0 :                    {normal}
          if LongFlagIsSet(sfOptions, efCursorToEnd) then
            EndCommand
          else
            HomeCommand;
        1 :                    {force edge of field}
          if StEnd > sfFWidth then begin
            SP := sfFWidth;
            if not PFlags[SP] then
              if SP < StBgn then
                SP := StBgn
              else while not PFlags[SP] do
                Inc(SP);
          end
          else
            SP := StEnd;
        2 :                    {force start of last subfield}
          begin
            SP := StEnd;
            SP := StartOfSubfield;
            if (StLen >= SP) and LongFlagIsSet(sfOptions, efCursorToEnd) then {!!.03}
              EndCommand;                                                     {!!.03}
          end;
        3 :
          begin
            SP := StEnd;
            if SP <> StartOfSubfield then {!!.01}
              WordLeft;
          end;
        4 :
          if sfFWidth = efMaxLen then begin          {!!.20}
            efHOffset := 0;                          {!!.20}
            Sp := sfFCPos;
          end                                        {!!.20}
          else
            Sp := sfFCPos+efHOffset;                 {!!.20}
      end;
      LastSP := SP;

      {set insert mode}
      if LongFlagIsSet(sfOptions, efForceMode) then begin
        SaveInsertMode := InsertMode;
        InsertMode := LongFlagIsSet(sfOptions, efForceOvertype);
      end
      else
        InsertMode := not InsertMode;
      ToggleInsertMode(InsertMode);

      {clear modified flag}
      ClearLongFlag(sfFlags, ifModified);

      {loop reading keys}
      Finished := False;
      FirstChar := True {(PosCode <> 4)} ;  {!!.13}
      FirstTime := (CC <> ccNone);
      repeat
        if DotPos <> 0 then begin
          {see if we need to fix a number}
          if (LastSP < DotPos) and (SP > DotPos) then
            FixNumber(True, False)
          else if (LastSP > DotPos) and (SP < DotPos) then
            FixNumber(False, True);
        end;

        {position cursor and redraw string}
        LastSP := SP;
        Redraw(True);

        {get next command and validate it}
        if FirstTime then
          FirstTime := False
        else begin
          ExchangeLongInts(LongInt(StPtr), LongInt(efEditSt));
          CC := CP.GetCommand(ChWord);
          ExchangeLongInts(LongInt(StPtr), LongInt(efEditSt));
        end;

        {don't allow ccIncChoice, ccDecChoice, or ccToggle}
        case CC of
          ccToggle,            {!!.03}
          ccIncChoice, ccDecChoice :
            if Lo(ChWord) = 0 then
              CC := ccNone
            else
              CC := ccChar;
        end;

        if ReadOnly or IsNested then begin
          if (CC <= 255) and not(CC in ReadOnlyCommands) then
            CC := ccNone;
        end
        else if (CC <= 255) and not(CC in StringCommands) then
          CC := ccNone;

        {deal with control characters if desired}
        if CC = ccCtrlChar then
          {don't allow control chars if attributes same and mapping on}
          if (CA = FA) and LongFlagIsSet(sfOptions, efMapCtrls) then
            CC := ccNone
          else begin
            SetCursorType(CtrlCharCursor);
            ChWord := CP.cpGetKey;
            CC := ccChar;
            if InsertMode then
              SetCursorType(InsertCursor)
            else
              SetCursorType(OvertypeCursor);
          end;

        {allow editing of the existing string}
        if FirstChar then
          FirstChar := feClearFirstFunc(CC, ChWord);
        case CC of
          ccNone :
            if LongFlagIsSet(sfOptions, efBeepOnError) then
              RingBell;

          ccChar :
            if (DotPos <> 0) and (Ch = DecimalChar) then begin
              CheckFirstChar;
              FixNumber(True, False);
              SP := DotPos+1;
            end
            else if CharIsOK then begin
              CheckFirstChar;
              if InsertMode then
                {insert mode}
                InsertChar
              else if SP <= StEnd then begin
                {overtype mode}
                St[SP] := Ch;
                if SP > StLen then
                  StLen := SP;
                AdvanceCursor;
              end;
              CheckAutoAdvance;
            end
            else begin
              FirstChar := False;
              if LongFlagIsSet(sfOptions, efBeepOnError) then
                RingBell;
            end;

          ccTab :
            DoTab;

          ccBackTab :
            DoBackTab;

          ccSelect {, ccNextField} : {!!.03}
            begin
              if IsNested then
                CC := ccNested;
              Finished := True;
            end;

          {$IFDEF UseMouse}
          ccMouseAuto, ccMouseDown, ccMouseSel : {!!.20} {this section is new}
            if IsNested or LongFlagIsSet(sfOptions, efMultChoice+efClickExit) then
              Finished := True
            else begin
              I := efMousePos(Row, Col);
              if I = 0 then
                Finished := True
              else if CC <> ccMouseAuto then begin
                SP := I;
                if SP < StBgn then
                  SP := StBgn
                else if SP > StEnd then
                  SP := StEnd
                else
                  while not PFlags[SP] do
                    Dec(SP);
              end;
            end;
          {$ENDIF}

          {ccMouseAuto,}           {!!.20}
          ccNextField, ccAltKey, {ccMouseDown,} {!!.03} {!!.20}
          ccAutoAdvance, ccPrevField, ccUp, ccDown, ccNextRec, ccPrevRec,
          ccFirstFld, {ccMouseSel,} ccLastFld, ccPageUp, ccPageDn, ccNested, {!!.20}
          ccDone, ccUser0..ccUser65335 :
            Finished := True;

          ccRestore, ccQuit :
            begin
              ReloadEditSt;
              if LongFlagIsSet(sfOptions, efCursorToEnd) then
                EndCommand
              else
                HomeCommand;
              Finished := (CC = ccQuit);
            end;

          ccHome :
            HomeCommand;

          ccEnd :
            EndCommand;

          ccDelEol :
            DeleteToEnd;

          ccDelBol :
            DeleteToBegin;

          ccDelLine :
            ClearString;

          ccLeft :
            begin
              if SP > StBgn then begin
                Dec(SP);
                while not PFlags[SP] do
                  Dec(SP);
              end
              else if LongFlagIsSet(sfOptions, efAutoAdvCurBegin) then
                SP := Pred(StBgn);
              CheckAutoAdvance;
            end;

          ccRight :
            begin
              if (SP < StEnd) or (LongFlagIsSet(sfOptions, efAutoAdvCurEnd) and not
               LongFlagIsSet(sfFlags, ifNotNext)) then
                AdvanceCursor;
              CheckAutoAdvance;
            end;

          ccWordLeft :
            WordLeft;

          ccWordRight :
            WordRight;

          ccDel :
            if SP <= StLen then
              DeleteChar;

          ccBack :
            if (SP > StBgn) then begin
              Dec(SP);
              while not PFlags[SP] do
                Dec(SP);
              DeleteChar;
              if efHOffset > 0 then                     {!!.20}
                {String horizontally scrolled}
                if efHOffset+sfFWidth >= StLen then     {!!.20}
                  {The rightmost portion of the string is displayed, so scroll}
                  Dec(efHOffset);                       {!!.20}
            end;

          ccDelWord :
            if SP <= StLen then
              DeleteWord;

          ccIns :
            if not LongFlagIsSet(sfOptions, efForceMode) then
              ToggleInsertMode(InsertMode);

          ccHelp :
            CP.cpGetHelp(UnitCode, nil, sfHelpIndex);
        end;

        {make sure it's OK to go to next/previous field}
        if Finished then
          Finished := EF.efOKtoAdvance(CC);

        {validate the entry if done}
        if Finished then begin
          {special handling of numbers}
                          {!!.02}
          if IsNumber and not LeaveBlank then begin {!!.11}
            SaveModified := St <> STmp;             {!!.11}
            FixNumber(True, True);
          end                                       {!!.11}
          else                                      {!!.11}
            SaveModified := False;                  {!!.11}

          {make sure the screen is up to date}
          Redraw(False);

          if (CC <> ccQuit) and not ReadOnly then begin
            I := ValidField(@EF, St);

            {swap strings for validation purposes}
            SwapStrings(St, efEditSt^);

            ErrorMsg := @emNullError;
            if (I <> 0) or not Validate(ErrCode, ErrorMsg) then begin
              {not done yet--swap back}
              Finished := False;
              FirstChar :=                                     {!!.12}
                LongFlagIsSet(EF.sfOptions, efClearFirstChar); {!!.12}
              SwapStrings(St, efEditSt^);
              if I <> 0 then begin
                SP := I;
                ErrCode := ecBadCharacter;
                ErrorMsg := @emBadCharacter;
              end
              else
                SP := LastSP;

              {display error message from validation routine}
              Inc(ErrCode, epWarning);
              ErrorRoutine(UnitCode, ErrCode, ErrorMsg^);
              if FirstChar then                            {!!.12}
                HomeCommand;                               {!!.12}
            end;
          end;
        end;

      until Finished;

      {set modified flag}
      if ((CC <> ccQuit) and (STmp <> efEditSt^)) or SaveModified then {!!.11}
        if not ReadOnly then                                           {!!.13}
          SetLongFlag(sfFlags, ifModified);

      {restore insert mode if it was forced one way or the other}
      if LongFlagIsSet(sfOptions, efForceMode) then
        InsertMode := SaveInsertMode;

      {restore break checking status}
      CheckBreak := SaveBreak;

      {restore cursor shape}
      SetCursorSize(Hi(CursorSL), Lo(CursorSL));

      if LongFlagIsSet(sfOptions, efAutoNumLock) then
        {restore previous NumLock state}
        if SaveNumLock then
          {$IFDEF VIRTUALPASCAL}
          SetKeyboardState( kbd_Numlock, True )
          {$ELSE}
          SetFlag(KeyboardFlags^, NumLockBit)                         {!!.20}
          {$ENDIF}
        else
          {$IFDEF VIRTUALPASCAL}
          SetKeyboardState( kbd_Numlock, False );
          {$ELSE}
          ClearFlag(KeyboardFlags^, NumLockBit);                      {!!.20}
          {$ENDIF}

      {we're not editing}
      ClearLongFlag(sfFlags, ifEditing);
    end;
  end;

  procedure CharEditor(var Field; Row, Col : Word; FA, CA : Byte;
                       PasswordChar : Char; PosCode : Byte;
                       ReadOnly : Boolean; var CC, ChWord : Word;
                       var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                       UnitCode : Byte; var CP : CommandProcessor);
    {-Edit a character field}
  var
    EF : EntryField absolute Field;
    PFlags : PictureFlags;
    Ch : Char absolute ChWord;
    St : string;
    StPtr : StringPtr;
    StLen : Byte;
    CursorSL, I : Word;
    SP : Byte;
    SaveBreak : Boolean;
    Finished : Boolean;
    ErrCode : Word;
    ErrorMsg : StringPtr;
    FirstTime : Boolean;

    procedure CalcSP;
      {-Calculate proper value for SP}
    var
      I : Word;
    begin
      SP := 0;
      for I := 1 to Length(EF.efPicture^) do
        if PFlags[I] then begin
          SP := I;
          Exit;
        end;
    end;

    procedure InsertChar;
      {-Insert Ch at St[SP]}
    begin
      St[SP] := Ch;
      if LongFlagIsSet(EF.sfOptions, efAutoAdvCharEnd) then begin
        CC := ccAutoAdvance {ccSelect};   {!!.12}
        Finished := True;
      end;
    end;

    procedure Redraw(MoveCursor : Boolean);
      {-Position cursor and redraw string}
    begin
      with EF do begin
        Draw(St, Row, Col, FA, CA, 0, PasswordChar, PFlags);
        if MoveCursor then
          GoToXYAbs(Col+Pred(SP), Row);
      end;
    end;

    procedure ReloadEditSt;
      {-Reload the original string}
    begin
      St := EF.efEditSt^;
    end;

    function CharIsOK : Boolean;
      {-Return true if Ch can be added to the string}
    begin
      CharIsOK := CharOK(EF.efPicture^[SP], Ch, #255, True);
    end;

  begin
    with EF do begin
      {we're editing}
      SetLongFlag(sfFlags, ifEditing);
      StPtr := @St;

      {store cursor shape}
      CursorSL := CursorTypeSL;

      {initialize}
      InitPictureFlags(PFlags);

      {get the default string and calculate position for SP}
      CalcSP;
      if SP = 0 then begin
        CC := ccNone;
        Exit;
      end;
      ReloadEditSt;

      {save break checking state}
      SaveBreak := CheckBreak;
      CheckBreak := False;

      {set cursor shape}
      InsertMode := not InsertMode;
      ToggleInsertMode(InsertMode);

      {clear modified flag}
      ClearLongFlag(sfFlags, ifModified);

      {loop reading keys}
      Finished := False;
      FirstTime := (CC <> ccNone);
      repeat
        {position cursor and redraw string}
        Redraw(True);

        {get next command and validate it}
        if FirstTime then
          FirstTime := False
        else begin
          ExchangeLongInts(LongInt(StPtr), LongInt(efEditSt));
          CC := CP.GetCommand(ChWord);
          ExchangeLongInts(LongInt(StPtr), LongInt(efEditSt));
        end;

        if ReadOnly then begin
          if (CC <= 255) and not(CC in ReadOnlyCommands) then
            CC := ccNone;
        end
        else if (CC <= 255) and not(CC in StringCommands) then
          CC := ccNone;

        {allow editing of the existing string}
        case CC of
          ccChar :
            if CharIsOK then
              InsertChar
            else if LongFlagIsSet(sfOptions, efBeepOnError) then
              RingBell;

          ccIns :
            ToggleInsertMode(InsertMode);

          ccMouseAuto,           {!!.11}
          ccAltKey, ccMouseDown, {!!.03}
          ccTab, ccBackTab, ccSelect, ccNextField, ccAutoAdvance, ccLeft,
          ccWordLeft, ccRight, ccWordRight, ccPrevField, ccUp, ccDown,
          ccNextRec, ccPrevRec, ccFirstFld, ccMouseSel, ccLastFld, ccPageUp,
          ccPageDn, ccNested, ccDone, ccUser0..ccUser65335 :
            Finished := True;

          ccRestore, ccQuit :
            begin
              ReloadEditSt;
              Finished := (CC = ccQuit);
            end;

          ccHelp :
            CP.cpGetHelp(UnitCode, nil, sfHelpIndex);

          else
            if Lo(ChWord) <> 0 then begin
              if not ReadOnly then begin
                CC := ccChar;
                FirstTime := True;
              end;
            end
            else if LongFlagIsSet(sfOptions, efBeepOnError) then
              RingBell;
        end;

        {make sure it's OK to go to next/previous field}
        if Finished then
          Finished := efOKtoAdvance(CC);

        {validate the entry if done}
        if Finished then begin
          {make sure the screen is up to date}
          Redraw(False);

          if (CC <> ccQuit) and not ReadOnly then begin
            {make sure all characters in string are valid}
            I := ValidField(@EF, St);

            {swap strings for validation purposes}
            SwapStrings(St, efEditSt^);

            ErrorMsg := @emNullError;
            if (I <> 0) or not Validate(ErrCode, ErrorMsg) then begin
              {not done yet--swap back}
              Finished := False;
              SwapStrings(St, efEditSt^);
              if I <> 0 then begin
                ErrCode := ecBadCharacter;
                ErrorMsg := @emBadCharacter;
              end;

              {display error message from validation routine}
              Inc(ErrCode, epWarning);
              ErrorRoutine(UnitCode, ErrCode, ErrorMsg^);
            end;
          end;
        end;

      until Finished;

      {set modified flag}
      if (CC <> ccQuit) and (St <> efEditSt^) then
        SetLongFlag(sfFlags, ifModified);

      {restore break checking status}
      CheckBreak := SaveBreak;

      {restore cursor shape}
      SetCursorSize(Hi(CursorSL), Lo(CursorSL));

      {we're not editing}
      ClearLongFlag(sfFlags, ifEditing);
    end;
  end;

begin
  {$IFNDEF VIRTUALPASCAL}
  KeyboardFlags := Ptr(BiosDataSele, $17);                            {!!.20}
  {$ENDIF}
end.
