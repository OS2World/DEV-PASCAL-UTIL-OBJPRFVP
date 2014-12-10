{$IFDEF Windows}
  !! ERROR - This unit is not compatible with Windows !!
{$ENDIF}

{$R-,S-,I-,V-,B-,F+,O+,A-} {!!.01}

{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPCALC.PAS 1.30                      *}
{*               Programmer's Calculator                 *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpCalc;
  {-Programmer's calculator}

interface

uses
  Use32,
  {$IFDEF VIRTUALPASCAL}
  VpUtils,
  {$ENDIF}
  Dpmi,
  Dos,
  OpBcd,
  OpInline,
  OpString,
  OpConst,   {!!.20}
  OpRoot,
  OpCrt,
  OpCmd,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpFrame,
  OpWindow
  {$IFDEF UseDrag}     {!!.03}
  , OpDrag             {!!.03}
  {$ENDIF}             {!!.03}
  ;

  {$I OPCALC.ICD}  {configuration data}

const
  {option codes}
  {...none...}

  DefCalcOptions : Byte = 0;

  DefCrossBar : Char = 'Ä';
  DefLeftTee  : Char = 'Ã';
  DefRightTee : Char = '´';

  CalculatorWidth  = 43;     {width of calculator window}
  CalculatorHeight = 8;      {height of inner calculator window}
  NumStringWidth = 37;

type
  CalcType = (None, Add, Subtract, Multiply, Divide,
    AndOp, ModOp, NotOp, OrOp, XorOp, ShlOp, ShrOp);
  CalcMode = (Decimal, Hexadecimal, Binary, FloatPt, Exponential);
  CalcStatus = (Cleared, Finished, Num1, Num2);
  ExpStateType = (NoExp, DoingExp, HaveExp);
  NumericString = string[NumStringWidth];

  CalculatorPtr = ^Calculator;
  Calculator =
    object(CommandWindow)
      caOptions : Byte;           {option flags}
      caSubheadColor, caSubheadMono : Byte;
      caHighlightColor, caHighlightMono : Byte;
      {...}
      caBcdMode : Boolean;
      caHaveDecPt : Boolean;
      caMinusPending  : Boolean;
      caDiv0 : Boolean;
      caCalcMode : CalcMode;
      caCalcType : CalcType;
      caDigitCount : Byte;
      caNumStr1 : NumericString;
      caNumStr2 : NumericString;
      caSaveStr : NumericString;
      caResultStr : NumericString;
      caLong1 : LongInt;
      caLong2 : LongInt;
      caLongResult : LongInt;
      caSavedLong : LongInt;
      caBcd1 : BCD;
      caBcd2 : BCD;
      caBcdResult : BCD;
      caSavedBcd : BCD;
      caStatus : CalcStatus;
      caExponent : Integer;
      caExpSign : Boolean;
      caNegativeExp : Boolean;
      caExpState : ExpStateType;
      {....methods....}
      constructor Init(X1, Y1 : Byte);
        {-Initialize the calculator}
      constructor InitCustom(X1, Y1 : Byte;
                             LeftTee, CrossBar, RightTee : Char;
                             var Colors : ColorSet;
                             Options : LongInt);
       {-Initialize the calculator with custom colors and options}
      procedure ProcessSelf; virtual; {!!.01}
        {-Process calculator commands}
      function GetResult(TypedConst : Boolean) : string;
        {-Return the result of the last calculation as a string, optionally in
          typed constant format. If calculation not finished, value being
          entered is returned}
      {...}
      procedure caOptionsOn(OptionFlags : Byte);
        {-Activate multiple options}
      procedure caOptionsOff(OptionFlags : Byte);
        {-Deactivate multiple options}
      function caOptionsAreOn(OptionFlags : Byte) : Boolean;
        {-Return true if all specified options are on}
      {...}
      procedure SetSubheadAttr(Color, Mono : Byte);
        {-Set attributes for subheads}
      procedure SetHighlightAttr(Color, Mono : Byte);
        {-Set attributes for highlighted characters}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a calculator from a stream}
      procedure Store(var S : IdStream);
        {-Store a calculator in a stream}
    {$ENDIF}
{.Z+}
      {+++ internal methods +++}
      procedure UpdateContents; virtual;
      procedure caUpdateDisplay; virtual;
      procedure caShowMode; virtual;
      procedure caShowCommandList; virtual;
      procedure caDrawSubheads; virtual;
      procedure caGetBcdVal(var S : string; var B : BCD);
      procedure caGetExpString(var B : BCD; var BcdSt : string);
      procedure caGetBcdString(var B : BCD; var BcdSt : string);
      procedure caResetDigitCount;
      procedure caTrimZeros(var S : string);
      procedure caUpdateOneString(Which : Byte);
      procedure caUpdateAllStrings;
      function caCheckMathError : Boolean;
      procedure caSwitchMode(Mode : CalcMode);
      procedure caClearCurrentEntry;
      procedure caClearAll;
      procedure caClearEntry;
      procedure caPerformCalc;
      procedure caDoCalc(CT : CalcType);
      procedure caFinishCalc;
      procedure caSaveValue;
      procedure caInsertSavedValue;
      function  caAppendChar(Ch : Char) : Boolean;
      procedure caAddDigit(Digit : Integer);
      procedure caStartExponent;
      procedure caDoMinus;
      procedure caDoPlus;
      procedure caDoDecimalPoint;
      procedure caDoBackspace;
      {$IFDEF UseMouse}
      function caProcessMouseCommand(Cmd : Word) : Boolean; {!!.03}
        {-Process ccMouseSel command. Returns True to return control to user.}
      {$ENDIF}
{.Z-}
    end;

{.Z+}
  {$IFDEF UseStreams}
  procedure CalculatorStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing calculators}
  {$ENDIF}
{.Z-}

{.F-}
var
  {$IFDEF UseDrag}                     {!!.03}
  CalcCommands : DragProcessor;        {!!.03}
  {$ELSE}                              {!!.03}
  CalcCommands : CommandProcessor;
  {$ENDIF}                             {!!.03}

  {===========================================================}

implementation

  {calculator stuff}
const
  MaxLongIntDiv10 = 214748364;
  DefaultMode = FloatPt;
  Digits : array[0..$F] of Char = '0123456789ABCDEF';
  CommandLabel1 : string[54] =
  #1'C'#1'lear '#1'E'#1'ntry '#1'B'#1'inary '#1'D'#1'ecimal '#1'H'#1'ex '#1'F'#1'loat ex'#1'P'#1;
  CommandLabel2 : string[58] =
  ' '#1'S'#1'ave '#1'I'#1'nsert '#1'A'#1'nd '#1'M'#1'od '#1'N'#1'ot '#1'O'#1'r '#1'X'#1'or sh'#1'L'#1' sh'#1'R'#1' ';
  ModeStrings : array[CalcMode] of string[7] =
  ('  Dec  ', '  Hex  ', '  Bin  ', ' Float ', '  Exp  ');
  CalcChars : array[CalcType] of string[3] =
  ('   ', '+  ', '-  ', '*  ', 'ö  ', 'and', 'mod', 'not', 'or ', 'xor',
    'shl', 'shr');
  DoneChars : array[Boolean] of string[1] = (' ', '=');
  HighDigit :                {highest digit allowed in given CalcMode}
    array[CalcMode] of Byte = (9, $F, 1, 9, 9);
  MaxDigits :                {number of digits allowed in given CalcMode}
    array[CalcMode] of Byte = (10, 8, 32, 35, 35);
  Base : array[Decimal..Binary] of LongInt = (10, 16, 2);
  ZeroString : string[1] = '0';
  MinusZero : string[2] = '-0';
  NullString : string[1] = '';
  MathError : Boolean = False;

  {window-relative coordinates}
  Num1Row     = 1;
  Num2Row     = 2;
  ResultRow   = 3;
  ModeRow     = 4;
  SavedRow    = 5;
  CommandsRow = 6;
  LabelRow1   = 7;
  LabelRow2   = 8;
  NumCol      = 2;
  LabelCol    = 3;
  SymCol      = 40;
  SavedCol    = 33;

  constructor Calculator.Init(X1, Y1 : Byte);
    {-Initialize the calculator}
  begin
    {initialize calculator with default window options}
    if not Calculator.InitCustom(X1, Y1, DefLeftTee, DefCrossBar, DefRightTee,
                                 DefaultColorSet, DefWindowOptions) then
      Fail;
  end;

  constructor Calculator.InitCustom(X1, Y1 : Byte;
                                    LeftTee, CrossBar, RightTee : Char;
                                    var Colors : ColorSet;
                                    Options : LongInt);
    {-Initialize the calculator with custom colors and options}
  var
    X2, Y2 : Byte;
  begin
    {won't work if screen is too small}
    if (ScreenWidth < CalculatorWidth+2) or (ScreenHeight < CalculatorHeight) then begin
      InitStatus := epFatal+ecBadCoordinates;
      Fail;
    end;

    {calculate X2 and Y2}
    X2 := X1+Pred(CalculatorWidth);
    Y2 := Y1+Pred(CalculatorHeight);

    {force wUserContents on; make sure there's a frame and that the window
     isn't resized}
    ClearLongFlag(Options, wResizeable);
    SetLongFlag(Options, wUserContents+wBordered);

    {initialize the window}
    if not CommandWindow.InitCustom(X1, Y1, X2, Y2, Colors, Options,
                                    CalcCommands, ucCalc) then
      Fail;

    {add window dividers}
    wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, ModeRow, frTT);
    if RawError = 0 then                                      {!!.01}
      wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, CommandsRow, frTT);

    if RawError <> 0 then begin                               {!!.01}
      InitStatus := RawError;                                 {!!.01}
      Done;
      Fail;
    end;

    {initialize our own data fields}
    caOptions := DefCalcOptions;
    caCalcMode := DefaultMode;
    caBcdMode := (caCalcMode >= FloatPt);
    MathError := False;
    caDiv0 := False;
    caSavedLong := 0;
    caSavedBcd := ZeroBCD;
    caSaveStr := ZeroString;
    caClearAll;

    {set colors}
    caSubheadColor := Colors.FrameColor;
    caSubheadMono := Colors.FrameMono;
    caHighlightColor := Colors.HighlightColor;
    caHighlightMono := Colors.HighlightMono;
  end;

  procedure Calculator.caUpdateDisplay;
    {-Update the calculator display}
  var
    TA, HA : Byte;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {get attributes to use}
    TA := ColorMono(wTextColor, wTextMono);
    HA := ColorMono(caHighlightColor, caHighlightMono);

    RawWindow.wFastWrite(
      LeftPad(caNumStr1, NumStringWidth), Num1Row, NumCol, TA);
    RawWindow.wFastWrite(
      LeftPad(caNumStr2, NumStringWidth), Num2Row, NumCol, TA);
    RawWindow.wFastWrite(
      CalcChars[caCalcType], Num2Row, SymCol, HA);
    RawWindow.wFastWrite(
      LeftPad(caResultStr, NumStringWidth), ResultRow, NumCol, TA);
    RawWindow.wFastWrite(
      DoneChars[caStatus = Finished], ResultRow, SymCol, HA);
    RawWindow.wFastWrite(
      LeftPad(caSaveStr, NumStringWidth), SavedRow, NumCol, TA);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure Calculator.caShowMode;
    {-Show the current calculation mode}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    RawWindow.wFastWrite(
      ModeStrings[caCalcMode], ModeRow, NumCol,
      ColorMono(caSubheadColor, caSubheadMono));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure Calculator.caShowCommandList;
    {-Display help on available commands}
  var
    FxAttrs : FlexAttrs;        {attributes for FlexWrite}
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {set attributes for FlexWrite}
    FxAttrs[0] := ColorMono(wTextColor, wTextMono);
    FxAttrs[1] := ColorMono(caHighlightColor, caHighlightMono);

    {draw command labels}
    RawWindow.wFlexWrite(CommandLabel1, LabelRow1, LabelCol, FxAttrs);
    RawWindow.wFlexWrite(CommandLabel2, LabelRow2, LabelCol, FxAttrs);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure Calculator.caDrawSubheads;
    {-Draw subheads over the top of the window dividers}
  var
    SHA : Byte;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw subheads}
    SHA := ColorMono(caSubheadColor, caSubheadMono);
    RawWindow.wFastWrite(' Saved ', ModeRow, SavedCol, SHA);
    RawWindow.wFastWrite(' Commands ', CommandsRow, NumCol, SHA);

    {display the current mode}
    caShowMode;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure Calculator.UpdateContents;
    {-Redraw the calculator window}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw subheads on top of window divider}
    caDrawSubheads;

    {display numbers, etc.}
    caUpdateDisplay;

    {display command list}
    caShowCommandList;

    StackWindow.UpdateContents; {!!.01}

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure Calculator.caGetBcdVal(var S : string; var B : BCD);
    {-Convert string S to a BCD real}
  var
    Code : Word;
  begin
    ValBcd(S, B, Code);
    if Code <> 0 then begin
      S := ZeroString;
      B := ZeroBCD;
    end;
  end;

  procedure Calculator.caGetExpString(var B : BCD; var BcdSt : string);
    {-Convert a BCD to a string with an exponent, and delete 0's just before
      the 'E'}
  var
    I : Word;
  begin
    {convert to string}
    BcdSt := StrExpBcd(B, 0);

    {delete initial space, if any}
    if BcdSt[1] = ' ' then
      Delete(BcdSt, 1, 1);

    {delete 0's just before the 'E', if any}
    I := Pos('E', BcdSt);
    while BcdSt[Pred(I)] = '0' do begin
      Dec(I);
      Delete(BcdSt, I, 1);
    end;

    {delete '.' just before the 'E', if any}
    Dec(I);
    if BcdSt[I] = '.' then
      Delete(BcdSt, I, 1);
  end;

  procedure Calculator.caGetBcdString(var B : BCD; var BcdSt : string);
    {-Try to convert a BCD to a string without the exponent showing}
  var
    S : string;
    SLen : Byte absolute S;
  begin
    {convert B to a string}
    S := StrBcd(B, 0, 81);

    {delete any trailing 0's}
    while S[SLen] = '0' do
      Dec(SLen);

    {delete '.' at the end, if any}
    if S[SLen] = '.' then
      Dec(SLen);

    {if the string is still too large, convert to exponential format}
    if SLen > NumStringWidth then
      caGetExpString(B, BcdSt)
    else
      BcdSt := S;
  end;

  procedure Calculator.caResetDigitCount;
    {-Reset the digit count, etc.}
  var
    Epos : Word;
    Estr : string[4];
  begin
    {get digit count}
    if (caNumStr2 = ZeroString) or (caNumStr2 = MinusZero) then
      caDigitCount := 0
    else begin
      caDigitCount := Length(caNumStr2);
      caMinusPending := caNumStr2[1] = '-';
      if caMinusPending then begin
        case caCalcMode of
          Hexadecimal..Binary :
            begin
              {minus sign not allowed in Hex or Binary modes}
              Delete(caNumStr2, 1, 1);
              caMinusPending := False;
            end;
        end;
        {doesn't count toward total number of digits in any case}
        Dec(caDigitCount);
      end;
    end;

    {get exponent}
    if caBcdMode then begin
      caHaveDecPt := Pos('.', caNumStr2) <> 0;
      Epos := Pos('E', caNumStr2);
      if (Epos = 0) then
        caExpState := NoExp
      else begin
        {get exponent}
        Estr := Copy(caNumStr2, Succ(Epos), Length(caNumStr2));
        if not Str2Int(Estr, caExponent) then
          caExponent := 0;
        if Abs(caExponent) < 10 then
          caExpState := DoingExp
        else
          caExpState := HaveExp;
        caNegativeExp := (Pos('-', Estr) <> 0);
        caExpSign := caNegativeExp or (Pos('+', Estr) <> 0);
      end;
    end;
  end;

  procedure Calculator.caTrimZeros(var S : string);
    {-Trim initial 0's from S}
  var
    SLen : Byte absolute S;
  begin
    while (S[1] = '0') and (SLen > 1) do begin
      Dec(SLen);
      Move(S[2], S[1], SLen);
    end;
  end;

  procedure Calculator.caUpdateOneString(Which : Byte);
    {-Update the string specified by Which:
      1 = caNumStr1
      2 = caNumStr2
      3 = caResultStr
      4 = caSaveStr}
  var
    StPtr : ^String;
    LongPtr : ^LongInt;
    BcdPtr : ^BCD;
  begin
    {set string, number pointers}
    case Which of
      1 : if (caCalcType = NotOp) then begin
            caNumStr1 := NullString;
            Exit;
          end
          else begin
            StPtr := @caNumStr1;
            LongPtr := @caLong1;
            BcdPtr := @caBcd1;
          end;
      2 : begin
            StPtr := @caNumStr2;
            LongPtr := @caLong2;
            BcdPtr := @caBcd2;
          end;
      3 : begin
            StPtr := @caResultStr;
            LongPtr := @caLongResult;
            BcdPtr := @caBcdResult;
          end;
      4 : begin
            StPtr := @caSaveStr;
            LongPtr := @caSavedLong;
            BcdPtr := @caSavedBcd;
          end;
    end;

    {convert string}
    if caBcdMode and EqualBcd(BcdPtr^, ZeroBCD) then
      StPtr^ := ZeroString
    else
      case caCalcMode of
        Decimal :
          StPtr^ := Long2Str(LongPtr^);
        Hexadecimal :
          StPtr^ := HexL(LongPtr^);
        Binary :
          StPtr^ := BinaryL(LongPtr^);
        FloatPt :
          caGetBcdString(BcdPtr^, StPtr^);
        Exponential :
          caGetExpString(BcdPtr^, StPtr^);
      end;

    {trim any initial 0's}
    if not caBcdMode then
      caTrimZeros(StPtr^);
  end;

  procedure Calculator.caUpdateAllStrings;
    {-Update all the strings that currently have values associated with them}
  begin
    case caStatus of
      Finished :
        begin
          caUpdateOneString(1);
          caUpdateOneString(3);
        end;
      Num2 :
        caUpdateOneString(1);
    end;
    if caStatus <> Cleared then
      caUpdateOneString(2);
    caUpdateOneString(4);
  end;

  function Calculator.caCheckMathError : Boolean;
    {-Returns true if a math error occurred}
  begin
    caCheckMathError := MathError;
    if MathError then begin
      if caDiv0 then
        GotError(epWarning+ecUndefinedNum, 'Result is undefined')
      else
        GotError(epWarning+ecDivideByZero, 'Overflow error');
      caUpdateDisplay;
      MathError := False;
      caDiv0 := False;
    end;
  end;

  procedure Calculator.caSwitchMode(Mode : CalcMode);
    {-Switch calculation modes}
  begin
    {do nothing if we're already in correct mode}
    if Mode = caCalcMode then
      Exit;

    {translate data types if necessary}
    if (Mode >= FloatPt) then begin
      {reject certain calculation types for real numbers}
      case caCalcType of
        AndOp..ShrOp : if caStatus <> Finished then
                         Exit;
      end;
      if not caBcdMode then begin
        LongIntToBcd(caLong1, caBcd1);
        LongIntToBcd(caLong2, caBcd2);
        LongIntToBcd(caLongResult, caBcdResult);
      end
      else
        caGetBcdVal(caNumStr2, caBcd2);
    end
    else if caBcdMode then begin
      caLong1 := RoundBcd(caBcd1);
      caGetBcdVal(caNumStr2, caBcd2);
      caLong2 := RoundBcd(caBcd2);
      caLongResult := RoundBcd(caBcdResult);
    end;

    {exit in case of error}
    if caCheckMathError then
      Exit;

    {change the mode setting}
    caCalcMode := Mode;
    caBcdMode := (Mode >= FloatPt);
    caShowMode;

    {update strings, digit count, etc.}
    caUpdateAllStrings;
    caResetDigitCount;
  end;

  procedure Calculator.caClearCurrentEntry;
    {-Clear the current entry and reset related variables}
  begin
    caDigitCount := 0;
    caLong2 := 0;
    caBcd2 := ZeroBCD;
    caNumStr2 := ZeroString;
    caMinusPending := False;
    caHaveDecPt := False;
    caExpState := NoExp;
  end;

  procedure Calculator.caClearAll;
    {-Reset everything}
  begin
    {clear the current entry}
    caClearCurrentEntry;
    caStatus := Num1;

    {indicate that we're all clear}
    caCalcType := None;
    caStatus := Cleared;

    {clear numeric variables}
    caLong1 := 0;
    caLongResult := 0;
    caBcd1 := ZeroBCD;
    caBcdResult := ZeroBCD;

    {clear strings}
    caNumStr1 := NullString;
    caResultStr := NullString;
  end;

  procedure Calculator.caClearEntry;
    {-Reset the current entry}
  begin
    {if Finished with a calculation, clear everything...}
    if (caStatus = Finished) then
      caClearAll
    else
      {otherwise just the current entry}
      caClearCurrentEntry;
  end;

  procedure Calculator.caPerformCalc;
    {-Perform a calculation of type caCalcMode}
  begin
    if caBcdMode then begin
      caGetBcdVal(caNumStr2, caBcd2);
      case caCalcType of
        Add :
          AddBcd(caBcd1, caBcd2, caBcdResult);
        Subtract :
          SubBcd(caBcd1, caBcd2, caBcdResult);
        Multiply :
          MultBcd(caBcd1, caBcd2, caBcdResult);
        Divide :
          if EqualBcd(caBcd2, ZeroBCD) then begin
            MathError := True;
            caDiv0 := True;
            Exit;
          end
          else
            DivBcd(caBcd1, caBcd2, caBcdResult);
      end;
    end
    else
      case caCalcType of
        Add :
          caLongResult := caLong1+caLong2;
        Subtract :
          caLongResult := caLong1-caLong2;
        Multiply :
          caLongResult := caLong1*caLong2;
        ModOp,                                   {!!.01}
        Divide :
          if (caLong2 = 0) then begin
            MathError := True;
            caDiv0 := True;
            Exit;
          end
          else if caCalcType = ModOp then         {!!.01}
            caLongResult := caLong1 mod caLong2   {!!.01}
          else
            caLongResult := caLong1 div caLong2;
        AndOp :
          caLongResult := caLong1 and caLong2;
        (*                                        {!!.01}
        ModOp :                                   {!!.01}
            caLongResult := caLong1 mod caLong2;  {!!.01}
        *)                                        {!!.01}
        OrOp :
          caLongResult := caLong1 or caLong2;
        XorOp :
          caLongResult := caLong1 xor caLong2;
        ShlOp :
          if (caLong2 > 31) or (caLong2 < 0) then
            caLongResult := 0
          else
            caLongResult := caLong1 shl caLong2;
        ShrOp :
          if (caLong2 > 31) or (caLong2 < 0) then
            caLongResult := 0
          else
            caLongResult := caLong1 shr caLong2;
      end;

    {convert the result to a string}
    caUpdateOneString(3);
  end;

  procedure Calculator.caDoCalc(CT : CalcType);
    {-Prepare for a calculation of the specified type}
  begin
    if (caDigitCount = 0) and (caStatus <> Finished) then
      Exit;

    {reject certain calculation types for real numbers}
    if caBcdMode then
      case CT of
        AndOp..ShrOp : Exit;
      end;

    {NOT is a special case}
    if (CT = NotOp) then begin
      if (caStatus <> Num2) then begin
        caCalcType := NotOp;
        if caStatus <> Num1 then
          caLong2 := caLongResult;
        caLongResult := not caLong2;
        caStatus := Finished;
        caUpdateOneString(1);
        caUpdateOneString(2);
        caUpdateOneString(3);
      end;
      Exit;
    end;

    {move strings and values as necessary}
    case caStatus of
      Finished :
        begin
          {move caResultStr to Num1}
          caLong1 := caLongResult;
          caBcd1 := caBcdResult;
          caNumStr1 := caResultStr;
          caLongResult := 0;
          caBcdResult := ZeroBCD;
          caResultStr := NullString;
        end;
      Num1 :
        begin
          {move 1st number up}
          caLong1 := caLong2;
          if caBcdMode then
            caGetBcdVal(caNumStr2, caBcd2);
          caBcd1 := caBcd2;
          caNumStr1 := caNumStr2;
        end;
      Num2 :
        begin
          {do the calculation, then move result to first number}
          caPerformCalc;

          {handle errors}
          if caCheckMathError then begin
            caClearAll;
            Exit;
          end;

          caLong1 := caLongResult;
          caBcd1 := caBcdResult;
          caNumStr1 := caResultStr;
          caLongResult := 0;
          caBcdResult := ZeroBCD;
          caResultStr := NullString;
        end;
    end;

    {reset}
    caClearCurrentEntry;

    {store calculation type}
    caCalcType := CT;
    caStatus := Num2;
  end;

  procedure Calculator.caFinishCalc;
    {-Finish the current calculation}
  begin
    {exit if the status is wrong}
    if caStatus <> Num2 then
      Exit;

    {perform the actual calculation}
    caPerformCalc;

    {handle errors}
    if caCheckMathError then begin
      caClearAll;
      Exit;
    end;

    caExpState := NoExp;

    {change the status}
    caStatus := Finished;
  end;

  procedure Calculator.caSaveValue;
    {-Save the current entry. Or, if we've just finished a calculation, save
      the result.}
  begin
    case caStatus of
      Cleared : {do nothing} ;
      Finished :                 {save result}
        begin
          if caBcdMode then
            caSavedBcd := caBcdResult
          else
            caSavedLong := caLongResult;
          caSaveStr := caResultStr;
        end;
      else                     {save 2nd num}
        begin
          if caBcdMode then begin
            caGetBcdVal(caNumStr2, caBcd2);
            caSavedBcd := caBcd2;
          end
          else
            caSavedLong := caLong2;
          caSaveStr := caNumStr2;
        end;
    end;
  end;

  procedure Calculator.caInsertSavedValue;
    {-Insert a saved value into the current entry}
  begin
    case caStatus of
      {if Cleared or Finished, clear all and change status}
      Cleared..Finished :
        begin
          caClearAll;
          caStatus := Num1;
        end;
      else
        {just clear the current entry}
        caClearCurrentEntry;
    end;

    {insert the saved value}
    caLong2 := caSavedLong;
    caBcd2 := caSavedBcd;

    {update the current entry string, digit count, etc.}
    caUpdateOneString(2);
    caResetDigitCount;
  end;

  function Calculator.caAppendChar(Ch : Char) : Boolean;
    {-Append a character to caNumStr2}
  begin
    {check for overflow of digits}
    if caDigitCount >= MaxDigits[caCalcMode] then begin
      caAppendChar := False;
      Exit;
    end;

    if (caDigitCount = 0) and (Ch <> '.') then begin
      caNumStr2[Length(caNumStr2)] := Ch;
      caDigitCount := 1;
    end
    else begin
      Inc(caNumStr2[0]);
      caNumStr2[Length(caNumStr2)] := Ch;
      Inc(caDigitCount);
    end;
    caAppendChar := True;
  end;

  procedure Calculator.caAddDigit(Digit : Integer);
    {-Add a digit to the current entry}
  var
    AbsLong2 : LongInt;
    AbsExp,
    DigitToAdd : Integer;
  begin
    {check for illegal digit}
    if (caDigitCount >= MaxDigits[caCalcMode]) or (Digit > HighDigit[caCalcMode]) then
      Exit;

    {reject extra digits in an exponent}
    if caExpState = HaveExp then
      Exit;

    {reset if ...}
    if caStatus = Finished then
      caClearAll;

    if caStatus = Cleared then
      caStatus := Num1;

    {don't insert extra zeros}
    if ((caDigitCount = 0) or (caNumStr2 = MinusZero)) and (Digit = 0) then
      Exit;

    case caCalcMode of
      Decimal..Binary :      {longint operation}
        begin
          {check for potential overflow if we're in Decimal mode}
          if caCalcMode = Decimal then begin
            AbsLong2 := Abs(caLong2);
            if (AbsLong2 > MaxLongIntDiv10) or ((AbsLong2 = MaxLongIntDiv10) and (Digit > 7)) then
              Exit;
          end;

          {get digit to add}
          if caMinusPending then
            DigitToAdd := -Digit
          else
            DigitToAdd := Digit;

          {add it}
          caLong2 := (caLong2*Base[caCalcMode])+LongInt(DigitToAdd);
        end;
    else                     {floating point operation}
      if (caExpState = DoingExp) then begin
        AbsExp := Abs(caExponent);
        if (AbsExp > 6) or ((AbsExp = 6) and (Digit > 3)) then
          Exit;
        if caNegativeExp then
          DigitToAdd := -Digit
        else
          DigitToAdd := Digit;
        if (caExponent = 0) then
          caExponent := DigitToAdd
        else begin
          caExponent := (caExponent*10)+DigitToAdd;
          caExpState := HaveExp;
        end;
      end;
    end;

    {append the digit}
    if caAppendChar(Digits[Digit]) then
      {won't fail -- error checking already done} ;
  end;

  procedure Calculator.caStartExponent;
    {-Handle entry of 'E'}
  begin
    if (caExpState <> NoExp) or (caDigitCount = 0) then
      Exit;
    if not caAppendChar('E') then
      Exit;
    caExpState := DoingExp;
    caNegativeExp := False;
    caExpSign := False;
    caExponent := 0;
  end;

  procedure Calculator.caDoMinus;
    {-Handle entry of '-'}
  begin
    if (caExpState = DoingExp) then begin
      if (caExponent = 0) and not caExpSign then
        if caAppendChar('-') then begin
          caExpSign := True;
          caNegativeExp := True;
        end;
    end
    else
      if (caCalcType <> ShlOp) and (caCalcType <> ShrOp) and
      (caDigitCount = 0) and (HighDigit[caCalcMode] = 9) then begin
        if not caMinusPending then
          caNumStr2 := '-'+caNumStr2;
        caMinusPending := True;
      end
      else
        caDoCalc(Subtract);
  end;

  procedure Calculator.caDoPlus;
    {-Handle entry of '+'}
  begin
    if (caExpState = DoingExp) then begin
      if (caExponent = 0) and not caExpSign then
        if caAppendChar('+') then
          caExpSign := True;
    end
    else
      caDoCalc(Add);
  end;

  procedure Calculator.caDoDecimalPoint;
    {-Handle entry of '.'}
  begin
    if (not caBcdMode) or caHaveDecPt or (caExpState <> NoExp) then
      Exit;
    if caAppendChar('.') then
      caHaveDecPt := True;
  end;

  procedure Calculator.caDoBackspace;
    {-Handle entry of ^H (BkSp)}
  var
    Ch : Char;
  begin
    if (caNumStr2 = ZeroString) or (caStatus = Finished) then
      Exit;
    if (Length(caNumStr2) = 1) then begin
      caNumStr2 := ZeroString;
      caLong2 := 0;
    end
    else
      if (Length(caNumStr2) = 2) and (caNumStr2[1] = '-') then begin
        if caNumStr2 = MinusZero then begin
          caNumStr2 := ZeroString;
          caMinusPending := False;
        end
        else
          caNumStr2 := MinusZero;
        caLong2 := 0;
      end
      else begin
        Ch := caNumStr2[Length(caNumStr2)];
        Dec(caNumStr2[0]);
        if not caBcdMode then
          case Ch of
            '0'..'9' : caLong2 := caLong2 div Base[caCalcMode];
          end;
      end;
    caResetDigitCount;
  end;

{$IFNDEF VIRTUALPASCAL}
  procedure Int0Handler(BP : Word); interrupt;
    {-Traps INT $0 for divide by zero and BCD math errors}
  begin
    MathError := True;
  end;
{$ENDIF}

  {$IFDEF UseMouse}
  function Calculator.caProcessMouseCommand(Cmd : Word) : Boolean; {!!.03}
    {-Process ccMouseSel command. Returns True to return control to user.}
  var
    L : LongInt;
    FramePos : FramePosType;
    HotCode : Byte;
    Dragging : Boolean; {!!.03}
  begin
    caProcessMouseCommand := False;

    {determine position of mouse}
    L := cwMouseResults(Cmd, FramePos, HotCode);    {!!.03} {!!.13}

    {Should mouse event be ignored?}                             {!!.03}
    if cwIgnoreMouseEvent(Dragging, Cmd, FramePos, HotCode) then {!!.03}
      Exit;                                                      {!!.03}

    case HotCode of
      hsNone :           {not a hot spot}
        case FramePos of
          frInsideActive :       {inside window}
            {does nothing in this unit} ;

          frTL..frRR,            {on the frame}
          frInsideFrame,         {inside window frame but not in window boundaries}
          frOutsideFrame :       {outside window frame}
            caProcessMouseCommand := LongFlagIsSet(wFlags, wAllMouseEvents);
        end;

      {$IFDEF UseScrollBars}
      hsDecH,            {decrement fixtures of scroll bars}
      hsDecV,
      hsIncH,            {increment fixtures of scroll bars}
      hsIncV,
      hsBar :            {the slider portion of a scroll bar}
        {do nothing} ;
      {$ENDIF}

      hsSpot,            {a single character hot spot}
      hsRegion0..255 :   {a user-defined region relative to a frame}
        caProcessMouseCommand := (Cmd <> ccMouseAuto); {!!.03}
    end;
  end;
  {$ENDIF}

  procedure Calculator.ProcessSelf; {!!.01}
    {-Process calculator commands}
  const
    NumLockBit = $20;
  var
    KeyboardFlags : ^Byte; {absolute $40 : $17;}                        {!!.20}
    SaveNumLock : Boolean;
    Ch : Char absolute cwKey;
    SaveInt0 : Pointer;
    SaveCase : Boolean;
    AllDone : Boolean;
    {$IFDEF UseMouse}
    {SaveMouse : Boolean;}    {!!.01}
    {$ENDIF}
  begin
    (*                 {!!.01}
    cwCmd := ccError;
    if cwGetLastError <> 0 then
      Exit;
    *)

    {Draw initial screen if not already done}   {!!.01} {moved up}
    Draw;
    if RawError <> 0 then begin                 {!!.01}
      {GotError(wNotCurrent, emNullError);}     {!!.01}
      Exit;
    end;

    {switch INT $0 handlers}
    {$IFNDEF VIRTUALPASCAL}
    GetIntVec(0, SaveInt0);
    SetIntVec(0, @Int0Handler);
    {$ENDIF}

    {save NumLock state and force it on}
    {$IFDEF VIRTUALPASCAL}
    {$ELSE}
    KeyboardFlags := Ptr(BiosDataSele, $17);                          {!!.20}
    SaveNumLock := ByteFlagIsSet(KeyboardFlags^, NumLockBit);         {!!.20}
    SetByteFlag(KeyboardFlags^, NumLockBit);                          {!!.20}
    {$ENDIF}

    (*                {!!.01}
    {$IFDEF UseMouse}
    SaveMouse := MouseCursorOn;
    if cwCmdPtr^.MouseEnabled then
      ShowMouse;
    {$ENDIF}
    *)

    {hide the cursor}
    SetCursor(cuHidden);
    GotoXY(1, 1);

    AllDone := False;
    repeat
      {update the screen}
      caUpdateDisplay;

      with cwCmdPtr^ do begin
        {force upper case temporarily}
        SaveCase := cpOptionsAreOn(cpUpcase);
        cpOptionsOn(cpUpcase);

        {get the next command}
        GetNextCommand;

        {restore upper case setting}
        if SaveCase then
          cpOptionsOn(cpUpcase)
        else
          cpOptionsOff(cpUpcase);
      end;

      case cwCmd of
        {normal digits}
        ccChar :
          case Ch of
            '0'..'9' : caAddDigit(Ord(Ch) and $0F);
          end;
        ccHexA :
          caAddDigit($A);
        ccHexB :
          caAddDigit($B);
        ccHexC :
          caAddDigit($C);
        ccHexD :
          caAddDigit($D);
        ccHexE :
          caAddDigit($E);
        ccHexF :
          caAddDigit($F);
        ccDecimalPt :
          caDoDecimalPoint;

        {arithmetic operators}
        ccAdd :
          caDoPlus;
        ccSubtract :
          caDoMinus;
        ccMultiply :
          caDoCalc(Multiply);
        ccDivide :
          caDoCalc(Divide);

        {arithmetic/logical operations}
        ccAnd :
          caDoCalc(AndOp);
        ccShl :
          caDoCalc(ShlOp);
        ccMod :
          caDoCalc(ModOp);
        ccNot :
          caDoCalc(NotOp);
        ccOr :
          caDoCalc(OrOp);
        ccShr :
          caDoCalc(ShrOp);
        ccXor :
          caDoCalc(XorOp);

        {calculation modes}
        ccBinaryMode :
          caSwitchMode(Binary);
        ccDecimalMode :
          caSwitchMode(Decimal);
        ccFloatMode :
          caSwitchMode(FloatPt);
        ccHexMode :
          caSwitchMode(Hexadecimal);
        ccExpMode :
          caSwitchMode(Exponential);

        {commands}
        ccClearAll :
          caClearAll;
        ccClearEntry :
          if caBcdMode and (caExpState = NoExp) and (caStatus <> Finished) then
            caStartExponent
          else
            caClearEntry;
        ccInsertValue :
          caInsertSavedValue;
        ccSaveValue :
          caSaveValue;

        {other}
        ccBack :
          caDoBackspace;
        ccSelect :
          caFinishCalc;
        ccQuit,
        ccUser0..ccUser65335 :
          AllDone := True;
        ccHelp :
          RequestHelp(wHelpIndex);
        {$IFDEF UseMouse}
        ccMouseAuto,                             {!!.03}
        ccMouseDown,                             {!!.03}
        ccMouseSel :
          AllDone := caProcessMouseCommand(cwCmd); {!!.03}
        {$ENDIF}
        else if (cwCmd <= 255) and (GetExitCommandPtr <> nil) then      {!!.01}
          {Possibly a special exit command defined by a derived object} {!!.01}
          AllDone := (cwCmd in GetExitCommandPtr^);                     {!!.01}
      end;
    until AllDone or (cwCmd = ccError);

    {restore previous NumLock state}
{$IFDEF VIRTUALPASCAL}
    SetKeyboardState( NumLockBit, SaveNumLock );
{$ELSE}
    if SaveNumLock then
      SetByteFlag(KeyboardFlags^, NumLockBit)                           {!!.20}
    else
      ClearByteFlag(KeyboardFlags^, NumLockBit);                        {!!.20}

    {restore INT $0 handler}
    SetIntVec(0, SaveInt0);
{$ENDIF}

    {save window state}
    rwSaveWindowState;

    {$IFDEF UseMouse}
    {ShowMousePrim(SaveMouse);}      {!!.01}
    {$ENDIF}
  end;

  procedure Calculator.caOptionsOn(OptionFlags : Byte);
    {-Activate multiple options}
  begin
    SetByteFlag(caOptions, OptionFlags);
  end;

  procedure Calculator.caOptionsOff(OptionFlags : Byte);
    {-Deactivate multiple options}
  begin
    ClearByteFlag(caOptions, OptionFlags);
  end;

  function Calculator.caOptionsAreOn(OptionFlags : Byte) : Boolean;
    {-Return true if all specified options are on}
  begin
    caOptionsAreOn := (caOptions and OptionFlags = OptionFlags);
  end;

  procedure Calculator.SetSubheadAttr(Color, Mono : Byte);
    {-Set attributes for subheads}
  begin
    caSubheadColor := Color;
    caSubheadMono := MapMono(Color, Mono);
  end;

  procedure Calculator.SetHighlightAttr(Color, Mono : Byte);
    {-Set attributes for highlighted characters}
  begin
    caHighlightColor := Color;
    caHighlightMono := MapMono(Color, Mono);
  end;

  function Calculator.GetResult(TypedConst : Boolean) : string;
    {-Return the result of the last calculation as a string, optionally in
      typed constant format. If calculation not finished, value being
      entered is returned}
  var
    S : string[80];

    procedure MakeBcdConstant(var S : string);
      {-Return a typed constant representing the current BCD real of interest}
    var
      I : Word;
      B : BCD;
    begin
      if caStatus = Finished then
        B := caBcdResult
      else begin
        caGetBcdVal(caNumStr2, caBcd2);
        B := caBcd2;
      end;
      S := '(';
      for I := 1 to SizeOf(BCD) do
        S := S+'$'+HexB(B[I])+',';
      S[Length(S)] := ')';
    end;

  begin
    if caBcdMode and TypedConst then
      MakeBcdConstant(S)
    else begin
      if (caStatus = Finished) then
        S := caResultStr
      else
        S := caNumStr2;

      {add radix symbols for Turbo/TASM}
      case caCalcMode of
        Hexadecimal :
          S := '$'+S; {presumably for Turbo}
        Binary :
          S := S+'b'; {presumably for TASM}
      end;
    end;
    GetResult := S;
  end;

{$IFDEF UseStreams}

  constructor Calculator.Load(var S : IdStream);
    {-Load a calculator from a stream}
  begin
    {Load the underlying command window}
    if not CommandWindow.Load(S) then
      Fail;

    {set the command processor if necessary}
    if cwCmdPtr = nil then
      SetCommandProcessor(CalcCommands);

    {Read data specific to the calculator}
    S.Read(caOptions, Ofs(caExpState)-Ofs(caOptions)+SizeOf(caExpState));
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure Calculator.Store(var S : IdStream);
    {-Store a calculator in a stream}
  begin
    {Store the underlying command window}
    CommandWindow.Store(S);
    if S.PeekStatus <> 0 then
      Exit;

    {Write data specific to the calculator}
    S.Write(caOptions, Ofs(caExpState)-Ofs(caOptions)+SizeOf(caExpState));
  end;

  procedure CalculatorStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing calculators}
  begin
    {register the command window}
    CommandWindowStream(SPtr);

    {register the calculator}
    with SPtr^ do begin
      RegisterType(otCalculator, veCalculator, TypeOf(Calculator),
                   @Calculator.Store, @Calculator.Load);
      RegisterPointer(ptCalcCommands, @CalcCommands);
    end;
  end;

{$ENDIF}

begin
  {initialize command processor}
  CalcCommands.Init(@CalcKeySet, CalcKeyMax);
end.

