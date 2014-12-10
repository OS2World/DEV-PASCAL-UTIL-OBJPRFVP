{$IFDEF Windows}
  !! ERROR - This unit is not compatible with Windows !!
{$ENDIF}

{$R-,S-,I-,V-,B-,F+,O+,A-} {!!.01}

{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPQKREF.PAS 1.30                     *}
{*                Quick Reference Chart                  *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpQkRef;
  {-Programmer's Quick Reference Chart}

interface

uses
  Use32,
  Dos,
  OpString,
  OpInline,
  OpConst,      {!!.20}
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd,
  OpFrame,
  OpWindow
  {$IFDEF UseDrag}   {!!.03}
  , OpDrag           {!!.03}
  {$ENDIF}           {!!.03}
  ;

  {$I OPQKREF.ICD}  {configuration data}

const
  DefLeftTee  : Char = 'Ã';       {left T}
  DefCrossBar : Char = 'Ä';       {horizontal bar}
  DefRightTee : Char = '´';       {right T}

  qrMousePage      = $0001;  {clicking on scroll bar scrolls by page}
  qrSuppressBlink  = $0002;  {suppress blinking when displaying attributes?}

  DefQkRefOptions : Word = qrMousePage+qrSuppressBlink;

type
  String20 = string[20];
  QkRefChartPtr = ^QkRefChart;
  QkRefChart =
    object(CommandWindow)
      qrOptions : Word;           {option flags}
      qrFullPage : Byte;          {no. of rows in full page}
      qrWidth : Byte;             {width of window}
      qrRowOfs : Integer;         {vertical offset}
      qrColOfs : Integer;         {horizontal offset}
      qrMaxRowOfs : Integer;      {maximum vertical offset}
      qrMaxColOfs : Integer;      {maximum horizontal offset}
      {...}
      qrDimColor, qrDimMono : Byte;
      qrHighlightColor, qrHighlightMono : Byte;
      {....methods....}
      constructor Init(X1, Y1 , X2, Y2 : Byte);
        {-Initialize the quick reference chart}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             LeftTee, CrossBar, RightTee : Char;
                             var Colors : ColorSet;
                             Options : LongInt);
       {-Initialize the quick reference chart with custom colors and options}
      procedure ProcessSelf; virtual; {!!.01}
        {-Process browse commands}
      {...}
      procedure qrOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure qrOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function qrOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return true if all specified options are on}
      {...}
      procedure SetDimAttr(Color, Mono : Byte);
        {-Set attributes for dim characters}
      procedure SetHighlightAttr(Color, Mono : Byte);
        {-Set attributes for highlighted characters}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a quick ref chart from a stream}
      procedure Store(var S : IdStream);
        {-Store a quick ref chart in a stream}
    {$ENDIF}
{.Z+}
      {+++ internal methods +++}
      procedure UpdateContents; virtual;
      function qrDecimalByte(B : Byte) : String20;
      function qrMonochrome(A : Byte) : String20;
      function qrAscii(A : Byte) : String20;
      function qrRegKey(K : Byte) : String20;
      function qrAuxKey(K : Byte; var A : Byte) : String20;
      function qrScanCode(K : Byte) : String20;
      procedure qrDrawLine(Row, Attr : Byte);
      procedure qrDrawColumnLabels;
      procedure qrScrollVertical(Delta : Integer);
      procedure qrScrollHorizontal(Delta : Integer);
      procedure qrToggleBlink;
      procedure qrReinit;
      procedure qrDrawLinePrim(S : string; Row : Byte; A : string);
      procedure qrMerge(Offset : Byte;
                        var CString : string;
                        DString : string;
                        var AString : string;
                        Attr : Byte);
      {$IFDEF UseScrollBars}
      procedure qrSetupForScrollBars;
      procedure qrUpdateScrollBars;
      {$ENDIF}
      {$IFDEF UseMouse}
      function qrProcessMouseCommand(Cmd : Word) : Boolean;
      {$ENDIF}
{.Z-}
    end;

{.Z+}
  {$IFDEF UseStreams}
  procedure QkRefChartStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing quick ref charts}
  {$ENDIF}
{.Z-}

var
  {$IFDEF UseDrag}                      {!!.03}
  QkRefCommands : DragProcessor;        {!!.03}
  {$ELSE}                               {!!.03}
  QkRefCommands : CommandProcessor;
  {$ENDIF}                              {!!.03}

  {===========================================================}

implementation

const
  DividerRow = 2;
  FullWidth = 78;
type
  String20Ptr = ^String20;

const
  {for displaying video attributes}
  ColorBar : string[5] = '* * *'; {for samples of video attributes}
  ColorWheel : array[Black..White] of String[8] =
  ('Black   ', 'Blue    ', 'Green   ', 'Cyan    ', 'Red     ', 'Magenta ',
    'Brown   ', 'Lt Gray ', 'Dk Gray ', 'Lt Blue ', 'Lt Green', 'Lt Cyan ',
    'Lt Red  ', 'Pink    ', 'Yellow  ', 'White   ');

  {for displaying keys}
  Shift = 'Sh';
  Ctrl = '^';
  Alt : string[3] = 'Alt';

  {For the main display}
  ColumnLabels : string[FullWidth] =
  {         1         2         3         4         5         6         7
   123456789012345678901234567890123456789012345678901234567890123456789012345678
   xxx  x  xxxxxxxx  xx  xxx  xxx xxxxxx   xxxxxxx xxxxx xxxxxxxx xxxxxxx xxxxxxx }
  'Dec Chr  Binary  Hex Ascii Key Extended Scan    Color Foregrnd Backgrnd   Mono ';

{$IFDEF VIRTUALPASCAL}
  {$OrgName+}
  {$L VPQKREF.OBJ}
  {$L VPSCAN.OBJ}
{$ELSE}
  {$L OPQKREF.OBJ}
  {$L OPSCAN.OBJ}
{$ENDIF}

  function EscapeSequence(B : Byte) : String20Ptr; external;
    {-Return a pointer to a text string representing extended scan code B}
  function GetScanCodeName(B : Byte) : String20Ptr; external;
    {-Return a pointer to a text string representing scan code B}

  constructor QkRefChart.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize the quick reference chart}
  begin
    {initialize quick reference chart with default window options}
    if not QkRefChart.InitCustom(X1, Y1, X2, Y2, DefLeftTee, DefCrossBar,
                                 DefRightTee, DefaultColorSet, DefWindowOptions) then
      Fail;
  end;

  constructor QkRefChart.InitCustom(X1, Y1, X2, Y2 : Byte;
                                    LeftTee, CrossBar, RightTee : Char;
                                    var Colors : ColorSet;
                                    Options : LongInt);
    {-Initialize the quick reference chart with custom colors and options}
  begin
    {make sure there's a border}
    SetLongFlag(Options, wUserContents+wBordered);

    {initialize the window}
    if not CommandWindow.InitCustom(X1, Y1+2, X2, Y2, Colors, Options,
                                    QkRefCommands, ucQkRef) then
      Fail;

    {adjust frame coordinates}
    with wFrame do
      AdjustFrameCoords(frXL, frYL-2, frXH, frYH);

    {add window divider}
    if RawError = 0 then {!!.01}
      {add window dividers}
      wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, DividerRow, frTT);

    {check for error}
    if RawError <> 0 then begin           {!!.01}
      InitStatus := RawError;             {!!.01}
      Done;
      Fail;
    end;

    {initialize our own data fields}
    qrOptions := DefQkRefOptions;
    qrRowOfs := 0;
    qrColOfs := 0;
    qrMaxRowOfs := -1;
    qrMaxColOfs := -1;
    qrReinit;

    {set video attributes}
    qrDimColor := Colors.TextColor;
    qrDimMono := Colors.TextMono;
    qrHighlightColor := Colors.HighlightColor;
    qrHighlightMono := Colors.HighlightMono;
  end;

  procedure QkRefChart.qrDrawLinePrim(S : string; Row : Byte; A : string);
    {-Primitive routine to draw a line; accounts for horizontal scrolling}
  var
    SLen : Byte absolute S;
    ALen : Byte absolute A;
    SP, AP : ^string;
    I : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {move text horizontally}
    if qrColOfs > 0 then
      if qrColOfs > Length(S) then begin
        {draw blanks only}
        SLen := 0;
        ALen := 0;
      end
      else begin
        Move(S[qrColOfs+1], S[1], SLen-qrColOfs);
        Move(A[qrColOfs+1], A[1], ALen-qrColOfs);
        Dec(SLen, qrColOfs);
        Dec(ALen, qrColOfs);
      end;

    {fill end of line with blanks if necessary}
    if SLen < qrWidth then begin
      FillChar(S[Succ(SLen)], qrWidth-SLen, ' ');
      FillChar(A[Succ(ALen)], qrWidth-ALen, ColorMono(qrDimColor, qrDimMono));
    end;

    {make sure line isn't too long}
    SLen := qrWidth;
    ALen := qrWidth;

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw the line}
    FastWriteAttr(S, Row, wXL, A);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  function QkRefChart.qrDecimalByte(B : Byte) : String20;
    {-Return B in decimal}
  var
    S : string[3];
  begin
    Str(B:3, S);
    qrDecimalByte := S;
  end;

  function QkRefChart.qrMonochrome(A : Byte) : String20;
    {-Return string representing a monochrome attribute}
  type
    MonoAttrType = (
      Invisible, Reverse, IntUnderline, Intense, DimUnderline, Dim);
  const
    MonoStrings : array[MonoAttrType] of String[7] = (
      'Invis  ', 'Reverse', 'Int Und', 'Intense', 'Dim Und', 'Dim    ');
  var
    MAT : MonoAttrType;
  begin
    case A and $7F of
      $00, $08 :
        MAT := Invisible;
      $70, $78 :
        MAT := Reverse;
      else
        {check intensity bit}
        if A and $08 <> 0 then
          {check underline bit}
          if A and $07 = 1 then
            MAT := IntUnderline
          else
            MAT := Intense
        else
          {check underline bit}
          if A and $07 = 1 then
            MAT := DimUnderline
          else
            MAT := Dim;
    end;
    qrMonochrome := MonoStrings[MAT];
  end;

  function QkRefChart.qrAscii(A : Byte) : String20;
    {-Return a string representing an ASCII character code}
  const
    AsciiChars : array[0..32] of String[3] =
    ('NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL', 'BS ', 'HT ',
      'LF ', 'VT ', 'FF ', 'CR ', 'SO ', 'SI ', 'DLE', 'DC1', 'DC2', 'DC3',
      'DC4', 'NAK', 'SYN', 'ETB', 'CAN', 'EM ', 'SUB', 'ESC', 'FS ', 'GS ',
      'RS ', 'US ', 'SP ');
  begin
    qrAscii := '   ';
    case A of
      0..32 : qrAscii := AsciiChars[A];
      33..126 : qrAscii[1] := Chr(A);
      127 : qrAscii := 'DEL';
    end;
  end;

  function QkRefChart.qrRegKey(K : Byte) : String20;
    {-Return a string representing a regular keystroke}
  begin
    qrRegKey := '   ';
    case K of
      1..31 : begin
                qrRegKey[1] := '^';
                qrRegKey[2] := Chr(K+64);
              end;
      32 : qrRegKey := 'SP ';
      33..126 : qrRegKey[1] := Chr(K);
      127 : qrRegKey := '^BS';
    end;
  end;

  function QkRefChart.qrAuxKey(K : Byte; var A : Byte) : String20;
    {-Return a string representing an auxiliary keystroke}
  const
    EnhancedSet : set of Byte = [
      1, 14, 26..28, 39..41, 43, 51..53, 76, 133..148,
      151..153, 155..157, 159..163, 165];
  var
    S : String[9];
    SLen : Byte absolute S;
  begin
    {convert to a string}
    S := EscapeSequence(K)^;

    {pad the end of the string}
    if SLen < 9 then
      FillChar(S[Succ(SLen)], 9-SLen, ' ');

    {force length to 9 and return the string}
    SLen := 9;
    qrAuxKey := S;

    {select a video attribute}
    case K of
      $E9..$EF :
        A := ColorMono(wTextColor, wTextMono);
      else
        if K in EnhancedSet then
          A := ColorMono(qrHighlightColor, qrHighlightMono)
        else
          A := ColorMono(qrDimColor, qrDimMono);
    end;
  end;

  function QkRefChart.qrScanCode(K : Byte) : String20;
    {-Return a string representing a scan code}
  var
    S : String[7];
    SLen : Byte absolute S;
  begin
    {convert to a string}
    S := GetScanCodeName(K)^;

    {pad the end of the string}
    if SLen < 7 then
      FillChar(S[Succ(SLen)], 7-SLen, ' ');

    {force length to 7 and return the string}
    SLen := 7;
    qrScanCode := S;
  end;

  procedure QkRefChart.qrMerge(Offset : Byte;
                               var CString : string;
                               DString : string;
                               var AString : string;
                               Attr : Byte);
    {-Merge DString into CString at Offset. Fill AString with Attr.}
  var
    DLen : Byte absolute DString;
  begin
    Move(DString[1], CString[Offset], DLen);
    FillChar(AString[Offset], DLen, Attr);
  end;

  procedure QkRefChart.qrDrawLine(Row, Attr : Byte);
    {-Draw one line in the chart}
  var
    NameAttr, Tmp : Byte;
    S, A : string[FullWidth];
    SLen : Byte absolute S;
    ALen : Byte absolute A;
    Bright, Dim : Byte;
  begin
    Bright := ColorMono(wTextColor, wTextMono);
    Dim := ColorMono(qrDimColor, qrDimMono);

    SLen := FullWidth;
    FillChar(S[1], FullWidth, ' ');
    ALen := FullWidth;
    FillChar(A[1], FullWidth, Bright);

    {show attribute names as highlighted if blink bit set}
    if (Attr > 127) and FlagIsSet(qrOptions, qrSuppressBlink) then
      NameAttr := ColorMono(qrHighlightColor, qrHighlightMono)
    else
      NameAttr := Dim;

    {fill in the individual parts of the line}
    qrMerge(01, S, qrDecimalByte(Attr), A, Bright);
    qrMerge(06, S, Chr(Attr),           A, Dim);
    qrMerge(09, S, BinaryB(Attr),       A, Dim);
    qrMerge(19, S, HexB(Attr),          A, Bright);
    qrMerge(23, S, qrAscii(Attr),       A, Dim);
    qrMerge(28, S, qrRegKey(Attr),      A, Dim);
    qrMerge(32, S, qrAuxKey(Attr, Tmp), A, Tmp);
    qrMerge(41, S, qrScanCode(Attr),    A, Dim);
    if FlagIsSet(qrOptions, qrSuppressBlink) then
      Tmp := Attr and $7F
    else
      Tmp := Attr;
    qrMerge(49, S, ColorBar,            A, Tmp);
    Tmp := Attr and $F;
    qrMerge(55, S, ColorWheel[Tmp],     A, NameAttr);
    Tmp := Attr shr 4;
    if FlagIsSet(qrOptions, qrSuppressBlink) then
      Tmp := Tmp and $07;
    qrMerge(64, S, ColorWheel[Tmp],     A, NameAttr);
    qrMerge(72, S, qrMonochrome(Attr),  A, NameAttr);

    {now draw it}
    qrDrawLinePrim(S, Row, A);
  end;

  procedure QkRefChart.qrDrawColumnLabels;
    {-Draw column labels}
  var
    A : Byte;
  begin
    A := ColorMono(wTextColor, wTextMono);
    qrDrawLinePrim(ColumnLabels, wYL-2, CharStr(Char(A), FullWidth));
  end;

  {$IFDEF UseScrollBars}
  procedure QkRefChart.qrSetupForScrollBars;
    {-Set boundaries for all scroll bars}
  begin
    ChangeAllScrollBars(0, qrMaxColOfs, 0, qrMaxRowOfs);
  end;

  procedure QkRefChart.qrUpdateScrollBars;
    {-Update horizontal and vertical scroll bars}
  begin
    DrawAllSliders(qrColOfs, qrRowOfs);
  end;
  {$ENDIF}

  procedure QkRefChart.qrReinit;
    {-Initialize variables that can change if window is resized}
  {$IFDEF UseScrollBars}
  var
    SaveRow, SaveCol : Integer;
  {$ENDIF}
  begin
    {$IFDEF UseScrollBars}
    SaveRow := qrMaxRowOfs;
    SaveCol := qrMaxColOfs;
    {$ENDIF}

    qrFullPage := Succ(wYH-wYL);
    qrMaxRowOfs := 256-qrFullPage;
    if qrRowOfs > qrMaxRowOfs then
      qrRowOfs := qrMaxRowOfs;

    qrWidth := Succ(wXH-wXL);
    qrMaxColOfs := FullWidth-qrWidth;
    if qrColOfs > qrMaxColOfs then
      qrColOfs := qrMaxColOfs;

    {$IFDEF UseScrollBars}
    {do we need to reset boundaries for scroll bars?}
    if (qrMaxRowOfs <> SaveRow) or (qrMaxColOfs <> SaveCol) then
      qrSetupForScrollBars;
    {$ENDIF}
  end;

  procedure QkRefChart.UpdateContents;
    {-Redraw the quick reference chart}
  var
    Row, I : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {initialize variables that can change if window is resized}
    qrReinit;

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw column labels}
    qrDrawColumnLabels;

    I := 0;
    for Row := wYL to wYH do begin
      qrDrawLine(Row, qrRowOfs+I);
      Inc(I);
    end;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    {$IFDEF UseScrollBars}
    {update scroll bars}
    qrUpdateScrollBars;
    {$ENDIF}

    StackWindow.UpdateContents; {!!.01}
  end;

  procedure QkRefChart.qrScrollVertical(Delta : Integer);
    {-Scroll display vertically}
  var
    NewOfs : Integer;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    NewOfs := qrRowOfs+Delta;
    if NewOfs < 0 then
      NewOfs := 0
    else if NewOfs > qrMaxRowOfs then
      NewOfs := qrMaxRowOfs;

    Delta := NewOfs-qrRowOfs;
    qrRowOfs := NewOfs;

    case Delta of
      0 : {do nothing} ;
      1 :
        begin
          {$IFDEF UseMouse}
          HideMousePrim(SaveMouse);
          {$ENDIF}

          ScrollWindowUp(wXL, wYL, wXH, wYH, 1);
          qrDrawLine(wYH, qrRowOfs+qrFullPage-1);

          {$IFDEF UseMouse}
          ShowMousePrim(SaveMouse);
          {$ENDIF}

          {$IFDEF UseScrollBars}
          {update scroll bars}
          qrUpdateScrollBars;
          {$ENDIF}
        end;
      -1 :
        begin
          {$IFDEF UseMouse}
          HideMousePrim(SaveMouse);
          {$ENDIF}

          ScrollWindowDown(wXL, wYL, wXH, wYH, 1);
          qrDrawLine(wYL, qrRowOfs);

          {$IFDEF UseMouse}
          ShowMousePrim(SaveMouse);
          {$ENDIF}

          {$IFDEF UseScrollBars}
          {update scroll bars}
          qrUpdateScrollBars;
          {$ENDIF}
        end;
      else
        UpdateContents;
    end;
  end;

  procedure QkRefChart.qrScrollHorizontal(Delta : Integer);
    {-Scroll display horizontally}
  var
    NewOfs : Integer;
  begin
    NewOfs := qrColOfs+Delta;
    if NewOfs < 0 then
      NewOfs := 0
    else if NewOfs > qrMaxColOfs then
      NewOfs := qrMaxColOfs;           {!!.11}

    if NewOfs <> qrColOfs then begin
      qrColOfs := NewOfs;
      UpdateContents;
    end;
  end;

  procedure QkRefChart.qrToggleBlink;
    {-Toggle attribute blinking on/off}
  begin
    if FlagIsSet(qrOptions, qrSuppressBlink) then
      ClearFlag(qrOptions, qrSuppressBlink)
    else
      SetFlag(qrOptions, qrSuppressBlink);
    SetBlink(FlagIsSet(qrOptions, qrSuppressBlink));
    UpdateContents;
  end;

  {$IFDEF UseMouse}
  function QkRefChart.qrProcessMouseCommand(Cmd : Word) : Boolean;
    {-Process ccMouseSel command. Returns True to return control to user.}
  var
    L : LongInt;
    FramePos : FramePosType;
    HotCode : Byte;
    Dragging : Boolean; {!!.03}
  begin
    qrProcessMouseCommand := False;

    {determine position of mouse}
    L := cwMouseResults(Cmd, FramePos, HotCode);    {!!.03} {!!.13}

    {Should mouse event be ignored?}                             {!!.03}
    if cwIgnoreMouseEvent(Dragging, Cmd, FramePos, HotCode) then {!!.03}
      Exit;                                                      {!!.03}

    case HotCode of
      hsNone :               {not a hot spot}
        case FramePos of
          frInsideActive :   {inside window}
            {does nothing in this unit} ;

          frTL..frRR,        {on the frame}
          frInsideFrame,     {inside window frame but not in window boundaries}
          frOutsideFrame :   {outside window frame}
            qrProcessMouseCommand := LongFlagIsSet(wFlags, wAllMouseEvents);
        end;

      {$IFDEF UseScrollBars}
      hsDecV :               {the decrement fixture of a vertical scroll bar}
        if FlagIsSet(qrOptions, qrMousePage) then
          qrScrollVertical(-qrFullPage)
        else
          qrScrollVertical(-10);
      hsDecH :               {the decrement fixture of a horizontal scroll bar}
        qrScrollHorizontal(-10);
      hsIncV :               {the increment fixture of a vertical scroll bar}
        if FlagIsSet(qrOptions, qrMousePage) then
          qrScrollVertical(+qrFullPage)
        else
          qrScrollVertical(+1);
      hsIncH :               {the increment fixture of a horizontal scroll bar}
        qrScrollHorizontal(+10);
      hsBar :                {the slider portion of a scroll bar}
        case FramePos of
          frLL, frRR :       {vertical scroll bar}
            begin
              L := TweakSlider(FramePos, MouseKeyWordY+MouseYLo, L, 1);
              qrScrollVertical(L-qrRowOfs);
            end;
          else begin         {horizontal scroll bar}
            L := TweakSlider(FramePos, MouseKeyWordX+MouseXLo, L, 1);
            qrScrollHorizontal(L-qrColOfs);
          end;
        end;
      {$ENDIF}

      hsSpot,                {a single character hot spot}
      hsRegion0..255 :       {a user-defined region relative to a frame}
        qrProcessMouseCommand := (Cmd <> ccMouseAuto); {!!.03}
    end;
  end;
  {$ENDIF}

  procedure QkRefChart.ProcessSelf;   {!!.01}
    {-Process browse commands}
  var
    Ch : Char;
    AllDone : Boolean;
    {$IFDEF UseMouse}
    {SaveMouse : Boolean;}      {!!.01}
    {$ENDIF}
  begin
    (*                          {!!.01}
    cwCmd := ccError;
    if cwGetLastError <> 0 then
      Exit;
    *)

    {turn blinking on or off}
    SetBlink(FlagIsSet(qrOptions, qrSuppressBlink));

    {$IFDEF UseScrollBars}
    {make sure we're set up for scroll bars}
    qrSetupForScrollBars;
    {$ENDIF}

    {Draw initial screen if not already done}
    Draw;
    if RawError <> 0 then begin             {!!.01}
      {GotError(wNotCurrent, emNullError);} {!!.01}
      Exit;
    end;

    (*                                      {!!.01}
    {$IFDEF UseMouse}
    SaveMouse := MouseCursorOn;
    if cwCmdPtr^.MouseEnabled then
      ShowMouse;
    {$ENDIF}
    *)

    {hide the cursor}
    SetCursor(cuHidden);

    AllDone := False;
    repeat
      {get the next command}
      GetNextCommand;

      case cwCmd of
        ccUp :
          qrScrollVertical(-1);
        ccDown :
          qrScrollVertical(+1);
        ccPageUp :
          qrScrollVertical(-qrFullPage);
        ccPageDn :
          qrScrollVertical(+qrFullPage);
        ccTopOfFile :
          qrScrollVertical(-255);
        ccEndOfFile :
          qrScrollVertical(+255);
        ccLeft :
          qrScrollHorizontal(-1);
        ccRight :
          qrScrollHorizontal(+1);
        ccWordLeft :
          qrScrollHorizontal(-10);
        ccWordRight :
          qrScrollHorizontal(+10);
        ccHome :
          qrScrollHorizontal(-qrColOfs);
        ccEnd :
          qrScrollHorizontal(+qrMaxColOfs);
        ccBlinkToggle :
          qrToggleBlink;
        ccHelp :
          RequestHelp(wHelpIndex);
        ccError,
        ccQuit,
        ccUser0..ccUser65335 :
          AllDone := True;
        {$IFDEF UseMouse}
        ccMouseAuto,                             {!!.03}
        ccMouseDown,                             {!!.03}
        ccMouseSel :
          AllDone := qrProcessMouseCommand(cwCmd); {!!.03}
        {$ENDIF}
        else if (cwCmd <= 255) and (GetExitCommandPtr <> nil) then      {!!.01}
          {Possibly a special exit command defined by a derived object} {!!.01}
          AllDone := (cwCmd in GetExitCommandPtr^);                     {!!.01}
      end;

    until AllDone;

    {save window state}
    rwSaveWindowState;

    {$IFDEF UseMouse}
    {ShowMousePrim(SaveMouse);}     {!!.01}
    {$ENDIF}
  end;

  procedure QkRefChart.qrOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    SetFlag(qrOptions, OptionFlags); {!!.12}
  end;

  procedure QkRefChart.qrOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    ClearFlag(qrOptions, OptionFlags); {!!.12}
  end;

  function QkRefChart.qrOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return true if all specified options are on}
  begin
    qrOptionsAreOn := (qrOptions and OptionFlags = OptionFlags);
  end;

  procedure QkRefChart.SetDimAttr(Color, Mono : Byte);
    {-Set attributes for dim characters}
  begin
    qrDimColor := Color;
    qrDimMono := MapMono(Color, Mono);
  end;

  procedure QkRefChart.SetHighlightAttr(Color, Mono : Byte);
    {-Set attributes for highlighted characters}
  begin
    qrHighlightColor := Color;
    qrHighlightMono := MapMono(Color, Mono);
  end;

{$IFDEF UseStreams}

  constructor QkRefChart.Load(var S : IdStream);
    {-Load a quick ref chart from a stream}
  begin
    {Load the underlying command window}
    if not CommandWindow.Load(S) then
      Fail;

    {set the command processor if necessary}
    if cwCmdPtr = nil then
      SetCommandProcessor(QkRefCommands);

    {Read data specific to the quick ref chart}
    S.Read(qrOptions,
            Ofs(qrHighlightMono)-Ofs(qrOptions)+SizeOf(qrHighlightMono) );

    {check status}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure QkRefChart.Store(var S : IdStream);
    {-Store a quick ref chart in a stream}
  begin
    {Store the underlying command window}
    CommandWindow.Store(S);
    if S.PeekStatus <> 0 then
      Exit;

    {Write data specific to the quick ref chart}
    S.Write(qrOptions,
            Ofs(qrHighlightMono)-Ofs(qrOptions)+SizeOf(qrHighlightMono) );
  end;

  procedure QkRefChartStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing quick ref charts}
  begin
    {register the command window}
    CommandWindowStream(SPtr);

    {register the quick ref chart}
    with SPtr^ do begin
      RegisterType(otQkRefChart, veQkRefChart, TypeOf(QkRefChart),
                   @QkRefChart.Store, @QkRefChart.Load);
      RegisterPointer(ptQkRefCommands, @QkRefCommands);
    end;
  end;

{$ENDIF}

begin
  {initialize command processor}
  QkRefCommands.Init(@QkRefKeySet, QkRefKeyMax);
end.
