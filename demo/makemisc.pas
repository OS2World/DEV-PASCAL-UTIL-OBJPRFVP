{$R-,S-,I-,V-,B-,F-,O-,T-}

{*********************************************************}
{*                  MAKEMISC.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This unit requires that OPDEFINE.INC activate the following defines:
   UseScrollBars, UseHotSpots, UseAdjustableWindows, UseShadows
 This unit will use features activated with the following defines:
   UseMouse, UseBcd, N+, UseDates,
 ***************************************************************************}

{$IFNDEF UseScrollBars}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

{$IFNDEF UseHotSpots}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

{$IFNDEF UseShadows}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

unit MakeMisc;
  {-Miscellaneous routines, objects for MAKESCRN and MAKEMENU}

interface

{!!.20 Library renamed to OpLibrary throughout}

uses
  Use32,
{$IFDEF VIRTUALPASCAL}
  VpKeyb,
{$ENDIF}
  Dpmi,       {!!.20}
  dos,
  opinline,
  opconst,    {!!.20}
  oproot,
  opcmd,
  opcrt,
  {$IFDEF UseMouse}
  opmouse,
  {$ENDIF}
  opstring,
  opframe,
  opwindow,
  oppick;

const
  ucMakeMisc = 40;                {unit code}

  SglCrossBar = 'Ä';
  SglLeftTee  = 'Ã';
  SglRightTee = '´';
  DblCrossBar = 'Í';
  DblLeftTee  = 'Æ';
  DblRightTee = 'µ';
  DefCrossBar : Char = SglCrossBar;
  DefLeftTee  : Char = SglLeftTee;
  DefRightTee : Char = SglRightTee;

type
  csArray = array[0..255] of string[5];
  csArrayPtr = ^csArray;
  CharSelector =
    object(PickList)
      csItemPtr : csArrayPtr;

      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the char selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc);
        {-Initialize the char selector with custom options, colors}
      destructor Done; virtual;
        {-Dispose of char selector}

      procedure SetInitialCharChoice(Ch : Char);
        {-Set initial choice without scrolling if possible}
      function GetLastCharChoice : Char;
        {-Get last choice}

      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
      {+++internal methods+++}
      procedure csItemStringPrim(I : Word; var IString : String);
    end;

  HeaderPosSet = set of HeaderPosType;
  HeaderArray  = array[1..100] of HeaderPtr;

const
  DefHeaderPosSet : HeaderPosSet = [heTL, heTC, heTR, heBL, heBC, heBR];

type
  HeaderSelector =
    object(PickList)
      hsHeaders    : ^HeaderArray;
      hsCount      : Word;
      hsLabelColor : Byte;
      hsLabelMono  : Byte;

      constructor Init(X1, Y1, X2, Y2, MinW : Byte; var F : Frame);
        {-Initialize the header selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc;
                             CommandHandler : pkGenlProc;
                             var F : Frame;
                             MinW : Byte;
                             LeftTee, CrossBar, RightTee : Char;
                             HPS : HeaderPosSet);
        {-Initialize the header selector with custom options, colors}
      procedure UpdateContents; virtual;
        {-Redraw the header selector}
      destructor Done; virtual;
        {-Dispose of header selector}

      function GetLastHeader : HeaderPtr;
        {-Get last choice}

      procedure SetLabelAttr(Color, Mono : Byte);
        {-Set color for column labels}

      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
    end;

  HeaderTypeSelector =
    object(PickList)
      htsLo, htsHi : HeaderPosType;

      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the header type selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc;
                             Low, High : HeaderPosType);
        {-Initialize the header type selector with custom options, colors}

      function GetLastHeaderType : HeaderPosType;
        {-Get last choice}

      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
    end;

  FrameSelector =
    object(PickList)
      frBoxColor : Byte;
      frBoxMono : Byte;

      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the frame selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc;
                             LeftTee, CrossBar, RightTee : Char);
        {-Initialize the frame selector with custom options, colors}
      procedure UpdateContents; virtual;
        {-Redraw the frame selector}

      procedure SetInitialFrameChoice(var FA : FrameArray);
        {-Set initial choice without scrolling if possible}
      procedure GetLastFrameChoice(var FA : FrameArray);
        {-Get last choice}
      procedure SetBoxAttr(Color, Mono : Byte);
        {-Set color for selection box}

      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
      procedure PreMove; virtual;
        {-Called just prior to getting each keyboard command}
    end;

const
  csHeight = 18;
type
  ColorSelector =
    object(CommandWindow)
      csMax  : Byte;
      csMax0 : Byte;
      csWidth : Byte;
      csColor : Byte;
      csBoxColor : Byte;
      csBoxMono : Byte;
      csBoxAttr : Byte;
      csIndex   : Byte;

      constructor Init(X1, Y1 : Byte; ShowHigh : Boolean);
        {-Initialize the color selector}
      constructor InitCustom(X1, Y1 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             ShowHigh : Boolean);
       {-Initialize the color selector with custom colors and options}
      procedure UpdateContents; virtual;
        {-Redraw the color selector}
      procedure ProcessSelf; virtual;
        {-Select a color}
      {...}
      procedure SetColor(Color : Byte);
        {-Set default color choice}
      function GetColor : Byte;
        {-Get last color choice}
      procedure SetBoxAttr(Color, Mono : Byte);
        {-Set color for selection box}

      {+++internal methods+++}
      procedure csCalcRowCol(Attr : Byte; var Row, Col : Byte);
      function  csCalcColor(Row, Col : Byte) : Byte;
    end;

  ByteSet = set of Byte;
  lsObjectArray = array[1..512] of Word; {size is arbitrary}
  OpLibrarySelector =
    object(PickList)
      lsLib        : ^OpLibrary;
      lsCount      : Word;
      lsObjects    : ^lsObjectArray;
      lsSize       : Word;
      lsLabelColor : Byte;
      lsLabelMono  : Byte;

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       var Lib : OpLibrary;
                       TypeCodes : ByteSet;
                       AllowNew : Boolean);
        {-Initialize the library selector using default colors and options}
      constructor InitMulti(X1, Y1, X2, Y2 : Byte;
                            var Lib : OpLibrary;
                            TypeCodes : ByteSet;
                            AllowNew : Boolean);
        {-Initialize the library selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc;
                             CommandHandler : pkGenlProc;
                             var Lib : OpLibrary;
                             TypeCodes : ByteSet;
                             FlagFilter : Word;
                             AllowNew : Boolean;
                             LeftTee, CrossBar, RightTee : Char);
        {-Initialize the library selector with custom options, colors}
      destructor Done; virtual;
        {-Dispose of library selector}
      procedure UpdateContents; virtual;
        {-Redraw the library selector}

      procedure DeleteMarkedItems;
        {-Delete all marked items}

      function GetLastObjectChoice : Word;
        {-Get last choice. Return value is an index into the library's
          directory. 0 means '--- New ----' was selected.}

      function ChangeLibraries(var Lib : OpLibrary; TypeCodes : ByteSet;
                               FlagFilter : Word; AllowNew : Boolean) : Boolean; virtual;
        {-Switch libraries and or object types. TypeCodes is the set of OPROOT
          type codes for objects of interest, or []. FlagFilter is normally
          'deUnused+deDeleted', meaning "don't display deleted or unused
          entries". AllowNew indicates whether '--- New ---' should be
          displayed as the last choice in the pick list.}
      procedure SetLabelAttr(Color, Mono : Byte);
        {-Set color for column labels}

      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}

      {+++ internal methods +++}
      procedure MarkDeletedItems;
    end;

  ColorSetSelector =                       {!!.03}
    object(OpLibrarySelector)
      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
    end;

  OpLibraryInfo =
    object(StackWindow)
      liLib         : ^OpLibrary;
      liBrightColor : Byte;
      liBrightMono  : Byte;
      liDimColor    : Byte;
      liDimMono     : Byte;

      constructor Init(X1, Y1 : Byte; var Lib : OpLibrary; LibName : PathStr);
        {-Initialize the library info window using default colors and options}
      constructor InitCustom(X1, Y1 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             var Lib : OpLibrary;
                             LibName : PathStr);
        {-Initialize the library info window with custom options, colors}
      procedure UpdateContents; virtual;
        {-Redraw the library info window}
      procedure Process; virtual;
        {-Display the library info window and wait for a keypress}
    end;

{$IFDEF UseMouse}

const
  MapMouseToCursor : Boolean = True;

procedure SlowestMouse;
  {-Slow mouse down a lot}

procedure SlowMouse;
  {-Slow mouse down}

procedure FastMouse;
  {-Let mouse move at normal speed}

{$ENDIF}

function ReadKeyWordXY : Word;
  {-ReadKeyWord with mouse movement mapped to cursor movement and detection
    of ScrollLock}

function MoveOrResize(var RW : RawWindow;
                      MoveOnly : Boolean;
                      var Modified : Boolean) : Boolean;
  {-Adjust the size/location of a window. Returns False for insufficient
    memory}

function Decimal(N : LongInt; Width : Byte) : String;
  {-Return a decimal number right-padded to the specified width}

function MemInUse : LongInt;
  {-Returns amount of heap space in use}

{------- routines used when generating source code ----------}

const
  HeaderPosAbbrevs : array[HeaderPosType] of string[8] = (        {!!.01}
    'heCustom', 'heSpan', 'heTL', 'heTC', 'heTR', 'heBL', 'heBC', 'heBR');
  GenColorNames : Boolean = True;
  GenWFrame : Boolean = False;

  NamesInColorSet = 31;
  ColorSetNames : array[1..NamesInColorSet] of string[10] = (
    'Text', 'Ctrl', 'Frame', 'Header', 'Shadow',
    'Highlight', 'Prompt', 'SelPrompt', 'ProPrompt', 'Field',
    'SelField', 'ProField', 'ScrollBar', 'Slider', 'HotSpot',
    'Block', 'Marker', 'Delim', 'SelDelim', 'ProDelim',
    'SelItem', 'ProItem', 'HighItem', 'AltItem', 'AltSelItem',
    'FlexAHelp', 'FlexBHelp', 'FlexCHelp', 'UnselXref', 'SelXref',
    'Mouse');

function ColorName(Attr : Byte) : String;
  {-Return a readable name for an attribute}

function PascalString(S : string) : string;
  {-Convert S to a valid Pascal string, including appropriate quotes}

function PascalCtrlString(S : String) : String;
  {-Return a safe-to-write string for something that may contain ctrl chars}

function PascalChar(Ch : Char) : string;
  {-Convert Ch to a Pascal character}

function PascalFrame(FrCh : FrameArray) : string;
  {-Return a safe-to-write string for frame characters}

function WindowOptions(var Win : RawWindow) : string;
  {-Return the window options in a readable string}

procedure GenerateExploding(var OutF : Text;
                            var Win : RawWindow;
                            Indent : Word);
  {-Generate call to enable explosions for a window}

procedure GenerateShadows(var OutF : Text;
                          var Win : RawWindow;
                          Indent : Word);
  {-Generate call to draw shadows on a window}

procedure GenerateHeaders(var OutF : Text;
                          var Win : RawWindow;
                          Indent : Word);
  {-Generate calls to draw headers}

  {.F-}
const
  ccCenter     = ccUser20;
  ccHoriz      = ccUser21;
  ccVert       = ccUser22;

  {Keystroke to command mapping}
  MakeMiscKeyMax  = 200;
  MakeMiscKeySet  : array[0..MakeMiscKeyMax] of Byte = (
  {length keys         command type      key sequence}
  3,      $00, $0F,    ccBackTab,        {Shift-Tab}
  3,      $00, $23,    ccHoriz,          {AltH}
  3,      $00, $2E,    ccCenter,         {AltC}
  3,      $00, $2F,    ccVert,           {AltV}
  3,      $00, $3B,    ccHelp,           {F1}
  3,      $00, $47,    ccHome,           {Home}
  3,      $00, $48,    ccUp,             {Up}
  3,      $00, $49,    ccPageUp,         {PgUp}
  3,      $00, $4B,    ccLeft,           {Left}
  3,      $00, $4D,    ccRight,          {Right}
  3,      $00, $4F,    ccEnd,            {End}
  3,      $00, $50,    ccDown,           {Down}
  3,      $00, $51,    ccPageDn,         {PgDn}
  3,      $00, $73,    ccWordLeft,       {^Left}
  3,      $00, $74,    ccWordRight,      {^Right}
  3,      $00, $75,    ccScreenBot,      {^End}
  3,      $00, $76,    ccEndOfFile,      {^PgDn}
  3,      $00, $77,    ccScreenTop,      {^Home}
  3,      $00, $84,    ccTopOfFile,      {^PgUp}
  3,      $00, $D0,    ccUpLeft,         {* diagonal cursor movement *}
  3,      $00, $D1,    ccDownLeft,       {* commands generated by    *}
  3,      $00, $D2,    ccUpRight,        {* ReadKeyWordXY when mouse *}
  3,      $00, $D3,    ccDownRight,      {* is moved diagonally      *}
  2,      $01,         ccWordLeft,       {^A}
  2,      $03,         ccPageDn,         {^C}
  2,      $04,         ccRight,          {^D}
  2,      $05,         ccUp,             {^E}
  2,      $06,         ccWordRight,      {^F}
  2,      $09,         ccTab,            {^I, Tab}
  2,      $0D,         ccSelect,         {^M, Enter}
  2,      $12,         ccPageUp,         {^R}
  2,      $13,         ccLeft,           {^S}
  2,      $17,         ccUp,             {^W}
  2,      $18,         ccDown,           {^X}
  2,      $1A,         ccDown,           {^Z}
  2,      $1B,         ccQuit,           {Esc}
  3,      $11, $03,    ccEndOfFile,      {^Q^C}
  3,      $11, $04,    ccEnd,            {^Q^D}
  3,      $11, $05,    ccScreenTop,      {^Q^E}
  3,      $11, $12,    ccTopOfFile,      {^Q^R}
  3,      $11, $13,    ccHome,           {^Q^S}
  3,      $11, $18,    ccScreenBot,      {^Q^X}
  {$IFDEF UseMouse}
  3,      $00, $EF,    ccMouseSel,       {Click left}
  3,      $00, $EE,    ccQuit,           {Click right}
  3,      $00, $ED,    ccHelp,           {Click both}
  {$ELSE}
              0, 0, 0, 0, 0, 0,          {160}
  0, 0, 0, 0, 0, 0,                      {170}
  {$ENDIF}
                    0, 0, 0, 0,          {170}
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,          {180}
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,          {190}
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0           {200}
  );
  {.F+}
var
  MakeMiscCommands : CommandProcessor;

  {===========================================================}

implementation

var
  KbdFlagsPtr : ^Word;     {!!.20}

const
  MaxFrameTypes = 25;
  NoFrame = #255+#255+#255+#255+#255+#255+#255+#255;
  FrameTypes : array[1..MaxFrameTypes] of FrameArray = (
    NoFrame,                  {null frame}
    {13 line-type frames}
    'ÚÀ¿ÙÄÄ³³',               {single-line}
    'ÉÈ»¼ÍÍºº',               {double-line}
    'ÕÔ¸¾ÍÍ³³',               {single vertical, double horizontal}
    'ÖÓ·½ÄÄºº',               {double v, single h}
    'ÂÀÂÙÄÄ³³',               {single-line, flat top}
    'ËÈË¼ÍÍºº',               {double-line, flat top}
    'ÑÔÑ¾ÍÍ³³',               {single v, double h, flat top}
    'ÒÓÒ½ÄÄºº',               {double v, single h, flat top}
    'ÚÁ¿ÁÄÄ³³',               {single-line, flat bottom}
    'ÉÊ»ÊÍÍºº',               {double-line, flat bottom}
    'ÕÏ¸ÏÍÍ³³',               {single v, double h, flat bottom}
    'ÖÐ·ÐÄÄºº',               {double v, single h, flat bottom}
    '++++--||',               {printable ASCII}
    {4 solid frames}
    'ÛÛÛÛÛÛÛÛ',               {solid block}
    '°°°°°°°°',               {polka-dot block #1}
    '±±±±±±±±',               {polka-dot block #2}
    '²²²²²²²²',               {polka-dot block #3}
    {7 semi-solid frames}
    'þþþþþþþþ',               {squares}
    '********',               {asterisks}
    ^O^O^O^O^O^O^O^O,         {snowflakes}
    ^D^D^D^D^D^D^D^D,         {diamonds}
    ^G^G^G^G^G^G^G^G,         {dots 1}
    'ùùùùùùùù',               {dots 2}
    'úúúúúúúú');              {dots 3}
  ColorChoice : String[3] = ' * ';

var                        {!!.20}
  TotalAvail : LongInt;    {!!.20}

  {$I MAKEMISC.IN1}

  {------------ CharSelector methods ----------}

  constructor CharSelector.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize the char selector using default colors and options}
  begin
    if not CharSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions, PickSnaking) then
        Fail;
  end;

  constructor CharSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                      var Colors : ColorSet;
                                      Options : LongInt;
                                      Orientation : pkGenlProc);
    {-Initialize the char selector with custom options, colors}
  var
    I : Word;
  begin
    csItemPtr := nil;

    {initialize the pick list}
    if not PickList.InitAbstract(X1, Y1, X2, Y2, Colors, Options, 9, 256,
                                 Orientation, SingleChoice) then
      Fail;

    {try to pre-calculate the strings and store them on the heap}
    if GetMemCheck(Pointer(csItemPtr), SizeOf(csArray)) then
      for I := 0 to 255 do
        csItemStringPrim(I, csItemPtr^[I]);
  end;

  destructor CharSelector.Done;
    {-Dispose of char selector}
  begin
    FreeMemCheck(csItemPtr, SizeOf(csArray));
    PickList.Done;
  end;

  procedure CharSelector.ItemString(Item : Word;
                                    Mode : pkMode;
                                    var IType : pkItemType;
                                    var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  var
    ILen : Byte absolute IString;
  begin
    if csItemPtr <> nil then
      IString := csItemPtr^[Item-1]
    else
      csItemStringPrim(Item-1, IString);
    if pkOptionsAreOn(pkSetDefault) then
      if Item = GetDefaultChoice then
        IType := pkAlternate;
    IString := '  '+IString+'  ';
  end;

  procedure CharSelector.SetInitialCharChoice(Ch : Char);
    {-Set initial choice without scrolling if possible}
  begin
    SetInitialChoice(Ord(Ch)+1);
  end;

  function CharSelector.GetLastCharChoice : Char;
    {-Get last choice}
  begin
    GetLastCharChoice := Char(GetLastChoice-1);
  end;

  procedure CharSelector.csItemStringPrim(I : Word; var IString : String);
    {-Low-level routine to return the item string for item I}
  var
    Ch : Char absolute I;
  begin
    case Ch of
      #00 : IString := '0  #0';
      ' ' : IString := '32 SP';
      else  IString := Decimal(I, 4)+Ch;
    end;
  end;

  {------------ HeaderSelector methods ---------}
  constructor HeaderSelector.Init(X1, Y1, X2, Y2, MinW : Byte; var F : Frame);
    {-Initialize the header selector using default colors and options}
  begin
    if not HeaderSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions, PickVertical,
      SingleChoice, F, MinW, DefLeftTee, DefCrossBar, DefRightTee,
      DefHeaderPosSet) then
        Fail;
  end;

  constructor HeaderSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                        var Colors : ColorSet;
                                        Options : LongInt;
                                        Orientation : pkGenlProc;
                                        CommandHandler : pkGenlProc;
                                        var F : Frame;
                                        MinW : Byte;
                                        LeftTee, CrossBar, RightTee : Char;
                                        HPS : HeaderPosSet);
    {-Initialize the header selector with custom options, colors}
  var
    P : HeaderPtr;
    I, J, W : Word;
  begin
    hsCount := 0;
    hsHeaders := nil;

    {calculate maximum width}
    W := MinW;
    P := HeaderPtr(F.frHeaders.Head);
    for I := 1 to F.frHeaders.Size do begin
      if (P^.heType in HPS) then begin
        Inc(hsCount);
        W := MaxWord(W, Length(P^.heName^)+13);
      end;
      P := HeaderPtr(P^.dlNext);
    end;
    if (hsCount = 0) then begin
      InitStatus := epFatal+ecQueueEmpty; {???}
      Fail
    end
    else if (W = 0) then
      Fail;

    if X1+Pred(W) < X2 then
      X2 := X1+Pred(W);

    {allocate and initialize the header array}
    if not GetMemCheck(hsHeaders, hsCount*SizeOf(Pointer)) then
      Fail;
    P := HeaderPtr(F.frHeaders.Head);
    J := 1;
    for I := 1 to F.frHeaders.Size do begin
      if (P^.heType in HPS) then begin
        hsHeaders^[J] := P;
        Inc(J);
      end;
      P := HeaderPtr(P^.dlNext);
    end;

    {initialize the pick list}
    if not PickList.InitAbstract(X1, Y1+2, X2, Y2, Colors, Options,
                                 W, hsCount, Orientation, CommandHandler) then
      Fail;

    {adjust frame coordinates}
    with wFrame do
      AdjustFrameCoords(frXL, frYL-2, frXH, frYH);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {add window divider}
    wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, 2, frTT);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {set attribute for box}
    hsLabelColor := Colors.TextColor;
    hsLabelMono := Colors.TextMono;
  end;

  destructor HeaderSelector.Done;
    {-Dispose of header selector}
  begin
    FreeMemCheck(hsHeaders, hsCount*SizeOf(Pointer));
    PickList.Done;
  end;

  procedure HeaderSelector.SetLabelAttr(Color, Mono : Byte);
    {-Set color for column labels}
  begin
    hsLabelColor := Color;
    hsLabelMono := MapMono(Color, Mono);
  end;

  function HeaderSelector.GetLastHeader : HeaderPtr;
    {-Get last choice}
  begin
    GetLastHeader := hsHeaders^[GetLastChoice];
  end;

  procedure HeaderSelector.UpdateContents;
    {-Redraw the header selector}
  const
    Headers = ' Position   Text';
  var
    I : Word;
    S : string;
    SLen : Byte absolute S;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw the column headers}
    I := Succ(wXH-wXL);
    S := Pad(Headers, I);
    if SLen > I then
      SLen := I;
    FastWrite(S, wYL-2, wXL, ColorMono(hsLabelColor, hsLabelMono));

    {update the pick list}
    PickList.UpdateContents;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure HeaderSelector.ItemString(Item : Word;
                                      Mode : pkMode;
                                      var IType : pkItemType;
                                      var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  begin
    with hsHeaders^[Item]^ do begin
      IString := ' '+Pad(HeaderPosAbbrevs[heType], 9);
      IString := IString+''''+heName^+''' ';
    end;
  end;

  {------------ HeaderTypeSelector methods ---------}
const
  HeaderPosStrings : array[HeaderPosType] of string[13] = (
    'Custom', 'Span', 'Top left', 'Top center', 'Top right',
    'Bottom left', 'Bottom center', 'Bottom right');

  constructor HeaderTypeSelector.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize the header type selector using default colors and options}
  begin
    if not HeaderTypeSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, heTL, heBR) then
        Fail;
  end;

  constructor HeaderTypeSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                         var Colors : ColorSet;
                         Options : LongInt;
                         Orientation : pkGenlProc;
                         Low, High : HeaderPosType);
    {-Initialize the header type selector with custom options, colors}
  var
    W : Word;
    HPT : HeaderPosType;
  begin
    {heCustom, heSpan, heTL, heTC, heTR, heBL, heBC, heBR}
    if Low > High then begin
      htsLo := High;
      htsHi := Low;
    end
    else begin
      htsLo := Low;
      htsHi := High;
    end;

    {calculate maximum width}
    W := 0;
    for HPT := htsLo to htsHi do
      W := MaxWord(W, Length(HeaderPosStrings[HPT])+2);

    {initialize the pick list}
    if not PickList.InitAbstract(X1, Y1, X2, Y2, Colors, Options,
                                 W, Succ(Ord(htsHi)-Ord(htsLo)),
                                 Orientation, SingleChoice) then
      Fail;
  end;

  function HeaderTypeSelector.GetLastHeaderType : HeaderPosType;
    {-Get last choice}
  begin
    GetLastHeaderType := HeaderPosType(Ord(htsLo)+GetLastChoice-1);
  end;

  procedure HeaderTypeSelector.ItemString(Item : Word;
                                          Mode : pkMode;
                                          var IType : pkItemType;
                                          var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  var
    HPT : HeaderPosType;
  begin
    HPT := HeaderPosType(Ord(htsLo)+Item-1);
    IString := ' '+HeaderPosStrings[HPT]+' ';
  end;

  {------------ FrameSelector methods ----------}

  constructor FrameSelector.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize the frame selector using default colors and options}
  begin
    if not FrameSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;
  end;

  constructor FrameSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                       var Colors : ColorSet;
                                       Options : LongInt;
                                       Orientation : pkGenlProc;
                                       LeftTee, CrossBar, RightTee : Char);
    {-Initialize the frame selector with custom options, colors}
  begin
    {make sure there's a frame and that the window isn't resized}
    ClearLongFlag(Options, wResizeable);
    SetLongFlag(Options, wBordered+wClear);

    {initialize the pick list}
    if not PickList.InitAbstract(
      X1, Y1+4, X2, Y2, Colors, Options, 12, MaxFrameTypes,
      Orientation, SingleChoice) then
        Fail;

    {adjust frame coordinates}
    with wFrame do
      AdjustFrameCoords(frXL, frYL-4, frXH, frYH);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {add window divider}
    wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, 4, frTT);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {set attribute for box}
    frBoxColor := Colors.FrameColor;
    frBoxMono := Colors.FrameMono;
  end;

  procedure FrameSelector.UpdateContents;
    {-Redraw the frame selector}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {clear the top part of the window}
    OpCrt.ClearWindow(wXL, wYL-4, wXH, wYL-2, wBackChar,
      ColorMono(wTextColor, wTextMono));

    {update the pick list}
    PickList.UpdateContents;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure FrameSelector.SetBoxAttr(Color, Mono : Byte);
    {-Set color for selection box}
  begin
    frBoxColor := Color;
    frBoxMono := MapMono(Color, Mono);
  end;

  procedure FrameSelector.PreMove;
    {-Update the top of the frame selector window}
  var
    FA : Byte;
    X1, Y1, X2, Y2 : Byte;
    FR : FrameArray;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {calculate coordinates for frame}
    X1 := (wXH+wXL) shr 1 - 2;
    Y1 := wYL-4;
    X2 := X1+5;
    Y2 := Y1+2;

    {get attribute to use}
    FA := ColorMono(frBoxColor, frBoxMono);

    {get frame to draw}
    GetLastFrameChoice(FR);

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw the frame}
    ExchangeStructs(FR, FrameChars, SizeOf(FrameArray));
    FrameWindow(X1, Y1, X2, Y2, FA, FA, '');
    ExchangeStructs(FR, FrameChars, SizeOf(FrameArray));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure FrameSelector.ItemString(Item : Word;
                                     Mode : pkMode;
                                     var IType : pkItemType;
                                     var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  begin
    if Item = 1 then begin
      IString := '  --none--  ';
      IType := pkAlternate;
    end
    else
      IString := '  '+FrameTypes[Item]+'  ';
  end;

  procedure FrameSelector.SetInitialFrameChoice(var FA : FrameArray);
    {-Set initial choice without scrolling if possible}
  var
    I : Word;
  begin
    for I := 2 to MaxFrameTypes do
      if FA = FrameTypes[I] then begin
        SetInitialChoice(I);
        Exit;
      end;

    {no luck--set to no frame}
    SetInitialChoice(1);
  end;

  procedure FrameSelector.GetLastFrameChoice(var FA : FrameArray);
    {-Get last choice}
  var
    I : Word;
  begin
    I := GetLastChoice;
    if I > MaxFrameTypes then
      I := 1;
    FA := FrameTypes[I];
  end;

  {------------ ColorSelector methods ----------}

  constructor ColorSelector.Init(X1, Y1 : Byte; ShowHigh : Boolean);
    {-Initialize the color selector}
  begin
    {initialize calculator with default window options}
    if not ColorSelector.InitCustom(
      X1, Y1, DefaultColorSet, DefWindowOptions, ShowHigh) then
        Fail;
  end;

  constructor ColorSelector.InitCustom(X1, Y1 : Byte;
                                       var Colors : ColorSet;
                                       Options : LongInt;
                                       ShowHigh : Boolean);
   {-Initialize the color selector with custom colors and options}
  var
    X2, Y2 : Byte;
  begin
    {are we displaying attributes > $7F?}
    if ShowHigh then begin
      csMax := $FF;
      csMax0 := $F0;
      csWidth := 50;
    end
    else begin
      csMax := $7F;
      csMax0 := $70;
      csWidth := 26;
    end;

    {calculate X2 and Y2}
    X2 := X1+csWidth-1;
    Y2 := Y1+csHeight-1;

    {adjust options}
    ClearLongFlag(Options, wResizeable);
    SetLongFlag(Options, wClear+wUserContents+wSetMouse+wBordered);

    {initialize the window}
    if not CommandWindow.InitCustom(X1, Y1, X2, Y2, Colors, Options,
                                    MakeMiscCommands, ucMakeMisc) then
      Fail;

    wFrame.AddHeader(' 00 ', heBC);
    if RawError <> 0 then begin {!!.01}
      Done;
      InitStatus := RawError;   {!!.01}
      Fail;
    end;
    csIndex := wFrame.GetLastHeaderIndex;

    {set attribute for box}
    SetBoxAttr(Colors.FrameColor, Colors.FrameMono);
  end;

  procedure ColorSelector.SetBoxAttr(Color, Mono : Byte);
    {-Set color for selection box}
  begin
    csBoxColor := Color;
    csBoxMono := MapMono(Color, Mono);
    csBoxAttr := ColorMono(csBoxColor, csBoxMono);
  end;

  function ColorSelector.csCalcColor(Row, Col : Byte) : Byte;
    {-Return the video attribute to use at Row,Col}
  begin
    Dec(Row, wYL);
    Dec(Col, wXL);
    if (Row = 0) or (Col = 0) or (Row = csHeight-1) or (Col = csWidth-1) then
      csCalcColor := csBoxAttr
    else
      csCalcColor := (((Col-1) div 3) shl 4)+(csBoxAttr and $0F);
      {                 background    shl 4 +    foreground    }
  end;

  procedure ColorSelector.csCalcRowCol(Attr : Byte; var Row, Col : Byte);
    {-Calculate the row and column for an attribute}
  begin
    {calculate row}
    Row := Succ(wYL)+(Attr and $F);

    {calculate column}
    Col := Succ(wXL)+((Attr shr 4)*3);
  end;

  procedure ColorSelector.UpdateContents;
    {-Redraw the inner window of the color selector}
  var
    I, Row, Col, A : Byte;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    for A := 0 to csMax do begin
      csCalcRowCol(A, Row, Col);
      FastWrite(ColorChoice, Row, Col, A);
    end;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    StackWindow.UpdateContents; {!!.01}
  end;

  procedure ColorSelector.SetColor(Color : Byte);
    {-Set default color choice}
  begin
    csColor := Color;
  end;

  function ColorSelector.GetColor : Byte;
    {-Get last color choice}
  begin
    GetColor := csColor;
  end;

  procedure ColorSelector.ProcessSelf;
    {-Select a color}
  const
    BoxCharArray : array[-1..1, -2..2] of Char = (
      ('Ú', 'Ä', 'Ä', 'Ä', '¿'),
      ('³', ' ', '*', ' ', '³'),
      ('À', 'Ä', 'Ä', 'Ä', 'Ù'));
  var
    SaveAttr, Attr : Byte;
    AllDone        : Boolean;
    BoxBuffer      : array[-1..1, -2..2] of Word;
    BoxBufferPtr   : Pointer;
    SaveXL, SaveYL,
    SaveXH, SaveYH : Byte;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}

    procedure DrawAttributeBox(Attr, Row, Col : Byte);
      {-Draw box around current selection}
    const
      S : String[1] = ' ';
    var
      A : Byte;
      X, Y, RowDelta, ColDelta : Integer;
    begin
      for RowDelta := -1 to 1 do begin
        Y := Row+RowDelta;
        for ColDelta := -2 to 2 do begin
          X := Col+ColDelta;

          {leave the attribute of ' * ' alone}
          if (RowDelta = 0) and (Abs(ColDelta) < 2) then
            A := Attr
          else
            A := csCalcColor(Y, X);

          S[1] := BoxCharArray[RowDelta, ColDelta];
          FastWrite(S, Y, X, A);
        end;
      end;
    end;

    procedure ShowChoice(FirstCall : Boolean);
      {-Show the currently selected attribute}
    var
      Row, Col : Byte;
      Redraw : Boolean;
    begin
      {remove the previous box, if any}
      if not FirstCall then
        RestoreWindow(SaveXL, SaveYL, SaveXH, SaveYH, False, BoxBufferPtr);

      {calculate the row and column for the new one}
      csCalcRowCol(Attr, Row, Col);

      {save the portion of the screen that will be overwritten}
      SaveXL := Pred(Col);
      SaveYL := Pred(Row);
      SaveXH := Col+3;
      SaveYH := Succ(Row);
      if SaveWindow(SaveXL, SaveYL, SaveXH, SaveYH, False, BoxBufferPtr) then ;

      {draw the box that marks the current attribute}
      DrawAttributeBox(Attr, Row, Succ(Col));

      wFrame.ChangeHeaderString(csIndex, ' '+HexB(Attr)+' ', Redraw);
      wFrame.DrawHeader(csIndex);

      {$IFDEF UseMouse}
      {update the mouse cursor}
      if MouseInstalled then
        if FirstCall then                                          {!!.22}
          MouseGotoXY(Col-MouseXLo, Row-MouseYLo)                  {!!.22}
        else begin                                                 {!!.22}
          if Abs(Integer(MouseLastX)-(Col-MouseXLo)) > 2 then      {!!.22}
            MouseGotoXY(Col-MouseXLo, MouseLastY);                 {!!.22}
          if Abs(Integer(MouseLastY)-(Row-MouseYLo)) > 1 then      {!!.22}
            MouseGotoXY(MouseLastX, Row-MouseYLo);                 {!!.22}
        end;                                                       {!!.22}
      {$ENDIF}
    end;

  begin
    {Draw initial screen if not already done}
    ClearErrors;          {!!.01}
    Draw;
    if RawError <> 0 then {!!.01}
      Exit;               {!!.01}

    {hide the cursor}
    SetCursor(cuHidden);

    {initialize}
    BoxBufferPtr := @BoxBuffer;

    {$IFDEF UseMouse}
    {slow mouse down and hide it}
    SlowestMouse; {SlowMouse better?}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {get choices}
    AllDone := False;
    Attr := csColor and csMax;
    ShowChoice(True);
    repeat
      {get the next command}
      GetNextCommand;

      SaveAttr := Attr;
      case cwCmd of
        ccUp :
          if (Attr and $0F) = 0 then
            Inc(Attr, $0F)
          else
            Dec(Attr);
        ccDown :
          if (Attr and $0F) = $0F then
            Dec(Attr, $0F)
          else
            Inc(Attr);
        ccBackTab, {!!.22}
        ccLeft :
          if Attr <= $0F then
            Inc(Attr, csMax0)
          else
            Dec(Attr, $10);
        ccTab,     {!!.22}
        ccRight :
          if Attr >= csMax0 then
            Dec(Attr, csMax0)
          else
            Inc(Attr, $10);
        ccUpLeft  :
          begin
            if (Attr and $0F) <> 0 then
              Dec(Attr);
            if Attr > $0F then
              Dec(Attr, $10);
          end;
        ccDownLeft  :
          begin
            if (Attr and $0F) <> $0F then
              Inc(Attr);
            if Attr > $0F then
              Dec(Attr, $10);
          end;
        ccUpRight :
          begin
            if (Attr and $0F) <> 0 then
              Dec(Attr);
            if Attr < csMax0 then
              Inc(Attr, $10);
          end;
        ccDownRight :
          begin
            if (Attr and $0F) <> $0F then
              Inc(Attr);
            if Attr < csMax0 then
              Inc(Attr, $10);
          end;
        ccHome :
          Attr := Attr and $0F;
        ccEnd :
          Attr := (Attr and $0F)+csMax0;
        ccPageUp :
          Attr := Attr and csMax0;
        ccPageDn :
          Attr := (Attr and csMax0)+$0F;
        ccUser0..ccUser65335,
        ccMouseSel,
        ccSelect :
          begin
            csColor := Attr;
            AllDone := True;
          end;
        ccQuit :
          begin
            Attr := csColor;
            AllDone := True;
          end;
        ccHelp :
          RequestHelp(wHelpIndex);
        else
          cwCmd := ccNone;
      end;

      {draw new choice}
      if (Attr <> SaveAttr) then
        ShowChoice(False);

    until AllDone;

    {restore screen beneath box}
    RestoreWindow(SaveXL, SaveYL, SaveXH, SaveYH, False, BoxBufferPtr);

    {$IFDEF UseMouse}
    {reset mouse}
    FastMouse;
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  {------------ OpLibrarySelector methods ----------}

  {$F+}
  function lsMultipleChoiceCommand(var Cmd : Word; P : PickListPtr) : Boolean;
    {-Remap ccSelect to ccToggle}
  var
    B : Boolean;
  begin
    B := MultipleChoiceCommand(Cmd, P);
    if B and (Cmd = ccSelect) then begin
      Cmd := ccToggle;
      lsMultipleChoiceCommand := MultipleChoiceCommand(Cmd, P);
    end
    else
      lsMultipleChoiceCommand := B;
  end;

  procedure lsMultipleChoice(P : PickListPtr);
    {-Initialize for multiple choice pick list}
  begin
    with P^ do begin
      SetFlag(pkFlags, pkMultiChoice);
      pkCommand := lsMultipleChoiceCommand;
    end;
  end;
  {$F-}

  constructor OpLibrarySelector.Init(X1, Y1, X2, Y2 : Byte;
                                   var Lib : OpLibrary;
                                   TypeCodes : ByteSet;
                                   AllowNew : Boolean);
    {-Initialize the library selector using default colors and options}
  begin
    if not OpLibrarySelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, SingleChoice, Lib, TypeCodes, deUnused+deDeleted,
      AllowNew, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;

    SetPadSize(1, 0);
  end;

  constructor OpLibrarySelector.InitMulti(X1, Y1, X2, Y2 : Byte;
                                        var Lib : OpLibrary;
                                        TypeCodes : ByteSet;
                                        AllowNew : Boolean);
    {-Initialize the library selector using default colors and options}
  begin
    if not OpLibrarySelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, lsMultipleChoice, Lib, TypeCodes, deUnused,
      AllowNew, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;

    {Allocate bit set (if not already done) to mark selected items}
    AllocateSelectSet(pkItems);
    if RawError <> 0 then begin {!!.01}
      Done;
      InitStatus := RawError;   {!!.01}
      Fail;
    end;

    {set selection marker}
    SetSelectMarker(#251, '');

    {mark deleted items}
    MarkDeletedItems;
  end;

  constructor OpLibrarySelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                         var Colors : ColorSet;
                                         Options : LongInt;
                                         Orientation : pkGenlProc;
                                         CommandHandler : pkGenlProc;
                                         var Lib : OpLibrary;
                                         TypeCodes : ByteSet;
                                         FlagFilter : Word;
                                         AllowNew : Boolean;
                                         LeftTee, CrossBar, RightTee : Char);
    {-Initialize the library selector with custom options, colors}
  var
    AN : Byte absolute AllowNew;
  begin
    pkItems := 0;
    lsCount := 0;
    lsSize := 0;
    lsObjects := nil;

    lsLabelColor := Colors.TextColor;
    lsLabelMono := Colors.TextMono;

    {try to verify that the library is open and valid}
    if Lib.FindEntryByIndex(1)^.deSig <> DirectorySig then
      Fail;

    if not ChangeLibraries(Lib, TypeCodes, FlagFilter, AllowNew) then begin
      FreeMemCheck(lsObjects, lsSize);
      Fail;
    end;

    if not PickList.InitAbstract(
      X1, Y1+2, X2, Y2, Colors, Options, 22, lsCount+AN,
      Orientation, CommandHandler) then begin
        Done;
        Fail;
      end;

    {adjust frame coordinates}
    with wFrame do
      AdjustFrameCoords(frXL, frYL-2, frXH, frYH);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {add window divider}
    wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, 2, frTT);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;
  end;

  destructor OpLibrarySelector.Done;
    {-Dispose of library selector}
  begin
    if (lsObjects <> nil) and (lsSize > 0) then
      FreeMemCheck(lsObjects, lsSize);
    PickList.Done;
  end;

  procedure OpLibrarySelector.SetLabelAttr(Color, Mono : Byte);
    {-Set color for column labels}
  begin
    lsLabelColor := Color;
    lsLabelMono := MapMono(Color, Mono);
  end;

  function OpLibrarySelector.ChangeLibraries(var Lib : OpLibrary;
                                           TypeCodes : ByteSet;
                                           FlagFilter : Word;
                                           AllowNew : Boolean) : Boolean;
    {-Switch libraries and or object types}
  var
    I, Code, Ver : Word;
  begin
    ChangeLibraries := False;

    {dispose of existing list if necessary}
    if (lsObjects <> nil) and (lsSize > 0) then
      FreeMemCheck(lsObjects, lsSize);

    lsLib := @Lib;
    with Lib do begin
      {allocate memory for array}
      lsCount := 0;
      lsSize := liEntries*SizeOf(Word);
      if not GetMemCheck(lsObjects, lsSize) then begin {!!.20}
        lsSize := 0;                                   {!!.20}
        Exit;
      end;                                             {!!.20}

      {make a pass through the directory}
      for I := 1 to liEntries do
        with liDirectory^[I]^ do
          if not FlagIsSet(deFlags, FlagFilter) then
            if (TypeCodes = []) or (deCode in TypeCodes) then begin
              Inc(lsCount);
              lsObjects^[lsCount] := I;
            end;

      if (lsCount = 0) and not AllowNew then
        ChangeLibraries := False
      else begin
        ChangeLibraries := True;
        if pkItems <> 0 then
          ChangeNumItems(lsCount+Ord(AllowNew));
      end;
    end;
  end;

  procedure OpLibrarySelector.MarkDeletedItems;
    {-Mark all deleted items}
  var
    I : Word;
  begin
    ClearSelected;
    with lsLib^ do
      for I := 1 to lsCount do
        with liDirectory^[lsObjects^[I]]^ do
          if FlagIsSet(deFlags, deDeleted) then
            SelectItem(I);
  end;

  procedure OpLibrarySelector.DeleteMarkedItems;
    {-Delete all marked items}
  var
    I : Word;
  begin
    with lsLib^ do begin
      for I := 1 to lsCount do
        with liDirectory^[lsObjects^[I]]^ do
          if ItemIsSelected(I) then
            deOptionsOn(deDeleted)
          else
            deOptionsOff(deDeleted);
      UpdateDirectory;
    end;
  end;

  function OpLibrarySelector.GetLastObjectChoice : Word;
    {-Get last choice. Return value is an index into the library's
      directory. 0 means '--- New ---' was selected.}
  var
    I : Word;
  begin
    I := GetLastChoice;
    if (I <= lsCount) then
      GetLastObjectChoice := lsObjects^[I]
    else
      GetLastObjectChoice := 0;
  end;

  procedure OpLibrarySelector.UpdateContents;
    {-Redraw the library selector}
  const
    Headers = ' Name          Size';
  var
    I : Word;
    S : string;
    SLen : Byte absolute S;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw the column headers}
    I := Succ(wXH-wXL);
    S := Pad(Headers, I);
    if SLen > I then
      SLen := I;
    FastWrite(S, wYL-2, wXL, ColorMono(lsLabelColor, lsLabelMono));

    {update the pick list}
    PickList.UpdateContents;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure OpLibrarySelector.ItemString(Item : Word;
                                       Mode : pkMode;
                                       var IType : pkItemType;
                                       var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  begin
    if Item <= lsCount then begin
      with lsLib^.liDirectory^[lsObjects^[Item]]^ do begin
        if FlagIsSet(deFlags, deUnused) then
          IString := '(unused)'
        else
          IString := deName;

        if Mode <> pkSearch then
          IString := Pad(IString, 14)+Decimal(deLength, 6);
      end;
    end
    else begin
      IString := '------- New --------';
      IType := pkAlternate;
    end;
  end;

  {------------ ColorSetSelector methods ----------}                {!!.03}

  procedure ColorSetSelector.ItemString(Item : Word;                {!!.03}
                                       Mode : pkMode;
                                       var IType : pkItemType;
                                       var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  begin
    if Item > lsCount then begin
      IString := '----- Default ------';
      IType := pkAlternate;
    end
    else
      OpLibrarySelector.ItemString(Item, Mode, IType, IString);
  end;

  {------------ OpLibraryInfo methods ----------}

  constructor OpLibraryInfo.Init(X1, Y1 : Byte; var Lib : OpLibrary; LibName : PathStr);
    {-Initialize the library info window using default colors and options}
  begin
    if not OpLibraryInfo.InitCustom(
      X1, Y1, DefaultColorSet, DefWindowOptions, Lib, LibName) then
        Fail;
  end;

  constructor OpLibraryInfo.InitCustom(X1, Y1 : Byte;
                                     var Colors : ColorSet;
                                     Options : LongInt;
                                     var Lib : OpLibrary;
                                     LibName : PathStr);
    {-Initialize the library info window with custom options, colors}
  const
    Info = ' Info: ';
    PressAnyKey = ' Press any key... ';
  var
    X2, Y2 : Byte;
  begin
    {calculate remaining coordinates}
    X2 := X1+33;
    Y2 := Y1+15;

    {initialize window}
    if not StackWindow.InitCustom(
      X1, Y1, X2, Y2, Colors, Options or (wBordered+wUserContents)) then
        Fail;

    {add headers}
    LibName := JustFileName(LibName);
    wFrame.AddHeader(Info+LibName+' ', heTC);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;
    wFrame.AddHeader(PressAnyKey, heBC);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {save pointer to library}
    liLib := @Lib;

    {set attributes}
    with Colors do begin
      liBrightColor := HighlightColor;
      liBrightMono  := HighlightMono;
      liDimColor    := TextColor;
      liDimMono     := TextMono;
    end;
  end;

  procedure OpLibraryInfo.UpdateContents;
    {-Redraw the library info window}
  var
    A1, A2 : Byte;
  begin
    with liLib^ do begin
      A1 := ColorMono(liBrightColor, liBrightMono);
      A2 := ColorMono(liDimColor, liDimMono);

      wFastWrite('Directory information',         02, 02, A1);
        wFastWrite('Maximum entries',             03, 04, A2);
          wFastWrite(Long2Str(MaxEntries),        03, 28, A2);
        wFastWrite('Current entries',             04, 04, A2);
          wFastWrite(Long2Str(CurrentEntries),    04, 28, A2);
        wFastWrite('Deleted entries',             05, 04, A2);
          wFastWrite(Long2Str(DeletedEntries),    05, 28, A2);
        wFastWrite('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  ÄÄÄ', 06, 04, A2);
        wFastWrite('Available',                   07, 04, A2);
          wFastWrite(Long2Str(AvailableEntries),  07, 28, A2);
      wFastWrite('Library information',           09, 02, A1);
        wFastWrite('Actual file size',            10, 04, A2);
          wFastWrite(Long2Str(GetSize),           10, 28, A2);
        wFastWrite('Packed file size',            11, 04, A2);
          wFastWrite(Long2Str(PackedFileSize),    11, 28, A2);
      wFastWrite('Memory',                        13, 02, A1);
        wFastWrite('In use',                      14, 04, A2);
          wFastWrite(Long2Str(MemInUse),          14, 25, A2);         {!!.20}
        wFastWrite('Available',                   15, 04, A2);
          wFastWrite(Long2Str(MemAvail),          15, 25, A2);         {!!.20}
    end;
    StackWindow.UpdateContents; {!!.01}
  end;

  procedure OpLibraryInfo.Process;
    {-Display the library info window and wait for a keypress}
  var
    I : Word;
  begin
    {display the info window}
    ClearErrors;    {!!.01}
    Draw;
    if RawError <> 0 then {!!.01}
      Exit;

    {hide the cursor}
    SetCursor(cuHidden);

    {wait for a key or button press}
    {$IFDEF UseMouse}
    I := ReadKeyOrButton;
    {$ELSE}
    I := ReadKeyWord;
    {$ENDIF}
  end;

begin
{$IFNDEF VIRTUALPASCAL}
  KbdFlagsPtr := Ptr(BiosDataSele, $17);                               {!!.20}
{$ENDIF}
  TotalAvail := MemAvail;  {!!.20}

  {initialize command processor}
  MakeMiscCommands.Init(@MakeMiscKeySet, MakeMiscKeyMax);

  {$IFDEF UseMouse}
  {enable mouse support}
  MakeMiscCommands.cpOptionsOn(cpEnableMouse);
  {$ENDIF}

  {map mouse movements to cursor commands, catch changes to <ScrollLock>}
  MakeMiscCommands.SetGetKeyProc(ReadKeyWordXY);
end.

