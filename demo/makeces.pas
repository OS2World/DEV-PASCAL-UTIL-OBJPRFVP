{$R-,S-,I-,V-,B-,F+,O+}

{*********************************************************}
{*                  MAKECES.PAS 1.30                     *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This unit requires that OPDEFINE.INC activate the following defines:
   UseScrollBars, UseHotSpots, UseAdjustableWindows, UseShadows, UseStreams
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

{$IFNDEF UseStreams}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

unit MakeCES;
  {-Custom entry screens--for MAKESCRN}

interface

uses
  Use32,
  opinline,
  opstring,
  opconst,   {!!.20}
  oproot,
  opcrt,
  {$IFDEF UseMouse}
  opmouse,
  {$ENDIF}
  opcmd,
  opframe,
  opwindow,
  opabsfld,
  {$IFDEF UseDates}
  opdate,
  {$ENDIF}
  {$IFDEF UseBcd}
  opbcd,
  {$ENDIF}
  opfield,
  opselect,
  opentry;

const
  cesMaxCol         = 160;   {cesMaxCol _must_ be <= 254}
  cesMaxRow         = 200;   {cesMaxRow*cesMaxCol*2 must be <= 65521}

  cesMakeLine       = $0001; {set while making a line field}          {!!.03}
  cesMakeBox        = $0002; {set while making a box field}           {!!.03}
  cesMakeShBox      = $0004; {set while making a shadowed box field}  {!!.03}

type
  cesTextFieldType  = (cesText, cesHLine, cesVLine, cesBox, cesShBox);

  CustomEntryScreenPtr = ^CustomEntryScreen; {!!.03}
  CustomEntryScreen =
    object(ScrollingEntryScreen)
      cesSliderChar   : Char;
      cesBarChar      : Char;
      cesArrows       : ArrowArray;
      cesStageDelay   : Word;
      cesMouseSupport : Boolean;
      cesX, cesY      : Word;       {current position of the cursor}
      cesDummy        : record end;
      cesFlags        : Word;       {!!.03}

      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the selector}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt);
        {-Initialize the entry screen with custom window options}

      procedure cesGotoXY(X, Y : Integer);
        {-Move cursor to X,Y}
      procedure cesMoveXY(DX, DY : Integer);
        {-Move cursor relative to current position}
      procedure ResetXY;
        {-Reset cesX and cesY}

      function IsScrollable : Boolean;
        {-Returns True if entry screen is truly scrollable}
      function EntryFieldCount : Word;
        {-Returns the number of entry fields}
      function TextFieldCount(Mask : Word) : Word;
        {-Returns the number of text fields}

      function HasDataFields : Boolean;
        {-Returns True if entry screen has any data fields}
      function HasFields : Boolean;
        {-Returns True if entry screen has any fields (text or data)}
      function AllFieldsProtected : Boolean;                     {!!.11}
        {-Returns True if entry screen has all protected fields} {!!.11}

      procedure ResetShadowColor;
        {-Reset colors for shadows}
      procedure ResetScrollBarColor;
        {-Reset colors for scroll bars}
      procedure ResetHeaderColor;
        {-Reset colors for headers}
      procedure ResetTextFieldColor;
        {-Reset colors for text fields}
      procedure ResetPromptColor;
        {-Reset colors for prompts}
      procedure ResetSelectedPromptColor;
        {-Reset colors for selected prompts}
      procedure ResetProtectedPromptColor;
        {-Reset colors for protected prompts}
      procedure ResetFieldColor;
        {-Reset colors for fields}
      procedure ResetSelectedFieldColor;
        {-Reset colors for selected fields}
      procedure ResetProtectedFieldColor;
        {-Reset colors for protected fields}
      procedure ResetCtrlColor;
        {-Reset colors for control characters}
      procedure ResetAllColors;                   {!!.03}
        {-Reset all colors}                       {!!.03}

      procedure ForceReset;
        {-Reset flags, force reallocation of virtual screen}

      constructor Load(var S : IdStream);
        {-Load a custom entry screen from a stream}
      procedure Store(var S : IdStream);
        {-Store a custom entry screen in a stream}

      function TextFieldType(TFP : TextFieldPtr) : cesTextFieldType;  {!!.03}
        {-Return type of specified text field}
      procedure DoneAddingLineOrBox;                                  {!!.03}
        {-Called when finished adding a line or box field}
      procedure MarkLastTextField;                                    {!!.03}
        {-Mark the last text field}
      function FindLastTextField : TextFieldPtr;                      {!!.03}
        {-Find the text field marked by MarkLastTextField}
      function AddTextFieldAtCursor(S : string) : TextFieldPtr;       {!!.03}
        {-Add a text field at the position of the cursor}
      function AddLineFieldAtCursor(Ch1, Ch2, Ch3 : Char; Len : Word; {!!.03}
                                    Vert : Boolean) : TextLineFieldPtr;
        {-Add a (vertical) line field at the position of the cursor}
      function AddBoxFieldAtCursor(BoxChars : FrameArray; Wid, Ht : Word; {!!.03}
                                   Shadowed : Boolean) : TextBoxFieldPtr;
        {-Add a (shadowed) box field at the position of the cursor}
      function CopyTextFieldToCursor(TFP : TextFieldPtr) : Boolean;
        {-Make a copy of a text field at the position of the cursor}
      function MoveTextFieldToCursor(TFP : TextFieldPtr) : Boolean;
        {-Move the specified field to the position of the cursor}
      function FindTextFieldAtCursor(var TFP : TextFieldPtr;
                                     TextOnly : Boolean) : Boolean; {!!.03}
        {-Returns True if there's a text field at the cursor}
      function ModifyTextField(TFP : TextFieldPtr; S : string) : Boolean;
        {-Change the text in the specified text field}
      procedure ChangeTextFieldAttrs(TFP : TextFieldPtr; Color, Mono : Byte);
        {-Change the attributes of the specified text field}
      procedure ChangeBoxFieldAttrs(TBFP : TextBoxFieldPtr;        {!!.03}
                                    BColor, BMono : Byte;          {!!.03}
                                    SColor, SMono : Byte);         {!!.03}
        {-Change the attributes of the specified box field}        {!!.03}
      procedure DeleteTextField(TFP : TextFieldPtr);
        {-Delete the specified text field}
      procedure RenumberTextFields;
        {-Renumber all text fields}
      {procedure SortTextFields;}                                     {!!.11}
        {-Sort the text fields based on Row,Col coordinates}          {!!.11}
      procedure RepositionTextField(TFP : TextFieldPtr);              {!!.11}
        {-Move TFP to its proper position in the list of text fields} {!!.11}

      function FindEntryFieldAtCursor(var EFP : EntryFieldPtr) : Boolean;
        {-Returns True if there's an entry field at the cursor}
      procedure GuessCurrentField;
        {-Set current field as close to cursor as possible}
      procedure AddDummyField;
        {-Add a dummy field to the entry screen if necessary}
      function HaveDummyField : Boolean;
        {-Returns True if the dummy field is present}
      procedure RemoveDummyField;
        {-Remove the dummy field}
      function MoveEntryFieldToCursor(EFP : EntryFieldPtr) : Boolean;
        {-Move the specified field to the position of the cursor}
      procedure DeleteEntryField(EFP : EntryFieldPtr);
        {-Delete the specified entry field}
      procedure RenumberEntryFields;
        {-Renumber all entry fields}
      procedure SortEntryFields;
        {-Sort the entry fields based on Row,Col coordinates}
      procedure ExchangeFields(IP, JP : EntryFieldPtr);
        {-Exchange fields IP^ and JP^}
      procedure GetFieldCoordinates(EFP : EntryFieldPtr;
                                    var XL, YL, XH, YH : Integer);
        {-Return the coordinates for the specified field}

      function HaveFieldOnCurrentLine : Boolean;
        {-Returns True if there's at least one field on the current line}
      procedure InsertLineAtCursor;
        {-Insert a line at the position of the cursor}
      procedure DeleteLineAtCursor;
        {-Delete the line the cursor is on}

      procedure InitUserRecord;
        {-Initialize the user record}
      function ReallocUserRecord : Boolean;
        {-Reallocate the user record}

      function cesRowAtTop : Word;
        {-Returns the row at the top of the window}
      function cesRowAtBottom : Word;
        {-Returns the row at the bottom of the window}
      function cesReadRow(Row : Word) : string;
        {-Returns the contents of the specified row}

      constructor cesBind;
      constructor cesBindPrim;
      procedure asDrawKnownField(SFP : SelectFieldPtr); virtual;
      procedure asFixWindow(Redraw, ScrollByPage : Boolean); virtual;
      procedure asPageUpOrDown(Delta : Integer); virtual;
      procedure asSetupForScrollBars; virtual;
      procedure asUpdateScrollBars; virtual;
      procedure asResetFlags; virtual;
    end;

procedure CustomEntryScreenStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing scrolling entry screens}

  {==========================================================================}

implementation

  constructor CustomEntryScreen.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize the custom entry screen}
  begin
    {initialize using default window options}
    if not CustomEntryScreen.InitCustom(X1, Y1, X2, Y2, DefaultColorSet,
                                        DefWindowOptions) then
      Fail;
  end;

  constructor CustomEntryScreen.InitCustom(X1, Y1, X2, Y2 : Byte;
                                           var Colors : ColorSet;
                                           Options : LongInt);
    {-Initialize the custom entry screen}
  begin
    {initialize our special fields}
    cesStageDelay   := 15;
    cesSliderChar   := DefSliderChar;
    cesBarChar      := DefBarChar;
    cesArrows       := DefArrows;
    cesMouseSupport := True;
    cesX            := 1;
    cesY            := 1;
    cesFlags        := 0;         {!!.03}

    {perform normal scrolling entry screen initialization}
    Options := Options and not wSaveContents;
    if not ScrollingEntryScreen.InitCustom(X1, Y1, X2, Y2, Colors, Options) then
      Fail;
  end;

  constructor CustomEntryScreen.cesBind;
    {-Force the VMT link to that of a CustomEntryScreen}
  begin
    {calling another constructor without an 'ObjectName.' prefix will change
     the VMT link}
    cesBindPrim;
  end;

  constructor CustomEntryScreen.cesBindPrim;
    {-Dummy constructor called by cesBind to change the VMT link}
  begin
  end;

  function CustomEntryScreen.IsScrollable : Boolean;
    {-Returns True if entry screen is truly scrollable}
  begin
    IsScrollable := (sesVS.vRows > Height) or (sesVS.vCols > Width);
  end;

  function CustomEntryScreen.EntryFieldCount : Word;
    {-Returns the number of entry fields}
  begin
    if not HasDataFields then
      EntryFieldCount := 0
    else
      EntryFieldCount := asCount;
  end;

  function CustomEntryScreen.TextFieldCount(Mask : Word) : Word; {!!.03} {rewritten}
    {-Returns the number of text fields}
  var
    I : Word;
    TFP : TextFieldPtr;
  begin
    I := 0;
    TFP := TextFieldPtr(asTextFields.Head);
    while TFP <> nil do begin
      if (Mask = $FFFF) or (TFP^.tfFlags and Mask = Mask) then
        Inc(I);
      TFP := TextFieldPtr(TFP^.slNext);
    end;
    TextFieldCount := I;
  end;

  procedure CustomEntryScreen.asDrawKnownField(SFP : SelectFieldPtr);
    {-Draw a field when its address is known}
  var
    EFP : EntryFieldPtr absolute SFP;
    FRow, FCol : Integer;
    PRow, PCol : Integer;
    FA, PA, CA, DA : Byte;
    HaveP, HaveF : Boolean;
    S : string;
    SLen : Byte absolute S;
    SaveO : LongInt;           {!!.11}
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    if EFP = nil then
      Exit;

    with EFP^ do begin
      {don't display nested fields}
      if (efDataSize = 0) then
        Exit;

      {adjust coordinates}
      HaveP := asFixCoordinates(sfPRow, sfPCol, sfPWidth, 1, PRow, PCol);
      HaveF := asFixCoordinates(sfFRow, sfFCol, sfFWidth, sfFHeight, FRow, FCol);
      if not (HaveP or HaveF) then
        Exit;

      {determine attributes to use for the field}
      SaveO := sfOptions;                             {!!.11}
      ClearLongFlag(sfOptions, efProtected+efHidden); {!!.11}
      esCalcAttrs(EFP, FA, CA, PA, DA);
      sfOptions := SaveO;                             {!!.11}

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      {draw the prompt}
      if HaveP then
        FastWrite(efPrompt^, PRow, PCol, PA);

      if HaveF then begin
        {draw field delimiters if desired}
        if (esLeftD <> #0) and (esRightD <> #0) and
           not LongFlagIsSet(sfFlags, ifMultiLine) then begin
             if (FCol > wXL) or ((FCol > 1) and (VirtualSegment <> VideoSegment)) then
               FastWrite(esLeftD, FRow, FCol-1, DA);
             if (FCol+sfFWidth <= wXH) or (VirtualSegment <> VideoSegment) then
               FastWrite(esRightD, FRow, FCol+sfFWidth, DA);
           end;

        {draw the picture mask}
        S := efPicture^;
        SLen := sfFWidth;
        FastWrite(S, FRow, FCol, FA);
      end;

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;
  end;

  function CustomEntryScreen.cesRowAtTop : Word;
    {-Returns the row at the top of the window}
  begin
    cesRowAtTop := Succ(sesRowOfs);
  end;

  function CustomEntryScreen.cesRowAtBottom : Word;
    {-Returns the row at the bottom of the window}
  begin
    cesRowAtBottom := cesRowAtTop+(wYH-wYL);
  end;

  procedure CustomEntryScreen.asFixWindow(Redraw, ScrollByPage : Boolean);
    {-Fix the edit window if necessary}
  var
    SaveRowOffset : Integer;
    SaveColOffset : Integer;
    RowAtTop : Integer;
    RowAtBottom : Integer;
    ColAtLeft : Integer;
    ColAtRight : Integer;
    Height1 : Word;
    Width1 : Word;

    procedure ScrollRight(Cols : Integer);
    var
      MaxColOfs : Integer;
    begin
      if Cols <> 0 then begin
        Inc(sesColOfs, Cols);
        if sesColOfs < 0 then
          sesColOfs := 0;
      end;

      MaxColOfs := cesMaxCol-Width1-1;
      if (MaxColOfs > 0) and (sesColOfs > MaxColOfs) then
        sesColOfs := MaxColOfs;
    end;

    procedure ScrollDown(Rows : Integer);
    var
      MaxRowOfs : Integer;
    begin
      if Rows <> 0 then begin
        Inc(sesRowOfs, Rows);
        if sesRowOfs < 0 then
          sesRowOfs := 0;
      end;

      MaxRowOfs := cesMaxRow-Height1-1;
      if (MaxRowOfs > 0) and (sesRowOfs > MaxRowOfs) then
        {make sure end of last page is displayed at bottom of window}
        sesRowOfs := MaxRowOfs;
  end;

  begin
    SaveRowOffset := sesRowOfs;
    SaveColOffset := sesColOfs;
    Height1 := wYH-wYL;
    Width1 := wXH-wXL;
    RowAtTop := cesRowAtTop;
    RowAtBottom := cesRowAtBottom;
    ColAtLeft := Succ(sesColOfs);
    ColAtRight := ColAtLeft+Width1;

    {scroll the window vertically if necessary}
    if cesY > RowAtBottom then
      ScrollDown(cesY-RowAtBottom)
    else if cesY < RowAtTop then
      ScrollDown(cesY-RowAtTop)
    else
      ScrollDown(0);

    {scroll the window horizontally if necessary}
    if cesX < ColAtLeft then
      ScrollRight(cesX-ColAtLeft)
    else if cesX > ColAtRight then
      ScrollRight(cesX-ColAtRight)
    else
      ScrollRight(0);

    if Redraw then
      if (sesRowOfs <> SaveRowOffset) or (sesColOfs <> SaveColOffset) then
        {update physical screen}
        VScreenToScreen;
  end;

  procedure CustomEntryScreen.asPageUpOrDown(Delta : Integer);
    {-Process PgUp/PgDn commands}
    {-Note: Page calculations treat 0 as first page #}
  var
    WHeight : Integer;
    MaxPage : Integer;
    CurPage : Integer;
    NewPage : Integer;
    NewRow  : Integer;
    NewOfs  : Integer;
    MaxRowOfs : Integer;
  begin
    WHeight := Succ(wYH-wYL);
    MaxPage := Pred(cesMaxRow) div WHeight;
    CurPage := Pred(cesY) div WHeight;

    {don't switch pages on PgUp if start of CurPage not on screen}
    if (Delta < 0) and (sesRowOfs > (CurPage * WHeight)) and (CurPage = MaxPage-1) then
      NewPage := CurPage
    else
      NewPage := CurPage + Delta;

    {don't go too far}
    if (NewPage < 0) then
      NewPage := 0
    else if (NewPage > MaxPage) then
      NewPage := MaxPage;

    if (NewPage = MaxPage) and (CurPage = MaxPage) and (Delta > 0) then
      {special case--PgDn while already on last page}
      NewRow := cesMaxRow
    else begin
      NewRow := Succ(NewPage * WHeight);
      if NewRow < 1 then
        NewRow := 1;
    end;

    NewOfs := Pred(NewRow);
    MaxRowOfs := cesMaxRow-(wYH-wYL)-1;
    if (MaxRowOfs > 0) and (NewOfs > MaxRowOfs) then
      {make sure end of last page is displayed at bottom of window}
      sesRowOfs := MaxRowOfs
    else
      sesRowOfs := NewOfs;

    cesY := NewRow;
    VScreenToScreen;
    wGotoXY(cesX-sesColOfs, cesY-sesRowOfs);
  end;

  procedure CustomEntryScreen.asSetupForScrollBars;
    {-Set boundaries for all scroll bars}
  begin
    ChangeAllScrollBars(1, cesMaxCol, 1, cesMaxRow);
  end;

  procedure CustomEntryScreen.asUpdateScrollBars;
    {-Update horizontal and vertical scroll bars}
  begin
    DrawAllSliders(cesX, cesY);
  end;

  procedure CustomEntryScreen.asResetFlags;
    {-Reset internal flags}
  var
    SFP : SelectFieldPtr;
    LowCol, HighCol : Word;
  begin
    {do nothing if first/last row already known}
    if (asMinRow <> 0) and (asMaxRow <> 0) then
      Exit;

    {no fast updates}
    ClearByteFlag(asOptions, esFastUpdates);

    {find the first unprotected field}
    SFP := Pointer(asFields.Head);
    asMinRow := SFP^.sfFRow;

    {find the last unprotected field}
    SFP := Pointer(asFields.Tail);
    asMaxRow := SFP^.sfFRow;

    {do nothing if min/max column already known}
    if (asMinCol <> 0) and (asMaxCol <> 0) then begin
      {$IFDEF UseScrollBars}
      {set up for scroll bars and exit}
      asSetupForScrollBars;
      {$ENDIF}
      Exit;
    end;

    LowCol := $FFFF;
    HighCol := 0;
    SFP := Pointer(asFields.Head);
    while SFP <> nil do
      with SFP^ do begin
        {check field}
        LowCol := MinWord(LowCol, sfFCol);
        HighCol := MaxWord(HighCol, sfFCol);

        {follow link}
        SFP := Pointer(SFP^.dlNext);
      end;

    asMinCol := LowCol;
    asMaxCol := HighCol;

    {$IFDEF UseScrollBars}
    {setup for scroll bars}
    asSetupForScrollBars;
    {$ENDIF}
  end;

  procedure CustomEntryScreen.cesGotoXY(X, Y : Integer); {!!.03} {rewritten}
    {-Move cursor to X,Y}
  var
    TFP : TextFieldPtr;
    TLFP : TextLineFieldPtr absolute TFP;
    TBFP : TextBoxFieldPtr absolute TFP;
  begin
    if X < 1 then
      X := 1
    else if X > cesMaxCol then
      X := cesMaxCol;
    if Y < 1 then
      Y := 1
    else if Y > cesMaxRow then
      Y := cesMaxRow;

    cesX := X;
    cesY := Y;

    if FlagIsSet(cesFlags, cesMakeLine+cesMakeBox) then begin
      {shadowed box has extra row and column}
      if FlagIsSet(cesFlags, cesMakeShBox) then begin
        Inc(X);
        Inc(Y);
      end;

      {need to resize virtual screen?}
      if (X > sesVS.vsWidth) or (Y > sesVS.vsHeight) then begin
        {calculate new size}
        X := MaxWord(X, sesVS.vsWidth);
        Y := MaxWord(Y, sesVS.vsHeight);

        {deallocate existing one}
        sesVS.Done;

        {allocate new one}
        if not sesVS.Alloc(Y, X) then begin
          GotError(epFatal+ecOutOfMemory, emInsufficientMemory);
          Exit;
        end;
      end;

      {adjust the line/box field coordinates}
      TFP := FindLastTextField;
      if FlagIsSet(cesFlags, cesMakeBox) then
        with TBFP^ do begin
          tbfColH := cesX;
          tbfRowH := cesY;
        end
      else with TLFP^ do
        if FlagIsSet(tfFlags, tfVertical) then
          tlfLength := Succ(cesY-tfRow)
        else
          tlfLength := Succ(cesX-tfCol);

      {redraw screen}
      ResetScreen;
      UpdateContents;
    end;

    asFixWindow(True, False);
    wGotoXY(cesX-sesColOfs, cesY-sesRowOfs);
    asUpdateScrollBars;
  end;

  procedure CustomEntryScreen.cesMoveXY(DX, DY : Integer);
    {-Move cursor relative to current position}
  var
    X, Y : Integer;
  begin
    X := cesX+DX;
    Y := cesY+DY;
    if X < 1 then
      X := 1
    else if X > cesMaxCol then
      X := cesMaxCol;
    if Y < 1 then
      Y := 1
    else if Y > cesMaxRow then
      Y := cesMaxRow;
    if (X <> cesX) or (Y <> cesY) then
      cesGotoXY(X, Y);
  end;

  procedure CustomEntryScreen.ResetXY;
    {-Reset cesX and cesY}
  begin
    cesX := sesColOfs+(Integer(WhereXabs)-Pred(wXL));
    cesY := sesRowOfs+(Integer(WhereYabs)-Pred(wYL));
  end;

  function CustomEntryScreen.HasDataFields : Boolean;
    {-Returns True if entry screen has any data fields}
  {var}                                    {!!.03}
    {EFP : EntryFieldPtr;}                 {!!.03}
  begin
    {EFP := EntryFieldPtr(asFields.Head);} {!!.03}
    HasDataFields := (asCount > 1) or not HaveDummyField;
  end;

  function CustomEntryScreen.HasFields : Boolean;
    {-Returns True if entry screen has any fields (text or data)}
  begin
    HasFields := (asTextFields.Head <> nil) or HasDataFields;
  end;

  function CustomEntryScreen.AllFieldsProtected : Boolean;   {!!.11}
    {-Returns True if entry screen has all protected fields}
  var
    EFP : EntryFieldPtr;
  begin
    AllFieldsProtected := False;
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      if not asFieldIsProtected(EFP) then
        Exit
      else
        EFP := EntryFieldPtr(EFP^.dlNext);
    AllFieldsProtected := True;
  end;

  procedure CustomEntryScreen.ForceReset;
    {-Reset flags, force reallocation of virtual screen}
  begin
    if sesScrollable then begin
      sesVS.Done;
      sesScrollable := False;
      wMinW := 1;
    end;
    asMinRow := 0;
    asMaxRow := 0;
    asMinCol := 0;
    asMaxCol := 0;
  end;

  procedure CustomEntryScreen.ResetShadowColor;
    {-Reset colors for shadows}
  begin
    with asColors do
      RawWindow.ResetShadowColor(ShadowColor, ShadowMono);
  end;

  procedure CustomEntryScreen.ResetScrollBarColor;
    {-Reset colors for scroll bars}
  begin
    with asColors do
      RawWindow.ResetScrollBarColor(
        SliderColor, SliderMono,
        ScrollBarColor, ScrollBarMono,
        HotSpotColor, HotSpotMono);
  end;

  procedure CustomEntryScreen.ResetHeaderColor;
    {-Reset colors for headers}
  begin
    with asColors do
      RawWindow.ResetHeaderColor(HeaderColor, HeaderMono);
  end;

  procedure CustomEntryScreen.ResetTextFieldColor;
    {-Reset colors for text fields}
  var
    TFP : TextFieldPtr;
    ColorAttrs : FlexAttrs;
    MonoAttrs : FlexAttrs;
  begin
    FillChar(ColorAttrs, SizeOf(FlexAttrs), wTextColor);
    FillChar(MonoAttrs, SizeOf(FlexAttrs), wTextMono);
    TFP := TextFieldPtr(asTextFields.Head);
    while TFP <> nil do
      with TFP^ do begin
        tfColorAttrs := ColorAttrs;
        tfMonoAttrs  := MonoAttrs;
        TFP := TextFieldPtr(slNext);
      end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ResetPromptColor;
    {-Reset colors for prompts}
  var
    EFP : EntryFieldPtr;
  begin
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      with EFP^ do begin
        sfPromptColor := asColors.PromptColor;
        sfPromptMono := asColors.PromptMono;
        EFP := EntryFieldPtr(dlNext);
      end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ResetSelectedPromptColor;
    {-Reset colors for selected prompts}
  var
    EFP : EntryFieldPtr;
  begin
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      with EFP^ do begin
        sfSelPromptColor := asColors.SelPromptColor;
        sfSelPromptMono := asColors.SelPromptMono;
        EFP := EntryFieldPtr(dlNext);
      end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ResetProtectedPromptColor;
    {-Reset colors for protected prompts}
  var
    EFP : EntryFieldPtr;
  begin
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      with EFP^ do begin
        sfProPromptColor := asColors.ProPromptColor;
        sfProPromptMono := asColors.ProPromptMono;
        EFP := EntryFieldPtr(dlNext);
      end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ResetFieldColor;
    {-Reset colors for fields}
  var
    EFP : EntryFieldPtr;
  begin
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      with EFP^ do begin
        sfFieldColor := asColors.FieldColor;
        sfFieldMono := asColors.FieldMono;
        EFP := EntryFieldPtr(dlNext);
      end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ResetSelectedFieldColor;
    {-Reset colors for selected fields}
  var
    EFP : EntryFieldPtr;
  begin
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      with EFP^ do begin
        sfSelFieldColor := asColors.SelFieldColor;
        sfSelFieldMono := asColors.SelFieldMono;
        EFP := EntryFieldPtr(dlNext);
      end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ResetProtectedFieldColor;
    {-Reset colors for protected fields}
  var
    EFP : EntryFieldPtr;
  begin
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      with EFP^ do begin
        sfProFieldColor := asColors.ProFieldColor;
        sfProFieldMono := asColors.ProFieldMono;
        EFP := EntryFieldPtr(dlNext);
      end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ResetCtrlColor;
    {-Reset colors for control characters}
  var
    EFP : EntryFieldPtr;
  begin
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      with EFP^ do begin
        sfCtrlColor := asColors.CtrlColor;
        sfCtrlMono := asColors.CtrlMono;
        EFP := EntryFieldPtr(dlNext);
      end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ResetAllColors;  {!!.03}
    {-Reset all colors}
  begin
    ResetTextFieldColor;
    ResetHeaderColor;
    ResetShadowColor;
    ResetScrollBarColor;
    ResetPromptColor;
    ResetSelectedPromptColor;
    ResetProtectedPromptColor;
    ResetFieldColor;
    ResetSelectedFieldColor;
    ResetProtectedFieldColor;
    ResetCtrlColor;
  end;

  procedure CustomEntryScreen.RepositionTextField(TFP : TextFieldPtr); {!!.11}
    {-Move TFP to its proper position in the list of text fields}
  var
    P : TextFieldPtr;
    IsLess : Boolean;
  begin
    ForceReset; {!!.12}
    if asTextFields.Head = asTextFields.Tail then
      Exit;
    P := TextFieldPtr( TextFieldPtr(asTextFields.Head) );
    IsLess := False;
    while P <> nil do begin
      if P <> TFP then begin
        if TFP^.tfRow < P^.tfRow then
          IsLess := True
        else if P^.tfRow = TFP^.tfRow then
          IsLess := TFP^.tfCol < P^.tfCol;
        if IsLess then begin
          if Pointer(TFP^.slNext) <> P then begin
            asTextFields.Remove(TFP);
            asTextFields.PlaceBefore(TFP, P);
            RenumberTextFields;
            {ForceReset;} {!!.12}
          end;
          Exit;
        end;
      end;
      P := TextFieldPtr(P^.slNext);
    end;
  end;

  function CustomEntryScreen.TextFieldType(TFP : TextFieldPtr) : cesTextFieldType;  {!!.03}
    {-Return type of specified text field}
  begin
    with TFP^ do
      case tfFlags and (tfLine+tfVertical+tfBox+tfShadowed) of
        tfLine            : TextFieldType := cesHLine;
        tfLine+tfVertical : TextFieldType := cesVLine;
        tfBox             : TextFieldType := cesBox;
        tfBox+tfShadowed  : TextFieldType := cesShBox;
        else                TextFieldType := cesText;
      end;
  end;

  procedure CustomEntryScreen.DoneAddingLineOrBox; {!!.03}
    {-Called when finished adding a line or box field}
  var
    TFP : TextFieldPtr;
  begin
    TFP := FindLastTextField;
    if TFP <> nil then
{$IFDEF VIRTUALPASCAL}
      ClearFlag16(TFP^.tfFlags, tfMarkEnd);
{$ELSE}
      ClearFlag(TFP^.tfFlags, tfMarkEnd);
{$ENDIF}

    ClearFlag(cesFlags, cesMakeLine+cesMakeBox+cesMakeShBox);
  end;

  procedure CustomEntryScreen.MarkLastTextField; {!!.03}
    {-Mark the last text field}
  var
    TFP : TextFieldPtr;
  begin
    TFP := FindLastTextField;
    if TFP <> nil then
{$IFDEF VIRTUALPASCAL}
      ClearFlag16(TFP^.tfFlags, tfMarkEnd);
{$ELSE}
      ClearFlag(TFP^.tfFlags, tfMarkEnd);
{$ENDIF}

{$IFDEF VIRTUALPASCAL}
    SetFlag16(TextFieldPtr(asTextFields.Tail)^.tfFlags, tfMarkEnd);
{$ELSE}
    SetFlag(TextFieldPtr(asTextFields.Tail)^.tfFlags, tfMarkEnd);
{$ENDIF}
  end;

  function CustomEntryScreen.FindLastTextField : TextFieldPtr; {!!.03}
    {-Find the text field marked by MarkLastTextField}
  var
    TFP : TextFieldPtr;
  begin
    FindLastTextField := nil;
    TFP := TextFieldPtr(asTextFields.Head);
    while TFP <> nil do begin
      with TFP^ do
        if FlagIsSet(tfFlags, tfMarkEnd) then begin
          FindLastTextField := TFP;
          Exit;
        end;
      TFP := TextFieldPtr(TFP^.slNext);
    end;
  end;

  function CustomEntryScreen.AddTextFieldAtCursor(S : string) : TextFieldPtr; {!!.11} {rewritten}
    {-Add a text field at the position of the cursor}
  var
    TFP : TextFieldPtr;
  begin
    AddTextField(S, cesY, cesX);
    if GetLastError = 0 then begin
      TFP := TextFieldPtr(asTextFields.Tail);
      MarkLastTextField;
      RepositionTextField(TFP);
      AddTextFieldAtCursor := TFP;
    end
    else
      AddTextFieldAtCursor := nil;
  end;

  function CustomEntryScreen.AddLineFieldAtCursor(Ch1, Ch2, Ch3 : Char; {!!.11} {rewritten}
                                                  Len : Word;
                                                  Vert : Boolean) : TextLineFieldPtr;
    {-Add a (vertical) line field at the position of the cursor}
  var
    TLFP : TextLineFieldPtr;
  begin
    AddLineField(Ch1, Ch2, Ch3, cesY, cesX, Len, Vert);
    if GetLastError = 0 then begin
      TLFP := TextLineFieldPtr(asTextFields.Tail);
      MarkLastTextField;
      RepositionTextField(TLFP);
      AddLineFieldAtCursor := TLFP;
    end
    else
      AddLineFieldAtCursor := nil;
  end;

  function CustomEntryScreen.AddBoxFieldAtCursor(BoxChars : FrameArray;  {!!.11} {rewritten}
                                                 Wid, Ht : Word;
                                                 Shadowed : Boolean) : TextBoxFieldPtr;
    {-Add a (shadowed) box field at the position of the cursor}
  var
    TBFP : TextBoxFieldPtr;
  begin
    if Shadowed then
      AddShadowedBoxField(BoxChars, cesX, cesY, cesX+Pred(Wid), cesY+Pred(Ht))
    else
      AddBoxField(BoxChars, cesX, cesY, cesX+Pred(Wid), cesY+Pred(Ht));
    if GetLastError = 0 then begin
      TBFP := TextBoxFieldPtr(asTextFields.Tail);
      MarkLastTextField;
      RepositionTextField(TBFP);
      AddBoxFieldAtCursor := TBFP;
    end
    else
      AddBoxFieldAtCursor := nil;
  end;

  function CustomEntryScreen.CopyTextFieldToCursor(TFP : TextFieldPtr) : Boolean;
    {-Make a copy of a text field at the position of the cursor}
  var
    TLFP : TextLineFieldPtr absolute TFP;
    TBFP : TextBoxFieldPtr absolute TFP;
    TFT  : cesTextFieldType;
    FA : FrameArray;
  begin                                        {!!.11} {rewritten}
    with TLFP^, TBFP^ do begin
      Move(tfString^[1], FA, SizeOf(FrameArray));
      TFT := TextFieldType(TFP);
      case TFT of
        cesHLine, cesVLine :
          AddLineFieldCustom(
            tfString^[1], tfString^[2], tfString^[3], cesY, cesX, tlfLength,
            tfColorAttrs[0], tfMonoAttrs[0], (TFT = cesVLine) );
        cesBox :
          AddBoxFieldCustom(
            FA, cesX, cesY, cesX+(tbfColH-tfCol), cesY+(tbfRowH-tfRow),
            tfColorAttrs[0], tfMonoAttrs[0]);
        cesShBox :
          AddShadowedBoxFieldCustom(
            FA, cesX, cesY, cesX+(tbfColH-tfCol), cesY+(tbfRowH-tfRow),
            tfColorAttrs[0], tfMonoAttrs[0], tfColorAttrs[1], tfMonoAttrs[1]);
        else
          AddTextFieldCustom(
            tfString^, cesY, cesX, tfColorAttrs[0], tfMonoAttrs[0]);
      end;
    end;
    if GetLastError = 0 then begin
      MarkLastTextField;
      RepositionTextField( TextFieldPtr(asTextFields.Tail) );
      CopyTextFieldToCursor := True;
    end
    else
      CopyTextFieldToCursor := False;
  end;

  function CustomEntryScreen.MoveTextFieldToCursor(TFP : TextFieldPtr) : Boolean;
    {-Move the specified field to the position of the cursor}
  var
    TBFP : TextBoxFieldPtr absolute TFP;
  begin                                        {!!.11} {rewritten}
    with TBFP^ do
      {adjust the coordinates}
      if (tfRow = cesY) and (tfCol = cesX) then
        MoveTextFieldToCursor := False
      else begin
        MoveTextFieldToCursor := True;
        if FlagIsSet(tfFlags, tfBox) then begin
          Inc(tbfRowH, cesY-tfRow);
          Inc(tbfColH, cesX-tfCol);
        end;
        tfRow := cesY;
        tfCol := cesX;
        RepositionTextField(TFP);
        ForceReset;
    end;
  end;

  function CustomEntryScreen.FindTextFieldAtCursor(var TFP : TextFieldPtr;
                                                   TextOnly : Boolean) : Boolean; {!!.03}
    {-Returns True if there's a text field at the cursor}
  var
    TLFP : TextLineFieldPtr absolute TFP;
    TBFP : TextBoxFieldPtr absolute TFP;
  begin                                        {!!.03} {rewritten}
    FindTextFieldAtCursor := True;
    TFP := TextFieldPtr(asTextFields.Head);
    while TFP <> nil do begin
      with TLFP^, TBFP^ do
        case TextFieldType(TFP) of
          cesVLine :
            if not TextOnly then
              if (cesX = tfCol) then
                if (cesY >= tfRow) and (cesY <= tfRow+Pred(tfHeight)) then
                  Exit;
          cesHLine :
            if not TextOnly then
              if (cesY = tfRow) then
                if (cesX >= tfCol) and (cesX <= tfCol+Pred(tfWidth)) then
                  Exit;
          cesBox, cesShBox :
            if not TextOnly then
              if ( ((cesY = tfRow) or (cesY = tbfRowH)) and
                   ((cesX >= tfCol) and (cesX <= tbfColH)) ) or
                 ( ((cesX = tfCol) or (cesX = tbfColH)) and
                   ((cesY >= tfRow) and (cesY <= tbfRowH)) ) then
                Exit;
          else
            if (cesY = tfRow) and (cesX >= tfCol) and
               (cesX <= tfCol+Length(tfString^)-1) then
              Exit;
        end;
      TFP := TextFieldPtr(TFP^.slNext);
    end;
    FindTextFieldAtCursor := False;
  end;

  function CustomEntryScreen.ModifyTextField(TFP : TextFieldPtr;
                                             S : string) : Boolean;
    {-Change the text in the specified text field}
  var
    SP : StringPtr;
    SLen : Byte absolute S;
  begin
    ModifyTextField := True;

    {is the new string the same length?}
    if SLen = Length(TFP^.tfString^) then begin
      {just change the string}
      TFP^.tfString^ := S;
      ForceReset;
    end
    else begin
      SP := StringToHeap(S);
      if SP = nil then
        ModifyTextField := False
      else begin
        DisposeString(TFP^.tfString);
        TFP^.tfString := SP;
        ForceReset;
      end;
    end;
  end;

  procedure CustomEntryScreen.ChangeTextFieldAttrs(TFP : TextFieldPtr;
                                                   Color, Mono : Byte);
    {-Change the attributes of the specified text field}
  begin
    with TFP^ do begin
      FillChar(tfColorAttrs, SizeOf(tfColorAttrs), Color);
      FillChar(tfMonoAttrs, SizeOf(tfMonoAttrs), Mono);
    end;
    ResetScreen;
  end;

  procedure CustomEntryScreen.ChangeBoxFieldAttrs(TBFP : TextBoxFieldPtr; {!!.03}
                                                  BColor, BMono : Byte;
                                                  SColor, SMono : Byte);
    {-Change the attributes of the specified box field}
  begin
    ChangeTextFieldAttrs(TBFP, BColor, BMono);
    with TBFP^ do begin
      tfColorAttrs[1] := SColor;
      tfMonoAttrs[1] := SMono;
    end;
  end;

  procedure CustomEntryScreen.RenumberTextFields;
    {-Renumber all text fields}
  var
    I : Word;
    TFP : TextFieldPtr;
  begin
    I := 0;
    TFP := TextFieldPtr(asTextFields.Head);
    while TFP <> nil do begin
      TFP^.tfID := I;
      Inc(I);
      TFP := TextFieldPtr(TFP^.slNext);
    end;
  end;

  procedure CustomEntryScreen.DeleteTextField(TFP : TextFieldPtr);
    {-Delete the specified text field}
  begin
    asTextFields.Delete(TFP);
    RenumberTextFields;
    ForceReset;
  end;

  function CustomEntryScreen.FindEntryFieldAtCursor(var EFP : EntryFieldPtr) : Boolean;
    {-Returns True if there's an entry field at the cursor}
  var
    HD : Byte;

    function CursorInRange(Row, Col : Integer; fWidth : Byte) : Boolean;
      {-Return True if cursor is on this part of the field}
    begin
      {assume failure}
      CursorInRange := False;

      {get out if fWidth = 0}
      if fWidth = 0 then
        Exit;

      {check the row and column coordinates}
      CursorInRange :=
        (cesY = Row) and (cesX >= Col) and (cesX <= Col+Pred(fWidth));
    end;

  begin
    if HaveDummyField then
      FindEntryFieldAtCursor := False
    else begin
      HD := Ord((esLeftD <> #0) and (esRightD <> #0));
      FindEntryFieldAtCursor := True;
      EFP := EntryFieldPtr(asFields.Head);
      while EFP <> nil do begin
        with EFP^ do
          if CursorInRange(sfPRow, sfPCol, sfPWidth) or
             CursorInRange(sfFRow, sfFCol-HD, sfFWidth+HD+HD) then
               Exit;
        EFP := EntryFieldPtr(EFP^.dlNext);
      end;
      FindEntryFieldAtCursor := False;
    end;
  end;

  procedure CustomEntryScreen.GuessCurrentField;
    {-Set current field as close to cursor as possible}
  var
    EFP : EntryFieldPtr;
  begin
    {if we're on a field, reset the current field}
    if FindEntryFieldAtCursor(EFP) then
      SetNextField(EFP^.sfID)
    else begin
      asScrollVertically(cesY);
      if asNext = nil then
        if cesY > asMaxRow then
          asSeekToLast
        else
          asSeekToFirst;
      asCurrent := asNext;
      asScrollHorizontally(cesX);
    end;
  end;

  function CustomEntryScreen.HaveFieldOnCurrentLine : Boolean;
    {-Returns True if there's at least one field on the current line}
  var
    TFP : TextFieldPtr;
    EFP : EntryFieldPtr;
  begin
    HaveFieldOnCurrentLine := True;

    TFP := TextFieldPtr(asTextFields.Head);
    while TFP <> nil do begin
      if (cesY = TFP^.tfRow) then
        Exit;
      TFP := TextFieldPtr(TFP^.slNext);
    end;

    if not HaveDummyField then begin
      EFP := EntryFieldPtr(asFields.Head);
      while EFP <> nil do begin
        with EFP^ do
          if (sfPRow = cesY) or (sfFRow = cesY) then
            Exit;
        EFP := EntryFieldPtr(EFP^.dlNext);
      end;
    end;

    HaveFieldOnCurrentLine := False;
  end;

  procedure CustomEntryScreen.InsertLineAtCursor;  {!!.03} {rewritten}
    {-Insert a line at the position of the cursor}
  var
    TFP : TextFieldPtr;
    TLFP : TextLineFieldPtr absolute TFP;
    TBFP : TextBoxFieldPtr absolute TFP;
    TFT : cesTextFieldType;
    EFP : EntryFieldPtr;
    Changed : Boolean;
  begin
    {do nothing if beyond last field}
    if cesY > sesVS.vsHeight then
      Exit;

    {don't add too many lines}
    if sesVS.vsHeight = cesMaxRow then begin
      GotError(epWarning+ecTooManyLines, emTooManyLines);
      Exit;
    end;

    TFP := TextFieldPtr(asTextFields.Head);
    Changed := False;
    while TFP <> nil do begin
      with TLFP^, TBFP^ do begin
        TFT := TextFieldType(TFP);
        if (tfRow >= cesY) then begin
          Inc(tfRow);
          case TFT of
            cesBox, cesShBox : Inc(tbfRowH);
          end;
          Changed := True;
        end
        else case TFT of
          cesVLine :
            if (tfRow+tlfLength-1) >= cesY then begin
              Inc(tlfLength);
              Changed := True;
            end;
          cesBox, cesShBox :
            if (tbfRowH >= cesY) then begin
              Inc(tbfRowH);
              Changed := True;
            end;
        end;
        TFP := TextFieldPtr(slNext);
      end;
    end;
    if Changed then
      ForceReset;

    if HaveDummyField then
      Exit;

    EFP := EntryFieldPtr(asFields.Head);
    Changed := False;
    while EFP <> nil do begin
      with EFP^ do begin
        if (sfPRow >= cesY) then begin
          Inc(sfPRow);
          Changed := True;
        end;
        if (sfFRow >= cesY) then begin
          Inc(sfFRow);
          Changed := True;
        end;
        EFP := EntryFieldPtr(dlNext);
      end;
    end;

    if Changed then
      ForceReset;
  end;

  procedure CustomEntryScreen.DeleteLineAtCursor;  {!!.03} {rewritten}
    {-Delete the line the cursor is on}
  var
    TFP : TextFieldPtr;
    TLFP : TextLineFieldPtr absolute TFP;
    TBFP : TextBoxFieldPtr absolute TFP;
    TFT : cesTextFieldType;
    EFP : EntryFieldPtr;
    NextP : Pointer;
    Changed : Boolean;
  begin
    TFP := TextFieldPtr(asTextFields.Head);
    Changed := False;
    while TFP <> nil do begin
      with TLFP^, TBFP^ do begin
        NextP := slNext;
        TFT := TextFieldType(TFP);
        if (tfRow = cesY) then begin
          asTextFields.Delete(TFP);
          Changed := True;
        end
        else if (tfRow > cesY) then begin
          Dec(tfRow);
          case TFT of
            cesBox, cesShBox : Dec(tbfRowH);
          end;
          Changed := True;
        end
        else case TFT of
          cesVLine :
            if (tfRow+tlfLength-1) >= cesY then begin
              Dec(tlfLength);
              Changed := True;
            end;
          cesBox, cesShBox :
            if (tbfRowH >= cesY) then begin
              Dec(tbfRowH);
              Changed := True;
            end;
        end;
        TFP := TextFieldPtr(slNext);
      end;
      TFP := NextP;
    end;
    if Changed then begin
      RenumberTextFields;
      ForceReset;
    end;

    if HaveDummyField then
      Exit;

    EFP := EntryFieldPtr(asFields.Head);
    Changed := False;
    while EFP <> nil do begin
      with EFP^ do begin
        NextP := dlNext;
        if (sfPRow = cesY) or (sfFRow = cesY) then begin
          Changed := True;
          asFields.Delete(EFP);
          Dec(asCount);
        end
        else begin
          if (sfPRow > cesY) then begin
            Dec(sfPRow);
            Changed := True;
          end;
          if (sfFRow > cesY) then begin
            Dec(sfFRow);
            Changed := True;
          end;
        end;
      end;
      EFP := NextP;
    end;
    if Changed then begin
      {SortEntryFields;}   {!!.02}

      RenumberEntryFields;

      AddDummyField;

      {reallocate user record}
      if ReallocUserRecord then {won't fail};

      ForceReset;
    end;
  end;

  procedure CustomEntryScreen.AddDummyField;
    {-Add a dummy field to the entry screen if necessary}
  begin
    if asCount = 0 then
      {add dummy field--use help index to flag it as such}
      AddNestedField('', 1, 1, '', 1, 1, 1, $FFFF);
  end;

  function CustomEntryScreen.HaveDummyField : Boolean;
    {-Returns True if the dummy field is present}
  var
    EFP : EntryFieldPtr;
  begin
    EFP := EntryFieldPtr(asFields.Head);
    HaveDummyField := (EFP^.sfHelpIndex = $FFFF);
  end;

  procedure CustomEntryScreen.RemoveDummyField;
    {-Remove the dummy field}
  var
    EFP : EntryFieldPtr;
  begin
    if HaveDummyField then begin
      EFP := EntryFieldPtr(asFields.Head);

      asFields.Delete(EFP);
      Dec(asCount);

      RenumberEntryFields;
    end;
  end;

  procedure CustomEntryScreen.DeleteEntryField(EFP : EntryFieldPtr);
    {-Delete the specified entry field}
  begin
    asFields.Delete(EFP);
    Dec(asCount);

    RenumberEntryFields;

    AddDummyField;

    {reallocate user record}
    if ReallocUserRecord then {won't fail};

    ForceReset;
  end;

  procedure CustomEntryScreen.GetFieldCoordinates(EFP : EntryFieldPtr;
                                                  var XL, YL, XH, YH : Integer);
    {-Return the coordinates for the specified field}

    function GetXH(Col, Wid : Word) : Word;
    begin
      if Wid = 0 then
        GetXH := Col
      else
        GetXH := Col+Pred(Wid);
    end;

  begin
    with EFP^ do begin
      XL := MinWord(sfPCol, sfFCol);
      YL := MinWord(sfPRow, sfFRow);
      XH := MaxWord(GetXH(sfPCol, sfPWidth), GetXH(sfFCol, sfFWidth));
      YH := MaxWord(sfPRow, sfFRow);
    end;
  end;

  function CustomEntryScreen.MoveEntryFieldToCursor(EFP : EntryFieldPtr) : Boolean;
    {-Move the specified field to the position of the cursor}
  var
    XD, YD : Integer;
  begin
    with EFP^ do begin
      {calculate Delta}
      XD := Integer(cesX)-Integer(sfPCol);
      YD := Integer(cesY)-Integer(sfPRow);

      {does field need to be moved?}
      if (XD = 0) and (YD = 0) then
        MoveEntryFieldToCursor := False
      else begin
        {adjust the coordinates}
        Inc(sfPCol, XD);
        Inc(sfPRow, YD);
        Inc(sfFCol, XD);
        Inc(sfFRow, YD);

        {reset everything}
        SortEntryFields;
        ForceReset;

        MoveEntryFieldToCursor := True;
      end;
    end;
  end;

  procedure CustomEntryScreen.RenumberEntryFields;
    {-Renumber all entry fields}
  var
    EFP : EntryFieldPtr;
    I : Word;
  begin
    I := 0;
    EFP := EntryFieldPtr(asFields.Head);
    asKnown := EFP;                      {!!.02}
    while EFP <> nil do begin
      EFP^.sfID := I;
      Inc(I);
      EFP := EntryFieldPtr(EFP^.dlNext);
    end;
    asCount := I;                        {!!.12}
  end;

  procedure CustomEntryScreen.ExchangeFields(IP, JP : EntryFieldPtr); {!!.03} {rewritten}
    {-Exchange fields IP^ and JP^}
  var
    IWP : ^Word absolute IP;
    JWP : ^Word absolute JP;

    procedure ChangePrevsNext(EFP, P : DoubleNodePtr);
    begin
      EFP := EFP^.dlPrev;
      if EFP = nil then
        asFields.dlHead := P
      else if EFP^.dlNext <> P then
        EFP^.dlNext := P;
    end;

    procedure ChangeNextsPrev(EFP, P : DoubleNodePtr);
    begin
      EFP := EFP^.dlNext;
      if EFP = nil then
        asFields.dlTail := P
      else if EFP^.dlPrev <> P then
        EFP^.dlPrev := P;
    end;

  begin
    if SizeOf(IP^) = SizeOf(JP^) then begin
      {exchange the data}
      ExchangeStructs(
        IP^.sfNextID, JP^.sfNextID,
        SizeOf(IP^)-SizeOf(DoubleListNode)-SizeOf(IP^.sfID));

      {exchange the VMT pointers}
      ExchangeWords(IWP^, JWP^);
    end
    else begin
      ChangePrevsNext(IP, JP);
      ChangeNextsPrev(IP, JP);
      ChangePrevsNext(JP, IP);
      ChangeNextsPrev(JP, IP);
      if IP^.dlNext = Pointer(JP) then begin
        IP^.dlNext := JP^.dlNext;
        JP^.dlNext := IP;
        JP^.dlPrev := IP^.dlPrev;
        IP^.dlPrev := JP;
{$IFDEF VIRTUALPASCAL}
        ExchangeWord16s(IP^.sfID, JP^.sfID);
{$ELSE}
        ExchangeWords(IP^.sfID, JP^.sfID);
{$ENDIF}
      end
      else if JP^.dlNext = Pointer(IP) then begin
        JP^.dlNext := IP^.dlNext;
        IP^.dlNext := JP;
        IP^.dlPrev := JP^.dlPrev;
        JP^.dlPrev := IP;
{$IFDEF VIRTUALPASCAL}
        ExchangeWord16s(IP^.sfID, JP^.sfID);
{$ELSE}
        ExchangeWords(IP^.sfID, JP^.sfID);
{$ENDIF}
      end
      else with IP^ do
        ExchangeStructs(IP^.dlNext, JP^.dlNext, Ofs(sfNextID)-Ofs(dlNext));
    end;
  end;

  procedure CustomEntryScreen.SortEntryFields;
    {-Sort the entry fields based on Row,Col coordinates}
  const
    IEP : EntryFieldPtr = nil;
    JEP : EntryFieldPtr = nil;
  var
    Low, High : Word;
    I, J, K, Offset : Word;
    InOrder : Boolean;

    function IsLess(I, J : Word) : Boolean;
      {-Return True if EntryField I is less than EntryField J}
    var
      IP : SelectFieldPtr absolute IEP;
      JP : SelectFieldPtr absolute JEP;
    begin
      IP := FindField(I);
      JP := FindField(J);
      if IP^.sfFRow < JP^.sfFRow then
        IsLess := True
      else if IP^.sfFRow > JP^.sfFRow then
        IsLess := False
      else
        IsLess := IP^.sfFCol < JP^.sfFCol;
    end;

  begin
    if asCount = 0 then    {!!.01}
      Exit;                {!!.01}
    Low := 0;
    High := asCount-1;
    Offset := High;
    while Offset > Low do begin
      Offset := (Pred(Low)+Offset) shr 1;
      repeat
        InOrder := True;
        K := Pred(Low)+High-Offset;
        I := Offset;
        for J := Low to K do begin
          Inc(I);
          if IsLess(I, J) then begin
            {exchange the fields}
            ExchangeFields(IEP, JEP);

            {not in order yet}
            InOrder := False;
          end;
        end;
      until InOrder;

      {reset current, next field pointers}
      asCurrent := Pointer(asFields.Head);
      asNext := asCurrent;
      asKnown := asNext;                {!!.02}
    end;
  end;

  procedure CustomEntryScreen.InitUserRecord;
    {-Initialize the user record}
  var
    EFP : EntryFieldPtr;
    TOP : Pointer;
  begin
    if esUserRecPtr = nil then
      Exit;

    {initialize whole thing with 0's}
    FillChar(esUserRecPtr^, esUserRecSize, 0);

    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do begin
      {make sure that all fields are initialized to a legal value}
      TOP := TypeOf(EFP^);
      with EFP^ do
        if TOP = TypeOf(ArrayField) then
          FillChar(efVarPtr^, efDataSize, ' ')
        else if TOP = TypeOf(CharField) then begin
          if (efRangeLo.rtChar <= ' ') and (efRangeHi.rtChar >= ' ') then
            Char(efVarPtr^) := ' '
          else if (efRangeLo.rtChar = efRangeHi.rtChar) then
            Char(efVarPtr^) := ' '
          else
            Char(efVarPtr^) := efRangeLo.rtChar
        end
        else if TOP = TypeOf(LongIntField) then begin
          if (efRangeLo.rtLong > 0) or (efRangeHi.rtLong < 0) then
            LongInt(efVarPtr^) := efRangeLo.rtLong;
        end
        else if TOP = TypeOf(WordField) then begin
          if (efRangeLo.rtLong > 0) or (efRangeHi.rtLong < 0) then
            Word(efVarPtr^) := efRangeLo.rtLong;
        end
        else if TOP = TypeOf(IntegerField) then begin
          if (efRangeLo.rtLong > 0) or (efRangeHi.rtLong < 0) then
            Integer(efVarPtr^) := efRangeLo.rtLong;
        end
        else if TOP = TypeOf(ByteField) then begin
          if (efRangeLo.rtLong > 0) or (efRangeHi.rtLong < 0) then
            Byte(efVarPtr^) := efRangeLo.rtLong;
        end
        else if TOP = TypeOf(ShortIntField) then begin
          if (efRangeLo.rtLong > 0) or (efRangeHi.rtLong < 0) then
            ShortInt(efVarPtr^) := efRangeLo.rtLong;
        end
        else if TOP = TypeOf(RealField) then begin
          if (efRangeLo.rtReal > 0) or (efRangeHi.rtReal < 0) then
            Real(efVarPtr^) := efRangeLo.rtReal
          else
            Real(efVarPtr^) := BadReal;
        end
        {$IFDEF UseBcd}
        else if TOP = TypeOf(BcdField) then begin
          if GreaterBCD(efRangeLo.rtBcd, ZeroBcd) or
             LessBCD(efRangeHi.rtBcd, ZeroBcd) then
            Bcd(efVarPtr^) := efRangeLo.rtBcd
          else
            Bcd(efVarPtr^) := BadBcd;
        end
        {$ENDIF}
        {$IFOPT N+}
        else if TOP = TypeOf(ExtendedField) then begin
          if (efRangeLo.rtExt > 0) or (efRangeHi.rtExt < 0) then
            Extended(efVarPtr^) := efRangeLo.rtExt
          else
            Extended(efVarPtr^) := BadExt;
        end
        else if TOP = TypeOf(DoubleField) then begin
          if (efRangeLo.rtExt > 0) or (efRangeHi.rtExt < 0) then
            Double(efVarPtr^) := efRangeLo.rtExt
          else
            Double(efVarPtr^) := BadDbl;
        end
        else if TOP = TypeOf(SingleField) then begin
          if (efRangeLo.rtExt > 0) or (efRangeHi.rtExt < 0) then
            Single(efVarPtr^) := efRangeLo.rtExt
          else
            Single(efVarPtr^) := BadSgl;
        end
        else if TOP = TypeOf(CompField) then begin
          if (efRangeLo.rtExt > 0) or (efRangeHi.rtExt < 0) then
            Comp(efVarPtr^) := efRangeLo.rtExt
          else
            Comp(efVarPtr^) := BadComp;
        end
        {$ENDIF}
        {$IFDEF UseDates}
        else if TOP = TypeOf(DateField) then begin
          if efRangeLo.rtDate = efRangeHi.rtDate then
            Date(efVarPtr^) := BadDate
          else
            Date(efVarPtr^) := efRangeLo.rtDate;
        end
        else if TOP = TypeOf(TimeField) then begin
          if efRangeLo.rtTime = efRangeHi.rtTime then
            Time(efVarPtr^) := BadTime
          else
            Time(efVarPtr^) := efRangeLo.rtTime;
        end;
        {$ELSE}
        ;
        {$ENDIF}

      EFP := EntryFieldPtr(EFP^.dlNext);
    end;
  end;

  function CustomEntryScreen.ReallocUserRecord : Boolean;
    {-Reallocate the user record}
  var
    EFP : EntryFieldPtr;
    I : Word;
    P : Pointer;
    PSO : OS absolute P;
  begin
    {dispose of existing user record}
    FreeMemCheck(esUserRecPtr, esUserRecSize);

    {reset variables}                           {!!.01}
    esUserRecPtr := nil;                        {!!.01}
    esUserRecSize := 0;                         {!!.01}
    ClearByteFlag(asOptions, esDeallocUserRec); {!!.01}
    if HaveDummyField then begin                {!!.01}
      ReallocUserRecord := True;                {!!.01}
      Exit;                                     {!!.01}
    end;                                        {!!.01}

    {calculate sizes of all fields}
    EFP := EntryFieldPtr(asFields.Head);
    I := 0;
    while EFP <> nil do begin
      Inc(I, EFP^.efDataSize);
      EFP := EntryFieldPtr(EFP^.dlNext);
    end;

    {allocate new user record}
    if not GetMemCheck(P, I) then begin
      if not GetMemCheck(esUserRecPtr, esUserRecSize) then ;
      ReallocUserRecord := False;
      Exit;
    end;

    {save the new pointer and record size}
    esUserRecPtr := P;
    esUserRecSize := I;
    SetByteFlag(asOptions, esDeallocUserRec);

    {reset all variable pointers}
    EFP := EntryFieldPtr(asFields.Head);
    while EFP <> nil do
      with EFP^ do begin
        efVarPtr := P;
        Inc(PSO.O, efDataSize);
        EFP := EntryFieldPtr(dlNext);
      end;

    {initialize user record}
    InitUserRecord;

    ReallocUserRecord := True;
  end;

  function CustomEntryScreen.cesReadRow(Row : Word) : string;
    {-Returns the contents of the specified row}
  var
    S : string;
    SLen : Byte absolute S;
  begin
    if Row > sesVS.vRows then
      cesReadRow := ''
    else begin
      sesVS.Activate;
      FastRead(VirtualWidth, Row, 1, S);
      sesVS.Deactivate;
      VirtualSegment := VideoSegment;
      while S[SLen] = ' ' do
        Dec(SLen);
      cesReadRow := S;
    end;
  end;

  constructor CustomEntryScreen.Load(var S : IdStream);
    {-Load a custom entry screen from a stream}
  begin
    {Load the underlying scrolling entry screen}
    if not ScrollingEntryScreen.Load(S) then
      Fail;

    {just in case}
    RenumberEntryFields; {!!.12}

    {read data specific to the custom entry screen}
    S.ReadRange(cesSliderChar, cesDummy);
    cesX := 1;
    cesY := 1;
    cesFlags := 0;         {!!.03}

    {initialize user record}
    InitUserRecord;
  end;

  procedure CustomEntryScreen.Store(var S : IdStream);
    {-Store a custom entry screen in a stream}
  begin
    {Store the scrolling entry screen}
    ScrollingEntryScreen.Store(S);
    if S.PeekStatus <> 0 then
      Exit;

    {Write data specific to the custom entry screen}
    S.WriteRange(cesSliderChar, cesDummy);
  end;

  procedure CustomEntryScreenStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing scrolling entry screens}
  begin
    {register all pertinent types}
    ScrollingEntryScreenStream(SPtr);
    ExplodingWindowStream(SPtr);

    AllPictureFieldsStream(SPtr);
    AllSimpleFieldsStream(SPtr);
    AllNumericFieldsStream(SPtr);
    NestedFieldStream(SPtr);

    SPtr^.RegisterType(otCustomEntryScreen, veCustomEntryScreen,
                       TypeOf(CustomEntryScreen),
                       @CustomEntryScreen.Store, @CustomEntryScreen.Load);
  end;

{$IFDEF InitAllUnits}   {!!.01}
begin
{$ENDIF}                {!!.01}
end.
