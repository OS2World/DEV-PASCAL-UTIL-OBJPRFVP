{$S-,I-,F+}
{$V-}                        {<- required for OPENTRY}
{$M 16384,16384,600000}

{*********************************************************}
{*                   BEST.PAS 1.30                       *}
{*    An example program for Object Professional 1.30    *}
{*     Copyright (c) TurboPower Software 1988, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program requires that OPDEFINE.INC activate the following defines:
   UseAdjustableWindows
 This program will use features activated with the following defines:
   UseMouse, UseScrollBars, UseHotSpots, UseShadows
 ***************************************************************************}

{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

program BigEntryScreenTest;
  {-Demonstrates scrollable entry screens}
uses
  Use32,
  opcrt,
  opstring,
  opconst,           {!!.20}
  oproot,
  {$IFDEF UseMouse}
  opmouse,
  {$ENDIF}
  opcmd,
  opframe,
  opwindow,
  opfield,
  opselect,
  opentry;

const
  OurColorSet : ColorSet = (
    TextColor            : $1B; TextMono            : $07;
    CtrlColor            : $71; CtrlMono            : $70;
    FrameColor           : $17; FrameMono           : $0F;
    HeaderColor          : $71; HeaderMono          : $70;
    ShadowColor          : $08; ShadowMono          : $70;
    HighlightColor       : $71; HighlightMono       : $70;
    PromptColor          : $1B; PromptMono          : $07;
    SelPromptColor       : $1B; SelPromptMono       : $07;
    ProPromptColor       : $1B; ProPromptMono       : $07;
    FieldColor           : $1F; FieldMono           : $0F;
    SelFieldColor        : $71; SelFieldMono        : $70;
    ProFieldColor        : $1F; ProFieldMono        : $0F;
    ScrollBarColor       : $17; ScrollBarMono       : $07;
    SliderColor          : $17; SliderMono          : $07;
    HotSpotColor         : $71; HotSpotMono         : $07;
    BlockColor           : $2F; BlockMono           : $0F;
    MarkerColor          : $0F; MarkerMono          : $70;
    DelimColor           : $1F; DelimMono           : $07;
    SelDelimColor        : $71; SelDelimMono        : $70;
    ProDelimColor        : $1F; ProDelimMono        : $07;
    SelItemColor         : $2F; SelItemMono         : $70;
    ProItemColor         : $17; ProItemMono         : $07;
    HighItemColor        : $1F; HighItemMono        : $0F;
    AltItemColor         : $1F; AltItemMono         : $0F;
    AltSelItemColor      : $2F; AltSelItemMono      : $70;
    FlexAHelpColor       : $1F; FlexAHelpMono       : $0F;
    FlexBHelpColor       : $1F; FlexBHelpMono       : $0F;
    FlexCHelpColor       : $1B; FlexCHelpMono       : $70;
    UnselXrefColor       : $1E; UnselXrefMono       : $09;
    SelXrefColor         : $5F; SelXrefMono         : $70;
    MouseColor           : $4F; MouseMono           : $70
  );
  SinWindowFrame : FrameArray = 'ÚÀ¿ÙÄÄ³³';
  Step           = 1;
  MaxRows        = 68;       {absolute maximum number of rows}
  MinRows        = 1;        {absolute minimum number of rows}
  DefRows        = MaxRows;  {default number of rows}
  RowsInUse      : Word = DefRows; {current number of rows}
  MaxCols        = 6;        {maximum number of columns across}
  MaxLen         = 5;        {width of strings to edit}
  InsufficientMemory : string[19] = 'Insufficient memory';
var
  SES            : ScrollingEntryScreen;
  Scrap          : array[1..MaxRows, 1..MaxCols] of string[MaxLen];
  I, Row, Col, C : Word;
  BoxAttr        : Byte;
  BoxTextAttr    : Byte;
  VideoMode      : Byte;

  procedure ClearStatusLine;
    {-Clear the status line}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    with OurColorSet do
      FastWrite(CharStr(' ', 80), ScreenHeight, 1, ColorMono(HighlightColor, HighlightMono));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure Message(Msg : string; Centered : Boolean);
    {-Display a message on the status line}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    if Centered then
      Msg := Center(Msg, ScreenWidth)
    else
      Msg := Pad(Msg, ScreenWidth);

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    with OurColorSet do
      FastWrite(Msg, ScreenHeight, 1, ColorMono(HighlightColor, HighlightMono));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure ErrorProc(UnitCode : Byte; var ErrCode : Word; Msg : string);
    {-Error handler}
  var
    I : Word;
  begin
    Msg := ' '+Msg+'. Press any key...';
    Message(Msg, False);

    NormalCursor;
    GotoXYabs(Length(Msg)+1, ScreenHeight);

    I := ReadKeyWord;

    ClearStatusLine;
  end;

  procedure FatalError(Msg : string);
    {-Display an error message and halt}
  var
    I : Word;
  begin
    I := 0;
    ErrorProc(ucNone, I, Msg);

    SES.Erase;

    {$IFDEF UseMouse}
    HideMouse;
    {$ENDIF}

    ClrScr;
    Halt;
  end;

  procedure HideFields;
    {-Hide/unhide fields based on the current value of RowsInUse}
  const
    LastRowsInUse  : Word = 0;
  var
    I, Row, Col    : Word;
  begin
    {don't do all this if we don't need to}
    if RowsInUse = LastRowsInUse then
      Exit;

    {unhide fields in use}
    I := 1;
    for Row := 1 to RowsInUse do
      for Col := 1 to MaxCols do begin
        {unhide the field}
        SES.ChangeHidden(I, False);
        Inc(I);
      end;

    {hide fields not in use}
    for Row := RowsInUse+1 to MaxRows do
      for Col := 1 to MaxCols do begin
        {hide the field}
        SES.ChangeHidden(I, True);
        Inc(I);
      end;

    {save this for later}
    LastRowsInUse := RowsInUse;
  end;

  procedure PostEdit(ESP : EntryScreenPtr);
    {-Called after a field has been edited}
  begin
    with ESP^ do
      {update entry screen if RowsInUse changed}
      if GetCurrentID = 0 then
        HideFields;
  end;

  procedure InstallUserCommands;
    {-Install user-defined exit commands}
  begin
    with EntryCommands do begin
      {ccUser0 = move window: AltM}
      AddCommand(ccUser0, 1, $3200, 0);

      {ccUser1 = resize window: AltR}
      AddCommand(ccUser1, 1, $1300, 0);

      {ccUser2 = zoom window: AltZ}
      AddCommand(ccUser2, 1, $2C00, 0);

      {ccUser3: toggle scrolling modes: AltS}
      AddCommand(ccUser3, 1, $1F00, 0);

      {check for errors}
      if GetLastError <> 0 then
        RingBell;
    end;
  end;

  procedure ToggleZoom;
    {-Toggle zoom status of the browse window}
  var
    Redraw : Boolean;
  begin
    with SES, wFrame do begin
      if IsZoomed then begin
        ChangeHeaderString(5, #24, Redraw);
        Unzoom;
      end
      else begin
        ChangeHeaderString(5, #18, Redraw);
        Zoom;
      end;

      if ClassifyError(GetLastError) = etFatal then
        FatalError(InsufficientMemory);
    end;
  end;

  procedure KeyboardMove;
    {-Handle a keyboard event}
  var
    Finished : Boolean;
  begin
    Message(' Use cursor keys to move, <Enter> to accept', True);
    Finished := False;
    with SES do
      repeat
        case ReadKeyWord of
          $4700 : MoveWindow(-Step, -Step); {Home}
          $4800 : MoveWindow(0, -Step);     {Up arrow}
          $4900 : MoveWindow(Step, -Step);  {PgUp}
          $4B00 : MoveWindow(-Step, 0);     {Left Arrow}
          $4D00 : MoveWindow(Step, 0);      {Right Arrow}
          $4F00 : MoveWindow(-Step, Step);  {End}
          $5000 : MoveWindow(0, Step);      {Down arrow}
          $5100 : MoveWindow(Step, Step);   {PgDn}
          $1C0D : Finished := True;         {Enter}
        end;

        if ClassifyError(GetLastError) = etFatal then
          FatalError(InsufficientMemory);
      until Finished;
    ClearStatusLine;
  end;

  procedure KeyboardResize;
    {-Handle a keyboard event}
  var
    Finished : Boolean;
  begin
    Message(' Use cursor keys to resize, <Enter> to accept', True);
    Finished := False;
    with SES do
      repeat
        case ReadKeyWord of
          $4700 : ResizeWindow(-Step, -Step); {Home}
          $4800 : ResizeWindow(0, -Step);     {Up}
          $4900 : ResizeWindow(Step, -Step);  {PgUp}
          $4B00 : ResizeWindow(-Step, 0);     {Left}
          $4D00 : ResizeWindow(Step, 0);      {Right}
          $4F00 : ResizeWindow(-Step, Step);  {End}
          $5000 : ResizeWindow(0, Step);      {Down}
          $5100 : ResizeWindow(Step, Step);   {PgDn}
          $1C0D : Finished := True;           {Enter}
        end;

        if ClassifyError(GetLastError) = etFatal then
          FatalError(InsufficientMemory);
      until Finished;
    ClearStatusLine;
  end;

  function Delta(I : Integer) : Integer;
  begin
    if I < -4 then
      Delta := -1
    else if I > 4 then
      Delta := 1
    else
      Delta := 0;
  end;

  procedure CallEdit;
    {-Call the editor}
  var
    AllDone : Boolean;
    Clicked : Boolean;
    FP : FramePosType;
    HC : Byte;
    BP : LongInt;
    XAbs : Byte;
    YAbs : Byte;
    MicH : Integer;
    MicV : Integer;
  begin
    {$IFDEF UseMouse}
    HideMouse;
    {$ENDIF}

    {clear the screen}
    TextChar := #178;
    TextAttr := $07;
    ClrScr;

    ClearStatusLine;

    {$IFDEF UseMouse}
    ShowMouse;
    {$ENDIF}

    AllDone := False;
    with SES do
      repeat
        Process;

        case GetLastCommand of
          ccUser0 :      {move window}
            if not IsZoomed then        {!!.01}
              KeyboardMove;

          ccUser1 :      {resize window}
            if not IsZoomed then        {!!.01}
              KeyboardResize;

          ccUser2 :      {zoom window}
            ToggleZoom;

          ccUser3 :      {toggle scroll by page}
            begin
              {toggle the scroll-by-page setting}
              if esOptionsAreOn(esScrollbyPage) then
                esOptionsOff(esScrollbyPage)
              else
                esOptionsOn(esScrollbyPage);

              {ring bell to acknowledge the command}
              RingBell;
            end;

          {$IFDEF UseMouse}
          ccMouseSel :
            begin
              XAbs := MouseLastX+MouseXLo;
              YAbs := MouseLastY+MouseYLo;
              EvaluatePos(XAbs, YAbs);
              BP := PosResults(FP, HC);
              if FP <> frOutsideFrame then
                case HC of
                  hsRegion0 : {Close}
                    AllDone := True;

                  hsRegion1 : {Zoom}
                    ToggleZoom;

                  hsRegion2 : {Move}
                    if not IsZoomed then begin    {!!.01}
                      Message('Drag window to new location, then click left mouse button', True);
                      HideMouse;
                      Dec(XAbs, wFrame.frXL);
                      Dec(YAbs, wFrame.frYL);
                      GetMickeyCount(MicH, MicV);
                      repeat
                        GetMickeyCount(MicH, MicV);
                        MoveWindow(Delta(MicH), Delta(MicV));
                        if ClassifyError(GetLastError) = etFatal then
                          FatalError(InsufficientMemory);
                        if MousePressed then
                          Clicked := (MouseKeyWord = MouseLft)
                        else
                          Clicked := False;
                      until Clicked;
                      Inc(XAbs, wFrame.frXL);
                      Inc(YAbs, wFrame.frYL);
                      MouseGoToXY(XAbs, YAbs);
                      ClearStatusLine;
                      ShowMouse;
                    end;

                  hsRegion3 : {Resize}
                    if not IsZoomed then begin    {!!.01}
                      Message('Move mouse to resize window, then click left mouse button', True);
                      HideMouse;
                      GetMickeyCount(MicH, MicV);
                      repeat
                        GetMickeyCount(MicH, MicV);
                        ResizeWindow(Delta(MicH), Delta(MicV));
                        if ClassifyError(GetLastError) = etFatal then
                          FatalError(InsufficientMemory);
                        if MousePressed then
                          Clicked := (MouseKeyWord = MouseLft)
                        else
                          Clicked := False;
                      until Clicked;
                      MouseGoToXY(wFrame.frXH, wFrame.frYH);
                      ClearStatusLine;
                      ShowMouse;
                    end;
                end;
            end;
          {$ENDIF}

          ccError, ccQuit, ccDone :
            AllDone := True;
        end;
      until AllDone;
  end;

begin
  {initialize the strings we're editing}
  FillChar(Scrap, SizeOf(Scrap), 0);

  {install user-defined exit commands}
  InstallUserCommands;

  {$IFDEF UseMouse}
  if MouseInstalled then begin
    {use a red diamond for our mouse cursor}
    with OurColorSet do
      SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+$04);
    ShowMouse;

    {enable mouse support}
    EntryCommands.cpOptionsOn(cpEnableMouse);
  end;
  {$ENDIF}

  {initialize entry screen}
  if not SES.InitCustom(
    11, 4, 70, 22, OurColorSet, DefWindowOptions or wBordered) then
    Halt;

  with SES, wFrame do begin
    {set window options}
    SetFrameType(SinWindowFrame);
    EnableExplosions(10);

    {$IFDEF UseShadows}
    AddShadow(shBR, shSeeThru);
    {$ENDIF}

    {don't let the entry screen be moved on top of the status line}
    SetPosLimits(1, 1, ScreenWidth, ScreenHeight-1);

    {$IFDEF UseScrollBars}
    {add scroll bars}
    AddCustomScrollBar(frBB, 0, MaxLongInt, 1, 1, #178, #176, OurColorSet);
    AddCustomScrollBar(frRR, 0, MaxLongInt, 1, 1, #178, #176, OurColorSet);
    {$ENDIF}

    {add headers}
    AddHeader(' Big Entry Screen Test ', heTC);         {0}
    AddCustomHeader(#180, frTL,  1, 0, $17, $0F);       {1}
    AddCustomHeader(#7,   frTL,  2, 0, $71, $70);       {2}
    AddCustomHeader(#195, frTL,  3, 0, $17, $0F);       {3}
    AddCustomHeader(#180, frTR, -3, 0, $17, $0F);       {4}
    AddCustomHeader(#24,  frTR, -2, 0, $71, $70);       {5}
    AddCustomHeader(#195, frTR, -1, 0, $17, $0F);       {6}

    {$IFDEF UseHotSpots}
    {add hot spots}
    AddHotRegion(frTL, hsRegion0, 2, 0, 1, 1);          {Close}
    AddHotRegion(frTR, hsRegion1, -2, 0, 1, 1);         {Zoom}
    AddHotBar(frTT,    hsRegion2);                      {Move}
    AddHotRegion(frBR, hsRegion3, 0, 0, 1, 1);          {Resize}
    {$ENDIF}

    {set procedure pointers}
    SetPostEditProc(PostEdit);
    SetErrorProc(ErrorProc);

    {set field options}
    esFieldOptionsOn(efTrimBlanks+efAutoAdvance+efClearFirstChar);

    {set entry screen options}
    esOptionsOn(esScrollbyPage);
    SetWrapMode(StopAtEdges);
    SetDelimiters('[', ']');

    {first field allows user to change number of visible rows}
    AddWordField(
      'Rows in entry screen [1-68]:', {prompt}
      1, 2,                       {row,col for prompt}
      '99',                       {picture mask for field}
      1, 33,                      {row,col for field}
      0,                          {help index}
      MinRows,                    {minimum value}
      MaxRows,                    {maximum value}
      RowsInUse);                 {word variable to edit}

    {construct the rest of the entry screen}
    I := 1;
    for Row := 1 to MaxRows do
      for Col := 1 to MaxCols do begin
        C := (20*Pred(Col))+2;
        AddStringField(
          'Field '+Long2Str(I),   {prompt}
          Row+1, C,               {row,col for prompt}
          '',                     {picture mask for field}
          Row+1, C+11,            {row,col for field}
          MaxLen,                 {width of field}
          I,                      {help index}
          Scrap[Row,Col]);        {string variable to edit}
        Inc(I);
      end;

    {allocate the virtual screen}
    AllocateScreen;

    {hide/unhide fields}
    HideFields;

    CallEdit;

    {erase the entry screen}
    Erase;
  end;

  {dispose of the entry screen (not really necessary here)} {!!.13} {moved}
  SES.Done;                                                 {!!.13} {down}

  {clean up screen}
  {$IFDEF UseMouse}
  HideMouse;
  {$ENDIF}

  ClrScr;
end.
