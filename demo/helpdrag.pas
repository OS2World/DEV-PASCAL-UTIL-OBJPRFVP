(*

  HELPDRAG
  --------
  This program is an example of using the OPDRAG unit with a help window.
  Points worth noting:

     1) If UseDrag is defined in OPDEFINE.INC, or if UseDragAnyway is defined
        below, the OPDRAG unit will be used.

     2) By changing comment braces, you can try some different options:
        paged or scrolling help window; the pkSelectOnClick option, which
        affects both the pick list and the help cross-selection behavior with
        mouse clicks.

     3) The ToggleHelpZoom procedure shows how a routine can work in concert
        with the HandleMousePress routine, in this case to modify window
        headers when a hot spot is selected.

     4) HELPDRAG uses the ENTRY.HLP file by default, but will read another
        file if its complete name is specified on the DOS command line.

*)

{$R-,S-}
{$I OPDEFINE.INC}

{$IFDEF UseDrag}
  {$DEFINE UsingDrag}
{$ELSE}
  {.$DEFINE UseDragAnyway} {<--- define this to force use of OPDRAG}
  {$IFDEF UseDragAnyway}
    {$DEFINE UsingDrag}
  {$ENDIF}
{$ENDIF}

{$IFNDEF UseHotSpots}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseMouse}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

program HelpDrag;
uses
  Dos,
  OpConst,
  OpRoot,
  OpString,
  OpCrt,
  OpMouse,
  OpCmd,
  OpFrame,
  OpWindow,
  {$IFDEF UsingDrag}
  OpDrag,
  {$ENDIF}
  OpPick,
  OpHelp;

const
  DefColors  : Colorset = (
    TextColor       : $1E; TextMono       : $07;
    CtrlColor       : $1E; CtrlMono       : $0F;
    FrameColor      : $13; FrameMono      : $07;
    HeaderColor     : $3F; HeaderMono     : $70;
    ShadowColor     : $08; ShadowMono     : $0F;
    HighlightColor  : $4F; HighlightMono  : $70;
    PromptColor     : $30; PromptMono     : $07;
    SelPromptColor  : $30; SelPromptMono  : $07;
    ProPromptColor  : $30; ProPromptMono  : $07;
    FieldColor      : $1E; FieldMono      : $07;
    SelFieldColor   : $31; SelFieldMono   : $0F;
    ProFieldColor   : $17; ProFieldMono   : $07;
    ScrollbarColor  : $13; ScrollbarMono  : $07;
    SliderColor     : $13; SliderMono     : $0F;
    HotspotColor    : $30; HotspotMono    : $70;
    BlockColor      : $3E; BlockMono      : $0F;
    MarkerColor     : $5F; MarkerMono     : $70;
    DelimColor      : $31; DelimMono      : $0F;
    SelDelimColor   : $31; SelDelimMono   : $0F;
    ProDelimColor   : $31; ProDelimMono   : $0F;
    SelItemColor    : $3E; SelItemMono    : $70;
    ProItemColor    : $17; ProItemMono    : $07;
    HighItemColor   : $1F; HighItemMono   : $0F;
    AltItemColor    : $1F; AltItemMono    : $0F;
    AltSelItemColor : $3F; AltSelItemMono : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono  : $0F;
    FlexBHelpColor  : $1C; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono  : $70;
    UnselXrefColor  : $1A; UnselXrefMono  : $09;
    SelXrefColor    : $5F; SelXrefMono    : $70;
    MouseColor      : $4F; MouseMono      : $70
  );

var
  {$IFDEF UseDragAnyway}
  HelpCommands : DragProcessor;
  {$ENDIF}
  {$IFDEF UsingDrag}
  ZoomHeaderNum : byte;
  {$ENDIF}
  HelpFile : pathstr;
  MAttr : Word;
  Finished : Boolean;
  M : scrollinghelpwindow;
(*
  M : pagedhelpwindow;
*)

{$IFDEF UsingDrag}
procedure ToggleHelpZoom(var M : AbstractHelpWindow);
  {-change the zoom headers on the two help frames}
const
  ZoomedChar = #18;
  NotZoomedChar = #24;
var
  ZChar : Char;
  Redraw : Boolean;
begin
  if ZoomHeaderNum = 255 then
    exit;
  if M.IsZoomed then
    ZChar := ZoomedChar
  else
    ZChar := NotZoomedChar;
  M.ChangeHeader(ZoomHeaderNum, ZChar);
  M.hwFrame.ChangeHeaderString(ZoomHeaderNum, ZChar, Redraw);
end;
{$ENDIF}

begin
  {this demo requires mouse support}
  if not MouseInstalled then begin
    WriteLn('boring without a mouse');
    halt;
  end;

  {initialize the screen}
  TextChar := #176;
  ClrScr;

  MAttr := Word(ColorMono(DefColors.MouseColor, DefColors.MouseMono)) shl 8;

{$IFDEF UsingDrag}
  {$IFDEF UseDragAnyway}
  if not HelpCommands.Init(@HelpKeySet, HelpKeyMax) then
    Halt;
  HelpCommands.SetMouseCursor(MAttr or $04, MAttr or $12, MAttr or $1D);
  {$ENDIF}
{$ELSE}
  HelpCommands.cpOptionsOn(cpEnableMouse);
  SoftMouseCursor($0000, MAttr or $04);
{$ENDIF}

  {create a help window}
  if ParamCount > 0 then
    HelpFile := ParamStr(1)
  else
    HelpFile := 'ENTRY.HLP';
  if not M.InitCustom(9, 8, 72, 18, DefColors,
                      DefWindowOptions or wBordered,
                      HelpFile, PickVertical) then Halt;

(*
  {enable this to have clicks take effect immediately}
  M.pkOptionsOn(pkSelectOnClick);
*)

  {keep the topic name list in memory for faster moves}
  M.hwOptionsOn(hwStaticNameBuffer+hwHighlightXref);

{$IFDEF UseDragAnyway}
  {attach the DragProcessor to the help window}
  M.SetCommandProcessor(HelpCommands);
{$ENDIF}

{$IFDEF UsingDrag}
  {add hot spot for zooming}
  M.wFrame.AddCustomHeader(#24, frtr, -1, 0,
                           DefColors.HeaderColor, DefColors.HeaderMono);
  M.wFrame.AddHotRegion(frTR, ZoomHotCode, -1, 0, 1, 1);
  ZoomHeaderNum := M.wFrame.GetLastHeaderIndex;

  {add hot spots for moving and resizing}
  M.wFrame.AddHotBar(frTT, MoveHotCode);
  M.wFrame.AddCustomHeader(#240, frBR, 0, 0,
                           DefColors.FrameColor, DefColors.FrameMono);
  M.wFrame.AddHotRegion(frBR, ResizeHotCode, 0, 0, 1, 1);

{$ELSE}
  {scrolling by line is too slow without dragging}
  M.pkOptionsOn(pkMousePage);
  M.hwOptionsOn(hwMousePage);
{$ENDIF}

  {add scroll bars for demo purposes}
  M.wFrame.AddScrollBar(frRR, 0, MaxLongInt, DefColors);

  {add shadow for demo purposes}
  M.wFrame.AddShadow(shBR, shSeeThru);

  {make the help mode frame look like the pick mode frame so far}
  M.hwFrame.fCopy(M.wFrame);

  {add index and topic headers}
  M.wFrame.AddHeader(' Topic Index ', heTC);
  M.AddTopicHeader(1, 60, heTC);

  {add previous topic hot spot}
  M.hwFrame.AddHotSpot(frTL, #20, DefColors);
  M.SetPrevTopicHotSpot(hsSpot, frTL);

  {limit the sizeability for demo purposes}
  M.SetSizeLimits(32, 4, ScreenWidth, ScreenHeight);

  {show the mouse cursor}
  ShowMouse;

  M.SetTopic(1);

  {process help window}
  Finished := False;
  repeat
    M.Process;
    case M.GetLastCommand of
      {$IFDEF UsingDrag}
      ccMouseDown : begin
                      if HandleMousePress(M) = ZoomHotCode then
                        {change zoom header on help frames}
                        ToggleHelpZoom(M);
                      if not M.InHelpMode then
                        M.SetTopic(0);
                    end;
      {$ENDIF}
      ccSelect   : M.SetTopic(M.GetTopicChoice);
      ccQuit     : if M.InHelpMode then begin
                     {return to topic index before quitting}
                     M.TopicStackPtr^.Clear;
                     M.SetTopic(0);
                   end else
                     Finished := true;
      ccError    : Finished := true;
    end;
  until Finished;

  {erase and dispose memo window}
  M.Done;

  {make sure mouse is hidden before returning to DOS}
  HideMouse;
end.
