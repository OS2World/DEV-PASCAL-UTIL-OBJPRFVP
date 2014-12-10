(*

  PICKDRAG
  --------
  This program is an example of using the OPDRAG unit with a PickList. Points
  worth noting:

     1) If UseDrag is defined in OPDEFINE.INC, or if UseDragAnyway is defined
        below, the OPDRAG unit will be used.

     2) By changing comment braces, you can try several different options:
        single or multiple choice pick lists; the pick list pkSelectOnClick
        option; different mouse cursor shapes.

*)

{$R-,S-}
{$I OPDEFINE.INC}

{$IFDEF UseDrag}
  {$DEFINE UsingDrag}
{$ELSE}
  {$DEFINE UseDragAnyway}         {<--- define this to force use of OPDRAG}
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

program PickDrag;
uses
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
  OpPick;

const
  DefColors : ColorSet = (
    TextColor : $1E;       TextMono : $07;
    CtrlColor : $1E;       CtrlMono : $0F;
    FrameColor : $13;      FrameMono : $07;
    HeaderColor : $3F;     HeaderMono : $70;
    ShadowColor : $08;     ShadowMono : $0F;
    HighlightColor : $4F;  HighlightMono : $70;
    PromptColor : $30;     PromptMono : $07;
    SelPromptColor : $30;  SelPromptMono : $07;
    ProPromptColor : $30;  ProPromptMono : $07;
    FieldColor : $1E;      FieldMono : $07;
    SelFieldColor : $31;   SelFieldMono : $0F;
    ProFieldColor : $17;   ProFieldMono : $07;
    ScrollbarColor : $13;  ScrollbarMono : $07;
    SliderColor : $13;     SliderMono : $0F;
    HotspotColor : $30;    HotspotMono : $70;
    BlockColor : $3E;      BlockMono : $0F;
    MarkerColor : $5F;     MarkerMono : $70;
    DelimColor : $31;      DelimMono : $0F;
    SelDelimColor : $31;   SelDelimMono : $0F;
    ProDelimColor : $31;   ProDelimMono : $0F;
    SelItemColor : $3E;    SelItemMono : $70;
    ProItemColor : $17;    ProItemMono : $07;
    HighItemColor : $1F;   HighItemMono : $0F;
    AltItemColor : $1F;    AltItemMono : $0F;
    AltSelItemColor : $3F; AltSelItemMono : $70;
    FlexAHelpColor : $1F;  FlexAHelpMono : $0F;
    FlexBHelpColor : $1F;  FlexBHelpMono : $0F;
    FlexCHelpColor : $1B;  FlexCHelpMono : $70;
    UnselXrefColor : $1E;  UnselXrefMono : $09;
    SelXrefColor : $5F;    SelXrefMono : $70;
    MouseColor : $4F;      MouseMono : $70
    );

var
  {$IFDEF UseDragAnyway}
  PickCommands : DragProcessor;
  {$ENDIF}
  {$IFDEF UsingDrag}
  ZoomHeaderNum : Byte;
  HotCode : Byte;
  {$ENDIF}
  M : PickList;
  MAttr : Word;

{$F+}
  procedure TheStringProc(Item : Word; Mode : pkMode; var IType : pkItemType;
                          var IString : String; PickPtr : PickListPtr);
  begin
    IString := Long2Str(Item);
  end;

begin
  {this demo requires mouse support}
  if not MouseInstalled then begin
    WriteLn('boring without a mouse');
    Halt;
  end;

  {initialize the screen}
  TextChar := #176;
  ClrScr;

  MAttr := Word(ColorMono(DefColors.MouseColor, DefColors.MouseMono)) shl 8;

{$IFDEF UsingDrag}
  {$IFDEF UseDragAnyway}
  {Initialize the new picklist dragprocessor}
  if not PickCommands.Init(@PickKeySet, PickKeyMax) then
    Halt;
  PickCommands.SetMouseCursor(MAttr or $04, MAttr or $12, MAttr or $1D);
  {$ENDIF}
(*
  {activate this for a see-through mouse cursor}
  PickCommands.SetScreenMask($00FF);
  PickCommands.SetMouseCursor(MAttr, MAttr, MAttr);
*)
{$ELSE}
  {Enable the mouse}
  PickCommands.cpOptionsOn(cpEnableMouse);
  SoftMouseCursor($0000, MAttr or $04);
{$ENDIF}


  {create a picklist}
  {single choice picklist}
  if not M.InitCustom(10, 10, 50, 20, DefColors,
                      DefWindowOptions or wBordered,
                      10, 100, TheStringProc,
                      PickVertical, SingleChoice) then Halt;
(*
  {multiple choice picklist}
  if not M.InitCustom(10, 10, 50, 20, DefColors,
                      DefWindowOptions or wBordered,
                      10, 100, TheStringProc,
                      PickVertical, MultipleChoice) then Halt;
*)
  M.SetSelectMarker(#16, '');

{$IFDEF UseDragAnyway}
  {attach the DragProcessor to the PickList}
  M.SetCommandProcessor(PickCommands);
{$ENDIF}

(*
  {activate this for immediate selection as soon as mouse button released}
  M.pkOptionsOn(pkSelectOnClick);
*)

{$IFDEF UsingDrag}
  {add hot spot for zooming}
  M.wFrame.AddCustomHeader(#24, frtr, -1, 0,
                           DefColors.HeaderColor, DefColors.HeaderMono);
  M.wFrame.AddHotRegion(frtr, ZoomHotCode, -1, 0, 1, 1);
  ZoomHeaderNum := M.wFrame.GetLastHeaderIndex;

  {add hot spot for moving}
  M.wFrame.AddHeader(' top bar to drag ', heTC);
  M.wFrame.AddHotBar(frTT, MoveHotCode);

  {add hot spot for resizing}
  M.wFrame.AddHeader(' lower right corner to resize ', heBC);
  M.wFrame.AddCustomHeader(#240, frBR, 0, 0,
                           DefColors.FrameColor, DefColors.FrameMono);
  M.wFrame.AddHotRegion(frBR, ResizeHotCode, 0, 0, 1, 1);

{$ELSE}
  {scrolling by line is too slow without dragging}
  M.pkOptionsOn(pkMousePage);
{$ENDIF}

  {add scroll bar for demo purposes}
  M.wFrame.AddScrollBar(frRR, 0, MaxLongInt, DefColors);

  {scrolling by page is too fast with a live scroll bar}
  M.pkOptionsOff(pkMousePage);

  {add shadow for demo purposes}
  M.wFrame.AddShadow(shBR, shSeeThru);

  {limit the sizeability for demo purposes}
  M.SetSizeLimits(32, 4, ScreenWidth, ScreenHeight);

  {show the mouse cursor}
  ShowMouse;

  {process picklist}
  repeat
    M.Process;
    case M.GetLastCommand of
      {$IFDEF UsingDrag}
      ccMouseDown : if HandleMousePress(M) = ZoomHotCode then
                      if M.IsZoomed then
                        M.ChangeHeader(ZoomHeaderNum, #18)
                      else
                        M.ChangeHeader(ZoomHeaderNum, #24);
      {$ENDIF}
      ccSelect : FastWrite(Pad(Long2Str(M.GetLastChoice), 4),
                           ScreenHeight, 1, $70);
    end;
  until M.GetLastCommand in [ccQuit, ccError];

  {erase and dispose memo window}
  M.Done;

  {make sure mouse is hidden before returning to DOS}
  HideMouse;
end.
