(*

   DIALOG2
   -------
   The simple dialog box in this program contains two push button controls and
   one cluster of radio buttons. Points worth noting:

     1) If UseDrag is defined in OPDEFINE.INC, or if UseDragAnyway is defined
        below, the OPDRAG unit will be used.

     2) The orientation of the radio buttons control is horizontal rather than
        vertical. This is done by adding the individual radio buttons with
        AddRadioButtonAt rather than AddRadioButton. Notice too that <Up> and
        <Down> don't change the selection when the orientation is horizontal,
        but <Left> and <Right> do.
*)

{$V-}
{$I OPDEFINE.INC}

{$IFDEF UseDrag}
  {$DEFINE UsingDrag}
{$ELSE}
  {$DEFINE UseDragAnyway} {<--- define this to force use of OPDRAG}
  {$IFDEF UseDragAnyway}
    {$DEFINE UsingDrag}
  {$ENDIF}
{$ENDIF}

{$IFNDEF UseHotSpots}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

program Dialog2;
  {-Dialog box example #2}
uses
  Dos,
  OpInline,
  OpString,
  OpDos,
  OpConst,  {!!.20}
  OpRoot,
  OpCrt,
{$IFDEF UseMouse}
  OpMouse,
{$ENDIF}
  OpAbsFld,
  OpCmd,
  OpField,
  OpCtrl,
  OpFrame,
  OpWindow,
{$IFDEF UsingDrag}
  OpDrag,
{$ENDIF}
  OpSelect,
  OpDialog;

type
  PrinterType = (LPT1, LPT2, LPT3, Prn);
const
  Colors : ColorSet = (
    TextColor       : $70; TextMono        : $70;
    CtrlColor       : $3A; CtrlMono        : $08;
    FrameColor      : $7F; FrameMono       : $70;
    HeaderColor     : $7F; HeaderMono      : $70;
    ShadowColor     : $08; ShadowMono      : $00;
    HighlightColor  : $4F; HighlightMono   : $70;
    PromptColor     : $70; PromptMono      : $70;
    SelPromptColor  : $7F; SelPromptMono   : $70;
    ProPromptColor  : $70; ProPromptMono   : $07;
    FieldColor      : $1E; FieldMono       : $07;
    SelFieldColor   : $1F; SelFieldMono    : $0F;
    ProFieldColor   : $70; ProFieldMono    : $07;
    ScrollBarColor  : $17; ScrollBarMono   : $07;
    SliderColor     : $17; SliderMono      : $0F;
    HotSpotColor    : $17; HotSpotMono     : $0F;
    BlockColor      : $1E; BlockMono       : $0F;
    MarkerColor     : $1F; MarkerMono      : $70;
    DelimColor      : $7E; DelimMono       : $0F;
    SelDelimColor   : $11; SelDelimMono    : $0F;
    ProDelimColor   : $7E; ProDelimMono    : $0F;
    SelItemColor    : $2F; SelItemMono     : $70;
    ProItemColor    : $77; ProItemMono     : $07;
    HighItemColor   : $7F; HighItemMono    : $0F;
    AltItemColor    : $3F; AltItemMono     : $0F;
    AltSelItemColor : $2F; AltSelItemMono  : $70;
    FlexAHelpColor  : $7F; FlexAHelpMono   : $0F;
    FlexBHelpColor  : $7F; FlexBHelpMono   : $0F;
    FlexCHelpColor  : $7B; FlexCHelpMono   : $70;
    UnselXrefColor  : $7E; UnselXrefMono   : $09;
    SelXrefColor    : $9F; SelXrefMono     : $70;
    MouseColor      : $4F; MouseMono       : $70);
  dColors : DialogColorSet = (
    HiPromptColor   : $7E; HiPromptMono    : $0F;
    ButtonColor     : $20; ButtonMono      : $07;
    DefButtonColor  : $2B; DefButtonMono   : $07;
    HiButtonColor   : $2E; HiButtonMono    : $0F;
    SelButtonColor  : $2F; SelButtonMono   : $0F;
    ProButtonColor  : $70; ProButtonMono   : $70;
    BtnShadowColor  : $70; BtnShadowMono   : $70;
    ClusterColor    : $30; ClusterMono     : $07;
    ProClusterColor : $70; ProClusterMono  : $07;
    HiClusterColor  : $3E; HiClusterMono   : $0F;
    SelClusterColor : $3F; SelClusterMono  : $07);
  {$IFDEF UseMouse}
  MouseChar      : Char = #04;
  {$ENDIF}

  PrnVar         : PrinterType = LPT1;
var
  DB             : DialogBox;
  Status         : Word;
  Finished       : Boolean;
{$IFDEF UseDragAnyway}
  DragCommands   : DragProcessor;
{$ENDIF}

function InitDialogBox : Word;
  {-Initialize dialog box}
const
  WinOptions = wBordered+wClear+wUserContents;
begin
  with DB do begin
    {instantiate dialog box}
    if not InitCustom(
      19, 10, 61, 14, Colors, WinOptions, dColors) then begin
        InitDialogBox := InitStatus;
        Exit;
      end;

    with wFrame, Colors do begin
      {$IFDEF UseShadows}
      AddShadow(shBR, shSeeThru);
      {$ENDIF}

      AddHeader(' Select Printer ', heTC);

      {add hot spot for closing the window}
      AddCustomHeader('[ ]', frTL, +2, 0, FrameColor,  FrameMono);
      AddCustomHeader('�',   frTL, +3, 0, $7A,         HeaderMono);
      AddHotRegion(frTL, CloseHotCode, +3, 0, 1, 1);  {!!.30}

      {$IFDEF UsingDrag}
      {add hot spot for moving the window}
      AddHotBar(frTT, MoveHotCode);
      {$ENDIF}
    end;

  {idPrn:}
    AddRadioButtons('', 02, 03, 02, 03, 39, 1, 0, 0, PrnVar);
      AddRadioButtonAt('LPT&1 ', 01, 01, Ord(LPT1) );
      AddRadioButtonAt('LPT&2 ', 01, 11, Ord(LPT2) );
      AddRadioButtonAt('LPT&3 ', 01, 21, Ord(LPT3) );
      AddRadioButtonAt('&Prn ',  01, 31, Ord(Prn) );

  {idOK:}
    AddPushButton('O&K',    04, 11, 8, 0,  ccSelect, True);

  {idCancel:}
    AddPushButton('Cancel', 04, 23, 8, 0,  ccQuit,   False);

    InitDialogBox := RawError;
  end;
end;

begin
  {initialize DialogBox}
  Status := InitDialogBox;
  if Status <> 0 then begin
    WriteLn('Error initializing DialogBox: ', Status);
    Halt(1);
  end;

{$IFDEF UseDragAnyway}
  {initialize DragProcessor}
  DragCommands.Init(@DialogKeySet, DialogKeyMax);
  DB.SetCommandProcessor(DragCommands);
{$ELSE}
  {$IFNDEF UsingDrag}
    {$IFDEF UseMouse}
    if MouseInstalled then
      with Colors do begin
        {activate mouse cursor}
        SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+
                                Byte(MouseChar));
        ShowMouse;

        {enable mouse support}
        DialogCommands.cpOptionsOn(cpEnableMouse);
      end;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  {fill in a backdrop}
  TextAttr := $07;
  TextChar := '�';
  ClrScr;

  Finished := False;

  repeat
    {process commands}
    DB.Process;
    case DB.GetLastCommand of
     {$IFDEF UseMouse}
       {$IFDEF UsingDrag}
        ccMouseDown,
        ccMouseSel :
          {did user click on the hot spot for closing?}
          if HandleMousePress(DB) = CloseHotCode then begin  {!!.30}
            ClearMouseEvents;
            Finished := True;
          end;
       {$ELSE}
        ccMouseSel :
          Finished := True;
       {$ENDIF}
     {$ENDIF}
      ccQuit, ccSelect, ccError :
        Finished := True;
    end;
  until Finished;

  DB.Done;

  {$IFDEF UseMouse} {!!.22}
  HideMouse;
  {$ENDIF}          {!!.22}
  ClrScr;
end.
