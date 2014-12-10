(*

   DIALOG4
   -------
   The dialog box in this program contains all five types of controls: push
   buttons, radio buttons, check boxes, an edit control, and a window control
   (a pick list). Points worth noting:

     1) If UseDrag is defined in OPDEFINE.INC, or if UseDragAnyway is defined
        below, the OPDRAG unit will be used.

     2) The file dialog box created for this demo was turned into an object
        (FileDialog) and put in its own unit (FDialog) so that it could be
        incorporated easily into other programs.

     3) The file list control in the dialog box is allowed to be empty; this
        is made possible by the new pkProcessZero option for PickLists.

     4) The drive and directory list is sorted using the new SortNameDirDrive
        procedure.
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

program Dialog4;
  {-Dialog box example #4}
uses
  Use32,
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
  OpPick,
  OpDir,
  OpSelect,
  OpDialog,
  FDialog;

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
    ProPromptColor  : $70; ProPromptMono   : $70;
    FieldColor      : $1E; FieldMono       : $07;
    SelFieldColor   : $1F; SelFieldMono    : $0F;
    ProFieldColor   : $70; ProFieldMono    : $70;
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
    AltItemColor    : $3E; AltItemMono     : $0F;
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
    SelClusterColor : $3F; SelClusterMono  : $0F);
  {$IFDEF UseMouse}
  MouseChar      : Char = #04;
  {$ENDIF}
var
  FDB            : FileDialog;
  Cmd, Status    : Word;
  Finished       : Boolean;
  FName          : PathStr;
{$IFDEF UseDragAnyway}
  DragCommands   : DragProcessor;
{$ENDIF}

function InitDialogBox : Word;
  {-Initialize dialog box}
const
  WinOptions = wBordered+wClear+wUserContents;
begin
  with FDB do begin
    {instantiate dialog box}
    if not InitCustom(
      17, 5,           {top left corner (X,Y)}
      Colors,          {main color set}
      WinOptions,      {window options}
      dColors,         {dialog box-specific colors}
      hProtected,      {help button: *protected*, hidden, or visible}
      '*.*'            {file mask}
      ) then begin
        InitDialogBox := InitStatus;
        Exit;
      end;

    with wFrame, Colors do begin
      {$IFDEF UseShadows}
      AddShadow(shBR, shSeeThru);
      {$ENDIF}

      AddHeader(' File Open ', heTC);

      {add hot spot for closing the window}
      AddCustomHeader('[ ]', frTL, +2, 0, HeaderColor, HeaderMono);
      AddCustomHeader('þ',   frTL, +3, 0, $7A,         HeaderMono);
      AddHotRegion(frTL, CloseHotCode, +3, 0, 1, 1); {!!.30}

      {$IFDEF UsingDrag}
      {add hot spot for moving the window}
      AddHotBar(frTT, MoveHotCode);
      {$ENDIF}
    end;

    InitDialogBox := RawError;
  end;
end;

begin
  {$IFDEF UseScrollBars}
  {select alternate scroll bar arrows}
  DefArrows := TriangleArrows;
  {$ENDIF}

  {initialize dialog box}
  Status := InitDialogBox;
  if Status <> 0 then begin
    WriteLn('Error initializing dialog box: ', Status);
    Halt(1);
  end;

{$IFDEF UseDragAnyway}
  {initialize DragProcessor}
  DragCommands.Init(@DialogKeySet, DialogKeyMax);
  FDB.SetCommandProcessor(DragCommands);
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
  TextChar := '±';
  ClrScr;

  Finished := False;
  FName := '';
  repeat
    {process commands}
    FDB.Process;

    Cmd := FDB.GetLastCommand;
    case Cmd of
     {$IFDEF UseMouse}
       {$IFDEF UsingDrag}
        ccMouseDown,
        ccMouseSel :
          {did user click on the hot spot for closing?}
          if HandleMousePress(FDB) = CloseHotCode then begin {!!.30}
            ClearMouseEvents;
            Finished := True;
          end;
       {$ELSE}
        ccMouseSel :
          Finished := True;
       {$ENDIF}
     {$ENDIF}
      ccSelect :
        begin
          FName := FDB.GetFileName;
          Finished := True;
        end;
      ccQuit, ccError :
        Finished := True;
    end;
  until Finished;

  FDB.Done;

  {$IFDEF UseMouse} {!!.22}
  HideMouse;
  {$ENDIF}          {!!.22}
  ClrScr;

  if FName <> '' then
    WriteLn('File selected: ', FName);
end.
