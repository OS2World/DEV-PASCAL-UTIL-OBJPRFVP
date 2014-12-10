(*

   DIALOG3
   -------
   The dialog box in this program contains all five types of controls: push
   buttons, radio buttons, check boxes, an edit control, and a window control
   (a pick list). Points worth noting:

     1) If UseDrag is defined in OPDEFINE.INC, or if UseDragAnyway is defined
        below, the OPDRAG unit will be used.

     2) If TestStream is defined below, the dialog box will be stored in a
        stream after it is instantiated and then read back from the stream
        before being processed.

     3) The special highlighting of the picklist's current item is achieved by
        turning the pkAltCurrent option on and the pkDrawActive option off.

     4) To allow alpha keys such as <C> and <R> to move the cursor out of the
        pick list, 'SetSearchMode(PickAnyCharExit)' is called.

     5) The SetNextField method is used to position the cursor initially
        within the cluster of checkboxes.

     6) The Help pushbutton is protected and therefore is inaccessible.
*)

{$V-}
{$I OPDEFINE.INC}

{$IFDEF UseStreams}
  {.$DEFINE TestStream}
{$ENDIF}

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

program Dialog3;
  {-Dialog box example #3}
uses
  Use32,
  Dos,
  OpInline,
  OpString,
  OpConst,  {!!.20}
  OpRoot,
  OpDos,
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
  OpSelect,
  OpDialog;

type
  UserRecord =
    record
      RadioVar       : Byte;
      CheckBoxArray  : array[1..5] of Boolean;
      FName          : PathStr;
    end;
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

  {control ID's}
  idRadio        = 0;
  idCheckBox     = idRadio+1;
  idEditor       = idCheckBox+1;
  idWindow       = idEditor+1;
  idOK           = idWindow+1;
  idCancel       = idOK+1;
  idHelp         = idCancel+1;

  {radio button cluster ID's}
  rbidGiant = 0;
  rbidLarge = 1;
  rbidBig   = 2;
  rbidSmall = 3;
  rbidTiny  = 4;

  hiRadio        = 1;
  hiCheckBox     = hiRadio+1;
  hiEditor       = hiCheckBox+1;
  hiWindow       = hiEditor+1;
  hiOK           = hiWindow+1;
  hiCancel       = hiOK+1;
  hiHelp         = hiCancel+1;

  {user record}
  UR : UserRecord = (
    RadioVar       : 3;
    CheckBoxArray  : (False, True, False, True, False);
    FName          : '');
var
  DB             : DialogBox;
  PLP            : PickListPtr;
  Status         : Word;
  Finished       : Boolean;
{$IFDEF UseDragAnyway}
  DragCommands   : DragProcessor;
{$ENDIF}
{$IFDEF TestStream}
  S              : BufIdStream;
{$ENDIF}

const
  StateStrings : array[1..51] of string[19] = (
    {01} 'AK Alaska',         {02} 'AL Alabama',          {03} 'AR Arkansas',
    {04} 'AZ Arizona',        {05} 'CA California',       {06} 'CO Colorado',
    {07} 'CT Connecticut',    {08} 'DC Dist of Columbia', {09} 'DE Delaware',
    {10} 'FL Florida',        {11} 'GA Georgia',          {12} 'HI Hawaii',
    {13} 'IA Iowa',           {14} 'ID Idaho',            {15} 'IL Illinois',
    {16} 'IN Indiana',        {17} 'KS Kansas',           {18} 'KY Kentucky',
    {19} 'LA Louisana',       {20} 'MA Massachusetts',    {21} 'MD Maryland',
    {22} 'ME Maine',          {23} 'MI Michigan',         {24} 'MN Minnesota',
    {25} 'MO Missouri',       {26} 'MS Mississippi',      {27} 'MT Montana',
    {28} 'NC North Carolina', {29} 'ND North Dakota',     {30} 'NE Nebraska',
    {31} 'NH New Hampshire',  {32} 'NJ New Jersey',       {33} 'NM New Mexico',
    {34} 'NV Nevada',         {35} 'NY New York',         {36} 'OH Ohio',
    {37} 'OK Oklahoma',       {38} 'OR Oregon',           {39} 'PA Pennsylvania',
    {40} 'RI Rhode Island',   {41} 'SC South Carolina',   {42} 'SD South Dakota',
    {43} 'TN Tennessee',      {44} 'TX Texas',            {45} 'UT Utah',
    {46} 'VA Virginia',       {47} 'VT Vermont',          {48} 'WA Washington',
    {49} 'WI Wisconsin',      {50} 'WV West Virginia',    {51} 'WY Wyoming');

{$F+}

procedure StateChoice(Item : Word; Mode : pkMode;
                     var IType : pkItemType;
                     var IString : string;
                     PickPtr : PickListPtr);
  {-Return a state string given an index}
begin
  if Mode = pkGetType then
    Exit;
  IString := StateStrings[Item];
  if Mode = pkSearch then
    {return only the first two characters -- the abbreviation}
    IString[0] := #2;
end;

{$F-}

function InitPickList : Word;
  {-Initialize pick list}
const
  WinOptions = wClear+wUserContents+wNoCoversBuffer;
begin
  New(PLP, InitCustom( {!!.20}
     {$IFDEF UseScrollBars}
      27, 09, 68, 16, Colors, WinOptions, 21, 51, StateChoice,
     {$ELSE}
      27, 09, 68, 17, Colors, WinOptions, 21, 51, StateChoice,
     {$ENDIF}
      PickSnaking, SingleChoice));
  if PLP = nil then begin {!!.20}
    InitPickList := InitStatus;
    Exit;
  end;

  with PLP^ do begin {!!.20}
    with dColors do begin
      SetTextAttr(ClusterColor, ClusterMono);
      SetPickAttr(pkNormal, False, ClusterColor, ClusterMono);
    end;

    {$IFDEF UseScrollBars}
    with wFrame do
      AdjustFrameCoords(frXL, frYL, frXH, frYH+1);
    wFrame.AddCustomScrollBar(frBB, 0, MaxLongInt, 0, 0, 'þ', '°', Colors);
    {$ENDIF}

    pkOptionsOn(pkAltCurrent);
    pkOptionsOff(pkDrawActive);
    SetPadSize(1, 1);
    SetSearchMode(PickAnyCharExit);

    InitPickList := RawError;
  end;
end;

{$F+}
procedure PostFocus(DBP : DialogBoxPtr);
begin
  with DBP^ do begin
    if GetLastCommand = ccItemChange then
      case GetCurrentID of
        idRadio :
          case GetCurrentItemID of
            rbidGiant,
            rbidLarge,
            rbidBig :
              begin
                ChangeProtection(idEditor, True);
                DrawField(idEditor);
              end;
            rbidSmall,
            rbidTiny :
              begin
                ChangeProtection(idEditor, False);
                DrawField(idEditor);
              end;
          end;
      end;
  end;
end;
{$F-}

function InitDialogBox : Word;
  {-Initialize dialog box}
const
  WinOptions = wBordered+wClear+wUserContents;
begin
  with DB do begin
    {instantiate dialog box}
    if not InitCustom(
      09, 4, 70, 20, Colors, WinOptions, dColors) then begin
        InitDialogBox := InitStatus;
        Exit;
      end;

    {enable user hooks}
    SetPostFocusProc(PostFocus);

    with wFrame, Colors do begin
      {$IFDEF UseShadows}
      AddShadow(shBR, shSeeThru);
      {$ENDIF}

      {add hot spot for closing the window}
      AddCustomHeader('[ ]', frTL, +2, 0, HeaderColor, HeaderMono);
      AddCustomHeader('þ',   frTL, +3, 0, $7A,         HeaderMono);
      AddHotRegion(frTL, CloseHotCode, +3, 0, 1, 1); {!!.30}

      {$IFDEF UsingDrag}
      {add hot spot for moving the window}
      AddHotBar(frTT, MoveHotCode);
      {$ENDIF}
    end;

  {idRadio:}
    dgControlOptionsOn(dcItemChangeExit);
    AddRadioButtons('&Radio buttons', 02, 03, 03, 03, 13, 5, 13, hiRadio, UR.RadioVar);
      AddRadioButton('&Giant', 0);
      AddRadioButton('&Large', 1);
      AddRadioButton('&Big',   2);
      AddRadioButton('&Small', 3);
      AddRadioButton('&Tiny',  4);
    dgControlOptionsOff(dcItemChangeExit);

  {idCheckBox:}
    { dgControlOptionsOn(dcSelectLocally); }
    AddCheckBoxes('&Check boxes',     09, 03, 10, 03, 13, 5, 13, hiCheckBox);
      AddCheckBox('Box &1', UR.CheckBoxArray[1]);
      AddCheckBox('Box &2', UR.CheckBoxArray[2]);
      AddCheckBox('Box &3', UR.CheckBoxArray[3]);
      AddCheckBox('Box &4', UR.CheckBoxArray[4]);
      AddCheckBox('Box &5', UR.CheckBoxArray[5]);

  {idEditor:}
    dgFieldOptionsOn(efClearFirstChar+efAutoAdvance);
    AddSimpleEditControl(
      '&Edit control', 2, 19, 'X', 3, 19, 42, 79, hiEditor, UR.FName);

  {idWindow:}
    dgFieldOptionsOn(efAllowEscape);
    dgSecFieldOptionsOn(sefSwitchCommands);
    AddWindowControl('&Window control', 05, 19, 6, 19, hiWindow, ccNone, PLP^); {!!.20}

  {idOK:}
    AddPushButton('O&K',      16, 10, 8, hiOK,     ccSelect, True);

  {idCancel:}
    AddPushButton('Cancel',   16, 29, 8, hiCancel, ccQuit,   False);

  {idHelp:}
    dgFieldOptionsOn(efProtected);
    AddPushButton('Help',     16, 48, 8, hiHelp,   ccHelp,   False);
    dgFieldOptionsOff(efProtected);

    {start out on check boxes}
    SetNextField(idCheckBox);

    InitDialogBox := RawError;
  end;
end;

{$IFDEF TestStream}

procedure RegisterTypes(var S : IdStream);
  {-Register data types and pointers}
begin
  {register dialog box}
  S.RegisterHier(DialogBoxStream);

  {register control types}
  S.RegisterHier(RadioButtonsStream);
  S.RegisterHier(CheckBoxesStream);
  S.RegisterHier(SimpleEditControlStream);
  S.RegisterHier(WindowControlStream);

  {register user record}
  S.RegisterPointer(1000, @UR);

  {register user-written routines}
  S.RegisterPointer(1001, @StateChoice);
  S.RegisterPointer(1002, @SingleChoice);
  S.RegisterPointer(1003, @PickSnaking);

  {$IFDEF UseDragAnyway}
  {register pointer to drag processor}
  S.RegisterPointer(1004, @DragCommands);
  {$ENDIF}

  {register the pick list}
  S.RegisterHier(PickListStream);
end;

{$ENDIF}

begin
  {$IFDEF UseScrollBars}
  {select alternate scroll bar arrows}
  DefArrows := TriangleArrows;
  {$ENDIF}

  {initialize PickList}
  Status := InitPickList;
  if Status <> 0 then begin
    WriteLn('Error initializing PickList: ', Status);
    Halt(1);
  end;

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

{$IFDEF TestStream}

  {set user record}
  DB.SetUserRecord(UR, SizeOf(UR));

  {create stream file}
  S.Init('DIALOG3.STM', SCreate, 4096);

  {register types and store the dialog box}
  RegisterTypes(S);
  S.Put(DB);
  Status := S.GetStatus;
  if Status <> 0 then begin
    WriteLn('Store error: ', Status);
    Halt(2);
  end;
  S.Done;

  {dispose of the parent *and* its children}
  DB.Done;

  {reopen stream file}
  S.Init('DIALOG3.STM', SOpen, 4096);

  {register types and load the dialog box}
  RegisterTypes(S);
  S.Get(DB);
  Status := S.GetStatus;
  if Status <> 0 then begin
    WriteLn('Load error: ', Status);
    WriteLn('InitStatus: ', InitStatus);
    WriteLn('VMT offset: $', HexW(S.idVmtError));
    Halt(3);
  end;
  S.Done;
{$ENDIF}

  {fill in a backdrop}
  TextAttr := $07;
  TextChar := '±';
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
