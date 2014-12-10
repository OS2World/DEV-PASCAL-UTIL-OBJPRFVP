{$R-,S-,I-,V-,B-,F+,O+}

{$I OPDEFINE.INC}

{***************************************************************************
 This unit requires that OPDEFINE.INC activate the following defines:
   UseScrollBars, UseHotSpots, UseShadows, UseAdjustableWindows, UseStreams
 This program will use features activated with the following defines:
   UseMouse
 ***************************************************************************}
{$IFNDEF UseScrollBars}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}
{$IFNDEF UseHotSpots}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}
{$IFNDEF UseShadows}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}
{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}
{$IFNDEF UseStreams}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}


{*********************************************************}
{*                   MMMAIN.PAS 1.30                     *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit MMMain;
  {-Main routine for MAKEMENU, organized to reduce compilation RAM needs}

interface

{!!.20 Library renamed to OpLibrary throughout}

uses
  Use32,
  dos,                            {DOS/BIOS stuff}
  opinline,                       {inline macros}
  opconst,                        {!!.20}
  oproot,                         {base objects, error codes, etc.}
  opcmd,                          {command processing}
  opcrt,                          {low-level screen handling}
  {$IFDEF UseMouse}
  opmouse,                        {mouse handling}
  {$ENDIF}
  opstring,                       {string handling}
  opdos,                          {misc. DOS/BIOS routines}
  opframe,                        {frames, shadows, etc.}
  opwindow,                       {window management}
  opsedit,                        {simple line editor}
  oppick,                         {pick lists}
  opdir,                          {directory lists}
  opmenu,                         {menus}
  makemisc,                       {miscellaneous routines, objects}
  mmutil;                         {miscellaneous utility routines}

  procedure Main;
    {-Main program block}

  {===========================================================}

implementation

{.F-} {This directive controls a source code formatter, not the compiler}
type
  BackdropType = (bdPlain, bdCanvas, bdGrid);

const
  DefLibExt = 'OPL';              {default file extension for libraries}
  DefSrcExt = 'PAS';              {default file extension for source code}
  DefDocExt = 'DOC';              {default file extension for object document}
  DefLibEntries = 15;             {default size of library's directory}

  {backdrop on which menus are designed}
  DefBackdrop : BackdropType = bdCanvas;

  {defaults for new menus}
  DefXL         = 2;
  DefYL         = 2;
  DefXH         = 4;
  DefYH         = 2;
  DefOptions    : LongInt = wBordered+wClear+wUserContents+wCoversOnDemand; {!!.01}
  DefFrame      : FrameArray = 'ÕÔ¸¾ÍÍ³³';
  DefDelay      : Word = 15;
  DefExplosions : Boolean = False;
  DefSound      : Boolean = False;
  DefShadow     : ShadowDrawType = shNone;
  DefOrient     : mnOrientation = Horizontal;
  DefMOptions   : Word = mnAlphaMatch+mnSelectOnMatch+
                         mnArrowSelect+mnAllHotSpots+mnMouseSupport;

  DefLeftMark   : String[5] = '';
  DefRightMark  : String[5] = '';

  DefHelpActive : Boolean = False;
  DefHelpRow    : Byte = 24;
  DefHelpLineColor : Byte = $1E;
  DefHelpLineMono  : Byte = $07;

  DefUserProcs  : Boolean = True;      {Generate user proc shells in source?}
  DefCaseState  : Boolean = True;      {Generate case statement shell in source?}
  DefTypeConst  : Boolean = False;     {Generate typed constants for items/help?}

  {default colors for new menus}
  DefColors  : ColorSet = (
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
    ScrollBarColor  : $13; ScrollBarMono  : $07;
    SliderColor     : $13; SliderMono     : $0F;
    HotSpotColor    : $30; HotSpotMono    : $70;
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
    FlexBHelpColor  : $1F; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono  : $70;
    UnselXrefColor  : $1E; UnselXrefMono  : $09;
    SelXrefColor    : $5F; SelXrefMono    : $70;
    MouseColor      : $4F; MouseMono      : $70
  );

  {$IFDEF UseMouse}
  MouseChar  : Char = #04;
  {$ENDIF}

  HaveLib    : Boolean = False; {do we have a library}
  HaveObj    : Boolean = False; {do we have a user menu}
  Modified   : Boolean = False; {has the menu been modified}
  LibName    : PathStr = '';    {name of current library}

  FirstCmd   : Byte = ccSelect; {first commands to execute}

  MaxKeysInMenu = 256*8;      {affects bitset used to automatically assign keys}

  HorizTabSize = 10;          {used by move and resize window commands}
  VertTabSize  = 5;

  {Convenient synonyms}
  MainMenuCmd     = ccUser0;
  MoveWindowCmd   = ccUser1;
  ResizeWindowCmd = ccUser2;
  ExitCmd         = ccUser3;
  SaveFileCmd     = ccUser4;
  NewFileCmd      = ccUser5;
  InfoCmd         = ccUser7;

  PathLen = SizeOf(PathStr)-1;

var
  UserMenu   : Menu;         {menu being designed}
  Lib        : OpLibrary;    {current library}
  ObjName    : DirEntryName; {name of current object in library's directory}
  MainMenu   : Menu;         {main menu}
  Item       : Word;         {last main menu item selected}
  Cmd        : Word;         {last main menu command}
  AllDone    : Boolean;      {True when ready to quit}
  Prompt     : StackWindow;  {hidden prompt line}
  HelpLine   : RawWindow;    {optional user help line}
  SaveDefColors : ColorSet;  {copy of default color set} {!!.03}

const
  {Keystroke to command mapping}
  HotKeyMax = 50;
  HotKeySet : array[0..HotKeyMax] of Byte = (
  {length keys         command type      key sequence}
  3,      $00, $13,    ResizeWindowCmd,  {AltR}
  3,      $00, $17,    InfoCmd,          {AltI}
  3,      $00, $2D,    ExitCmd,          {AltX}
  3,      $00, $32,    MoveWindowCmd,    {AltM}
  3,      $00, $3C,    SaveFileCmd,      {F2}
  3,      $00, $3D,    NewFileCmd,       {F3}
  3,      $00, $44,    MainMenuCmd,      {F10}
                       0, 0, 0,          {30}
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,          {40}
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0           {50}
  );

const
  {main menu item codes}
  mmDefaults              = 01;
    mmBackdrop            = 02;
    mmColorsDef           = 03;
      mmTextDef           = 04;
      mmFrameDef          = 05;
      mmHeaderDef         = 06;
      mmShadowDef         = 07;
      mmHiliteDef         = 08;
      mmSelectedDef       = 09;
      mmProtectedDef      = 10;
      mmMouseDef          = 11;
      mmHelpLineDef       = 12;
      mmLoadColors        = 13;  {!!.03}
      mmSaveColors        = 14;  {!!.03}
    mmEffectsDef          = 15;
      mmDelayDef          = 16;
      mmExplosionsDef     = 17;
      mmShadowsDef        = 18;
      mmSoundDef          = 19;
      mmLeftMarkDef       = 20;
      mmRightMarkDef      = 21;
    mmFrameCharDef        = 22;
    mmHelpDef             = 23;
      mmHelpActive        = 24;
      mmHelpRow           = 25;
    mmOrientDef           = 26;
    mmSourceDef           = 27;
      mmColorConstants    = 28;
      mmUserProcs         = 29;
      mmCaseState         = 30;
      mmTypeConst         = 31;
    mmTogglesDef          = 32;
      mmAltMatch          = 33;     {!!.03} {And renumbered following}
      mmArrowPull         = 34;
      mmCharMatch         = 35;
      mmCharSelect        = 36;
      mmSelectErase       = 37;
      mmItemTopic         = 38;
      mmMouseSupport      = 39;
      mmRememberPull      = 40;
      mmSelectOnClick     = 41;

  mmSubmenu               = 42;
    mmAddSub              = 43;
    mmDragSub             = 44;
    mmMoveSub             = 45;
    mmResizeSub           = 46;
    mmSettingsSub         = 47;
      mmColorsSub         = 48;
        mmTextSub         = 49;
        mmFrameSub        = 50;
        mmHeaderSub       = 51;
        mmShadowSub       = 52;
        mmHiliteSub       = 53;
        mmSelectedSub     = 54;
        mmProtectedSub    = 55;
      mmEffectsSub        = 56;
        mmDelaySub        = 57;
        mmExplosionsSub   = 58;
        mmShadowsSub      = 59;
        mmSoundSub        = 60;
        mmPadSub          = 61;
        mmLeftMarkSub     = 62;
        mmRightMarkSub    = 63;
      mmFrameCharSub      = 64;
      mmHeadersSub        = 65;
        mmHeaderAdd       = 66;
        mmHeaderEdit      = 67;
        mmHeaderKill      = 68;
      mmOrientSub         = 69;
      mmSpanSub           = 70;
        mmSpanAdd         = 71;
        mmSpanEdit        = 72;
        mmSpanKill        = 73;
    mmKillSub             = 74;

  mmItem                  = 75;
    mmAddItem             = 76;
    mmNameItem            = 77;
    mmEditItem            = 78;
    mmKeyItem             = 79;
    mmSelectChar          = 80;
    mmHelpText            = 81;
    mmMoveItem            = 82;
    mmReorderItem         = 83;
    mmDragItem            = 84;
    mmKillItem            = 85;

  mmObject                = 86;
    mmObjLoad             = 87;
    mmSave                = 88;
    mmRename              = 89;
    mmSaveAs              = 90;
    mmObjInfo             = 91;
    mmTest                = 92;
    mmGenerate            = 93;
    mmDocument            = 94;
    mmRekey               = 95;

  mmLibrary               = 96;
    mmLoad                = 97;
    mmNew                 = 98;
    mmDelete              = 99;
    mmInfo                = 100;
    mmPack                = 101;
    mmQuit                = 102;
{.F+}

  {Provide fast access to each menu item for dynamic protection of items}
const
  FirstMenuItem   = mmDefaults;
  LastMenuItem    = mmQuit;
type
  mmPointerArray  = array[FirstMenuItem..LastMenuItem] of MenuItemNodePtr;
var
  mmItems         : mmPointerArray;

type
  MenuInfoWindow =
    object(StackWindow)
      miBrightColor : Byte;
      miBrightMono  : Byte;
      miDimColor    : Byte;
      miDimMono     : Byte;

      constructor Init(X1, Y1 : Byte);
        {-Initialize the menu info window using default colors and options}
      constructor InitCustom(X1, Y1 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt);
        {-Initialize the menu info window with custom options, colors}
      procedure UpdateContents; virtual;
        {-Redraw the entry screen info window}
      procedure Process; virtual;
        {-Display the menu info window and wait for a keypress}
    end;

  {$I MMMAIN.IN1}    {Initialization, library and object I/O}
  {$I MMMAIN.IN2}    {Many menu making commands}
  {$I MMMAIN.IN3}    {Generate source and documentation files}
  {$I MMMAIN.IN4}    {Many more menu making commands}

  procedure ProtectMenuItems;
    {-Protect menu items as necessary}
  const
    SaveHaveLib    : Boolean = True;
    SaveHaveObj    : Boolean = True;
    SaveHaveItems  : Boolean = True;
    SaveModified   : Boolean = True;
    SaveHasFrame   : Boolean = True;
    SaveHasHeaders : Boolean = True;
    SaveHasSpans   : Boolean = True;
    SaveIsSubmenu  : Boolean = True;
  var
    HaveItems : Boolean;
    IsSubMenu : Boolean;
    Changed : Boolean;

    procedure SetProtection(ItemCode : LongInt; Enable : Boolean);
      {-Protect disabled items}
    begin
      with mmItems[ItemCode]^ do
        if Enable then
          Unprotect
        else
          Protect;
    end;

  begin
    if not HaveLib then
      HaveObj := False;
    if not HaveObj then begin
      Modified := False;
      IsSubmenu := False;
      HaveItems := False;
    end else begin
      IsSubMenu := (UserMenu.ActiveSubPtr <> nil);
      HaveItems := (UserMenu.ActiveItemPtr <> nil);
    end;

    Changed := False;

    if HaveObj then begin
      with UserMenu.ActiveWinPtr^ do begin
        if SaveHasFrame <> HasFrame then begin
          SetProtection(mmHeadersSub,  HasFrame);
          Changed := True;
          SaveHasFrame := HasFrame;
        end;

        if SaveHasHeaders <> HasHeaders then begin
          SetProtection(mmHeaderEdit,  HasHeaders);
          SetProtection(mmHeaderKill,  HasHeaders);
          Changed := True;
          SaveHasHeaders := HasHeaders;
        end;

        if SaveHasSpans <> HasSpans then begin
          SetProtection(mmSpanEdit,    HasSpans);
          SetProtection(mmSpanKill,    HasSpans);
          Changed := True;
          SaveHasSpans := HasSpans;
        end;
      end;

      if SaveIsSubmenu <> IsSubMenu then begin
        SetProtection(mmKillSub,       IsSubMenu);
        Changed := True;
        SaveIsSubmenu := IsSubmenu;
      end;
    end;

    if HaveLib <> SaveHaveLib then begin
      SetProtection(mmLoadColors,    HaveLib); {!!.03}
      SetProtection(mmSaveColors,    HaveLib); {!!.03}
      SetProtection(mmObject,        HaveLib);
      SetProtection(mmDelete,        HaveLib);
      SetProtection(mmInfo,          HaveLib);
      SetProtection(mmPack,          HaveLib);
      Changed := True;
      SaveHaveLib := HaveLib;
    end;

    if HaveObj <> SaveHaveObj then begin
      SetProtection(mmSubmenu,       HaveObj);
      SetProtection(mmItem,          HaveObj);
      SetProtection(mmRename,        HaveObj);
      SetProtection(mmSaveAs,        HaveObj);
      SetProtection(mmObjInfo,       HaveObj);
      SetProtection(mmTest,          HaveObj);
      Changed := True;
      SaveHaveObj := HaveObj;
    end;

    if HaveItems <> SaveHaveItems then begin
      SetProtection(mmAddSub,        HaveItems);
      SetProtection(mmNameItem,      HaveItems);
      SetProtection(mmEditItem,      HaveItems);
      SetProtection(mmKeyItem,       HaveItems);
      SetProtection(mmSelectChar,    HaveItems);
      SetProtection(mmHelpText,      HaveItems);
      SetProtection(mmMoveItem,      HaveItems);
      SetProtection(mmReorderItem,   HaveItems);
      SetProtection(mmDragItem,      HaveItems);
      SetProtection(mmKillItem,      HaveItems);
      SetProtection(mmGenerate,      HaveItems);
      SetProtection(mmDocument,      HaveItems);
      SetProtection(mmRekey,         HaveItems);
      Changed := True;
      SaveHaveItems := HaveItems;
    end;

    if Modified <> SaveModified then begin
      SetProtection(mmSave,          Modified);
      Changed := True;
      SaveModified := Modified;
    end;

    {redraw the entire menu if it is the current window and protection changed}
    if Changed and MainMenu.IsCurrent then
      MainMenu.Redraw;
  end;

  function GetColorSet(var CS : ColorSet) : Boolean; {!!.03}
    {-Load a color set into CS}
  const
    TypeCodes = [otLoadableColorSet];
  var
    CSS : ColorSetSelector;
    LCS : LoadableColorSet;
    I : Word;
  begin
    GetColorSet := False;
    if not HaveLib then
      Exit;

    {initialize the color set selector}
    if not CSS.Init(30, 6, 51, 21, Lib, TypeCodes, True) then begin
      InsufficientMemory;
      Exit;
    end;

    {add a shadow}
    CSS.wFrame.AddShadow(shBR, shSeeThru);

    {change the attribute of the column labels}
    with MainColors do
      CSS.SetLabelAttr(HighItemColor, HighItemMono);

    {make a selection}
    CSS.Process;
    case CSS.GetLastCommand of
      ccQuit, ccError :
        begin
          CSS.Erase;
          CSS.Done;
          Exit;
        end;
    end;

    {is it an existing object or a new one?}
    I := CSS.GetLastObjectChoice;
    if I = 0 then begin
      CS := SaveDefColors;
      GetColorSet := True;
    end
    else with Lib do begin
      {load the color set}
      GetEntry(FindEntryByIndex(I)^.GetEntryName, LCS);
      I := GetStatus;
      if I = 0 then begin
        CS := LCS.lcsColors;
        GetColorSet := True;
      end
      else
        PopupErrorMessage('Internal error '+Long2Str(I)+' loading color set');
    end;

    CSS.Erase;
    CSS.Done;
  end;

  procedure LoadColorSet; {!!.03}
    {-Load a color set from a library}
  var
    Esc : Boolean;
  begin
    if GetColorSet(DefColors) then
      with UserMenu, DefColors do begin
        DefHelpLineColor := TextColor;
        DefHelpLineMono := TextMono;
        if HaveObj then
          if PopupYesNo('', 'Apply colors globally?', YesChar, Esc) and not Esc then begin
            SetAllNormAttr(TextColor, TextMono);
            SetAllFrameAttr(FrameColor, FrameMono);
            SetAllHeaderAttr(HeaderColor, HeaderMono);
            SetAllShadowAttr(ShadowColor, ShadowMono);
            SetAllHighAttr(HighItemColor, HighItemMono);
            SetAllSelectAttr(SelItemColor, SelItemMono);
            SetAllProtectAttr(ProItemColor, ProItemMono);
            SetHelpAttr(TextColor, TextMono);
            RedrawUserObj(False);
            Modified := True;
          end;
      end;
  end;

  procedure SaveColorSet; {!!.03}
    {-Save a color set in a library}
  const
    TypeCodes = [otLoadableColorSet];
    Prompt1 = 'Library''s directory is full. Proceed?';
    Prompt2 = 'Name of color set: ';
    MaxLen = SizeOf(DirEntryName)-1;
  var
    LS : OpLibrarySelector;
    LCS : LoadableColorSet;
    CSName : DirEntryName;
    AllowNew, OK, Esc, Proceed : Boolean;
    I : Word;
  begin
    if not HaveLib then
      Exit;

    {any room left in library's directory for new objects?}
    AllowNew := Lib.AvailableEntries > 0;
    if not AllowNew then begin
      Proceed := PopupYesNo('Warning', Prompt1, YesChar, Esc);
      if Esc or not Proceed then
        Exit;
    end;

    {initialize the color set selector}
    if not LS.Init(30, 6, 51, 21, Lib, TypeCodes, AllowNew) then begin
      InsufficientMemory;
      Exit;
    end;

    {add a shadow}
    LS.wFrame.AddShadow(shBR, shSeeThru);

    {change the attribute of the column labels}
    with MainColors do
      LS.SetLabelAttr(HighItemColor, HighItemMono);

    {make a selection}
    LS.Process;
    case LS.GetLastCommand of
      ccQuit, ccError :
        begin
          LS.Erase;
          LS.Done;
          Exit;
        end;
    end;

    {initialize loadable color set}
    LCS.Init(DefColors);

    {is it an existing object or a new one?}
    I := LS.GetLastObjectChoice;
    if I = 0 then begin
      {prompt for a name}
      CSName := '';
      repeat
        if (not PopupGetString('New object', Prompt2, True, True, MaxLen, CSName)) or
           (CSName = '') then begin
             LS.Erase;
             LS.Done;
             Exit;
           end;

        {check for conflict with existing name}
        OK := Lib.FindAnyDirectoryIndex(CSName) = 0;
        if not OK then
          PopupErrorMessage('Name is already in use');
      until OK;
    end
    else
      CSName := Lib.FindEntryByIndex(I)^.GetEntryName;

    with Lib do begin
      {save the color set}
      PutEntry(CSName, LCS);
      I := GetStatus;
      if I <> 0 then
        PopupErrorMessage('Internal error '+Long2Str(I)+' storing color set')
      else
        if FlushDosBuffers(Handle) then {};
    end;

    LS.Erase;
    LS.Done;
  end;

  procedure ProcessMenuItem(Item : Word);
    {-Process commands from MAKEMENU menu}
  begin
    with UserMenu do
      case Item of
        {mmDefaults}
          mmBackdrop :
            ToggleBackdrop;
          {mmColorsDef}
            mmTextDef..mmHelpLineDef :
              ChangeDefColors(Item);
            mmLoadColors :             {!!.03}
              LoadColorSet;            {!!.03}
            mmSaveColors :             {!!.03}
              SaveColorSet;            {!!.03}
          {mmEffectsDef}
            mmDelayDef :
              ChangeDefDelay;
            mmExplosionsDef :
              ChangeDefExplosions;
            mmShadowsDef :
              ToggleDefShadows;
            mmSoundDef :
              ToggleDefSound;
            mmLeftMarkDef :
              EditDefMark(False);
            mmRightMarkDef :
              EditDefMark(True);
          mmFrameCharDef :
            EditDefFrame;
          {mmHelpDef}
            mmHelpActive :
              ToggleDefHelp;
            mmHelpRow :
              ChangeDefHelpRow;
          mmOrientDef :
            ToggleDefOrient;
          {mmSourceDef}
            mmColorConstants :
              GenColorNames := not GenColorNames;
            mmUserProcs :
              DefUserProcs := not DefUserProcs;
            mmCaseState :
              DefCaseState := not DefCaseState;
            mmTypeConst :
              DefTypeConst := not DefTypeConst;
          {mmTogglesDef}
            mmAltMatch :                       {!!.03}
              ChangeUserFlags(mnAltMatch);     {!!.03}
            mmArrowPull :
              ChangeUserFlags(mnArrowSelect);
            mmCharMatch :
              ChangeUserFlags(mnAlphaMatch);
            mmCharSelect :
              ChangeUserFlags(mnSelectOnMatch);
            mmSelectErase :
              ChangeUserFlags(mnPopOnSelect);
            mmItemTopic :
              ChangeUserFlags(mnUseItemForTopic);
            mmMouseSupport :
              ChangeUserFlags(mnMouseSupport);
            mmRememberPull :
              ChangeUserFlags(mnAllowPending);
            mmSelectOnClick :
              ChangeUserFlags(mnSelectOnClick);

        {mmSubmenu}
          mmAddSub :
            AddASubMenu;
          mmDragSub :
            DragSubMenu;
          mmMoveSub :
            MoveSubMenu;
          mmResizeSub :
            ResizeSubMenu;
          {mmSettingsSub}
            {mmColorsSub}
              mmTextSub..mmProtectedSub :
                ChangeSubColors(Item);
            {mmEffectsSub}
              mmDelaySub :
                ChangeSubDelay;
              mmExplosionsSub :
                ChangeSubExplosions;
              mmShadowsSub :
                ToggleSubShadows;
              mmSoundSub :
                ToggleSubSound;
              mmPadSub  :
                SetSubPadding;
              mmLeftMarkSub :
                EditMark(False);
              mmRightMarkSub :
                EditMark(True);
            mmFrameCharSub :
              EditSubFrame;
            {mmHeadersSub}
              mmHeaderAdd :
                AddSubHeader;
              mmHeaderEdit :
                EditSubHeader;
              mmHeaderKill :
                RemoveSubHeader;
            mmOrientSub :
              ToggleSubOrient;
            {mmSpanSub}
              mmSpanAdd :
                AddSubSpan;
              mmSpanEdit :
                EditSubSpan;
              mmSpanKill :
                RemoveSubSpan;
          mmKillSub :
            KillSubMenu;

        {mmItem}
          mmAddItem :
            AddMenuItem;
          mmNameItem :
            NameMenuItem;
          mmEditItem :
            EditMenuItem;
          mmMoveItem :
            MoveMenuItem;
          mmReorderItem :
            ReorderMenuItem;
          mmDragItem :
            DragMenuItem;
          mmKeyItem :
            SetKeyMenuItem;
          mmSelectChar :
            SetSelectChar;
          mmHelpText :
            EditHelpText;
          mmKillItem :
            RemoveMenuItem;

        {mmObject}
          mmObjLoad :
            LoadUserMenu;
          mmSave :
            SaveUserMenu;
          mmRename :
            RenameObject(False);
          mmSaveAs :
            RenameObject(True);
          mmObjInfo :
            ShowMenuInfo;
          mmTest :
            ScrollAround;
          mmGenerate :
            GenerateSource;
          mmDocument :
            DocumentUserMenu;
          mmRekey :
            RekeyUserMenu;

        {mmLibrary}
          mmLoad :
            LoadOpLibrary;
          mmNew :
            NewOpLibrary;
          mmDelete :
            DeleteObjects;
          mmInfo :
            ShowOpLibraryInfo;
          mmPack :
            PackOpLibrary;
          mmQuit :
            AllDone := OKToQuit;

      end;
  end;

  procedure Main;
    {-Main program block for MAKEMENU}
  begin
    {save copy of default color set} {!!.03}
    SaveDefColors := DefColors;      {!!.03}

    {initialize main menu, enable mouse, clear screen, etc.}
    Initialize;

    with MainMenu do begin
      {Get library from command line if specified}
      if (ParamCount <> 0) and OpenOpLibrary(ParamStr(1)) then
        {Let user select from list of menus in library}
        LoadUserMenuPrim
      else begin
        {Simulate selection of Load item when first starting}
        DefaultPath(mmLoad);
        MenuCommands.SetCommandList(@FirstCmd, SizeOf(FirstCmd));
      end;

      AllDone := False;
      repeat
        {protect menu items as necessary}
        ProtectMenuItems;

        {get menu choice}
        Process;
        Item := MenuChoice;

        {get last command}
        Cmd := GetLastCommand;

        case Cmd of
          ccSelect :                 {Enter}
            ProcessMenuItem(Item);
          MoveWindowCmd :            {Alt M}
            if HaveObj then
              ProcessMenuItem(mmMoveSub);
          ResizeWindowCmd :          {Alt R}
            if HaveObj then
              ProcessMenuItem(mmResizeSub);
          SaveFileCmd :              {F2}
            if HaveObj then
              SaveUserMenu;
          NewFileCmd :               {F3}
            LoadOpLibrary;
          InfoCmd :                  {Alt I}
            if HaveObj then
              ShowMenuInfo
            else
              ShowOpLibraryInfo;
          MainMenuCmd :              {F10}
            ScrollAround;
          ccQuit :                   {Esc}
            if HaveObj then
              ScrollAround
            else
              AllDone := OkToQuit;
          ccError, ExitCmd :         {Error, Alt X}
            AllDone := OkToQuit;
        end;

      until AllDone;

      if IsCurrent then
        {erase the menu}
        Erase;
    end;

    {close the library if it is open}
    CloseOpLibrary;

    {$IFDEF UseMouse}
    HideMouse;
    {$ENDIF}

    {clean up the screen}
    Window(1, 1, ScreenWidth, ScreenHeight);
    TextAttr := 7;
    ClrScr;
    NormalCursor;
  end;

end.
