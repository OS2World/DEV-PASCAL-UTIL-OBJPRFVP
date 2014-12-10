{$S-,R-,V-,I-,B-,F+,O-,A-} {!!.01}

{*********************************************************}
{*                  DESKMAIN.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{.$DEFINE NoPop}            {If the following directive is defined, DESKPOP
                             will be compiled as a non-resident program. The
                             state of this define MUST agree with the
                             identically named define in DESKPOP.PAS}

{***************************************************************************
 This program requires that OPDEFINE.INC activate the following defines:
   UseDates, UseScrollBars, UseHotSpots, UseAdjustableWindows,
   UseShadows
 This program will use features activated with the following defines:
   UseMouse, ThwartSideKick
 ***************************************************************************}

{$IFNDEF UseDates}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseScrollBars}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseHotSpots}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseShadows}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFDEF UseDrag}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

unit DeskMain;
  {-Main body of DeskPop}

interface

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
  opbrowse,                       {file browser}
  opcalc,                         {calculator}
  opqkref,                        {quick reference chart}
  opmemo,                         {memo editor}
  opeditor,                       {text editor}
  opabsfld,                       {abstract field, picture mask stuff}
  opfield,                        {field definitions}
  opedit,                         {line editor}
  opselect,                       {selectors}
  opentry,                        {entry screens}
  oppick,                         {pick lists}
  opdir,                          {directory lists}
  opcal,                          {popup calendar}
  opmenu,                         {menus}
  tpui                            {TurboPower user interface}
  {$IFNDEF NoPop}                 {!!.01}
  , opswap1;                      {swappable TSRs}
  {$ELSE}                         {!!.01}
  ;                               {!!.01}
  {$ENDIF}                        {!!.01}

const
  Version         = '1.30';
  ModuleName      : String[7] = 'DESKPOP'; {module name for standard interface}
  ProgName        : String[35] = 'DESKPOP '+Version+': Popup Desktop Manager';
  Copyright       : String[41] = 'Copyright (c) 1992 by TurboPower Software';

procedure InitializePopup;
  {-Initialize and try to go resident}

  {==========================================================================}

implementation

type
  PhonePickList =
    object(PickList)
      constructor Init(X1, Y1, X2, Y2 : Byte;
                       ItemWidth : Byte;
                       NumItems : Word;
                       StringProc : pkStringProc;
                       Orientation : pkGenlProc;
                       CommandHandler : pkGenlProc);
        {-Initialize a phone pick list}
      procedure UpdateContents; virtual;
    end;

const
  {window ID's}
  DirWinId        = 1;
  BrowseWinId     = 2;
  CalcWinId       = 3;
  EditWinId       = 4;
  CalWinId        = 5;
  PhoneWinId      = 6;
  QkRefWinId      = 7;
  HelpWinId       = 8;
  AboutId         = 9; {!!.03}

var
  Desk            : DeskTop;         {desktop window manager}
  QkRefWin        : QkRefChart;      {quick reference chart}
  CalcWin         : Calculator;      {pop-up calculator}
  BrowseWin       : Browser;         {file browser}
  EditWin         : TextEditor;      {editor}
  DirWin          : DirList;         {directory list}
  PhoneWin        : PhonePickList;   {phonebook}
  CalWin          : Calendar;        {calendar}
  HelpWin         : Browser;         {help window - displays help file}
  MainMenu        : Menu;            {main menu}
  LocalMenu       : Menu;            {basic local menu}
  SaveWin         : RawWindow;       {used to save underlying screen}

  {$IFDEF UseMouse}
  MSP             : MouseStatePtr;
  MSPsize         : Word;
  {$ENDIF}
const
  EditTitle       = 'Editor: ';
  BrowseTitle     = 'Browser: ';
  PhoneTitle      = 'Phones: ';
  HelpTitle       = ' Help: ';
  LoadError       = 'Unable to install DESKPOP';
  YesNoSt         : array[Boolean] of array[1..3] of Char = ('No ', 'Yes');
  Disable         : Boolean = False;

  {--------- include files ---------------------}

  {$I DESKMAIN.ICD}  {configuration data}
  {$I DESKMAIN.IN1}  {generic local menu, directory, calculator}
  {$I DESKMAIN.IN2}  {editor, phone book}

  {--------- routines for calendar -----------}

  procedure StatusCal;
    {-Display the status line for the calendar window}
  const
    StatusMsg =
      '<F9> Menu   <PgUp>, <PgDn> Month   <^PgUp>, <^PgDn> Year   <^Home> Today';
  begin
    Message(StatusMsg, True);
  end;

  function InitCal(X1, Y1, X2, Y2 : Byte; Msg : string) : Boolean;
    {-Initialize the calendar window}
  const
    CalTitle = 'Calendar';
  begin
    InitCal := False;

    with CalWin do begin
      if not Init(X1, Y1) then begin
        InsufficientMemory;
        Exit;
      end;

      {add headers, hot spots, etc.}
      CustomizeWindow(CalWin, CalTitle, Length(CalTitle));
      if ClassifyError(RawError) = etFatal then {!!.12}
        Done
      else
        InitCal := True;
    end;
  end;

  procedure ProcessCal(var Cmd : Word);
    {-Process the commands for the calendar window}
  label
    Reevaluate;
  var
    AllDone : Boolean;

    function ProcessLocalMenu(var Cmd : Word) : Boolean;
      {-Process local menu commands}
    const
      LastChoice : Word = 1;
    var
      Stop : Boolean;
      P : Pointer;
    begin
      StatusLocalMenu;

      ProcessLocalMenu := True;
      Stop := False;
      repeat
        case GetLocalMenuCmd(CalWin, LastChoice) of
          ccSelect :
            begin
              case LastChoice of
                1 : Cmd := ccQuit;
                2 : Cmd := MoveWindowCmd;
              end;
              ProcessLocalMenu := False;
              Stop := True;
            end;
          {$IFDEF UseMouse}
          ccMouseSel :
            begin
              P := Desk.WindowClickedOn;
              if P = @CalWin then
                Stop := True
              else if P <> nil then begin
                Cmd := ccMouseSel;
                ProcessLocalMenu := False;
                Stop := True;
              end;
            end;
          {$ENDIF}
          MainMenuCmd :
            begin
              Cmd := MainMenuCmd;
              ProcessLocalMenu := False;
              Stop := True;
            end;
          LocalMenuCmd,
          ccQuit,
          ccError :
            Stop := True;
        end;
      until Stop;
      EraseLocalMenu;
    end;

  begin
    AllDone := False;
    repeat
      {draw status line}
      StatusCal;

      {call the calendar}
      CalWin.Process;

      {get the command}
      Cmd := CalWin.GetLastCommand;

Reevaluate:
      {process exit commands}
      if not Desk.ProcessExitCommands(Cmd) then begin
        {process any exit commands that TPUI can't handle for us}
        case Cmd of
          LocalMenuCmd :
            if not ProcessLocalMenu(Cmd) then
              goto Reevaluate;
          NextWindowCmd..SelectWindow8,
          ExitTsrCmd,
          UnloadTsrCmd,
          MainMenuCmd,
          ccError,
          ccQuit :
            AllDone := True;
        end;
      end;
    until AllDone;
  end;

  procedure CloseCal(EraseFirst : Boolean);
    {-Close the calendar window}
  begin
    if EraseFirst then
      CalWin.Erase;
    CalWin.Done;
  end;

  function OkToCloseCal : Boolean;
    {-Return True if OK to close the calendar window}
  begin
    OkToCloseCal := True;
  end;

  {--------- routines for quick reference chart -----------}

  function InitQkRef(X1, Y1, X2, Y2 : Byte; Msg : string) : Boolean;
    {-Initialize the quick reference chart window}
  const
    QkRefTitle = 'Quick Reference Chart';
  begin
    InitQkRef := False;

    with QkRefWin do begin
      if not Init(X1, Y1, X2, Y2) then begin
        InsufficientMemory;
        Exit;
      end;

      {add headers, hot spots, etc.}
      CustomizeWindow(QkRefWin, QkRefTitle, Length(QkRefTitle));

      {customize colors}
      SetDimAttr(QkRefDimColor, QkRefDimMono);
      SetHighlightAttr(QkRefHighlightColor, QkRefHighlightMono);

      if ClassifyError(RawError) = etFatal then {!!.12}
        Done
      else
        InitQkRef := True;
    end;
  end;

  procedure ProcessQkRef(var Cmd : Word);
    {-Process commands for the quick reference chart window}
  label
    Reevaluate;
  var
    AllDone : Boolean;

    function ProcessLocalMenu(var Cmd : Word) : Boolean;
      {-Process local menu commands}
    const
      LastChoice : Word = 1;
    var
      Stop : Boolean;
      P : Pointer;
    begin
      StatusLocalMenu;

      ProcessLocalMenu := True;
      Stop := False;
      repeat
        case GetLocalMenuCmd(QkRefWin, LastChoice) of
          ccSelect :
            begin
              case LastChoice of
                1 : Cmd := ccQuit;
                2 : Cmd := MoveWindowCmd;
                3 : Cmd := ResizeWindowCmd;
                4 : Cmd := ZoomWindowCmd;
              end;
              ProcessLocalMenu := False;
              Stop := True;
            end;
          {$IFDEF UseMouse}
          ccMouseSel :
            begin
              P := Desk.WindowClickedOn;
              if P = @QkRefWin then
                Stop := True
              else if P <> nil then begin
                Cmd := ccMouseSel;
                ProcessLocalMenu := False;
                Stop := True;
              end;
            end;
          {$ENDIF}
          MainMenuCmd :
            begin
              Cmd := MainMenuCmd;
              ProcessLocalMenu := False;
              Stop := True;
            end;
          LocalMenuCmd,
          ccQuit,
          ccError :
            Stop := True;
        end;
      until Stop;
      EraseLocalMenu;
    end;

  begin
    AllDone := False;
    repeat
      {draw status line}
      StatusCalc;

      {browse through the chart}
      QkRefWin.Process;

      {get the command}
      Cmd := QkRefWin.GetLastCommand;

Reevaluate:
      {process exit commands}
      if not Desk.ProcessExitCommands(Cmd) then begin
        {process any exit commands that TPUI can't handle for us}
        case Cmd of
          LocalMenuCmd :
            if not ProcessLocalMenu(Cmd) then
              goto Reevaluate;
          NextWindowCmd..SelectWindow8,
          ExitTsrCmd,
          UnloadTsrCmd,
          MainMenuCmd,
          ccError,
          ccQuit :
            AllDone := True;
        end;
      end;
    until AllDone;
  end;

  procedure CloseQkRef(EraseFirst : Boolean);
    {-Close the quick reference chart window}
  begin
    if EraseFirst then
      QkRefWin.Erase;
    QkRefWin.Done;
  end;

  function OkToCloseQkRef : Boolean;
    {-Return True if OK to close the quick reference chart window}
  begin
    OkToCloseQkRef := True;
  end;

  {--------- routines for file browser -----------}

var
  BrowseMenu         : Menu;
const
  BrowseMenuChoice   : Word = 1;
  ValidBrowseOptions : CharSet = [brBackward, brNoCase, brGlobal];

  procedure BrowseMenuCustom(var Name : String; Key : LongInt;
                             Selected, Highlighted : Boolean;
                             WPtr : RawWindowPtr);
    {-String customization routine for BrowseMenu}
  const
    HexAsciiSt : array[Boolean] of array[1..5] of Char = ('Ascii', 'Hex  ');
    ShowStripSt : array[Boolean] of array[1..5] of Char = ('Show ', 'Strip');

    procedure MergeYesNo(YN : Boolean; Col : Byte);
    begin
      Move(YesNoSt[YN], Name[Col], 3);
    end;

  begin
    with BrowseWin do
      case Word(Key) of
        06 : MergeYesNo(not brBlockOn, 09);
        10 : MergeYesNo(brOptionsAreOn(brTabExpand), 14);
        11 : Move(ShowStripSt[brOptionsAreOn(brStripHigh)], Name[14], 5);
        12 : Move(HexAsciiSt[brOptionsAreOn(brHexMode)], Name[14], 5);
        14 : MergeSearchSt(brSearchSt, Name, 10, 10);
        16 : MergeOptionsSt(brOptionSt, ValidBrowseOptions, Name, 10);
        17 : MergeYesNo(SearchOptionIsSet(brOptionSt, brGlobal), 14);
        18 : MergeYesNo(SearchOptionIsSet(brOptionSt, brNoCase), 14);
        19 : MergeYesNo(SearchOptionIsSet(brOptionSt, brBackward), 14);
      end;
  end;

  function InitBrowseMenu : Boolean;
    {-Initialize the directory window's local menu}
  begin
    InitBrowseMenu := False;

    with BrowseMenu do begin
      {initialize the menu}
      if not InitCustom(2, 2, 15, 10, TpUiMenuColors, DefWindowOptions, Vertical) then
        Exit;

      {add menu items}
      AddItem('Close',            01, 1, 01);
      AddItem('Move',             02, 1, 02);
      AddItem('Resize',           03, 1, 03);
      AddItem('Zoom',             04, 1, 04);
      AddSeparator('Ã', 'Ä', '´', 05);
      AddItem('Block',            06, 1, 05);
        AddFramedSubMenu(4, 9, 15, 11, Vertical, DefWindowFrame);
        AddItem('Hidden Yes',     01, 1, 06);
        AddItem('Print',          02, 1, 07);
        AddItem('Write',          03, 1, 08);
        ItemsDone;
      AddItem('Options',          07, 1, 09);
        AddFramedSubMenu(4, 10, 22, 12, Vertical, DefWindowFrame);
        AddItem('Expand tabs Yes',   01, 1, 10);
        AddItem('High bits   Strip', 02, 1, 11);
        AddItem('Mode        Ascii', 03, 1, 12);
        ItemsDone;
      AddItem('Search',           08, 1, 13);
        AddFramedSubMenu(4, 11, 19, 13, Vertical, DefWindowFrame);
        AddItem('Again   [1234567...]', 01, 1, 14);
        AddItem('Find',                 02, 1, 15);
        AddItem('Options [UBG]',        03, 1, 16);
          AddFramedSubMenu(14, 14, 30, 16, Vertical, DefWindowFrame);
          AddItem('Global      Yes',    01, 1, 17);
          AddItem('Ignore case Yes',    02, 1, 18);
          AddItem('Reverse     Yes',    03, 1, 19);
          ItemsDone;
        ItemsDone;
      AddItem('Text markers',     09, 1, 20);
        AddFramedSubMenu(4, 12, 9, 13, Vertical, DefWindowFrame); {!!.01}
        AddItem('Jump',           01, 1, 21);
        AddItem('Set',            02, 1, 22);
        ItemsDone;
      ItemsDone;

      {install error handler}
      SetErrorProc(TpUiErrorProc);

      {install string customization routine}
      SetCustomStringProc(BrowseMenuCustom);

      {move the window}
      MoveWindow(BrowseWin.wFrame.frXL, BrowseWin.wFrame.frYL);

      {check for errors}
      if ClassifyError(RawError) = etFatal then begin {!!.12}
        Done;
        Exit;
      end;

      {display the menu}
      DefaultPath(BrowseMenuChoice);
      Draw;

      {check for errors}
      if ClassifyError(RawError) = etFatal then {!!.12}
        Done
      else
        InitBrowseMenu := True;
    end;
  end;

  function GetBrowseMenuCmd : Byte;
    {-Get next menu choice; initialize and draw menu if necessary}
  begin
    {is the menu on the screen?}
    if (wStack.TopWindow <> @BrowseMenu) then
      {initialize and display it}
      if not InitBrowseMenu then begin
        InsufficientMemory;
        GetBrowseMenuCmd := ccError;
        Exit;
      end;

    with BrowseMenu do begin
      {get the next menu command}
      Process;
      BrowseMenuChoice := MenuChoice;
      GetBrowseMenuCmd := GetLastCommand;
    end;
  end;

  procedure EraseBrowseMenu;
    {-Erase BrowseMenu}
  begin
    BrowseMenu.Erase;
    BrowseMenu.Done;
  end;

  procedure BrowseWinStatusProc(BP : BrowserPtr);
    {-Display the status line for the file browser window}
  const
    StatusMsg =
      ' <F3> New file  <F9> Menu  <AltH> Hex/Ascii  <Esc> Close  ³ ';
  begin
    with BrowseWin do begin
      if brOptionsAreOn(brNewFile) then begin
        {modify the frame header that shows the filename}
        ChangeTitle(BrowseWin, BrowseTitle+JustFilename(CurrentFileName));

        {clear the new file flag}
        brOptionsOff(brNewFile);
      end;

      ShowStatusString(StatusMsg, 1, Length(StatusMsg));
      if brWorkingFlag <> 0 then
        ShowStatusString('Working...', 61, 20)
      else begin
        ShowStatusString('Line ', 61, 5);
        ShowStatusNumber(brCurLine, 66, 6);
        ShowStatusString('Col ', 72, 4);
        ShowStatusNumber(brColOfs+1, 76, 5);
      end;
    end;
  end;

  function InitBrowse(X1, Y1, X2, Y2 : Byte; Msg : string) : Boolean;
    {-Initialize the file browser window}
  const
    MaxLen = SizeOf(PathStr)-1;
    TitleSize = Length(BrowseTitle)+12;
  var
    FName : PathStr;
  begin
    InitBrowse := False;

    {get the name of the file to browse}
    if Msg <> '' then
      FName := Msg
    else begin
      FName := '';
      if not GetFile(0, 'File to browse: ', True, True, False, True,
                     MaxLen, BrowseDefExt, FName) then
        Exit;
    end;

    with BrowseWin do begin
      {initialize the browser}
      if not Init(X1, Y1, X2, Y2, LongInt(BrowsePages) * OnePage) then begin
        InsufficientMemory;
        Exit;
      end;

      {install default error handler}
      SetErrorProc(TpUiErrorProc);

      {try to open it}
      OpenFile(FName);
      if ClassifyError(RawError) = etFatal then begin {!!.12}
        Done;
        Exit;
      end;

      {initialize procedure pointers}
      SetEditProc(EditProc);
      SetGetFileProc(GetFile);
      SetStatusProc(BrowseWinStatusProc);

      {add headers, hot spots, etc.}
      CustomizeWindow(BrowseWin, BrowseTitle+JustFileName(FName), TitleSize);

      {customize colors}
      SetTextAttr(BrowseTextColor, BrowseTextMono);

      if ClassifyError(RawError) = etFatal then {!!.12}
        Done
      else
        InitBrowse := True;
    end;

  end;

  procedure ProcessBrowse(var Cmd : Word);
    {-Process the commands for the file browser window}
  label
    Reevaluate;
  var
    AllDone : Boolean;

    procedure DoBrowseCommand(Cmd : Word; OffScreen : Boolean);
      {-Execute the specified browser command. If OffScreen is True, switch to
        a virtual screen first.}
    var
      CmdList : array[1..2] of Byte;
    begin
      if OffScreen then begin
        BrowseWin.SetCursor(cuHidden);
        BrowseWin.ActivateWrite;
      end
      else
        EraseBrowseMenu;

      CmdList[1] := Cmd;
      CmdList[2] := ccUser55;
      BrowseWin.ProcessAutoPilot(CmdList, 2);

      StatusLocalMenu;
      if OffScreen then
        BrowseWin.DeactivateWrite;
    end;

    function ProcessLocalMenu(var Cmd : Word) : Boolean;
      {-Process local menu commands}
    var
      Stop : Boolean;
      MN : Byte;
      Ch : Char;
      P : Pointer;

      procedure UpdateMenu;
      begin
        {BrowseMenu.ActivateWrite;} {!!.01}
        BrowseMenu.Redraw;
        {BrowseMenu.DeactivateWrite;} {!!.01}
      end;

    begin
      StatusLocalMenu;

      ProcessLocalMenu := True;
      Stop := False;
      with BrowseWin do
        repeat
          case GetBrowseMenuCmd of
            ccSelect :
              case BrowseMenuChoice of
                1..4 :
                  begin
                    case BrowseMenuChoice of
                      1 : Cmd := ccQuit;
                      2 : Cmd := MoveWindowCmd;
                      3 : Cmd := ResizeWindowCmd;
                      4 : Cmd := ZoomWindowCmd;
                    end;
                    ProcessLocalMenu := False;
                    Stop := True;
                  end;
                06 : DoBrowseCommand(ccBlkToggle, True);
                07 : DoBrowseCommand(ccBlkPrint, True);
                08 : DoBrowseCommand(ccBlkWrite, False);
                10 : DoBrowseCommand(ccTabExpand, True);
                11 : DoBrowseCommand(ccStripHigh, True);
                12 : DoBrowseCommand(ccHexMode, True);
                14 : if Length(brSearchSt) <> 0 then
                       DoBrowseCommand(ccReSearch, False);
                15 : DoBrowseCommand(ccSearch, False);
                17..19 :
                  begin
                    case BrowseMenuChoice of
                      17 : Ch := brGlobal;
                      18 : Ch := brNoCase;
                      19 : Ch := brBackward;
                    end;
                    ToggleSearchOption(brOptionSt, Ch,  ValidBrowseOptions);
                    UpdateMenu;
                  end;
                21 : if GetMarkerNumber(True, MN) then
                       DoBrowseCommand(ccJmpMark0+MN, True);
                22 : if GetMarkerNumber(False, MN) then
                       DoBrowseCommand(ccSetMark0+MN, True);
              end;
            {$IFDEF UseMouse}
            ccMouseSel :
              begin
                P := Desk.WindowClickedOn;
                if P = @BrowseWin then
                  Stop := True
                else if P <> nil then begin
                  Cmd := ccMouseSel;
                  ProcessLocalMenu := False;
                  Stop := True;
                end;
              end;
            {$ENDIF}
            MainMenuCmd :
              begin
                Cmd := MainMenuCmd;
                ProcessLocalMenu := False;
                Stop := True;
              end;
            LocalMenuCmd,
            ccQuit,
            ccError :
              Stop := True;
          end;
        until Stop or (RawError <> 0); {!!.12}
      EraseBrowseMenu;
    end;

  begin
    AllDone := False;
    repeat
      {browse}
      BrowseWin.Process;

      {get the command}
      Cmd := BrowseWin.GetLastCommand;

Reevaluate:
      {process exit commands}
      if not Desk.ProcessExitCommands(Cmd) then begin
        {process any exit commands that TPUI can't handle for us}
        case Cmd of
          LocalMenuCmd :
            if not ProcessLocalMenu(Cmd) then
              goto Reevaluate;
          NextWindowCmd..SelectWindow8,
          ExitTsrCmd,
          UnloadTsrCmd,
          MainMenuCmd,
          ccError,
          ccQuit :
            AllDone := True;
        end;
      end;
    until AllDone;
  end;

  procedure CloseBrowse(EraseFirst : Boolean);
    {-Close the file browser window}
  begin
    if EraseFirst then
      BrowseWin.Erase;
    BrowseWin.Done;
  end;

  function OkToCloseBrowse : Boolean;
    {-Return True if OK to close the file browser window}
  begin
    OkToCloseBrowse := True;
  end;

  {-------- main menu ------}

  procedure DrawMainMenuStatusMsg;
    {-Draw the main menu's status line}
  const
    StatusMsg =
      '<F5> Zoom  <F6> Next  <^F6> Prev  <AltM> Move  <AltR> Resize  <AltX> Exit';
  begin
    Message(StatusMsg, True);
  end;

  procedure InitMenu;
    {-Initialize the main menu}
  begin
    with MainMenu do begin
      {initialize the menu}
      if not InitCustom(1, 1, 80, 1, TpUiMenuColors,
                        DefWindowOptions and not wBordered, Horizontal) then begin
        WriteLn(emInsufficientMemory);
        Halt;
      end;

      {add menu items}
      AddItem(' ð ' ,         02, 0, AboutId);      {!!.03}
      AddItem(' Files ' ,     05, 2, DirWinId);     {!!.03}
      AddItem(' Browser ',    12, 2, BrowseWinId);  {!!.03}
      AddItem(' Calculator ', 21, 2, CalcWinId);    {!!.03}
      AddItem(' Editor ',     33, 2, EditWinId);    {!!.03}
      AddItem(' caLendar ',   41, 4, CalWinId);     {!!.03}
      AddItem(' Phonebook ',  51, 2, PhoneWinId);   {!!.03}
      AddItem(' Reference ',  62, 2, QkRefWinId);   {!!.03}
      AddItem(' Help ',       73, 2, HelpWinId);    {!!.03}
      ItemsDoneCustom(False, 1);

      {install error handler}
      SetErrorProc(TpUiErrorProc);
    end;

    {add special user exit commands}
    MenuCommands.SetSecondaryKeyPtr(@TpUiKeySet, TpUiKeyMax);

    {$IFDEF UseMouse}
      {enable mouse support in OPMENU}
      if MouseInstalled then
        MenuCommands.cpOptionsOn(cpEnableMouse);
    {$ENDIF}
  end;

  {-------------- general ----------------}

  procedure InitializeDesktop;
    {-Initialize the desktop window manager}
  begin
    with Desk do begin
      Init(3, 4, ScreenWidth-3, ScreenHeight-3, 7);

      {$IFDEF UseMouse}
        {enable mouse support in OPEDIT}
        if MouseInstalled then
          EditCommands.cpOptionsOn(cpEnableMouse);
      {$ENDIF}

      {Window 1: Files}
      DirCommands.Init(@DirKeySet, DirKeyMax);
      if not AddWindow(DirWin, InitDir, ProcessDir, CloseDir,
                       OkToCloseDir, DirCommands) then
        Halt;

      {Window 2: File Browser}
      if not AddWindow(BrowseWin, InitBrowse, ProcessBrowse, CloseBrowse,
                       OkToCloseBrowse, BrowseCommands) then
        Halt;

      {Window 3: Calculator}
      if not AddWindow(CalcWin, InitCalc, ProcessCalc, CloseCalc,
                       OkToCloseCalc, CalcCommands) then
        Halt;

      {Window 4: Editor}
      if not AddWindow(EditWin, InitEditor, ProcessEditor, CloseEditor,
                       OkToCloseEditor, EditorCommands) then
        Halt;

      {Window 5: Calendar}
      if not AddWindow(CalWin, InitCal, ProcessCal, CloseCal,
                       OkToCloseCal, CalCommands) then
        Halt;

      {Window 6: Phone book}
      InitPhoneCommands;
      if not AddWindow(PhoneWin, InitPhone, ProcessPhone, ClosePhone,
                       OkToClosePhone, PhoneCommands) then
        Halt;

      {Window 7: Quick Reference Chart}
      if not AddWindow(QkRefWin, InitQkRef, ProcessQkRef, CloseQkRef,
                       OkToCloseQkRef, QkRefCommands) then
        Halt;

      {install help routine for all command processors}
      MenuCommands.SetHelpProc(OurHelpProc);
      DirCommands.SetHelpProc(OurHelpProc);
      BrowseCommands.SetHelpProc(OurHelpProc);
      CalcCommands.SetHelpProc(OurHelpProc);
      EditorCommands.SetHelpProc(OurHelpProc);
      CalCommands.SetHelpProc(OurHelpProc);
      PhoneCommands.SetHelpProc(OurHelpProc);
      QkRefCommands.SetHelpProc(OurHelpProc);
    end;
  end;

  procedure About;  {!!.03}
    {-Display program name, copyright info}
  var
    W : CommandWindow absolute HelpWin;
    I : Word;
  begin

    if not W.Init(18, 5, 62, 19, BrowseCommands, 0) then begin
      InsufficientMemory;
      Exit;
    end;
    W.wOptionsOff(wResizeable);
    CustomizeWindow(W, '', 0);
    W.SetCursor(cuHidden);
    W.wFrame.AddHeader(' About ', heTC);
    W.Draw;
    W.wFastCenter(ProgName, 2, TextAttr);
    W.wFastCenter('Created with Object Professional', 3, TextAttr);
    W.wFastCenter(Copyright, 4, TextAttr);
    W.wFastCenter('P.O. Box 49009', 6, TextAttr);
    W.wFastCenter('Colo Springs, CO 80949', 7, TextAttr);
    W.wFastCenter('Tech support: 719-260-6641', 9, TextAttr);
    W.wFastCenter('Order line: 800-333-4160', 10, TextAttr);
    W.wFastCenter('This program may be distributed freely', 12, TextAttr);
    W.wFastCenter('Press any key...', 14, TextAttr);
    I := GetKey;
    W.Done;
  end;

  procedure PopupEntryPoint;
    {-Routine called when hotkey is pressed}
  const
    SaveCurrent : RawWindowPtr = nil;
    EmuBit      = $01;
    SaveColor   : Boolean = True;  {save value of UseColor}
  var
    PoppedOnce  : Boolean;
    Cmd         : Word;            {command used to exit from a window}
    Item        : LongInt;         {main menu item selection}
    NextWin     : Byte;            {next window to go to}
    Save8x8     : Boolean;         {using 8x8 font?}
    CursorSL    : Word;            {saved cursor scan lines}
    EmuFlag     : Byte
{$IFNDEF VIRTUALPASCAL}
    absolute $40:$87
{$ENDIF};
    SaveEmu     : Boolean;         {saved cursor emulation state}
    WP          : WindowPtr;

{$IFNDEF NoPop}                 {!!.01}
    procedure UnloadPrim;
      {-Try to unload the TSR}
    begin
      if Desk.OkToQuit or (Cmd = ccError) then begin
        if DisableTsr then begin
          Cmd := ExitTsrCmd;
          Message('Unloading DESKPOP...', True);
          Delay(750);
        end
        else begin
          ErrorMessage('Unable to unload DESKPOP', True);
          if Cmd = ccError then begin
            {don't pop up again}
            PopupsOff;
            Cmd := ExitTsrCmd;
          end
          else
            Cmd := MainMenuCmd;
        end
      end
      else
        Cmd := MainMenuCmd;
    end;
{$ENDIF}                        {!!.01}

  begin
    {re-initialize CRT}
    ReInitCrt;

    {make sure we're in text mode}
    if not (InTextMode and (ScreenWidth = 80)) then begin
      RingBell;
      Exit;
    end;

    {$IFDEF UseMouse}                      {!!.12}
    if MouseInstalled then begin           {!!.12}
      {$IFNDEF NoPop}                      {!!.12}
      {save the state of the mouse driver} {!!.12}
      SaveMouseState(MSP, False);          {!!.12}
      {$ENDIF}                             {!!.12}
    end;                                   {!!.12}
    {$ENDIF}                               {!!.12}

    {switch out of 8x8 font if appropriate}
    Save8x8 := Font8x8Selected;
    if Save8x8 then begin
      {save cursor info}
      CursorSL := CursorTypeSL;
      SaveEmu := ByteFlagIsSet(EmuFlag, EmuBit);

      {switch to 80x25 screen}
      SelectFont8x8(False);
    end
    else if ScreenHeight <> 25 then begin       {!!.01}
      RingBell;                                 {!!.01}
      Exit;                                     {!!.01}
    end;                                        {!!.01}

    {$IFNDEF NoPop}  {!!.03}
    PopupsOff;       {!!.03}
    {$ENDIF}         {!!.03}

    {$IFDEF UseMouse}
    if MouseInstalled then begin
      {$IFNDEF NoPop}                 {!!.01}
      {save the state of the mouse driver}
      {SaveMouseState(MSP, False);}   {!!.12}
      {$ENDIF}                        {!!.01}

      {reinitialize the mouse}
      ReinitMouse;

      {enable our event handler}
      EnableEventHandling;
    end;
    {$ENDIF}

    {draw the main window}
    SaveWin.Draw;
    PoppedOnce := (SaveCurrent <> nil);
    if not PoppedOnce then begin
      ClearWholeScreen;
      SaveColor := UseColor;
    end
    else begin
      CurrentWindow := SaveCurrent;

      {do we need to redraw all windows?}
      if SaveColor <> UseColor then begin
        {find the window at the bottom of the stack}
        WP := WindowPtr(wStack.Peek(1));
        if (WP <> nil) and (wStack.SP > 0) then
          with WP^ do begin
            ActivateWrite;
            Erase;
            ClearWholeScreen;
            Draw;
            DeactivateWrite;
          end;

        SaveColor := not SaveColor;
      end;
    end;
    {$IFDEF UseMouse}
    ShowMouse;
    {$ENDIF}

    {display the main menu and assign it an arbitrarily large window number}
    if not PoppedOnce then begin
      MainMenu.Draw;
      MainMenu.wNumber := 200;
      NextWin := 0;
      Cmd := MainMenuCmd;
    end
    else if MainMenu.IsCurrent then
      Cmd := MainMenuCmd
    else
      Cmd := ProcessWindowCmd;

    repeat
      with MainMenu do
        if Cmd <> MainMenuCmd then begin
          Select;
          EraseAllSubMenus(False, True);
          Desk.ProcessCommands(Cmd);
          {$IFDEF UseMouse}
          if Cmd = SelectWindowCmd then begin
            Select;
            if SelectItemByPos(MouseKeyWordX+MouseXLo, MouseKeyWordY+MouseYLo) then {};
            Cmd := MainMenuCmd;
          end;
          {$ENDIF}
          {$IFNDEF NoPop}                 {!!.01}
          if (Cmd = UnloadTsrCmd) or (Cmd = ccError) then
            UnloadPrim;
          {$ENDIF}                        {!!.01}
        end
        else begin
          Select;
          DrawMainMenuStatusMsg;

          Process;
          Item := MenuChoice;
          Cmd := GetLastCommand;

          if (GetLastCommand = ccSelect) then  {!!.03}
            case Item of                       {!!.03}
              DirWinId..QkRefWinId :           {!!.03}
                begin                          {!!.03}
                  NextWin := Item;             {!!.03}
                  Cmd := ProcessWindowCmd;     {!!.03}
                end                            {!!.03}
          end;                                 {!!.03}

          case Cmd of
            ccSelect :
              if Item = AboutId then    {!!.03}
                About                   {!!.03}
              else                      {!!.03}
                DisplayHelpFile;        {!!.03}
            ShowMemoryCmd :
              ShowAvailableMemory;
            ProcessWindowCmd :
              if NextWin <> 0 then
                if Desk.ActivateWindow(NextWin, '') then
                  NextWin := 0
                else
                  Cmd := MainMenuCmd;
            SelectWindow1..SelectWindow8 :
              if Desk.ProcessExitCommands(Cmd) then
                Cmd := MainMenuCmd;
            PrevWindowCmd,
            NextWindowCmd :
              if Desk.dtActive <= 1 then
                Cmd := MainMenuCmd;
            MainMenuCmd,
            ccQuit :
              if Desk.dtActive = 0 then
                Cmd := ExitTsrCmd
              else
                Cmd := ProcessWindowCmd;
            ExitTsrCmd :
              {just exit} ;
          {$IFNDEF NoPop}                 {!!.01}
            ccError,
            UnloadTsrCmd :
              UnloadPrim;
          {$ENDIF}                        {!!.01}
            {$IFDEF UseMouse}
            ccMouseSel :
              begin
                Desk.SelectWindow(Cmd);
                if Cmd = ccNone then
                  Cmd := MainMenuCmd;
              end;
            {$ENDIF}
            else
              Cmd := MainMenuCmd;
          end;
        end;
    until (Cmd = ExitTsrCmd) or (Cmd = ccError); {!!.03}

    {$IFDEF UseMouse}
    HideMouse;
    {$ENDIF}

    {restore the underlying screen}
    SaveCurrent := CurrentWindow;
    CurrentWindow := @SaveWin;
    SaveWin.Erase;
    CurrentWindow := nil {SaveCurrent} ;        {!!.01}

    {switch back to 8x8 font if appropriate}
    if Save8x8 then begin
      {switch to 8x8 font}
      SelectFont8x8(True);

      {restore cursor state exactly}
      SetCursorSize(Hi(CursorSL), Lo(CursorSL));
      if SaveEmu then
        SetByteFlag(EmuFlag, EmuBit)
      else
        ClearByteFlag(EmuFlag, EmuBit);
    end;

    {$IFDEF UseMouse}
    if MouseInstalled then begin
      {disable our event handler}
      DisableEventHandling;

      {$IFNDEF NoPop}                 {!!.01}
      {restore the state of the mouse driver}
      RestoreMouseState(MSP, False);
      {$ENDIF}                        {!!.01}
    end;
    {$ENDIF}

    {$IFNDEF NoPop}  {!!.03}
    PopupsOn;        {!!.03}
    {$ENDIF}         {!!.03}
  end;

{$IFNDEF NoPop}                 {!!.03}
  function ScreenWord : string; {!!.03}
    {-Parse word from screen at cursor}
  var
    Bpos : Byte;
    EPos : Byte;
    SLine : string;

    function WordChar(Ch : Char) : Boolean;
      {-Return true if Ch is a character in a word representing a phone number}
    begin
      WordChar := (Pos(Ch, '0123456789-') <> 0);
    end;

  begin
    {Assume failure}
    ScreenWord := '';

    {Read entire screen row}
    FastRead(ScreenWidth, WhereYabs, 1, SLine);

    {Back up until in a word}
    EPos := WhereXabs;
    while (EPos > 1) and not WordChar(SLine[EPos]) do
      Dec(EPos);

    {Get out if no word available}
    if not WordChar(SLine[EPos]) then
      Exit;

    {Find beginning of word}
    Bpos := EPos;
    while (Bpos > 0) and WordChar(SLine[Bpos]) do
      Dec(Bpos);

    {Find end of word}
    while (EPos <= Length(SLine)) and WordChar(SLine[EPos]) do
      Inc(EPos);

    {Extract the word from the string}
    ScreenWord := Copy(SLine, Bpos+1, EPos-Bpos-1);
  end;

  procedure DialerEntryPoint;     {!!.03}
    {-Entry point for phone dialer pop-up}
  var
    S : string;
    P : Pointer;
    SaveStatusRow : Byte;
    CursorXY, CursorSL : Word;
    CD : ComData;
  begin
    {re-initialize CRT}
    ReInitCrt;

    {make sure we're in text mode}
    if not InTextMode then begin
      RingBell;
      Exit;
    end;

    S := ScreenWord;
    if S = '' then
      RingBell
    else begin
      {position status line}
      SaveStatusRow := StatusRow;
      if StatusRow <> ScreenHeight then
        StatusRow := ScreenHeight;
      if StatusRow = WhereYabs then
        StatusRow := 1;

      {try to save the status line}
      if not SaveStatusLine(P, CursorXY, CursorSL) then begin
        RingBell;
        StatusRow := SaveStatusRow;
        Exit;
      end;

      {dial the phone number}
      SaveCommState(CD);
      DialPhonePrim(S);
      RestoreCommState(CD);

      {restore status line}
      RestoreStatusLine(P, CursorXY, CursorSL);
      StatusRow := SaveStatusRow;
    end;
  end;

{$ENDIF}                        {!!.03}


  {------- routines to parse command line, go resident -----------}

  procedure Abort(Msg : string);
    {-Display Msg and abort}
  begin
    WriteLn(Msg);
    Halt(1);
  end;

{$IFNDEF NoPop}                 {!!.01}

  procedure Warning(Message : String);
    {-Display warning message, wait for keypress, if key is ESC, then Abort}
  var
    C : Char;
  begin
    WriteLn('Warning: ', Message);
    Write(^M^J'Press ESC to abort, any other key to continue...');
    C := ReadKey;
    Write(^M);
    ClrEOL;
    if C = ^[ then
      Halt;
  end;

  procedure DisableYourself;
    {-Unload resident copy of DESKPOP (if possible) and report results}
  var
    IFC : IfcPtr;
    Save : Boolean;
  begin
    {restore all vectors taken over by this copy of DESKPOP}
    RestoreAllVectors;

    {get the IFCPtr for this module}
    IFC := ModulePtrByName(ModuleName);

    {make sure it is already installed}
    if IFC = nil then
      WriteLn('DESKPOP not installed.')
    else with IFC^, CSDataPtr^ do begin
      {save state of swap messages and disable them}
      Save := SwapMsgOn;
      SwapMsgOn := False;

      {this may take a while...}
      WriteLn('Unloading DESKPOP...');

      {call the CmdEntryPtr}
      CmdEntryPtr;

      {check status of Unload attempt}
      if LongInt(UserData) = 1 then
        WriteLn('DESKPOP unloaded')
      else
        WriteLn('Unable to unload DESKPOP');

      {restore state of swap messages}
      SwapMsgOn := Save;
    end;

    Halt;
  end;

  procedure ShowHelp;
    {-Displays help message with DESKPOP options}
  begin
    WriteLn(^M^J'Usage: DESKPOP [Options]'^M^J);
    WriteLn('Options are:');
    WriteLn('  /U          Unload DESKPOP from memory');
    WriteLn('  /E          don''t use EMS');
    WriteLn('  /M          squelch swapping Messages');
{$IFDEF SupportXms}  {!!.02}
    WriteLn('  /X          use XMS memory if available');
    WriteLn('  /1          single swap file (for RAM disks or XMS)');
{$ELSE}
    WriteLn('  /1          single swap file (for RAM disks)');
{$ENDIF}
    WriteLn('  /A          use normal file attribute for swap file (instead of hidden)');
    {$IFDEF UseMouse}
    WriteLn('  /K          don''t use mouse (Kill mouse)');
    {$ENDIF}
    WriteLn('  /Ppathname  specify Pathname to use for swapping');
    WriteLn('  /?          display this help screen');
    Halt(0);
  end;

  procedure ParseCommandLine;
    {-Interpret command line parameters}
  var
    I   : Word;
    Opt : String;

    procedure InvalidOption;
      {-displays error message and aborts}
    begin
      WriteLn(Opt+' is an invalid option');
      ShowHelp;
    end;

  begin
    {resident mode if no parameters specified}
    if ParamCount = 0 then
      Exit;

    for I := 1 to ParamCount do begin
      Opt := ParamStr(I);
      if ((Opt[1] = '/') or (Opt[1] = '-')) and (Length(Opt) >= 2) then begin
        case UpCase(Opt[2]) of
          'U' : DisableYourself;
          'E' : SwapUseEMS := False;
          'M' : ShowSwapMsg := False;
          '1' : SetSingleSwapFile(True);               {!!.02}
        {$IFDEF SupportXms}  {!!.02}
          'X' : begin                                  {!!.02}
                  SwapUseXms := True;                  {!!.02}
                  EmsOverXms := False;                 {!!.02}
                end;                                   {!!.02}
        {$ENDIF}
          'A' : SetSwapFileAttr(False);                {!!.02}
          'P' : begin
                  SwapPathName := StUpcase(Copy(Opt,3,Length(Opt)));
                  if SwapPathName[Length(SwapPathName)] <> '\' then
                    SwapPathName := SwapPathName + '\';
                end;
          {$IFDEF UseMouse}
          'K' : DefMouseSupport := False;
          {$ENDIF}
          '?' : ShowHelp;
          else InvalidOption;
        end;
      end;
    end;

    {$IFDEF UseMouse}
    MouseInstalled := MouseInstalled and DefMouseSupport;
    {$ENDIF}
  end;

  function DriveIsFixed(Drive : Char) : Boolean;
    {-Return true if drive is not removable}
  var
    SubDrive : Char;
  begin
    case GetDiskClass(Drive, SubDrive) of
      Floppy360, Floppy720, Floppy12, Floppy144, OtherFloppy :
        DriveIsFixed := False;
      else
        DriveIsFixed := True;
    end;
  end;

  function PathIsValidFixedDisk(Path : String) : Boolean;
    {-Return true if drive specified by Path is a valid fixed disk}
  var
    Drive : Char;
    F : File;
    E : Integer;
  begin
    Assign(F,SwapPathName+SwapName1);
    Reset(F, 1);                        {!!.13}
    E := IoResult;                      {!!.13}
    if E <> 0 then begin                {!!.13}
      Rewrite(F,1);
      E := IoResult;
    end;                                {!!.13}
    case E of
      0 : begin
            Close(F);
            if IoResult <> 0 then
              Abort('Error closing swap file');
          end;
      5 : begin
            {swap files exists, but access denied, probably still
             hidden/system file. Not an error, swap file will be deleted
             by swap system}
          end;
      else
        Abort('Cannot create swap file. Invalid path or drive not ready');
    end;
    if Path[2] = ':' then
      Drive := UpCase(Path[1])
    else
      Drive := DefaultDrive;
    PathIsValidFixedDisk := DriveIsFixed(Drive);
  end;

  procedure DeleteSwapFile(Name : String);             {!!.03}
    {-Deletes the swap file if it already exists}
  var
    F : File;
  begin
    Assign(F, Name);
    {first insure it has a file attribute of 0}
    SetFAttr(F, 0);
    {if we fail, then no such file exists}
    if DosError = 0 then begin
      Erase(F);
      if IoResult <> 0 then ;
    end;
  end;

{$ENDIF}                        {!!.01}

  procedure InitializePopup;
    {-Initialize and try to go resident}
  var
    SwapToEms, SwapToXms : Boolean;                 {!!.02}
  begin
{$IFNDEF NoPop}                 {!!.01}
    {determine whether to use special $FF msg row}
    case CurrentDisplay of
      MCGA, EGA, VGA : SetSwapMsgRow($FF);
    end;

    {signon message}
    HighVideo;
    WriteLn(^M^J, ProgName, ^M^J, Copyright, ^M^J);
    LowVideo;

    {interpret command line parameters}
    ParseCommandLine;

    {check to make sure the swap path refers to a valid FIXED disk}
    SwapToEms := WillSwapUseEms(MaxParagraphsToKeep);
  {$IFDEF SupportXms}  {!!.02}
    SwapToXms := WillSwapUseXms(MaxParagraphsToKeep);    {!!.02}
  {$ELSE}
    SwapToXms := False;
  {$ENDIF}
    if (not (SwapToEms or SwapToXms)) and               {!!.02}
       (not PathIsValidFixedDisk(SwapPathName)) then     {!!.02}
      Warning('The selected swap path refers to a removable drive!');

    {check to see if we're already installed}
    if ModuleInstalled(ModuleName) then
      Abort('DESKPOP is already loaded. Aborting...');

    {install the module with special external interface to allow disabling}
    InstallModule(ModuleName,DisablePopup);

    {check to see if SideKick is loaded}
    if SideKickLoaded then
      Abort('Can''t be loaded after SideKick!');

    {$IFDEF UseMouse}
    if MouseInstalled then begin
      {allocate the buffer used to save the state of the mouse}
      MSPsize := MouseStateBufferSize;

      {if the size is 0 or > 1000, assume that it's not safe to use the mouse}
      if (MSPsize = 0) or (MSPsize > 1000) then begin                {!!.02}
        HighVideo;                                                   {!!.02}
        WriteLn('This mouse driver does not support TSR functions'); {!!.02}
        NormVideo;                                                   {!!.02}
        MouseInstalled := False
      end                                                            {!!.02}
      else
        GetMem(MSP, MSPsize);
    end;
    {$ENDIF}
{$ENDIF}                        {!!.01}

    {force screen dimensions to 80*25 temporarily} {!!.01}
    ScreenHeight := 25;                            {!!.01}
    ScreenWidth := 80;                             {!!.01}

    {init window used to save the underlying screen}
    if not SaveWin.InitCustom(1, 1, 80, 25, TpUiColors, wSaveContents) then
      Abort(emInsufficientMemory);

    {initialize the main menu}
    InitMenu;

    {initialize the desktop}
    InitializeDesktop;

    {reset ScreenHeight and ScreenWidth} {!!.01}
    ScreenHeight := VirtualHeight;       {!!.01}
    ScreenWidth := VirtualWidth;         {!!.01}

{$IFNDEF NoPop}                 {!!.01}
    {go resident}
    if DefinePop(HotKey1, PopupEntryPoint, Ptr(SSeg, SPtr)) then begin
      {show how to invoke DESKPOP}
      Write('DESKPOP loaded. ');
      if HotKey1Str <> '' then
        WriteLn('Press ', HotKey1Str, ' to activate desktop.');

      {add secondary hot key}
      if not AddHotKey(HotKey1, HotKey2) then {error};

      {check for a modem}  {!!.03}
      ComPortBaseCheck;    {!!.03}
      if HaveModem then    {!!.03}
        if DefinePop(HotKey3, DialerEntryPoint, Ptr(SSeg, SPtr)) then {!!.03}
          WriteLn('Press ', HotKey3Str, ' to invoke phone dialer.');  {!!.03}

      {Enable popups}
      PopupsOn;
      if SwapToEms then
        WriteLn('Using EMS memory for swap')
    {$IFDEF SupportXms}  {!!.02}
      else if SwapToXms then                      {!!.02}
        WriteLn('Using XMS memory for swap')      {!!.02}
    {$ENDIF}
      else begin                                  {!!.03}
        WriteLn('Swapping to ', SwapPathName, SwapName1);
        DeleteSwapFile(SwapName1);                {!!.03}
      end;                                        {!!.03}
      SetSwapMsgOn(ShowSwapMsg);

      {$IFDEF UseMouse}
      if MouseInstalled then begin
        DisableEventHandling;
        HideMouse;
      end;
      {$ENDIF}

      {terminate and stay resident}
      StayResSwap(MaxParagraphsToKeep-2,
                 0,
                 SwapPathName+SwapName1,
                 SwapPathName+SwapName2,
                 True);
    end;

    {if we get here we failed}
    Abort(LoadError);
{$ELSE}                         {!!.01}
    PopupEntryPoint;            {!!.01}
{$ENDIF}                        {!!.01}
  end;

  procedure InitDefaults;
    {-Initialize default settings based on configuration options}

    procedure SetWordOption(var Flags : Word; Mask : Word; On : Boolean);
    begin
      if On then
        SetFlag(Flags, Mask)
      else
        ClearFlag(Flags, Mask);
    end;

    procedure SetLongOption(var Flags : LongInt; Mask : LongInt; On : Boolean);
    begin
      if On then
        SetLongFlag(Flags, Mask)
      else
        ClearLongFlag(Flags, Mask);
    end;

  begin
    {$IFNDEF NoPop}                 {!!.01}
    SwapUseEms := UseEmsIfAvail;
    {$ENDIF}                        {!!.01}

    {$IFDEF UseMouse}
    if not DefMouseSupport then
      OpMouse.MouseInstalled := False;
    {$ENDIF}
    OpCrt.DefColorChoice := DefColorMap;

    SetWordOption(DefBrowseOptions, brTabExpand, BrowseExpandTabs);
    SetWordOption(DefBrowseOptions, brStripHigh, BrowseStripHigh);
    SetWordOption(DefBrowseOptions, brHexMode, BrowseHexMode);
    OpBrowse.DefBrowseExt := BrowseDefExt;

    SetLongOption(DefEditorOptions, teIndent, EditAutoIndent);
    SetLongOption(DefEditorOptions, teWordWrap, EditWordWrap);
    SetLongOption(DefEditorOptions, teDeleteJoins, EditDeleteJoins);
    SetLongOption(DefEditorOptions, teIndentIsPara, EditIndentIsPara);
    SetLongOption(DefEditorOptions, teAllowTrunc, EditReadPartial);
    SetLongOption(DefEditorOptions, teMakeBackups, EditMakeBackups);
    SetLongOption(DefEditorOptions, teSmartTabs, EditSmartTabs);
    SetLongOption(DefEditorOptions, teWrapAtLeft, EditWrapAtLeft);
    OpMemo.DefTabDelta := EditTabDelta;
    OpEditor.DefEditorExt := EditDefExt;
  end;

begin
  {initialize default settings based on configuration options}
  InitDefaults;
end.
