{$IFNDEF VIRTUALPASCAL}
{$S-,R-,V-,I-,B-,F-,A-}
{$M 4096,4096,655360}
{$ENDIF}

{*********************************************************}
{*                    SMACS.PAS 1.30                     *}
{*            Memory Resident Macro Processor            *}
{*     An example program for Object Professional 1.0    *}
{*      Copyright (c) TurboPower Software 1987, 1992.    *}
{* Portions Copyright (c) Sunny Hill Software 1985, 1986 *}
{*     and used under license to TurboPower Software     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{$DEFINE UseEnhKbd}        {if DEFINEd, SMACS supports enhanced keyboard}

{***************************************************************************
 This program will use features activated with the following defines:
   ThwartSideKick
 This program is not designed to work with the following define:
   UseDrag
 ***************************************************************************}

{$IFDEF UseDrag} {!!.10}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

program SuperMacs;

uses
  {$IFDEF UseEnhKbd}
  OpEnhKbd,  {MUST come before OPTSR!}
  {$ENDIF}
  Dos,
  OpInline,
  OpString,
  OpClone,
  OpDos,
  OpCrt,
  OpCmd,
  OpSEdit,
  OpInt,
  OpTsr,
  OpMacro,
  OpMacEd;

const
  {** keep the following together to allow easy patching **}
  ModuleName       : string[10] = 'SUPER MACS';
  ExeName          : string[12] = 'SMACS.EXE';

  {define our hot keys}
  MainHotKey       : Word = $0829; {Alt+ '`'}
  RecordOnHotKey   : Word = $080D; {Alt + '='}
  RecordOffHotKey  : Word = $080C; {Alt + '-'}
  ReaderHotKey     : Word = $0839; {Alt + ' '}

  OurPlaybackDelay : Word = 0;     {delay factor for macro playback}
  MacroHeap        : Word = 4096;  {heap space to set aside for macros}

  {$IFDEF UseEnhKbd}
  UseEnhancedCalls : Boolean = True;
  {$ENDIF}

  CfgEnd           : Byte = $FF;

  {**** end of configuration data ****}

type
  OrientationType = (LeftRight, UpDown);
  String10 = string[10];
  String40 = string[40];
  String64 = string[64];
  String80 = string[80];
  MenuItem = record
               Row, Col, Len : Byte;
             end;
  VideoWord = record
                Ch : Char; Attr : Byte;
              end;
  ScreenType = array[1..50, 1..80] of VideoWord; {50 rows * 80 columns}
const
  {screen messages}
  ProgName        : string[34] = 'SUPER MACS: Macro Processor 1.30';
  Copyright       : string[47] =
    'Copyright (c) 1987, 1992 by TurboPower Software';
  NoCanPop        : string[41] = 'Unable to install SUPER MACS. Aborting...';
  MainMenuString  : string[78] =
    '  Edit  Delete  Read  Write  List  Status  Configure  Unload  cApture  Print  ';
  TableFull       : string[23] = 'Macro table overflow...';
  OutOfMemory     : string[22] = 'Insufficient memory...';
  MacroSaved      : string[14] = 'Macro saved...';
  MacroNotFound   : string[18] = 'Macro not found...';
  MacroDeleted    : string[16] = 'Macro deleted...';
  NoneDeleted     : string[20] = 'No macros deleted...';
  NoMacros        : string[20] = 'No macros defined...';
  MacroFileMsg    : string[12] = 'Macro file: ';
  MacroNameTitle  : string[12] = ' Macro Name ';
  MacroListTitle  : string[16] = ' Defined Macros ';
  NormalCharMsg   : string[43] = 'Are you sure you want to redefine this key?';
  ConfirmMsg      : string[30] = 'Already a macro key. Redefine?';
  MainMenuMsg     : string[09] = 'Main menu';
  BeginMacroMsg   : string[11] = 'Begin macro';
  EndMacroMsg     : string[09] = 'End macro';
  ScreenReaderMsg : string[13] = 'Screen reader';

  {colors}
  BrightColor  = $0F;
  DimColor     = $0E;
  ReverseColor = $1F;
  BrightMono   = $0F;
  DimMono      = $07;
  ReverseMono  = $70;

  SmacsColors : ColorSet = (
    TextColor       : $0E; TextMono       : $0F;
    CtrlColor       : $0E; CtrlMono       : $0F;
    FrameColor      : $0E; FrameMono      : $07;
    HeaderColor     : $0F; HeaderMono     : $0F;
    ShadowColor     : $00; ShadowMono     : $00;
    HighlightColor  : $00; HighlightMono  : $00;
    PromptColor     : $0E; PromptMono     : $07;
    SelPromptColor  : $0E; SelPromptMono  : $07;
    ProPromptColor  : $0E; ProPromptMono  : $07;
    FieldColor      : $0E; FieldMono      : $07;
    SelFieldColor   : $0E; SelFieldMono   : $0F;
    ProFieldColor   : $0E; ProFieldMono   : $07
  );

  {constants that determine the location of the main menu}
  MenuTop = 1;
  MenuMid = 2;
  MenuBot = 3;
  MenuLeft = 1;
  MenuRight = 80;

  {constants that determine the location of message boxes}
  MsgTop = 6;
  MsgBot = 8;
  MsgLeft = 5;
  MsgRight = 76;

  {coordinates for macro editor's window}
  EdTop = 4;
  EdLeft = MenuLeft;
  EdRight = MenuRight;

  {coordinates for configuration menu window}
  ConfigTop = 4;
  {$IFDEF UseEnhKbd}
    ConfigBot = 14;
  {$ELSE}
    ConfigBot = 13;
  {$ENDIF}
  ConfigLeft = 19;
  ConfigRight = 80;

  {coordinates for macro list window}
  ListTop = 4;
  ListLeft = 5;
  ListRight = 76;
  ListBot : Byte = 0;

  {external functions}
  LoadFile = 0;
  ExecMacroByKey = 1;
  ExecMacroByAddr = 2;
  GetMacroState = 3;
  SetMacroState = 4;
  MacroDefinedCheck = 5;
  DefineMacroFunc = 6;
  UnloadProgram = 7;

  {miscellaneous}
  NullString : string[1] = '';
  InPopupMenu : Boolean = False; {to prevent conflicts}
  InhibitRestore : Boolean = False;
  WholeScreenSaved : Boolean = False;
  AbortKey = $0300;          {returned when ^@ pressed}
  Esc = #27;
  Enter = ^M;
  ReaderStackSize = 2048;
  RecordStackSize = 512;
var
  Reverse,                   {reverse video attribute}
  Bright,                    {bright video attribute}
  Dim,                       {dim video attribute}
  TempMacroKey : Word;       {temporary macro key}
  TempMacroName : String80;  {temporary macro name}
  SaveMacrosAreOn : Boolean; {saved value of MacrosAreOn}
  MainHotKeyString : string[40];
  FName : String64;
  RowsToSave : Word;

  {screen buffers}
  MainBufPtr,
  MessageBufPtr,
  ConfigBufPtr,
  ListBufPtr,
  EditorBufPtr : Pointer;
  ScreenBufPtr : ^ScreenType absolute MainBufPtr;

  {stacks for popups -- main popup uses the regular stack}
  ReaderStack : array[1..ReaderStackSize] of Byte;
  RecordingOnStack : array[1..RecordStackSize] of Byte;
  RecordingOffStack : array[1..RecordStackSize] of Byte;

  {menus}
const
  MainItems = 10;
type
  MainMenuType = record
                   Last, Current : Word;
                   Choices : string[MainItems];
                   MenuItems : array[1..MainItems] of MenuItem;
                 end;
const
  MainMenu : MainMenuType = (
    Last : 1;
    Current : 1;
    Choices : 'EDRWLSCUAP';
    MenuItems :
    ( (Row : MenuMid; Col : 04; Len : 4), {Edit}
      (Row : MenuMid; Col : 10; Len : 6), {Delete}
      (Row : MenuMid; Col : 18; Len : 4), {Read}
      (Row : MenuMid; Col : 24; Len : 5), {Write}
      (Row : MenuMid; Col : 31; Len : 4), {List}
      (Row : MenuMid; Col : 37; Len : 6), {Save}
      (Row : MenuMid; Col : 45; Len : 9), {Configure}
      (Row : MenuMid; Col : 56; Len : 6), {Unload}
      (Row : MenuMid; Col : 64; Len : 7), {cApture}
      (Row : MenuMid; Col : 73; Len : 5)) {Print}
    );

  LastConfigChoice : Word = 1;
  ConfigChoice : Word = 1;
  {$IFDEF UseEnhKbd}
    ConfigItems = 9;
  {$ELSE}
    ConfigItems = 8;
  {$ENDIF}

var
  ListChoice : Word;
  LastListChoice : Word;
  ListItems : Word;
  ListItemAtTop : Word;
  MaxListItems : Word;

  {$L OPSCAN.OBJ}

  {$F+}
  function GetScanCodeName(B : Byte) : StringPtr; external;
    {-Return a pointer to a text string representing scan code B}
  {$F-}

  procedure SetAttributes;
    {-Set video attribute variables based on the current video mode}
  begin
    {set video attributes}
    Bright := ColorMono(BrightColor, BrightMono);
    Dim := ColorMono(DimColor, DimMono);
    Reverse := ColorMono(ReverseColor, ReverseMono);
  end;

  function Decimal(L : LongInt; Width : Byte) : String10;
    {-Return a string representing a decimal number}
  var
    S : String10;
  begin
    Str(L:Width, S);
    Decimal := S;
  end;

  procedure PromptBox(var Title : string; TopRow, BotRow,
                      LeftCol, RightCol : Byte);
    {-Draw a prompt box}
  var
    Blank : String80;
    BLen : Byte absolute Blank;
    Row : Word;
  begin
    if not(InTextMode and (ScreenWidth = 80)) then
      Exit;

    {save the screen if it hasn't been done yet}
    if not WholeScreenSaved then begin
      if SaveWindow(1, 1, 80, MinWord(RowsToSave, ScreenHeight), False, MainBufPtr) then
        {won't fail} ;
      WholeScreenSaved := True;
    end;

    {draw a box around the window}
    FrameWindow(LeftCol, TopRow, RightCol, BotRow, Dim, Bright, Title);

    {clear the inside of the window}
    BLen := Pred(RightCol-LeftCol);
    FillChar(Blank[1], BLen, ' ');
    for Row := Succ(TopRow) to Pred(BotRow) do
      FastWrite(Blank, Row, Succ(LeftCol), Dim);
  end;

  procedure Message(var Title, Msg : string);
    {-Display a message box}
  begin
    if not(InTextMode and (ScreenWidth = 80)) then
      Exit;
    PromptBox(Title, MsgTop, MsgBot, MsgLeft, MsgRight);
    FastWrite(Msg, Pred(MsgBot), MsgLeft+2, Dim);
  end;

  procedure RestoreMessageWindow;
    {-Restore the screen beneath the message window}
  begin
    if InhibitRestore then
      Exit;

    {restore the message window using the main screen buffer}
    RestoreWindow(1, MsgTop, 80, MsgBot, False, MessageBufPtr);
  end;

  function MessageCharPrim(var Title, Msg : string; Valid : CharSet) : Word;
    {-Display a message box, wait for a key, and return it}
  var
    SaveState : Boolean;
    I : Word;
    Ch : Char absolute I;
    SaveXY, SaveSL : Word;
  begin
    if not(InTextMode and (ScreenWidth = 80)) then
      Exit;
    Message(Title, Msg);
    SaveState := MacrosAreOn;
    MacrosOff;

    GetCursorState(SaveXY, SaveSL);
    GotoXYabs(MsgLeft+2+Length(Msg), Pred(MsgBot));
    NormalCursor;

    repeat
      I := ReadKeyWord;
    until ((Valid = []) or (Ch in Valid));
    MessageCharPrim := I;

    RestoreCursorState(SaveXY, SaveSL);

    MacrosAreOn := SaveState;
    RestoreMessageWindow;
  end;

  function MessageChar(var Title, Msg : string) : Word;
    {-Display a message box, wait for a key, and return it}
  begin
    MessageChar := MessageCharPrim(Title, Msg, []);
  end;

  function MessageYesOrNo(var Title : string; Msg : String80; Default : Char) : Boolean;
    {-Display a message box and ask a yes-no question. Returns true for yes.}
  var
    SaveState : Boolean;
    I : Word;
    Ch : Char absolute I;
  begin
    Msg := Msg+' ['+Default+'] ';
    I := MessageCharPrim(NullString, Msg, [^M, ^[, 'Y', 'y', 'N', 'n']);
    case Ch of
      'Y', 'y' :
        MessageYesOrNo := True;
      'N', 'n', ^[ :
        MessageYesOrNo := False;
      else
        MessageYesOrNo := Upcase(Default) = 'Y';
    end;
  end;

  procedure MessageDelay(Msg : String80; N : Word);
    {-Display a message box, and delay for a count of N}
  begin
    Message(NullString, Msg);
    Delay(N);
    RestoreMessageWindow;
  end;

  function MessageString(var Title, Msg, Reply : string;
                         ForceUpper : Boolean) : Boolean;
    {-Display a message box, and get the user's reply to the prompt}
  var
    MaxLen : Word;
    SLE : SimpleLineEditor;
  begin
    {show message box}
    Message(Title, NullString);

    {calculate maximum length of string}
    MaxLen := (MsgRight-Succ(MsgLeft))-Succ(Length(Msg))-2;
    if Length(Msg) = 0 then
      Inc(MaxLen);

    {read the string and restore the window}
    with SLE do begin
      Init(SmacsColors);
      if ForceUpper then
        seOptionsOn(seForceUpper);
      ReadString(Msg, Pred(MsgBot), MsgLeft+2, MaxLen, MaxLen, Reply);
      MessageString := (GetLastCommand <> ccQuit);
      Done;
    end;
    RestoreMessageWindow;
  end;

  procedure ErrorMessage(Msg : String80);
    {-Display an error message and wait for a keystroke}
  var
    Junk : Word;
  begin
    Msg := Msg+'. Press any key...';
    Junk := MessageChar(NullString, Msg);
  end;

  procedure PrintFile;
    {-Prompts for a file name, then submits the file to the MS or PC-DOS
      concurrent print utility. Works only with DOS 3.0 or greater.}
  const
    Title : string[12] = ' Print File ';
    Msg : string[15] = 'File to print: ';
  begin
    {can't do anything without PRINT.COM}
    if not PrintInstalled then begin
      if DosVersion < $300 then
        ErrorMessage('DOS 3.0 or higher is required')
      else
        ErrorMessage('PRINT.COM not installed');
      Exit;
    end;

    {get file to print}
    if MessageString(Title, Msg, FName, False) then
      if (FName <> '') then
        {submit the filename to PRINT.COM}
        case SubmitPrintFile(FName) of
          2..5,
          8..9,
          12, 15 : ErrorMessage('Unable to print the file');
        end;
  end;

  procedure EditMacro;
    {-Edit a macro}
  const
    Title : string[12] = ' Edit Macro ';
    GetKey : string[28] = 'Macro to edit (^@ to cancel)';
    BlankMacro : array[0..1] of Word = (0, EndOfMacro);
  var
    N, Rows : Word;
    SP : StringPointer;
    MP : MacroRecPtr;
    KeyOK, Modified : Boolean;
  begin
    {get the key}
    InhibitRestore := True;
    TempMacroKey := MessageChar(Title, GetKey);
    InhibitRestore := False;

    {check for ^@}
    if TempMacroKey = AbortKey then begin
      RestoreMessageWindow;
      Exit;
    end;

    {locate the macro, if it exists}
    N := FindMacroIndex(TempMacroKey);
    if N = 0 then
      MP := @BlankMacro
    else
      MP := MacroPointers[N];

    {warn about assigning new macros to regular keys}
    KeyOK := (Lo(TempMacroKey) < 32) or (Lo(TempMacroKey) > 126);
    if (N = 0) and not KeyOK then
      if not MessageYesOrNo(NullString, NormalCharMsg, 'N') then
        Exit;

    {get the macro name}
    if (N = 0) then
      SP := @NullString
    else begin
      SP := MacroNames[N];
      if (SP = nil) then
        SP := @NullString;
    end;
    TempMacroName := SP^;

    {edit the macro name}
    if MessageString(MacroNameTitle, NullString, TempMacroName, False) then begin
      {number of rows in editing window}
      Rows := MinWord(RowsToSave, ScreenHeight);

      {edit the keystrokes}
      MP := EditKeys(TempMacroKey, MP^, EdLeft, EdTop, EdRight, Rows,
                     Dim, Bright, Modified);

      {changes to just the name count as modifications}
      if not Modified then
        Modified := (TempMacroName <> SP^);

      {restore the window}
      RestoreWindow(EdLeft, EdTop, EdRight, Rows, False, EditorBufPtr);

      {see if the macro was just deleted}
      if (N <> 0) and (TempMacro.NumKeys = 0) then begin
        if DeallocateMacro(TempMacroKey) then
          {nothing} ;
        MessageDelay(MacroDeleted, 1000);
      end
      else
        {allocate space for and define the macro if it was changed}
        if Modified then
          case AllocateMacro(TempMacroKey, MP^, TempMacroName) of
            1..2 : ErrorMessage(OutOfMemory);
          end;
    end;
  end;

  procedure DeleteMacros;
    {-Delete one or all macros}
  const
    Title : string[15] = ' Delete Macros ';
    DeleteAll : string[18] = 'Delete all macros?';
    Warning : string[13] = 'Are you sure?';
    PressKey : string[27] = 'Macro to delete (^@ aborts)';
  var
    All : Boolean;
    MacroToDelete : Word;
  begin
    if MacroCount = 0 then begin
      MessageDelay(NoMacros, 1000);
      Exit;
    end;

    {ask user if all macros are to be cleared}
    InhibitRestore := True;
    All := MessageYesOrNo(Title, DeleteAll, 'N');

    if All then begin
      {ask for confirmation before clearing them}
      All := MessageYesOrNo(NullString, Warning, 'N');
      InhibitRestore := False;

      if All then begin
        ClearMacros;
        MessageDelay('All macros deleted...', 1000)
      end
      else
        MessageDelay(NoneDeleted, 1000);
    end
    else begin
      MacroToDelete := MessageChar(Title, PressKey);
      InhibitRestore := False;

      {check for ^@}
      if MacroToDelete = AbortKey then
        MessageDelay(NoneDeleted, 1000)
      else
        if DeallocateMacro(MacroToDelete) then
          MessageDelay(MacroDeleted, 1000)
        else
          MessageDelay(MacroNotFound, 1000);
    end;
  end;

  procedure LoadMacros(Prompt : Boolean; var FName : string);
    {-Loads a macro file. If Prompt is true, it prompts for the file to
      load. Otherwise it loads the file passed in FName.}
  const
    Title : string[17] = ' Read Macro File ';
    ReadError : string[29] = 'Error reading from macro file';
    OverwriteMsg : string[26] = 'Overwrite existing macros?';
  var
    Merge : Boolean;
  begin
    {prompt for file name if desired}
    if Prompt and (MacroCount <> 0) then begin
      InhibitRestore := True;
      Merge := not MessageYesOrNo(NullString, OverwriteMsg, 'N');
      InhibitRestore := False;
    end
    else
      Merge := False;

    {prompt for file name if desired}
    if Prompt and not MessageString(Title, MacroFileMsg, FName, False) then
      Exit;

    {exit if we still don't have a filename}
    if (FName = '') then
      Exit;

    {Read macro file}
    case ReadMacroFile(FName, Merge) of
      $00 : {OK} ;
      $FE : ErrorMessage(TableFull);
      $FF : ErrorMessage('Invalid macro file format.');
      else  ErrorMessage(ReadError);
    end;
  end;

  procedure WriteMacros;
    {-Write a macro file}
  const
    Title : string[18] = ' Write Macro File ';
    WriteError : string[27] = 'Error writing to macro file';
    OverwriteMsg : string[26] = 'File exists. Overwrite it?';
  begin
    {get file name}
    if not MessageString(Title, MacroFileMsg, FName, False) then
      Exit;

    {exit if we don't have a filename}
    if (FName = '') then
      Exit;

    {check for existence}
    if ExistFile(FName) then
      if not MessageYesOrNo(NullString, OverwriteMsg, 'N') then
        Exit;

    {write the macros to a file}
    if WriteMacroFile(FName) <> 0 then
      ErrorMessage(WriteError);
  end;

  function UnloadTSR : Boolean;
    {-If safe, give option to unload the TSR}
  const
    Msg : string[38] = 'Are you sure you want to unload SMACS?';
    UnloadingMsg : string[18] = 'Unloading SMACS...';
  var
    Unload : Boolean;
  begin
    {get confirmation}
    InhibitRestore := True;
    Unload := MessageYesOrNo(NullString, Msg, 'N');
    InhibitRestore := False;

    if not Unload then
      RestoreMessageWindow
    {try to disable the TSR}
    else if DisableTSR then begin
      {$IFDEF UseEnhKbd}
        {deinstall ENHKBD's interrupt handlers!}
        RestoreKbdVectors;
      {$ENDIF}
      MessageDelay(UnloadingMsg, 2000);
    end
    else begin
      Unload := False;
      ErrorMessage('Not safe to unload SMACS right now');
    end;

    UnloadTSR := Unload;
  end;

  procedure CaptureScreen;
    {-Writes the saved screen to an ASCII file.}
  const
    Title : string[16] = ' Capture Screen ';
    Msg : string[15] = 'File to write: ';
  var
    FileToSave : Text;
    FileHandle : Word absolute FileToSave;
    Rows, Row, Col, IOstat : Word;
    Ch : Char;
  label
    RestartPoint;
  begin
RestartPoint:
    {Prompt for filename}
    if not MessageString(Title, Msg, FName, False) then
      Exit;

    {exit if we don't have a filename}
    if (FName = '') then
      Exit;

    {open the file}
    Assign(FileToSave, FName);
    Rewrite(FileToSave);
    IOstat := IoResult;

    if IOstat = 0 then begin
      {make sure this isn't going to the screen}
      if HandleIsConsole(FileHandle) then begin
        ErrorMessage('Cannot write screen contents to console');
        Close(FileToSave);
        IOstat := IoResult;
        goto RestartPoint;
      end;

      {number of rows}
      Rows := MinWord(RowsToSave, ScreenHeight);

      {write the contents of the screen buffer to the file}
      for Row := 1 to Rows do begin
        for Col := 1 to 80 do
          if IOstat = 0 then begin
            Write(FileToSave, ScreenBufPtr^[Row, Col].Ch);
            IOstat := IoResult;
          end;
        if IOstat = 0 then begin
          WriteLn(FileToSave);
          IOstat := IoResult;
        end;
      end;
      Close(FileToSave);
      Inc(IOstat, IoResult);
    end;

    if IOstat <> 0 then
      ErrorMessage('Error writing file.');
  end;

  procedure ShowStatus;
    {-Show number of macros defined, memory available}
  begin
    {show status}
    ErrorMessage(Decimal(MacroCount, 0)+' macro(s) defined, '+
      Decimal(MemAvail, 0)+' bytes available');
  end;

  procedure NearCall(ProcOfs : Word);
    {-ProcOfs is the offset of a routine to be called near.}
  inline(
    $5B/                     {pop bx}
    $FF/$D3);                {call bx}

  function GetMenuChoice(Orientation : OrientationType;
                         AllowWrap : Boolean;
                         LastItem : Word;
                         var CurrentChoice : Word;
                         var Choices : string;
                         UpdateMenuOfs : Word) : Char;
    {-Get a choice from an SMACS menu}
  var
    ChWord : Word;
    Ch : Char absolute ChWord;
    I : Word;
  begin
    repeat
      {update the screen}
      NearCall(UpdateMenuOfs);

      {get choice}
      ChWord := ReadKeyWord;
      Ch := Upcase(Ch);
      if Ch = #0 then begin
        case Hi(ChWord) of
          {ignore everything but arrow keys, Home, and End}
          72 :               {up arrow}
            if Orientation = UpDown then
              if (CurrentChoice > 1) then
                Dec(CurrentChoice)
              else
                if AllowWrap then
                  CurrentChoice := LastItem;
          80 :               {down arrow}
            if Orientation = UpDown then
              if (CurrentChoice < LastItem) then
                Inc(CurrentChoice)
              else
                if AllowWrap then
                  CurrentChoice := 1;
          75 :               {left arrow}
            if Orientation = LeftRight then
              if (CurrentChoice > 1) then
                Dec(CurrentChoice)
              else
                if AllowWrap then
                  CurrentChoice := LastItem;
          77 :               {right arrow}
            if Orientation = LeftRight then
              if (CurrentChoice < LastItem) then
                Inc(CurrentChoice)
              else
                if AllowWrap then
                  CurrentChoice := 1;
          71 :               {Home}
            CurrentChoice := 1;
          79 :               {End}
            CurrentChoice := LastItem;
        else Ch := #0;
        end;
      end
      else begin
        {normal character}
        I := Pos(Ch, Choices);
        if I <> 0 then begin
          Ch := Enter;
          CurrentChoice := I;
        end;
      end;
    until (Ch = Esc) or (Ch = Enter);

    if Ch = Esc then
      GetMenuChoice := Esc
    else begin
      GetMenuChoice := Choices[CurrentChoice];

      {update the screen one last time}
      NearCall(UpdateMenuOfs);
    end;
  end;

  procedure SaveConfiguration;
    {-Save current configuration}
  label
    ExitPoint;
  const
    Title : string[36] = ' Enter full name of executable file ';
    Prompt : string[11] = 'File name: ';
  var
    C : Cloner;
    FName : String64;
    ShortName : string[12];
    Found : Boolean;
    I : LongInt;
  begin
    {find executable file}
    FName := ExeName;
    repeat
      {locate SMACS.EXE}
      Found := ExistOnPath(FName, FName);
      ShortName := JustFileName(FName);

      if not Found then begin
        InhibitRestore := True;
        ErrorMessage(ShortName+' not found');
        InhibitRestore := False;

        {prompt for full pathname}
        if not MessageString(Title, Prompt, FName, True) then
          Exit;
      end;
    until Found;

    {initialize the Cloner}
    I := MaxAvail;
    if I > 65521 then
      I := 65521;
    if not C.InitCustom(FName, UpdateDate, I) then begin
      ErrorMessage(OutOfMemory);
      Exit;
    end;

    {find configuration data}
    if not C.FindDefaultsEnd(ModuleName, SizeOf(ModuleName), 0) then begin
      MessageDelay('Configuration data not found. Operation failed.', 1000);
      goto ExitPoint;
    end;

    {store new defaults}
    C.StoreDefaults(C.GetPos, ModuleName, Ofs(CfgEnd)-Ofs(ModuleName));

    {check for error}
    if C.GetLastError <> 0 then
      ErrorMessage('Error: Unable to write changes to '+ShortName)
    else
      MessageDelay('Configuration data saved...', 1000);

ExitPoint:
    {release buffer}
    C.Done;
  end;

  function HotKeyToString(Key : Word) : String40;
    {-Given a hotkey, return a string representing it}
  const
    RtShift = $01;
    LtShift = $02;
    Ctrl    = $04;
    Alt     = $08;
  var
    St : String40;

    procedure Add(Mask : Byte; S : String10);
    begin
      if Hi(Key) and Mask <> 0 then begin
        S := '<'+S+'>';
        if St = '' then
          St := S
        else
          St := St+S;
      end;
    end;

  begin
    St := '';
    Add(Ctrl,    'Ctrl');
    Add(Alt,     'Alt');
    Add(LtShift, 'LtShift');
    Add(RtShift, 'RtShift');

    St := St+'<';
    St := St+GetScanCodeName(Lo(Key))^;
    HotKeyToString := St+'>';
  end;

  procedure UpdateConfigMenu;
    {-Update the highlight bar in the configuration menu}
  var
    Cols : Word;
  begin
    Cols := Pred(ConfigRight-ConfigLeft);
    ChangeAttribute(Cols, ConfigTop+LastConfigChoice, Succ(ConfigLeft), Dim);
    ChangeAttribute(Cols, ConfigTop+ConfigChoice, Succ(ConfigLeft), Reverse);
    LastConfigChoice := ConfigChoice;
  end;

  procedure DrawConfigMenu;
    {-Draw the configuration menu}
  const
    Title : string[20] = ' Configuration Menu ';
    OnOrOff : array[Boolean] of string[3] = ('OFF', 'ON ');
  var
    FirstCol,
    SecondCol, Row : Byte;
  begin
    {draw the menu box}
    PromptBox(Title, ConfigTop, ConfigBot, ConfigLeft, ConfigRight);

    FirstCol := ConfigLeft+2;
    SecondCol := FirstCol+20;
    Row := Succ(ConfigTop);

    FastWrite(MainMenuMsg, Row, FirstCol, Dim);
    FastWrite(HotKeyToString(MainHotKey), Row, SecondCol, Dim);
    Inc(Row);

    FastWrite(BeginMacroMsg, Row, FirstCol, Dim);
    FastWrite(HotKeyToString(RecordOnHotKey), Row, SecondCol, Dim);
    Inc(Row);

    FastWrite(EndMacroMsg, Row, FirstCol, Dim);
    FastWrite(HotKeyToString(RecordOffHotKey), Row, SecondCol, Dim);
    Inc(Row);

    FastWrite(ScreenReaderMsg, Row, FirstCol, Dim);
    FastWrite(HotKeyToString(ReaderHotKey), Row, SecondCol, Dim);
    Inc(Row);

    FastWrite('Macro memory', Row, FirstCol, Dim);
    FastWrite(Decimal(MacroHeap, 0), Row, SecondCol, Dim);
    Inc(Row);

    FastWrite('Playback delay', Row, FirstCol, Dim);
    FastWrite(Decimal(OurPlaybackDelay, 0), Row, SecondCol, Dim);
    Inc(Row);

    FastWrite('Macro playback', Row, FirstCol, Dim);
    FastWrite(OnOrOff[SaveMacrosAreOn], Row, SecondCol, Dim);
    Inc(Row);

    {$IFDEF UseEnhKbd}
      FastWrite('Remap keyboard', Row, FirstCol, Dim);
      FastWrite(OnOrOff[UseEnhancedCalls], Row, SecondCol, Dim);
      Inc(Row);
    {$ENDIF}

    FastWrite('Save configuration', Row, FirstCol, Dim);
  end;

  procedure ResetHotKey(var HotKey : Word; Name : String80);
    {-Change a hotkey}
  const
    Msg : string[33] = 'Press new hot key or ^@ to cancel';
    {table converts ^PgDn to PgDn, etc}
    ScanTable : array[0..165] of Byte = (
      {     00  01  02  03  04  05  06  07  08  09  0A  0B  0C  0D  0E  0F}
      {00} $46, $01, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0E, $0F,
      {01} $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1A, $1B, $1C, $00, $1E, $1F,
      {02} $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $00, $2B, $2C, $2D, $2E, $2F,
      {03} $30, $31, $32, $33, $34, $35, $00, $37, $00, $00, $00, $3B, $3C, $3D, $3E, $3F,
      {04} $40, $41, $42, $43, $44, $00, $00, $47, $48, $49, $4A, $4B, $4C, $4D, $4E, $4F,
      {05} $50, $51, $52, $53, $3B, $3C, $3D, $3E, $3F, $40, $41, $42, $43, $44, $3B, $3C,
      {06} $3D, $3E, $3F, $40, $41, $42, $43, $44, $3B, $3C, $3D, $3E, $3F, $40, $41, $42,
      {07} $43, $44, $37, $4B, $4D, $4F, $51, $47, $02, $03, $04, $05, $06, $07, $08, $09,
      {08} $0A, $0B, $0C, $0D, $49, $57, $58, $57, $58, $57, $58, $57, $58, $48, $4A, $4C,
      {09} $4E, $50, $52, $53, $0F, $00, $00, $47, $48, $49, $00, $4B, $4C, $4D, $00, $4F,
      {0A} $50, $51, $52, $53, $00, $0F);
  var
    SaveKeyFlags,
    ScanCode,
    NewHotKey : Word;
    KeyFlags : Byte absolute $40 : $17;
    Done : Boolean;
    {$IFDEF UseEnhKbd}
    SaveEnhanced : Boolean;
    {$ENDIF}
  begin
    {display a prompt box}
    Name := ' New hot key for '+Name+' ';
    Message(Name, Msg);

    {$IFDEF UseEnhKbd}
      {force keyboard remapping on}
      SaveEnhanced := EnableEnhanced;
      EnableEnhanced := True;
    {$ENDIF}

    repeat
      ScanCode := ReadKeyWord;
      SaveKeyFlags := Word(KeyFlags) and $0F; {.$0F = 00001111b}

      {don't change HotKey unless valid hot key or AbortKey is pressed}
      if (ScanCode <> AbortKey) and (SaveKeyFlags <> 0) then begin
        {translate extended scan codes}
        if (Lo(ScanCode) = 0) or (Lo(ScanCode) = $E0) then begin
          ScanCode := Hi(ScanCode);
          if ScanCode > 165 then
            ScanCode := 0
          else
            ScanCode := ScanTable[ScanCode];
        end
        else
          {deal with special cases}
          case Hi(ScanCode) of
            $01,             {Esc}
            $0E,             {BkSp}
            $0F,             {Tab}
            $1C : {ok} ;     {Enter}
          else
            {use the scan code returned by CharToMacro}
            ScanCode := Hi(CharToMacro(Chr(Lo(ScanCode))));
          end;

        {merge saved shift flags and scan code}
        NewHotKey := (SaveKeyFlags shl 8)+ScanCode;
        if ChangeHotKey(HotKey, NewHotKey) then begin
          HotKey := NewHotKey;
          Done := True;
        end
        else begin
          RingBell;
          Done := False;
        end;
      end
      else
        Done := (ScanCode = AbortKey);
    until Done;

    {$IFDEF UseEnhKbd}
      {restore keyboard remapping state}
      EnableEnhanced := SaveEnhanced;
    {$ENDIF}

    {restore window}
    RestoreMessageWindow;
  end;

  procedure ConfigureSmacs;
    {-Display and allow changes to default settings}
  const
    MemoryMsg : string[45] = 'Memory to set aside for macros (500..65000): ';
    DelayMsg : string[45] = 'Delay factor during macro playback (0..100): ';
  var
    Ch : Char;
    Restore : Boolean;

    procedure ChangeWord(var W : Word; Low, High, Width : Word; var Msg : string);
      {-Change a word variable}
    var
      SLE : SimpleLineEditor;
      S : String[80];
      Finished : Boolean;
      L : LongInt;
    begin
      Message(NullString, NullString);
      with SLE do begin
        Init(SmacsColors);
        L := W;
        repeat
          S := Long2Str(L);
          ReadString(Msg, Pred(MsgBot), MsgLeft+2, Width, Width, S);
          if GetLastCommand = ccQuit then
            Finished := True
          else if not Str2Long(S, L) then begin
            {Invalid number}
            RingBell;
            L := W;
          end
          else if (L < Low) or (L > High) then
            RingBell
          else
            Finished := True;
        until Finished;
        W := L;
        Done;                       {!!.13}
      end;
      RestoreMessageWindow;
    end;

  begin
    repeat
      {draw the menu}
      DrawConfigMenu;

      {get menu choice}
      Ch := GetMenuChoice(UpDown, True, ConfigItems, ConfigChoice, NullString,
        Ofs(UpdateConfigMenu));

      if (Ch <> Esc) and (ConfigChoice = 7) then
        Restore := False
      else
        Restore := True;

      {restore the window using the main screen buffer}
      if Restore then
        RestoreWindow(1, ConfigTop, 80, ConfigBot, False, ConfigBufPtr);

      {exit now if Esc was pressed}
      if Ch = Esc then
        Exit;

      case ConfigChoice of
        1 :                  {set main hot key}
          ResetHotKey(MainHotKey, MainMenuMsg);
        2 :                  {set begin macro hot key}
          ResetHotKey(RecordOnHotKey, BeginMacroMsg);
        3 :                  {set end macro hot key}
          ResetHotKey(RecordOffHotKey, EndMacroMsg);
        4 :                  {set screen reader hot key}
          ResetHotKey(ReaderHotKey, ScreenReaderMsg);
        5 :                  {set MacroHeap}
          ChangeWord(MacroHeap, 500, 65000, 5, MemoryMsg);
        6 :                  {Set playback delay}
          begin
            ChangeWord(OurPlaybackDelay, 0, 100, 3, DelayMsg);
            PlaybackDelay := OurPlaybackDelay;
          end;
        7 :                  {toggle macro playback}
          SaveMacrosAreOn := not SaveMacrosAreOn;

        {$IFDEF UseEnhKbd}
        8 :                  {toggle keyboard remapping}
          begin
            UseEnhancedCalls := not UseEnhancedCalls;
            EnableEnhanced := UseEnhancedCalls;
          end;
        9 :                  {save current configuration}
          SaveConfiguration;
        {$ELSE}
        8 :                {save current configuration}
          SaveConfiguration;
        {$ENDIF}
      end;
    until False;
  end;

  procedure DrawMacro(N : Word; Row : Byte);
    {-Draw the N'th defined macro}
  const
    Unnamed : string[9] = '(unnamed)';
  var
    I, J, Key : Word;
    PN : ^String80;
    S : String80;
    SLen : Byte absolute S;
    KeyString : string[12];
    Dummy : Boolean;
  begin
    J := 0;
    I := 0;

    {find the N'th macro}
    repeat
      Inc(I);
      if DefinedKeys[I] <> EndOfMacro then
        Inc(J);
    until (J = N);

    {get the key and the name (if any)}
    Key := DefinedKeys[I];
    PN := MacroNames[I];

    {build the line}
    KeyToString(Key, KeyString, Dummy);
    SLen := Pred(ListRight-ListLeft);
    FillChar(S[1], SLen, ' ');
    Move(KeyString[1], S[2], Length(KeyString));
    if PN = nil then
      Move(Unnamed[1], S[13], Length(Unnamed))
    else begin
      {calculate bytes to move}
      I := Length(PN^);
      J := Pred(ListRight-ListLeft)-13;
      if I > J then
        I := J;

      {move the string}
      Move(PN^[1], S[13], I);
    end;

    {display the line}
    FastWrite(S, Row, Succ(ListLeft), Dim);
  end;

  procedure DrawList;
    {-Draw a window full of macros}
  var
    I, ItemAtBottom, Row : Word;
  begin
    ItemAtBottom := ListItemAtTop+Pred(MaxListItems);
    Row := Succ(ListTop);
    for I := ListItemAtTop to ItemAtBottom do begin
      DrawMacro(I, Row);
      Inc(Row);
    end;
  end;

  procedure UpdateMacroList;
    {-Update the highlight bar in the configuration menu}
  var
    ItemAtBottom : Word;
  begin
    {see if there can be a need to scroll}
    if ListItems > MaxListItems then begin
      ItemAtBottom := ListItemAtTop+Pred(MaxListItems);
      if (ListChoice = 1) or (ListChoice = ListItems) then begin
        {we're at top or bottom of list}
        if ListChoice = 1 then
          ListItemAtTop := 1
        else
          ListItemAtTop := ListItems-Pred(MaxListItems);

        {redraw whole thing}
        DrawList;
      end
      else begin
        if ListChoice > LastListChoice then begin
          {scroll display}
          ScrollWindowUp(Succ(ListLeft), Succ(ListTop), Pred(ListRight),
            Pred(ListBot), 1);

          {display next entry}
          ListChoice := Succ(ItemAtBottom);
          DrawMacro(ListChoice, Pred(ListBot));
          Inc(ListItemAtTop);
        end
        else
          if ListChoice < LastListChoice then begin
            {scroll display}
            ScrollWindowDown(Succ(ListLeft), Succ(ListTop), Pred(ListRight),
              Pred(ListBot), 1);

            {display previous entry}
            ListChoice := Pred(ListItemAtTop);
            DrawMacro(ListChoice, Succ(ListTop));
            Dec(ListItemAtTop);
          end;
      end;
    end;

    LastListChoice := ListChoice;
  end;

  procedure ListMacros;
    {-List currently defined macros}
  var
    Ch : Char;
    Rows : Byte;
  begin
    {make sure there's something to display}
    ListItems := MacroCount;
    if ListItems = 0 then
      MessageDelay(NoMacros, 1000)
    else begin
      {always start at the top}
      ListChoice := 1;
      LastListChoice := 1;
      ListItemAtTop := 1;

      {calculate maximum number of macros that will be displayed at once}
      Rows := MinWord(RowsToSave, ScreenHeight);
      MaxListItems := Pred(Rows)-ListTop;
      if MaxListItems > ListItems then
        MaxListItems := ListItems;

      {draw the window border}
      ListBot := Succ(ListTop+MaxListItems);
      FrameWindow(ListLeft, ListTop, ListRight, ListBot, Dim, Bright,
        MacroListTitle);

      {draw the list}
      DrawList;

      {loop until Escape is pressed}
      repeat
        {get menu choice}
        Ch := GetMenuChoice(UpDown, False, ListItems, ListChoice, NullString,
          Ofs(UpdateMacroList));
      until (Ch = Esc);

      {restore the window using the main screen buffer}
      RestoreWindow(1, ListTop, 80, ListBot, False, ListBufPtr);
    end;
  end;

  procedure UpdateMainMenu;
    {-Update the highlight status of the menu entries}
  begin
    with MainMenu do begin
      with MenuItems[Last] do
        ChangeAttribute(Len+2, Row, Pred(Col), Dim);
      with MenuItems[Current] do
        ChangeAttribute(Len+2, Row, Pred(Col), Reverse);
      Last := Current;
    end;
  end;

  procedure MainMenuLoop;
    {-Display main menu and process selections}
  var
    Ch : Char;
    Done : Boolean;
  begin
    Done := False;

    {draw the main menu}
    PromptBox(NullString, MenuTop, MenuBot, MenuLeft, MenuRight);
    FastWrite(ProgName, MenuTop, (80-Length(ProgName)) shr 1, Reverse);
    FastWrite(MainMenuString, Succ(MenuTop), Succ(MenuLeft), Dim);

    repeat
      with MainMenu do
        {get menu choice}
        Ch := GetMenuChoice(LeftRight, True, MainItems, Current, Choices,
          Ofs(UpdateMainMenu));

      {process choice}
      case Ch of
        'E' : EditMacro;
        'D' : DeleteMacros;
        'R' : LoadMacros(True, FName);
        'W' : WriteMacros;
        'L' : ListMacros;
        'S' : ShowStatus;
        'C' : ConfigureSmacs;
        'U' : Done := UnloadTSR;
        'A' : CaptureScreen;
        'P' : PrintFile;
        Esc : Done := True;
      end;
    until Done;
  end;

  {$F+}
  procedure MainPop(var Regs : Registers);
    {-Main popup procedure in SMACS}
  var
    SaveXY,
    SaveSL : Word;           {for saving cursor position and shape}
  begin
    {re-initialize CRT}
    ReInitCrt;
    SetAttributes;
    if InTextMode and (ScreenWidth = 80) then begin
      {turn popups and macros off}
      PopupsOff;
      InPopupMenu := True;
      SaveMacrosAreOn := MacrosAreOn;
      MacrosOff;

      {save cursor state and hide it}
      GetCursorState(SaveXY, SaveSL);
      HiddenCursor;

      {save the screen}
      if SaveWindow(1, 1, 80, MinWord(RowsToSave, ScreenHeight), False, MainBufPtr) then
        {won't fail} ;
      WholeScreenSaved := True;

      {enter main menu}
      MainMenuLoop;

      {Restore the screen}
      RestoreWindow(1, 1, 80, MinWord(RowsToSave, ScreenHeight), False, MainBufPtr);
      WholeScreenSaved := False;
      RestoreCursorState(SaveXY, SaveSL);

      {reenable popups and macros}
      MacrosAreOn := SaveMacrosAreOn;
      InPopupMenu := False;
      PopupsOn;
    end
    else
      RingBell;
  end;
  {$F-}

  {$F+}
  procedure ReaderPop(var Regs : Registers);
    {-Pop up procedure which allows reading the screen}
  const
    Title : string[15] = ' Screen reader ';
    Msg : string[19] = 'Press key to define';
    CancelReadMsg : string[18] = 'Macro not saved...';
  var
    ScreenPtr : ^ScreenType;
    SaveXY, SaveSL : Word;   {for storing cursor position and shape}
    LineBuffer : string[81]; {for reading from the screen}
    LineLen : Byte absolute LineBuffer;
    CurRow, CurCol,          {current cursor coordinates}
    StartRow, StartCol,      {start of marked block}
    Row, Cols, I : Byte;
    MaxRow : Word;
    ChWord : Word;
    Ch : Char absolute ChWord;
    KeyOK,
    SaveMacrosAreOn,         {used to save status of MacrosAreOn}
    Highlight,               {true if initial point has been marked}
    WinSelected : Boolean;   {true after window was selected}
    NewRow : Word;

    procedure MarkBlock(TopRow, BotRow, LeftCol, RightCol : Byte);
      {-Mark the specified block}
    var
      Row, Cols : Word;
    begin
      Cols := Succ(RightCol-LeftCol);
      for Row := TopRow to BotRow do
        ChangeAttribute(Cols, Row, LeftCol, Reverse);
    end;

    procedure RestoreBlock(TopRow, BotRow, LeftCol, RightCol : Byte);
      {-Unmark the specified block}
    var
      Row, Cols : Word;
    begin
      Cols := Succ(RightCol-LeftCol);
      for Row := TopRow to BotRow do
        MoveScreen(ScreenBufPtr^[Row, LeftCol], ScreenPtr^[Row, LeftCol], Cols);
    end;

    procedure IncRow(N : Word);
      {-Move the cursor N rows down}
    var
      I : Word;
    begin
      for I := 1 to N do begin
        {make sure we don't go too far down}
        if CurRow = MaxRow then
          Exit;

        Inc(CurRow);
        if Highlight then
          if (CurRow > StartRow) and (CurCol >= StartCol) then
            MarkBlock(Pred(CurRow), CurRow, StartCol, CurCol);
      end;
    end;

    procedure DecRow(N : Integer);
      {-Move the cursor N rows up}
    var
      OldRow, I : Word;
    begin
      for I := 1 to N do begin
        {make sure we don't go too far up}
        if (CurRow = 1) or (Highlight and (CurRow = StartRow)) then
          Exit;

        OldRow := CurRow;
        Dec(CurRow);
        if Highlight then
          if (OldRow > StartRow) and (CurCol >= StartCol) then
            RestoreBlock(OldRow, OldRow, StartCol, CurCol);
      end;
    end;

    procedure IncCol(N : Word);
      {-Move the cursor N columns to the right}
    var
      I : Word;
    begin
      for I := 1 to N do begin
        {make sure we don't go too far right}
        if CurCol = ScreenWidth then
          Exit;

        Inc(CurCol);
        if Highlight then
          if (CurCol > StartCol) and (CurCol >= StartCol) then
            MarkBlock(StartRow, CurRow, Pred(CurCol), CurCol);
      end;
    end;

    procedure DecCol(N : Word);
      {-Move the cursor N columns to the left}
    var
      OldCol, I : Word;
    begin
      for I := 1 to N do begin
        {make sure we don't go too far left}
        if (CurCol = 1) or (Highlight and (CurCol = StartCol)) then
          Exit;

        OldCol := CurCol;
        Dec(CurCol);
        if Highlight then
          if (OldCol > StartCol) and (CurCol >= StartCol) then
            RestoreBlock(StartRow, CurRow, OldCol, OldCol);
      end;
    end;

    procedure TabRight;
      {-Moves the cursor to the next tab stop}
    var
      NewCol : Word;
    begin
      if CurCol < ScreenWidth then begin
        NewCol := Succ(Succ(Pred(CurCol) shr 3) shl 3); {shr 3 = div 8}
        IncCol(NewCol-CurCol);
      end;
    end;

    procedure TabLeft;
      {-Moves the cursor back to the last tab stop}
    var
      NewCol : Word;
    begin
      NewCol := CurCol;
      if (Pred(NewCol) and 7) = 0 then
        if NewCol > 8 then
          Dec(NewCol, 8)
        else
          NewCol := 1
      else
        NewCol := Succ(Pred(NewCol) and $F8);
      DecCol(CurCol-NewCol);
    end;

  begin
    {don't pop up if we're already on the screen}
    if InPopupMenu then begin
      RingBell;
      Exit;
    end;

    InPopupMenu := True;

    {re-initialize CRT}
    ReInitCrt;
    SetAttributes;
    MaxRow := MinWord(RowsToSave, ScreenHeight);

    if InTextMode and (ScreenWidth = 80) then begin

      {Can't interrupt this one}
      PopupsOff;

      {turn macros off in case one is in progress}
      SaveMacrosAreOn := MacrosAreOn;
      MacrosOff;

      {Store current window coordinates in case we interrupted MainPop}
      GetCursorState(SaveXY, SaveSL);

      {save the screen}
      if SaveWindow(1, 1, 80, MaxRow, False, MainBufPtr) then
        {won't fail} ;
      WholeScreenSaved := True;
      ScreenPtr := Ptr(VideoSegment, 0);

      WinSelected := False;  {Window is not selected now}
      Highlight := False;
      CurCol := WherexAbs;   {Get cursor pos to start with}
      CurRow := WhereyAbs;
      BlockCursor;

      repeat
        {Move to position}
        GotoxyAbs(CurCol, CurRow);
        ChWord := ReadKeyWord;
        if Ch = #0 then
          case Hi(ChWord) of
            72 :             {Up}
              DecRow(1);
            80 :             {Down}
              IncRow(1);
            75 :             {Left}
              DecCol(1);
            77 :             {Right}
              IncCol(1);
            115,             {^Left}
            15 :             {Shift-Tab}
              TabLeft;
            116 :            {^Right}
              TabRight;
            119,             {^Home}
            132 :            {^PgUp}
              DecRow(Pred(MaxRow));
            117,             {^End}
            118 :            {^PgDn}
              IncRow(Pred(MaxRow));
            73 :             {PgUp}
              begin
                NewRow := CurRow;
                if (CurRow mod 5) = 0 then
                  Dec(NewRow, 5)
                else
                  Dec(NewRow, CurRow mod 5);
                DecRow(CurRow-NewRow);
              end;
            81 :             {PgDn}
              begin
                NewRow := Succ(CurRow div 5)*5;
                IncRow(NewRow-CurRow);
              end;
            71 :             {Home}
              DecCol(ScreenWidth);
            79 :             {End}
              IncCol(ScreenWidth);
          end
        else
          case Ch of
            ^H :             {BkSp}
              DecCol(1);
            ' ' :            {space}
              IncCol(1);
            ^I :             {Tab}
              TabRight;
            Esc :            {Esc}
              begin
                Highlight := False;
                WinSelected := True;
              end;
            Enter :          {Enter}
              if not Highlight then begin
                {save starting point}
                StartCol := CurCol;
                StartRow := CurRow;
                Highlight := True;

                {change attribute to reverse video at cursor}
                ChangeAttribute(1, CurRow, CurCol, Reverse);
              end
              else
                WinSelected := True;
          end;
      until WinSelected;

      {restore the screen}
      RestoreWindow(1, 1, 80, MaxRow, False, MainBufPtr);

      if Highlight then
        with TempMacro do begin
          NumKeys := 1;
          Cols := Succ(CurCol-StartCol);
          for Row := StartRow to CurRow do begin
            {read the line off the screen}
            FastRead(Cols, Row, StartCol, LineBuffer);

            {trim trailing blanks}
            while LineBuffer[LineLen] = ' ' do
              Dec(LineLen);

            {add a carriage return on new line}
            if Row < CurRow then begin
              Inc(LineLen);
              LineBuffer[LineLen] := ^M;
            end;

            {move the character into the macro}
            for I := 1 to LineLen do
              if NumKeys < MaxKeysInMacro then begin
                KeyArray[NumKeys] := CharToMacro(LineBuffer[I]);
                Inc(NumKeys);
              end;
          end;

          {mark end of macro}
          KeyArray[NumKeys] := EndOfMacro;
          Dec(NumKeys);

          {pop up a prompt box asking for TempMacroKey}
          InhibitRestore := True;
          TempMacroKey := MessageChar(Title, Msg);
          InhibitRestore := False;

          {warn about assigning macros to regular keys}
          KeyOK := (Lo(TempMacroKey) < 32) or (Lo(TempMacroKey) > 126);
          if not KeyOK then
            KeyOK := MessageYesOrNo(NullString, NormalCharMsg, 'N');

          if KeyOK and (FindMacroIndex(TempMacroKey) <> 0) then
            KeyOK := MessageYesOrNo(NullString, ConfirmMsg, 'N');

          if not KeyOK then
            MessageDelay(CancelReadMsg, 1000)
          else
            {allocate the macro}
            case AllocateMacro(TempMacroKey, TempMacro, NullString) of
              {0 or 3 are OK, 1 and 2 are errors}
              1 : ErrorMessage(TableFull);
              2 : ErrorMessage(OutOfMemory);
            else MessageDelay(MacroSaved, 500);
            end;
        end;

      WholeScreenSaved := False;
      RestoreCursorState(SaveXY, SaveSL);
      PopupsOn;
      MacrosAreOn := SaveMacrosAreOn;
    end
    else
      Write(^G);

    InPopupMenu := False;
  end;
  {$F-}

  {$F+}
  procedure PopupRecordingOn(var Regs : Registers);
    {-Popup procedure that turns macro recording on}
  const
    Title : string[14] = ' Record macro ';
    Msg : string[19] = 'Press key to define';
    CancelMsg : string[32] = 'Macro recording not activated...';
  var
    SaveXY, SaveSL : Word;
    KeyOK : Boolean;
  begin
    {don't pop up if PopupRecordingOff is active or we're still recording}
    if InPopupMenu or MacroRecording then
      Exit;

    {prevent conflicting popups}
    InPopupMenu := True;

    {re-initialize CRT}
    ReInitCrt;
    SetAttributes;

    {check for text mode}
    if not(InTextMode and (ScreenWidth = 80)) then begin
      InPopupMenu := False;
      RingBell;
      Exit;
    end;

    {save cursor info and hide it}
    GetCursorState(SaveXY, SaveSL);
    HiddenCursor;

    {pop up a prompt box asking for ScrapMacroKey}
    InhibitRestore := True;
    ScrapMacroKey := MessageChar(Title, Msg);
    ScrapMacro.NumKeys := 0;
    ScrapMacro.KeyArray[1] := EndOfMacro;

    {warn about assigning macros to regular keys}
    KeyOK := (Lo(ScrapMacroKey) < 32) or (Lo(ScrapMacroKey) > 126);
    if not KeyOK then
      KeyOK := MessageYesOrNo(NullString, NormalCharMsg, 'N');

    if KeyOK and (FindMacroIndex(ScrapMacroKey) <> 0) then
      KeyOK := MessageYesOrNo(NullString, ConfirmMsg, 'N');

    if KeyOK then begin
      {Delete any existing macro}
      if DeallocateMacro(ScrapMacroKey) then {} ;

      {turn macro recording on}
      MacroRecordingOn;
    end
    else
      MessageDelay(CancelMsg, 1000);

    {restore the message window}
    InhibitRestore := False;
    RestoreMessageWindow;

    {restore cursor}
    RestoreCursorState(SaveXY, SaveSL);

    WholeScreenSaved := False;
    InPopupMenu := False;
  end;
  {$F-}

  {$F+}
  procedure PopupRecordingOff(var Regs : Registers);
    {-Popup procedure that turns macro recording off}
  var
    SaveXY, SaveSL : Word;
    Escaped : Boolean;
  begin
    {don't popup if PopupRecordingOn is active or we're not recording}
    if InPopupMenu or not MacroRecording then
      Exit;

    {prevent conflicting popups}
    InPopupMenu := True;

    {turn macro recording off}
    MacroRecordingOff;

    {re-initialize the screen}
    ReInitCrt;
    SetAttributes;

    {check for text mode}
    if not(InTextMode and (ScreenWidth = 80)) then begin
      RingBell;
      InPopupMenu := False;
      Exit;
    end;

    {save cursor info and hide it}
    GetCursorState(SaveXY, SaveSL);
    HiddenCursor;

    {mark the end of the macro}
    with ScrapMacro do
      KeyArray[Succ(NumKeys)] := EndOfMacro;

    {macro empty?}
    if ScrapMacro.NumKeys = 0 then begin
      {Delete the macro}
      if DeallocateMacro(ScrapMacroKey) then
        {do nothing} ;
      MessageDelay(MacroDeleted, 500);
    end
    else begin
      {prompt for a macro name}
      TempMacroName := '';
      InhibitRestore := True;
      if not MessageString(MacroNameTitle, NullString, TempMacroName, False) then
        {ok} ;
      InhibitRestore := False;

      {allocate space for the macro}
      case AllocateMacro(ScrapMacroKey, ScrapMacro, TempMacroName) of
        {0 or 3 are OK, 1 and 2 are errors}
        1 : ErrorMessage(TableFull);
        2 : ErrorMessage(OutOfMemory);
        else MessageDelay(MacroSaved, 500);
      end;
    end;

    {restore cursor}
    RestoreCursorState(SaveXY, SaveSL);

    WholeScreenSaved := False;
    InPopupMenu := False;
  end;
  {$F-}

  procedure ExternalIFC(BP : Word); interrupt;
    {-This ISR interfaces with the outside world}
  var
    Regs : IntRegisters absolute BP;
    FPtr : ^String64;
    StPtr : ^String64;
    SaveXY, SaveSL : Word;
    SavePSP : Word;
  begin
    {interrupts on}
    InterruptsOn;

    PopupsOff;
    if not InPopupMenu then begin
      with Regs do
        case AH of
          LoadFile :         {load a macro file}
            begin
              {initialize screen in case we need to show an error message}
              ReInitCrt;
              SetAttributes;
              GetCursorState(SaveXY, SaveSL);

              {DS:DX points to name of macro file to load}
              FPtr := Ptr(DS, DX);
              LoadMacros(False, FPtr^);

              {put cursor back}
              RestoreCursorState(SaveXY, SaveSL);
              WholeScreenSaved := False;
            end;

          ExecMacroByKey :   {execute a macro defined in SMACS}
            {macro key passed in BX}
            StartMacroByKey(BX);

          ExecMacroByAddr :  {execute another program's macro}
            {DS:DX points to the macro}
            StartMacro(Ptr(DS, DX));

          GetMacroState :    {return current macro state and turn macros off}
            begin
              {return state in AL}
              AL := Byte(MacrosAreOn);
              MacrosAreOn := False;
            end;

          SetMacroState :    {restore macro state returned by GetMacroState}
            {set state in AL}
            MacrosAreOn := Boolean(AL);

          MacroDefinedCheck : {return AL = 1 if key in BX is defined}
            AL := Byte(MacroAddress(BX) <> nil);

          DefineMacroFunc :  {allocate macro}
            begin
              {DS:DX points to string to be turned into a macro}
              StPtr := Ptr(DS, DX);
              StringToMacro(StPtr^, @TempMacro, MaxKeysInMacro);
              {ES:DI points to name of the macro}
              StPtr := Ptr(ES, DI);
              {BX has macro key}
              AL := AllocateMacro(BX, TempMacro, StPtr^);
              {AL has AllocateMacro's return code}
            end;

          UnloadProgram :    {unload SMACS}
            begin
              {save current PSP}
              SavePSP := GetPSP;

              {switch back to our PSP}
              SetPSP(PrefixSeg);

              {try to unload SMACS}
              AL := Ord(DisableTSR);

              {$IFDEF UseEnhKbd}
              if AL = Ord(True) then
                RestoreKbdVectors;
              {$ENDIF}

              {switch back to previous PSP}
              SetPSP(SavePSP);
            end;
        end;
    end;
    PopupsOn;
  end;

  procedure Abort(Msg : String80);
    {-Display Msg and Halt with error}
  begin
    WriteLn(Msg);
    Halt(1);
  end;

  procedure Initialize;
    {-Initialize global variables}
  begin
    {initialize globals}
    PlaybackDelay := OurPlaybackDelay;
    FName := '';
    SetAttributes;
    MainHotKeyString := HotKeyToString(MainHotKey);

    {$IFDEF UseEnhKbd}
      {enable/disable keyboard remapping}
      EnableEnhanced := UseEnhancedCalls;
    {$ENDIF}

    {set pointers to screen buffer used by main menu}
    case EnhancedDisplay of
      VGA : RowsToSave := 50;
      EGA : RowsToSave := 43;
      else RowsToSave := 25;
    end;
    GetMem(ScreenBufPtr, 160*RowsToSave); {80 * 2 * RowsToSave}
    MessageBufPtr := @ScreenBufPtr^[MsgTop, 1];
    EditorBufPtr := @ScreenBufPtr^[EdTop, 1];
    ConfigBufPtr := @ScreenBufPtr^[ConfigTop, 1];
    ListBufPtr := @ScreenBufPtr^[ListTop, 1];
  end;

  procedure Install;
    {-Process command line parameters, install the module}
  const
    UnloadCmd : string[2] = '-U';
  var
    CmdLine : string[127];
    CmdLen : Byte absolute CmdLine;
    P : IfcPtr;
    Regs : IntRegisters;
    RequestUnload : Boolean;
  begin
    {get first parameter}
    CmdLine := ParamStr(1);

    RequestUnload := (StUpcase(CmdLine) = UnloadCmd);
    if RequestUnload then
      CmdLen := 0;

    {check to see if we're already installed}
    P := ModulePtrByName(ModuleName);
    if P <> nil then
      if (CmdLen <> 0) and (P^.CmdEntryPtr <> nil) then begin
        {they want to load a macro file}
        Regs.DS := Seg(CmdLine);
        Regs.DX := Ofs(CmdLine);
        Regs.AH := LoadFile;
        EmulateInt(Regs, P^.CmdEntryPtr);
        Halt;
      end
      else
        if RequestUnload and (P^.CmdEntryPtr <> nil) then begin
          {they want to unload SMACS}
          {restore interrupt vectors}
          RestoreAllVectors;
          {$IFDEF UseEnhKbd}
            RestoreKbdVectors;
          {$ENDIF}

          {make the function call}
          Regs.AH := UnloadProgram;
          EmulateInt(Regs, P^.CmdEntryPtr);
          if Boolean(Regs.AL) then
            WriteLn(ModuleName, ' unloaded')
          else
            WriteLn('Unable to unload ', ModuleName);

          Halt;
        end
        else
          Abort(ModuleName+' already installed, press '+MainHotKeyString+
            ' to see the menu.');

    {check to see if SideKick is loaded}
    if SideKickLoaded then
      Abort('Can''t be loaded after SideKick!');

    {install our module}
    InstallModule(ModuleName, @ExternalIFC);

    {check for macro file to load}
    if (CmdLen <> 0) then begin
      LoadMacros(False, CmdLine);
      MacrosOn;
    end;
  end;

begin
  {smooth scrolling on CGA's}
  BiosScroll := False;

  {signon message}
  HighVideo;
  WriteLn(ProgName, ^M^J, Copyright, ^M^J);
  LowVideo;
  ProgName := ' '+ProgName+' ';

  {initialization}
  Initialize;
  Install;

     {Initialize popup that turns recording on}
  if DefinePop(RecordOnHotKey, PopupRecordingOn,
               @RecordingOnStack[RecordStackSize], True) and
     {Initialize popup that turns recording off}
     DefinePop(RecordOffHotKey, PopupRecordingOff,
               @RecordingOffStack[RecordStackSize], True) and
     {Initialize the screen reader}
     DefinePop(ReaderHotKey, ReaderPop,
               @ReaderStack[ReaderStackSize], True) and
     {Initialize main resident procedure}
     DefinePop(MainHotKey, MainPop,
               Ptr(SSeg, SPtr), True) then begin

       {show instructions}
       WriteLn(
         ModuleName, ' loaded, press ', MainHotKeyString,
         ' to activate main menu.');
       WriteLn(
         LongInt(MaxParagraphsToKeep-ParagraphsToKeep-(MacroHeap shr 4)) div 64,
         'K RAM available');

       {enable popups and macros}
       PopupsOn;
       MacrosOn;

       {terminate and stay resident}
       StayRes(ParagraphsToKeep+(MacroHeap shr 4), 0);
     end;

  {if we get to here we failed}
  Abort(NoCanPop);
end.
