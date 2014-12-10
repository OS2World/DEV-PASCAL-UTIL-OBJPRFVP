{$S-,R-,V-,I-,B-,F+}
{$M 16384,16384,600000}

{*********************************************************}
{*                   EDITOR.PAS 1.30                     *}
{*     An example program for Object Professional 1.0    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program will use features activated with the following defines:
   UseMouse, UseScrollBars
 ***************************************************************************}

program OpEditorDemo;
  {-Demo program for OPEDITOR}

uses
  Use32,
  Dos,
  OpCmd,
  OpCrt,
  OpString,
  OpConst,  {!!.20}
  OpRoot,
  OpDos,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpEdit,
  OpFrame,
  OpWindow,
  OpMemo,
  OpEditor;

const
  OurColorSet : ColorSet = (
    TextColor       : $1B; TextMono       : $07;
    CtrlColor       : $1C; CtrlMono       : $0F;
    FrameColor      : $1B; FrameMono      : $07;
    HeaderColor     : $2F; HeaderMono     : $70; {use for status line}
    ShadowColor     : $07; ShadowMono     : $07;
    HighlightColor  : $4F; HighlightMono  : $70;
    PromptColor     : $1F; PromptMono     : $0F; {use for message line}
    SelPromptColor  : $1F; SelPromptMono  : $0F;
    ProPromptColor  : $07; ProPromptMono  : $07;
    FieldColor      : $1E; FieldMono      : $07;
    SelFieldColor   : $1E; SelFieldMono   : $07;
    ProFieldColor   : $07; ProFieldMono   : $07;
    ScrollBarColor  : $07; ScrollBarMono  : $07;
    SliderColor     : $07; SliderMono     : $07;
    HotSpotColor    : $70; HotSpotMono    : $07;
    BlockColor      : $3E; BlockMono      : $0F;
    MarkerColor     : $4F; MarkerMono     : $70;
    DelimColor      : $0F; DelimMono      : $0F;
    SelDelimColor   : $70; SelDelimMono   : $70;
    ProDelimColor   : $07; ProDelimMono   : $07;
    SelItemColor    : $2F; SelItemMono    : $70;
    ProItemColor    : $17; ProItemMono    : $07;
    HighItemColor   : $1F; HighItemMono   : $0F;
    AltItemColor    : $1F; AltItemMono    : $0F;
    AltSelItemColor : $2F; AltSelItemMono : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono  : $0F;
    FlexBHelpColor  : $1F; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono  : $70;
    UnselXrefColor  : $1E; UnselXrefMono  : $09;
    SelXrefColor    : $5F; SelXrefMono    : $70;
    MouseColor      : $4A; MouseMono      : $70
  );
var
  I, FSize    : LongInt;
  TE          : TextEditor;
  BufSize     : Word;
  ExitCode    : Byte;
  FName       : PathStr;
  YN          : Byte;

  procedure MergeString(T : String; N : Byte; var S : String);
    {-Merge T into S at column N}
  begin
    Move(T[1], S[N], Length(T));
  end;

  procedure MergeNumber(N : LongInt; Col : Byte; var S : String);
    {-Merge the number N into S at Col}
  var
    St : String[15];
    StLen : Byte absolute St;
  begin
    St := Long2Str(N);
    Move(St[1], S[Col], StLen);
  end;

  procedure MergeNumberRight(N : LongInt; Col : Byte; var S : String);
    {-Merge the number N into S, right-aligned at Col}
  var
    St : String[15];
    StLen : Byte absolute St;
  begin
    St := Long2Str(N);
    Move(St[1], S[Col-Pred(StLen)], StLen);
  end;

  procedure UserHook(CPP : CommandProcessorPtr; MT : MatchType; Key : Word);
    {-Called each time CommandProcessor evaluates a keystroke}
  var
    S : string[2];
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    S := '  ';
    if MT = PartMatch then
      if Lo(Key) < Ord(' ') then begin
        S[1] := '^';
        S[2] := Char(Lo(Key)+$40);
      end
      else
        S[1] := '+';

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    with OurColorSet do
      FastWrite(S, ErrorRow, 1, ColorMono(PromptColor, PromptMono));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure OurStatusProc(MP : MemoPtr);
    {-Display status line}
  const
    OnOff : array[Boolean] of string[3] = ('Off', 'On ');
    Save : array[Boolean] of string[4] = ('    ', 'Save');

    RawStatusLine : string[80] =
      {         1         2         3         4         5         6         7         8}
      {12345678901234567890123456789012345678901234567890123456789012345678901234567890}
      '               Line:       Col:           /       Insert Fixed Indent Wrap Save ';
      { FILENAME.EXT  Line: 12345 Col: 123 12345/12345   Insert Fixed Indent Wrap Save } {!!.20}
  var
    S : string[5];
    Status : string[80];
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    with TextEditorPtr(MP)^ do begin
      {get filename if it changed}
      if teOptionsAreOn(teNewFile) then begin
        FName := JustFileName(mfFileName);
        FName := StUpcase(FName);
        teOptionsOff(teNewFile);
      end;

      {get a copy of the raw status line}
      Status := RawStatusLine;

      {insert filename}
      MergeString(FName, 2, Status);

      {insert line number}
      MergeNumber(meCurLine, 22, Status);

      {insert column number}
      MergeNumber(meCurCol, 33, Status);

      {insert bytes used/maximum}
      MergeNumberRight(meTotalBytes, 41, Status);   {!!.20}
      MergeNumber(meBufSize-2, 43, Status);         {!!.20}
      {Note: OPMEMO/OPEDITOR maintains a safety margin of 2 bytes}

      {insert remaining fields}
      if not teOptionsAreOn(teInsert) then
        MergeString(' Over ', 51, Status);
      if teOptionsAreOn(teSmartTabs) then
        MergeString('Smart ', 58, Status);
      if not teOptionsAreOn(teIndent) then
        FillChar(Status[64], 6, ' ');
      if not teOptionsAreOn(teWordWrap) then
        FillChar(Status[71], 4, ' ');
      if not teOptionsAreOn(teModified) then
        FillChar(Status[76], 4, ' ');

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      {display status line}
      FastWrite(Status, StatusRow, 1, ColorMono(StatusColor, StatusMono));

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;
  end;

  procedure Abort(Msg : string);
    {-Display an error message and halt}
  begin
    {$IFDEF UseMouse}
    {hide the mouse cursor}
    HideMouse;
    {$ENDIF}

    Window(1, 1, ScreenWidth, ScreenHeight);
    ClrScr;
    WriteLn(Msg);
    Halt(1);
  end;

  procedure ClearPromptLine;
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
      FastWrite(CharStr(' ', 80), ErrorRow, 1, ColorMono(PromptColor, PromptMono));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure DisplayMessage(Msg : string);
    {-Display a message at the top of the screen}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    ClearPromptLine;
    with OurColorSet do
      FastWrite(Msg, ErrorRow, 1, ColorMono(PromptColor, PromptMono));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    GotoXYabs(Length(Msg)+2, ErrorRow);
  end;

  procedure ErrorProc(UnitCode : Byte; var ErrCode : Word; Msg : string);
    {-Error handler}
  var
    I : Word;
    CursorSL, CursorXY : Word;
  begin
    {save the cursor position and shape}
    GetCursorState(CursorXY, CursorSL);

    {clear the status line}
    ClearPromptLine;

    {display the error message}
    NormalCursor;
    DisplayMessage(' '+Msg+'. Press any key...');

    {wait for a keypress}
    I := ReadKeyWord;

    {clear the prompt line}
    ClearPromptLine;

    {Restore cursor position and shape}
    RestoreCursorState(CursorXY, CursorSL);
  end;

  function EditProc(MsgCode : Word;
                    Prompt : string;
                    ForceUp : Boolean;
                    TrimBlanks : Boolean;
                    MaxLen : Byte;
                    var S : string) : Boolean;
   {-Line editing routine}
  var
    LE : LineEditor;
    Width : Byte;
  begin
    with LE do begin
      ClearPromptLine;
      Init(OurColorSet);
      if ForceUp then
        leEditOptionsOn(leForceUpper)
      else
        leEditOptionsOff(leForceUpper);
      if TrimBlanks then
        leEditOptionsOn(leTrimBlanks)
      else
        leEditOptionsOff(leTrimBlanks);
      Prompt := ' '+Prompt;
      if Length(Prompt)+MaxLen > 80 then
        Width := 79-Length(Prompt)
      else
        Width := MaxLen;
      ReadString(Prompt, 1, 1, MaxLen, Width, S);
      EditProc := (GetLastCommand <> ccQuit);
      ClearPromptLine;
    end;
  end;

  function YesNoFunc(MsgCode : Word; Prompt : string;
                     Default : Byte; QuitAndAll : Boolean) : Byte;
    {-Get a response to a yes-no question}
  var
    LE : LineEditor;
    Ch : Char;
    CharsToTake : CharSet;
  begin
    with LE do begin
      ClearPromptLine;
      Init(OurColorSet);
      leEditOptionsOn(leAllowEscape+leDefaultAccepted+leForceUpper);
      if Default = teYes then
        Ch := 'Y'
      else
        Ch := 'N';
      if QuitAndAll then begin
        CharsToTake := ['Y', 'N', 'A', 'Q'];
        Prompt := Prompt+' (Y/N/A/Q)'
      end
      else
        CharsToTake := ['Y', 'N'];
      ReadChar(Prompt, 1, 1, CharsToTake, Ch);
      if GetLastCommand = ccQuit then
        YesNoFunc := teQuit
      else case Ch of
        'Y' : YesNoFunc := teYes;
        'N' : YesNoFunc := teNo;
        'A' : YesNoFunc := teAll;
        'Q' : YesNoFunc := teQuit;
      end;
      ClearPromptLine;
    end;
  end;

  function GetFile(MsgCode : Word; Prompt : string;
                   ForceUp, TrimBlanks, Writing, MustExist : Boolean;
                   MaxLen : Byte; DefExt : ExtStr;
                   var S : string) : Boolean;
    {-Get a filename}
  var
    I : Word;
  begin
    if not EditProc(0, Prompt, ForceUp, TrimBlanks, MaxLen, S) then
      GetFile := False
    else if Writing then
      if ExistFile(S) then
        GetFile := YesNoFunc(0, 'File exists. Overwrite it?', teNo, False) = teYes
      else
        GetFile := True
    else if ExistFile(S) or not MustExist then
      GetFile := True
    else begin
      I := 0;
      ErrorProc(ucNone, I, 'File not found');
      GetFile := False;
    end;
  end;

  procedure InstallUserCommands;
    {-Install user-defined exit commands}
  begin
    {^F2 = Save and exit}
    EditorCommands.AddCommand(ccSaveExit, 1, $5F00, 0);
    {AltF2 = Abandon file}
    EditorCommands.AddCommand(ccAbandonFile, 1, $6900, 0);
  end;

begin
  {calculate size of edit buffer}
  I := MaxAvail-10000;
  if I > $FFF1 then
    BufSize := $FFF1
  else
    BufSize := I;
  if I <= 0 then
    Halt;

  {get name of file to edit}
  FName := ParamStr(1);
  if Length(FName) = 0 then begin
    Write('File to edit: ');
    BufLen := 64;
    ReadLn(FName);
  end;

  {halt if no filename specified}
  if Length(FName) = 0 then
    Halt(0);
  FName := StUpcase(FName);

  {$IFDEF UseMouse}
  if MouseInstalled then begin
    {use a red diamond for our mouse cursor}
    with OurColorSet do
      SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+$04);

    {enable mouse support in OPEDITOR}
    EditorCommands.cpOptionsOn(cpEnableMouse);
  end;
  {$ENDIF}

  {install user-defined exit commands}
  InstallUserCommands;

  {initialize the editor window}
  DefaultColorSet := OurColorSet;
  with TE, OurColorSet do begin
    {$IFDEF UseScrollBars}
    if not Init(1, 3, ScreenWidth-1, ScreenHeight, BufSize) then
      Abort(emInsufficientMemory);
    {$ELSE}
    if not Init(1, 3, ScreenWidth, ScreenHeight, BufSize) then
      Abort(emInsufficientMemory);
    {$ENDIF}

    {make sure that the whole screen gets saved}
    AdjustFrameCoords(1, 1, ScreenWidth, ScreenHeight);

    {$IFDEF UseScrollBars}
    {add a custom scroll bar}
    wFrame.AddCustomScrollBar(frRR, 0, MaxLongInt, 2, 0, '²', '°', OurColorSet);
    {$ENDIF}

    {use our special status display routine}
    SetStatusProc(OurStatusProc);
    StatusColor := HeaderColor;
    StatusMono := HeaderMono;

    {don't allow reading of partial files}
    teOptionsOff(meAllowTrunc);

    {try to open the file before installing error handler}
    ReadFile(FName, FSize);
    if GetLastError <> 0 then
      Abort('Error reading '+FName);

    {set procedure pointers}
    SetEditProc(EditProc);
    SetErrorProc(ErrorProc);
    SetGetFileProc(GetFile);
    SetYesNoProc(YesNoFunc);
    EditorCommands.SetUserHookProc(UserHook);

    {draw the interior of the editor window}
    Draw;

    {clear the status line}
    ClearPromptLine;

    {$IFDEF UseMouse}
    {show the mouse}
    ShowMouse;
    {$ENDIF}

    repeat
      {start editing}
      Process;

      {process exit command}
      ExitCode := GetLastCommand;
      case ExitCode of
        ccQuit,                {quit}
        ccAbandonFile :        {abandon file}
          if not teOptionsAreOn(teModified) then
            ExitCode := ccQuit
          {file was modified--verify that user wants to quit}
          else begin
            YN := YesNoFunc(0, emFileModified, teYes, False);
            case YN of
              teYes :
                begin
                  SaveFile;
                  ExitCode := ccQuit
                end;
              teNo :
                ExitCode := ccQuit;
              else
                ExitCode := ccNone;
            end;
          end;
      end;
    until (ExitCode = ccQuit) or (ExitCode = ccSaveExit);

    {$IFDEF UseMouse}
    {hide the mouse cursor}
    HideMouse;
    {$ENDIF}

    {erase the memo window}
    Erase;
  end;
end.
