{$S-,R-,V-,I-,B-}
{$M 16384,16384,600000}

{*********************************************************}
{*                    MEMO.PAS 1.30                      *}
{*     An example program for Object Professional 1.0    *}
{*      Copyright (c) TurboPower Software 1988, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program will use features activated with the following defines:
   UseMouse
 ***************************************************************************}

program OpMemoTest;
  {-Test program for OPMEMO}

uses
  Dos,
  OpConst,                   {!!.20}
  OpRoot,
  OpCmd,                     {command processing}
  OpCrt,                     {CRT unit}
  OpString,                  {string handling}
  {$IFDEF UseMouse}
  OpMouse,                   {mouse routines}
  {$ENDIF}
  OpFrame,                   {frames, shadows, etc.}
  OpWindow,                  {windows}
  OpMemo;                    {memo field editor}

const
  OurColorSet : ColorSet = (
    TextColor      : $1B; TextMono      : $07;
    CtrlColor      : $1C; CtrlMono      : $0F;
    FrameColor     : $1B; FrameMono     : $07;
    HeaderColor    : $2F; HeaderMono    : $70; {use for status line}
    ShadowColor    : $07; ShadowMono    : $07;
    HighlightColor : $07; HighlightMono : $07;
    PromptColor    : $1F; PromptMono    : $0F; {use for message line}
    SelPromptColor : $07; SelPromptMono : $07;
    ProPromptColor : $07; ProPromptMono : $07;
    FieldColor     : $0F; FieldMono     : $0F;
    SelFieldColor  : $70; SelFieldMono  : $70;
    ProFieldColor  : $07; ProFieldMono  : $07;
    ScrollBarColor : $07; ScrollBarMono : $07;
    SliderColor    : $07; SliderMono    : $07;
    HotSpotColor   : $70; HotSpotMono   : $07;
    BlockColor     : $07; BlockMono     : $07;
    MarkerColor    : $07; MarkerMono    : $07;
    DelimColor     : $0F; DelimMono     : $0F;
    SelDelimColor  : $70; SelDelimMono  : $70;
    ProDelimColor  : $07; ProDelimMono  : $07;
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
    MouseColor      : $4F; MouseMono      : $70
  );
var
  I, FSize    : LongInt;
  MF          : MemoFile;
  BufSize     : Word;
  ExitCode    : Byte;
  FName       : PathStr;

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

  procedure ClearMessageLine;
    {-Clear the message line}
  begin
    FastWrite(CharStr(' ', ScreenWidth), ErrorRow, 1,
              ColorMono(ErrorColor, ErrorMono));
  end;

  procedure DisplayMessage(Msg : string);
    {-Display a message at the top of the screen}
  begin
    ClearMessageLine;
    FastWrite(Msg, ErrorRow, 1, ColorMono(ErrorColor, ErrorMono));
    GotoXYabs(Length(Msg)+2, ErrorRow);
  end;

  function YesNo(Msg : string) : Byte;
    {-Get a response to a yes/no question. Return True for Y, False for N}
  var
    ChWord : Word;
    Ch : Char absolute ChWord;
  begin
    DisplayMessage(Msg);
    repeat
      ChWord := ReadKeyWord;
      Ch := Upcase(Ch);
    until (Ch = 'Y') or (Ch = 'N') or (Ch = #27);
    case Ch of
      'N' : YesNo := 0;
      'Y' : YesNo := 1;
      else YesNo := 2;
    end;
    ClearMessageLine;
  end;

  procedure SaveMemoFile;
    {-Save the file in the edit buffer}
  begin
    DisplayMessage('Saving file...');
    MF.SaveFile;
    if MF.GetLastError <> 0 then
      Abort('');
    ClearMessageLine;
  end;

  procedure InstallUserCommands;
    {-Install user-defined exit commands}
  begin
    {ccUser0 = save file and continue: ^KS, F2}
    MemoCommands.AddCommand(ccUser0, 2, Ord(^K), Ord(^S));
    MemoCommands.AddCommand(ccUser0, 1, $3C00, 0);

    {ccUser1 = save file and exit: ^KX, ^F2}
    MemoCommands.AddCommand(ccUser1, 2, Ord(^K), Ord(^X));
    MemoCommands.AddCommand(ccUser1, 1, $5F00, 0);

    {ccUser2 = abandon file: ^KQ, AltF2}
    MemoCommands.AddCommand(ccUser2, 2, Ord(^K), Ord(^Q));
    MemoCommands.AddCommand(ccUser2, 1, $6900, 0);

    if MemoCommands.GetLastError <> 0 then
      RingBell;
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

  {$IFDEF UseMouse}
  if MouseInstalled then begin
    {use a red diamond for our mouse cursor}
    with OurColorSet do
      SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+$04);

    {enable mouse support in OPMEMO}
    MemoCommands.cpOptionsOn(cpEnableMouse);
  end;
  {$ENDIF}

  {install user-defined exit commands}
  InstallUserCommands;

  {initialize the memo window}
  DefaultColorSet := OurColorSet;
  with MF, OurColorSet do begin
    if not InitAndAlloc(1, 3, ScreenWidth, ScreenHeight, BufSize) then
      Abort(emInsufficientMemory);

    {make sure that the whole screen gets saved}
    AdjustFrameCoords(1, 1, ScreenWidth, ScreenHeight);

    {don't allow reading of partial files}
    meOptionsOff(meAllowTrunc);

    {try to open the file before installing error handler}
    ReadFile(FName, FSize);
    if GetLastError <> 0 then
      Abort('Error reading '+FName);

    {use built-in status and error handlers}
    SetStatusProc(MemoStatus);
    StatusColor := HeaderColor;
    StatusMono := HeaderMono;

    SetErrorProc(MemoError);
    ErrorColor := PromptColor;
    ErrorMono := PromptMono;

    {draw the interior of the memo window}
    Draw;

    {clear the message line}
    ClearMessageLine;

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
        ccError :              {a fatal error occurred}
          ExitCode := ccQuit;
        ccUser0,               {save and continue}
        ccUser1 :              {save and quit}
          SaveMemoFile;
        ccQuit,                {quit}
        ccUser2 :              {abandon file}
          if not meOptionsAreOn(meModified) then
            ExitCode := ccQuit
          {file was modified--ask if user wants to save changes}
          else case YesNo(emFileModified) of
            0 :
              ExitCode := ccQuit;
            1 :
              begin
                SaveMemoFile;
                ExitCode := ccQuit;
              end;
            2 :
              ExitCode := ccNone;
          end;
      end;
    until (ExitCode = ccQuit) or (ExitCode = ccUser1);

    {$IFDEF UseMouse}
    {hide the mouse cursor}
    HideMouse;
    {$ENDIF}

    {erase the memo window}
    Erase;
  end;
end.
