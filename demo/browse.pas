{$V-,F+}

{*********************************************************}
{*                   BROWSE.PAS 1.30                     *}
{*     An example program for Object Professional 1.0    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program will use features activated with the following defines:
   UseMouse, UseScrollBars
 ***************************************************************************}

program Browse;
  {-File browser}

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
  OpBrowse;

const
  OurColorSet : ColorSet = (
    TextColor       : $1E; TextMono       : $07;
    CtrlColor       : $1E; CtrlMono       : $07;
    FrameColor      : $1B; FrameMono      : $0F;
    HeaderColor     : $0F; HeaderMono     : $0F;
    ShadowColor     : $07; ShadowMono     : $07;
    HighlightColor  : $4F; HighlightMono  : $70;
    PromptColor     : $3F; PromptMono     : $07;
    SelPromptColor  : $3F; SelPromptMono  : $07;
    ProPromptColor  : $07; ProPromptMono  : $07;
    FieldColor      : $31; FieldMono      : $07;
    SelFieldColor   : $31; SelFieldMono   : $07;
    ProFieldColor   : $07; ProFieldMono   : $07;
    ScrollBarColor  : $07; ScrollBarMono  : $07;
    SliderColor     : $07; SliderMono     : $07;
    HotSpotColor    : $70; HotSpotMono    : $07;
    BlockColor      : $2F; BlockMono      : $0F;
    MarkerColor     : $0F; MarkerMono     : $70;
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
  FName : string[64];
  BR : Browser;
  SearchRecord : SearchRec;
  Path : string[64];
  NextParam, Wid : Byte;

  procedure ClearStatusLine;
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
      FastWrite(CharStr(' ', ScreenWidth), 1, 1, ColorMono(PromptColor, PromptMono)); {!!.20}

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure ErrorProc(UnitCode : Byte; var ErrCode : Word; Msg : string);
    {-Error handler}
  var
    I : Word;
    CursorSL, CursorXY : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {save the cursor position and shape}
    GetCursorState(CursorXY, CursorSL);

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {clear the status line}
    ClearStatusLine;

    {display the error message}
    Msg := ' '+Msg+'. Press any key...';
    with OurColorSet do
      FastWrite(Msg, 1, 1, ColorMono(PromptColor, PromptMono));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    {position the cursor at the end of the message}
    NormalCursor;
    GotoXYabs(Length(Msg)+1, 1);

    {wait for a keypress}
    I := ReadKeyWord;

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
      ClearStatusLine;
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
      if Length(Prompt)+MaxLen > ScreenWidth then {!!.20}
        Width := Pred(ScreenWidth)-Length(Prompt) {!!.20}
      else
        Width := MaxLen;
      ReadString(Prompt, 1, 1, MaxLen, Width, S);
      EditProc := (GetLastCommand <> ccQuit);
    end;
  end;

  function YesNo(Prompt : string; Default : Char) : Boolean;
    {-Get a response to a yes-no question}
  var
    LE : LineEditor;
  begin
    with LE do begin
      ClearStatusLine;
      Init(OurColorSet);
      leEditOptionsOn(leAllowEscape);
      YesNo := YesOrNo(Prompt, 1, 1, Default);
    end;
  end;

  function GetFile(MsgCode : Word; Prompt : string;
                   ForceUp, TrimBlanks, Writing, MustExist : Boolean;
                   MaxLen : Byte;  DefExt : ExtStr;
                   var S : string) : Boolean;
    {-Get a filename}
  var
    I : Word;
  begin
    if not EditProc(MsgCode, Prompt, ForceUp, TrimBlanks, MaxLen, S) then
      GetFile := False
    else if Writing then
      if ExistFile(S) then
        GetFile := YesNo('File exists. Overwrite it?', 'N')
      else
        GetFile := True
    else if ExistFile(S) or not MustExist then
      GetFile := True
    else begin
      I := ecFileNotFound;
      ErrorProc(ucNone, I, emFileNotFound);
      GetFile := False;
    end;
  end;

  procedure Help;
    {-Display help message}
  begin
     WriteLn('BROWSE. Copyright (c) 1989,92 by TurboPower Software'^M^J);
     WriteLn('Usage: ');
     WriteLn('  BROWSE filename.ext [filename.ext]');
     WriteLn('  Multiple filenames may be specified after the program name, and');
     WriteLn('  filenames may contain wildcard characters ("*", "?") if desired.');
     Halt(0);
  end;

  function BrowseFile : Boolean;
    {-Browse the next file in SearchRecord}
  begin
    with BR do begin
      {try to open the file}
      OpenFile(Path+SearchRecord.Name);

      {check the error code}
      DosError := GetLastError;

      if DosError = 0 then begin
        {clear the status line} {!!.20}
        Draw;                   {!!.20}
        ClearStatusLine;        {!!.20}

        {browse through the file}
        Process;

        {evaluate the exit command}
        case GetLastCommand of
          ccUser0 :     {user wants to quit}
            DosError := ecFileNotFound;
          ccError :     {an error occurred}
            DosError := GetLastError;
        end;

        {close the file}
        CloseFile;
        if DosError = 0 then
          DosError := GetLastError;
      end;
    end;

    BrowseFile := (DosError = 0);
  end;

begin
  {any parameters?}
  if ParamCount = 0 then
    Help;

  {<F10>: stop browsing}
  BrowseCommands.AddCommand(ccUser0, 1, $4400, 0);

  {<AltX>: stop browsing}
  BrowseCommands.AddCommand(ccUser0, 1, $2D00, 0);

  {<AltH>: toggle hex mode}
  BrowseCommands.AddCommand(ccHexMode, 1, $2300, 0);

  with BR do begin
    {$IFDEF UseScrollBars}
    Wid := ScreenWidth-1;
    {$ELSE}
    Wid := ScreenWidth;
    {$ENDIF}

    {initialize browse window}
    if not InitCustom(1, 2, Wid, ScreenHeight,
                      OurColorSet,
                      DefWindowOptions,
                      MaxAvail-(6*ScreenWidth*ScreenHeight)) then begin {!!.20}
      WriteLn(emInsufficientMemory);
      Halt;
    end;

    {make sure that the whole screen gets saved}
    AdjustFrameCoords(1, 1, ScreenWidth, ScreenHeight);

    {$IFDEF UseScrollBars}
    {add a custom scroll bar}
    wFrame.AddCustomScrollBar(frRR, 0, MaxLongInt, 1, 0, '²', '°', OurColorSet);
    {$ENDIF}

    {set procedure pointers}
    StatusColor := OurColorSet.PromptColor;
    StatusMono := OurColorSet.PromptMono;
    SetStatusProc(BrowseStatus);
    SetEditProc(EditProc);
    SetErrorProc(ErrorProc);
    SetGetFileProc(GetFile);

    {$IFDEF UseMouse}
    if MouseInstalled then begin
      {use a red diamond for our mouse cursor}
      with OurColorSet do
        SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+$04);

      {enable mouse support in OPBROWSE}
      BrowseCommands.cpOptionsOn(cpEnableMouse);
    end;
    {$ENDIF}

    NextParam := 1;
    repeat
      {get the path}
      Path := JustPathName(ParamStr(NextParam));
      Path := AddBackSlash(Path);

      {get the first matching file}
      FindFirst(ParamStr(NextParam), $6, SearchRecord);
      if DosError = 0 then
        {display the file}
        if not BrowseFile then
          {stop the loop}
          NextParam := ParamCount
        else
          {display the rest of the files}
          while DosError = 0 do begin
            {get the next matching file}
            FindNext(SearchRecord);
            if DosError = 0 then
              {display the file}
              if not BrowseFile then
                {stop the loop}
                NextParam := ParamCount;
          end;

      {get next parameter}
      Inc(NextParam);
    until NextParam > ParamCount;

    if not IsActive then
      {browser was never displayed}
      WriteLn('No matching files found')
    else
      {restore the screen}
      Erase;
  end;
end.
