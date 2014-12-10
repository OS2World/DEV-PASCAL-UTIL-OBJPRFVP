
{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPSEDIT.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpSEdit;
  {-Simple line editor}

interface

uses
  Use32,
  Dpmi,          {!!.20}
  OpConst,       {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF VirtualPascal}
  VpUtils,
  {$ENDIF}
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd
  {$IFDEF UseDrag}  {!!.03}
  , OpDrag          {!!.03}
  {$ENDIF}          {!!.03}
  ;

  {$I OPSEDIT.ICD}  {configuration data}

const
  {option codes for simple line editor}
  seMapCtrls         = $00000001; {map control characters}
  seCursorToEnd      = $00000002; {put cursor at end of string initially}
  seTrimBlanks       = $00000004; {trim leading/trailing blanks when finished}
  seClearFirstChar   = $00000008; {clear string if first character is ASCII}
  seBeepOnError      = $00000010; {beep when illegal char entered?}
  seHouseCursorAtEnd = $00000020; {extra char of width to hold cursor when string full?}
  seForceUpper       = $00000040; {force chars to upper case?}
  seForceLower       = $00000080; {force chars to lower case?}
  seWindowRelative   = $00000100; {coordinates treated relative to current window?}
  seInsertByDefault  = $00000200; {default to insert mode?}

  BadSimpEditOptions : LongInt = 0;
  DefSimpEditOptions : LongInt = seCursorToEnd+seTrimBlanks+seClearFirstChar+
                                 seMapCtrls+seHouseCursorAtEnd+
                                 seInsertByDefault;
type
  ClearFirstFunc = function(var Cmd, Key : Word) : Boolean;

  SimpleLineEditorPtr = ^SimpleLineEditor;
  SimpleLineEditor =
    object(Root)
      seCmd         : Word;       {last command}
      seKey         : Word;       {last key pressed}
      seCmdPtr      : CommandProcessorPtr; {command processor}
      seUnitCode    : Byte;       {unit code}
      seInsertMode  : Boolean;    {insert mode}
      seOptions     : LongInt;    {edit options}
      seHelpIndex   : Word;       {help index}
      sePromptColor : Byte;       {colors for prompts}
      sePromptMono  : Byte;
      seFieldColor  : Byte;       {colors for edit fields}
      seFieldMono   : Byte;
      seCtrlColor   : Byte;       {colors for control characters}
      seCtrlMono    : Byte;
      seCFF         : ClearFirstFunc; {function to check for first char}

      constructor Init(var Colors : ColorSet);
        {-Sets default values for attributes, etc.}
      function GetLastCommand : Word;
        {-Return last command entered by user}
      function GetLastKey : Word;
        {-Return last keystroke entered by user}
      {...}
      procedure ReadString(Prompt : string; Row, Col, MaxLen, Width : Byte;
                           var S : string);
        {-Edit a string}
      {...}
      procedure seOptionsOn(OptionFlags : LongInt);
        {-Activate multiple options}
      function seOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified options are on}
      procedure seOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple options}
      {...}
      procedure SetPromptAttr(Color, Mono : Byte);
        {-Set attributes for prompts}
      procedure SetFieldAttr(Color, Mono : Byte);
        {-Set attributes for line being edited}
      procedure SetCtrlAttr(Color, Mono : Byte);
        {-Set attributes for control characters}
      {...}
      procedure SetHelpIndex(Index : Word);
        {-Set the help index}
      procedure SetCommandProcessor(var CP : CommandProcessor);
        {-Set command processor to use}
      procedure SetClearFirstFilter(CFF : ClearFirstFunc);
        {-Set procedure to call for ClearFirstChar test}
    end;

var
  {$IFDEF UseDrag}                     {!!.03}
  SimpEditCommands : DragProcessor;    {!!.03}
  {$ELSE}                              {!!.03}
  SimpEditCommands : CommandProcessor; {command processor used for line editor}
  {$ENDIF}                             {!!.03}

  {==========================================================================}

implementation

  function DefClearFirstFunc(var Cmd, Key : Word) : Boolean;
    {-Default ClearFirstFunc}
  begin
    DefClearFirstFunc := (Cmd = ccChar);
  end;

  constructor SimpleLineEditor.Init(var Colors : ColorSet);
    {-Sets default values for attributes, etc.}
  begin
    seCmd         := ccNone;
    seKey         := 0;
    seCmdPtr      := @SimpEditCommands;
    seUnitCode    := ucSEdit;
    seInsertMode  := True;
    seOptions     := DefSimpEditOptions;
    seHelpIndex   := 0;
    sePromptColor := Colors.SelPromptColor;
    sePromptMono  := Colors.SelPromptMono;
    seFieldColor  := Colors.SelFieldColor;
    seFieldMono   := Colors.SelFieldMono;
    seCtrlColor   := Colors.CtrlColor;
    seCtrlMono    := Colors.CtrlMono;
    seCFF         := DefClearFirstFunc;
  end;

  function SimpleLineEditor.GetLastCommand : Word;
    {-Return last command entered by user}
  begin
    GetLastCommand := seCmd;
  end;

  function SimpleLineEditor.GetLastKey : Word;
    {-Return last keystroke entered by user}
  begin
    GetLastKey := seKey;
  end;

  procedure SimpleLineEditor.seOptionsOn(OptionFlags : LongInt);
    {-Activate multiple options}
  begin
    SetLongFlag(seOptions, OptionFlags and not BadSimpEditOptions);
  end;

  function SimpleLineEditor.seOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified options are on}
  begin
    seOptionsAreOn := seOptions and OptionFlags = OptionFlags;
  end;

  procedure SimpleLineEditor.seOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple options}
  begin
    ClearLongFlag(seOptions, OptionFlags and not BadSimpEditOptions);
  end;

  procedure SimpleLineEditor.SetPromptAttr(Color, Mono : Byte);
    {-Set attributes for prompts}
  begin
    sePromptColor := Color;
    sePromptMono := MapMono(Color, Mono);
  end;

  procedure SimpleLineEditor.SetFieldAttr(Color, Mono : Byte);
    {-Set attributes for edit fields}
  begin
    seFieldColor := Color;
    seFieldMono := MapMono(Color, Mono);
  end;

  procedure SimpleLineEditor.SetCtrlAttr(Color, Mono : Byte);
    {-Set attributes for control characters}
  begin
    seCtrlColor := Color;
    seCtrlMono := MapMono(Color, Mono);
  end;

  procedure SimpleLineEditor.SetHelpIndex(Index : Word);
    {-Set the help index}
  begin
    seHelpIndex := Index;
  end;

  procedure SimpleLineEditor.SetCommandProcessor(var CP : CommandProcessor);
    {-Set command processor to use}
  begin
    seCmdPtr := @CP;
  end;

  procedure SimpleLineEditor.SetClearFirstFilter(CFF : ClearFirstFunc);
    {-Set procedure to call for ClearFirstChar test}
  begin
    seCFF := CFF;
  end;

  procedure SimpleLineEditor.ReadString(Prompt : string;
                                        Row, Col, MaxLen, Width : Byte;
                                        var S : string);
    {-Edit a string}
  type
    XY = record
           X, Y : Byte;
         end;
  var
    Ch : Char absolute seKey;
    St : string;
    StLen : Byte absolute St;
    CursorSL, SaveXY : Word;
    FA, CA, Sp : Byte;
    FirstChar : Boolean;
    SaveBreak : Boolean;
    Finished : Boolean;
    StOffset : Byte;
    EditLen : Byte;
    MaxBoxLen : Byte;
    SaveInsertMode : Boolean;
    EMC : Boolean;

    procedure ToggleInsertMode;
      {-Toggle between insert and overtype mode, keeping BIOS keyboard flag up
        to date}
    const
      InsertBit  = $80;
{$IFDEF VIRTUALPASCAL}
    begin
{$ELSE}
    var
      KeyboardFlags : ^Word; {absolute $40 : $17;}                      {!!.20}
    begin
      KeyboardFlags := Ptr(BiosDataSele, $17);                          {!!.20}
{$ENDIF}
      {toggle insert flag}
      seInsertMode := not seInsertMode;

      {use fat cursor if inserting}
      if seInsertMode then begin
        SetCursorType(InsertCursor);
        {$IFDEF VIRTUALPASCAL}
        SetKeyboardState( kbd_Insert, True );
        {$ELSE}
        SetFlag(KeyboardFlags^, InsertBit);                              {!!.20}
        {$ENDIF}
      end
      else begin
        SetCursorType(OvertypeCursor);
        {$IFDEF VIRTUALPASCAL}
        SetKeyboardState( kbd_Insert, False );
        {$ELSE}
        ClearFlag(KeyboardFlags^, InsertBit);                            {!!.20}
        {$ENDIF}
      end;
    end;

    procedure Draw;
      {-Draw the string}
    var
      A, I : Byte;
      Ch : Char;
      {$IFDEF UseMouse}
      SaveMouse : Boolean;
      {$ENDIF}
    begin
      {pad the end of the string}
      FillChar(St[Succ(StLen)], MaxBoxLen-StLen, ' ');

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      {draw the string}
      for I := 1 to EditLen do begin
        Ch := St[StOffset+I];
        if (Ch < ' ') and EMC then begin
          Ch := Chr(Ord(Ch) or $40);
          A := CA;
        end
        else
          A := FA;
        FastFill(1, Ch, Row, Col+Pred(I), A);
      end;

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;

    procedure Redraw(MoveCursor : Boolean);
      {-Position cursor and redraw string}
    begin
      if Sp = 0 then
        Sp := 1
      else if (StLen < MaxLen) and (Sp > StLen) then
        Sp := Succ(StLen)
      else if Sp > MaxLen then
        if LongFlagIsSet(seOptions, seHouseCursorAtEnd) then
          Sp := Succ(MaxLen)
        else
          Sp := MaxLen;
      if Sp > EditLen+StOffset then
        StOffset := Sp-EditLen
      else if Sp < Succ(StOffset) then
        StOffset := Pred(Sp);
      Draw;
      if MoveCursor then
        GoToXYAbs(Col+Pred(SP)-StOffset, Row);
    end;

    procedure InsertChar;
      {-Insert Ch at St[SP]}
    begin
      if StLen < MaxLen then begin
        Insert(Ch, St, Sp);
        if StLen > MaxLen then
          StLen := MaxLen;
        Inc(Sp);
      end
      else if LongFlagIsSet(seOptions, seBeepOnError) then
        RingBell;
    end;

    procedure WordLeft;
      {-Cursor left one word}
    begin
      if Sp > 1 then begin
        Dec(Sp);
        while (Sp >= 1) and ((Sp > StLen) or (St[Sp] = ' ')) do
          Dec(Sp);
        while (Sp >= 1) and (St[Sp] <> ' ') do
          Dec(Sp);
        Inc(Sp);
      end;
    end;

    procedure WordRight;
      {-Cursor right one word}
    begin
      if Sp <= StLen then begin
        Inc(Sp);
        while (Sp <= StLen) and (St[Sp] <> ' ') do
          Inc(Sp);
        while (Sp <= StLen) and (St[Sp] = ' ') do
          Inc(Sp);
      end;
    end;

    procedure DeleteWord;
      {-Delete word at cursor}
    var
      DelEnd : Byte;
    begin
      if Sp <= StLen then begin
        {start deleting at the cursor}
        DelEnd := Sp;

        {delete all of the current word, if any}
        if St[Sp] <> ' ' then
          while (St[DelEnd] <> ' ') and (DelEnd <= StLen) do
            Inc(DelEnd);

        {delete any spaces prior to the next word, if any}
        while (St[DelEnd] = ' ') and (DelEnd <= StLen) do
          Inc(DelEnd);

        Delete(St, Sp, DelEnd-Sp);
      end;
    end;

    procedure DrawPrompt;
      {-Draw the prompt}
    var
      SaveMouse : Boolean;
    begin
      if Prompt <> '' then begin
        {$IFDEF UseMouse}
        HideMousePrim(SaveMouse);
        {$ENDIF}

        {draw the prompt}
        FastWrite(Prompt, Row, Col, ColorMono(sePromptColor, sePromptMono));

        {string goes just after prompt}
        Inc(Col, Length(Prompt));

        {$IFDEF UseMouse}
        ShowMousePrim(SaveMouse);
        {$ENDIF}
      end;
    end;

    procedure TrimSpaces;
      {-Trim leading and trailing blanks}
    begin
      while (StLen > 0) and (St[StLen] = ' ') do
        Dec(StLen);
      while (StLen > 0) and (St[1] = ' ') do
        Delete(St, 1, 1);
    end;

  begin
    {adjust coordinates if necessary}
    if LongFlagIsSet(seOptions, seWindowRelative) then begin
      Inc(Row, XY(WindMin).Y);
      Inc(Col, XY(WindMin).X);
    end;

    if MaxLen > 254 then
      MaxLen := 254;

    if LongFlagIsSet(seOptions, seHouseCursorAtEnd) then
      MaxBoxLen := MaxLen+1
    else
      MaxBoxLen := MaxLen;

    if (Width = MaxLen) or (Width >= MaxBoxLen) then
      EditLen := MaxBoxLen
    else
      EditLen := Width;

    {Store cursor shape and position}
    GetCursorState(SaveXY, CursorSL);

    {Save break checking state}
    SaveBreak := CheckBreak;
    CheckBreak := False;

    {Get the default string}
    St := S;
    if StLen > MaxLen then
      StLen := MaxLen;

    {set insert mode}
    if LongFlagIsSet(seOptions, seInsertByDefault) then
      {force insert mode}
      seInsertMode := True;
    seInsertMode := not seInsertMode;
    ToggleInsertMode;

    {position the cursor}
    StOffset := 0;
    if LongFlagIsSet(seOptions, seCursorToEnd) then
      Sp := Succ(StLen)
    else
      Sp := 1;

    {map control characters?}
    EMC := LongFlagIsSet(seOptions, seMapCtrls);

    {draw the prompt}
    DrawPrompt;

    {colors for field, control characters}
    FA := ColorMono(seFieldColor, seFieldMono);
    CA := ColorMono(seCtrlColor, seCtrlMono);

    {Loop reading keys}
    Finished := False;
    FirstChar := True;
    repeat
      {Position cursor and redraw string}
      Redraw(True);

      {get next command}
      seCmd := seCmdPtr^.GetCommand(seKey);

      {deal with control characters if desired}
      if seCmd = ccCtrlChar then
        {don't allow control characters if attributes same and ctrl mapping on}
        if (CA = FA) and EMC then
          seCmd := ccNone
        else begin
          SetCursorType(CtrlCharCursor);
          seKey := seCmdPtr^.cpGetKey;
          seCmd := ccChar;
          if seInsertMode then
            SetCursorType(InsertCursor)
          else
            SetCursorType(OvertypeCursor);
        end;


      if FirstChar then
        FirstChar := seCFF(seCmd, seKey);

      case seCmd of
        ccNone :             {not a command}
          if LongFlagIsSet(seOptions, seBeepOnError) then
            RingBell;

        ccChar :             {A character to enter the string}
          begin
            if LongFlagIsSet(seOptions, seForceUpper) then
              Ch := Upcase(Ch)
            else if LongFlagIsSet(seOptions, seForceLower) then
              Ch := Locase(Ch);

            {clear the input string if appropriate}
            if FirstChar and LongFlagIsSet(seOptions, seClearFirstChar) then begin
              FirstChar := False;
              StOffset := 0;
              StLen := 0;
              Sp := 1;
            end;

            if seInsertMode then
              InsertChar
            {overtype mode}
            else if Sp <= MaxLen then begin
              St[Sp] := Ch;
              if Sp > StLen then
                StLen := Sp;
              Inc(Sp);
            end;
          end;

        ccMouseSel,
        ccSelect,
        ccUser0..ccUser65335 :
          begin
            {trim blanks?}
            if LongFlagIsSet(seOptions, seTrimBlanks) then
              TrimSpaces;

            Finished := True;
          end;

        ccRestore,           {Restore default and continue}
        ccQuit :             {Restore default string and quit}
          begin
            St := S;
            if LongFlagIsSet(seOptions, seCursorToEnd) then
              Sp := Succ(StLen)
            else
              Sp := 1;
            if StLen > MaxLen then
              StLen := MaxLen;
            Finished := (seCmd = ccQuit);
          end;

        ccHome :             {Cursor to beginning of line}
          Sp := 1;

        ccEnd :              {Cursor to end of line}
          Sp := Succ(StLen);

        ccDelEol :           {Delete from cursor to end of line}
          St := Copy(St, 1, Pred(Sp));

        ccDelBol :           {Delete from beginning of line to the cursor}
          begin
            Delete(St, 1, Pred(Sp));
            Sp := 1;
          end;

        ccDelLine :          {Delete entire line}
          begin
            StLen := 0;
            Sp := 1;
          end;

        ccLeft :             {Cursor left by one character}
          if Sp > 1 then
            Dec(Sp);

        ccRight :            {Cursor right by one character}
          if (SP <= StLen) then
            Inc(Sp);

        ccWordLeft :         {Cursor left one word}
          WordLeft;

        ccWordRight :        {Cursor right one word}
          WordRight;

        ccDel :              {Delete current character}
          if Sp <= StLen then
            Delete(St, Sp, 1);

        ccBack :             {Backspace one character}
          if Sp > 1 then begin
            Dec(Sp);
            Delete(St, Sp, 1);
            if StOffset > 0 then
              {String horizontally scrolled}
              if StOffset+EditLen >= StLen then
                {The rightmost portion of the string is displayed, so scroll}
                Dec(StOffset);
          end;

        ccDelWord :          {Delete word to right of cursor}
          DeleteWord;

        ccIns :              {Toggle insert mode}
          ToggleInsertMode;

        ccHelp :             {Help}
          seCmdPtr^.cpGetHelp(seUnitCode, nil, seHelpIndex);
      end;
    until Finished;

    {make sure screen is up to date}
    Redraw(False);

    {set S}
    S := St;

    {restore break checking status}
    CheckBreak := SaveBreak;

    {restore cursor shape and position}
    RestoreCursorState(SaveXY, CursorSL);
  end;

begin
  {initialize command processor}
  SimpEditCommands.Init(@SimpEditKeySet, SimpEditKeyMax);
end.
