
{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$IFDEF VIRTUALPASCAL}
  !! ERROR: This unit is not compatible with Virtual Pascal !!
{$ENDIF}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPMACED.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpMacEd;
  {-Macro editor for Object Professional macros}

interface

uses
  Dos,
  Dpmi,      {!!.30}
  OpConst,   {!!.20}
  OpInline,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpRoot,
  OpString, {!!.01}
  OpCmd,
  OpInt,
  OpMacro;

  {$I OPMACED.ICD}  {configuration data}

const
 NonMacroKey = $FFFF;        {if passed to EditKeys, Key is not translated into
                              a string and displayed while editing}
var
  TempMacro : MacroRec;      {temporary macro used for editing}

function EditKeys(Key : Word; var Macro : MacroRec;
                  MinCol, MinRow, MaxCol, MaxRow,
                  Dim, Bright : Byte;
                  var Modified : Boolean) : MacroRecPtr;
  {-Edit the Macro associated with Key.}

var
  MacEdCmd : Word;
  MacEdCommands : CommandProcessor; {command processor used for macro editor}

  {==========================================================================}

implementation

const
  MaxScanCode = $A500;       {scan code for Alt-Tab}
var
  KnownKeyPtr,               {key whose location is known}
  KnownRow,                  {row for KnownKeyPtr}
  KnownCol,                  {column for KnownKeyPtr}
  LoCol,                     {dimensions for the editing window}
  HiCol,
  LoRow,
  HiRow,
  RowAtTop,                  {row currently displayed at top of window}
  RowAtBottom : Word;        {row currently displayed at bottom of window}
  RegKeyLength,
  AuxKeyLength : array[0..255] of Byte;

  {$L OPMACED.OBJ}

  {$F-}
  procedure ComputeScreenPos(KeyPtr : Word; var Row, Col : Word); external;
    {-Compute the Row and Col positions for KeyPtr}
  {$F+}

  function EditKeys(Key : Word; var Macro : MacroRec;
                    MinCol, MinRow, MaxCol, MaxRow,
                    Dim, Bright : Byte;
                    var Modified : Boolean) : MacroRecPtr;
    {-Edit the Macro associated with Key}
  const
    ScrollMask = $10;        {mask for scroll lock bit}
  var
    ChWord : Word;
    Ch : Char absolute ChWord;
    KbFlag : ^Byte {absolute $40 : $17}; {!!.30}
    CurRow, CurCol, Width : Word;
    ScrollLock, LastScroll : Boolean;
    Done, Inserting : Boolean;
    KeyPtr : Word;
    KeyName : string[12];
    BlankLine : string[255]; {!!.22}
    SaveTextAttr : Byte;
    RP, CP : Word;
    SaveXY, SaveSL : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}

    procedure ClrEol(Row, Col : Byte);
      {-Clear to end of line}
    {$IFDEF UseMouse}
    var
      SaveMouse : Boolean;
    {$ENDIF}
    begin
      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}
      BlankLine[0] := Chr(Succ(HiCol-Col));
      FastWrite(BlankLine, Row, Col, Dim);
      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;

    procedure ClrEow(Row, Col : Byte);
      {-Clear to end of window}
    {$IFDEF UseMouse}
    var
      SaveMouse : Boolean;
    {$ENDIF}
    begin
      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}
      ClrEol(Row, Col);
      for Row := Succ(Row) to HiRow do
        ClrEol(Row, LoCol);
      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;

    procedure ShowCommandMode;
      {-Indicate whether we're in Command or Literal mode}
    {$IFDEF UseMouse}
    var
      SaveMouse : Boolean;
    {$ENDIF}
    begin
      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}
      FastWrite(emComLit[ScrollLock], Pred(LoRow), HiCol-21, Dim);
      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;

    procedure ShowInsertMode;
      {-Indicate whether we're in Insert or Overtype mode}
    const
      InsertBit  = $80;
    var
      KeyboardFlags : Word absolute $40 : $17;
      {$IFDEF UseMouse}
      SaveMouse : Boolean;
      {$ENDIF}
    begin
      if Inserting then begin
        SetCursorType(InsertCursor);
        SetFlag(KeyboardFlags, InsertBit);
      end
      else begin
        SetCursorType(OvertypeCursor);
        ClearFlag(KeyboardFlags, InsertBit);
      end;

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}
      FastWrite(emInsOver[Inserting], Pred(LoRow), HiCol-10, Dim);
      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;

    procedure DisplayKeys(Row, Col : Word; OneRow : Boolean; KeyPtr : Word);
      {-Write the macro to the screen}
    var
      Attr : Byte;
      S : string[11];
      Special : Boolean;
      {$IFDEF UseMouse}
      SaveMouse : Boolean;
      {$ENDIF}
    begin
      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}
      if (Row < RowAtTop) then begin
        ScrollWindowDown(LoCol, LoRow, HiCol, HiRow, 1);
        Dec(RowAtTop);
        Dec(RowAtBottom);

        {point to first key on the row}
        ComputeScreenPos(KeyPtr, RP, CP);
        while (RP = Row) and (KeyPtr > 1) do begin
          Dec(KeyPtr);
          ComputeScreenPos(KeyPtr, RP, CP);
        end;
      end
      else
        if (Row > RowAtBottom) then begin
          ScrollWindowUp(LoCol, LoRow, HiCol, HiRow, 1);
          Inc(RowAtTop);
          Inc(RowAtBottom);

          {point to first key on the row}
          ComputeScreenPos(KeyPtr, RP, CP);
          while (RP = Row) and (KeyPtr > 1) do begin
            Dec(KeyPtr);
            ComputeScreenPos(KeyPtr, RP, CP);
          end;
        end;

      while (KeyPtr <= TempMacro.NumKeys) do begin
        {convert the key to a string}
        KeyToString(TempMacro.KeyArray[KeyPtr], S, Special);
        Inc(KeyPtr);

        {Check for wrap}
        if (Col+Length(S) > HiCol) then begin
          {String starts on next row}
          ClrEol(Row-(RowAtTop-LoRow), Col);
          Inc(Row);
          if (Row > RowAtBottom) or OneRow then
            Exit
          else
            Col := LoCol;
        end;
        if Special then
          Attr := Bright
        else
          Attr := Dim;

        FastWrite(S, Row-(RowAtTop-LoRow), Col, Attr);
        Inc(Col, Length(S));
      end;
      ClrEow(Row-(RowAtTop-LoRow), Col);
      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;

    procedure InsertKey(Key : Word; var KeyPtr : Word);
      {-Insert a key into the current macro at KeyPtr}
    var
      NextRP, NextCP : Word;
      {$IFDEF UseMouse}
      SaveMouse : Boolean;
      {$ENDIF}
    begin
      with TempMacro do begin
        if ((NumKeys = MaxKeysInMacro) and Inserting) or (Key > MaxScanCode) then
          Exit;

        if not(Inserting) or (KeyPtr > NumKeys) then begin
          KeyArray[KeyPtr] := Key;
          if KeyPtr > NumKeys then begin
            NumKeys := KeyPtr;
            KeyArray[Succ(NumKeys)] := EndOfMacro;
          end;
        end
        else begin
          Inc(NumKeys);
          MoveFast(KeyArray[KeyPtr], KeyArray[Succ(KeyPtr)],
            Succ(NumKeys-KeyPtr) shl 1); {!!.01}
          KeyArray[KeyPtr] := Key;
        end;

        {compute positions of current key and next key}
        ComputeScreenPos(KeyPtr, RP, CP);
        ComputeScreenPos(Succ(KeyPtr), NextRP, NextCP);

        {scroll window if key we're about to point to is off the screen}
        if (NextRP > RowAtBottom) then begin
          {$IFDEF UseMouse}
          HideMousePrim(SaveMouse);
          {$ENDIF}
          ScrollWindowUp(LoCol, LoRow, HiCol, HiRow, 1);
          {$IFDEF UseMouse}
          ShowMousePrim(SaveMouse);
          {$ENDIF}

          Inc(RowAtTop);
          Inc(RowAtBottom);
        end;

        DisplayKeys(RP, CP, False, KeyPtr);
        if KeyPtr < MaxKeysInMacro then
          Inc(KeyPtr);
        Modified := True;
      end;
    end;

    procedure DeleteKey(KeyPtr : Word);
      {-Delete the key at KeyPtr}
    begin
      with TempMacro do begin
        if NumKeys = 0 then
          Exit;
        if KeyPtr < NumKeys then
          MoveFast(KeyArray[Succ(KeyPtr)], KeyArray[KeyPtr],
            Succ(NumKeys-KeyPtr) shl 1); {!!.01}
        KeyArray[NumKeys] := EndOfMacro;
        Dec(NumKeys);
        ComputeScreenPos(KeyPtr, RP, CP);
        DisplayKeys(RP, CP, False, KeyPtr);
        Modified := True;
      end;
    end;

    procedure IncKeyPtr(var KeyPtr : Word);
      {-Move one key to the right}
    begin
      if KeyPtr <= TempMacro.NumKeys then begin
        Inc(KeyPtr);
        ComputeScreenPos(KeyPtr, RP, CP);
        if RP > RowAtBottom then
          DisplayKeys(RP, LoCol, True, KeyPtr);
      end;
    end;

    procedure DecKeyPtr(var KeyPtr : Word);
      {-Move one key to left}
    begin
      if KeyPtr > 1 then begin
        Dec(KeyPtr);
        ComputeScreenPos(KeyPtr, RP, CP);
        if RP < RowAtTop then
          DisplayKeys(RP, LoCol, True, KeyPtr);
      end;
    end;

  begin
    Modified := False;
    TempMacro := Macro;
    SaveTextAttr := TextAttr;
    TextAttr := Dim;

    {save cursor state}
    GetCursorState(SaveXY, SaveSL);

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw a frame around the window}
    FrameWindow(MinCol, MinRow, MaxCol, MaxRow, Bright, Bright, '');

    {show key being edited}
    if Key <> NonMacroKey then begin
      KeyToString(Key, KeyName, Done);
      FastWrite(emMacro+KeyName+' ', MinRow, MinCol+2, Dim);
    end;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    {adjust coordinates of window}
    Inc(MinCol);
    Dec(MaxCol);
    Inc(MinRow);
    Dec(MaxRow);

    {copy coordinates to data segment for fast access in subroutines}
    LoCol := MinCol;
    LoRow := MinRow;
    HiCol := MaxCol;
    HiRow := MaxRow;

    Done := False;
    Inserting := True;
    KnownKeyPtr := 1;
    KnownRow := MinRow;
    KnownCol := MinCol;

    KbFlag := Ptr(BiosDataSele, $17);              {!!.30}
    ScrollLock := (KbFlag^ and ScrollMask) <> 0;   {!!.30}
    LastScroll := not ScrollLock;
    RowAtTop := MinRow;
    RowAtBottom := MaxRow;

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {show instructions centered at bottom if possible}
    Width := Succ(MaxCol-MinCol);
    if Width >= Length(emMacedInstructions) then
      FastWrite(emMacedInstructions, Succ(MaxRow),
        (Width-Length(emMacedInstructions)) shr 1+Succ(MinCol), Dim);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    KeyPtr := 1;
    FillChar(BlankLine[1], SizeOf(BlankLine)-1, ' '); {!!.22}
    DisplayKeys(MinRow, MinCol, False, KeyPtr);

    ShowInsertMode;

    with TempMacro do
      repeat
        ComputeScreenPos(KeyPtr, RP, CP);
        GotoXYabs(CP, RP-(RowAtTop-MinRow));
        repeat
          {Watch the scroll state while waiting for a keystroke}
          ScrollLock := (KbFlag^ and ScrollMask) <> 0; {!!.30}
          if ScrollLock <> LastScroll then begin
            ShowCommandMode;
            LastScroll := ScrollLock;
          end;
        until MacEdCommands.cpKeyPressed;

        {get the next command}
        MacEdCmd := MacEdCommands.GetCommand(ChWord);

        {if in literal mode, insert the key}
        if ScrollLock then
          MacEdCmd := ccChar;

        case MacEdCmd of
          ccHelp :
            MacEdCommands.cpGetHelp(ucMaced, nil, 0);
          ccLeft :
            DecKeyPtr(KeyPtr);
          ccRight :
            IncKeyPtr(KeyPtr);
          ccUp :
            if RP = MinRow then
              KeyPtr := 1
            else begin
              CurRow := Pred(RP);
              CurCol := CP;
              KeyPtr := 1;
              repeat
                Inc(KeyPtr);
                ComputeScreenPos(KeyPtr, RP, CP);
              until ((RP = CurRow) and (CP >= CurCol)) or (RP > CurRow);
              if (CP > CurCol) or (RP > CurRow) then begin
                Dec(KeyPtr);
                ComputeScreenPos(KeyPtr, RP, CP);
                {don't go up more than one row}
                if RP < CurRow then begin
                  Inc(KeyPtr);
                  ComputeScreenPos(KeyPtr, RP, CP);
                end;
              end;
              if RP < RowAtTop then
                DisplayKeys(RP, MinCol, True, KeyPtr);
            end;
          ccDown :
            begin
              CurRow := RP;
              CurCol := CP;
              repeat
                IncKeyPtr(KeyPtr);
                ComputeScreenPos(KeyPtr, RP, CP);
              until (KeyPtr > NumKeys) or ((RP > CurRow) and (CP >= CurCol));
            end;
          ccIns :
            begin
              Inserting := not Inserting;
              ShowInsertMode;
            end;
          ccDel :
            if KeyPtr <= NumKeys then
              DeleteKey(KeyPtr);
          ccBack :
            if KeyPtr > 1 then begin
              DecKeyPtr(KeyPtr);
              DeleteKey(KeyPtr);
            end;
          ccQuit :
            begin
              {Restore TempMacro to original}
              TempMacro := Macro;
              Modified := False;
              Done := True;
            end;
          ccSelect, ccUser0..ccUser65335 :
            Done := True;
          ccDelLine :
            begin
              Modified := True;
              KeyArray[1] := EndOfMacro;
              NumKeys := 0;
              KeyPtr := 1;
              RowAtTop := MinRow;
              RowAtBottom := MaxRow;
              DisplayKeys(MinRow, MinCol, False, KeyPtr);
            end;
          else
            InsertKey(ChWord, KeyPtr);
        end;
      until Done;

    TextAttr := SaveTextAttr;
    RestoreCursorState(SaveXY, SaveSL);
    EditKeys := @TempMacro;
  end;

  procedure SetupKeyLength; {!!.11} {rewritten}
    {-Define lengths of text key representations}
  var
    I : Byte;
  begin
    {Default length for ASCII characters}
    FillChar(RegKeyLength, SizeOf(RegKeyLength), 1);

    {Control characters}
    FillChar(RegKeyLength[1], 31, 4);

    {Special cases}
    RegKeyLength[127] := 7;
    RegKeyLength[255] := 6;

    {Extended scan codes}
    for I := 0 to 255 do
      AuxKeyLength[I] := 2+Length(EscapeSequence(I)^);
  end;

begin
  {initialize command processor}
  MacEdCommands.Init(@MacEdKeySet, MacEdKeyMax);
  SetupKeyLength;
end.
