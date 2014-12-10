{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPMEMO.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1988, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpMemo;
  {-Memo field editor}


interface

uses
  Use32,
  {$IFDEF VirtualPascal}
  VpSysLow,
  VPUtils,
  {$ENDIF}
  Dpmi,       {!!.20}
  Dos,
  OpConst,    {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd,
  OpFrame,
  OpWindow
  {$IFDEF UseDrag} {!!.03}
  , OpDrag         {!!.03}
  {$ENDIF}         {!!.03}
  ;

  {$I OPMEMO.ICD}  {configuration data}

const
  {option codes}
  meInsert         = $00000001;  {True if in insert mode}
  meIndent         = $00000002;  {True if in auto-indent mode}
  meReadOnly       = $00000004;  {True if in read-only mode}
  meWordWrap       = $00000008;  {True if word wrap is on}
  meDeleteJoins    = $00000010;  {True if <Del> can join lines}
  meModified       = $00000020;  {True if edits have been made}
  meIndentIsPara   = $00000040;  {indent starts paragraph?}
  meMousePage      = $00000080;  {clicking on scroll bar scrolls by page}
  meAllowTrunc     = $00000100;  {read partial files?}
  meMapCtrls       = $00000200;  {map control characters?}
  meMakeBackups    = $00000400;  {make backup files?}
  meStreamReload   = $00000800;  {reload previous file when activating a
                                  memofile stored in a stream}
  meReformatting   = $00001000;  {flag set while reformatting}
  meNewFile        = $00002000;  {new file loaded}
  meDeallocate     = $00004000;  {did OPMEMO allocate the buffer?}
  meWrapAtLeft     = $00008000;  {wrap to prev line on <Left> at column 1}
  meNoCtrlZ        = $00010000;  {suppress ^Z when writing to disk}
  meInUpdate       = $00400000;  {correct for bad meTotalBytes val}   {!!.22}
  meInProcess      = $00800000;  {internal flag set while in Process} {!!.03}

  {default options}
  DefMemoOptions   : LongInt = meInsert+meIndent+meWordWrap+meMapCtrls+
                               meMousePage+meMakeBackups;
                                                         {!!.03}
  BadMemoOptions   : LongInt = meModified+meReformatting+meInUpdate+meInProcess;
                                                         {!!.22}
  DefTabDelta      : Byte = 8;
  DefMaxLength     : Byte = 254;
  meWordDelims     : CharSet = [^I, ' '..'"', '.', ',', ':', ';', '?', '!',
                                '*','(', ')', '[', ']', '{', '}', '<', '>',
                                '+', '-', '/', '\', '''', '$', '=', '^', '#'];

type
  MemoBuf = array[1..65521] of Char;
  MemoBufPtr = ^MemoBuf;
  MemoPtr = ^Memo;
  MemoStatusProc = procedure (MP : MemoPtr);
  Memo =
    object(CommandWindow)
      meBufPtr : MemoBufPtr;   {pointer to text buffer}
      meBufSize : Word;        {size of buffer}
      meOptions : LongInt;     {option settings}
      meMaxLines : Integer;    {maximum number of lines}
      meMaxLength : Byte;      {!do not make larger than 254!}
      meTabDelta : Byte;       {distance between tab stops}
      meMargin : Byte;         {right margin}
      meCtrlColor : Byte;      {control characters - color}
      meCtrlMono : Byte;       {control characters - mono}
      meStatusProc : MemoStatusProc; {user status routine}
      {...}
      meTotalBytes : Word;     {bytes in buffer}
      meTotalLines : Integer;  {lines in buffer}
      meLineAtTop : Integer;   {line at top of edit window}
      meBufPosTop : Word;      {index into buffer for start of line at top}
      meCurLine : Integer;     {line number of current line}
      meBufPos : Word;         {index into buffer for start of current line}
      meCurCol : Byte;         {position of cursor within current line}
      meColDelta : Byte;       {for horizontal scrolling}
      meKnownLine : Integer;   {used to speed up scrolling/searching}
      meKnownOfs : Word;       {"    "  "     "  "}
      meSaveCurLine : Integer; {current line when scroll bars last drawn}
      meSaveCurCol : Byte;     {current column when scroll bars last drawn}
      {-------------------------internal variables used while editing}
      meSt : string;           {text of current line}
      meOldSt : string;        {text of current line when first loaded}
      meOldCol : Byte;         {column when cursor first moved there}
      meOldModified : Boolean; {previous state of meModified flag}
      meForceRedraw : Boolean; {need to redraw entire window?}
      meOK : Boolean;          {last operation OK?}
      meWinWidth : Byte;       {width of inner window}
      meSaveTotalLines : Integer; {meTotalLines when scroll bar last drawn}
      {...methods...}
      constructor Init(X1, Y1, X2, Y2 : Byte;
                       BufferSize : Word;
                       BufPtr : Pointer);
        {-Create a memo window}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             BufferSize : Word;
                             BufPtr : Pointer);
        {-Create a memo window with custom window options}
      destructor Done; virtual;
        {-Deallocate text buffer if appropriate}
      {...}
      procedure ProcessSelf; virtual;   {!!.01}
        {-Process editing commands}
      procedure ReinitBuffer;
        {-Reinitialize internal variables when contents of buffer have changed}
      {...}
      procedure meOptionsOn(OptionFlags : LongInt);
        {-Turn options on}
      procedure meOptionsOff(OptionFlags : LongInt);
        {-Turn options off}
      function meOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified options are on}
      procedure SetCtrlAttr(Color, Mono : Byte);
        {-Set attributes for control characters}
      {...}
      procedure SetStatusProc(MSP : MemoStatusProc);
        {-Set status procedure}
      procedure SetLineLimit(N : Integer);
        {-Set limit on number of lines}
      procedure SetTabSize(TabSize : Byte);
        {-Set size for fixed tabs}
      procedure SetRightMargin(RM : Byte);
        {-Set right margin}
      procedure SetMaxLength(Len : Byte);
        {-Set maximum line length}
      {...}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a memo from a stream}
      procedure Store(var S : IdStream);
        {-Store a memo in a stream}
    {$ENDIF}
{.Z+}
      {+++ internal methods +++}
      procedure UpdateContents; virtual;
      procedure meShowStatus; virtual;
      procedure meCallErrorRoutine(Code : Word);
      function  meFindLineIndex(LineNum : Integer) : Word;
      function  meFindLineLength(LineNum : Integer) : Word;
      function  meCalcLineNumber(var Buffer; LastIndex : Word) : Word;
      procedure meCalcLineAndCol(I : Word; var Line, Col : Integer);
      procedure meRecount(var Buffer);
      procedure meInitBufferState(var Buffer); virtual;
      procedure meGetLine(var S : string; LineNum : Integer);
      procedure meDrawLine(St : String; LineNum : Integer); virtual;
      procedure meToggleOption(OptionCode : LongInt);
      procedure meSetOption(OptionCode : LongInt; IsOn : Boolean);
      procedure meIsModified(Yes : Boolean);
      procedure meUpdateScreenAndCursor;
      procedure meRedraw;
      procedure meTrimSpaces; virtual;
      function  meInsertOK(N : LongInt) : Boolean;
      procedure meToggleInsertMode;
      procedure meDrawCurrentLine;
      procedure meSaveCurrentLine(Trim : Boolean);
      procedure meScrollDisplay(Lines : Integer);
      function  meTooManyLinesCheck : Boolean;
      procedure meMakeHole(HolePos, Size : LongInt);
      procedure meInsLinePrim(LineNum, Col : Integer);
      procedure meLoadLine(LineNum : Integer; Truncate : Boolean); virtual;
      procedure meGotoLine(LineNum : Integer; Trim : Boolean);
      procedure meDelLinePrim(LineNum : Integer);
      procedure meJoinLinePrim(LineNum : Integer);
      procedure mePutLineAtTop(LineNum : Integer; Editing : Boolean);
      function  meGetIndent(S : string) : Byte;
      function  meWordWrapPrim : Byte;
      procedure meWrapLine(Trim : Boolean);
      procedure meReformatParagraph;
      procedure meDeleteWordPrim;
      function  mePadLineToCursor : Boolean;
      procedure meJoinAtEndOfLine;
      procedure meTopOfFile;
      procedure meEndOfFile;
      procedure meReformatGlobally;
      procedure meCheckLineLimit;
      procedure meInsertChar(Ch : Char);
      procedure meNewLine(MoveCursor : Boolean);
      procedure meDeleteLine;
      procedure meDeleteEol;
      procedure mePageUp;
      procedure mePageDown;
      procedure meInsertTab(TabPos : Word);
      procedure meDoFixedTab;
      procedure meWordLeft;
      procedure meWordRight;
      procedure meDoBackspace;
      procedure meDeleteChar;
      procedure meScrollUp;
      procedure meScrollDown;
      procedure meDoCtrlChar;
      {$IFDEF UseScrollBars}
      procedure meUpdateScrollBars; virtual; {!!.14}
      {$ENDIF}
      procedure meAdjustDeltaPrim;
      procedure meAdjustDelta;
      function  meIsAlpha(Ch : Char) : Boolean;
      {$IFDEF UseMouse}
      function  meProcessMouseCommand(Cmd : Word) : Boolean; {!!.03}
      {$ENDIF}
      {Note: following are hooks for OPEDITOR--needed for block/text markers}
      procedure meCharsInserted(LineNum : Integer; StartCol : Byte; Count : Integer); virtual;
      procedure meLinesDeleted(StartLine, Count : Integer); virtual;
      procedure meLineBroken(StartLine : Integer; StartCol : Byte); virtual;
      procedure meLineJoined(LineNum : Integer); virtual;
{.Z-}
    end;

  MemoFilePtr = ^MemoFile;
  MemoFile =
    object(Memo)
      mfFileName : PathStr;   {name of file being edited}
      {...methods...}
      constructor Init(X1, Y1, X2, Y2 : Byte;
                       BufferSize : Word;
                       BufPtr : Pointer);
        {-Create a memo window for editing files}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             BufferSize : Word;
                             BufPtr : Pointer);
        {-Create a memo window for editing files}
      constructor InitAndAlloc(X1, Y1, X2, Y2 : Byte;
                               BufferSize : Word);
        {-Create a memo window for editing files and allocate a buffer}
      constructor InitCustomAndAlloc(X1, Y1, X2, Y2 : Byte;
                                     var Colors : ColorSet;
                                     Options : LongInt;
                                     BufferSize : Word);
        {-Create a memo window for editing files and allocate a buffer}
      {...}
      procedure ReadFile(FName : string; var FSize : LongInt); virtual; {!!.03}
        {-Read a file into the buffer, returning a status code}
      procedure SaveFile; virtual; {!!.03}
        {-Save the text buffer in mfFileName}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a memofile from a stream}
      procedure Store(var S : IdStream);
        {-Store a memofile in a stream}
    {$ENDIF}
{.Z+}
      {+++ internal methods +++}
      procedure mfReadFilePrim(FName : string; var FSize : LongInt;
                               ErrorPrefix : Word);
{.Z-}
    end;

{.Z+}
{$IFDEF UseStreams}
procedure MemoStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing memos}

procedure MemoFileStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing memofiles}
{$ENDIF}
{.Z-}

var
  {$IFDEF UseDrag}                 {!!.03}
  MemoCommands : DragProcessor;    {!!.03}
  {$ELSE}                          {!!.03}
  MemoCommands : CommandProcessor; {command processor used for memo editor}
  {$ENDIF}                         {!!.03}

const
  {the commands in this set are disallowed in read-only mode}
  DisallowedInReadOnlyMode : set of Byte =
   [ccChar, ccCtrlChar, ccSelect, ccInsertLine, ccBack, ccDel, ccRestore,
    ccDelEol, ccDelLine, ccDelWord, ccIns, ccTab, ccIndent, ccWordWrap,
    ccReformatP, ccReformatG,                                        {!!.10}
    ccCenterLine, ccSaveFile, ccSaveNamed, ccSaveSwitch, ccSaveExit, {!!.10}
    ccBlkCopy, ccBlkMove, ccBlkDelete, ccBlkUCase, ccBlkLCase,       {!!.10}
    ccBlkTCase, ccBlkIndent, ccBlkUnindent, ccBlkRead, ccReplace];   {!!.10}

const
  {used only by MemoStatus}
  StatusRow   : Byte = 2;    {default to second line of screen for status line}
  StatusColor : Byte = $F;   {color attribute for status line}
  StatusMono  : Byte = $F;   {mono attribute for status line}
const
  {used only by MemoError}
  ErrorRow    : Byte = 1;    {default to top line of screen for error messages}
  ErrorColor  : Byte = $F;   {color attribute for error message line}
  ErrorMono   : Byte = $F;   {mono attribute for error message line}

{.F+}

procedure MemoStatus(MP : MemoPtr);
  {-Display status line}

procedure MemoError(UnitCode : Byte; var ErrorCode : Word; ErrorMsg : string);
  {-Display error message and wait for key press}

  {==========================================================================}

implementation

const
  SafetyMargin = 2;
  CtrlZ : Char = ^Z;
  CRLF : array[1..2] of Char = ^M^J;
{$IFDEF VIRTUALPASCAL}
  SearchFailed = $FFFFFFFF;
{$ELSE}
  SearchFailed = $FFFF;
{$ENDIF}

{$IFNDEF VIRTUALPASCAL}
  {$L OPMEMO.OBJ}
{$ENDIF}

  {$I OPMEMO.IN1}  {misc. Memo methods, MemoFiles, streams}

  function Scan(Limit : Integer; Chr : Char; T : Pointer) : Integer;
    {-Scan limit chars for Ch; Ch not found if Result=Limit}
{$IFDEF VIRTUALPASCAL}
    assembler; {$FRAME+} {$USES ecx,edx,edi}
    asm
      cld
      mov   al,Chr
      mov   ecx,Limit
      or    ecx,ecx
      pushf
      jns   @x1
      neg   ecx
      std
    @x1:
      mov   edx,ecx
      mov   edi,T
      repne scasb
      jne   @x2
      inc   ecx
    @x2:
      sub   edx,ecx
      mov   eax,edx
      popf
      jns   @x3
      neg   eax
    @x3:
    end;
{$ELSE}
    external;
{$ENDIF}

  procedure MemoStatus(MP : MemoPtr);
    {-Display status line}
  const
    OnOff : array[Boolean] of string[3] = ('Off', 'On ');
    Save : array[Boolean] of string[4] = ('    ', 'SAVE');
    StatusLine : string[80] =
      {         1         2         3         4         5         6         7         8}
      {12345678901234567890123456789012345678901234567890123456789012345678901234567890}
      ' Line: xxxxx  Column: xxx  100%  Insert: Off  Indent: Off  Word wrap: Off  SAVE ';
  var
    S : string[5];
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    with MP^ do begin
      {insert line number}
      S := Long2Str(meCurLine);
      S := Pad(S, 5);
      MoveFast(S[1], StatusLine[8], 5);  {!!.01}

      {insert column number}
      S := Long2Str(meCurCol);
      S := Pad(S, 3);
      MoveFast(S[1], StatusLine[23], 3);  {!!.01}

      {insert percentage of buffer used}
      S := Real2Str(Trunc((meTotalBytes*100.0)/(meBufSize-SafetyMargin)), 3, 0);
      MoveFast(S[1], StatusLine[28], 3);  {!!.01}

      {insert remaining fields}
      MoveFast(OnOff[meOptionsAreOn(meInsert)][1], StatusLine[42], 3); {!!.01}
      MoveFast(OnOff[meOptionsAreOn(meIndent)][1], StatusLine[55], 3); {!!.01}
      MoveFast(OnOff[meOptionsAreOn(meWordWrap)][1], StatusLine[71], 3);
      {!!.01}
      MoveFast(Save[meOptionsAreOn(meModified)][1], StatusLine[76], 4);
      {!!.01}

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      {display status line}
      FastWrite(StatusLine, StatusRow, 1, ColorMono(StatusColor, StatusMono));

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;
  end;

  procedure MemoError(UnitCode : Byte; var ErrorCode : Word; ErrorMsg : string);
    {-Display error message and wait for key press}
  var
    I : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    ErrorMsg := Pad(ErrorMsg+'. Press any key...', ScreenWidth);

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {display error message}
    FastWrite(ErrorMsg, ErrorRow, 1, ColorMono(ErrorColor, ErrorMono));

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    {flush the keyboard buffer}
    while KeyPressed do
      I := ReadKeyWord;

    {wait for key press}
    I := ReadKeyWord;

    {clear error message line}
    FastWrite(CharStr(' ', ScreenWidth), ErrorRow, 1,
              ColorMono(ErrorColor, ErrorMono));
  end;

  procedure Memo.meCallErrorRoutine(Code : Word);
    {-Call the user-defined error routine}
  var
    Msg : string[45];
  begin
    case Code mod 10000 of
      ecBufferFull  :
        Msg := emBufferFull;
      ecLineTooLong :
        Msg := emLineTooLong;
      ecTooManyLines :
        Msg := emTooManyLines;
      ecOverLineLimit :
        Msg := emOverLineLimit;
      else
        Msg := emUnknownError;
    end;
    GotError(Code, Msg);
  end;

  procedure Memo.meShowStatus;
    {-Display a status line}
  begin
    if @meStatusProc <> nil then
      if IsCurrent and (VirtualSegment = VideoSegment) then
        meStatusProc(@Self);
  end;

  constructor Memo.Init(X1, Y1, X2, Y2 : Byte;
                        BufferSize : Word;
                        BufPtr : Pointer);
    {-Create a memo window}
  begin
    {initialize using default window options}
    if not Memo.InitCustom(X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions, BufferSize, BufPtr) then
      Fail;
  end;

  constructor Memo.InitCustom(X1, Y1, X2, Y2 : Byte;
                              var Colors : ColorSet;
                              Options : LongInt;
                              BufferSize : Word; BufPtr : Pointer);
    {-Create a memo window}
  begin
    {set this in case init fails}
    meBufPtr := nil;

    {force wUserContents on}
    SetLongFlag(Options, wUserContents);

    {initialize the window}
    if not CommandWindow.InitCustom(X1, Y1, X2, Y2, Colors, Options,
                                    MemoCommands, ucMemo) then
      Fail;

    {initialize option settings, etc.}
    meOptions := DefMemoOptions;
    meMaxLines := MaxInt;
    meMaxLength := DefMaxLength;
    meTabDelta := DefTabDelta;
    meMargin := wXH-wXL;
    meOK := True;                {!!.20}

    {initialize colors}
    with Colors do begin
      meCtrlColor := CtrlColor;
      meCtrlMono := CtrlMono;
    end;

    {initialize procedure pointers}
    @meStatusProc := nil;

    {initialize meTotalLines, meTotalBytes, etc.}
    meBufPtr := BufPtr;
    meBufSize := BufferSize;
    meInitBufferState(BufPtr^);
  end;

  destructor Memo.Done;
    {-Deallocate buffers}
  begin
    {deallocate edit buffer if appropriate}
    if LongFlagIsSet(meOptions, meDeallocate) then
      FreeMemCheck(meBufPtr, meBufSize);

    {call ancestor's destructor}
    CommandWindow.Done;
  end;

  procedure Memo.meOptionsOn(OptionFlags : LongInt);
    {-Activate multiple options}
  begin
    SetLongFlag(meOptions, OptionFlags and not BadMemoOptions);
  end;

  procedure Memo.meOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple options}
  begin
    ClearLongFlag(meOptions, OptionFlags and not BadMemoOptions);
  end;

  function Memo.meOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified options are on}
  begin
    meOptionsAreOn := (meOptions and OptionFlags = OptionFlags);
  end;

  procedure Memo.SetCtrlAttr(Color, Mono : Byte);
    {-Set attributes for control characters}
  begin
    meCtrlColor := Color;
    meCtrlMono := MapMono(Color, Mono);
  end;

  procedure Memo.SetStatusProc(MSP : MemoStatusProc);
    {-Set status procedure}
  begin
    meStatusProc := MSP;
  end;

  procedure Memo.SetLineLimit(N : Integer);
    {-Set limit on number of lines}
  begin
    meMaxLines := N;
  end;

  procedure Memo.SetTabSize(TabSize : Byte);
    {-Set size for fixed tabs}
  begin
    meTabDelta := TabSize;
  end;

  procedure Memo.SetRightMargin(RM : Byte);
    {-Set right margin}
  begin
    meMargin := RM;
  end;

  procedure Memo.SetMaxLength(Len : Byte);
    {-Set maximum line length}
  begin
    if Len > 254 then
      meMaxLength := 254
    else
      meMaxLength := Len;
  end;

  procedure Memo.meCharsInserted(LineNum : Integer; StartCol : Byte; Count : Integer);
    {-Dummy routine}
  begin
  end;

  procedure Memo.meLinesDeleted(StartLine, Count : Integer);
    {-Dummy routine}
  begin
  end;

  procedure Memo.meLineBroken(StartLine : Integer; StartCol : Byte);
    {-Dummy routine}
  begin
  end;

  procedure Memo.meLineJoined(LineNum : Integer);
    {-Dummy routine}
  begin
  end;

  function Memo.meCalcLineNumber(var Buffer; LastIndex : Word) : Word;
    {-Calculate the line number corresponding to LastIndex}
  var
    I, J, Line : Word;
    Buf : MemoBuf absolute Buffer;
  begin
    Line := 1;
    I := 1;
    repeat
      J := Search(Buf[I], Succ(LastIndex-I), CRLF, 2);
      if J <> SearchFailed then begin
        Inc(Line);
        Inc(I, J+2);
      end;
    until (J = SearchFailed) or (I >= LastIndex);
    meCalcLineNumber := Line;
  end;

  procedure Memo.meCalcLineAndCol(I : Word; var Line, Col : Integer);
    {-Calculate a line number and column corresponding to index I}
  begin
    Line := meCalcLineNumber(meBufPtr^, I);
    Col := I-meFindLineIndex(Line)+1;
  end;

  procedure Memo.meRecount(var Buffer);
    {-Count total lines, total bytes, etc.}
  var
    I, J : Word;
    Buf : MemoBuf absolute Buffer;
  begin
    {initialize edit buffer state variables}
    meKnownLine := 1;
    meKnownOfs := 1;
    meSaveTotalLines := 0;

    {find end of text buffer}
    I := Search(Buffer, meBufSize, CtrlZ, 1);

    if (I = SearchFailed) or (I = 0) then begin
      {buffer is empty}
      meTotalBytes := 1;
      meTotalLines := 1;
      Buf[1] := CtrlZ;
    end
    else begin
      meTotalBytes := I+1;

      {count total number of lines}
      meTotalLines := meCalcLineNumber(Buffer, meTotalBytes);
    end;
  end;

  procedure Memo.meInitBufferState(var Buffer);
    {-Initialize the edit buffer status fields}
  begin
    {initialize edit buffer state variables}
    ClearLongFlag(meOptions, meModified);
    meOldModified := False;
    meLineAtTop := 1;
    meBufPosTop := 1;
    meCurLine := 1;
    meBufPos := 1;
    meCurCol := 1;
    meColDelta := 0;
    meSaveCurLine := 1;
    meSaveCurCol := 1;
    meForceRedraw := True;

    {count total lines, total bytes, etc.}
    meRecount(Buffer);

    {load line 1 but don't report errors}
    SetLongFlag(meOptions, meReformatting);
    meLoadLine(1, True);
    ClearLongFlag(meOptions, meReformatting);
  end;

  procedure Memo.ReinitBuffer;
    {-Reinitialize internal variables when contents of buffer have changed}
  begin
    meInitBufferState(meBufPtr^);
  end;

  function Memo.meFindLineIndex(LineNum : Integer) : Word;
    {-Return the index into the edit buffer for the specified line number}
  var
    I : Integer;
  begin
    if LineNum = 1 then begin
      meKnownLine := 1;
      meKnownOfs := 1;
    end
    else begin
      if (LineNum < meKnownLine div 2) then begin
        meKnownLine := 1;
        meKnownOfs := 1;
      end;
      if LineNum >= meKnownLine then
        while meKnownLine < LineNum do begin
          I := Succ(meTotalBytes-meKnownOfs);
          if I < 0 then
            I := MaxInt;
          Inc(meKnownOfs, Succ(Scan(I, ^J, @meBufPtr^[meKnownOfs])));
          if meBufPtr^[meKnownOfs-2] = ^M then
            Inc(meKnownLine);
        end
      else begin
        {linenum < knownline, search backwards}
        Dec(meKnownOfs, 2);
        while meKnownLine > LineNum do begin
          I := meKnownOfs;
          if I < 0 then
            I := MaxInt;
          Inc(Integer(meKnownOfs), Pred(Scan(-I, ^J, @meBufPtr^[meKnownOfs])));
          if meBufPtr^[meKnownOfs] = ^M then
            Dec(meKnownLine);
        end;

        {point to start of next line}
        Inc(meKnownOfs, 2);
      end;
    end;

    meFindLineIndex := meKnownOfs;
  end;

  function Memo.meFindLineLength(LineNum : Integer) : Word;
    {-Find the length of the specified line}
  var
    I, J : Word;
  begin
    if LineNum > meTotalLines then
      meFindLineLength := 0
    else begin
      {find starting index for line}
      J := meFindLineIndex(LineNum);

      {calculate length}
      I := Search(meBufPtr^[J], Succ(meTotalBytes-J), CRLF, 2);
      if I = SearchFailed then
        meFindLineLength := meTotalBytes-J
      else
        meFindLineLength := I;
    end;
  end;

  procedure Memo.meGetLine(var S : string; LineNum : Integer);
    {-Get the LineNum'th line from the buffer for the specified control block,
      and store it in S}
  var
    I, J : Word;
    SLen : Byte absolute S;
  begin
    if LineNum > meTotalLines then
      SLen := 0
    else begin
      {find starting index and length for line}
      J := meFindLineIndex(LineNum);
      I := meFindLineLength(LineNum);

      {truncate if line is too long}
      if I > 254 then
        SLen := 254
      else
        SLen := I;

      MoveFast(meBufPtr^[J], S[1], SLen);  {!!.01}
    end;
  end;

  procedure Memo.meDrawLine(St : String; LineNum : Integer);
    {-Draw the string St, which represents the specified line number}
  var
    StLen : Byte absolute St;
    TA, CA : Byte;
  begin
    {calculate screen row}
    Dec(LineNum, Pred(meLineAtTop));
    Inc(LineNum, Pred(wYL));

    {adjust for ColDelta}
    if (meColDelta > 0) and (StLen > 0) then
      if meColDelta >= StLen then
        StLen := 0
      else begin
        MoveFast(St[meColDelta+1], St[1], StLen-meColDelta);  {!!.01}
        Dec(StLen, meColDelta);
      end;

    {pad the end of the string}
    if StLen < meWinWidth then
      FillChar(St[Succ(StLen)], meWinWidth-StLen, ' ');

    {change the length}
    StLen := meWinWidth;

    {draw the string}
    TA := ColorMono(wTextColor, wTextMono);
    if LongFlagIsSet(meOptions, meMapCtrls) then begin
      CA := ColorMono(meCtrlColor, meCtrlMono);
      FastWriteCtrl(St, LineNum, wXL, TA, CA);
    end
    else
      FastWrite(St, LineNum, wXL, TA)
  end;

  procedure Memo.UpdateContents;
    {-Redraw the complete memo window}
  var
    I, J : Integer;
    F : Word;                                                      {!!.22}
    S : String;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    if LongFlagIsSet(meOptions, meInUpdate) then begin             {!!.22}
      {adjust for possible call via DeactivateWrite}               {!!.22}
      F := meTotalBytes;                                           {!!.22}
      Dec(meTotalBytes, Integer(Length(meSt)-Length(meOldSt)));    {!!.22}
    end;

    meAdjustDelta;

    J := meLineAtTop+(wYH-wYL);
    for I := meLineAtTop to J do begin
      if (I = meCurLine) and LongFlagIsSet(meOptions, meInProcess) then {!!.03}
        S := meSt                                                       {!!.03}
      else                                                              {!!.03}
        meGetLine(S, I);
      meDrawLine(S, I);
    end;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    {make sure scroll bars are scaled properly}
    meSaveTotalLines := -1;

    {$IFDEF UseScrollBars}
    meUpdateScrollBars;
    {$ENDIF}

    if LongFlagIsSet(meOptions, meInUpdate) then                  {!!.22}
      meTotalBytes := F;                                          {!!.22}

    StackWindow.UpdateContents; {!!.01}
  end;

  {$IFDEF UseScrollBars}
  procedure Memo.meUpdateScrollBars;
    {-Update horizontal and vertical scroll bars}
  begin
    if meTotalLines <> meSaveTotalLines then begin
      ChangeAllScrollBars(0, Word(meMaxLength)+1-meWinWidth, 1, meTotalLines);
      meSaveTotalLines := meTotalLines;
    end;
    DrawAllSliders(meColDelta, meCurLine);
    meSaveCurLine := meCurLine;
    meSaveCurCol := meCurCol;
  end;
  {$ENDIF}

  procedure Memo.meToggleOption(OptionCode : LongInt);
    {-Toggle the specified option flag}
  begin
    if LongFlagIsSet(meOptions, OptionCode) then
      ClearLongFlag(meOptions, OptionCode)
    else
      SetLongFlag(meOptions, OptionCode);
  end;

 procedure Memo.meSetOption(OptionCode : LongInt; IsOn : Boolean);
    {-Set the specified option based on IsOn}
  begin
    if IsOn then
      SetLongFlag(meOptions, OptionCode)
    else
      ClearLongFlag(meOptions, OptionCode);
  end;

  procedure Memo.meIsModified(Yes : Boolean);
    {-Set modified flag}
  begin
    meSetOption(meModified, Yes);
  end;

  procedure Memo.meRedraw;
    {-Redraw the complete memo window}
  var
    I, J : Integer;
    S : String;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    if LongFlagIsSet(meOptions, meReformatting) then
      Exit;

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {reset known line}
    meKnownLine := meLineAtTop;
    meKnownOfs := meBufPosTop;

    J := meLineAtTop+(wYH-wYL);
    for I := meLineAtTop to J do
      if (I = meCurLine) then
        meDrawLine(meSt, I)
      else begin
        meGetLine(S, I);
        meDrawLine(S, I);
      end;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    {$IFDEF UseScrollBars}
    meUpdateScrollBars;
    {$ENDIF}

    meForceRedraw := False;
  end;

  procedure Memo.meTrimSpaces;
    {-Trim trailing blanks from current line}
  var
    StLen : Byte absolute meSt;
    I : Integer;
  begin
    if meSt[StLen] = ' ' then begin
      I := StLen;
      while meSt[StLen] = ' ' do
        Dec(StLen);
      meCharsInserted(meCurLine, StLen+1, Integer(StLen)-I);
    end;
  end;

  function Memo.meInsertOK(N : LongInt) : Boolean;
    {-Return True if OK to insert N bytes into the edit buffer. Calls user
      error handler if not OK.}
  var
    I : LongInt;
    StLen : Byte absolute meSt;
  begin
    {allow a safety margin}
    I := meTotalBytes+SafetyMargin;

    {calculate actual meTotalBytes+N}
    Inc(I, N+(LongInt(StLen)-Length(meOldSt)));

    if I <= meBufSize then
      meInsertOK := True
    else begin
      meInsertOK := False;
      meCallErrorRoutine(epWarning+ecBufferFull);
    end;
  end;

  procedure Memo.meToggleInsertMode;
    {-Toggle between insert and overtype mode, keeping BIOS keyboard flag up
      to date}
  var
    BiosKbdFlag : ^Byte; {absolute $0040 : $0017}                     {!!.20}
  begin
    {$IFDEF VIRTUALPASCAL}
    {toggle insert flag}
    meToggleOption(meInsert);

    {use hidden cursor if on virtual screen}
    if VirtualSegment <> VideoSegment then
      SetCursor(cuHidden)
    {use fat cursor if inserting}
    else
    if LongFlagIsSet(meOptions, meInsert) then
      begin
        SetCursor(InsertCursor);
//!!!        SetKeyboardState( kbd_Insert, True );
      end
    else
      begin
        SetCursor(OvertypeCursor);
//!!!        SetKeyboardState( kbd_Insert, False );
      end;
    {$ELSE}
    BiosKbdFlag := Ptr(BiosDataSele, $17);                            {!!.20}
    {toggle insert flag}
    meToggleOption(meInsert);

    {use hidden cursor if on virtual screen}
    if VirtualSegment <> VideoSegment then
      SetCursor(cuHidden)
    {use fat cursor if inserting}
    else if LongFlagIsSet(meOptions, meInsert) then begin
      SetCursor(InsertCursor);
      BiosKbdFlag^ := BiosKbdFlag^ or $80;                             {!!.20}
    end
    else begin
      SetCursor(OvertypeCursor);
      BiosKbdFlag^ := BiosKbdFlag^ and $7F;                            {!!.20}
    end;
    {$ENDIF}
  end;

  procedure Memo.meDrawCurrentLine;
    {-Draw the current line}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    if LongFlagIsSet(meOptions, meReformatting) then
      Exit;

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw the current line}
    meDrawLine(meSt, meCurLine);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure Memo.meMakeHole(HolePos, Size : LongInt);
    {-Make a hole of the specified size}
  begin
    if Size > 0 then
      {make room for new text}
      MoveFast(meBufPtr^[HolePos], meBufPtr^[HolePos+Size],
      Succ(meTotalBytes-HolePos)) {!!.01}
    else if Size < 0 then
      {delete characters}
      MoveFast(meBufPtr^[HolePos-Size], meBufPtr^[HolePos],
      Succ(meTotalBytes-HolePos)+Size); {!!.01}
    Inc(meTotalBytes, Size);
  end;

  procedure Memo.meSaveCurrentLine(Trim : Boolean);
    {-Patch the current line back into place}
  var
    I, J : Word;
    K : Integer;
    StLen : Byte absolute meSt;
  begin
    {reset known line}
    meKnownLine := meCurLine;
    meKnownOfs := meBufPos;

    if Trim then
      meTrimSpaces;
    if meSt = meOldSt then
      Exit;

    {find the actual length of the current line}
    I := meBufPos;
    J := meFindLineLength(meCurLine);

    {calculate difference in size}
    K := Integer(StLen)-J;

    {make a hole for the text}
    meMakeHole(I, K);

    {insert the text}
    MoveFast(meSt[1], meBufPtr^[I], StLen);  {!!.01}

    meOldSt := meSt;
    meIsModified(True);
    meOldModified := True;
  end;

  procedure Memo.meScrollDisplay(Lines : Integer);
    {-Scroll the editing window up or down}
  var
    S : string;
    SaveTextAttr : Byte;
    I, J, K : Integer;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    if Lines = 0 then
      Exit;

    SaveTextAttr := TextAttr;
    TextAttr := ColorMono(wTextColor, wTextMono);

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    Inc(meLineAtTop, Lines);
    meBufPosTop := meFindLineIndex(meLineAtTop);

    meAdjustDeltaPrim;
    if not LongFlagIsSet(meOptions, meReformatting) then
      if meForceRedraw or (Word(Succ(wYH-wYL)) < Abs(Lines)) then
        meRedraw
      else begin
        if Lines < 0 then begin
          ScrollWindowDown(wXL, wYL, wXH, wYH, -Lines);
          J := meLineAtTop;
          K := Pred(J-Lines);
        end
        else begin
          ScrollWindowUp(wXL, wYL, wXH, wYH, Lines);
          J := meLineAtTop+(wYH-wYL)-Pred(Lines);
          K := Pred(J+Lines);
        end;

        {draw the line(s) replacing the one(s) that scrolled off}
        for I := J to K do begin
          meGetLine(S, I);
          meDrawLine(S, I);
        end;
      end;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    TextAttr := SaveTextAttr;
  end;

  function Memo.meTooManyLinesCheck : Boolean;
    {-Check to see if there are too many lines}
  begin
    if Word(meTotalLines) >= Word(meMaxLines) then begin
      meCallErrorRoutine(epWarning+ecTooManyLines);
      meOK := False;
      meTooManyLinesCheck := True;
    end
    else
      meTooManyLinesCheck := False;
  end;

  procedure Memo.meInsLinePrim(LineNum, Col : Integer);
    {-Primitive routine to insert a line break}
  var
    I, J : Word;
    Delta : Integer;
  begin
    if meTooManyLinesCheck then
      Exit;

    {find the place to insert the line break}
    I := meFindLineIndex(LineNum)+Pred(Col);

    {see if we need to trim some blanks}
    J := Pred(I);
    while (J > 0) and (meBufPtr^[J] = ' ') do
      Dec(J);
    Inc(J);

    {see if there's room}
    Delta := I-J;
    meOK := meInsertOK(2-Delta);
    if not meOK then
      Exit;

    {we broke a line}
    meLineBroken(LineNum, Col);

    {we inserted some characters}
    meCharsInserted(LineNum, Col-Delta, -Delta);

    {make room for a CRLF}
    meMakeHole(J, 2-Delta);

    {insert the CRLF}
    MoveFast(CRLF, meBufPtr^[J], 2);  {!!.01}

    {adjust line counter}
    Inc(meTotalLines);

    meIsModified(True);
    meOldModified := True;
  end;

  procedure Memo.meLoadLine(LineNum : Integer; Truncate : Boolean);
    {-Load the specified line}
  var
    I, J, K, N, Max : Word;
    StLen : Byte absolute meSt;
  begin
    {find the line we're moving to}
    meBufPos := meFindLineIndex(LineNum);
    meCurLine := LineNum;

    {find the length of the line}
    I := meFindLineLength(LineNum);

    {calc max length of line}
    if Truncate then
      Max := meMaxLength
    else
      Max := 255;

    {insert carriage return if line is too long}
    if I > Max then begin
      {determine where to break the line}
      K := Max;
      N := meFindLineIndex(LineNum);
      J := N+Pred(K);
      while (J > N) and (meBufPtr^[J] <> ' ') do begin
        Dec(J);
        Dec(K);
      end;
      if J = N then
        K := Max;

      {try to break the line}
      Inc(meMaxLines);
      meInsLinePrim(LineNum, K);
      Dec(meMaxLines);

      if not meOK then begin
        {something overflowed--force the line break}
        Inc(N, K);
        meBufPtr^[N] := ^M;
        meBufPtr^[N+1] := ^J;
        Inc(meTotalLines);
      end;

      {report the break if we're not reformatting}
      if not LongFlagIsSet(meOptions, meReformatting) then
        meCallErrorRoutine(epWarning+ecLineTooLong);

      {force screen to be redrawn}
      meForceRedraw := True;

      {recalculate the length}
      I := meFindLineLength(LineNum);
    end;

    {load the line into meSt and meOldSt}
    StLen := I;
    MoveFast(meBufPtr^[meBufPos], meSt[1], StLen);  {!!.01}
    meOldSt := meSt;
    meOldCol := meCurCol;
    meIsModified(meOldModified);
  end;

  procedure Memo.meGotoLine(LineNum : Integer; Trim : Boolean);
    {-Save the current line and move the cursor to the LineNum'th line}
  var
    I : Word;
  begin
    {don't go too far}
    if LineNum > meTotalLines then
      LineNum := meTotalLines;

    {save the line we've been editing}
    meSaveCurrentLine(Trim);

    {load the line}
    meLoadLine(LineNum, Trim);

    {scroll the display if necessary}
    if LineNum < meLineAtTop then
      meScrollDisplay(LineNum-meLineAtTop)
    else begin
      I := meLineAtTop+(wYH-wYL);
      if LineNum > I then
        meScrollDisplay(LineNum-I);
    end;
  end;

  procedure Memo.meDelLinePrim(LineNum : Integer);
    {-Primitive routine to delete a line}
  var
    I, J : Word;
  begin
    {find the line we're deleting}
    I := meFindLineIndex(LineNum);

    {find the length of the line}
    J := Search(meBufPtr^[I], Succ(meTotalBytes-I), CRLF, 2);
    if J = SearchFailed then
      J := meTotalBytes-meBufPos
    else
      Inc(J, 2);

    {delete it}
    meMakeHole(I, -J);
    Dec(meTotalLines);
    if meTotalLines = 0 then begin
      meTotalLines := 1;
      meTotalBytes := 1;
      meBufPtr^[1] := ^Z;
    end;

    {we deleted a line}
    meLinesDeleted(LineNum, 1);

    {mark text as modified}
    meIsModified(True);
    meOldModified := True;
  end;

  procedure Memo.meJoinLinePrim(LineNum : Integer);
    {-Primitive routine to join two lines}
  var
    I : Word;
  begin
    {we're joining a line}
    meLineJoined(LineNum);

    {find the place to join the lines}
    I := meFindLineIndex(LineNum);

    {make room for a CRLF}
    meMakeHole(I-2, -2);

    Dec(meTotalLines);
    meBufPtr^[meTotalBytes+1] := ^Z;

    meIsModified(True);
    meOldModified := True;
  end;

  procedure Memo.mePutLineAtTop(LineNum : Integer; Editing : Boolean);
    {-Position the specified line at top of editing window}
  begin
    if LineNum < 1 then
      LineNum := 1
    else if LineNum > meTotalLines then
      LineNum := meTotalLines;
    meSaveCurrentLine(Editing);
    meBufPosTop := meFindLineIndex(LineNum);
    meLineAtTop := LineNum;
    if Editing then
      meRedraw;
  end;

  function Memo.meGetIndent(S : string) : Byte;
    {-Get the indentation level of S}
  var
    I : Word;
    SLen : Byte absolute S;
  begin
    I := 0;
    while S[SLen] = ' ' do
      Dec(SLen);
    while (I < SLen) and (S[I+1] = ' ') do
      Inc(I);
    meGetIndent := I;
  end;

  function Memo.meIsAlpha(Ch : Char) : Boolean;
    {-Returns True if Ch is not a blank or a word delimiter}
  begin
    meIsAlpha := not (Ch in meWordDelims);
  end;

  procedure Memo.meDeleteWordPrim;
    {-Primitive routine to delete a word}
  type
    CharClass = (Blank, Alpha, Other);
  var
    Startclass : CharClass;
    DelEnd : Word;
    StLen : Byte absolute meSt;
    I : Integer;

    function GetClass(Ch : Char) : CharClass;
      {-Classify a character}
    begin
      if Ch = ' ' then
        GetClass := Blank
      else if (Ch in meWordDelims) then
        GetClass := Other
      else
        GetClass := Alpha;
    end;

  begin
    if meCurCol > StLen then
      Exit;

    {start deleting at the cursor}
    DelEnd := meCurCol;

    if meSt[meCurCol] <> ' ' then begin
      {In a word or on a delimiter -- delete to next space}
      StartClass := GetClass(meSt[DelEnd]);
      while (GetClass(meSt[DelEnd]) = StartClass) and (DelEnd <= StLen) do
        Inc(DelEnd);
    end;

    {In white space - delete spaces}
    while (meSt[DelEnd] = ' ') and (DelEnd <= StLen) do
      Inc(DelEnd);

    {delete the word}
    I := DelEnd-meCurCol;
    Delete(meSt, meCurCol, I);

    {we deleted some characters}
    meCharsInserted(meCurLine, meCurCol, -I);
  end;

  function Memo.mePadLineToCursor : Boolean;
    {-Pad current line so that it ends just before the cursor}
  var
    StLen : Byte absolute meSt;
    I : Word;
  begin
    mePadLineToCursor := False;

    if meCurCol > StLen+1 then begin
      {can we pad the end of the line?}
      I := Pred(meCurCol-StLen);
      if (I+StLen > meMaxLength) then begin
        meCallErrorRoutine(epWarning+ecBufferFull);
        Exit;
      end
      else if not meInsertOK(I) then
        Exit;

      {pad the end of the line and save it}
      FillChar(meSt[Succ(StLen)], I, ' ');
      Inc(StLen, I);
    end;

    mePadLineToCursor := True;
    meSaveCurrentLine(False);
  end;

  procedure Memo.meJoinAtEndOfLine;
    {-Join the following line with the current one at the cursor}
  begin
    if (meCurLine < meTotalLines) then begin
      if not mePadLineToCursor then
        Exit;
      meJoinLinePrim(meCurLine+1);
      meLoadLine(meCurLine, True);
      meForceRedraw := True;
    end;
  end;

  procedure Memo.meTopOfFile;
    {-Reset for top of file}
  begin
    mePutLineAtTop(1, True);
    meGotoLine(1, True);
    meCurCol := 1;
    meOldCol := 1;
  end;

  procedure Memo.meCheckLineLimit;
    {-Display error message if line limit exceeded}
  begin
    if meTotalLines > meMaxLines then begin
      meRedraw;
      meCallErrorRoutine(epWarning+ecOverLineLimit);
    end;
  end;

  procedure Memo.meInsertChar(Ch : Char);
    {-Insert the specified character at the cursor}
  var
    StLen : Byte absolute meSt;
    InsertMode : Boolean;
    WW, WrapOK : Boolean; {!!.02}
  begin
    WW := LongFlagIsSet(meOptions, meWordWrap);                      {!!.02}
    InsertMode := LongFlagIsSet(meOptions, meInsert);                {!!.12}
    WrapOK := (Ch <> ' ') or ((StLen = meMaxLength) and InsertMode); {!!.02}
    if (meCurCol <= meMaxLength) or                                  {!!.02}
      ((meCurCol < 255) and WW and WrapOK) then begin                {!!.02}
      if (meCurCol > StLen) then
        FillChar(meSt[Succ(StLen)], meCurCol-StLen, ' ');

      {InsertMode := LongFlagIsSet(meOptions, meInsert);}            {!!.12}
      if not InsertMode then begin
        {overtype mode}
        if (meCurCol <= meMaxLength) or WW then begin {!!.02}
          meSt[meCurCol] := Ch;
          if (Ch <> ' ') and (meCurCol > StLen) then  {!!.13}
            if meInsertOK(meCurCol-StLen) then        {!!.13}
              StLen := meCurCol
            else                                      {!!.13}
              Exit;                                   {!!.13}
          Inc(meCurCol);
        end;
      end
      else if (StLen < meMaxLength) or WW then begin {!!.02}
        {insert mode}
        if meCurCol > StLen then begin
          if Ch = ' ' then
            Inc(meCurCol)
          else if meInsertOK(meCurCol-StLen) then begin
            StLen := meCurCol;
            meSt[meCurCol] := Ch;
            Inc(meCurCol);
          end;
        end
        else if meInsertOK(1) then begin
          Insert(Ch, meSt, meCurCol);
          meCharsInserted(meCurLine, meCurCol, 1);
          Inc(meCurCol);
          WrapOK := WrapOK and                    {!!.22}
                    ((meCurCol > stLen) or        {!!.22}
                     (meSt[meCurCol] <> ' ') or   {!!.22}
                     (meCurCol > meMargin + 1));  {!!.22}
        end;
      end;

      if LongFlagIsSet(meOptions, meWordWrap) then
        if (meCurCol > meMargin) and (StLen > meMargin) and WrapOK then   {!!.02}
          {if (Ch <> ' ') or ((StLen = meMaxLength) and InsertMode) then} {!!.02}
            meWrapLine(True);
    end;
  end;

  procedure Memo.meNewLine(MoveCursor : Boolean);
    {-Process <Enter> command}
  var
    I : Word;
    StLen : Byte absolute meSt;
    SaveCol : Byte;
  begin
    I := meGetIndent(meSt);
    if I >= meCurCol then
      I := 0;
    SaveCol := meCurCol;
    if LongFlagIsSet(meOptions, meInsert) or not MoveCursor then begin
      if LongFlagIsSet(meOptions, meIndent) and (meCurCol <= StLen) and (I > 0) then begin
        Insert(CharStr(' ', I), meSt, meCurCol);
        meCharsInserted(meCurLine, meCurCol, I);
      end;
      meSaveCurrentLine(True);
      if meCurCol > StLen then
        meCurCol := Succ(StLen);
      meInsLinePrim(meCurLine, meCurCol);
      meForceRedraw := True;
    end;

    if meOK then
      if not MoveCursor then begin
        {reload the current line}
        meCurCol := SaveCol;
        meLoadLine(meCurLine, True);
      end
      else begin
        {move to the new line}
        meGotoLine(meCurLine+1, True);
        if meOptionsAreOn(meInsert+meIndent) then
          meCurCol := Succ(I)
        else
          meCurCol := 1;
        meOldCol := meCurCol;
      end;
  end;

  procedure Memo.meDeleteLine;
    {-Delete line at cursor}
  var
    StLen : Byte absolute meSt;
  begin
    if meCurLine = meTotalLines then begin
      meCharsInserted(meCurLine, 1, -StLen);
      StLen := 0;
      meCurCol := 1;
      meSaveCurrentLine(True);
    end
    else begin
      meDelLinePrim(meCurLine);
      meCurCol := 1;
      meLoadLine(meCurLine, True);
      meForceRedraw := True;
    end;
  end;

  procedure Memo.mePageUp;
    {-Do page up command}
  var
    I, J : Word;
  begin
    if meLineAtTop > 1 then begin
      I := (wYH-wYL);
      if I > meCurLine then begin
        mePutLineAtTop(1, True);
        meGotoLine(1, True);
      end
      else begin
        J := meCurLine-meLineAtTop;
        mePutLineAtTop(meLineAtTop-I, True);
        meGotoLine(meLineAtTop+J, True);
      end;
    end
    else
      meGotoLine(1, True);
  end;

  procedure Memo.mePageDown;
    {-Do page down command}
  var
    I, J : Word;
  begin
    I := Succ(wYH-wYL);
    if (meTotalLines > I) or not LongFlagIsSet(meOptions, meReadOnly) then
      if meLineAtTop < meTotalLines then begin
        if meTotalLines <= I then begin
          mePutLineAtTop(meTotalLines, True);
          meGotoLine(meTotalLines, True);
        end
        else begin
          J := meCurLine-meLineAtTop;
          mePutLineAtTop(meLineAtTop+Pred(I), True);
          meGotoLine(meLineAtTop+J, True);
        end;
      end;
  end;

  procedure Memo.meEndOfFile;
    {-Go to end of file}
  var
    I : Word;
    StLen : Byte absolute meSt;
  begin
    I := wYH-wYL;
    if meCurLine < meTotalLines-I then
      mePutLineAtTop(meTotalLines-I, True);
    meGotoLine(meTotalLines, True);
    meCurCol := Succ(StLen);
    meOldCol := meCurCol;
  end;

  procedure Memo.meInsertTab(TabPos : Word);
    {-Move cursor to TabPos}
  var
    StLen : Byte absolute meSt;
    I : Integer;
  begin
    if LongFlagIsSet(meOptions, meInsert) and (meCurCol <= StLen) then begin
      if meInsertOK(TabPos-meCurCol) and (TabPos <= meMargin) and
         (meMaxLength-StLen > TabPos-meCurCol) then begin
        I := TabPos-meCurCol;
        Insert(CharStr(' ', I), meSt, meCurCol);
        meCharsInserted(meCurLine, meCurCol, I);
        meCurCol := TabPos;
      end;
    end
    else begin
      if TabPos > 255 then
        TabPos := 255;
      meCurCol := TabPos;
    end;
  end;

  procedure Memo.meDoFixedTab;
    {-Do fixed tab command}
  var
    I : Word;
  begin
    I := Succ(Succ(Pred(meCurCol) div meTabDelta) * Word(meTabDelta));
    meInsertTab(I);
  end;

  procedure Memo.meWordLeft;
    {-Cursor left one word}
  var
    StLen : Byte absolute meSt;
  begin
    if meCurCol > 1 then begin
      if meCurCol > StLen then
        meCurCol := Succ(StLen);
      Dec(meCurCol);
      while (meCurCol > 0) and not meIsAlpha(meSt[meCurCol]) do
        Dec(meCurCol);
      while (meCurCol > 0) and meIsAlpha(meSt[meCurCol]) do
        Dec(meCurCol);
      Inc(meCurCol);
    end
    else if meCurLine > 1 then begin
      meGotoLine(meCurLine-1, True);
      meCurCol := Succ(StLen);
      meOldCol := meCurCol;
    end;
  end;

  procedure Memo.meWordRight;
    {-Cursor right one word}
  var
    StLen : Byte absolute meSt;
  begin
    if meCurCol <= StLen then begin
      while (meCurCol <= StLen) and meIsAlpha(meSt[meCurCol]) do
        Inc(meCurCol);
      while (meCurCol <= StLen) and not meIsAlpha(meSt[meCurCol]) do
        Inc(meCurCol);
    end
    else if meCurLine < meTotalLines then begin
      meGotoLine(meCurLine+1, True);
      meCurCol := 1;
      meOldCol := 1;
    end;
  end;

  procedure Memo.meDoBackspace;
    {-Do backspace command}
  var
    StLen : Byte absolute meSt;
  begin
    if meCurCol > 1 then begin
      Dec(meCurCol);
      Delete(meSt, meCurCol, 1);
      meCharsInserted(meCurLine, meCurCol, -1);
    end
    else if meCurLine > 1 then begin
      meGotoLine(meCurLine-1, True);
      meCurCol := Succ(StLen);
      meJoinLinePrim(meCurLine+1);
      meLoadLine(meCurLine, True);
      meForceRedraw := True;
      meOldCol := meCurCol;
    end;
  end;

  procedure Memo.meScrollUp;
    {-Scroll up one line}
  var
    I : Word;
  begin
    if meLineAtTop > 1 then begin
      meSaveCurrentLine(True);
      meScrollDisplay(-1);
      I := meLineAtTop+(wYH-wYL);
      if meCurLine > I then
        meGotoLine(I, True);
    end;
  end;

  procedure Memo.meScrollDown;
    {-Scroll down one line}
  begin
    if meLineAtTop < meTotalLines then begin
      meSaveCurrentLine(True);
      meScrollDisplay(1);
      if meCurLine < meLineAtTop then
        meGotoLine(meLineAtTop, True);
    end;
  end;

  procedure Memo.meDeleteChar;
    {-Delete character at cursor}
  var
    StLen : Byte absolute meSt;
  begin
    if meCurCol <= StLen then begin
      Delete(meSt, meCurCol, 1);
      meCharsInserted(meCurLine, meCurCol, -1);
    end
    else if LongFlagIsSet(meOptions, meDeleteJoins) then
      meJoinAtEndOfLine;
  end;

  procedure Memo.meDeleteEol;
    {-Delete to end of line}
  var
    I : Integer;
    StLen : Byte absolute meSt;
  begin
    if StLen >= meCurCol then begin
      I := StLen;
      StLen := Pred(meCurCol);
      meCharsInserted(meCurLine, meCurCol, StLen-I);
    end;
  end;

  procedure Memo.meDoCtrlChar;
    {-Process ccCtrlChar command}
  var
    Ch : Char absolute cwKey;
  begin
    {don't allow control characters if attributes are the same}
    if ColorMono(wTextColor, wTextMono) = ColorMono(meCtrlColor, meCtrlMono) then
      cwCmd := ccNone
    else begin
      SetCursor(CtrlCharCursor);
      cwKey := cwCmdPtr^.cpGetKey;
      case Ch of
        {reinterpret potentially troublesome control characters}
        ^M :
          cwCmd := ccSelect;
        ^J, ^Z :
          cwCmd := ccNone;
        else
          cwCmd := ccChar;
      end;
      if LongFlagIsSet(meOptions, meInsert) then
        SetCursor(InsertCursor)
      else
        SetCursor(OvertypeCursor);
    end;
  end;

  procedure Memo.meAdjustDeltaPrim;
    {-Adjust cursor, column delta}
  begin
    {get current width of edit window}
    meWinWidth := Succ(wXH-wXL);

    {make sure cursor hasn't gone too far}
    if meCurCol > meMaxLength+1 then
      meCurCol := meMaxLength+1
    else if meCurCol = 0 then
      meCurCol := 255;

    {scroll horizontally as necessary}
    if meCurCol > meWinWidth+meColDelta then begin
      meColDelta := meCurCol-meWinWidth;
      meForceRedraw := True;
    end
    else if meCurCol <= meColDelta then begin
      meColDelta := Pred(meCurCol);
      meForceRedraw := True;
    end;
  end;

  procedure Memo.meAdjustDelta;
    {-Adjust column delta, etc.}
  var
    H1 : Word;
  begin
    {adjust cursor, column delta}
    meAdjustDeltaPrim;

    {scroll vertically as necessary}
    H1 := wYH-wYL;
    if meCurLine > meLineAtTop+H1 then begin
      mePutLineAtTop(meCurLine-H1, False);
      meForceRedraw := True;
    end
    else if meCurLine < meLineAtTop then begin
      mePutLineAtTop(meCurLine, False);
      meForceRedraw := True;
    end;
  end;

  procedure Memo.meUpdateScreenAndCursor;
    {-Update the screen and cursor, scrolling as necessary}
  var
    I : Word;
    StLen : Byte absolute meSt;
  begin
    {scroll horizontally and vertically as necessary}
    meAdjustDelta;

    {set modified flag}
    meTrimSpaces;
    meIsModified(meOldModified or (meSt <> meOldSt));

    {update screen}
    if meForceRedraw then begin
      meRedraw;
      meSaveCurLine := meCurLine;
      meSaveCurCol := meCurCol;
    end
    else begin
      {redraw current line}
      meDrawCurrentLine;

      {$IFDEF UseScrollBars}
      {update scroll bars if necessary}
      if (meSaveCurLine <> meCurLine) or
         (meSaveCurCol <> meCurCol) or
         (meTotalLines <> meSaveTotalLines) then
           meUpdateScrollBars;
      {$ENDIF}
    end;

    {position cursor}
    GoToXYAbs(wXL+Pred(meCurCol)-meColDelta, wYL+(meCurLine-meLineAtTop));

    {update meTotalBytes field for status routine}
    I := meTotalBytes;
    Inc(meTotalBytes, Integer(StLen)-Length(meOldSt));

    {account for possible virtual screen access}    {!!.22}
    SetLongFlag(meOptions, meInUpdate);             {!!.22}

    {call status routine}
    meShowStatus;

    {account for possible virtual screen access}    {!!.22}
    ClearLongFlag(meOptions, meInUpdate);           {!!.22}

    {reset meTotalBytes field}
    meTotalBytes := I;
  end;

  procedure Memo.ProcessSelf;     {!!.01}
    {-Process editing commands}
  var
    Ch : Char absolute cwKey;
    StLen : Byte absolute meSt;
    I : Word;
    {SaveBreak : Boolean;}        {!!.01}
    Finished : Boolean;
    {$IFDEF UseMouse}
    {SaveMouse : Boolean;}        {!!.01}
    {$ENDIF}
  begin
    (*                            {!!.01}
    {check for pending error}
    if cwGetLastError <> 0 then begin
      cwCmd := ccError;
      Exit;
    end;

    {Save break checking state}
    SaveBreak := CheckBreak;
    CheckBreak := False;
    *)

    {set cursor shape}
    meToggleOption(meInsert);
    meToggleInsertMode;

    {initialize miscellaneous variables}
    meKnownLine := 1;
    meKnownOfs := 1;
    meOldModified := LongFlagIsSet(meOptions, meModified);

    {Draw initial screen if not already done}
    Draw;
    if RawError <> 0 then begin             {!!.01}
      {GotError(wNotCurrent, emNullError);} {!!.01}
      Exit;
    end;

    (*                                      {!!.01}
    {$IFDEF UseMouse}
    SaveMouse := MouseCursorOn;
    if cwCmdPtr^.MouseEnabled then
      ShowMouse;
    {$ENDIF}
    *)

    {get the first line}
    meLoadLine(meCurLine, True);

    {see if we exceeded the line limit}
    meCheckLineLimit;

    {loop while reading keys}
    Finished := False;
    meForceRedraw := False;
    SetLongFlag(meOptions, meInProcess); {!!.03}
    repeat
      meOK := True;

      {update screen/cursor/status line}
      meUpdateScreenAndCursor;

      {get next command}
      GetNextCommand;

      {make sure command is allowable if in read-only mode}
      if LongFlagIsSet(meOptions, meReadOnly) then
        if cwCmd in DisallowedInReadOnlyMode then
          cwCmd := ccNone;

      {deal with control characters}
      if cwCmd = ccCtrlChar then
        meDoCtrlChar;

      case cwCmd of
        ccChar :             {A character to enter the string}
          meInsertChar(Ch);

        ccSelect :           {new line}
          meNewLine(True);

        ccInsertLine :       {insert line at cursor}
          meNewLine(False);

        ccSaveFile,          {must be implemented by user}
        ccNewFile,
        ccUser0..ccUser65335, {user-defined exit commands}
        ccQuit :             {exit from editor}
          begin
            meSaveCurrentLine(True);
            Finished := True;
          end;

        ccHome :             {Cursor to beginning of line}
          meCurCol := 1;

        ccEnd :              {Cursor to end of line}
          meCurCol := Succ(StLen);

        ccDelEol :           {Delete from cursor to end of line}
          meDeleteEol;

        ccDelLine :          {Delete entire line}
          meDeleteLine;

        ccRestore :          {Restore default and continue}
          begin
            meSt := meOldSt;
            meCurCol := meOldCol;
          end;

        ccLeft :             {Cursor left by one character}
          if meCurCol > 1 then
            Dec(meCurCol)
          else if LongFlagIsSet(meOptions, meWrapAtLeft) then
            meWordLeft;

        ccRight :            {Cursor right by one character}
          if meCurCol < 255 then
            Inc(meCurCol);

        ccUp :               {Cursor up one line}
          if meCurLine > 1 then
            meGotoLine(meCurLine-1, True);

        ccDown :             {Cursor down one line}
          if meCurLine < meTotalLines then
            meGotoLine(meCurLine+1, True);

        ccScrollUp :         {Scroll display up one line}
          meScrollUp;

        ccScrollDn :         {Scroll display down one line}
          meScrollDown;

        ccPageUp :           {Scroll display up one page}
          mePageUp;

        ccPageDn :           {Scroll display down one page}
          mePageDown;

        ccScreenTop :        {Cursor to top of screen}
          meGotoLine(meLineAtTop, True);

        ccScreenBot :        {Cursor to bottom of screen}
          meGotoLine(meLineAtTop+(wYH-wYL), True);

        ccTopOfFile :        {Cursor to top of file}
          meTopOfFile;

        ccEndOfFile :        {Cursor to bottom of file}
          meEndOfFile;

        ccTab :              {Tab}
          meDoFixedTab;

        ccWordLeft :         {Cursor left one word}
          meWordLeft;

        ccWordRight :        {Cursor right one word}
          meWordRight;

        ccDel :              {Delete current character}
          meDeleteChar;

        ccBack :             {Backspace one character}
          meDoBackspace;

        ccDelWord :          {Delete word to right of cursor}
          if meCurCol <= StLen then
            meDeleteWordPrim
          else
            meJoinAtEndOfLine;

        ccIns :              {Toggle insert mode}
          meToggleInsertMode;

        ccIndent :           {Toggle auto-indent mode}
          meToggleOption(meIndent);

        ccWordWrap :         {Toggle word wrap}
          meToggleOption(meWordWrap);

        ccReformatP :        {Reformat paragraph}
          if LongFlagIsSet(meOptions, meWordWrap) then begin {!!.03}
            meSaveCurrentLine(True);                         {!!.01}
            meReformatParagraph;
            meCheckLineLimit;
          end;

        ccReformatG :        {Global reformat}
          if LongFlagIsSet(meOptions, meWordWrap) then begin {!!.03}
            meReformatGlobally;
            meCheckLineLimit;
          end;

        {$IFDEF UseMouse}
        ccMouseAuto,                               {!!.03}
        ccMouseDown,                               {!!.03}
        ccMouseSel :            {Mouse select}
          if cwCmdPtr^.MouseEnabled then
            if meProcessMouseCommand(cwCmd) then begin {!!.03}
              meSaveCurrentLine(True);
              Finished := True;
            end;
        {$ENDIF}

        ccHelp :             {Help}
          RequestHelp(wHelpIndex);
        else if (cwCmd <= 255) and (GetExitCommandPtr <> nil) then begin {!!.11}
          {Possibly a special exit command defined by a derived object} {!!.01}
          Finished := (cwCmd in GetExitCommandPtr^);                    {!!.01}
          if Finished then                                              {!!.11}
            meSaveCurrentLine(True);                                    {!!.11}
        end;                                                            {!!.11}
      end;

    until Finished or (cwCmd = ccError);

    ClearLongFlag(meOptions, meInProcess); {!!.03}

    {redraw the screen one last time}
    meRedraw;

    {restore break checking status}
    {CheckBreak := SaveBreak;}     {!!.01}

    {save window state}
    rwSaveWindowState;

    {$IFDEF UseMouse}
    {ShowMousePrim(SaveMouse);}    {!!.01}
    {$ENDIF}
  end;

begin
  {initialize command processor}
  MemoCommands.Init(@MemoKeySet, MemoKeyMax);
end.
