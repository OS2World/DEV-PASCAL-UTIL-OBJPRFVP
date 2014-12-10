{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPEDITOR.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1988, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpEditor;
  {-Text file editor}


interface

uses
  Use32,
  {$IFDEF VIRTUALPASCAL}
  VpSysLow,
  {$ENDIF}
  Dos,
  OpConst,     {!!.20}
  OpInline,
  OpString,
  OpDos,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd,
  OpFrame,
  OpWindow,
  {$IFDEF UseDrag} {!!.03}
  OpDrag,          {!!.03}
  {$ENDIF}         {!!.03}
  OpMemo;

  {$I OPEDITOR.ICD}  {configuration data}

const
  {option codes}
  teInsert         = $00000001;  {True if in insert mode}
  teIndent         = $00000002;  {True if in auto-indent mode}
  teReadOnly       = $00000004;  {True if in read-only mode}
  teWordWrap       = $00000008;  {True if word wrap is on}
  teDeleteJoins    = $00000010;  {True if <Del> can join lines}
  teModified       = $00000020;  {True if edits have been made}
  teIndentIsPara   = $00000040;  {indent starts paragraph?}
  teMousePage      = $00000080;  {clicking on scroll bar scrolls by page}
  teAllowTrunc     = $00000100;  {read partial files?}
  teMapCtrls       = $00000200;  {map control characters?}
  teMakeBackups    = $00000400;  {make backup files?}
  teStreamReload   = $00000800;  {reload previous file when activating a
                                  text editor stored in a stream}
  teReformatting   = $00001000;  {flag set while reformatting}
  teNewFile        = $00002000;  {new file loaded}
  teDeallocate     = $00004000;  {did OPMEMO allocate the buffer?}
  teWrapAtLeft     = $00008000;  {wrap to prev line on <Left> at column 1}
  teNoCtrlZ        = $00010000;  {suppress ^Z when writing to disk}
  teNoRepeatGlobal = $00020000;  {ignore global option on repeat last find} {!!.03}

  teInProcess      = $00800000;  {internal flag set while in Process} {!!.03}
  teSmartTabs      = $01000000;  {smart tabs or fixed tabs?}
  teSearching      = $02000000;  {flag set while searching}
  teSearchStart    = $04000000;  {flag set at beginning of a search}
  teSearchEnd      = $08000000;  {flag set at end of a search}
  teHighlightOn    = $10000000;  {highlighting on}
  teHighlightBack  = $20000000;  {highlighting goes backwards?}
  teMarkersOn      = $40000000;  {text markers visible?}
  teBlockOn        = $80000000;  {block markers on?}

  {default options}
  DefEditorOptions : LongInt = teInsert+teIndent+teWordWrap+teMapCtrls+
                               teMousePage+teMakeBackups+teSmartTabs+
                               teDeallocate;
  BadEditorOptions : LongInt = teModified+teReformatting+teSearching+
                               teSearchStart+teSearchEnd+teHighlightOn+
                                               {!!.03}
                               teHighlightBack+teInProcess;
  DefEditorExt   : ExtStr = '';   {default file extension}
  MaxBlockIndent = 10;            {maximum indentation level for blocks}
  DefBlockIndent : Byte = 2;      {default indentation level for blocks}

  MaxMarker = 9;                  {Maximum allowed position marker}
  MaxSearchLen = 30;              {Maximum length of search string}

  {search option characters}
  MaxSearchOptions = 6;
  teBackward  = 'B';
  teNoCase    = 'U';
  teGlobal    = 'G';
  teNoConfirm = 'N';
  teBlockOnly = 'L';
  teWordOnly  = 'W';  {!!.30}

  {codes for yes-no functions}
  teNo        = 0;
  teYes       = 1;
  teQuit      = 2;
  teAll       = 3;
type
  teSearchType = (tescNone, tescSearch, tescReplace);
  MarkerRec =
    record
      Line, Col : Integer;
    end;
  MarkerArray = array[0..MaxMarker] of MarkerRec;
  EditorYesNoFunc = function (MsgCode : Word; Prompt : string;
                              Default : Byte; QuitAndAll : Boolean) : Byte;
  EditorEditFunc = function (MsgCode : Word; Prompt : string;
                             ForceUp, TrimBlanks : Boolean;
                             MaxLen : Byte; var S : string) : Boolean;
  EditorGetFileFunc = function (MsgCode : Word; Prompt : string;
                                ForceUp, TrimBlanks, Writing, MustExist : Boolean;
                                MaxLen : Byte; DefExt : ExtStr;
                                var S : string) : Boolean;

  TextEditorPtr = ^TextEditor;
  TextEditor =
    object(MemoFile)
      {----------------------------Colors}
      teBlockColor : Byte;        {Marked blocks - color}
      teBlockMono  : Byte;        {Marked blocks - mono}
      teMarkerColor  : Byte;      {Text markers - color}
      teMarkerMono   : Byte;      {Text markers - mono}
      teHighlightColor : Byte;    {Found text - color}
      teHighlightMono : Byte;     {Found text - mono}
      {----------------------------Search stuff}
      teSearchSt : string[MaxSearchLen]; {String to search for}
      teReplaceSt : string[MaxSearchLen]; {String to replace it with}
      teOptionSt : string[MaxSearchOptions]; {Search options}
      teLastSearch : teSearchType; {Type of last search operation}
      teReplacements : Word;      {Number of replacements made}
      {----------------------------Block/text markers}
      teBlkBegin : MarkerRec;     {Start of block}
      teBlkEnd : MarkerRec;       {End of block}
      teOldBegin : MarkerRec;     {Saved value of teBlkBegin}
      teOldEnd : MarkerRec;       {Saved value of teBlkEnd}
      teMarkers : MarkerArray;    {Array of position markers}
      teMarkerFlags : Word;       {Marker flags}
      teOldMarkerFlags : Word;    {Saved value of teMarkerFlags}
      teLastPosition : MarkerRec; {Position before last line change}
      {----------------------------Options}
      teBlockIndent : Byte;       {indent level for blocks}
      teLPT : Byte;               {line printer (1-3)}
      teDefExt : ExtStr;          {Default extension}
      {----------------------------Procedure pointers}
      teEditFunc : EditorEditFunc; {Function to edit a string}
      teGetFile : EditorGetFileFunc; {Function to get a filename}
      teYesNoFunc : EditorYesNoFunc; {Function to get a yes/no response}
      {...methods...}
      constructor Init(X1, Y1, X2, Y2 : Byte; BufferSize : Word);
        {-Create a window for editing files}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             BufferSize : Word);
        {-Create a custom window for editing files}
      constructor InitBuf(X1, Y1, X2, Y2 : Byte;
                          BufferSize : Word;
                          BufPtr : Pointer);
        {-Create a window for editing files, buffer already allocated}
      constructor InitBufCustom(X1, Y1, X2, Y2 : Byte;
                                var Colors : ColorSet;
                                Options : LongInt;
                                BufferSize : Word;
                                BufPtr : Pointer);
        {-Create a custom window for editing files, buffer already allocated}
      procedure ProcessSelf; virtual; {!!.01}
        {-Process editing commands}
      {...}
      procedure teOptionsOn(OptionFlags : LongInt);
        {-Turn options on}
      procedure teOptionsOff(OptionFlags : LongInt);
        {-Turn options off}
      function teOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified options are on}
      {...}
      procedure SetEditProc(EF : EditorEditFunc);
        {-Set edit function}
      procedure SetGetFileProc(GFF : EditorGetFileFunc);
        {-Set get file function}
      procedure SetYesNoProc(YNF : EditorYesNoFunc);
        {-Set yes-no function}
      {...}
      procedure SetBlockAttr(Color, Mono : Byte);
        {-Set attributes for marked blocks}
      procedure SetMarkerAttr(Color, Mono : Byte);
        {-Set attributes for text markers}
      procedure SetHighlightAttr(Color, Mono : Byte);
        {-Set attributes for highlighted (found) text}
      {...}
      procedure SetBlockIndent(Indent : Byte);
        {-Set block indentation level}
      procedure SetDefaultExtension(DefExt : ExtStr);
        {-Default extension to use when prompting for filenames}
      procedure SetPrinter(LptNum : Byte);
        {-Set printer (1-3)}
      {...}
      procedure GotoLineCol(LineNum : Integer; Col : Byte);
        {-Move cursor to LineNum, Col. Must not be called until editor window
          has been displayed once.}
      procedure GotoOffset(Offset : Word);
        {-Move cursor to specified offset in the text buffer. Must not be
          called until editor window has been displayed once.}
      procedure InsertTextAtCursor(var Data; Bytes : Word);
        {-Insert specified number of Bytes of Data into the text buffer at the
          cursor}
      procedure InsertStringAtCursor(S : String);
        {-Insert string S into the text buffer at the cursor}
      procedure InsertBlockAtCursor(var Data; Bytes : Word); {!!.01}
        {-Insert specified number of Bytes of Data into the text buffer at the
          cursor, and mark the inserted text as a block}
      procedure InsertBlockedStringAtCursor(S : String);  {!!.01}
        {-Insert string S into the text buffer at the cursor, and mark the
          inserted text as a block}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a text editor from a stream}
      procedure Store(var S : IdStream);
        {-Store a text editor in a stream}
    {$ENDIF}
{.Z+}
      {+++ methods that exist to be overridden +++}
      function  teGetString(MsgCode : Word; Prompt : string;
                            ForceUp, TrimBlanks : Boolean;
                            MaxLen : Byte; var S : string) : Boolean; virtual;
      function  teGetFileName(MsgCode : Word; Prompt : string;
                              var FName : PathStr;
                              Writing, MustExist : Boolean) : Boolean; virtual;
      function  teYesNo(MsgCode : Word; Prompt : string; Default : Byte;
                        QuitAndAll : Boolean) : Byte; virtual;
      {+++ internal methods +++}
      procedure meInitBufferState(var Buffer); virtual;
      procedure meDrawLine(St : String; LineNum : Integer); virtual;
      procedure meLoadLine(LineNum : Integer; Truncate : Boolean); virtual;
      procedure meTrimSpaces; virtual;
      procedure meCharsInserted(LineNum : Integer; StartCol : Byte; Count : Integer); virtual;
      procedure meLinesDeleted(StartLine, Count : Integer); virtual;
      procedure meLineBroken(StartLine : Integer; StartCol : Byte); virtual;
      procedure meLineJoined(LineNum : Integer); virtual;
      procedure teResetMarkers;
      function  teGetNumber(MsgCode : Word; Prompt : string; var L : LongInt;
                            LLo, LHi : LongInt) : Boolean;
      procedure teRestoreLine;
      procedure teDoSmartTab;
      procedure teCenterLine;
      procedure teGotoLinePrompted;
      procedure teSetRightMarginPrompted;
      procedure teSetBlockIndentPrompted;
      procedure teSetTabSizePrompted;
      function  teCharInBlock(Line : Integer; Col : Byte) : Boolean;
      function  teCursorInBlock(EndOK : Boolean) : Boolean;
      procedure teChangeCaseBlock(Cmd : Byte);
      procedure teChangeIndentBlock(Delta : Integer);
      procedure teToggleBlock;
      procedure teMarkBlockBegin;
      procedure teMarkBlockEnd;
      procedure teMarkWord;
      procedure teMarkBlockPrim(var MarkA, MarkB : MarkerRec);
      procedure teGotoBlockMarker(var Marker : MarkerRec);
      procedure teGotoMarker(MarkerNum : Byte);
      procedure teSetMarker(MarkerNum : Byte);
      procedure teSaveLastPosition(LineNum, ColNum : Integer);
      function  teBlockDiscontinuous : Boolean;
      function  teBlockCheck(HiddenOK : Boolean) : Boolean;
      procedure teBlockBounds(var Start, Stop, Total : Word; Save : Boolean);
      procedure teMarkerLinesToOffsets(DoBlocks : Boolean);
      procedure teMarkerOffsetsToLines(StartOfs : Word; Delta : LongInt;
                                       DoBlocks : Boolean);
      procedure teAdjustMarkerOffsets(Start, Stop, Dest, Total : Word;
                                      DoBlocks : Boolean);
      procedure teCopyBlock;
      procedure teMoveBlock;
      procedure teDeleteBlock;
      procedure teBlockRead;
      procedure teBlockWrite(ToPrinter, AppendIt : Boolean); virtual; {!!.30}
      procedure teSaveNamedFile;
      procedure teLoadNewFile;
      procedure teFindString(Prompt : Boolean; teST : teSearchType);
      procedure teInsertTextAtCursor(var Data; Bytes : Word;  {!!.01}
                                     Mark : Boolean);
{.Z-}
    end;

{.Z+}
  {$IFDEF UseStreams}
  procedure TextEditorStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing text editors}
  {$ENDIF}
{.Z-}

var
  {$IFDEF UseDrag}                   {!!.03}
  EditorCommands : DragProcessor;    {!!.03}
  {$ELSE}                            {!!.03}
  EditorCommands : CommandProcessor; {command processor used for text editor}
  {$ENDIF}                           {!!.03}

  {==========================================================================}

implementation

const
  SafetyMargin = 2;
  CtrlZ : Char = ^Z;
  SearchFailed = $FFFF;

  {$I OPEDITOR.IN1}

  constructor TextEditor.Init(X1, Y1, X2, Y2 : Byte; BufferSize : Word);
    {-Create a window for editing text files}
  begin
    {initialize using default window options}
    if not TextEditor.InitCustom(X1, Y1, X2, Y2, DefaultColorSet,
                                 DefWindowOptions, BufferSize) then
      Fail;
  end;

  constructor TextEditor.InitCustom(X1, Y1, X2, Y2 : Byte;
                                    var Colors : ColorSet;
                                    Options : LongInt;
                                    BufferSize : Word);
    {-Create a custom window for editing text files}
  begin
    {do normal MemoFile initialization}
    if not MemoFile.InitCustomAndAlloc(X1, Y1, X2, Y2, Colors, Options, BufferSize) then
      Fail;

    {change the unit code}
    cwUnitCode := ucEditor;

    {initialize fields specific to TextEditor}
    with Colors do begin
      teBlockColor := BlockColor;
      teBlockMono  := BlockMono;
      teMarkerColor := MarkerColor;
      teMarkerMono  := MarkerMono;
      teHighlightColor := HighlightColor;
      teHighlightMono := HighlightMono;
    end;

    teSearchSt := '';
    teReplaceSt := '';
    teOptionSt := '';
    teLastSearch := tescNone;
    teReplacements := 0;

    @teEditFunc := nil;
    @teGetFile := nil;
    @teYesNoFunc := nil;

    teResetMarkers;
    teBlockIndent := DefBlockIndent;
    teDefExt := DefEditorExt;
    teLPT := 1;

    {override initialization done by OPMEMO}
    cwCmdPtr := @EditorCommands;
    meOptions := DefEditorOptions;
  end;

  constructor TextEditor.InitBuf(X1, Y1, X2, Y2 : Byte;
                                 BufferSize : Word;
                                 BufPtr : Pointer);
    {-Create a window for editing files, buffer already allocated}
  begin
    {initialize using default window options}
    if not TextEditor.InitBufCustom(X1, Y1, X2, Y2, DefaultColorSet,
                                    DefWindowOptions, BufferSize, BufPtr) then
      Fail;
  end;

  constructor TextEditor.InitBufCustom(X1, Y1, X2, Y2 : Byte;
                                       var Colors : ColorSet;
                                       Options : LongInt;
                                       BufferSize : Word;
                                       BufPtr : Pointer);
    {-Create a custom window for editing files, buffer already allocated}
  begin
    {do normal MemoFile initialization}
    if not MemoFile.InitCustom(X1, Y1, X2, Y2, Colors, Options,
                               BufferSize, BufPtr) then
      Fail;

    {change the unit code}
    cwUnitCode := ucEditor;

    {initialize fields specific to TextEditor}
    with Colors do begin
      teBlockColor := BlockColor;
      teBlockMono  := BlockMono;
      teMarkerColor := MarkerColor;
      teMarkerMono  := MarkerMono;
      teHighlightColor := HighlightColor;
      teHighlightMono := HighlightMono;
    end;

    teSearchSt := '';
    teReplaceSt := '';
    teOptionSt := '';
    teLastSearch := tescNone;
    teReplacements := 0;

    @teEditFunc := nil;
    @teGetFile := nil;
    @teYesNoFunc := nil;

    teResetMarkers;
    teBlockIndent := DefBlockIndent;
    teDefExt := DefEditorExt;
    teLPT := 1;

    {override initialization done by OPMEMO}
    cwCmdPtr := @EditorCommands;
    meOptions := DefEditorOptions and not teDeallocate;
  end;

  procedure TextEditor.teOptionsOn(OptionFlags : LongInt);
    {-Activate multiple options}
  begin
    SetLongFlag(meOptions, OptionFlags and not BadEditorOptions);
  end;

  procedure TextEditor.teOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple options}
  begin
    ClearLongFlag(meOptions, OptionFlags and not BadEditorOptions);
  end;

  function TextEditor.teOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified options are on}
  begin
    teOptionsAreOn := (meOptions and OptionFlags = OptionFlags);
  end;

  procedure TextEditor.meInitBufferState(var Buffer);
    {-Initialize the edit buffer status fields}
  begin
    MemoFile.meInitBufferState(Buffer);
    teResetMarkers;
  end;

  procedure TextEditor.teResetMarkers;
    {-Reset text/block markers, etc.}
  begin
    meSetOption(teBlockOn+teMarkersOn, False);
    meForceRedraw := True;
    FillChar(teBlkBegin, SizeOf(teBlkBegin), 0);
    teBlkEnd := teBlkBegin;
    teOldBegin := teBlkBegin;
    teOldEnd := teBlkBegin;
    teLastPosition := teBlkBegin;
    FillChar(teMarkers, SizeOf(teMarkers), 0);
    teMarkerFlags := 0;
    teOldMarkerFlags := 0;
  end;

  procedure TextEditor.meDrawLine(St : String; LineNum : Integer);
    {-Draw the specified line}
  var
    StLen : Byte absolute St;
    SearchLen : Byte absolute teSearchSt;
    ASt : string;
    AStLen : Byte absolute ASt;
    TA, CA, BA, MA, HA : Byte;
    I, J : Word;
    BLine, ELine : Integer;
    BCol, ECol : Byte;
  begin
    {pad character string}
    J := MaxWord(Word(meWinWidth)+meColDelta, 255);
    FillChar(St[Succ(StLen)], J-StLen, ' ');

    {initialize attribute string}
    TA := ColorMono(wTextColor, wTextMono);
    FillChar(ASt[1], J, TA);
    AStLen := J;

    {map control characters}
    if (StLen > 0) and LongFlagIsSet(meOptions, teMapCtrls) then begin
      CA := ColorMono(meCtrlColor, meCtrlMono);
      for I := meColDelta+1 to StLen do
        if St[I] < ' ' then begin
          Inc(Byte(St[I]), 64);
          ASt[I] := Char(CA);
        end;
    end;

    {account for block markers}
    if LongFlagIsSet(meOptions, teBlockOn) then begin
      BLine := teBlkBegin.Line;
      ELine := teBlkEnd.Line;
      if teBlkBegin.Col > J then
        BCol := J
      else
        BCol := teBlkBegin.Col;
      if teBlkEnd.Col > J then
        ECol := J
      else
        ECol := teBlkEnd.Col;
      BA := ColorMono(teBlockColor, teBlockMono);
      if (LineNum >= BLine) and (LineNum <= ELine) then
        {is this the first line of the block?}
        if (LineNum = BLine) then
          {is this also the last line of the block?}
          if (LineNum = ELine) then
            {entire block is within this one line}
            FillChar(ASt[BCol], ECol-BCol, BA)
          else
            {first line of block}
            FillChar(ASt[BCol], Succ(J-BCol), BA)
        {is this the last line of the block?}
        else if (LineNum = ELine) then
          {first part of line is inside the block}
          FillChar(ASt[1], ECol-1, BA)
        else
          {line is completely within the block}
          FillChar(ASt[1], J, BA);
    end;

    {account for text markers}
    if LongFlagIsSet(meOptions, teMarkersOn) and (teMarkerFlags <> 0) then begin
      MA := ColorMono(teMarkerColor, teMarkerMono);
      for I := 0 to MaxMarker do
        with teMarkers[I] do
          if LineNum = Line then begin
            St[Col] := Char(Ord('0')+I);
            ASt[Col] := Char(MA);
          end;
    end;

    {highlight string at cursor?}
    if (LineNum = meCurLine) and LongFlagIsSet(meOptions, teHighlightOn) then begin
      HA := ColorMono(teHighlightColor, teHighlightMono);
      I := meCurCol;
      if not LongFlagIsSet(meOptions, teHighlightBack) then
        Dec(I, Pred(SearchLen));
      FillChar(ASt[I], SearchLen, HA);
    end;

    {adjust for ColDelta}
    I := meColDelta+1;
    if (I > 1) then begin
      MoveFast(St[I], St[1], meWinWidth);  {!!.01}
      MoveFast(ASt[I], ASt[1], meWinWidth);  {!!.01}
    end;

    {set the length bytes}
    StLen := meWinWidth;
    AStLen := StLen;

    {draw the string}
    FastWriteAttr(St, Word(wYL)+(LineNum-meLineAtTop), wXL, ASt)
  end;

  procedure TextEditor.meLoadLine(LineNum : Integer; Truncate : Boolean);
    {-Load the specified line}
  begin
    MemoFile.meLoadLine(LineNum, Truncate);

    teOldBegin := teBlkBegin;
    teOldEnd := teBlkEnd;
    teOldMarkerFlags := teMarkerFlags;
  end;

  procedure TextEditor.meTrimSpaces;
    {-Trim trailing blanks from current line}
  var
    StLen : Byte absolute meSt;
    I : Word;
  begin
    if meSt[StLen] = ' ' then begin
      {we don't want to change text markers beyond the end of the line}
      I := teMarkerFlags;
      teMarkerFlags := 0;

      {trim the blanks}
      MemoFile.meTrimSpaces;

      {restore marker flags}
      teMarkerFlags := I;
    end;
  end;

  procedure TextEditor.meCharsInserted(LineNum : Integer;
                                       StartCol : Byte;
                                       Count : Integer);
    {-Called when Count characters are inserted at or deleted from the current
      line at StartCol}
  var
    I : Word;

    procedure FixMarker(var MR : MarkerRec; IsTextMarker : Boolean);
      {-Fix one marker}
    var
      TCol : Integer;
    begin
      with MR do
        if (Line = LineNum) and (Col >= StartCol) then begin
          TCol := Col;
          if IsTextMarker or (TCol > StartCol) then
            Inc(TCol, Count);

          if TCol < StartCol then
            Col := StartCol
          else
            Col := TCol;
        end;
    end;

  begin
    if Count = 0 then
      Exit;

    {fix teLastPosition}
    FixMarker(teLastPosition, True);

    {fix block markers}
    if (teBlkBegin.Line <> 0) or (teBlkEnd.Line <> 0) then begin
      FixMarker(teBlkBegin, False);
      FixMarker(teBlkEnd, False);
      if teBlockDiscontinuous then
        meSetOption(teBlockOn, False);
    end;

    {fix text markers}
    if teMarkerFlags <> 0 then
      for I := 0 to MaxMarker do
        FixMarker(teMarkers[I], True);
  end;

  procedure TextEditor.meLinesDeleted(StartLine, Count : Integer);
    {-Called when Count lines are deleted at StartLine}
  var
    EndLine : Integer;
    I : Word;

    procedure FixMarker(var MR : MarkerRec; IsTextMarker : Boolean);
      {-Fix one marker}
    begin
      with MR do
        if Line >= StartLine then
          {are we in a line that was deleted?}
          if IsTextMarker then
            if (Line <= EndLine) then
              Line := 0
            else
              Dec(Line, Count)
          else if Line = StartLine then
            Col := 1
          else if Line > StartLine then
            Dec(Line, Count);
    end;

  begin
    {calculate last line deleted}
    EndLine := StartLine+Count-1;

    {fix teLastPosition}
    FixMarker(teLastPosition, True);

    {fix block markers}
    if (teBlkBegin.Line <> 0) or (teBlkEnd.Line <> 0) then begin
      FixMarker(teBlkBegin, False);
      FixMarker(teBlkEnd, False);
      if teBlockDiscontinuous then
        meSetOption(teBlockOn, False);
    end;

    {fix text markers}
    if teMarkerFlags <> 0 then
      for I := 0 to MaxMarker do
        FixMarker(teMarkers[I], True);
  end;

  procedure TextEditor.meLineBroken(StartLine : Integer; StartCol : Byte);
    {-Called when a line is broken}
  var
    I : Word;

    procedure FixMarker(var MR : MarkerRec);
      {-Fix one marker}
    begin
      with MR do
        if (Line = StartLine) and (Col >= StartCol) then begin
          Inc(Line);
          Dec(Col, Pred(StartCol));
        end
        else if (Line > StartLine) then
          Inc(Line);
    end;

  begin
    {fix teLastPosition}
    FixMarker(teLastPosition);

    {fix block markers}
    if (teBlkBegin.Line <> 0) or (teBlkEnd.Line <> 0) then begin
      FixMarker(teBlkBegin);
      FixMarker(teBlkEnd);
      if teBlockDiscontinuous then
        meSetOption(teBlockOn, False);
    end;

    {fix text markers}
    if teMarkerFlags <> 0 then
      for I := 0 to MaxMarker do
        FixMarker(teMarkers[I]);
  end;

  procedure TextEditor.meLineJoined(LineNum : Integer);
    {-Called when two lines are joined}
  var
    I : Word;
    PrevL : Integer;

    procedure FixMarker(var MR : MarkerRec);
      {-Fix one marker}
    begin
      with MR do
        if (Line = LineNum) then begin
          Inc(Col, PrevL);
          Dec(Line);
        end
        else if (Line > LineNum) then
          Dec(Line);
    end;

  begin
    {get length of previous line}
    PrevL := meFindLineLength(LineNum-1);

    {fix teLastPosition}
    FixMarker(teLastPosition);

    {fix block markers}
    if (teBlkBegin.Line <> 0) or (teBlkEnd.Line <> 0) then begin
      FixMarker(teBlkBegin);
      FixMarker(teBlkEnd);
      if teBlockDiscontinuous then
        meSetOption(teBlockOn, False);
    end;

    {fix text markers}
    if teMarkerFlags <> 0 then
      for I := 0 to MaxMarker do
        FixMarker(teMarkers[I]);
  end;

  procedure TextEditor.SetEditProc(EF : EditorEditFunc);
    {-Set edit function}
  begin
    teEditFunc := EF;
  end;

  procedure TextEditor.SetGetFileProc(GFF : EditorGetFileFunc);
    {-Set get file function}
  begin
    teGetFile := GFF;
  end;

  procedure TextEditor.SetYesNoProc(YNF : EditorYesNoFunc);
    {-Set yes-no function}
  begin
    teYesNoFunc := YNF;
  end;

  procedure TextEditor.SetBlockAttr(Color, Mono : Byte);
    {-Set attributes for marked blocks}
  begin
    teBlockColor := Color;
    teBlockMono := MapMono(Color, Mono);
  end;

  procedure TextEditor.SetMarkerAttr(Color, Mono : Byte);
    {-Set attributes for text markers}
  begin
    teMarkerColor := Color;
    teMarkerMono := MapMono(Color, Mono);
  end;

  procedure TextEditor.SetHighlightAttr(Color, Mono : Byte);
    {-Set attributes for highlighted (found) text}
  begin
    teHighlightColor := Color;
    teHighlightMono := MapMono(Color, Mono);
  end;

  procedure TextEditor.teGotoMarker(MarkerNum : Byte);
    {-Go to stored mark position}
  begin
    if MarkerNum <= MaxMarker then
      if teMarkers[MarkerNum].Line = 0 then        {!!.13}
        GotError(epWarning+ecNoMarker, emNoMarker) {!!.13}
      else                                         {!!.13}
        teGotoBlockMarker(teMarkers[MarkerNum]);
  end;

  procedure TextEditor.teSetMarker(MarkerNum : Byte);
    {-Store current file position as marker}
  begin
    if MarkerNum <= MaxMarker then
      with teMarkers[MarkerNum] do
        if (Line = meCurLine) and (Col = meCurCol) then begin
          {Clear existing marker}
          Line := 0;
          ClearFlag(teMarkerFlags, 1 shl MarkerNum);
        end
        else begin
          {Set marker here}
          Line := meCurLine;
          Col := meCurCol;
          meSetOption(teMarkersOn, True);
          meForceRedraw := True;
          SetFlag(teMarkerFlags, 1 shl MarkerNum);
        end;
  end;

  function TextEditor.teBlockDiscontinuous : Boolean;
    {-Returns True if block does not mark a continuous stream}
  var
    MaxCol : Word;
  begin
    if (teBlkBegin.Line = 0) or (teBlkBegin.Line > teBlkEnd.Line) then
      teBlockDiscontinuous := True
    else if (teBlkBegin.Line < teBlkEnd.Line) then
      teBlockDiscontinuous := False
    else
      {both markers on same line}
      teBlockDiscontinuous := teBlkBegin.Col >= teBlkEnd.Col;
  end;

  function TextEditor.teBlockCheck(HiddenOK : Boolean) : Boolean;
    {-Returns True if a block is marked and (if HiddenOK is False) displayed}
  begin
    if teBlockDiscontinuous or not (HiddenOK or LongFlagIsSet(meOptions, teBlockOn)) then begin
      GotError(epWarning+ecNoBlock, emNoBlock);
      teBlockCheck := False;
    end
    else
      teBlockCheck := True;
  end;

  procedure TextEditor.teMarkBlockPrim(var MarkA, MarkB : MarkerRec);
    {-Set block marker MarkA and fix MarkB}
  var
    StLen : Byte absolute meSt;
  begin
    with MarkA do begin
      Line := meCurLine;
      if meCurCol > StLen then
        Col := Succ(StLen)
      else
        Col := meCurCol;
    end;

    if MarkB.Line = 0 then
      meSetOption(teBlockOn, False)
    else if teBlockDiscontinuous then begin
      meSetOption(teBlockOn, False);
      MarkB.Line := 0;
    end
    else
      meSetOption(teBlockOn, True);
    meForceRedraw := True;
  end;

  procedure TextEditor.teMarkBlockBegin;
    {-Mark beginning of block}
  begin
    teMarkBlockPrim(teBlkBegin, teBlkEnd);
  end;

  procedure TextEditor.teMarkBlockEnd;
    {-Mark end of block}
  begin
    teMarkBlockPrim(teBlkEnd, teBlkBegin);
  end;

  procedure TextEditor.teMarkWord;
    {-Mark the current word as a block}
  label
    Retry;
  var
    StLen : Byte absolute meSt;
    C : Word;
  begin
    {if line is empty don't change anything}
    if StLen = 0 then
      Exit;

    C := meCurCol;
    if C > StLen then
      {Cursor past end of line, mark word to left}
      C := StLen;

Retry:
    if meIsAlpha(meSt[C]) then begin
      {in a word, scan to left edge of word}
      while (C > 0) and meIsAlpha(meSt[C]) do
        Dec(C);
      Inc(C);
    end
    else if C = StLen then begin
      {in white space, scan left to end of next word}
      while (C > 0) and not meIsAlpha(meSt[C]) do
        Dec(C);
      if C > 0 then
        goto Retry;
    end
    else begin
      {in white space, scan right to next word}
      while (C <= StLen) and not meIsAlpha(meSt[C]) do
        Inc(C);
      if C > StLen then begin
        {no luck - try going to the left}
        C := StLen;
        goto Retry;
      end;
    end;

    {get out if we didn't have any luck}
    if (C = 0) or (C > StLen) then
      Exit;

    {block begins at left edge of word}
    teBlkBegin.Col := C;

    {scan right past end of word}
    while (C <= StLen) and meIsAlpha(meSt[C]) do
      Inc(C);

    {block ends just past right edge of word}
    teBlkEnd.Col := C;

    {set markers to this line}
    teBlkEnd.Line := meCurLine;
    teBlkBegin.Line := meCurLine;

    meSetOption(teBlockOn, True);
    meForceRedraw := True;
  end;

  procedure TextEditor.teGotoBlockMarker(var Marker : MarkerRec);
    {-Reposition to a block marker}
  var
    L : Integer;
    C : Byte;
  begin
    with Marker do
      if Line <> 0 then begin
        if Line > meTotalLines then
          L := meTotalLines
        else
          L := Line;
        if Col > Succ(meMaxLength) then
          C := Succ(meMaxLength)
        else
          C := Col;
        while (meCurLine <> L) or (meCurCol <> C) do begin
          meCurCol := C;
          if (L <> meCurLine) then
            meGotoLine(L, True)
          else
            Exit;
        end
      end
      else if teBlockCheck(False) then
        {always displays an error message} ;
  end;

  procedure TextEditor.teToggleBlock;
    {-Toggle block visibility}
  begin
    if teBlockDiscontinuous then
      meSetOption(teBlockOn, False)
    else
      meToggleOption(teBlockOn);
    meForceRedraw := True;
  end;

  function TextEditor.teCharInBlock(Line : Integer; Col : Byte) : Boolean;
    {-Returns True if character at Line,Col is inside marked block}
  begin
    if (Line = teBlkBegin.Line) then
      if (teBlkBegin.Line = teBlkEnd.Line) then
        {whole block is on one line}
        teCharInBlock := (Col >= teBlkBegin.Col) and (Col < teBlkEnd.Col)
      else
        {cursor is on first line of block}
        teCharInBlock := (Col >= teBlkBegin.Col)
    else if (Line = teBlkEnd.Line) then
      {cursor is on last line of block}
      teCharInBlock := (Col < teBlkEnd.Col)
    else if (Line > teBlkBegin.Line) and (Line < teBlkEnd.Line) then
      {cursor is in the middle of the block}
      teCharInBlock := True
    else
      teCharInBlock := False;
  end;

  function TextEditor.teCursorInBlock(EndOK : Boolean) : Boolean;
    {-Returns True if cursor is inside marked block}
  begin
    if teBlockDiscontinuous then
      teCursorInBlock := False
    else if EndOK and (meCurLine = teBlkEnd.Line) and (meCurCol = teBlkEnd.Col) then
      teCursorInBlock := True
    else
      teCursorInBlock := teCharInBlock(meCurLine, meCurCol);
  end;

  procedure TextEditor.teBlockBounds(var Start, Stop, Total : Word;
                                     Save : Boolean);
    {-Return the starting and ending offsets for a marked block}
  begin
    {save the current line}
    if Save then
      meSaveCurrentLine(True);

    {find the end points of the block}
    Start := meFindLineIndex(teBlkBegin.Line)+teBlkBegin.Col-1;
    Stop := meFindLineIndex(teBlkEnd.Line)+teBlkEnd.Col-2;
    Total := Stop-Start+1;
  end;

  procedure TextEditor.teChangeCaseBlock(Cmd : Byte);
    {-Change the case of all characters in block or of character at cursor}
  var
    StLen : Byte absolute meSt;
    I, Total : Word;
    Start, Stop : Word;

    function ToggleCase(Ch : Char) : Char;
      {-Toggle the case of the specified character}
    var
      NewCh : Char;
    begin
      NewCh := UpCase(Ch);
      if NewCh <> Ch then
        ToggleCase := NewCh
      else
        ToggleCase := Locase(Ch);
    end;

    procedure FixChar(var Ch : Char);
      {-Fix the case of Ch based on the user's command}
    begin
      if Ch > ' ' then
        case Cmd of
          ccBlkUCase : Ch := UpCase(Ch);
          ccBlkLCase : Ch := LoCase(Ch);
          ccBlkTCase : Ch := ToggleCase(Ch);
        end;
    end;

  begin
    if LongFlagIsSet(meOptions, teBlockOn) and teCursorInBlock(True) then
      if teBlkBegin.Line = teBlkEnd.Line then begin
        {current line has whole block}
        for I := teBlkBegin.Col to Pred(teBlkEnd.Col) do
          if I > StLen then
            Exit
          else
            FixChar(meSt[I]);
      end
      else begin
        {find the end points of the block}
        teBlockBounds(Start, Stop, Total, True);

        {change the case of the characters in the buffer}
        for I := Start to Stop do
          FixChar(meBufPtr^[I]);

        {reload the current line}
        meLoadLine(meCurLine, True);

        {redraw whole screen}
        meForceRedraw := True;

        {mark file as modified}
        meOldModified := True;
        meIsModified(True);
      end
    else if (meCurCol <= StLen) then
      {change the case of the character at the cursor}
      FixChar(meSt[meCurCol]);
  end;

  procedure TextEditor.teChangeIndentBlock(Delta : Integer);
    {-Change the indentation level of a marked block}
  var
    StLen : Byte absolute meSt;
    I, J : Integer;
    LastLine : Integer;
  begin
    {do nothing if Delta is 0}
    if Delta = 0 then
      Exit;

    {make sure that a block is marked and displayed, and cursor is in it}
    if not (teBlockCheck(False) and teCursorInBlock(True)) then
      Exit;

    {save the current line}
    meSaveCurrentLine(True);

    {save current position of cursor}
    teSaveLastPosition(meCurLine, meCurCol);

    {get last line to un/indent}
    LastLine := teBlkEnd.Line;
    if teBlkEnd.Col = 1 then
      Dec(LastLine);

    {un/indent the block}
    meCurLine := teBlkBegin.Line;
    repeat
      {load the current line}
      meLoadLine(meCurLine, True);
      if StLen > 0 then begin
        if Delta > 0 then begin
          if meInsertOK(Delta) then begin
            {make room for the spaces}
            meMakeHole(meBufPos, Delta);

            {fill them in}
            FillChar(meBufPtr^[meBufPos], Delta, ' ');

            {adjust markers}
            meCharsInserted(meCurLine, 1, Delta);
          end
          else
            {we need to terminate the loop}
            meCurLine := LastLine;
        end
        else begin
          {get current indentation}
          I := meGetIndent(meSt);

          {get number of spaces to delete}
          if I+Delta < 0 then
            J := -I                    {!!.02}
          else
            J := Delta;

          {delete them from the buffer}
          if J <> 0 then begin
            meMakeHole(meBufPos, J);

            {adjust markers}
            meCharsInserted(meCurLine, 1, J);
          end;
        end;
      end;

      {go to next line}
      Inc(meCurLine);

      {get last line to un/indent}
      LastLine := teBlkEnd.Line;
      if teBlkEnd.Col = 1 then
        Dec(LastLine);
    until meCurLine > LastLine;

    {mark file as modified}
    meOldModified := True;
    meIsModified(True);

    {force screen to be redrawn}
    meForceRedraw := True;

    {get new offset of line at top}
    meKnownLine := 1;
    meKnownOfs := 1;
    meBufPosTop := meFindLineIndex(meLineAtTop);

    {restore cursor to previous position}
    meLoadLine(teLastPosition.Line, True);
    teGotoBlockMarker(teLastPosition);
  end;

  procedure TextEditor.teMarkerLinesToOffsets(DoBlocks : Boolean);
    {-Convert the line numbers in all text markers to offsets}
  var
    I : Word;

    procedure FixMarker(var MR : MarkerRec);
      {-Fix one marker}
    var
      L : Word;
    begin
      with MR do
        if Line <> 0 then begin
          L := meFindLineLength(Line);
          Word(Line) := meKnownOfs+Pred(Col);
          if Col <= L then
            Col := 0
          else
            Dec(Col, L);
          Dec(Word(Line), Col);
        end;
    end;

  begin
    {fix markers}
    FixMarker(teLastPosition);
    if DoBlocks then begin
      FixMarker(teBlkBegin);
      FixMarker(teBlkEnd);
    end;

    {do nothing else if no markers set}
    if teMarkerFlags = 0 then
      Exit;

    {convert line numbers to offsets}
    for I := 0 to MaxMarker do
      FixMarker(teMarkers[I]);
  end;

  function TextEditor.teGetString(MsgCode : Word; Prompt : string;
                                  ForceUp, TrimBlanks : Boolean;
                                  MaxLen : Byte; var S : string) : Boolean;
    {-Prompt for a string}
  begin
    if cwCmdPtr^.CommandStringPending then begin
      S := cwCmdPtr^.GetCommandString;
      teGetString := True;
    end
    else if @teEditFunc = nil then
      teGetString := False
    else
      teGetString := teEditFunc(MsgCode, Prompt, ForceUp, TrimBlanks, MaxLen, S);
  end;

  function TextEditor.teGetNumber(MsgCode : Word; Prompt : string;
                                  var L : LongInt; LLo, LHi : LongInt) : Boolean;
    {-Prompt for a number}
  var
    OK : Boolean;
    St : String[10];
    LT : LongInt;
  begin
    teGetNumber := False;
    LT := L;
    St := Long2Str(LT);

    repeat
      if (not teGetString(MsgCode, Prompt, False, True, 10, St)) or (St = '') then
        Exit;
      OK := Str2Long(St, LT) and (LT >= LLo) and (LT <= LHi);
      if not OK then
        GotError(epWarning+ecInvalidNumber, emInvalidNumber);
    until OK;

    L := LT;
    teGetNumber := True;
  end;

  procedure TextEditor.teGotoLinePrompted;
    {-Prompt for and go to a particular line}
  var
    LineL : LongInt;
  begin
    LineL := meCurLine;
    if teGetNumber(epMessage+mcLineNumber, emLineNumber, LineL, 1, 65535) then
      meGotoLine(LineL, True);
  end;

  procedure TextEditor.teSetRightMarginPrompted;
    {-Prompt for a new right margin}
  var
    Margin : LongInt;
  begin
    Margin := meMargin;
    if teGetNumber(epMessage+mcRightMargin, emRightMargin, Margin, 1, meMaxLength) then
      meMargin := Margin;
  end;

  procedure TextEditor.SetBlockIndent(Indent : Byte);
    {-Set block indentation level}
  begin
    if Indent > MaxBlockIndent then
      teBlockIndent := MaxBlockIndent
    else if Indent > 0 then
      teBlockIndent := Indent;
  end;

  procedure TextEditor.SetDefaultExtension(DefExt : ExtStr);
    {-Default extension to use when prompting for filenames}
  begin
    teDefExt := DefExt;
  end;

  procedure TextEditor.SetPrinter(LptNum : Byte);
    {-Set printer (1-3)}
  begin
    case LptNum of
      1..3 : teLPT := LptNum;
      else   teLPT := 1;
    end;
  end;

  procedure TextEditor.teSetBlockIndentPrompted;
    {-Prompt for a new block indentation level}
  var
    Indent : LongInt;
  begin
    Indent := teBlockIndent;
    if teGetNumber(epMessage+mcIndentLevel, emIndentLevel, Indent, 1, MaxBlockIndent) then
      teBlockIndent := Indent;
  end;

  procedure TextEditor.teSetTabSizePrompted;
    {-Prompt for a new tab size}
  var
    TabSize : LongInt;
  begin
    TabSize := meTabDelta;
    if teGetNumber(epMessage+mcTabSize, emTabSize, TabSize, 1, 10) then
      meTabDelta := TabSize;
  end;

  procedure TextEditor.teDoSmartTab;
    {-Do smart tab command}
  var
    meStLen : Byte absolute meSt;
    TabPos : Byte;

    function NextIndentCol(LineNum : Word; Start : Byte) : Byte;
      {-Return the column number where next tab past start is}
    var
      S : string;
      SLen : Byte absolute S;
      Next : Byte;
    begin
      meGetLine(S, LineNum);

      if SLen = 0 then
        Next := 0
      else if (Start <= SLen) then begin
        Next := Start;
        if (S[Start] <> ' ') then
          {Start is in a word - advance to next blank}
          while (Next <= SLen) and (S[Next] <> ' ') do
            Inc(Next);

        {In white space - advance to next non-blank}
        while (Next <= SLen) and (S[Next] = ' ') do
          Inc(Next);
      end
      else
        {don't go anywhere}
        Next := Start;

      NextIndentCol := Next;
    end;

    function FollowingIndent : Word;
      {-Return the indent of any following non-blank line}
    var
      L : Word;
    begin
      FollowingIndent := 0;

      L := meCurLine;
      if L < meTotalLines then begin
        repeat
          Inc(L);
        until (L > meTotalLines) or (meFindLineLength(L) <> 0);
        if L <= meTotalLines then
          FollowingIndent := NextIndentCol(L, 1);
      end;
    end;

  begin
    {Check previous line}
    if meCurLine = 1 then
      TabPos := 0
    else
      TabPos := NextIndentCol(meCurLine-1, meCurCol);

    if (TabPos > meCurCol) then
      {Insert spaces at the cursor}
      meInsertTab(TabPos)
    else if (TabPos = 0) or (meCurCol > meStLen) then begin
      {Tab cursor to following line}
      TabPos := FollowingIndent;
      if TabPos > meCurCol then
        meInsertTab(TabPos);
    end;
  end;

  function TextEditor.teYesNo(MsgCode : Word; Prompt : string; Default : Byte;
                              QuitAndAll : Boolean) : Byte;
    {-Get a response to a yes-no question}
  var
    S : string[1];
  begin
    if cwCmdPtr^.CommandStringPending then begin
      S := cwCmdPtr^.GetCommandString;
      teYesNo := Default;
      if S <> '' then
        case S[1] of
          'N' : teYesNo := teNo;
          'Y' : teYesNo := teYes;
          'Q' : teYesNo := teQuit;
          'A' : teYesNo := teAll;
        end;
    end
    else if @teYesNoFunc = nil then
      teYesNo := teQuit
    else
      teYesNo := teYesNoFunc(MsgCode, Prompt, Default, QuitAndAll);
  end;

  function TextEditor.teGetFileName(MsgCode : Word; Prompt : string;
                                    var FName : PathStr;
                                    Writing : Boolean;
                                    MustExist : Boolean) : Boolean;
    {-Prompt for a filename}
  const
    MaxLen = SizeOf(PathStr)-1;
  begin
    if cwCmdPtr^.CommandStringPending then begin
      FName := DefaultExtension(cwCmdPtr^.GetCommandString, teDefExt);
      teGetFileName := (FName <> '');
    end
    else if @teGetFile = nil then
      teGetFileName := False
    else if teGetFile(MsgCode, Prompt, True, True, Writing, MustExist,
                      MaxLen, teDefExt, FName) then begin
      {FName := DefaultExtension(FName, teDefExt);} {!!.01}
      teGetFileName := (FName <> '');
    end
    else
      teGetFileName := False;
  end;

  procedure TextEditor.teSaveNamedFile;
    {-Save the current file under a different name}
  var
    FName : PathStr;
  begin
    FName := mfFileName;
    if teGetFileName(epMessage+mcSaveAs, emBlockWrite, FName, True, False) then begin
      mfFileName := FName;
      meSaveCurrentLine(True);
      SaveFile;
      meSetOption(teNewFile, True);
    end;
  end;

  procedure TextEditor.teLoadNewFile;
    {-Load a new file}
  var
    FName : PathStr;
    L : LongInt;
    ReadAnyway : Boolean;
    YN : Byte;
  begin
    ReadAnyway := False;
    if LongFlagIsSet(meOptions, teModified) then begin
      YN := teYesNo(epMessage+mcFileModified, emFileModified, teYes, False);
      if YN = teYes then begin
        meSaveCurrentLine(True);
        SaveFile;
        meShowStatus;
        if cwGetLastError <> 0 then
          Exit;
      end
      else if YN = teQuit then
        Exit
      else
        ReadAnyway := True;
    end;

    FName := mfFileName;
    if teGetFileName(epMessage+mcNewFile, emNewFile, FName, False, False) then
      if (FName <> mfFileName) or ReadAnyway then begin
        mfReadFilePrim(FName, L, epNonFatal);
        if cwGetLastError = 0 then
          meLoadLine(meCurLine, True);
      end;
  end;

  procedure TextEditor.teSaveLastPosition(LineNum, ColNum : Integer);
    {-Save cursor position in teLastPosition}
  begin
    with teLastPosition do begin
      Line := LineNum;
      Col := ColNum;
    end;
  end;

  procedure TextEditor.ProcessSelf; {!!.01}
    {-Process editing commands}
  var
    HaveFilename : Boolean;
    Ch : Char absolute cwKey;
    StLen : Byte absolute meSt;
    I : Word;
    {SaveBreak : Boolean;}     {!!.01}
    Finished : Boolean;
    SaveCurLine : Integer;
    SaveCurCol : Byte;
    {$IFDEF UseMouse}
    {SaveMouse : Boolean;}     {!!.01}
    {$ENDIF}
  begin
    (*                         {!!.01}
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
    meOldModified := LongFlagIsSet(meOptions, teModified);

    {Draw initial screen if not already done}
    Draw;
    if RawError <> 0 then begin             {!!.01}
      {GotError(wNotCurrent, emNullError);} {!!.01}
      Exit;
    end;

    (*                {!!.01}
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

    {do we have a filename?}
    HaveFilename := (mfFileName <> '');

    {loop while reading keys}
    Finished := False;
    meForceRedraw := False;
    teSaveLastPosition(meCurLine, meCurCol);
    SetLongFlag(meOptions, teInProcess); {!!.03}
    repeat
      meOK := True;

      {update screen/cursor/status line}
      meUpdateScreenAndCursor;

      {get next command}
      GetNextCommand;

      {make sure command is allowable if in read-only mode}
      if LongFlagIsSet(meOptions, teReadOnly) then
        if cwCmd in DisallowedInReadOnlyMode then
          cwCmd := ccNone;

      {deal with control characters}
      if cwCmd = ccCtrlChar then
        meDoCtrlChar;

      {save current line and column}
      SaveCurLine := meCurLine;
      SaveCurCol := meCurCol;

      case cwCmd of
        ccChar :             {A character to enter the string}
          meInsertChar(Ch);

        ccSelect :           {new line}
          meNewLine(True);

        ccInsertLine :       {insert line at cursor}
          meNewLine(False);

        ccAbandonFile,       {abandon file}
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
          teRestoreLine;

        ccLeft :             {Cursor left by one character}
          if meCurCol > 1 then
            Dec(meCurCol)
          else if LongFlagIsSet(meOptions, teWrapAtLeft) then
            meWordLeft;

        ccRight :            {Cursor right by one character}
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
          if LongFlagIsSet(meOptions, teSmartTabs) then
            teDoSmartTab
          else
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
          if LongFlagIsSet(meOptions, teWordWrap) then begin {!!.03}
            meSaveCurrentLine(True);                         {!!.01}
            meReformatParagraph;
            meCheckLineLimit;
          end;

        ccReformatG :        {Global reformat}
          if LongFlagIsSet(meOptions, teWordWrap) then begin {!!.03}
            meReformatGlobally;
            meCheckLineLimit;
          end;

        ccCenterLine :       {center the current line}
          teCenterLine;

        ccNewFile :          {read in a new file}
          if HaveFilename then
            teLoadNewFile;

        ccSaveFile :         {save file and continue}
          if HaveFilename then begin
            meSaveCurrentLine(True);
            SaveFile;
          end;

        ccSaveNamed :        {save file under a new name}
          if HaveFilename then
            teSaveNamedFile;

        ccSaveSwitch :       {save file and load a new one}
          if HaveFilename then begin
            meSaveCurrentLine(True);
            SaveFile;
            if cwGetLastError = 0 then begin
              meShowStatus;
              teLoadNewFile;
            end;
          end;

        ccSaveExit :         {save file and exit}
          if HaveFilename then begin
            meSaveCurrentLine(True);
            SaveFile;
            Finished := True;
          end;

        ccBlkBegin :         {mark block begin}
          teMarkBlockBegin;

        ccBlkEnd :           {mark block end}
          teMarkBlockEnd;

        ccBlkWord :          {mark current word as a block}
          teMarkWord;

        ccBlkToggle :        {toggle display of block}
          teToggleBlock;

        ccBlkCopy :          {copy block}
          begin                                {!!.13}
            teCopyBlock;
            meCheckLineLimit;                  {!!.13}
          end;                                 {!!.13}

        ccBlkMove :          {move block}
          teMoveBlock;

        ccBlkDelete :        {delete block}
          teDeleteBlock;

        ccBlkUCase,          {convert characters in block to upper case}
        ccBlkLCase,          {convert characters in block to lower case}
        ccBlkTCase :         {toggle case of characters in block}
          teChangeCaseBlock(cwCmd);

        ccBlkIndent :        {indent marked block}
          teChangeIndentBlock(teBlockIndent);

        ccBlkUnindent :      {unindent marked block}
          teChangeIndentBlock(-teBlockIndent);

        ccSetIndent :        {set indentation level for block un/indent}
          teSetBlockIndentPrompted;

        ccBlkRead :          {read block}
          begin                                {!!.13}
            teBlockRead;
            meCheckLineLimit;                  {!!.13}
          end;                                 {!!.13}

        ccBlkWrite :         {write block}
          (*teBlockWrite(False);*)             {!!.30}
          teBlockWrite(False, False);          {!!.30}

        ccBlkPrint :         {print block}
          (*teBlockWrite(True);*)              {!!.30}
          teBlockWrite(True, False);           {!!.30}

        ccBlkAppend :        {append block}    {!!.30}
          teBlockWrite(False, True);           {!!.30}

        ccJmpBegin :         {jump to beginning of block}
          teGotoBlockMarker(teBlkBegin);

        ccJmpEnd :           {jump to end of block}
          teGotoBlockMarker(teBlkEnd);

        ccSetMark0..ccSetMark9 : {set text marker 0..9}
          teSetMarker(cwCmd-ccSetMark0);

        ccJmpMark0..ccJmpMark9 : {jump to text marker 0..9}
          teGotoMarker(cwCmd-ccJmpMark0);

        ccPrevPos :          {cursor to previous position}
          teGotoBlockMarker(teLastPosition);

        ccMarkToggle :       {toggle display of text markers}
          begin
            meToggleOption(teMarkersOn);
            meForceRedraw := (teMarkerFlags <> 0);
          end;

        ccJmpLine :          {jump to a specific line}
          teGotoLinePrompted;

        ccSearch :           {search}
          teFindString(True, tescSearch);

        ccReplace :          {search and replace}
          teFindString(True, tescReplace);

        ccReSearch :         {repeat last search}
          teFindString(False, teLastSearch);

        ccTabToggle :        {toggle smart/fixed tabs}
          meToggleOption(teSmartTabs);

        ccTabSize :          {set size for fixed tabs}
          teSetTabSizePrompted;

        ccRtMargin :         {set right margin}
          teSetRightMarginPrompted;

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

      {fix teLastPosition if necessary}
      if teLastPosition.Line = 0 then
        teSaveLastPosition(meCurLine, meCurCol)
      else case cwCmd of
        ccBlkCopy,
        ccBlkMove,
        ccBlkDelete,
        ccBlkRead :
          {do nothing} ;
        else
          if meCurLine <> SaveCurLine then
            teSaveLastPosition(SaveCurLine, SaveCurCol);
      end;
      if cwGetLastError <> 0 then          {!!.13}
        cwCmd := ccError;                  {!!.13}
    until Finished or (cwCmd = ccError);

    ClearLongFlag(meOptions, teInProcess); {!!.03}

    {redraw the screen one last time}
    meRedraw;

    {restore break checking status}
    {CheckBreak := SaveBreak;}     {!!.01}

    {save window state}
    rwSaveWindowState;

    {$IFDEF UseMouse}
    {ShowMousePrim(SaveMouse);}     {!!.01}
    {$ENDIF}
  end;

{$IFDEF UseStreams}

  constructor TextEditor.Load(var S : IdStream);
    {-Load a text editor from a stream}
  var
    FName : String;
    FSize : LongInt;
  begin
    {initialize fields not stored to 0}
    FillChar(teSearchSt,
      Ofs(teLastPosition)-Ofs(teSearchSt)+SizeOf(teLastPosition), 0);

    {Load the underlying memofile}
    if not MemoFile.Load(S) then
      Fail;

    {Read data specific to the text editor}
    S.ReadRange(teBlockColor, teSearchSt);
    S.ReadRange(teBlockIndent, teEditFunc);
    @teEditFunc := S.ReadPointer;
    @teGetFile := S.ReadPointer;
    @teYesNoFunc := S.ReadPointer;

    {check for read errors}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {read in the old file if desired (MemoFile.Load won't do it)}
    FName := S.ReadString;
    if FName <> '' then begin
      ReadFile(FName, FSize);
      if cwGetLastError <> 0 then begin
        InitStatus := cwGetLastError;
        Done;
        Fail;
      end;
    end;
  end;

  procedure TextEditor.Store(var S : IdStream);
    {-Store a text editor in a stream}
  var
    FName : String;
    SaveReload : Boolean;
  begin
    {don't let MemoFile.Load reload a file}
    SaveReload := LongFlagIsSet(meOptions, teStreamReload);
    ClearLongFlag(meOptions, teStreamReload);

    {Store the memofile}
    MemoFile.Store(S);
    meSetOption(meOptions, SaveReload);
    if S.PeekStatus <> 0 then
      Exit;

    {Write data specific to the text editor}
    S.WriteRange(teBlockColor, teSearchSt);
    S.WriteRange(teBlockIndent, teEditFunc);
    S.WriteUserPointer(@teEditFunc, ptNil);
    S.WriteUserPointer(@teGetFile, ptNil);
    S.WriteUserPointer(@teYesNoFunc, ptNil);

    {save the current filename if desired}
    if SaveReload then
      FName := mfFileName
    else
      FName := '';
    S.WriteString(FName);
  end;

  procedure TextEditorStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing text editors}
  begin
    {register the memofile}
    MemoFileStream(SPtr);

    {register the text editor}
    with SPtr^ do begin
      RegisterType(otTextEditor, veTextEditor, TypeOf(TextEditor),
                   @TextEditor.Store, @TextEditor.Load);
      RegisterPointer(ptEditorCommands, @EditorCommands);
    end;
  end;

{$ENDIF}

begin
  {initialize command processor}
  EditorCommands.Init(@EditorKeySet, EditorKeyMax);
end.
