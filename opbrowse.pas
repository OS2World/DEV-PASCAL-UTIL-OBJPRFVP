{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$R-,S-,I-,V-,B-,F+,O+,A-} {!!.01}

{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPBROWSE.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1985,1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpBrowse;
  {-General purpose virtual file browser}


interface

uses
  Use32,
  Dos,
  OpInline,
  OpString,
  OpConst,  {!!.20}
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd,
  OpDos,
  OpFrame,
  OpWindow
  {$IFDEF UseDrag} {!!.03}
  , OpDrag         {!!.03}
  {$ENDIF}         {!!.03}
  ;

  {$I OPBROWSE.ICD}  {configuration data}

const
  MaxBuf = 4095;                  {Max offset in each page buffer}
  MaxPages = 499;                 {Maximum number of pages-1} {!!.20}
  MaxMarker = 9;                  {Max allowed position marker}
  MaxScrollCol : Word = 999;      {Maximum horizontal scrolling - arbitrary}
  MaxSearchLen = 30;              {Maximum length of search string}
  DefBrowseExt : ExtStr = '';     {Default file extension}

  {option codes}
  brHexMode        = $0001;  {hex mode}
  brTabExpand      = $0002;  {tab expansion}
  brStripHigh      = $0004;  {strip high bits (except hex mode)}
  brMousePage      = $0008;  {clicking on scroll bar scrolls by page}
  brNewFile        = $0010;  {flag set when a new file is loaded}
  brStreamReload   = $0020;  {reload previous file when activating a browser
                              stored in a stream}

  DefBrowseOptions : Word = brTabExpand+brMousePage;
  BadBrowseOptions : Word = 0;

  {search option characters}
  brBackward  = 'B';
  brNoCase    = 'U';
  brGlobal    = 'G';

  BrowseFileOpenMode : Byte = $20; {!!.13 FileMode: read only, deny write}

type
  BufferArray = array[0..MaxBuf] of Char;
  PageRec =
    record
      BlkNum : Word;              {BlkNum shl LogBufSize = FilePos}
      BufCnt : Word;              {Number of bytes in Buffer}
      LastUsed : Word;            {Quasi-time when last accessed}
      Buffer : ^BufferArray;      {Points to page buffer}
    end;
  PageArray = array[0..MaxPages] of PageRec;
const
  PageSize  = SizeOf(PageRec);    {size of a PageRec}
  OnePage   = PageSize+MaxBuf+1;  {amount of memory for one page}
type
  MarkerRec =
    record
      FilePos : LongInt;
      LineNum : LongInt;  {!!.03}
    end;
  MarkerArray = array[0..MaxMarker] of MarkerRec;

  BrowserPtr = ^Browser;
  BrowseEditFunc = function (MsgCode : Word; Prompt : string;
                             ForceUp, TrimBlanks : Boolean;
                             MaxLen : Byte; var S : string) : Boolean;
  BrowseGetFileFunc = function (MsgCode : Word; Prompt : string;
                                ForceUp, TrimBlanks : Boolean;
                                Writing, MustExist : Boolean;
                                MaxLen : Byte; DefExt : ExtStr;
                                var S : string) : Boolean;
  BrowseStatusProc = procedure(BP : BrowserPtr);
  Browser =
    object(CommandWindow)
      {... window stuff...}
      brPages : ^PageArray;       {Array of page buffers, 0..PageMax}
      brPageMax : Integer;        {Highest page allocated}
      brColMax : Integer;         {Maximum ColOfs}
      brWinWidth : Byte;          {Width of window}
      {----------------------------}
      brOptions : Word;           {option flags}
      brMask : Byte;              {for stripping high bits}
      brLPT : Byte;               {line printer (1-3)}
      brDefExt : ExtStr;          {default extension}
      brErrorDest : JumpRecord;   {For error handling}
      {----------------------------Procedure pointers}
      brEditFunc : BrowseEditFunc; {Function to edit a string}
      brGetFileFunc : BrowseGetFileFunc; {Function to get a filename}
      brStatusProc : BrowseStatusProc; {Routine to display status line}
      {----------------------------Colors}
      brBlockColor : Byte;        {Marked blocks - color}
      brBlockMono  : Byte;        {Marked blocks - mono}
      brMarkerColor  : Byte;      {Text markers - color}
      brMarkerMono   : Byte;      {Text markers - mono}
      brHighlightColor : Byte;    {Highlighted text - color}
      brHighlightMono  : Byte;    {Highlighted text - mono}
      {----------------------------Search stuff}
      brSearchSt : string[MaxSearchLen]; {String to search for}
      brOptionSt : string[5];     {Search options}
      {----------------------------Values following reinitialized in OpenFile}
      brLastPos : LongInt;        {Highest file offset = size-1}
      brCurPos : LongInt;         {Current display position in file}
      brEndPos : LongInt;         {Position of last line in window}
      brFndPos : LongInt;         {Position where search string found}
      brCurLine : LongInt;        {Current display line in file, 1..Max} {!!.03}
      brLastLine : LongInt;       {Last line number in file}             {!!.03}
      brLastLine2 : LongInt;      {Last line number in secondary mode (hex)} {!!.03}
      brColOfs : Integer;         {Horizontal scroll offset, 0..MaxCol}
      brMarkers : MarkerArray;    {Array of position markers}
      brBlkBegin : MarkerRec;     {Start of block}
      brBlkEnd : MarkerRec;       {End of block}
      brTicks : Word;             {Quasi-time, for page buffer replacement}
      brWorkingFlag : ShortInt;   {non-0 when a time-consuming routine is in progress}
      brBlockOn : Boolean;        {True when block is visible}
      {----------------------------File to browse}
      brFile : file;              {File being browsed}
      {....methods....}
      constructor Init(X1, Y1, X2, Y2 : Byte; HeapToUse : LongInt);
        {-Initialize the browser}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HeapToUse : LongInt);
        {-Initialize the browser with custom window options}
      destructor Done; virtual;
        {-Deallocate page buffers}
      procedure ProcessSelf; virtual; {!!.01}
        {-Process browse commands}
      {...}
      procedure OpenFile(FName : String);
        {-Open file for browsing}
      procedure CloseFile;
        {-Close the current browse file}
      function CurrentFileName : String;
        {-Return the name of the current browse file}
      {...}
      procedure brOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure brOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function brOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return true if all specified options are on}
      procedure ToggleModes;
        {-Toggle between hex and ASCII modes}
      procedure ToggleHighBitStripping;
        {-Toggle stripping of high bits}
      procedure SetDefaultExtension(DefExt : ExtStr);
        {-Default extension to use when prompting for filenames}
      procedure SetPrinter(LptNum : Byte);
        {-Set printer (1-3)}
      {...}
      procedure SetStatusProc(SP : BrowseStatusProc);
        {-Set status procedure}
      procedure SetEditProc(EF : BrowseEditFunc);
        {-Set edit function}
      procedure SetGetFileProc(GF : BrowseGetFileFunc);
        {-Set edit function}
      {...}
      procedure SetBlockAttr(Color, Mono : Byte);
        {-Set attributes for marked blocks}
      procedure SetMarkerAttr(Color, Mono : Byte);
        {-Set attributes for text markers}
      procedure SetHighlightAttr(Color, Mono : Byte);
        {-Set attributes for found text}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a browser from a stream}
      procedure Store(var S : IdStream);
        {-Store a browser in a stream}
    {$ENDIF}
      procedure GotoLine(LineNum : LongInt); {!!.03}
        {-Scroll display to the specified line}
      procedure GotoOffset(Offset : LongInt);
        {-Scroll display to the line nearest Offset}
      procedure Transfer(StartPos, StopPos : LongInt;
                         var Buffer; BufSize : Word);
        {-Copy browse characters from StartPos to StopPos into Buffer}
{.Z+}
      {+++ internal methods +++}
      procedure UpdateContents; virtual;
      function brGetFileName(MsgCode : Word; Prompt : string;
                             ForceUp, TrimBlanks : Boolean;
                             Writing, MustExist : Boolean;
                             MaxLen : Byte; var S : string) : Boolean; virtual;
      function brEdit(MsgCode : Word; Prompt : string;
                      ForceUp, TrimBlanks : Boolean;
                      MaxLen : Byte; var S : string) : Boolean; virtual;
      procedure brShowStatus; virtual;
      procedure brBufferLoaded(var Buffer; ByteCount : Word); virtual;
      function  brLineLength : Word;
      function  brHexLineNum(Pos : LongInt) : LongInt; {!!.03}
      function  brHexPos(Pos : LongInt) : LongInt;
      function  brHex8 : Boolean;
      procedure brResetMarkers;
      procedure brResetMask; {!!.02}
      procedure brGetWorkingPage;
      procedure brGetWorkingChar;
      procedure brCurWorkingSet;
      procedure brEndWorkingSet;
      procedure brLastLineCheck;
      function  brActualCol(Col : Integer) : Integer;  {!!.13}
      procedure brDrawLine(Y : Byte); virtual; {!!.03}
      procedure brNextLine;
      procedure brPrevLine;
      procedure brTransferPrim(Start, Stop : LongInt; var Buffer; BufSize : Word);
      procedure brWorking(ForceUpdate : Boolean);
      procedure brJumpToFilePos(FilePos, LineNum : LongInt); {!!.03}
      procedure brLineUp;
      procedure brLineDown;
      procedure brPageDown;
      procedure brPageUp;
      procedure brCharLeft(Delta : Integer);
      procedure brCharRight(Delta : Integer);
      procedure brLeftEdge;
      procedure brRightEdge;
      procedure brGotoMarker(MarkerNum : Word);
      procedure brTopOfFile;
      procedure brBottomOfFile(MoveOnly : Boolean);
      procedure brSetMarker(MarkerNum : Word);
      procedure brMarkBlockAtTop(var Marker : MarkerRec; IncludeTop : Boolean);
      procedure brMarkBlockAtBottom(var Marker : MarkerRec);
      procedure brGotoBlockMarker(var Marker : MarkerRec);
      procedure brToggleBlock;
      procedure brGotoLinePrim(Line : LongInt); {!!.03}
      procedure brGotoLine;
      procedure brFindString(Prompt : Boolean);
      procedure brWriteMarkedBlock(ToPrinter : Boolean); virtual;
      procedure brLoadNewFile;
      procedure brToggleOption(Option : Word);
      {$IFDEF UseScrollBars}
      procedure brSetupForScrollBars;
      procedure brUpdateScrollBars; virtual; {!!.14}
      {$ENDIF}
      {$IFDEF UseMouse}
      function  brProcessMouseCommand(Cmd : Word) : Boolean; {!!.03}
      {$ENDIF}
{.Z-}
    end;

{.Z+}
  {$IFDEF UseStreams}
  procedure BrowserStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing browsers}
  {$ENDIF}
{.Z-}

var
  {$IFDEF UseDrag}                    {!!.03}
  BrowseCommands : DragProcessor;     {!!.03}
  {$ELSE}                             {!!.03}
  BrowseCommands : CommandProcessor;
  {$ENDIF}                            {!!.03}

const
  {used only by BrowseStatus}
  StatusRow   : Byte = 1;    {default to first line of screen for status line}
  StatusColor : Byte = $F;   {color attribute for status line}
  StatusMono  : Byte = $F;   {mono attribute for status line}

procedure BrowseStatus(BP : BrowserPtr);
  {-Display status line}

{.Z+}                        {!!.03} {following declarations interfaced}
const
  LogBufSize    = 12;        {Log base 2 of page buffer size}
  NoBlk         = $FFFF;     {Flag that no block is in buffer}
  Hex8Threshold = 50;
  Hex8Width     = 40;
  Hex16Width    = 75;
  Hex8Max       = LongInt($FFFF)*8;
var
  WorkPos : LongInt;         {meWorking file position}
  WorkBlk : Word;            {meWorking file block = WorkPos shr LogBufSize}
  WorkBeg : Integer;         {First valid offset of WorkPtr}
  WorkEnd : Word;            {Next invalid offset of WorkPtr}
  WorkLine : LongInt;        {meWorking line number} {!!.03}
  WorkPtr : ^Char;           {Pointer to working character}
  WorkOfs : Integer absolute WorkPtr;
  WorkChr : Char;            {meWorking character}
{.Z-}

  {=======================================================================}

implementation


  procedure IncWorkingSet;
    {-Increment working position}
  begin
    Inc(WorkPos);
    Inc(WorkOfs);
  end;

  procedure DecWorkingSet;
    {-Decrement working position}
  begin
    Dec(WorkPos);
    Dec(WorkOfs);
  end;

  {$I OPBROWSE.IN1}

  constructor Browser.Init(X1, Y1, X2, Y2 : Byte; HeapToUse : LongInt);
    {-Initialize the browser}
  begin
    {initialize browser with default window options}
    if not Browser.InitCustom(X1, Y1, X2, Y2, DefaultColorSet,
                              DefWindowOptions, HeapToUse) then
      Fail;
  end;

  constructor Browser.InitCustom(X1, Y1, X2, Y2 : Byte;
                                 var Colors : ColorSet;
                                 Options : LongInt;
                                 HeapToUse : LongInt);
    {-Initialize the browser}
  var
    I : Word;
    PageCount : Integer;
  begin
    {initialize some fields so that destructor will work}
    brPageMax := -1;
    brPages := nil;
    FillChar(brSearchSt, (Ofs(brFile)-Ofs(brSearchSt))+SizeOf(brFile), 0);
    FileRec(brFile).Mode := fmClosed;

    {force wUserContents on}
    SetLongFlag(Options, wUserContents);

    {initialize the window}
    if not CommandWindow.InitCustom(X1, Y1, X2, Y2, Colors, Options,
                                    BrowseCommands, ucBrowse) then
      Fail;

    {initialize default options, etc.}
    brOptions := DefBrowseOptions;
    brWinWidth := wXH-wXL+1;
    brColMax := MaxScrollCol;
    wHelpIndex := 0;
    brDefExt := DefBrowseExt;
    brLPT := 1;

    {initialize procedure pointers}
    @brEditFunc := nil;
    @brGetFileFunc := nil;
    @brStatusProc := nil;

    {initialize colors}
    with Colors do begin
      brBlockColor := BlockColor;
      brBlockMono := BlockMono;
      brMarkerColor := MarkerColor;
      brMarkerMono := MarkerMono;
      brHighlightColor := HighlightColor;
      brHighlightMono := HighlightMono;
    end;

    {initialize the page buffers}
    if HeapToUse > MaxAvail then
      HeapToUse := MaxAvail;
    PageCount := HeapToUse div OnePage;
    if PageCount > Succ(MaxPages) then                                 {!!.20}
      PageCount := Succ(MaxPages);                                     {!!.20}

    if PageCount < 2 then begin
      {must have at least 2 pages}
      InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    {allocate page array}
    brPageMax := PageCount-1;
    I := PageCount*PageSize;
    if not GetMemCheck(brPages, I) then begin
      InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    {allocate page buffers}
    FillChar(brPages^, I, 0);
    for I := 0 to brPageMax do
      if not GetMemCheck(brPages^[I].Buffer, MaxBuf+1) then begin
        InitStatus := epFatal+ecOutOfMemory;
        Done;
        Fail;
      end;
  end;

  destructor Browser.Done;
    {-Deallocate page buffers}
  var
    I : Integer;
  begin
    if brPages <> nil then begin
      {Free the page buffers}
      for I := brPageMax downto 0 do
        FreeMemCheck(brPages^[I].Buffer, MaxBuf+1);
      FreeMemCheck(brPages, (brPageMax+1)*PageSize);

      {close the browse file}
      if FileRec(brFile).Mode <> fmClosed then
        CloseFile;
    end;

    {call ancestor's destructor}
    CommandWindow.Done;
  end;

  procedure Browser.brResetMask; {!!.02}
    {-Reset brMask field}
  begin
    {strip high bits?}
    if FlagIsSet(brOptions, brStripHigh) then
      brMask := $7F
    else
      brMask := $FF;
  end;

  procedure Browser.brResetMarkers;
    {-Reset all markers}
  begin
    FillChar(brMarkers, SizeOf(MarkerArray), $FF);
    FillChar(brBlkBegin, SizeOf(brBlkBegin), 0);
    FillChar(brBlkEnd, SizeOf(brBlkEnd), 0);
    brFndPos := $FFFFFFFF;

    brResetMask; {!!.02}
    (*           {!!.02}
    {strip high bits?}
    if FlagIsSet(brOptions, brStripHigh) then
      brMask := $7F
    else
      brMask := $FF;
    *)
  end;

  procedure Browser.brOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    if FlagIsSet(OptionFlags, brHexMode) and not FlagIsSet(brOptions, brHexMode) then begin
      ExchangeLongInts(brLastLine, brLastLine2); {!!.03}
      brResetMarkers;
    end;
    SetFlag(brOptions, OptionFlags and not BadBrowseOptions);
    brResetMask; {!!.02}
  end;

  procedure Browser.brOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    if FlagIsSet(OptionFlags, brHexMode) and FlagIsSet(brOptions, brHexMode) then begin
      ExchangeLongInts(brLastLine, brLastLine2); {!!.03}
      brResetMarkers;
    end;
    ClearFlag(brOptions, OptionFlags and not BadBrowseOptions);
    brResetMask; {!!.02}
  end;

  function Browser.brOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return true if all specified options are on}
  begin
    brOptionsAreOn := (brOptions and OptionFlags = OptionFlags);
  end;

  procedure Browser.SetStatusProc(SP : BrowseStatusProc);
    {-Set status procedure}
  begin
    brStatusProc := SP;
  end;

  procedure Browser.SetEditProc(EF : BrowseEditFunc);
    {-Set edit function}
  begin
    brEditFunc := EF;
  end;

  procedure Browser.SetGetFileProc(GF : BrowseGetFileFunc);
    {-Set edit function}
  begin
    brGetFileFunc := GF;
  end;

  procedure Browser.SetBlockAttr(Color, Mono : Byte);
    {-Set attributes for marked blocks}
  begin
    brBlockColor := Color;
    brBlockMono := MapMono(Color, Mono);
  end;

  procedure Browser.SetMarkerAttr(Color, Mono : Byte);
    {-Set attributes for text markers}
  begin
    brMarkerColor := Color;
    brMarkerMono := MapMono(Color, Mono);
  end;

  procedure Browser.SetHighlightAttr(Color, Mono : Byte);
    {-Set attributes for found text}
  begin
    brHighlightColor := Color;
    brHighlightMono := MapMono(Color, Mono);
  end;

  function Browser.brHexLineNum(Pos : LongInt) : LongInt; {!!.03}
    {-Find the line number corresponding to offset Pos}
  begin
    if brHex8 then
      brHexLineNum := (Pos div 8)+1
    else
      brHexLineNum := (Pos div 16)+1;
  end;

  function Browser.brHexPos(Pos : LongInt) : LongInt;
    {-Find the start of the line containing Pos}
  begin
    if brHex8 then
      brHexPos := Pos and $FFFFFFF8
    else
      brHexPos := Pos and $FFFFFFF0;
  end;

  function Browser.brHex8 : Boolean;
    {-Return True if we should use/are using 8-byte hex display}
  begin
    brHex8 := (brWinWidth <= Hex8Threshold) and (brLastPos < Hex8Max);
  end;

{$IFDEF UseScrollBars}
  procedure Browser.brSetupForScrollBars;
    {-Set boundaries for all scroll bars}
  begin
    ChangeAllScrollBars(0, MaxScrollCol, 0, brLastPos);
  end;

  procedure Browser.brUpdateScrollBars;
    {-Update horizontal and vertical scroll bars}
  var
    O : LongInt;
  begin
    if brCurLine = brLastLine then
      O := brLastPos
    else
      O := brCurPos;
    DrawAllSliders(brColOfs, O);
  end;
{$ENDIF}

  procedure Browser.brWorking(ForceUpdate : Boolean);
    {-Indicate that there will be a delay}
  begin
    {update status line if this is first call}
    if brWorkingFlag <> -1 then
      if (brWorkingFlag = 1) or ForceUpdate then begin
        brWorkingFlag := -1;
        brShowStatus;
      end
      else
        brWorkingFlag := 1;
  end;

  procedure Browser.brGetWorkingPage;
    {-Find or load page buffer, given WorkPos}
  label
    ExitPoint;
  var
    I : Word;
    LRU : Word;
    LP : Word;
  begin
    WorkBlk := WorkPos shr LogBufSize;
    LRU := $FFFF;

    {Scan loaded pages}
    for I := 0 to brPageMax do
      with brPages^[I] do
        if BlkNum = WorkBlk then begin
          LP := I;
          goto ExitPoint;
        end
        else if LastUsed < LRU then begin
          {Mark best page to load if needed}
          LRU := LastUsed;
          LP := I;
        end;

    {Page isn't loaded, read it now}
    with brPages^[LP] do begin
      brWorking(False);

      Seek(brFile, LongInt(WorkBlk) shl LogBufSize);
      I := IoResult;
      if I = 0 then begin
        BlockRead(brFile, Buffer^, MaxBuf+1, BufCnt);
        I := IoResult;
      end;

      if I = 0 then
        brBufferLoaded(Buffer^, BufCnt)
      else begin
        GotError(epFatal+I, emReadError);
        if brErrorDest.JmpPt = nil then
          Exit
        else
          LongJump(brErrorDest, 1);
      end;
    end;

ExitPoint:
    {Save page buffer information}
    with brPages^[LP] do begin
      {increment tick counter}
      Inc(brTicks);
      if brTicks = $FFFF then begin
        {Tick wraparound, reset all usage flags}
        brTicks := 0;
        for I := 0 to brPageMax do
          brPages^[I].LastUsed := 0;
      end;

      LastUsed := brTicks;
      BlkNum := WorkBlk;
      WorkPtr := Pointer(Buffer);
      WorkBeg := WorkOfs;
      WorkEnd := WorkBeg+BufCnt;
      Inc(WorkOfs, WorkPos-LongInt(WorkBlk) shl LogBufSize);
    end;
  end;

  procedure Browser.brGetWorkingChar;
    {-Return working character from WorkPos}
  const
    Null : Char = #0;
  begin
    if (WorkOfs >= WorkEnd) or (WorkOfs < WorkBeg) then
      {Out of working page buffer}
      if WorkPos > brLastPos then
        {Past end of file}
        WorkPtr := @Null
      else
        {Get required page}
        brGetWorkingPage;
    Byte(WorkChr) := Byte(WorkPtr^) and brMask;
  end;

  procedure Browser.brCurWorkingSet;
    {-Start the working set at the current position}
  begin
    if FlagIsSet(brOptions, brHexMode) then
      WorkPos := brHexPos(brCurPos)
    else
      WorkPos := brCurPos;
    WorkPtr := nil;
    WorkBeg := MaxInt;
    WorkEnd := 0;
  end;

  procedure Browser.brEndWorkingSet;
    {-Start the working set at the end of screen position}
  begin
    WorkPos := brEndPos;
    WorkPtr := nil;
    WorkEnd := 0;
  end;

  procedure Browser.brLastLineCheck;
    {-Check to see if brLastLine is initialized, calculate if not}
  begin
    if brLastLine = 0 then
      if FlagIsSet(brOptions, brHexMode) then
        brLastLine := brHexLineNum(brLastPos)
      else begin
        {Find last line in file}
        brWorking(True);

        brCurPos := 0;
        brCurLine := 1;
        brCurWorkingSet;
        while WorkPos < brLastPos do begin
          brNextLine;
          Inc(brCurLine);
        end;
        brLastLine := brCurLine;
      end;
  end;

  procedure InsertHex(var Dest; Chr : Char);
    {-Convert Ch to hex and store in Dest}
  {$IFDEF VIRTUALPASCAL}
  assembler; {$FRAME-} {$USES edi,ecx}
  asm
    mov   al,chr
    mov   edi,Dest
    mov   ah,al
    and   ah,0fh
    cmp   ah,0ah
    jb    @x1
    add   ah,'A'-0ah
    jmp   @x2
  @x1:
    add   ah,'0'
  @x2:
    shr   al,4
    cmp   al,0ah
    jb    @x3
    add   al,'A'-0ah
    jmp   @x4
  @x3:
    add   al,'0'
  @x4:
    stosw
  end;
  {$ELSE}
  inline(
    $58/                   {pop ax       ;char into al}
    $5F/                   {pop di       ;es:di => dest}
    $07/                   {pop es}
    $88/$C4/               {mov ah,al    ;ah = al}
    $80/$E4/$0F/           {and ah,$0f   ;ah has low nibble}
    $80/$FC/$0A/           {cmp ah,$0a   ;ah >= $a?}
    $72/$05/               {jb x1}
    $80/$C4/$37/           {add ah,'A'-$a}
    $EB/$03/               {jmp short x2}
                           {x1:}
    $80/$C4/$30/           {add ah,'0'}
                           {x2:}
    $B1/$04/               {mov cl,4     ;al has high nibble}
    $D2/$E8/               {shr al,cl}
    $3C/$0A/               {cmp al,$0a   ;al >= $a?}
    $72/$04/               {jb x3}
    $04/$37/               {add al,'A'-$a}
    $EB/$02/               {jmp short x4}
                           {x3:}
    $04/$30/               {add al,'0'}
                           {x4:}
    $AB);                  {stosw        ;store it}
  {$ENDIF}

  function Browser.brActualCol(Col : Integer) : Integer;  {!!.13}
    {-Return actual column for Col, accounting for tab expansion}
  var
    I : Integer;
    AC : Integer;
  begin
    if FlagIsSet(brOptions, brHexMode) or not FlagIsSet(brOptions, brTabExpand) then begin
      brActualCol := Col;
      Exit;
    end;

    brCurWorkingSet;

    AC := 0;
    for I := 1 to Col do begin
      if (WorkOfs >= WorkEnd) then
        brGetWorkingChar
      else
        Byte(WorkChr) := Byte(WorkPtr^) and brMask;
      if WorkChr = ^I then
        AC := (AC+8) and $FFF8
      else
        Inc(AC);
      Inc(WorkPos);
      Inc(WorkOfs);
    end;
    brCurWorkingSet;
    brActualCol := AC;
  end;

  procedure Browser.brDrawLine(Y : Byte);
    {-Draw working line to row Y of screen}
  label
    EndLoop, EndHexLoop, NormalChar;
  var
    X : Integer;
    XTab : Integer;
    XRight : Integer;
    MNum : Word;
    Attr : Byte;
    SLine : String;
    SRec : record                 {Record used to shift string right by one}
             SLen : Byte;
             ShStr : String;
           end absolute SLine;
    HexSt : string[8];
    Col1, Col2 : Word;
    Count : Word;
    ColOfs : Word;
    LastPos : LongInt;
    WinWidth : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {Check for markers}
    MNum := $FFFF;
    for X := 0 to MaxMarker do
      if brMarkers[X].FilePos = WorkPos then
        MNum := X;

    {Select attribute for line}
    if WorkPos = brFndPos then
      Attr := ColorMono(brHighlightColor, brHighlightMono)
    else if brBlockOn and (WorkPos >= brBlkBegin.FilePos) and (WorkPos < brBlkEnd.FilePos) then
      Attr := ColorMono(brBlockColor, brBlockMono)
    else
      Attr := ColorMono(wTextColor, wTextMono);

    {are we in hex mode?}
    ColOfs := brColOfs;
    LastPos := brLastPos;
    WinWidth := brWinWidth;
    if FlagIsSet(brOptions, brHexMode) then begin
      {Initialize screen line}                             {!!.03}
      if brHex8 then                                       {!!.03}
        SRec.SLen := Hex8Width                             {!!.03}
      else                                                 {!!.03}
        SRec.SLen := Hex16Width;                           {!!.03}

      {skip all this if we're past end-of-file}
      if (WorkPos > LastPos) or (brColOfs >= SRec.SLen) then begin {!!.03}
        SRec.SLen := 0;
        goto EndHexLoop;
      end;

      FillChar(SLine[1], SRec.SLen, ' ');

      {plug in file offset and initialize}
      HexSt := HexL(WorkPos);
      if SRec.SLen = Hex8Width then begin
        Col1 := 8;
        Col2 := Hex8Width-7;
        Count := 8;
        MoveFast(HexSt[4], SLine[1], 5); {!!.01}
      end
      else begin
        Col1 := 10;
        Col2 := Hex16Width-15;
        Count := 16;
        MoveFast(HexSt[3], SLine[1], 6); {!!.01}
      end;

      for X := 1 to Count do
        {do nothing if beyond end of file}
        if WorkPos <= LastPos then begin
          {get next character}
          if (WorkOfs >= WorkEnd) then
            brGetWorkingChar
          else
            Byte(WorkChr) := Byte(WorkPtr^) and brMask;

          {plug everything in}
          InsertHex(SLine[Col1], WorkPtr^);
          if brMask = $FF then
            SLine[Col2] := WorkChr
          else case WorkChr of
            ' '..'~' :
              SLine[Col2] := WorkChr
            else
              SLine[Col2] := '.';
          end;

          {advance counters}
          Inc(Col1, 3);
          Inc(Col2, 1);
          Inc(WorkPos);
          Inc(WorkOfs);
        end;

      {if we're scrolled, shift text to the left}
      if (ColOfs >= WinWidth) then
        SRec.SLen := 0
      else if (ColOfs > 0) then begin
        MoveFast(SLine[ColOfs+1], SLine[1], SRec.SLen-ColOfs); {!!.01}
        Dec(SRec.SLen, ColOfs);
      end;

EndHexLoop:
      {pad end of line with blanks}
      if WinWidth > SRec.SLen then
        FillChar(SLine[SRec.SLen+1], WinWidth-SRec.SLen, ' ');
      SRec.SLen := WinWidth;
    end
    else begin
      {Initialize screen line}
      SRec.SLen := WinWidth;
      FillChar(SLine[1], WinWidth, ' ');

      {skip all this if we're past end-of-file}
      if WorkPos > LastPos then
        goto EndLoop;

      X := 1;
      XRight := ColOfs+WinWidth;
      while X <= XRight do begin
        if WorkPos > LastPos then begin
          {Past end of file}
          if X > ColOfs then
            {Transfer character to line buffer}
            SLine[X-ColOfs] := ' ';
          Inc(X);
        end
        else begin
          if (WorkOfs >= WorkEnd) then
            brGetWorkingChar
          else
            Byte(WorkChr) := Byte(WorkPtr^) and brMask;
          case WorkChr of
            ^M :                  {End of line}
              goto EndLoop;
            ^I :                  {Expand tabs}
              if FlagIsSet(brOptions, brTabExpand) then begin
                XTab := ((X+7) and $FFF8)+1; {!!.13}
                if XTab > XRight then
                  XTab := XRight;
                while X < XTab do begin
                  if X > ColOfs then
                    SLine[X-ColOfs] := ' ';
                  Inc(X);
                end;
                Inc(WorkPos);
                Inc(WorkOfs);
              end
              else
                goto NormalChar;
            else begin
NormalChar:
              if X > ColOfs then begin
                if (WorkChr = ^Z) then
                  WorkChr := ' ';
                {Transfer character to line buffer}
                SLine[X-ColOfs] := WorkChr;
              end;
              Inc(X);
              Inc(WorkPos);
              Inc(WorkOfs);
            end;
          end;
        end;
      end;
    end;

EndLoop:
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {Write line buffer to screen}
    if MNum <> $FFFF then begin
      {Put mark at left edge}
      FastWrite(Char(Ord('0')+MNum), Y, wXL, ColorMono(brMarkerColor, brMarkerMono));
      SRec.ShStr[0] := Char(WinWidth-1);
      FastWrite(SRec.ShStr, Y, wXL+1, Attr);
    end
    else
      FastWrite(SLine, Y, wXL, Attr);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  function Browser.brLineLength : Word;
    {-Return length of line starting at WorkPos}
  var
    X : Word;
  begin
    X := 0;
    if FlagIsSet(brOptions, brHexMode) then
      if brHex8 then
        brLineLength := Hex8Width
      else
        brLineLength := Hex16Width
    else repeat
      brGetWorkingChar;
      if WorkPos > brLastPos then begin
        {Past end of file}
        brLineLength := X;
        Exit;
      end else
        case WorkChr of
          ^I :                  {Expand tabs}
            begin
              if FlagIsSet(brOptions, brTabExpand) then
                X := (X+8) and $FFF8                    {!!.13}
              else
                Inc(X);
              IncWorkingSet;
            end;
          ^M :                  {End of line or file}
            begin
              brLineLength := X;
              Exit;
            end;
        else
          Inc(X);
          IncWorkingSet;
        end;
    until False;
  end;

  procedure Browser.brNextLine;
    {-Move working position forward to start of next line}
  var
    C, Count : Word;
  begin
    if FlagIsSet(brOptions, brHexMode) then begin
      if brHex8 then
        Count := 8
      else
        Count := 16;
      Inc(WorkPos, Count);
      if WorkPos > brLastPos then
        WorkPos := brLastPos+1;
      Inc(WorkOfs, Count);

      {force page change if necessary}
      brGetWorkingChar;
    end
    else repeat
      {Scan the rest of the current page buffer}
      for C := 1 to WorkEnd-WorkOfs do
        if WorkPos > brLastPos then
          {Past end of file}
          Exit
        else if (Char(Byte(WorkPtr^) and brMask) = ^M) then begin
          IncWorkingSet;
          brGetWorkingChar;
          if WorkChr = ^J then
            IncWorkingSet;
          Exit;
        end
        else
          IncWorkingSet;

      {Force page change}
      brGetWorkingChar;

      {Past end of file?}          {!!.14}
      if WorkPos > brLastPos then  {!!.14}
        Exit;                      {!!.14}
    until False;
  end;

  procedure Browser.brPrevLine;
    {-Move working position backward to start of previous line}
  label
    ExitPoint;
  var
    C, Count : Word;
  begin
    if FlagIsSet(brOptions, brHexMode) then begin
      if brHex8 then
        Count := 8
      else
        Count := 16;
      if WorkPos >= Count then
        Dec(WorkPos, Count)
      else
        WorkPos := 0;
      Dec(WorkOfs, Count);

      {force page change if necessary}
      brGetWorkingChar;
    end
    else begin
      {Skip over previous EOL}
      if WorkPos = 0 then
        Exit;
      DecWorkingSet;
      brGetWorkingChar;
      if WorkChr = ^J then begin
        DecWorkingSet;
        brGetWorkingChar;
      end;
      if WorkPos = 0 then
        Exit;
      if WorkChr = ^M then begin
        DecWorkingSet;
        brGetWorkingChar;
      end;

      repeat
        {Scan front part of page buffer}
        for C := 0 to WorkOfs-WorkBeg do
          case Char(Byte(WorkPtr^) and brMask) of
            ^M {, ^J} :         {!!.11}
              goto ExitPoint;
            else
              DecWorkingSet;
          end;
        if WorkPos < 0 then
          goto ExitPoint;

        {Force page change}
        brGetWorkingChar;
      until False;

  ExitPoint:
      {Next character after end of previous line}
      IncWorkingSet;
      brGetWorkingChar;     {!!.11}
      if WorkChr = ^J then  {!!.11}
        IncWorkingSet;      {!!.11}
    end;
  end;

  procedure Browser.UpdateContents;
    {-Redraw the complete browse window}
  var
    Y : Word;
    Hex : Boolean;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {make sure width is correct}
    brWinWidth := wXH-wXL+1;

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    brCurWorkingSet;
    Hex := FlagIsSet(brOptions, brHexMode);
    for Y := wYL to wYH do begin
      if Y = wYH then
        brEndPos := WorkPos;
      brDrawLine(Y);
      if not Hex then
        brNextLine;
    end;

    {$IFDEF UseScrollBars}
    {update scroll bars}
    brUpdateScrollBars;
    {$ENDIF}

    StackWindow.UpdateContents; {!!.01}

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure Browser.brJumpToFilePos(FilePos, LineNum : LongInt); {!!.03}
    {-Set current filepos and redraw}
  begin
    brCurPos := FilePos;
    brCurLine := LineNum;
    brCurWorkingSet;
    if IsCurrent then
      UpdateContents;
  end;

  procedure Browser.brLineUp;
    {-Scroll up one line}
  var
    Y : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    if brCurPos > 0 then begin
      brCurWorkingSet;
      brPrevLine;
      Dec(brCurLine);
      brCurPos := WorkPos;

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      ScrollWindowDown(wXL, wYL, wXH, wYH, 1);
      brDrawLine(wYL);

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}

      if brEndPos >= brLastPos then begin
        {Range of lines may be compressed}
        brCurWorkingSet;
        for Y := wYL to wYH-1 do
          brNextLine;
      end
      else begin
        brEndWorkingSet;
        brPrevLine;
      end;
      brEndPos := WorkPos;
    end;
  end;

  procedure Browser.brLineDown;
    {-Scroll down one line}
    {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    if FlagIsSet(brOptions, brHexMode) and (brCurLine = brLastLine) then
      Exit
    else if brCurPos < brLastPos then begin
      brCurWorkingSet;
      brNextLine;
      Inc(brCurLine);
      brCurPos := WorkPos;
      brEndWorkingSet;
      brNextLine;
      brEndPos := WorkPos;

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      ScrollWindowUp(wXL, wYL, wXH, wYH, 1);
      brDrawLine(wYH);

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end
    else if brLastLine = 0 then
      brLastLine := brCurLine;
  end;

  procedure Browser.brPageDown;
    {-Move current pointer down one window's worth and redraw}
  var
    Y : Word;
  begin
    if brCurPos < brLastPos then begin
      brCurWorkingSet;
      for Y := 1 to wYH-wYL do
        if WorkPos < brLastPos then begin
          brNextLine;
          Inc(brCurLine);
        end
        else if brLastLine = 0 then
          brLastLine := brCurLine;
      if FlagIsSet(brOptions, brHexMode) and (WorkPos > brLastPos) then begin
        brCurLine := brHexLineNum(brLastPos);
        brCurPos := brHexPos(brLastPos);
      end
      else
        brCurPos := WorkPos;
      UpdateContents;
    end;
  end;

  procedure Browser.brPageUp;
    {-Move current pointer up one window's worth and redraw}
  var
    Y : Word;
  begin
    if brCurPos > 0 then begin
      brCurWorkingSet;
      for Y := 1 to wYH-wYL do
        if WorkPos > 0 then begin
          brPrevLine;
          Dec(brCurLine);
        end;
      brCurPos := WorkPos;
      UpdateContents;
    end;
  end;

  procedure Browser.brCharLeft(Delta : Integer);
    {-Scroll left by Delta columns}
  begin
    if brColOfs > 0 then begin
      Dec(brColOfs, Delta);
      if brColOfs < 0 then
        brColOfs := 0;
      UpdateContents;
    end;
  end;

  procedure Browser.brCharRight(Delta : Integer);
    {-Scroll right by Delta columns}
  begin
    if brColOfs < brColMax then begin
      Inc(brColOfs, Delta);
      if brColOfs > brColMax then
        brColOfs := brColMax;
      UpdateContents;
    end;
  end;

  procedure Browser.brLeftEdge;
    {-Scroll to left edge}
  begin
    brCharLeft(brColMax);
  end;

  procedure Browser.brRightEdge;
    {-Scroll to right edge}
  var
    Len : Word;
    MaxLen : Word;
    Y : Integer;
  begin
    if brCurPos < brLastPos then begin
      {Find longest line in window}
      brCurWorkingSet;
      MaxLen := 0;
      for Y := 0 to wYH-wYL do
        if WorkPos < brLastPos then begin
          Len := brLineLength;
          brNextLine;
          if Len > MaxLen then
            MaxLen := Len;
        end;
      {Scroll to show right edge}
      Y := MaxLen-brColOfs-brWinWidth;
      if Y > 0 then
        brCharRight(Y);
    end;
  end;

  procedure Browser.brGotoMarker(MarkerNum : Word);
    {-Go to stored mark position}
  begin
    if MarkerNum <= MaxMarker then
      with brMarkers[MarkerNum] do
        if FilePos <> $FFFFFFFF then
          brJumpToFilePos(FilePos, LineNum);
  end;

  procedure Browser.brTopOfFile;
    {-Move to top of file}
  begin
    if brCurPos > 0 then
      brJumpToFilePos(0, 1);
  end;

  procedure Browser.brBottomOfFile(MoveOnly : Boolean);
    {-Move to bottom of file}
  begin
    if brCurPos < brLastPos then begin
      brLastLineCheck;
      if FlagIsSet(brOptions, brHexMode) then
        brCurPos := brHexPos(brLastPos)
      else
        brCurPos := brLastPos;
      brCurLine := brLastLine;
      brCurWorkingSet;
      if not MoveOnly then
        brPageUp
      else begin
        brPrevLine;
        brNextLine;
        brCurPos := WorkPos;
      end;
    end;
  end;

  procedure Browser.brSetMarker(MarkerNum : Word);
    {-Store current file position as marker}
  begin
    if MarkerNum <= MaxMarker then begin
      with brMarkers[MarkerNum] do
        if FilePos = brCurPos then
          {Clear existing marker}
          FilePos := $FFFFFFFF
        else if brCurPos < brLastPos then begin
          {Set marker here}
          FilePos := brCurPos;
          LineNum := brCurLine;
        end;
      UpdateContents;
    end;
  end;

  procedure Browser.brMarkBlockAtTop(var Marker : MarkerRec;
                                     IncludeTop : Boolean);
    {-Save top block marker}
  begin
    with Marker do begin
      if IncludeTop then begin
        brCurWorkingSet;
        brNextLine;
        FilePos := WorkPos;
      end
      else
        FilePos := brCurPos;
      LineNum := brCurLine+Ord(IncludeTop);
      brBlockOn := True;
      UpdateContents;
    end;
  end;

  procedure Browser.brMarkBlockAtBottom(var Marker : MarkerRec);
    {-Save bottom block marker}
  var
    Y : Word;
  begin
    with Marker do begin
      if brCurPos < brLastPos then begin
        brCurWorkingSet;
        WorkLine := brCurLine;
        for Y := 0 to wYH-wYL do
          if WorkPos < brLastPos then begin
            brNextLine;
            Inc(WorkLine);
          end;
        FilePos := WorkPos;
        LineNum := WorkLine;
      end
      else begin
        FilePos := brCurPos;
        LineNum := brCurLine;
      end;
      brBlockOn := True;
      UpdateContents;
    end;
  end;

  procedure Browser.brGotoBlockMarker(var Marker : MarkerRec);
    {-Reposition to a block marker}
  begin
    with Marker do
      if LineNum <> 0 then
        brJumpToFilePos(FilePos, LineNum);
  end;

  procedure Browser.brToggleBlock;
    {-Toggle block visibility}
  begin
    brBlockOn := not brBlockOn;
    UpdateContents;
  end;

  procedure Browser.ToggleModes;
    {-Toggle between hex and ASCII modes}
  begin
    {switch modes}
    if FlagIsSet(brOptions, brHexMode) then
      brOptionsOff(brHexMode)
    else
      brOptionsOn(brHexMode);

    WorkPos := brCurPos;
    if (brLastLine <> 0) and (WorkPos > (brLastPos div 2)) then begin
      brCurPos := brLastPos;
      brCurLine := brLastLine;
    end
    else begin
      brCurPos := 0;
      brCurLine := 1;
    end;
    GotoOffset(WorkPos);
    UpdateContents;
  end;

  procedure Browser.ToggleHighBitStripping;
    {-Toggle stripping of high bits}
  begin
    brToggleOption(brStripHigh);
    brLastLine := 0;
    brCurPos := 0;
    brCurLine := 1;
    {brResetMarkers;} {!!.02}
    brResetMask;      {!!.02}
    brJumpToFilePos(0, 1);
  end;

  procedure Browser.SetDefaultExtension(DefExt : ExtStr);
    {-Default extension to use when prompting for filenames}
  begin
    brDefExt := DefExt;
  end;

  procedure Browser.SetPrinter(LptNum : Byte);
    {-Set printer (1-3)}
  begin
    case LptNum of
      1..3 : brLPT := LptNum;
      else   brLPT := 1;
    end;
  end;

  procedure Browser.brToggleOption(Option : Word);
    {-Toggle the specified option}
  begin
    if FlagIsSet(brOptions, Option) then
      ClearFlag(brOptions, Option)
    else
      SetFlag(brOptions, Option);
    UpdateContents;
  end;

  {$IFDEF UseMouse}
  function Browser.brProcessMouseCommand(Cmd : Word) : Boolean; {!!.03}
    {-Process ccMouseSel command. Returns True to return control to user.}
  var
    L : LongInt;
    FramePos : FramePosType;
    HotCode : Byte;
    Dragging : Boolean;  {!!.03}
  begin
    brProcessMouseCommand := False;

    {determine position of mouse}
    L := cwMouseResults(Cmd, FramePos, HotCode);    {!!.03} {!!.13}

    {Should mouse event be ignored?}                             {!!.03}
    if cwIgnoreMouseEvent(Dragging, Cmd, FramePos, HotCode) then {!!.03}
      Exit;                                                      {!!.03}

    case HotCode of
      hsNone :           {not a hot spot}
        case FramePos of
          frInsideActive :       {inside window}
            {does nothing in this unit} ;

          frTL..frRR,            {on the frame}
          frInsideFrame,         {inside window frame but not in window boundaries}
          frOutsideFrame :       {outside window frame}
            brProcessMouseCommand := LongFlagIsSet(wFlags, wAllMouseEvents);
        end;
      {$IFDEF UseScrollBars}
      hsDecV :           {the decrement fixture of a vertical scroll bar}
        if FlagIsSet(brOptions, brMousePage) then
          brPageUp
        else
          brLineUp;
      hsDecH :           {the decrement fixture of a horizontal scroll bar}
        brCharLeft(10);
      hsIncV :           {the increment fixture of a vertical scroll bar}
        if FlagIsSet(brOptions, brMousePage) then
          brPageDown
        else
          brLineDown;
      hsIncH :           {the increment fixture of a horizontal scroll bar}
        brCharRight(10);
      hsBar :            {the slider portion of a scroll bar}
        case FramePos of
          frLL, frRR :   {vertical scroll bar}
            begin
              L := TweakSlider(FramePos, MouseKeyWordY+MouseYLo, L, 1);
              if FlagIsSet(brOptions, brHexMode) then
                if brHex8 then
                  L := brHexPos(L+$07)
                else
                  L := brHexPos(L+$0F);
              if L <= 0 then
                {goto top of file}
                brTopOfFile
              else begin
                if L >= brLastPos then
                  {goto end of file}
                  brBottomOfFile(True)
                else
                  {goto specified position}
                  GotoOffset(L);
                UpdateContents;
              end;
            end;
          else begin     {horizontal scroll bar}
            brColOfs := TweakSlider(FramePos, MouseKeyWordX+MouseXLo, L, 1);
            UpdateContents;
          end;
        end;
      {$ENDIF}
      hsSpot,            {a single character hot spot}
      hsRegion0..255 :   {a user-defined region relative to a frame}
        brProcessMouseCommand := (Cmd <> ccMouseAuto); {!!.03}
    end;

  end;
  {$ENDIF}

  procedure Browser.ProcessSelf; {!!.01}
    {-Process browse commands}
  var
    CmdNum : Word;
    Finished : Boolean;
    LKW : Byte;
    SaveCurPos : LongInt;
    SaveColOfs : Integer;
    {$IFDEF UseMouse}
    {SaveMouse : Boolean;} {!!.01}
    {$ENDIF}
  begin
    {Set up error handler}
    if SetJump(brErrorDest) <> 0 then begin
      {control returns here if error occurs while browsing}
      cwCmd := ccError;
      Exit;
    end;

    {Clear any errors} {!!.01}
    ClearErrors;       {!!.01}

    {$IFDEF UseScrollBars}
    {make sure we're set up for scroll bars}
    brSetupForScrollBars;
    {$ENDIF}

    {Draw initial screen if not already done}
    brWorkingFlag := 0;
    Draw;
    if RawError <> 0 then begin                 {!!.01}
      {GotError(wNotCurrent, emNullError);}     {!!.01}
      Exit;
    end;

    (*                {!!.01}
    {$IFDEF UseMouse}
    SaveMouse := MouseCursorOn;
    if cwCmdPtr^.MouseEnabled then
      ShowMouse;
    {$ENDIF}
    *)

    {save current offsets}
    SaveCurPos := brCurPos;
    SaveColOfs := brColOfs;

    {initialize}
    SetCursor(cuHidden);
    GotoXY(1, 1);
    Finished := False;
    CmdNum := 1;

    repeat
      {update the status line}
      brWorkingFlag := 0;
      brShowStatus;

      {update scroll bars if necessary}
      if (SaveCurPos <> brCurPos) or (SaveColOfs <> brColOfs) then begin
        {$IFDEF UseScrollBars}
        brUpdateScrollBars;
        {$ENDIF}
        SaveCurPos := brCurPos;
        SaveColOfs := brColOfs;
      end;

      {get the next command}
      GetNextCommand;

      {Execute command}
      case cwCmd of
        ccUp :
          brLineUp;
        ccDown :
          brLineDown;
        ccPageUp :
          brPageUp;
        ccPageDn :
          brPageDown;
        ccLeft :
          brCharLeft(1);
        ccRight :
          brCharRight(1);
        ccHome :
          brLeftEdge;
        ccEnd :
          brRightEdge;
        ccWordLeft :
          brCharLeft(10);
        ccWordRight :
          brCharRight(10);
        ccTopOfFile :
          brTopOfFile;
        ccEndOfFile :
          brBottomOfFile(False);
        ccSetMark0..ccSetMark9 :
          brSetMarker(cwCmd-ccSetMark0);
        ccJmpMark0..ccJmpMark9 :
          brGotoMarker(cwCmd-ccJmpMark0);
        ccSearch :
          brFindString(True);
        ccReSearch :
          brFindString(False);
        ccBlkBegin :
          brMarkBlockAtTop(brBlkBegin, False);
        ccBlkEnd :
          brMarkBlockAtTop(brBlkEnd, True);
        ccBlkBottom :
          brMarkBlockAtBottom(brBlkEnd);
        ccJmpBegin :
          brGotoBlockMarker(brBlkBegin);
        ccJmpEnd :
          brGotoBlockMarker(brBlkEnd);
        ccBlkWrite :
          brWriteMarkedBlock(False);
        ccBlkPrint :
          brWriteMarkedBlock(True);
        ccNewFile :
          brLoadNewFile;
        ccBlkToggle :
          brToggleBlock;
        ccJmpLine :
          brGotoLine;
        ccHexMode :
          ToggleModes;
        ccStripHigh :
          ToggleHighBitStripping;
        ccTabExpand :
          brToggleOption(brTabExpand);
        ccQuit,
        ccUser0..ccUser65335 :
          Finished := True;
        {$IFDEF UseMouse}
        ccMouseAuto,                                {!!.03}
        ccMouseDown,                                {!!.03}
        ccMouseSel :
          Finished := brProcessMouseCommand(cwCmd); {!!.03}
        {$ENDIF}
        ccHelp :
          RequestHelp(wHelpIndex);
        else if (cwCmd <= 255) and (GetExitCommandPtr <> nil) then      {!!.01}
          {Possibly a special exit command defined by a derived object} {!!.01}
          Finished := (cwCmd in GetExitCommandPtr^);                    {!!.01}
      end;
      if cwGetLastError <> 0 then
        cwCmd := ccError;
    until Finished or (cwCmd = ccError);

    {update the status line again}
    brShowStatus;

    {save window state}
    rwSaveWindowState;

    {$IFDEF UseMouse}
    {ShowMousePrim(SaveMouse);} {!!.01}
    {$ENDIF}
  end;

{$IFDEF UseStreams}

  constructor Browser.Load(var S : IdStream);
    {-Load a browser from a stream}
  var
    FName : String;
    I : Word;
  begin
    {initialize fields not stored to 0}
    brPages := nil;
    FillChar(brSearchSt, Ofs(brFile)-Ofs(brSearchSt)+SizeOf(brFile), 0);

    {Load the underlying command window}
    if not CommandWindow.Load(S) then
      Fail;

    {set the command processor if necessary}
    if cwCmdPtr = nil then
      SetCommandProcessor(BrowseCommands);

    {Read data specific to the browser}
    S.ReadRange(brPageMax, brErrorDest);
    FillChar(brErrorDest, SizeOf(brErrorDest), 0);

    @brEditFunc := S.ReadPointer;
    @brGetFileFunc := S.ReadPointer;
    @brStatusProc := S.ReadPointer;

    S.ReadRange(brBlockColor, brSearchSt);

    FName := S.ReadString;

    {check for read errors}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {try to allocate page array}
    I := (brPageMax+1)*PageSize;
    if not GetMemCheck(brPages, I) then begin
      InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    {allocate page buffers}
    FillChar(brPages^, I, 0);
    for I := 0 to brPageMax do
      if not GetMemCheck(brPages^[I].Buffer, MaxBuf+1) then begin
        InitStatus := epFatal+ecOutOfMemory;
        Done;
        Fail;
      end;

    if FName <> '' then begin
      {filename not empty--try to reload the file}
      OpenFile(FName);

      {fail if unable to open the file}
      if cwGetLastError <> 0 then begin
        InitStatus := cwGetLastError;
        Done;
        Fail;
      end;
    end;
  end;

  procedure Browser.Store(var S : IdStream);
    {-Store a browser in a stream}
  var
    FName : String;
  begin
    {Store the underlying command window}
    CommandWindow.Store(S);
    if S.PeekStatus <> 0 then
      Exit;

    {Write data specific to the browser}
    S.WriteRange(brPageMax, brErrorDest);

    S.WriteUserPointer(@brEditFunc, ptNil);
    S.WriteUserPointer(@brGetFileFunc, ptNil);
    S.WriteUserPointer(@brStatusProc, ptNil);

    S.WriteRange(brBlockColor, brSearchSt);

    {save the current filename if desired}
    if FlagIsSet(brOptions, brStreamReload) then
      FName := CurrentFileName
    else
      FName := '';
    S.WriteString(FName);
  end;

  procedure BrowserStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing browsers}
  begin
    {register the command window}
    CommandWindowStream(SPtr);

    {register the browser}
    with SPtr^ do begin
      RegisterType(otBrowser, veBrowser, TypeOf(Browser),
                   @Browser.Store, @Browser.Load);
      RegisterPointer(ptBrowseCommands, @BrowseCommands);
    end;
  end;

{$ENDIF}

  procedure BrowseStatus(BP : BrowserPtr);
    {-Display status line}
  const
    RawStatus : string[80] =
    {         1         2         3     x   4         5         6         7         8}
    {12345678901234567890123456789012345678901234567890123456789012345678901234567890}
    '  x                                                       Line x       Col x    ';
  var
    S : string[80];
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}

    procedure MergeString(T : String; N : Byte; var S : String);
    begin
      MoveFast(T[1], S[N], Length(T)); {!!.01}
    end;

  begin
    with BP^ do begin
      S := RawStatus;
      MergeString(JustFilename(CurrentFileName), 3, S);
      if brWorkingFlag <> 0 then
        MergeString('Working...', 36, S);
      MergeString(Long2Str(brCurLine), 64, S);
      MergeString(Long2Str(brColOfs+1), 76, S);

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      FastWrite(S, StatusRow, 1, ColorMono(StatusColor, StatusMono));

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;
  end;

begin
  {initialize command processor}
  BrowseCommands.Init(@BrowseKeySet, BrowseKeyMax);
end.

