{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                    OPHELP.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpHelp;
  {-General purpose help facility}

{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

interface

uses
  Use32,
  Dos,
  OpConst,             {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpDos,
  OpCrt,
  OpCmd,
  OpFrame,
  OpWindow,
  {$IFDEF UseDrag}     {!!.03}
  OpDrag,              {!!.03}
  {$ENDIF}             {!!.03}
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpPick;

  {$I OPHELP.ICD}  {configuration data}

  {.F-}
const
  {---- Option codes for help windows ----}
  hwStaticNameBuffer = $0001; {Preallocate and keep name buffers at all times}
  hwPickIndex        = $0002; {Enable help index command}
  hwClearStackAtExit = $0004; {Clear the previous topic stack when exiting help}
  hwMousePage        = $0008; {Clicking on scroll bar scrolls by page}
  hwAdjustScroll     = $0010; {Adjust horizontal scroll limits automatically}
  hwBlockMark        = $0020; {Enable block marking}
  hwHighlightXref    = $0040; {Enable Xref highlighting} {!!.13}

  {---- Internal flags ----}
  hwChangedXref      = $0080; {Changed Xref with mouse} {!!.03}
  hwPickListInit     = $0100; {PickList initialized}
  hwModeChangePending= $0200; {Window mode change pending}
  hwProcessingHelp   = $0400; {Already processing help commands, don't reenter}
  hwHelpFrameInit    = $0800; {Help frame initialized}
  hwHelpModeActive   = $1000; {Help mode active, not pick mode}
  hwHelpFileOpen     = $2000; {Help file is open}
  hwHelpInMem        = $4000; {Help is bound into code}
  hwNamesLoaded      = $8000; {Name and index buffers are loaded in RAM}

  {---- Default options ----}
  DefHelpOptions : Word = hwPickIndex+hwAdjustScroll;
  BadHelpOptions : Word = hwChangedXref+ {!!.03}
                          hwPickListInit+hwModeChangePending+hwProcessingHelp+
                          hwHelpFrameInit+hwHelpModeActive+hwHelpFileOpen+
                          hwHelpInMem+hwNamesLoaded;

  HelpFileOpenMode : Byte = $20; {FileMode: read only, deny write}

  {---- Capacity of help system ----}
  MaxPagesPerSection  = 20;   {Maximum number of help pages per section}
  MaxXrefsPerSection  = 50;   {Maximum number of topic xrefs per section}
  MaxPreviousTopics   = 20;   {Maximum previous topics stacked}
  FlexStackSize       =  4;   {Maximum number of nested attributes}

  {---- Special characters used in the help file ----}
  Attr1Toggle   = ^A;         {Character toggles special attribute 1}
  Attr2Toggle   = ^B;         {Character toggles special attribute 2}
  Attr3Toggle   = ^C;         {Character toggles special attribute 3}
  IndexMarker   = ^D;         {Character marks topic number that follows}
  XrefToggle    = ^E;         {Character toggles xref highlight}
  LiteralMarker = ^F;         {Character marks literal ASCII char following}
  LineBrkMark   = ^M;         {Denotes end of line of help}
  PageBrkMark   = ^L;         {Denotes end of page of help}
  SectEndMark   = #0;         {Denotes end of help section}

  {Synonym definitions}
  HelpId : array[0..3] of Char = 'OPH1'; {Identifier at start of help file}
  NoHelpAvailable = $FFFFFFFF;{No help is available for topic}
  NoTopicName     = $FFFF;    {No topic name for this topic}
  XrefForIndex    = 0;        {Special xref denotes help index}
  PickTopic       = 0;        {Special topic number brings up pick list}

  {Extra commands that exit from help index pick list}
  HelpExitCommands : ByteSet = [ccPrevTopic];

  {Characters that comprise a word for ScreenWordXY}
  WordChars : string[127] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789';

type
  XlateArray = array[0..15] of Byte; {Most common characters in help text}

  HelpHeaderPtr = ^HelpHeader;
  HelpHeader =                   {At start of help file}
    record
      ID           : LongInt;    {Marks help file version}
      HighestTopic : Word;  {Highest topic number}
      BiggestTopic : Word;  {Size of largest topic in uncompressed bytes}
      PickTopics   : Word;  {Number of topics in pick index}
      SizeOfNames  : Word;  {Bytes in all topic names (packed table)}
      LongestName  : Byte;       {Longest topic name}
      LongestPick  : Byte;       {Longest name in pick index}
      WindowWidth  : Byte;       {Preferred width of help window}
      Scrolling    : Boolean;    {True if scrolling display is preferred}
      MaxLineLen   : SmallInt;   {Maximum line length}
      XlateTable   : XlateArray; {Table for decompression}
    end;

  BytePtr      = ^Byte;
  CharArray    = array[0..65520] of Char;
  CharArrayPtr = ^CharArray;
  WordArray    = array[1..(65521 div SizeOf(Word))] of Word;
  WordArrayPtr = ^WordArray;
  FilePtr      = ^File;

  HelpNameRec  =             {Entry for one topic name}
    record
      hnOfs    : Word;  {Name buffer offset. Keep first!}
      {for future expansion}
    end;
  NameIndex    = array[1..(65521 div SizeOf(HelpNameRec))] of HelpNameRec;
  NameIndexPtr = ^NameIndex;

  HelpIndexRec =             {Index entry for help file positions}
    record
      Start   : LongInt;     {File position of topic}
      CompLen : Word;        {Compressed length of topic}
    end;
  HelpIndex    = array[1..(65521 div SizeOf(HelpIndexRec))] of HelpIndexRec;
  HelpIndexPtr = ^HelpIndex;

  {Components of a help file}
  HelpFileSection = (hwHeader,     {Fixed size header}
                     hwNameIndex,  {Offset index for packed topic names}
                     hwNames,      {Packed topic names}
                     hwTopicIndex, {Sorted topic index for pick list}
                     hwFileIndex); {Index to help topics themselves}

  AbstractHelpWindowPtr= ^AbstractHelpWindow;

  hwMatchFunc     = function (VaryingName, FixedName : String;
                              HPtr : AbstractHelpWindowPtr) : Boolean;
  hwNestedFunc    = function (HPtr : AbstractHelpWindowPtr) : Boolean;

  AbstractHelpWindow =              {Abstract version of a help window}
    object(PickList)
      hwHdrP       : HelpHeaderPtr; {Help header pointer}
      hwOfsP       : NameIndexPtr;  {Name offset and flags for each topic}
      hwNamP       : CharArrayPtr;  {Topic name buffer pointer}
      hwTopP       : WordArrayPtr;  {Topic index buffer pointer}
      hwIndP       : HelpIndexPtr;  {File index pointer, nil for help on disk}
      hwBufP       : CharArrayPtr;  {Decompression buffer base pointer}
      hwCurTopic   : Word;          {Current topic loaded into buffers}
      hwPrvTopic   : Word;          {Previous topic}
      hwPckTopic   : Word;          {Topic picked in pick window}
      hwCurLen     : Word;          {Uncompressed length of current topic} {!!.13}

      hwFlags      : Word;          {Option and status flags}
      hwPickPad    : Byte;          {Characters to pad left of pick items}
      hwHelpPad    : Byte;          {Characters to pad help left and right}

      {... Specialized headers on help window}
      hwTopicHdr   : Byte;          {Header number for topic header}
      hwHdrPadSize : Byte;          {Spaces around topic header}
      hwMaxHdrLen  : Byte;          {Longest header length}
      hwPrevHotSpot: Byte;          {Hot spot number for previous topic}
      hwPrevSpotPos: FrameCharType; {Hot spot position for previous topic}
      hwMoreRec    : MoreRec;       {Describes "more" header}

      {... Items required because of dual nature of help/pick window}
      hwFrame      : Frame;         {Frame used when help mode is active}
      hwCursor     : CursorType;    {Cursor shape}
      hwCursorX    : Byte;          {Absolute X position of cursor}
      hwCursorY    : Byte;          {Absolute Y position of cursor}

      hwFileP      : FilePtr;       {Help file pointer, nil for help in memory}
      hwStack      : StaticQueue;   {Topic stack}
      hwUserNested : hwNestedFunc;  {User-supplied nested xref function}

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       HelpFile : PathStr;
                       PickOrientation : pkGenlProc);
        {-Initialize help window and open help file}
      constructor InitMem(X1, Y1, X2, Y2 : Byte;
                          HelpAddr : Pointer;
                          PickOrientation : pkGenlProc);
        {-Initialize help window using help bound into code}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HelpFile : PathStr;
                             PickOrientation : pkGenlProc);
        {-Initialize help window and open help file}
      constructor InitDeluxe(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HelpFile : PathStr;
                             PickOrientation : pkGenlProc;
                             HelpOptions : Word);
        {-Initialize help window and open help file}
      constructor InitMemCustom(X1, Y1, X2, Y2 : Byte;
                                var Colors : ColorSet;
                                Options : LongInt;
                                HelpAddr : Pointer;
                                PickOrientation : pkGenlProc);
        {-Initialize help window using help bound into code}
      constructor InitMemDeluxe(X1, Y1, X2, Y2 : Byte;
                                var Colors : ColorSet;
                                Options : LongInt;
                                HelpAddr : Pointer;
                                PickOrientation : pkGenlProc;
                                HelpOptions : Word);
        {-Initialize help window using help bound into code}

      destructor Done; virtual;
        {-Close up and dispose of help window}
      procedure ItemString(Item : Word; Mode : pkMode; var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
      procedure PopAndDraw; {!!.13}
        {-Pop last set topic and draw it}
      procedure SetAndPrepTopic(Topic : Word);  {!!.20}
        {-Push and pop given topic, initializing all data structures}
      procedure Draw; virtual;
        {-Choose appropriate frame and draw help window or pick index}
      procedure UpdateContents; virtual;
        {-Redraw the help window in pick or help mode}
      procedure ProcessSelf; virtual; {!!.01}
        {-Draw help index or current topic}

      procedure SetTopic(Topic : Word);
        {-Specify which topic is displayed next. 0 for index}
      function GetTopicChoice : Word;
        {-Return topic chosen by pick}
      function InHelpMode : Boolean;
        {-Return True if window shows help, not pick list}
      function TopicStackPtr : StaticQueuePtr;
        {-Return a pointer to the topic stack}
      function CurrentTopic : Word;
        {-Return the topic number currently loaded into the help buffers}
      function TopicExists(Topic : Word) : Boolean; {!!.02}
        {-Return True if specified topic exists in help file}

      procedure hwOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure hwOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function hwOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}

      procedure SetPickPadSize(PadSize : Byte);
        {-Set number of characters for padding of pick items}
      procedure SetHelpPadSize(Pad : Byte);
        {-Set number of blank characters at edges of help window}
      procedure AddTopicHeader(PadSize : Byte; MaxLen : Byte;
                               Posn : HeaderPosType);
        {-Add a header to display the topic name}
      procedure AddMoreHelpHeader(PromptStr : String80;
                                  Posn : HeaderPosType;
                                  UpStr, DnStr, SepStr : String10;
                                  UpPos, DnPos, SepPos : Byte);
        {-Add a specialized more header to indicate more help}
      procedure InitHelpFrame;
        {-Make help frame look like wFrame}
      {$IFDEF UseHotSpots}
      procedure SetPrevTopicHotSpot(SpotNum : Byte; Posn : FrameCharType);
        {-Activate a hot spot for previous topic command}
      {$ENDIF}

      procedure SetNestedFunc(NF : hwNestedFunc);
        {-Set function to call for nested xref commands}

      function HighestTopic : Word;
        {-Return highest topic number in help system}
      function PickTopics : Word;
        {-Return number of help index topics in help system}
      function FindNextTopic(PrevMatch : Word; Circular : Boolean;
                             var Name : String;
                             MatchFunc : hwMatchFunc) : Word; virtual;
        {-Return Topic number with specified Name, starting past PrevMatch,
          searching circularly if Circular is True}
      function FindTopic(Name : String;
                         MatchFunc : hwMatchFunc) : Word; virtual;
        {-Return Topic number of help with specified Name, 0 if not found}
      function ScreenWordXY(XPos, YPos : Byte;
                            ScanBack, ScanForward : Boolean) : String;
        {-Parse word from screen at absolute (XPos, YPos)}
      function CurrentXrefTopic : Word; virtual;
        {-Return help topic of current cross reference, $FFFF if none}

      function SearchTopic(Topic, StartOfs : Word; TxtSt : String) : Word; {!!.13}
        {-Start searching specific topic at StartOfs for TxtSt. Returns
          match offset, or $FFFF if no match.}
      procedure SetTopicAndPos(Topic, TOfs : Word; ShowLen : Byte); virtual; {!!.13}
        {-Specify next topic to display. Page or cursor will display
          specified text offset}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a help system from a stream}
      procedure Store(var S : IdStream);
        {-Store a help system in a stream}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      {$IFDEF UseAdjustableWindows}
      procedure rwResizeAdjustCursor(oXL, oYL, oXH, oYH : Byte); virtual;
      procedure rwResizeFrames(FX1, FY1, FX2, FY2 : Byte); virtual;
      procedure rwUpdateWindowLimits(DX, DY : Integer); virtual; {!!.01}
      procedure SetPosLimits(MinX, MinY, MaxX, MaxY : Byte); {!!.12}
      procedure SetFrameLimits(MinW, MinH, MaxW, MaxH : Byte); {!!.12}
      {$ENDIF}
      procedure hwRawInit;
      procedure hwInitPart2;
      procedure hwSetHelpAttrs(var Colors : ColorSet);
      procedure hwSetWidth(Options : LongInt; X1 : Byte; var X2 : Byte);
      function hwOpenHelp(var HelpFile : PathStr) : Word;
      function hwCheckHeader : Word;
      procedure hwHelpFileInfo(HFS : HelpFileSection;
                               var SeekOfs : LongInt; var Length : Word);
      function hwLoadSection(HFS : HelpFileSection; var hwP) : Word;
      function hwLoadTopicNames : Word;
      procedure hwFreeTopicNames;
      function hwRealTopicName(TopicOfs : Word) : String;
      function hwGetTopicName(Topic : Word; var TopicName : String) : Word;
      function hwGetHeaderString(var TopicName : String) : Word;
      function hwMaxPickLength : Byte;
      procedure hwDecompress(Len : Word; SP, DP : BytePtr);
      function hwLoadTopic : Word;
      function hwSetAttr(AttrChar : Char) : Byte;
      procedure hwSetHelpMode(HelpMode : Boolean);
      function hwHelpIndex : Boolean;
      procedure hwChangeHeader(HdrNum : Byte; HdrStr : String);
      procedure hwPickHelpTopic;
      {$IFDEF UseScrollBars}
      procedure hwSetupForScrollBars; virtual;
      procedure hwUpdateScrollBars; virtual;
      {$ENDIF}
      function hwFindTextOfs : Word; virtual; {!!.13}
      procedure hwNewTopic(Topic : Word); virtual;
      procedure hwPushNewTopic(Topic : Word); virtual;
      procedure hwPushCurTopic; virtual;
      procedure hwPopCurTopic; virtual;
      procedure hwUpdateHelp; virtual;
      procedure hwForceRedraw; virtual; {!!.13}
      function hwShowCurTopic : Boolean; virtual; {!!.13}
      procedure hwShowTopTopic; virtual;
      {.Z-}
    end;

  Xrefs = 0..MaxXrefsPerSection;  {Valid cross-reference index}

  FlexStack = array[0..FlexStackSize] of Char; {Stacks current attribute codes}

  {------------------- Paged help ---------------------}

type
  Pages = 0..MaxPagesPerSection;  {Valid help page index}

  phStackRec =               {Info needed to display previous topics}
    record
      STopic : Word;         {Which topic}
      SPage  : Pages;        {Page in topic}
      SXnum  : Xrefs;        {Xref item selected}
    end;

  phPageRec =                {Display state at the start of a given page}
    record
      TextOfs : Word;        {Offset into decompressed text}
      FlexSP  : Byte;        {Flex stack pointer}
      FlexStk : FlexStack;   {Stack of active attribute indexes}
    end;
  phPageArray = array[1..MaxPagesPerSection] of phPageRec;

  phXrefRec =                  {Describes a given cross-reference}
    record
      XrefPage  : Pages;       {Which page the xref displays on}
      XrefRow   : Byte;        {Which row of window}
      XrefCol   : Byte;        {Which col of window}
      XrefOfs   : Word;        {Offset in uncompressed text buffer}
      XrefTopic : Word;        {Which topic is cross-referenced}
    end;
  phXrefArray = array[1..MaxXrefsPerSection] of phXrefRec;

  PagedHelpWindowPtr = ^PagedHelpWindow;
  PagedHelpWindow =            {Paged help window object}
    object(AbstractHelpWindow)
      phCurPage : Pages;       {Current page number}
      phPrvPage : Pages;       {Previously selected page number}
      phPageCnt : Pages;       {Number of pages in current topic}
      phCurXref : Xrefs;       {Current xref number}
      phXrefCnt : Xrefs;       {Number of xrefs in topic}
      phInitHgt : Byte;        {Window height when pagination performed}
      phPageInx : phPageArray; {Information about the pages}
      phXrefInx : phXrefArray; {Information about the xrefs}
      {.Z+}
      constructor Init(X1, Y1, X2, Y2 : Byte;
                       HelpFile : PathStr;
                       PickOrientation : pkGenlProc);
        {-Initialize help window and open help file}
      constructor InitMem(X1, Y1, X2, Y2 : Byte;
                          HelpAddr : Pointer;
                          PickOrientation : pkGenlProc);
        {-Initialize help window using help bound into code}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HelpFile : PathStr;
                             PickOrientation : pkGenlProc);
        {-Initialize help window and open help file}
      constructor InitDeluxe(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HelpFile : PathStr;
                             PickOrientation : pkGenlProc;
                             HelpOptions : Word);
        {-Initialize help window and open help file}
      constructor InitMemCustom(X1, Y1, X2, Y2 : Byte;
                                var Colors : ColorSet;
                                Options : LongInt;
                                HelpAddr : Pointer;
                                PickOrientation : pkGenlProc);
        {-Initialize help window using help bound into code}
      constructor InitMemDeluxe(X1, Y1, X2, Y2 : Byte;
                                var Colors : ColorSet;
                                Options : LongInt;
                                HelpAddr : Pointer;
                                PickOrientation : pkGenlProc;
                                HelpOptions : Word);
        {-Initialize help window using help bound into code}

      procedure SetTopicAndPos(Topic, TOfs : Word; ShowLen : Byte); virtual; {!!.13}
        {-Specify next topic to display. Page or cursor will display
          specified text offset}

      function CurrentXrefTopic : Word; virtual;
        {-Return help topic of current cross reference, $FFFF if none}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a help system from a stream}
      procedure Store(var S : IdStream);
        {-Store a help system in a stream}
    {$ENDIF}
      {.Z-}

      {++++ for internal use ++++}
      {.Z+}
      procedure phNewPage(var Finished : Boolean;
                          var PrvPageRec, CurPageRec : phPageRec);
      procedure phInitTopic;
      function phNextPageOfs : Word;
      procedure phDrawXref(Xref : Xrefs);
      function phFirstXref : Xrefs;
      {$IFDEF UseMouse}
      function phMatchXref(Xref : Xrefs; MX, MY : Byte) : Boolean;
      function phFindXref(MX, MY : Byte) : Xrefs;
      function phEvaluateMouseCmd : Boolean;
      {$ENDIF}
      procedure phIncXrefPrim(Delta : Integer);
      procedure phIncHorizXref(Delta : Integer);
      procedure phIncVertXref(Delta : Integer);
      function phSelectXref : Boolean;
      function phEvaluateCmd : Boolean;

      {$IFDEF UseScrollBars}
      procedure hwSetupForScrollBars; virtual;
      procedure hwUpdateScrollBars; virtual;
      {$ENDIF}
      procedure hwNewTopic(Topic : Word); virtual;
      procedure hwPushNewTopic(Topic : Word); virtual;
      procedure hwPushCurTopic; virtual;
      procedure hwPopCurTopic; virtual;
      procedure hwUpdateHelp; virtual;
      procedure hwForceRedraw; virtual; {!!.13}
      function hwShowCurTopic : Boolean; virtual; {!!.13}
      procedure hwShowTopTopic; virtual;
      {.Z-}
    end;

  {------------------- Scrolled help ---------------------}

  shLineRec =                  {Info describing the start of a line}
    record
      TextOfs : Word;          {Offset into decompressed text}
      FlexSP  : Byte;          {Flex stack pointer}
      FlexStk : FlexStack;     {Stack of active attribute indexes}
    end;

  shStackRec =                 {Info needed to display previous topics}
    record
      STopic : Word;           {Which topic}
      First  : shLineRec;      {State of first line}
      Last   : shLineRec;      {State of last line}
      HorOfs : Word;           {Horizontal screen offset}
      CursX  : Byte;           {Cursor X position}
      CursY  : Byte;           {Cursor Y position}
    end;

  ScrollingHelpWindowPtr = ^ScrollingHelpWindow;

  shSendFunc = function (T : CharArrayPtr; Len : Word;
                         SH : ScrollingHelpWindowPtr) : Boolean;

  ScrollingHelpWindow =        {Scrolled help window object}
    object(AbstractHelpWindow)
      shFirst   : shLineRec;   {Describes topmost visible line}
      shLast    : shLineRec;   {Describes lowest visible line}
      shCurCol  : Integer;     {Current horizontal scroll}
      shPrvCol  : Integer;     {Previous horizontal scroll}
      shMaxCol  : Integer;     {Maximum allowed horizontal scroll}
      shVertScr : Integer;     {Non-zero when fast vertical scroll required}
      shInitBotOfs : Word;     {Text offset of initial bottom line in window}
      shMaxBotOfs  : Word;     {Text offset of lowest help line}
      shInitHgt : Word;        {Initial window height} {!!.03}
      shPrvX : Byte;           {Previous x position}                 {!!.13}
      shPrvY : Byte;           {Previous y position}                 {!!.13}
      shPrvXRX : Byte;         {Previous xref's x starting position} {!!.13}
      shPrvXRY : Byte;         {Previous xref's y starting position} {!!.13}
      shPrvXRO : Word;         {Previous xref's buffer offset}       {!!.13}
      shPrvXRC : Word;         {Previous xref's line column}         {!!.13}
      shPrvXRT : Word;         {Previous xref's topic}               {!!.13}

      {... Cut/paste ...}
      shBlockColor : Byte;          {Block color attribute}
      shBlockMono  : Byte;          {Block mono attribute}
      shBlockBegin : Word;          {Start of marked block}
      shBlockEnd   : Word;          {End of marked block}
      shBlockOn    : Boolean;       {Toggles block on/off}
      shSend       : shSendFunc;    {Procedure pointer called by shCopyBlock}
      {.Z+}
      constructor Init(X1, Y1, X2, Y2 : Byte;
                       HelpFile : PathStr;
                       PickOrientation : pkGenlProc);
        {-Initialize help window and open help file}
      constructor InitMem(X1, Y1, X2, Y2 : Byte;
                          HelpAddr : Pointer;
                          PickOrientation : pkGenlProc);
        {-Initialize help window using help bound into code}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HelpFile : PathStr;
                             PickOrientation : pkGenlProc);
        {-Initialize help window and open help file}
      constructor InitDeluxe(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HelpFile : PathStr;
                             PickOrientation : pkGenlProc;
                             HelpOptions : Word);
        {-Initialize help window and open help file}
      constructor InitMemCustom(X1, Y1, X2, Y2 : Byte;
                                var Colors : ColorSet;
                                Options : LongInt;
                                HelpAddr : Pointer;
                                PickOrientation : pkGenlProc);
        {-Initialize help window using help bound into code}
      constructor InitMemDeluxe(X1, Y1, X2, Y2 : Byte;
                                var Colors : ColorSet;
                                Options : LongInt;
                                HelpAddr : Pointer;
                                PickOrientation : pkGenlProc;
                                HelpOptions : Word);
        {-Initialize help window using help bound into code}
      {.Z-}
      procedure SetMaxScroll(MaxFirstCol : Word);
        {-Set a new value for maximum horizontal scrolling}
      procedure SetSendFunc(SF : shSendFunc);
        {-Set function to be called by ccBlkCopy command}
      procedure SetBlockAttr(Color, Mono : Byte);
        {-Set the attribute for blocks}
      procedure ResetBlock;
        {-Hide and clear the current block. No screen update}
      procedure HideBlock;
        {-Hide the current block. No screen update}

      {.Z+}
      procedure SetTopicAndPos(Topic, TOfs : Word; ShowLen : Byte); virtual; {!!.13}
        {-Specify next topic to display. Page or cursor will display
          specified text offset}
      procedure SetHelpPadSize(Pad : Byte);
        {-Set number of blank characters at edges of help window}
      function CurrentXrefTopic : Word; virtual;
        {-Return help topic of current cross reference, $FFFF if none}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a help system from a stream}
      procedure Store(var S : IdStream);
        {-Store a help system in a stream}
    {$ENDIF}
      {.Z-}

      {++++ for internal use ++++}
      {.Z+}
      function shFinalInit : Word;
      procedure shSetMaxScroll;
      procedure shInitFirst(var Line : shLineRec);
      procedure shNextLine(var Line, NewLine : shLineRec);
      procedure shPrevLine(var Line, NewLine : shLineRec);
      procedure shFindLastLine;
      procedure shDrawLine(Row : Byte; var Line : shLineRec);
      function shFindXrefTopic(MX, MY : Byte) : Word;
      {$IFDEF UseMouse}
      procedure shPositionCursor(X, Y : Byte);
      function shEvaluateMouseCmd : Boolean;
      {$ENDIF}
      function shLineLen(Line : shLineRec) : Word;
      procedure shFindCurLine(var Line : shLineRec);
      procedure shFindCurPos(var Line : shLineRec; var LineCol : Word);
      procedure shHorizScroll(LinePos : Integer);
      function shSelectXref : Boolean;
      function shSeparator(Ch : Char) : Boolean;
      procedure shDownLine;
      procedure shUpLine;
      procedure shScrollDown;
      procedure shScrollUp;
      procedure shLeft;
      procedure shRight;
      procedure shPageUp;
      procedure shPageDn;
      procedure shScreenTop;
      procedure shScreenBot;
      procedure shStartOfLine;
      procedure shEndOfLine;
      procedure shTopOfFile;
      procedure shEndOfFile;
      procedure shWordLeft;
      procedure shWordRight;
      procedure shXrefLeft;
      procedure shXrefRight;
      function shBlockPosAtCursor : Word;
      procedure shMarkBlockBegin;
      procedure shMarkBlockEnd;
      procedure shBlockToggle;
      function  shBlockCopy : Boolean;
      function shEvaluateCmd : Boolean;
      procedure shInitMaxOfs;
      procedure shUpdateHelpAsNeeded;
      procedure shDrawHighlight(Attr, X, Y : Byte; O, LC : Word); {!!.13}
      procedure shUpdateHighlight; {!!.13}
      {$IFDEF UseScrollBars}
      procedure hwSetupForScrollBars; virtual;
      procedure hwUpdateScrollBars; virtual;
      {$ENDIF}
      function hwFindTextOfs : Word; virtual; {!!.13}
      procedure hwNewTopic(Topic : Word); virtual;
      procedure hwPushNewTopic(Topic : Word); virtual;
      procedure hwPushCurTopic; virtual;
      procedure hwPopCurTopic; virtual;
      procedure hwUpdateHelp; virtual;
      procedure hwForceRedraw; virtual; {!!.13}
      function hwShowCurTopic : Boolean; virtual; {!!.13}
      procedure hwShowTopTopic; virtual;
      {.Z-}
    end;

var
  {$IFDEF UseDrag}                   {!!.03}
  HelpCommands : DragProcessor;      {!!.03}
  {$ELSE}                            {!!.03}
  HelpCommands : CommandProcessor;
  {$ENDIF}                           {!!.03}

  {.F+}

  {----------- Detect scrolling vs. paged help files -------------}

  function ScrollingHelpFile(HelpFile : PathStr) : Boolean;
    {-Return True if HelpFile is designed for a scrolling help window}

  {------------ Stream registration --------------}

{$IFDEF UseStreams}

  procedure PagedHelpWindowStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing paged help windows}

  procedure ScrollingHelpWindowStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing scrolling help windows}

{$ENDIF}

  {------------ Default hook routines -------------}

  function DefaultTopicMatch(VaryingName, FixedName : String;
                             HPtr : AbstractHelpWindowPtr) : Boolean;
    {-Default match function}

  function NoNestedAction(HPtr : AbstractHelpWindowPtr) : Boolean;
    {-Defaulted nested xref function that does nothing}

  {=================================================================}

implementation

  {$I OPHELP.IN1}           {Raw help window}
  {$I OPHELP.IN2}           {Paged help window}
  {$I OPHELP.IN3}           {Scrolled help window}

begin
  {initialize command processor}
  HelpCommands.Init(@HelpKeySet, HelpKeyMax);
end.
