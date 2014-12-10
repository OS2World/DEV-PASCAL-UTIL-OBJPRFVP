{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$R-,S-,I-,V-,B-,F+,O+,A-} {!!.01}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                    OPPICK.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1987,1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{Special thanks to Dan T. Davis for his numerous contributions to this unit}

unit OpPick;
  {-Pick items from lists}


interface

uses
  Use32,
  OpConst,          {!!.20}
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
  {$IFDEF UseDrag}  {!!.03}
  , OpDrag          {!!.03}
  {$ENDIF}          {!!.03}
  ;

  {$I OPPICK.ICD}  {configuration data}

  {.F-}
const
  {---- Option codes ----}
  pkStick           = $0001;      {Stick at edges of scrolling lists}
  pkExitAtEdges     = $0002;      {Exit picklist for cursor at extreme edges}
  pkAlterPageRow    = $0004;      {Alter screen row when paging}
  pkMousePage       = $0008;      {Clicking on scroll bar scrolls by page}
  pkDrawActive      = $0010;      {Draw and Pick leave selected item visible
                                   and frame status headers showing}
  pkMinHeight       = $0020;      {Minimize height depending on number of items}
  pkSetDefault      = $0040;      {Store the initial choice when entering Pick}
  pkOptFullExtent   = $0080;      {When optimizing size, account for full
                                   extent of window, including shadows}
  pkUseItemForTopic = $0100;      {Use the current item number for help topic,
                                   else use the code in the command processor}
  pkSelectOnClick   = $0800;      {Implicit Enter on first mouse click} {!!.03}
  pkProcessZero     = $1000;      {Allow Process of zero items}         {!!.03}
  pkNoHighlightPad  = $4000;      {Don't pad item highlights}           {!!.01}
  pkAltCurrent      = $8000;      {Draw current item in alt attribute}  {!!.03}

  {... for internal use ...}
  pkMultiChoice     = $0200;      {Set for multiple choice picklist}
  pkFlexWrite       = $0400;      {Set when flexwriting is active}
  pkBitSetAlloc     = $2000;      {Set when bit set allocated}

  DefPickOptions    : Word = pkStick+pkAlterPageRow+pkDrawActive+
                             pkMinHeight+pkSetDefault+pkOptFullExtent;
  BadPickOptions    : Word = pkBitSetAlloc+pkFlexWrite+pkMultiChoice;{!!.01}

  {---- Secondary (internal) option flags ----}                      {!!.01}
  {These flags are not stored in streams}                            {!!.01}
  pkItemStrActive   = $0001;      {CompleteItemString method active} {!!.01}
  pkPickActive      = $0002;      {Set when Pick method active}      {!!.01}
  pkRedrawPage      = $0004;      {Set when page must be redrawn}    {!!.01}
  pkRedrawOne       = $0008;      {Set when one item must be redrawn}{!!.01}
  pkItemWrap        = $0010;      {Wrap from last to first item}     {!!.01}
  pkChangedItems    = $0020;      {Set when mouse moves to new item} {!!.03}
  pkFakingOneItem   = $0040;      {Set when Process has 0 items}     {!!.03}
  pkSelStrings      = $0080;      {Set when select strings not empty}{!!.03}
  pkPadStrings      = $0100;      {Set when pad strings not empty}   {!!.03}
  pkEvaluating      = $0200;      {Set when testing a command}       {!!.13}

  DefSecPickOptions : Word = 0;   {Default secondary options}        {!!.01}

  pkMaxSearchLen    = 15;         {Maximum length of search string}

  {---- Pick item categories ----}
  pkNormal          = 0;          {Normal items}
  pkAlternate       = 1;          {Alternate items}
  pkProtected       = 2;          {Protected items}
  pkPrivate         = 3;          {Reserved for TurboPower - NO LONGER USED}
  pkSemiProtected   = 3;          {Semi-protected items}             {!!.03}

  {---- Orientations ----}
  pkNoOrient        = 0;
  pkVertical        = 1;
  pkHorizontal      = 2;
  pkSnaking         = 3;

type
  String10     = String[10];
  String80     = String[80];

  PickListPtr      = ^PickList;   {Pointer to a PickList}

  pkItemType       = pkNormal..pkSemiProtected; {!!.03}
  {....                      0..3      Selected      0..3}
  pkBaseColorArray = array[pkItemType, Boolean] of FlexAttrs;
  {....                   mono/color ------> all the colors = 64 bytes}
  pkColorArray     = array[Boolean] of pkBaseColorArray;

  pkSearchString   = String[pkMaxSearchLen]; {Search string}

  {Modes of action for user string function}
  pkMode           = (pkDisplay, pkSearch, pkGetType);

  pkStringProc     = procedure (Item : Word;
                                Mode : pkMode;
                                var IType : pkItemType;
                                var IString : String;
                                PickPtr : PickListPtr);

  pkMoveType       = (LeftTop,   UpLeft,   UpMiddle,   UpRight, RightTop,
                   LeftMiddle,                                  RightMiddle,
                   LeftBottom, DownLeft, DownMiddle, DownRight, RightBottom,
                         PgUp,
                         PgDn);
  pkGenlProc       = procedure (P : PickListPtr);
  pkMoveArray      = array[pkMoveType] of pkGenlProc;
  pkSearchFunc     = function (P : PickListPtr) : Boolean;
  pkGetCurrentFunc = function (First, Row, Col : Word; P : PickListPtr) : Word;
  pkSetCurrentProc = procedure (Choice, First : Word; P : PickListPtr);
  pkSetScrollProc  = procedure (FramePos : FramePosType; MPosX, MPosY : Byte;
                                UserVal : LongInt; P : PickListPtr);
  pkScrolledFunc   = function (pChoice, pFirst : Word; pRow, pCol : Byte;
                               P : PickListPtr) : Boolean;
  pkCommandFunc    = function (var Cmd : Word; P : PickListPtr) : Boolean;

  MoreRec          =
    record
                                  {First field removed} {!!.01}
      HdrNum       : Byte;        {Header number to modify}
      UpP          : Byte;        {Position in string for Up}
      DnP          : Byte;        {Position in string for Down}
      SepP         : Byte;        {Position in string for Separator}
      FillCh       : Char;        {Character used to fill in missing Up/Down}
      UpStrP       : StringPtr;   {String for more Up}
      DnStrP       : StringPtr;   {String for more Down}
      SepStrP      : StringPtr;   {String for separator when both Up/Down show}
      MoreStrP     : StringPtr;   {String to display for More}
    end;

  PickList         =
    object(CommandWindow)
      {... Following are modified when pick list is initialized}
      pkXL         : Byte;        {Initially requested size for window}
      pkYL         : Byte;
      pkXH         : Byte;
      pkYH         : Byte;
      pkFlags      : Word;        {Bit-mapped flags}
      pkReqdWidth  : Byte;        {Requested columns per item in pick window}
      pkItemWidth  : Byte;        {Actual columns per item in pick window}
      pkLeftPad    : Byte;        {Number of chars to left-pad names}
      pkRightPad   : Byte;        {Number of chars to right-pad names}
      pkOrient     : Byte;        {Orientation type in use}
      pkCols       : Byte;        {Maximum visible columns of items}
      pkWidth      : Byte;        {Active columns in pick window}
      pkHeight     : Byte;        {Active rows in pick window}
      pkItems      : Word;        {Number of items}
      pkMinRows    : Word;        {Minimum value for pkItemRows}
      pkMaxRows    : Word;        {Maximum value for pkHeight}
      pkMaxFirst   : Word;        {Largest allowed value for pkFirst}
      pkItemRows   : Word;        {Total number of items in one column}
      pkScroll     : Word;        {Number of items to skip when scrolling}
      pkColorPtr   : ^pkBaseColorArray; {Pointer to active attrs: color or mono}
      pkColors     : pkColorArray; {All the colors, so many colors}

      {... Following are modified when browsing around the pick list}
      pkInitChoice : Word;        {Initial item at start of pick}
      pkChoice     : Word;        {Current item}
      pkFirst      : Word;        {Choice in upper left corner of window}
      pkRow        : Word;        {Window-relative row of current item}
      pkCol        : Word;        {Window-relative logical column
                                   of current item}

      {... Following have to do with searching}
      pkSearchStr  : pkSearchString;{Current search string}
      pkSearchStart: Byte;        {Offset in string to search from}
      pkSearchHdr  : Byte;        {Header number for search status}
      pkSearchLen  : Byte;        {Maximum length of search status string}

      {... Following describes the "more" header}
      pkMoreRec    : MoreRec;     {Describes "more" header}

      {... Following procedure variables are initialized based on orientation}
      pkOrientInit : pkGenlProc;       {Called to initialize orientation}
      pkPrimMoves  : pkMoveArray;      {Primitive move routines}
      pkGetCurrent : pkGetCurrentFunc; {Get the current choice
                                        based on position}
      pkSetCurrent : pkSetCurrentProc; {Set the current position
                                        based on choice}
      pkReinit     : pkGenlProc;  {Reinitialize orientation-specific parameters}
      {$IFDEF UseScrollBars}
      pkUpdScrBar  : pkGenlProc;      {Update scroll bars: orientation-specific}
      pkSetScroll  : pkSetScrollProc; {Set position based on scroll bar pos}
      {$ENDIF}
      pkScrolled   : pkScrolledFunc;  {Determine whether a special case scroll
                                       is OK}
      pkCommandInit: pkGenlProc;      {Routine to initialize single vs. multi}
      pkCommand    : pkCommandFunc;   {Command handler to use: single vs.
                                       multi-choice}

      {... Following user-defined procedures are also available
           via virtual methods}
      pkString     : pkStringProc;{User-supplied string procedure}
      pkMove       : pkGenlProc;  {User-supplied move procedure}
      pkSearcher   : pkSearchFunc;{Search function}

      {... Following are used only for multi-pick calls}
      pkSelectSet  : BitSet;      {Marks selected files}
      pkLeftSel    : String[5];   {Strings on left and right to mark selection}
      pkRightSel   : String[5];
      pkBlkStart   : Word;        {Start of block to mark}

      {... Following are used for column dividers} {!!.13}
      pkDividers   : Boolean;     {True when dividers used}
      pkTopJoinCh  : Char;        {Join character at top of column}
      pkBarCh      : Char;        {Vertical bar character}
      pkBotJoinCh  : Char;        {Join character at bottom of column}

      {... Miscellaneous data not stored in a stream}
      pkSecFlags   : Word;        {Secondary (internal) flags}

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       ItemWidth : Byte;
                       NumItems : Word;
                       StringProc : pkStringProc;
                       Orientation : pkGenlProc;
                       CommandHandler : pkGenlProc);
        {-Initialize a pick window}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             ItemWidth : Byte;
                             NumItems : Word;
                             StringProc : pkStringProc;
                             Orientation : pkGenlProc;
                             CommandHandler : pkGenlProc);
        {-Initialize a pick window with custom window options}
      constructor InitAbstract(X1, Y1, X2, Y2 : Byte;
                               var Colors : ColorSet;
                               Options : LongInt;
                               ItemWidth : Byte;
                               NumItems : Word;
                               Orientation : pkGenlProc;
                               CommandHandler : pkGenlProc);
         {-Constructor to be called by derived types that override
           the ItemString method}
      constructor InitDeluxe(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             ItemWidth : Byte;
                             NumItems : Word;
                             StringProc : pkStringProc;
                             Orientation : pkGenlProc;
                             CommandHandler : pkGenlProc;
                             PickOptions : Word);
        {-Initialize a pick window with custom window and pick options}
      constructor InitAbstractDeluxe(X1, Y1, X2, Y2 : Byte;
                                     var Colors : ColorSet;
                                     Options : LongInt;
                                     ItemWidth : Byte;
                                     NumItems : Word;
                                     Orientation : pkGenlProc;
                                     CommandHandler : pkGenlProc;
                                     PickOptions : Word);
         {-Constructor to be called by derived types that override the
           ItemString method, with custom pick options}

      destructor Done; virtual;
        {-Dispose of PickList}

      procedure AddMoreHeader(PromptStr : String80;
                              Posn : HeaderPosType;
                              UpStr, DnStr, SepStr : String10;
                              UpPos, DnPos, SepPos : Byte);
        {-Add a specialized more header to indicate more pick items}
      procedure AddSearchHeader(MaxLen : Byte; Posn : HeaderPosType);
        {-Add a specialized search string header}

      procedure UpdateContents; virtual;
        {-Redraw the complete pick window}
      procedure ProcessSelf; virtual; {!!.01}
        {-Process pick commands leading to a choice}

      procedure ItemString(Item : Word; Mode : pkMode; var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
      procedure CompleteItemString(Item : Word; Mode : pkMode;
                                   var IType : pkItemType;
                                   var IString : String); virtual;
        {-Supplies, marks, and pads item string}
      procedure PreMove; virtual;
        {-Called just prior to getting each keyboard command}
      function ItemSearch : Boolean; virtual;
        {-Called to search list each time an alphanumeric character is entered}
      function OKToChangeChoice : Boolean; virtual;  {!!.13}
        {-Called just prior to changing to a new choice in Process}
      procedure PositionCursor(Item : Word; ACol, ARow : Byte); virtual; {!!.13}
        {-Position the hardware cursor for selected item}

      procedure SetInitialChoice(Choice : Word);
        {-Set initial choice without scrolling if possible}
      procedure SetChoice(Choice, FirstChoice : Word);
        {-Set initial choice with scrolling control}
      procedure SetDefaultChoice(Choice : Word);
        {-Set the default choice number returned by GetDefaultChoice}
      function GetDefaultChoice : Word;
        {-Return default choice number}

      function GetLastChoice : Word;
        {-Return last selected item number}
      function GetItemString(Item : Word) : string;
        {-Return specified item string in search format}
      function GetLastChoiceString : string;
        {-Return last choice in search format}
      procedure WhereSelect(var X, Y : Byte);
        {-Return window-relative coordinates of selected item}
      procedure WhereSelectAbs(var X, Y : Byte);
        {-Return absolute coordinates of selected item}
      function GetOrientation : Byte;
        {-Return the orientation code}
      function GetItemCols : Byte;
        {-Return number of visible item columns}
      function GetNumItems : Word;
        {-Return number of pick items}

      procedure pkOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure pkOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function pkOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}

      procedure SetRowLimits(MinRows, MaxRows : Word);
        {-Set values for min and max rows}
      procedure SetPadSize(LeftPad, RightPad : Byte);
        {-Set number of characters for padding of pick items}
      procedure SetMoveProc(MP : pkGenlProc);
        {-Set user move procedure}
      procedure SetSearchMode(SF : pkSearchFunc);
        {-Set a different search mode}
      procedure SetSearchStart(SStart : Byte);
        {-Set start offset for string searching}
      procedure ChangeNumItems(NumItems : Word);
        {-Change the number of items to display}
      procedure ChangeItemWidth(ItemWidth : Byte);
        {-Change the item width}
      procedure ChangeStringProc(StringProc : pkStringProc);
        {-Change the string function}
      procedure ChangeOrientation(Orientation : pkGenlProc);
        {-Change the orientation}
      procedure ChangeCommandHandler(CommandHandler : pkGenlProc);
        {-Change between single and multiple choice}
      {$IFDEF UseAdjustableWindows}
      procedure OptimizeSize;
        {-Resize window to surround items}
      {$ENDIF}
      procedure EnableDividers(Top, Bar, Bot : Char);    {!!.13}
        {-Enable column dividers}
      procedure DisableDividers;                         {!!.13}
        {-Disable column dividers}

      procedure SetPickAttr(ItemType : pkItemType; Selected : Boolean;
                            Color, Mono : Byte);
        {-Set one group of colors -- disables flexwriting}
      procedure SetPickFlex(ItemType : pkItemType; Selected : Boolean;
                            var Color, Mono : FlexAttrs);
        {-Set one group of colors for flexwriting}

      procedure SetSelectMarker(Left, Right : String);
        {-Define strings to mark selected items}
      procedure AllocateSelectSet(MaxItems : Word);
        {-Allocate the select set for the maximum items}
      procedure ClearSelected;
        {-Clear selected items}
      procedure MarkAllSelected;
        {-Mark all items selected}
      procedure SelectItem(Item : Word); virtual;
        {-Select an item}
      procedure DeselectItem(Item : Word); virtual;
        {-Deselect an item}
      function ItemIsSelected(Item : Word) : Boolean;
        {-Return True if item is selected}
      function GetSelectedCount : Word;
        {-Return number of items selected}
      procedure InitSequence(var Item : Word);
        {-Prepare to get first selected item}
      function HaveSelected(Item : Word) : Boolean;
        {-Return True if there is a selected item}
      procedure NextSelected(var Item : Word);
        {-Position Item at the next selected item}

      function IsProtected(Item : Word) : Boolean;
        {-Return True if item is protected from selection}
      function IsSemiProtected(Item : Word) : Boolean; virtual; {!!.03}
        {-Return true if an item is semi-protected}             {!!.03}
      function NumSelectableItems : Word;
        {-Return number of items that are not protected}

    {$IFDEF UseStreams}
      constructor Load00(var S : IdStream);
        {-Load a pick list from a stream - version 00}
      constructor Load(var S : IdStream);
        {-Load a pick list from a stream}
      procedure Store(var S : IdStream);
        {-Store a pick list in a stream}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      procedure EvaluateCmd(var Cmd : Word; var NextChoice : Word;
                            var NextFirst : Word);
      procedure pkGetUnprotected(Cmd : Word);
      procedure pkCheckProtected(var GetCmd : Boolean;
                                 var bChoice, iChoice : Word);
      procedure pkCommonValidation;
      function pkValidPos(Row, Col : Word) : Boolean;
      procedure pkValidateChoiceColFirst;
      procedure pkValidateChoiceRowFirst;
      procedure pkLowerFirstChoice;
      procedure pkRaiseFirstChoice;
      procedure pkSetUpperLeft;
      procedure pkSetLowerRight;
      procedure pkInitPickSize1; virtual; {!!.13}
      procedure pkInitPickSize2;
      procedure pkInitPick;
      procedure pkDrawItem(Item : Word; Row, Col : Byte; Selected : Boolean); virtual; {!!.22}
      procedure pkPadRows;
      procedure pkPadCols;
      procedure pkDrawPage(ShowSelected : Boolean);
      procedure pkScrollUp(pChoice : Word; pRow, pCol : Byte);
      procedure pkScrollDown(pChoice : Word; pRow, pCol : Byte);
      procedure pkScrollLeft(pChoice : Word; pRow, pCol : Byte);
      procedure pkScrollRight(pChoice : Word; pRow, pCol : Byte);
      function pkScanForMatch(var Item : Word) : Boolean;

      procedure pkUpdateSearch;
      procedure pkResetSearchStr; virtual; {!!.03}

      procedure pkClearMoreRec(var MRec : MoreRec);
      function pkInitMoreRec(var MRec : MoreRec; var Fr : Frame;
                             var PromptStr : String80; Posn : HeaderPosType;
                             var UpStr, DnStr, SepStr : String10;
                             UpPos, DnPos, SepPos : Byte;
                             Fill : Char) : Word;
      procedure pkDoneMoreRec(var MRec : MoreRec);
      procedure pkUpdateMoreRec(var MRec : MoreRec;
                                MoreUp, MoreDown, HaveMore : Boolean);
      procedure pkResetMoreRec(var MRec : MoreRec; HaveMore : Boolean);
    {$IFDEF UseStreams}
      function pkLoadMoreRec(var MRec : MoreRec; var Fr : Frame;
                             var S : IdStream) : Word;
      procedure pkStoreMoreRec(var MRec : MoreRec; var S : IdStream);
    {$ENDIF}
      function pkProcessCursorCommand(var Cmd : Word) : Boolean; virtual; {!!.13}
      {$IFDEF UseMouse}
      procedure pkMoveToNewRowCol(nRow, nCol : Byte);    {!!.03}
      function pkProcessMouseCommand(var Cmd : Word; Multi : Boolean) : Boolean;
      {$ENDIF}
      function pkCheckWrap(Cmd : Word) : Word;
      procedure pkSetPickAttr(ItemType : pkItemType; Selected : Boolean;
                              Color, Mono : Byte);
      procedure pkUpdatePick(pFirst, pChoice : Word;
                             pRow, pCol : Byte); virtual; {!!.01}
      procedure pkSetItemCount(NumItems : Word); {!!.03}
      {.Z-}
    end;

var
  {$IFDEF UseDrag}                   {!!.03}
  PickCommands : DragProcessor;      {!!.03}
  {$ELSE}                            {!!.03}
  PickCommands : CommandProcessor;
  {$ENDIF}                           {!!.03}

  {---- Search modes ----}
  function PickNoSearch(P : PickListPtr) : Boolean;
    {-No searching mode}
  function PickStringSearch(P : PickListPtr) : Boolean;
    {-String searching mode}
  function PickAltStringSearch(P : PickListPtr) : Boolean;
    {-Alternate string searching mode}
  function PickCharSearch(P : PickListPtr) : Boolean;
    {-Character searching mode}
  function PickCharExit(P : PickListPtr) : Boolean;
    {-Character searching with exit on match}
  function PickAnyCharExit(P : PickListPtr) : Boolean;
    {-Exit on any alpha character. No highlight repositioning}

  {---- List orientations ----}
  procedure PickVertical(P : PickListPtr);
    {-Initialize pick for items arranged vertically}
  procedure PickHorizontal(P : PickListPtr);
    {-Initialize pick for items arranged horizontally}
  procedure PickSnaking(P : PickListPtr);
    {-Initialize pick for items arranged vertically, snaking}

  {---- Command mode initialization ----}
  procedure SingleChoice(P : PickListPtr);
    {-Initialize for a single choice picklist}
  procedure MultipleChoice(P : PickListPtr);
    {-Initialize for a multiple choice picklist}

  {---- Premove routines ----}
  procedure NoMoveAction(P : PickListPtr);
    {-User hook routine called just before getting each pick command}

{$IFDEF UseStreams}
  {---- Stream registration ----}
  procedure PickListStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing pick lists}
{$ENDIF}

  procedure NoPickString(Item : Word; Mode : pkMode;   {!!.11}
                         var IType : pkItemType; var IString : String;
                         PickPtr : PickListPtr);
    {-Default user string proc must be overridden}

  {---- Command handlers (for internal use) ----}
  {.Z+}
  function SingleChoiceCommand(var Cmd : Word; P : PickListPtr) : Boolean;
    {-Evaluate a command, returning True if PickList should exit}
  function MultipleChoiceCommand(var Cmd : Word; P : PickListPtr) : Boolean;
    {-Evaluate a command, returning True if PickList should exit}
  {.Z-}

  {====================================================================}

implementation

  {$I OPPICK.IN1}                 {Low level routines and methods}
  {$I OPPICK.IN2}                 {PickList object}

begin
  {Initialize command processor}
  PickCommands.Init(@PickKeySet, PickKeyMax);
end.
