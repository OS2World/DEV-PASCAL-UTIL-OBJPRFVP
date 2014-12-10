
{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPMENU.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpMenu;
  {-Pulldown menu routines}

interface

uses
  Use32,
  OpConst,         {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd,
  OpFrame,
  OpAbsWin,        {!!.30}
  OpWindow
  {$IFDEF UseDrag} {!!.03}
  , OpDrag         {!!.03}
  {$ENDIF}         {!!.03}
  ;

  {$I OPMENU.ICD}  {configuration data}

  {.F-}
const
  {---- Option codes ----}
  mnAlphaMatch      = $0001;      {Check for alpha matches}
  mnSelectOnMatch   = $0002;      {Implicit <enter> on alpha char match}
  mnAllowPending    = $0004;      {Submenu stays down after passing no submenu}
  mnArrowSelect     = $0008;      {Select submenus with arrows}
  mnPopOnSelect     = $0010;      {Pop current submenu after selecting an item}
  mnAllHotSpots     = $0020;      {Exit on any mouse hotspot in menu system}
  mnUseItemForTopic = $0040;      {User the current item key for help topic,
                                   else use the code in the command processor}
  mnSelectOnClick   = $0080;      {Implicit <enter> on first mouse click}
  mnMouseSupport    = $0100;      {Set when mouse support is requested
                                   (used by MAKEMENU only)}
  mnMainSelect      = $0200;      {Set when main menu shows selection}
  mnMainHighlight   = $0400;      {Set when main menu shows highlight}
  mnAltMatch        = $0800;      {Alt-keys can select top level menu}  {!!.03}
  {... for internal use ...}
  mnChangedItems    = $2000;      {Set when item changed by mouse}      {!!.03}
  mnCurrItemActive  = $4000;      {Set when CurrentItem proc is active} {!!.01}
  mnSubPending      = $8000;      {Set when a submenu is pending}

  DefMenuOptions    : Word = mnAlphaMatch+mnSelectOnMatch+mnArrowSelect+
                             mnAllHotSpots;
  BadMenuOptions    : Word = mnChangedItems+                            {!!.03}
                             mnSubPending+mnCurrItemActive;             {!!.01}
  MenuDepth         : Word = 8;   {Initial maximum nesting of submenus}

  {---- Item option codes ----}
  inProtected       = $01;        {Protected item}
  inSemiProtected   = $02;        {Semi-protected item}                 {!!.03}
  {... for internal use ...}
  inOnHeap          = $80;        {Item strings on heap}

  DefItemOptions    : Byte = 0;

  {---- Item list codes ----}
  ilKeepHighlight   = $40;        {Set to show select char in highlight}{!!.21}
  ilCustomActive    = $80;        {Set when custom item proc is active} {!!.01}

  DefItemListOptions : Byte = 0;  {!!.01}

  {---- Special key value that indicates no key ----}
  mnNotAKey         = $FFFFFFFF;  {Use with ItemsDone to set to first item}

  inAllocConsPtr    : Boolean = False; {Used by MAKEMENU only}

type
  MenuPtr           = ^Menu;      {Pointers to objects}
  SubMenuPtr        = ^SubMenu;
  MenuItemListPtr   = ^MenuItemList;
  MenuItemNodePtr   = ^MenuItemNode;

  {Templates for user-definable hook routines}
  mnCustomProc      = procedure (var Name : String; Key : LongInt;
                                 Selected, Highlighted : Boolean;
                                 WPtr : RawWindowPtr);
  mnGenlProc        = procedure (MPtr : MenuPtr);
  mnCurrentItemProc = procedure (CurrentItem : MenuItemNodePtr; MPtr : MenuPtr);

  mnOrientation     =             {Item list orientation}
    (Vertical, Horizontal, NoOrient);

  MenuItemNode      =             {Describes one menu item}
    object(DoubleListNode)
      inDisplayPos  : Byte;       {Offset from edge of window}
      inSelectPos   : Byte;       {Offset of selection character, 0 for none}
      inFlags       : Byte;       {Bit flags}
      inMenuKey     : LongInt;    {Code returned to identify menu item}
      inSubPtr      : SubMenuPtr; {Points to submenu if any}
      inNamePtr     : StringPtr;  {Points to item name}
      inHelpPtr     : StringPtr;  {Points to associated help string}
      inConsPtr     : StringPtr;  {Points to Pascal constant name-MAKEMENU only}
      {.Z+}
      constructor Init(Name : String;
                       DisplayPos, SelectPos : Byte;
                       Key : LongInt;
                       Help : String);
        {-Initialize item, allocating string space on heap}
      constructor InitP(NamePtr : StringPtr;
                        DisplayPos, SelectPos : Byte;
                        Key : LongInt;
                        HelpPtr : StringPtr);
        {-Initialize item, not allocating string space on heap}
      destructor Done; virtual;
        {-Dispose of item node}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load an item node from a stream}
      procedure Store(var S : IdStream);
        {-Store an item node in a stream}
    {$ENDIF}
      {.Z-}

      procedure Protect;
        {-Protect item}
      procedure Unprotect;
        {-Unprotect item}
      procedure SemiProtect; {!!.03}
        {-Semi-protect item} {!!.03}
      function ChangeName(Name : String) : Boolean;
        {-Change the name of item}
      procedure ChangeKey(Key : LongInt);
        {-Change the item key}
      function ChangeHelp(Help : String) : Boolean;
        {-Change the help string}
      procedure ChangeSelectPos(SelectPos : Byte);
        {-Change the select character offset}
      procedure ChangeDisplayPos(DisplayPos : Byte);
        {-Change the display offset}
      procedure ChangeSubPtr(SPtr : SubMenuPtr);
        {-Change submenu pointer of item}

      procedure DisplayInfo(var DisplayPos, SelectPos : Byte; var S : String);
        {-Return string and display offsets for item}
      function HelpString : String;
        {-Return help string for item}
      function ItemKey : LongInt;
        {-Return menu key for item}
      function SubPtr : SubMenuPtr;
        {-Return submenu pointer for item}
      function IsProtected : Boolean;
        {-Return True if item is protected}
      function IsSemiProtected : Boolean;        {!!.03}
        {-Return True if item is semi-protected} {!!.03}
      function SelectChar : Char;
        {-Return alpha selection char (uppercase)}
      function GetDisplayPos : Byte;
        {-Return just the display offset}

      {++++ for internal use ++++}
      {.Z+}
      function inSetConsName(Name : String) : Boolean;
      {.Z-}
    end;

  MenuItemList          =              {List of items for one submenu}
    object(CircularList)
      ilCurrent     : MenuItemNodePtr; {Currently selected item}
      ilSearch      : MenuItemNodePtr; {Item matching a particular search}
      ilOrient      : mnOrientation;   {Orientation of list}
      ilPad         : Byte;            {Number of characters to left-pad item}
      ilLeftSel     : String[5];       {Left selection string}
      ilRightSel    : String[5];       {Right selection string}
      ilNormColor   : Byte;            {Normal item color}
      ilNormMono    : Byte;
      ilSelectColor : Byte;            {Selected item color}
      ilSelectMono  : Byte;
      ilHighColor   : Byte;            {Pick highlight color}
      ilHighMono    : Byte;
      ilProtectColor: Byte;            {Protected item color}
      ilProtectMono : Byte;
      ilCustom      : mnCustomProc;    {User-supplied string customizer}
      ilFlags       : Byte;            {Internal flags -- not in stream} {!!.01}
      {.Z+}
      constructor Init(Orient : mnOrientation;
                       var Colors : ColorSet);
        {-Initialize an empty item list}
      destructor Done; virtual;
        {-Destroy an item list}

      procedure SetNormAttr(Color, Mono : Byte);
        {-Set attributes for normal item text}
      procedure SetSelectAttr(Color, Mono : Byte);
        {-Set attributes for selected item text}
      procedure SetHighAttr(Color, Mono : Byte);
        {-Set attributes for pick highlight text}
      procedure SetProtectAttr(Color, Mono : Byte);
        {-Set attributes for protected item text}
      procedure SetLeftPad(LeftPad : Byte);
        {-Set the number of characters to left pad each item when displayed}
      procedure SetSelectMarker(Left, Right : String);
        {-Define strings to mark selected items}

      procedure SetCustomStringProc(CustomProc : mnCustomProc);
        {-Set a different string customizer}
      procedure CustomizeItem(var Name : String; Key : LongInt;
                              Selected, Highlighted : Boolean;
                              WPtr : RawWindowPtr); virtual;
        {-Customize the string for the given item}
      procedure DrawItem(IPtr : MenuItemNodePtr;
                         Selected, Highlighted : Boolean;
                         WPtr : RawWindowPtr); virtual;
        {-Draw one item}
      procedure Draw(WPtr : RawWindowPtr;
                     ShowSelected, ShowHighlight : Boolean);
        {-Draw entire list of items within window WPtr^}

      function Orientation : mnOrientation;
        {-Return orientation of MenuItemList}
      function CurrentItemPtr : MenuItemNodePtr;
        {-Return pointer to current item}
      function CurrentSubPtr : SubMenuPtr;
        {-Return submenu pointer of current item}
      function CurrentKey : LongInt;
        {-Return key of current item}
      function FirstKey : LongInt;
        {-Return key of the first item on the list}
      function FindItemByKey(Key : LongInt) : MenuItemNodePtr;
        {-Return pointer to matching item, or nil for none}
      function FindItemByChar(Ch : Char) : MenuItemNodePtr;
        {-Return pointer to matching item, or nil for none}
      function FindItemByPos(X, Y : Byte;
                             WPtr : RawWindowPtr) : MenuItemNodePtr;
        {-Return pointer to item at window-relative position, or nil for none}
      procedure OptimumSize(var W, H : Byte);
        {-Return optimum window size for the list of items}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a list from a stream}
      procedure Store(var S : IdStream);
        {-Store a list in a stream}
    {$ENDIF}

      {++++ for internal use ++++}
      function ilHasUnprotected : Boolean;
      procedure ilSetCurrent(Current : MenuItemNodePtr);
      function ilFirstUnprotected : MenuItemNodePtr;
      function ilPrevUnprotected : MenuItemNodePtr;
      function ilListHasMoreItems : Boolean;
      procedure ilUpdateCurrent(WPtr : RawWindowPtr;
                                NewCurrent : MenuItemNodePtr);
      procedure ilPrev(WPtr : RawWindowPtr);
      procedure ilNext(WPtr : RawWindowPtr);
      procedure ilHome(WPtr : RawWindowPtr);
      procedure ilEnd(WPtr : RawWindowPtr);
      function ilAddItem(Name : String; DisplayPos, SelectPos : Byte;
                         Key : LongInt; Help : String) : Boolean;
      function ilAddItemP(NamePtr : StringPtr; DisplayPos, SelectPos : Byte;
                          Key : LongInt; HelpPtr : StringPtr) : Boolean;
      function ilItemNumber(IPtr : MenuItemNodePtr) : Word;
      {.Z-}
    end;

  SubMenu           =             {Collection of items in a window}
    object(RawWindow)
      smItems       : MenuItemList;   {List of menu items}
      {.Z+}
      constructor Init(X1, Y1, X2, Y2 : Byte; Orient : mnOrientation);
        {-Create a submenu with default options and colors}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte; var Colors : ColorSet;
                             Options : LongInt; Orient : mnOrientation);
        {-Create a submenu with custom options}
      destructor Done; virtual;
        {-Destroy a submenu}
      procedure UpdateContents; virtual;
        {-Update the contents}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a submenu from a stream}
      procedure Store(var S : IdStream);
        {-Store a submenu to a stream}
    {$ENDIF}
      {.Z-}
      {++++ for internal use ++++}
    end;

  {Hook templates for "visitation" routines described below}
  mnItemProc        = procedure (IPtr : MenuItemNodePtr; var D; MPtr : MenuPtr);
  mnWindowProc      = procedure (WPtr : RawWindowPtr; var D; MPtr : MenuPtr);
  mnListProc        = procedure (LPtr : MenuItemListPtr; var D; MPtr : MenuPtr);

  Menu              =
    object(CommandWindow)
      mnItems       : MenuItemList; {Main list of menu items}
      mnStack       : PointerStack; {Stack of active submenus}
      mnSearch      : PointerStack; {Stack used for searching tree}
      mnStackPos    : Word;         {Stack pos: used for building and redrawing}
      mnFlags       : Word;         {Options}
      mnHelpRow     : Byte;         {Help row -- a convenience for MAKEMENU}
      mnHelpColor   : Byte;         {Help line colors}
      mnHelpMono    : Byte;
      mnLastKey     : LongInt;           {Last choice}
      mnCurrItem    : MenuItemNodePtr;   {Pointer to current item}
      mnMove        : mnGenlProc;        {User-supplied pre-move procedure}
      mnCurrItemProc: mnCurrentItemProc; {User-supplied active item procedure}

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       Orient : mnOrientation);
        {-Create a menu with default options and colors}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orient : mnOrientation);
        {-Create a menu with custom window options}

      {... For changing or checking menu options}
      procedure mnOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure mnOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function mnOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}

      {... For building menu systems}
      procedure AddItem(Name : String; DisplayPos, SelectPos : Byte;
                        Key : LongInt);
        {-Add item to current item list, allocating string space on heap}
      procedure AddItemPtr(var Name : String; DisplayPos, SelectPos : Byte;
                           Key : LongInt);
        {-Add item to current item list, not allocating string space on heap}
      procedure AddItemHelp(Name : String; DisplayPos, SelectPos : Byte;
                            Key : LongInt; Help : String);
        {-Add item to current item list, allocating string space on heap}
      procedure AddItemHelpPtr(var Name : String; DisplayPos, SelectPos : Byte;
                               Key : LongInt; var Help : String);
        {-Add item to current item list, not allocating string space on heap}
      procedure AddSeparator(FirstChar, SpanChar, LastChar : Char;
                             DisplayPos : Byte);
        {-Add a separator bar at offset DisplayPos within the window}
      procedure AddSeparatorColor(FirstChar, SpanChar, LastChar : Char;
                                  DisplayPos : Byte;
                                  AttrColor, AttrMono : Byte);
        {-Add a separator bar at offset DisplayPos within the window with
          specified colors}
      procedure AddSubMenu(X1, Y1, X2, Y2 : Byte; Orient : mnOrientation);
        {-Add an unframed submenu with options and colors cloned from main menu}
      procedure AddFramedSubMenu(X1, Y1, X2, Y2 : Byte; Orient : mnOrientation;
                                 FA : FrameArray);
        {-Add a framed submenu with options and colors cloned from main menu}
      procedure AddSubMenuCustom(X1, Y1, X2, Y2 : Byte; var Colors : ColorSet;
                                 Options : LongInt; Orient : mnOrientation;
                                 FA : FrameArray);
        {-Add a submenu with custom window options}
      procedure ItemsDone;
        {-Terminate the current item list. Set first item as default.}
      procedure ItemsDoneCustom(OptimizeSize : Boolean; DefaultKey : LongInt);
        {-Terminate the current item list and customize size and default key}

      {... For changing characteristics of the current item list}
      procedure SetNormAttr(Color, Mono : Byte);
        {-Set attributes for normal item text}
      procedure SetSelectAttr(Color, Mono : Byte);
        {-Set attributes for selected item text}
      procedure SetHighAttr(Color, Mono : Byte);
        {-Set attributes for pick highlight text}
      procedure SetProtectAttr(Color, Mono : Byte);
        {-Set attributes for protected item text}
      procedure SetFrameAttr(Color, Mono : Byte);
        {-Set frame attributes of all windows in menu system}
      procedure SetHeaderAttr(Color, Mono : Byte);
        {-Set header attributes of all windows in menu system}
      {$IFDEF UseShadows}
      procedure SetShadowAttr(Color, Mono : Byte);
        {-Set shadow attributes of all windows in menu system}
      {$ENDIF}
      procedure SetLeftPad(LeftPad : Byte);
        {-Set the number of characters to left pad each item when displayed}
      procedure SetSelectMarker(Left, Right : String);
        {-Define strings to mark selected items}
      procedure SetSelectHighlight(IsOn : Boolean);                         {!!.21}
        {-Turn on/off display of the select char in the highlighted item} {!!.21}

      {... For ornamenting the current submenu}
      procedure AddHeader(S : string; Posn : HeaderPosType);
        {-Add a standard header}
      procedure AddHeaderColor(S : string; Posn : HeaderPosType;
                               AttrColor, AttrMono : Byte);
        {-Add a standard header with custom color}
      procedure AddCustomHeader(S : string; Posn : FrameCornerType;
                                DX, DY : Integer;
                                AttrColor, AttrMono : Byte);
        {-Add a custom header}
      {$IFDEF UseShadows}
      procedure AddShadow(Posn : ShadowPosType; DrawType : ShadowDrawType);
        {-Add a standard shadow}
      procedure AddShadowColor(Posn : ShadowPosType; DrawType : ShadowDrawType;
                               AttrColor, AttrMono : Byte);
        {-Add a standard shadow with custom color}
      {$ENDIF}
      procedure EnableExplosions(StageDelay : Word);
        {-Enable exploding windows with StageDelay milliseconds per stage}
      procedure EnableNormalOpen;
        {-Disable exploding windows or alternate draw routines}

      {... For changing appearance of all submenus at once}
      procedure SetAllNormAttr(Color, Mono : Byte);
        {-Set normal attributes of all items in menu system}
      procedure SetAllSelectAttr(Color, Mono : Byte);
        {-Set select attributes of all items in menu system}
      procedure SetAllHighAttr(Color, Mono : Byte);
        {-Set highlight attributes of all items in menu system}
      procedure SetAllProtectAttr(Color, Mono : Byte);
        {-Set protect attributes of all items in menu system}
      procedure SetAllFrameAttr(Color, Mono : Byte);
        {-Set frame attributes of all windows in menu system}
      procedure SetAllHeaderAttr(Color, Mono : Byte);
        {-Set header attributes of all windows in menu system}
      {$IFDEF UseShadows}
      procedure SetAllShadowAttr(Color, Mono : Byte);
        {-Set shadow attributes of all windows in menu system}
      procedure AddShadows(MainMenu : Boolean; Posn : ShadowPosType;
                           DrawType : ShadowDrawType);
        {-Add shadows to all submenus}
      {$ENDIF}
      procedure EnableAllExplosions(StageDelay : Word);
        {-Enable exploding windows with StageDelay milliseconds per stage}
      procedure EnableAllAlternateOpen(OpenProc, CloseProc : RawWindowProc;
                                       StageDelay : Word);
        {-Enable alternate draw and erase routines}
      procedure EnableAllNormalOpen;
        {-Disable exploding windows or alternate draw routines}
      procedure SetAllSelectMarker(Left, Right : String);
        {-Define strings to mark selected items in all submenus}
      procedure SetHelpAttr(Color, Mono : Byte);
        {-Set the colors for the help line}
      procedure GetHelpAttr(var Color, Mono : Byte);
        {-Get the colors stored for the help line}

      {... General purpose hooks for customizing menu systems}
      procedure VisitAllItems(IProc : mnItemProc; var D);
        {-Call the specified procedure for all items in the menu system}
      procedure VisitAllWindows(WProc : mnWindowProc; var D);
        {-Call the specified procedure for all submenus in the menu system}
      procedure VisitAllItemLists(LProc : mnListProc; var D);
        {-Call the specified procedure for all item lists in the menu system}

      {... For specifying user hook routines}
      procedure SetCurrentItemProc(IP : mnCurrentItemProc);
        {-Set procedure to be called whenever the active item is drawn}
      procedure SetCustomStringProc(CP : mnCustomProc);
        {-Set item string customization procedure}
      procedure CurrentItem(IPtr : MenuItemNodePtr); virtual;
        {-Called just after the currently selected item is drawn}

      {... For getting a menu choice from the user}
      procedure ProcessSelf; virtual; {!!.01}
        {-Process menu commands leading to a choice}
      function MenuChoice : LongInt;
        {-Return the last menu choice}

      {... For changing the display state of a menu system}
      procedure EraseCurrentSubMenu;
        {-Erase current submenu}
      procedure EraseAllSubMenus(ShowSelect, ShowHighlight : Boolean);
        {-Erase all submenus, leave main menu showing in specified state}
      procedure EraseClear;
        {-Erase menu and clear active submenus for next draw}
      procedure SelectItem(Key : LongInt);
        {-Select specified item and update screen}
      procedure SelectSubMenu(Key : LongInt);
        {-Select current item of submenu containing Key and update screen}
      function SelectItemByPos(X, Y : Byte) : Boolean;
        {-Select visible item (if any) at absolute position and update screen}
      procedure Redraw;
        {-Erase and redraw menu system, presumably after changing an item}

      procedure ProtectItem(Key : LongInt);
        {-Protect specified item. No screen update}
      procedure SemiProtectItem(Key : LongInt);          {!!.03}
        {-Semi-protect specified item. No screen update} {!!.03}
      procedure UnprotectItem(Key : LongInt);
        {-Unprotect specified item. No screen update}
      procedure DefaultItem(Key : LongInt);
        {-Make item Key the default of its submenu. No screen update}
      procedure DefaultPath(Key : LongInt);
        {-Select path to specified item. No screen update}

      {... For referring to existing items or submenus}
      function ActiveListPtr : MenuItemListPtr;
        {-Return pointer to active item list}
      function ActiveWinPtr : RawWindowPtr;
        {-Return pointer to active raw window}
      function ActiveSubPtr : SubMenuPtr;
        {-Return pointer to active submenu}
      function ActiveItemPtr : MenuItemNodePtr;
        {-Return pointer to currently active item}
      function FindItem(Key : LongInt) : MenuItemNodePtr;
        {-Return pointer to item node}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a list from a stream}
      procedure Store(var S : IdStream);
        {-Store a list in a stream}
    {$ENDIF}

      {... For overriding inherited methods}
      destructor Done; virtual;
        {-Destroy a menu}
      procedure UpdateContents; virtual;
        {-Update the contents}
      procedure Erase; virtual;
        {-Erase menu system. Remembers previous submenus for next draw}
      function IsCurrent : Boolean; virtual;
        {-Return True if menu system is the current window}
      {$IFDEF UseAdjustableWindows}
      procedure MoveWindow(DX, DY : Integer); virtual;
        {-Move a menu}
      procedure UpdateScreenSize;
        {-Update window size, position, and limits for current screen size}
      {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      procedure PreMove; virtual;
        {-Called just before getting each keyboard command}
      procedure SetMoveProc(MP : mnGenlProc);
        {-Set procedure to be called prior to getting each keyboard command}
      function Intersects(W : AbstractWindowPtr) : Boolean; virtual; {!!.03}
        {-Return true if Self intersects another window W}
      procedure mnCloneColors(var DefColors, Colors : ColorSet);
      procedure mnAddSubPrim(X1, Y1, X2, Y2 : Byte; var Colors : ColorSet;
                             Options : LongInt; Orient : mnOrientation;
                             Framed : Boolean; FA : FrameArray);
      procedure mnMarkCurrentItem; {!!.01}
      function mnEraseAllSubs : Boolean;
      function mnEvaluateCmd : Boolean; virtual; {!!.13}
      procedure mnVisitAnItemList(var List : MenuItemList;
                                  LProc : mnListProc; var D);
      procedure mnVisitAWindow(var List : MenuItemList;
                               WProc : mnWindowProc; var D);
      procedure mnVisitAnItem(var List : MenuItemList;
                              IProc : mnItemProc; var D);
      procedure mnDrawActiveItems;
      function mnFindKeyAndStack(var List : MenuItemList; Key : LongInt;
                                 ProtectedOK : Boolean) : Boolean;
      function mnFindItemAndList(Key : LongInt;
                                 var LPtr : MenuItemListPtr) : MenuItemNodePtr;
      function mnParentHasMoreItems : Boolean;
      function mnPopSubMenu : Boolean;
      procedure mnPushDraw(SPtr : SubMenuPtr);
      function mnPushSubMenu : Boolean;
      procedure mnCheckSubPending;
      function mnParentOrient : mnOrientation;
      procedure mnMoveByOne(ToNext : Boolean);
      procedure mnArrowSelection(Orient : mnOrientation; ToNext : Boolean);
      function mnCharSelection : Boolean;
      function mnAltCharSelection : Boolean; {!!.03}
      function mnSelectItem : Boolean;
      {$IFDEF UseMouse}
      function mnMouseSelection : Boolean;
      {$ENDIF}
      procedure mnDisposeSubMenus(var List : MenuItemList);
      {.Z-}
    end;

var
  {$IFDEF UseDrag}                   {!!.03}
  MenuCommands : DragProcessor;      {!!.03}
  {$ELSE}                            {!!.03}
  MenuCommands : CommandProcessor;
  {$ENDIF}                           {!!.03}

  {.F+}

  {---------- stream registration -----------}

{$IFDEF UseStreams}
  procedure MenuStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing menus}
{$ENDIF}

  {---------- default user hook routines ----------}

  procedure NoCustomization(var Name : String; Key : LongInt;
                            Selected, Highlighted : Boolean;
                            WPtr : RawWindowPtr);
    {-Default string customization routine that does nothing}

  procedure NoMoveAction(MPtr : MenuPtr);
    {-Default pre-move routine that does nothing}

  procedure NoCurrItemAction(CurrentItem : MenuItemNodePtr; MPtr : MenuPtr);
    {-Default active item move routine that does nothing}

  {====================================================================}

implementation

  {$I OPMENU.IN1}       {Items, item lists, submenus}
  {$I OPMENU.IN2}       {Menu object}

begin
  {Initialize command processor}
  MenuCommands.Init(@MenuKeySet, MenuKeyMax);
end.
