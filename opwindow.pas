{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}


{*********************************************************}
{*                  OPWINDOW.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpWindow;
  {-High level support for text windows}


interface

uses
  Use32,
{$IFDEF VIRTUALPASCAL}
  VpSysLow,
{$ENDIF}
  OpConst,  {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd,
  OpFrame,
  OpAbsWin; {!!.30}

const
  {------------ OPWINDOW option codes -----------}
  wBordered         = $00000001;  {Make space for window border?}
  wClear            = $00000002;  {Clear the window when it is first displayed?}
  wSaveContents     = $00000004;  {Save the contents of the window when erased?}
  wUserContents     = $00000008;  {User will restore contents on request?}
  wSoundEffects     = $00000010;  {Use sound effects during explosions?}
  wAltFrame         = $00000020;  {Use alternate frame type when non-current?}
  wSetMouse         = $00000040;  {Set mouse window, cursor on entry to window?}
  wFullMouseWindow  = $00000080;  {Force to full mouse window on entry?}
  wResizeable       = $00000100;  {Can be resized and zoomed?}
  wAllMouseEvents   = $00000200;  {Mouse events frOutsideFrame and
                                   frInsideFrame cause an exit?}
  wStoreContents    = $00000400;  {Store window _contents_ in streams?}
  wNoCoversBuffer   = $00000800;  {Don't allocate covers buffer}    {!!.01}
  wCoversOnDemand   = $00001000;  {Covers buffer only while active} {!!.01}

  {-------- OPWINDOW internal status flags ------}
  wHasChildren      = $10000000;  {Window has children (in stream)} {!!.01}
  wIsChild          = $20000000;  {Window is a child of another}    {!!.01}
  wZoomed           = $40000000;  {Window is zoomed}
  wActive           = $80000000;  {Window is on-screen}

  DefWindowOptions  : LongInt =    {Settings when window initialized}
    wClear+wSaveContents+wResizeable+wStoreContents;

  TogWindowOptions  : LongInt =    {Window options that require only a
                                    simple toggle operation when changed}
    wUserContents+wSoundEffects+wAltFrame+wSetMouse+
    wFullMouseWindow+wResizeable+wAllMouseEvents+wStoreContents;

  {------------ OPWINDOW error codes ------------}
  wNotCurrent   =
    epNonFatal+ecWinNotCurrent;   {Window must be current for this operation}
  wOutOfHeap    =
    epFatal+ecOutOfMemory;        {Insufficient heap space}

  {------------ WindowStack flags ------------}
  wsActivated       = $0001;      {Set when window stack in temporary state}
  wsTileActivated   = $0002;      {Set when window stack in tiled temp state} {!!.03}
  wsOptimizeTile    = $0004;      {Set when tile optimization is allowed} {!!.03}

type
  RawWindowPtr      = ^RawWindow;
  RawWindowProc     = procedure(WPtr : RawWindowPtr);

  RawWindow         =             {A window without stacking support}
    object(AbstractWindow)
      wTextColor    : Byte;       {Attr for contents - color}
      wTextMono     : Byte;       {Attr for contents - mono}
      wFlags        : LongInt;    {Bit-mapped status and option flags}
      wBackChar     : Char;       {Background character for clearing}
      wRes          : SmallWord;  {Error code}
      wCursor       : CursorType; {Cursor shape}
      wCursorX      : Byte;       {Absolute X position of cursor}
      wCursorY      : Byte;       {Absolute Y position of cursor}
      wStageDelay   : Word;       {Milliseconds per stage of explosion}
      wFramePos     : FramePosType; {Results from last call to EvaluatePos}
      wHotCode      : Byte;       {Hot spot code}
      wUserVal      : LongInt;    {User scroll bar value}
      wUserData     : LongInt;    {Reserved for user data storage}

      {----- Saves state of screen when window was drawn -----}
      wTextAttr     : Byte;       {Text attribute}
      wWindMin      : SmallWord;  {Window coordinates}
      wWindMax      : SmallWord;
      wInitCursor   : Word;       {Scan lines when drawn}
      wInitXY       : Word;       {Absolute XY position of cursor when drawn}

      {$IFDEF UseMouse}
      wMouseCurX    : Byte;       {Window-relative position of mouse cursor}
      wMouseCurY    : Byte;
      wMouseCurOn   : Boolean;    {Whether mouse cursor is on or off}

      {----- Saves state of mouse when window was drawn -----}
      wMouseXL      : Byte;       {Window coordinates}
      wMouseYL      : Byte;
      wMouseXH      : Byte;
      wMouseYH      : Byte;
      wMInitX       : Byte;       {Window-relative cursor position}
      wMInitY       : Byte;
      wMCurOn       : Boolean;    {Whether mouse cursor is on or off}
      {$ELSE}
      wDummyArray   : array[1..10] of Byte;{Makes streams work w/ or w/o mouse}
      {$ENDIF}

      wScrWidth     : Byte;       {Screen width when window created}
      wScrHeight    : Byte;       {Screen height when window created}

      {-- Structured components of raw window --}
      wFrame        : Frame;      {Frame to use when current, if any}
      aFrame        : Frame;      {Alternate Frame to use when not current}
      wContents     : ScreenRect; {Contents of the window and frame}
      wCovers       : ScreenRect; {What the window and frame cover}
      wOpenProc     : RawWindowProc; {Routine that opens window}
      wCloseProc    : RawWindowProc; {Routine that closes window}

      {-- Fields that won't be stored in a stream --}
      wSaveCurrent  : RawWindowPtr;  {Window that was current before this one}
      wFramePtr     : FramePtr;   {Address of frame currently in wFrame} {!!.01}

      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Create a window with default options and colors}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt);
        {-Create a window with custom options}
      constructor wCopy(var W : RawWindow);
        {-Initialize a window from another one, making a unique copy of
          all dynamic structures}
      destructor Done; virtual;
        {-Destroy a window}

      procedure AdjustFrameCoords(X1, Y1, X2, Y2 : Byte);
        {-Change frame coordinates of an inactive window.
          Completely clears existing frame definition}
      procedure DeltaAdjustFrame(dX1, dY1, dX2, dY2 : Integer);
        {-Change frame coordinates of an inactive window.
          Reallocates wCovers buffer but changes nothing else}
      {$IFDEF UseAdjustableWindows}
      procedure SetSizeLimits(MinW, MinH, MaxW, MaxH : Byte);
        {-Set limits for sizing of active window}
      procedure SetPosLimits(MinX, MinY, MaxX, MaxY : Byte);
        {-Set limits for moving and drawing (generally limited by frame)}
      procedure SetFrameLimits(MinW, MinH, MaxW, MaxH : Byte);
        {-Set limits for sizing of frame}
      procedure UpdateScreenSize;
        {-Update window size, position, and limits for current screen size}
      {$ENDIF}

      procedure EnableExplosions(StageDelay : Word);
        {-Enable exploding windows with StageDelay milliseconds per stage}
      procedure EnableAlternateOpen(OpenProc, CloseProc : RawWindowProc;
                                    StageDelay : Word);
        {-Enable alternate draw and erase routines}
      procedure EnableNormalOpen;
        {-Disable exploding windows or alternate draw routines}

      procedure wOptionsOn(OptionCodes : LongInt);
        {-Turn options on}
      procedure wOptionsOff(OptionCodes : LongInt);
        {-Turn options off}
      function wOptionsAreOn(OptionCodes : LongInt) : Boolean;
        {-Return true if all specified options are on}
      procedure SetTextAttr(Color, Mono : Byte); virtual;
        {-Set attributes for normal window text}
      procedure SetBackChar(BackChar : Char); virtual;
        {-Set background character}

      procedure wGotoXY(X, Y : Integer); virtual;
        {-Move cursor to window-relative coordinates}
      procedure wWhereXY(var X, Y : Integer); virtual;
        {-Return window-relative coordinates of cursor}
      procedure wWhereXYAbs(var X, Y : Byte);
        {-Return absolute coordinates of cursor}
      procedure SetCursor(CT : CursorType);
        {-Set cursor shape}
      function GetCursor : CursorType;
        {-Return current cursor shape}
      procedure ChangeHeader(Index : Byte; S : string);
        {-Change header string, and if window is current, update screen}

      function InWindow(AX, AY : Byte) : Boolean;
        {-Return true if specified absolute position is within active window}
      function InExtent(AX, AY : Byte; IncludeShadows : Boolean) : Boolean; {!!.12}
        {-Return true if specified absolute position is within extent of window}
      function InFrame(AX, AY : Byte; IncludeShadows : Boolean) : Boolean; {!!.12}
        {-Return true if position is within extent of window, but not within active window}
      function CursorInWindow : Boolean;
        {-Return true if cursor is currently within active window}
      procedure EvaluatePos(X, Y : Byte);
        {-Evaluate specified absolute position relative to window,
          hotspots, and scrollbars. Access returned values using PosResults}
      {$IFDEF UseMouse}
      procedure EvaluateMousePos;
        {-Evaluate current mouse position as with EvaluatePos}
      {$ENDIF}
      function PosResults(var FramePos : FramePosType;
                          var HotCode : Byte) : LongInt;
        {-Return values from most recent call to
          EvaluatePos or EvaluateMousePos}

      procedure Error(Code : Word); virtual;
        {-Report that an error occurred}
      function GetLastError : Word; virtual;
        {-Return and clear the last error code, 0 if none}
      function PeekLastError : Word; virtual;
        {-Return last error code without resetting internal variable}
      function RawError : Word; {!!.01}
        {-Return RawWindow internal error code without resetting it}

      function IsActive : Boolean; virtual;
        {-Return True if window is on-screen}
      function IsCurrent : Boolean; virtual;
        {-Return True if window is the current one}
      {$IFDEF UseAdjustableWindows}
      function IsZoomed : Boolean; virtual;
        {-Return True if window is currently zoomed}
      {$ENDIF}

      function MainFramePtr : AbstractFramePtr; virtual;
        {-Return a pointer to frame used when window is current}
      function AltFramePtr : AbstractFramePtr; virtual;
        {-Return a pointer to frame used when window is active but not current}
      function ActiveFramePtr : AbstractFramePtr; virtual;
        {-Return a pointer to frame the window is currently displaying}

      procedure Coordinates(var X1, Y1, X2, Y2 : Byte);
        {-Return the active coordinates of the window}
      procedure Extent(var X1, Y1, X2, Y2 : Byte; IncludeShadows : Boolean);
        {-Return the complete extent of the window, counting shadows if
          requested}
      function Intersects(W : AbstractWindowPtr) : Boolean; virtual; {!!.03}
        {-Return true if Self intersects another window W}

      procedure Draw; virtual;
        {-Draw or update a window}
      procedure UpdateContents; virtual;
        {-Update the contents of the currently displayed window}
      procedure Erase; virtual;
        {-Erase a window}
      procedure SaveContents; virtual;
        {-Save current window contents in the contents buffer}

      {$IFDEF UseScrollBars}
      procedure DrawSlider(Posn : FrameEdgeType; UserVal : LongInt);
        {-Draw or update the slider}
      procedure EraseSlider(Posn : FrameEdgeType);
        {-Erase the slider}
      function TweakSlider(Posn : FrameEdgeType; MousePosn : Byte;
                           UserVal : LongInt; Step : LongInt) : LongInt;
        {-Adjust user value until slider exactly matches mouse position}
      procedure ChangeScrollBar(Posn : FrameEdgeType;
                                MinUser, MaxUser : LongInt);
        {-Change user range of existing scroll bar}
      procedure ChangeAllScrollBars(MinHoriz, MaxHoriz : LongInt;
                                    MinVert, MaxVert : LongInt); virtual;
        {-Change user ranges of existing scroll bars. Erases sliders}
      procedure DrawAllSliders(HorizVal, VertVal : LongInt); virtual;
        {-Draw or update all sliders}
      {$ENDIF}

      procedure Clear; virtual;
        {-Clear active window area with wTextXXXX and wBackChar}
      procedure ClearContents;
        {-Clear region within frame with wTextXXXX and wBackChar}

      {$IFDEF UseAdjustableWindows}
      procedure MoveWindow(DX, DY : Integer); virtual;
        {-Move a window}
      procedure ResizeWindow(DX, DY : Integer); virtual;
        {-Resize a window}
      procedure AdjustWindow(X1, Y1, X2, Y2 : Word); virtual;
        {-Set new coordinates and adjust all related structures}
      procedure Zoom; virtual;
        {-Increase window size to maximum limits temporarily}
      procedure Unzoom; virtual;
        {-Return zoomed window to pre-zoomed size}
      {$ENDIF}

      procedure ScrollHoriz(Cols : Integer); virtual;
        {-Scroll contents horizontally}
      procedure ScrollVert(Rows : Integer); virtual;
        {-Scroll contents vertically}

      procedure MarkCurrent; virtual;
        {-Called when window is made current}
      procedure MarkNotCurrent; virtual;
        {-Called when window is deselected}

      {--- window-relative fastwrite routines ---}
      procedure wFastWrite(St : string; Row, Col, Attr : Byte);
        {-Write a string}
      procedure wFastText(St : string; Row, Col : Byte);
        {-Write St at Row,Col without changing the underlying video attribute}
      procedure wFastVert(St : string; Row, Col, Attr : Byte);
        {-Write a string vertically}
      procedure wFastFill(Number : Word; Ch : Char; Row, Col, Attr : Byte);
        {-Fill Number chars at Row,Col in Attr}
      procedure wChangeAttribute(Number : Word; Row, Col, Attr : Byte);
        {-Change Number video attributes to Attr starting at Row,Col}
      procedure wFastCenter(St : string; Row, Attr : Byte);
        {-Write St centered on Row in Attr}
      procedure wFastFlush(St : string; Row, Attr : Byte);
        {-Write St flush right on Row in Attr}
      procedure wFastRead(Number, Row, Col : Byte; var St : string);
        {-Read Number characters from the screen into St starting at Row,Col}
      procedure wReadAttribute(Number, Row, Col : Byte; var St : string);
        {-Read Number attributes from the screen into St starting at Row,Col}
      procedure wWriteAttribute(St : String; Row, Col : Byte);
        {-Write string of attributes St at Row,Col without changing characters}
      procedure wFlexWrite(St : string; Row, Col : Byte;
                           var FAttrs : FlexAttrs);
        {-Write a string flexibly using window-relative coordinates}
      procedure wFastWriteCtrl(St : String; Row, Col, Attr, Ctrl : Byte);
        {-Write St at Row,Col in Attr (video attribute) without snow.
          Control characters displayed in Ctrl as upper-case letters}

      {--- frame-relative fastwrite routines ---}
      procedure fFastWrite(St : string; Row, Col, Attr : Byte);
        {-Write a string}
      procedure fFastText(St : string; Row, Col : Byte);
        {-Write St at Row,Col without changing the underlying video attribute}
      procedure fFastVert(St : string; Row, Col, Attr : Byte);
        {-Write a string vertically}
      procedure fFastFill(Number : Word; Ch : Char; Row, Col, Attr : Byte);
        {-Fill Number chars at Row,Col in Attr}
      procedure fChangeAttribute(Number : Word; Row, Col, Attr : Byte);
        {-Change Number video attributes to Attr starting at Row,Col}
      procedure fFastCenter(St : string; Row, Attr : Byte);
        {-Write St centered on Row in Attr}
      procedure fFastFlush(St : string; Row, Attr : Byte);
        {-Write St flush right on Row in Attr}
      procedure fFastRead(Number, Row, Col : Byte; var St : string);
        {-Read Number characters from the screen into St starting at Row,Col}
      procedure fReadAttribute(Number, Row, Col : Byte; var St : string);
        {-Read Number attributes from the screen into St starting at Row,Col}
      procedure fWriteAttribute(St : String; Row, Col : Byte);
        {-Write string of attributes St at Row,Col without changing characters}
      procedure fFlexWrite(St : string; Row, Col : Byte;
                           var FAttrs : FlexAttrs);
        {-Write a string flexibly using window-relative coordinates}
      procedure fFastWriteCtrl(St : String; Row, Col, Attr, Ctrl : Byte);
        {-Write St at Row,Col in Attr (video attribute) without snow.
          Control characters displayed in Ctrl as upper-case letters}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}

      {---- primarily for use by screen design tools ----}
      {.Z+}
      function IsExploding : Boolean;
        {-Returns True if window explodes}
      function HasFrame : Boolean;
        {-Returns True if wFrame has a frame}
      function HasHeaders : Boolean;
        {-Returns True if wFrame has headers (not counting spans)}
      function HasSpans : Boolean;
        {-Returns True if wFrame has spans}
      function HasHeaderType(HPT : HeaderPosType; var HP : HeaderPtr) : Boolean;
        {-Returns True if wFrame has a header of the specified type.
          Also returns a pointer to the header node in HP}
      procedure ResetHeaderColor(Color, Mono : Byte);
        {-Reset colors for all headers of wFrame}
      {$IFDEF UseShadows}
      function ShadowType : ShadowDrawType;
        {-Returns type of shadows on wFrame}
      function HasShadows : Boolean;
        {-Returns True if wFrame has shadows}
      procedure RemoveShadows;
        {-Remove shadows, if any}
      procedure ResetShadowColor(Color, Mono : Byte);
        {-Reset colors for all shadows of wFrame}
      {$ENDIF}
      {$IFDEF UseScrollBars}
      function HaveScrollBar(FET : FrameEdgeType) : Boolean;
        {-Returns True if wFrame has scroll bar on specified edge}
      function HasScrollBars : Boolean;
        {-Returns True if wFrame has scroll bars}
      procedure ResetScrollBarColor(SliColor, SliMono : Byte;
                                    BarColor, BarMono : Byte;
                                    ArrowColor, ArrowMono : Byte);
        {-Reset colors for all scroll bars of wFrame}
      {$ENDIF}
      {.Z-}

      {++++ for internal use ++++}
      {.Z+}
      procedure rwZeroOut;
      procedure rwDefaultFrameCoords(var FX1, FY1, FX2, FY2 : Byte);
      function rwFrameSetup(FX1, FY1, FX2, FY2 : Byte;
                            var wBorder, aBorder : FrameArray;
                            var wColors, aColors : ColorSet) : Word;
      procedure rwSetBordered(IsOn : Boolean);
      procedure rwDone;
      procedure rwClear;
      procedure rwClearContents;
      procedure rwSaveContents; {!!.01}
      procedure rwDraw; {!!.01}
      procedure rwSetWindowState; {!!.01}
      procedure rwErase; {!!.01}
      {$IFDEF UseAdjustableWindows}
      procedure rwResizeAdjustCursor(oXL, oYL, oXH, oYH : Byte); virtual;
      procedure rwResizeFrames(FX1, FY1, FX2, FY2 : Byte); virtual;
      function rwValidCoords(X1, Y1, X2, Y2 : Byte;
                             var FX1, FY1, FX2, FY2 : Byte) : Boolean; {!!.01}
      function rwValidZoomCoords(var X1, Y1, X2, Y2 : Byte) : Boolean; {!!.01}
      procedure rwResizePrim(X1, Y1, X2, Y2, FX1, FY1, FX2, FY2 : Byte); {!!.01}
      procedure rwUpdateWindowLimits(DX, DY : Integer); virtual; {!!.01}
      procedure rwResizeInactive(X1, Y1, X2, Y2 : Byte);
      procedure rwResizeCurrent(X1, Y1, X2, Y2 : Byte);
      procedure rwMarkUnzoomed; {!!.03} {!!.10}
      {$ENDIF}

      procedure rwScrollHorizPart(Cols : Integer; sXH : Byte);
      procedure rwScrollVert(Rows : Integer);
      function wAbsWnCoords(Row, Col : Byte) : Boolean; {!!.01}
      function wAbsFrCoords(Row, Col : Byte) : Boolean; {!!.01}
      function rwClassifyCursorType : CursorType;
      procedure rwSaveWindowState;
      function rwValidMainHeader(Index : Byte) : Boolean; {!!.01}
      {.Z-}
    end;

  WindowStackPtr    = ^WindowStack;
  WindowPtr         = ^StackWindow;
  StackWindowPtr    = ^StackWindow;

  StackWindowProc   = procedure (WPtr : WindowPtr);

  StackWindow       =             {A window with stacking support}
    object(RawWindow)
      wNumber       : SmallWord;       {A unique number assigned when window is stacked}
      wActiveStack  : WindowStackPtr;  {Stack this window is pushed on}
      wStackProc    : StackWindowProc; {Routine to update window number headers}
      wChildList    : SingleListPtr;   {Pointer to child list}     {!!.01}
      wActiveChild  : WindowPtr;       {Active child, nil if none} {!!.01}
      wParentPtr    : WindowPtr;       {Parent, nil if none}       {!!.01}

      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Create a stacked window with default options and colors}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet; Options : LongInt);
        {-Create a window with custom options}
      constructor wCopy(var S : StackWindow);
        {-Initialize a window from another one, making a unique copy of
          all dynamic structures}
      destructor Done; virtual;                                          {!!.01}
        {-Destroy child list and then window itself}                     {!!.01}

      procedure SetStackProc(SP : StackWindowProc);
        {-Set window stack proc}
      function WindowNumber : Word;
        {-Return window number. 0 if inactive, else 1 or greater}
      procedure AssignNumber; virtual;
        {-Called to assign window number}

      procedure Draw; virtual;
        {-Draw or update a window and push it on stack}
      procedure Erase; virtual;
        {-Erase a window and pop from stack}
      procedure Select; virtual;
        {-Make a window the current one by pulling to the top of stack}
      procedure UpdateContents; virtual;                                 {!!.01}
        {-Update the contents of any child windows}                      {!!.01}
      function IsCurrent : Boolean; virtual;                             {!!.01}
        {-Return True if window or its parent is current one}            {!!.01}

      procedure DrawHidden(var Under : StackWindow);
        {-Draw a window, inserting it in the stack underneath window Under}
      procedure EraseHidden;
        {-Erase a window even if it's hidden on the window stack}
      function OnStack : Boolean;
        {-Return True if window is displayed on the window stack}

      procedure ActivateWrite;
        {-Activate fastwriting to a hidden window}
      procedure DeactivateWrite;
        {-Deactivate fastwriting and display changes}

      {!!.01}
      {---- Child window routines ----}
      procedure AddChild(W : WindowPtr);
        {-Validate window W as child and add to child list}
      procedure RemoveChild(W : WindowPtr);                             {!!.03}
        {-Remove window W as child}                                     {!!.03}
      procedure SetActiveChild(W : WindowPtr);
        {-Make W the active child window. If W nil, Self becomes active}
      function ActiveChild : WindowPtr;
        {-Return the address of the current child window, nil if none}
      function IsChild : Boolean;
        {-Return True if window is a child of another}
      function IsParent : Boolean;
        {-Return True if window is a parent of at least one other}
      function ParentPtr : WindowPtr;
        {-Return address of parent window, nil if none}
      function ChildListPtr : SingleListPtr;
        {-Return the address of the list of child windows}
      function ChildIndex(W : WindowPtr) : Word;
        {-Return the index of child W. 0=Self, 1=first child, $FFFF=no match}
      function ChildPtr(Index : Word) : WindowPtr;
        {-Return the address of the child with specified Index, nil if none}

      {.Z+}
      {--- override RawWindow routines to update hidden windows ---}
      procedure Clear; virtual;
        {-Clear the window to wTextXXXX and wBackChar}
      procedure ClearContents;
        {-Clear region within active frame with wTextXXXX and wBackChar}
      procedure SaveContents; virtual;
        {-Save current window contents in the contents buffer}

      {$IFDEF UseAdjustableWindows}
      procedure AdjustWindow(X1, Y1, X2, Y2 : Word); virtual;
        {-Set new coordinates and adjust all related structures}
      procedure MoveWindow(DX, DY : Integer); virtual;
        {-Move a window}
      procedure ResizeWindow(DX, DY : Integer); virtual;
        {-Resize a window}
      procedure Zoom; virtual;
        {-Increase window size to specified limits temporarily}
      procedure Unzoom; virtual;
        {-Return zoomed window to original size}
      {$ENDIF}

      procedure ScrollHoriz(Cols : Integer); virtual;
        {-Scroll contents horizontally}
      procedure ScrollVert(Rows : Integer); virtual;
        {-Scroll contents vertically}

      {$IFDEF UseScrollBars}
      procedure DrawAllSliders(HorizVal, VertVal : LongInt); virtual;
        {-Draw or update all sliders}
      procedure ChangeAllScrollBars(MinHoriz, MaxHoriz : LongInt;
                                    MinVert, MaxVert : LongInt); virtual;
        {-Change user ranges of existing scroll bars. Erases sliders}
      {$ENDIF}

      {--- override RawWindow routines to write to hidden windows ---}
      procedure wFastWrite(St : string; Row, Col, Attr : Byte);
        {-Write a string using window-relative coordinates}
      procedure wFastText(St : string; Row, Col : Byte);
        {-Write St at Row,Col without changing the underlying video attribute.}
      procedure wFastVert(St : string; Row, Col, Attr : Byte);
        {-Write a string vertically using window-relative coordinates}
      procedure wFastFill(Number : Word; Ch : Char; Row, Col, Attr : Byte);
        {-Fill Number chs at Row,Col in Attr without snow}
      procedure wFastCenter(St : string; Row, Attr : Byte);
        {-Write St centered on Row in Attr without snow}
      procedure wFastFlush(St : string; Row, Attr : Byte);
        {-Write St flush right on Row in Attr without snow}
      procedure wFastRead(Number, Row, Col : Byte; var St : string);
        {-Read Number characters from the screen into St starting at Row,Col}
      procedure wReadAttribute(Number, Row, Col : Byte; var St : string);
        {-Read Number attributes from the screen into St starting at Row,Col}
      procedure wWriteAttribute(St : String; Row, Col : Byte);
        {-Write string of attributes St at Row,Col without changing characters}
      procedure wChangeAttribute(Number : Word; Row, Col, Attr : Byte);
        {-Change Number video attributes to Attr starting at Row,Col}
      procedure wFlexWrite(St : string; Row, Col : Byte; var FAttrs : FlexAttrs);
        {-Write a string flexibly using window-relative coordinates}
      procedure wFastWriteCtrl(St : String; Row, Col, Attr, Ctrl : Byte);
        {-Write St at Row,Col in Attr (video attribute) without snow.
          Control characters displayed in Ctrl as upper-case letters}

      procedure fFastWrite(St : string; Row, Col, Attr : Byte);
        {-Write a string}
      procedure fFastText(St : string; Row, Col : Byte);
        {-Write St at Row,Col without changing the underlying video attribute}
      procedure fFastVert(St : string; Row, Col, Attr : Byte);
        {-Write a string vertically}
      procedure fFastFill(Number : Word; Ch : Char; Row, Col, Attr : Byte);
        {-Fill Number chars at Row,Col in Attr}
      procedure fChangeAttribute(Number : Word; Row, Col, Attr : Byte);
        {-Change Number video attributes to Attr starting at Row,Col}
      procedure fFastCenter(St : string; Row, Attr : Byte);
        {-Write St centered on Row in Attr}
      procedure fFastFlush(St : string; Row, Attr : Byte);
        {-Write St flush right on Row in Attr}
      procedure fFastRead(Number, Row, Col : Byte; var St : string);
        {-Read Number characters from the screen into St starting at Row,Col}
      procedure fReadAttribute(Number, Row, Col : Byte; var St : string);
        {-Read Number attributes from the screen into St starting at Row,Col}
      procedure fWriteAttribute(St : String; Row, Col : Byte);
        {-Write string of attributes St at Row,Col without changing characters}
      procedure fFlexWrite(St : string; Row, Col : Byte;
                           var FAttrs : FlexAttrs);
        {-Write a string flexibly using window-relative coordinates}
      procedure fFastWriteCtrl(St : String; Row, Col, Attr, Ctrl : Byte);
        {-Write St at Row,Col in Attr (video attribute) without snow.
          Control characters displayed in Ctrl as upper-case letters}

      function ActiveFramePtr : AbstractFramePtr; virtual; {!!.13}
        {-Return a pointer to frame the window is currently displaying}
      {.Z-}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      procedure swEraseChildWindows; {!!.01}
      procedure swLinkToParent; {!!.01}
      {$IFDEF UseAdjustableWindows}
      function swCheckChildFit(X1, Y1, X2, Y2 : Word) : Boolean; {!!.01}
      function swMoveChildWindows(DX, DY : Integer) : Boolean; {!!.01}
      {$ENDIF}
      function swPrepWrite : Boolean; {!!.01}
      procedure swUnprepWrite; {!!.01}
      procedure swSelectTiled; {!!.03}
      procedure swActivateWriteTiled; {!!.03}
      procedure swDeactivateWriteTiled; {!!.03}
      {.Z-}
    end;

  StackWindowListNodePtr = ^StackWindowListNode;
  StackWindowListNode =
    object(SingleListNode)
      swPtr : WindowPtr;
      {.Z+}
      constructor Init(W : WindowPtr);
      destructor Done; virtual;
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
    {$ENDIF}
      {.Z-}
    end;

  ByteSet = set of Byte;
  ByteSetPtr = ^ByteSet;

  CommandWindowPtr = ^CommandWindow;
  CommandWindow =
    object(StackWindow)
      wHelpIndex    : SmallWord;  {Help topic number for this window}
      cwCmd         : Word;       {Last command entered by user}
      cwKey         : Word;       {Last key entered by user}
      cwCmdPtr      : CommandProcessorPtr; {Command processor}
      cwErrorProc   : ErrorProc;  {Routine to display status/error messages}
      cwError       : Word;       {Last error code}
      cwUnitCode    : Byte;       {Unit code}
      cwExitSetPtr  : ByteSetPtr; {Pointer to set of special exit commands}
      cwLastDownHs  : Byte;       {Hot spot for last ccMouseDown} {!!.03}
      cwLastDownFp  : FramePosType; {FramePos for last ccMouseDown} {!!.13}

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       var CP : CommandProcessor;
                       UnitCode : Byte);
        {-Create a command window with default options and colors}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             var CP : CommandProcessor;
                             UnitCode : Byte);
        {-Create a command window with custom options}

      procedure ProcessSelf; virtual;                                {!!.01}
        {-Process commands}
      procedure Process; virtual;
        {-Process commands of Self or active child}                  {!!.01}
      procedure ProcessAutoPilot(var CmdList; NumCmds : Word);
        {-Process commands, starting with those in CmdList}

      procedure GetNextCommand; virtual;
        {-Called by Process to get next command from user}
      procedure RequestHelp(HelpIndex : Word); virtual;
        {-Called by Process to request context-sensitive help}

      procedure SetCommandProcessor(var CP : CommandProcessor);
        {-Set command processor to use}
      function GetCommandProcessor : CommandProcessorPtr;
        {-Return pointer to window's command processor}
      function GetLastCommand : Word;
        {-Return last command entered by user}
      procedure SetLastCommand(Cmd : Word);
        {-Change last command}
      function GetLastKey : Word;
        {-Return last keystroke entered by user}

      procedure SetHelpIndex(Index : Word);
        {-Set help topic number for this window}
      function GetHelpIndex : Word;
        {-Return the help topic number for this window}

      procedure SetErrorProc(EP : ErrorProc);
        {-Set error handler}
      procedure GotError(ErrorCode : Word; ErrorMsg : string); virtual; {!!.12}
        {-To be called when an error occurs}
      function GetLastError : Word; virtual;
        {-Return last CommandWindow error code}
      function PeekLastError : Word; virtual;
        {-Return last CommandWindow error code without resetting internal var}
      function ClassifyError(ErrorCode : Word) : Byte;
        {-Return the class of the specified error code}
      procedure ClearErrors; virtual; {!!.01}
        {-Clear all pending errors}

      procedure SetExitCommands(ExitSetPtr : ByteSetPtr);
        {-Specify set of special exit commands}
      function GetExitCommandPtr : ByteSetPtr;
        {-Return pointer to set of special exit commands}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      function cwGetLastError : Word;
      function cwActiveCwPtr : CommandWindowPtr; {!!.01}
      procedure Error(Code : Word); virtual;
    {$IFDEF UseMouse}                                                   {!!.10}
      function cwMouseResults(Cmd : Word;                               {!!.13}
                              var FramePos : FramePosType;              {!!.03}
                              var HotCode : Byte) : LongInt;
      procedure cwSetLastDown(FramePos : FramePosType; HotCode : Byte); {!!.03}
      function cwIgnoreMouseEvent(var Dragging : Boolean;               {!!.03}
                                  Cmd : Word; FramePos : FramePosType;
                                  HotCode : Byte) : Boolean;
    {$ENDIF}                                                            {!!.10}
      {.Z-}
    end;

  VirtScreenPtr = ^VirtScreen;
  VirtScreen =
    object(Root)                  {A virtual screen}
      vsWidth       : SmallWord;  {Width of virtual screen}
      vsHeight      : SmallWord;  {Height of virtual screen}
      vsVideoSeg    : Word;       {Segment for this virtual screen}
      vsSize        : SmallWord;  {Usable bytes in buffer}
      vsBufPtr      : Pointer;    {Pointer to buffer}
      vsAllocated   : Boolean;    {True if VirtScreen allocated buffer}
      vsSaveSeg     : Word;       {Virtual segment when activated}
      vsSaveWidth   : SmallWord;  {Virtual width when activated}
      vsSaveHeight  : SmallWord;  {Virtual height when activated}

      constructor Alloc(Height, Width : Word);
        {-Create a virtual screen of the specified dimensions}
      constructor Init(Height, Width : Word; BufSeg : Word);
        {-Initialize a virtual screen with a preallocated buffer}
      destructor Done; virtual;
        {-Deallocate a virtual screen}

      procedure Activate;
        {-Activate fast write and read routines on virtual screen}
      procedure Deactivate;
        {-Deactivate fast access to virtual screen}

      procedure Clear(Attr : Byte; Fill : Char);
        {-Clear virtual screen}
      procedure WriteTo(S : string; Row, Col : Word; Attr : Byte);
        {-Write a string to virtual screen}
      procedure ReadFrom(Number : Byte; Row, Col : Word; var S : string);
        {-Read number characters from the virtual screen starting at Row,Col}
      procedure ScrollHoriz(Cols : Integer; Attr : Byte; Fill : Char);
        {-Scroll contents horizontally}
      procedure ScrollVert(Rows : Integer; Attr : Byte; Fill : Char);
        {-Scroll contents vertically}

      procedure CopyFromWindow(Row, Col : Word);
        {-Copy current window to position <Row,Col> on virtual screen}
      procedure CopyToWindow(Row, Col : Word);
        {-Copy from position <Row,Col> on virtual screen to current window}
      procedure CopyFromScreen(Row, Col : Word);
        {-Copy physical screen to position <Row,Col> on virtual screen}
      procedure CopyToScreen(Row, Col : Word);
        {-Copy from position <Row,Col> on virtual screen to physical screen}

      function vRows : Word;
        {-Return number of rows in virtual screen}
      function vCols : Word;
        {-Return number of columns in virtual screen}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      procedure vsZeroOut;
      {.Z-}
    end;

  WindowStack =
    object(PointerStack)          {A stack of windows}
      wsScreen      : VirtScreen; {Virtual screen used for clean screen updates}
      wsLevel       : Word;       {Level to which windows temporarily popped}
      wsFlags       : Word;       {Window stack options}
      {following not stored in streams}                                      {!!.11}
      wsCurrPtr     : RawWindowPtr; {Current window temporarily deactivated} {!!.11}

      constructor Init(Max : Word);
        {-Allocate stack and initialize}
      destructor Done; virtual;
        {-Clean up and dispose}

      function UnstackTop : WindowPtr;
        {-Erase and unstack window at top of stack}
      function TopWindow : WindowPtr;
        {-Return a pointer to the window at top of stack}
      function EvaluateStackPos(X, Y : Byte) : WindowPtr;
        {-Evaluate position and return pointer to indicated window, if any}
      {$IFDEF UseMouse}
      function EvaluateStackMousePos : WindowPtr;
        {-Evaluate mouse position using EvaluatePos}
      {$ENDIF}
      procedure OptimizeTiled(IsOn : Boolean); {!!.03}
        {-Turn tiled window optimization on or off}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream. Stores all currently stacked windows}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      function wsFindWindow(WPtr : WindowPtr; var Covered : Boolean) : Word; {!!.03}
      function wsActivate : Boolean;
      procedure wsDeactivate;
      function wsPopWindows(Level : Word; var BaseCursorSL : Word) : Boolean; {!!.01}
      function wsFreeNumber : Word;
      {.Z-}
    end;

  LoadableColorSetPtr = ^LoadableColorSet;
  LoadableColorSet =
    object(Root)
      lcsColors      : ColorSet;

      constructor Init(var Colors : ColorSet);
        {-Initialize this color set from another one}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a color set from a stream}
      procedure Store(var S : IdStream);
        {-Store a color set in a stream}
    {$ENDIF}
    end;

  PackRec =                  {!!do not change!!}
    record
      Link    : Word;   {number of bytes to next PackRec}
      FillVal : Byte;        {fill value}
      FillCnt : Word;   {fill count}
    end;
  PackedScreen = array[1..4000] of Byte; {dummy--actual size varies}
  PackedWindowPtr = ^PackedWindow;
  PackedWindow =             {!!do not change!!}
    object(Root)
      pwDealloc : Boolean;       {packed screen needs to be deallocated}
      pwSize    : Word;          {size of the packed screen}
      pwTopRow  : Byte;          {coordinates for top left corner of window}
      pwTopCol  : Byte;
      pwRows    : Byte;          {height of window}
      pwCols    : Byte;          {width of window}
      pwAStart  : Word;          {index to start of attributes section in Contents}
      pwCDelta  : Word;          {bytes before first PackRec - chars}
      pwADelta  : Word;          {bytes before first PackRec - attrs}
      pwScreen  : ^PackedScreen; {the contents of the packed screen}

      constructor Init(XL, YL, XH, YH : Byte);
        {-Create a packed window}
      constructor InitFromMemory(P : Pointer);
        {-Initialize a packed window from one stored to disk with Write
          and bound into the EXE file with BINOBJ}
      destructor Done; virtual;
        {-Deallocate the packed screen}
      {...}
      constructor Read(FName : string);
        {-Read the packed window stored in FName into memory}
      function Write(FName : string) : Word;
        {-Store the packed window in FName}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a packed window from a stream}
      procedure Store(var S : IdStream);
        {-Store a packed window in a stream}
    {$ENDIF}
      procedure Display;
        {-Display the packed window}
      procedure DisplayAt(Row, Col : Word);
        {-Display the packed window at Row,Col. If necessary, the coordinates
          are adjusted to allow it to fit on the screen.}
      procedure MapColors;
        {-Map the colors of the packed window for improved appearance on
          mono/B&W displays}

      {+++ internal methods +++}
      {.Z+}
      procedure pwDisplay(Row, Col : Word);
      {.Z-}
    end;


  {----------- Default options and convenient constants -----------}
const
  DefCursor         : CursorType = cuNormal; {Cursor type}
const
  DefBackChar       : Char = ' ';  {Background character}

  {----------------- Initialized variables ------------------}
const
  CurrentWindow     : RawWindowPtr = {Pointer to current window}
    nil;
  wStack            : WindowStack =  {Stack of active windows}
    (psTop:0; psSize:0; psStatus:0; psBase:nil;
     wsScreen:(vsWidth:0; vsHeight:0;vsVideoSeg:0;
               vsSize:0; vsBufPtr:nil; vsAllocated:False);
     wsLevel:0;
     wsFlags:wsOptimizeTile); {!!.03}

  {-------------- Exploding window routines ----------------}

  {.Z+}
  procedure ExplodeOpenProc(WPtr : RawWindowPtr);
    {-Explode a window frame}

  procedure ExplodeCloseProc(WPtr : RawWindowPtr);
    {-Implode a window frame and restore screen contents}

  procedure EmptyOpenCloseProc(WPtr : RawWindowPtr);
    {-Default open/close procedure, does nothing}
  {.Z-}

{$IFDEF UseStreams}

  {-------------- Stream registration routines -------------}

  procedure RawWindowStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing raw windows}

  procedure ExplodingWindowStream(SPtr : IdStreamPtr);
    {-Register procedures used for exploding windows}

  procedure StackWindowStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing stacked windows}

  procedure CommandWindowStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing command windows}

  procedure WindowStackStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing window stacks}

  procedure VirtScreenStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing virtual screens}

  procedure LoadableColorSetStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing loadable color sets}

  procedure PackedWindowStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing packed windows}

{$ENDIF}

  {=========================================================}

implementation

  {$I OPWINDOW.IN1}
  {$I OPWINDOW.IN2}
  {$I OPWINDOW.IN3}
  {$I OPWINDOW.IN4} {!!.13}

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.

