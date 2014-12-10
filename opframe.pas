{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPFRAME.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpFrame;
  {-Objects for managing window frames}


interface

uses
  Use32,
  OpConst,    {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd;

const
  {-------------- Header flags -----------------}
  heDisabled        = $01;        {Set when header is disabled (not drawn)}

  {$IFDEF UseScrollBars}
  {------------ Scroll bar flags ---------------}
  sbVertical        = $01;        {Set for a vertical scroll bar}
  sbVisible         = $02;        {Set when slider is visible}
  sbUserInit        = $04;        {Set when user value is initialized}
  {$ENDIF}

  {------------ Hot spot position codes --------}
  hsNone            = 0;          {Not a hot spot}
  hsDecV            = 1;          {Decrement fixture of a vertical scroll bar}
  hsIncV            = 2;          {Increment fixture of a vertical scroll bar}
  hsDecH            = 3;          {Decrement fixture of a horizontal scroll bar}
  hsIncH            = 4;          {Increment fixture of a horizontal scroll bar}
  hsBar             = 5;          {Slider portion of a horizontal scroll bar}
  hsSpot            = 6;          {Single character hot spot}
  hsRegion0         = 7;          {User-defined region relative to a frame}
  hsRegion1         = 8;
  hsRegion2         = 9;
  hsRegion3         = 10;
  hsRegion4         = 11;
  hsRegion5         = 12;
  hsRegion6         = 13;
  hsRegion7         = 14;
  hsRegion8         = 15;
  hsRegion9         = 16;
  hsOutsideActive   = 254;        {Used internally} {!!.03}
  hsReservedForTP   = 255;        {Used internally}

  {---- Constants used as special synonyms ----}
const
  NoFrameChar       = #255;       {Character indicates no frame in a FrameArray}
  SeeThruChar       = #255;       {Character indicates see-thru custom shadow}

  {---- Convenient window frame constants ----}
const
  DefWindowFrame    : FrameArray = 'ÕÔ¸¾ÍÍ³³';
const
  DblWindowFrame    : FrameArray = 'ÉÈ»¼ÍÍºº';
const
  SglWindowFrame    : FrameArray = 'ÚÀ¿ÙÄÄ³³';
const
  NoWindowFrame     : FrameArray = NoFrameChar+NoFrameChar+
                                   NoFrameChar+NoFrameChar+
                                   NoFrameChar+NoFrameChar+
                                   NoFrameChar+NoFrameChar;

  {---- Arrow arrays used in drawing scroll bars ----}
  {$IFDEF UseScrollBars}
type
  ArrowDirections   = (arrowLeft, arrowRight, arrowUp, arrowDown);
  ArrowArray        = array[ArrowDirections] of Char;
const
  DefArrows         : ArrowArray = #27#26#24#25; {Left-Right-Up-Down arrows}
const
  TriangleArrows    : ArrowArray = #17#16#30#31; {Alternate, using triangles}

const
  {---- Characters used for drawing scroll bars ----}
  DefSliderChar     : Char = #178; {Character for slider           : ²}
  DefBarChar        : Char = #176; {Character for scroll bar       : °}
  {$ENDIF}

  {$IFDEF UseShadows}
  {---- Characters used for drawing opaque shadows ----}
const
  DefShadowCharH    : Char = #223; {Character for horizontal shadow: ß}
  DefShadowCharV    : Char = #219; {Character for vertical shadow  : Û}
  {$ENDIF}

type
  AbstractFramePtr  = ^AbstractFrame;
  AbstractFrame     =             {Abstract type for extensibility}
    object(Root)
      frXL          : Word;            {Absolute screen coordinates}
      frYL          : Word;
      frXH          : Word;
      frYH          : Word;

      {$IFDEF UseAdjustableWindows}
      frMinW        : Word;            {Min width and height (for resizing)}
      frMinH        : Word;
      frMaxW        : Word;            {Max width and height}
      frMaxH        : Word;
      {$ENDIF}

      frMinXL       : Word;            {Min coordinates (for clipping)}
      frMinYL       : Word;
      frMaxXH       : Word;            {Max coordinates}
      frMaxYH       : Word;

      {.Z+}
      constructor Init(X1, Y1, X2, Y2 : Word);
        {-Initialize coordinates}
      destructor Done; virtual;
        {-Deallocate frame}

      procedure SetFrameAttr(Color, Mono : Byte); virtual;
        {-Set attributes for frame border}

      {$IFDEF UseAdjustableWindows}
      procedure SetSizeLimits(MinW, MinH, MaxW, MaxH : Word);
        {-Set limits for sizing of frame}
      {$ENDIF}
      procedure SetClipLimits(MinXL, MinYL, MaxXH, MaxYH : Word);
        {-Set limits for drawing frame (shadows get clipped)}

      procedure Coordinates(var X1, Y1, X2, Y2 : Word);
        {-Return the coordinates of the frame}
      procedure WithinFrameCoords(var X1, Y1, X2, Y2 : Word);
        {-Return the coordinates of region within, not including, the border}

      procedure Error(Code : Word); virtual;
        {-Report that an error occurred}
      function GetLastError : Word; virtual;
        {-Return and clear the last error code, 0 if none}
      function PeekLastError : Word; virtual;
        {-Return the last error code, 0 if none}
      {.Z-}
      procedure ClearErrors;
        {-Clear all pending errors}

      {.Z+}
      procedure Draw; virtual;
        {-Draw the frame}
      {$IFDEF UseAdjustableWindows}
      procedure AdjustFrame(X1, Y1, X2, Y2 : Word);
        {-Set new coordinates}
      {$ENDIF}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}
      {.Z-}
    end;


  ScreenBuffer      = array[0..65520] of Byte;
  ScreenBufferPtr   = ^ScreenBuffer;
  ScreenRect        =
    object(Root)
      srXL          : Byte;       {Screen coordinates}
      srYL          : Byte;
      srXH          : Byte;
      srYH          : Byte;
      srContents    : Boolean;    {True if Store and Load write contents}
      srAllocated   : Boolean;    {True if ScreenRect allocated buffer}
      srBufPtr      : ScreenBufferPtr; {Pointer to screen buffer}
      srRes         : Word;       {Error code}

      constructor Alloc(X1, Y1, X2, Y2 : Byte);
        {-Initialize coordinates and allocate a screen buffer}
      constructor Init(X1, Y1, X2, Y2 : Byte; BufPtr : Pointer);
        {-Initialize coordinates and assign a preallocated buffer}
      destructor Done; virtual;
        {-Deallocate buffer}
      function BufSize : Word;
        {-Return required size of buffer in bytes}
      procedure Clear(FChar : Char; FAttr : Byte);
        {-Clear with character and attribute}
      procedure CopyFromScreen;
        {-Copy from screen to buffer}
      procedure CopyToScreen;
        {-Copy buffer to screen}
      procedure CopyPartToScreen(X1, Y1, X2, Y2 : Byte);
        {-Copy part of the buffer to the screen}
      procedure Adjust(X1, Y1, X2, Y2 : Byte);
        {-Adjust buffer coordinates to X1, Y1, X2, Y2}
      procedure Transfer(X1, Y1, X2, Y2 : Byte;
                         FChar : Char; FAttr : Byte);
        {-Transfer buffer to new buffer at coordinates X1, Y1, X2, Y2}
      function srResult : Word;
        {-Return most recent status code}
      procedure StoreContents(IsOn : Boolean);
        {-Specify whether Store writes contents of screen buffer}
      function HaveContents : Boolean;
        {-Return True if contents were loaded by Load}
      procedure Coordinates(var X1, Y1, X2, Y2 : Byte);
        {-Return coordinates of screen rectangle}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      procedure srZeroOut;
    {$IFDEF UseStreams}
      procedure srUnPack1(var S : IdStream; OPos : Word);
      procedure srUnpack(var S : IdStream);
      procedure srPack1(var S : IdStream; IPos : Word);
      procedure srPack(var S : IdStream);
    {$ENDIF}
      {.Z-}
    end;


const
  frInsideActive = 8;             {Continues sequence started in OPCRT}
  frInsideFrame  = 9;
  frOutsideFrame = 10;

type
  FramePosType      = frTL..frOutsideFrame;
  FrameCornerType   = frTL..frBR;
  FrameEdgeType     = frTT..frRR;

  HeaderPosType     = (heCustom, heSpan, heTL, heTC, heTR, heBL, heBC, heBR);
  HeaderPtr         = ^HeaderNode;
  HeaderNodePtr     = ^HeaderNode;
  HeaderNode        =
    object(DoubleListNode)
      heName        : StringPtr;  {Text of header}
      heMaxLen      : Byte;       {Capacity of header}
      heAttrColor   : Byte;       {Attribute - color}
      heAttrMono    : Byte;       {Attribute - mono}
      heFlags       : Byte;       {Options - header disabled}
      heDX          : SmallInt;    {Frame-relative coordinates for header}
      heDY          : SmallInt;
      heType        : HeaderPosType; {Type of header, used when resizing}
      hePosn        : FrameCharType; {Portion of frame header is relative to}
      heDummy       : record end; {Makes Load and Store cleaner}
      {.Z+}
      constructor Init(Name : String; AttrColor, AttrMono : Byte;
                       DX, DY : Integer;
                       hType : HeaderPosType;
                       hPosn : FrameCharType);
        {-Initialize header node}
      destructor Done; virtual;
        {-Deallocate header node}
      procedure ChangeAttr(Color, Mono : Byte);
        {-Change attribute of an existing header}
      function Modify(Name : String; DX, DY : Integer) : Boolean; virtual; {!!.12}
        {-Change name and position of existing header}
      procedure Draw(XL, YL, XH, YH : Byte; Framed : Boolean); virtual; {!!.01}
        {-Draw one header node}
      procedure Coordinates(XL, YL, XH, YH : Byte;
                            var heXL, heYL, heXH, heYH : Byte); virtual; {!!.01}
        {-Return the absolute coordinates of a rectangle surrounding header}
      procedure Update(XL, YL, XH, YH : Byte; Framed : Boolean); virtual; {!!.01}
        {-Adjust internal coords based on given frame coords}
      procedure Disable(IsOn : Boolean);
        {-Disable or reenable display of header}
      function Disabled : Boolean;
        {-Return True if header is disabled}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}
      {.Z-}
    end;


  {$IFDEF UseShadows}
  ShadowPtr         = ^ShadowNode;
  ShadowNodePtr     = ^ShadowNode;
  ShadowNode        =
    object(DoubleListNode)
      shChar        : Char;       {Character for shadow}
      shAttrColor   : Byte;       {Attribute - color}
      shAttrMono    : Byte;       {Attribute - mono}
      shPosn        : FrameEdgeType; {Edge of frame shadow is relative to}
      shDX          : SmallInt;    {Coordinates relative to specified edge}
      shDY          : SmallInt;
      shWidth       : Byte;       {Size of shadow}
      shHeight      : Byte;
      shBufPtr      : Pointer;    {Pointer to covers buffer}
      {.Z+}
      constructor Init(ShadChar : Char;
                       AttrColor, AttrMono : Byte;
                       Posn : FrameEdgeType;
                       DX, DY : Integer;
                       Width, Height : Byte);
        {-Initialize shadow node}
      destructor Done; virtual;
        {-Deallocate shadow node}
      procedure Draw(XL, YL, XH, YH : Byte;
                     minXL, minYL, maxXH, maxYH : Byte);
        {-Draw one shadow node}
      procedure Erase(XL, YL, XH, YH : Byte;
                      minXL, minYL, maxXH, maxYH : Byte);
        {-Erase one shadow node}
      procedure Coordinates(XL, YL, XH, YH : Byte;
                            minXL, minYL, maxXH, maxYH : Byte;
                            var shXL, shYL, shXH, shYH : Byte);
        {-Return the absolute coordinates of the shadow, clipping as needed}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}
      {.Z-}
    end;
  ShadowDrawType    = (shSeeThru, shOverWrite, shNone);
  ShadowPosType     = (shBR, shBL);
  {$ENDIF}

  {$IFDEF UseHotSpots}
  HotSpotPtr        = ^HotNode;
  HotNodePtr        = ^HotNode;
  HotNode =
    object(DoubleListNode)
      hnUser        : Byte;           {Code returned to user}
      hnWidth       : Byte;           {Size of hot rectangle}
      hnHeight      : Byte;
      hnDX          : SmallInt;        {Offset from that corner}
      hnDY          : SmallInt;
      hnType        : HeaderPosType;  {Type of header, used when resizing}
      hnPosn        : FrameCharType;  {Corner or edge of frame}
      {.Z+}
      constructor Init(hType : HeaderPosType;
                       Posn : FrameCharType;
                       DX, DY : Integer;
                       Width, Height : Byte;
                       User : Byte);
        {-Initialize hot node}
      destructor Done; virtual;
        {-Deallocate hot node}
      procedure Modify(DX, DY : Integer; Width, Height : Byte);
        {-Change size or position of hot spot}
      function Within(X, Y, X1, Y1, X2, Y2 : Byte; var User : Byte) : Boolean;
        {-Return True and User code if [X, Y] is within the hot spot}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}
      {.Z-}
    end;
  {$ENDIF}

  {$IFDEF UseScrollBars}
  ScrollBarPtr      = ^ScrollBar;
  ScrollBar =
    object(Root)
      sbFlags       : Byte;       {Vertical, Visible, etc.}
      sbFixPos      : Byte;       {Fixed absolute coordinate for bar}
      sbMinPos      : Byte;       {Minimum absolute coordinate for bar}
      sbMaxPos      : Byte;       {Maximum absolute coordinate for bar}
      sbCurPos      : Byte;       {Current absolute coordinate for slider}
      sbMinUser     : LongInt;    {Minimum user value}
      sbMaxUser     : LongInt;    {Maximum user value}
      sbCurUser     : LongInt;    {Current user value}
      sbSliChar     : Char;       {Character to use for slider}
      sbBarChar     : Char;       {Character to use for scroll bar}
      sbDecChar     : Char;       {Character to use for decrement arrow}
      sbIncChar     : Char;       {Character to use for increment arrow}
      sbSliColor    : Byte;       {Attribute for slider}
      sbSliMono     : Byte;
      sbBarColor    : Byte;       {Attribute for scroll bar}
      sbBarMono     : Byte;
      sbArrowColor  : Byte;       {Attribute for arrows}
      sbArrowMono   : Byte;
      {.Z+}
      constructor Init(Vertical : Boolean;
                       FixPos, MinPos, MaxPos : Byte;
                       MinUser, MaxUser : LongInt;
                       SliChar, BarChar, DecChar, IncChar : Char;
                       var Colors : ColorSet);
        {-Initialize a scroll bar}
      destructor Done; virtual;
        {-Dispose of scroll bar}
      procedure ChangeUserRange(MinUser, MaxUser : LongInt;
                                UpdateScreen : Boolean);
        {-Change the user range. Erases slider if UpdateScreen is True}
      procedure SetUserValue(UserVal : LongInt);
        {-Initialize the user value without drawing it}
      procedure Draw;
        {-Draw the scroll bar the first time, without slider}
      function UserValue(SliderPos : Byte) : LongInt;
        {-Convert slider position (sbMinPos+1..sbMaxPos-1) to User value}
      function SliderPosition(UserVal : LongInt) : Byte;
        {-Convert User value to slider position}
      procedure EraseSlider;
        {-Erase the current slider}
      procedure DrawSlider(UserVal : LongInt);
        {-Draw the slider, erasing the current one if needed}
      function SliderVisible : Boolean;
        {-Return True if the slider is visible}
      function SliderInitialized : Boolean;
        {-Return True if the user value is initialized}
      procedure ClearSlider;
        {-Mark the slider as not visible}
      procedure DrawCurrentSlider;
        {-Draw the current slider}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}

      {++++ for internal use ++++}
      procedure sbAdjust(dFixPos, dMinPos, dMaxPos : Integer);
      {.Z-}
    end;
  ScrollBarArray    = array[FrameEdgeType] of ScrollBarPtr;
  {$ENDIF}


  FramePtr          = ^Frame;
  Frame =
    object(AbstractFrame)
      frFrameColor  : Byte;       {Attr for frame - color}
      frFrameMono   : Byte;       {Attr for frame - mono}
      frHeaderColor : Byte;       {Attr for headers - color}
      frHeaderMono  : Byte;       {Attr for headers - mono}
      {$IFDEF UseShadows}
      frShadowColor : Byte;       {Attr for shadow - color}
      frShadowMono  : Byte;       {Attr for shadow - mono}
      {$ENDIF}
      frFramed      : Boolean;    {True if any FrameChars are not NoFrameChar}
      frChars       : FrameArray; {Frame characters}
      frRes         : Word;       {Error code}
      frHeaders     : DoubleList; {List of headers}
      {$IFDEF UseScrollBars}
      frBars        : ScrollBarArray; {Array of pointers to scroll bars}
      {$ENDIF}
      {$IFDEF UseShadows}
      frShadows     : DoubleList; {List of shadows}
      {$ENDIF}
      {$IFDEF UseHotSpots}
      frHotSpots    : DoubleList; {List of hot spots}
      {$ENDIF}

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       FA : FrameArray;
                       var Colors : ColorSet);
        {-Initialize frame coordinates and all fields}
      constructor fCopy(var F : Frame);
        {-Initialize a frame from another,
          making a unique copy of all dynamic structures}
      destructor Done; virtual;
        {-Destroy a frame}

      procedure SetFrameType(FA : FrameArray);
        {-Change the frame type}
      procedure SetFrameAttr(Color, Mono : Byte); virtual;
        {-Set attributes for frame characters}
      procedure SetHeaderAttr(Color, Mono : Byte; ChangeAll : Boolean);
        {-Set attributes for header characters}
      procedure SetSpanAttr(Color, Mono : Byte); {!!.22}
        {-Set attributes for all existing span headers}
      {$IFDEF UseShadows}
      procedure SetShadowAttr(Color, Mono : Byte; ChangeAll : Boolean);
        {-Set attributes for shadow characters}
      {$ENDIF}

      procedure AddHeader(S : string; Posn : HeaderPosType);
        {-Add a standard header}
      procedure AddHeaderColor(S : string; Posn : HeaderPosType;
                               AttrColor, AttrMono : Byte);
        {-Add a standard header with custom color}
      procedure AddCustomHeader(S : string; Posn : FrameCornerType;
                                DX, DY : Integer;
                                AttrColor, AttrMono : Byte);
        {-Add a custom header}
      procedure AddSpanHeader(FirstChar, SpanChar, LastChar : Char;
                              Dpos : Integer; Posn : FrameEdgeType);
        {-Add a header spanning the frame}
      procedure AddSpanHeaderColor(FirstChar, SpanChar, LastChar : Char;
                                   Dpos : Integer; Posn : FrameEdgeType;
                                   AttrColor, AttrMono : Byte);
        {-Add a header spanning the frame with custom color}
      procedure AddAnyHeader(HPtr : HeaderNodePtr); {!!.01}
        {-Add a header of any type derived from HeaderNode}

      function GetLastHeaderIndex : Byte;
        {-Return index of last header added}
      procedure ChangeHeaderString(Index : Byte; S : string;
                                   var Redraw : Boolean);
        {-Change existing header string and update position}
      procedure ChangeHeaderAttr(Index : Byte; Color, Mono : Byte);
        {-Change existing header colors}
      procedure DrawHeader(Index : Byte);
        {-Draw specified header}
      procedure DisableHeader(Index : Byte; IsOn : Boolean);
        {-Disable or enable specified header}

      {$IFDEF UseShadows}
      procedure AddShadow(Posn : ShadowPosType; DrawType : ShadowDrawType);
        {-Add a standard shadow}
      procedure AddShadowColor(Posn : ShadowPosType; DrawType : ShadowDrawType;
                               AttrColor, AttrMono : Byte);
        {-Add a standard shadow with custom color}
      procedure AddCustomShadow(ShChar : Char; Edge : FrameEdgeType;
                                DX, DY : Integer; W, H : Byte;
                                AttrColor, AttrMono : Byte);
        {-Add a custom shadow rectangle}
      {$ENDIF}

      {$IFDEF UseScrollBars}
      procedure AddScrollBar(Posn : FrameEdgeType;
                             MinUser, MaxUser : LongInt;
                             var Colors : ColorSet);
        {-Add a standard scroll bar}
      procedure AddCustomScrollBar(Posn : FrameEdgeType;
                                   MinUser, MaxUser : LongInt;
                                   DecOffset, IncOffset : Byte;
                                   SliChar, BarChar : Char;
                                   var Colors : ColorSet);
        {-Add a custom scroll bar}
      {$ENDIF}

      {$IFDEF UseHotSpots}
      procedure AddHotSpot(Posn : FrameCornerType;
                           HotChar : Char;
                           var Colors : ColorSet);
        {-Add a hot spot in a frame corner}
      procedure AddHotBar(Posn : FrameEdgeType; Code : Byte);
        {-Add a hot region covering an entire frame edge}
      procedure AddHotRegion(Posn : FrameCharType;
                             Code : Byte;
                             DX, DY : Integer;
                             Width, Height : Byte);
        {-Add a fixed size hot region relative to the frame corner}
      procedure AddHotHeader(hType : HeaderPosType; Code : Byte; Len : Byte);
        {-Add a hot region to coincide with a header}
      function GetLastHotIndex : Byte;
        {-Return index of last hot region}
      procedure ChangeHotHeader(Index : Byte; Len : Byte);
        {-Change the length (and position) of a hot header}
      {$ENDIF}

      {$IFDEF UseAdjustableWindows}
      procedure SetSizeLimits(MinW, MinH, MaxW, MaxH : Byte);
        {-Set limits for sizing of frame}
      {$ENDIF}
      procedure SetClipLimits(MinXL, MinYL, MaxXH, MaxYH : Byte);
        {-Set limits for drawing frame (shadows get clipped)}

      {$IFDEF UseScrollBars}
      procedure SetSliderValue(Posn : FrameEdgeType; UserVal : LongInt);
        {-Change the user value without drawing it}
      function CurUserValue(Posn : FrameEdgeType) : LongInt;
        {-Return the current user value for specified scroll bar}
      {$ENDIF}

      procedure Draw; virtual;
        {-Draw the frame, scrollbars, headers, and shadows}
      procedure UpdateFrame; virtual;
        {-Draw the frame, scrollbars, and headers (no shadows)}

      function IsFramed : Boolean;
        {-Return True if any of the FrameChars are not NoFrameChar}

      procedure Coordinates(var X1, Y1, X2, Y2 : Byte);
        {-Return the coordinates of the frame}
      procedure WithinFrameCoords(var X1, Y1, X2, Y2 : Byte);
        {-Return the coordinates of region within, not including, the border}

      {$IFDEF UseAdjustableWindows}
      procedure AdjustFrame(X1, Y1, X2, Y2 : Byte);
        {-Set new coordinates and adjust related structures}
      {$ENDIF}

      procedure Error(Code : Word); virtual;
        {-Report that an error occurred}
      function GetLastError : Word; virtual;
        {-Return and clear the last error code, 0 if none}
      function PeekLastError : Word; virtual;
        {-Return the last error code, 0 if none}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      constructor frZeroOut;
      constructor frInitFrom(var F : Frame);
      procedure frComputeSpanHeader(var S : string;
                                    FirstChar, SpanChar, LastChar : Char;
                                    DPos : Integer;
                                    Posn : FrameEdgeType;
                                    var DX, DY : Integer);
      function frFindHeader(Index : Byte) : HeaderPtr;
      procedure frChangeHeaderString(He : HeaderPtr; S : string;
                                     var Redraw : Boolean);
      procedure frGetColors(var Colors : ColorSet);
      {$IFDEF UseHotSpots}
      function frFindHotSpot(Index : Byte) : HotSpotPtr;
      {$ENDIF}
      procedure DrawBorder;
        {-Draw just the border part}
      procedure DrawHeaders;
        {-Draw just the header part}
      {$IFDEF UseShadows}
      procedure RemoveShadows;
        {-Remove shadows if any}
      procedure DrawShadows;
        {-Draw just the shadow part}
      procedure EraseShadows;
        {-Erase (restore) just the shadow part}
      {$ENDIF}
      {$IFDEF UseScrollBars}
      procedure DrawSlider(Posn : FrameEdgeType; UserVal : LongInt);
        {-Draw or update the slider}
      procedure EraseSlider(Posn : FrameEdgeType);
        {-Erase the slider}
      procedure ClearSlider(Posn : FrameEdgeType);
        {-Mark slider not visible}
      procedure DrawScrollBars;
        {-Draw just the scroll bars}
      procedure EraseSliders;
        {-Erase just the visible sliders}
      procedure ClearSliders;
        {-Mark all sliders as not visible}
      procedure ChangeScrollBar(Posn : FrameEdgeType;
                                MinUser, MaxUser : LongInt;
                                UpdateScreen : Boolean);
        {-Change user range of existing scroll bar}
      {$ENDIF}
      {.Z-}
    end;

  {---------------------------------------------------------}

{$IFDEF UseStreams}

procedure ScreenRectStream(SPtr : IdStreamPtr);
  {-Register types needed for streams containing ScreenRects}

procedure FrameStream(SPtr : IdStreamPtr);
  {-Register types needed for streams containing Frames}

{$ENDIF}

{.Z+}
procedure DoLower(var L : Byte; Max : Byte);
  {-Lower L to no more than Max}

procedure DoRaise(var H : Byte; Min : Byte);
  {-Raise H to no less than Min}
{.Z-}

  {=========================================================}

implementation

  {$I OPFRAME.IN1}                {Screen rect, AbstractFrame, ...}
  {$I OPFRAME.IN2}                {Frame}

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.


