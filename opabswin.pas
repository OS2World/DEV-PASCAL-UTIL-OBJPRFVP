
{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}


{*********************************************************}
{*                  OPABSWIN.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpAbsWin;
  {-Low level support for text windows}
  {-This code moved from OPWINDOW.PAS }


interface

uses
  Use32,
  OpConst,
  OpFrame,
  OpRoot;

type
  AbstractWindowPtr = ^AbstractWindow;
  AbstractWindow    =             {Abstract window type}
    object(Root)
      wXL           : Word;       {Active window coordinates}
      wXH           : Word;
      wYL           : Word;
      wYH           : Word;

      {$IFDEF UseAdjustableWindows}
      wMinW         : Word;            {Min width and height}
      wMinH         : Word;
      wMaxW         : Word;            {Max width and height}
      wMaxH         : Word;

      wMinXL        : Word;            {Min coordinates}
      wMinYL        : Word;
      wMaxXH        : Word;            {Max coordinates}
      wMaxYH        : Word;

      wzXL          : Word;            {Saved coordinates while zoomed}
      wzXH          : Word;            {All zero when not zoomed}
      wzYL          : Word;
      wzYH          : Word;
      {$ENDIF}

      {---- these methods are not overridden by RawWindow ----}
      function Width : Word;
        {-Return the active width of the window}
      function Height : Word;
        {-Return the active height of the window}
      procedure ClearErrors; virtual; {!!.01}
        {-Clear all pending errors}

      {.Z+}
      constructor Init(X1, Y1, X2, Y2 : Word);
        {-Initialize coordinates}
      destructor Done; virtual;
        {-Destroy window}

      {$IFDEF UseAdjustableWindows}
      procedure SetSizeLimits(MinW, MinH, MaxW, MaxH : Word);
        {-Set limits for sizing of active window}
      procedure SetPosLimits(MinX, MinY, MaxX, MaxY : Word);
        {-Set limits for moving and sizing}
      {$ENDIF}
      procedure Coordinates(var X1, Y1, X2, Y2 : Word);
        {-Return the active coordinates of the window}
      function InWindow(AX, AY : Word) : Boolean;
        {-Return true if specified absolute position is within window}
      function Intersects(W : AbstractWindowPtr) : Boolean; virtual; {!!.03}
        {-Return true if Self intersects another window W}

      procedure Draw; virtual;
        {-Draw or update window}
      procedure Clear; virtual;
        {-Clear window}
      procedure Erase; virtual;
        {-Erase window}
      procedure Select; virtual;
        {-Make a window the current one}

      procedure Process; virtual;
        {-Give keyboard control to window until it returns it}
      function GetLastCommand : Word;
        {-Return the command used to exit the Process method}
      procedure Error(Code : Word); virtual;
        {-Report that an error occurred}
      function GetLastError : Word; virtual;
        {-Return and clear the last error code, 0 if none}
      function PeekLastError : Word; virtual;
        {-Return last error code without resetting internal variable}

      {$IFDEF UseAdjustableWindows}
      procedure AdjustWindow(X1, Y1, X2, Y2 : Word); virtual;
        {-Set new coordinates and adjust all related structures}
      procedure MoveWindow(DX, DY : Integer); virtual;
        {-Move window}
      procedure ResizeWindow(DX, DY : Integer); virtual;
        {-Resize window}
      procedure Zoom; virtual;
        {-Increase window size to maximum limits temporarily}
      procedure Unzoom; virtual;
        {-Return zoomed window to pre-zoomed size}
      {$ENDIF}

      procedure SetTextAttr(Color, Mono : Byte); virtual;
        {-Set attributes for normal window text}
      procedure SetBackChar(BackChar : Char); virtual;
        {-Set background character used while clearing and scrolling}

      procedure wGotoXY(X, Y : Integer); virtual;
        {-Move cursor to window-relative coordinates}
      procedure wWhereXY(var X, Y : Integer); virtual;
        {-Return window-relative coordinates of cursor}

      function IsActive : Boolean; virtual;
        {-Return True if window is active}
      function IsCurrent : Boolean; virtual;
        {-Return True if window is current}
      {$IFDEF UseAdjustableWindows}
      function IsZoomed : Boolean; virtual;
        {-Return True if window is zoomed}
      {$ENDIF}

      function MainFramePtr : AbstractFramePtr; virtual;
        {-Return a pointer to frame used when window is current}
      function AltFramePtr : AbstractFramePtr; virtual;
        {-Return a pointer to frame used when window is active but not current}
      function ActiveFramePtr : AbstractFramePtr; virtual;
        {-Return a pointer to frame the window is currently displaying}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from stream}
      procedure Store(var S : IdStream);
        {-Store to stream}
    {$ENDIF}

      {---- hook methods, not usually called by instances ----}
      procedure UpdateContents; virtual;
        {-Called to update the window's contents}
      procedure MarkCurrent; virtual;
        {-Called when window is made current}
      procedure MarkNotCurrent; virtual;
        {-Called when window is deselected}
      {.Z-}
    end;

implementation

{----------------- Abstract window routines -------------------}

  constructor AbstractWindow.Init(X1, Y1, X2, Y2 : Word);
    {-Initialize coordinates}
  begin
    if not Root.Init then
      Fail;

    if (X1 > X2) or (Y1 > Y2) then begin    {!!.02} {!!.03}
      InitStatus := epFatal+ecWinCoordsBad;         {!!.03}
      Fail;                                 {!!.02} {!!.03}
    end;

    wXL := X1;
    wXH := X2;
    wYL := Y1;
    wYH := Y2;

    {$IFDEF UseAdjustableWindows}
    wMinW := 1;
    wMinH := 1;
    wMaxW := $FFFF;
    wMaxH := $FFFF;

    wMinXL := 1;
    wMinYL := 1;
    wMaxXH := $FFFF;
    wMaxYH := $FFFF;

    wzXL := 0;
    wzXH := 0;
    wzYL := 0;
    wzYH := 0;
    {$ENDIF}
  end;

  destructor AbstractWindow.Done;
    {-Destroy window}
  begin
    Root.Done;
  end;

  {$IFDEF UseAdjustableWindows}
  procedure AbstractWindow.SetSizeLimits(MinW, MinH, MaxW, MaxH : Word);
    {-Set limits for sizing of active window}
  begin
    wMinW := MinW;
    wMinH := MinH;
    wMaxW := MaxW;
    wMaxH := MaxH;
  end;

  procedure AbstractWindow.SetPosLimits(MinX, MinY, MaxX, MaxY : Word);
    {-Set limits for moving and sizing}
  begin
    wMinXL := MinX;
    wMinYL := MinY;
    wMaxXH := MaxX;
    wMaxYH := MaxY;
  end;
  {$ENDIF}

  procedure AbstractWindow.Coordinates(var X1, Y1, X2, Y2 : Word);
    {-Return the active coordinates of the window}
  begin
    X1 := wXL;
    X2 := wXH;
    Y1 := wYL;
    Y2 := wYH;
  end;

  function AbstractWindow.Width : Word;
    {-Return the active width of the window}
  begin
    Width := wXH-wXL+1;
  end;

  function AbstractWindow.Height : Word;
    {-Return the active height of the window}
  begin
    Height := wYH-wYL+1;
  end;

  function AbstractWindow.InWindow(AX, AY : Word) : Boolean;
    {-Return true if specified absolute position is within window}
  begin
    InWindow := (AX >= wXL) and (AX <= wXH) and (AY >= wYL) and (AY <= wYH);
  end;

  function AbstractWindow.Intersects(W : AbstractWindowPtr) : Boolean; {!!.03}
    {-Return true if Self intersects another window W}
  begin
    Intersects := (wXL <= W^.wXH) and (wXH >= W^.wXL) and
                  (wYL <= W^.wYH) and (wYH >= W^.wYL);
  end;

  procedure AbstractWindow.Draw;
    {-Draw or update window}
  begin
  end;

  procedure AbstractWindow.Clear;
    {-Clear window}
  begin
  end;

  procedure AbstractWindow.Erase;
    {-Erase window}
  begin
  end;

  procedure AbstractWindow.Select;
    {-Make a window the current one}
  begin
    Abstract;
  end;

  procedure AbstractWindow.Process;
    {-Give keyboard control to window until it returns it}
  begin
  end;

  function AbstractWindow.GetLastCommand : Word;
    {-Return the command used to exit the Process method}
  begin
    GetLastCommand := 0;
  end;

  procedure AbstractWindow.Error(Code : Word);
    {-Report that an error occurred}
  begin
    RunError(Code);
  end;

  function AbstractWindow.GetLastError : Word;
    {-Return and clear the last error code, 0 if none}
  begin
    GetLastError := 0;
  end;

  function AbstractWindow.PeekLastError : Word;
    {-Return last error code, 0 if none}
  begin
    PeekLastError := 0;
  end;

  procedure AbstractWindow.ClearErrors;
    {-Clear all pending errors}
  begin
    while GetLastError <> 0 do ;
  end;

  {$IFDEF UseAdjustableWindows}
  procedure AbstractWindow.AdjustWindow(X1, Y1, X2, Y2 : Word);
    {-Set new coordinates and adjust all related structures}
  begin
    if not IsZoomed then begin
      wXL := X1;
      wXH := X2;
      wYL := Y1;
      wYH := Y2;
    end;
  end;

  procedure AbstractWindow.MoveWindow(DX, DY : Integer);
    {-Move window}
  begin
    AdjustWindow(wXL+DX, wYL+DY, wXH+DX, wYH+DY);
  end;

  procedure AbstractWindow.ResizeWindow(DX, DY : Integer);
    {-Resize window}
  begin
    AdjustWindow(wXL, wYL, wXH+DX, wYH+DY);
  end;

  procedure AbstractWindow.Zoom;
    {-Increase window size to maximum limits temporarily}
  begin
    if not IsZoomed then begin
      Coordinates(wzXL, wzXH, wzYL, wzYH);
      AdjustWindow(wMinXL, wMinYL, wMaxXH, wMaxYH);
    end;
  end;

  procedure AbstractWindow.Unzoom;
    {-Return zoomed window to pre-zoomed size}
  begin
    if IsZoomed then begin
      AdjustWindow(wzXL, wzXH, wzYL, wzYH);
      wzXL := 0;
      wzXH := 0;
      wzYL := 0;
      wzYH := 0;
    end;
  end;
  {$ENDIF}

  procedure AbstractWindow.SetTextAttr(Color, Mono : Byte);
    {-Set attributes for normal window text}
  begin
  end;

  procedure AbstractWindow.SetBackChar(BackChar : Char);
    {-Set background character used while clearing and scrolling}
  begin
  end;

  procedure AbstractWindow.wGotoXY(X, Y : Integer);
    {-Move cursor to window-relative coordinates}
  begin
  end;

  procedure AbstractWindow.wWhereXY(var X, Y : Integer);
    {-Return window-relative coordinates of cursor}
  begin
  end;

  function AbstractWindow.IsCurrent : Boolean;
    {-Return True if window is current}
  begin
    Abstract;
  end;

  function AbstractWindow.IsActive : Boolean;
    {-Return True if window is active}
  begin
    Abstract;
  end;

  {$IFDEF UseAdjustableWindows}
  function AbstractWindow.IsZoomed : Boolean;
    {-Return True if window is zoomed}
  begin
    IsZoomed := (wzXL <> 0);
  end;
  {$ENDIF}

  function AbstractWindow.MainFramePtr : AbstractFramePtr;
    {-Return a pointer to frame used when window is current}
  begin
    MainFramePtr := nil;
  end;

  function AbstractWindow.AltFramePtr : AbstractFramePtr;
    {-Return a pointer to frame used when window is active but not current}
  begin
    AltFramePtr := nil;
  end;

  function AbstractWindow.ActiveFramePtr : AbstractFramePtr;
    {-Return a pointer to frame the window is currently displaying,
      nil if none}
  begin
    ActiveFramePtr := nil;
  end;

  procedure AbstractWindow.UpdateContents;
    {-Called to update the window's contents}
  begin
  end;

  procedure AbstractWindow.MarkCurrent;
    {-Called when window is made current}
  begin
  end;

  procedure AbstractWindow.MarkNotCurrent;
    {-Called when window is deselected}
  begin
  end;

{$IFDEF UseStreams}

  constructor AbstractWindow.Load(var S : IdStream);
    {-Load from stream}
  begin
    Abstract;
  end;

  procedure AbstractWindow.Store(var S : IdStream);
    {-Store to stream}
  begin
    Abstract;
  end;

{$ENDIF}

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
