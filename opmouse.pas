{$IFDEF Windows}
  !! ERROR - This unit is not compatible with Windows !!
{$ENDIF}

{$S-,R-,V-,I-,B-,F+,O-,A-}

{$IFDEF Dpmi}                               {!!.20}
  {$C FIXED PRELOAD PERMANENT}              {!!.20}
{$ENDIF}                                    {!!.20}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPMOUSE.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1988, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpMouse;
  {-Mouse interface routines. Designed for use in text mode only.}

interface

uses
  Use32,
  Dpmi,         {!!.20}
{$IFDEF Dpmi}   {!!.20}
  WinApi,       {!!.20}
{$ENDIF}        {!!.20}
{$IFDEF VIRTUALPASCAL}
  VpSysLow,
  VPUtils,
  {$IFDEF OS2}
  Os2Def, Os2Base,
  {$ENDIF}
{$ENDIF}
  OpCrt;

var
  MouseInstalled : Boolean;   {True if a mouse is installed}
  MouseButtons   : Byte;       {Number of mouse buttons (0, 2, 3)}
  MouseCursorOn  : Boolean;   {True if mouse cursor is visible}

type
  ButtonStatus = (
    NoButton, LeftButton, RightButton, BothButtons,
    {the following values are possible only on a 3-button mouse}
    CenterButton, LeftAndCenterButtons, RightAndCenterButtons, All3Buttons);

const
  DisableEventHandler  = $00;
  MouseMoved           = $01;
  LeftButtonPressed    = $02;
  LeftButtonReleased   = $04;
  RightButtonPressed   = $08;
  RightButtonReleased  = $10;
  CenterButtonPressed  = $20;
  CenterButtonReleased = $40;
  AllMouseEvents       = $7F;
type
  MouseEventType = DisableEventHandler..AllMouseEvents;

const
  DefaultScreenMask = $FFFF;
  DefaultCursorMask = $7700;

type
  MouseState =
    record
      BufSize : Word;
      Buffer : array[1..400] of Byte;
    end;
  MouseStatePtr = ^MouseState;

var
  {current window coordinates for mouse}
  MouseXLo : Byte;           {0-based}
  MouseYLo : Byte;           {0-based}
  MouseXHi : Byte;           {1-based}
  MouseYHi : Byte;           {1-based}

const
  {if True, MouseKeyWord waits for the button to be released before returning
   its key code}
  WaitForButtonRelease : Boolean = True;

  {pseudo-scan codes returned by MouseKeyWord--DO NOT CHANGE THESE}
  MouseLft    = $EF00;       {left button}
  MouseRt     = $EE00;       {right button}
  MouseBoth   = $ED00;       {both buttons}
  MouseCtr    = $EC00;       {center button}
  MouseLftCtr = $EB00;       {left and center buttons}
  MouseRtCtr  = $EA00;       {right and center buttons}
  MouseThree  = $E900;       {all three buttons}
var
  MouseKeyWordX : Byte;      {mouse coordinates at time of call to MouseKeyWord}
  MouseKeyWordY : Byte;

const
  MouseRoutine : Pointer = nil;
  MouseRoutineEvent : MouseEventType = DisableEventHandler;

  MouseEvent : MouseEventType = DisableEventHandler;

  MouseStatus : ButtonStatus = NoButton;

{$IFDEF VIRTUALPASCAL}
  Function MouseLastX : Byte;
  Function MouseLastY : Byte;

const
  MouseStopping: Boolean = False;
  tidMouse: Longint = 0;
{$ELSE}
  MouseLastX : Byte = 1;
  MouseLastY : Byte = 1;
{$ENDIF}

function MousePressed : Boolean;
  {-Return True if a mouse button is currently being pressed}

function MouseKeyWord : Word;
  {-Return a pseudo-scan code based on which mouse button is being pressed}

function KeyOrButtonPressed : Boolean;
  {-Return True if a key or mouse button has been pressed}

function ReadKeyOrButton : Word;
  {-Return next key or mouse button}

procedure EnableEventHandling;
  {-Enable the event handler needed for MousePressed and MouseKeyWord}

procedure DisableEventHandling;
  {-Disable the event handler installed by EnableEventHandling}

procedure InitializeMouse;
  {-Reinitializes mouse and sets MouseInstalled}

procedure ShowMouse;
  {-Show the mouse cursor.}

procedure HideMouse;
  {-Hide the mouse cursor}

procedure MouseWhereXY(var MouseX, MouseY : Byte; var Status : ButtonStatus);
  {-Return mouse position and button status}

function MouseWhereX : Byte;
  {-Return current X coordinate for mouse}

function MouseWhereY : Byte;
  {-Return current Y coordinate for mouse}

procedure MouseWhereXYabs(var MouseX, MouseY : Byte; var Status : ButtonStatus);
  {-Return mouse position (absolute) and button status}

function MouseWhereXabs : Byte;
  {-Return current X coordinate (absolute) for mouse}

function MouseWhereYabs : Byte;
  {-Return current Y coordinate (absolute) for mouse}

procedure MouseGotoXY(MouseX, MouseY : Byte);
  {-Set mouse position}

function MouseButtonPressed(Button : ButtonStatus; var Count : Word;
                            var LastX, LastY : Byte) : Boolean;
  {-Returns True if the Button to check has been pressed. If so, Count has the
    number of times it has been pressed, and LastX/LastY have its position the
    last time it was pressed.}

function MouseButtonReleased(Button : ButtonStatus; var Count : Word;
                             var LastX, LastY : Byte) : Boolean;
  {-Returns True if the Button to check has been released. If so, Count has the
    number of times it has been released, and LastX/LastY have its position the
    last time it was released.}

procedure MouseWindow(XLow, YLow, XHigh, YHigh : Byte);
  {-Sets window coordinates to be observed by the mouse}

procedure FullMouseWindow;
  {-Sets mouse window coordinates to full screen}

procedure StoreMouseCoordinates(var WC : WindowCoordinates);
  {-Store the mouse window coordinates in WC}

procedure RestoreMouseCoordinates(WC : WindowCoordinates);
  {-Restore previously saved mouse window coordinates}

function MouseInWindow(XLo, YLo, XHi, YHi : Byte) : Boolean;
  {-Return True if mouse is within the specified window}

procedure SoftMouseCursor(ScreenMask, CursorMask : Word);
  {-Set mouse to use a software cursor}

procedure HardMouseCursor(StartLine, EndLine : Word);
  {-Set mouse to use the hardware cursor. StartLine and EndLine specify the
    shape of the cursor.}

procedure NormalMouseCursor;
  {-Set normal scan lines for mouse cursor based on current video mode}

procedure FatMouseCursor;
  {-Set larger scan lines for mouse cursor based on current video mode}

procedure BlockMouseCursor;
  {-Set scan lines for a block mouse cursor}

procedure HiddenMouseCursor;
  {-Hide the mouse cursor}

procedure GetMickeyCount(var Horizontal, Vertical : Integer);
  {-Returns the horizontal and vertical mickey count since the last call to
    this function. Negative numbers indicate movement up or to the left;
    positive numbers indicate movement down or to the right.}

procedure SetMickeyToPixelRatio(Horizontal, Vertical : Integer);
  {-Sets the mickey-to-pixel ratio. Default setting is 8,16. A setting of
    16,32 slows down the mouse considerably. A setting of 4,8 makes the
    mouse fly.}

procedure SetMouseEventHandler(EventMask : MouseEventType; UserRoutine : Pointer);
  {-Sets the address of a routine to be called when the specified mouse
    events occur. OPMOUSE handles the saving of the mouse driver's registers
    and sets up the DS register for the UserRoutine. Information about the
    Event is passed to UserRoutine using the global variables MouseEvent,
    MouseStatus, MouseLastX, and MouseLastY}

  {-- The remaining routines may not be implemented by all mouse drivers!! --}

function GetMousePage : Byte;
  {-Returns the video page where the mouse is being displayed}
  {-- May not be implemented in all mouse drivers!! --}

procedure SetMousePage(Page : Byte);
  {-Sets the video page where the mouse will be displayed}
  {-- May not be implemented in all mouse drivers!! --}

  {-- the following routines are intended primarily for use in TSR's --}

function MouseStateBufferSize : Word;
  {-Returns amount of memory needed to save the state of the mouse driver}

procedure SaveMouseState(var MSP : MouseStatePtr; Allocate : Boolean);
  {-Save the state of the mouse driver, allocating the buffer if requested.}

procedure RestoreMouseState(var MSP : MouseStatePtr; Deallocate : Boolean);
  {-Restore the state of the mouse driver and Deallocate the buffer if
    requested}

procedure HideMousePrim(var MouseState : Boolean);
  {-Save state of mouse cursor in MouseState and hide it}

procedure ShowMousePrim(MouseState : Boolean);
  {-Hide or unhide the mouse cursor}

  {==========================================================================}

implementation

{$IFNDEF VIRTUALPASCAL}
type
  {for typecasting in save/restore state calls}
  SO =
    record
      O, S : Word;
    end;

  HiLo =
    record
      LoWord, HiWord : Word;
    end;
{$ENDIF}

var
  SaveExitProc : Pointer;
  EventHandlerInstalled : Boolean;

{$IFDEF VIRTUALPASCAL}
Var
  MousePosX    : Integer; // 0-based
  MousePosY    : Integer; // 0-based
  DownX        : Byte;    // 0-based
  DownY        : Byte;    // 0-based
  LastButtons  : Byte;

const
  MickeyScaleX : Longint = 16;
  MickeyScaleY : Longint = 16;

  procedure InitializeMouse;
  begin
    MouseButtons := SysTVDetectMouse;
    if MouseButtons > 0 then
      begin
        SysTVInitMouse(MousePosX, MousePosY);
        LastButtons := 0;
        MouseInstalled := True;
      end
    else
      MouseInstalled := False;
  end;

  procedure ShowMouseLow;
  begin
    if MouseInstalled then
      SysTVShowMouse;
  end;

  procedure HideMouseLow;
  begin
   If MouseInstalled then
     SysTVHideMouse;
    end;

  procedure UpdateMouse;
  begin
    SysTVUpdateMouseWhere( MousePosX, MousePosY );
  end;

  // Return 1-based coordinates
  procedure MouseWhereXY(var MouseX, MouseY : Byte;
                         var Status : ButtonStatus);
  begin
    UpdateMouse;
    Status := MouseStatus;
    MouseX := MousePosX+1-MouseXLo;
    MouseY := MousePosY+1-MouseYLo;
  end;

  // Return 1-based coordinate
  function MouseWhereX : Byte;
  begin
    UpdateMouse;
    MouseWhereX := MousePosX-MouseXLo+1;
  end;

  // Return 1-based coordinate
  function MouseWhereY : Byte;
  begin
    UpdateMouse;
    MouseWhereY := MousePosY-MouseYLo+1;
  end;

  procedure MouseGotoXY(MouseX, MouseY : Byte);
  begin
    SysTVSetCurPos(MouseX+1+MouseXLo, MouseY+1+MouseYLo);
  end;

  function MouseButtonPressed(Button : ButtonStatus;
                              var Count : Word;
                              var LastX, LastY : Byte) : Boolean;
  begin
  end;

  function MouseButtonReleased(Button : ButtonStatus;
                               var Count : Word;
                               var LastX, LastY : Byte) : Boolean;
  begin
  end;

  procedure MouseWindow(XLow, YLow, XHigh, YHigh : Byte);
  begin
    MouseXLo := XLow-1;
    MouseYLo := YLow-1;
    MouseXHi := XHigh;
    MouseYHi := YHigh;
  end;

  procedure SoftMouseCursor(ScreenMask, CursorMask : Word);
  begin
  end;

  procedure HardMouseCursor(StartLine, EndLine : Word);
  begin
    SysTVSetCurType(StartLine, EndLine, StartLine <= EndLine);
  end;

  function GetMousePage : Byte;
  begin
    GetMousePage := 0;
  end;

  procedure SetMousePage(Page : Byte);
  begin
  end;

  // Return positions moved since last call
  procedure GetMickeyCount(var Horizontal, Vertical : Integer);
  const
    MouOldX : Longint = 0;
    MouOldY : Longint = 0;
  begin
    Horizontal := (MousePosX-MouOldX)*MickeyScaleX;
    Vertical := (MousePosY-MouOldY)*MickeyScaleY;
    MouOldX := MousePosX;
    MouOldY := MousePosY
  end;

  procedure SetMickeyToPixelRatio(Horizontal, Vertical : Integer);
  begin
    MickeyScaleX := Horizontal;
    MickeyScaleY := Vertical;
  end;

  function GetStorageSize : Word;
  {-Returns amount of memory needed to save state of mouse driver}
  begin
  end;

  procedure SaveMouseStatePrim(var Buffer);
  {-Save mouse state in Buffer}
  begin
  end;

  procedure RestoreMouseStatePrim(var Buffer);
  {-Restore mouse state from Buffer}
  begin
  end;

  // Return 1-based coordinates
  Function MouseLastX : Byte;
  begin
    MouseLastX := DownX+1;
  end;

  Function MouseLastY : Byte;
  begin
    MouseLastY := DownY+1;
  end;

  function MouseThread(p: Pointer): Longint;
  var
    Event    : TSysMouseEvent;
    Status   : ButtonStatus;
    DosEvent : Longint;
  begin
    while not MouseStopping do
      if SysTVGetMouseEvent(Event) then
        with Event do
          begin
            Status := MouseStatus;
            DosEvent := 0; // Simulate Dos "event" flags

            // Translate buttons
            if smeButtons = 0 then
              MouseStatus := NoButton
            else if smeButtons = 1 then
              MouseStatus := LeftButton
            else if smeButtons = 2 then
              MouseStatus := RightButton
            else if smeButtons = 3 then
              MouseStatus := BothButtons;

            // Compare with previous state
            if ( Status = LeftButton ) and ( MouseStatus = NoButton ) then
              DosEvent := LeftButtonReleased;
            if ( MouseStatus = LeftButton ) then
              DosEvent := DosEvent or LeftButtonPressed;
            if ( Status = RightButton ) and ( MouseStatus in [LeftButton,NoButton] ) then
              DosEvent := DosEvent or RightButtonReleased;
            if ( MouseStatus in [ RightButton, BothButtons ] ) then
              DosEvent := DosEvent or RightButtonPressed;

            // Determine if mouse has moved
            if (smePos.X <> MousePosX) or (smePos.y <> MousePosY) then
              begin
                MousePosX := smePos.X;
                MousePosY := smePos.Y;
                DosEvent := DosEvent or MouseMoved;
              end;

            // Remember last button action position
            if MouseStatus <> noButton then
              begin
                DownX := smePos.X;
                DownY := smePos.Y;
              end;

            // If event matches event mask, call user routine
            MouseEvent := DosEvent;
            if DosEvent and MouseRoutineEvent <> 0 then
              if assigned(MouseRoutine) then
                asm
                  call [MouseRoutine]
                end;
          end
      else
        // No mouse event in queue: Wait a little while before re-checking
        SysCtrlSleep(30);
    tidMouse := 0;
  end;

  procedure MouseEventHandler;
  begin
    // Mothing.  What should I do here? :)
  end;

  procedure MouseEventPrim(EventMask : MouseEventType; UserRoutine : Pointer);
  begin
    MouseStopping := False;
    // Set up mouse thread parameters
    MouseRoutineEvent := EventMask;
    if Longint(UserRoutine) <> Longint(@MouseEventHandler) then
      MouseRoutine := UserRoutine;

    // Start mousethread if not already active
    if tidMouse = 0 then
      tidMouse := VPBeginThread( MouseThread, 16384, nil );
  end;

{$ELSE}
  {$L OPMOUSE.OBJ}
  {$L OPMOUSE2.OBJ}
  procedure InitializeMouse;
    external {OPMOUSE.OBJ};
  procedure ShowMouseLow;
    external {OPMOUSE.OBJ};
  procedure HideMouseLow;
    external {OPMOUSE.OBJ};
  procedure MouseWhereXY(var MouseX, MouseY : Byte;
                         var Status : ButtonStatus);
    external {OPMOUSE2.OBJ};
  function MouseWhereX : Byte;
    external {OPMOUSE2.OBJ};
  function MouseWhereY : Byte;
    external {OPMOUSE2.OBJ};
  procedure MouseGotoXY(MouseX, MouseY : Byte);
    external {OPMOUSE2.OBJ};
  function MouseButtonPressed(Button : ButtonStatus;
                              var Count : Word;
                              var LastX, LastY : Byte) : Boolean;
    external {OPMOUSE2.OBJ};
  function MouseButtonReleased(Button : ButtonStatus;
                               var Count : Word;
                               var LastX, LastY : Byte) : Boolean;
    external {OPMOUSE2.OBJ};
  procedure MouseWindow(XLow, YLow, XHigh, YHigh : Byte);
    external {OPMOUSE.OBJ};
  procedure SoftMouseCursor(ScreenMask, CursorMask : Word);
    external {OPMOUSE2.OBJ};
  procedure HardMouseCursor(StartLine, EndLine : Word);
    external {OPMOUSE2.OBJ};
  function GetMousePage : Byte;
    external {OPMOUSE2.OBJ};
  procedure SetMousePage(Page : Byte);
    external {OPMOUSE2.OBJ};
  procedure GetMickeyCount(var Horizontal, Vertical : Integer);
    external {OPMOUSE2.OBJ};
  procedure SetMickeyToPixelRatio(Horizontal, Vertical : Integer);
    external {OPMOUSE2.OBJ};

  {these procedures, used internally, are all called FAR}
  procedure MouseEventPrim(EventMask : MouseEventType; UserRoutine : Pointer);
    external {OPMOUSE2.OBJ};
  procedure MouseEventHandler;
    external {OPMOUSE2.OBJ};
  function GetStorageSize : Word;
    {-Returns amount of memory needed to save state of mouse driver}
    external {OPMOUSE.OBJ};
  procedure SaveMouseStatePrim(var Buffer);
    {-Save mouse state in Buffer}
    external {OPMOUSE.OBJ};
  procedure RestoreMouseStatePrim(var Buffer);
    {-Restore mouse state from Buffer}
    external {OPMOUSE.OBJ};

  {these procedures, used internally, are all called NEAR}
  procedure ScaleUpY;
    external {OPMOUSE.OBJ};
  procedure ScaleUpX;
    external {OPMOUSE.OBJ};
  procedure ScaleDownY;
    external {OPMOUSE.OBJ};
  procedure ScaleDownX;
    external {OPMOUSE.OBJ};
{$ENDIF}

{$IFDEF Dpmi}  {!!.20 - new for protected mode}
  function GetStorageSizePM : Word;
  var
    DR : DPMIRegisters;
  begin
    FillChar(DR, SizeOf(DR), 0);
    DR.AX := 21;
    if SimulateRealModeInt($33, DR) <> 0 then
      GetStorageSizePM := 0
    else
      GetStorageSizePM := DR.BX;
  end;

  procedure SaveMouseStatePM(Buffer : Pointer);
  var
    DR : DPMIRegisters;
  begin
    FillChar(DR, SizeOf(DR), 0);
    DR.AX := 22;
    DR.ES := SO(Buffer).S;
    DR.DX := SO(Buffer).O;
    if SimulateRealModeInt($33, DR) <> 0 then ;
  end;

  procedure RestoreMouseStatePM(Buffer : Pointer);
  var
    DR : DPMIRegisters;
  begin
    FillChar(DR, SizeOf(DR), 0);
    DR.AX := 23;
    DR.ES := SO(Buffer).S;
    DR.DX := SO(Buffer).O;
    if SimulateRealModeInt($33, DR) <> 0 then ;
  end;
{$ENDIF}

  function MousePressed : Boolean;
    {-Return True if a mouse button is currently being pressed}
  begin
    if not(MouseInstalled and EventHandlerInstalled) then
      MousePressed := False
    else
      MousePressed := MouseStatus <> NoButton;
  end;

  function MouseKeyWord : Word;
    {-Return a pseudo scan code based on which key is being pressed}
  const
    ScanTable : array[LeftButton..All3Buttons] of Word = (MouseLft, MouseRt,
      MouseBoth, MouseCtr, MouseLftCtr, MouseRtCtr, MouseThree);
    BitsTable : array[ButtonStatus] of Byte = (0, 1, 1, 2, 1, 2, 2, 3);
  var
    Status, TempStatus : ButtonStatus;
    SaveBitsOn, BitsOn : Byte;
  begin
    {return bogus key code if no mouse or event handler not installed}
    if not(MouseInstalled and EventHandlerInstalled) then begin
      MouseKeyWord := $FFFF;
      Exit;
    end;

    {force interrupts on}
    {$IFNDEF VIRTUALPASCAL}
    inline($FB);               {sti}
    {$ENDIF}

    {wait for a button to be pressed}
    Status := MouseStatus;
    while Status = NoButton do begin
      {make sure TSR's can pop up}
      {$IFNDEF VIRTUALPASCAL}
      inline($cd/$28);
      {$ENDIF}
      Status := MouseStatus;
    end;

    if WaitForButtonRelease then begin
      {save the current number of buttons that are on}
      SaveBitsOn := BitsTable[Status];

      {wait for the button(s) now being pressed to be released}
      TempStatus := MouseStatus;
      while (Byte(TempStatus) and Byte(Status)) <> 0 do begin
        {see if an additional button has been pressed}
        BitsOn := BitsTable[TempStatus];
        if BitsOn > SaveBitsOn then begin
          {another button was pressed--we want it too}
          Status := TempStatus;
          SaveBitsOn := BitsOn;
        end;

        {make sure TSR's can pop up}
        {$IFNDEF VIRTUALPASCAL}
        inline($cd/$28);
        {$ENDIF}

        TempStatus := MouseStatus;
      end;
    end;

    {turn interrupts off}
    {$IFNDEF VIRTUALPASCAL}
    inline($FA);
    {$ENDIF}

    {return pseudo-scan code}
    MouseKeyWord := ScanTable[Status];

    {save current mouse coordinates}
    MouseKeyWordX := MouseLastX;
    MouseKeyWordY := MouseLastY;

    {turn interrupts on}
    {$IFNDEF VIRTUALPASCAL}
    inline($FB);
    {$ENDIF}
  end;

  procedure ShowMouse;
    {-Show the mouse cursor.}
  begin
    if not MouseCursorOn then
      ShowMouseLow;
  end;

  procedure HideMouse;
    {-Hide the mouse cursor}
  begin
    if MouseCursorOn then
      HideMouseLow;
  end;

  procedure HideMousePrim(var MouseState : Boolean);
    {-Save state of mouse cursor in MouseState and hide it}
  begin
    MouseState := MouseCursorOn;
    if MouseCursorOn then
      HideMouseLow;
  end;

  procedure ShowMousePrim(MouseState : Boolean);
    {-Hide or unhide the mouse cursor}
  begin
    if MouseState then
      ShowMouse
    else
      HideMouse;
  end;

  procedure NormalMouseCursor;
    {-Set normal scan lines for mouse cursor based on current video mode}
  var
    ScanLines : Word;
  begin
    if Font8x8Selected then
      ScanLines := $0507
    else if CurrentMode = 7 then
      ScanLines := $0B0C
    else
      ScanLines := $0607;
    HardMouseCursor(Hi(ScanLines), Lo(ScanLines));
  end;

  procedure FatMouseCursor;
    {-Set larger scan lines for mouse cursor based on current video mode}
  var
    ScanLines : Word;
  begin
    if Font8x8Selected then
      ScanLines := $0307
    else if CurrentMode = 7 then
      ScanLines := $090C
    else
      ScanLines := $0507;
    HardMouseCursor(Hi(ScanLines), Lo(ScanLines));
  end;

  procedure BlockMouseCursor;
    {-Set scan lines for a block mouse cursor}
  var
    EndLine : Byte;
  begin
    if Font8x8Selected or (CurrentMode <> 7) then
      EndLine := $07
    else
      EndLine := $0C;
    HardMouseCursor(0, EndLine);
  end;

  procedure HiddenMouseCursor;
    {-Hide the mouse cursor}
  begin
    HardMouseCursor($20, 0);
  end;

  procedure FullMouseWindow;
    {-Sets mouse window coordinates to full screen}
  begin
    MouseWindow(1, 1, ScreenWidth, ScreenHeight);
  end;

  procedure StoreMouseCoordinates(var WC : WindowCoordinates);
    {-Store the mouse window coordinates in WC}
  begin
    with WC do begin
      XL := Succ(MouseXLo);
      YL := Succ(MouseYLo);
      XH := MouseXHi;
      YH := MouseYHi;
    end;
  end;

  procedure RestoreMouseCoordinates(WC : WindowCoordinates);
    {-Restore previously saved window coordinates}
  begin
    with WC do
      MouseWindow(XL, YL, XH, YH);
  end;

  function MouseInWindow(XLo, YLo, XHi, YHi : Byte) : Boolean;
    {-Return True if mouse is within the specified window}
  var
    mX, mY : Byte;
    Status : ButtonStatus;
  begin
    if not MouseInstalled then
      MouseInWindow := False
    else begin
      {get current position of mouse and see if it's inside the window}
      MouseWhereXY(mX, mY, Status);
      MouseInWindow := (mX >= XLo) and (mX <= XHi) and (mY >= YLo) and (mY <= YHi);
    end;
  end;

  function MouseStateBufferSize : Word;
    {-Returns amount of memory needed to save the state of the mouse driver}
  var
    I : Word;
  begin
    if not MouseInstalled then
      MouseStateBufferSize := 0
    else begin
{$IFDEF Dpmi}                        {!!.20}
      I := GetStorageSizePM;         {!!.20}
{$ELSE}                              {!!.20}
      I := GetStorageSize;
{$ENDIF}                             {!!.20}
      if I <> 0 then
        Inc(I, SizeOf(Word));
      MouseStateBufferSize := I;
    end;
  end;

  procedure SaveMouseState(var MSP : MouseStatePtr; Allocate : Boolean);
    {-!!.20 rewritten - Save the state of the mouse driver, allocating the
      buffer if requested.}
  var
    I : Word;
{$IFDEF Dpmi}
    L : Longint;
{$ENDIF}
  begin
{$IFDEF Dpmi}
    {make sure a mouse is installed}
    if not MouseInstalled then
      Exit;

    {see how much memory we need}
    I := MouseStateBufferSize;

    {exit if 0 was returned or insufficient memory exists}
    if (I = 0) or (I > 2048) then {!!.22}
      Exit;

    {allocate state buffer if required}
    if Allocate then begin
      GetMem(MSP, I);
      {MSP^.BufSize := I;}        {!!.22}
    end;
    MSP^.BufSize := I;            {!!.22}

    {allocate real-mode memory for the state call}
    L := GlobalDosAlloc(I);
    if L = 0 then begin
      if Allocate then begin
        FreeMem(MSP, I);
        MSP := nil;
      end;
      Exit;
    end;

    {get the state info}
    SaveMouseStatePM(Ptr(HiLo(L).HiWord, 0));
    Move(Ptr(HiLo(L).LoWord, 0)^, MSP^.Buffer, I-2);

    {free the real-mode memory}
    L := GlobalDosFree(HiLo(L).LoWord);


{$ELSE}

    if Allocate then begin
      {assume failure}
      MSP := nil;

      {make sure a mouse is installed}
      if not MouseInstalled then
        Exit;

      {see how much memory we need}
      I := MouseStateBufferSize;

      {exit if 0 was returned or insufficient memory exists}
      if (I = 0) or (I > MaxAvail) then
        Exit;

      {allocate the MouseState record}
      GetMem(MSP, I);

      {fill in the MouseState record}
      MSP^.BufSize := I;
    end;

    SaveMouseStatePrim(MSP^.Buffer);
{$ENDIF}
  end;

  procedure RestoreMouseState(var MSP : MouseStatePtr; Deallocate : Boolean);
    {-!!.20 rewritten - Restore the state of the mouse driver and Deallocate
      the buffer if requested}
{$IFDEF Dpmi}
  var
    L : LongInt;
{$ENDIF}
  begin
    {exit if MSP is nil}
    if (MSP = nil) or not MouseInstalled then
      Exit;

{$IFDEF Dpmi}
    {allocate real mode memory buffer}
    L := GlobalDosAlloc(MSP^.BufSize);
    if L = 0 then begin
      if Deallocate then begin
        FreeMem(MSP, MSP^.BufSize);
        MSP := nil;
      end;
      Exit;
    end;

    {restore the mouse state}
    Move(MSP^.Buffer, Ptr(HiLo(L).LoWord, 0)^, MSP^.BufSize-2);
    RestoreMouseStatePM(Ptr(HiLo(L).HiWord, 0));

    {free the real mode memory buffer}
    L := GlobalDosFree(HiLo(L).LoWord);

{$ELSE}

    RestoreMouseStatePrim(MSP^.Buffer);
{$ENDIF}

    if Deallocate then begin
      {deallocate the buffer}
      FreeMem(MSP, MSP^.BufSize);

      {set MSP to nil so we won't do the same thing twice}
      MSP := nil;
    end;
  end;

  procedure EnableEventHandling;
    {-Enable the event handler needed for MousePressed and MouseKeyWord}
  begin
    if MouseInstalled and not EventHandlerInstalled then begin
      MouseEventPrim(AllMouseEvents, @MouseEventHandler);
      EventHandlerInstalled := True;
    end;
  end;

  procedure SetMouseEventHandler(EventMask : MouseEventType; UserRoutine : Pointer);
    {-Sets the address of a routine to be called when the specified mouse
      events occur}
  begin
    {make sure a mouse is installed}
    if not MouseInstalled then
      Exit;

    if EventMask = DisableEventHandler then
      MouseRoutine := nil
    else
      MouseRoutine := UserRoutine;
    if MouseRoutine = nil then
      MouseRoutineEvent := DisableEventHandler
    else
      MouseRoutineEvent := EventMask;

    {enable the event handler if it isn't already}
    EnableEventHandling;
  end;

  procedure DisableEventHandling;
    {-Disable the event handler installed by EnableEventHandling}
  begin
    if EventHandlerInstalled then begin
      {disable the event handler}
      MouseEventPrim(DisableEventHandler, nil);

      {set flag to indicate that we're not installed}
      EventHandlerInstalled := False;

      {reset variables}
      MouseRoutine := nil;
      MouseRoutineEvent := DisableEventHandler;
      MouseEvent := DisableEventHandler;
      MouseStatus := NoButton;
      {$IFDEF VirtualPascal}
      MouseStopping := True;
      {$ENDIF}
    end;
  end;

  function KeyOrButtonPressed : Boolean;
    {-Return True if a key or mouse button has been pressed}
  begin
    KeyOrButtonPressed := KeyPressed or MousePressed;
  end;

  function ReadKeyOrButton : Word;
    {-Return next key or mouse button}
  var
    I : Word;
  begin
    I := $FFFF;
    repeat
      if KeyPressed then
        I := ReadKeyWord
      else if MousePressed then
        I := MouseKeyWord
      else
        {give TSR's a chance to pop up}
        {$IFNDEF VIRTUALPASCAL}
        inline($cd/$28)
        {$ENDIF}
        ;
    until I <> $FFFF;
    ReadKeyOrButton := I;
  end;

  procedure MouseWhereXYabs(var MouseX, MouseY : Byte; var Status : ButtonStatus);
    {-Return mouse position (absolute) and button status}
  begin
    MouseWhereXY(MouseX, MouseY, Status);
    Inc(MouseX, MouseXLo);
    Inc(MouseY, MouseYLo);
  end;

  function MouseWhereXabs : Byte;
    {-Return current X coordinate (absolute) for mouse}
  begin
    MouseWhereXabs := MouseWhereX+MouseXLo;
  end;

  function MouseWhereYabs : Byte;
    {-Return current Y coordinate (absolute) for mouse}
  begin
    MouseWhereYabs := MouseWhereY+MouseYLo;
  end;

  procedure ExitHandler;
    {-Reinitialize and hide mouse on exit}
  begin
    {restore previous exit handler}
    ExitProc := SaveExitProc;

    {reinitialize the mouse--disables all event handlers and hides mouse}
{$IFDEF VIRTUALPASCAL}
//    MouClose( hMouse );
{$ELSE}
    InitializeMouse;
{$ENDIF}
  end;

begin
  {initialize the mouse if one is installed (sets MouseInstalled)}
  InitializeMouse;

  {no need to install exit handler if not installed}
  if MouseInstalled then begin
    FullMouseWindow;
    SaveExitProc := ExitProc;
    ExitProc := @ExitHandler;
  end;
end.
