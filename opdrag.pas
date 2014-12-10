{$IFDEF Windows}
  !! ERROR - This unit is not compatible with Windows !!
{$ENDIF}

{$R-,S-,I-,V-,B-,F+,O-,A-}

{$IFDEF Dpmi}                               {!!.20}
  {$C FIXED PRELOAD PERMANENT}              {!!.20}
{$ENDIF}                                    {!!.20}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPDRAG.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1988, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{Thanks to Steve Sneed for his modifications to add an
 initial delay before autorepeat action begins}

unit OpDrag;
  {-A unit of routines for implementing mouse dragging}

interface

uses          {!!.20}
  Use32,
  {$IFDEF VirtualPascal}
  VpSysLow,
  {$ENDIF}
  Dos,
  OpConst,    {!!.20}
  OpInline,
  OpRoot,
  OpString,
  OpCrt,
  OpMouse,
  OpCmd,
  OpFrame,
  OpWindow;

const
  MoveHotCode   = hsRegion0;
  ResizeHotCode = hsRegion1;
  ZoomHotCode   = hsRegion2;
  CloseHotCode  = hsRegion3;      {!!.30}

  MouseLftDown  = $E800;          {Pseudo-scan code for down on left button}
  MouseLftAuto  = $E700;          {autorepeat or move with left button down}

  AutoRepeatTicks : Word = 0;     {Ticks to delay between auto mouse downs}
  AutoDelayTicks  : Word = 0;     {Ticks to delay before first auto down} {!!.11}

type
  DragProcessorPtr = ^DragProcessor;
  DragProcessor =
    object(CommandProcessor)
      dpScreenMask         : Word; {Screen mask used always}
      dpDefaultMouseCursor : Word; {Mouse cursor used normally}
      dpMoveMouseCursor    : Word; {Mouse cursor used during moves}
      dpResizeMouseCursor  : Word; {Mouse cursor used during resize}
      dpMickeyRatio        : Byte; {Scale factor: mickeys -> characters}
      dpDummy              : record end; {For convenience of Store and Load}

      constructor Init(KeysPtr : CmdTablePtr; MaxIndex : Word);
        {-Initialize DragProcessor}
      procedure SetMouseCursor(DefaultCur, MoveCur, ResizeCur : Word);
        {-Set the mouse cursor masks for various conditions}
      procedure SetScreenMask(Mask : Word);
        {-Set the screen mask used for the mouse cursor}
      procedure SetMickeyRatio(Ratio : Byte);
        {-Set the mickey to character scale factor}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load from a stream}
      procedure Store(var S : IdStream);
        {-Store in a stream}
    {$ENDIF}

      {.Z+}
      function cpGetKey : Word; virtual;
        {-Called to get next keystroke}
      function cpKeyPressed : Boolean; virtual;
        {-Called to see if keys are pending}
      procedure cpOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure cpOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      {.Z-}
    end;

{$IFDEF UseAdjustableWindows}
function HandleMousePress(var M : CommandWindow) : Byte;
  {-handle mouse presses not already handled by commandwindow,
    returning a hot region code, if any}
{$ENDIF}

{$IFDEF UseStreams}
procedure DragProcessorStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing drag processors}
{$ENDIF}

procedure InstallISRs;
  {-Install mouse event handler and Int1C Handler}

procedure RemoveISRs;
  {-Restore Int1C and disable mouse event handling}

procedure SetChainedEventHandler(EventMask : MouseEventType;
                                 UserRoutine : Pointer);
  {-Install a secondary mouse event handler}

procedure ClearMouseEvents;
  {-Clear the mouse event queue}

function MouseEventsPending : Boolean;
  {-Return True if mouse events are available to read}

procedure StoreMouseEvent(K : Word; X, Y : Byte);
  {-Store a new event in the queue. Overwrites last event if queue is full}

function ReadMouseEvent(var X, Y : Byte) : Word;
  {-Read and remove the first event from the queue}

function PeekMouseEvent(var X, Y : Byte) : Word;
  {-Read the first event from the queue but leave it there}

procedure WaitForButtonUp;
  {-Take events from the queue until the left button is released}

procedure SetAutoDelay(Ticks : Word); {!!.11}
  {-Clear mouse events and set new autodelay value}

  {====================================================================}

implementation

const
  ISRsInstalled : Boolean = False;

type
  MouseEventRec =
    record
      KW : Word; {Scan word}
      MX : Byte; {Mouse position}
      MY : Byte;
    end;
const
  EventQueueMax = 7;
type
  EventQueueArray = array[0..EventQueueMax] of MouseEventRec;
var
  SaveExit : Pointer;
  SaveInt1C : Pointer;
  TickCounter : Word;
  DelayCounter : Word;                                              {!!.11}
  MouseEvents : EventQueueArray;
  QueueTail : Word;
  QueueHead : Word;
  ChainedEventHandler : procedure;
  ChainedEventMask : MouseEventType;

  procedure ClearMouseEvents;
    {-Clear the mouse event queue}
  begin
    QueueTail := 0;
    QueueHead := 0;
  end;

  function MouseEventsPending : Boolean;
    {-Return True if mouse events are available to read}
  begin
    MouseEventsPending := (QueueTail <> QueueHead);
  end;

  procedure StoreMouseEvent(K : Word; X, Y : Byte);
    {-Store a new event in the queue. Overwrites last event if queue is full}
  var
    SaveTail : Word;
  begin
    SaveTail := QueueTail;
    if QueueTail = EventQueueMax then
      QueueTail := 0
    else
      Inc(QueueTail);
    if QueueTail = QueueHead then
      {Queue is full, overwrite last element}
      QueueTail := SaveTail;
    {Store element}
    with MouseEvents[QueueTail] do begin
      KW := K;
      MX := X;
      MY := Y;
    end;
  end;

  function ReadMouseEvent(var X, Y : Byte) : Word;
    {-Read and remove the first event from the queue}
  begin
    {It's an error to call ReadMouseEvent if events are not pending}
    if QueueHead = EventQueueMax then
      QueueHead := 0
    else
      Inc(QueueHead);
    with MouseEvents[QueueHead] do begin
      ReadMouseEvent := KW;
      X := MX;
      Y := MY;
    end;
  end;

  function PeekMouseEvent(var X, Y : Byte) : Word;
    {-Read the first event from the queue but leave it there}
  var
    Head : Word;
  begin
    {It's an error to call PeekMouseEvent if events are not pending}
    if QueueHead = EventQueueMax then
      Head := 0
    else
      Head := QueueHead+1;
    with MouseEvents[Head] do begin
      PeekMouseEvent := KW;
      X := MX;
      Y := MY;
    end;
  end;

  procedure SetAutoDelay(Ticks : Word); {!!.11}
    {-Clear mouse events and set new autodelay value}
  begin
    ClearMouseEvents;
    AutoDelayTicks := Ticks;
  end;

  procedure MouseHandler;
    {-Handle asynchronous mouse events}
  var
    K : Word;
  begin
    {Assume no event that we care about}
    K := 0;

{$IFDEF VirtualPascal}
    if MouseStatus = LeftButton then begin
      {Left button is down}
      Inc(TickCounter);
      Inc(DelayCounter);                                              {!!.11}
      if (DelayCounter > AutoDelayTicks) and                          {!!.11}
         (TickCounter > AutoRepeatTicks) then begin
        StoreMouseEvent(MouseLftAuto, MouseLastX, MouseLastY);
        TickCounter := 0;
      end;
    end;
{$ENDIF}

    if MouseStatus = LeftButton then begin
      {Left button is only button down}
      if MouseEvent and LeftButtonPressed <> 0 then begin
        {The button was just pushed}
        K := MouseLftDown;
        {Reset the autorepeat counters}
        TickCounter := 0;
        DelayCounter := 0;                                            {!!.11}
      end else if MouseEvent and MouseMoved <> 0 then begin
        {The mouse was moved}
        K := MouseLftAuto;
        {Reset the autorepeat counters}
        TickCounter := 0;
        DelayCounter := 0;                                            {!!.11}
      end;
    end else if MouseStatus = NoButton then begin
      {No buttons remain down}
      if MouseEvent and LeftButtonReleased <> 0 then
        K := MouseLft
      else if MouseEvent and RightButtonReleased <> 0 then
        K := MouseRt
      else if MouseEvent and CenterButtonReleased <> 0 then
        K := MouseCtr;
    end;

    if K <> 0 then
      StoreMouseEvent(K, MouseLastX, MouseLastY);

    {Called chained event handler if appropriate}
    if @ChainedEventHandler <> nil then
      if MouseEvent and ChainedEventMask <> 0 then
        ChainedEventHandler;
  end;

{$IFDEF VIRTUALPASCAL}
  procedure InstallISRs;
    {-Install mouse event handler and Int1C Handler}
  begin
    if not ISRsInstalled then begin
      MouseStopping := False;
      SetMouseEventHandler(AllMouseEvents, @MouseHandler);
      ISRsInstalled := True;
    end;
  end;

  procedure RemoveISRs;
    {-Restore Int1C and disable mouse event handling}
  begin
    if ISRsInstalled then begin
      DisableEventHandling;
      ISRsInstalled := False;
    end;
  end;
{$ELSE}
  procedure MouseInt1C; interrupt;
    {-Handle timer ticks as they apply to mouse events}
  begin
    {Call old int 1C ISR}
    inline($9C/$FF/$1E/>SaveInt1C); {PUSHF; CALL FAR [>SaveInt1C]}

    if MouseStatus = LeftButton then begin
      {Left button is down}
      Inc(TickCounter);
      Inc(DelayCounter);                                              {!!.11}
      if (DelayCounter > AutoDelayTicks) and                          {!!.11}
         (TickCounter > AutoRepeatTicks) then begin
        StoreMouseEvent(MouseLftAuto, MouseLastX, MouseLastY);
        TickCounter := 0;
      end;
    end;
  end;

  procedure InstallISRs;
    {-Install mouse event handler and Int1C Handler}
  begin
    if not ISRsInstalled then begin
      GetIntVec($1C, SaveInt1C);
      SetIntVec($1C, @MouseInt1C);
      SetMouseEventHandler(AllMouseEvents, @MouseHandler);
      ISRsInstalled := True;
    end;
  end;

  procedure RemoveISRs;
    {-Restore Int1C and disable mouse event handling}
  begin
    if ISRsInstalled then begin
      SetIntVec($1C, SaveInt1C);
      DisableEventHandling;
      ISRsInstalled := False;
    end;
  end;
{$ENDIF}

  procedure SetChainedEventHandler(EventMask : MouseEventType;
                                   UserRoutine : Pointer);
    {-Install a secondary mouse event handler}
  begin
    ChainedEventMask := EventMask;
    @ChainedEventHandler := UserRoutine;
  end;

  procedure MouseExit;
    {-Exit procedure to undo interrupt handlers}
  begin
    ExitProc := SaveExit;
    RemoveISRs;
  end;

  procedure WaitForButtonUp;
    {-Take events from the queue until the left button is released}
  var
    X, Y : Byte;
    EventKey : Word;
  begin
    EventKey := 0;
    repeat
      if MouseEventsPending then
        EventKey := ReadMouseEvent(X, Y);
    until EventKey = MouseLft;
  end;

  constructor DragProcessor.Init(KeysPtr : CmdTablePtr; MaxIndex : Word);
    {-Initialize DragProcessor}
  var
    Status : Word;
  begin
    if not CommandProcessor.Init(KeysPtr, MaxIndex) then
      Fail;

    {Don't use the "keyboard" routines in OPMOUSE}
    cpGetKeyProc := ReadKeyWord;
    cpKeyPressedProc := KeyPressed;

    {Set default values for DragProcessor fields}
    dpScreenMask := $0000;         {Mask out existing color and character}
    dpDefaultMouseCursor := $7004; {Reverse video diamond}
    dpMoveMouseCursor := $7012;    {Reverse video up/down arrows}
    dpResizeMouseCursor := $701D;  {Reverse video left/right arrows}
    dpMickeyRatio := 16;           {16:1 scaling}

    {Skip the rest if no mouse}
    if not MouseInstalled then
      Exit;

    {Add command for left button down}
    AddCommand(ccMouseDown, 1, MouseLftDown, 0);
    {Add command for left button autorepeat}
    AddCommand(ccMouseAuto, 1, MouseLftAuto, 0);
    Status := GetLastError;
    if Status <> 0 then begin
      InitStatus := epFatal+(Status mod 10000);
      Fail;
    end;

    {The mouse is enabled and so is dragging}
    SetFlag(cpOptions, cpEnableMouse+cpMouseDrag);

    {Set the default mouse cursor}
    SoftMouseCursor(dpScreenMask, dpDefaultMouseCursor);
  end;

  procedure DragProcessor.SetMouseCursor(DefaultCur, MoveCur, ResizeCur : Word);
    {-Set the mouse cursor masks for various conditions}
  begin
    dpDefaultMouseCursor := DefaultCur;
    dpMoveMouseCursor := MoveCur;
    dpResizeMouseCursor := ResizeCur;
    SoftMouseCursor(dpScreenMask, dpDefaultMouseCursor);
  end;

  procedure DragProcessor.SetScreenMask(Mask : Word);
    {-Set the screen mask used for the mouse cursor}
  begin
    dpScreenMask := Mask;
    SoftMouseCursor(dpScreenMask, dpDefaultMouseCursor);
  end;

  procedure DragProcessor.SetMickeyRatio(Ratio : Byte);
    {-Set the mickey to character scale factor}
  begin
    dpMickeyRatio := Ratio;
  end;

{$IFDEF UseStreams}
  constructor DragProcessor.Load(var S : IdStream);
    {-Load from a stream}
  begin
    {Load the underlying CommandProcessor}
    if not CommandProcessor.Load(S) then
      Fail;

    {Don't use the "keyboard" routines in OPMOUSE}
    if @cpGetKeyProc = @ReadKeyOrButton then         {!!.11}
      cpGetKeyProc := ReadKeyWord;
    if @cpKeyPressedProc = @KeyOrButtonPressed then  {!!.11}
      cpKeyPressedProc := KeyPressed;

    {Load the rest of the DragProcessor fields}
    S.ReadRange(dpScreenMask, dpDummy);

    {Skip the rest if no mouse}
    if not MouseInstalled then
      Exit;

    {Set the default mouse cursor}
    SoftMouseCursor(dpScreenMask, dpDefaultMouseCursor);
  end;

  procedure DragProcessor.Store(var S : IdStream);
    {-Store in a stream}
  begin
    {Store the underlying CommandProcessor}
    CommandProcessor.Store(S);
    if S.PeekStatus <> 0 then
      Exit;

    {Store the rest of the DragProcessor fields}
    S.WriteRange(dpScreenMask, dpDummy);
  end;

  procedure DragProcessorStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing drag processors}
  begin
    CommandProcessorStream(SPtr);
    with SPtr^ do
      RegisterType(otDragProcessor, veDragProcessor,
                   TypeOf(DragProcessor),
                   @DragProcessor.Store, @DragProcessor.Load);
  end;
{$ENDIF}

  function DragProcessor.cpKeyPressed : Boolean;
    {-Called to see if keys are pending}
  begin
    {!!.12} {Assure background tasks get called regularly}
    if CommandProcessor.cpKeyPressed then
      cpKeyPressed := True
    else if MouseEventsPending then
      cpKeyPressed := True
    else
      cpKeyPressed := False;
  end;

  function DragProcessor.cpGetKey : Word;
    {-Called to get next keystroke}
  begin
    repeat
      if MouseEventsPending then begin
        {Read event from mouse queue}
        cpGetKey := ReadMouseEvent(MouseKeyWordX, MouseKeyWordY);
        Exit;
      end else if CommandProcessor.cpKeyPressed then begin
        {Read key from keyboard buffer}
        cpGetKey := CommandProcessor.cpGetKey;
        Exit;
      end;
      {$IFDEF VirtualPascal}
      // Free timeslices if no action required
      SysCtrlSleep( 31 );
      {$ELSE}
      inline($CD/$28); {!!.30}
      {$ENDIF}
    until False;
  end;

  procedure DragProcessor.cpOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    CommandProcessor.cpOptionsOn(OptionFlags and not cpEnableMouse);
    if FlagIsSet(OptionFlags, cpEnableMouse) then {!!.11}
      SetFlag(cpOptions, cpEnableMouse);          {!!.11}
  end;

  procedure DragProcessor.cpOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    CommandProcessor.cpOptionsOff(OptionFlags and not cpEnableMouse);
    if FlagIsSet(OptionFlags, cpEnableMouse) then {!!.11}
      ClearFlag(cpOptions, cpEnableMouse);        {!!.11}
  end;

  {----------------------------------------------------------------------}

  {$IFDEF UseAdjustableWindows}
  function HandleMousePress(var M : CommandWindow) : Byte;
  var
    DP : DragProcessorPtr;
    Dummy : LongInt;
    FramePos : FramePosType;
    HotCode : Byte;
    XAbs : Byte;
    YAbs : Byte;
    MicH : Integer;
    MicV : Integer;
    DH : Integer;
    DV : Integer;

    function Delta(I : Integer) : Integer;
      {-Convert integer to -1, 0, or 1}
    begin
      if I < 0 then
        Delta := -1
      else if I > 0 then
        Delta := 1
      else
        Delta := 0;
    end;

    function MickeyToChar(I : Integer) : Integer;
      {-Convert mickeys to character coordinates}
    var
      Mics : Integer;
    begin
      Mics := I div DP^.dpMickeyRatio;
      {if (Mics = 0) and (I <> 0) then} {!!.20}
        {Don't ignore small movements}
        {MickeyToChar := Delta(I)}      {!!.20}
      {else}                            {!!.20}
        MickeyToChar := Mics;
    end;

    function CheckCoordinates(DH, DV : Integer; Resize : Boolean) : Boolean;
    var
      NX1, NY1, NX2, NY2 : Byte;
    begin
      M.Coordinates(NX1, NY1, NX2, NY2);
      Inc(NX2, DH);
      Inc(NY2, DV);
      if not Resize then begin
        Inc(NX1, DH);
        Inc(NY1, DV);
      end;
      CheckCoordinates :=
        M.rwValidCoords(NX1, NY1, NX2, NY2, NX1, NY1, NX2, NY2);
    end;

    procedure Adjust(MouseShape : Word; Resize : Boolean);
      {-Move or resize the window using a mouse}
    var
      MXL, MYL, MXH, MYH : Byte;
      Valid : Boolean;
      SavH, SavV : Integer;                                             {!!.21}
    begin
      {save current mouse window coordinates}
      MXL := MouseXLo;
      MYL := MouseYLo;
      MXH := MouseXHi;
      MYH := MouseYHi;

      {read mickey count once to clear internal counters}
      GetMickeyCount(MicH, MicV);
      SavH := 0;  SavV := 0;                                            {!!.21}

      {lock the mouse cursor at its current position}
      MouseWindow(XAbs, YAbs, XAbs, YAbs);

      {change to specialized mouse cursor shape}
      SoftMouseCursor(DP^.dpScreenMask, MouseShape);

      {loop while button remains down}
      while MousePressed do begin
        {see how much mouse moved}
        GetMickeyCount(MicH, MicV);
        Inc(SavH, MicH);                                                {!!.21}
        Inc(SavV, MicV);                                                {!!.21}
        DH := MickeyToChar(SavH);                                       {!!.21}
        DV := MickeyToChar(SavV);                                       {!!.21}

        {DH := MickeyToChar(MicH);}                                     {!!.21}
        {DV := MickeyToChar(MicV);}                                     {!!.21}

        {tickle the background task}
        if DP^.cpKeyPressed then ; {!!.12}

        if (DH <> 0) or (DV <> 0) then begin
          {mouse moved a significant amount}
          SavH := 0;  SavV := 0;                                        {!!.21}

          {convert mouse pos to frame-relative}
          Dec(XAbs, M.wFrame.FrXH);
          Dec(YAbs, M.wFrame.FrYH);

          {slow movement near limits}
          Valid := CheckCoordinates(DH, DV, Resize);
          if not Valid then begin
            DH := Delta(DH);
            DV := Delta(DV);
            Valid := CheckCoordinates(DH, DV, Resize);
          end;

          if Valid then
            if Resize then
              M.ResizeWindow(DH, DV)
            else
              M.MoveWindow(DH, DV);
          M.ClearErrors;

          {convert mouse pos back to absolute}
          Inc(XAbs, M.wFrame.FrXH);
          Inc(YAbs, M.wFrame.FrYH);

          {lock the mouse cursor at its new position}
          MouseWindow(XAbs, YAbs, XAbs, YAbs);
          MouseGoToXY(1, 1);
        end;
      end;

      {restore original mouse shape, window, position}
      SoftMouseCursor(DP^.dpScreenMask, DP^.dpDefaultMouseCursor);
      MouseWindow(MXL+1, MYL+1, MXH, MYH);
      MouseGoToXY(XAbs-MXL, YAbs-MYL);

      {empty out the event queue}
      ClearMouseEvents;
    end;

  begin
    {assure the command processor supports dragging}
    DP := DragProcessorPtr(M.cwCmdPtr);
    {if TypeOf(DP^) <> TypeOf(DragProcessor) then begin}       {!!.20}
    if not FlagIsSet(DP^.cpOptions, cpMouseDrag) then begin    {!!.20}
      HandleMousePress := hsNone;
      Exit;
    end;

    {get absolute mouse position and see where it falls in the window}
    XAbs := MouseKeyWordX+MouseXLo;
    YAbs := MouseKeyWordY+MouseYLo;
    M.EvaluatePos(XAbs, YAbs);
    Dummy := M.PosResults(FramePos, HotCode);
    HandleMousePress := HotCode;

    {evaluate the logical position}
    case HotCode of
      MoveHotCode : if not M.IsZoomed then
                      Adjust(DP^.dpMoveMouseCursor, False);
      ResizeHotCode : if not M.IsZoomed then
                        Adjust(DP^.dpResizeMouseCursor, True);
      ZoomHotCode : begin
                      if M.IsZoomed then
                        M.UnZoom
                      else
                        M.Zoom;
                      while MousePressed do {wait} ;
                      ClearMouseEvents;
                    end;
      CloseHotCode :                 {!!.30}
        begin                        {!!.30}
          M.SetLastCommand(ccQuit);  {!!.30}
          while MousePressed do ;    {!!.30}
          ClearMouseEvents;          {!!.30}
        end;                         {!!.30}
    end;
  end;
  {$ENDIF}

begin
  SaveExit := ExitProc;
  ExitProc := @MouseExit;
  ClearMouseEvents;
  @ChainedEventHandler := nil;
  if MouseInstalled then
    InstallISRs;
end.

