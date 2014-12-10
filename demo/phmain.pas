{$R-,S-,V-,F+,A-,I-}

{*********************************************************}
{*                   PHMAIN.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{.$DEFINE NoPop}  {!!.03}   {If the following directive is defined, POPHELP
                             will be compiled as a non-resident program. The
                             state of this define MUST agree with the
                             identically named define in POPHELP.PAS}

{$IFDEF Dpmi}     {!!.20}   {Force non-resident compile if in pmode}
  {$DEFINE NoPop}
{$ENDIF}


{$IFNDEF NoPop}   {!!.03}
  {$DEFINE EnableCutPaste}  {Define to enable block cut/paste from help to
                             an underlying editor.  Increases resident kernel
                             by approximately 2K bytes.  The state of this
                             define MUST agree with the identically named
                             define in POPHELP.PAS}
{$ENDIF}          {!!.03}

{.$DEFINE CheckStack}

{***************************************************************************
 This program requires that OPDEFINE.INC activate the following defines:
   UseAdjustableWindows, UseMouse
 This program is not designed to work with the following define:
   UseDrag
 This program uses less code space if OPDEFINE.INC deactivates the following:
   UseHotSpots, UseScrollBars, UseShadows
 ***************************************************************************}

{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFDEF UseDrag} {!!.10}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

unit PHMain;

interface

uses
  Dos,
  OpInline,
  OpDos,
  OpString,
  OpCrt,
  OpConst,
  OpRoot,
  OpMouse,
  OpFrame,
  OpWindow,
  OpCmd,
  OpPick,
  OpHelp,
  {$IFNDEF NoPop}                 {!!.03}
  OpSwap1,
  {$ENDIF}                        {!!.03}
  OpSwitch;

const
  {Installation area}
  DBName       : PathStr = 'OPRO.HLP';  {Default help file to load}

  HeapOverHead : Word = $1300;          {Total heap overhead}

{$IFNDEF NoPop}                 {!!.03}
  {Swap file names}
  Swap1Name  : String[65] = 'POPHELP.SW1'; {Length limited by OPSWAP1}
  Swap2Name  : String[65] = 'POPHELP.SW2';

  {Popup hot keys}
  HotKeyScreen : Word = $023B;    {LShift-F1}
  HotKeyLast   : Word = $023C;    {LShift-F2}
  HotKeyIndex  : Word = $023D;    {LShift-F3}
  HotKeyNameScreen : String[19] = '<Left Shift><F1>';
  HotKeyNameLast   : String[19] = '<Left Shift><F2>';
  HotKeyNameIndex  : String[19] = '<Left Shift><F3>';
{$ENDIF}                        {!!.03}

  {Function Key line for bottom frame}                              {!!.01}
  FKeyHdrLen = 78;                                                  {!!.01}
  FKeyHdr : String[FKeyHdrLen] =                                    {!!.01}
  ' F1-Index AltF1-Prev F4-Paste F5-Zoom F6-Resize F7-BlkBegin F8-BlkEnd F9-Next ';

  {Last found topic and whether it was a full or partial match}
  SaveLookupString : String[128] = '';
  LookupPartial : Boolean = False;
  LookupByTopic : Boolean = True;  {!!.13}

  {Command keys within the popup}
  NewDBKey : Word = $3D00;        {F3}
  MoveKey1 : Word = $4000;        {F6}
  MoveKey2 : Word = $3200;        {Alt-M}
  ZoomKey1 : Word = $3F00;        {F5}
  ZoomKey2 : Word = $2C00;        {Alt-Z}
  NextKey1 : Word = $4300;        {F9}
  NextKey2 : Word = $000C;        {^L}
  ExitKey1 : Word = $2D00;        {Alt-X}                 {!!.01}
  TextKey1 : Word = $3C00;        {F2}                    {!!.13}

  InitHelpX : Byte = 2;        {Initial help window (not inc. frame) position}
  InitHelpY : Byte = 2;
  InitHelpWidth : Byte = 78;
  InitHelpHeight : Byte = 10;
  MinWidth = 20;
  MinHeight = 5;

  ResidentMacroSize = 512;     {Size of resident keyboard string}

  {Help options for PopHelp}
  HelpOpts = hwPickIndex + hwAdjustScroll +
             hwStaticNameBuffer
             {$IFDEF EnableCutPaste}
             + hwBlockMark
             {$ENDIF}
             + hwHighlightXref; {!!.13}

  {Window options for PopHelp}
  HelpWinOpts = wClear + wResizeable + wUserContents + wBordered; {!!}

  HelpColorSet : ColorSet = (
    TextColor       : $30; TextMono       : $07;
    CtrlColor       : $30; CtrlMono       : $07;
    FrameColor      : $71; FrameMono      : $07;
    HeaderColor     : $71; HeaderMono     : $70;
    ShadowColor     : $30; ShadowMono     : $07;
    HighlightColor  : $30; HighlightMono  : $07;
    PromptColor     : $30; PromptMono     : $07;
    SelPromptColor  : $30; SelPromptMono  : $07;
    ProPromptColor  : $30; ProPromptMono  : $07;
    FieldColor      : $30; FieldMono      : $07;
    SelFieldColor   : $30; SelFieldMono   : $07;
    ProFieldColor   : $30; ProFieldMono   : $07;
    ScrollBarColor  : $30; ScrollBarMono  : $07;
    SliderColor     : $30; SliderMono     : $07;
    HotSpotColor    : $30; HotSpotMono    : $07;
    BlockColor      : $1E; BlockMono      : $70;
    MarkerColor     : $30; MarkerMono     : $07;
    DelimColor      : $30; DelimMono      : $07;
    SelDelimColor   : $30; SelDelimMono   : $07;
    ProDelimColor   : $30; ProDelimMono   : $07;
    SelItemColor    : $5E; SelItemMono    : $70;
    ProItemColor    : $30; ProItemMono    : $07;
    HighItemColor   : $30; HighItemMono   : $07;
    AltItemColor    : $30; AltItemMono    : $07;
    AltSelItemColor : $30; AltSelItemMono : $07;
    FlexAHelpColor  : $1F; FlexAHelpMono  : $70;
    FlexBHelpColor  : $3E; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $31; FlexCHelpMono  : $09;
    UnselXrefColor  : $3B; UnselXrefMono  : $01;
    SelXrefColor    : $5E; SelXrefMono    : $70;
    MouseColor      : $30; MouseMono      : $07
  );

type
  MacroKeyArray = array[1..ResidentMacroSize+1] of Word;

{$IFDEF EnableCutPaste}
var
  {Pointers to OpReplay procedures}
  CallStartMacro : procedure(P : Pointer);
  CallStartChaining : procedure;
  CallStopChaining : procedure;
  CallWaitInit : procedure(OneMsec, WaitCnt : Word; UserDataPtr : Pointer);
  CallCharToMacro : function(C : Char) : Word;

  {Pointers to CS data}
  BufferReadyPtr : Pointer;
  ResidentMacroPtr  : ^MacroKeyArray;
  OrigInt16Addr : Pointer;
{$ENDIF}

procedure LoadPopHelp;
  {-Called from main program to get us going}

  {========================================================================}

implementation

type
  StringPtr = ^String;
  SegOfs =
    record
      O, S : Word;
    end;
  String127 = String[127];

const
  ProgName   : String[8] = 'POPHELP';
  Version    : String[5] = '1.30';
  TwoMonitors: Boolean = False;               {True if using dual monitors}
  LeaveDisplay : Boolean = False;             {True if help stays visible}
  BiosCursor : Boolean = False;               {True to control cursor with BIOS}
  MinPartialMatch = 4;                        {Min length for partial matches}

  {Default options}
  GoingResident : Boolean = True;
  UnloadRequest : Boolean = False;
  UseSwapping   : Boolean = True;

  {Convenient synonyms}
  HKSNewDBCmd     = ccUser0;
  HKSMoveCmd      = ccUser1;
  HKSZoomCmd      = ccUser2;
  HKSNextCmd      = ccUser3;
  HKSTextCmd      = ccUser4;
  UpArrow         = $4800;
  LeftArrow       = $4B00;
  RightArrow      = $4D00;
  DownArrow       = $5000;
  PageUp          = $4900;
  PageDown        = $5100;
  CtrlLeftArrow   = $7300;
  CtrlRightArrow  = $7400;
  ShiftUpArrow    = $4838;
  ShiftLeftArrow  = $4B34;
  ShiftRightArrow = $4D36;
  ShiftDownArrow  = $5032;

  EnterKey        = $1C0D;

  {External IFC Commands/Results}
  {$IFDEF EnableCutPaste}
  PasteBlock = 0;
  {$ENDIF}
  UnloadTSR = 1;
  UnloadSuccessful = 2;
  UnloadFailed = 3;

  {$IFDEF EnableCutPaste}
  {Wait time for TSR to swap back in and fill a new buffer}
  MacroWaitMsec = 10000;
  {$ENDIF}

var
  Help : ScrollingHelpWindowPtr;           {Help window}
  SaveLastTopic : Word;                    {Last help topic}
  CursorX : Byte;                          {Cursor location at popup time}
  CursorY : Byte;                          {Cursor location at popup time}
  CommandToExec : String[127];
  PromptWindowPtr : Pointer;               {Ptr to covers buffer for prompt}

  SaveVecs : Array[0..17] of Pointer;      {For custom saving of vectors}
  PopupStack : array[1..4000] of Byte;     {Resident stack}

  {$IFDEF EnableCutPaste}
  HelpBuf : CharArrayPtr;                  {Pointer to marked help text}
  BufInx : Word;                           {Current index into marked block}
  TotalLen : Word;                         {Length of marked help text}
  {$ENDIF}

  SaveXY : Word;                           {Saved info for prompt window} {!!.13}
  SaveSL : Word;                                                          {!!.13}
  SaveWC : WindowCoordinates;

  {Mouse save area}
  MSP : MouseStatePtr;
  MSPsize : Word;

  {Header indexes}
  IndexZoomHdr : Word;
  HelpZoomHdr : Word;
  FKeyHdr1 : Word;                                               {!!.01}
  FKeyHdr2 : Word;                                               {!!.01}

  procedure Beep(Hertz, Millisecs : Word);
    {-Emit a tone}
  begin
    Sound(Hertz);
    Delay(Millisecs);
    NoSound;
  end;

  function SetBlock(var Paragraphs : Word) : Boolean;
    {-Change size of DOS memory block allocated to this program}
  var
    IRR : Registers;
  begin
    with IRR do begin
      AH := $4A;
      ES := PrefixSeg;
      BX := Paragraphs;
      MsDos(IRR);
      Paragraphs := BX;
      SetBlock := not Odd(Flags);
    end;
  end;

  function PtrDiff(H, L : Pointer) : LongInt;
    {-Return the number of bytes between H^ and L^. H is the higher address}
  var
    High : SegOfs absolute H;
    Low : SegOfs absolute L;
  begin
    PtrDiff := (LongInt(High.S) shl 4+High.O)-(LongInt(Low.S) shl 4+Low.O);
  end;

{$IFNDEF Dpmi} {!!.20}

{$IFDEF Heap6} {New version of ShrinkHeap for TP6}

  function ShrinkHeap(UserParas : Word) : Boolean;
    {-Shrinks the heap to provide Paras free DOS memory}
  var
    NewFreePtr : Pointer;
    ParasWeHave : Word;
    ParasToKeep : Word;
    FreeListSize : Word;
  begin
    ShrinkHeap := False;

    {Paragraphs currently allocated to program}
    ParasWeHave := SegOfs(HeapEnd).S-PrefixSeg;

    {Paragraphs we need to keep}
    ParasToKeep := (PtrDiff(HeapPtr,Ptr(PrefixSeg,0)) shr 4) + 16 + UserParas;

    {Deallocate memory for DOS}
    if not SetBlock(ParasToKeep) then
      Exit;

    {Note the new end of heap}
    HeapEnd := Ptr(PrefixSeg+ParasToKeep, 0);

    {Adjust the PSP record of the top of memory}
    MemW[PrefixSeg:2] := SegOfs(HeapEnd).S;

    ShrinkHeap := True;
  end;

{$ELSE}

  function EndOfHeap : Pointer;
    {-Returns a pointer to the end of the free list}
  var
    FreeSegOfs : SegOfs absolute FreePtr;
  begin
    if FreeSegOfs.O = 0 then
      {the free list is empty, add $1000 to the segment}
      EndOfHeap := Ptr(FreeSegOfs.S+$1000, 0)
    else
      EndOfHeap := Ptr(FreeSegOfs.S+(FreeSegOfs.O shr 4), 0);
  end;

  function ShrinkHeap(UserParas : Word) : Boolean;
    {-Shrinks the heap to provide Paras free DOS memory}
  var
    TopOfHeap : Pointer;
    NewFreePtr : Pointer;
    ParasWeHave : Word;
    ParasToKeep : Word;
    ParasToGive : Word;
    FreeListSize : Word;
  begin
    ShrinkHeap := False;

    {Pointer to next address past program}
    TopOfHeap := Ptr(SegOfs(FreePtr).S+$1000, 0);

    {Paragraphs currently allocated to program}
    ParasWeHave := SegOfs(TopOfHeap).S-PrefixSeg;

    {Paragraphs we need to keep}
    ParasToKeep := (PtrDiff(HeapPtr,Ptr(PrefixSeg,0)) shr 4) + 16 + UserParas;

    {Paragraphs we want to give away}
    ParasToGive := ParasWeHave - ParasToKeep;

    {Assure space free at top of heap}
    if PtrDiff(EndOfHeap, HeapPtr) shr 4 < ParasToGive then
      Exit;

    {Size of free list to move}
    FreeListSize := PtrDiff(TopOfHeap, EndOfHeap);

    {Adjust free list down}
    NewFreePtr := Ptr(SegOfs(FreePtr).S-ParasToGive, SegOfs(FreePtr).O);
    if FreeListSize > 0 then
      Move(FreePtr^, NewFreePtr^, FreeListSize);
    FreePtr := NewFreePtr;

    {Deallocate memory for DOS}
    Dec(ParasWeHave, ParasToGive);
    if not SetBlock(ParasWeHave) then
      Exit;

    {Adjust the PSP record of the top of memory}
    MemW[PrefixSeg:2] := SegOfs(FreePtr).S+$1000;

    ShrinkHeap := True;
  end;

{$ENDIF}

{$ENDIF}  {!!.20}

  procedure SwapVectorsOut;
    {-Custom swap vectors}
  begin
    GetIntVec($00, SaveVecs[1]);
    GetIntVec($02, SaveVecs[2]);
    GetIntVec($1B, SaveVecs[0]);
    GetIntVec($23, SaveVecs[3]);
    GetIntVec($24, SaveVecs[4]);
    GetIntVec($34, SaveVecs[5]);
    GetIntVec($35, SaveVecs[6]);
    GetIntVec($36, SaveVecs[7]);
    GetIntVec($37, SaveVecs[8]);
    GetIntVec($38, SaveVecs[9]);
    GetIntVec($39, SaveVecs[10]);
    GetIntVec($3A, SaveVecs[11]);
    GetIntVec($3B, SaveVecs[12]);
    GetIntVec($3C, SaveVecs[13]);
    GetIntVec($3D, SaveVecs[14]);
    GetIntVec($3E, SaveVecs[15]);
    GetIntVec($3F, SaveVecs[16]);
    GetIntVec($75, SaveVecs[17]);

    SetIntVec($00, SaveInt00);
    SetIntVec($02, SaveInt02);
    SetIntVec($1B, SaveInt1B);
    SetIntVec($23, SaveInt23);
    SetIntVec($24, SaveInt24);
    SetIntVec($34, SaveInt34);
    SetIntVec($35, SaveInt35);
    SetIntVec($36, SaveInt36);
    SetIntVec($37, SaveInt37);
    SetIntVec($38, SaveInt38);
    SetIntVec($39, SaveInt39);
    SetIntVec($3A, SaveInt3A);
    SetIntVec($3B, SaveInt3B);
    SetIntVec($3C, SaveInt3C);
    SetIntVec($3D, SaveInt3D);
    SetIntVec($3E, SaveInt3E);
    SetIntVec($3F, SaveInt3F);
    SetIntVec($75, SaveInt75);
  end;

  procedure SwapVectorsIn;
    {-Custom swap vectors}
  begin
    SetIntVec($00, SaveVecs[1]);
    SetIntVec($02, SaveVecs[2]);
    SetIntVec($1B, SaveVecs[0]);
    SetIntVec($23, SaveVecs[3]);
    SetIntVec($24, SaveVecs[4]);
    SetIntVec($34, SaveVecs[5]);
    SetIntVec($35, SaveVecs[6]);
    SetIntVec($36, SaveVecs[7]);
    SetIntVec($37, SaveVecs[8]);
    SetIntVec($38, SaveVecs[9]);
    SetIntVec($39, SaveVecs[10]);
    SetIntVec($3A, SaveVecs[11]);
    SetIntVec($3B, SaveVecs[12]);
    SetIntVec($3C, SaveVecs[13]);
    SetIntVec($3D, SaveVecs[14]);
    SetIntVec($3E, SaveVecs[15]);
    SetIntVec($3F, SaveVecs[16]);
    SetIntVec($75, SaveVecs[17]);
  end;

  function Execute(Command : String127; ExtraParas : Word) : Word;
    {-Execute a child process}
  var
    ComPath : PathStr;
  begin
{$IFNDEF Dpmi}  {!!.20}
    if not ShrinkHeap(ExtraParas) then begin
      Execute := $FFFF;
      Exit;
    end;
{$ENDIF}  {!!.20}

    ComPath := GetEnv('COMSPEC');
    Command := '/C '+Command;

    SwapVectorsOut;
    Exec(ComPath, Command);
    SwapVectorsIn;

    Execute := DosError;

  end;

 {$IFDEF EnableCutPaste}
  procedure PasteNextBlock;
    {-Fill and paste the next help text block}
  var
    Len, Cnt : Word;
  begin
    {Filter the block of characters into the ResidentMacro buffer}
    Cnt := 2;
    while (Cnt <= ResidentMacroSize) and (BufInx < TotalLen) do begin
      case HelpBuf^[BufInx] of
        ^A,^B,^C,^E : ;              {Skip flex and highlight commands}
        ^D          : Inc(BufInx,2); {Skip over xref indices}   {!!.01}
        else
          if HelpBuf^[BufInx] > ^E then begin
            ResidentMacroPtr^[Cnt] := CallCharToMacro(HelpBuf^[BufInx]);
            Inc(Cnt);
          end;
      end;
      Inc(BufInx);
    end;
    ResidentMacroPtr^[Cnt] := $0FFFF;

    {Set chaining}
    if BufInx = TotalLen then
      CallStopChaining
    else
      CallStartChaining;

    {Tell OpReplay a new macro is ready}
    CallStartMacro(ResidentMacroPtr);

    {Set semaphore for GetMoreBytes (in case it's waiting)}
    Byte(BufferReadyPtr^) := 1;
  end;

  function PasteHelpText(T : CharArrayPtr; Len : Word;
                         SH : ScrollingHelpWindowPtr) : Boolean;
    {-Pastes text from help to underlying editor}
  begin
    {Set global vars}
    HelpBuf := T;
    BufInx := 0;
    TotalLen := Len;

    {Make sure wait values are set}
    CallWaitInit(OpCrt.OneMs, MacroWaitMsec, @CSSwapData^.ThisIFC.UserData);

    {Paste the first block}
    PasteNextBlock;

    {Tell Help what to do}
    SH^.HideBlock;
    SH^.Draw;
    PasteHelpText := True;
  end;
 {$ENDIF}

  procedure AdjustFkeyHeader(NewWidth : Byte);                {!!.01}
    {-Adjust FKey Header to truncate at complete words}
  var
    S : String[FKeyHdrLen];
    I : Byte;
    ReDraw : Boolean;
  begin
    with Help^ do begin
      {Adjust FKey header}
      if NewWidth > FKeyHdrLen then
        NewWidth := FKeyHdrLen;
      if NewWidth <= FkeyHdrLen then begin
        I := NewWidth;
        while (I > MinWidth) and (FKeyHdr[I] <> ' ') do
          Dec(I);
        FKeyHdr[0] := Char(I);
      end;
      S := Pad(FKeyHdr, NewWidth);
      {Change the Fkey header on the current frame}
      wFrame.ChangeHeaderString(FKeyHdr1, S, ReDraw);
      hwFrame.ChangeHeaderString(FKeyHdr2, S, ReDraw);
    end;
  end;

  function KeyboardMove : Boolean;
    {-Move a window using the keyboard. Returns False if insufficient memory.}
  const
    Step = 1;
    SpeedStepH = 10;
    SpeedStepV = 4;
    Title : string[50] = ' '#27#24#25#26' to move, '+ {!!.01}
                         'Shift '#27#24#25#26' to resize, '+ {!!.01}
                         #17#217' to accept ';
  var
    AllDone : Boolean;
    Redraw : Boolean;
    SaveHeader : String;
    HP : HeaderPtr;
    MoveIt : Boolean;
    XL, YL, XH, YH : Byte;

    procedure SetMove(DX, DY : Integer);
    begin
      Inc(XL, DX);
      Inc(YL, DY);
      Inc(XH, DX);
      Inc(YH, DY);
    end;
    procedure SetResize(DX, DY : Integer);
    begin
      Inc(XH, DX);
      Inc(YH, DY);
    end;

  begin
    KeyboardMove := True;
    AllDone := False;

    {Leave immediately if window is zoomed}
    if Help^.IsZoomed then
      Exit;

    {Change to different window title}
    HP := HeaderPtr(Help^.wFrame.frHeaders.Head);
    SaveHeader := HP^.heName^;
    Help^.wFrame.ChangeHeaderString(0, Title, ReDraw);
    Help^.wFrame.Draw;

    {Remove "more" header while resizing}
    Help^.wFrame.DisableHeader(1, True);                           {!!.01}
    Help^.wFrame.UpdateFrame;                                      {!!.01}

    {Move and/or resize the window}
    with Help^ do
      repeat
        MoveIt := True;
        Coordinates(XL, YL, XH, YH);
        case ReadKeyWord of
          {Normal moves (always in jumps)}
          UpArrow    : SetMove(0, -SpeedStepV);
          LeftArrow  : SetMove(-SpeedStepH, 0);
          RightArrow : SetMove(SpeedStepH, 0);
          DownArrow  : SetMove(0, SpeedStepV);

          {Normal resize}
          ShiftUpArrow    : SetResize(0, -Step);
          ShiftLeftArrow  : SetResize(-Step, 0);
          ShiftRightArrow : SetResize(Step, 0);
          ShiftDownArrow  : SetResize(0, Step);

          {Speed resize}
          PageUp         : SetResize(0, -SpeedStepV);
          CtrlLeftArrow  : SetResize(-SpeedStepH, 0);
          CtrlRightArrow : SetResize(SpeedStepH, 0);
          PageDown       : SetResize(0, SpeedStepV);

          EnterKey : AllDone := True;

          else
            MoveIt := False;
        end;

        if MoveIt and not AllDone then begin
          {Limit check edges}
          if ShortInt(XL) <= 1 then
            SetMove(-(XL)+2, 0);
          if ShortInt(YL) <= 1 then
            SetMove(0, -(YL)+2);
          if XH > ScreenWidth-1 then
            SetMove((ScreenWidth-XH-1), 0);
          if YH > ScreenHeight-1 then
            SetMove(0, ScreenHeight-YH-1);

          {Truncate (or extend) the Fkey header by complete words}
          AdjustFkeyHeader(Succ(XH - XL));

          {Adjust (move or resize) the window}
          AdjustWindow(XL, YL, XH, YH);
          if GetLastError = wOutOfHeap then begin
            KeyboardMove := False;
            Beep(1100, 100);
            Exit;
          end;
        end;
      until AllDone;

    {Restore the original header}
    Help^.wFrame.ChangeHeaderString(0, SaveHeader, ReDraw);
    Help^.wFrame.DisableHeader(1, False);                        {!!.01}
    Help^.wFrame.Draw;
  end;

  procedure SetZoomHeaders(Zoomed : Boolean);
    {-Sets zooms headers to passed state}
  begin
    with Help^ do begin
      wFrame.DisableHeader(IndexZoomHdr, Zoomed);
      hwFrame.DisableHeader(HelpZoomHdr, Zoomed);
    end;
  end;

  function ToggleZoom : Boolean;
    {Zoom/Unzoom the help window}
  var
    Success : Boolean;
    Zoomed : Boolean;
    S : String[FKeyHdrLen];
  begin
    with Help^ do begin
      Zoomed := IsZoomed;
      {Set zoom headers for new state}
      SetZoomHeaders(Zoomed);

      {Disable fkey headers temporarily}
      wFrame.DisableHeader(FKeyHdr1, True);                           {!!.03}
      hwFrame.DisableHeader(FKeyHdr2, True);                          {!!.03}

      {Zoom/unzoom the window, new headers will be applied}
      if IsZoomed then
        Unzoom
      else
        Zoom;

      {Did it work?}
      if not (GetLastError = 0) then begin
        {Zoom failed, restore headers and beep}
        SetZoomHeaders(not Zoomed);
        Beep(1100, 100);
        ToggleZoom := False;
      end else
        ToggleZoom := True;

      {Reenable FKey headers}                                         {!!.03}
      wFrame.DisableHeader(FKeyHdr1, False);                          {!!.03}
      hwFrame.DisableHeader(FKeyHdr2, False);                         {!!.03}

      {Make sure FKey header is drawn for full width of window}       {!!.01}
      AdjustFKeyHeader(Width);                                        {!!.01}
      wFrame.DrawHeader(FKeyHdr1);                                    {!!.01}
    end;
  end;

  procedure ClearPromptWindow;
  var
    S : PathStr;
  begin
    FillChar(S[1], SizeOf(S)-1, ' ');
    S[0] := #78;
    FastWrite(S, 13, 2,
              ColorMono(HelpColorSet.TextColor, HelpColorSet.TextMono));
  end;

  procedure ErrorPromptWindow(Msg : PathStr);
    {-Report error in the prompt window}
  const
    Press : string[18] = '. Press any key...';
  var
    KW : Word;
  begin
    ClearPromptWindow;
    Msg := Msg+Press;
    if Length(Msg) > 77 then
      Msg[0] := #77;
    FastWrite(Msg, 13, 2,
              ColorMono(HelpColorSet.TextColor, HelpColorSet.TextMono));
    NormalCursor;
    GotoXYAbs(2+Length(Msg), 13);
    KW := ReadKeyWord;
  end;

  procedure OpenPromptWindow(Title : String);
    {-Save underlying screen and frame prompt window}
  begin
    {Save underlying screen}
    if not SaveWindow(1, 12, 80, 14, False, PromptWindowPtr) then
      {Should never fail}
      Exit;
    GetCursorState(SaveXY, SaveSL);
    StoreWindowCoordinates(SaveWC);

    {Draw new prompt window}
    FrameWindow(1, 12, 80, 14,
                ColorMono(HelpColorSet.FrameColor, HelpColorSet.FrameMono),
                ColorMono(HelpColorSet.HeaderColor, HelpColorset.HeaderMono),
                Title);
    ClearPromptWindow;
    Window(1, 1, ScreenWidth, ScreenHeight);

    {Prepare for a prompt}
    TextAttr := ColorMono(HelpColorSet.FlexBHelpColor, HelpColorSet.FlexBHelpMono);
  end;

  procedure ReadPromptString(Prompt : String; var S : String);
    {-Read a string in the prompt window}
  begin
    GotoXYAbs(2, 13);
    Write(Prompt);
    NormalCursor;
    OpCrt.BufLen := 64;
    TextAttr := ColorMono(HelpColorSet.TextColor, HelpColorSet.TextMono);
    ReadLn(S);
    HiddenCursor;
  end;

  procedure ClosePromptWindow;
    {-Restore screen state under prompt window}
  begin
    RestoreWindowCoordinates(SaveWC);
    RestoreWindow(1, 12, 80, 14, False, PromptWindowPtr);
    RestoreCursorState(SaveXY, SaveSL);
  end;

  procedure SetHelpWindowLimits;
    {-Set Limits for the help window based on the current screen size}
  begin
    with Help^ do begin
      {For the "normal" window}
      SetSizeLimits(MinWidth, MinHeight, ScreenWidth, ScreenHeight);
      SetFrameLimits(MinWidth, MinHeight, ScreenWidth, ScreenHeight);
      SetPosLimits(1, 1, ScreenWidth, ScreenHeight);

      {For the hwFrame}
      hwFrame.SetSizeLimits(MinWidth, MinHeight, ScreenWidth, ScreenHeight);
      hwFrame.SetClipLimits(1, 1, ScreenWidth, ScreenHeight);
    end;
  end;

  procedure CustomizeHelpWindow;
    {Add embellishments to the help window}
  begin
    with Help^ do begin
      hwOptionsOn(HelpOpts);
      SetSearchMode(PickAltStringSearch);
      AddTopicHeader(1, 60, heTC);
      wFrame.AddHeader(' Topic Index ', heTC);
      AddMoreHelpHeader(' || ', heTL, #24, #25, '', 2, 3, 0);
      AddMoreHeader(' || ', heTL, #24, #25, '', 2, 3, 0);
      wFrame.AddCustomHeader(' Zoomed ', frTR, -10, 0,
                             HelpColorSet.HeaderColor, HelpColorSet.HeaderMono);
      IndexZoomHdr := wFrame.GetLastHeaderIndex;
      wFrame.DisableHeader(IndexZoomHdr, True);
      hwFrame.AddCustomHeader(' Zoomed ', frTR, -10, 0,
                              HelpColorSet.HeaderColor, HelpColorSet.HeaderMono);
      HelpZoomHdr := hwFrame.GetLastHeaderIndex;
      hwFrame.DisableHeader(HelpZoomHdr, True);
      wFrame.AddCustomHeader(FKeyHdr, frBL, 1, 0,                  {!!.01}
                              HelpColorSet.HeaderColor, HelpColorSet.HeaderMono);
      FKeyHdr1 := wFrame.GetLastHeaderIndex;                       {!!.01}
      hwFrame.AddCustomHeader(FKeyHdr, frBL, 1, 0,                 {!!.01}
                              HelpColorSet.HeaderColor, HelpColorSet.HeaderMono);
      FKeyHdr2 := hwFrame.GetLastHeaderIndex;                      {!!.01}
      {$IFDEF EnableCutPaste}
      SetSendFunc(PasteHelpText);
      SetBlockAttr(HelpColorSet.BlockColor, HelpColorSet.BlockMono);
      {$ENDIF}
    end;
  end;

  procedure LoadHelpObject(var CurName: PathStr);
    {-Reload the previous help object (in DbName)}
  begin
    Help := New(ScrollingHelpWindowPtr,
                InitCustom(InitHelpX, InitHelpY,
                           InitHelpX+InitHelpWidth-1,
                           InitHelpY+InitHelpHeight-1,
                           HelpColorSet,
                           HelpWinOpts,
                           CurName,
                           PickHorizontal));

    if Help <> Nil then begin        {!!.01}
      CustomizeHelpWindow;
      SetHelpWindowLimits;
      SaveLastTopic := 0;
    end;                             {!!.01}
  end;

  procedure DatabaseMenu(var DBName : PathStr);
    {-Prompt for the name of the help database to use}
  var
    DBTemp : PathStr;
  begin
    {Erase the help screen}
    Help^.Erase;

    OpenPromptWindow(' New Help Database ');
    ReadPromptString('Enter name: ', DBTemp);
    DBTemp := Trim(DBTemp);

    if Length(DBTemp) > 0 then begin
      {Try to load the new file}
      DBTemp := StUpcase(DBTemp);
      DBTemp := DefaultExtension(DBTemp, 'HLP');
      if ExistOnPath(DBTemp, DBTemp) then begin
        {Clear entire heap (except for prompt cover buffer)}
        Dispose(Help, Done);
        wStack.Done;

        {Start a new help system}
        LoadHelpObject(DBTemp);
        if Help <> Nil then
          {Successful open}
          DBName := DBTemp
        else begin
          ErrorPromptWindow('Error '+Long2Str(InitStatus)+' loading '+DBTemp);
          LoadHelpObject(DBName);
        end;
      end else
        ErrorPromptWindow(DBTemp+' not found');
    end;

    ClosePromptWindow;
  end;

  procedure JumpAwayFromCursor;
    {-Positions the help window as far away from the cursor as possible}
  var
    HelpX, HelpY : Byte;
  begin
    if CursorX > ScreenWidth div 2 then
      HelpX := 2
    else
      HelpX := Pred(ScreenWidth+1-Help^.Width);
    if CursorY > ScreenHeight div 2 then
      HelpY := 2
    else
      HelpY := Pred(ScreenHeight+1-Help^.Height);

    {Move the inactive help window}
    Help^.AdjustWindow(HelpX, HelpY,
                       HelpX+Pred(Help^.Width), HelpY+Pred(Help^.Height));
  end;

  function ReinitVideo : Boolean;
    {-Initialize video in preparation for popping up}
  var
    Xlo, Ylo, Xhi, Yhi : Byte;
    Ht : Byte;
  begin
    ReinitVideo := False;

    {$IFDEF CheckStack}
    {Initialize the stack segment}
    FillChar(Mem[SSeg:0], SPtr-100, $AA);
    {$ENDIF}

    {Always do this stuff}
    ReinitCrt;
    if BiosCursor then begin
      CursorX := WhereXAbs;
      CursorY := WhereYAbs;
    end else
      WhereXYdirect(CursorX, CursorY);

    {Then check for dual monitors}
    if TwoMonitors then begin
      if MouseInstalled then begin
        SaveMouseState(MSP, False);
        InitializeMouse;
      end;
      SwitchScreens(Screen2);
      NormalCursor;
      ReinitVideo := True;
      Exit;
    end;

    {Continue for single monitor}
    if not InTextMode then begin
      Beep(400, 200);
      Exit;
    end;

    Help^.UpdateScreenSize;
    ReinitVideo := True;
  end;

  procedure ExitVideo;
    {-Clean up screen when exiting popup}
  {$IFDEF CheckStack}
  var
    I : Word;
  {$ENDIF}
  begin
    if not LeaveDisplay then
      Help^.Erase;

    {$IFDEF CheckStack}
    {Scan to see how much unused stack space there is}
    for I := 0 to SPtr do
      if Mem[SSeg:I] <> $AA then begin
        Write(I);
        Exit;
      end;
    {$ENDIF}

    if TwoMonitors then begin
      if MouseInstalled then
        RestoreMouseState(MSP, False);
      HiddenCursor;
      SwitchScreens(Screen1);
    end else
      {Assure cursor is back in place}
      GotoXYabs(CursorX, CursorY);
  end;

  function ExactMatch(TopicName, ScreenName : String;
                      H : AbstractHelpWindowPtr) : Boolean;
    {-Return True if ScreenName and TopicName are identical}
  begin
    ExactMatch := (CompUCString(ScreenName, TopicName) = Equal);
  end;

  function StartingMatch(TopicName, ScreenName : String;
                         H : AbstractHelpWindowPtr) : Boolean;
    {-Return True if ScreenName is found within TopicName at position 1}
  begin
    ScreenName := StUpcase(ScreenName);
    TopicName := StUpcase(TopicName);
    StartingMatch := (Pos(ScreenName, TopicName) = 1);
  end;

  procedure RepeatFindTopic;
    {-Circ search for SaveLookupString, beep if not found}
  var
    NewTopic : Word;
    MatchFunc : hwMatchFunc;
  begin
    {Error if no current lookup string or matched topic}       {!!.20}
    if SaveLookupString = '' then begin                        {!!.20}
      Beep(1100, 100);                                         {!!.20}
      Exit;                                                    {!!.20}
    end;                                                       {!!.20}

    {Set correct match function}
    if LookUpPartial then
      MatchFunc := StartingMatch
    else
      MatchFunc := ExactMatch;

    {Get next topic}
    with Help^ do begin
      NewTopic := FindNextTopic(CurrentTopic, True,
                                SaveLookupString, MatchFunc);

      {Beep if not found or wrapped, return old topic number}
      if NewTopic = 0 then
        Beep(1100, 100)
      else
        SetTopic(NewTopic);
    end;
  end;

  procedure RepeatFindText(var MatchTopic, MatchOfs : Word); {!!.13}
    {-Search for last text again}
  label
    ExitPoint;
  var
    SaveTopic : Word;
    MTopic : Word;
    MOfs : Word;
    KW : Word;
    Status : Word;
  begin
    Help^.wFastFlush('Searching', Help^.Height, ColorMono($CF, $F0));

    SaveTopic := MatchTopic;
    MTopic := SaveTopic;
    MOfs := MatchOfs;
    repeat
      if KeyPressed then begin
        KW := ReadKeyWord;
        goto ExitPoint;
      end;
      MOfs := Help^.SearchTopic(MTopic, MOfs, SaveLookupString);
      if MOfs = $FFFF then begin
        {No match, try next topic}
        MOfs := 0;
        if MTopic = Help^.HighestTopic then
          MTopic := 1
        else
          inc(MTopic);
      end else begin
        MatchTopic := MTopic;
        MatchOfs := MOfs;
        Exit;
      end;
    until MTopic = SaveTopic;

    {Text not found}
ExitPoint:
    Help^.wFastFlush('         ', Help^.Height,
                     ColorMono(HelpColorSet.TextColor, HelpColorSet.TextMono));
    Beep(300, 200);
    OpenPromptWindow('');
    ErrorPromptWindow('Search text not found');
    ClosePromptWindow;

    {Force reload of buffer with original topic}
    Help^.hwCurTopic := MatchTopic;
    Status := Help^.hwLoadTopic;
    {Undo inc() made by caller}
    if MatchOfs <> 0 then
      dec(MatchOfs);
  end;

  procedure FindText(var MatchTopic, MatchOfs : Word); {!!.13}
    {-Prompt for text to search for and find it}
  begin
    OpenPromptWindow(' Text to Search for ');
    ReadPromptString('Enter text: ', SaveLookupString);
    ClosePromptWindow;
    if SaveLookupString = '' then
      Exit;
    LookupByTopic := False;

    RepeatFindText(MatchTopic, MatchOfs);
  end;

                        {!!.01}
  procedure ProcessHelp(ShowIndex, ShowPrev : Boolean;
                        CurTopic, CurOfs : Word);
    {-Standard process loop for all entry points}
    {!!.13} {Numerous changes for text search}
  label
    NewHelpObject;
  var
    Finished : Boolean;
    Failed : Boolean;
  begin
NewHelpObject:
    Failed := False;
    Finished := False;
    with Help^ do
      repeat
        {Display the help index}
        if ShowIndex then begin {!!.01}
          SetTopic(0);
          Process;
          if (cwGetLastError = 0) and (GetLastCommand = ccSelect) then begin
            CurTopic := GetTopicChoice;
            CurOfs := 0; {!!.13}
          end;
        end;
        {Display the help topic}
        if (CurTopic <> 0) or ShowPrev then begin {!!.01}
          if not ShowPrev then begin {!!.01}
            if CurOfs = 0 then
              SetTopic(CurTopic)      {!!.01}
            else
              SetTopicAndPos(CurTopic, CurOfs, Length(SaveLookupString));
          end;
          repeat
            Process;
            if cwGetLastError <> 0 then
              Finished := True
            else if (GetLastCommand = ccQuit) and not InHelpMode then
              {Escaped from pick window within help}
              Finished := True
            else begin
              case GetLastCommand of
                HKSMoveCmd  : Failed := not KeyboardMove;
                HKSNewDBCmd : begin
                              DatabaseMenu(DBName);
                              ShowIndex := True; {!!.01}
                              ShowPrev := False; {!!.01}
                              CurTopic := 0; {!!.01}
                              CurOfs := 0; {!!.13}
                              goto NewHelpObject;
                              end;
                HKSZoomCmd  : Failed := not ToggleZoom;
                HKSNextCmd  : if InHelpMode then        {!!.13}
                                if LookupByTopic then   {!!.13}
                                  RepeatFindTopic
                                else begin              {!!.13}
                                  inc(CurOfs);
                                  RepeatFindText(CurTopic, CurOfs);
                                  ShowIndex := False;
                                  ShowPrev := False;
                                  goto NewHelpObject;
                                end;
                HKSTextCmd  : if InHelpMode then begin {!!.13}
                                CurTopic := CurrentTopic;
                                CurOfs := Help^.hwFindTextOfs+1;
                                FindText(CurTopic, CurOfs);
                                ShowIndex := False;
                                ShowPrev := False;
                                goto NewHelpObject;
                              end;

                else Finished := True;
              end;
              if not InHelpMode then begin
                CurTopic := 0;
                CurOfs := 0; {!!.13}
                ShowIndex := True; {!!.01}
                ShowPrev := False; {!!.01}
                goto NewHelpObject;
              end;
            end;
          until Finished or Failed or (GetLastCommand = ccQuit);
        end else case GetLastCommand of
          {Evaluate pick list exit command}
          HKSMoveCmd  : Failed := not KeyboardMove;
          HKSNewDBCmd : begin
                        DatabaseMenu(DBName);
                        ShowIndex := True; {!!.01}
                        ShowPrev := False; {!!.01}
                        CurTopic := 0; {!!.01}
                        CurOfs := 0; {!!.13}
                        goto NewHelpObject;
                        end;
          HKSZoomCmd  : Failed := not ToggleZoom;

          HKSNextCmd,
          HKSTextCmd  : ; {!!.13}

          ccPrevTopic : begin {!!.01}
                        ShowIndex := False; {!!.01}
                        ShowPrev := True; {!!.01}
                        end; {!!.01}

          else Finished := True;
        end;
      until Finished or Failed;

    if Failed then begin
      {Clear heap and reload the help object}
      Dispose(Help, Done);
      wStack.Done;
      LoadHelpObject(DBName);
      CurTopic := 0;
      CurOfs := 0; {!!.13}
      goto NewHelpObject;
    end;
  end;

  procedure PopHelpIndex;
    {-Pop up stub for help index}
  begin
    if not ReinitVideo then
      Exit;
    JumpAwayFromCursor;
    ProcessHelp(True, False, 0, 0);
    ExitVideo;
  end;

  procedure PopLastTopic;
    {-Pop up stub for retrieving the last selected topic}
  var
    ExistPrev : Boolean; {!!.02}
  begin
    if not ReinitVideo then
      Exit;
    ExistPrev := (Help^.TopicStackPtr^.Elements <> 0); {!!.02}
    if ExistPrev then                                                   {!!.20}
      ProcessHelp(False, True, Help^.CurrentTopic, Help^.hwFindTextOfs) {!!.20}
    else                                                                {!!.20}
      ProcessHelp(True, False, 0, 0);                                   {!!.20}
    ExitVideo;
  end;

  procedure PopScreenHelp;
    {-Pops up stub for screen help function (looks back on line if necessary}
  var
    Topic : Word;
    S : String[80];
  begin
    if not ReinitVideo then
      Exit;
    JumpAwayFromCursor;

    {If TwoMonitors then switch back to the first screen}
    if TwoMonitors then
      SwitchScreens(Screen1);

    {Look for exact match first}
    LookupPartial := False;
    S := Help^.ScreenWordXY(CursorX, CursorY, True, False);
    Topic := Help^.FindTopic(S, ExactMatch);
    if (Topic = 0) and (Length(S) >= MinPartialMatch) then begin
      {Look for partial match if that fails}
      Topic := Help^.FindTopic(S, StartingMatch);
      if Topic <> 0 then
        LookupPartial := True;
    end;

    {Switch back to the right monitor}
    if TwoMonitors then
      SwitchScreens(Screen2);

    {Show the help}
    SaveLookupString := S;
    if Topic = 0 then begin
      {No match found, show the help index}
      Beep(1100, 100);
      LookupByTopic := False;
      SaveLookupString := ''; {!!.20}
      ProcessHelp(True, False, 0, 0);
    end else begin
      {Match found, show the topic}
      LookupByTopic := True; {!!.13}
      ProcessHelp(False, False, Topic, 0);
    end;
    {SaveLookupString := '';} {!!.20}

    ExitVideo;
  end;

  procedure PressAKey;
    {-Put up a prompt and wait for keypress}
  var
    KW : Word;
  begin
    Write('Press a key to continue...');
    KW := ReadKeyWord;
    Write(^M);
    ClrEol;
    {Check for break}
    if (KW = 0) or (Lo(KW) = 3) then
      Halt;
  end;

{$IFNDEF NoPop}                 {!!.03}
  procedure WriteHelp;
    {-Write installation help information}
  var
    Console : Boolean;
  begin
    {Allow redirection of help screens}
    Assign(Output, '');
    Rewrite(Output);
    Console := HandleIsConsole(StdOutHandle);

    WriteLn(ProgName, ' is a pop-up online reference engine which can be installed with the');
    WriteLn('following command line syntax:');
    WriteLn;
    WriteLn('    ', ProgName, ' [HelpFile] [Options]');
    WriteLn;
    WriteLn('HelpFile is an optional pathname to a help file. This version of ', ProgName, ' will');
    WriteLn('automatically load the ', DBName, ' file by searching in the current directory');
    WriteLn('and on the DOS path. Specify a different directory or a different help file');
    WriteLn('by putting it on the ', ProgName, ' command line.');
    WriteLn;
    if Console then
      PressAKey;

    WriteLn('The following options may be specified on the command line:');
    WriteLn;
    WriteLn('    /A         Use normal file attribute for swap file (instead of hidden).');
    WriteLn('    /B         Force use of black and white video attributes.');
    WriteLn('    /D         Force swapping to disk even if EMS is available.');
    WriteLn('    /E ProgToExec CommandLine');
    WriteLn('               Execute a program instead of going resident. No swapping.');
    WriteLn('               This option must be the last one on the command line.');
    WriteLn('    /H Height  Specify help window height other than ', InitHelpHeight, '.');
    WriteLn('    /I HotKey  Specify alternate hot key for help index.');
    WriteLn('    /L HotKey  Specify alternate hot key for screen lookup.');
    WriteLn('    /M         Disable message appearing while TSR swaps to or from disk.');
    WriteLn('    /N         Force entire TSR to remain in memory (no swapping).');
    WriteLn('    /P HotKey  Specify alternate hot key for previous topic.');
    WriteLn('    /S Path    Specify drive and directory for disk swapping.');
    Writeln('    /T         Activate the "twin" display option.');
{$IFDEF SupportXms}  {!!.02}
    WriteLn('    /X         Use XMS memory if available.');
    WriteLn('    /1         Single swap file (for RAM disks or XMS).');
{$ELSE}
    WriteLn('    /1         Single swap file (for RAM disks).');
{$ENDIF}

    WriteLn('    /U         Unload TSR from memory.');
    WriteLn('    /V         Leave help text visible on second display after popping down.');
    WriteLn;
    if Console then
      PressAKey;
    WriteLn;
    WriteLn('A HotKey is specified as a hexadecimal word. The top byte specifies');
    WriteLn('the shift key(s) to be pressed and can be zero. The bottom byte');
    WriteLn('specifies the scan code for the hot key. Two hot keys may not have');
    WriteLn('the same scan code, even if the shift code differs.');
    WriteLn;
    WriteLn('Shift key codes: Alt:08, Ctrl:04, Left Shift:02, Right Shift:01');
    WriteLn('For example: /L 0844 performs lookup when <Alt><F10> is pressed.');
    WriteLn;
    WriteLn('Unless modified, ', ProgName, ' responds to the following hot keys:');
    WriteLn;
    WriteLn(' ', HotKeyNameScreen:19, '   Look up help topic based on word at cursor.');
    WriteLn(' ', HotKeyNameLast:19,   '   Redisplay previous help topic.');
    WriteLn(' ', HotKeyNameIndex:19,  '   Display help index.');
    WriteLn;
    WriteLn('See the Help on Help topic for additional information.');
    Halt;
  end;
{$ENDIF}                        {!!.03}

  procedure Abort(Msg : String; Code : Byte);
    {-Write a message and halt}
  begin
    WriteLn(Msg);
    WriteLn;
    WriteLn(ProgName, ' /? for help');
    Halt(Code);
  end;

{$IFNDEF NoPop}                 {!!.03}
  procedure DisableResidentCopy(IFC : IfcPtr);
    {-Using the IfcPtr, disable the known resident copy of ourself}
  var
    Save : Boolean;
  begin
    with IFC^ do begin
      RestoreAllVectors;
      Save := CSDataPtr^.SwapMsgOn;     {Save state of swap messages}
      CSDataPtr^.SwapMsgOn := False;    {Disable swap messages}

      LongInt(UserData) := UnLoadTSR;   {UserData = UnLoadTSR command}
      CmdEntryPtr;                      {Call the CmdEntryPtr}

      {Check status of Unload attempt}
      if LongInt(UserData) = UnloadSuccessful then begin
        WriteLn(ProgName, ' removed from memory');
        Halt;
      end else begin
        {Restore state of swap messages}
        CSDataPtr^.SwapMsgOn := Save;
        Abort('Unable to remove '+ProgName+' from memory', 1);
      end;
    end;
  end;

  procedure ExternalIfc;
    {-Dispatches external requests (currently only unload or pastenextbuffer)}
  var
    TempSaveInt16 : Pointer;                                {!!.01}
    CurInt16 : Pointer;                                     {!!.02}
  begin
    with CSSwapData^.ThisIFC do
      case LongInt(UserData) of
        UnloadTSR :
          begin

            {$IFDEF EnableCutPaste}
            {Make Vectors reflect original Int16}
            TempSaveInt16 := CSSwapData^.Vectors[$16];      {!!.01}
            SetVecOnReturn($16, OrigInt16Addr);             {!!.01}
            if not CSSwapData^.SwapEnabled then begin       {!!.03}
              GetIntVec($16, CurInt16);                     {!!.03}
              SetIntVec($16, OrigInt16Addr);                {!!.03}
            end;                                            {!!.03}
            {$ENDIF}

            {Try to remove the TSR}
            if DisableTSR then
              LongInt(UserData) := UnloadSuccessful
            else begin
              {$IFDEF EnableCutPaste}                       {!!.01}
              CSSwapData^.Vectors[$16] := TempSaveInt16;    {!!.01}
              if not CSSwapData^.SwapEnabled then           {!!.03}
                SetIntVec($16, CurInt16);                   {!!.03}
              {$ENDIF}                                      {!!.01}
              LongInt(UserData) := UnloadFailed;
            end;
          end;

        {$IFDEF EnableCutPaste}
        PasteBlock :
          PasteNextBlock;
        {$ENDIF}
      end;
  end;

  procedure SpawnApplication;
    {-Spawns an application by calling the Execute routine from TPExec}
  var
    Result : Word;
  begin
    Result := Execute(CommandToExec, HeapOverhead);
    {Don't count on return address on stack, since it will have been overwritten}
    if Result = 0 then
      Halt
    else
      Abort('Unable to execute program. DOS error '+Long2Str(Result), Result);
  end;

  procedure InitPopUps(IndexRoutine, ScreenRoutine, LastRoutine : Pointer);
    {-Init the popup routines and install Module}
  var
    P : Pointer;
  begin
    {Mark ourself as installed}
    InstallModule(ProgName, ExternalIfc);

    if SideKickLoaded then
      Abort('Cannot load after SideKick', 1);

    {Set up hot keys and popups}
    P := @PopupStack[SizeOf(PopupStack)];

    if not DefinePop(HotKeyIndex, PopupProc(IndexRoutine), P) or
       not DefinePop(HotKeyScreen, PopupProc(ScreenRoutine), P) or
       not DefinePop(HotKeyLast, PopupProc(LastRoutine), P) then
      Abort('Unable to initialize '+ProgName, 2);

  end;

  procedure InstallCheck;
    {-Are we installed? Unload if requested}
  var
    IFC : IfcPtr;
    Regs : IntRegisters;
  begin
    {Check to see if we're already installed}
    IFC := ModulePtrByName(ProgName);

    if IFC <> nil then
      {We are already installed}
      if UnloadRequest then
        {Try to unload}
        DisableResidentCopy(IFC)
      else
        Abort(ProgName+' already installed', 1)
    else if UnloadRequest then
      Abort(ProgName+' not currently installed', 1);
  end;

  function CompletePath(Path : PathStr) : PathStr;
    {-Convert a relative directory name into a complete one}
  var
    DrCh : Char;
    ColPos : Byte;
    DrNum : Byte;
    I : Word;
    SaveDir : PathStr;
    CurDir : PathStr;
  begin
    GetDir(0, CurDir);
    ColPos := pos(':', Path);
    if ColPos = 2 then begin
      {Get current directory on specified drive}
      DrCh := Upcase(Path[1]);
      if DrCh >= 'A' then
        GetDir(Byte(DrCh)-Byte('A')+1, SaveDir)
      else
        ColPos := 0;
    end;
    ChDir(Path);
    if IoResult = 0 then begin
      GetDir(0, Path);
      if ColPos = 2 then begin
        {Restore current directory on other drive}
        ChDir(SaveDir);
        {Watch out! ChDir may set IoResult}
        I := IoResult;
      end;
    end;
    ChDir(CurDir);
    CompletePath := AddBackSlash(Path);
  end;

  function CompletePathName(Name : string) : string;
    {-Convert a potentially relative file name into a complete one}
  var
    JustName : string[13];
  begin
    JustName := JustFilename(Name);
    Name := CompletePath(JustPathname(Name));
    CompletePathName := Name+JustName;
  end;

  procedure DeleteSwapFile(Name : String);             {!!.03}
    {-Deletes the swap file if it already exists}
  var
    F : File;
  begin
    Assign(F, Name);
    {first insure it has a file attribute of 0}
    SetFAttr(F, 0);
    {if we fail, then no such file exists}
    if DosError = 0 then begin
      Erase(F);
      if IoResult <> 0 then ;
    end;
  end;

  procedure GoResident;
    {-Go resident}
  var
    WillUseEms, WillUseXms : Boolean; {!!.02}
  begin
    {Determine whether EMS can be used for swapping}
    WillUseEms := WillSwapUseEms(ParagraphsToKeep+HeapOverhead);
  {$IFDEF SupportXms}  {!!.02}
    WillUseXms := WillSwapUseXms(ParagraphsToKeep+HeapOverhead);
  {$ELSE}
    WillUseXms := False;
  {$ENDIF}

    if UseSwapping then
      if WillUseEms or WillUseXms then
        {Don't show the swap message}
        SetSwapMsgOn(False)
      else begin
        {Generate complete pathnames for Swap1Name and Swap2Name}
        Swap1Name := CompletePathName(Swap1Name);
        Swap2Name := CompletePathName(Swap2Name);
        if (Length(Swap1Name) > 64) or (Length(Swap2Name) > 64) then begin
          WriteLn('Complete path to swap files must not exceed 64 characters');
          WriteLn(Swap1Name);
          Abort(Swap2Name, 1);
        end;
        {Set attribute and row for swapping message}
        SetSwapMsgAttr($1b);
        SetSwapMsgRow(1);
        DeleteSwapFile(Swap1Name);                 {!!.03}
      end;

    WriteLn('Default hot keys:');
    WriteLn('  ', HotKeyNameScreen, ' displays help for word at cursor position');
    WriteLn('  ', HotKeyNameLast,   ' displays last help topic');
    WriteLn('  ', HotKeyNameIndex,  ' displays help index');
    WriteLn;
    Write('Installing ', ProgName);
    if UseSwapping then
      if WillUseEms then
        Write(' in EMS')
    {$IFDEF SupportXms}  {!!.02}
      else if WillUseXms then
        Write(' in XMS')
    {$ENDIF}
      else
        Write(' with swap files in ', JustPathName(Swap1Name));
    WriteLn;

    StayResSwap(ParagraphsToKeep+HeapOverhead, 0,
                Swap1Name, Swap2Name, UseSwapping);
    Abort('Unable to install '+ProgName, 1);               {!!.01}
  end;

  function GetParam(CmdPtr : StringPtr; var CmdPos : Integer) : String;
    {-Return next parameter from command line at CmdPtr^}
  var
    I: Integer;
  begin
    {Skip blanks}
    while (CmdPos <= Length(CmdPtr^)) and (CmdPtr^[CmdPos] <= ' ') do
      Inc(CmdPos);
    {Scan to end of parameter}
    I := CmdPos;
    if CmdPos <= Length(CmdPtr^) then
      repeat
        Inc(CmdPos);
      until (CmdPos > Length(CmdPtr^)) or
            (CmdPtr^[CmdPos] <= ' ') or
            (CmdPtr^[CmdPos] = '/');
    GetParam:=Copy(CmdPtr^, I, CmdPos-I);
  end;

  procedure ParseCommandLine;
    {-Gets command line options and sets various parameters.}
  var
    CmdPtr : StringPtr;
    CmdPos : Integer;
    Code : Word;
    Param : String[127];
    StartDB : PathStr;

    procedure GetHotKey(var HotKey : Word);
      {-Parse a hot key from the command line}
    begin
      {Get next parameter}
      Param := GetParam(CmdPtr, CmdPos);
      if Length(Param) = 0 then
        Abort('No hot key specified', 1);
      if Param[1] <> '$' then
        Param := '$'+Param;
      Val(Param, HotKey, Code);
      if (Code <> 0) {or (Hi(HotKey) = 0)} then {!!.01}
        Abort('Invalid hot key specified', 1);
    end;

  begin
    StartDB := '';

    {Scan command line}
    CmdPtr:=Ptr(PrefixSeg,$80);
    CmdPos:=1;
    Param := GetParam(CmdPtr, CmdPos);

    while Length(Param) <> 0 do begin
      case Param[1] of
        '/', '-' :
          if Length(Param) <> 2 then
            Abort('Invalid parameter: '+Param, 1)
          else
            case Upcase(Param[2]) of

              'B' : {Force plain B&W color set}
                begin
                  DefColorChoice := ForceMono;
                  HelpColorSet.FlexCHelpMono := $07;
                  HelpColorSet.UnselXrefMono := $0F;
                end;

              'C' : {Force cursor control through BIOS}
                begin
                  BiosCursor := True;
                  DirectVideo := False;
                end;

              'D' : {Force swap to disk}
                SwapUseEms := False;

              'E' : {Exec another program}
                begin
                  GoingResident := False;
                  {Program is the rest of the command line}
                  CommandToExec := Copy(CmdPtr^, CmdPos, 255);
                  CmdPos := 255;
                  if Length(CommandToExec) = 0 then
                    Abort('No program to execute specified', 1);
                end;

              'H' : {Specify help height}
                begin
                  {Get next parameter}
                  Param := GetParam(CmdPtr, CmdPos);
                  if Length(Param) = 0 then
                    Abort('No help height specified', 1);
                  Val(Param, InitHelpHeight, Code);
                  if (Code <> 0) or (InitHelpHeight > ScreenHeight-2) then
                    Abort('Invalid help height: '+Param, 1);
                end;

              'I' : {Index hot key}
                GetHotKey(HotKeyIndex);

              'L' : {Lookup hot key}
                GetHotKey(HotKeyScreen);

              'M' : {Don't show swap message}
                SetSwapMsgOn(False);

              'N' : {Force not to swap}
                UseSwapping := False;

              'P' : {Previous topic hot key}
                GetHotKey(HotKeyLast);

              'S' : {Specify swap path}
                begin
                  Param := GetParam(CmdPtr, CmdPos);
                  if Length(Param) = 0 then
                    Abort('No swap path specified', 1);
                  Param := AddBackSlash(Param);
                  Swap1Name := Param+Swap1Name;
                  Swap2Name := Param+Swap2Name;
                end;

              'T' : TwoMonitors := True;

              'U' : {Unload from command line}
                UnloadRequest := True;

              'V' : LeaveDisplay := True;

            {$IFDEF SupportXms}  {!!.02}
              'X' : begin
                      SwapUseXms := True;
                      EmsOverXms := False;
                    end;
            {$ENDIF}
              '1' : SetSingleSwapFile(True);

              'A' : SetSwapFileAttr(False);
              'Q' : SetQuickFixMode(True);

              '?' : {Request for help}
                WriteHelp;

            else
              Abort('Invalid parameter: '+Param, 1);
            end;
      else
        if Length(StartDB) = 0 then begin
          StartDB := StUpcase(Param);
          StartDB := DefaultExtension(StartDB, 'HLP');
        end else
          Abort('Too many help files specified: '+Param, 1);
      end;

      {Get next parameter}
      Param := GetParam(CmdPtr, CmdPos);
    end;

    {Override the default help file if requested}
    if Length(StartDB) <> 0 then
      DBName := StartDB;

    {Check for hotkey conflicts}
    if (Lo(HotKeyScreen) = Lo(HotKeyLast)) or
       (Lo(HotKeyScreen) = Lo(HotKeyIndex)) or
       (Lo(HotKeyLast) = Lo(HotKeyIndex)) then
      Abort('Hot key conflict', 1);

    {Check for dual monitors}
    if TwoMonitors then
      if not HasDualDisplays then begin
        TwoMonitors := False;
        LeaveDisplay := False;
      end else begin
        {Using dual monitors - prepare to deal with mouse on popup}
        if MouseInstalled then begin
          MSPsize := MouseStateBufferSize;
          {check for a reasonable buffer size}
          if (MSPsize = 0) or (MSPsize > 1000) then
            MouseInstalled := False
          else
            if not GetMemCheck(MSP, MSPsize) then
              Abort('Internal error: insufficient memory', 1);
        end;
      end
    else
      LeaveDisplay := False;
  end;
{$ENDIF}                        {!!.03}

  procedure LoadPopHelp;
    {-Check conditions and go resident or exec}
  begin
{$IFNDEF NoPop}                 {!!.03}
    {Copyright}
    Writeln(ProgName, ' Version ', Version,
            '. Copyright (c) 1989,92, TurboPower Software'^M^J);

    {Check for command line options}
    ParseCommandLine;

    {See if we're already installed; uninstall or abort if so}
    InstallCheck;
{$ENDIF}                        {!!.03}

    {Add help commands}
    HelpCommands.AddCommand(HKSNewDBCmd, 1, NewDBKey, 0);
    HelpCommands.AddCommand(HKSMoveCmd, 1, MoveKey1, 0);
    HelpCommands.AddCommand(HKSMoveCmd, 1, MoveKey2, 0);
    HelpCommands.AddCommand(HKSZoomCmd, 1, ZoomKey1, 0);
    HelpCommands.AddCommand(HKSZoomCmd, 1, ZoomKey2, 0);
    HelpCommands.AddCommand(HKSNextCmd, 1, NextKey1, 0);
    HelpCommands.AddCommand(HKSNextCmd, 1, NextKey2, 0);
    HelpCommands.AddCommand(ccQuit, 1, ExitKey1, 0);               {!!.01}
    HelpCommands.AddCommand(HKSTextCmd, 1, TextKey1, 0);           {!!.13}
    HelpCommands.AddCommand(HKSTextCmd, 2, Byte(^Q), Byte(^F));    {!!.13}

    if HelpCommands.GetLastError <> 0 then
      Abort('Internal error: adding command keys', 1);

    {Allocate the prompt window now (avoids heap fragmentation)}
    if not GetMemCheck(PromptWindowPtr, 2*3*80) then
      Abort('Internal error: insufficient memory', 1);

    {Open the help file}
    LoadHelpObject(DBName);
    if Help = Nil then
      case InitStatus mod 10000 of
        1..7, 9..1000 : Abort('Unable to open '+DBName, 3);
        8 : Abort('Insufficient memory', 1);                     {!!.01}
        8220 : Abort(DBName+' is not a valid help file', 2);     {!!.01}
        else
          Abort('Unexpected error code '+Long2Str(InitStatus), 1);
      end;

{$IFNDEF NoPop}                 {!!.03}
    {Init the hot-key activated routines}
    InitPopUps(@PopHelpIndex, @PopScreenHelp, @PopLastTopic);

    {Go resident or spawn process}
    PopupsOn;
    if GoingResident then
      GoResident                  {Install as a TSR}
    else
      SpawnApplication;           {Exec application, do not remain resident}
{$ELSE}                         {!!.03}
    ProcessHelp(True, False, 0, 0);
    Help^.Erase;
{$ENDIF}                        {!!.03}
  end;

end.
