{$S-,R-,V-,I-,B-,F-}

{*********************************************************}
{*                   WINWOW.PAS 1.30                     *}
{*     An example program for Object Professional 1.0    *}
{*      Copyright (c) TurboPower Software 1987, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program requires that OPDEFINE.INC activate the following defines:
   UseAdjustableWindows
 This program will use features activated with the following defines:
   UseShadows
 ***************************************************************************}
{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

program WindowWOW;

uses
  OpString,
  OpConst,   {!!.20}
  OpRoot,
  OpCrt,
  OpFrame,
  OpWindow;

const
  MaxWindow = 8;             {Total random windows to display}

  WowColorSet : ColorSet = (
    TextColor   : $3B; TextMono    : $07;
    CtrlColor   : $07; CtrlMono    : $07;
    FrameColor  : $3E; FrameMono   : $0F;
    HeaderColor : $4E; HeaderMono  : $70;
    ShadowColor : $07; ShadowMono  : $70
  );

  MainWAttr = $3B;           {Window attribute for main window}
  MainFAttr = $3E;           {Frame attribute for main window}
  MainHAttr = $4E;           {Header attribute for main window}

  RandFAttr = $0E;           {Frame attribute for random windows}
  RandHAttr = $1E;           {Header attribute for random windows}
  RandHAttrMono = $70;       {Header attribute for random windows - mono}
  RandUFttr = $07;           {Unselected frame attribute for random windows}
  RandUHttr = $07;           {Unselected header attribute for random windows}

  ActiveFr : FrameArray = 'ÉÈ»¼ÍÍºº';
  InActiFr : FrameArray = 'ÚÀ¿ÙÄÄ³³';

var
  Main : WindowPtr;
  W : array[0..MaxWindow] of WindowPtr;
  V : WindowPtr;
  CW : Integer;
  MaxLines : Integer;
  WidthBase : Integer;
  HeightBase : Integer;
  VS : VirtScreen;
  R : Word;
  C : Word;

  procedure ErrorMem;
    {-Report out of memory error}
  begin
    Window(1, 1, ScreenWidth, ScreenHeight);
    NormVideo;
    ClrScr;
    NormalCursor;
    SetBlink(True);
    WriteLn('Insufficient Memory');
    Halt(1);
  end;

  function RandomStr(Len : Byte) : string;
    {-Return a random string of characters of length Len}
  var
    B : Byte;
  begin
    for B := 1 to Len do
      RandomStr[B] := Chr(Random(96)+32);
    RandomStr[0] := Chr(Len);
  end;

  procedure RandomWindow(Num : Integer);
    {-Initialize random window coordinates}
  var
    Attr : Byte;
    XL, YL, XH, YH : Byte;
  begin
    {Get dimensions including frame}
    repeat
      XL := Random(ScreenWidth);
      XH := XL+12+Random(WidthBase);
    until (XL > 1) and (XH < ScreenWidth);
    repeat
      YL := Random(ScreenHeight-1);
      YH := YL+3+Random(HeightBase);
    until (YL > 1) and (YH < ScreenHeight-1);

    {Get attribute}
    repeat
      Attr := MapColor(Succ(Random(255)));
    until (Attr and $F) <> (Attr shr 4);

    {Create the window}
    New(W[Num], InitCustom(XL+1, YL+1, XH-1, YH-1, WowColorSet,
                           wClear+wSaveContents+wBordered+wAltFrame+
                           wResizeable));
    if W[Num] = nil then
      ErrorMem;
    with W[Num]^ do begin
      enableexplosions(20);
      settextattr(Attr, MapColor(Attr));
      with wFrame do begin
        setframetype(activefr);
        setframeattr(RandFAttr, MapColor(RandFAttr));
        setheaderattr(RandHAttr, RandHAttrMono, False);
        addheader(' Window '+Long2Str(Num)+' ', heTC);
        {$IFDEF UseShadows}
        setshadowattr($1E, $07, False);
        defshadowcharv := #176;
        defshadowcharh := #176;
        addshadow(shBR, shOverwrite);
        {$ENDIF}
      end;
      with aFrame do begin
        setframetype(inactifr);
        setframeattr(RandUFttr, MapColor(RandUFttr));
        setheaderattr(RandUHttr, MapColor(RandUHttr), False);
        addheader(' Hidden '+Long2Str(Num)+' ', heTC);
      end;
      if GetLastError <> 0 then
        ErrorMem;
    end;
  end;

  procedure WriteLine(Num, Sta, Wid : Integer);
    {-Write one line to a window}
  begin
    Write(RandomStr(Wid));
  end;

  procedure UpdateWindow(Num : Integer);
    {-Update the contents of one window}
  var
    R : Integer;
    C : Integer;
  begin
    with W[Num]^ do begin
      GotoXY(1, 1);
      if Odd(Num) then begin
        R := (60-(wYH-wYL)) shr 1;
        C := (128-(wXH-wXL)) shr 1;
        VS.CopyToWindow(R, C);
        Delay(15);
      end else begin
        for R := 1 to Height-1 do
          WriteLine(Num, R, Width);
        WriteLine(Num, R+1, Width-1);
      end;
    end;
  end;

  procedure DrawWindow(Num : Integer);
    {Initialize contents of one window}
  begin
    W[Num]^.Draw;
    UpdateWindow(Num);
  end;

  function Min(X, Y : Integer) : Integer;
    {-Return lesser of two integers}
  begin
    if X < Y then
      Min := X
    else
      Min := Y;
  end;

  procedure ScrollAndMove(Num : Integer);
    {-Move window while scrolling it}
  const
    XDel : array[1..4] of Integer = (1, -1, -1, 1);
    YDel : array[1..4] of Integer = (-1, -1, 1, 1);
  var
    D, O, I, J, K : Integer;
  begin
    with W[Num]^ do begin
      {Choose a random direction and distance to move}
      O := 1+Random(4);
      case O of
        1: D := Min(ScreenWidth-wXH-3, wYL-3);
        2: D := Min(wXL-2, wYL-3);
        3: D := Min(wXL-2, ScreenHeight-wYH-3);
        4: D := Min(ScreenWidth-wXH-3, ScreenHeight-wYH-3);
      end;
      {There's a chance the window can't move at all}
      if D <= 0 then
        Exit;
      D := 1+Random(D);

      for I := 1 to D do begin
        {Scroll a while - controls speed of move}
        if not Odd(Num) then begin
          WriteLn;
          K := Min(MaxLines, Random(Height-1));
          for J := 1 to K do
            WriteLine(Num, J, Width);
          WriteLine(Num, K+1, Width-1);
        end;
        {Move window one row/column diagonally at a time}
        MoveWindow(XDel[O], YDel[O]);
        if GetLastError <> 0 then
          Exit;
      end;
    end;
  end;

  function SetDelta(N, O : Integer) : Integer;
    {-Determine step size}
  begin
    if N > O then
      SetDelta := 1
    else if N < O then
      SetDelta := -1
    else
      SetDelta := 0;
  end;

  procedure SizeAndRedraw(Num : Integer);
    {-Change size and refill window}
  var
    nXH1, nYH1 : Integer;
    XDel, YDel : Integer;
  begin
    with W[Num]^ do begin
      repeat
        nXH1 := wXL+11+Random(WidthBase);
      until nXH1 < ScreenWidth;
      repeat
        nYH1 := wYL+2+Random(HeightBase);
      until nYH1 <= ScreenHeight-1;
      XDel := SetDelta(nXH1, wXH+1);
      YDel := SetDelta(nYH1, wYH+1);

      while (wXH+1 <> nXH1) or (wYH+1 <> nYH1) do begin
        ResizeWindow(XDel, YDel);
        if GetLastError <> 0 then
          Exit;
        UpdateWindow(Num);
        if nXH1 = wXH+1 then
          XDel := 0;
        if nYH1 = wYH+1 then
          YDel := 0;
      end;
    end;
  end;

  procedure ScrollVScreen(Num : Integer);
    {-Scroll the virtual screen over the current window}
  var
    R, C : Word;
    NewR, NewC : Word;
    Rdel, Cdel : Integer;
  begin
    if Odd(Num) then
      with W[Num]^ do begin
        R := (60-(wYH-wYL)) shr 1;
        C := (128-(wXH-wXL)) shr 1;
        NewR := 1+Random(60-(wYH-wYL));
        NewC := 1+Random(128-(wXH-wXL));
        Rdel := SetDelta(NewR, R);
        Cdel := SetDelta(NewC, C);
        while (R <> NewR) or (C <> NewC) do begin
          VS.CopyToWindow(R, C);
          Delay(20);
          if R = NewR then
            Rdel := 0;
          if C = NewC then
            Cdel := 0;
          Inc(R, Rdel);
          Inc(C, Cdel);
      end;
    end;
  end;

  procedure WriteHunk(R, C : Word; A : Byte);
    {-Write one portion of the virtual screen}
  begin
    FastWrite('    Windows     ', R, C, A);
    FastWrite('  that can be   ', R+1, C, A);
    FastWrite(' Moved Stacked  ', R+2, C, A);
    FastWrite(' Scrolled Sized ', R+3, C, A);
  end;

begin
  {make sure we can run under a multitasking environment}
  DetectMultitasking := True;
  ReinitCrt;

  {smooth scrolling on CGA's}
  BiosScroll := False;

  {turn break checking off}
  CheckBreak := False;

  {set a reasonable number of lines to scroll per move}
  if CheckSnow then
    MaxLines := 5
  else
    MaxLines := 15;

  {and a reasonable set of sizes for resizing}
  WidthBase := ScreenWidth div 3;
  HeightBase := (2*ScreenHeight-1) div 3;

  {cursor off}
  HiddenCursor;

  {turn blinking off to get more colors}
  SetBlink(False);

  {Make a main window}
  new(main, initcustom(2, 2, ScreenWidth-1, ScreenHeight-1, WowColorSet,
                       wClear+wSaveContents+wBordered));
  with main^ do begin
    if GetLastError <> 0 then
      ErrorMem;
    enableexplosions(10);
    settextattr(MainWAttr, MapColor(MainWAttr));
    with wFrame do begin
      setframetype(activefr);
      setframeattr(MainFAttr, MapColor(MainFAttr));
      setheaderattr(MainHAttr, MapColor(MainHAttr), False);
      addheader(' Object Professional OPWINDOW Demonstration ', heTC);
      addheader(' Windows that can be Moved, Stacked, Scrolled, and Sized ', heBC);
    end;
  end;
  main^.draw;

  {Make random events more random}
  Randomize;

  {Make and initialize the virtual screen}
  with VS do begin
    Alloc(60, 128);
    Clear(0, ' ');
    Activate;
    for R := 0 to 14 do
      for C := 0 to 7 do
        WriteHunk(4*R+1, 16*C+1, 1+Random(255));
    Deactivate;
  end;

  {Make and display a pile of windows}
  for CW := 0 to MaxWindow do begin
    RandomWindow(CW);
    DrawWindow(CW);
  end;

  {Select random windows until key pressed}
  CW := MaxWindow;
  repeat
    {Choose among the various effects with weighted randomness}
    case Random(10) of
      0 : SizeAndRedraw(CW);
      1..7 : ScrollAndMove(CW);
      8..9 : ScrollVScreen(CW);
    end;

    if not KeyPressed then begin
      {Pick another window}
      CW := Random(MaxWindow+1);
      {Pull it to the top of stack}
      W[CW]^.Select;
    end;
  until KeyPressed;
  CW := ReadKeyWord;

  {Erase and dispose of the windows}
  repeat
    V := wStack.UnstackTop;
    if V <> nil then
      V^.Done;
  until V = nil;

  {Restore the cursor and the pallette}
  NormalCursor;
  SetBlink(True);
end.
