program StackWindowExample;  {EXSWIN.PAS}
uses
  use32,
  OpString, OpCrt, OpFrame, OpWindow;
var
  Win : array[1..3] of StackWindowPtr;
  WinOpts : LongInt;
  I, J : Integer;
begin
  {Make three bordered StackWindows}
  WinOpts := wBordered+wClear+wSaveContents;
  New(Win[1], InitCustom(30, 03, 50, 13, DefaultColorSet, WinOpts));
  New(Win[2], InitCustom(35, 08, 55, 18, DefaultColorSet, WinOpts));
  New(Win[3], InitCustom(40, 13, 60, 23, DefaultColorSet, WinOpts));
  if (Win[1] = nil) or (Win[2] = nil) or (Win[3] = nil) then begin
    WriteLn('Failed to initialize StackWindows');
    Halt;
  end;
  {Add some frame features and draw each window}
  for I := 1 to 3 do
    with Win[I]^ do begin
      wFrame.AddHeader('Window '+Long2Str(I), heTC);
      wFrame.AddShadow(shBR, shOverWrite);
      wFrame.AddScrollBar(frRR, 1, 9, DefaultColorSet);
      EnableExplosions(20);
      Draw;
      for J := 2 to 10 do
        wFastText('Hello Mars', J, 5);
      DrawSlider(frRR, I);
    end;
  {Switch between the windows until <Escape> is pressed}
  I := 1;
  while ReadKeyWord <> $011B do begin
    Win[I]^.Select;
    Inc(I);
    if I > 3 then
      I := 1;
  end;
  {Clear window 1, even it it is hidden}
  Win[1]^.Clear;
  {Erase and dispose of each window}
  for I := 1 to 3 do begin
    J := ReadKeyWord;
    Win[I]^.EraseHidden;
    Dispose(Win[I], Done);
  end;
  {Deallocate the window stack}
  wStack.Done;
end.
