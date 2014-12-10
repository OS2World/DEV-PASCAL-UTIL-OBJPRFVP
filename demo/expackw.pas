program PackedWindowExample;  {EXPACKW.PAS}
uses
  OpCrt, OpRoot, OpFrame, OpWindow;
var
  PWin : PackedWindow;
  I, J : Integer;
begin
  {Write some characters to the screen}
  for I := 1 to 25 do
    for J := 1 to 26 do
      FastWrite(Char(64+J), I, 2*J+I, TextAttr);
  {Create a PackedWindow}
  if not PWin.Init(10, 5, ScreenWidth-10, ScreenHeight-5) then begin
    WriteLn('Failed to Init PackedWindow,  Status = ', InitStatus);
    Halt;
  end;
  {Clear the initial screen}
  ClrScr;
  FastWrite('Press any key to restore the PackedWindow', 13, 23, TextAttr);
  I := ReadKeyWord;
  {Display the PackedWindow, clean up}
  PWin.Display;
  PWin.Done;
end.
