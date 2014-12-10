program WindowStackExample;  {EXWSTACK.PAS}
uses OpCrt, OpFrame, OpWindow;
var
  Win : WindowPtr;
  WinOpts : LongInt;
  I, J : Integer;
  S : String;
begin
  {Make three framed, resizable StackWindows}
  WinOpts := wBordered+wClear+wSaveContents+wResizeable;
  New(Win, InitCustom(30, 3, 50, 13, DefaultColorSet, WinOpts));
  Win^.Draw;
  New(Win, InitCustom(35, 8, 55, 18, DefaultColorSet, WinOpts));
  Win^.Draw;
  New(Win, InitCustom(40, 13, 60, 23, DefaultColorSet, WinOpts));
  Win^.Draw;
  Delay(1000);
  {Demonstrate the use of default wStack}
  for I := 1 to wStack.SP do begin
    Win := WindowPtr(wStack.Peek(I));
    for J := 2 to 10 do
      Win^.wFastWrite('Hello Mars', J, 5, TextAttr);
    Delay(1000);
  end;
  repeat
    Win := wStack.UnStackTop;
    if Win <> nil then begin
      Dispose(Win, Done);
      Delay(1000);
    end;
  until Win = nil;
  wStack.Done;
end.
