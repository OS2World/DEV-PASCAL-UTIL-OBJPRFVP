program RawWindowExample;  {EXRAWWIN.PAS}
uses
  OpRoot, OpString, OpCrt, OpFrame, OpWindow;
var
  W1, W2 : RawWindow;
procedure Pause;
  {-Wait for a keypress}
var
  KW : Word;
begin
  KW := ReadKeyWord;
end;
begin
  ClrScr;
  {Initialize first window}
  W1.InitCustom(10, 5, 40, 20, DefaultColorSet, wClear+wBordered+wAltFrame);
  W1.EnableExplosions(20);
  W1.wFrame.AddHeader(' First Window ', heTL);
  W1.aFrame.AddHeader(' inactive now ', heTL);
  W1.aFrame.SetFrameType(SglWindowFrame);
  {Initialize second, non-overlapping window}
  W2.InitCustom(45, 10, 75, 15, DefaultColorSet, wClear+wBordered);
  W2.wFrame.AddHeader(' Second Window ', heBC);
  W2.wFrame.AddShadow(shBR, shOverWrite);
  {Draw first window, write to it, change to block cursor}
  W1.Draw;
  WriteLn('In the first window');
  WriteLn('Change to block cursor');
  W1.SetCursor(cuBlock);
  Pause;
  {Draw second window, write to it}
  W2.Draw;
  Writeln('In the second window');
  W2.wFastText('Text can be clipped when it is too long to fit', W2.Height, 1);
  W2.wGotoXY(W2.Width, W2.Height);
  Pause;
  {Switch back to first window}
  W1.Draw;
  Pause;
  {Erase both windows, starting with the second}
  W2.Erase;
  Pause;
  W1.Erase;
  {Dispose of windows}
  W2.Done;
  W1.Done;
end.
