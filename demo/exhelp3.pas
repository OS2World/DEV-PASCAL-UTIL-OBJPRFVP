program PageOrScrollExample;  {EXHELP3.PAS}
uses
  OpCrt, OpRoot, OpCmd, OpMouse, OpFrame, OpWindow, OpPick, OpHelp;
const
  HelpFile = 'ENTRY.HLP';
var
  H : AbstractHelpWindowPtr;
  Scrolling : Boolean;
begin
  {Determine if help file prefers scrolling}
  Scrolling := ScrollingHelpFile(HelpFile);
  {Allocate a help window of the preferred type}
  if Scrolling then
    H := New(ScrollingHelpWindowPtr,
             InitDeluxe(9, 8, 72, 18, DefaultColorSet,
                        DefWindowOptions or wBordered,
                        HelpFile, PickVertical,
                        hwStaticNameBuffer+hwPickIndex))
  else
    H := New(PagedHelpWindowPtr,
             InitDeluxe(9, 8, 72, 18, DefaultColorSet,
                        DefWindowOptions or wBordered,
                        HelpFile, PickVertical,
                        hwStaticNameBuffer+hwPickIndex));
  if H = nil then begin
    WriteLn('Error initializing help window: ', InitStatus);
    Halt;
  end;
  {Add some features}
  H^.AddTopicHeader(1, 60, heTC);
  H^.wFrame.AddHeader(' Topic Index ', heTC);
  H^.wFrame.AddScrollBar(frRR, 0, MaxLongInt, DefaultColorSet);
  H^.hwFrame.AddScrollBar(frRR, 0, MaxLongInt, DefaultColorSet);
  if Scrolling then
    {Horizontal scroll bar for scrolling help window only}
    H^.hwFrame.AddScrollBar(frBB, 0, MaxLongInt, DefaultColorSet);
  {Display topic 1}
  H^.SetTopic(1);
  H^.Process;
  {Deallocate help window}
  Dispose(H, Done);
end.
