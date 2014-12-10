program HelpBlockExample;  {EXHELP2.PAS}
uses
  Use32,
  OpCrt, OpRoot, OpCmd, OpMouse, OpFrame, OpWindow, OpPick, OpHelp;
const
  HelpFile = 'ENTRY.HLP';
  BlockFile = 'ENTRY.BLK';
var
  SH : ScrollingHelpWindow;
  {$F+}
  function MyCopyFunc(T : CharArrayPtr; Len : Word;
                      SH : ScrollingHelpWindowPtr) : Boolean;
  var
    I : Word;
    F : Text;
  begin
    {Don't exit the Process method}
    MyCopyFunc := False;
    {$I-}
    {Append to the block file}
    Assign(F, BlockFile);
    Append(F);
    if IoResult <> 0 then
      Rewrite(F);
    {$I+}
    {Scan the help information}
    I := 0;
    repeat
      case T^[I] of
        Attr1Toggle..Attr3Toggle,
        XrefToggle :       {Modifying video attribute}
          ;
        IndexMarker :      {Cross-reference topic follows}
          Inc(I, 2);
        LineBrkMark,       {End of line}
        PageBrkMark :      {End of page--acts like end of line here}
          WriteLn(F);
      else
        {Normal text}
        Write(F, T^[I]);
      end;
      Inc(I);
    until I >= Len;
    WriteLn(F);
    Close(F);
    {Reset the block and update the window}
    SH^.HideBlock;
    SH^.Draw;
  end;
begin
  if not SH.InitDeluxe(9, 8, 72, 18,
                       DefaultColorSet,
                       DefWindowOptions or wBordered,
                       HelpFile,
                       PickVertical,
                       hwStaticNameBuffer+hwPickIndex+hwBlockMark)
  then begin
    WriteLn('Error initializing help window: ', InitStatus);
    Halt;
  end;
  {Add some features}
  SH.AddTopicHeader(1, 60, heTC);
  SH.wFrame.AddHeader(' Topic Index ', heTC);
  SH.wFrame.AddScrollBar(frRR, 0, MaxLongInt, DefaultColorSet);
  SH.hwFrame.AddScrollBar(frRR, 0, MaxLongInt, DefaultColorSet);
  SH.hwFrame.AddScrollBar(frBB, 0, MaxLongInt, DefaultColorSet);
  SH.SetBlockAttr($70, $70);
  SH.SetSendFunc(MyCopyFunc);
  {Display topic 1}
  SH.SetTopic(1);
  SH.Process;
  {Deallocate help window}
  SH.Done;
end.
