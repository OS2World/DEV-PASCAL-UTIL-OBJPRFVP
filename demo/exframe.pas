program FrameExample;  {EXFRAME.PAS}
uses
  OpCrt, OpString, OpRoot, OpFrame, OpWindow;
var
  Win : RawWindow;
  WinOpts : LongInt;
  I, SliderPos : Byte;
  Finished : Boolean;
begin
  {Make a framed, resizable RawWindow}
  WinOpts := wBordered+wClear;
  if not Win.InitCustom(30, 5, 50, 15, DefaultColorSet, WinOpts) then begin
    Writeln('Failed to initialize RawWindow, Status ', InitStatus);
    Halt;
  end;
  {Add some Frame features}
  with Win.wFrame do begin
    SetFrameType(DblWindowFrame);
    AddShadow(shBR, shOverWrite);
    AddHeader('Top Header', heTC);
    AddHeader('Bottom Header', heBC);
    AddScrollBar(frRR, 1, 9, DefaultColorSet);
  end;
  {Draw the window, fill it with text}
  Win.Draw;
  for I := 2 to 10 do
    Win.wFastText('Hello Mars', I, 5);
  {Update window/slider based on up/down cursor commands}
  SliderPos := 1;
  Finished := False;
  repeat
    {Update Win and ScrollBar}
    Win.DrawSlider(frRR, SliderPos);
    Win.wGoToXY(3, SliderPos+1);
    case ReadKeyWord of
      $4800 : if SliderPos > 1 then    {Up arrow}
                Dec(SliderPos);
      $5000 : if SliderPos < 9 then    {Down arrow}
                Inc(SliderPos);
      $011B : Finished := True;        {Escape}
    end;
  until Finished;
  Win.Done;
end.
