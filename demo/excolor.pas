program ColorSetExample;  {EXCOLOR.PAS}
uses
  OpCrt, OpRoot, OpFrame, OpWindow;
var
  MyLcs : LoadableColorSet;
  S : BufIdStream;
  Status : Word;
begin
  {Start off with the default color set}
  MyLcs.Init(DefaultColorSet);
  {Customize the LoadableColorSet}
  with MyLcs.lcsColors do begin
    SetTextAttr($17, $07);
    SetFrameAttr($16, $07);
    SetHeaderAttr($16, $07);
  end;
  {Make a Stream}
  if not S.Init('MYLCS.STM', SCreate, 1024) then begin
    Writeln ('Failed to Create Stream,  Status = ', InitStatus);
    Halt;
  end;
  S.RegisterHier(LoadableColorSetStream);
  S.Put(MyLcs);
  Status := S.GetStatus;
  if Status <> 0 then begin
    WriteLn('Failed to Store MyLcs, Status = ', Status);
    Halt;
  end;
  S.Done;
  {MYLCS.STM is now available for use by other programs}
end.
