program StringArrayExample;  {EXSARRAY.PAS}
uses
  OpRoot;
var
  Messages : StringArray;
  Failed : Boolean;
begin
  {Create a StringArray (estimate 10 messages at 20 characters each)}
  if not Messages.Init(10, 200) then begin
    WriteLn('Failed to Create StringArray,  Status = ', InitStatus);
    Halt;
  end;
  {Add some messages}
  Failed := False;
  if Messages.AddString('Out of Memory') = 0 then
    Failed := True;
  if Messages.AddString('Parameter out of range') = 0 then
    Failed := True;
  if Messages.AddString('Busy') = 0 then
    Failed := True;
  {Did any of the Adds fail?}
  if Failed then begin
    Writeln('Out of Memory');
    Halt;
  end;
  {Display a Message}
  WriteLn(Messages.GetString(2));
  {Dispose of the StringArray}
  Messages.Done;
end.
