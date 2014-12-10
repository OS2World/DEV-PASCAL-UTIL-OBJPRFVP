program StaticQueueExample;  {EXSQUEUE.PAS}
uses
  OpRoot;
var
  Name : String;
  NameQueue : StaticQueue;
  I : Integer;
begin
  {Create a StaticQueue capable of holding 5 strings, each up to 255 chars}
  if not NameQueue.Init(5, SizeOf(Name), True) then begin
    WriteLn('Failed to create StaticQueue,  Status = ', InitStatus);
    Halt;
  end;
  {Create three Names, add to tail of the queue}
  for I := 1 to 3 do begin
    Write('Enter name: ');
    ReadLn(Name);
    NameQueue.PushTail(Name);
    if NameQueue.GetStatus <> 0 then begin
      WriteLn('Failed to Push Name');
      Halt;
    end;
  end;
  {Retrieve elements from the head of the queue}
  for I := 1 to 3 do begin
    NameQueue.PopHead(Name);
    if NameQueue.GetStatus <> 0 then
      WriteLn ('Failed to Pop Name')
    else
      WriteLn(Name);
  end;
  {Dispose of the queue}
  NameQueue.Done;
end.
