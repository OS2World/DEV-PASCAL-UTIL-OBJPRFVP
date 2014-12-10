program PointerStackExample;  {EXPSTACK.PAS}
uses
  OpRoot;
type
  {An object for manipulating Name and Age}
  NameDataPtr = ^NameData;
  NameData =
    object(Root)
      Name : String[20];
      Age : Byte;
      constructor Init;
      procedure Display;
    end;
var
  NamePtr : NameDataPtr;
  NamePtrStack : PointerStack;
  I : Integer;
  Status : Word;
  {----- NameData Methods -------------------}
  constructor NameData.Init;
  begin
    WriteLn;
    Write('Name:  ');
    ReadLn(Name);
    Write('Age:   ');
    ReadLn(Age);
  end;
  procedure NameData.Display;
  begin
    WriteLn;
    Writeln(Name,' ',Age);
  end;
begin
  {Create a stack for storing up to 5 pointers}
  if not NamePtrStack.Init(5) then begin
    WriteLn('Failed to Init PointerStack,  Status = ', InitStatus);
    Halt;
  end;
  {Create and Push three NameData objects}
  for I := 1 to 3 do begin
    New(NamePtr, Init);
    NamePtrStack.Push(NamePtr);
    Status := NamePtrStack.GetStatus;
    if Status <> 0 then begin
      WriteLn('Failed to push NameData, Status = ', Status);
      Halt;
    end;
  end;
  {Peek at the very first object pushed}
  NamePtr := NameDataPtr(NamePtrStack.Peek(1));
  NamePtr^.Display;
  {Peek at the top item, without actually popping it}
  NamePtr := NameDataptr(NamePtrStack.PeekTop);
  NamePtr^.Display;
  {Now pop each NameDataPtr off and display it, the 4th call will fail}
  for I := 1 to 4 do begin
    NamePtr := NameDataPtr(NamePtrStack.Pop);
    if NamePtr = nil then begin
      WriteLn('Stack is empty');
      Halt;
    end;
    NamePtr^.Display;
  end;
  NamePtrStack.Done;
end.
