program SingleListExample;  {EXSLIST.PAS}
uses
  OpRoot;
type
  {An object for manipulating Name and Age, derived from SingleListNode}
  NameDataPtr = ^NameData;
  NameData =
    object(SingleListNode)
      Name : String[20];
      Age  : Byte;
      constructor Init;
      procedure Display;
    end;
var
  NamePtr : NameDataPtr;
  NameList : SingleList;
  I : Integer;
  {----- NameData Methods -------------------}
  constructor NameData.Init;
  begin
    if not SingleListNode.Init then
      Fail;
    WriteLn;
    Write('Name:  ');
    ReadLn(Name);
    Write('Age:   ');
    ReadLn(Age);
  end;
  procedure NameData.Display;
  begin
    WriteLn;
    Writeln(Name, ' ', Age);
  end;
begin
  {Create a SingleList}
  NameList.Init;
  {Create three NameData objects, append to the List}
  for I := 1 to 3 do begin
    New(NamePtr, Init);
    NameList.Append(NamePtr);
  end;
  {Insert a NameData object at the front of the List}
  New(NamePtr, Init);
  NameList.Insert(NamePtr);
  {Get the Head, then traverse the entire List}
  NamePtr := NameDataPtr(NameList.Head);
  while NamePtr <> nil do begin
    NamePtr^.Display;
    NamePtr := NameDataPtr(NameList.Next(NamePtr));
  end;

  {Traverse the List again, this time removing AND disposing each object}
  NamePtr := NameDataPtr(NameList.Head);
  while NamePtr <> nil do begin
    NameList.Delete(NamePtr);
    NamePtr := NameDataPtr(NameList.Head);
  end;

  {Done with the list in full}
  NameList.Done;
end.
