program LibraryExample;  {EXLIB.PAS}
uses OpRoot;
const
  otNameData = 1000;         {Name object type}
  veNameData = 0;            {Name object version}
type
  {An object for manipulating Name and Age}
  NameDataPtr = ^NameData;
  NameData =
    object(Root)
      Name : String[20];
      Age : Byte;
      constructor Init;
      procedure Display;
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
    end;
var
  Name : NameData;
  Lib : OpLibrary; {!!.20}
  I : Integer;
  S : String[3];
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
    Writeln(Name, ' ', Age);
  end;
  constructor NameData.Load(var S : IdStream);
  begin
    if not Root.Init then Fail;
    Name := S.ReadString;
    S.Read(Age, SizeOf(Age));
    if S.PeekStatus <> 0 then Fail;
  end;
  procedure NameData.Store(var S : IdStream);
  begin
    S.WriteString(Name);
    S.Write(Age, SizeOf(Age));
  end;
  {$F+}
  procedure NameDataStream(SPtr : IdStreamPtr);
  begin
    SPtr^.RegisterType(otNameData, veNameData, TypeOf(NameData),
                       @NameData.Store, @NameData.Load);
  end;
  {$F-}
begin
  {Create a Library and register the NameData object}
  if not Lib.Create('NAMEDATA.OPL', 1024, 'NAMEDATA', 25) then begin
    WriteLn('Failed to create Library. Status = ', InitStatus); Halt;
  end;
  Lib.RegisterHier(NameDataStream);
  {Create and Store three NameData objects}
  for I := 1 to 3 do begin
    Name.Init;
    Str(I, S);
    Lib.PutEntry('NAME'+S, Name);
    Status := Lib.GetStatus;
    if Status <> 0 then
      WriteLn('Failed to Add NAME', S, ', Status = ', Status)
    else
      WriteLn('NAME', S, ' added to Library');
    Name.Done;
  end;
  Lib.Done;
  {Re-open the existing Library, Re-register the objects}
  if not Lib.Init('NAMEDATA.OPL', SOpen, 1024, 'NAMEDATA') then begin
    WriteLn('Failed to Open Library,  Status = ', InitStatus);
    Halt;
  end;
  Lib.RegisterHier(NameDataStream);
  {Retrieve the third entry by name, display it, then close the Library}
  Lib.GetEntry('NAME3', Name);
  if Lib.GetStatus <> 0 then begin
    WriteLn('Error reading NAME3');
    Halt;
  end;
  Name.Display;
  Lib.Done;
  Name.Done;
end.
