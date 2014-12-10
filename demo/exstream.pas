program StreamExample;  {EXSTREAM.PAS}
uses
  OpRoot;
const
  otNameData = 1000;         {Name object types}
  otCustData = 1001;
  veNameData = 0;            {Name object versions}
  veCustData = 0;
type
  {An object for manipulating Name and Age}
  NameDataPtr = ^NameData;
  NameData =
    object(Root)
      Name : String[20];
      Age  : Byte;
      constructor Init;
      procedure Display;
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
    end;
  {Descendant from NameData, adds Company name}
  CustData =
    object(NameData)
      CompanyName : String[20];
      constructor Init;
      procedure Display;
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
    end;
var
  Customer : CustData;
  S : BufIdStream;
  I, Status : Word;
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
  constructor NameData.Load(var S : IdStream);
  begin
    Root.Init;
    Name := S.ReadString;
    S.Read(Age, SizeOf(Age));
    if S.PeekStatus <> 0 then
      Fail;
  end;
  procedure NameData.Store(var S : IdStream);
  begin
    S.WriteString(Name);
    S.Write(Age, SizeOf(Age));
  end;
  {$F+}
  procedure NameDataStream(SPtr : IdStreamPtr);
  begin
    with SPtr^ do begin
      RegisterType(otNameData, veNameData,       {Object code, version number}
                   TypeOf(NameData),             {Object VMT pointer}
                   @NameData.Store,              {Stream write procedure}
                   @NameData.Load);              {Stream read procedure}
    end;
  end;
  {$F-}
  {---- CustData methods -----------------}
  constructor CustData.Init;
  begin
    NameData.Init;
    Write('Co. Name: ');
    ReadLn(CompanyName);
  end;
  procedure CustData.Display;
  begin
    NameData.Display;
    WriteLn(CompanyName);
  end;
  constructor CustData.Load(var S : IdStream);
  begin
    if not NameData.Load(S) then
      Fail;
    CompanyName := S.ReadString;
    if S.PeekStatus <> 0 then
      Fail;
  end;
  procedure CustData.Store(var S : IdStream);
  begin
    NameData.Store(S);
    S.WriteString(CompanyName);
  end;
  {$F+}
  procedure CustDataStream(SPtr : IdStreamPtr);
  begin
    NameDataStream(SPtr);
    with SPtr^ do
      RegisterType(otCustData, veCustData, TypeOf(CustData),
                   @CustData.Store, @CustData.Load);
  end;
  {$F-}
begin
  {Create a Stream, check for success}
  if not S.Init('CUSTOMER.STM', SCreate, 1024) then begin
    Writeln('Failed to Create Stream,  Status = ', InitStatus);
    Halt;
  end;
  {Register object hierarchy}
  S.RegisterHier(CustDataStream);
  Status := S.GetStatus;
  if Status <> 0 then begin
    Writeln('Failed to Register objects,  Status = ', Status);
    Halt;
  end;
  {Create and Store three Customer objects, then close the Stream}
  for I := 1 to 3 do begin
    Customer.Init;
    S.Put(Customer);
    Status := S.GetStatus;
    if Status <> 0 then begin
      WriteLn('Failed to Write Customer,   Status = ', Status);
      Halt;
    end;
    Customer.Done;
  end;
  S.Done;
  {Re-open the existing Stream}
  if not S.Init('CUSTOMER.STM', SOpen, 1024) then begin
    Writeln('Failed to Open Stream,  Status = ', InitStatus);
    Halt;
  end;
  {Re-register object hierarchy}
  S.RegisterHier(CustDataStream);
  Status := S.GetStatus;
  if Status <> 0 then begin
    Writeln('Failed to register objects. Status = ', Status);
    Halt;
  end;
  {Retrieve Customers from Stream, Display them, then close the Stream}
  for I := 1 to 3 do begin
    S.Get(Customer);
    Status := S.GetStatus;
    if Status <> 0 then begin
      WriteLn ('Failed to read Customer. Status = ', Status);
      Halt;
    end;
    Customer.Display;
    Customer.Done;
  end;
  S.Done;
end.
