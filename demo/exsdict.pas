program StringDictionaryExample;  {EXSDICT.PAS}
uses
  OpRoot;
type
  NameDataPtr = ^NameData;
  NameData = record
    Age : Byte;
    Addr : String;
  end;
var
  NamePtr : NameDataPtr;
  NameDict : StringDict;
  Status : Word;
begin
  {Initialize a StringDict}
  if not NameDict.Init then begin
    Writeln('Failed to make StringDict,  Status = ', InitStatus);
    Halt;
  end;
  {Create and add some names to the StringDict}
  New(NamePtr);
  with NamePtr^ do begin
    Age := 20;
    Addr := '123 Vintage Lane';
  end;
  NameDict.Add('Nessus', LongInt(NamePtr));
  New(NamePtr);
  with NamePtr^ do begin
    Age := 46;
    Addr := 'Long House';
  end;
  NameDict.Add('Beowulf', LongInt(NamePtr));
  {Check status after Adds}
  Status := NameDict.GetStatus;
  if Status <> 0 then begin
    WriteLn('Failed to add Names, Status = ', Status);
    Halt;
  end;
  {Search for a particular name}
  if NameDict.Member('Nessus', LongInt(NamePtr)) then begin
    WriteLn('Found');
    with NamePtr^ do begin
      WriteLn('Age ', Age);
      WriteLn('Address ', Addr);
    end;
  end else
    Writeln('Not found');
  {Retrieve and dispose the NamePtrs}
  if NameDict.Member('Nessus', LongInt(NamePtr)) then
    Dispose(NamePtr);
  if NameDict.Member('Beowulf', LongInt(NamePtr)) then
    Dispose(NamePtr);
  NameDict.Done;
end.
