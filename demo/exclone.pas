{$A-}               {required for OPCLONE}
program CloneTest;  {EXCLONE.PAS}
uses
  Dos, OpInline, OpString, OpRoot, OpClone;
const
  {mark start of installation area}
  Id : string[12] = 'CloneTest ID';
  {configuration data}
  TimesRun : LongInt = 0;
  TestString : string[20] = 'Hello World!';
  {mark end of installation area}
  CfgEnd : Byte = 0;
var
  C : Cloner;
begin
  {show that it works}
  WriteLn('Program has been run successfully ', TimesRun, ' time(s)');
  Inc(TimesRun);
  WriteLn('Test string = "', TestString, '"');
  Write('Enter new test string: ');
  ReadLn(TestString);
  {initialize cloner}
  if not C.InitCustom('EXCLONE.EXE', UpdateAll, DefBufSize) then
    WriteLn('Unable to modify EXE file')
  else begin
    {locate configuration data}
    if not C.FindDefaultsEnd(Id, SizeOf(Id), 0) then
      WriteLn('Unable to locate configuration data')
    else begin
      {write out the new values}
      C.StoreDefaults(C.GetPos, Id, Ofs(CfgEnd)-Ofs(Id));
      {check for errors}
      if C.GetLastError <> 0 then
        WriteLn('Error storing configuration data')
      else
        WriteLn('Configuration data stored successfully');
    end;
    {close the EXE file}
    C.Done;
  end;
end.
