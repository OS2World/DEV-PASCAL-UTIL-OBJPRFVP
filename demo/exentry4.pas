{$V-}                        {<- required for OPENTRY}
program ScrollingEntryExample; {EXENTRY4.PAS}
uses
  OpString, OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpField, OpSelect, OpEntry;
type
  InfoRec = record
              Name : String[30];
              Age : Byte;
              Wage : Real;
              Misc : array[1..10] of String[10];
            end;
var
  Info : InfoRec;
  ExitCommand : Word;
  SES : ScrollingEntryScreen;
  Finished : Boolean;
  I : Word;
begin
  ClrScr;
  FillChar(Info, SizeOf(Info), 0);
  if not SES.InitCustom(25, 5, 55, 10,                 {Window coordinates}
                        DefaultColorSet,               {ColorSet}
                        DefWindowOptions OR wBordered) {Window options}
  then begin
    WriteLn('Failed to init EntryScreen. Status = ', InitStatus); Halt;
  end;
  {add window frills}
  SES.wFrame.AddShadow(shBR, shOverWrite);
  SES.wFrame.AddHeader(' Scrolling Entry Screen ', heTC);
  {set wrap mode}
  SES.SetWrapMode(StopAtEdges);
  {Add our data entry fields}
  SES.esFieldOptionsOn(efClearFirstChar);
  SES.AddStringField('Name:', 2, 1, '', 2, 7, 20, 0, Info.Name);
  SES.AddByteField('Age:', 3, 1, '999', 3, 7, 0, 1, 150, Info.Age);
  SES.AddRealField('Wage:',4, 1, '999.99', 4, 7, 0, 0.0, 0.0, 2, Info.Wage);
  for I := 1 to 10 do
    SES.AddStringField(
      'Misc'+Long2Str(I), I+4, 1, '', I+4, 7, 10, 0, Info.Misc[I]);
  {allocate the virtual screen}
  SES.AllocateScreen;
  {check for errors}
  I := SES.GetLastError;
  if I <> 0 then begin
    WriteLn('Error ', I, ' creating entry screen'); Halt;
  end;
  Finished := False;
  repeat
    SES.Process;
    case SES.GetLastCommand of
      ccQuit, ccDone : Finished := True;
      ccError :
        begin
          WriteLn('Fatal error ', SES.GetLastError);
          Finished := True;
        end;
      {... process other exit commands ...}
    end;
  until Finished;
  SES.Erase;
  SES.Done;
end.
