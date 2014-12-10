{$V-}                        {<- required for OPENTRY}
program EntryExample; {EXENTRY1.PAS}
uses OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpField, OpSelect, OpEntry;
type
  InfoRec = record
              Name : string[30];
              Age : Byte;
              Wage, Weekly : Real;
            end;
var
  Info : InfoRec;
  ES : EntryScreen;
  Finished : Boolean;
  {$F+}
  procedure PreEdit(ESP : EntryScreenPtr);
  var
    S : string[80];
  begin
    case ESP^.GetCurrentID of
      0 : S := 'Last name, first name';     {Info.Name}
      1 : S := 'Enter age [1-150]    ';     {Info.Age}
      2 : S := 'Enter hourly wage    ';     {Info.Wage}
    end;
    ESP^.wFastWrite(S, 7, 4, TextAttr);
  end;
  procedure PostEdit(ESP : EntryScreenPtr);
  begin
    case ESP^.GetCurrentID of
      0, 1 : {no post-edit activity required};
      2 : begin
            if Info.Wage = BadReal then         {!!}
              Info.Weekly := 0                  {!!}
            else                                {!!}
              Info.Weekly := 40.0 * Info.Wage;
            ESP^.DrawField(3);
          end;
      3 : {protected field};
    end;
  end;
  {$F-}
begin
  ClrScr;
  FillChar(Info, SizeOf(Info), 0);
  if not ES.InitCustom(25, 5, 55, 11,                 {Window coordinates}
                       DefaultColorSet,               {ColorSet}
                       DefWindowOptions OR wBordered) {Window options}
  then begin
    WriteLn('Failed to init EntryScreen. Status = ', InitStatus);
    Halt;
  end;
  {add some window frills}
  ES.wFrame.AddShadow(shBR, shOverWrite);
  ES.wFrame.AddHeader(' Entry Screen ', heTC);
  {add fields}
  ES.esFieldOptionsOn(efClearFirstChar);
  ES.AddStringField('Name:', 2,1, '',        2,9, 20, 0,          Info.Name);
  ES.AddByteField('Age:',    3,1, '999',     3,9,     0, 1, 150,  Info.Age);
  ES.AddRealField('Wage:',   4,1, '99.99',   4,9,     0, 0, 0, 0, Info.Wage);
  ES.esFieldOptionsOn(efProtected);
  ES.AddRealField('Weekly:', 5,1, '9999.99', 5,9,     0, 0, 0, 0, Info.Weekly);
  ES.esFieldOptionsOff(efProtected);
  {set procedure pointers}
  ES.SetPreEditProc(PreEdit);
  ES.SetPostEditProc(PostEdit);
  Finished := False;
  repeat
    ES.Process;
    case ES.GetLastCommand of
      ccDone, ccQuit : Finished := True;
      ccError :
        begin
          WriteLn('Fatal error ', ES.GetLastError);
          Finished := True;
        end;
      {... process other exit commands ...}
    end;
  until Finished;
  ES.Erase;
  ES.Done;
end.
