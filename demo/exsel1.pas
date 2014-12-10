{.$DEFINE AutoSelect} {if defined, selector simulates entry screen}
{$V-,F+}
program SelectorExample; {EXSEL1.PAS}
uses
  Use32,
  OpCrt, OpString, OpRoot, OpCmd, OpFrame, OpWindow, OpSelect, OpEdit;
var
  Sel      : Selector;
  Finished : Boolean;
  Data     : array[0..5] of string[12];
  I        : Word;
  procedure Action(SP : SelectorPtr; ID : Word);
  var
    LE : LineEditor;
  begin
    LE.Init(DefaultColorSet);
    LE.leEditOptionsOff(leHouseCursorAtEnd);
    {$IFDEF AutoSelect}
    LE.leEditOptionsOn(leAutoAdvance);
    LE.leSecEditOptionsOff(sleNoFieldMovement);
    {$ENDIF}
    LE.ReadString('', WhereYabs, WhereXabs, 12, 12 , Data[ID]);
    {$IFDEF AutoSelect}
    if LE.GetLastCommand = ccSelect then
      SP^.SetLastCommand(ccNextField)
    else
      SP^.SetLastCommand(LE.GetLastCommand);
    {$ENDIF}
    LE.Done;
  end;
  procedure GetField(ID : Word; NeedPrompt : Boolean; var S : string);
  begin
    if NeedPrompt then
      S := 'Field ' + Long2Str(ID)
    else
      S := Pad(Data[ID], 12);
  end;
  procedure PreSelect(SP : SelectorPtr; ID : Word);
  begin
    FastWrite('Current Field:  ' + Long2Str(ID), 25, 1, TextAttr);
  end;
  procedure PostSelect(SP : SelectorPtr; ID : Word);
  begin
    FastWrite('Previous Field: ' + Long2Str(ID), 24, 1, TextAttr);
  end;
begin
  {instantiate Sel}
  if not Sel.InitCustom(27, 6, 52, 15,              {Window coordinates}
                        DefaultColorSet,            {ColorSet}
                        DefWindowOptions+wBordered) {Window options}
  then begin
    WriteLn('Failed to init Selector. Status = ', InitStatus);
    Halt;
  end;
  {add some window frills}
  Sel.EnableExplosions(10);
  Sel.wFrame.AddShadow(shBR, shOverWrite);
  Sel.wFrame.AddScrollBar(frRR, 0, MaxLongInt, DefaultColorSet);
  Sel.wFrame.AddHeader(' Selector ', heTC);
  Sel.SetGetFieldProc(GetField);
  Sel.SetActionProc(Action);
  Sel.SetPreSelectProc(PreSelect);
  Sel.SetPostSelectProc(PostSelect);
  {$IFDEF AutoSelect}
  Sel.slOptionsOn(slAutoSelect);
  {$ENDIF}
  {add text fields}
  Sel.AddTextField('-Fixed text field-', 1, 4);
  Sel.AddTextField('-Fixed text field-', 10, 4);
  {add regular fields}
  FillChar(Data, SizeOf(Data), 0);
  for I := 1 to 6 do
    Sel.AddField(I+2, 03, 07,     {Prompt Row, Col, Width}
                 I+2, 12, 12,     {Field Row, Col, Width}
                 0);              {Help Index}
  Finished := False;
  repeat
    Sel.Process;
    case Sel.GetLastCommand of
      ccQuit :  Finished := True;
      ccError : begin
                  WriteLn('Fatal error ', Sel.GetLastError);
                  Finished := True;
                end;
      {... process other exit commands ...}
    end;
  until Finished;
  Sel.Erase;
  Sel.Done;
end.
