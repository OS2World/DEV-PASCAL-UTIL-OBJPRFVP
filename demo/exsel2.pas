{$V-,F+}
program ScrollingSelectorExample; {EXSEL2.PAS}
uses
  Use32,
  OpCrt, OpString, OpRoot, OpCmd, OpFrame, OpWindow, OpSelect, OpEdit;
var
  SSel     : ScrollingSelector;
  Finished : Boolean;
  Data     : array[0..29] of string[12];
  I        : Word;
  procedure Action(SP : SelectorPtr; ID : Word);
    {-Called by Selector when Enter is pressed}
  var
    LE : LineEditor;
  begin
    LE.Init(DefaultColorSet);
    LE.leEditOptionsOff(leHouseCursorAtEnd);
    LE.ReadString('', WhereYabs, WhereXabs, 12, 12 , Data[ID]);
    LE.Done;
  end;
  procedure GetField(ID : Word; NeedPrompt : Boolean; var S : string);
    {-Called by Selector to display field strings}
  begin
    if NeedPrompt then
      S := 'Field '+LeftPad(Long2Str(ID), 2)
    else
      S := Pad(Data[ID], 12);
  end;
begin
  FillChar(Data, SizeOf(Data), 0);
  if not SSel.InitCustom(27, 6, 52, 15,                 {Window coordinates}
                         DefaultColorSet,               {ColorSet}
                         DefWindowOptions OR wBordered) {Window options}
  then begin
    WriteLn('Failed to init Selector. Status = ', InitStatus); Halt;
  end;
  {add some window frills}
  SSel.EnableExplosions(10);
  SSel.wFrame.AddShadow(shBR, shOverWrite);
  SSel.wFrame.AddScrollBar(frRR, 0, MaxLongInt, DefaultColorSet);
  SSel.wFrame.AddHeader(' Scrolling Selector ', heTC);
  {selector options}
  SSel.SetWrapMode(StopAtEdges); {<-- best setting for a ScrollingSelector}
  {set procedure pointers}
  SSel.SetGetFieldProc(GetField);
  SSel.SetActionProc(Action);
  {add fields}
  for I := 1 to 30 do
    SSel.AddField(I, 03, 08,     {Prompt Row, Col, Width}
                  I, 12, 12,     {Field Row, Col, Width}
                  0);            {Help Index}
  SSel.AddTextField('-Fixed text field-', 31, 4);
  {allocate the virtual screen}
  SSel.AllocateScreen;
  {check for errors}
  I := SSel.GetLastError;
  if I <> 0 then begin
    WriteLn('Error ', I, ' creating selector');
    Halt;
  end;
  Finished := False;
  repeat
    SSel.Process;
    case SSel.GetLastCommand of
      ccQuit :  Finished := True;
      ccError :
        begin
          WriteLn('Fatal error ', SSel.GetLastError);
          Finished := True;
        end;
      {... process other exit commands ...}
    end;
  until Finished;
  {erase and dispose of selector}
  SSel.Erase;
  SSel.Done;
end.
