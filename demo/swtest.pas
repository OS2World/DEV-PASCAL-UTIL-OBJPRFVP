program SwitchTest;
  {-Demonstrates use of the OPSWITCH unit.}

uses
  OpCrt, OpSwitch;

var
  I : Word;
  Ch : Char;
const
  Values : array[WhichScreen] of Char = ('!', ' ');

begin
  {select preferred modes to switch to}
  MonoMode := 7;
  ColorMode := CO80;

  {make sure we have dual displays}
  if not HasDualDisplays then begin
    WriteLn('This program requires dual displays');
    Halt(1);
  end;

  {display brief instructions}
  CheckBreak := False;
  Write('Press <Esc> to stop, any other key to switch screens...');
  Delay(1000);

  ClrScr;

  repeat
    {display something}
    Write(Values[CurrentScreen]);

    if KeyPressed then begin
      Ch := ReadKey;

      {quit if ESC pressed}
      if Ch = #27 then begin
        SwitchScreens(Screen1);
        Exit;
      end
      {else switch screens}
      else if CurrentScreen = Screen1 then
        SwitchScreens(Screen2)
      else
        SwitchScreens(Screen1);

      {increment counter for current screen}
      Inc(Values[CurrentScreen]);
    end;
  until False;
end.
