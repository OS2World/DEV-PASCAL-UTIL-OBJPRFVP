{$R+}
program TestErrHan;  {EXERRHAN.PAS}

uses
  Dos,
  OpInline, {!!.22}
  OpErrHan,
  OpString;
var
  I, J, K : Integer;
  V : 1..10;
  {$F+} {Must use the FAR model}
  function RecoverError : Boolean;
    {-Invoked when an error occurs. Return True to enable recovery}
  var
    I : Word;
  begin
    {Clear IoResult}
    I := IoResult;
    WriteLn('Error handler sees error ', ExitCode:3, ' at ',
            HexW(OS(ErrorAddr).S), ':', HexW(OS(ErrorAddr).O)); {!!.22}
    RecoverError := True;
  end;
begin
  {Enable error recovery, and install user routine}
  ErrorRecovery(True, @RecoverError);
  I := 0;
  J := 1;
  K := J div I;
  if ExitCode <> 0 then
    WriteLn('Divide by zero error occurred');
  I := 20;
  V := I;
  if ExitCode <> 0 then
    WriteLn('Range error occurred');
end.
