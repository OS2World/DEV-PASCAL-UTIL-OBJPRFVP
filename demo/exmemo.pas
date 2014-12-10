program MemoExample; {EXMEMO.PAS}
uses
  OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpMemo;
const
  BufSize = 20;
var
  M : Memo;
  Buffer : array[1..BufSize] of Char;
  ExitCommand : Word;
begin
  {mark the text buffer as empty}
  Buffer[1] := ^Z;
  {initialize memo editor with custom window options}
  if not M.InitCustom(2, 4, 79, 24,                   {Window coordinates}
                      DefaultColorSet,               {ColorSet}
                      DefWindowOptions or wBordered, {Window options}
                      BufSize,                       {Buffer size}
                      @Buffer)                       {Buffer pointer}
  then begin
    WriteLn('Failed to init Memo. Status = ', InitStatus);
    Halt;
  end;
  {Use built-in status and error handlers}
  M.SetStatusProc(MemoStatus);
  M.SetErrorProc(MemoError);
  repeat
    M.Process;
    ExitCommand := M.GetLastCommand;
    case ExitCommand of
      ccQuit, ccError : {do nothing} ;
      {...process user exit commands...}
    end;
  until (ExitCommand = ccQuit) or (ExitCommand = ccError);
  M.Erase;
  M.Done;
  ClrScr;
end.
