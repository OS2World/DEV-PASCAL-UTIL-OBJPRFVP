program MemoFileExample; {EXMEMOF.PAS}
uses
  OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpMemo;
const BufSize = 65521;
var
  MF : MemoFile;
  FSize : LongInt;
  ExitCommand : Word;
begin
  {initialize MemoFile with custom window options}
  if not MF.InitCustomAndAlloc(2, 4, 79, 24,               {Window coordinates}
                               DefaultColorSet,            {ColorSet}
                               DefWindowOptions or wBordered, {Window options}
                               BufSize)                    {Buffer size}
  then begin
    WriteLn('Failed to init MemoFile. Status = ', InitStatus);
    Halt;
  end;
  {Use built-in status and error handlers}
  MF.SetStatusProc(MemoStatus);
  MF.SetErrorProc(MemoError);
  {create/read in a file}
  MF.ReadFile('TEMP.MEM', FSize);
  {assign ccSaveFile to <F2> and ccNewFile to <F3>}
  MemoCommands.AddCommand(ccSaveFile, 1, $3C00, 0);
  MemoCommands.AddCommand(ccNewFile,  1, $3D00, 0);
  repeat
    MF.Process;
    {Process exit commands}
    ExitCommand := MF.GetLastCommand;
    case ExitCommand of
      ccSaveFile : MF.SaveFile;
      ccQuit     : if MF.meOptionsAreOn(meModified) then
                     MF.SaveFile;
      ccNewFile  : begin
                     {...prompt for new filename, call ReadFile...}
                   end;
      {...user exit commands...}
    end;
  until (ExitCommand = ccQuit) or (ExitCommand = ccError);
  MF.Erase;
  MF.Done;
  ClrScr;
end.
