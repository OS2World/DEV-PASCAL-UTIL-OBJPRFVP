program EditorExample; {EXEDITOR.PAS}
uses OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpMemo, OpEditor;
var
  TE : TextEditor;
  FSize : LongInt;
  ExitCommand : Word;
  AllDone : Boolean;
begin
  {instantiate a TextEditor with a bordered window}
  if not TE.InitCustom(2, 4, 79, 24,                {Window coordinates}
                       DefaultColorSet,             {ColorSet}
                       DefWindowOptions or wBordered, {Window options}
                       65521)                       {Buffer size}
  then begin
    WriteLn('Failed to init TextEditor. Status = ', InitStatus);
    Halt;
  end;
  {use built-in status and error handlers provided by OPMEMO}
  TE.SetStatusProc(MemoStatus);
  TE.SetErrorProc(MemoError);
  {clear error line} {!!.13}
  FastFill(ScreenWidth, ' ', ErrorRow, 1, ColorMono(ErrorColor, ErrorMono)); {!!.13}
  {Create/read in a text file}
  TE.ReadFile('READ.ME', FSize);
  AllDone := False;
  repeat
    TE.Process;
    ExitCommand := TE.GetLastCommand;
    case ExitCommand of
      ccSaveExit,            {Save and exit -- file already saved}
      ccAbandonFile,         {Abandon file}
      ccError :              {Fatal error}
        AllDone := True;
      {...user exit commands..}
    end;
  until AllDone;
  TE.Erase;
  TE.Done;
  ClrScr;
end.
