program BrowseExample; {EXBROWSE.PAS}
uses
  Dos, OpCrt, OpRoot, OpCmd, OpDos, OpFrame, OpWindow, OpBrowse;
var
  B : Browser;
  Status : Word;
  Finished : Boolean;
begin
  if not B.InitCustom(2, 3, ScreenWidth-1, ScreenHeight-1,   {Coordinates}
                      DefaultColorSet,               {ColorSet}
                      DefWindowOptions or wBordered, {Window options}
                      16384)                         {heap space to use} {!!.20}
  then begin
    WriteLn('Failed to init Browser. Status = ', InitStatus);
    Halt;
  end;
  {Open a file, check for success}
  B.OpenFile('d:\vp2b2\source\opro\demo\EXBROWSE.PAS');
  Status := B.GetLastError;
  if Status <> 0 then begin
    WriteLn('Failed to load file. Status = ', Status);
    Halt;
  end;
  {use built-in status routine}
  B.SetStatusProc(BrowseStatus);
  Finished := False;
  repeat
    B.Process;
    case B.GetLastCommand of
      ccQuit, ccError: Finished := True;
      {...user exit commands...}
    end;
  until Finished;
  B.Erase;
  B.Done;
  ClrScr;
end.
