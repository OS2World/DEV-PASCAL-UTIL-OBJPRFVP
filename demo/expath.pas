program PathListExample;  {EXPATH.PAS}
uses
  Dos, OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpPick, OpDir;
var
  Path : PathList;
  SelPath : PathStr;
  Result : Word;
begin
  {Make a PathList object with custom window options}
  if not Path.InitCustom(25, 5, 55, 20,             {Window coordinates}
                         DefaultColorSet,           {ColorSet}
                         DefWindowOptions or wBordered, {Window options}
                         8000,                      {Max heap space for files}
                         PickVertical,              {Pick orientation}
                         SinglePath)                {Command handler}
  then begin
    WriteLn('Failed to Init PathList,  Status = ', InitStatus);
    Halt;
  end;
  Path.paOptionsOn(paShowSize+paAltCurDir);
  Path.SetPadSize(1, 1);

  {Pick a directory}
  SelPath := Path.GetPathName('C');
  if SelPath <> '' then
    WriteLn('Selected: ', SelPath)
  else begin
    Result := Path.GetLastError;
    if Result <> 0 then
      WriteLn('Error selecting directory, Result = ', Result)
    else
      WriteLn('No directory selected');
  end;
  Path.Done;
end.
