program DirectoryListExample;  {EXDIR.PAS}
uses
  Dos, OpString, OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpPick, OpDir;
var
  Dir : DirList;
  Finished : Boolean;
begin
  if not Dir.InitCustom(25, 5, 55, 20,                 {Window coordinates}
                        DefaultColorSet,               {ColorSet}
                        DefWindowOptions or wBordered, {Window options}
                        MaxAvail,                      {Heap space for files}
                        PickVertical,                  {Pick orientation}
                        SingleFile)                    {Command handler}
  then begin
    WriteLn('Failed to Init DirList,  Status = ', InitStatus);
    Halt;
  end;
  {Set desired DirList features}
  Dir.SetPosLimits(1, 1, ScreenWidth, ScreenHeight-1);
  Dir.SetPadSize(1, 1);
  Dir.diOptionsOn(diOptimizeSize);
  Dir.AddMaskHeader(True, 1, 30, heTC);
  Dir.SetSortOrder(SortName);
  Dir.SetNameSizeTimeFormat('<dir>', 'Mm/dd/yy', 'Hh:mmt');
  Dir.SetMask('*.*', AnyFile);
  {Pick a file}
  Finished := False;
  repeat
    Dir.Process;
    case Dir.GetLastCommand of
      ccSelect :
        FastWrite(Pad('Selected: '+Dir.GetSelectedPath, ScreenWidth),
                  ScreenHeight, 1, TextAttr);
      ccError  :
        begin               {!!.01}
          FastWrite('Error '+Long2Str(Dir.RawError), {!!.11}
                    ScreenHeight, 1, TextAttr);
          Finished := True; {!!.01}
        end;                {!!.01}
      ccQuit   :
        Finished := True;
      {Process other exit commands}
    end;
  until Finished;
  Dir.Done;
end.
