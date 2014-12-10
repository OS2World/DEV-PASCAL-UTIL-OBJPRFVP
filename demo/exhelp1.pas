program HelpExample;  {EXHELP1.PAS}
uses
  OpCrt, OpRoot, OpCmd, OpMouse, OpFrame, OpWindow, OpPick, OpHelp;
var
  PH : PagedHelpWindow;
  Finished : Boolean;
begin
  {Make a PagedHelpWindow with custom window options}
  if not PH.InitCustom(9, 8, 72, 18,                    {Window coordinates}
                       DefaultColorSet,                 {ColorSet}
                       DefWindowOptions or wBordered,   {Window options}
                       'ENTRY.HLP',                     {Help file}
                       PickVertical)                    {Pick Orientation}
  then begin
    WriteLn('Failed to Init PagedHelpWindow,  Status = ', InitStatus);
    Halt;
  end;
  {Add some features}
  PH.EnableExplosions(10);
  PH.wFrame.AddHeader(' Topic Index ', heTC);
  PH.AddMoreHeader(' || for more ', heBR, #24, #25, '', 2, 3, 0);
  PH.AddTopicHeader(1, 60, heTC);
  PH.AddMoreHelpHeader(
    ' PgUp/PgDn for more ', heBR, 'PgUp', 'PgDn', '/', 2, 7, 6);
  PH.wFrame.AddScrollBar(frRR, 0, MaxLongInt, DefaultColorSet);
  PH.hwFrame.AddScrollBar(frRR, 0, MaxLongInt, DefaultColorSet);
  {Loop through help system}
  Finished := False;
  repeat
    {Display master index and pick a help topic}
    PH.SetTopic(PickTopic);
    PH.Process;
    case PH.GetLastCommand of
      ccError,
      ccQuit :
        Finished := True;
      ccSelect :
        begin
          PH.SetTopic(PH.GetTopicChoice);
          repeat
            PH.Process;
            case PH.GetLastCommand of
              ccError :
                Finished := True;
              ccQuit :
                if not PH.InHelpMode then
                  {Escaped from pick window within help}
                  Finished := True;
              {...other Help or PickList exit commands...}
            end;
          until Finished or (PH.GetLastCommand = ccQuit);
        end;
      {...other PickList exit commands...}
    end;
  until Finished;
  PH.Done;
end.
