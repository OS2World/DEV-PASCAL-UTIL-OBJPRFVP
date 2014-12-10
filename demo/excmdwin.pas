program CommandWindowExample;  {EXCMDWIN.PAS}
uses
  OpCrt,
  OpConst,   {!!.20}
  OpRoot,
  OpCmd,
  OpFrame,
  OpWindow;
const
  {Define a trivial KeySet of a few cursor commands}
  KeyMax  = 19;
  KeySet  : array[0..KeyMax] of Byte = (
  {length keys         command type   key sequence}
  3,      $00, $48,    ccUp,          {Up}
  3,      $00, $50,    ccDown,        {Down}
  3,      $00, $4B,    ccLeft,        {Left}
  3,      $00, $4D,    ccRight,       {Right}
  2,      $1B,         ccQuit,        {Esc}
  0);                                 {Terminate with null!}
type
  SampleWindow =
    object(CommandWindow)
      procedure ProcessSelf; virtual; {!!.23}
    end;
var
  Commands : CommandProcessor;
  CmdWin : SampleWindow;
  Finished : Boolean;
  procedure SampleWindow.ProcessSelf; {!!.23}
  begin
    repeat
      {Get a command}
      GetNextCommand;
      case GetLastCommand of
        ccUp :    WriteLn('ccUp');
        ccDown :  WriteLn('ccDown');
        ccLeft :  WriteLn('ccLeft');
        ccRight : WriteLn('ccRight');
        ccQuit :  WriteLn('ccQuit');
        ccChar :  WriteLn('ccChar: ', Char(Lo(GetLastKey)));
        else      WriteLn('ccNone');
      end;
    until (GetLastCommand = ccQuit) or (GetLastCommand = ccError);
  end;
begin
  {Make a small CommandProcessor}
  Commands.Init(@KeySet, KeyMax);
  {Make a bordered CommandWindow}
  if not CmdWin.InitCustom(30, 5, 50, 15,                  {Window coordinates}
                           DefaultColorSet,                {Color set}
                           wBordered+wClear+wSaveContents, {Window options}
                           Commands,                       {Command processor}
                           ucNone)                         {Unit code}
  then begin
    WriteLn('Failed to init CommandWindow. Status = ', InitStatus);
    Halt;
  end;
  {Add headers and draw window}
  CmdWin.wFrame.AddHeader(' Command window ', heTC);
  CmdWin.wFrame.AddHeader(' <Esc> to Quit ', heBC);
  CmdWin.Draw;
  {Get and process commands}
  Finished := False;
  repeat
    CmdWin.Process;
    case CmdWin.GetLastCommand of
      ccQuit : Finished := True;                         {Quit}
      ccError : begin                                    {Error}
                  WriteLn('Error: ', CmdWin.GetLastError);
                  Finished := True;
                end;
      ccUser0..ccUser55 : WriteLn('user command');       {Handle exit command}
    end;
  until Finished;
  {Clean up}
  CmdWin.Done;
  Commands.Done;
end.
