program CommandProcessorExample; {EXCMDPRC.PAS}
uses
  Use32, OpCrt, OpCmd;
const
  {Define a small KeySet of a few cursor commands}
  KeySetId : string[14] = 'KeySet follows';
  KeyMax  = 30;
  KeySet  : array[0..KeyMax] of Byte = (
  {length keys         command type   key sequence}
  3,      $00, $48,    ccUp,          {Up}
  3,      $00, $50,    ccDown,        {Down}
  3,      $00, $4B,    ccLeft,        {Left}
  3,      $00, $4D,    ccRight,       {Right}
  2,      $1B,         ccQuit,        {Esc}
  0,0,0,0,0,0,0,0,0,0,0,0);           {<-room for more commands}
var
  Commands : CommandProcessor;
  Cmd, Key : Word;
begin
  {Make a small CommandProcessor}
  Commands.Init(@KeySet, KeyMax);

  {Add a user exit command -- <F1>}
  Commands.AddCommand(ccUser0, 1, $3B00, 0);
  {check for error adding the command}
  if Commands.GetLastError <> 0 then
    RingBell;

  repeat
    Cmd := Commands.GetCommand(Key);
    case Cmd of
      ccChar  : WriteLn('ccChar: "', Char(Lo(Key)), '"' );
      ccUp    : WriteLn('ccUp');
      ccDown  : WriteLn('ccDown');
      ccLeft  : WriteLn('ccLeft');
      ccRight : WriteLn('ccRight');
      ccQuit  : WriteLn('ccQuit');
      ccUser0 : WriteLn('ccUser0');
      else      WriteLn('ccNone');
    end;
  until (Cmd = ccQuit);
end.
