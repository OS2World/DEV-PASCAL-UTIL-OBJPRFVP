program CommandPackerDemo; {EXCMDPK.PAS}
uses
  Use32, OpCrt, OpCmd;
const
  {Define a KeySet of a few cursor commands}
  KeySetId : string[14] = 'KeySet follows';
  KeyMax  = 30;
  KeySet  : array[0..KeyMax] of Byte = (
  {length keys         command type   key sequence}
  2,      $1B,         ccQuit,        {Esc}
  3,      $00, $4B,    ccLeft,        {Left}
  3,      $00, $4D,    ccRight,       {Right}
  3,      $00, $48,    ccUp,          {Up}
  3,      $00, $50,    ccDown,        {Down}
  0,0,0,0,0,0,0,0,0,0,0,0);           {<-room for more commands}

  MaxCmd  = ccDown;             {highest legal command in table}
  NumCols = 3;                  {number of columns in unpacked command table}
var
  Packer : CommandPacker;       {CommandPacker object}
  UKeySet : array[1..MaxCmd, 1..NumCols] of KeyRec; {Unpacked command table}
  Commands : CommandProcessor;  {CommandProcessor object}
  Cmd, Key : Word;              {last key, command}
begin
  {Unpack the KeySet command table}
  Packer.Init(@KeySet,       {Pointer to packed command table}
              MaxCmd,        {Highest legal command in packed command table}
              KeyMax,        {Highest legal index into packed command table}
              @UKeySet,      {Pointer to unpacked command table}
              NumCols,       {Number of "columns" in unpacked command table}
              False);        {Commands are bytes, not words}

  {UKeySet is now available for modification...change ccUp from <Up> to <F1>}
  UKeySet[ccUp, 1].Keys := #$00#$3B;
  UKeySet[ccUp, 1].Modified := True;

  {Check the modified UKeySet for keystroke conflicts}
  if Packer.ConflictsFound then
    WriteLn('Keystroke conflict was found');

  {Repack the UKeySet array into the KeySet array}
  if not Packer.PackKeys then
    WriteLn('Failed to repack command table')
  else
    Writeln('Keys repacked successfully');

  {Demonstrate our custom command table}
  Commands.Init(@KeySet, KeyMax);

  repeat
    Cmd := Commands.GetCommand(Key);
    case Cmd of
      ccUp    : WriteLn('ccUp');
      ccDown  : WriteLn('ccDown');
      ccLeft  : WriteLn('ccLeft');
      ccRight : WriteLn('ccRight');
      ccQuit  : WriteLn('ccQuit');
      else      WriteLn('ccNone');
    end;
  until (Cmd = ccQuit);
end.
