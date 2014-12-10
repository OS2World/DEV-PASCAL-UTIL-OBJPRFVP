{$I-}
program TFDDExample;  {EXPRNLW3.PAS}
uses OpRoot, OpPrnLow;
var  Lst : Text;
     PrnLpt2 : BiosPrinter;
begin
  {Initialize the BiosPrinter object for LPT2}
  if not PrnLpt2.Init(Lpt2) then begin
    WriteLn('Failed to init printer. Status = ', InitStatus); Halt;
  end;
  {Assign the printer object to the text file Lst and open the file}
  AssignPrnObject(Lst, PrnLpt2);
  Rewrite(Lst);
  if IoResult <> 0 then begin
    WriteLn('Error opening TFDD'); Halt;
  end;
  WriteLn(Lst, 'This output goes to LPT2 via BIOS services.');
  Close(Lst);
end.
