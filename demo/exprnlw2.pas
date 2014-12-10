program BiosPrtExample;  {EXPRNLW2.PAS}
uses
  OpRoot, OpPrnLow;
const
  Hello : String[12] = 'Hello World'^L;
var
  MyPrinter : BiosPrinter;
begin
  if not MyPrinter.Init(Lpt1) then begin
    WriteLn('Failed to init printer. Status = ', InitStatus);
    Halt;
  end;
  MyPrinter.PrnPutBlock(Hello[1], Length(Hello));
  MyPrinter.Done;
end.
