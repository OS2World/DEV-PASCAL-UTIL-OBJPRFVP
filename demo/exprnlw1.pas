program DosPrtExample;  {EXPRNLW1.PAS}
uses
  OpRoot, OpPrnLow;
const
  Hello : String[12] = 'Hello World'^L;
var
  MyPrinter : DosPrinter;
begin
  if not MyPrinter.Init('PRN', Prn) then begin
    WriteLn('Failed to init printer. Status = ', InitStatus);
    Halt;
  end;
  MyPrinter.PrnPutBlock(Hello[1], Length(Hello));
  MyPrinter.Done;
end.
