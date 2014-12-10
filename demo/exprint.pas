program PrinterExample;  {EXPRINT.PAS}
uses OpRoot, OpPrnLow, OpPrint;
var  MyPrinter : Printer;
begin
  if not MyPrinter.Init('PRN', Prn, 0) then begin
    WriteLn('Failed to init printer. Status = ', InitStatus);
    Halt;
  end;
  MyPrinter.AddPrintMode('Italics', #27'4', #27'5', 0, 0, Italics, 0);
  MyPrinter.TurnOnByID(Italics);
  MyPrinter.PrintStr('Hello World');
  MyPrinter.Position(50, 1, rtAbsolute);
  MyPrinter.TurnOffByID(Italics);
  MyPrinter.PrintStr('Goodbye World');
  MyPrinter.FormFeed;
  MyPrinter.Done;
end.

