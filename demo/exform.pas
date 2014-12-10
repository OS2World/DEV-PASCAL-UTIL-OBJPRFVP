{$V-}                         {<- required for OPFORM}
program FormExample;  {EXFORM.PAS}
uses
  Dos, OpInline, OpString, OpRoot, OpDos, OpDate,
  OpPrnLow, OpPrint, OpDevice, OpForm;
var
  PrnPtr : PrinterPtr;        {pointer to the Printer object}
  FrmPtr : PrintedFormPtr;    {pointer to the PrintedForm object}
  Index : Word;               {record counter}
  DataRec :                   {data record}
    record
      Name : String;
      Addr : String;
      DaysLate : Word;
      Balance : Real;
    end;
{$F+}
function GetNextRecord(P : PrintBlockPtr) : Boolean;
begin
  if Index > 10 then
    GetNextRecord := False
  else
    with DataRec do begin
      {Fill the global record var with data}
      Name := 'Name' + Long2Str(Index);
      Addr := 'Address' + Long2Str(Index);
      DaysLate := 120;
      Balance := 12.34;
      GetNextRecord := True;
      Inc(Index);
    end;
end;
{$F-}
procedure BuildForm;
var
  I : Word;
begin
  {Make a printer device and a form}
  PrnPtr := New(PrinterPtr, Init('DUMP.PRN', DiskFile, pGenericLineFields)); {!!.20}
  if PrnPtr = nil then begin
    WriteLn('Failed to init Printer');
    Halt;
  end;
  FrmPtr := New(PrintedFormPtr, Init(1, PrnPtr));
  if FrmPtr = nil then begin
    WriteLn('Failed to init Form');
    Halt;
  end;
  {Define the fields of the form}
  FrmPtr^.NewPage;
  FrmPtr^.SetHeader(Center('FormExample Header', 80), 1, 1, Dim);
  FrmPtr^.NewBlock(2, 1, GetNextRecord);
  FrmPtr^.AddStringField(1, 'Customer Name:', 2, 3, Bold, 16,
                         2, NullPicture, 2, 20, Dim, 20, DataRec.Name);
  FrmPtr^.AddStringField(3, 'Customer Address:', 3, 3, Bold, 16,
                         4, NullPicture, 3, 20, Dim, 20, DataRec.Addr);
  FrmPtr^.AddWordField(5, 'Days Late:', 4, 3, Bold, 16,
                       6, NullPicture, 4, 20, Dim, 5, DataRec.DaysLate);
  FrmPtr^.AddRealField(7, 'Overdue Balance:', 4, 30, Bold, 16,
                       8, '$###,###.##', 4, 48, Dim, 2, 11, DataRec.Balance);
  FrmPtr^.AddBox(10, 1, 1, 65, 5, 0, 0, SingleLineBox, Bold);
  for I := 1 to 9 do
    FrmPtr^.DupCurrentBlock(2+(I*5), 1);
end;

begin
  BuildForm;                  {initialize the form}
  Index := 1;                 {start with the first record}
  FrmPtr^.Process;            {print the report}
  Dispose(FrmPtr, Done);      {clean up}
  Dispose(PrnPtr, Done);
end.
