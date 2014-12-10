program MakeLibrary;  {EXPRNLIB.PAS}
uses
  OpRoot, OpPrnLow, OpPrint, OpDevice;
const
  LibSig = 'PRNLIB';
  LibName = 'PRINTER.OPL';
  MaxEntries = 24;
var
  Lib : OpLibrary; {!!.20}
  Dev : Printer;
procedure ConstructorError(ObjectType : String);
begin
  WriteLn('Error creating ', ObjectType, ' Status = ', InitStatus);
  Halt;
end;
procedure CheckLibError;
var
  Status : Word;
begin
  Status := Lib.GetStatus;
  if Status <> 0 then begin
    WriteLn('Library error. Status = ', Status);
    Halt;
  end;
end;
begin
  {create a new library}
  if not Lib.Create(LibName, 4096, LibSig, MaxEntries) then
    ConstructorError('Library');
  {register object and procedure types in library}
  Lib.RegisterHier(PrinterStream);
  Lib.RegisterHier(LJDeviceStream);

  {write the GenericEpson device to the library}
  if not Dev.Init('', LPT1, 0) then
    ConstructorError('Printer');
  GenericEpsonRegister(@Dev);
  Lib.PutEntry('GENERICEPSON', Dev);
  CheckLibError;

  {write the LaserJet device to the library}
  Deregister(@Dev);
  LJRegister(@Dev);
  Lib.PutEntry('LASERJET', Dev);
  CheckLibError;

  LJFontGRegister(@Dev);
  Lib.PutEntry('LASERJETFNTG', Dev);
  CheckLibError;

  Deregister(@Dev);
  LJRegister(@Dev);
  LJFontFRegister(@Dev);
  Lib.PutEntry('LASERJETFNTF', Dev);

  Deregister(@Dev);
  LJPlusRegister(@Dev);
  Lib.PutEntry('LASERJETPLUS', Dev);

  Deregister(@Dev);
  LJIIRegister(@Dev);
  Lib.PutEntry('LASERJETII', Dev);

  Deregister(@Dev);
  DiabloRegister(@Dev);
  Lib.PutEntry('DIABLO', Dev);
  CheckLibError;

  Deregister(@Dev);
  EpsonMXRegister(@Dev);
  Lib.PutEntry('EPSONMX', Dev);
  CheckLibError;

  Deregister(@Dev);
  EpsonFXRegister(@Dev);
  Lib.PutEntry('EPSONFX', Dev);
  CheckLibError;

  Deregister(@Dev);
  EpsonLQRegister(@Dev);
  Lib.PutEntry('EPSONLQ', Dev);
  CheckLibError;

  Deregister(@Dev);
  ProPrinterIIRegister(@Dev);
  Lib.PutEntry('PROPRINTERII', Dev);
  CheckLibError;

  Deregister(@Dev);
  HPDeskJetRegister(@Dev);
  Lib.PutEntry('DESKJET', Dev);
  CheckLibError;

  Deregister(@Dev);
  KXP1093Register(@Dev);
  Lib.PutEntry('KXP1093', Dev);
  CheckLibError;

  Deregister(@Dev);
  ToshibaP321Register(@Dev);
  Lib.PutEntry('TOSHIBAP321', Dev);
  CheckLibError;

  Deregister(@Dev);
  StarMicronicsRegister(@Dev);
  Lib.PutEntry('STARMICRONIC', Dev);
  CheckLibError;

  Deregister(@Dev);
  Nec3510Register(@Dev);
  Lib.PutEntry('NEC3510', Dev);
  CheckLibError;

  Deregister(@Dev);
  Nec8800Register(@Dev);
  Lib.PutEntry('NEC8800', Dev);
  CheckLibError;

  Deregister(@Dev);
  ProwriterRegister(@Dev);
  Lib.PutEntry('PROWRITER', Dev);
  CheckLibError;

  Deregister(@Dev);
  FacitRegister(@Dev);
  Lib.PutEntry('FACIT4512', Dev);
  CheckLibError;

  Deregister(@Dev);
  DataProductsRegister(@Dev);
  Lib.PutEntry('DATAPRODUCTS', Dev);
  CheckLibError;

  {close the library}
  Lib.Done;
end.
