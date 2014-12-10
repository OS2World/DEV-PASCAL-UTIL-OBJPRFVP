{$F+,V-,S-,R-}

{$I OPDEFINE.INC}
{*********************************************************}
{*                  FORMTEST.PAS 1.30                    *}
{*     An example program for Object Professional 1.0    *}
{*     Copyright (c) TurboPower Software 1988, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

program FormTest;
{This is a simple test program of the OpForm unit. Type
    FormTest -?<cr>
 to get a help message.

 If no parameters are given, then the output is sent to a disk file called
 DUMP.PRN.  To send the output to a Laserjet compatible printer attached to
 LPT1, you would type
   FormTest -L1 -TL
             ^   ^
             |   |
             |   +--indicates Laserjet
             +------indicates use LPT1
}

uses
  Dos,
  OpString,
  OpConst,    {!!.20}
  OpRoot,
  OpDos,
  {$IFDEF UseDates}
  OpDate,
  {$ENDIF}
  {$IFDEF UseBcd}
  OpBcd,
  {$ENDIF}
  OpCrt,
  OpAbsFld,
  OpPrnLow,
  OpPrint,
  OpDevice,
  OpForm;

var
  PrnPtr       : PrinterPtr;           {pointer to the Printer driver object}
  Frm          : PrintedFormPtr;              {pointer to the form object}

  {our record of data}
  DataRec      : record
                   AcctNum,
                   Name,
                   Phone        : String[40];
                   DaysOverdue  : Word;
                   Balance      : Real;
                   {$IFDEF UseDates}             {!!.10}
                   CurTime      : Time;
                   {$ENDIF}                      {!!.10}
                 end;

const
  Form1        = 1;                    {our Form's ID}
  StreamBufSize= 2048;                 {buffer size for streams}

  LJLineWidth  = 9.6;                  {decipoint line width for LJ}
  ReadStream   : Boolean = False;      {if true, reads form from stream}
  WriteStream  : Boolean = False;      {if true, writes form to stream}

  Index        : Word = 1;             {index into typed constant data arrays}

  {string constants used in the sample form}
  Title        = 'OpForm Sample';
  Signiture    = 'Signature: ';
  NameStr      = 'Bryan E. Slatner';     {!!.30}
  OpFormMsg    =
    'OpForm is included in Object Professional from TurboPower Software';

  CCStr1       = 'cc: Kim Kokkonen';
  CCStr2       = '    Julian Bucknall';  {!!.30}
  CCStr3       = '    Terry Hughes';
  CCStr4       = '    Lee Inman';        {!!.30}

  AcctNumPr    = 'Account #';
  NamePr       = 'Name';
  PhonePr      = 'Phone';
  BalancePr    = 'Balance';
  AgingPr      = 'Days Overdue';
  TimePr       = 'This report was printed at';

  {list of month names}
  MonthNames   : Array[1..12] of String[9] = ('January',
                                              'February',
                                              'March',
                                              'April',
                                              'May',
                                              'June',
                                              'July',
                                              'August',
                                              'September',
                                              'October',
                                              'November',
                                              'December');
  {account number data}
  AcctNumList   : Array[1..6] of String[10] = ('1000001',
                                               '1000002',
                                               '1000003',
                                               '1000004',
                                               '1000005',
                                               '1000006');
  {list of names}
  NameList   : Array[1..6] of String[30] = ('Phil Donald',
                                            'Jack Rivera',
                                            'Dan Hather',
                                            'Barbara Salters',
                                            'Conrad Chung',
                                            'Fred Jennings');
  {list of phone numbers}
  PhoneList  : Array[1..6] of String[14] = ('(415) 333-3333',
                                            '(213) 666-5432',
                                            '(818) 444-6969',
                                            '(213) 644-0987',
                                            '(415) 444-5678',
                                            '(213) 345-6776');
  {list of balances}
  BalanceList : Array[1..6] of Real = (1876.23,45899.76,12.13,
                                       144.57,3909.84,56.75);
  {list of days overdue}
  OverdueList : Array[1..6] of Word = (30, 60, 90, 90, 120, 30);

  BoldUnderline    = 9;           {printer attribute for Bold and Underlined}

  PrinterType      : PrnType       = DiskFile;
  KindOfPrn        : Word          = pPlain;

  PrnOptions       : Word          = 0;

  BalancePic       : String[11] = '$###,###.##';
  PrnFileName      : String[69]    = 'DUMP.PRN';
  StreamFileName   = 'FORMTEST.STM';
  CartridgeLetter  : Char = #0;   {assume no cartridge is installed}
  PrnTestNum    : Byte = 1;
  PrnTestMask      : Byte = $90;

procedure DoHelp;
  {-Display command line option help message and halt}
begin
  WriteLn(^M^J'FormTest [options]'^M^J^M^J);
  WriteLn('  /?         Display this help message');
  WriteLn('  /N         Specify print file name (may be PRN)');
  WriteLn('  /S         Suppress Form Feed');
  {$IFDEF UseStreams}                            {!!.10}
  WriteLn('  /W         Write stream');
  WriteLn('  /R         Read stream');
  {$ENDIF}                                       {!!.10}
  WriteLn('  /Ln        Specify LPT number (where n is 1,2 or 3)');
  WriteLn('  /Onxx      Printer Online Test (where n is the printer test number');
  WriteLn('                                  if n is 4, xx is hex success mask)');
  WriteLn('  /Tn        Specify type of printer (where n is)');
  WriteLn('                                       L for HP Laserjet');
  WriteLn('                                       M for Epson MX');
  WriteLn('                                       F for Epson FX');
  WriteLn('                                       I for IBM Proprinter');
  WriteLn('                                       D for Diablo');
  WriteLn('  /Fl        Specify Laserjet Font Cartridge Letter (where l is letter)');
  Halt;
end;

procedure ParseCommandLine;
  {-Get options (if any) off command line}
var
  I : Byte;
  W : Word;
  Opt : String;
begin
  for I := 1 to ParamCount do begin
    Opt := ParamStr(I);
    if (Opt[1] in ['-','/']) and (Length(Opt) >= 2) then
      case Upcase(Opt[2]) of
        'N' : begin
                PrnFileName := StUpcase(Copy(Opt,3,Length(Opt)));
                if PrnFileName = 'PRN' then
                  PrinterType := Prn;
              end;
        'L' : PrinterType := PrnType(Ord(Opt[3]) - Ord('1'));
        'T' : case UpCase(Opt[3]) of
                'L' : KindOfPrn   := pLaserjet;
                'M' : KindOfPrn   := pEpsonMX;
                'D' : KindOfPrn   := pDiablo;
                'F' : KindOfPrn   := pEpsonFX;
                'I' : KindOfPrn   := pProprinterII;
                else WriteLn(Opt,' is an invalid printer type');
              end;
        'O' : begin
                PrnTestNum := Ord(Opt[3]) - Ord('0');
                if PrnTestNum > 4 then
                  WriteLn(Opt,' is an invalid printer test')
                else if PrnTestNum = 4 then
                  if Str2Word('$'+Copy(Opt,4,2),W) then
                    PrnTestMask := W
                  else
                    WriteLn(Opt, ' contains an invalid printer mask');
              end;
        'F' : CartridgeLetter := UpCase(Opt[3]);
        'S' : PrnOptions := PrnOptions or pNoFormFeed;
        {$IFDEF UseStreams}                      {!!.10}
        'W' : WriteStream := True;
        'R' : ReadStream := True;
        {$ENDIF}                                 {!!.10}
        '?','H' : DoHelp;
        else WriteLn(Opt,' is an invalid option');
      end;
  end;
end;

procedure ShowPrinterInfo;
  {-Display information about printer device}
var
  LPI,CPI,Point,LPP : Dimension;
  Test, Mask : Byte;
  BP : BiosPrinterPtr;
begin
  with Frm^.fPrinterPtr^, GetBasePrinter^ do begin
    WriteLn(^M^J'Printer type: ',PrnNames[TypeOfPrn]);
    WriteLn('Device name: ',NameOfPrn);
    GetPrinterInfo(LPI,CPI,Point,LPP);
    WriteLn('Lines per inch  ',LPI:6:2);
    WriteLn('Chars per inch  ',CPI:6:2);
    WriteLn('Character Point ',Point:6:2);
    WriteLn('Lines per page  ',LPP:4:0,^M^J);
    case TypeOfPrn of
      LPT1..LPT3 : begin
                     BP := BiosPrinterPtr(GetBasePrinter);
                     BP^.GetTestAndMask(Test, Mask);
                     WriteLn('Using BIOS services');
                     WriteLn('Using printer test ',Test);
                     if Test = 4 then
                       WriteLn('Using success mask $',HexB(Mask));
                   end;
      Prn, DiskFile : WriteLn('Using DOS services');
    end;
  end;
end;

function YesNo(S : String) : Boolean;
  {-Display Yes/No prompt, return True if Yes selected}
var
  C : Char;
begin
  Write(S);
  repeat
    C := UpCase(ReadKey);
  until C in ['Y','N',^[];
  WriteLn(C);
  YesNo := C = 'Y';
end;

procedure MemReport;
  {-Display memory usage (for debugging)}
begin
  WriteLn('MemAvail = ',MemAvail);
  WriteLn('MaxAvail = ',MaxAvail);
end;

procedure ReportErrorCheck(S : String);
  {-Check for errors, on error report error number and abort}
var
  E : Word;
begin
  E := Frm^.fGetLastError;
  if E <> 0 then begin
    WriteLn(S);
    WriteLn('ErrorCode = ',E);
    Halt;
  end;
end;

function PrinterErrorHandler(P : PrinterPtr;
                             ErrorCode : Word;
                             Recoverable : Boolean) : Boolean;
  {-Printer error handler, prompts for retry}
begin
  WriteLn('Printer error = ',ErrorCode);
  if Recoverable then
    PrinterErrorHandler := YesNo('Printer error, retry? ')
  else
    PrinterErrorHandler := False;
end;

function UserBlockFunction0(P : PrintBlockPtr) : Boolean;
  {Block function for page}
begin
  UserBlockFunction0 := True;
end;

function UserBlockFunction(P : PrintBlockPtr) : Boolean;
  {block function for data blocks}
begin
  {we only have 6 blocks worth of data, so fail if index is greater than 6}
  if Index > 6 then begin
    UserBlockFunction := False;
    Exit;
  end;
  {for this example, data is just retrieved from our typed constant arrays}
  with DataRec do begin
    AcctNum      := AcctNumList[Index];
    Name         := NameList[Index];
    Phone        := PhoneList[Index];
    Balance      := BalanceList[Index];
    DaysOverdue  := OverdueList[Index];
  end;
  UserBlockFunction := True;
  Inc(Index);
end;

procedure InitPrinterAndForm;
  {-Initialize the Printer object}
var
  BP : BiosPrinterPtr;
begin
  case PrinterType of
    LPT1..LPT3 : PrnPtr := New(PrinterPtr,Init(LptNames[LptType(PrinterType)],
                               LptType(PrinterType),PrnOptions));
    Prn        : PrnPtr := New(PrinterPtr,Init('PRN',Prn,PrnOptions));
    DiskFile   : PrnPtr := New(PrinterPtr,Init(PrnFileName,DiskFile,PrnOptions));
  end;
  if PrnPtr = NIL then begin
    WriteLn('Error initializing printer');
    Halt;
  end;
  with PrnPtr^ do
    SetPrintErrorFunc(PrinterErrorHandler);
  Frm := New(PrintedFormPtr,Init(Form1,PrnPtr));
  if Frm = Nil then begin
    WriteLn('Can not allocate PrintedForm object');
    Halt;
  end;
  case PrinterType of
    LPT1..LPT3 : begin
                   BP := BiosPrinterPtr(PrnPtr^.GetBasePrinter);
                   BP^.SetTestAndMask(PrnTestNum, PrnTestMask);
                 end;
  end;
  with Frm^ do
    case KindOfPrn of
      pLaserjet : begin
                    WriteLn('Registering Laserjet');
                    fSetPrnRegisteredType(LJRegister);
                    case CartridgeLetter of
                      'G' : begin
                              WriteLn('Registering Font G');
                              fSetPrnRegisteredType(LJFontGRegister);
                            end;
                      'F' : begin
                              WriteLn('Registering Font F');
                              fSetPrnRegisteredType(LJFontFRegister);
                            end;
                    end;
                    with fPrinterPtr^ do
                      AddPrintMode('BoldUnderline',#27'(s3B'#27'&dD',
                                    #27'(s0S'#27'&d@',0,0,BoldUnderline,0);
                  end;
      pDiablo   : begin
                    WriteLn('Registering Diablo');
                    fSetPrnRegisteredType(DiabloRegister);
                  end;
      pEpsonMX  : begin
                    WriteLn('Registering Epson MX');
                    fSetPrnRegisteredType(EpsonMXRegister);
                  end;
    end;
end;

function CurrentDateStr : String;
  {-Return the current system date as string in 'Monthname DD, YYYY' format}
var
  Y,M,D,DW,H,S,S100 : Word;
begin
  GetDate(Y,M,D,DW);
  CurrentDateStr := MonthNames[M] + ' ' + Long2Str(D) + ', ' + Long2Str(Y);
end;

procedure InitFields;
  {-Init the print fields}
var
  L, I : Byte;
begin
  with Frm^ do begin
    NewPage;

    NewBlock(0,2,UserBlockFunction0);
    ReportErrorCheck('error allocating new block');

    {$IFDEF UseStreams}                          {!!.10} {!!.11}
    SetUserRecord(DataRec,SizeOf(DataRec));              {!!.11}
    {$ENDIF}                                     {!!.10} {!!.11}

    SetHeader(LeftPad(CurrentDateStr,79),1,1,Italics);
    ReportErrorCheck('error setting header');
    SetFooter(Center(OpFormMsg,79),59,1,Italics);
    ReportErrorCheck('error setting footer');

    AddTextField(1,Title,6,32,Bold,Length(Title));
    ReportErrorCheck('error adding title');

    {$IFDEF UseDates}                            {!!.10}
    AddTimeField(2,TimePr,44,3,Italics,Length(TimePr),3,
                 'HH:mm te',44,Length(TimePr)+4,Italics,8,DataRec.CurTime);
    ReportErrorCheck('error adding time');       {!!.10}
    {$ENDIF}                                     {!!.10}

    AddTextField(4,NameStr,49,15,Italics,Length(NameStr));
    ReportErrorCheck('error adding name');
    L := Length(Signiture)+Length(NameStr)+1;
    AddTextField(5,PadCh(Signiture,'_',L),47,3,Bold,L);

    ReportErrorCheck('error adding signiture');

    AddTextField(6,CCStr1,53,3,Compressed,Length(CCStr1));
    ReportErrorCheck('error adding CCStr1');
    AddTextField(7,CCStr2,54,3,Compressed,Length(CCStr2));
    ReportErrorCheck('error adding CCStr2');
    AddTextField(8,CCStr3,55,3,Compressed,Length(CCStr3));
    ReportErrorCheck('error adding CCStr3');
    AddTextField(8,CCStr4,56,3,Compressed,Length(CCStr4));

    if KindOfPrn = pLaserjet then begin
      AddBox(9,LJRow2Deci(10,PrnPtr),LJCol2Deci(4,PrnPtr),      {!!.22}
             LJCol2Deci(70,PrnPtr),LJRow2Deci(32,PrnPtr),       {!!.22}
             LJLineWidth * 2,LJLineWidth * 2,SingleLineBox,0);  {!!.22}
      ReportErrorCheck('error adding Box');
      AddShaded(10,LJRow2Deci(10,PrnPtr),LJCol2Deci(4,PrnPtr),  {!!.22}
              LJCol2Deci(70,PrnPtr),LJRow2Deci(32,PrnPtr),4);   {!!.22}
      ReportErrorCheck('error adding Shaded');
    end
    else begin
      AddBox(9,4,14,50,5,1,1,SingleLineBox,0);
      AddBox(10,10,4,70,32,1,1,DoubleLineBox,0);
    end;
    NewBlock(10,8,UserBlockFunction);
    ReportErrorCheck('error adding first data block');

    {$IFDEF UseStreams}                          {!!.10}
    SetUserRecord(DataRec,SizeOf(DataRec));
    {$ENDIF}                                     {!!.10}

    with DataRec do begin
      AddStringField(11,AcctNumPr,1,1,Italics,10,12,NullPicture,1,18,Dim,20,AcctNum);
      ReportErrorCheck('error adding account number field');
      AddStringField(13,NamePr,2,1,Italics,10,14,NullPicture,2,18,Dim,20,Name);
      ReportErrorCheck('error adding name field');
      AddStringField(15,PhonePr,3,1,Italics,10,16,NullPicture,3,18,Dim,20,Phone);
      ReportErrorCheck('error adding phone field');
      AddRealField(17,BalancePr,4,40,Italics,10,18,BalancePic,
                   4,51,Bold,2,11,Balance);
      ReportErrorCheck('error adding balance field');
      AddWordField(19,AgingPr,4,1,Italics,12,20,NullPicture,4,18,
                   Bold,8,DaysOverdue);
      ReportErrorCheck('error adding overdue field');
    end;

    {duplicate the current block 5 more times down the page}
    for I := 3 to 7 do begin
      DupCurrentBlock(I * 5,8);
      ReportErrorCheck('error adding block '+Long2Str(I));
    end;
  end;
end;

{$IFDEF UseStreams}                              {!!.10}
procedure InitForStreams(var S : IdStream);

begin
  with S do begin
    RegisterHier(FormStream);
    RegisterHier(AllPrintFieldsStream);
    RegisterHier(LJDeviceStream);
    RegisterPointer(1005,@DataRec);
    RegisterPointer(1006,@UserBlockFunction0);
    RegisterPointer(1007,@UserBlockFunction);
    RegisterPointer(1008,@PrinterErrorHandler);
  end;
end;
{$ENDIF}                                         {!!.10}

var
  F                : File;
  ErrorCode        : Word;
  {$IFDEF UseStreams}                            {!!.10}
  OurStream        : BufIdStream;
  {$ENDIF}                                       {!!.10}
begin
  ParseCommandLine;
  MemReport;
  {$IFDEF UseStreams}                            {!!.10}
  if ReadStream then begin
    WriteLn('Reading from stream...');
    OurStream.Init(StreamFileName,SOpen, StreamBufSize);
    InitForStreams(OurStream);
    Frm := PrintedFormPtr(OurStream.GetPtr);
    ErrorCode := OurStream.GetStatus;
    if (Frm = Nil) or (ErrorCode <> 0) then begin
      WriteLn('load error ',ErrorCode);
      Halt;
    end;
    OurStream.Done;
  end
  else begin
    InitPrinterAndForm;
    InitFields;
  end;
  {$ELSE}                                        {!!.10}
  InitPrinterAndForm;                            {!!.10}
  InitFields;                                    {!!.10}
  {$ENDIF}                                       {!!.10}
  MemReport;
  ShowPrinterInfo;
  WriteLn;

  {$IFDEF UseStreams}                            {!!.10}
  if WriteStream then begin
    WriteLn('Writing stream...');
    OurStream.Init(StreamFileName, SCreate, StreamBufSize);
    InitForStreams(OurStream);
    OurStream.PutPtr(Frm);
    ErrorCode := OurStream.GetStatus;
    if (Frm = Nil) or (ErrorCode <> 0) then begin
      WriteLn('store error ',ErrorCode);
      Halt;
    end;
    OurStream.Done;
  end;
  {$ENDIF}                                       {!!.10}

  {$IFDEF UseDates}                              {!!.10}
  DataRec.CurTime := CurrentTime;
  {$ENDIF}                                       {!!.10}

  with Frm^, fPrinterPtr^, GetBasePrinter^ do begin
    if (TypeOfPrn = DiskFile) or (PrinterReady) then begin
      Process;
      ErrorCode := fGetLastError;
      if ErrorCode <> 0 then
        WriteLn('Error processing form.  ErrorCode = ',ErrorCode);
    end
    else
      WriteLn('Printer is not ready');
  end;

  {$IFDEF UseStreams}                            {!!.10}
  if ReadStream then
    PrnPtr := Frm^.fPrinterPtr;
  {$ENDIF}                                       {!!.10}

  {deregistering the pointer is done to assure that any memory that may have
   been allocated when it was registered (laserjet is the only one currently
   that allocates memory) is released}
  Deregister(PrnPtr);

  {dispose of the form, if the printer was loaded dynamically by Load, then
   it will automatically disposed.}
  {$IFDEF UseStreams}                            {!!.10}
  if ReadStream then
    Dispose(Frm, Done)
  else begin
    Dispose(Frm, Done);
    Dispose(PrnPtr, Done);
  end;
  {$ELSE}                                        {!!.10}
  Dispose(Frm, Done);                            {!!.10}
  Dispose(PrnPtr, Done);                         {!!.10}
  {$ENDIF}                                       {!!.10}
  WriteLn;
  MemReport;
end.

