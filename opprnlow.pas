{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                 OPPRNLOW.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpPrnLow;
  {-Impements routines for outputing data to printer (and printer like)
    devices.}

{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$IFDEF VIRTUALPASCAL}
  !! ERROR: This unit is not *YET* compatible with VP/2 applications !!
{$ENDIF}

interface

uses
  Dos,
  OpConst,        {!!.20}
  OpInline,
  OpRoot,
  OpDos;

const
  PrnHandle        = 4;           {the dos standard PRN device handle}
  PrnTfddID        = $12345678;

  NoPrinterTest    = 0;
  DefPrinterTest   = 2;           {!!.13}

  DefSuccessMask   = $90;

  bpTimedOut       = $01;
  bpIOError        = $08;
  bpSelected       = $10;
  bpOutOfPaper     = $20;
  bpAcknowledge    = $40;
  bpNotBusy        = $80;

  PrinterNotReady : Word = 152;
type
  PrnType = (LPT1,LPT2,LPT3,Prn,DiskFile,UserDefined); {types of printers}
  LptType = LPT1..LPT3;                                {subset for int 17}
  PrnCallType = (OpenCall,PutCall,CloseCall,FlushCall,StatusCall,ResetCall);
  {Passed to PrnXlatErrorCode to indicate operation which generated raw error
  code passed to it}

  XlatCharTable    = Array[Char] of Char;

  BasePrinterPtr   = ^BasePrinter;       {Pointer to the base printer object}
  BasePrinter =                          {the base printer object}
    object(Root)
      pTypeOfPrn     : PrnType;        {the type of this printer}
      pDataWord      : Word;           {a word for data (LPT number or handle)}
      pLastError     : Word;           {the last error code (transalted)}
      pLastRawError  : Word;           {the last error code (raw)}
      pLastPut       : Word;           {Num chars written by last TFDD call}
      pLastOperation : PrnCallType;    {the last operation performed}
      pXlatChar      : XlatCharTable;  {translation table for chars}
      pNameOfPrn     : PathStr;        {the name of this printer}
      constructor Init(PrnName : PathStr;
                       TypeOfPrinter : PrnType);
        {-initializes a printer object}

      {the Virtual methods}
      procedure PrnPutChar(Character : Char); Virtual;
        {-Puts a character to the output device}
      procedure PrnPutBlock(var Block; BlockSize : Word); Virtual;
        {-Puts a block of characters to the output device}
      function PrnOpen : Boolean; Virtual;
        {-Opens the output device}
      procedure PrnClose; Virtual;
        {-Closes an output device}
      function PrnStatus : Word; Virtual;
        {-Return the status of the device}
      procedure PrnReset; Virtual;
        {-Reset the output device}
      procedure PrnFlush; Virtual;
        {-Flush the device}
      function PrnXlatErrorCode(Call : PrnCallType;
                             ErrorCode : Word) : Word; Virtual;
        {-translate a raw error code into appropriate user error code}
      function CharXlat(C : Char) : Char; Virtual;
        {-Translate the Character C according to built in translation table}
      destructor Done; Virtual;
        {-flush, close, and deallocate memory used by a printer object}

      {non-virtual methods}
      function TypeOfPrn : PrnType;
        {-Returns the type of printer}
      function NameOfPrn : String;
        {-Returns the printer name}
      function PrnError  : Word;
        {-Returns the last error and resets pLastError to zero}
      function PrnRawError : Word;
        {-Returns the raw error code returned by the last operation}
      function PrnErrorOp : PrnCallType;
        {-Returns the type of the last operation}
      function PrnLastPut : Word;
        {-Returns the number of characters successfully written by last
          PrnPutBlock}
      {methods related to character translation table}
      procedure XlatCharItem(CharIndex : Char; Character : Char);
        {-Change the character associated with the ascii code of CharIndex to
          Character}
      procedure ClearXlatCharTable;
        {-Clears the character translation table (the default state)}
      procedure NewXlatCharTable(var Table : XlatCharTable);
        {-sets entire XlatChar table}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a BasePrinter from a stream}
      procedure Store(var S : IdStream);
        {-Store a BasePrinter in a stream}
    {$ENDIF}
      {.Z+}
      {...private methods...}
      procedure pError(RawErrorCode : Word; Operation : PrnCallType);
      {.Z-}
    end;

  {derived type for DOS based printer object}
  DosPrinterPtr = ^DosPrinter;
  DosPrinter =
    object(BasePrinter)
      {the Virtual methods}
      constructor Init(PrnName : PathStr;
                       TypeOfPrinter : PrnType);
        {-initializes a printer object}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a DosPrinter from a stream}
      procedure Store(var S : IdStream);
        {-Store a DosPrinter in a stream}
    {$ENDIF}
      {.Z+}
      procedure PrnPutChar(Character : Char); Virtual;
        {-Puts a character to the output device}
      function PrnOpen : Boolean; Virtual;
        {-Opens the output device}
      procedure PrnClose; Virtual;
        {-Closes an output device}
      function PrnStatus : Word; Virtual;
        {-Return the status of the device}
      procedure PrnFlush; Virtual;
        {-Flush the device}
      {.Z-}
    end;

  {derived type for DOS based printer object that appends to file}{!!.20}
  AppendDosPrinterPtr = ^AppendDosPrinter;
  AppendDosPrinter =
    object(DosPrinter)
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load an AppendDosPrinter from a stream}
      procedure Store(var S : IdStream);
        {-Store a AppendDosPrinter in a stream}
    {$ENDIF}
      {.Z+}
      function PrnOpen : Boolean; Virtual;
        {-Opens the output device}
    end;

  CharFunc         = function (C : Char; DataWord : Word) : Word;
  OpenFunc         = function (Name : PathStr;
                               PrnTyp : PrnType;
                               var DataWord : Word) : Word;
  DeviceFunc       = function (DataWord : Word) : Word;
  XlatFunc         = function (Call : PrnCallType;
                               ErrorCode : Word) : Word;

  {The most flexible low level printer object}
  FlexiblePrinterPtr = ^FlexiblePrinter; {!!.30}
  FlexiblePrinter =
    object(BasePrinter)
      fpPutCharPrim          : CharFunc;
      fpOpenPrim             : OpenFunc;
      fpClosePrim            : DeviceFunc;
      fpStatusPrim           : DeviceFunc;
      fpResetPrim            : DeviceFunc;
      fpFlushPrim            : DeviceFunc;
      fpXlatPrim             : XlatFunc;
      {the Virtual methods}
      constructor Init(PrnName       : PathStr;
                       TypeOfPrinter : PrnType;
                       PutCharPrim   : CharFunc;
                       OpenPrim      : OpenFunc;
                       ClosePrim     : DeviceFunc;
                       StatusPrim    : DeviceFunc;
                       ResetPrim     : DeviceFunc;
                       FlushPrim     : DeviceFunc;
                       XlatPrim      : XlatFunc);
        {-initializes a printer object}
      {.Z+}
      procedure PrnPutChar(Character : Char); Virtual;
        {-Puts a character to the output device}
      function PrnOpen : Boolean; Virtual;
        {-Opens the output device}
      procedure PrnClose; Virtual;
        {-Closes an output device}
      function PrnStatus : Word; Virtual;
        {-Return the status of the device}
      procedure PrnReset; Virtual;
        {-Reset the output device}
      procedure PrnFlush; Virtual;
        {-Flush the device}
      function PrnXlatErrorCode(Call : PrnCallType;
                                ErrorCode : Word) : Word; Virtual;
        {-translate a raw error code into appropriate user error code}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a FlexiblePrinter from a stream}
      procedure Store(var S : IdStream);
        {-Store a FlexiblePrinter in a stream}
    {$ENDIF}
      {.Z-}
    end;

  BiosPrinterPtr = ^BiosPrinter;
  BiosPrinter =
    object(FlexiblePrinter)
      bpPrinterTest       : Byte;
      bpSuccessMask       : Byte;

      constructor Init(LPTNumber : LPTType);
      constructor InitCustom(LPTNumber : LPTType;
                             PrinterTestNumber, SuccessMask : Byte);
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a BiosPrinter from a stream}
      procedure Store(var S : IdStream);
        {-Store a BiosPrinter in a stream}
    {$ENDIF}
      procedure PrnPutChar(Character : Char); Virtual;
        {-Puts a character to the output device}
      function GetLptPort : LPTType;
        {-Returns the LPT port number for this printer}
      procedure SetLptPort(LPTNo : LPTType);
        {-Sets the LPT port number for this printer}
      procedure SetXlatErrorFunc(Xlat : XlatFunc);
        {-Sets the XlatError routine}
      function GetXlatErrorFunc : Pointer;
        {-Returns the Xlat Error routine}
      procedure GetTestAndMask(var PrinterTestNumber, Mask : Byte);
        {return the printer test number and success mask}
      procedure SetTestAndMask(PrinterTestNumber, Mask : Byte);
        {set the printer test number and success mask}
      {.Z+}
      function PrnXlatErrorCode(Call : PrnCallType;
                             ErrorCode : Word) : Word; Virtual;
        {-translate a raw error code into appropriate user error code}
      {.Z-}
    end;

  TextBufPtr = ^TextBuf;
  {The TextRec type for Printer object based }
  PrnTextRec =
    record
      Handle     : Word;
      Mode       : Word;
      BufSize    : Word;
      Private    : Word;
      BufPos     : Word;
      BufEnd     : Word;
      BufPtr     : TextBufPtr;
      OpenFunc   : Pointer;
      InOutFunc  : Pointer;
      FlushFunc  : Pointer;
      CloseFunc  : Pointer;
      { 16 byte user data area, we use 8 bytes }
      PrnObject  : BasePrinterPtr;
      PrnID      : LongInt;
      UserData   : array[1..8] of Char;
      Name       : array[0..79] of Char;
      Buffer     : TextBuf;
     end;

const
  NilPointer : Pointer = Nil;
  {names of the LPT devices}
  LptNames : Array[LptType] of String[4]  = ('LPT1','LPT2','LPT3');
  PrnNames : Array[PrnType] of String[12] = ('LPT1','LPT2','LPT3',
                                             'PRN','Disk File','User Defined');
var
  NoXlatFunc : XlatFunc absolute NilPointer;

{.Z+}
{$IFDEF UseStreams}
procedure BasePrinterStream(SPtr : IdStreamPtr);
  {-Register types needed for streams containing BasePrinters}

procedure DosPrinterStream(SPtr : IdStreamPtr);
  {-Register types needed for streams containing DosPrinters}

procedure AppendDosPrinterStream(SPtr : IdStreamPtr);               {!!.20}
  {-Register types needed for streams containing AppendDosPrinters}

procedure BiosPrinterStream(SPtr : IdStreamPtr);
  {-Register types needed for streams containing BiosPrinters}
{$ENDIF}
{.Z-}

procedure DefaultXlatCharTable(var Table : XlatCharTable);
  {-Initialize Table to default state}

procedure AssignPrnObject(var F : Text;
                         var PrinterObj : BasePrinter);
  {-like Turbo's assign, except associates Text variable with a printer object}

function GetPrnObject(var F : Text) : BasePrinterPtr;
  {-Return a pointer to the Prn Object associated with a Text file.
    Returns Nil if not a text file associated with a Prn object}

function CallPrnStatus(var F : Text) : Word;
  {-Call PrnStatus method for Text file associated with Prn Object,
    If not Prn Object TFDD, then $FFFF is returned.}

{.Z+}
function Int17Out(Ch : Char; LPTNo : Word) : Byte;
{ send a character to LPTNo via ROM BIOS int 17h func 0h }
Inline(
  $5A/                   {  pop  dx        ; get printer number}
  $58/                   {  pop  ax        ; get character}
  $30/$E4/               {  xor  ah,ah     ; set AH for func 0}
  $CD/$17/               {  int  $17       ; printer bios function call}
  $86/$E0);              {  xchg al,ah     ; put byte result in AL}

function PrnTest1Prim(ErrorCode : Word) : Boolean;
  {-Printer ready test 1}
Inline(
  $58/                   {  pop     ax}
  $34/$80/               {  xor     al,$80}
  $A8/$A9/               {  test    al,$A9}
  $75/$04/               {  jnz     PrnError}
  $B0/$01/               {  mov     al,1}
  $EB/$02/               {  jmp     short ExitPoint}
                         {PrnError:}
  $30/$C0);              {  xor     al,al}
                         {ExitPoint:}

function PrnTest2Prim(ErrorCode : Word) : Boolean;
  {-Printer ready test 2}
Inline(
  $58/                   {  pop     ax}
  $A8/$29/               {  test    al,$29}
  $75/$04/               {  jnz     PrnError}
  $B0/$01/               {  mov     al,1}
  $EB/$02/               {  jmp     short ExitPoint}
                         {PrnError:}
  $30/$C0);              {  xor     al,al}
                         {ExitPoint:}

function PrnTest3Prim(ErrorCode : Word) : Boolean;
  {-Printer ready test 3}
Inline(
  $58/                   {  pop   ax}
  $24/$A0/               {  and   al,$A0}
  $79/$06/               {  jns   PrnError}
  $7A/$04/               {  jpe   PrnError}
  $B0/$01/               {  mov   al,1}
  $EB/$02/               {  jmp   short ExitPoint}
                         {PrnError:}
  $30/$C0);              {  xor   al,al}
                         {ExitPoint:}

function PutPrim(C : Char; LPTNo : Word) : Word;
  {-Puts a single character to specified LPT number}

function Int17Reset(LPTNo : Word) : Word;
  {-Use Bios Int 17 service to reset the printer}

function LptDeviceReady(LPTNo : Word) : Word;
  {-Call Bios Int 17 service to get current status of printer}

function DosFileOut(Character : Char; Handle : Word) : Word;
  {-assembly routine to output one character using DOS}
{.Z-}

  {==========================================================================}

implementation

  function DosClose(Handle : Word) : Word;
    {-Close a DOS file}
  var
    Regs : Registers;
  begin
    with Regs do begin
      AH := $3E;
      BX := Handle;
      MsDos(Regs);
      if Odd(Flags) then
        DosClose := AX
      else
        DosClose := 0;
    end;
  end;

  function DosCreate(Name : PathStr; var Handle : Word) : Word;

  var
    Regs : Registers;
  begin
    Name := Name + #0;
    with Regs do begin
      AX := $3C00;
      CX := 0;
      DS := Seg(Name);
      DX := Ofs(Name[1]);
      MsDos(Regs);
      if Odd(Flags) then begin
        DosCreate := AX;
        Handle := 0;
      end
      else begin
        Handle := AX;
        DosCreate := 0;
      end;
    end;

  end;

  function DosSeekEOF(Handle : Word) : Word; {!!.20}
  var
    Regs : Registers;
  begin
    with Regs do begin
      AX := $4202;
      BX := Handle;
      CX := 0;
      DX := 0;
      MsDos(Regs);
      if Odd(Flags) then
        DosSeekEOF := AX
      else
        DosSeekEOF := 0;
    end;
  end;

  function DosAppend(Name : PathStr; var Handle : Word) : Word; {!!.20}

  var
    Regs : Registers;
  begin
    Name := Name + #0;
    with Regs do begin
      AX := $3D02;
      DS := Seg(Name);
      DX := Ofs(Name[1]);
      MsDos(Regs);
      if Odd(Flags) then begin
        if AX = 2 then
          DosAppend := DosCreate(Name, Handle)
        else
          DosAppend := AX;
      end
      else begin
        Handle := AX;
        DosAppend := DosSeekEOF(Handle);
      end;
    end;

  end;

  function DosFileOutPrim(C : Char; Handle : Word) : Word;
  Inline(
    $5B/                   {  pop  bx        ; get handle}
    $8C/$DF/               {  mov  di,ds     ; save ds in di}
    $8C/$D0/               {  mov  ax,ss     ; move ss to ds}
    $8E/$D8/               {  mov  ds,ax     ;}
    $89/$E2/               {  mov  dx,sp     ; ofs(C) := SP}
    $B9/$01/$00/           {  mov  cx,1      ; one char to write}
    $B4/$40/               {  mov  ah,$40    ; write file or device}
    $CD/$21/               {  int  $21       ; dos call}
    $72/$02/               {  jc   DFOExit   ; error if carry flag set}
    $31/$C0/               {  xor  ax,ax     ; no error, return 0}
                           {DFOExit:}
    $5A/                   {  pop  dx        ; remove C from stack}
    $8E/$DF);              {  mov  ds,di     ; restore DS}

  function DosFileOut(Character : Char; Handle : Word) : Word;

  begin
    DosFileOut := DosFileOutPrim(Character,Handle);
  end;

  function DosDeviceReady(Handle : Word) : Boolean;
  var
    Regs : Registers;

  begin
    with Regs do begin
      DosDeviceReady := False;
      {first determine that this is a character device using IOCtrl 0}
      AX := $4400;
      BX := Handle;
      MsDos(Regs);
      if Odd(Flags) then
        Exit;
      {if it is a block device, indicate it is ready and exit}
      if not FlagIsSet(DX,$40) then begin
        DosDeviceReady := True;
        Exit;
      end;

      {now call IOCtrl function 7 to get output status}
      AX := $4407;
      BX := Handle;
      MsDos(Regs);
      if Odd(Flags) then
        Exit;
      DosDeviceReady := AL = $FF;
    end;
  end;

  function Int17ResetPrim(LPTNo : Word) : Word;
  Inline(
    $5A/                   {  pop   dx}
    $B4/$01/               {  mov   ah,1}
    $CD/$17/               {  int   $17}     {!!.01}
    $86/$C4/               {  xchg  ah,al}
    $30/$E4);              {  xor   ah,ah}   {!!.01}

  function Int17Reset(LPTNo : Word) : Word;

  begin
    Int17Reset := Int17ResetPrim(LPTNo);
  end;

  function LptReadyPrim(LPTNo : Word) : Word;
  Inline(
    $5A/                   {  pop   dx}
    $B4/$02/               {  mov   ah,2}
    $CD/$17/               {  int   $17}     {!!.01}
    $86/$C4/               {  xchg  ah,al}
    $30/$E4);              {  xor   ah,ah}   {!!.01}


  function LptDeviceReady(LPTNo : Word) : Word;

  begin
    LptDeviceReady := LptReadyPrim(LPTNo);
  end;

  constructor BasePrinter.Init(PrnName : PathStr;
                               TypeOfPrinter : PrnType);

  begin
    if not Root.Init then
      Fail;
    pLastError := 0;                {!!.01}
    pLastRawError := 0;             {!!.01}
    pTypeOfPrn := TypeOfPrinter;
    pNameOfPrn := PrnName;
    ClearXlatCharTable;
    if not PrnOpen then begin
      InitStatus := pLastError;
      Fail;
    end;
  end;

  destructor BasePrinter.Done;

  begin
    PrnClose;
  end;

{$IFDEF UseStreams}

  constructor BasePrinter.Load(var S : IdStream);

  begin
    Root.Init;
    S.ReadRange(pTypeOfPrn,pNameOfPrn);
    pNameOfPrn := S.ReadString;
    if (S.PeekStatus <> 0) then begin
      Fail;
      Done;
    end;
    if not PrnOpen then begin
      InitStatus := pLastError;
      Fail;
    end;
  end;

  procedure BasePrinter.Store(var S : IdStream);

  begin
    S.WriteRange(pTypeOfPrn,pNameOfPrn);
    S.WriteString(pNameOfPrn);
  end;

  procedure BasePrinterStream(SPtr : IdStreamPtr);
    {-Register types needed for streams containing BasePrinters}

  begin
    with SPtr^ do
      RegisterType(otBasePrinter,veBasePrinter,TypeOf(BasePrinter),
                   @BasePrinter.Store,@BasePrinter.Load);
  end;

{$ENDIF}

  procedure BasePrinter.PrnPutChar(Character : Char);

  begin
    Abstract;
  end;

  procedure BasePrinter.PrnPutBlock(var Block; BlockSize : Word);

  type
    CharArray = Array[1..$FFF0] of Char;

  var
    B    : CharArray absolute Block;
    I    : Word;
  begin
    pLastPut := 0;
    for I := 1 to BlockSize do begin
      PrnPutChar(B[I]);
      if pLastError = 0 then
        Inc(pLastPut)
      else
        Exit;
    end;
  end;

  function BasePrinter.PrnOpen : Boolean;

  begin
    PrnOpen := True;
  end;

  procedure BasePrinter.PrnClose;

  begin
  end;

  procedure BasePrinter.PrnFlush;

  begin
  end;

  procedure BasePrinter.PrnReset;

  begin
  end;

  function BasePrinter.PrnStatus : Word;

  begin
    PrnStatus := 0;
  end;

  procedure BasePrinter.pError(RawErrorCode : Word;
                                  Operation : PrnCallType);

  begin
    pLastRawError := RawErrorCode;
    pLastOperation := Operation;
    pLastError := PrnXlatErrorCode(Operation,RawErrorCode);
  end;

  function BasePrinter.TypeOfPrn : PrnType;

  begin
    TypeOfPrn := pTypeOfPrn;
  end;

  function BasePrinter.NameOfPrn : String;

  begin
    NameOfPrn := pNameOfPrn;
  end;

  function BasePrinter.PrnError : Word;

  begin
    PrnError   := pLastError;
    pLastError := 0;
  end;

  function BasePrinter.PrnRawError : Word;

  begin
    PrnRawError := pLastRawError;
  end;

  function BasePrinter.PrnErrorOp : PrnCallType;

  begin
    PrnErrorOp := pLastOperation;
  end;

  function BasePrinter.PrnLastPut : Word;

  begin
    PrnLastPut := pLastPut;
  end;

  function BasePrinter.PrnXlatErrorCode(Call : PrnCallType;
                                           ErrorCode : Word) : Word;

  begin
    PrnXlatErrorCode := ErrorCode;       {return original errorcode}
  end;

  procedure BasePrinter.ClearXlatCharTable;
  begin
    DefaultXlatCharTable(pXlatChar);
  end;

  procedure BasePrinter.NewXlatCharTable(var Table : XlatCharTable);

  begin
    pXlatChar := Table;
  end;

  procedure BasePrinter.XlatCharItem(CharIndex : Char; Character : Char);

  begin
    pXlatChar[CharIndex] := Character;
  end;

  function BasePrinter.CharXlat(C : Char) : Char;

  begin
    CharXlat := pXlatChar[C];
  end;

  constructor DosPrinter.Init(PrnName : PathStr;
                              TypeOfPrinter : PrnType);
  begin
    if not BasePrinter.Init(PrnName,TypeOfPrinter) then
      Fail;
  end;

{$IFDEF UseStreams}

  constructor DosPrinter.Load(var S : IdStream);

  begin
    BasePrinter.Load(S);
  end;

  procedure DosPrinter.Store(var S : IdStream);

  begin
    BasePrinter.Store(S);
  end;

  procedure DosPrinterStream(SPtr : IdStreamPtr);
    {-Register types needed for streams containing DosPrinters}
  begin
    BasePrinterStream(SPtr);
    SPtr^.RegisterType(otDosPrinter,veDosPrinter,
                       TypeOf(DosPrinter),
                       @DosPrinter.Store,@DosPrinter.Load);
  end;

{$ENDIF}

  procedure DosPrinter.PrnPutChar(Character : Char);

  begin
    pError(DosFileOut(CharXlat(Character),pDataWord),PutCall);
  end;

  function DosPrinter.PrnOpen : Boolean;

  begin
    if pTypeOfPrn = PRN then
      pDataWord := PrnHandle
    else
      pError(DosCreate(pNameOfPrn,pDataWord),OpenCall);
    PrnOpen := (pLastError = 0);
  end;

  procedure DosPrinter.PrnClose;

  begin
    if pTypeOfPrn = PRN then
      pError(0, CloseCall)
    else begin                                  {!!.11}
      pError(DosClose(pDataWord),CloseCall);
      pDataWord := $FFFF;                       {!!.11}
    end;                                        {!!.11}
  end;

  procedure DosPrinter.PrnFlush;

  begin
    if FlushDosBuffers(pDataWord) then
      pError(0,FlushCall)
    else
      pError(ecTooManyFiles, FlushCall);
  end;

  function DosPrinter.PrnStatus : Word;

  begin
    pError(Ord(not DosDeviceReady(pDataWord)),StatusCall);
    PrnStatus := pLastError;
  end;

{!!.20 begin}

  function AppendDosPrinter.PrnOpen : Boolean;

  begin
    if pTypeOfPrn = PRN then
      pDataWord := PrnHandle
    else
      pError(DosAppend(pNameOfPrn,pDataWord),OpenCall);
    PrnOpen := (pLastError = 0);
  end;

{$IFDEF UseStreams}

  constructor AppendDosPrinter.Load(var S : IdStream);

  begin
    DosPrinter.Load(S);
  end;

  procedure AppendDosPrinter.Store(var S : IdStream);

  begin
    DosPrinter.Store(S);
  end;

  procedure AppendDosPrinterStream(SPtr : IdStreamPtr);
    {-Register types needed for streams containing DosPrinters}
  begin
    DosPrinterStream(SPtr);
    SPtr^.RegisterType(otAppendDosPrinter,veAppendDosPrinter,
                       TypeOf(AppendDosPrinter),
                       @AppendDosPrinter.Store,@AppendDosPrinter.Load);
  end;

{$ENDIF}

{!!.20 end}

  constructor FlexiblePrinter.Init(PrnName       : PathStr;
                                   TypeOfPrinter : PrnType;
                                   PutCharPrim   : CharFunc;
                                   OpenPrim      : OpenFunc;
                                   ClosePrim     : DeviceFunc;
                                   StatusPrim    : DeviceFunc;
                                   ResetPrim     : DeviceFunc;
                                   FlushPrim     : DeviceFunc;
                                   XlatPrim      : XlatFunc);

  begin
    fpPutCharPrim     := PutCharPrim;
    fpOpenPrim        := OpenPrim;
    fpClosePrim       := ClosePrim;
    fpStatusPrim      := StatusPrim;
    fpResetPrim       := ResetPrim;
    fpFlushPrim       := FlushPrim;
    fpXlatPrim        := XlatPrim;
    if not BasePrinter.Init(PrnName,TypeOfPrinter) then
      Fail;
  end;

{$IFDEF UseStreams}

  constructor FlexiblePrinter.Load(var S : IdStream);

  begin
    @fpPutCharPrim := S.ReadPointer;
    @fpOpenPrim    := S.ReadPointer;
    @fpClosePrim   := S.ReadPointer;
    @fpStatusPrim  := S.ReadPointer;
    @fpResetPrim   := S.ReadPointer;
    @fpFlushPrim   := S.ReadPointer;
    @fpXlatPrim    := S.ReadPointer;
    if S.PeekStatus <> 0 then
      Fail;
    if not BasePrinter.Load(S) then
      Fail;
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure FlexiblePrinter.Store(var S : IdStream);

  begin
    S.WritePointer(@fpPutCharPrim);
    S.WritePointer(@fpOpenPrim);
    S.WritePointer(@fpClosePrim);
    S.WritePointer(@fpStatusPrim);
    S.WritePointer(@fpResetPrim);
    S.WritePointer(@fpFlushPrim);
    S.WritePointer(@fpXlatPrim);
    BasePrinter.Store(S);
  end;

  procedure FlexiblePrinterStream(SPtr : IdStreamPtr);
    {-Register types needed for streams containing BasePrinters}

  begin
    BasePrinterStream(SPtr);
    with SPtr^ do begin
      RegisterType(otFlexiblePrinter,veFlexiblePrinter,
                   TypeOf(FlexiblePrinter),
                   @FlexiblePrinter.Store,@FlexiblePrinter.Load);
    end;
  end;

{$ENDIF}

  procedure FlexiblePrinter.PrnPutChar(Character : Char);

  begin
    pError(fpPutCharPrim(CharXlat(Character),pDataWord),PutCall);
  end;

  function FlexiblePrinter.PrnOpen : Boolean;

  begin
    pError(fpOpenPrim(pNameOfPrn,pTypeOfPrn,pDataWord),OpenCall);
    PrnOpen := pLastError = 0;
  end;

  procedure FlexiblePrinter.PrnClose;

  begin
    pError(fpClosePrim(pDataWord),CloseCall);
  end;

  procedure FlexiblePrinter.PrnFlush;

  begin
    pError(fpFlushPrim(pDataWord),FlushCall);
  end;

  procedure FlexiblePrinter.PrnReset;

  begin
    pError(fpResetPrim(pDataWord),ResetCall);
  end;

  function FlexiblePrinter.PrnStatus : Word;

  begin
    pError(fpStatusPrim(pDataWord),StatusCall);
    PrnStatus := pLastError;
  end;

  function FlexiblePrinter.PrnXlatErrorCode(Call : PrnCallType;
                                            ErrorCode : Word) : Word;

  begin
    PrnXlatErrorCode := fpXlatPrim(Call,ErrorCode);
  end;


  function PutPrim(C : Char; LPTNo : Word) : Word;

  begin
    PutPrim := Int17Out(C,LPTNo);
  end;

  function OpenPrim(Name : PathStr;
                    PrnTyp : PrnType;
                    var DataWord : Word) : Word;

  begin
    OpenPrim := $90;
  end;

  function NullPrim(DataWord : Word) : Word;

  begin
    NullPrim := 0;
  end;

  function XlatPrim(Call : PrnCallType; ErrorCode : Word) : Word;

  begin
    XlatPrim := 0;
  end;

  constructor BiosPrinter.Init(LPTNumber : LPTType);

  begin
    bpSuccessMask      := DefSuccessMask;
    bpPrinterTest      := DefPrinterTest;
    pDataWord          := Ord(LPTNumber);
    if not FlexiblePrinter.Init(LptNames[LPTNumber],
                                PrnType(LPTNumber),
                                PutPrim,
                                OpenPrim,
                                NullPrim,
                                LptDeviceReady,
                                Int17Reset,
                                NullPrim,
                                NoXlatFunc) then
      Fail;
  end;

  constructor BiosPrinter.InitCustom(LPTNumber : LPTType;
                                     PrinterTestNumber, SuccessMask : Byte);
  begin
    bpSuccessMask      := SuccessMask;
    bpPrinterTest      := PrinterTestNumber;
    pDataWord          := Ord(LPTNumber);
    if not FlexiblePrinter.Init(LptNames[LPTNumber],
                                PrnType(LPTNumber),
                                PutPrim,
                                OpenPrim,
                                NullPrim,
                                LptDeviceReady,
                                Int17Reset,
                                NullPrim,
                                NoXlatFunc) then
      Fail;
  end;

{$IFDEF UseStreams}

  constructor BiosPrinter.Load(var S : IdStream);

  begin
    if not FlexiblePrinter.Load(S) then
      Fail;
    S.Read(bpPrinterTest,SizeOf(bpPrinterTest) + SizeOf(bpSuccessMask));
  end;

  procedure BiosPrinter.Store(var S : IdStream);

  begin
    FlexiblePrinter.Store(S);
    if S.PeekStatus <> 0 then
      Exit;
    S.Write(bpPrinterTest,SizeOf(bpPrinterTest) + SizeOf(bpSuccessMask));
  end;

  procedure BiosPrinterStream(SPtr : IdStreamPtr);

  begin
    FlexiblePrinterStream(SPtr);
    with SPtr^ do begin
      RegisterType(otBiosPrinter,veBiosPrinter,TypeOf(BiosPrinter),
                   @BiosPrinter.Store,@BiosPrinter.Load);
      RegisterPointer(ptPutPrim,@PutPrim);
      RegisterPointer(ptOpenPrim,@OpenPrim);
      RegisterPointer(ptClosePrim,@NullPrim);
      RegisterPointer(ptReadyPrim,@LptDeviceReady);
      RegisterPointer(ptResetPrim,@Int17Reset);
      RegisterPointer(ptFlushPrim,@NullPrim);
      RegisterPointer(ptXlatPrim,@XlatPrim);
    end;
  end;

{$ENDIF}

  procedure BiosPrinter.PrnPutChar(Character : Char);
    {-Puts a character to the output device}
  begin
    if PrnStatus = 0 then
      FlexiblePrinter.PrnPutChar(Character);
  end;

  function BiosPrinter.PrnXlatErrorCode(Call : PrnCallType;
                                        ErrorCode : Word) : Word;
    {-translate a raw error code into appropriate user error code}
  begin
    if @fpXlatPrim <> Nil then begin
      PrnXlatErrorCode := fpXlatPrim(Call,ErrorCode);
      Exit;
    end;
    PrnXlatErrorCode := 0;

    {case Call of
      PutCall, StatusCall :}                        {!!.10}
    if Call = StatusCall then                       {!!.10}
      case bpPrinterTest of
        0 : begin end;                            {always succeed}
        1 : if not PrnTest1Prim(ErrorCode) then       {test 1}
              PrnXlatErrorCode := PrinterNotReady;
        2 : if not PrnTest2Prim(ErrorCode) then       {test 2}
              PrnXlatErrorCode := PrinterNotReady;
        3 : if not PrnTest3Prim(ErrorCode) then       {!!.10}
              PrnXlatErrorCode := PrinterNotReady;
        4 :                                       {test 4}
            if (Byte(ErrorCode) and bpSuccessMask) <> bpSuccessMask then
              PrnXlatErrorCode := PrinterNotReady;
        else
          PrnXlatErrorCode := 255;  {special code indicating invalid test}
      end;
    {end;}                                          {!!.10}
  end;

  function BiosPrinter.GetLptPort : LPTType;
    {-Returns the LPT port number for this printer}
  begin
    GetLptPort := LPTType(pDataWord);
  end;

  procedure BiosPrinter.SetLptPort(LPTNo : LPTType);
    {-Sets the LPT port number for this printer}
  begin
    pDataWord := Word(LPTNo);
    pNameOfPrn:= LptNames[LPTNo];
  end;

  procedure BiosPrinter.SetXlatErrorFunc(Xlat : XlatFunc);
    {-Sets the XlatError routine}
  begin
    fpXlatPrim := Xlat;
  end;

  function BiosPrinter.GetXlatErrorFunc : Pointer;
    {-Returns the Xlat Error routine}
  begin
    GetXlatErrorFunc := @fpXlatPrim;
  end;

  procedure BiosPrinter.GetTestAndMask(var PrinterTestNumber, Mask : Byte);
    {return the printer test number and success mask}
  begin
    PrinterTestNumber := bpPrinterTest;
    Mask := bpSuccessMask;
  end;

  procedure BiosPrinter.SetTestAndMask(PrinterTestNumber, Mask : Byte);
    {set the printer test number and success mask}
  begin
    bpPrinterTest := PrinterTestNumber;
    bpSuccessMask := Mask;
  end;

  procedure DefaultXlatCharTable(var Table : XlatCharTable);
    {-Initialize Table to default state}
  var
    C : Char;
  begin
    for C := #0 to #255 do
      Table[C] := C;
  end;

  function TFDDOutput(var F : PrnTextRec) : Integer;

  var
    ErrorCode        : Integer;
  begin
    with F, PrnObject^ do begin
      PrnPutBlock(BufPtr^,BufPos);
      ErrorCode := PrnError;
      TFDDOutput := ErrorCode;
(*                                            {!!.11}
      if ErrorCode = 0 then                   {!!.11}
*)                                            {!!.11}
        BufPos := 0
    end;
  end;

  function TFDDOutputFlush(var F : PrnTextRec) : Integer;

  var
    ErrorCode        : Integer;

  begin
    ErrorCode := TFDDOutput(F);
      if ErrorCode = 0 then
      with F, PrnObject^ do begin
        PrnFlush;
        ErrorCode := PrnError;
      end;
    TFDDOutputFlush := ErrorCode;
  end;

  function TFDDOpen(var F : PrnTextRec) : Integer;

  begin
(*                                                 {!!.03}
    with F, PrnObject^ do
      if PrnOpen then
        TFDDOpen := 0
      else
        TFDDOpen := PrnError;
*)                                                 {!!.03}
    TFDDOpen := 0                                  {!!.03}
  end;

  function TFDDClose(var F : PrnTextRec) : Integer;

  begin
    with F, PrnObject^ do begin
      PrnClose;
      TFDDClose := PrnError;
    end;
  end;

  procedure AssignPrnObject(var F : Text;
                            var PrinterObj : BasePrinter);
  { like Turbo's assign, except associates Text variable with one of the LPTs }
  var
    PrinterName : String;
    PrnF        : PrnTextRec absolute F;

  begin
    with PrnF do begin
      Mode        := fmClosed;
      BufSize     := SizeOf(Buffer);
      BufPtr      := @PrnF.Buffer;
      OpenFunc    := @TFDDOpen;
      CloseFunc   := @TFDDClose;
      InOutFunc   := @TFDDOutput;
      FlushFunc   := @TFDDOutputFlush;
      PrnObject   := @PrinterObj;
      PrnID       := PrnTfddID;
      PrinterName := PrinterObj.NameOfPrn;
      Move(PrinterName[1],Name,Length(PrinterName)+1);
      BufPos := 0; { reset BufPos }
    end;
  end;

  function GetPrnObject(var F : Text) : BasePrinterPtr;
    {-Return a pointer to the Prn Object associated with a Text file.
      Returns Nil if not a text file associated with a Prn object}
  var
    T : PrnTextRec absolute F;

  begin
    with T do
      if PrnID = PrnTfddID then
        GetPrnObject := PrnObject
      else
        GetPrnObject := Nil;
  end;

  function CallPrnStatus(var F : Text) : Word;
    {-Call PrnStatus method for Text file associated with Prn Object,
      If not Prn Object TFDD, then $FFFF is returned.}
  var
    P : BasePrinterPtr;
  begin
    P := GetPrnObject(F);
    if P <> Nil then
      CallPrnStatus := P^.PrnStatus
    else
      CallPrnStatus := $FFFF;
  end;

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
