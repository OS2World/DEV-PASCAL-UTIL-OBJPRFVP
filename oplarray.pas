{$IFDEF Windows}
  !! ERROR - This unit is not compatible with Windows !!
{$ENDIF}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPLARRAY.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpLarray;
  {-Dynamically allocated arrays that use >64k of RAM, virtual memory, or EMS
    memory.}

interface

uses
  Dos,
  Use32,
  OpConst,    {!!.20}
  OpInline,
  OpString,
  OpDos,
  OpRoot
  {$IFDEF VirtualPascal}
  , VpSysLow
  {$ELSE}               {!!.20}  {!!.SAG}
  {$IFNDEF Dpmi}
  , OpEMS
  {$IFDEF SupportXms}          {!!.03}
  , OpXms                      {!!.03}
  {$ENDIF}                     {!!.03}
  {$ENDIF}                     {!!.20}
  {$ENDIF}
  ;

const
  AllOfMemory    = 655360;     {640K}

  PageSizeMax    = 16350;
  MaxHeapBlock   = 65519;      {Largest single block on heap, less one}

  IdString       : string[6] = 'LARRAY'; {Written at the start of array files}
  VArrayID       : String[6] = 'VARRAY';

  {$IFDEF VirtualPascal}                      {!!.20}
  MaxPriority    = 2;                         {!!.20}
  {$ELSE}                                     {!!.20}
  {$IFDEF Dpmi}                               {!!.20}
  MaxPriority    = 2;                         {!!.20}
  {$ELSE}                                     {!!.20}
  {$IFDEF SupportXms}                         {!!.03}
  MaxPriority    = 4;                         {!!.03}
  {$ELSE}                                     {!!.03}
  MaxPriority    = 3;                         {!!.03}
  {$ENDIF}                                    {!!.03}
  {$ENDIF}                                    {!!.20}
  {$ENDIF}                                    {!!.20}

  {-------------OPLARRAY option lFlags----------------}
  lKeepDiskCurrent = $01;   {Keep disk current for virtual array?}
  lDeleteFile      = $02;   {Delete virtual array file on Done?}
  lRangeCheck      = $04;   {Use range checking for OpArray object?}

  {internal flag}
  lTpVArrayFormat  = $80;   {maintain TpVArray header format} {!!.03}

type
  InitType         = (ExactInit, FastInit); {initialization options}
  ArrayType        = (lNoArray,
                      lRamArray,
                      lEMSArray,
                      lVirtualArray,
                      lXmsArray);         {The types of arrays}        {!!.03}

  AutoPriority     = Array[1..MaxPriority] of ArrayType; {A list of priorities}
  ArrayLI =                  {used to allow an inline macro to return 2 words}
    record                   {as the function result}
      POfs,        {lo/ax}
      PNum : SmallWord; {hi/dx}
    end;
  ArrayRC =                  {used to allow an inline macro to return 2 words}
    record                   {as the function result}
      R,       {lo/ax}
      C : SmallWord; {hi/dx}
    end;

  LArrayDimensions =
    record                   {the number of rows and columns in an Array}
      dRows,
      dCols : Word;
    end;
  {Internal Data storage}
  DataPage         = array[0..MaxHeapBlock] of Byte;
  DataPagePtr      = ^DataPage;
  PageIndex        = array[0..PageSizeMax] of DataPagePtr;
  PageIndexPtr     = ^PageIndex;

  {Header for description of a virtual array}
  {DO NOT CHANGE WITHOUT UPDATING RECORD OFFSETS IN OPLARRAY.IN3}
  VArrayHeader =
    record
      RowsInPage : Word;
      ColsInPage : Word;
      PagesDown : Word;
      PagesAcross : Word;
      ElSize : Word;         {Bytes in one array element}
      PageSize : Word;       {Bytes of data in one page}
      PageCount : Word;      {Number of pages in entire array}
      ID : string[6];        {Identifies array files}
      NextAvail : LongInt;   {Next file position for new page}
    end;

  OldVArrayHeader =
    record
      RowsInPage : Word;
      ColsInPage : Word;
      PagesDown : Word;
      PagesAcross : Word;
      ElSize : Word;         {Bytes in one array element}
      PageSize : Word;       {Bytes of data in one page}
      PageCount : Word;      {Number of pages in entire array}
      NextAvail : LongInt;   {Next file position for new page}
      ID : string[6];        {Identifies array files}
    end;

  {Descriptor of a page kept in RAM}
  PageRec =
    record
      PageNum : Word;        {Index into disk pointer table}
      LRU : Word;            {Quasi-time when page last used}
      Used : Boolean;        {True when page is loaded with data}
      Dirty : Boolean;       {True when RAM page differs from disk page}
      Data : DataPage;       {Actual data for page}
    end;
  PageRecPtr       = ^PageRec;

  RamPageType      = 0..16000;
  DiskPageType     = 0..16000;

  {Pointers to pages in RAM}
  RamPageIndex     = array[RamPageType] of PageRecPtr;

  {Map between disk page and RAM page, holds NotInRAM if not in RAM}
  RamPageMap       = array[DiskPageType] of Word;

  {Offset within disk file of each page}
  DiskPageMap      = array[DiskPageType] of LongInt;

  OpArrayPtr       = ^OpArray;

  {the base object type for Large Arrays}
  AbstractArrayPtr = ^AbstractArray;
  AbstractArray =
    object(Root)
      TypeOfLArray   : ArrayType;   {The type of this array}
      Dimension      : LArrayDimensions; {the dimensions of this array}
      TotalHeapToUse : LongInt;     {the maximum heap to use}
      lFlags         : Byte;        {Array Option Flags}
      LastError      : Word;        {Holds the last error number encountered}
      Internal       : Pointer;     {pointer to variant internal structure}
      lErrorProc     : ErrorProc;   {the large array error procedure}

      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       HeapToUse : LongInt; ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(HeapToUse : LongInt; ArrayOptions : Byte);
        {-load an array from disk}
      destructor Done; Virtual;
        {-dispose of an array}
      function ErrorA : Word;
        {-return last error code (0 indicates no error)}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      procedure SafeSetA(Row, Col : Word; var Value); Virtual;
        {-set an array element and safely deal with out of range coordinates}
      procedure SafeRetA(Row, Col : Word; var Value); Virtual;
        {-Return an array element and safely deal with out of range coordinates}
      procedure ClearA(var Value; Initialize : InitType); Virtual;
        {-clear the array}
      function GetElementSize : Word; Virtual;
        {-Return the element size for the array}
      procedure StoreA(FileName : String); Virtual;
        {-Store large array in a file}
      procedure CloseA; Virtual;
        {-close and flush an array}
      procedure FlushA; Virtual;
        {-flush changes to disk}
      function RangeError(Row, Col : Word) : Boolean; Virtual;
        {-returns true if passed Row or Col are out of range}
      function TypeOfArray : ArrayType;
        {-returns the type of array}
      procedure SetErrorProc(EP : ErrorProc);
        {-setup a user errorproc}
      procedure GetErrorProc(var EP : ErrorProc);
        {-get current errorproc}
      procedure ArrayDimensions(var Rows,Cols : Word);
        {-return array dimensions}

      procedure lOptionsOn(OptionCodes : Word);
        {-set array options on}
      procedure lOptionsOff(OptionCodes : Word);
        {-set array options off}
      function lOptionsAreOn(OptionCodes : Word) : Boolean;
        {-check options}
      {.Z+}
      procedure Error(ErrNum : Word); Virtual;
        {-sets LastError and acts based on error action lFlags}
      {.Z-}
    end;

  RamArrayPtr = ^RamArray;
  {Large Array object for RAM based Arrays}
  RamArray =
    object(AbstractArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       HeapToUse : LongInt; ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte);
        {-load an array from disk}
      {.Z+}
      destructor Done; Virtual;
        {-dispose of an array}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      procedure SafeSetA(Row, Col : Word; var Value); Virtual;
        {-set an array element and safely deal with out of range coordinates}
      procedure SafeRetA(Row, Col : Word; var Value); Virtual;
        {-Return an array element and safely deal with out of range coordinates}
      procedure ClearA(var Value; Initialize : InitType); Virtual;
        {-clear the array}

      procedure StoreA(FileName : String); Virtual;
        {-Store large array in a file}
      function RangeError(Row, Col : Word) : Boolean; Virtual;
        {-returns true if passed Row or Col are out of range}
      function GetElementSize : Word; Virtual;
        {-Return the element size for the array}
      {.Z-}
    end;

  RamRCArrayPtr = ^RamRCArray;
  {Special Large RAM Array object that uses Range Checking}
  RamRCArray =
    object(RamArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       HeapToUse : LongInt; ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte);
        {-load an array from disk}
      {.Z+}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      {.Z-}
    end;

{$IFNDEF VirtualPascal}
{$IFNDEF Dpmi}                                                                  {!!.20}
  EmsArrayPtr = ^EmsArray;
  {Large Array object for EMS based Arrays}
  EmsArray =
    object(AbstractArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       HeapToUse : LongInt; ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte);
        {-load an array from disk}
      {.Z+}
      destructor Done; Virtual;
        {-dispose of an array}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      procedure SafeSetA(Row, Col : Word; var Value); Virtual;
        {-set an array element and safely deal with out of range coordinates}
      procedure SafeRetA(Row, Col : Word; var Value); Virtual;
        {-Return an array element and safely deal with out of range coordinates}
      procedure ClearA(var Value; Initialize : InitType); Virtual;
        {-clear the array}
      procedure StoreA(FileName : String); Virtual;
        {-Store large array in a file}
      function RangeError(Row, Col : Word) : Boolean; Virtual;
        {-returns true if passed Row or Col are out of range}
      function GetElementSize : Word; Virtual;
        {-Return the element size for the array}
      {.Z-}
    end;

  EmsRCArrayPtr = ^EmsRCArray;
  {Special Large EMS Array Object that uses Range Checking}
  EmsRCArray =
    object(EmsArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       HeapToUse : LongInt; ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte);
        {-load an array from disk}
      {.Z+}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      {.Z-}
    end;
{$ENDIF}                                                               {!!.20}
{$ENDIF}

  VirtualArrayPtr = ^VirtualArray;
  {Large Array object for Virtual Arrays}
  VirtualArray =
    object(AbstractArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       FileName : String; HeapToUse : LongInt;
                       ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte);
        {-load an array from disk}
      {.Z+}
      destructor Done; Virtual;
        {-dispose of an array}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      procedure SafeSetA(Row, Col : Word; var Value); Virtual;
        {-set an array element and safely deal with out of range coordinates}
      procedure SafeRetA(Row, Col : Word; var Value); Virtual;
        {-Return an array element and safely deal with out of range coordinates}
      procedure ClearA(var Value; Initialize : InitType); Virtual;
        {-clear the array}
      procedure StoreA(FileName : String); Virtual;
        {-Store large array in a file}
      procedure CloseA; Virtual;
        {-Close and flush the array}
      procedure FlushA; Virtual;
        {-flush changes to disk}
      function RangeError(Row, Col : Word) : Boolean; Virtual;
        {-returns true if passed Row or Col are out of range}
      function GetElementSize : Word; Virtual;
        {-Return the element size for the array}
      {.Z-}
    end;

  PrimitiveArrayPtr = ^PrimitiveArray;
  {Virtual Large Array Object with full control over internal page structure}
  PrimitiveArray =
    object(VirtualArray)
      constructor Init(RowsInPage : Word;
                       ColsInPage : Word;
                       PagesDown : Word;
                       PagesAcross : Word;
                       ElementSize : Word;
                       FileName : string;
                       HeapToUse : LongInt;
                       ArrayOptions : Byte);
        {-make a new array}
    end;

  VirtualRCArrayPtr = ^VirtualRCArray;
  {Special Virtual Array Object that uses Range Checking}
  VirtualRCArray =
    object(VirtualArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       FileName : String; HeapToUse : LongInt;
                       ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte);
        {-load an array from disk}
      {.Z+}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      {.Z-}
    end;

{$IFNDEF VirtualPascal}
{$IFNDEF Dpmi}                                              {!!.20}
  {$IFDEF SupportXms}                                 {begin !!.03}
  XmsArrayPtr = ^XmsArray;
  {Large Array object for XMS based Arrays}
  XmsArray =
    object(AbstractArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       HeapToUse : LongInt; ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte);
        {-load an array from disk}
      {.Z+}
      destructor Done; Virtual;
        {-dispose of an array}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      procedure SafeSetA(Row, Col : Word; var Value); Virtual;
        {-set an array element and safely deal with out of range coordinates}
      procedure SafeRetA(Row, Col : Word; var Value); Virtual;
        {-Return an array element and safely deal with out of range coordinates}
      procedure ClearA(var Value; Initialize : InitType); Virtual;
        {-clear the array}
      procedure StoreA(FileName : String); Virtual;
        {-Store large array in a file}
      function RangeError(Row, Col : Word) : Boolean; Virtual;
        {-returns true if passed Row or Col are out of range}
      function GetElementSize : Word; Virtual;
        {-Return the element size for the array}
      {.Z-}
    end;

  XmsRCArrayPtr = ^XmsRCArray;
  {Special Large XMS Array Object that uses Range Checking}
  XmsRCArray =
    object(XmsArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                       HeapToUse : LongInt; ArrayOptions : Byte);
        {-make a new array}
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte);
        {-load an array from disk}
      {.Z+}
      procedure SetA(Row, Col : Word; var Value); Virtual;
        {-set an array element}
      procedure RetA(Row, Col : Word; var Value); Virtual;
        {-return an array element}
      {.Z-}
    end;
  {$ENDIF}                                               {end !!.03}
{$ENDIF}                                                     {!!.20}
{$ENDIF}

  {Object for automatic "best fit" Large Arrays}
  OpArray =
    object(AbstractArray)
      constructor Init(Rows, Cols : Word; ElementSize : Word;
                        FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte; var Priority : AutoPriority);
      constructor LoadA(FileName : String; HeapToUse : LongInt;
                        ArrayOptions : Byte;
                        var Priority : AutoPriority);
        {-make a new array}
    end;

const
  {A reasonable default priority for OpArrays}
  {$IFDEF VirtualPascal}
  DefaultPriority  : AutoPriority = (lRamArray,lVirtualArray);    {!!.20}
  {$ELSE}                                                         {!!.20}
  {$IFDEF Dpmi}                                                   {!!.20}
  DefaultPriority  : AutoPriority = (lRamArray,lVirtualArray);    {!!.20}
  {$ELSE}                                                         {!!.20}
  {$IFDEF SupportXms}                                             {!!.03}
  DefaultPriority  : AutoPriority = (lRamArray,lEMSArray,         {!!.03}
                                     lXMSArray,lVirtualArray);    {!!.03}{!!.13}
  {$ELSE}                                                         {!!.03}
  DefaultPriority  : AutoPriority = (lRamArray,lEMSArray,lVirtualArray); {!!.03}
  {$ENDIF}                                                        {!!.03}
  {$ENDIF}                                                        {!!.20}
  {$ENDIF}

  {Determines whether the default error handler will beep on an error}
  DefBeepOnError   : Boolean = True;

var
  {$IFNDEF VirtualPascal}
  {$IFNDEF Dpmi}                                             {!!.20}
  EmsAvailable : Boolean;        {True if EMS is available}
  {$IFDEF SupportXms}                                        {!!.03}
  XmsAvailable : Boolean;        {True if XMS is available}  {!!.03}
  {$ENDIF}                                                   {!!.03}
  {$ENDIF}                                                   {!!.20}
  {$ENDIF}
  DefaultErrorProc : ErrorProc;  {points to error handler used by}   {!!.01}
                                 {default by array objects}

procedure DisplayLErrorProc(UnitCode : Byte; var ErrorCode : Word;
                            ErrorMsg : String);
  {-An error procedure that displays a standard message for errors generated
    by this unit.  Does not halt on errors}

  {==========================================================================}

implementation

const
  EMSInitialized   : Boolean = False; {indicates whether EMS has been
                                       initialized}
  {Ram array constants}
  RPPOfs         = 0;          {Offsets within header record of each field}
  CPPOfs         = 2;
  PDOfs          = 4;
  PAOfs          = 6;
  ESOfs          = 8;
  PSOfs          = 10;
  PCOfs          = 12;

  {EMS Array constants}
  EmsColsOfs = 0;            {Offsets within header record of each field}
  EmsSiOfs = 2;
  EmsRowsOfs = 4;

  {Virtual array constants}
  vRPPOfs = 0;                {Offsets within header record of each field}
  vCPPOfs = 2;                {  used within inline macro below}
  vPDOfs = 4;
  vPAOfs = 6;
  vESOfs = 8;

type
  SegOfs =                   {structure of a pointer}
    record
      Ofst, Segm : Word;
    end;

  procedure Reset(var F : File; RecLen : Word);    {!!.13}
  var
    SaveFMode : Word;
  begin
    SaveFMode := FileMode;
    FileMode := 2;
    System.Reset(F, RecLen);
    FileMode := SaveFMode;
  end;

  function DivUp(X, Y : LongInt) : Word;
    {-Compute X div Y and round up}
  begin
    DivUp := (X div Y)+Ord(X mod Y <> 0);
  end;

  procedure DefaultLErrorProc(UnitCode : Byte; var ErrorCode : Word;
                              ErrorMsg : String);
    {-The default error procedure}
  begin
    if DefBeepOnError then
      Write(^G);
  end;

  procedure DisplayLErrorProc(UnitCode : Byte; var ErrorCode : Word;
                              ErrorMsg : String);
    {-An error procedure that displays a string indicating the nature of the
      error}
  begin
    ErrorCode := ErrorCode mod epNonFatal;
    Write('Large array error: ');
    case ErrorCode of
      ecFileNotFound    : WriteLn(emFileNotFound);
      ecPathNotFound    : WriteLn(emPathNotFound);
      ecOutOfMemory     : WriteLn(emInsufficientmemory);
      ecDeviceWrite     : WriteLn(emWriteError);
      ecDeviceRead      : WriteLn(emReadError);
      ecRowOutOfRange   : WriteLn(emRowOutOfRange);
      ecColOutOfRange   : WriteLn(emColOutOfRange);
      ecNotLArrayFile   : WriteLn(emNotLArrayFile);
      ecElementTooBig   : WriteLn(emElementTooBig);
      ecBadDimensions   : WriteLn(emBadDimensions);
      ecLessThanOnePage : WriteLn(emLessThanOnePage);
      ecFlushError      : WriteLn(emFlushError);
      ecInsufficientEms : WriteLn(emInsufficientEms);
      ecEmsAllocation   : WriteLn(emEmsAllocation);
      ecNoEms           : WriteLn(emNoEms);
      ecEmsPageMapping  : WriteLn(emEmsPageMapping);
      ecCantFreeEms     : WriteLn(emCantFreeEms);
      ecElSizeIsZero    : WriteLn(emElSizeIsZero);
      else WriteLn('Unknown error code', ErrorCode);
    end;
    if DefBeepOnError then
      Write(^G);
  end;

  {$I OPLARRAY.IN1}     {pulls in the code for the RamArray object}

{$IFNDEF VirtualPascal}
{$IFNDEF Dpmi}                                                       {!!.20}
  {$I OPLARRAY.IN2}     {pulls in the code for the EmsArray object}
{$ENDIF}                                                             {!!.20}
{$ENDIF}                                                             {!!.20}

  {$I OPLARRAY.IN3}     {pulls in the code for the VirtualArray object}

{$IFNDEF VirtualPascal}
{$IFNDEF Dpmi}                                                       {!!.20}
  {$IFDEF SupportXms}                                                {!!.03}
  {$I OPLARRAY.IN4}     {pulls in the code for the XmsArray object}  {!!.03}
  {$ENDIF}                                                           {!!.03}
{$ENDIF}                                                             {!!.20}
{$ENDIF}                                                             {!!.20}

  {constructors}
  constructor AbstractArray.Init(Rows, Cols : Word; ElementSize : Word;
                                 HeapToUse : LongInt; ArrayOptions : Byte);
    {-Init Constructor for base object type, initializes common fields}
  begin
    LastError := 0;
    lFlags := ArrayOptions;
    Dimension.dRows := Rows;
    Dimension.dCols := Cols;
    TotalHeapToUse := HeapToUse;
    lErrorProc := DefaultErrorProc;         {!!.01}
    Internal := NIL;
    if not Root.Init then
      Fail;
  end;

  constructor AbstractArray.LoadA(HeapToUse : LongInt; ArrayOptions : Byte);
    {-Base object LoadA, initializes common fields}
  begin
    LastError := 0;
    lFlags := ArrayOptions;
    TotalHeapToUse := HeapToUse;
    lErrorProc := DefaultLErrorProc;
    Internal := NIL;
    if not Root.Init then
      Fail;
  end;

  destructor AbstractArray.Done;
    {-Do nothing Destructor}
  begin
    Abstract;
  end;

  {methods}
  function AbstractArray.TypeOfArray : ArrayType;
    {-Return the type of an array object}
  begin
    TypeOfArray := TypeOfLArray;
  end;

  procedure AbstractArray.SetA(Row, Col : Word; var Value);
    {-Do nothing SetA method}
  begin
    Abstract;
  end;

  procedure AbstractArray.RetA(Row, Col : Word; var Value);
    {-Do nothing RetA method}
  begin
    Abstract;
  end;

  procedure AbstractArray.SafeSetA(Row, Col : Word; var Value);
    {-Do nothing SafeSetA method}
  begin
    Abstract;
  end;

  procedure AbstractArray.SafeRetA(Row, Col : Word; var Value);
    {-Do nothing SafeRetA method}
  begin
    Abstract;
  end;

  procedure AbstractArray.ClearA(var Value; Initialize : InitType);
    {-Do nothing ClearA method}
  begin
    Abstract;
  end;

  procedure AbstractArray.lOptionsOn(OptionCodes : Word);
    {-Turns the options in OptionCodes on}
  begin
    {setting of range check flag not allowed, so mask it out}
    OptionCodes := OptionCodes and (not lRangeCheck);
    SetByteFlag(lFlags,OptionCodes);
  end;

  procedure AbstractArray.lOptionsOff(OptionCodes : Word);
    {-Turns the options in OptionCodes off}
  begin
    {clearing of range check flag not allowed, so mask it out}
    OptionCodes := OptionCodes and (not lRangeCheck);
    ClearByteFlag(lFlags,OptionCodes);
  end;

  function AbstractArray.lOptionsAreOn(OptionCodes : Word) : Boolean;
    {-Return True if the flags indicated by OptionCodes are on}
  begin
    lOptionsAreOn := ByteFlagIsSet(lFlags,OptionCodes);
  end;

  procedure AbstractArray.Error(ErrNum : Word);
    {-Method used internally when error occurs}
  begin
    LastError := ErrNum;               {store error in object's data field}
    if @lErrorProc <> Nil then
      lErrorProc(ucLArray,ErrNum,'');    {Call the error handler}
  end;

  function AbstractArray.ErrorA : Word;
    {-Return the last error number and reset the error variable}
  begin
    ErrorA := LastError;
    LastError := 0;
  end;

  procedure AbstractArray.CloseA;
    {-close the large array file.}
  begin
  end;

  function AbstractArray.RangeError(Row, Col : Word) : Boolean;
    {-Do nothing RangeError function}
  begin
    Abstract;
  end;

  procedure AbstractArray.SetErrorProc(EP : ErrorProc);
    {-Sets the procedure to be called when an error occurs}
  begin
    lErrorProc := EP;
  end;

  procedure AbstractArray.GetErrorProc(var EP : ErrorProc);
    {-Returns a procedure pointer to the current error handler}
  begin
    EP := lErrorProc;
  end;

  procedure AbstractArray.StoreA(FileName : String);
    {-Do nothing StoreA method}
  begin
    Abstract;
  end;

  procedure AbstractArray.FlushA;
    {-Do nothing FlushA method}
  begin
  end;

  procedure AbstractArray.ArrayDimensions(var Rows, Cols : Word);
    {-Return an array's dimensions}
  begin
    with Dimension do begin
      Rows := dRows;
      Cols := dCols;
    end;
  end;

  function AbstractArray.GetElementSize : Word;
    {-Return the element size for the array}
  begin
    Abstract;
  end;

  constructor OpArray.Init(Rows, Cols : Word; ElementSize : Word;
                            FileName : String; HeapToUse : LongInt;
                            ArrayOptions : Byte;
                            var Priority : AutoPriority);
    {-automatically choose type of array based on available resources}
  var
    KeepLooking,UseRangeCheck : Boolean;
    I : Byte;
  begin
    if not AbstractArray.Init(Rows, Cols, ElementSize,
                              HeapToUse, ArrayOptions) then
      Fail;

    {In the order specified by the PriorityList, check each array type
     specified to see if the required resources are available to allocate the
     array, stopping at the first array type that has the needed resources.
     Fails if no array type has the available resources.}

    I := 1;
    KeepLooking := True;
    UseRangeCheck := ByteFlagIsSet(lRangeCheck,ArrayOptions);
    while (I <= MaxPriority) and KeepLooking do begin    {!!.03}
      case Priority[I] of
        lRamArray :
          if UseRangeCheck then
            KeepLooking := not RamRCArray(Self).Init(Rows, Cols, ElementSize,
                                                     HeapToUse,
                                                     ArrayOptions)
          else
            KeepLooking := not RamArray(Self).Init(Rows, Cols, ElementSize,
                                                   HeapToUse,
                                                   ArrayOptions);
{$IFNDEF VirtualPascal}
{$IFNDEF Dpmi} {!!.20}
        lEMSArray :
          if UseRangeCheck then
            KeepLooking := not EmsRCArray(Self).Init(Rows, Cols, ElementSize,
                                                     HeapToUse,
                                                     ArrayOptions)
          else
            KeepLooking := not EmsArray(Self).Init(Rows, Cols, ElementSize,
                                                   HeapToUse,
                                                   ArrayOptions);
{$ENDIF}       {!!.20}
{$ENDIF}
        lVirtualArray :
          if UseRangeCheck then
            KeepLooking := not VirtualRCArray(Self).Init(Rows, Cols,
                                                         ElementSize,
                                                         FileName,
                                                         HeapToUse,
                                                         ArrayOptions)
          else
            KeepLooking := not VirtualArray(Self).Init(Rows, Cols, ElementSize,
                                                       FileName, HeapToUse,
                                                       ArrayOptions);

{$IFNDEF VirtualPascal}
{$IFNDEF Dpmi} {!!.20}

        {$IFDEF SupportXms}                             {begin !!.03}
        lXmsArray :
          if UseRangeCheck then
            KeepLooking := not XmsRCArray(Self).Init(Rows, Cols, ElementSize,
                                                     HeapToUse,
                                                     ArrayOptions)
          else
            KeepLooking := not XmsArray(Self).Init(Rows, Cols, ElementSize,
                                                   HeapToUse,
                                                   ArrayOptions);
        {$ENDIF}                                        {end !!.03}
{$ENDIF}         {!!.20}
{$ENDIF}
      end;
      if KeepLooking then
        Inc(I);             {increment the priority index}
    end;
    if KeepLooking then     {if no resources avail, then fail}
      Fail;
  end;

  constructor OpArray.LoadA(FileName : String; HeapToUse : LongInt;
                            ArrayOptions : Byte; var Priority : AutoPriority);
    {-automatically chose type of array based on available resources}
  var
    KeepLooking,UseRangeCheck : Boolean;
    I : Byte;
  begin
    if not AbstractArray.LoadA(HeapToUse, ArrayOptions) then
      Fail;

    {Check for the file's existence before attempting load}
    if not ExistFile(FileName) then begin
      Error(epFatal+ecFileNotFound);
      InitStatus := epFatal+ecFileNotFound;
      Fail;
    end;

    {In the order specified by the PriorityList, check each array type
     specified to see if the required resources to load the array are
     available, stopping at the first array type that has the needed
     resources. Fails if no array type has the available resources.}

    KeepLooking := True;
    I := 1;
    UseRangeCheck := ByteFlagIsSet(lRangeCheck,ArrayOptions);
    while (I <= MaxPriority) and KeepLooking do begin     {!!.03}
      case Priority[I] of
        lRamArray :
          if UseRangeCheck then
            KeepLooking := not RamRCArray(Self).LoadA(FileName, HeapToUse,
                                                      ArrayOptions)
          else
            KeepLooking := not RamArray(Self).LoadA(FileName, HeapToUse,
                                                    ArrayOptions);
{$IFNDEF VirtualPascal}
{$IFNDEF Dpmi}   {!!.20}
        lEMSArray :
          if UseRangeCheck then
            KeepLooking := not EmsRCArray(Self).LoadA(FileName, HeapToUse,
                                                      ArrayOptions)
          else
            KeepLooking := not EmsArray(Self).LoadA(FileName, HeapToUse,
                                                    ArrayOptions);
{$ENDIF}         {!!.20}
{$ENDIF}
        lVirtualArray :
          if UseRangeCheck then
            KeepLooking := not VirtualRCArray(Self).LoadA(FileName,
                                                          HeapToUse,
                                                          ArrayOptions)
          else
            KeepLooking := not VirtualArray(Self).LoadA(FileName, HeapToUse,
                                                        ArrayOptions);
{$IFNDEF VirtualPascal}
{$IFNDEF Dpmi}   {!!.20}
        {$IFDEF SupportXms}
        lXmsArray :                                     {begin !!.03}
          if UseRangeCheck then
            KeepLooking := not XmsRCArray(Self).LoadA(FileName, HeapToUse,
                                                      ArrayOptions)
          else
            KeepLooking := not XmsArray(Self).LoadA(FileName, HeapToUse,
                                                    ArrayOptions);
        {$ENDIF}                                        {end !!.03}
{$ENDIF}         {!!.20}
{$ENDIF}
      end;
      if KeepLooking then
        Inc(I);
    end;
    if KeepLooking then
      Fail;
  end;

begin
  {$IFNDEF VirtualPascal}
  {$IFNDEF Dpmi}  {!!.20}
  EmsAvailable := EmsInstalled;
  {$IFDEF SupportXms}
  XmsAvailable := XmsInstalled;
  {$ENDIF}
  {Initialize quasi-clock}
  LRUCount := 0;                          {!!.01}
  {$ENDIF}
  {$ENDIF}        {!!.20}
  DefaultErrorProc  := DefaultLErrorProc; {!!.01}
end.
