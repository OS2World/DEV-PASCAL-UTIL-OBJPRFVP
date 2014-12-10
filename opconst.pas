{$S-,R-,V-,I-,B-,F+,O-,A-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPCONST.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit
  OpConst;

interface

  {--------- error classes (types) ---------}
const
  etFatal           = 0;  {fatal errors}
  etNonFatal        = 1;  {non-fatal I/O errors}
  etWarning         = 2;  {warnings, user input errors, etc.}
  etMessage         = 3;  {simple messages}
  etNoError         = 4;  {no error}

  {--------- error class prefixes ---------}

  epFatal           = etFatal    * 10000;
  epNonFatal        = etNonFatal * 10000;
  epWarning         = etWarning  * 10000;
  epMessage         = etMessage  * 10000;

  {--------- error codes ---------}

  ecOK              = 00000; {no error, success}                    {!!.20}

  {DOS errors}
  ecFileNotFound    = 00002; {file not found}
  ecPathNotFound    = 00003; {path not found}
  ecTooManyFiles    = 00004; {too many open files}
  ecAccessDenied    = 00005; {file access denied}
  ecInvalidHandle   = 00006; {invalid file handle}
  ecOutOfMemory     = 00008; {insufficient memory}
  ecInvalidDrive    = 00015; {invalid drive}
  ecNoMoreFiles     = 00018; {no more files}

  {Turbo Pascal I/O errors}
  ecDiskRead        = 00100; {attempt to read beyond end of file}
  ecDiskFull        = 00101; {disk is full}
  ecNotAssigned     = 00102; {file not Assign-ed}
  ecNotOpen         = 00103; {file not open}
  ecNotOpenInput    = 00104; {file not open for input}
  ecNotOpenOutput   = 00105; {file not open for output}
  ecInvalidFormat   = 00106; {invalid format for packed window}

  {DOS critical errors}
  ecWriteProtected  = 00150; {disk is write-protected}
  ecUnknownUnit     = 00151; {unknown disk unit}
  ecDriveNotReady   = 00152; {drive is not ready}
  ecUnknownCommand  = 00153; {unknown command}
  ecCrcError        = 00154; {data error}
  ecBadStructLen    = 00155; {bad request structure length}
  ecSeekError       = 00156; {seek error}
  ecUnknownMedia    = 00157; {unknown media type}
  ecSectorNotFound  = 00158; {disk sector not found}
  ecOutOfPaper      = 00159; {printer is out of paper}
  ecDeviceWrite     = 00160; {device write error}
  ecDeviceRead      = 00161; {device read error}
  ecHardwareFailure = 00162; {general failure}

  {new Turbo-compatible runtime errors}                   {!!.03}
  ecAbstractCall    = 00211; {call to an abstract method} {!!.03}

  {reported by OPBROWSE and OPEDITOR}
  ecFileIsEmpty     = 01000; {file is empty}

{$IFDEF UseStreams}
  {reported by OPROOT.IdStream}
  ecIdUnknown       = 01101; {unknown object id read from stream}
  ecLoadFailed      = 01102; {load constructor failed within Get or GetPtr}
  ecInvalidData     = 01103; {attempt to read/write bad data}

  {reported by OPROOT.OpLibrary}
  ecNameBlank       = 01200; {can't add entry if name is blank}
  ecPackFailed      = 01201; {error packing OpLibrary}
  ecBadSignature    = 01202; {bad signature in directory}
{$ENDIF}

  {reported by OPROOT.StaticQueue}
  ecQueueFull       = 02000; {queue is full. new element rejected}
  ecQueueEmpty      = 02001; {queue is empty. no element returned}

  {reported by OPROOT.StringArray}
  ecArrayTooBig     = 02002; {string array from text file exceeds 64K} {!!.02}

{$IFDEF UseStreams}
  {reported by OPROOT.OpLibrary}
  ecDirectoryFull   = 02020; {directory full--can't add entry}
{$ENDIF}

  {reported by OPCMD}
  ecPartialMatch    = 02050; {cannot add command due to partial match}
  ecKeyTableFull    = 02051; {cannot add command due to full key table}

  {reported by OPLARRAY}
  ecInsufficientEms = 02100; {insufficient EMS memory to allocate}
  ecEmsAllocation   = 02101; {error allocating EMS page}
  ecNoEms           = 02102; {EMS operation failed because no EMS available}
  ecEmsPageMapping  = 02103; {error mapping EMS page}
  ecCantFreeEms     = 02104; {cannot deallocate EMS handle}

  ecXmsError        = 02110; {error moving memory to/from XMS}
  ecInsufficientXms = 02111; {insufficient XMS memory to allocate}
  ecCantFreeXms     = 02112; {unable to free XMS handle}
  ecNoXms           = 02113; {no XMS memory present}
  ecElSizeNotEven   = 02114; {element size is not even}

  {reported by OPDIR}
  ecDirTooBig       = 02200; {too many files to fit into directory buffer}
  ecPathListEmpty   = 02201; {can't save path list because it's empty} {!!.30}

  {reported by OPFIELD, OPSELECT, OPENTRY}
  ecFieldRequired   = 07000; {field is required}
  ecBadFormat       = 07001; {bad format (number/date/time, etc.}
  ecOutOfRange      = 07002; {value entered is out of range}
  ecBlanksInField   = 07003; {field contains blanks}
  ecPartialEntry    = 07004; {field contains a partial entry}
  ecBadCharacter    = 07005; {field contains an illegal character}

  {reported by OPMEMO, OPEDITOR, OPBROWSE}
  ecStringNotFound  = 07100; {search string not found}
  ecNotToScreen     = 07101; {cannot write output to screen}
  ecInvalidNumber   = 07102; {invalid number entered}

  {reported by OPMEMO, OPEDITOR}
  ecBufferFull      = 07110; {edit buffer is full}
  ecLineTooLong     = 07111; {line too long, CRLF inserted}
  ecTooManyLines    = 07112; {max line limit would be exceeded}
  ecOverLineLimit   = 07113; {max line limit already exceeded}
  ecFileTruncated   = 07114; {file truncated}
  ecFileTooLarge    = 07115; {file too large to fit in buffer}
  ecNoBlock         = 07116; {block not marked or hidden}
  ecNoMarker        = 07117; {marker not set}  {!!.13}

  {reported by OPCALC}
  ecDivideByZero    = 07200; {divide overflow}
  ecUndefinedNum    = 07201; {error returned by 80x87 coprocessor}

  {reported by OPDIR}
  ecNoPickSelection = 07300; {no pick selection made}

{$IFDEF UseStreams}
  {reported by OPROOT.OpLibrary}
  ecDuplicateName   = 07400; {name already in use}
  ecEntryNotFound   = 07401; {OpLibrary entry not found}
{$ENDIF}

  {reported by OPROOT.StringSet}
  ecDupString       = 08000; {string is already in string set}

{$IFDEF UseStreams}
  {reported by OPROOT.IdStream}
  ecIdNotRegistered = 08010; {user object not registered with stream}
  ecIdRegisteredDup = 08011; {attempt to register duplicate object id}
  ecPtrNotRegistered= 08012; {user pointer not registered with stream} {!!.03}
{$ENDIF}

  {reported by OPLARRAY}
  ecElSizeIsZero    = 08050; {element size is zero}
  ecIncorrectSize   = 08051; {incorrect dimensions for disk file}
  ecRowOutOfRange   = 08052; {specified row is out of range}
  ecColOutOfRange   = 08053; {specified column is out of range}
  ecNotLArrayFile   = 08054; {specified file is not a OpLarray file}
  ecElementTooBig   = 08055; {Element size is too big}
  ecBadDimensions   = 08056; {an invalid dimension was specified}
  ecLessThanOnePage = 08057; {less than one page in RAM}
  ecFlushError      = 08058; {error flushing large array file}

  {reported by OPWINDOW and others}
  ecWinCoordsBad    = 08100; {bad coordinates specified for a window}
  ecWinNotActive    = 08101; {window must be active for this operation}
  ecWinNotCurrent   = 08102; {window must be current for this operation}
  ecWinIsActive     = 08103; {window must be inactive for this operation}
  ecWinInaccessible = 08104; {stacked window not accessible}
  ecWinIsZoomed     = 08105; {zoomed window cannot be zoomed again}
  ecWinBadIndex     = 08106; {invalid header or hot spot index}
  ecWinNotSizeable  = 08107; {window must be sizeable for this operation}
  ecNoProcessor     = 08108; {no command processor is available}
  ecWinIsChild      = 08109; {operation is not valid on a child window} {!!.01}

  {reported by OPPICK}
  ecNoPickItems     = 08200; {attempt to pick from empty list}
  ecBadPickOrient   = 08201; {invalid pick orientation}
  ecBadPickHandler  = 08202; {invalid pick command handler}

  {reported by OPHELP}
  ecHelpInvalid     = 08220; {invalid help file format}
  ecNoHelpForTopic  = 08221; {no help for specified topic}
  ecNoBoundHelp     = 08222; {pointer for bound-in help not registered}

  {reported by OPSELECT, OPENTRY}
  ecTooManyFields   = 08300; {too many fields in a selector/entry screen}
  ecBadCoordinates  = 08301; {bad coordinates for a field}
  ecNotScrollable   = 08302; {virtual screen not allocated}
  ecNoFields        = 08303; {selector/entry screen has no fields}
  ecFieldNotFound   = 08305; {attempt to position cursor on non-existent field}
  ecWinFieldError   = 08306; {can't add window field to ScrollingEntryScreen} {!!.01}
  ecBadChildError   = 08307; {child window already has parent or is active}

  {reported by OPPRINT}
  ecInvalidPrinter  = 08400; {specified Printer type is invalid}
  ecPosOutOfRange   = 08401; {printer position out of range}
  ecNoPositionPrim  = 08402; {no printer position routine specified}
  ecFontStkOverflow = 08403; {stack overflow in font stack}
  ecFontStackError  = 08404; {stack underflow in font stack}
  ecByteStkOverflow = 08405; {stack overflow in a byte stack}
  ecNoPrnDriver     = 08406; {no BasePrinterPtr specified for Printer}

  {reported by OPFORM}
  ecNoFieldsOnPage  = 08420; {page to be printed has no active fields}
  ecNoBlockDefined  = 08421; {attempt to add field when no block defined}
  ecNoSuchField     = 08422; {FieldID represents non-existent field}
  ecDiagNotAllowed  = 08423; {diagonal lines not allowed}

  {reported by various units}
  ecNilPointer      = 08500; {nil pointer where there shouldn't be one}
  ecBadParam        = 08501; {bad parameter to a procedure}

  mcLineNumber      = 09000; {prompt for line number}
  mcOptionString    = 09001; {prompt for search options}
  mcBlockWrite      = 09002; {prompt for file to write marked block to}
  mcBlockRead       = 09003; {prompt for file to read in as marked block}
  mcNewFile         = 09004; {prompt for new file name}
  mcRightMargin     = 09005; {prompt for a right margin}
  mcIndentLevel     = 09006; {prompt for an indent level}
  mcTabSize         = 09007; {prompt for a tab size}
  mcFileModified    = 09008; {"file modified--save it?" prompt}
  mcSaveAs          = 09009; {new name for a file}
  mcSearchFor       = 09010; {prompt for a search string}
  mcReplaceWith     = 09011; {prompt for a replacement string}
  mcReplaceYesNo    = 09012; {prompt to confirm a replacement}

  {--------- generic error messages ---------}
const
  NullMsgLen : Byte = 0;
var
  emNullError : string absolute NullMsgLen;
const
  emUnknownError : string[5] = 'Error';
const
  emInsufficientMemory : string[19] = 'Insufficient memory';
const
  emNoMoreFiles : string[17] = 'No matching files';
const
  emReadError : string[18] = 'Error reading file';
const
  emWriteError : string[21] = 'Error writing to file';
const
  emOpenError : string[19] = 'Unable to open file';
const
  emFileNotFound : string[14] = 'File not found';
const
  emPathNotFound : string[14] = 'Path not found';
const
  emCloseError : string[18] = 'Error closing file';
const
  emBufferFull : string[19] = 'Edit buffer is full';
const
  emLineTooLong : string[39] = 'Line too long, carriage return inserted';
const
  emTooManyLines : string[41] = 'Limit on number of lines has been reached';
const
  emOverLineLimit : string[42] = 'Limit on number of lines has been exceeded';
const
  emFileTooLarge : string[22] = 'File too large to edit';
const
  emFileTruncated : string[14] = 'File truncated';
const
  emFileIsEmpty : string[13] = 'File is empty';
const
  emFileModified : string[23] = 'File modified. Save it?';
const
  emInvalidNumber : string[14] = 'Invalid number';
const
  emStringNotFound : string[23] = 'Search string not found';
const
  emNotToScreen : string[27] = 'Cannot write file to screen';
const
  emNewFile : string[10] = 'New file: ';
const
  emBlockWrite : string[18] = 'File to write to: ';
const
  emBlockRead : string[14] = 'File to read: ';
const
  emSearchFor : string[12] = 'Search for: ';
const
  emReplaceWith : string[14] = 'Replace with: ';
const
  emOptionString : string[9] = 'Options: ';
const
  emReplaceYesNo : string[8] = 'Replace?';
const
  emLineNumber : string[13] = 'Line number: ';
const
  emTabSize : string[10] = 'Tab size: ';
const
  emIndentLevel : string[19] = 'Indentation level: ';
const
  emRightMargin : string[14] = 'Right margin: ';
const
  emNoBlock : string[16] = 'No block defined';
const                                          {!!.13}
  emNoMarker : string[14] = 'Marker not set';  {!!.13}
const
  emBadCharacter : string[25] = 'Illegal character entered';
const
  emBlanksInField : string[29] = 'Blank spaces not allowed here';
const
  emPartialEntry : string[32] = 'Partial entries not allowed here';
const
  emOutOfRange : string[24] = 'Value not in valid range';
const
  emFieldRequired : string[18] = 'Field is required';
const
  emInvalidDate : string[12] = 'Invalid date';
const
  emInvalidTime : string[12] = 'Invalid time';
const
  emMacro : string[8] = ' Macro: ';
const
  emInsOver : array[Boolean] of string[10] = (' Overtype ', '  Insert  ');
const
  emComLit : array[Boolean] of string[9] = (' Command ', ' Literal ');
const
  emMacedInstructions : string[71] =
    ' '^[^X^Y^Z' to move, '^Q'Ù to accept, <Esc> to cancel, <ScrollLock> for literal ';
const
  emNoPickItems : string[16] = 'No items to pick';
const
  emDirTooBig : string[14] = 'Too many files';
const
  emInvalidDrive : string[13] = 'Invalid drive';
const
  emDriveNotReady : string[15] = 'Drive not ready'; {!!.01}
const
  emRowOutOfRange : string[16] = 'Row out of range';
const
  emColOutOfRange : string[19] = 'Column out of range';
const
  emNotLArrayFile : string[20] = 'Not an OPLARRAY file';
const
  emElementTooBig : string[15] = 'Element too big';
const
  emBadDimensions : string[17] = 'Invalid Dimension';
const
  emLessThanOnePage : string[25] = 'Less than one page in RAM';
const
  emFlushError : string[31] = 'Error flushing large array file';
const
  emInsufficientEMS : string[33] = 'Insufficient amount of EMS memory';
const
  emEmsAllocation : string[20] = 'EMS allocation error';
const
  emNoEms : string[16] = 'No EMS available';
const
  emEmsPageMapping : string[22] = 'Error mapping EMS page';
const
  emCantFreeEms : string[27] = 'Can''t deallocate EMS handle';
const
  emElSizeIsZero : string[20] = 'Element size if zero';
const
  emInvalidPrinter : string[23] = 'Printer type is invalid';
const
  emPosOutOfRange : string[29] = 'Printer position out of range';
const
  emNoPositionPrim : string[31] = 'No position primitive specified';
const
  emFontStkOverflow : string[28] = 'Stack overflow in Font Stack';
const
  emFontStackError : string[16] = 'Font Stack error';
const
  emByteStkOverflow : string[19] = 'Byte Stack overflow';
const
  emNoPrnDriver : string[24] = 'No BasePrinter installed';
const
  emNoFieldsOnPage : string[17] = 'No fields on page';
const
  emNoBlockDefined : string[16] = 'No block defined';
const
  emNoSuchField : string[13] = 'No such field';
const
  emDiagNotAllowed : string[26] = 'Diagonal lines not allowed';

const
  {--------- unit codes ---------}

  ucNone            = 00; {no unit}
  ucEdit            = 01; {OPEDIT}
  ucPick            = 02; {OPPICK}
  ucDir             = 03; {OPDIR}
  ucMenu            = 04; {OPMENU}
  ucSelect          = 05; {OPSELECT}
  ucEntry           = 06; {OPENTRY}
  ucMemo            = 07; {OPMEMO}
  ucEditor          = 08; {OPEDITOR}
  ucBrowse          = 09; {OPBROWSE}
  ucLArray          = 10; {OPLARRAY}
  ucMacEd           = 11; {OPMACED}
  ucHelp            = 12; {OPHELP}
  ucSEdit           = 13; {OPSEDIT}

  {codes for "unofficial" units}
  ucCalc            = 25; {OPCALC}
  ucQkRef           = 26; {OPQKREF}
  ucCal             = 27; {OPCAL}
  ucDialog          = 28; {OPDIALOG} {!!.03}
  {... codes up to 99 are reserved for TurboPower}
  ucReservedForTP   = 99;

{$IFDEF UseStreams}

  {--------- object type codes (for streams) ---------}
  otPointerStack         = 01;
  otSingleList           = 02;
  otDoubleList           = 03;
  otCircularList         = 04;
  otBitSet               = 05;
  otStringDict           = 06;
  otStringSet            = 07;
  otScreenRect           = 08;
  otHeaderNode           = 09;
  otShadowNode           = 10;
  otHotNode              = 11;
  otScrollBar            = 12;
  otFrame                = 13;
  otRawWindow            = 14;
  otWindowStack          = 15;
  otStackWindow          = 16;
  otCommandWindow        = 17;
  otVirtScreen           = 18;
  otLoadableColorSet     = 19;
  otPickList             = 20;
  otDirList              = 21;
  otPathList             = 22;
  otItemNode             = 23;
  otItemList             = 24;
  otSubMenu              = 25;
  otMenu                 = 26;
  otCalendar             = 27;
  otQkRefChart           = 28;
  otCalculator           = 29;
  otLineEditor           = 30;
  otBrowser              = 31;
  otMemo                 = 32;
  otMemoFile             = 33;
  otTextEditor           = 34;
  otAbstractSelector     = 35;
  otSelector             = 36;
  otScrollingSelector    = 37;
  otEntryScreen          = 38;
  otScrollingEntryScreen = 39;
  otTextField            = 40;
  otSelectField          = 41;
  otEntryField           = 42;
  otCommandProcessor     = 43;
  otPackedWindow         = 44;
  otDirEntry             = 45;
  otStringArray          = 46;
  otStaticQueue          = 47;
  otAbstractHelpWindow   = 48;
  otPagedHelpWindow      = 49;
  otScrollingHelpWindow  = 50;
  otCustomEntryScreen    = 51;    {for MAKESCRN}
  otLargeBitSet          = 52;
  otStackWindowListNode  = 53;    {!!.01}
  otTextLineField        = 54;    {!!.03}
  otTextBoxField         = 55;    {!!.03}
  otDragProcessor        = 56;    {!!.03}

  otBasePrinter          = 60;
  otFlexiblePrinter      = 61;
  otBiosPrinter          = 62;
  otDosPrinter           = 63;
  otAppendDosPrinter     = 71;                             {!!.20}
  otPrintMode            = 64;
  otPrinter              = 65;
  otPrintField           = 66;
  otPrintBlock           = 67;
  otPrintPage            = 68;
  otForm                 = 69;
  otReport               = 70;

  otArrayEField          = 100;
  otBcdEField            = 101;
  otBooleanEField        = 102;
  otByteEField           = 103;
  otCharEField           = 104;
  otChoiceEField         = 105;
  otCompEField           = 106;
  otDateEField           = 107;
  otDateStEField         = 108;
  otDoubleEField         = 109;
  otExtendedEField       = 110;
  otIntegerEField        = 111;
  otLongIntEField        = 112;
  otMultiLineField       = 113;
  otNestedEField         = 114;
  otPickEField           = 115;
  otRealEField           = 116;
  otShortIntEField       = 117;
  otSingleEField         = 118;
  otStringEField         = 119;
  otTimeEField           = 120;
  otWordEField           = 121;
  otYesNoEField          = 122;
  otWindowEField         = 123;    {!!.01}
  otSmallIntEField       = 124;    {!!.OS2}
  otSmallWordEField      = 125;    {!!.OS2}

  otStringPField         = 150;
  otCharPField           = 151;
  otWordPField           = 152;
  otBooleanPField        = 153;
  otYesNoPField          = 154;
  otLongIntPField        = 155;
  otIntegerPField        = 156;
  otBytePField           = 157;
  otShortIntPField       = 158;
  otRealPField           = 159;
  otBCDPField            = 160;
  otExtendedPField       = 161;
  otDoublePField         = 162;
  otSinglePField         = 163;
  otCompPField           = 164;
  otDatePField           = 165;
  otTimePField           = 166;
  otLinePField           = 167;
  otBoxPField            = 168;
  otShadedPField         = 169;

  otControl              = 200; {!!.03}
  otPushButton           = 201; {!!.03}
  otCluster              = 202; {!!.03}
  otClusterItem          = 203; {!!.03}
  otRadioButtons         = 204; {!!.03}
  otCheckBoxes           = 205; {!!.03}

  otDialogBox            = 210; {!!.03}
  otWindowControl        = 211; {!!.03}

  {Note: FBROWSE  uses otFBrowser   = 999}
  {      OPSPREAD uses otSpreadList = 998}
  {      OOFILER  uses otFileblock  = 997} {!!.30}

  {... codes up to 999 are reserved for TurboPower}
  otReservedForTP        = 999;

  {--------- object version codes (for streams) ---------}
  vePointerStack         = 00;
  veSingleList           = 00;
  veDoubleList           = 00;
  veCircularList         = 00;
  veBitSet               = 00;
  veStringDict           = 00;
  veStringSet            = 00;
  veScreenRect           = 00;
  veHeaderNode           = 00;
  veShadowNode           = 00;
  veHotNode              = 00;
  veScrollBar            = 00;
  veFrame                = 00;
  veRawWindow            = 00;
  veWindowStack          = 00;
  veStackWindow          = 00;
  veCommandWindow        = 00;
  veVirtScreen           = 00;
  veLoadableColorSet     = 00;
  vePickList             = 01;  {!!.13}
  veDirList              = 01;  {!!.03}
  vePathList             = 00;
  veItemNode             = 00;
  veItemList             = 00;
  veSubMenu              = 00;
  veMenu                 = 00;
  veCalendar             = 00;
  veQkRefChart           = 00;
  veCalculator           = 00;
  veLineEditor           = 00;
  veBrowser              = 00;
  veMemo                 = 00;
  veMemoFile             = 00;
  veTextEditor           = 00;
  veAbstractSelector     = 00;
  veSelector             = 00;
  veScrollingSelector    = 00;
  veEntryScreen          = 00;
  veScrollingEntryScreen = 00;
  veTextField            = 00;
  veSelectField          = 00;
  veEntryField           = 00;
  veCommandProcessor     = 00;
  vePackedWindow         = 00;
  veDirEntry             = 00;
  veStringArray          = 00;
  veStaticQueue          = 00;
  veAbstractHelpWindow   = 00;
  vePagedHelpWindow      = 00;
  veScrollingHelpWindow  = 00;
  veCustomEntryScreen    = 00;
  veLargeBitSet          = 00;
  veStackWindowListNode  = 00; {!!.01}
  veTextLineField        = 00; {!!.03}
  veTextBoxField         = 00; {!!.03}
  veDragProcessor        = 00; {!!.03}

  veBasePrinter          = 00;
  veFlexiblePrinter      = 00;
  veBiosPrinter          = 00;
  veDosPrinter           = 00;
  veAppendDosPrinter     = 00;                       {!!.20}
  vePrintMode            = 00;
  vePrinter              = 00;
  vePrintField           = 00;
  vePrintBlock           = 00;
  vePrintPage            = 00;
  veForm                 = 00;
  veReport               = 00;

  veArrayEField          = 00;
  veBcdEField            = 00;
  veBooleanEField        = 00;
  veByteEField           = 00;
  veCharEField           = 00;
  veChoiceEField         = 00;
  veCompEField           = 00;
  veDateEField           = 00;
  veDateStEField         = 00;
  veDoubleEField         = 00;
  veExtendedEField       = 00;
  veIntegerEField        = 00;
  veLongIntEField        = 00;
  veMultiLineField       = 00;
  veNestedEField         = 00;
  vePickEField           = 00;
  veRealEField           = 00;
  veShortIntEField       = 00;
  veSingleEField         = 00;
  veStringEField         = 00;
  veTimeEField           = 00;
  veWordEField           = 00;
  veYesNoEField          = 00;
  veWindowEField         = 00;    {!!.01}
  veSmallIntEField       = 00;    {!!.OS2}
  veSmallWordEField      = 00;    {!!.OS2}

  veStringPField         = 00;
  veCharPField           = 00;
  veWordPField           = 00;
  veBooleanPField        = 00;
  veYesNoPField          = 00;
  veLongIntPField        = 00;
  veIntegerPField        = 00;
  veBytePField           = 00;
  veShortIntPField       = 00;
  veRealPField           = 00;
  veBCDPField            = 00;
  veExtendedPField       = 00;
  veDoublePField         = 00;
  veSinglePField         = 00;
  veCompPField           = 00;
  veDatePField           = 00;
  veTimePField           = 00;
  veLinePField           = 00;
  veBoxPField            = 00;
  veShadedPField         = 00;

  veControl              = 00; {!!.03}
  vePushButton           = 00; {!!.03}
  veCluster              = 00; {!!.03}
  veClusterItem          = 00; {!!.03}
  veRadioButtons         = 00; {!!.03}
  veCheckBoxes           = 00; {!!.03}

  veDialogBox            = 01; {!!.20}
  veWindowControl        = 00; {!!.03}

  {--------- pointer codes (for streams) ----------}
  ptEntryUserRec         = $FFFF; {used by OPENTRY}
  ptFormUserRec          = $FFFF; {used by OPFORM}

  ptNil                  = 0000; {nil pointer}

  ptEmptyOpenCloseProc   = 0001; {default non-exploding windows}
  ptExplodeOpenProc      = 0002; {for opening exploding windows}
  ptExplodeCloseProc     = 0003; {for closing exploding windows}
  ptDefaultStackProc     = 0004; {default routine called when window is stacked}
  ptDefaultErrorProc     = 0005; {default routine called when error occurs}

  ptPickVertical         = 0011; {pick orientation routines}
  ptPickHorizontal       = 0012;
  ptPickSnaking          = 0013;
  ptPickNoSearch         = 0014; {default pick alpha search function}
  ptPickStringSearch     = 0015; {other pick search routines}
  ptPickAltStringSearch  = 0016;
  ptPickCharSearch       = 0017;
  ptPickCharExit         = 0018;
  ptPickAnyCharExit      = 0019;
  ptNoPickString         = 0020; {default pick string function}
  ptPickNoMoveAction     = 0021; {default pick move procedure}
  ptPickCommands         = 0022; {default command processor}
  ptPickSingleChoice     = 0023; {single choice command handler}
  ptPickMultipleChoice   = 0024; {multiple choice command handler}

  ptDirSortNone          = 0030; {default directory sort function}
  ptDirSortName          = 0031; {other directory sort orders}
  ptDirSortDirName       = 0032;
  ptDirSortSize          = 0033;
  ptDirSortTime          = 0034;

  ptDirNameFormat        = 0035; {default directory format function}
  ptDirNameSizeKFormat   = 0036; {other directory formats}
  ptDirNameSizeFormat    = 0037;
  ptDirNameTimeFormat    = 0038;
  ptDirNameSizeKTimeFormat = 0039;
  ptDirNameSizeTimeFormat  = 0040;
  ptDirAllFormat         = 0041;

  ptDirRejectNone        = 0042; {default directory reject function}
  ptDirRejectExtensions  = 0043; {other reject functions}
  ptDirRejectFiles       = 0044;

  ptDirStringProc        = 0045; {single pick string procedure}
  ptDirMultiProc         = 0046; {multi pick string procedure}

  ptSortNameDirDrive     = 0047; {new sort routine} {!!.03}

  ptFullDrawProc         = 0060; {full path pick string procedure}
  ptLineDrawProc         = 0061; {line draw path pick string procedure}

  ptMenuNoCustomization  = 0070; {default menu customization procedure}
  ptMenuNoMoveAction     = 0071; {default menu move procedure}
  ptMenuNoCurrItemAction = 0072; {default current item procedure}
  ptMenuCommands         = 0073; {default command processor}

  ptCalendarPick         = 0080; {calendar pick string procedure}
  ptCalCommands          = 0081; {default command processor}
  ptQkRefCommands        = 0082; {default command processor}
  ptCalcCommands         = 0083; {default command processor}

  ptReadKeyWord          = 0085;
  ptKeyPressed           = 0086;
  ptReadKeyOrButton      = 0087;
  ptKeyOrButtonPressed   = 0088;

  ptEditCommands         = 0090; {default command processor}

  ptBrowseCommands       = 0100; {default command processor}

  ptMemoCommands         = 0110; {default command processor}

  ptEditorCommands       = 0120; {default command processor}

  ptSelectCommands       = 0130; {default command processor}

  ptDialogCommands       = 0135; {!!.03}

  ptEntryCommands        = 0140; {default command processor}

  ptStringEditor         = 0150; {field editors}
  ptSimpleStringEditor   = 0151;
  ptCharEditor           = 0152;
  ptSimpleCharEditor     = 0153;
  ptNumberEditor         = 0154;
  ptPickEditor           = 0155;

  ptDrawString           = 0160; {field draw routines}
  ptSimpleDrawString     = 0161;

  {field conversion routines}
  ptStringConversion        = 0170;
  ptSimpleStringConversion  = 0171;
  ptCharConversion          = 0172;
  ptSimpleCharConversion    = 0173;
  ptBooleanConversion       = 0174;
  ptSimpleBooleanConversion = 0175;
  ptYesNoConversion         = 0176;
  ptSimpleYesNoConversion   = 0177;
  ptLongConversion          = 0178;
  ptSimpleLongConversion    = 0179;
  ptWordConversion          = 0180;
  ptSimpleWordConversion    = 0181;
  ptIntConversion           = 0182;
  ptSimpleIntConversion     = 0183;
  ptByteConversion          = 0184;
  ptSimpleByteConversion    = 0185;
  ptShortConversion         = 0186;
  ptSimpleShortConversion   = 0187;
  ptRealConversion          = 0188;
  ptSimpleRealConversion    = 0189;
  ptBcdConversion           = 0190;
  ptSimpleBcdConversion     = 0191;
  ptExtConversion           = 0192;
  ptSimpleExtConversion     = 0193;
  ptDblConversion           = 0194;
  ptSimpleDblConversion     = 0195;
  ptSglConversion           = 0196;
  ptSimpleSglConversion     = 0197;
  ptCompConversion          = 0198;
  ptSimpleCompConversion    = 0199;

  ptNullConversion          = 0200; {!!.03}

  ptSmallWordConversion      = 0201; {!!OS2}
  ptSimpleSmallWordConversion= 0202; {!!OS2}
  ptSmallIntConversion       = 0203; {!!OS2}
  ptSimpleSmallIntConversion = 0204; {!!OS2}

  {field validation routines}
  ptValidateChar         = 0210;
  ptSimpleValidateChar   = 0211;
  ptValidateLong         = 0212;
  ptSimpleValidateLong   = 0213;
  ptValidateReal         = 0214;
  ptSimpleValidateReal   = 0215;
  ptValidateExt          = 0216;
  ptSimpleValidateExt    = 0217;
  ptValidateBcd          = 0218;
  ptSimpleValidateBcd    = 0219;

  ptNestedFieldVar       = 0230;

  ptHelpNoMoveAction     = 0240;
  ptHelpNoNestedAction   = 0241;
  ptHelpCommands         = 0242;
  ptHelpNoCopyFunc       = 0243;

  ptPutPrim              = 0250;
  ptOpenPrim             = 0251;
  ptClosePrim            = 0252;
  ptReadyPrim            = 0253;
  ptResetPrim            = 0254;
  ptFlushPrim            = 0255;
  ptXlatPrim             = 0256;
  ptLJPosition           = 0260;
  ptLJGraphics           = 0261;
  ptLJLine               = 0262;
  ptLJRule               = 0263;

  ptStringPConv          = 0270;
  ptCharPConv            = 0271;
  ptWordPConv            = 0272;
  ptBoolPConv            = 0273;
  ptYesNoPConv           = 0274;
  ptLongPConv            = 0275;
  ptIntPConv             = 0276;
  ptBytePConv            = 0277;
  ptShortPConv           = 0278;

  ptRealPConv            = 0279;
  ptBCDPConv             = 0280;
  ptExtPConv             = 0281;
  ptDblPConv             = 0282;
  ptSglPConv             = 0283;
  ptCompPConv            = 0284;
  ptDatePConv            = 0285;
  ptTimePConv            = 0286;
  ptDummyPConv           = 0287;

  {Note: OPSPREAD uses ptPickSpread = 998}

  {... codes up to 999 are reserved for TurboPower}
  ptReservedForTP        = 0999;

{$ENDIF}

function HeapFunc(Size : Word) : Integer;
  {-Return nil pointer if insufficient memory}

{===================================================================}

implementation

  {!!.21} {Heap error function moved here so that OPROOT can be overlaid}
  function HeapFunc(Size : Word) : Integer;
    {-Return nil pointer if insufficient memory}
  begin
    {$IFDEF Heap6}       {!!.10}
    if Size = 0 then     {!!.10}
      HeapFunc := 2      {!!.10}
    else                 {!!.10}
    {$ENDIF}             {!!.10}
      HeapFunc := 1;
  end;

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
