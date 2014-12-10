
{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPFORM.PAS 1.30                     *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit OpForm;
  {-Implements high level printed form and report support}

interface

uses
  Dos,
  OpConst,   {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpDos,
  {$IFDEF UseDates}
  OpDate,
  {$ENDIF}
  {$IFDEF UseBcd}
  OpBcd,
  {$ENDIF}
  OpAbsFld,
  OpPrnLow,
  OpDevice,
  OpPrint;

const
  {option flags for PrintField object}
  pfHexadecimal       = $00040000; {display as hexadecimal number}    {!!.01}
  pfSuppressZero      = $00020000; {suppress zeros}
  pfRightJustify      = $00010000; {right justify field}
  pfInhibitAttrOn     = $00008000; {inhibit calls to turn the PrintMode on}
  pfInhibitAttrOff    = $00004000; {inhibit calls to turn the PrintMode off}
  pfInhibitString     = $00002000; {inhibit the writing of the field's string}
  pfInhibitPos        = $00001000; {inhibit the head positioning for a field}
  pfHidden            = $00000200; {a hidden field (won't be printed)}

  pfPattern           = $00000010; {indicates a pattern fill}
  pfSpecial           = $00000008; {calls the special proc}
  pfGraphics          = $00000004; {reserved for future use}
  pfLineDraw          = $00000002; {calls the Line draw primitive}
  pfDirective         = $00000001; {indicates field is a print directive}

  {used internally}
  pfHorizontal        = $02000000; {line field is horizontal}        {!!.20}
  pfExpandableLine    = $04000000; {field is a line field}           {!!.20}
  pfTextField         = $08000000; {field is a text field}           {!!.11}
  pfRealVar           = $10000000; {field is a real variable of some sort}
  pfDeallocVarPtr     = $20000000; {object must deallocate the VarPtr}
  pfDeallocPicture    = $40000000; {object must deallocate the picture mask}
  pfClonedField       = $80000000; {object created by clone method}  {!!.03}

  {special Option flags used by directives}
  pfDirectiveOn       = pfDirective + pfInhibitPos + pfInhibitString +
                        pfInhibitAttrOff;
  pfDirectiveOff      = pfDirective + pfInhibitPos + pfInhibitString +
                        pfInhibitAttrOn;

  {flag for internal use by PrintBlock}
  pbDeallocUserRec    = $01;  {object must deallocate user record}

  {Option Flags used internally by PrintPage}
  ppHeaderActive      = $0001; {a header has been defined}
  ppFooterActive      = $0002; {a footer has been defined}
  ppNoSortFields      = $0004; {do not sort fields (if positionable printer)}

  {Option flags used internally by PrintedForm}                      {!!.21}
  fDisposeOfPrinter   = $0001; {dispose - if loaded from stream}     {!!.21}

  {Option to allow the form to be stored without the printer object} {!!.22}
  fDontStorePrinter   = $0002; {Don't store printer in stream}       {!!.22}

  fidExpandedLine     = $FFFE; {ID of linefields that have been expanded}{!!.20}
  ReservedFieldID     = $FFFF;

  {constants for box type}
  frTL                = 0;
  frBL                = 1;
  frTR                = 2;
  frBR                = 3;
  frTT                = 4;
  frBB                = 5;
  frLL                = 6;
  frRR                = 7;

type
  {pointer types for Fields, Blocks, Pages, Forms and Reports}
  PrintFieldPtr       = ^PrintField;
  PrintBlockPtr       = ^PrintBlock;
  PrintPagePtr        = ^PrintPage;
  PrintedFormPtr      = ^PrintedForm;
  ReportPtr           = ^Report;

  {definition of a PrintField conversion function, called for each field}
  PrintConversionFunc = function (PFP : PrintFieldPtr) : Boolean;

  {definition of the User Block function, called for each block}
  UserBlockFunc       = function (PB : PrintBlockPtr) : Boolean;

  {definition of the user page function, called for each page}
  UserPageFunc        = function (PP : PrintPagePtr) : Boolean;

  {definition of the User form function, called before and after form is
   printed}
  UserFormFunc        = function (PreForm : Boolean; F : PrintedFormPtr) : Boolean;

  {procedure definitions used by VisitAll routines}
  PrintFieldProc      = procedure (pfPtr : PrintFieldPtr;
                                   var D; FPtr : PrintedFormPtr);
  PrintBlockProc      = procedure (pbPtr : PrintBlockPtr;
                                   var D; FPtr : PrintedFormPtr);
  FormProc            = procedure (fPtr : PrintedFormPtr;
                                   var D; RPtr : ReportPtr);
  PrintPageProc       = procedure (ppPtr : PrintPagePtr;
                                   var D; FPtr : PrintedFormPtr);

  {stuff related to boxes}
  BoxCharType         = frTL..frRR;
  BoxCharSet          = Array[BoxCharType] of Char;

  {.Z+}
  {special object type used to allow secondary list of PrintFields}
  FieldListPtr  = ^FieldListNode;
  FieldListNode =
    object(SingleListNode)
      flField : PrintFieldPtr;
      constructor Init(PF : PrintFieldPtr);
        {-Called when Node is created}
      destructor Done; Virtual;
    end;
  {.Z-}

  {the Base print field}
  PrintField =
    object(AbstractField)
      {...data for the field...}
      pfID           : Word;                 {user assigned ID}
      pfRow          : Dimension;            {row for this field}
      pfCol          : Dimension;            {col for this field}
      pfWidth        : Dimension;            {width of this field}
      pfHeight       : Dimension;            {height of this field}
      pfAttr         : Byte;                 {Print Mode Attr}
      pfDPlaces      : Byte;                 {num decimal places}
      pfMaxLen       : Byte;                 {max lenth (in Chars) of field}
      pfPadChar      : Char;                 {the character used to pad field}
      pfDataSize     : Word;                 {size of actual data type}
      pfOptions      : LongInt;              {option flags}
      pfLastError    : Word;                 {holds the last error}
      pfPicture      : StringPtr;            {number of decimal places}
      pfVarPtr       : Pointer;              {untyped pointer to field's data}
      pfString       : StringPtr;            {ptr to printable string}
      pfConvert      : PrintConversionFunc;  {string conversion routine}
      {...methods...}
      {.Z+}
      constructor Init(ID : Word;
                       Picture : string;
                       Row, Col : Dimension; Attr : Byte;
                       FieldWidth, FieldHeight : Dimension;
                       MaxLen   : Word;
                       DataSize : Word;
                       DecimalPlaces : Byte;
                       CP : PrintConversionFunc;
                       var PrintVar;
                       PadChar : Char;
                       OptionFlags : LongInt);
        {-Initialize a print field}
      destructor Done; Virtual;
        {-Deallocate a print field}
      function pfGetID : Word;
        {-Returns the field's ID}
      function pfGetLastError : Word;
        {-Return Last Error code and reset to zero}
      {.Z-}
      procedure pfOptionsOn(OptionFlags : LongInt);
        {-Activate multiple options}
      procedure pfOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple options}
      function pfOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Determine if multiple options are on}
      function Convert : Boolean; Virtual;
        {-Convert from native type to printable string}
      procedure Print(PrinterP : PrinterPtr); Virtual;
        {-Print the print field}
      procedure SetConversion(ConvertFunc : PrintConversionFunc);
        {-Sets the conversion routine}
      function GetFieldAttr : Byte;
        {-Return the print attribute for this Field}
      procedure GetFieldPos(var Row, Col : Dimension);
        {-Return the row and col for this field}
      procedure SetFieldPos(Row, Col : Dimension);
        {-Sets the row and col for this field}
      procedure pfSetPicture(NewPicture : String);
        {-Sets the picture mask}
      {.Z+}
    {$IFDEF UseStreams}
      {...stream support...}
      constructor Load(var S : IdStream);
        {-Load an entry field from a stream}
      procedure Store(var S : IdStream);
        {-Store an entry field in a stream}
    {$ENDIF}
      function pfGetPicture  : String;
        {-Returns the current picture mask string (without padding)}
      function GetFieldString : String;
        {-Return the string associated with this field}
      {...for internal use...}
      function pfFieldIsHidden : Boolean;
      function afMaxLen : Byte; virtual;
      function afPicture : String; virtual;
      function afDPlaces : Byte; virtual;
      function afNoLiterals : Boolean; virtual;
      function afNumeric : Boolean; virtual;
      function afHexadecimal : Boolean; virtual;
      function afRightJustified : Boolean; virtual;
      function afIsReal : Boolean; Virtual;
      function pfLong2Str(L : LongInt; W : Byte) : String;
      procedure pfPadField;
      procedure pfAdjustBase(OldBaseRow,OldBaseCol,
                             NewBaseRow,NewBaseCol : Dimension); Virtual;
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
      procedure pfZeroOut;
      function pfVarPtrIsStatic : Boolean; Virtual;
      constructor InitZero;
      {.Z-}
    end;

  {.Z+}
  {Print Field for strings}
  StringPrintFieldPtr = ^StringPrintField;
  StringPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintSt : String;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  {Print field for Characters}
  CharPrintFieldPtr  = ^CharPrintField;
  CharPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row,Col : Dimension;
                       Attr    : Byte;
                       Width   : Byte;
                       var PrintCh : Char;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  {Print field for Words}
  WordPrintFieldPtr = ^WordPrintField;
  WordPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row,Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintWord : Word;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;

  BooleanPrintFieldPtr = ^BooleanPrintField;
  BooleanPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintBool : Boolean;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  YesNoPrintFieldPtr = ^YesNoPrintField;
  YesNoPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintYesNo : Boolean;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  LongPrintFieldPtr = ^LongPrintField;
  LongPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintLong : LongInt;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  IntPrintFieldPtr = ^IntPrintField;
  IntPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintInt : Integer;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  BytePrintFieldPtr = ^BytePrintField;
  BytePrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintByte : Byte;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  ShortPrintFieldPtr = ^ShortPrintField;
  ShortPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintShort : ShortInt;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  RealPrintFieldPtr = ^RealPrintField;
  RealPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       DecimalPlaces : Byte;
                       var PrintReal : Real;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  {$IFDEF UseBCD}
  BCDPrintFieldPtr = ^BCDPrintField;
  BCDPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       DecimalPlaces : Byte;
                       var PrintBcd : Bcd;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  {$ENDIF}

  {$IFOPT N+}
  ExtPrintFieldPtr = ^ExtPrintField;
  ExtPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       DecimalPlaces : Byte;
                       var PrintExt : Extended;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  DblPrintFieldPtr = ^DblPrintField;
  DblPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       DecimalPlaces : Byte;
                       var PrintDbl : Double;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  SglPrintFieldPtr = ^SglPrintField;
  SglPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       DecimalPlaces : Byte;
                       var PrintSgl : Single;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  CompPrintFieldPtr = ^CompPrintField;
  CompPrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       DecimalPlaces : Byte;
                       var PrintComp : Comp;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;

  {$ENDIF}

  {$IFDEF UseDates}
  DatePrintFieldPtr = ^DatePrintField;
  DatePrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintDate : Date;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;

  TimePrintFieldPtr = ^TimePrintField;
  TimePrintField =
    object(PrintField)
      constructor Init(FieldID : Word;
                       Picture : String;
                       Row, Col : Dimension;
                       Attr : Byte;
                       Width : Byte;
                       var PrintTime : Time;
                       PadCh : Char;
                       OptionFlags : LongInt);
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
    end;
  {$ENDIF}

  {PrintField derived object for Line fields}
  LineFieldPtr         = ^LineField;
  LineFieldProc        = procedure (Line : LineFieldPtr);
  LinePrintProc        = procedure (P : PrinterPtr;
                                    Line : LineFieldPtr);
  LineField =
    object(PrintField)
      lfRow1         : Dimension; {actual top left row}
      lfCol1         : Dimension; {actual top left col}
      lfRow2         : Dimension; {Height}
      lfCol2         : Dimension; {width}
      lfPrint        : LinePrintProc;
      constructor Init(FieldID : Word;
                       Row1, Col1,
                       Row2,Col2,
                       LineThickness : Dimension;
                       Attr : Byte;
                       PrintPrim : LinePrintProc);
        {-Create a new LineField}
      destructor Done; Virtual;
        {-Destroy a line field}
      procedure Print(PrinterP : PrinterPtr); Virtual;
        {-Print a line field}
      procedure GetLineDim(var Row1,Col1,Row2,Col2 : Dimension);
        {-Return the dimensions for this line}
      procedure SetLineDim(Row1,Col1,Row2,Col2 : Dimension);
        {-Sets the dimensions for this line}

    {$IFDEF UseStreams}
      {...stream support...}
      constructor Load(var S : IdStream);
        {-Load an entry field from a stream}
      procedure Store(var S : IdStream);
        {-Store an entry field in a stream}
    {$ENDIF}
      {...methods for internal use...}
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
      function pfVarPtrIsStatic : Boolean; Virtual;
      constructor InitZero;
      procedure lfZeroOut;
      procedure pfAdjustBase(OldBaseRow,OldBaseCol,
                             NewBaseRow,NewBaseCol : Dimension); Virtual;
      function lfExpand(Block : PrintBlockPtr;
                        P : PrinterPtr) : Boolean; Virtual;         {!!.20}
    end;

  {PrintField derived type for Shaded Areas}
  ShadedFieldPtr = ^ShadedField;
  ShadedField =
    object(LineField)
      constructor Init(FieldID : Word;
                       Row, Col,
                       Width,Height : Dimension;
                       Intensity : Byte;
                       PrintPrim : LinePrintProc);
        {-Create a shaded area field}
      destructor Done; Virtual;
        {-Destroy a shaded area field}
      procedure Print(PrinterP : PrinterPtr); Virtual;
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
      function lfExpand(Block : PrintBlockPtr;
                        P : PrinterPtr) : Boolean; Virtual;         {!!.20}
    end;

  BoxFieldPtr = ^BoxField;
  BoxField =
    object(LineField)
      bfVThickness    : Dimension;
      bfHThickness    : Dimension;
      bfBoxChars      : BoxCharSet;
      constructor Init(FieldID : Word;
                       Row, Col,
                       Width,Height : Dimension;
                       HLineThickness, VLineThickness : Dimension;
                       BoxChars : BoxCharSet;
                       Attr : Byte);
        {-Create a box field}
      constructor InitZero;
      procedure bfZeroOut;
      procedure Print(PrinterP : PrinterPtr); Virtual;
      procedure pfClone(CloneFrom : PrintFieldPtr;
                        var CloneTo : PrintFieldPtr); Virtual;
      function lfExpand(Block : PrintBlockPtr;
                        P : PrinterPtr) : Boolean; Virtual;         {!!.20}
    {$IFDEF UseStreams}
      {...stream support...}
      constructor Load(var S : IdStream);
        {-Load an entry field from a stream}
      procedure Store(var S : IdStream);
        {-Store an entry field in a stream}
    {$ENDIF}
    end;

  {singly linked list of Fields in a block}
  PrintBlock =
    object(SingleListNode)
      pbBaseRow         : Dimension;     {base row for this block}
      pbBaseCol         : Dimension;     {base col for this block}
      pbLastError       : Word;          {last error in this block}
      pbFlags           : Byte;
      pbUserRecSize     : Word;
      pbUserRecPtr      : Pointer;

      pbFields          : DoubleList;    {fields for this block}
      pbCurrentField    : PrintFieldPtr; {the current field}
      pbUserBlockFunc   : UserBlockFunc; {Block function for this block}

      constructor Init(BaseRow, BaseCol : Dimension);
        {-Create a Print Block}
      destructor Done; Virtual;
        {-Destroy a print block}
      procedure pbAddField(pf : PrintFieldPtr);
        {-Add a field to this block}
      procedure pbSetUserBlockFunc(pbFunc : UserBlockFunc);
        {-Set the User Block Function for this Block}
      function pbCallUserBlockFunc : Boolean; Virtual;
        {-Call the user block funtion and return True if block is to be
          processed}
      function pbGetLastError : Word;
        {-Get the last error code and reset error code to zero}
    {$IFDEF UseStreams}
      {...stream support...}
      constructor Load(var S : IdStream);
        {-Load a PrintBlock from a stream}
      procedure Store(var S : IdStream);
        {-Store a PrintBlock in a stream}
      procedure pbSetUserRecord(var UserRec; UserRecSize : Word);
        {-Set the address and size of the user record}
      function pbGetUserRecord : Pointer;
        {-Return a pointer to the user record}
      function pbGetUserRecordSize : Word;
        {-Return the size of the user record}
    {$ENDIF}
      {...methods for internal use...}
      constructor pbInitFrom(var B : PrintBlock);
      constructor pbCopy(var B : PrintBlock;
                         NewBaseRow, NewBaseCol : Dimension);
      procedure pbZeroOut;
      procedure pbClone(CloneFrom : PrintBlockPtr; var CloneTo : PrintBlockPtr);
    {$IFDEF UseStreams}
      function pbRestoreUserVars(var IdS : IdStream;
                                 Last : PrintFieldPtr) : Boolean;
      function pbRestoreOneUserVar(var IdS : IdStream;
                                   PFP : PrintFieldPtr) : Boolean;
      function pbFixUserVars(var IdS : IdStream;
                             var PFP : PrintFieldPtr) : Boolean;
      function pbFixOneUserVar(var IdS : IdStream;
                               PFP : PrintFieldPtr) : Boolean;
    {$ENDIF}
      procedure pbAddTextField(pFieldID : Word; Prompt : String;
                               pRow, pCol : Dimension;
                               pAttr : Byte;
                               pWidth : Byte);              {!!.20}
      procedure pbAddLine(pOptions              : Word;
                          FieldID               : Word;
                          Row, Col              : Dimension;
                          Len,LineThickness     : Dimension;
                          Horizontal            : Boolean;
                          LineChar              : Char;
                          Attr                  : Byte);       {!!.20}
        {-Adds a line field}
    end;
  {a page of output, which is a list of blocks on the page}
  PrintPage =
    object(SingleListNode)
      ppFlags           : Word;
      ppLastError       : Word;
      ppBlocks          : SingleList;
      ppPrePage         : UserPageFunc;
      ppHeader          : PrintFieldPtr;
      ppFooter          : PrintFieldPtr;
      ppCurrentField    : PrintFieldPtr;
      ppCurrentBlock    : PrintBlockPtr;
      ppFields          : SingleList;

      constructor Init;
      destructor Done; Virtual;
      procedure Print(PrinterP : PrinterPtr); Virtual;
        {-Print the print page}

      procedure ppAddBlock(BaseRow, BaseCol : Dimension;
                           BlockFunc : UserBlockFunc);
        {-Add a print block to this page definition}
      procedure ppAddField(pf : PrintFieldPtr);
        {-Add a print field to the current block of this page}

      procedure SetPrePageFunc(ppFunc : UserPageFunc);
        {-set the function that gets called prior to the printing of the page}
      function ppGetLastError : Word;
        {-Return Last Error and reset}

      procedure SetHeader(HeaderStr : String; Row, Col : Dimension;
                          HAttr : Byte);
        {-Define a header}
      procedure SetHeaderFunc(Picture : String;
                              HeadFunc : PrintConversionFunc);
        {-Set the HeadFunc function pointer}
      function GetHeader : String;
        {-Returns the header string}

      procedure SetFooter(FooterStr : String; Row, Col : Dimension;
                          FAttr : Byte);
        {-Define a Footer}
      procedure SetFooterFunc(Picture : String;
                              FootFunc : PrintConversionFunc);
        {-Set the HeadFunc function pointer}
      function GetFooter : String;
        {-Returns the Footer string}
    {$IFDEF UseStreams}
      {...stream support...}
      constructor Load(var S : IdStream);
        {-Load a PrintPage from a stream}
      procedure Store(var S : IdStream);
        {-Store a PrintPage in a stream}
    {$ENDIF}
      {...methods for internal use...}
      function ppCallUserPageFunc : Boolean;
      function ppLowestField : PrintFieldPtr;
      function ppGetCurrentBlock : PrintBlockPtr;
      function ppGetCurrentField : PrintFieldPtr;
      function ppLowerField(A,B : PrintFieldPtr) : Boolean;
      procedure ppAddBlockPrim(Block : PrintBlockPtr);
      procedure ppZeroOut;
      constructor ppCopy(var P : PrintPage);
      constructor ppInitFrom(var P : PrintPage);
    end;
  {.Z-}

  {a PrintedForm, which is a list of pages}
  PrintedForm =
    object(SingleListNode)
      {page description}
      fOptions       : Word;         {PrintedForm option flags}
      fTopMargin     : Dimension;    {top margin}
      fBottomMargin  : Dimension;    {bottom margin}
      fLeftMargin    : Dimension;    {left margin}
      fRightMargin   : Dimension;    {right margin}
      fUserData      : Pointer;      {4 bytes of user data}
      fLastError     : Word;         {the last error for this PrintedForm}
      fTabSize       : Byte;         {size of Tab}
      fNumCopies     : Byte;         {number of copies}
      fID            : Byte;         {this PrintedForm's ID}
      fPromptPadCh   : Char;         {default prompt pad character}
      fFieldPadCh    : Char;         {default field pad character}
      fPromptOpts    : LongInt;      {default option flags for prompt fields}
      fFieldOpts     : LongInt;      {default option flags for data fields}
      fPrinter       : PrinterPtr;   {the printer driver}
      fPages         : SingleList;   {list of pages associated with this form}
      fCurrentPage   : PrintPagePtr; {the current page}

      {...procedure/function pointers...}
      fPrePostForm   : UserFormFunc;   {pre/post form function}
      fErrorHandler  : PrintErrorFunc; {printer error handler}

      {...methods...}
      constructor Init(FormID : Byte; PrinterP : PrinterPtr);
        {-Initialize a PrintedForm}
      constructor InitCustom(FormID : Byte;
                             TopMargin,BottomMargin,
                             LeftMargin,RightMargin : Dimension;
                             PrinterP : PrinterPtr);
        {-Initialize a PrintedForm with custom options}
      destructor Done; Virtual;
        {-Dispose of a PrintedForm}
      procedure Process; Virtual;
        {-Print the Form}
      function fPrinterPtr : PrinterPtr;
        {-Returns the PrinterPtr for this form}
      function fGetLastError : Word;
        {-Returns the last error and resets to zero}
      procedure NewBlock(BaseRow, BaseCol : Dimension;
                         BlockFunc : UserBlockFunc);
        {-Starts a new block}
      procedure DupCurrentBlock(NewRow, NewCol : Dimension);
        {-Duplicates the current block at the new block coordinates}
      procedure DupCurrentPage;
        {-Duplicates the current page}
      procedure PromptOptionsOn(OptionFlags : LongInt);
        {-Turn options for the prompts on}
      procedure FieldOptionsOn(OptionFlags : LongInt);
        {-Turn options for the fields on}
      procedure PromptOptionsOff(OptionFlags : LongInt);
        {-Turn options for the prompt off}
      procedure FieldOptionsOff(OptionFlags : LongInt);
        {-Turn options for the fields off}
      function PromptOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Determine whether options for the prompts are on}
      function FieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Determine whether options for the fields are on}


      procedure AddTextField(pFieldID : Word; Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte);
        {-Adds a text string field (no user variable)}
      procedure AddStringField(pFieldID : Word;
                               Prompt : String;
                               pRow, pCol : Dimension;
                               pAttr : Byte;
                               pWidth : Byte;
                               fFieldID : Word;
                               Picture : String;
                               fRow, fCol : Dimension;
                               fAttr : Byte;
                               fWidth : Byte;
                               var PrintSt : String);
        {-Adds prompt and string field}
      procedure AddCharField(pFieldID : Word;
                             Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte;
                             fFieldID : Word;
                             Picture : String;
                             fRow, fCol : Dimension;
                             fAttr      : Byte;
                             fWidth     : Byte;
                             var PrintChar : Char);
        {-Adds prompt and character field}
      procedure AddBooleanField(pFieldID : Word;
                                Prompt : String;
                                pRow, pCol : Dimension;
                                pAttr : Byte;
                                pWidth : Byte;
                                fFieldID : Word;
                                Picture  : String;
                                fRow, fCol : Dimension;
                                fAttr : Byte;
                                fWidth : Byte;
                                var PrintBool : Boolean);
        {-Adds prompt and Boolean field}
      procedure AddYesNoField(pFieldID : Word;
                              Prompt : String;
                              pRow, pCol : Dimension;
                              pAttr : Byte;
                              pWidth : Byte;
                              fFieldID : Word;
                              Picture : String;
                              fRow, fCol : Dimension;
                              fAttr : Byte;
                              fWidth : Byte;
                              var PrintYesNo : Boolean);
        {-Adds prompt and Yes/No field}
      procedure AddLongField(pFieldID : Word;
                             Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte;
                             fFieldID : Word;
                             Picture : String;
                             fRow, fCol : Dimension;
                             fAttr : Byte;
                             fWidth : Byte;
                             var PrintLong : LongInt);
        {-Adds prompt and LongInt field}
      procedure AddWordField(pFieldID : Word;
                             Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte;
                             fFieldID : Word;
                             Picture : String;
                             fRow, fCol : Dimension;
                             fAttr : Byte;
                             fWidth : Byte;
                             var PrintWord : Word);
        {-Adds prompt and Word field}
      procedure AddIntField(pFieldID : Word;
                            Prompt : String;
                            pRow, pCol : Dimension;
                            pAttr : Byte;
                            pWidth : Byte;
                            fFieldID : Word;
                            Picture : String;
                            fRow, fCol : Dimension;
                            fAttr : Byte;
                            fWidth : Byte;
                            var PrintInt : Integer);
        {-Adds prompt and Integer field}
      procedure AddByteField(pFieldID : Word;
                             Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte;
                             fFieldID : Word;
                             Picture : String;
                             fRow, fCol : Dimension;
                             fAttr : Byte;
                             fWidth : Byte;
                             var PrintByte : Byte);
        {-Adds prompt and Byte field}
      procedure AddShortField(pFieldID : Word;
                              Prompt : String;
                              pRow, pCol : Dimension;
                              pAttr : Byte;
                              pWidth : Byte;
                              fFieldID : Word;
                              Picture : String;
                              fRow, fCol : Dimension;
                              fAttr : Byte;
                              fWidth : Byte;
                              var PrintShort : ShortInt);
        {-Adds prompt and ShortInt field}
      procedure AddRealField(pFieldID : Word;
                             Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte;
                             fFieldID : Word;
                             Picture : String;
                             fRow, fCol : Dimension;
                             fAttr : Byte;
                             fDPlaces : Byte;
                             fWidth : Byte;
                             var PrintReal : Real);
        {-Adds prompt and Real field}
      {$IFDEF UseBCD}
      procedure AddBCDField(pFieldID : Word;
                            Prompt : String;
                            pRow, pCol : Dimension;
                            pAttr : Byte;
                            pWidth : Byte;
                            fFieldID : Word;
                            Picture : String;
                            fRow, fCol : Dimension;
                            fAttr : Byte;
                            fDPlaces : Byte;
                            fWidth : Byte;
                            var PrintBCD : BCD);
        {-Adds prompt and BCD field}
      {$ENDIF}
      {$IFOPT N+}
      procedure AddExtField(pFieldID : Word;
                            Prompt : String;
                            pRow, pCol : Dimension;
                            pAttr : Byte;
                            pWidth : Byte;
                            fFieldID : Word;
                            Picture : String;
                            fRow, fCol : Dimension;
                            fAttr : Byte;
                            fDPlaces : Byte;
                            fWidth : Byte;
                            var PrintExt : Extended);
        {-Adds prompt and Extended field}
      procedure AddDblField(pFieldID : Word;
                            Prompt : String;
                            pRow, pCol : Dimension;
                            pAttr : Byte;
                            pWidth : Byte;
                            fFieldID : Word;
                            Picture : String;
                            fRow, fCol : Dimension;
                            fAttr : Byte;
                            fDPlaces : Byte;
                            fWidth : Byte;
                            var PrintDbl : Double);
        {-Adds prompt and Double field}
      procedure AddSglField(pFieldID : Word;
                            Prompt : String;
                            pRow, pCol : Dimension;
                            pAttr : Byte;
                            pWidth : Byte;
                            fFieldID : Word;
                            Picture : String;
                            fRow, fCol : Dimension;
                            fAttr : Byte;
                            fDPlaces : Byte;
                            fWidth : Byte;
                            var PrintSgl : Single);
        {-Adds prompt and Single field}
      procedure AddCompField(pFieldID : Word;
                             Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte;
                             fFieldID : Word;
                             Picture : String;
                             fRow, fCol : Dimension;
                             fAttr : Byte;
                             fWidth : Byte;
                             var PrintComp : Comp);
        {-Adds prompt and Comp field}
      {$ENDIF}

      {$IFDEF UseDates}
      procedure AddDateField(pFieldID : Word;
                             Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte;
                             fFieldID : Word;
                             Picture : String;
                             fRow, fCol : Dimension;
                             fAttr : Byte;
                             fWidth : Byte;
                             var PrintDate : Date);
        {-Adds prompt and Date field}
      procedure AddTimeField(pFieldID : Word;
                             Prompt : String;
                             pRow, pCol : Dimension;
                             pAttr : Byte;
                             pWidth : Byte;
                             fFieldID : Word;
                             Picture : String;
                             fRow, fCol : Dimension;
                             fAttr : Byte;
                             fWidth : Byte;
                             var PrintTime : Time);
        {-Adds prompt and Time field}
      {$ENDIF}
      procedure AddDirective(FieldID : Word;
                             Row,Col : Dimension;
                             On      : Boolean;
                             Attr    : Byte);
        {-Adds a print directive}
      procedure AddLine(FieldID               : Word;
                        Row, Col              : Dimension;
                        Len,LineThickness     : Dimension;
                        Horizontal            : Boolean;
                        LineChar              : Char;
                        Attr                  : Byte);
        {-Adds a line field}
      procedure AddBox(FieldID : Word;
                       Row, Col       : Dimension;
                       Width, Height  : Dimension;
                       HLineThickness : Dimension;
                       VLineThickness : Dimension;
                       BoxChars : BoxCharSet;
                       Attr : Byte);
        {-Adds a box field}
      procedure AddShaded(FieldID : Word;
                          Row,Col,
                          Width,Height : Dimension;
                          Intensity : Byte);
        {-Adds a shaded box field}
      procedure NewPage;
        {-Start a new page}
      procedure AddPage(Page : PrintPagePtr);
        {-Add an existing page ptr to this form}
      procedure SetHeader(HeaderStr : String; Row, Col : Dimension;
                          HAttr : Byte);
        {-Define a header for the current page}
      procedure SetHeaderFunc(Picture : String;
                              HeadFunc : PrintConversionFunc);
        {-Set the HeadFunc function pointer for the current page}
      function GetHeader : String;
        {-Returns the header string for the current page}

      procedure SetFooter(FooterStr : String; Row, Col : Dimension;
                          FAttr : Byte);
        {-Define a Footer for the current page}
      procedure SetFooterFunc(Picture : String;
                              FootFunc : PrintConversionFunc);
        {-Set the HeadFunc function pointer for the current page}
      function GetFooter : String;
        {-Returns the Footer string for the current page}

      procedure SetErrorHandler(ErrorFunc : PrintErrorFunc);
        {-Sets the user error handler}
      procedure AddPrintMode(Name,OnEscSeq,OffEscSeq : String;
                             Width, Height : Dimension;
                             ID : Byte; OptionFlags : Word);
        {-Add a Print Mode to the Printer's linked list of modes}
      function FindField(FieldID : Word;
                         var Page : PrintPagePtr;
                         var Block : PrintBlockPtr) : PrintFieldPtr;
        {-Finds the first field with the specified FieldID}
      function FindNextField(Field  : PrintFieldPtr;
                             var Page : PrintPagePtr;
                             var Block : PrintBlockPtr) : PrintFieldPtr;
        {-Finds the next field with the same FieldID as Field}

      procedure ChangeConversion(FieldID : Word;
                                 ConvertFunc : PrintConversionFunc);
        {-Change field's conversion routine}
      procedure SetUserData(P : Pointer);
        {-Sets the user data field}
      function GetUserData : Pointer;
        {-Returns the user data field}

      procedure SetPrePageFunc(ppFunc : UserPageFunc);
        {-Sets the function that gets called before the current page is printed}
      procedure SetPrePostFormFunc(ppFunc : UserFormFunc);
        {-Sets the function that gets called before and after printing the form}

      procedure ExpandLineFields; Virtual;              {!!.20}
        {-Expand line fields to printer specific format}

    {$IFDEF UseStreams}
      {...stream support...}
      constructor Load(var S : IdStream);
        {-Load a PrintedForm from a stream}
      procedure Store(var S : IdStream);
        {-Store a PrintedForm in a stream}
      procedure SetUserRecord(var UserRec; UserRecSize : Word);
        {-Set the address and size of the user record for current block}
      function GetUserRecord : Pointer;
        {-Return a pointer to the user record for current block}
      function GetUserRecordSize : Word;
        {-Return the size of the user record for current block}
    {$ENDIF}
      {.Z+}
      procedure SetPromptPadChar(PadChar : Char);
        {-Sets the prompt Pad Character}
      function GetPromptPadChar : Char;
        {-Returns the prompt pad character}
      procedure SetFieldPadChar(PadChar : Char);
        {-Sets the Field Pad Character}
      function GetFieldPadChar : Char;
        {-Returns the Field pad character}

      {...methods for internal use...}
      function fPrintErrorHandler(Recoverable : Boolean) : Boolean; Virtual;
        {-Routine to handle errors}
      function PrePostFormFunc(PreForm : Boolean) : Boolean;
      procedure VisitAllPrintFields(PfProc : PrintFieldProc; var D);
      procedure VisitAllPrintBlocks(PbProc : PrintBlockProc; var D);
      procedure VisitAllPrintPages(PpProc : PrintPageProc; var D);
      procedure fSetPrnRegisteredType(RegProc : PrnRegProc);
        {-Sets the underlying Printer object's registered hardware type}
      procedure AddPrintFieldPrim(pf : PrintFieldPtr);
        {-Add a print field to the current block}
      procedure fZeroPtrs;
      procedure SetMargins(TopMargin,BottomMargin,
                           LeftMargin,RightMargin  : Dimension);
      procedure fAdjustCoordinates(var Row,Col : Dimension); Virtual;
        {-Adjust a set of block/page relative coordinates to absolute
          printer coordinates.  Assumes relative to current block on
          current page.}

      {added !!.30}
      procedure fAdjustCoordinatesDeci(var Row, Col : Dimension); virtual;
        {-Adjust a set of block/page relative coordinates to absolute
          printer coordinates.  The inputs are in decipoints }

      procedure fRelativeFromAbs(AbsRow, AbsCol : Dimension;
                                 var Row, Col   : Dimension); Virtual;
        {-Return a set of block/page relative coordinates from absolute
          printer coordinates.  Returns Row and Col relative to current
          block on current page.}
      {.Z-}
    end;

  {a report, which is a list of forms}
  Report =
    object(Root)
      rLastError     : Word;         {the last error for this report}
      rForms         : SingleList;   {a list of forms in this report}
      rCurrentForm   : PrintedFormPtr;      {the current PrintedForm}

      constructor Init;
        {-Create a report}
      destructor Done; Virtual;
        {-Destroy a report}
      function rGetLastError : Word;
        {-Get last error code and clear error code}
      procedure Process; Virtual;
        {-Print the print field}
      procedure AddForm(fPtr : PrintedFormPtr);
        {-Add a PrintedForm to Report by pointer}
      function FindForm(FormID : Byte) : PrintedFormPtr;
        {-Return a pointer to the PrintedForm with the specified ID}
    {$IFDEF UseStreams}
      {...stream support...}
      constructor Load(var S : IdStream);
        {-Load a Report from a stream}
      procedure Store(var S : IdStream);
        {-Store a Report in a stream}
    {$ENDIF}
      {.Z+}
      procedure VisitAllForms(fProc : FormProc; var D);
      {.Z-}
    end;

const
  DefPadCh            : Char = ' ';
  DefNumCopies        : Byte = 1;
  DefTabSize          : Byte = 8;
  NullPicture         : String[1] = '';
  SingleLineBox       : BoxCharSet = ('Ú','À','¿','Ù','Ä','Ä','³','³');
  DoubleLineBox       : BoxCharSet = ('É','È','»','¼','Í','Í','º','º');

  DefTopMargin        : Dimension = 0.0;
  DefBottomMargin     : Dimension = 0.0;
  DefLeftMargin       : Dimension = 0.0;
  DefRightMargin      : Dimension = 0.0;


var
  NoBlockFunc         : UserBlockFunc absolute OpPrnLow.NilPointer;


{.Z+}
{conversion routines}

function DummyConversion(PFP : PrintFieldPtr) : Boolean;
  {-Dummy conversion function that always returns True}

function PrintStringConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for strings}

function PrintWordConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Words}

function PrintCharConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Characters}

function PrintLongConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Words}

function PrintBoolConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for booleans}

function PrintYesNoConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for yes/no fields}

function PrintIntConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Integer fields}

function PrintByteConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Byte fields}

function PrintShortConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for ShortInt fields}

function PrintRealConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Real fields}

{$IFDEF UseBcd}
function PrintBCDConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for BCD fields}
{$ENDIF}

{$IFOPT N+}
function PrintExtConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Extended fields}

function PrintDblConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Double fields}

function PrintSglConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Single fields}

function PrintCompConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion routine for Comp fields}
{$ENDIF}

{$IFDEF UseDates}
function PrintDateConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion function for Date Fields}

function PrintTimeConvert(PFP : PrintFieldPtr) : Boolean;
  {-Conversion function for Time Fields}
{$ENDIF}
{.Z-}

{$IFDEF UseStreams}

{Stream support}
procedure PrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing PrintFields}
procedure StringPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing StringPrintFields}
procedure CharPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing CharPrintFields}
procedure WordPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing WordPrintFields}
procedure BooleanPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing BooleanPrintFields}
procedure YesNoPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing YesNoPrintFields}
procedure LongPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing LongPrintFields}
procedure IntPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing IntPrintFields}
procedure BytePrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing BytePrintFields}
procedure ShortPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing ShortPrintFields}
procedure RealPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing RealPrintFields}
{$IFDEF UseBCD}
procedure BcdPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing BcdPrintFields}
{$ENDIF}
{$IFOPT N+}
procedure ExtPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing ExtPrintFields}
procedure DblPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing DblPrintFields}
procedure SglPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing SglPrintFields}
procedure CompPrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing CompPrintFields}
{$ENDIF}
{$IFDEF UseDates}
procedure DatePrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing DatePrintFields}
procedure TimePrintFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing TimePrintFields}
{$ENDIF}
procedure LineFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing LineFields}
procedure ShadedFieldStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing ShadedFields}
procedure PrintBlockStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing PrintBlocks}
procedure PrintPageStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing PrintPages}
procedure FormStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing Forms}
procedure ReportStream(SPtr : IdStreamPtr);
  {-Registers types needed for streams containing Reports}
procedure AllBasicFieldsStream(SPtr : IdStreamPtr);
  {-Registers all Basic field types needed for streams}
{$IFOPT N+}
procedure All8087FieldStream(SPtr : IdStreamPtr);
  {-Registers all 8087 field types needed for streams}
{$ENDIF}
{$IFDEF UseDates}
procedure AllDateFieldsStream(SPtr : IdStreamPtr);
  {-Registers all date and time field types needed for streams}
{$ENDIF}
procedure AllLineFieldsStream(SPtr : IdStreamPtr);
  {-Registers all line, box and shading field types needed for streams}
procedure AllPrintFieldsStream(SPtr : IdStreamPtr);
  {-Registers all field types used by OPFORM}

{$ENDIF}

{Laserjet specific line draw}
procedure LJRule(P : PrinterPtr; Field : LineFieldPtr);
  {-Prints LineField as a Laserjet Rule}

  {==========================================================================}

implementation

{$I OPFORM.IN1}         {implementation of PrintField and PrintPage}
{$I OPFORM.IN2}         {implementation PrintedForm and Report}
{$I OPFORM.IN3}         {conversion and hardware specific routines}

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
