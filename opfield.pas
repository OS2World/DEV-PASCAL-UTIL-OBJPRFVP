{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPFIELD.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1988,1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpField;
  {-Data entry field routines}


interface

uses
  Use32,
  OpConst,      {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  {$IFDEF UseDates}
  OpDate,
  {$ENDIF}
  {$IFDEF UseBcd}
  OpBcd,
  {$ENDIF}
  OpAbsFld,
  OpSelect,     {!!.30}
  OpCmd;

const
  {option codes for entry fields}
  efProtected        = $00000001; {if set, field contents cannot be modified}
  efHidden           = $00000002; {field is hidden}
  efMapCtrls         = $00000004; {map control characters}
  efMultChoice       = $00000008; {a multiple-choice field}
  efInvisible        = $00000010; {if set, field is invisible}
  {^--correspond to flags for select fields}
  efAutoAdvCharBegin = $00000020; {dummy}
  efAutoAdvCharEnd   = $00000040; {advance to next field--character entered}
  efAutoAdvCurBegin  = $00000080; {advance to prev field when beg. of field passed}
  efAutoAdvCurEnd    = $00000100; {advance to next field--cursor key pressed}
  efAutoAdvanceChar  = efAutoAdvCharBegin+efAutoAdvCharEnd;
  efAutoAdvanceCursor= efAutoAdvCurBegin+efAutoAdvCurEnd;
  efAutoAdvanceBegin = efAutoAdvCharBegin+efAutoAdvCurBegin;
  efAutoAdvanceEnd   = efAutoAdvCharEnd+efAutoAdvCurEnd;
  efAutoAdvance      = efAutoAdvanceBegin+efAutoAdvanceEnd;
  efInsertPushes     = $00000200; {if set, inserting can push a character off end}
  efRightJustify     = $00000400; {if set, field is right-justified}
  efPasswordMode     = $00000800; {if set, contents of field are suppressed}
  efCursorToEnd      = $00001000; {put cursor at end of string initially}
  efTrimBlanks       = $00002000; {trim leading/trailing blanks when finished}
  efClearFirstChar   = $00004000; {clear string if first character is ASCII}
  efForceOvertype    = $00008000; {if ForceMode set, selects insert or overtype}
  efForceMode        = $00010000; {force insert or overtype, else use default}
  efClickExit        = $00020000; {produce ccClickExit on double click}
  efBeepOnError      = $00040000; {beep when illegal char entered?}
  efAutoNumLock      = $00080000; {auto-activate/deactivate NumLock--numbers only}
  efParensForMinus   = $00100000; {display negative numbers inside parentheses--numeric fields only}
  efExitAtEdges      = $00200000; {allow exit at edges--pick editor only}
  efRequired         = $00400000; {if set, field cannot be empty}
  efReadOnly         = $00800000; {if set, field cannot be edited} {!!.02}
  {...following options valid for simple editors only...}
  efHouseCursorAtEnd = $01000000; {extra char of width to hold cursor when string full?}
  efForceUpper       = $02000000; {force chars to upper case?}
  efForceLower       = $04000000; {force chars to lower case?}
  {...following option for simple char editor, pick editor only...}
  efAllowEscape      = $08000000; {allow escape?}
  {...following options for simple char editor only...}
  efShowReadChar     = $10000000; {display the character pressed?}
  efHideCursor       = $20000000; {hide cursor?}
  efDefaultAccepted  = $40000000; {allow user to accept default?}
  efMapExtended      = $80000000; {map extended keys?}

  DefEFieldOptions   : LongInt = efInsertPushes+efAutoAdvance+efTrimBlanks+
                                 efMapCtrls+efAllowEscape+efDefaultAccepted+
                                 efShowReadChar;
  BadEFieldOptions   : LongInt = efProtected+efMultChoice;

  {secondary entry field options}
  sefSmartExponents  = $02000000; {select 1.00 behavior for fields using E} {!!.02}
  sefSwitchCommands  = $04000000; {switch command processors--for WindowFields
                                   only} {!!.01}
  sefPadCurrentOnly  = $08000000; {use alternate pad character only when
                                   editing the field}
  sefSuppressZero    = $10000000; {suppress zeroes in conversion routines for
                                   longints, integers, etc.--not reals}
  sefNoFieldMovement = $20000000; {don't allow field movement commands to exit}
  {...following options used by line editors only...}
  sefWindowRelative  = $40000000; {coordinates treated relative to current window?}
  sefInsertByDefault = $80000000; {default to insert mode?}

  DefPadChar         : Char = ' '; {character used to pad strings}
  DefPasswordChar    : Char = ^G;

{.Z-}
type
  RangeType =
    record case Byte of                    {size}
      00 : (rtChar : Char);                 {01}
      01 : (rtByte : Byte);                 {01}
      02 : (rtSht  : ShortInt);             {01}
      03 : (rtInt  : SmallInt);             {02}
      04 : (rtWord : SmallWord);            {02}
      05 : (rt3    : array[1..3] of Byte);  {03} {for type-casting}
      06 : (rtLong : LongInt);              {04}
      07 : (rtSgl  : Single);               {04}
      08 : (rtPtr  : Pointer);              {04}
      09 : (rt5    : array[1..5] of Byte);  {05} {for type-casting}
      10 : (rtReal : Real);                 {06}
      11 : (rt7    : array[1..7] of Byte);  {07} {for type-casting}
      12 : (rtDbl  : Double);               {08}
      13 : (rtComp : Comp);                 {08}
      14 : (rt9    : array[1..9] of Byte);  {09} {for type-casting}
      15 : (rtExt  : Extended);             {10}
      {$IFDEF UseBcd}
      16 : (rtBcd  : BCD);                  {10}
      {$ENDIF}
      17 : (rt10   : array[1..10] of Byte); {10} {for type-casting}
      {$IFDEF UseDates}
      18 : (rtDate : Date);                 {2 or 4}
      19 : (rtTime : Time);                 {04}
      {$ENDIF}
    end;

  EntryFieldPtr = ^EntryField;
  ValidationFunc = function(EFP : EntryFieldPtr; var ErrCode : Word;
                            var ErrorSt : StringPtr) : Boolean;
  ConversionProc = procedure(EFP : EntryFieldPtr; PostEdit : Boolean);
  EditProc = procedure (var Field; Row, Col : Word; FA, CA : Byte;
                        PasswordChar : Char; PosCode : Byte;
                        ReadOnly : Boolean; var CC : Word; var ChWord : Word;
                        var InsertMode : Boolean; ErrorRoutine : ErrorProc;
                        UnitCode : Byte; var CP : CommandProcessor);
  DrawProc = procedure (var Field; St : string;
                        Row, Col : Word; FA, CA, POffset : Byte;
                        PasswordChar : Char; Flags : PictureFlags);
  EntryField =
    object(SelectField)
      efPrompt   : StringPtr;    {points to edit prompt}
      efPicture  : StringPtr;    {points to edit mask}
      efEditSt   : StringPtr;    {points to string to be edited}
      efValidate : ValidationFunc; {pointer to validation routine, or nil}
      efConvert  : ConversionProc; {points to conversion routine}
      efDraw     : DrawProc;     {routine to draw the field}
      efEditor   : EditProc;     {editor for this field}
      efMaxLen   : Byte;         {maximum length of string}
      efPadChar  : Char;         {char used to pad end of string}
      efDPlaces  : Byte;         {max decimal places, if no '.' in Picture}
      efRangeLo  : RangeType;    {valid range for the field}
      efRangeHi  : RangeType;
      efDataSize : SmallWord;    {size of data type}
      efVarPtr   : Pointer;      {points to the actual variable}
      efHOffset  : SmallInt;     {horizontal scrolling offset} {!!.20}
      {... methods ...}
      procedure efOptionsOn(OptionFlags : LongInt);
        {-Activate multiple options}
      procedure efOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple options}
      function efOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified options are on}
      procedure sefOptionsOn(OptionFlags : LongInt);
        {-Activate multiple secondary options}
      procedure sefOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple secondary options}
      function sefOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified secondary options are on}
{.Z+}
      {+++ internal methods +++}
      constructor Init(ID : Word;           var Prompt : string;
                       pRow, pCol : Word;   var Picture : string;
                       fRow, fCol : Word;   fWidth, fHeight : Byte;
                       HlpNdx : Word;       var RangeLo, RangeHi : RangeType;
                       DataSize : Word;     DecimalPlaces : Byte;
                       VF : ValidationFunc; CP : ConversionProc;
                       DP : DrawProc;       EP : EditProc;
                       var EditVar;         PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field}
      constructor InitNPP(ID : Word;           var Prompt : string;
                          pRow, pCol : Word;   var Picture : string;
                          fRow, fCol : Word;   fWidth, fHeight : Byte;  HlpNdx : Word;
                          var RangeLo, RangeHi : RangeType;
                          DataSize : Word;     DecimalPlaces : Byte;
                          var EditVar;         PadChar : Char;
                          Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field (No Procedure Pointers)}
      destructor Done; virtual;
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
    {$ENDIF}
      function Validate(var Code : Word; var SP : StringPtr) : Boolean; virtual;
      procedure Convert(PostEdit : Boolean); virtual;
      procedure Draw(var St : string; Row, Col : Word; FA, CA, POffset : Byte;
                     PasswordChar : Char; var Flags : PictureFlags); virtual;
      procedure Edit(Row, Col : Word;     FA, CA : Byte;
                     PasswordChar : Char; PosCode : Byte;
                     ReadOnly : Boolean;  var CC : Word;
                     var ChWord : Word;   var InsertMode : Boolean;
                     EP : ErrorProc;      UnitCode : Byte;
                     var CP : CommandProcessor); virtual;
      function afMaxLen : Byte; virtual;
      function afWidth : Byte; virtual; {!!.22}
      function afPicture : String; virtual;
      function afDPlaces : Byte; virtual;
      function afNoLiterals : Boolean; virtual;
      function afNumeric : Boolean; virtual;
      function afHexadecimal : Boolean; virtual;
      function afRightJustified : Boolean; virtual;
      function afIsReal : Boolean; virtual;
      procedure efTrimSpaces; virtual;
      function efFieldIsEmpty : Boolean; virtual;
      function efOKtoAdvance(CC : Word) : Boolean; virtual;       {!!.03}
      {$IFDEF UseMouse}                                           {!!.20}
      function efMousePos(Row, Col : Word) : Byte;                {!!.20}
      {$ENDIF}                                                    {!!.20}
      function efStr2Long(S : string; var L : LongInt) : Boolean;
      function efLong2Str(L : LongInt; W : Byte) : string;
      procedure efIncrement; virtual;
      procedure efSetSemiHidden(TF : Boolean);
{.Z-}
    end;

{.Z+}
  StringFieldPtr = ^StringField;
  StringField =
    object(EntryField)
      constructor Init(ID : Word;         var Prompt : string;
                       pRow, pCol : Word; var Picture : string;
                       fRow, fCol : Word; fWidth : Byte;
                       HlpNdx : Word;  var EditSt : String;
                       PadChar : Char;    Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type string}
      constructor InitSim(ID : Word;         var Prompt : string;
                       pRow, pCol : Word; PicChar : Char;
                       fRow, fCol : Word; fWidth, MaxLen : Byte;
                       HlpNdx : Word;  var EditSt : String;
                       PadChar : Char;    Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type string (simple)}
    end;

  ArrayFieldPtr = ^ArrayField;
  ArrayField =
    object(EntryField)
      constructor Init(ID : Word;         var Prompt : string;
                       pRow, pCol : Word; var Picture : string;
                       fRow, fCol : Word; fWidth : Byte;
                       HlpNdx : Word;  var EditVar;
                       PadChar : Char;    Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type array (of char)}
      procedure Convert(PostEdit : Boolean); virtual;
        {-Convert from array of char to edit string or vice versa}
    end;

 CharFieldPtr = ^CharField;
 CharField =
   object(EntryField)
     constructor Init(ID : Word;             var Prompt : string;
                      pRow, pCol : Word;     var Picture : string;
                      fRow, fCol : Word;     HlpNdx : Word;
                      CharLo, CharHi : Char; var EditChar : Char;
                      PadChar : Char;        Options, IFlags : LongInt;
                      var Colors : ColorSet);
      {-Initialize an entry field of type char}
     constructor InitSim(ID : Word;             var Prompt : string;
                      pRow, pCol : Word;     PicChar : Char;
                      fRow, fCol : Word;     HlpNdx : Word;
                      CharLo, CharHi : Char; var EditChar : Char;
                      PadChar : Char;        Options, IFlags : LongInt;
                      var Colors : ColorSet);
      {-Initialize an entry field of type char (simple)}
   end;

  BooleanFieldPtr = ^BooleanField;
  BooleanField =
     object(EntryField)
       constructor Init(ID : Word;              var Prompt : string;
                        pRow, pCol : Word;      var Picture : string;
                        fRow, fCol : Word;      HlpNdx : Word;
                        var EditBool : Boolean; PadChar : Char;
                        Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type boolean}
       constructor InitSim(ID : Word;              var Prompt : string;
                        pRow, pCol : Word;      fRow, fCol : Word;
                        HlpNdx : Word;       var EditBool : Boolean;
                        PadChar : Char;         Options, IFlags : LongInt;
                        var Colors : ColorSet);
        {-Initialize an entry field of type boolean (simple)}
      procedure efIncrement; virtual;
     end;

  YesNoFieldPtr = ^YesNoField;
  YesNoField =
    object(EntryField)
      constructor Init(ID : Word;               var Prompt : string;
                       pRow, pCol : Word;       var Picture : string;
                       fRow, fCol : Word;       HlpNdx : Word;
                       var EditYesNo : Boolean; PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type yes-no}
      constructor InitSim(ID : Word;         var Prompt : string;
                       pRow, pCol : Word; fRow, fCol : Word;
                       HlpNdx : Word;  var EditYesNo : Boolean;
                       PadChar : Char;    Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type yes-no (simple)}
      procedure efIncrement; virtual;
    end;

  LongIntFieldPtr = ^LongIntField;
  LongIntField =
    object(EntryField)
      constructor Init(ID : Word;                var Prompt : string;
                       pRow, pCol : Word;        var Picture : string;
                       fRow, fCol : Word;        HlpNdx : Word;
                       LongLo, LongHi : LongInt; var EditLong : LongInt;
                       PadChar : Char;           Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type longint}
      constructor InitNum(ID : Word;                var Prompt : string;
                       pRow, pCol : Word;        var Picture : string;
                       fRow, fCol : Word;        HlpNdx : Word;
                       LongLo, LongHi : LongInt; var EditLong : LongInt;
                       PadChar : Char;           Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type longint (numeric)}
      constructor InitSim(ID : Word;                var Prompt : string;
                       pRow, pCol : Word;        PicChar : Char;
                       fRow, fCol : Word;        fWidth : Byte;
                       HlpNdx : Word;         LongLo, LongHi : LongInt;
                       var EditLong : LongInt;   PadChar : Char;
                       Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type longint (simple)}
    end;

  WordFieldPtr = ^WordField;
  WordField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       WordLo, WordHi : Word; var EditWord : Word;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type word}
      constructor InitNum(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       WordLo, WordHi : Word; var EditWord : Word;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type word (numeric)}
      constructor InitSim(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     PicChar : Char;
                       fRow, fCol : Word;     fWidth : Byte;
                       HlpNdx : Word;      WordLo, WordHi : Word;
                       var EditWord : Word;   PadChar : Char;
                       Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type word (simple)}
    end;

  SmallWordFieldPtr = ^SmallWordField;
  SmallWordField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       WordLo, WordHi : SmallWord; var EditWord : SmallWord;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type wordSmallWord}
      constructor InitNum(ID : Word;             var Prompt : string;
                          pRow, pCol : Word;     var Picture : string;
                          fRow, fCol : Word;     HlpNdx : Word;
                          WordLo, WordHi : SmallWord; var EditWord : SmallWord;
                          PadChar : Char;        Options, IFlags : LongInt;
                          var Colors : ColorSet);
        {-Initialize an entry field of type SmallWord (numeric)}
      constructor InitSim(ID : Word;             var Prompt : string;
                          pRow, pCol : Word;     PicChar : Char;
                          fRow, fCol : Word;     fWidth : Byte;
                          HlpNdx : Word;      WordLo, WordHi : SmallWord;
                          var EditWord : SmallWord;   PadChar : Char;
                          Options, IFlags : LongInt;
                          var Colors : ColorSet);
        {-Initialize an entry field of type SmallWord (simple)}
    end;

  IntegerFieldPtr = ^IntegerField;
  IntegerField =
    object(EntryField)
      constructor Init(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      var Picture : string;
                       fRow, fCol : Word;      HlpNdx : Word;
                       IntLo, IntHi : Integer; var EditInt : Integer;
                       PadChar : Char;         Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type integer}
      constructor InitNum(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      var Picture : string;
                       fRow, fCol : Word;      HlpNdx : Word;
                       IntLo, IntHi : Integer; var EditInt : Integer;
                       PadChar : Char;         Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type integer (numeric)}
      constructor InitSim(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      PicChar : Char;
                       fRow, fCol : Word;      fWidth : Byte;
                       HlpNdx : Word;       IntLo, IntHi : Integer;
                       var EditInt : Integer;  PadChar : Char;
                       Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type integer (simple)}
    end;

  SmallIntFieldPtr = ^SmallIntField;
  SmallIntField =
    object(EntryField)
      constructor Init(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      var Picture : string;
                       fRow, fCol : Word;      HlpNdx : Word;
                       IntLo, IntHi : SmallInt; var EditInt : SmallInt;
                       PadChar : Char;         Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type SmallInt}
      constructor InitNum(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      var Picture : string;
                       fRow, fCol : Word;      HlpNdx : Word;
                       IntLo, IntHi : SmallInt; var EditInt : SmallInt;
                       PadChar : Char;         Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type SmallInt (numeric)}
      constructor InitSim(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      PicChar : Char;
                       fRow, fCol : Word;      fWidth : Byte;
                       HlpNdx : Word;       IntLo, IntHi : SmallInt;
                       var EditInt : SmallInt;  PadChar : Char;
                       Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type SmallInt (simple)}
    end;

  ByteFieldPtr = ^ByteField;
  ByteField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       ByteLo, ByteHi : Byte; var EditByte : Byte;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type byte}
      constructor InitNum(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       ByteLo, ByteHi : Byte; var EditByte : Byte;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type byte (numeric)}
      constructor InitSim(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     PicChar : Char;
                       fRow, fCol : Word;     fWidth : Byte;
                       HlpNdx : Word;      ByteLo, ByteHi : Byte;
                       var EditByte : Byte;   PadChar : Char;
                       Options, IFlags : LongInt;
                       var Colors : ColorSet);
      {-Initialize an entry field of type byte (simple)}
    end;

  ShortIntFieldPtr = ^ShortIntField;
  ShortIntField =
    object(EntryField)
      constructor Init(ID : Word;                   var Prompt : string;
                       pRow, pCol : Word;           var Picture : string;
                       fRow, fCol : Word;           HlpNdx : Word;
                       ShortLo, ShortHi : ShortInt; var EditShort : ShortInt;
                       PadChar : Char;              Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type shortint}
      constructor InitNum(ID : Word;                   var Prompt : string;
                       pRow, pCol : Word;           var Picture : string;
                       fRow, fCol : Word;           HlpNdx : Word;
                       ShortLo, ShortHi : ShortInt; var EditShort : ShortInt;
                       PadChar : Char;              Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type shortint (numeric)}
      constructor InitSim(ID : Word;                   var Prompt : string;
                       pRow, pCol : Word;           PicChar : Char;
                       fRow, fCol : Word;           fWidth : Byte;
                       HlpNdx : Word;            ShortLo, ShortHi : ShortInt;
                       var EditShort : ShortInt;    PadChar : Char;
                       Options, IFlags : LongInt;   var Colors : ColorSet);
        {-Initialize an entry field of type shortint (simple)}
    end;

  RealFieldPtr = ^RealField;
  RealField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       RealLo, RealHi : Real; Places : Byte;
                       var EditReal : Real;   PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type real}
      constructor InitNum(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       RealLo, RealHi : Real; Places : Byte;
                       var EditReal : Real;   PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type real (numeric)}
      constructor InitSim(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     PicChar : Char;
                       fRow, fCol : Word;     fWidth : Byte;
                       HlpNdx : Word;      RealLo, RealHi : Real;
                       Places : Byte;         var EditReal : Real;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type real (simple)}
    end;

{$IFDEF UseBcd}

  BcdFieldPtr = ^BcdField;
  BcdField =
    object(EntryField)
      constructor Init(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      var Picture : string;
                       fRow, fCol : Word;      HlpNdx : Word;
                       var BcdLo, BcdHi : Bcd; Places : Byte;
                       var EditBcd : Bcd;      PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type BCD}
      constructor InitNum(ID : Word;              var Prompt : string;
                          pRow, pCol : Word;      var Picture : string;
                          fRow, fCol : Word;      HlpNdx : Word;
                          var BcdLo, BcdHi : Bcd; Places : Byte;
                          var EditBcd : Bcd;      PadChar : Char;
                          Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type BCD (numeric)}
      constructor InitSim(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      PicChar : Char;
                       fRow, fCol : Word;      fWidth : Byte;
                       HlpNdx : Word;       var BcdLo, BcdHi : Bcd;
                       Places : Byte;          var EditBcd : Bcd;
                       PadChar : Char;         Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type BCD (simple)}
    end;

{$ENDIF}

{$IFOPT N+}

  ExtendedFieldPtr = ^ExtendedField;
  ExtendedField =
    object(EntryField)
      constructor Init(ID : Word;               var Prompt : string;
                       pRow, pCol : Word;       var Picture : string;
                       fRow, fCol : Word;       HlpNdx : Word;
                       ExtLo, ExtHi : Extended; Places : Byte;
                       var EditExt : Extended;  PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type extended}
      constructor InitNum(ID : Word;               var Prompt : string;
                          pRow, pCol : Word;       var Picture : string;
                          fRow, fCol : Word;       HlpNdx : Word;
                          ExtLo, ExtHi : Extended; Places : Byte;
                          var EditExt : Extended;  PadChar : Char;
                          Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type extended (numeric)}
      constructor InitSim(ID : Word;         var Prompt : string;
                       pRow, pCol : Word;    PicChar : Char;
                       fRow, fCol : Word;    fWidth : Byte;
                       HlpNdx : Word;        ExtLo, ExtHi : Extended;
                       Places : Byte;        var EditExt : Extended;
                       PadChar : Char;       Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type extended (simple)}
    end;

  DoubleFieldPtr = ^DoubleField;
  DoubleField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       DblLo, DblHi : Double; Places : Byte;
                       var EditDbl : Double;  PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type double}
      constructor InitNum(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       DblLo, DblHi : Double; Places : Byte;
                       var EditDbl : Double;  PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type double (numeric)}
      constructor InitSim(ID : Word;      var Prompt : string;
                       pRow, pCol : Word; PicChar : Char;
                       fRow, fCol : Word; fWidth : Byte;
                       HlpNdx : Word;     DblLo, DblHi : Double;
                       Places : Byte;     var EditDbl : Double;
                       PadChar : Char;    Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type double (simple)}
    end;

  SingleFieldPtr = ^SingleField;
  SingleField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       SglLo, SglHi : Single; Places : Byte;
                       var EditSgl : Single;  PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type single}
      constructor InitNum(ID : Word;             var Prompt : string;
                          pRow, pCol : Word;     var Picture : string;
                          fRow, fCol : Word;     HlpNdx : Word;
                          SglLo, SglHi : Single; Places : Byte;
                          var EditSgl : Single;  PadChar : Char;
                          Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize an entry field of type single (numeric)}
      constructor InitSim(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     PicChar : Char;
                       fRow, fCol : Word;     fWidth : Byte;
                       HlpNdx : Word;      SglLo, SglHi : Single;
                       Places : Byte;         var EditSgl : Single;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type single (simple)}
    end;

  CompFieldPtr = ^CompField;
  CompField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       CompLo, CompHi : Comp; var EditComp : Comp;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type comp}
      constructor InitNum(ID : Word;             var Prompt : string;
                          pRow, pCol : Word;     var Picture : string;
                          fRow, fCol : Word;     HlpNdx : Word;
                          CompLo, CompHi : Comp; var EditComp : Comp;
                          PadChar : Char;        Options, IFlags : LongInt;
                          var Colors : ColorSet);
        {-Initialize an entry field of type comp (numeric)}
      constructor InitSim(ID : Word;             var Prompt : string;
                          pRow, pCol : Word;     PicChar : Char;
                          fRow, fCol : Word;     fWidth : Byte;
                          HlpNdx : Word;         CompLo, CompHi : Comp;
                          var EditComp : Comp;   PadChar : Char;
                          Options, IFlags : LongInt;
                          var Colors : ColorSet);
        {-Initialize an entry field of type comp (simple)}
    end;

{$ENDIF}

{$IFDEF UseDates}

  DateFieldPtr = ^DateField;
  DateField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       DateLo, DateHi : Date; var EditDate : Date;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type date}
      function Validate(var Code : Word; var SP : StringPtr) : Boolean; virtual;
        {-Validate the string being edited}
      procedure Convert(PostEdit : Boolean); virtual;
        {-Convert from date to string or vice versa}
    end;

  DateStFieldPtr = ^DateStField;
  DateStField =
    object(EntryField)
      constructor Init(ID : Word;         var Prompt : string;
                       pRow, pCol : Word; var Picture : string;
                       fRow, fCol : Word; HlpNdx : Word;
                       var EditDateSt : DateString;
                       PadChar : Char;    Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type date string}
      function Validate(var Code : Word; var SP : StringPtr) : Boolean; virtual;
        {-Validate the string being edited}
      procedure Convert(PostEdit : Boolean); virtual;
        {-Convert from date string to edit string or vice versa}
    end;

  TimeFieldPtr = ^TimeField;
  TimeField =
    object(EntryField)
      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     var Picture : string;
                       fRow, fCol : Word;     HlpNdx : Word;
                       TimeLo, TimeHi : Time; var EditTime : Time;
                       PadChar : Char;        Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize an entry field of type time}
      function Validate(var Code : Word; var SP : StringPtr) : Boolean; virtual;
        {-Validate the string being edited}
      procedure Convert(PostEdit : Boolean); virtual;
        {-Convert from time to string or vice versa}
    end;

{$ENDIF}
{.Z-}

  IncChoiceProc = procedure(var Value; FieldID : Word; Factor : Integer; var St : string);

{.Z+}
  ChoiceFieldPtr = ^ChoiceField;
  ChoiceField =
    object(EntryField)
      cfIncChoice : IncChoiceProc;
      constructor Init(ID : Word;           var Prompt : string;
                       pRow, pCol : Word;   var Picture : string;
                       fRow, fCol : Word;   HlpNdx : Word;
                       DataSize : Word;     Increment : IncChoiceProc;
                       var EditVar;         PadChar : Char;
                       Options, IFlags : LongInt;
                       var Colors : ColorSet);
        {-Initialize a multiple-choice entry field}
      procedure Convert(PostEdit : Boolean); virtual;
        {-Conversion routine for multiple choice fields}
      procedure Edit(Row, Col : Word;     FA, CA : Byte;
                     PasswordChar : Char; PosCode : Byte;
                     ReadOnly : Boolean;  var CC : Word;
                     var ChWord : Word;   var InsertMode : Boolean;
                     EP : ErrorProc;      UnitCode : Byte;
                     var CP : CommandProcessor); virtual;
        {-Edit a multiple choice field}
      {...}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a multiple choice entry field from a stream}
      procedure Store(var S : IdStream);
        {-Store a multiple choice entry field in a stream}
    {$ENDIF}
      procedure efIncrement; virtual;
    end;

  NestedFieldPtr = ^NestedField;
  NestedField =
    object(EntryField)
      constructor Init(ID : Word;         var Prompt : string;
                       pRow, pCol : Word; var Picture : string;
                       fRow, fCol : Word; fWidth : Byte;
                       HlpNdx : Word;     PadChar : Char;
                       Options, IFlags : LongInt;
                       var Colors : ColorSet);
         {-Initialize a nested entry field}
      procedure Convert(PostEdit : Boolean); virtual;
        {-Conversion routine for nested fields}
    end;

  MultiLineFieldPtr = ^MultiLineField;
  MultiLineField =
    object(EntryField)
      constructor Init(ID : Word;           var Prompt : string;
                       pRow, pCol : Word;   PicChar : Char;
                       fRow, fCol : Word;   fWidth : Byte;
                       fHeight : Byte;  HlpNdx : Word;
                       var EditVar;         PadChar : Char;
                       Options, IFlags : LongInt; var Colors : ColorSet);
        {-Initialize a multi-line field}
      procedure Edit(Row, Col : Word;     FA, CA : Byte;
                     PasswordChar : Char; PosCode : Byte;
                     ReadOnly : Boolean;  var CC : Word;
                     var ChWord : Word;   var InsertMode : Boolean;
                     EP : ErrorProc;      UnitCode : Byte;
                     var CP : CommandProcessor); virtual;
        {-Edit the field}
      procedure Draw(var St : string; Row, Col : Word; FA, CA, POffset : Byte;
                     PasswordChar : Char; var Flags : PictureFlags); virtual;
        {-Draw the field (St ignored)}
      procedure Convert(PostEdit : Boolean); virtual;
        {-Conversion routine}
      {+++++++++ following methods for internal use only +++++++}
      procedure efTrimSpaces; virtual;
      function  efFieldIsEmpty : Boolean; virtual;
      function  mfLinePtr(LineNum : Byte) : StringPtr;
      function  mfOrigLinePtr(LineNum : Byte) : StringPtr;
      procedure mfDrawOne(St : string; Row, Col : Byte; FA, CA : Byte);
    end;

{$IFDEF UseStreams}

  {------- stream registration routines ------------}

procedure EntryFieldStream(SPtr : IdStreamPtr);
  {-Register all types for entry fields}

procedure StringFieldStream(SPtr : IdStreamPtr);
  {-Register all types for string fields}
procedure StringFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for string fields (simple)}
procedure ArrayFieldStream(SPtr : IdStreamPtr);
  {-Register all types for array of char fields}
procedure CharFieldStream(SPtr : IdStreamPtr);
  {-Register all types for char fields}
procedure CharFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for char fields (simple)}
procedure BooleanFieldStream(SPtr : IdStreamPtr);
  {-Register all types for boolean fields}
procedure BooleanFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for boolean fields (simple)}
procedure YesNoFieldStream(SPtr : IdStreamPtr);
  {-Register all types for yes-no fields}
procedure YesNoFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for yes-no fields (simple)}
procedure LongIntFieldStream(SPtr : IdStreamPtr);
  {-Register all types for longint fields}
procedure LongIntFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for longint fields (numeric)}
procedure LongIntFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for longint fields (simple)}
procedure WordFieldStream(SPtr : IdStreamPtr);
  {-Register all types for word fields}
procedure WordFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for word fields (numeric)}
procedure WordFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for word fields (simple)}
procedure SmallWordFieldStream(SPtr : IdStreamPtr);
  {-Register all types for SmallWord fields}
procedure SmallWordFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for SmallWord fields (numeric)}
procedure SmallWordFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for wordSmallWord fields (simple)}
procedure IntegerFieldStream(SPtr : IdStreamPtr);
  {-Register all types for integer fields}
procedure IntegerFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for integer fields (numeric)}
procedure IntegerFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for integer fields (simple)}
procedure SmallIntFieldStream(SPtr : IdStreamPtr);
  {-Register all types for integer fields}
procedure SmallIntFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for integer fields (numeric)}
procedure SmallIntFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for integer fields (simple)}
procedure ByteFieldStream(SPtr : IdStreamPtr);
  {-Register all types for byte fields}
procedure ByteFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for byte fields (numeric)}
procedure ByteFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for byte fields (simple)}
procedure ShortIntFieldStream(SPtr : IdStreamPtr);
  {-Register all types for shortint fields}
procedure ShortIntFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for shortint fields (numeric)}
procedure ShortIntFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for shortint fields (simple)}
procedure RealFieldStream(SPtr : IdStreamPtr);
  {-Register all types for real fields}
procedure RealFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for real fields (numeric)}
procedure RealFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for real fields (simple)}
{$IFDEF UseBcd}
procedure BcdFieldStream(SPtr : IdStreamPtr);
  {-Register all types for BCD fields}
procedure BcdFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for BCD fields (numeric)}
procedure BcdFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for BCD fields (simple)}
{$ENDIF}
{$IFOPT N+}
procedure ExtendedFieldStream(SPtr : IdStreamPtr);
  {-Register all types for extended fields}
procedure ExtendedFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for extended fields (numeric)}
procedure ExtendedFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for extended fields (simple)}
procedure DoubleFieldStream(SPtr : IdStreamPtr);
  {-Register all types for double fields}
procedure DoubleFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for double fields (numeric)}
procedure DoubleFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for double fields (simple)}
procedure SingleFieldStream(SPtr : IdStreamPtr);
  {-Register all types for single fields}
procedure SingleFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for single fields (numeric)}
procedure SingleFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for single fields (simple)}
procedure CompFieldStream(SPtr : IdStreamPtr);
  {-Register all types for comp fields}
procedure CompFieldNumStream(SPtr : IdStreamPtr);
  {-Register all types for comp fields (numeric)}
procedure CompFieldSimStream(SPtr : IdStreamPtr);
  {-Register all types for comp fields (simple)}
{$ENDIF}
{$IFDEF UseDates}
procedure DateFieldStream(SPtr : IdStreamPtr);
  {-Register all types for date fields}
procedure DateStFieldStream(SPtr : IdStreamPtr);
  {-Register all types for date string fields}
procedure TimeFieldStream(SPtr : IdStreamPtr);
  {-Register all types for time fields}
{$ENDIF}
procedure NestedFieldStream(SPtr : IdStreamPtr);
  {-Register all types for nested entry fields}
procedure ChoiceFieldStream(SPtr : IdStreamPtr);
  {-Register all types for multiple choice fields}
procedure MultiLineFieldStream(SPtr : IdStreamPtr);
  {-Register all types for multiline fields}

{$ENDIF}

  {------- generic draw routines ------------}

procedure DrawString(var Field; St : string; Row, Col : Word; FA, CA, POffset : Byte;
                     PasswordChar : Char; Flags : PictureFlags);

procedure SimpleDrawString(var Field; St : string; Row, Col : Word;
                           FA, CA, POffset : Byte; PasswordChar : Char;
                           Flags : PictureFlags);

  {------- conversion routines ------}

procedure StringConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for strings}
procedure SimpleStringConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for strings}
procedure CharConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for characters}
procedure SimpleCharConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for characters}
procedure BooleanConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for booleans}
procedure SimpleBooleanConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for booleans}
procedure YesNoConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for yes/no's}
procedure SimpleYesNoConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for yes-no fields}
procedure LongConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for longints}
procedure SimpleLongConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for longints}
procedure WordConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for words}
procedure SimpleWordConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for words}
procedure SmallWordConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for smallwords}
procedure SimpleSmallWordConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for smallwords}
procedure IntConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for integers}
procedure SimpleIntConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for integers}
procedure SmallIntConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for smallintegers}
procedure SimpleSmallIntConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for smallintegers}
procedure ByteConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for bytes}
procedure SimpleByteConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for bytes}
procedure ShortConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for shortints}
procedure SimpleShortConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for shortints}
procedure RealConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for reals}
procedure SimpleRealConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for reals}
{$IFDEF UseBcd}
procedure BcdConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for BCD reals}
procedure SimpleBcdConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for BCD's}
{$ENDIF}
{$IFOPT N+}
procedure ExtConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for extendeds}
procedure SimpleExtConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for extendeds}
procedure DblConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for doubles}
procedure SimpleDblConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for doubles}
procedure SglConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for singles}
procedure SimpleSglConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for singles}
procedure CompConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for comps}
procedure SimpleCompConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Conversion routine for comps}
{$ENDIF}

  {------- validation routines ------}

function NullValidation(EFP : EntryFieldPtr; var ErrCode : Word;
                        var ErrorSt : StringPtr) : Boolean;
  {-Do-nothing validation routine}
function ValidateChar(EFP : EntryFieldPtr; var ErrCode : Word;
                      var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type char}
function SimpleValidateChar(EFP : EntryFieldPtr; var ErrCode : Word;
                            var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type char}
function ValidateLong(EFP : EntryFieldPtr; var ErrCode : Word;
                      var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type longint/word/integer/byte/shortint}
function SimpleValidateLong(EFP : EntryFieldPtr; var ErrCode : Word;
                            var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type longint/word/integer/byte/shortint}
function ValidateReal(EFP : EntryFieldPtr; var ErrCode : Word;
                      var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type real}
function SimpleValidateReal(EFP : EntryFieldPtr; var ErrCode : Word;
                            var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type real}
{$IFDEF UseBcd}
function ValidateBCD(EFP : EntryFieldPtr; var ErrCode : Word;
                     var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type BCD}
function SimpleValidateBcd(EFP : EntryFieldPtr; var ErrCode : Word;
                           var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type BCD}
{$ENDIF}
{$IFOPT N+}
function ValidateExt(EFP : EntryFieldPtr; var ErrCode : Word;
                     var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type extended/double/single/comp}
function SimpleValidateExt(EFP : EntryFieldPtr; var ErrCode : Word;
                           var ErrorSt : StringPtr) : Boolean;
  {-Validate a field of type extended}
{$ENDIF}

{.Z-}

  {-------special validation routines-------}

function ValidateNoBlanks(EFP : EntryFieldPtr; var ErrCode : Word;
                          var ErrorSt : StringPtr) : Boolean;
  {-Validate that no usable subfields in a string contain spaces}

function ValidateNotPartial(EFP : EntryFieldPtr; var ErrCode : Word;
                            var ErrorSt : StringPtr) : Boolean;
  {-Validate that no usable subfields in a string contain spaces unless all do}

const
  {the following special characters are used by ValidateSubfields}
  PartialChar   = 'p';       {prohibit Partial entry in this subfield}
  ReqdChar      = 'r';       {entire subfield is Required}
  UnlessChar    = 'u';       {no blanks in subfield unless whole field is blank}

function ValidateSubfields(SubfieldMask : string; EFP : EntryFieldPtr;
                           var ErrCode : Word;
                           var ErrorSt : StringPtr) : Boolean;
  {-Validate that subfields in a string meet the requirements of SubfieldMask.
    Note: This is NOT a regular validation routine. It should be called only
    by a higher-level validation routine that has a SubfieldMask to give it.}

  {-------other type and data declarations -------}

type
  CommandSet   = set of Byte;
const
  BlankRange   : RangeType = (rt10 : (0, 0, 0, 0, 0, 0, 0, 0, 0, 0));

var                                             {!!.02}
  BadReal : Real absolute OpAbsFld.BadReal;     {!!.02}
{$IFOPT N+}
var                                             {!!.02}
  BadExt  : Extended absolute OpAbsFld.BadExt;  {!!.02}
var                                             {!!.02}
  BadComp : Comp absolute OpAbsFld.BadComp;     {!!.02}
var                                             {!!.02}
  BadDbl  : Double absolute OpAbsFld.BadDbl;    {!!.02}
var                                             {!!.02}
  BadSgl  : Single absolute OpAbsFld.BadSgl;    {!!.02}
{$ENDIF}
{$IFDEF UseBcd}
var
  BadBcd  : Bcd absolute OpAbsFld.BadBcd;       {!!.02}
{$ENDIF}

  {------------ command sets ---------------}

const
  {standard command sets}
  StringCommands : CommandSet = {all commands available}
    [ccNone..ccUser55];

const
  ReadOnlyCommands : CommandSet = {no editing commands}
    [ccSelect, ccQuit, ccMouseSel, ccHome, ccEnd, ccLeft, ccRight, ccUp,
     ccAltKey,                 {!!.11}
     ccMouseDown, ccMouseAuto, {!!.03}
     ccDown, ccWordLeft, ccWordRight, ccPageUp, ccPageDn, ccIns, ccHelp,
     ccTab, ccBackTab, ccIncChoice, ccDecChoice, ccNextField, ccAutoAdvance,
     ccPrevField, ccNextRec, ccPrevRec, ccFirstFld, ccLastFld, ccNested,
     ccDone, ccClickExit, ccUser0..ccUser55];

const
  NumberCommands : CommandSet = {commands available for editing numbers}
    [ccChar, ccSelect, ccQuit, ccMouseSel, ccRestore, ccHome, ccEnd, ccLeft,
     ccAltKey,                 {!!.11}
     ccMouseDown, ccMouseAuto, {!!.03}
     ccRight, ccUp, ccDown, ccWordLeft, ccWordRight, ccPageUp, ccPageDn,
     ccBack, ccDel, ccDelLine, ccIns, ccHelp, ccTab, ccBackTab, ccNextField,
     ccAutoAdvance, ccPrevField, ccNextRec, ccPrevRec, ccFirstFld, ccLastFld,
     ccNested, ccDone, ccClickExit, ccUser0..ccUser55];

const
  ChoiceCommands : CommandSet = {commands available for multiple choice fields}
    [ccChar, ccSelect, ccQuit, ccMouseSel, ccRestore, ccLeft, ccRight,
     ccAltKey,                 {!!.11}
     ccMouseDown, ccMouseAuto, {!!.03}
     ccWordLeft, ccWordRight, ccHelp, ccTab, ccBackTab, ccIncChoice,
     ccDecChoice, ccNextField, ccAutoAdvance, ccPrevField, ccUp, ccDown,
     ccPageUp, ccPageDn, ccNextRec, ccPrevRec, ccFirstFld, ccLastFld,
     ccNested, ccDone, ccClickExit, ccUser0..ccUser55];

{.Z+}

function CharOK(PicChar : Char; var Ch : Char;
                PrevCh : Char; Fix : Boolean) : Boolean;
  {-Return True if Ch is in character set corresponding to PicChar.. PrevCh is
    the last character before Ch in the subfield, or ' ' if none.}

function ValidField(EFP : EntryFieldPtr; var St : string) : Byte;
  {-Check an entire field for valid characters. If error is found, result has
    offset into the string so that cursor can be positioned on it.}

procedure FixRealPrim(var S : string; Validating : Boolean); {!!.02}
  {-Get a string representing a real ready for Val}

{.Z-}

  {==========================================================================}

implementation

uses
  OpFEdit;

  {$I OPFIELD.IN1}  {misc., TextFields, SelectFields, EntryFields}
  {$I OPFIELD.IN2}  {descendents of EntryFields}
  {$I OPFIELD.IN3}  {multiple choice fields, multi-line fields}
  {$I OPFIELD.IN4}  {drawing, validation, and conversion routines}
  {$I OPFIELD.IN5}  {stream registration routines}

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
