{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPENTRY.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1988,1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpEntry;
  {-Data entry routines}


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
  OpCmd,
  {$IFDEF UseBcd}
  OpBcd,
  {$ENDIF}
  {$IFDEF UseDates}
  OpDate,
  {$ENDIF}
  OpAbsFld,
  OpFEdit,
  OpField,
  OpFrame,
  OpWindow,
  {$IFDEF UseDrag}            {!!.03}
  OpDrag,                     {!!.03}
  {$ENDIF}                    {!!.03}
  {$IFDEF PickListFields}
  OpPick,
  {$ENDIF}
  OpSelect;

  {$I OPENTRY.ICD}  {configuration data}

{.F-}

const
  {option codes for entry screens}
  esScrollByPage     = $01; {scroll by page?}
  esMousePage        = $02; {clicking on scroll bar scrolls by page}
  esReadOnly         = $04; {read-only mode}
  esSamePosition     = $08; {same cursor position for current field as before?}
  esUpdateActive     = $10; {used internally} {!!.01}
  esDeallocUserRec   = $40; {used internally}
  esFastUpdates      = $80; {used internally}

  DefEntryOptions    : Byte = esMousePage;                  {!!.01}
                                                            {!!.01}
  BadEntryOptions    : Byte = esFastUpdates+esDeallocUserRec+esUpdateActive;
  BadEsFieldOptions  : LongInt = efMultChoice;
type
  EntryScreenPtr = ^EntryScreen;
  esUserProc = procedure(ESP : EntryScreenPtr);
  esUserValidationFunc = function(ESP : EntryScreenPtr;
                                  var EFP : EntryFieldPtr;
                                  var ErrCode : Word;
                                  var ErrMsg : StringPtr) : Boolean;
  esFieldProc = procedure(EFP : EntryFieldPtr; var D; ESP : EntryScreenPtr);
  EntryScreen =
    object(AbstractSelector)
      {-----------------------------Procedure pointers}
      esPreEditProc   : esUserProc; {called before each edit}
      esPostEditProc  : esUserProc; {called after each edit}
      esValidateFunc  : esUserValidationFunc; {called before exiting}
      {-----------------------------Delimiters}
      esLeftD         : Char;       {field delimiters (null = none)}
      esRightD        : Char;
      {-----------------------------Miscellaneous}
      esPasswordChar  : Char;       {character used in password mode}
      esPadChar       : Char;       {character used to pad strings}
      esInsertMode    : Boolean;    {insert mode on?}
      esFieldFlags    : LongInt;    {internal flags for fields}
      {-----------------------------User record data}
      esUserRecSize   : Word;       {size of the record}
      esUserRecPtr    : Pointer;    {address of the record}
      {-----------------------------Other}
      esCFF           : ClearFirstFunc; {routine called by field editor}
      {... methods ...}
      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the entry screen}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt);
        {-Initialize the entry screen with custom window options}
      destructor Done; virtual;
        {-Dispose of the entry screen}
      procedure ProcessSelf; virtual; {!!.01}
        {-Process data entry commands}
      {-----------------------------------Add new fields}
      procedure AddStringField(Prompt : string;  pRow, pCol : Word;
                               Picture : string; fRow, fCol : Word;
                               fWidth : Byte;    HelpIndex : Word;
                               var EditSt : string);
        {-Add a field of type string}
      procedure AddSimpleStringField(Prompt : string;       pRow, pCol : Word;
                                     PicChar : Char;    fRow, fCol : Word;
                                     fWidth, MaxLen : Byte; HelpIndex : Word;
                                     var EditSt : string);
        {-Add a field of type string (simple)}
      procedure AddArrayField(Prompt : string;  pRow, pCol : Word;
                              Picture : string; fRow, fCol : Word;
                              fWidth : Byte;    HelpIndex : Word;
                              var EditVar);
        {-Add a field of type array (of char)}
      procedure AddCharField(Prompt : string;  pRow, pCol : Word;
                             Picture : string; fRow, fCol : Word;
                             HelpIndex : Word; CharLo, CharHi : Char;
                             var EditChar : Char);
        {-Add a field of type char}
      procedure AddSimpleCharField(Prompt : string;    pRow, pCol : Word;
                                   PicChar : Char; fRow, fCol : Word;
                                   HelpIndex : Word;   CharLo, CharHi : Char;
                                   var EditChar : Char);
        {-Add a field of type char (simple)}
      procedure AddBooleanField(Prompt : string;  pRow, pCol : Word;
                                Picture : string; fRow, fCol : Word;
                                HelpIndex : Word; var EditBool : Boolean);
        {-Add a field of type boolean}
      procedure AddSimpleBooleanField(Prompt : string;   pRow, pCol : Word;
                                      fRow, fCol : Word; HelpIndex : Word;
                                      var EditBool : Boolean);
        {-Add a field of type boolean (simple)}
      procedure AddYesNoField(Prompt : string;  pRow, pCol : Word;
                              Picture : string; fRow, fCol : Word;
                              HelpIndex : Word; var EditYesNo : Boolean);
        {-Add a field of type yes/no}
      procedure AddSimpleYesNoField(Prompt : string;   pRow, pCol : Word;
                                    fRow, fCol : Word; HelpIndex : Word;
                                    var EditYesNo : Boolean);
        {-Add a field of type yes/no (simple)}
      procedure AddLongField(Prompt : string;  pRow, pCol : Word;
                             Picture : string; fRow, fCol : Word;
                             HelpIndex : Word; LongLo, LongHi : LongInt;
                             var EditLong : LongInt);
        {-Add a field of type LongInt}
      procedure AddNumericLongField(Prompt : string;  pRow, pCol : Word;
                                    Picture : string; fRow, fCol : Word;
                                    HelpIndex : Word; LongLo, LongHi : LongInt;
                                    var EditLong : LongInt);
        {-Add a field of type LongInt (numeric)}
      procedure AddSimpleLongField(Prompt : string;    pRow, pCol : Word;
                                   PicChar : Char; fRow, fCol : Word;
                                   fWidth : Byte;      HelpIndex : Word;
                                   LongLo, LongHi : LongInt;
                                   var EditLong : LongInt);
        {-Add a field of type LongInt (simple)}
      procedure AddWordField(Prompt : string;  pRow, pCol : Word;
                             Picture : string; fRow, fCol : Word;
                             HelpIndex : Word; WordLo, WordHi : Word;
                             var EditWord : Word);
        {-Add a field of type word}
      procedure AddNumericWordField(Prompt : string;  pRow, pCol : Word;
                                    Picture : string; fRow, fCol : Word;
                                    HelpIndex : Word; WordLo, WordHi : Word;
                                    var EditWord : Word);
        {-Add a field of type word (numeric)}
      procedure AddSimpleWordField(Prompt : string;    pRow, pCol : Word;
                                   PicChar : Char; fRow, fCol : Word;
                                   fWidth : Byte;      HelpIndex : Word;
                                   WordLo, WordHi : Word;
                                   var EditWord : Word);
        {-Add a field of type word (simple)}
      procedure AddSmallWordField(Prompt : string;  pRow, pCol : Word;
                                  Picture : string; fRow, fCol : Word;
                                  HelpIndex : Word; WordLo, WordHi : SmallWord;
                                  var EditWord : SmallWord);
        {-Add a field of type word}
      procedure AddNumericSmallWordField(Prompt : string;  pRow, pCol : Word;
                                         Picture : string; fRow, fCol : Word;
                                         HelpIndex : Word; WordLo, WordHi : SmallWord;
                                         var EditWord : SmallWord);
        {-Add a field of type word (numeric)}
      procedure AddSimpleSmallWordField(Prompt : string;    pRow, pCol : Word;
                                        PicChar : Char; fRow, fCol : Word;
                                        fWidth : Byte;      HelpIndex : Word;
                                        WordLo, WordHi : SmallWord;
                                        var EditWord : SmallWord);
        {-Add a field of type word (simple)}
      procedure AddIntField(Prompt : string;  pRow, pCol : Word;
                            Picture : string; fRow, fCol : Word;
                            HelpIndex : Word; IntLo, IntHi : Integer;
                            var EditInt : Integer);
        {-Add a field of type integer}
      procedure AddNumericIntField(Prompt : string;  pRow, pCol : Word;
                                   Picture : string; fRow, fCol : Word;
                                   HelpIndex : Word; IntLo, IntHi : Integer;
                                   var EditInt : Integer);
        {-Add a field of type integer (numeric)}
      procedure AddSimpleIntField(Prompt : string;    pRow, pCol : Word;
                                  PicChar : Char; fRow, fCol : Word;
                                  fWidth : Byte;      HelpIndex : Word;
                                  IntLo, IntHi : Integer;
                                  var EditInt : Integer);
        {-Add a field of type integer (simple)}
      procedure AddSmallIntField(Prompt : string;  pRow, pCol : Word;
                                 Picture : string; fRow, fCol : Word;
                                 HelpIndex : Word; IntLo, IntHi : SmallInt;
                                 var EditInt : SmallInt);
        {-Add a field of type SmallInt}
      procedure AddNumericSmallIntField(Prompt : string;  pRow, pCol : Word;
                                        Picture : string; fRow, fCol : Word;
                                        HelpIndex : Word; IntLo, IntHi : SmallInt;
                                        var EditInt : SmallInt);
        {-Add a field of type SmallInt (numeric)}
      procedure AddSimpleSmallIntField(Prompt : string;    pRow, pCol : Word;
                                       PicChar : Char; fRow, fCol : Word;
                                       fWidth : Byte;      HelpIndex : Word;
                                       IntLo, IntHi : SmallInt;
                                       var EditInt : SmallInt);
        {-Add a field of type SmallInt (simple)}
      procedure AddByteField(Prompt : string;  pRow, pCol : Word;
                             Picture : string; fRow, fCol : Word;
                             HelpIndex : Word; ByteLo, ByteHi : Byte;
                             var EditByte : Byte);
        {-Add a field of type byte}
      procedure AddNumericByteField(Prompt : string;  pRow, pCol : Word;
                                    Picture : string; fRow, fCol : Word;
                                    HelpIndex : Word; ByteLo, ByteHi : Byte;
                                    var EditByte : Byte);
        {-Add a field of type byte (numeric)}
      procedure AddSimpleByteField(Prompt : string;    pRow, pCol : Word;
                                   PicChar : Char; fRow, fCol : Word;
                                   fWidth : Byte;      HelpIndex : Word;
                                   ByteLo, ByteHi : Byte;
                                   var EditByte : Byte);
        {-Add a field of type byte (simple)}
      procedure AddShortField(Prompt : string;   pRow, pCol : Word;
                              Picture : string;  fRow, fCol : Word;
                              HelpIndex : Word;  ShortLo, ShortHi : ShortInt;
                              var EditShort : ShortInt);
        {-Add a field of type shortint}
      procedure AddNumericShortField(Prompt : string;   pRow, pCol : Word;
                                     Picture : string;  fRow, fCol : Word;
                                     HelpIndex : Word;  ShortLo, ShortHi : ShortInt;
                                     var EditShort : ShortInt);
        {-Add a field of type shortint (numeric)}
      procedure AddSimpleShortField(Prompt : string;    pRow, pCol : Word;
                                    PicChar : Char; fRow, fCol : Word;
                                    fWidth : Byte;      HelpIndex : Word;
                                    ShortLo, ShortHi : ShortInt;
                                    var EditShort : ShortInt);
        {-Add a field of type shortint (simple)}
      procedure AddRealField(Prompt : string;   pRow, pCol : Word;
                             Picture : string;  fRow, fCol : Word;
                             HelpIndex : Word;  RealLo, RealHi : Real;
                             Places : Byte;     var EditReal : Real);
        {-Add a field of type real}
      procedure AddNumericRealField(Prompt : string;  pRow, pCol : Word;
                                    Picture : string; fRow, fCol : Word;
                                    HelpIndex : Word; RealLo, RealHi : Real;
                                    Places : Byte;    var EditReal : Real);
        {-Add a field of type real (numeric)}
      procedure AddSimpleRealField(Prompt : string;       pRow, pCol : Word;
                                   PicChar : Char;    fRow, fCol : Word;
                                   fWidth : Byte;         HelpIndex : Word;
                                   RealLo, RealHi : Real; Places : Byte;
                                   var EditReal : Real);
        {-Add a field of type real (simple)}
      {$IFDEF UseBcd}
      procedure AddBcdField(Prompt : string;   pRow, pCol : Word;
                            Picture : string;  fRow, fCol : Word;
                            HelpIndex : Word;  BcdLo, BcdHi : BCD;
                            Places : Byte;     var EditBcd : BCD);
        {-Add a field of type BCD}
      procedure AddNumericBcdField(Prompt : string;  pRow, pCol : Word;
                                   Picture : string; fRow, fCol : Word;
                                   HelpIndex : Word; BcdLo, BcdHi : BCD;
                                   Places : Byte;    var EditBcd : BCD);
        {-Add a field of type BCD (numeric)}
      procedure AddSimpleBcdField(Prompt : string;    pRow, pCol : Word;
                                  PicChar : Char; fRow, fCol : Word;
                                  fWidth : Byte;      HelpIndex : Word;
                                  BcdLo, BcdHi : BCD; Places : Byte;
                                  var EditBcd : BCD);
        {-Add a field of type BCD (simple)}
      {$ENDIF}
      {$IFOPT N+}
      procedure AddExtField(Prompt : string;  pRow, pCol : Word;
                            Picture : string; fRow, fCol : Word;
                            HelpIndex : Word; ExtLo, ExtHi : Extended;
                            Places : Byte;    var EditExt : Extended);
        {-Add a field of type extended}
      procedure AddNumericExtField(Prompt : string;  pRow, pCol : Word;
                                   Picture : string; fRow, fCol : Word;
                                   HelpIndex : Word; ExtLo, ExtHi : Extended;
                                   Places : Byte;    var EditExt : Extended);
        {-Add a field of type extended (numeric)}
      procedure AddSimpleExtField(Prompt : string;         pRow, pCol : Word;
                                  PicChar : Char;      fRow, fCol : Word;
                                  fWidth : Byte;           HelpIndex : Word;
                                  ExtLo, ExtHi : Extended; Places : Byte;
                                  var EditExt : Extended);
        {-Add a field of type extended (simple)}
      procedure AddDblField(Prompt : string;  pRow, pCol : Word;
                            Picture : string; fRow, fCol : Word;
                            HelpIndex : Word; DblLo, DblHi : Double;
                            Places : Byte;    var EditDbl : Double);
        {-Add a field of type double}
      procedure AddNumericDblField(Prompt : string;  pRow, pCol : Word;
                                   Picture : string; fRow, fCol : Word;
                                   HelpIndex : Word; DblLo, DblHi : Double;
                                   Places : Byte;    var EditDbl : Double);
        {-Add a field of type double (numeric)}
      procedure AddSimpleDblField(Prompt : string;       pRow, pCol : Word;
                                  PicChar : Char;    fRow, fCol : Word;
                                  fWidth : Byte;         HelpIndex : Word;
                                  DblLo, DblHi : Double; Places : Byte;
                                  var EditDbl : Double);
        {-Add a field of type double (simple)}
      procedure AddSglField(Prompt : string;  pRow, pCol : Word;
                            Picture : string; fRow, fCol : Word;
                            HelpIndex : Word; SglLo, SglHi : Single;
                            Places : Byte;    var EditSgl : Single);
        {-Add a field of type single}
      procedure AddNumericSglField(Prompt : string;  pRow, pCol : Word;
                                   Picture : string; fRow, fCol : Word;
                                   HelpIndex : Word; SglLo, SglHi : Single;
                                   Places : Byte;    var EditSgl : Single);
        {-Add a field of type single (numeric)}
      procedure AddSimpleSglField(Prompt : string;       pRow, pCol : Word;
                                  PicChar : Char;    fRow, fCol : Word;
                                  fWidth : Byte;         HelpIndex : Word;
                                  SglLo, SglHi : Single; Places : Byte;
                                  var EditSgl : Single);
        {-Add a field of type single (simple)}
      procedure AddCompField(Prompt : string;  pRow, pCol : Word;
                             Picture : string; fRow, fCol : Word;
                             HelpIndex : Word; CompLo, CompHi : Comp;
                             var EditComp : Comp);
        {-Add a field of type comp}
      procedure AddNumericCompField(Prompt : string;  pRow, pCol : Word;
                                    Picture : string; fRow, fCol : Word;
                                    HelpIndex : Word; CompLo, CompHi : Comp;
                                    var EditComp : Comp);
        {-Add a field of type comp (numeric)}
      procedure AddSimpleCompField(Prompt : string;       pRow, pCol : Word;
                                   PicChar : Char;    fRow, fCol : Word;
                                   fWidth : Byte;         HelpIndex : Word;
                                   CompLo, CompHi : Comp; var EditComp : Comp);
        {-Add a field of type comp (simple)}
      {$ENDIF}
      {$IFDEF UseDates}
      procedure AddDateField(Prompt : string;      pRow, pCol : Word;
                             Picture : DateString; fRow, fCol : Word;
                             HelpIndex : Word;     DateLo, DateHi : Date;
                             var EditDate : Date);
        {-Add a field of type date}
      procedure AddDateStField(Prompt : string;      pRow, pCol : Word;
                               Picture : DateString; fRow, fCol : Word;
                               HelpIndex : Word;     var EditDate : DateString);
        {-Add a field of type date string}
      procedure AddTimeField(Prompt : string;      pRow, pCol : Word;
                             Picture : DateString; fRow, fCol : Word;
                             HelpIndex : Word;     TimeLo, TimeHi : Time;
                             var EditTime : Time);
        {-Add a field of type time}
      {$ENDIF}
      procedure AddNestedField(Prompt : string;  pRow, pCol : Word;
                               Picture : string; fRow, fCol : Word;
                               fWidth : Byte;    HelpIndex : Word);
        {-Add a field that is associated with a nested form}
      procedure AddNestedStringField(Prompt : string;  pRow, pCol : Word;
                                     Picture : string; fRow, fCol : Word;
                                     fWidth : Byte;    HelpIndex : Word;
                                     var EditSt : string);
        {-Add a string field that is associated with a nested form}
      procedure AddChoiceField(Prompt : string;  pRow, pCol : Word;
                               Picture : string; fRow, fCol : Word;
                               HelpIndex : Word; DataSize : Byte;
                               Increment : IncChoiceProc;
                               var EditVar);
        {-Add a multiple choice field}
      procedure AddMultiLineField(Prompt : string;        pRow, pCol : Word;
                                  PicChar : Char;         fRow, fCol : Word;
                                  fWidth, fHeight : Byte; HelpIndex : Word;
                                  var EditVar);
        {-Add a multi-line field}
      procedure AddWindowField(Prompt : string;  pRow, pCol : Word; {!!.01}
                               fRow, fCol: Word;  HelpIndex : Word;
                               var CW : CommandWindow);
        {-Add a child window to be treated as a "field"}
      {$IFDEF PickListFields}
      procedure AddPickStringField(Prompt : string;     pRow, pCol : Word;
                                   fRow, fCol : Word;   fWidth : Byte;
                                   HelpIndex : Word;    var EditSt : string;
                                   var PL : PickList);
        {-Add a field of type string to be edited with a pick list}
      {$ENDIF}
      procedure AddUserField(Prompt : string;     pRow, pCol : Word;
                             Picture : string;    fRow, fCol : Word;
                             fWidth : Byte;       HelpIndex : Word;
                             RangeLo : RangeType; RangeHi : RangeType;
                             DataSize : Word;     DecimalPlaces : Byte;
                             Validate: ValidationFunc;
                             Convert : ConversionProc;
                             FDraw   : DrawProc;
                             Editor  : EditProc;
                             var EditVar);
        {-Add a field of a user-defined type}
      {-----------------------------------Procedure pointers}
      procedure SetPreEditProc(PEP : esUserProc);
        {-Specify routine to call just before field is edited}
      procedure SetPostEditProc(PEP : esUserProc);
        {-Specify routine to call after each edit}
      procedure SetValidateFunc(UVF : esUserValidationFunc);
        {-Specify routine to call before exiting}
      procedure SetClearFirstFilter(CFF : ClearFirstFunc);
        {-Set procedure to call for ClearFirstChar test}
      {----------------------------Entry screen options}
      procedure esOptionsOn(OptionFlags : Byte);
        {-Activate multiple options}
      procedure esOptionsOff(OptionFlags : Byte);
        {-Deactivate multiple options}
      function esOptionsAreOn(OptionFlags : Byte) : Boolean;
        {-Return true if all specified options are on}
      {----------------------------Field options}
      procedure esFieldOptionsOn(OptionFlags : LongInt);
        {-Activate multiple field options}
      procedure esFieldOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple field options}
      function esFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified field options are on}
      procedure esSecFieldOptionsOn(OptionFlags : LongInt);
        {-Activate multiple secondary field options}
      procedure esSecFieldOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple secondary field options}
      function esSecFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified secondary field options are on}
      {-----------------------------------Other options}
      procedure SetForceMode(Force, Overtype : Boolean);
        {-Force insert or overtype mode, else use previous setting}
      procedure SetPadChar(Ch : Char);
        {-Set character used to pad ends of strings}
      procedure SetPasswordChar(Ch : Char);
        {-Set character used in password mode}
      procedure SetBeepOnError(IsOn : Boolean);
        {-Activate/deactivate beeping on error option for all fields}
      {-----------------------------------Field delimiters}
      procedure SetDelimiters(Left, Right : Char);
        {-Set field delimiters for the entry screen (null = none)}
      procedure SetDelimAttr(Color, Mono : Byte);
        {-Set attributes for delimiters (unselected)}
      procedure SetSelectedDelimAttr(Color, Mono : Byte);
        {-Set attributes for delimiters (selected)}
      procedure SetProtectedDelimAttr(Color, Mono : Byte);
        {-Set attributes for delimiters (protected)}
    {$IFDEF UseStreams}
      {-----------------------------------Streams}
      constructor Load(var S : IdStream);
        {-Load an entry screen from a stream}
      procedure Store(var S : IdStream);
        {-Store an entry screen in a stream}
      procedure SetUserRecord(var UserRec; UserRecSize : Word);
        {-Set the address and size of the user record}
      function GetUserRecord : Pointer;
        {-Return a pointer to the user record}
      function GetUserRecordSize : Word;
        {-Return the size of the user record}
    {$ENDIF}
      {-----------------------------------Miscellaneous}
      procedure ChangeRequired(ID : Word; IsOn : Boolean);
        {-Modify the required status of a field after it has been added}
      procedure ChangeValidation(ID : Word; VF : ValidationFunc);
        {-Change the validation routine for the specified field}
      procedure ChangeConversion(ID : Word; CP : ConversionProc);
        {-Change the conversion routine for the specified field}
      procedure ChangeRange(ID : Word; var RangeLo, RangeHi);
        {-Change the valid range for the specified field}
      function CurrentFieldModified : Boolean;
        {-Return True if current field was modified. Call only from within a
          post-edit routine.}
      function EvaluateCommand(var Cmd : Word) : Word;
        {-Given a command, return the ID for the field the cursor will move
          to next. Cmd may be modified on return. This routine is intended to
          be called only from within a post-edit routine!}
      procedure GetFieldCoordinates(var Row, Col, Wid : Byte);
        {-Returns the absolute screen coordinates for the current field. This
          routine is intended to be called only from within a pre-edit
          routine!}
      function GetLastField : EntryFieldPtr;
        {-Return pointer to last field in entry screen}
      function RevalidateAll(var EFP : EntryFieldPtr; var ErrCode : Word;
                             var ErrMsg : StringPtr) : Boolean;
        {-Re-validate all unprotected fields in entry screen. If an error is
          found, EFP will point to the field with the error, an error code and
          error message will be returned, and RevalidateAll will be False.}
      procedure VisitAllEntryFields(FProc : esFieldProc; var D);
        {-Call the specified procedure for all fields in the entry screen}
{.Z+}
      {+++ internal methods +++}
      procedure PositionCursorAt(SFP : SelectFieldPtr); virtual;
      procedure asDrawKnownField(SFP : SelectFieldPtr); virtual;
      procedure asResetFlags; virtual;
      procedure esPreEdit; virtual;
      procedure esPostEdit; virtual;
      function esValidate(var EFP : EntryFieldPtr; var ErrCode : Word;
                          var ErrMsg : StringPtr) : Boolean; virtual;
      procedure esCalcAttrs(EFP : EntryFieldPtr; var FA, CA, PA, DA : Byte);
      procedure esDrawFieldPrim(EFP : EntryFieldPtr; ConvertIt : Boolean); virtual; {!!.12}
      function  esRequiredFieldsOK(var EFP : EntryFieldPtr; var ErrCode : Word;
                                   var ErrMsg : StringPtr) : Boolean;
      procedure esAppendField(P : Pointer);
      function  esCheckCoords(var Prompt : string; pRow, pCol : Word;
                              fRow, fCol, fWidth, fHeight : Word) : Boolean;
      function  esParamsOK(var Prompt : string; pRow, pCol : Word;
                           var Picture : string; fRow, fCol : Word;
                           var fWidth : Byte) : Boolean;
      {$IFDEF UseMouse}
      function asMouseOnField(SFP : SelectFieldPtr;
                            Row, Col : Integer) : Boolean; virtual;
      function esEvaluateMouseCommand(var Cmd : Word;
                                      var ScrollByPage : Boolean;
                                      Execute : Boolean) : Boolean;
      {$ENDIF}
      {$IFDEF UseStreams}
      function esFixOneUserVar(var IdS : IdStream;
                               EFP : EntryFieldPtr) : Boolean;
      function esFixUserVars(var IdS : IdStream;
                             var EFP : EntryFieldPtr) : Boolean;
      function esRestoreOneUserVar(var IdS : IdStream;
                                   EFP : EntryFieldPtr) : Boolean;
      function esRestoreUserVars(var IdS : IdStream;
                                 Last : EntryFieldPtr) : Boolean;
      {$ENDIF}
      constructor esBind;
      constructor esBindPrim;
{.Z-}
    end;

  ScrollingEntryScreenPtr = ^ScrollingEntryScreen;
  ScrollingEntryScreen =
    object(EntryScreen)
      sesScrollable : Boolean;     {False until initialized}
      sesRowOfs     : Integer;     {number of rows above top of window}
      sesColOfs     : Integer;     {number of cols to left of window}
      sesVS         : VirtScreen;  {virtual screen for scrollable window}
      {....methods....}
      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the entry screen}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt);
        {-Initialize the entry screen with custom window options}
      destructor Done; virtual;
        {-Deallocate field list, screen buffers}
      procedure AllocateScreen;
        {-Call after last field added to allocate virtual screen}
      procedure VScreenToScreen; virtual;
        {-Copy the virtual screen to the physical screen}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a scrolling entry screen from a stream}
      procedure Store(var S : IdStream);
        {-Store a scrolling entry screen in a stream}
    {$ENDIF}
{.Z+}
      {+++ internal methods +++}
      procedure UpdateContents; virtual;
      function asFixCoordinates(InRow, InCol : Word; Wid, Ht : Byte;
                                var OutRow, OutCol : Integer) : Boolean; virtual;
      procedure asDrawKnownField(SFP : SelectFieldPtr); virtual;
      procedure asFixWindow(Redraw, ScrollByPage : Boolean); virtual;
      procedure asPageUpOrDown(Delta : Integer); virtual;
      function asMaximumRow : Word; virtual;
      function asMaximumCol : Word; virtual;
      constructor sesBind;
      constructor sesBindPrim;
{.Z-}
    end;

{$IFDEF PickListFields}
{.Z+}
  PickFieldPtr = ^PickField;
  PickField =
    object(EntryField)
      pfPick : PickListPtr;   {pointer to the pick list}

      constructor Init(ID : Word;             var Prompt : string;
                       pRow, pCol : Word;     fRow, fCol : Word;
                       fWidth : Byte;         HelpIndex : Word;
                       var EditSt : String;   PadChar : Char;
                       Options : LongInt;     IFlags : LongInt; {!!.01}
                       var Colors : ColorSet; var PL : PickList);
        {-Initialize a pick list field}
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a pick list field from a stream}
      procedure Store(var S : IdStream);
        {-Store a pick list field in a stream}
      {$ENDIF}
    end;
{.Z-}
{$ENDIF}

{.Z+}
  WindowFieldPtr = ^WindowField; {!!.01}
  WindowField =                  {!!.01}
    object(EntryField)
      wfWin : CommandWindowPtr;

      constructor Init(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      fRow, fCol : Word;
                       fWidth, fHeight : Byte; HelpIndex : Word;
                       Options : LongInt;      IFlags : LongInt;
                       var Colors : ColorSet;  var CW : CommandWindow);
        {-Initialize a command window field}
      procedure Convert(PostEdit : Boolean); virtual;
      procedure Draw(var St : string;
                     Row, Col : Word;
                     FA, CA, POffset : Byte;
                     PasswordChar : Char;
                     var Flags : PictureFlags); virtual;
      procedure Edit(Row, Col : Word;     FA, CA : Byte;
                     PasswordChar : Char; PosCode : Byte;
                     ReadOnly : Boolean;  var CC : Word;
                     var ChWord : Word;   var InsertMode : Boolean;
                     EP : ErrorProc;      UnitCode : Byte;
                     var CP : CommandProcessor); virtual;
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a window field from a stream}
      procedure Store(var S : IdStream);
        {-Store a window field from a stream}
      {$ENDIF}
    end;
{.Z-}

{.F+}

{----------- streams ------------}

{.Z+}
{$IFDEF UseStreams}

procedure EntryScreenStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing entry screens}

procedure ScrollingEntryScreenStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing scrolling entry screens}

procedure AllPictureFieldsStream(SPtr : IdStreamPtr);
  {-Register all entry fields that use one of the regular (picture) editors}

procedure AllSimpleFieldsStream(SPtr : IdStreamPtr);
  {-Register all entry fields that use one of the simple editors}

procedure AllNumericFieldsStream(SPtr : IdStreamPtr);
  {-Register all entry fields that use the numeric editor}

procedure AllEntryFieldsStream(SPtr : IdStreamPtr);
  {-Register all possible types of entry fields}

{$IFDEF PickListFields}
procedure PickFieldStream(SPtr : IdStreamPtr);
  {-Register all types needed for a PickField}
{$ENDIF}

procedure WindowFieldStream(SPtr : IdStreamPtr); {!!.01}
  {-Register all types needed for a WindowField}

{$ENDIF}
{.Z-}

var
  {$IFDEF UseDrag}                      {!!.03}
  EntryCommands : DragProcessor;        {!!.03}
  {$ELSE}                               {!!.03}
  EntryCommands : CommandProcessor;
  {$ENDIF}                              {!!.03}

{.Z+}
const
  WindowFieldParent : WindowPtr = nil;
{.Z-}

  {==========================================================================}

implementation

  {.F-}
const
  MaxFields = 2000;  {max number of fields in an entry screen}

  {$I OPENTRY.IN1}           {Routines to add fields}
  {$I OPENTRY.IN2}           {PickFields, ScrollingEntryScreens, streams}

  constructor EntryScreen.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize an entry screen}
  begin
    {initialize using default window options}
    if not EntryScreen.InitCustom(X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions) then
      Fail;
  end;

  constructor EntryScreen.InitCustom(X1, Y1, X2, Y2 : Byte;
                                     var Colors : ColorSet;
                                     Options : LongInt);
    {-Initialize an entry screen}
  begin
    {initialize the object as a selector/window}
    if not AbstractSelector.InitCustom(X1, Y1, X2, Y2, Colors, Options,
                                  EntryCommands, ucEntry) then
      Fail;

    {initialize fields specific to entry screens}
    esLeftD := #0;
    esRightD := #0;
    @esPreEditProc := nil;
    @esPostEditProc := nil;
    @esValidateFunc := nil;
    esInsertMode := True;  {!!.01}
    esPasswordChar := DefPasswordChar;
    esPadChar := DefPadChar;

    asOptions := DefEntryOptions;
    asFieldOptions := DefEFieldOptions;
    esFieldFlags := DefSEFieldOptions;
    esUserRecPtr := nil;
    esUserRecSize := 0;

    esCFF := DefClearFirstFunc;
  end;

  destructor EntryScreen.Done;
    {-Dispose of the entry screen}
  begin
    if ByteFlagIsSet(asOptions, esDeallocUserRec) then
      FreeMemCheck(esUserRecPtr, esUserRecSize);
    AbstractSelector.Done;
  end;

  constructor EntryScreen.esBind;
    {-Force the VMT link to that of an EntryScreen}
  begin
    {calling another constructor without an 'ObjectName.' prefix will change
     the VMT link}
    esBindPrim;
  end;

  constructor EntryScreen.esBindPrim;
    {-Dummy constructor called by esBind to change the VMT link}
  begin
  end;

  procedure EntryScreen.esPreEdit;
    {-Called before each edit}
  begin
    if @esPreEditProc <> nil then
      esPreEditProc(@Self);
  end;

  procedure EntryScreen.esPostEdit;
    {-Called after each edit}
  begin
    if @esPostEditProc <> nil then
      esPostEditProc(@Self);
  end;

  function EntryScreen.esValidate(var EFP : EntryFieldPtr; var ErrCode : Word;
                                  var ErrMsg : StringPtr) : Boolean;
    {-Called before exiting}
  begin
    if @esValidateFunc = nil then
      esValidate := RevalidateAll(EFP, ErrCode, ErrMsg)
    else
      esValidate := esValidateFunc(@Self, EFP, ErrCode, ErrMsg);
  end;

  procedure EntryScreen.esCalcAttrs(EFP : EntryFieldPtr; var FA, CA, PA, DA : Byte);
    {-Determine attributes for fields, control characters, and prompts}
  begin
    with EFP^ do begin
      {do nothing if field is invisible}
      if LongFlagIsSet(sfOptions, sfInvisible) then
        Exit;

      {is field hidden?}
      if LongFlagIsSet(sfOptions, sfHidden) then begin
        FA := ColorMono(wTextColor, wTextMono);   {!!.01}
        PA := FA;
        DA := FA;
        CA := FA;
      end
      else with asColors do begin
        if asFieldIsProtected(EFP) then begin
          FA := ColorMono(sfProFieldColor, sfProFieldMono);
          PA := ColorMono(sfProPromptColor, sfProPromptMono);
          DA := ColorMono(ProDelimColor, ProDelimMono);
        end                      {!!.01}
        else if (sfID = asCurID) or LongFlagIsSet(sfFlags, ifEditing) then begin
          FA := ColorMono(sfSelFieldColor, sfSelFieldMono);
          PA := ColorMono(sfSelPromptColor, sfSelPromptMono);
          DA := ColorMono(SelDelimColor, SelDelimMono);
        end
        else begin
          FA := ColorMono(sfFieldColor, sfFieldMono);
          PA := ColorMono(sfPromptColor, sfPromptMono);
          DA := ColorMono(DelimColor, DelimMono);
        end;

        {determine attribute for control characters}
        CA := ColorMono(sfCtrlColor, sfCtrlMono);
      end;
    end;
  end;

  procedure EntryScreen.esDrawFieldPrim(EFP : EntryFieldPtr; ConvertIt : Boolean);
    {-Redraw the specified field}
  var
    FRow, FCol : Integer;
    PRow, PCol : Integer;
    PFlags : PictureFlags;
    FA, PA, CA, DA : Byte;
    Offset : Integer;         {!!.20}
    Hidden, SemiHidden : Boolean;
    LD, RD : Char;
    HaveP, HaveF : Boolean;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin                {!!.01} {This routine was substantially rewritten}
    if EFP = nil then
      Exit;
    with EFP^ do begin
      {don't convert if editing} {!!.02}
      ConvertIt :=
        ConvertIt and not LongFlagIsSet(sfFlags, ifEditing); {!!.02}

      {don't draw it if field is invisible}
      if LongFlagIsSet(sfOptions, sfInvisible) then
        Exit;

      {adjust coordinates}
      HaveP := asFixCoordinates(sfPRow, sfPCol, sfPWidth, 1, PRow, PCol);
      HaveF := asFixCoordinates(sfFRow, sfFCol, afWidth, sfFHeight, FRow, FCol); {!!.22}
      if not (HaveP or HaveF) then
        Exit;

      {do the conversion if necessary}
      if ConvertIt then
        Convert(False);

      {set up for Draw routine}
      InitPictureFlags(PFlags);

      {is the field hidden or semi-hidden?}
      Hidden := LongFlagIsSet(sfOptions, sfHidden);
      SemiHidden := LongFlagIsSet(sfFlags, ifSemiHidden);

      {determine attributes to use for the field}
      esCalcAttrs(EFP, FA, CA, PA, DA);

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      {draw the prompt}
      if HaveP then
        if Hidden then
          FastFill(sfPWidth, ' ', PRow, PCol, PA)
        else
          FastWrite(efPrompt^, PRow, PCol, PA);

      if HaveF then begin
        {draw field delimiters if desired}
        if (esLeftD <> #0) and (esRightD <> #0) and
           not LongFlagIsSet(sfFlags, ifMultiLine) then begin
             if Hidden then begin
               LD := ' ';
               RD := ' ';
             end
             else begin
               LD := esLeftD;
               RD := esRightD;
             end;
             if (FCol > wXL) or ((FCol > 1) and (VirtualSegment <> VideoSegment)) then
               FastFill(1, LD, FRow, FCol-1, DA);
             if (FCol+afWidth <= wXH) or (VirtualSegment <> VideoSegment) then {!!.22}
               FastFill(1, RD, FRow, FCol+afWidth, DA);                        {!!.22}
           end;

        if SemiHidden and not Hidden then
          {make it look hidden to the Draw routine}
          SetLongFlag(sfOptions, sfHidden);

        {draw the string}
        if (sfID = asCurID) or LongFlagIsSet(sfFlags, ifEditing) then {!!.20}
          Offset := efHOffset                                         {!!.20}
        else                                                          {!!.20}
          Offset := 0;                                                {!!.20}
        Draw(efEditSt^, FRow, FCol, FA, CA, Offset, esPasswordChar, PFlags); {!!.20}

        if SemiHidden and not Hidden then
          {clear the phony hidden flag}
          ClearLongFlag(sfOptions, sfHidden);
      end;

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;
  end;

  procedure EntryScreen.esOptionsOn(OptionFlags : Byte);
    {-Activate multiple options}
  begin
    SetByteFlag(asOptions, OptionFlags and not BadEntryOptions);
  end;

  procedure EntryScreen.esOptionsOff(OptionFlags : Byte);
    {-Deactivate multiple options}
  begin
    ClearByteFlag(asOptions, OptionFlags and not BadEntryOptions);
  end;

  function EntryScreen.esOptionsAreOn(OptionFlags : Byte) : Boolean;
    {-Return true if all specified options are on}
  begin
    esOptionsAreOn := (asOptions and OptionFlags) = OptionFlags;
  end;

  procedure EntryScreen.esFieldOptionsOn(OptionFlags : LongInt);
    {-Activate multiple field options}
  begin
    SetLongFlag(asFieldOptions, OptionFlags and not BadEsFieldOptions);
  end;

  procedure EntryScreen.esFieldOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple field options}
  begin
    ClearLongFlag(asFieldOptions, OptionFlags and not BadEsFieldOptions);
  end;

  function EntryScreen.esFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified field options are on}
  begin
    esFieldOptionsAreOn := (asFieldOptions and OptionFlags) = OptionFlags;
  end;

  procedure EntryScreen.esSecFieldOptionsOn(OptionFlags : LongInt);
    {-Activate multiple secondary field options}
  begin
    SetLongFlag(esFieldFlags, OptionFlags and not BadSEFieldOptions);
  end;

  procedure EntryScreen.esSecFieldOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple secondary field options}
  begin
    ClearLongFlag(esFieldFlags, OptionFlags and not BadSEFieldOptions);
  end;

  function EntryScreen.esSecFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified secondary field options are on}
  begin
    esSecFieldOptionsAreOn := (esFieldFlags and OptionFlags) = OptionFlags;
  end;

  procedure EntryScreen.SetPreEditProc(PEP : esUserProc);
    {-Pointer to routine to display help message just before field is edited}
  begin
    esPreEditProc := PEP;
  end;

  procedure EntryScreen.SetPostEditProc(PEP : esUserProc);
    {-Set pointer to routine to be called after each edit}
  begin
    esPostEditProc := PEP;
  end;

  procedure EntryScreen.SetValidateFunc(UVF : esUserValidationFunc);
    {-Specify routine to call before exiting}
  begin
    esValidateFunc := UVF;
  end;

  procedure EntryScreen.SetClearFirstFilter(CFF : ClearFirstFunc);
    {-Set procedure to call for ClearFirstChar test}
  begin
    esCFF := CFF;
  end;

  procedure EntryScreen.SetPadChar(Ch : Char);
    {-Set character used to pad ends of strings}
  begin
    esPadChar := Ch;
  end;

  procedure EntryScreen.SetPasswordChar(Ch : Char);
    {-Set character used in password mode}
  begin
    esPasswordChar := Ch;
  end;

  procedure EntryScreen.SetBeepOnError(IsOn : Boolean);
    {-Activate/deactivate beeping on error option for all fields}
  var
    SFP : SelectFieldPtr;
  begin
    {set it for the entry screen}
    if IsOn then
      SetLongFlag(asFieldOptions, efBeepOnError)
    else
      ClearLongFlag(asFieldOptions, efBeepOnError);

    {any fields to change the setting for?}
    if asCount > 0 then begin
      SFP := Pointer(asFields.Head);
      while SFP <> nil do begin
        if IsOn then
          SetLongFlag(SFP^.sfOptions, efBeepOnError)
        else
          ClearLongFlag(SFP^.sfOptions, efBeepOnError);
        SFP := Pointer(SFP^.dlNext);
      end;
    end;
  end;

  procedure EntryScreen.SetForceMode(Force, Overtype : Boolean);
    {-Force insert or overtype mode, else use previous setting}
  begin
    if Force then begin
      esFieldOptionsOn(efForceMode);
      if Overtype then
        esFieldOptionsOn(efForceOvertype)
      else
        esFieldOptionsOff(efForceOvertype);
    end
    else
      esFieldOptionsOff(efForceMode+efForceOvertype);
  end;

  procedure EntryScreen.SetDelimiters(Left, Right : Char);
    {-Set field delimiters for the entry screen (null = none)}
  begin
    esLeftD := Left;
    esRightD := Right;
  end;

  procedure EntryScreen.SetDelimAttr(Color, Mono : Byte);
    {-Set attributes for delimiters (unselected)}
  begin
    asColors.SetDelimAttr(Color, Mono);
  end;

  procedure EntryScreen.SetSelectedDelimAttr(Color, Mono : Byte);
    {-Set attributes for delimiters (selected)}
  begin
    asColors.SetSelectedDelimAttr(Color, Mono);
  end;

  procedure EntryScreen.SetProtectedDelimAttr(Color, Mono : Byte);
    {-Set attributes for delimiters (protected)}
  begin
    asColors.SetProtectedDelimAttr(Color, Mono);
  end;

  procedure EntryScreen.asDrawKnownField(SFP : SelectFieldPtr);
    {-Draw a field when its address is known}
  begin
    esDrawFieldPrim(Pointer(SFP), True);
  end;

  procedure EntryScreen.asResetFlags;
    {-Reset internal flags}
  var
    SFP, SFPL : SelectFieldPtr;
    LowCol, HighCol : Word;
    I : Word;
  begin
    {do nothing if first/last row already known}
    if (asMinRow <> 0) and (asMaxRow <> 0) then
      Exit;

    {no fast updates}
    ClearByteFlag(asOptions, esFastUpdates);

    {reset flags for all fields}
    SFP := Pointer(asFields.Head);
    while SFP <> nil do begin
      with SFP^ do begin
        ClearLongFlag(sfFlags, ifNotNext+ifNotPrev);
        if LongFlagIsSet(sfOptions, efAutoAdvanceCursor) then
          SetLongFlag(sfOptions, efAutoAdvanceCursor);
        if LongFlagIsSet(sfOptions, efAutoAdvanceChar) then
          SetLongFlag(sfOptions, efAutoAdvanceChar);
      end;
      SFP := Pointer(SFP^.dlNext);
    end;

    {find the first unprotected field}
    SFP := Pointer(asFields.Head);
    while asFieldIsProtected(SFP) do
      SFP := Pointer(SFP^.dlNext);
    if SFP = nil then begin                       {!!.14}
      GotError(epFatal+ecNoFields, emNullError);  {!!.14}
      Exit;                                       {!!.14}
    end;                                          {!!.14}
    asMinRow := SFP^.sfFRow;

    {find the last unprotected field}
    SFPL := Pointer(asFields.Tail);
    while asFieldIsProtected(SFPL) do
      SFPL := Pointer(SFPL^.dlPrev);
    asMaxRow := SFPL^.sfFRow;

    if (asWrapMode = StopAtEdges) or
       ((asWrapMode = WrapAtEdges) and (SFP = SFPL)) then
      with SFP^ do begin
        ClearLongFlag(sfOptions, efAutoAdvanceBegin);
        SetLongFlag(sfFlags, ifNotPrev);
      end;

    if (asWrapMode = StopAtEdges) or
       ((asWrapMode = WrapAtEdges) and (SFP = SFPL)) then
      with SFPL^ do begin
        ClearLongFlag(sfOptions, efAutoAdvanceEnd);
        SetLongFlag(sfFlags, ifNotNext);
      end;

    {do nothing if min/max column already known}
    if (asMinCol <> 0) and (asMaxCol <> 0) then begin
      {$IFDEF UseScrollBars}
      {set up for scroll bars and exit}
      asSetupForScrollBars;
      {$ENDIF}
      Exit;
    end;

    LowCol := $FFFF;
    HighCol := 0;
    SFP := Pointer(asFields.Head);
    while SFP <> nil do
      with SFP^ do begin
        {check field}
        LowCol := MinWord(LowCol, sfFCol);
        HighCol := MaxWord(HighCol, sfFCol);

        {follow link}
        SFP := Pointer(SFP^.dlNext);
      end;

    asMinCol := LowCol;
    asMaxCol := HighCol;

    {$IFDEF UseScrollBars}
    {setup for scroll bars}
    asSetupForScrollBars;
    {$ENDIF}
  end;

  procedure EntryScreen.PositionCursorAt(SFP : SelectFieldPtr);
    {-Puts the cursor on the beginning of the specified field}
  var
    Row, Col : Integer;
    StBgn : Byte;
    Flags : PictureFlags;
    SaveCurrent : SelectFieldPtr;
  begin
    with EntryFieldPtr(SFP)^ do begin
      {make sure the field is on the screen}
      SaveCurrent := asCurrent;
      asCurrent := SFP;
      asFixWindow(True, ByteFlagIsSet(asOptions, esScrollByPage));
      asCurrent := SaveCurrent;

      if asFixCoordinates(sfFRow, sfFCol, afWidth, sfFHeight, Row, Col) then begin {!!.22}
        {find the first non-literal}
        InitPictureFlags(Flags);
        for StBgn := 1 to MaxWord(1, Length(efPicture^)) do  {!!.03}
          if Flags[StBgn] then begin
            GoToXYabs(Col+Pred(StBgn), Row);
            {$IFDEF UseScrollBars}
            asUpdateScrollBars;
            {$ENDIF}
            Exit;
          end;
      end;

      GotError(epFatal+ecFieldNotFound, emNullError);
    end;
  end;

  function EntryScreen.esRequiredFieldsOK(var EFP : EntryFieldPtr;
                                          var ErrCode : Word;
                                          var ErrMsg : StringPtr) : Boolean;
    {-Make sure all required fields are filled in}
  var
    SFP : SelectFieldPtr absolute EFP;
  begin
    {start with first unprotected field}
    SFP := Pointer(asFields.Head);
    while (SFP <> nil) do begin
      with EFP^ do                                     {!!.01}
        {is it a required field?}                      {!!.01}
        if LongFlagIsSet(sfOptions, efRequired) then   {!!.01}
          {check only unprotected fields}              {!!.01}
          if not asFieldIsProtected(SFP) then          {!!.01}
            {it can't be empty}                        {!!.01}
            if efFieldIsEmpty then begin               {!!.01}
              esRequiredFieldsOK := False;
              ErrCode := epWarning+ecFieldRequired;
              ErrMsg := @emFieldRequired;
              Exit;
            end;

      {next field}
      SFP := Pointer(SFP^.dlNext);
    end;

    {restore current field}
    esRequiredFieldsOK := True;
  end;

  {$IFDEF UseMouse}
  function EntryScreen.asMouseOnField(SFP : SelectFieldPtr;
                                    Row, Col : Integer) : Boolean;
    {-Returns True if the mouse cursor is on this field}
  var
    HD      : Byte;
  begin
    HD := Ord((esLeftD <> #0) and (esRightD <> #0));
    with SFP^ do
      {check the location of both the prompt and the field}
      asMouseOnField :=
        asMouseOnFieldPrim(Row, Col, sfPRow, sfPCol, sfPWidth, 1) or
        asMouseOnFieldPrim(Row, Col, sfFRow, sfFCol-HD, afWidth+HD+HD, sfFHeight); {!!.22}
  end;

  function EntryScreen.esEvaluateMouseCommand(var Cmd : Word;
                                              var ScrollByPage : Boolean;
                                              Execute : Boolean) : Boolean;
    {-Evaluate ccMouseSel command. Parameters:
        Cmd may be changed to ccClickExit or ccNested on return.
        ScrollByPage may be forced to True on return.
        Execute should be False if goal is only to evaluate the command.
     }
  var
    esCurrent : EntryFieldPtr absolute asCurrent;
  begin
    esEvaluateMouseCommand := False;
    with esCurrent^ do
      case asProcessMouseCommand(Cmd, ScrollByPage) of {!!.03}
        1 : esEvaluateMouseCommand := True;
        2 : {user clicked on current field}
            if (Cmd = ccMouseSel) then                    {!!.03}
              if (not (ByteFlagIsSet(asOptions, esReadOnly) or LongFlagIsSet(sfOptions, efReadOnly))) and {!!.02}
                 (LongFlagIsSet(sfFlags, ifBoolean) or LongFlagIsSet(sfOptions, efMultChoice)) then begin {!!.02}
                   if Execute then begin  {!!.03}
                    {increment the value}
                    efIncrement;
                    Convert(False);       {!!.03}
                   end                    {!!.03}
                  else
                    {indicate that the value will be incremented}
                    Cmd := ccIncChoice;
              end
              else if LongFlagIsSet(sfOptions, efClickExit) then begin
                Cmd := ccClickExit;
                esEvaluateMouseCommand := True;
              end
              else if LongFlagIsSet(sfFlags, ifNested) then begin
                Cmd := ccNested;
                esEvaluateMouseCommand := True;
              end;
        3 : {user clicked on another field}
            if LongFlagIsSet(asNext^.sfFlags, ifNested) then begin
              {jump right to the nested field and exit}
              if (Cmd = ccMouseSel) then                  {!!.12}
                Cmd := ccNested;
              if Execute then begin
                asCurrent := asNext;
                asCurID := asNext^.sfID;
              end;
              esEvaluateMouseCommand := True;               {!!.11}
                {(Cmd = ccMouseSel) or (Cmd = ccNested);}   {!!.11}
            end;
      end;
  end;
  {$ENDIF}

  procedure EntryScreen.ProcessSelf; {!!.03} {numerous changes}
    {-Process data entry commands}
  var
    esCurrent : EntryFieldPtr absolute asCurrent;
    EFP : EntryFieldPtr;
    ErrCode : Word;
    ErrMsg : StringPtr;
    Finished, Stop, OK : Boolean;
    Dummy, Modified : Boolean;
    ScrollByPage : Boolean;
    PosCode : Byte;
    Row, Col : Integer;
    FA, CA, PA, DA : Byte;
    Cmd, SaveCmd : Word;
    SaveCurrent : EntryFieldPtr;
    SaveCFF : ClearFirstFunc;
    SaveFlags : LongInt;            {!!.11}
  label
    TopOfLoop, ExitPoint;

    function ReadOnly : Boolean;
    begin
      ReadOnly := ByteFlagIsSet(asOptions, esReadOnly) or
                  LongFlagIsSet(esCurrent^.sfOptions, efReadOnly);
    end;

  begin
    {do nothing if there are no fields}
    if asCount = 0 then begin
      GotError(epFatal+ecNoFields, emNullError);
      Exit;
    end;

    { establish default visitation order }        {!!.30}
    asEstablishDefaultLinks;                      {!!.30}

    {make sure asNext <> nil}
    if asNext = nil then
      if asCurrent = nil then
        asNext := Pointer(asFields.Head)
      else
        asNext := asCurrent;
    asCurrent := asNext;

    {don't start on a protected field}
    if not asNextFieldOK then begin               {!!.13}
      GotError(epFatal+ecNoFields, emNullError);  {!!.13}
      Exit;
    end;                                          {!!.13}

    {draw everything}
    asCurID := BadFieldID;

    {Draw initial screen if not already done}
    Draw;
    if RawError <> 0 then
      Exit;

    {set read-only and scroll-by-page flags}
    ScrollByPage := ByteFlagIsSet(asOptions, esScrollByPage);

    Finished := False;
    SaveCurrent := nil;
    repeat

TopOfLoop:
      {make sure the next field is OK}
      if asNext = nil then
        asNext := asCurrent
      else if not asNextFieldOK then begin
        GotError(epFatal+ecNoFields, emNullError);
        goto ExitPoint;
      end;

      {reset current field}
      asCurrent := asNext;
      asCurID := asCurrent^.sfID;
      asNext := nil;

      with esCurrent^ do begin
        {make sure current field is on the screen}
        asFixWindow(True, ScrollByPage);

        {reset ScrollByPage}
        ScrollByPage := ByteFlagIsSet(asOptions, esScrollByPage);

        {update entire screen if necessary}
        if not ByteFlagIsSet(asOptions, esFastUpdates) then
          UpdateContents;

        {do the conversion}
        Convert(False);

        repeat
          {}
          SaveCmd := cwCmd;
          cwCmd := ccNone;

          {call the pre-edit routine}
          esPreEdit;

          {make sure asNext wasn't changed}
          if asNext <> nil then
            goto TopOfLoop;

          {set position code if necessary}
          if ByteFlagIsSet(asOptions, esSamePosition) then begin
            ClearByteFlag(asOptions, esSamePosition);
            PosCode := 4;
          end
          else if SaveCurrent = esCurrent then
            PosCode := 4
          else case SaveCmd of
            ccLeft :
              PosCode := 1;
            ccBackTab :
              PosCode := 2;
            ccWordLeft :
              PosCode := 3;
            ccUp :
              if sfFHeight > 1 then
                PosCode := 1
              else
                PosCode := 0;
            else
              PosCode := 0;
          end;
          SaveCurrent := esCurrent;

          {draw the current field}
          esDrawFieldPrim(esCurrent, False);

          {position cursor on the current field}
          if PosCode <> 4 then
            PositionCursorAt(asCurrent);

          {get video attributes for this field}
          esCalcAttrs(esCurrent, FA, CA, PA, DA);

          {get screen coordinates for the current field}
          if asFixCoordinates(sfFRow, sfFCol, afWidth, sfFHeight, Row, Col) then {}; {!!.22}

          {call the editor for the current field}
          SaveCFF := feClearFirstFunc;
          feClearFirstFunc := esCFF;
          Modified := False;
          repeat
            Stop := True;
            SaveFlags := sfFlags;                           {!!.11}
            ClearLongFlag(sfFlags, ifSemiHidden);           {!!.11}
            Edit(
              Row, Col, FA, CA, esPasswordChar, PosCode, ReadOnly,
              cwCmd, cwKey, esInsertMode, cwErrorProc, cwUnitCode, cwCmdPtr^);
            if LongFlagIsSet(SaveFlags, ifSemiHidden) then  {!!.11}
              SetLongFlag(sfFlags, ifSemiHidden);           {!!.11}
           {$IFDEF UseMouse}
           case cwCmd of
            ccMouseAuto,          {!!.13}
            ccMouseSel, ccMouseDown :
              if (cwCmdPtr^.MouseEnabled) then begin
                Cmd := cwCmd;
                Stop := esEvaluateMouseCommand(Cmd, Dummy, False) or {!!.12}
                        (Cmd = ccIncChoice) or                       {!!.12}
                        ((asNext <> asCurrent) and (asNext <> nil));
                if Stop and (asNext <> nil) then                     {!!.20}
                  if LongFlagIsSet(sfOptions, efRequired) and        {!!.20}
                    efFieldIsEmpty then begin                        {!!.20}
                      {display error message}                        {!!.20}
                      if Cmd <> ccMouseAuto then                     {!!.20}
                        GotError(epWarning+ecFieldRequired, emFieldRequired); {!!.20}
                      ClearErrors;                                   {!!.20}
                      Stop := False;                                 {!!.20}
                    end;                                             {!!.20}
                if not Stop then begin
                  cwCmd := ccNone;
                  PosCode := 4;
                end;
              end;
            end;
           {$ENDIF}
            Modified := Modified or LongFlagIsSet(sfFlags, ifModified);
          until Stop;
          feClearFirstFunc := SaveCFF;

          {do the conversion if the field was modified}
          if Modified then begin
            SetLongFlag(sfFlags, ifModified);
            {trim leading and trailing blanks if desired}
            if LongFlagIsSet(sfOptions, efTrimBlanks) then
              efTrimSpaces;
            Convert(True);
          end;

          {if Escape not pressed, validate and convert}
          OK := True;
          if (cwCmd <> ccQuit) and (not ReadOnly) then begin
            {if it's a required field, it can't be empty}
            case cwCmd of
              ccNested,
              ccClickExit,
              ccMouseAuto,          {!!.13}
              ccMouseDown,
              ccMouseSel,
              ccAltKey,             {!!.30}
              ccUser0..ccUser65335 :
                {don't check} ;
              else
                if LongFlagIsSet(sfOptions, efRequired) and efFieldIsEmpty then begin
                  {display error message}
                  GotError(epWarning+ecFieldRequired, emFieldRequired);
                  ClearErrors;  {!!.20}
                  OK := False;
                end;
            end;
          end;

          {redraw the field}
          DrawField(asCurID);
        until OK;

        {call post-edit routine}
        esPostEdit;

        {special case if only one field}
        if asCount = 1 then
          case cwCmd of
            ccDown,
            ccTab,
            ccWordRight,
            ccRight :
              cwCmd := ccExitAtBot;
            ccUp,
            ccBackTab,
            ccPrevField,
            ccWordLeft,
            ccLeft :
              cwCmd := ccExitAtTop;
          end;

        {move from field to field if appropriate}
        case cwCmd of
          ccUp :
            asUpField;

          ccDown :
            asDownField;

          ccLeft,
          ccWordLeft :
            asGotoPrevField;

          ccBackTab,
          ccPrevField :
            if not asIDisValid(sfPrevID) then
              (*asGotoPrevField*)    {!!.30}
              asGotoPrevFieldVisit;  {!!.30}

          ccRight,
          ccWordRight :
            asGotoNextField;

          ccSelect,
          ccTab,
          ccNextField,
          ccAutoAdvance :
            if not asIDisValid(sfNextID) then
              (*asGotoNextField;*)   {!!.30}
              asGotoNextFieldVisit;  {!!.30}

          ccFirstFld :
            asSeekToFirst;

          ccLastFld :
            asSeekToLast;

          ccPageUp :
            begin
              asPageUpOrDown(-1);
              ScrollByPage := True;
            end;

          ccPageDn :
            begin
              asPageUpOrDown(+1);
              ScrollByPage := True;
            end;

          ccNextRec,
          ccPrevRec,
          ccNested,
          ccDone,
          ccQuit,
          ccAltKey, {!!.30}
          ccUser0..ccUser65335 :
            Finished := True;

          {$IFDEF UseMouse}
          ccMouseAuto,                                                 {!!.13}
          ccMouseDown,
          ccMouseSel :          {user clicked left mouse button}
            if cwCmdPtr^.MouseEnabled then
              Finished := esEvaluateMouseCommand(cwCmd, ScrollByPage, True)
                and (cwCmd <> ccMouseAuto);                            {!!.13}
          {$ENDIF}
        end;
      end;

      {if WrapMode = ExitAtEdges, ccExitAtTop and ccExitAtBot are exit commands}
      case cwCmd of
        ccExitAtTop,
        ccExitAtBot :
          Finished := True;
      end;

      {if we're exiting, make sure all required fields are filled in}
      if Finished then
        case cwCmd of
          ccMouseDown,             {!!.11}
          ccMouseSel,              {!!.11}
          ccNested,
          ccQuit,
          ccClickExit,
          ccAltKey,                {!!.30}
          ccUser0..ccUser65335 :
            {don't check} ;
          else if not (esRequiredFieldsOK(EFP, ErrCode, ErrMsg) and
                       esValidate(EFP, ErrCode, ErrMsg)) then
            begin
              {put the cursor on the field that caused the error}
              PositionCursorAt(EFP);

              {update screen}
              if not ByteFlagIsSet(asOptions, esFastUpdates) then
                UpdateContents;

              {display error message}
              GotError(ErrCode, ErrMsg^);
              ClearErrors;

              {move to the field with the error}
              asNext := EFP;

              {don't exit}
              Finished := False;
            end;
        end;

    until Finished or (cwCmd = ccError);

ExitPoint:
    {save window state}
    rwSaveWindowState;
  end;

begin
  {initialize command processor}
  EntryCommands.Init(@EntryKeySet, EntryKeyMax);
end.
