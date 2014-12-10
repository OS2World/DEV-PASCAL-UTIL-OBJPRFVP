{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$R-,S-,I-,V-,B-,F+,O+,A-} {!!.01}

{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPSELECT.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}


unit OpSelect;
  {-Selection windows}

interface

uses
  Use32,
  Dos,
  OpConst,    {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd,
  OpAbsFld,
  (*OpField,*)      {!!.30}
  OpFrame,
  OpWindow
  {$IFDEF UseDrag}  {!!.03}
  , OpDrag          {!!.03}
  {$ENDIF}          {!!.03}
  ;

  {$I OPSELECT.ICD}  {configuration data}

{!!.30 Moved from OPFIELD.PAS}
const
  BadFieldID = $FFFF; {illegal field ID value}

{!!.30 Moved from OPFIELD.PAS}
const
  {option codes for select fields}
  sfProtected        = $00000001; {field is protected}
  sfHidden           = $00000002; {field is hidden}
  sfMapCtrls         = $00000004; {map control characters?}
  sfIncDec           = $00000008; {field can be incremented/decremented}
  sfInvisible        = $00000010; {if set, field is invisible}

  DefSFieldOptions : LongInt = sfMapCtrls;
  BadSFieldOptions : LongInt = sfProtected+sfHidden+sfInvisible;

  {internal flags--stored with secondary options}
  ifBoolean          = $00000001; {a boolean or yes/no field}
  ifNumeric          = $00000002; {edit from right to left--for numbers only}
  ifSimple           = $00000004; {simple field}
  ifNested           = $00000008; {if set, field has a nested form assoc. with it}
  ifMultiLine        = $00000010; {multi-line field}
  ifModified         = $00000020; {marks fields modified while editing}
  ifNotNext          = $00000040; {disallow ccNextField command}
  ifNotPrev          = $00000080; {disallow ccPrevField command}
  ifNoMouseExit      = $00000100; {don't allow ccMouseSel to exit field (multi-line only)}
  ifSuppressFirst    = $00000200; {don't display field first time--for simple character editor only}
  ifNoLiterals       = $00000400; {set if picture mask has no literal characters}
  ifEditing          = $00000800; {set when field is being edited}
  ifHexadecimal      = $00001000; {if set, field's value is shown in hex}
  ifSemiHidden       = $00002000; {if set, field is semi-hidden}
  ifRealVar          = $00004000; {if set, field is of a real/BCD/8087 real}

  DefSEFieldOptions  : LongInt = 0;
  BadSEFieldOptions  : LongInt = ifBoolean+ifNumeric+ifSimple+ifNested+
                                 ifMultiLine+ifModified+ifNotNext+ifNotPrev+
                                 ifNoMouseExit+ifSuppressFirst+ifNoLiterals+
                                 ifEditing+ifHexadecimal+ifSemiHidden+ifRealVar;

{.Z-}
{!!.30 Moved from OPFIELD.PAS}
type
  SelectFieldPtr = ^SelectField;
  SelectField =
    object(AbstractField)
      {... double list stuff ...}
      sfID             : SmallWord;    {field ID--assigned automatically}
      sfNextID         : SmallWord;    {.$FFFF, or ID of field to move to on <Tab>}
      sfPrevID         : SmallWord;    {.$FFFF, or ID of field to move to on <ShTab>}
      {----------------------------Screen coordinates}
      sfPRow           : SmallWord;    {for prompt}
      sfPCol           : SmallWord;
      sfPWidth         : Byte;
      sfFRow           : SmallWord;    {for field}
      sfFCol           : SmallWord;
      sfFWidth         : Byte;    {width of the field}
      sfFHeight        : Byte;    {height of the field}
      sfFCPos          : Byte;    {current column position within the field}
      sfFRPos          : Byte;    {current row position within the field}
      {----------------------------Colors}
      sfPromptColor    : Byte;    {for unselected prompt}
      sfPromptMono     : Byte;
      sfSelPromptColor : Byte;    {for selected prompt}
      sfSelPromptMono  : Byte;
      sfProPromptColor : Byte;    {for protected prompt}
      sfProPromptMono  : Byte;
      sfFieldColor     : Byte;    {for unselected field}
      sfFieldMono      : Byte;
      sfSelFieldColor  : Byte;    {for selected field}
      sfSelFieldMono   : Byte;
      sfProFieldColor  : Byte;    {for protected field}
      sfProFieldMono   : Byte;
      sfCtrlColor      : Byte;    {for control characters}
      sfCtrlMono       : Byte;
      {----------------------------Options}
      sfOptions        : LongInt; {primary option settings}
      sfFlags          : LongInt; {secondary option settings}
      sfHelpIndex      : SmallWord;    {index to pass to help routine}
      {----------------------------Other}
      sfFieldName    : StringPtr; {for internal use}
      {....methods....}
      procedure sfOptionsOn(OptionFlags : LongInt);
        {-Activate multiple options}
      procedure sfOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple options}
      function sfOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified options are on}
      {...}
      procedure SetPromptAttr(Color, Mono : Byte);
        {-Set attributes for prompts}
      procedure SetSelectedPromptAttr(Color, Mono : Byte);
        {-Set attributes for selected prompts}
      procedure SetProtectedPromptAttr(Color, Mono : Byte);
        {-Set attributes for protected prompts}
      procedure SetFieldAttr(Color, Mono : Byte);
        {-Set attributes for unselected fields}
      procedure SetSelectedFieldAttr(Color, Mono : Byte);
        {-Set attributes for selected fields}
      procedure SetProtectedFieldAttr(Color, Mono : Byte);
        {-Set attributes for protected fields}
      procedure SetCtrlAttr(Color, Mono : Byte);
        {-Set attributes for control characters}
{.Z+}
      {+++ internal methods +++}
      function afWidth : Byte; virtual; {!!.22}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
    {$ENDIF}
      constructor Init(FieldID : Word; PromptRow, PromptCol : Word;
                       PromptWidth : Byte; FieldRow, FieldCol : Word;
                       fWidth, fHeight : Byte; HlpNdx : Word;
                       Options, IFlags : LongInt; var Colors : ColorSet);
      destructor Done; virtual;
      function sfSetFieldName(S : string) : Boolean;
{.Z-}
    end;

{.Z+}
{!!.30 Moved from OPFIELD.PAS}
const                                                               {!!.03}
  tfLine             = $0001;     {set for line fields}             {!!.03}
  tfVertical         = $0002;     {if set, line field is vertical}  {!!.03}
  tfBox              = $0004;     {set for box fields}              {!!.03}
  tfShadowed         = $0008;     {if set, box field is shadowed}   {!!.03}
  tfMarkEnd          = $8000;     {used internally by MAKESCRN}     {!!.03}

type
  {!!.30 Moved from OPFIELD.PAS}
  TextFieldPtr = ^TextField;
  TextField =
    object(SingleListNode)
      {...single list stuff...}
      tfID           : SmallWord;      {field ID--assigned automatically}
      tfFlags        : SmallWord;      {flags}
      tfRow, tfCol   : SmallWord;      {coordinates for the text}
      tfColorAttrs   : FlexAttrs; {colors}
      tfMonoAttrs    : FlexAttrs;
      tfString       : StringPtr; {text string}
      {...methods...}
      constructor Init(FieldID : Word; var St : String; Row, Col : Word;
                       var ColorAttrs, MonoAttrs : FlexAttrs);
        {-Called when field is created}
      destructor Done; virtual;
        {-Called when field is destroyed}
      procedure Draw(XDelta, YDelta : Byte); virtual;
        {-Draw the field}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a text field from a stream}
      procedure Store(var S : IdStream);
        {-Store a text field in a stream}
    {$ENDIF}
      {+++ internal methods +++}
      function tfWidth : Word; virtual;   {!!.03}
        {-Return the width of the text field}
      function tfHeight : Word; virtual;  {!!.03}
        {-Return the height of the text field}
    end;

  {!!.30 Moved from OPFIELD.PAS}
  TextLineFieldPtr = ^TextLineField;      {!!.03}
  TextLineField =                         {!!.03}
    object(TextField)
      tlfLength : SmallWord;

      constructor Init(FieldID : Word; Ch1, Ch2, Ch3 : Char;
                       Row, Col, Length : Word; Color, Mono : Byte;
                       Vertical : Boolean);
        {-Create horizontal or vertical line field}
      procedure Draw(XDelta, YDelta : Byte); virtual;
        {-Draw the field}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a line field from a stream}
      procedure Store(var S : IdStream);
        {-Store a line field in a stream}
    {$ENDIF}
      {+++ internal methods +++}
      function tfWidth : Word; virtual;
        {-Return the width of the line field}
      function tfHeight : Word; virtual;
        {-Return the height of the line field}
    end;

  {!!.30 Moved from OPFIELD.PAS}
  TextBoxFieldPtr = ^TextBoxField;      {!!.03}
  TextBoxField =                        {!!.03}
    object(TextField)
      tbfRowH, tbfColH : SmallWord;

      constructor Init(FieldID : Word; BoxChars : FrameArray;
                       XL, YL, XH, YH : Word; Color, Mono : Byte);
        {-Create box field}
      constructor AddShadow(SColor, SMono : Byte);
        {-Add a shadow to the box field}
      procedure Draw(XDelta, YDelta : Byte); virtual;
        {-Draw the field}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a text field from a stream}
      procedure Store(var S : IdStream);
        {-Store a text field in a stream}
    {$ENDIF}
      {+++ internal methods +++}
      function tfWidth : Word; virtual;
        {-Return the width of the text field}
      function tfHeight : Word; virtual;
        {-Return the height of the text field}
    end;

{.F-}
const
  {!!.30 Moved from OPFIELD.PAS}
  IsOn           = True;
  IsOff          = False;

  {!!.30 Moved from OPFIELD.PAS}
  {option codes for selectors}
  slScrollbyPage   = $01; {scroll by page?}
  slMousePage      = $02; {clicking on scroll bar scrolls by page}
  slUpdateActive   = $10; {used internally} {!!.01}
  slAutoSelect     = $20; {auto-select current field?}
  slFastUpdates    = $80; {used internally}

  DefSelectOptions : Byte = slMousePage;
  BadSelectOptions : Byte = slFastUpdates+slUpdateActive; {!!.01}

type
  {!!.01}
  WrapMode = (StopAtEdges, WrapAtEdges, ExitAtEdges, ExitAtTop, ExitAtBot);

const
  DefWrapMode : WrapMode = WrapAtEdges;

type
  AbstractSelectorPtr = ^AbstractSelector;
  ScreenUpdateProc = procedure (ASP : AbstractSelectorPtr);
  asFieldProc = procedure(SFP : SelectFieldPtr; var D; ASP : AbstractSelectorPtr);
  asTextFieldProc = procedure(TFP : TextFieldPtr; var D; ASP : AbstractSelectorPtr);
  AbstractSelector =
    object(CommandWindow)
      {----------------------------Field stuff}
      asFields       : DoubleList; {list of fields}
      asTextFields   : SingleList; {list of text fields}
      asCurrent      : SelectFieldPtr; {pointer to current field}
      asNext         : SelectFieldPtr; {pointer to field to move to}
      asKnown        : SelectFieldPtr; {last field found by FindField}
      asCurID        : SmallWord;      {ID of current field}
      asCount        : SmallWord;      {number of fields}
      asMinRow       : SmallWord;      {min/max row coordinates}
      asMaxRow       : SmallWord;
      asMinCol       : SmallWord;      {min/max column coordinates}
      asMaxCol       : SmallWord;
      {----------------------------Options}
      asOptions      : Byte;      {option flags}
      asFieldOptions : LongInt;   {option flags for fields}
      asWrapMode     : WrapMode;  {wrap mode}
      asColors       : ColorSet;  {color set}
      asUpdateProc   : ScreenUpdateProc; {called when entire screen redrawn}
      {....methods....}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             var CP : CommandProcessor;
                             UnitCode : Byte);
        {-Initialize an abstract selector with custom window options}
      destructor Done; virtual;
        {-Deallocate field list, screen buffers}
      procedure DrawField(ID : Word);
        {-Draw the specified field}
      procedure ResetScreen;
        {-Call when contents of field(s) changed without calling DrawField}
      {----------------------------Selector options}
      procedure SetWrapMode(WM : WrapMode); virtual; {!!.03}
        {-Set the wrap mode for the selector}
      procedure SetScreenUpdateProc(SUP : ScreenUpdateProc);
        {-Set procedure to be called after a complete screen update}
      {----------------------------Colors}
      procedure SetPromptAttr(Color, Mono : Byte);
        {-Set attributes for prompts}
      procedure SetSelectedPromptAttr(Color, Mono : Byte);
        {-Set attributes for selected prompts}
      procedure SetProtectedPromptAttr(Color, Mono : Byte);
        {-Set attributes for protected prompts}
      procedure SetFieldAttr(Color, Mono : Byte);
        {-Set attributes for unselected fields}
      procedure SetSelectedFieldAttr(Color, Mono : Byte);
        {-Set attributes for selected fields}
      procedure SetProtectedFieldAttr(Color, Mono : Byte);
        {-Set attributes for protected fields}
      procedure SetCtrlAttr(Color, Mono : Byte);
        {-Set attributes for control characters}
      procedure ChangePromptAttr(ID : Word; Color, Mono : Byte);
        {-Change attributes for prompts}
      procedure ChangeSelectedPromptAttr(ID : Word; Color, Mono : Byte);
        {-Change attributes for selected prompts}
      procedure ChangeProtectedPromptAttr(ID : Word; Color, Mono : Byte);
        {-Change attributes for protected prompts}
      procedure ChangeFieldAttr(ID : Word; Color, Mono : Byte);
        {-Change attributes for unselected fields}
      procedure ChangeSelectedFieldAttr(ID : Word; Color, Mono : Byte);
        {-Change attributes for selected fields}
      procedure ChangeProtectedFieldAttr(ID : Word; Color, Mono : Byte);
        {-Change attributes for protected fields}
      procedure ChangeCtrlAttr(ID : Word; Color, Mono : Byte);
        {-Change attributes for control characters}
      {----------------------------Text fields}
      procedure AddTextField(St : string; Row, Col : Word);
        {-Add a text field}
      procedure AddCenteredTextField(St : string; Row : Word); {!!.03}
        {-Add a text field, centered within the window}
      procedure AddTextFieldCustom(St : string; Row, Col : Word; Color, Mono : Byte);
        {-Add a text field with specific colors}
      procedure AddTextFieldDeluxe(St : string; Row, Col : Word;
                                   var ColorAttrs, MonoAttrs : FlexAttrs);
        {-Add a text field with special colors}
      procedure AddLineField(Ch1, Ch2, Ch3 : Char;      {!!.03}
                             Row, Col, Length : Word;
                             Vertical : Boolean);
        {-Add a line field}
      procedure AddLineFieldCustom(Ch1, Ch2, Ch3 : Char;    {!!.03}
                                   Row, Col, Length : Word;
                                   Color, Mono : Byte; Vertical : Boolean);
        {-Add a line field with specific colors}
      procedure AddBoxField(BoxChars : FrameArray;                 {!!.03}
                            XL, YL, XH, YH : Word);
        {-Add a box field}
      procedure AddBoxFieldCustom(BoxChars : FrameArray;           {!!.03}
                            XL, YL, XH, YH : Word; Color, Mono : Byte);
        {-Add a box field with specific colors}
      procedure AddShadowedBoxField(BoxChars : FrameArray;         {!!.03}
                                    XL, YL, XH, YH : Word);
        {-Add a shadowed box field}
      procedure AddShadowedBoxFieldCustom(BoxChars : FrameArray;   {!!.03}
                                          XL, YL, XH, YH : Word;
                                          BColor, BMono, SColor, SMono : Byte);
        {-Add a shadowed box field with specific colors}
      {----------------------------Miscellaneous}
      function FindField(ID : Word) : SelectFieldPtr;
        {-Return a pointer to the specified field}
      function FindFieldByName(Name : string) : SelectFieldPtr; {!!.01}
        {-Return a pointer to the field associated with Name (case-insensitive)}
      function FindTextField(ID : Word) : TextFieldPtr;
        {-Return a pointer to the specified text field}
      procedure SetNextField(ID : Word);
        {-Set value of asNext}
      function GetCurrentID : Word;
        {-Get the ID for the current field}
      function GetHelpIndex(ID : Word) : Word;
        {-Get the help index for the specified field}
      procedure SetAllFieldLinks(var LinksMap);
        {-Set the forward and backward links for all fields}
      procedure SetFieldLinks(ID, Next, Prev : Word);
        {-Specify the fields to jump to when <Tab> or <ShTab> pressed on a
          given field}
      procedure ChangeProtection(ID : Word; IsOn : Boolean);
        {-Change the protection status of the specified field}
      procedure ChangeHidden(ID : Word; IsOn : Boolean);
        {-Change the hidden status of the specified field}
      procedure ChangeInvisibility(ID : Word; IsOn : Boolean);
        {-Change the invisibility status of the specified field}
      procedure PositionCursorAt(SFP : SelectFieldPtr); virtual;
        {-Puts the cursor on the beginning of the specified field}
      procedure DrawBackground; virtual;                              {!!.03}
        {-Hook to give opportunity to draw background beneath fields} {!!.03}
      {...}
      procedure VisitAllSelectFields(FProc : asFieldProc; var D);
        {-Call the specified procedure for all fields in the selector}
      procedure VisitAllTextFields(FProc : asTextFieldProc; var D);
        {-Call the specified procedure for all text fields in the selector}
{.Z+}
      {+++ internal methods +++}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
    {$ENDIF}
      procedure UpdateContents; virtual;
      procedure asUpdate; virtual;
      function  asFixCoordinates(InRow, InCol : Word; Wid, Ht : Byte;
                                 var OutRow, OutCol : Integer) : Boolean; virtual;
      procedure asDrawKnownField(SFP : SelectFieldPtr); virtual;
      procedure asDrawAllTextFields(AdjustForWindow : Boolean);
      procedure asFixWindow(Redraw, ScrollByPage : Boolean); virtual;
      procedure asPageUpOrDown(Delta : Integer); virtual;
      {$IFDEF UseMouse}
      function  asMouseOnFieldPrim(Row, Col : Integer;
                                   StartRow, StartCol : Integer;
                                   fWidth, fHeight : Byte) : Boolean;
      function  asMouseOnField(SFP : SelectFieldPtr;
                               Row, Col : Integer) : Boolean; virtual;
      function  asProcessMouseCommand(Cmd : Word;  {!!.03}
                                      var ScrollByPage : Boolean) : Byte; virtual; {!!.13}
      {$ENDIF}
      function  asFieldIsProtected(SFP : Pointer) : Boolean;
      function  asIDisValid(ID : Word) : Boolean;
      function  asNextFieldOK : Boolean;
      procedure asSeekToFirst;
      procedure asSeekToLast;
      procedure asWrapAtTop;
      procedure asWrapAtBottom;
      procedure asEstablishDefaultLinks; {!!.30}
      procedure asGotoPrevField;
      procedure asGotoPrevFieldVisit;    {!!.30}
      procedure asGotoNextField;
      procedure asGotoNextFieldVisit;    {!!.30}
      function  asFindBestField(Col : Word) : Boolean;
      procedure asMoveCursorRight;
      procedure asMoveCursorLeft;
      procedure asScrollHorizontally(TargetCol : Integer);
      procedure asSeekTo(Row : Integer; GoForward : Boolean);
      procedure asScrollVertically(TargetRow : Integer);
      procedure asUpField;
      procedure asDownField;
      procedure asHomeField;
      procedure asEndField;
      {$IFDEF UseScrollBars}
      procedure asSetupForScrollBars; virtual;
      procedure asUpdateScrollBars; virtual;
      {$ENDIF}
      procedure asResetFlags; virtual;
      function  asMaximumRow : Word; virtual;
      function  asMaximumCol : Word; virtual;
      function  asCoordsOK(Row, Col, Wid, Ht : Word) : Boolean; {!!.03}
{.Z-}
    end;

  SelectorPtr = ^Selector;
  GetFieldProc = procedure(ID : Word; NeedPrompt : Boolean; var S : string);
  SelectProc = procedure(SP : SelectorPtr; ID : Word);
  Selector =
    object(AbstractSelector)
      slGetFieldProc   : GetFieldProc; {call to get strings to display}
      slActionProc     : SelectProc;  {call when <Enter> pressed}
      slPreSelectProc  : SelectProc;  {call when moving to a new field}
      slPostSelectProc : SelectProc;  {call when moving from it to another}
      {....methods....}
      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the selector}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt);
        {-Initialize a selector with custom window options}
      procedure ProcessSelf; virtual; {!!.01}
        {-Process selection commands}
      procedure AddField(PromptRow, PromptCol : Word;
                         PromptWidth : Byte;
                         FieldRow, FieldCol : Word;
                         FieldWidth : Byte;
                         HelpIndex : Word);
        {-Add a field with the specified coordinates and help index}
      {----------------------------Selector options}
      procedure slOptionsOn(OptionFlags : Byte);
        {-Activate multiple options}
      procedure slOptionsOff(OptionFlags : Byte);
        {-Deactivate multiple options}
      function slOptionsAreOn(OptionFlags : Byte) : Boolean;
        {-Return true if all specified options are on}
      {----------------------------Field options}
      procedure slFieldOptionsOn(OptionFlags : LongInt);
        {-Activate multiple options}
      procedure slFieldOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple options}
      function slFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified options are on}
      {----------------------------Procedure pointers}
      procedure SetGetFieldProc(GFP : GetFieldProc);
        {-Set routine to call to get strings to display}
      procedure SetActionProc(AP : SelectProc);
        {-Set routine to call when field is selected}
      procedure SetPreSelectProc(PSP : SelectProc);
        {-Set routine to call before moving to new field}
      procedure SetPostSelectProc(PSP : SelectProc);
        {-Set routine to call before moving to next one}
    {$IFDEF UseStreams}
      {----------------------------Streams}
      constructor Load(var S : IdStream);
        {-Load a selector from a stream}
      procedure Store(var S : IdStream);
        {-Store a selector in a stream}
    {$ENDIF}
{.Z+}
      {+++ internal methods +++}
      function slGetField(ID : Word; NeedPrompt : Boolean) : string; virtual;
      procedure slAction(ID : Word); virtual;
      procedure slPreSelect(ID : Word); virtual;
      procedure slPostSelect(ID : Word); virtual;
      procedure asDrawKnownField(SFP : SelectFieldPtr); virtual;
      procedure slCalcAttrs(SFP : SelectFieldPtr; var FA, CA, PA : Byte);
{.Z-}
    end;

  ScrollingSelectorPtr = ^ScrollingSelector;
  ScrollingSelector =
    object(Selector)
      ssScrollable : Boolean;     {False until initialized}
      ssRowOfs     : SmallInt;     {number of rows above top of window}
      ssColOfs     : SmallInt;     {number of cols to left of window}
      ssVS         : VirtScreen;  {virtual screen for scrollable window}
      {....methods....}
      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the selector}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt);
        {-Initialize a selector with custom window options}
      destructor Done; virtual;
        {-Deallocate field list, screen buffers}
      procedure AllocateScreen;
        {-Call after last field added to allocate virtual screen}
      procedure VScreenToScreen; virtual;
        {-Copy the virtual screen to the physical screen}
    {$IFDEF UseStreams}
      {----------------------------Streams}
      constructor Load(var S : IdStream);
        {-Load a selector from a stream}
      procedure Store(var S : IdStream);
        {-Store a selector in a stream}
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
{.Z-}
    end;

{.F+}

{.Z+}
{$IFDEF UseStreams}
  {------- stream registration routines ------------}
procedure TextFieldStream(SPtr : IdStreamPtr);     {!!.30}
  {-Register all types for text fields}            {!!.30}
procedure SelectFieldStream(SPtr : IdStreamPtr);   {!!.30}
  {-Register all types for select fields}          {!!.30}

procedure AbstractSelectorStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing abstract selectors}
procedure SelectorStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing selectors}
procedure ScrollingSelectorStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing scrolling selectors}
{$ENDIF}
{.Z-}

var
  {$IFDEF UseDrag}                   {!!.03}
  SelectCommands : DragProcessor;    {!!.03}
  {$ELSE}                            {!!.03}
  SelectCommands : CommandProcessor;
  {$ENDIF}                           {!!.03}

  {======================================================================}

implementation

type
  SFPtr = SelectFieldPtr;

  {$I OPSELECT.IN1}  {Selectors, ScrollingSelectors, streams}
  {$I OPSELECT.IN2}  {SelectField/TextField}  {!!.30}

  {--------------- methods for AbstractSelector's ----------------}

  constructor AbstractSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                     var Colors : ColorSet;
                                     Options : LongInt;
                                     var CP : CommandProcessor;
                                     UnitCode : Byte);
    {-Initialize the selector}
  begin
    {initialize field lists}
    asFields.Init;
    asTextFields.Init;

    {force wResizeable off and wUserContents on}
    ClearLongFlag(Options, wResizeable);
    SetLongFlag(Options, wUserContents);

    {initialize the window}
    if not CommandWindow.InitCustom(X1, Y1, X2, Y2, Colors,
                                    Options, CP, UnitCode) then
      Fail;

    {initialize remaining fields}
    asCount := 0;
    asCurID := BadFieldID;
    asCurrent := nil;
    asNext := nil;
    asKnown := nil;
    asMinRow := 0;
    asMaxRow := 0;
    asMinCol := 0;
    asMaxCol := 0;
    asOptions := DefSelectOptions;
    asFieldOptions := DefSFieldOptions;
    asWrapMode := DefWrapMode;
    asColors := Colors;
    @asUpdateProc := nil;
  end;

  destructor AbstractSelector.Done;
    {-Deallocate field list, screen buffers}
  begin
    {deallocate field lists}
    asFields.Done;
    asTextFields.Done;

    {deallocate screen buffers}
    CommandWindow.Done;
  end;

  procedure AbstractSelector.asUpdate;
    {-Called when entire screen is being redrawn}
  begin
    if @asUpdateProc <> nil then
      if not ByteFlagIsSet(asOptions, slUpdateActive) then begin {!!.01}
        SetByteFlag(asOptions, slUpdateActive);                  {!!.01}
        asUpdateProc(@Self);
        ClearByteFlag(asOptions, slUpdateActive);                {!!.01}
      end;                                                       {!!.01}
  end;

  procedure AbstractSelector.DrawBackground;                      {!!.03}
    {-Hook to give opportunity to draw background beneath fields} {!!.03}
  begin                                                           {!!.03}
  end;                                                            {!!.03}

  procedure AbstractSelector.asDrawKnownField(SFP : SelectFieldPtr);
    {-Draw the field}
  begin
    Abstract;
  end;

  procedure AbstractSelector.DrawField(ID : Word);
    {-Draw the specified field}
  var
    SFP : SelectFieldPtr;
    I : Word;
  begin
    SFP := FindField(ID);
    if SFP <> nil then begin
      I := asCurID;
      asCurID := BadFieldID;
      asDrawKnownField(SFP);
      asCurID := I;
    end;
  end;

  procedure AbstractSelector.ResetScreen;
    {-Call when contents of field(s) changed without calling DrawField}
  begin
    {no fast updates}
    ClearByteFlag(asOptions, slFastUpdates);
  end;

  procedure AbstractSelector.asFixWindow(Redraw, ScrollByPage : Boolean);
    {-Dummy routine}
  begin
  end;

  function AbstractSelector.asFixCoordinates(InRow, InCol : Word; Wid, Ht : Byte;
                                        var OutRow, OutCol : Integer) : Boolean;
    {-Adjust coordinates}
  begin
    asFixCoordinates := True;
    OutRow := InRow+Pred(wYL);
    OutCol := InCol+Pred(wXL);
  end;

  function AbstractSelector.asMaximumRow : Word;
    {-Return maximum row coordinate relative to window}
  begin
    asMaximumRow := Succ(wYH-wYL);
  end;

  function AbstractSelector.asMaximumCol : Word;
    {-Return maximum column coordinate relative to window}
  begin
    asMaximumCol := Succ(wXH-wXL);
  end;

  function AbstractSelector.asCoordsOK(Row, Col, Wid, Ht : Word) : Boolean; {!!.03}
    {-Return True if field coordinates are OK}
  begin
            {!!.03}
    if (Row+Pred(Ht) > asMaximumRow) or (Col+Pred(Wid) > asMaximumCol) then begin
      GotError(epFatal+ecBadCoordinates, emNullError);
      asCoordsOK := False;
    end
    else
      asCoordsOK := True;
  end;

  procedure AbstractSelector.AddTextField(St : string; Row, Col : Word);
    {-Add a text field}
  var
    CA, MA : FlexAttrs;
  begin
    FillChar(CA, SizeOf(CA), wTextColor); {!!.03}
    FillChar(MA, SizeOf(MA), wTextMono);  {!!.03}

    AddTextFieldDeluxe(St, Row, Col, CA, MA);
  end;

  procedure AbstractSelector.AddCenteredTextField(St : string; Row : Word); {!!.03}
    {-Add a text field, centered within the window}
  var
    Col : Word;
    StLen : Byte absolute St;
  begin
    if StLen >= Width then
      Col := 1
    else
      Col := Succ((Width-StLen) shr 1);
    AddTextField(St, Row, Col);
  end;

  procedure AbstractSelector.AddTextFieldCustom(St : string; Row, Col : Word;
                                                Color, Mono : Byte);
    {-Add a text field with specific colors}
  var
    CA, MA : FlexAttrs;
  begin
    FillChar(CA, SizeOf(CA), Color);
    FillChar(MA, SizeOf(MA), MapMono(Color, Mono));

    AddTextFieldDeluxe(St, Row, Col, CA, MA);
  end;

  procedure AbstractSelector.AddTextFieldDeluxe(St : string; Row, Col : Word;
                                                var ColorAttrs, MonoAttrs : FlexAttrs);
    {-Add a text field with special colors}
  var
    ID : Word;
    TFP : TextFieldPtr;
    StLen : Byte {absolute St} ;        {!!.10}
  begin
    {do nothing if string is empty}
    StLen := FlexLen(St);               {!!.10}
    if StLen = 0 then
      Exit;

    {check for pending error}
    if cwGetLastError <> 0 then
      Exit;

    {check coordinates}
    if not asCoordsOK(Row, Col, StLen, 1) then {!!.03}
      Exit;

    {get the field ID}
    if asTextFields.Tail = nil then
      ID := 0
    else
      ID := TextFieldPtr(asTextFields.Tail)^.tfID+1;

    {allocate the field}
    New(TFP, Init(ID, St, Row, Col, ColorAttrs, MonoAttrs));

    {add it to the list or report the error}
    if TFP = nil then
      GotError(epFatal+ecOutOfMemory, emInsufficientMemory)
    else
      {add it to the linked list}
      asTextFields.Append(TFP);
  end;

  procedure AbstractSelector.AddLineField(Ch1, Ch2, Ch3 : Char;   {!!.03}
                                          Row, Col, Length : Word;
                                          Vertical : Boolean);
    {-Add a line field}
  begin
    AddLineFieldCustom(
      Ch1, Ch2, Ch3, Row, Col, Length, wTextColor, wTextMono, Vertical);
  end;

  procedure AbstractSelector.AddLineFieldCustom(Ch1, Ch2, Ch3 : Char;  {!!.03}
                                                Row, Col, Length : Word;
                                                Color, Mono : Byte;
                                                Vertical : Boolean);
    {-Add a line field with specific colors}
  var
    ID, W, H : Word;
    TLFP : TextLineFieldPtr;
  begin
    if Length = 0 then
      Exit;

    {check for pending error}
    if cwGetLastError <> 0 then
      Exit;

    {check coordinates}
    if Vertical then begin
      W := 1;
      H := Length;
    end
    else begin
      W := Length;
      H := 1;
    end;
    if not asCoordsOK(Row, Col, W, H) then
      Exit;

    {get the field ID}
    if asTextFields.Tail = nil then
      ID := 0
    else
      ID := TextFieldPtr(asTextFields.Tail)^.tfID+1;

    {allocate the field}
    Mono := MapMono(Color, Mono);
    New(TLFP, Init(ID, Ch1, Ch2, Ch3, Row, Col, Length, Color, Mono, Vertical));

    {add it to the list or report the error}
    if TLFP = nil then
      GotError(epFatal+ecOutOfMemory, emInsufficientMemory)
    else
      {add it to the linked list}
      asTextFields.Append(TLFP);
  end;

  procedure AbstractSelector.AddBoxField(BoxChars : FrameArray;      {!!.03}
                                         XL, YL, XH, YH : Word);
    {-Add a box field}
  begin
    AddBoxFieldCustom(BoxChars, XL, YL, XH, YH, wTextColor, wTextMono);
  end;

  procedure AbstractSelector.AddBoxFieldCustom(BoxChars : FrameArray; {!!.03}
                                               XL, YL, XH, YH : Word;
                                               Color, Mono : Byte);
    {-Add a box field with specific colors}
  var
    ID : Word;
    TBFP : TextBoxFieldPtr;
  begin
    {check box coordinates}
    if (XH < XL) or (YH < YL) then begin
      GotError(epFatal+ecBadCoordinates, emNullError);
      Exit;
    end;

    {check for pending error}
    if cwGetLastError <> 0 then
      Exit;

    {check coordinates}
    if not asCoordsOK(YL, XL, Succ(XH-XL), Succ(YH-YL)) then
      Exit;

    {get the field ID}
    if asTextFields.Tail = nil then
      ID := 0
    else
      ID := TextFieldPtr(asTextFields.Tail)^.tfID+1;

    {allocate the field}
    New(TBFP, Init(ID, BoxChars, XL, YL, XH, YH, Color, MapMono(Color, Mono)));

    {add it to the list or report the error}
    if TBFP = nil then
      GotError(epFatal+ecOutOfMemory, emInsufficientMemory)
    else
      {add it to the linked list}
      asTextFields.Append(TBFP);
  end;

  procedure AbstractSelector.AddShadowedBoxField(BoxChars : FrameArray; {!!.03}
                                                 XL, YL, XH, YH : Word);
    {-Add a shadowed box field}
  begin
    AddShadowedBoxFieldCustom(BoxChars, XL, YL, XH, YH,
                              wTextColor, wTextMono, wTextColor, wTextMono);
  end;

  procedure AbstractSelector.AddShadowedBoxFieldCustom(BoxChars : FrameArray; {!!.03}
                                                       XL, YL, XH, YH : Word;
                                                       BColor, BMono : Byte;
                                                       SColor, SMono : Byte);
    {-Add a shadowed box field with specific colors}
  var
    P1, P2 : TextBoxFieldPtr;
  begin
    {check coordinates, accounting for shadow}
    if not asCoordsOK(YL, XL, (XH-XL)+2, (YH-YL)+2) then
      Exit;

    {save pointer to end of text field list}
    P1 := Pointer(asTextFields.Tail);

    {add a regular box field}
    AddBoxFieldCustom(BoxChars, XL, YL, XH, YH, BColor, BMono);

    {add a shadow if the box field was added}
    P2 := Pointer(asTextFields.Tail);
    if P2 <> P1 then
      P2^.AddShadow(SColor, SMono);
  end;

  procedure AbstractSelector.asDrawAllTextFields(AdjustForWindow : Boolean);
    {-Draw all the text fields}
  var
    TFP : TextFieldPtr;
    XDelta, YDelta : Word;
  begin
    TFP := Pointer(asTextFields.Head);
    while TFP <> nil do begin
      {draw the field}
      if AdjustForWindow then begin
        XDelta := Pred(wXL);
        YDelta := Pred(wYL);
      end
      else begin
        XDelta := 0;
        YDelta := 0;
      end;
      TFP^.Draw(XDelta, YDelta);

      {point to next one}
      TFP := Pointer(TFP^.slNext);
    end;
  end;

  procedure AbstractSelector.UpdateContents;
    {-Redraw the entire selection window}
  var
    SFP : SelectFieldPtr;
    ID : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {do nothing if there are no fields}           {!!.11}
    if asCount = 0 then begin                     {!!.11}
      GotError(epFatal+ecNoFields, emNullError);  {!!.11}
      Exit;                                       {!!.11}
    end;                                          {!!.11}

    {reset flags if necessary}
    asResetFlags;
    if RawError <> 0 then                         {!!.12}
      Exit;                                       {!!.12}

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw background, if any}  {!!.03}
    DrawBackground;            {!!.03}

    {draw text fields}
    asDrawAllTextFields(True);

    {temporarily change asCurID}
    ID := asCurID;
    asCurID := BadFieldID;

    {start with first field}
    SFP := Pointer(asFields.Head);
    while SFP <> nil do begin
      {draw this field}
      asDrawKnownField(SFP);

      {point to next field in list}
      SFP := Pointer(SFP^.dlNext);
    end;

    {restore current ID}
    asCurID := ID;

    {call user screen update proc}
    asUpdate;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    {indicate that we've redrawn the entire screen}
    SetByteFlag(asOptions, slFastUpdates);

    {$IFDEF UseScrollBars}
    {update scroll bars}
    asUpdateScrollBars;
    {$ENDIF}

    StackWindow.UpdateContents; {!!.01}
  end;

  function AbstractSelector.asFieldIsProtected(SFP : Pointer) : Boolean;
    {-Return True if field is protected (or hidden or invisible)}
  begin
    with SelectFieldPtr(SFP)^ do
      asFieldIsProtected := (SFP <> nil) and                   {!!.12}
        LongFlagIsSet(sfOptions, sfProtected+sfHidden+sfInvisible);
  end;

  procedure AbstractSelector.asSeekToFirst;
    {-Move to first unprotected field}
  var
    SFP : SelectFieldPtr absolute asNext;
    Tmp : SelectFieldPtr;                     {!!.30}

  begin
    SFP := Pointer(asFields.Head);
    while asFieldIsProtected(SFP) do begin
      if (SFP^.sfNextID = BadFieldID) then    {!!.30}
        SFP := Pointer(SFP^.dlNext)           {!!.30}
      else begin                              {!!.30}
        Tmp := FindField(SFP^.sfNextID);      {!!.30}
        if (Tmp = nil) then                   {!!.30}
          SFP := Pointer(SFP^.dlNext)         {!!.30}
        else                                  {!!.30}
          SFP := Tmp;                         {!!.30}
      end;                                    {!!.30}
    end;
  end;

  procedure AbstractSelector.asSeekToLast;
    {-Move to last unprotected field}
  var
    SFP : SelectFieldPtr absolute asNext;
    Tmp : SelectFieldPtr;                     {!!.30}

  begin
    SFP := Pointer(asFields.Tail);
    while asFieldIsProtected(SFP) do begin
      if (SFP^.sfPrevID = BadFieldID) then    {!!.30}
        SFP := Pointer(SFP^.dlPrev)           {!!.30}
      else begin                              {!!.30}
        Tmp := FindField(SFP^.sfPrevID);      {!!.30}
        if (Tmp = nil) then                   {!!.30}
          SFP := Pointer(SFP^.dlPrev)         {!!.30}
        else                                  {!!.30}
          SFP := Tmp;                         {!!.30}
      end;                                    {!!.30}
    end;
  end;

  procedure AbstractSelector.asWrapAtTop;
    {-Deal with wrap at top of edit screen}
  var
    SFP : SelectFieldPtr absolute asNext;
  begin
    case asWrapMode of
      WrapAtEdges :
        asSeekToLast;
      ExitAtBot,     {!!.01}
      StopAtEdges :
        SFP := nil;
      ExitAtTop,     {!!.01}
      ExitAtEdges :
        begin
          SFP := nil;
          cwCmd := ccExitAtTop;
        end;
    end;
  end;

  {!!.30 - New}
  procedure AbstractSelector.asEstablishDefaultLinks;
    {-Set field links to default list order }
  var
    TempFld : SelectFieldPtr;
    NextFld : SelectFieldPtr;
    PrevFld : SelectFieldPtr;

  begin
    TempFld := SelectFieldPtr(asFields.dlHead);
    while (TempFld <> nil) do begin
      NextFld := SelectFieldPtr(TempFld^.dlNext);
      PrevFld := SelectFieldPtr(TempFld^.dlPrev);
      if (TempFld^.sfNextID = BadFieldID) and (NextFld <> nil) then
        TempFld^.sfNextID := NextFld^.sfID;
      if (TempFld^.sfPrevID = BadFieldID) and (PrevFld <> nil) then
        TempFld^.sfPrevID := PrevFld^.sfID;
      TempFld := NextFld;
    end;
  end;

  procedure AbstractSelector.asWrapAtBottom;
    {-Deal with wrap at bottom of edit screen}
  var
    SFP : SelectFieldPtr absolute asNext;
  begin
    case asWrapMode of
      WrapAtEdges :
        asSeekToFirst;
      ExitAtTop,     {!!.01}
      StopAtEdges :
        SFP := nil;
      ExitAtBot,     {!!.01}
      ExitAtEdges :
        begin
          SFP := nil;
          cwCmd := ccExitAtBot;
        end;
    end;
  end;

  procedure AbstractSelector.asGotoPrevField;
    {-Move cursor to previous field}
  var
    SFP : SelectFieldPtr absolute asNext;
  begin
    SFP := Pointer(asCurrent^.dlPrev);
    while asFieldIsProtected(SFP) do
      SFP := Pointer(SFP^.dlPrev);
    if SFP = nil then
      asWrapAtTop;
  end;

  {!!.30 - New}
  procedure AbstractSelector.asGotoPrevFieldVisit;
    {-Move cursor to previous field in visitation order }
  var
    SFP : SelectFieldPtr absolute asNext;

  begin
    if (asCurrent^.sfPrevID = BadFieldID) then begin
      asWrapAtTop;
      Exit;
    end;

    SFP := FindField(asCurrent^.sfPrevID);
    if (SFP = nil) then begin
      asWrapAtTop;
      Exit;
    end;

    while (SFP <> nil) and asFieldIsProtected(SFP) do begin
      if (SFP^.sfPrevID = BadFieldID) then begin
        asWrapAtTop;
        Exit;
      end;

      SFP := FindField(SFP^.sfPrevID);
    end;

    if (SFP = nil) then
      asWrapAtTop;
  end;

  procedure AbstractSelector.asGotoNextField;
    {-Move cursor to next field}
  var
    SFP : SelectFieldPtr absolute asNext;
  begin
    SFP := Pointer(asCurrent^.dlNext);
    while asFieldIsProtected(SFP) do
      SFP := Pointer(SFP^.dlNext);
    if SFP = nil then
      asWrapAtBottom;
  end;

  {!!.30 - New}
  procedure AbstractSelector.asGotoNextFieldVisit;
    {-Move cursor to next field in visitation order }
  var
    SFP : SelectFieldPtr absolute asNext;

  begin
    if (asCurrent^.sfNextID = BadFieldID) then begin
      asWrapAtBottom;
      Exit;
    end;

    SFP := FindField(asCurrent^.sfNextID);
    if (SFP = nil) then begin
      asWrapAtBottom;
      Exit;
    end;

    while (SFP <> nil) and asFieldIsProtected(SFP) do begin
      if (SFP^.sfNextID = BadFieldID) then begin
        asWrapAtBottom;
        Exit;
      end;

      SFP := FindField(SFP^.sfNextID);
    end;

    if (SFP = nil) then
      asWrapAtBottom;
  end;

  function AbstractSelector.asFindBestField(Col : Word) : Boolean;
    {-Find the best field on same row as asNext}
  var
    SFP : SelectFieldPtr absolute asNext;
    Row : Word;
    SaveSFP : Pointer;

    function ScanRight : Boolean;
      {-Scan to the right}
    begin
      with SFP^ do
        if dlNext = nil then
          ScanRight := False
        else with SFPtr(dlNext)^ do
          ScanRight := (sfFRow = Row) and (sfFCol <= Col);
    end;

  begin
    asFindBestField := True;
    SaveSFP := nil;
    Row := SFP^.sfFRow;

    {seek to the first field on the row}
    while (SFP^.dlPrev <> nil) and (SFPtr(SFP^.dlPrev)^.sfFRow = Row) do
      {point to previous field}
      SFP := Pointer(SFP^.dlPrev);

    {find the last field on the row with a FieldCol <= Col}
    while ScanRight do begin
      {save the current field if it's not protected}
      if not asFieldIsProtected(SFP) then
        SaveSFP := SFP;

      {point to next field}
      SFP := Pointer(SFP^.dlNext);
    end;

    {OK if not protected}
    if asFieldIsProtected(SFP) then
      {use the last unprotected field if there is one}
      if SaveSFP <> nil then
        SFP := SaveSFP
      else begin
        {try the rest of the row}
        while (SaveSFP = nil) and (SFP^.dlNext <> nil) and (SFPtr(SFP^.dlNext)^.sfFRow = Row) do begin
          SFP := Pointer(SFP^.dlNext);
          {save the current field if it's not protected}
          if not asFieldIsProtected(SFP) then
            SaveSFP := SFP;
        end;

        {no luck if all fields on this row are protected}
        if SaveSFP = nil then
          asFindBestField := False;
      end;
  end;

  function AbstractSelector.asIDisValid(ID : Word) : Boolean;
    {-Return True if ID is a valid ID number for an unprotected field}
  var
    SFP : SelectFieldPtr absolute asNext;
  begin
    if ID = BadFieldID then
      asIDisValid := False
    else begin
      SFP := FindField(ID);
      asIDisValid := (SFP <> nil) and not asFieldIsProtected(SFP);
    end;
  end;

  procedure AbstractSelector.PositionCursorAt(SFP : SelectFieldPtr);
    {-Puts the cursor on the beginning of the specified field}
  var
    Row, Col : Integer;
    SaveCurrent : SelectFieldPtr;
  begin
    if (asCurrent <> nil) and (SFP <> nil) then            {!!.11}
      with asCurrent^ do begin
        {make sure the field is on the screen}
        SaveCurrent := asCurrent;
        asCurrent := SFP;
        asFixWindow(True, ByteFlagIsSet(asOptions, slScrollByPage));
        asCurrent := SaveCurrent;

        if asFixCoordinates(sfFRow, sfFCol, afWidth, sfFHeight, Row, Col) then begin {!!.22}
          GoToXYabs(Col, Row);
          {$IFDEF UseScrollBars}
          asUpdateScrollBars;
          {$ENDIF}
        end
        else
          GotError(epFatal+ecFieldNotFound, emNullError);
      end;
  end;

  procedure AbstractSelector.VisitAllSelectFields(FProc : asFieldProc; var D);
    {-Call the specified procedure for all fields in the selector}
  var
    SFP : SelectFieldPtr;
  begin
    SFP := Pointer(asFields.Head);
    while SFP <> nil do begin
      FProc(SFP, D, @Self);
      SFP := Pointer(SFP^.dlNext);
    end;
  end;

  procedure AbstractSelector.VisitAllTextFields(FProc : asTextFieldProc; var D);
    {-Call the specified procedure for all text fields in the selector}
  var
    TFP : TextFieldPtr;
  begin
    TFP := Pointer(asTextFields.Head);
    while TFP <> nil do begin
      FProc(TFP, D, @Self);
      TFP := Pointer(TFP^.slNext);
    end;
  end;

  procedure AbstractSelector.asUpField;
    {-Move cursor up one row}
  label
    Retry;
  var
    SFP : SelectFieldPtr absolute asNext;
    Row, Col : Word;

    procedure Wrap;
      {-Wrap at top of screen}
    begin
      asWrapAtTop;
      if SFP <> nil then
        if asFindBestField(Col) then {};
    end;

  begin
    {find next unprotected field with coordinates above this one}
    SFP := asCurrent;
    with SFP^ do begin
      Row := sfFRow;
      Col := sfFCol+Pred(sfFCPos);
    end;

Retry:
    if SFP^.dlPrev = nil then
      Wrap
    else begin
     {find the next row up with a field on it}
      repeat
        SFP := Pointer(SFP^.dlPrev);
      until (SFP = nil) or (SFP^.sfFRow < Row);

      {find anything?}
      if SFP = nil then
        Wrap
      else begin
        {we've found the next row up--now find the first field on it}
        Row := SFP^.sfFRow;
        while (SFP^.dlPrev <> nil) and (SFPtr(SFP^.dlPrev)^.sfFRow = Row) do
          SFP := Pointer(SFP^.dlPrev);

        {find the best field}
        if not asFindBestField(Col) then
          goto Retry;
      end;
    end;
  end;

  procedure AbstractSelector.asDownField;
    {-Move cursor down one row}
  label
    Retry;
  var
    SFP : SelectFieldPtr absolute asNext;
    Row, Col : Word;

    procedure Wrap;
      {-Wrap at bottom of window}
    begin
      asWrapAtBottom;
      if SFP <> nil then
        if asFindBestField(Col) then {};
    end;

  begin
    {find next unprotected field with coordinates below this one}
    SFP := asCurrent;
    with SFP^ do begin
      Row := sfFRow+Pred(sfFHeight);
      Col := sfFCol+Pred(sfFCPos);
    end;

Retry:
    if SFP^.dlNext = nil then
      Wrap
    else begin
      {find the next row down with a field on it}
      repeat
        SFP := Pointer(SFP^.dlNext);
      until (SFP = nil) or (SFP^.sfFRow > Row);

      {find anything?}
      if SFP = nil then
        Wrap
      {find the best field}
      else if not asFindBestField(Col) then begin
        {reset current row and retry}
        Row := SFP^.sfFRow;
        goto Retry;
      end;
    end;
  end;

  procedure AbstractSelector.asHomeField;
    {-Move cursor to first unprotected field on current row}
  var
    SFP : SelectFieldPtr absolute asNext;
    Row : Word;
  begin
    SFP := asCurrent;
    Row := SFP^.sfFRow;

    {find the first field on the row}
    while (SFP^.dlPrev <> nil) and (SFPtr(SFP^.dlPrev)^.sfFRow = Row) do
      SFP := Pointer(SFP^.dlPrev);

    {skip protected fields}
    while asFieldIsProtected(SFP) do
      SFP := Pointer(SFP^.dlNext);
  end;

  procedure AbstractSelector.asEndField;
    {-Move cursor to last unprotected field on current row}
  var
    SFP : SelectFieldPtr absolute asNext;
    Row : Word;
  begin
    SFP := asCurrent;
    Row := SFP^.sfFRow;

    {find the last field on the row}
    while (SFP^.dlNext <> nil) and (SFPtr(SFP^.dlNext)^.sfFRow = Row) do
      SFP := Pointer(SFP^.dlNext);

    {skip protected fields}
    while asFieldIsProtected(SFP) do
      SFP := Pointer(SFP^.dlPrev);
  end;

  function AbstractSelector.asNextFieldOK : Boolean;
    {-Make sure the next field isn't protected}
  var
    SFP : SelectFieldPtr absolute asNext;
    Tmp : SelectFieldPtr;                     {!!.30}

  begin
    asNextFieldOK := True;
    while asFieldIsProtected(SFP) do
      if (SFP^.sfNextID = BadFieldID) then    {!!.30}
        SFP := Pointer(SFP^.dlNext)           {!!.30}
      else begin                              {!!.30}
        Tmp := FindField(SFP^.sfNextID);      {!!.30}
        if (Tmp = nil) then                   {!!.30}
          SFP := Pointer(SFP^.dlNext)         {!!.30}
        else                                  {!!.30}
          SFP := Tmp;                         {!!.30}
      end;                                    {!!.30}

    if SFP = nil then begin
      {wrap around to first field}
      SFP := Pointer(asFields.Head);
      while asFieldIsProtected(SFP) do
        if (SFP^.sfNextID = BadFieldID) then  {!!.30}
          SFP := Pointer(SFP^.dlNext)         {!!.30}
        else begin                            {!!.30}
          Tmp := FindField(SFP^.sfNextID);    {!!.30}
          if (Tmp = nil) then                 {!!.30}
            SFP := Pointer(SFP^.dlNext)       {!!.30}
          else                                {!!.30}
            SFP := Tmp;                       {!!.30}
        end;                                  {!!.30}
      asNextFieldOK := (SFP <> nil);
    end;
  end;

  procedure AbstractSelector.asPageUpOrDown(Delta : Integer);
    {-Process PgUp/PgDn commands}
  var
    Col : Word;
    SFP : SelectFieldPtr;
  begin
    with asCurrent^ do
      Col := sfFCol+Pred(sfFCPos);
    if Delta = 1 then
      {move cursor to last row}
      asSeekToLast
    else
      {move cursor to first row}
      asSeekToFirst;
    if asFindBestField(Col) then {};
  end;

  {$IFDEF UseScrollBars}
  procedure AbstractSelector.asSetupForScrollBars;
    {-Set boundaries for all scroll bars}
  begin
    ChangeAllScrollBars(asMinCol, asMaxCol, asMinRow, asMaxRow);
  end;

  procedure AbstractSelector.asUpdateScrollBars;
    {-Update horizontal and vertical scroll bars}
  begin
    with asCurrent^ do
      DrawAllSliders(sfFCol, sfFRow);
  end;
  {$ENDIF}

  procedure AbstractSelector.asMoveCursorRight;
    {-Move cursor one field to the right on the current Row}
  begin
    {point asNext to next field}
    asGotoNextField;

    {if it's not on the same row, don't move the cursor}
    if (asNext <> nil) and (asNext^.sfFRow <> asCurrent^.sfFRow) then
      asNext := nil;
  end;

  procedure AbstractSelector.asMoveCursorLeft;
    {-Move cursor one field to the left on the current Row}
  begin
    {point asNext to previous field}
    asGotoPrevField;

    {if it's not on the same row, don't move the cursor}
    if (asNext <> nil) and (asNext^.sfFRow <> asCurrent^.sfFRow) then
      asNext := nil;
  end;

  procedure AbstractSelector.asScrollHorizontally(TargetCol : Integer);
    {-Scroll horizontally when mouse clicked on horizontal scroll bar}
  begin
    with asCurrent^ do begin
      {do nothing if we're already there}
      if TargetCol = sfFCol then
        Exit;

      {no exact match--find best field on current row}
      asNext := asCurrent;
      if asFindBestField(TargetCol) and (asNext = asCurrent) then
        {current field is an unacceptable choice}
        asNext := nil;

      if asNext = nil then
        {try another approach}
        if TargetCol < sfFCol then
          asMoveCursorLeft
        else
          asMoveCursorRight;
    end;
  end;

  procedure AbstractSelector.asSeekTo(Row : Integer; GoForward : Boolean);
    {-Find the first unprotected field on or near Row}
  var
    SFP : SelectFieldPtr absolute asNext;
    SaveSFP : SelectFieldPtr;
    CurRow : Word;
  begin
    CurRow := asCurrent^.sfFRow;

    asNext := asCurrent;
    SaveSFP := nil;
    if GoForward then begin
      SFP := Pointer(SFP^.dlNext);
      while SFP <> nil do begin
        {skip protected fields}
        if not asFieldIsProtected(SFP) then
          if (SFP^.sfFRow >= Row) then
            Exit
          else
            SaveSFP := SFP;
        SFP := Pointer(SFP^.dlNext);
      end;
    end
    else begin
      SFP := Pointer(SFP^.dlPrev);
      while SFP <> nil do begin
        {skip protected fields}
        if not asFieldIsProtected(SFP) then
          if (SFP^.sfFRow <= Row) then
            Exit
          else
            SaveSFP := SFP;
        SFP := Pointer(SFP^.dlPrev);
      end;
    end;
    SFP := SaveSFP;
  end;

  procedure AbstractSelector.asScrollVertically(TargetRow : Integer);
    {-Scroll vertically to TargetRow}
  begin
    with asCurrent^ do begin
      {do nothing if we're already there}
      if (TargetRow = sfFRow) then
        Exit;

      {seek to the right row}
      asSeekTo(TargetRow, TargetRow > sfFRow);

      if (asNext <> nil) then
        {is the result acceptable?}
        if (asNext^.sfFRow = sfFRow) then
          asNext := nil
        else
          if asFindBestField(sfFCol) then {won't fail};
    end;
  end;

  {$IFDEF UseMouse}
  function AbstractSelector.asMouseOnFieldPrim(Row, Col : Integer;
                                        StartRow, StartCol : Integer;
                                        fWidth, fHeight : Byte) : Boolean;
    {-Return True if cursor is on this part of the field}
  begin
    {assume failure}
    asMouseOnFieldPrim := False;

    {get out if fWidth = 0}
    if fWidth = 0 then
      Exit;

    {adjust coordinates}
    if asFixCoordinates(StartRow, StartCol, fWidth, fHeight, StartRow, StartCol) then ;

    {check the row and column coordinates}
    asMouseOnFieldPrim := (Row >= StartRow) and
                        (Row <= StartRow+Pred(fHeight)) and
                        (Col >= StartCol) and
                        (Col <= StartCol+Pred(fWidth));
  end;

  function AbstractSelector.asMouseOnField(SFP : SelectFieldPtr;
                                    Row, Col : Integer) : Boolean;
    {-Returns True if the mouse cursor is on this field}
  begin
    with SFP^ do
      {check the location of both the prompt and the field}
      asMouseOnField :=
        asMouseOnFieldPrim(Row, Col, sfPRow, sfPCol, sfPWidth, 1) or
        asMouseOnFieldPrim(Row, Col, sfFRow, sfFCol, afWidth, sfFHeight); {!!.22}
  end;

  function AbstractSelector.asProcessMouseCommand(Cmd : Word; {!!.03}
                                                  var ScrollByPage : Boolean) : Byte;
    {-Process ccMouseSel command. Return codes:
       0 = do nothing
       1 = exit from selector
       2 = user clicked mouse on current field
       3 = user clicked mouse on another field
     }
  const                       {!!.14}
    UpCmd : Word = ccUp;      {!!.14}
    DownCmd : Word = ccDown;  {!!.14}
  var
    FramePos : FramePosType;
    L : LongInt;
    HotCode : Byte;
    MouseAbsX, MouseAbsY : Integer;
    Dragging : Boolean; {!!.03}

    procedure LocateField(TargetRow, TargetCol : Integer);
      {-Locate a field corresponding to the specified coordinates}
    var
      SFP : SelectFieldPtr absolute asNext;
    begin
      {start with first field}
      SFP := Pointer(asFields.Head);

      {look through the entire linked list of fields}
      while SFP <> nil do begin
        {skip protected fields}
        if not asFieldIsProtected(SFP) then
          if asMouseOnField(SFP, TargetRow, TargetCol) then
            {we found the field}
             Exit;

        {advance to next field}
        SFP := Pointer(SFP^.dlNext);
      end;
    end;

  begin
    {assume that it's not an exit command}
    asProcessMouseCommand := 0;

    {determine position of mouse}
    L := cwMouseResults(Cmd, FramePos, HotCode);    {!!.03} {!!.13}

    {Should mouse event be ignored?}                             {!!.03}
    if cwIgnoreMouseEvent(Dragging, Cmd, FramePos, HotCode) then {!!.03}
      Exit;                                                      {!!.03}
    {$IFDEF UseScrollBars}                                           {!!.13}
    if (Cmd = ccMouseSel) and (HotCode <> hsNone) then               {!!.13}
      if cwCmdPtr^.cpOptionsAreOn(cpMouseDrag) then                  {!!.13}
        Exit;                                                        {!!.13}
    {$ENDIF}                                                         {!!.13}

    MouseAbsX := MouseKeyWordX+MouseXLo;
    MouseAbsY := MouseKeyWordY+MouseYLo;

    case HotCode of
      hsNone :           {not a hot spot}
        case FramePos of
          frInsideActive :       {inside window}
            if (Cmd = ccMouseDown) or (Cmd = ccMouseSel) then begin {!!.03}
{if (Cmd = ccMouseSel) then inline($cc);}                           {!!.10}
              LocateField(MouseAbsY, MouseAbsX);
              if (asNext = asCurrent) then begin
                if (Cmd = ccMouseSel) then begin
                  asProcessMouseCommand := 2;
                  asNext := nil;
                end;
              end
              else if (asNext <> nil) and ((Cmd = ccMouseDown) or not Dragging) then
                asProcessMouseCommand := 3
              else
                asNext := nil;
            end;

          frTL..frRR,            {on the frame}
          frInsideFrame,         {inside window frame but not in window boundaries}
          frOutsideFrame :       {outside window frame}
            asProcessMouseCommand := Ord(LongFlagIsSet(wFlags, wAllMouseEvents));
        end;

      {$IFDEF UseScrollBars}
      hsDecV :           {the decrement fixture of a vertical scroll bar}
        if ByteFlagIsSet(asOptions, slMousePage) then begin
          asPageUpOrDown(-1);
          ScrollByPage := True;
        end
        else if LongFlagIsSet(asCurrent^.sfFlags, ifMultiLine) then  {!!.14}
          GetCommandProcessor^.SetCommandList(@UpCmd, 1)             {!!.14}
        else
          asUpField;
      hsDecH :           {the decrement fixture of a horizontal scroll bar}
        asMoveCursorLeft;
      hsIncV :           {the increment fixture of a vertical scroll bar}
        if ByteFlagIsSet(asOptions, slMousePage) then begin
          asPageUpOrDown(1);
          ScrollByPage := True;
        end
        else if LongFlagIsSet(asCurrent^.sfFlags, ifMultiLine) then  {!!.14}
          GetCommandProcessor^.SetCommandList(@DownCmd, 1)           {!!.14}
        else
          asDownField;
      hsIncH :           {the increment fixture of a horizontal scroll bar}
        asMoveCursorRight;
      hsBar :            {the slider portion of a scroll bar}
        case FramePos of
          frLL, frRR :   {vertical scroll bar}
            asScrollVertically(TweakSlider(FramePos, MouseAbsY, L, 1));
          else           {horizontal scroll bar}
            asScrollHorizontally(TweakSlider(FramePos, MouseAbsX, L, 1) );
        end;
      {$ENDIF}

      hsSpot,            {a single character hot spot}
      hsRegion0..255 :   {a user-defined region relative to a frame}
        asProcessMouseCommand := Ord(Cmd <> ccMouseAuto); {!!.03}
    end;
  end;
  {$ENDIF}

  procedure AbstractSelector.SetWrapMode(WM : WrapMode);
    {-Set the wrap mode for the selector}
  begin
    {change wrap mode}
    asWrapMode := WM;

    {indicate that flags need to be reset}
    asMinRow := 0;
    asMaxRow := 0;
  end;

  procedure AbstractSelector.SetScreenUpdateProc(SUP : ScreenUpdateProc);
    {-Set procedure to be called after a complete screen update}
  begin
    asUpdateProc := SUP;
  end;

  procedure AbstractSelector.SetPromptAttr(Color, Mono : Byte);
    {-Set attributes for prompts}
  begin
    asColors.SetPromptAttr(Color, Mono);
  end;

  procedure AbstractSelector.SetSelectedPromptAttr(Color, Mono : Byte);
    {-Set attributes for selected prompts}
  begin
    asColors.SetSelectedPromptAttr(Color, Mono);
  end;

  procedure AbstractSelector.SetProtectedPromptAttr(Color, Mono : Byte);
    {-Set attributes for protected prompts}
  begin
    asColors.SetProtectedPromptAttr(Color, Mono);
  end;

  procedure AbstractSelector.SetFieldAttr(Color, Mono : Byte);
    {-Set attributes for unselected fields}
  begin
    asColors.SetFieldAttr(Color, Mono);
  end;

  procedure AbstractSelector.SetSelectedFieldAttr(Color, Mono : Byte);
    {-Set attributes for selected fields}
  begin
    asColors.SetSelectedFieldAttr(Color, Mono);
  end;

  procedure AbstractSelector.SetProtectedFieldAttr(Color, Mono : Byte);
    {-Set attributes for protected fields}
  begin
    asColors.SetProtectedFieldAttr(Color, Mono);
  end;

  procedure AbstractSelector.SetCtrlAttr(Color, Mono : Byte);
    {-Set attributes for control characters}
  begin
    asColors.SetCtrlAttr(Color, Mono);
  end;

  procedure AbstractSelector.ChangePromptAttr(ID : Word; Color, Mono : Byte);
    {-Change attributes for prompts}
  begin
    if FindField(ID) <> nil then
      FindField(ID)^.SetPromptAttr(Color, Mono);
  end;

  procedure AbstractSelector.ChangeSelectedPromptAttr(ID : Word; Color, Mono : Byte);
    {-Change attributes for selected prompts}
  begin
    if FindField(ID) <> nil then
      FindField(ID)^.SetSelectedPromptAttr(Color, Mono);
  end;

  procedure AbstractSelector.ChangeProtectedPromptAttr(ID : Word; Color, Mono : Byte);
    {-Change attributes for protected prompts}
  begin
    if FindField(ID) <> nil then
      FindField(ID)^.SetProtectedPromptAttr(Color, Mono);
  end;

  procedure AbstractSelector.ChangeFieldAttr(ID : Word; Color, Mono : Byte);
    {-Change attributes for unselected fields}
  begin
    if FindField(ID) <> nil then
      FindField(ID)^.SetFieldAttr(Color, Mono);
  end;

  procedure AbstractSelector.ChangeSelectedFieldAttr(ID : Word; Color, Mono : Byte);
    {-Change attributes for selected fields}
  begin
    if FindField(ID) <> nil then
      FindField(ID)^.SetSelectedFieldAttr(Color, Mono);
  end;

  procedure AbstractSelector.ChangeProtectedFieldAttr(ID : Word; Color, Mono : Byte);
    {-Change attributes for protected fields}
  begin
    if FindField(ID) <> nil then
      FindField(ID)^.SetProtectedFieldAttr(Color, Mono);
  end;

  procedure AbstractSelector.ChangeCtrlAttr(ID : Word; Color, Mono : Byte);
    {-Change attributes for control characters}
  begin
    if FindField(ID) <> nil then
      FindField(ID)^.SetCtrlAttr(Color, Mono);
  end;

  function AbstractSelector.FindField(ID : Word) : SelectFieldPtr;
    {-Return a pointer to the specified field}
  const
    SFP : SelectFieldPtr = nil;
  var
    I, J : Word;
    KnownID : Word;
  begin
    with asFields do
      if ID >= asCount then
        FindField := nil
      else begin
        KnownID := asKnown^.sfID;
        if ID = KnownID then
          {already known}
          SFP := asKnown
        else if ID = Succ(KnownID) then
          {it's the next field}
          SFP := Pointer(asKnown^.dlNext)
        else if ID = Pred(KnownID) then
          {it's the previous field}
          SFP := Pointer(asKnown^.dlPrev)
        else if ID = 0 then
          {it's the first field}
          SFP := Pointer(Head)
        else if ID = Pred(asCount) then
          SFP := Pointer(Tail)
        else begin
          {we'll have to search}
          if (ID > KnownID) then begin
            {it's beyond the known field--start at the next field}
            SFP := asKnown;
            J := Succ(KnownID);
          end
          else begin
            {it's before the known field--start at the first field}
            SFP := Pointer(Head);
            J := 1;
          end;

          {search for the field}
          for I := J to ID do
            SFP := Pointer(SFP^.dlNext);
        end;

        {save the pointer and return it}
        asKnown := SFP;
        FindField := SFP;
      end;
  end;

  function AbstractSelector.FindFieldByName(Name : string) : SelectFieldPtr; {!!.01}
    {-Return a pointer to the field associated with Name (case-insensitive)}
  var
    SFP : SelectFieldPtr;
  begin
    SFP := Pointer(asFields.Head);
    while SFP <> nil do begin
      with SFP^ do
        if (sfFieldName <> nil) and
           (CompUCString(sfFieldName^, Name) = Equal) then begin
          FindFieldByName := SFP;
          Exit;
        end;
      SFP := Pointer(SFP^.dlNext);
    end;
    FindFieldByName := nil;
  end;

  function AbstractSelector.FindTextField(ID : Word) : TextFieldPtr;
    {-Return a pointer to the specified text field}
  var
    TFP : TextFieldPtr;
  begin
    TFP := Pointer(asTextFields.Head);
    while TFP <> nil do
      if TFP^.tfID = ID then begin
        FindTextField := TFP;
        Exit;
      end
      else
        TFP := Pointer(TFP^.slNext);
    FindTextField := nil;
  end;

  procedure AbstractSelector.SetNextField(ID : Word);
    {-Set value of asNext}
  begin
    if ID < asCount then
      asNext := FindField(ID)
    else
      asNext := nil;
  end;

  function AbstractSelector.GetCurrentID : Word;
    {-Get the ID for the current field}
  begin
    GetCurrentID := asCurID;
  end;

  function AbstractSelector.GetHelpIndex(ID : Word) : Word;
    {-Get the help index for the specified field}
  var
    SFP : SelectFieldPtr;
  begin
    SFP := FindField(ID);
    if SFP = nil then
      GetHelpIndex := 0
    else
      GetHelpIndex := SFP^.sfHelpIndex;
  end;

  procedure AbstractSelector.SetAllFieldLinks(var LinksMap);
    {-Set the forward and backward links for all fields}
  var
    Map : array[0..2000] of Word absolute LinksMap;
    I : Integer;
    SFP : SelectFieldPtr;

    function FindBackwardLink(ID : Word) : Word;
    var
      I : Word;
    begin
      {assume failure}
      FindBackwardLink := BadFieldID;

      {search for ID in Map}
      for I := 0 to asCount-1 do
        if Map[I] = ID then begin
          FindBackwardLink := I;
          Exit;
        end;
    end;

  begin
    {set forward links}
    SFP := Pointer(asFields.Head);
    for I := 0 to asCount-1 do begin
      SFP^.sfNextID := Map[I];
      SFP := Pointer(SFP^.dlNext);
    end;

    {set backward links}
    SFP := Pointer(asFields.Head);
    for I := 0 to asCount-1 do begin
      SFP^.sfPrevID := FindBackwardLink(I);
      SFP := Pointer(SFP^.dlNext);
    end;
  end;

  procedure AbstractSelector.SetFieldLinks(ID, Next, Prev : Word);
    {-Specify the fields to jump to when <Tab> or <ShTab> pressed on a
      given field}
  var
    SFP : SelectFieldPtr;
  begin
    SFP := FindField(ID);
    if SFP <> nil then begin
      SFP^.sfNextID := Next;
      SFP^.sfPrevID := Prev;
    end;
  end;

  procedure AbstractSelector.ChangeProtection(ID : Word; IsOn : Boolean);
    {-Change the protection status of the specified field}
  var
    SFP : SelectFieldPtr;
  begin
    {locate the field}
    SFP := FindField(ID);
    if SFP <> nil then begin
      {change the protection status}
      if IsOn then
        SetLongFlag(SFP^.sfOptions, sfProtected)
      else
        ClearLongFlag(SFP^.sfOptions, sfProtected);

      {indicate that flags need to be reset}
      asMinRow := 0;
      asMaxRow := 0;
    end;
  end;

  procedure AbstractSelector.ChangeHidden(ID : Word; IsOn : Boolean);
    {-Change the hidden status of the specified field}
  var
    SFP : SelectFieldPtr;
  begin
    {locate the field}
    SFP := FindField(ID);
    if SFP <> nil then begin
      {change the hidden status}
      if IsOn then
        SetLongFlag(SFP^.sfOptions, sfHidden)
      else
        ClearLongFlag(SFP^.sfOptions, sfHidden);

      {indicate that flags need to be reset}
      asMinRow := 0;
      asMaxRow := 0;
    end;
  end;

  procedure AbstractSelector.ChangeInvisibility(ID : Word; IsOn : Boolean);
    {-Change the invisibility status of the specified field}
  var
    SFP : SelectFieldPtr;
  begin
    {locate the field}
    SFP := FindField(ID);
    if SFP <> nil then begin
      {change the invisibility status}
      if IsOn then
        SetLongFlag(SFP^.sfOptions, sfInvisible)
      else
        ClearLongFlag(SFP^.sfOptions, sfInvisible);

      {indicate that flags need to be reset}
      asMinRow := 0;
      asMaxRow := 0;
    end;
  end;

  procedure AbstractSelector.asResetFlags;
    {-Reset internal flags}
  var
    SFP : SelectFieldPtr;
    LowCol, HighCol : Word;
  begin
    {do nothing if min/max row already known}
    if (asMinRow <> 0) and (asMaxRow <> 0) then
      Exit;

    {no fast updates}
    ResetScreen;

    {find the first unprotected field and save the row it's on}
    SFP := Pointer(asFields.Head);
    while asFieldIsProtected(SFP) do
      SFP := Pointer(SFP^.dlNext);
    if SFP = nil then begin                       {!!.12}
      GotError(epFatal+ecNoFields, emNullError);  {!!.12}
      Exit;                                       {!!.12}
    end;                                          {!!.12}
    asMinRow := SFP^.sfFRow;

    {find the last unprotected field and save the row it's on}
    SFP := Pointer(asFields.Tail);
    while asFieldIsProtected(SFP) do
      SFP := Pointer(SFP^.dlPrev);
    asMaxRow := SFP^.sfFRow;

    {do nothing more if min/max column already known}
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

{$IFDEF UseStreams}
  procedure TextFieldStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing text fields}
  begin
    with SPtr^ do begin
      RegisterType(otTextField, veTextField,
                   TypeOf(TextField),
                   @TextField.Store, @TextField.Load);
      RegisterType(otTextLineField, veTextLineField,            {!!.03}
                   TypeOf(TextLineField),                       {!!.03}
                   @TextLineField.Store, @TextLineField.Load);  {!!.03}
      RegisterType(otTextBoxField, veTextBoxField,              {!!.03}
                   TypeOf(TextBoxField),                        {!!.03}
                   @TextBoxField.Store, @TextBoxField.Load);    {!!.03}
    end;
  end;

  procedure SelectFieldStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing select fields}
  begin
    with SPtr^ do
      RegisterType(otSelectField, veSelectField, TypeOf(SelectField),
                   @SelectField.Store, @SelectField.Load);
  end;
{$ENDIF}

begin
  {initialize command processor}
  SelectCommands.Init(@SelectKeySet, SelectKeyMax);
end.
