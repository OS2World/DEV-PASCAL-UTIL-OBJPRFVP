{$R-,S-,I-,V-,B-,F-,O-}

{*********************************************************}
{*                   MSUTIL.PAS 1.30                     *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This unit requires that OPDEFINE.INC activate the following defines:
   UseScrollBars, UseHotSpots, UseAdjustableWindows, UseShadows, UseStreams
 This unit will use features activated with the following defines:
   UseMouse, UseBcd, N+, UseDates,
 ***************************************************************************}

{$IFNDEF UseScrollBars}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

{$IFNDEF UseHotSpots}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

{$IFNDEF UseShadows}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

{$IFNDEF UseStreams}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

unit MSUtil;
  {-Utility routines for MAKESCRN}

interface

uses
  Use32,
  dos,
  opinline,
  oproot,
  opconst,                                                             {!!.20}
  opcmd,
  opcrt,
  {$IFDEF UseMouse}
  opmouse,
  {$ENDIF}
  opstring,
  opdos,
  opframe,
  opwindow,
  {$IFDEF UseBcd}
  opbcd,
  {$ENDIF}
  {$IFDEF UseDates}
  opdate,
  {$ENDIF}
  opabsfld,
  opfield,
  opedit,
  opselect,
  opentry,
  oppick,
  opdir,
  opmenu,
  makemisc,
  makeces;

type
  StatusPos  = (spHidden, spBottom, spTop);
  StatusWindow =
    object(StackWindow)
      swPos : StatusPos;

      constructor Init;
        {-Create a status window}
      procedure UpdateContents; virtual;
        {-Called to update the status window's contents}
      procedure Display;
        {-Display the status window}
      function IsTiled : Boolean;
        {-Returns True if no other windows overlap the status window}

      procedure SetStatusPos(NSP : StatusPos);
        {-Set the position of the status line}
      procedure Toggle;
        {-Toggle the status line}
      procedure FastUpdate(KPP : KeyPressedProc);
        {-Update the status line if time allows}
    end;

  BackdropType  = (bdPlain, bdCanvas, bdGrid);
const
  {backdrop for object being designed}
  DefBackdrop   : BackdropType = bdCanvas;

  GenUserHooks  : Boolean = True;
  GenHelpNames  : Boolean = True;
  GenStreamCode : Boolean = False;
  GenFieldNames : Boolean = False; {!!.01}

  PathLen       = SizeOf(PathStr)-1;

  SliderChar    = '²';
  ScrollBarChar = '°';
  BackdropAttr  = $07;
  BackdropChar  = '°';

  {colors}
  MakeScrnColors : ColorSet = (
    TextColor       : $1A; TextMono       : $07;
    CtrlColor       : $1A; CtrlMono       : $07;
    FrameColor      : $1A; FrameMono      : $07;
    HeaderColor     : $1F; HeaderMono     : $70;
    ShadowColor     : $08; ShadowMono     : $70;
    HighlightColor  : $1F; HighlightMono  : $0F;
    PromptColor     : $1A; PromptMono     : $07;
    SelPromptColor  : $1A; SelPromptMono  : $07;
    ProPromptColor  : $17; ProPromptMono  : $07;
    FieldColor      : $1F; FieldMono      : $0F;
    SelFieldColor   : $1F; SelFieldMono   : $0F;
    ProFieldColor   : $17; ProFieldMono   : $07;
    ScrollBarColor  : $13; ScrollBarMono  : $07;
    SliderColor     : $13; SliderMono     : $0F;
    HotSpotColor    : $30; HotSpotMono    : $70;
    BlockColor      : $3E; BlockMono      : $0F;
    MarkerColor     : $5F; MarkerMono     : $70;
    DelimColor      : $31; DelimMono      : $0F;
    SelDelimColor   : $31; SelDelimMono   : $0F;
    ProDelimColor   : $31; ProDelimMono   : $0F;
    SelItemColor    : $2F; SelItemMono    : $70;
    ProItemColor    : $17; ProItemMono    : $07;
    HighItemColor   : $1F; HighItemMono   : $0F;
    AltItemColor    : $1F; AltItemMono    : $0F;
    AltSelItemColor : $2F; AltSelItemMono : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono  : $0F;
    FlexBHelpColor  : $1F; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono  : $70;
    UnselXrefColor  : $1E; UnselXrefMono  : $09;
    SelXrefColor    : $5F; SelXrefMono    : $70;
    MouseColor      : $4F; MouseMono      : $70
  );

  {defaults for new entry screens}
  DefOptions : LongInt = wClear+wSaveContents+wResizeable+wUserContents;
  defXL      = 05;
  defYL      = 05;
  defXH      = 15;
  defYH      = 10;
  DefColors  : ColorSet = (
    TextColor       : $1E; TextMono       : $0F;
    CtrlColor       : $1E; CtrlMono       : $0F;
    FrameColor      : $13; FrameMono      : $07;
    HeaderColor     : $3F; HeaderMono     : $70;
    ShadowColor     : $08; ShadowMono     : $0F;
    HighlightColor  : $4F; HighlightMono  : $70;
    PromptColor     : $17; PromptMono     : $07;
    SelPromptColor  : $17; SelPromptMono  : $07;
    ProPromptColor  : $17; ProPromptMono  : $07;
    FieldColor      : $1E; FieldMono      : $07;
    SelFieldColor   : $31; SelFieldMono   : $0F;
    ProFieldColor   : $17; ProFieldMono   : $07;
    ScrollBarColor  : $13; ScrollBarMono  : $07;
    SliderColor     : $13; SliderMono     : $0F;
    HotSpotColor    : $30; HotSpotMono    : $70;
    BlockColor      : $3E; BlockMono      : $0F;
    MarkerColor     : $3F; MarkerMono     : $70;
    DelimColor      : $1E; DelimMono      : $0F;
    SelDelimColor   : $31; SelDelimMono   : $0F;
    ProDelimColor   : $1E; ProDelimMono   : $0F;
    SelItemColor    : $3E; SelItemMono    : $70;
    ProItemColor    : $17; ProItemMono    : $07;
    HighItemColor   : $1F; HighItemMono   : $0F;
    AltItemColor    : $1F; AltItemMono    : $0F;
    AltSelItemColor : $3F; AltSelItemMono : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono  : $0F;
    FlexBHelpColor  : $1F; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono  : $70;
    UnselXrefColor  : $1E; UnselXrefMono  : $09;
    SelXrefColor    : $3F; SelXrefMono    : $70;
    MouseColor      : $4F; MouseMono      : $70
  );

  {user exit commands}
  MainMenuCmd     = ccUser0;
  MoveWindowCmd   = ccUser1;
  ResizeWindowCmd = ccUser2;
  ExitCmd         = ccUser3;
  SaveFileCmd     = ccUser4;
  NewFileCmd      = ccUser5;
  TestCmd         = ccUser6;
  InfoCmd         = ccUser7;
  StatusCmd       = ccUser8;

  YesNoSt    : array[Boolean] of array[1..3] of Char = ('No ', 'Yes');
  WrapSt     : array[WrapMode] of array[1..7] of Char = (    {!!.01}
               'Stop   ', 'Wrap   ', 'Exit   ', 'ExitTop', 'ExitBot');
  SavePrompt : string[25] = 'Object modified. Save it?';

  DefLibEntries = 15;        {default size of library's directory}
  DefLibExt  = 'OPL';        {default file extension for libraries}
  DefSrcExt  = 'PAS';        {default file extension for source code}
  DefDocExt  = 'DOC';        {default file extension for documentation}

  HaveLib    : Boolean = False; {do we have a library?}
  HaveCES    : Boolean = False; {do we have an entry screen?}
  Modified   : Boolean = False; {has the entry screen been modified}
  LibName    : PathStr = ''; {name of current library}
var
  Status     : StatusWindow; {status window}
  CES        : CustomEntryScreen; {entry screen being designed}
  ObjName    : DirEntryName; {name of current object in library's directory}
  Lib        : OpLibrary;      {current library}

procedure PopupErrorMessage(Msg : string);
  {-Display an error message in a popup window}

procedure InsufficientMemory;
  {-Display an 'insufficient memory' error message in a popup window}

procedure OverlapError;
  {-Display an error message about overlapping fields}

procedure PopupMessage(Header, Msg : string);
  {-Display a non-error message in a popup window}

procedure PopupDelayMessage(Msg : string);
  {-Display a non-error message in a popup window for 3/4 of a second}

function PopupGetString(Header, Prompt : string;
                        ForceUp, TrimBlanks : Boolean;
                        MaxLen : Byte;
                        var S : string) : Boolean;
  {-Prompt for a string within a popup window}

function GetFileByList(var S : String) : Word;
  {-Let the user choose a filename from a list}

function PopupGetFileName(Header, Prompt : string; MaxLen : Byte;
                          DefExt : string; var S : string) : Boolean;
  {-Prompt for a filename within a popup window}

function PopupGetExistingFile(Header, Prompt : string; MaxLen : Byte;
                              DefExt : string; var S : string) : Boolean;
  {-Prompt for name of an existing file within a popup window}

function PopupGetLong(Header, Prompt : string; var L : LongInt;
                      LLo, LHi : LongInt) : Boolean;
  {-Prompt for a longint within a popup window}

function PopupGetWord(Header, Prompt : string; var W : Word;
                      WLo, WHi : Word) : Boolean;

function PopupGetByte(Header, Prompt : string; var B : Byte;
                      BLo, BHi : Byte) : Boolean;

function PopupYesNo(Header, Prompt : string; Default : Char;
                    var Escaped : Boolean) : Boolean;
  {-Prompt for a response to a yes-no question within a popup window}

function PopupGetChar(Header, Prompt : string;
                      ValidChars : CharSet;
                      var Ch : Char) : Boolean;
  {-Prompt for a character within a popup window}

function ConfirmAction(Header : string) : Boolean;
  {-Confirm an action}

function ExistCheck(FName : PathStr) : Boolean;
  {-Returns True if FName doesn't exist or may be overwritten}

procedure OurErrorProc(UnitCode : Byte; var ErrCode : Word; Msg : string);
  {-Error handler}

function AbleToRewrite(var OutF : Text; var Name : PathStr) : Boolean;
  {-Return True if able to Rewrite the specified file}

function ZeroPad(L : LongInt) : string;
  {-Return L as a zero-padded string}

procedure DrawBackdrop;
  {-Draw the backdrop for the object being designed}

procedure ToggleBackdrop;
  {-Switch to a new backdrop}

function MergeEntryScreen : Boolean;
  {-Load an entry screen}

procedure SaveEntryScreen;
  {-Save the current entry screen}

function CreateEntryScreen : Boolean;
  {-Create a new entry screen}

function CreateLibrary : Boolean;
  {-Create a new library}

function OpenLibrary(Name : PathStr) : Boolean;
  {-Open the library of the specified name, or create it if necessary}

procedure DisposeEntryScreen;
  {-Dispose of an entry screen}

procedure CloseLibrary;
  {-Close the current library}

procedure DeleteObjects;
  {-Display a list of objects and delete/undelete the one chosen}

procedure RenameObject(MakeNewObj : Boolean);
  {-Rename the current object or save it under new name}

procedure ShowLibraryInfo;
  {-Display info about the current library}

procedure PackLibrary;
  {-Pack the current library}

function OkToQuit : Boolean;
  {-Returns True if OK to quit program}
                                                  {!!.03}
function EditChar(Header : string; var Ch : Char; MustChange : Boolean) : Boolean;
  {-Edit the specified character}

function EditDelims : Boolean;
  {-Edit field delimiters}

procedure ToggleScrollBar(FET : FrameEdgeType);
  {-Toggle the specified scroll bar}

procedure ToggleShadows;
  {-Toggle shadow types}

function EditColor(HSt : string; var Color, Mono : Byte) : Boolean; {!!.03}
  {-Edit the specified pair of colors}

procedure EditAttr(HSt : string; var A : Byte; IsColor : Boolean);
  {-Edit the specified attribute}

function ChooseHeader(var HP : HeaderPtr) : Boolean;
  {-Choose a header to edit}

function ChooseHeaderType(var HPT : HeaderPosType) : Boolean;
  {-Choose a header type}

procedure RemoveHeader;
  {-Remove a header}

procedure EditHeader;
  {-Choose and edit a header}

procedure AddHeader;
  {-Add a header}

function EditFramePrim(var FR : FrameArray; Left : Byte) : Boolean; {!!.03}
  {-Select a frame type}

procedure EditFrame;
  {-Select a frame type}

{---------------- code generation, etc. ---------------}

const
  RealTypeCodes : set of Byte = [
    otRealEField, otBcdEField, otExtendedEField,
    otDoubleEField, otSingleEField, otCompEField];
  IntegerTypeCodes : set of Byte = [
    otLongIntEField, otWordEField, otIntegerEField,
    otByteEField, otShortIntEField];
  CharEditorTypeCodes : set of Byte = [
    otCharEField, otBooleanEField, otYesNoEField];
  HaveRangeTypeCodes : set of Byte = [
    otCharEField, otLongIntEField, otWordEField, otIntegerEField,
    otByteEField, otShortIntEField, otRealEField, otBcdEField,
    otExtendedEField, otDoubleEField, otSingleEField, otCompEField,
    otDateEField, otTimeEField];
  SimpleTypeCodes : set of Byte = [
    otStringEField, otCharEField, otBooleanEField, otYesNoEField,
    otLongIntEField, otWordEField, otIntegerEField, otByteEField,
    otShortIntEField, otRealEField, otBcdEField, otExtendedEField,
    otDoubleEField, otSingleEField, otCompEField];
  NumericTypeCodes : set of Byte = [
    otLongIntEField, otWordEField, otIntegerEField, otByteEField,
    otShortIntEField, otRealEField, otBcdEField, otExtendedEField,
    otDoubleEField, otSingleEField, otCompEField];
  PictureChars : set of Char = [
    AnyChar, ForceUp, ForceLo, ForceMixed, AlphaOnly, UpperAlpha, LowerAlpha,
    NumberOnly, DigitOnly, Scientific, HexOnly, BooleanOnly, YesNoOnly,
    {$IFDEF UseDates}
    NameOnly, NameOnlyU, MonthOnly, DayOnly, YearOnly, HourOnly, MinOnly, SecOnly,
    MonthOnlyU, DayOnlyU, HourOnlyU, MinOnlyU, SecOnlyU, TimeOnly, EmOnly,
    {$ENDIF}
    User1..User8];

  edNormal  = 0;                  {editor types}
  edSimple  = 1;
  edNumeric = 2;

  EditorSt : array[edNormal..edNumeric] of string[7] = (
    'Normal', 'Simple', 'Numeric');

function GetTypeCode(EFP : EntryFieldPtr) : Byte;
  {-Get the type code for the field}

function GetEditor(EFP : EntryFieldPtr) : Byte;
  {-Get the editor for the field}

function TypeCode2Str(TypeCode : Byte) : string;
  {-Convert a type code to a string}

function FieldName(EFP : EntryFieldPtr) : string;
  {-Return the user record field name for an entry field}

procedure GenerateSource;
  {-Generate source code for the current object}

procedure DocumentEntryScreen;
  {-Write a text listing of the entry screen}

  {===========================================================}

implementation

type
  TypeCodePacket =
    record
      TC : Byte;
      B : Boolean;
    end;
const
  Comma : string[02] = ', ';
var
  RegisteredTypes :
    array[otArrayEField..otYesNoEField, edNormal..edNumeric] of Boolean;
  Popup : StackWindow;  {generic popup window}
  OutF  : Text;

  {$I MSUTIL.IN1}

  procedure GenerateEntryScreen;
    {-Entry screen initialization}
  const
    MinBit = 0;
    MaxBit = 2;
    EsOptionNames : array[MinBit..MaxBit] of string[14] = (
      'esScrollByPage', 'esMousePage', 'esReadOnly');
    WMSt : array[WrapMode] of string[11] =  {!!.01}
      ('StopAtEdges', 'WrapAtEdges', 'ExitAtEdges', 'ExitAtTop', 'ExitAtBot');
  var
    X1 : Byte;
    Y1 : Byte;
    X2 : Byte;
    Y2 : Byte;
    Bit, Flag : Byte;
    SOn : string;
    SOff : string;
  begin
    with CES do begin
      {generate with statement}
      GenLine(2, 'with ES do begin');

      {Call the entry screen constructor}
      Coordinates(X1, Y1, X2, Y2);
      WriteLn(OutF, '    if not InitCustom(', X1, Comma, Y1, Comma,
              X2, Comma, Y2, ', EsColors, WinOptions) then begin');
      GenLine(6, 'InitEntryScreen := InitStatus;');
      GenLine(6, 'Exit;');
      GenLine(4, 'end;'^M^J);

      {Set frame type if any}
      if HasFrame then
        GenLine(4, 'wFrame.SetFrameType(Frame1);');

      {Generate code to ornament the main window}
      GenerateExploding(OutF, CES, 4);
      GenerateShadows(OutF, CES, 4);
      GenerateHeaders(OutF, CES, 4);
      GenerateScrollBars(4);

      {Set entry screen options on and off}
      SOn := '';
      SOff := '';
      for Bit := MinBit to MaxBit do begin
        Flag := 1 shl Bit;
        if esOptionsAreOn(Flag) then begin
          {don't set it if it's ON by default}
          if not ByteFlagIsSet(DefEntryOptions, Flag) then begin
            if Length(SOn) <> 0 then
              SOn := SOn+'+';
            SOn := SOn+EsOptionNames[Bit];
          end;
        end
        {don't clear it if it's OFF by default}
        else if ByteFlagIsSet(DefEntryOptions, Flag) then begin
          if Length(SOff) <> 0 then
            SOff := SOff+'+';
          SOff := SOff+EsOptionNames[Bit];
        end;
      end;
      if Length(SOn) <> 0 then
        GenLine(4, 'esOptionsOn('+SOn+');');
      if Length(SOff) <> 0 then
        GenLine(4, 'esOptionsOff('+SOff+');');

      {Set field options on and off}
      GenerateFieldOptions(asFieldOptions, DefEFieldOptions);
      GenerateSecFieldOptions(esFieldFlags, DefSEFieldOptions);

      {wrap mode}
      GenLine(4, 'SetWrapMode('+WMSt[asWrapMode]+');'); {!!.01}

      {pad character}
      if esPadChar <> DefPadChar then
        GenLine(4, 'SetPadChar('+PascalChar(esPadChar)+');');

      {password character}
      if esPasswordChar <> DefPasswordChar then
        GenLine(4, 'SetPasswordChar('+PascalChar(esPasswordChar)+');');

      {delimiters}
      if (esLeftD <> #0) and (esRightD <> #0) then
        GenLine(4, 'SetDelimiters('+PascalChar(esLeftD)+Comma+
                       PascalChar(esRightD)+');');

      (*                            {!!.11}
      {set procedure pointers}
      if GenUserHooks then begin
        BlankLine;
        GenLine(4, 'SetPreEditProc(PreEdit);');
        GenLine(4, 'SetPostEditProc(PostEdit);');
        GenLine(4, 'SetErrorProc(ErrorHandler);');
        GenLine(4, 'EntryCommands.SetHelpProc(DisplayHelp);');
      end;
      *)
    end;
  end;

  {$F+}  {!!.03} {rewritten}
  procedure WriteOneTextField(TFP : TextFieldPtr; var D; ASP : AbstractSelectorPtr);
    {-Generate an Addxxx call for the specified text field}
  const
    CSt : array[Boolean] of string[6] = ('', 'Custom');
    TFSt : array[Boolean] of string[5] = ('False', 'True');
    ShSt : array[Boolean] of string[8] = ('', 'Shadowed');
  var
    DC : Boolean;
    TLFP : TextLineFieldPtr absolute TFP;
    TBFP : TextBoxFieldPtr absolute TFP;
    TFT : cesTextFieldType;
  begin
    with CustomEntryScreenPtr(ASP)^, TLFP^, TBFP^ do begin
      {is this a custom text field?}
      DC := (tfColorAttrs[0] <> wTextColor) or (tfMonoAttrs[0] <> wTextMono);

      TFT := TextFieldType(TFP);
      case TFT of
        cesHLine, cesVLine :
          begin
            Write(OutF, '':4, 'AddLineField', CSt[DC], '(',
                  PascalChar(tfString^[1]), Comma,
                  PascalChar(tfString^[2]), Comma,
                  PascalChar(tfString^[3]), Comma,
                  tfRow, Comma, tfCol, Comma, tlfLength, Comma);
            if DC then
              {generate the Custom part of the call}
              Write(OutF, ^M^J, '':6, ColorName(tfColorAttrs[0]), Comma,
                                      ColorName(tfMonoAttrs[0]), Comma);
            Write(OutF, TFSt[TFT = cesVLine]);
          end;
        cesBox, cesShBox :
          begin
            DC := DC or (tfColorAttrs[1] <> wTextColor) or              {!!.10}
                        (tfMonoAttrs[1] <> wTextMono);                  {!!.10}
            Write(OutF, '':4, 'Add', ShSt[TFT = cesShBox], 'BoxField');
            Write(OutF, CSt[DC], '(', PascalCtrlString(tfString^), Comma,
                  tfCol, Comma, tfRow, Comma, tbfColH, Comma, tbfRowH);
            if DC then begin                                            {!!.10}
              GenLine(0, ',');
              Write(OutF, '':6, ColorName(tfColorAttrs[0]), Comma,
                                ColorName(tfMonoAttrs[0]) );
              if TFT = cesShBox then
                Write(OutF, Comma, ColorName(tfColorAttrs[1]), Comma,
                            ColorName(tfMonoAttrs[1]) );
            end;
          end;
        else
          begin
            {generate the first part of AddTextField call}
            GenLine(4, 'AddTextField'+CSt[DC]+'(');
            Write(OutF, '':6, PascalString(tfString^), Comma, tfRow, Comma, tfCol);
            if DC then begin
              {generate the Custom part of the call}
              GenLine(0, ',');
              Write(OutF, '':6, ColorName(tfColorAttrs[0]), Comma,
                                ColorName(tfMonoAttrs[0]) );
            end;
          end;
      end;

      {finish it off}
      GenLine(0, ');');
    end;
  end;

  function WriteValue(TC, TC2: Byte; var Value) : Boolean; {!!.11}
    {-Write a value of the specified type to OutF}
  var
    Range : RangeType absolute Value;
    Day, Month, Year : Integer;
    H, M, S : Byte;
    L : LongInt;

    procedure WriteLong(L : LongInt);
    begin
      if (L = $80000000) or (L = MaxLongInt) then
        Write(OutF, '$', HexL(L))
      else
        Write(OutF, L);
    end;

    procedure WriteFloat(F : Float); {!!.10} {rewritten}
    var
      W : Byte;
    begin
      if F = 0 then
        Write(OutF, '0')
      else begin
        {$IFOPT N+}
        case TC2 of                   {!!.11}
          otRealEField :
            W := $14;
          otSingleEField :
            W := $10;
          else
            W := $17;
        end;
        {$ELSE}
        W := $11;
        {$ENDIF}
        Write(OutF, FixExpReal(Real2Str(F, W, -1)));
      end;
    end;

  begin
    WriteValue := True;
    with Range do
      case TC of
        otCharEField :
          WriteString(PascalChar(rtChar));
        otLongIntEField :
          WriteLong(rtLong);
        otWordEField :
          WriteLong(rtWord);
        otIntegerEField :
          WriteLong(rtInt);
        otByteEField :
          WriteLong(rtByte);
        otShortIntEField :
          WriteLong(rtSht);
        otRealEField :
          if rtReal = BadReal then
            WriteString('BadReal')
          else
            WriteFloat(rtReal);
      {$IFDEF UseBcd}
        otBcdEField :
          if Bcds.Member(rtBCD, L) then
            case Integer(L) of
              -2 : WriteString('MinBCD');
              -1 : WriteString('MaxBCD');
               0 : WriteString('ZeroBCD');
              else Write(OutF, 'BcdVal', ZeroPad(L));
            end
          else
            WriteString('ZeroBcd {?}');
      {$ENDIF}
      {$IFOPT N+}
        otExtendedEField :
          if rtExt = BadExt then
            WriteString('BadExt')
          else
            WriteFloat(rtExt);
        otDoubleEField :
          if rtDbl = BadDbl then
            WriteString('BadDbl')
          else
            WriteFloat(rtDbl); {!!.10} {!!.11}
        otSingleEField :
          if rtSgl = BadSgl then
            WriteString('BadSgl')
          else
            WriteFloat(rtSgl);  {!!.10} {!!.11}
        otCompEField :
          if rtComp = BadComp then
            WriteString('BadComp')
          else
            WriteFloat(rtComp); {!!.10} {!!.11}
      {$ENDIF}
      {$IFDEF UseDates}
        otDateEField :
          if rtDate = MinDate then
            WriteString('MinDate')
          else if rtDate = MaxDate then
            WriteString('MaxDate')
          else if rtDate = Date1900 then
            WriteString('Date1900')
          else if rtDate = Date1980 then
            WriteString('Date1980')
          else if rtDate = BadDate then
            WriteString('BadDate')
          else begin
            DateToDMY(rtDate, Day, Month, Year);
            Write(OutF, 'DMYtoDate(', Day, Comma, Month, Comma, Year, ')');
          end;
        otTimeEField :
          if rtTime = MinTime then
            WriteString('MinTime')
          else if rtTime = MaxTime then
            WriteString('MaxTime')
          else if rtTime = BadTime then
            WriteString('BadTime')
          else begin
            TimeToHMS(rtTime, H, M, S);
            Write(OutF, 'HMStoTime(', H, Comma, M, Comma, S, ')');
          end;
      {$ENDIF}
        else
          WriteValue := False;
      end;
  end;

  function PictureToString(EFP : EntryFieldPtr; NoShow : Boolean) : string;
  var
    All : Boolean;
    EC : Byte;
    Ch : Char;
    S : string;
    SLen : Byte absolute S;
  begin
    with EFP^ do begin
      EC := GetEditor(EFP);
      if not NoShow then
        All := True
      else case GetTypeCode(EFP) of
        otBooleanEField, otYesNoEField :
          if (EC = edSimple) then begin
            {no picture for simple Boolean or YesNo fields}
            PictureToString := '{'+PascalChar(efPicture^[1])+',} ';
            Exit;
          end
          else
            {otherwise use whole picture}
            All := True;
        else
          {use only first char of picture for other simple fields}
          All := EC <> edSimple;
      end;
      if All then begin
        S := efPicture^;
        Ch := S[1];
        if (SLen > 20) and (S = CharStr(Ch, SLen)) then
          S := 'CharStr('+PascalChar(Ch)+Comma+Long2Str(SLen)+')'
        else
          S := PascalString(S);
      end
      else
        S := PascalChar(efPicture^[1]);
      PictureToString := S;
    end;
  end;

  function FieldString(TC : Byte) : string;
  begin
    case TC of
      otArrayEField    : FieldString := 'Array';
      otYesNoEField    : FieldString := 'YesNo';
      otLongIntEField  : FieldString := 'Long';
      otIntegerEField  : FieldString := 'Int';
      otShortIntEField : FieldString := 'Short';
    {$IFOPT N+}
      otExtendedEField : FieldString := 'Ext';
      otDoubleEField   : FieldString := 'Dbl';
      otSingleEField   : FieldString := 'Sgl';
    {$ENDIF}
    {$IFDEF UseDates}
      otDateStEField   : FieldString := 'DateSt';
    {$ENDIF}
      else               FieldString := TypeCode2Str(TC);
    end;
  end;

  {!!.01} {new routine}
  procedure WriteOneFieldName(EFP : EntryFieldPtr; var D; ESP : EntryScreenPtr);
    {-Generate a SetFieldName call for the specified entry field}
  var
    S : string[20];
    L : Byte absolute S;
  begin
    S := FieldName(EFP);
    WriteLn(OutF,
      '    SetFieldName(id', S, ',', '':21-L, PascalString(S), ');');
  end;

  procedure WriteOneEntryField(EFP : EntryFieldPtr; var D; ESP : EntryScreenPtr);
    {-Generate an Addxxx call for the specified entry field}
  const
    EditorString : array[edNormal..edNumeric] of string[7] = (
      '', 'Simple', 'Numeric');
  var
    TC, EC : Byte;

    function GetPicture : string;
    var
      S : string;
    begin
      S := PictureToString(EFP, True);
      case TC of
        otBooleanEField, otYesNoEField :
          if (EC = edSimple) then
            GetPicture := S
          else
            GetPicture := S+Comma;
        else
          GetPicture := S+Comma;
      end;
    end;

    procedure WriteWidth;
    begin
      {generate width for string, array, and all simple numeric types}
      case TC of
        otStringEField, otArrayEField :
          {show width};
        else
          if not ((TC in NumericTypeCodes) and (EC = edSimple)) then
            Exit;
      end;
      Write(OutF, EFP^.sfFWidth, Comma);
    end;

    procedure WriteRange(var Range : RangeType);
    var                                                 {!!.11}
      C : Byte;                                         {!!.11}
    begin
      case TC of                                        {!!.11}
        otDoubleEField, otSingleEField, otCompEField :  {!!.11}
          C := otExtendedEField;                        {!!.11}
        else                                            {!!.11}
          C := TC;                                      {!!.11}
      end;                                              {!!.11}
      if WriteValue(C, TC, Range) then                  {!!.11}
        WriteString(Comma);
    end;

    procedure WriteDecimalPlaces;
    begin
      {write decimal places for all real types but Comp}
      if (TC <> otCompEField) and (TC in RealTypeCodes) then
        Write(OutF, EFP^.efDPlaces, Comma);
    end;

    procedure GenerateColorChange(S : string; Color, Mono : Byte);
    begin
      WriteLn(OutF, '    Change', S, 'Attr('^M^J,
              '':6, SymName(EFP), Comma,
              ColorName(Color), Comma, ColorName(Mono), ');');
    end;

  begin
    with ESP^, EFP^ do begin
      {categorize the field by type and editor}
      TC := GetTypeCode(EFP);
      EC := GetEditor(EFP);

      {show the field id}
      GenLine(2, '{'+SymName(EFP)+':}');

      {fix option flags if necessary}
      GenerateFieldOptions(sfOptions, asFieldOptions);
      GenerateSecFieldOptions(sfFlags, esFieldFlags);

      {fix pad character if necessary}
      if efPadChar <> esPadChar then
        GenLine(4, 'SetPadChar('+PascalChar(efPadChar)+');');

      {generate basic AddxxxField call}
      WriteLn(OutF,
        '    Add', EditorString[EC], FieldString(TC), 'Field(');
      WriteLn(OutF,
        '':6, PascalString(efPrompt^), Comma, sfPRow, Comma, sfPCol, Comma);

      Write(OutF,
        '':6, GetPicture, sfFRow, Comma, sfFCol, Comma);
      WriteWidth;
      if (TC = otStringEField) and (EC = edSimple) then
        Write(OutF, efMaxLen, Comma);
      BlankLine;

      Write(OutF, '':6, HelpName(EFP), Comma);
      WriteRange(efRangeLo);
      WriteRange(efRangeHi);
      WriteDecimalPlaces;
      GenLine(0, 'UR.'+FieldName(EFP)+');');

      {put option flags back the way they were}
      GenerateFieldOptions(asFieldOptions, sfOptions);
      GenerateSecFieldOptions(esFieldFlags, sfFlags);

      {put pad character back the way it was}
      if efPadChar <> esPadChar then
        GenLine(4, 'SetPadChar('+PascalChar(esPadChar)+');');

      {fix colors as necessary}
      with asColors do begin
        if (sfPromptColor <> PromptColor) or (sfPromptMono <> PromptMono) then
          GenerateColorChange('Prompt', sfPromptColor, sfPromptMono);
        if (sfSelPromptColor <> SelPromptColor) or (sfSelPromptMono <> SelPromptMono) then
          GenerateColorChange('SelectedPrompt', sfSelPromptColor, sfSelPromptMono);
        if (sfProPromptColor <> ProPromptColor) or (sfProPromptMono <> ProPromptMono) then
          GenerateColorChange('ProtectedPrompt', sfProPromptColor, sfProPromptMono);
        if (sfFieldColor <> FieldColor) or (sfFieldMono <> FieldMono) then
          GenerateColorChange('Field', sfFieldColor, sfFieldMono);
        if (sfSelFieldColor <> SelFieldColor) or (sfSelFieldMono <> SelFieldMono) then
          GenerateColorChange('SelectedField', sfSelFieldColor, sfSelFieldMono);
        if (sfProFieldColor <> ProFieldColor) or (sfProFieldMono <> ProFieldMono) then
          GenerateColorChange('ProtectedField', sfProFieldColor, sfProFieldMono);
        if (sfCtrlColor <> CtrlColor) or (sfCtrlMono <> CtrlMono) then
          GenerateColorChange('Ctrl', sfCtrlColor, sfCtrlMono);
      end;

      BlankLine;
    end;
  end;

  procedure FillOneField(EFP : EntryFieldPtr; var D; ESP : EntryScreenPtr);
    {-Generate FillChar statements for particular fields}
  var
    TC : Byte;

    function ZeroFilled(var Value; Size : Byte) : Boolean;
    var
      Range : RangeType absolute Value;
      I : Word;
    begin
      ZeroFilled := False;
      for I := 1 to Size do
        if Range.rt10[I] <> 0 then
          Exit;
      ZeroFilled := True;
    end;

  begin
    TC := GetTypeCode(EFP);
    case TC of
      otArrayEField :
        WriteLn(OutF, '  FillChar(UR.', FieldName(EFP), ', SizeOf(UR.',
                      FieldName(EFP), '), '#39' '#39');');
      otCharEField, otLongIntEField, otWordEField, otIntegerEField,
      otByteEField, otShortIntEField, otRealEField, otBcdEField,
      otExtendedEField, otDoubleEField, otSingleEField, otCompEField,
      otDateEField, otTimeEField :
        with EFP^ do
          {do we need to assign a special value?}
          if not ZeroFilled(efVarPtr^, efDataSize) then begin
            WriteString('  UR.'+FieldName(EFP)+' := ');
            if not WriteValue(TC, TC, efVarPtr^) then  {!!.11}
              WriteString('?');
            GenLine(0, ';');
          end;
    end;
  end;
  {$F-}

  procedure RegisterOneFieldType(TC, EC : Byte);
    {-Generate source to register one field type}
  const
    EdCodeSt : array[edNormal..edNumeric] of string[3] = ('', 'Sim', 'Num');
  var
    S : string[20];
  begin
    case TC of
      otArrayEField, otYesNoEField, otDateStEField :
        S := FieldString(TC);
      else
        S := TypeCode2Str(TC);
    end;
    GenRegisterHier(2, S+'Field'+EdCodeSt[EC]);
  end;

  procedure GenerateSource; {!!.11} {rewritten}
    {-Generate source code for the current object}
  label
    CloseUp, CloseUp2;
  const
    UnitNameMsg = 'Name of unit [.';
    HasNoFields = 'Entry screen has no fields';
    SuccessMsg = 'Source code written successfully...';
    ErrorMsg = 'Error ';
    WritingSrc = ' writing source';
  var
    Status : Word;
    UnitName, TestName : PathStr;

    function ESS : string;
    begin
      if CES.IsScrollable then
        ESS := 'ScrollingEntryScreen'
      else
        ESS := 'EntryScreen';
    end;

    {$IFOPT N+}
    function NeedNPlus : Boolean;
    begin
      NeedNPlus := HaveTypeCode(otExtendedEField) or HaveTypeCode(otDoubleEField) or
                   HaveTypeCode(otSingleEField) or HaveTypeCode(otCompEField);
    end;
    {$ENDIF}

    {$IFDEF UseDates}
    function NeedOpDate : Boolean;
    begin
      NeedOpDate := HaveTypeCode(otDateEField) or HaveTypeCode(otDateStEField) or
                    HaveTypeCode(otTimeEField);
    end;
    {$ENDIF}

    procedure GeneratePreEditShell;
    begin
      GenLine(0, 'begin');
      GenLine(2,   'with ESP^ do');
      GenLine(4,     'case GetCurrentID of');
      CES.VisitAllEntryFields(WriteCaseLabel, OutF);
      GenLine(4,     'end;');
      GenLine(0, 'end;');
    end;

    procedure GenerateFieldRegistration;
    var
      TC, EC : Byte;
    begin
      FillChar(RegisteredTypes, SizeOf(RegisteredTypes), False);
      CES.VisitAllEntryFields(CheckFieldType, OutF);
      for TC := otArrayEField to otYesNoEField do
        for EC := edNormal to edNumeric do
          if RegisteredTypes[TC, EC] then
            RegisterOneFieldType(TC, EC);
    end;

    procedure MakeTestName;
    var
      Dir  : DirStr;
      Name : NameStr;
      Ext  : ExtStr;
    begin
      FSplit(UnitName, Dir, Name, Ext);
      TestName := CleanPathName(Dir+'T'+Name+Ext);
    end;

    procedure MakeBoilerplate(ForUnit : Boolean);
    begin
      {Generate compiler directives}
      GenLine(0, '{$R-,S-,I-,V-,B-}');
      {$IFOPT N+}
      if NeedNPlus then
        GenLine(0, '{$N+,E+}');
      {$ENDIF}

      {Starting boilerplate}
      BlankLine;
      if ForUnit then begin
        GenLine(0, 'unit '+JustName(UnitName)+';'^M^J);
        GenLine(0, 'interface'^M^J);
      end
      else
        GenLine(0, 'program T'+JustName(UnitName)+';'^M^J);
      GenLine(0, '{$I OPDEFINE.INC}'^M^J);
      if GenStreamCode then
        GenLine(0, '{.$DEFINE TestStream}'^M^J);
      if ForUnit and GenFieldNames then
        GenLine(0, '{.$DEFINE AddNames}'^M^J);
    end;

    function MakeUsesList(ForUnit : Boolean) : Boolean;
    begin
      MakeUsesList := False;
      GenLine(0, 'uses');
      GenLine(2,   'Dos,');
      GenLine(2,   'OpInline,');
      GenLine(2,   'OpString,');
      GenLine(2,   'OpRoot,');
      GenLine(2,   'OpCrt,');
      if GenColorNames then
        GenLine(2, 'OpColor,');
      if CES.cesMouseSupport then begin
        GenLine(2, '{$IFDEF UseMouse}');
        GenLine(2, 'OpMouse,');
        GenLine(2, '{$ENDIF}');
      end;
      {$IFDEF UseBcd}
      if HaveTypeCode(otBcdEField) then begin
        GenLine(2, 'OpBcd,');
        Status := 0;
        CES.VisitAllEntryFields(AddBcdRange, Status);
        if Status <> 0 then
          Exit;
      end;
      {$ENDIF}
      {$IFDEF UseDates}
      if NeedOpDate then
        GenLine(2, 'OpDate,');
      {$ENDIF}
      GenLine(2,   'OpAbsFld,');
      GenLine(2,   'OpCmd,');
      GenLine(2,   'OpField,');
      GenLine(2,   'OpFrame,');
      GenLine(2,   'OpWindow,');
      GenLine(2,   'OpSelect,');
      if ForUnit then
        GenLine(2,   'OpEntry;')
      else begin
        GenLine(2,   'OpEntry,');
        GenLine(2,   JustName(UnitName)+';');
      end;
      BlankLine;
      MakeUsesList := True;
    end;

  begin
    {have MAKEMISC generate 'wFrame.' overrides}
    GenWFrame := True;

    {make sure entry screen has some fields}
    if not CES.HasDataFields then begin
      PopupErrorMessage(HasNoFields);
      Exit;
    end;

    {Assure there's a bit of working memory}
    if (MemAvail < 4096) or (MaxAvail < 1024) then begin
      InsufficientMemory;
      Exit;
    end;

    {get the names of the source files to generate}
    UnitName := ForceExtension(CleanPathName(ObjName), DefSrcExt);
    if not PopupGetFileName(
      '', UnitNameMsg+DefSrcExt+']: ', PathLen, DefSrcExt, UnitName) then
        Exit;

    {create name for test file}
    MakeTestName;

    {Open output file}
    if not AbleToRewrite(OutF, UnitName) then
      Exit;

    {$IFDEF UseBcd}
    Bcds.Init;
    {$ENDIF}

    {generate boilerplate at start of file}
    MakeBoilerplate(True);

    {generate USES list}
    if not MakeUsesList(True) then
      goto CloseUp;

    {Write list of field ID constants}
    GenLine(0, '{Entry field constants}'^M^J'const');
    CES.VisitAllEntryFields(WriteFieldConstant, OutF);
    BlankLine;

    if GenHelpNames then begin
      {Write list of help ID's}
      GenLine(0, '{Help index constants}'^M^J'const');
      CES.VisitAllEntryFields(WriteHelpConstant, OutF);
      BlankLine;
    end;

    {$IFDEF UseBcd}
    {Write list of BCD constants}
    GenerateBcdConstants;
    {$ENDIF}

    {check for I/O errors}
    Status := IoResult;
    if Status <> 0 then
      goto CloseUp;

    {generate the user record}
    GenLine(0, 'type');
    GenLine(2, 'UserRecord =');
    GenLine(4, 'record');
    CES.VisitAllEntryFields(WriteFieldDeclaration, OutF);
    GenLine(4, 'end;');
    BlankLine;

    {generate procedure names}
    GenLine(0, 'function InitEntryScreen(var ES : '+ESS+';');
    GenLine(25,                         'var UR : UserRecord;');
    GenLine(25,                         'var EsColors : ColorSet) : Word;');
    GenLine(2,    '{-Initialize entry screen generated by MAKESCRN}');
    BlankLine;
    GenLine(0, 'procedure InitUserRecord(var UR : UserRecord);');
    GenLine(2,   '{-Initialize user record}');
    BlankLine;
    if GenStreamCode then begin
      GenLine(0, '{$IFDEF TestStream}');
      GenLine(0, 'procedure RegisterTypes(var S : IdStream);');
      GenLine(2,   '{-Register data types and pointers}');
      GenLine(0, '{$ENDIF}');
      BlankLine;
    end;

    {mark start of implementation section}
    GenLine(2,   '{'+CharStr('=', 59)+'}'^M^J);
    GenLine(0, 'implementation'^M^J);

    if GenStreamCode then begin
      GenLine(0, '{$IFDEF TestStream}'+^M^J);
      GenLine(0, 'procedure RegisterTypes(var S : IdStream);');
      GenLine(0, 'begin');
      GenLine(2,   '{register entry screen}');
      GenRegisterHier(2, ESS);
      if CES.IsExploding then
        GenRegisterHier(2, 'ExplodingWindow');
      BlankLine;

      GenLine(2,   '{register field types}');
      GenerateFieldRegistration;

      GenLine(0, 'end;');
      BlankLine;
      GenLine(0, '{$ENDIF}');
      BlankLine;
    end;

    {initialize user record with default values}
    CES.InitUserRecord;

    {generate InitUserRecord}
    GenLine(0, 'procedure InitUserRecord(var UR : UserRecord);');
    GenLine(0, 'begin');
    GenLine(2,   'FillChar(UR, SizeOf(UR), 0);');
    CES.VisitAllEntryFields(FillOneField, OutF);
    GenLine(0, 'end;');
    BlankLine;

    GenLine(0, 'function InitEntryScreen(var ES : '+ESS+';');
    GenLine(25,                         'var UR : UserRecord;');
    GenLine(25,                         'var EsColors : ColorSet) : Word;');
    {generate frame constant}
    if CES.HasFrame then
      GenerateFrameConstant;

    {generate window options constant}
    if not CES.HasFrame then
      GenLine(0, 'const');
    if not CES.IsScrollable then
      ClearLongFlag(CES.wFlags, wResizeable);
    GenLine(2, 'WinOptions = '+WindowOptions(CES)+';');
    SetLongFlag(CES.wFlags, wResizeable);

    if GenFieldNames then begin
      BlankLine;
      GenLine(0, '{$IFDEF AddNames}');
      GenLine(2, 'procedure SetFieldName(ID : Word; S : String);');
      GenLine(4,   '{-Give the specified field a name}');
      GenLine(2, 'begin');
      GenLine(4,   'if (S <> '''') and (ES.FindField(ID) <> nil) then');
      GenLine(6,     'if not ES.FindField(ID)^.sfSetFieldName(S) then {error};');
      GenLine(2, 'end;');
      GenLine(0, '{$ENDIF}');
      BlankLine;
    end;

    {Entry screen initialization}
    GenLine(0, 'begin');
    GenerateEntryScreen;
    Status := IoResult;
    if Status <> 0 then
      goto CloseUp;

    {generate text fields}
    if CES.TextFieldCount($FFFF) > 0 then begin
      BlankLine;
      CES.VisitAllTextFields(WriteOneTextField, OutF);
    end;

    {Generate entry fields}
    BlankLine;
    CES.VisitAllEntryFields(WriteOneEntryField, OutF);

    {Generate field names}
    if GenFieldNames then begin
      GenLine(4, '{$IFDEF AddNames}');
      CES.VisitAllEntryFields(WriteOneFieldName, OutF);
      GenLine(4, '{$ENDIF}');
      BlankLine;
    end;

    {check for I/O errors}
    Status := IoResult;
    if Status <> 0 then
      goto CloseUp;

    {generate call to allocate virtual screen if entry screen scrolls}
    if CES.IsScrollable then begin
      GenLine(4, 'AllocateScreen;');
      BlankLine;
    end;

    {Boilerplate to end function}
    GenLine(4,     'if RawError <> 0 then');  {!!.22}
    GenLine(6,       'ES.Done;');             {!!.22}
    BlankLine;                                {!!.22}
    GenLine(4,     'InitEntryScreen := RawError;');
    GenLine(2,   'end;');
    GenLine(0, 'end;');
    BlankLine;
    GenLine(0, 'end.');
    Status := IoResult;
    Close(OutF);
    if Status <> 0 then
      goto CloseUp;

    {Open output file for test program}
    if not AbleToRewrite(OutF, TestName) then
      goto CloseUp2;

    {generate boilerplate at start of file}
    MakeBoilerplate(False);

    {generate USES list}
    if not MakeUsesList(False) then
      goto CloseUp;

    if CES.cesMouseSupport then begin
      GenLine(2,   '{$IFDEF UseMouse}');
      GenLine(0, 'const');
      GenLine(2,   'MouseChar  : Char = #04;');
      GenLine(2,   '{$ENDIF}');
      BlankLine;
    end;

    {Write list of color constants}
    GenerateColorConstants;

    {generate global variables}
    GenLine(0, 'var');
    GenLine(2,   'ES     : '+ESS+';');
    GenLine(2, 'UR     : UserRecord;');
    GenLine(2, 'Status : Word;');
    if GenStreamCode then begin
      GenLine(2, '{$IFDEF TestStream}');
      GenLine(2, 'S      : BufIdStream;');
      GenLine(2, '{$ENDIF}');
    end;
    BlankLine;

    if GenUserHooks then begin
      {generate shell routines}
      GenLine(0, '{$F+}');
      GenLine(0, 'procedure PreEdit(ESP : EntryScreenPtr);');
      GenLine(2,   '{-Called just before a field is edited}');
      GeneratePreEditShell;
      BlankLine;

      GenLine(0, 'procedure PostEdit(ESP : EntryScreenPtr);');
      GenLine(2,   '{-Called just after a field has been edited}');
      GeneratePreEditShell;
      BlankLine;

      GenLine(0, 'procedure ErrorHandler(UnitCode : Byte; var ErrCode : Word; Msg : string);');
      GenLine(2,   '{-Report errors}');
      GenLine(0, 'begin');
      GenLine(2,   'RingBell;');
      GenLine(0, 'end;');
      BlankLine;

      GenLine(0, 'procedure DisplayHelp(UnitCode : Byte; IdPtr : Pointer; HelpIndex : Word);');
      GenLine(2,   '{-Display context sensitive help}');
      GenLine(0, 'begin');
      GenLine(0, 'end;');
      GenLine(0, '{$F-}');
      BlankLine;
    end;

    if GenStreamCode then begin
      GenLine(0, '{$IFDEF TestStream}');
      BlankLine;
      GenLine(0, 'procedure RegisterEntryScreen;');
      GenLine(2,   '{-Register data types and pointers}');
      GenLine(0, 'begin');
      GenLine(2,   '{register entry screen and its fields}');
      GenLine(2,   'RegisterTypes(S);');
      BlankLine;
      GenLine(2,   '{register user record}');
      GenLine(2,   'S.RegisterPointer(1000, @UR);');
      if GenUserHooks then begin
        BlankLine;
        GenLine(2,   '{register user-written routines}');
        GenLine(2,   'S.RegisterPointer(1001, @PreEdit);');
        GenLine(2,   'S.RegisterPointer(1002, @PostEdit);');
        GenLine(2,   'S.RegisterPointer(1003, @ErrorHandler);');
      end;
      GenLine(0, 'end;');
      BlankLine;
      GenLine(0, '{$ENDIF}');
      BlankLine;
    end;

    GenLine(0, 'begin');
    GenLine(2,   'ClrScr;');
    BlankLine;

    if CES.cesMouseSupport then begin
      GenLine(2, '{$IFDEF UseMouse}');
      GenLine(2, 'if MouseInstalled then');
      GenLine(4,   'with EsColors do begin');
      GenLine(6,     '{activate mouse cursor}');
      GenLine(6,     'SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+');
      GenLine(29,                    'Byte(MouseChar));');
      GenLine(6,     'ShowMouse;');
      GenLine(6,     '{enable mouse support}');
      GenLine(6,     'EntryCommands.cpOptionsOn(cpEnableMouse);');
      GenLine(4,   'end;');
      GenLine(2, '{$ENDIF}');
      BlankLine;
    end;

    {generate user record initialization code}
    GenLine(2, '{initialize user record}');
    GenLine(2, 'InitUserRecord(UR);');
    BlankLine;

    GenLine(2, '{initialize entry screen}');
    GenLine(2, 'Status := InitEntryScreen(ES, UR, EsColors);');
    GenLine(2, 'if Status <> 0 then begin');
    GenLine(4,   'WriteLn(''Error initializing entry screen: '', Status);');
    GenLine(4,   'Halt(1);');
    GenLine(2, 'end;');
    BlankLine;

    {set up user hooks}
    if GenUserHooks then begin
      GenLine(2, '{set up user hooks}');
      GenLine(2, 'ES.SetPreEditProc(PreEdit);');
      GenLine(2, 'ES.SetPostEditProc(PostEdit);');
      GenLine(2, 'ES.SetErrorProc(ErrorHandler);');
      GenLine(2, 'EntryCommands.SetHelpProc(DisplayHelp);');
      BlankLine;
    end;

    if GenStreamCode then begin
      GenLine(2, '{$IFDEF TestStream}');
      BlankLine;
      GenLine(2, '{set user record}');
      GenLine(2, 'ES.SetUserRecord(UR, SizeOf(UR));');
      BlankLine;
      GenLine(2, '{create stream file}');
      GenLine(2, 'if not S.Init('''+JustName(UnitName)+'.STM'', SCreate, 4096) then begin');
      GenLine(4,   'WriteLn(''Error creating stream'');');
      GenLine(4,   'Halt(2);');
      GenLine(2, 'end;');
      BlankLine;
      GenLine(2, '{register types and store the entry screen}');
      GenLine(2, 'RegisterEntryScreen;');
      GenLine(2, 'S.Put(ES);');
      GenLine(2, 'if S.GetStatus <> 0 then begin');
      GenLine(4,   'WriteLn(''Store error'');');
      GenLine(4,   'Halt(2);');
      GenLine(2, 'end;');
      GenLine(2, 'S.Done;');
      GenLine(2, 'ES.Done;');
      BlankLine;
      GenLine(2, '{reopen stream file}');
      GenLine(2, 'if not S.Init('''+JustName(UnitName)+'.STM'', SOpen, 4096) then begin');
      GenLine(4,   'WriteLn(''Error opening stream'');');
      GenLine(4,   'Halt(2);');
      GenLine(2, 'end;');
      BlankLine;
      GenLine(2, '{register types and load the entry screen}');
      GenLine(2, 'RegisterEntryScreen;');
      GenLine(2, 'S.Get(ES);');
      GenLine(2, 'if S.GetStatus <> 0 then begin');
      GenLine(4,   'WriteLn(''Load error'');');
      GenLine(4,   'Halt(3);');
      GenLine(2, 'end;');
      GenLine(2, 'S.Done;');
      BlankLine;
      GenLine(2, '{$ENDIF}');
      BlankLine;
    end;

    GenLine(2, '{test entry screen}');
    GenLine(2, 'ES.Process;');
    GenLine(2, 'ES.Erase;');
    BlankLine;

    if CES.cesMouseSupport then begin
      GenLine(2, '{$IFDEF UseMouse}');
      GenLine(2, 'HideMouse;');
      GenLine(2, '{$ENDIF}');
      BlankLine;
    end;

    GenLine(2, '{show exit command}');
    GenLine(2, 'WriteLn(''Exit command = '', ES.GetLastCommand);');
    GenLine(2, 'ES.Done;');
    GenLine(0, 'end.');
    Status := IoResult;

CloseUp:
    Close(OutF);
    if Status = 0 then
      Status := IoResult;

CloseUp2:
    {$IFDEF UseBcd}
    Bcds.Done;
    {$ENDIF}

    if Status = 0 then
      PopupDelayMessage(SuccessMsg)
    else
      PopupErrorMessage(ErrorMsg+Long2Str(Status)+WritingSrc);
  end;

  {-------------------- documentation -------------------}

const
  Ellipses : array[1..3] of Char = '...';

  {$F+}
  procedure WriteTextFieldDocs(TFP : TextFieldPtr; var D; ASP : AbstractSelectorPtr);
    {-Generate documentation for the specified text field}
  var
    S : string[80];
    SLen : Byte absolute S;
  begin
    with TFP^ do begin
      if FlagIsSet(tfFlags, tfLine+tfBox) then {!!.03}
        Exit;                                  {!!.03}

      {truncate text string to length of 59 if necessary}
      S := PascalString(tfString^);
      if SLen > 59 then begin
        Move(Ellipses, S[57], 3);
        SLen := 59;
      end;

      WriteLn(OutF, ' ', ZeroPad(tfID), tfRow:5, tfCol:4, '  ', S);
    end;
  end;

  procedure WriteLineFieldDocs(TFP : TextFieldPtr; var D; ASP : AbstractSelectorPtr); {!!.03}
    {-Generate documentation for the specified line field}
  const
    OSt : array[Boolean] of string[10] = ('Horizontal', 'Vertical');
  begin
    with TextLineFieldPtr(TFP)^ do begin
      if not FlagIsSet(tfFlags, tfLine) then
        Exit;

      WriteLn(OutF,
        ' ', ZeroPad(tfID), tfRow:5, tfCol:4, '  ',
        PascalChar(tfString^[1]), '  ',
        PascalChar(tfString^[2]), '  ',
        PascalChar(tfString^[3]),
        tlfLength:5, '  ',
        OSt[FlagIsSet(tfFlags, tfVertical)] );
    end;
  end;

  procedure WriteBoxFieldDocs(TFP : TextFieldPtr; var D; ASP : AbstractSelectorPtr); {!!.03}
    {-Generate documentation for the specified box field}
  begin
    with TextBoxFieldPtr(TFP)^ do begin
      if not FlagIsSet(tfFlags, tfBox) then
        Exit;

      WriteLn(OutF,
        ' ', ZeroPad(tfID), tfCol:4, tfRow:5, tbfColH:4, tbfRowH:4, '  ', {!!.13}
        PascalString(tfString^), ' ',
        Trim(YesNoSt[FlagIsSet(tfFlags, tfShadowed)]) );
    end;
  end;

  procedure WriteFieldDocs(EFP : EntryFieldPtr; var D; ESP : EntryScreenPtr);
    {-Generate documentation for the specified entry field}

    function PromptString : string;
    var
      S : string[40];
      SLen : Byte absolute S;
    begin
      S := PascalString(EFP^.efPrompt^);
      if SLen > 35 then begin
        Move(Ellipses, S[33], 3);
        SLen := 35;
      end;
      PromptString := Pad(S, 35);
    end;

    function PictureString : string;
    var
      S : string;
      SLen : Byte absolute S;
      Ch : Char;
    begin
      S := PictureToString(EFP, False);
      if SLen > 35 then begin
        Move(Ellipses, S[33], 3);
        SLen := 35;
      end;
      PictureString := Pad(S, 35);
    end;

    function TypeString : string;
    begin
      TypeString := Pad(TypeCode2Str(GetTypeCode(EFP)), 14);
    end;

  begin
    with EFP^ do begin
      WriteLn(OutF, ' ', Pad(FieldName(EFP), 21), TypeString,
                    PromptString, sfPRow:4, sfPCol:4);
      WriteLn(OutF, '':22, Pad(EditorSt[GetEditor(EFP)], 14),
                    PictureString, sfFRow:4, sfFCol:4, ^M^J);
    end;
  end;
  {$F-}

  procedure DocumentEntryScreen; {!!.03} {rewritten}
    {-Write a text listing of the entry screen}
  const
    DocNameMsg = 'Name of document file: ';
    SuccessMsg = 'Documentation written successfully...';
    ErrorMsg = 'Error ';
    WritingDoc = ' writing documentation';
    {
             1         2         3         4         5         6         7
    123456789012345678901234567890123456789012345678901234567890123456789012345
     nnn  rrr ccc  ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
    }
    Legend1 =
   'ID    Row Col  Text';
    LegendUnd1 =
   '----  --- ---  '; {+59 dashes}
    {
             1         2         3         4         5         6         7
    123456789012345678901234567890123456789012345678901234567890123456789012345
     nnn  rrr ccc  hhh  mmm  ttt  lll  ooooooooooo
    }
    Legend2 =
     'ID    Row Col  Head Mid  Tail Len  Orientation';
    LegendUnd2 =
     '----  --- ---  ---- ---- ---- ---  -----------';
    {
             1         2         3         4         5         6         7
    123456789012345678901234567890123456789012345678901234567890123456789012345
     nnn  xxx yyy xxx yyy  '----------'  yyy
    }
    Legend3 =
     'ID     XL  YL  XH  YH  Box chars   Shadow?';
    LegendUnd3 =
     '----  --- --- --- ---  ----------  -------';
    {
             1         2         3         4         5         6         7
    12345678901234567890123456789012345678901234567890123456789012345678901234
     Fieldxxx   Array of char  12345678901234567890123456789012345678  rrr ccc
                Numeric        12345678901234567890123456789012345678  rrr ccc
    }
    Legend4 =
   'Field name            Type / Editor Prompt / Picture                    Row Col';
    LegendUnd4 =
   '--------------------- ------------- ----------------------------------- --- ---';
  var
    Status : Word;
    DocName : PathStr;

    function SizeLastSaved : LongInt;
    var
      DEP : DirEntryPtr;
    begin
      DEP := Lib.FindAnyDirectoryEntry(ObjName);
      if DEP = nil then
        SizeLastSaved := 0
      else
        SizeLastSaved := DEP^.GetEntryLength;
    end;

  begin
    {Get file name}
    DocName := ForceExtension(CleanPathName(ObjName), DefDocExt);
    if not PopupGetFileName('', DocNameMsg, PathLen, DefDocExt, DocName) then
      Exit;

    {Open it}
    if not AbleToRewrite(OutF, DocName) then
      Exit;

    {Write the documentation}
    GenLine(0, 'Entry Screen Documentation');
    GenLine(0, '--------------------------');
    GenLine(2,   'Object:                 '+ObjName);
    GenLine(2,   'Library:                '+LibName);
    WriteLn(OutF, '  Size when last saved:   ', SizeLastSaved);
    BlankLine;

    {entry screen stats}
    WriteLn(OutF, '  Text fields:            ', CES.TextFieldCount($FFFF));
    WriteLn(OutF, '  Entry fields:           ', CES.EntryFieldCount);
    WriteLn(OutF, '  Scrollable:             ', Trim(YesNoSt[CES.IsScrollable]));
    BlankLine;

    {Listing of all text fields}
    if CES.TextFieldCount(0) > 0 then begin
      WriteLn(OutF, Legend1, ^M^J, LegendUnd1, CharStr('-', 59));
      CES.VisitAllTextFields(WriteTextFieldDocs, OutF);
      BlankLine;
    end;

    {Listing of all line fields}
    if CES.TextFieldCount(tfLine) > 0 then begin
      WriteLn(OutF, Legend2, ^M^J, LegendUnd2);
      CES.VisitAllTextFields(WriteLineFieldDocs, OutF);
      BlankLine;
    end;

    {Listing of all box fields}
    if CES.TextFieldCount(tfBox) > 0 then begin
      WriteLn(OutF, Legend3, ^M^J, LegendUnd3);
      CES.VisitAllTextFields(WriteBoxFieldDocs, OutF);
      BlankLine;
    end;

    {Listing of all entry fields}
    if CES.EntryFieldCount > 0 then begin
      WriteLn(OutF, Legend4, ^M^J, LegendUnd4);
      CES.VisitAllEntryFields(WriteFieldDocs, OutF);
    end;

    Status := IoResult;
    Close(OutF);
    if Status = 0 then
      Status := IoResult;

    if Status = 0 then
      PopupDelayMessage(SuccessMsg)
    else
      PopupErrorMessage(ErrorMsg+Long2Str(Status)+WritingDoc);
  end;

{begin}
end.
