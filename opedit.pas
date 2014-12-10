{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPEDIT.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpEdit;
  {-Line editor and keyboard input routines}

{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

interface

uses
  Use32,
  OpConst,     {!!.20}
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
  OpSelect,         {!!.30}
  OpFEdit,
  OpField
  {$IFDEF UseDrag}  {!!.03}
  , OpDrag          {!!.03}
  {$ENDIF}          {!!.03}
  ;

  {$I OPEDIT.ICD}  {configuration data}

const
  {----------- OPEDIT option codes -----------}
  {option codes for entry fields}
  leProtected        = $00000001; {if set, field contents cannot be modified}
  leHidden           = $00000002; {field is hidden}
  leMapCtrls         = $00000004; {map control characters}
  leMultChoice       = $00000008; {a multiple-choice field}
  leInvisible        = $00000010; {if set, field is invisible}
  {^--correspond to flags for select fields}
  leAutoAdvCharBegin = $00000020; {dummy}
  leAutoAdvCharEnd   = $00000040; {advance to next field--character entered}
  leAutoAdvCurBegin  = $00000080; {advance to prev field when beg. of field passed}
  leAutoAdvCurEnd    = $00000100; {advance to next field--cursor key pressed}
  leAutoAdvanceChar  = leAutoAdvCharBegin+leAutoAdvCharEnd;
  leAutoAdvanceCursor= leAutoAdvCurBegin+leAutoAdvCurEnd;
  leAutoAdvanceBegin = leAutoAdvCharBegin+leAutoAdvCurBegin;
  leAutoAdvanceEnd   = leAutoAdvCharEnd+leAutoAdvCurEnd;
  leAutoAdvance      = leAutoAdvanceBegin+leAutoAdvanceEnd;
  leInsertPushes     = $00000200; {if set, inserting can push a character off end}
  leRightJustify     = $00000400; {if set, field is right-justified}
  lePasswordMode     = $00000800; {if set, contents of field are suppressed}
  leCursorToEnd      = $00001000; {put cursor at end of string initially}
  leTrimBlanks       = $00002000; {trim leading/trailing blanks when finished}
  leClearFirstChar   = $00004000; {clear string if first character is ASCII}
  leForceOvertype    = $00008000; {if ForceMode set, selects insert or overtype}
  leForceMode        = $00010000; {force insert or overtype, else use default}
  leClickExit        = $00020000; {produce ccClickExit on double click}
  leBeepOnError      = $00040000; {beep when illegal char entered?}
  leAutoNumLock      = $00080000; {auto-activate/deactivate NumLock--numbers only}
  leParensForMinus   = $00100000; {display negative numbers inside parentheses--numeric fields only}
  leExitAtEdges      = $00200000; {allow exit at edges--pick editor only}
  leRequired         = $00400000; {if set, field cannot be empty}
  leReadOnly         = $00800000; {if set, field cannot be edited} {!!.02}
  {...following options valid for simple editors only...}
  leHouseCursorAtEnd = $01000000; {extra char of width to hold cursor when string full?}
  leForceUpper       = $02000000; {force chars to upper case?}
  leForceLower       = $04000000; {force chars to lower case?}
  {...following option for simple char editor, pick editor only...}
  leAllowEscape      = $08000000; {allow escape?}
  {...following options for simple char editor only...}
  leShowReadChar     = $10000000; {display the character pressed?}
  leHideCursor       = $20000000; {hide cursor?}
  leDefaultAccepted  = $40000000; {allow user to accept default?}
  leMapExtended      = $80000000; {map extended keys?}

  DefEditOptions     : LongInt = leCursorToEnd+leTrimBlanks+leClearFirstChar+
                                 leMapCtrls+leHouseCursorAtEnd;
  BadEditOptions     : LongInt = leProtected+leHidden+leMultChoice+leInvisible+
                                 leClickExit+leExitAtEdges+leRequired;

  {secondary options}
  sleSmartExponents  = $02000000; {select 1.00 behavior for fields using E} {!!.02}
  sleSwitchCommands  = $04000000; {switch command processors--for WindowFields
                                   only} {!!.01}
  slePadCurrentOnly  = $08000000; {use alternate pad character only when
                                   editing the field}
  sleSuppressZero    = $10000000; {suppress zeroes in conversion routines for
                                   longints, integers, etc.--not reals}
  sleNoFieldMovement = $20000000; {don't allow field movement commands to exit}
  sleWindowRelative  = $40000000; {coordinates treated relative to current window?}
  sleInsertByDefault = $80000000; {default to insert mode?}

  DefSecEditOptions  : LongInt = sleNoFieldMovement+sleInsertByDefault+
                                 ifNoMouseExit;
  BadSecEditOptions  : LongInt = ifBoolean+ifNumeric+ifSimple+ifNested+
                                 ifMultiLine+ifModified+ifNotNext+ifNotPrev+
                                 ifNoMouseExit+ifSuppressFirst+ifNoLiterals+
                                 ifEditing+ifSemiHidden+ifRealVar;

type
  LineEditorPtr = ^LineEditor;
  LineEditor =
    object(Root)
      leCmd         : Word;       {last command}
      leKey         : Word;       {last key pressed}
      leError       : Word;       {last error code}
      leCmdPtr      : CommandProcessorPtr; {command processor}
      leErrorProc   : ErrorProc;  {error handler}
      leUnitCode    : Byte;       {unit code}
      lePassword    : Char;       {char used to represent text in password mode}
      lePadChar     : Char;       {char used to pad field}
      leInsertMode  : Boolean;    {insert mode}
      leOptions     : LongInt;    {primary options}
      leFlags       : LongInt;    {secondary options}
      leHelpIndex   : Word;       {help index}
      leColors      : ColorSet;   {colors}
      leCFF         : ClearFirstFunc; {function to check for first char}
      {...methods...}
      constructor Init(var Colors : ColorSet);
        {-Sets default values for attributes, etc.}
      function GetLastCommand : Word;
        {-Return last command entered by user}
      function GetLastKey : Word;
        {-Return last keystroke entered by user}
      function GetLastError : Word;
        {-Get last error code}
      {...}
      procedure leEditOptionsOn(OptionFlags : LongInt);
        {-Activate multiple options}
      function leEditOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified options are on}
      procedure leEditOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple options}
      {...}
      procedure leSecEditOptionsOn(OptionFlags : LongInt);
        {-Activate multiple options}
      function leSecEditOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified options are on}
      procedure leSecEditOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple options}
      {...}
      procedure SetPadChar(Ch : Char);
        {-Set character used to pad strings being edited}
      procedure SetPasswordChar(Ch : Char);
        {-Set character used to represent text in password mode}
      procedure SetHelpIndex(Index : Word);
        {-Set the help index}
      procedure SetCommandProcessor(var CP : CommandProcessor);
        {-Set command processor to use}
      procedure SetErrorProc(EEP : ErrorProc);
        {-Set procedure to call on error}
      procedure SetClearFirstFilter(CFF : ClearFirstFunc);
        {-Set procedure to call for ClearFirstChar test}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a line editor from a stream}
      procedure Store(var S : IdStream);
        {-Store a line editor in a stream}
    {$ENDIF}
      {--------------------------------------Read routines--no picture mask}
      procedure ReadString(Prompt : string; Row, Col, MaxLen, Width : Byte;
                           var St : string);
        {-Edit a string}
      procedure ReadSpecial(Prompt : string; Row, Col, MaxLen, Width : Byte;
                            PictureChar : Char; var St : string);
        {-Edit a string}
      procedure ReadChar(Prompt : string; Row, Col : Byte;
                         ValidChars : CharSet; var Ch : Char);
        {-Display a prompt and wait for a key in ValidChars}
      function YesOrNo(Prompt : string; Row, Col : Byte; Default : Char) : Boolean;
        {-Return true for yes, false for no}
      procedure ReadLongInt(Prompt : string; Row, Col, Width : Byte;
                            LLo, LHi : LongInt; var L : LongInt);
        {-Edit a long integer}
      procedure ReadInteger(Prompt : string; Row, Col, Width : Byte;
                            NLo, NHi : Integer; var N : Integer);
        {-Edit an integer}
      procedure ReadWord(Prompt : string; Row, Col, Width : Byte;
                         WLo, WHi : Word; var W : Word);
        {-Edit a word}
      procedure ReadByte(Prompt : string; Row, Col, Width : Byte;
                         BLo, BHi : Byte; var B : Byte);
        {-Edit a byte}
      procedure ReadShortInt(Prompt : string; Row, Col, Width : Byte;
                             SLo, SHi : ShortInt; var SI : Shortint);
        {-Edit a ShortInt}
      procedure ReadReal(Prompt : string; Row, Col, Width, Places : Byte;
                         RLo, RHi : Real; var R : Real);
        {-Edit a real}
     {$IFDEF UseBcd}
      procedure ReadBcd(Prompt : string; Row, Col, Width, Places : Byte;
                        BLo, BHi : Bcd; var B : BCD);
        {-Edit a BCD real}
      {$ENDIF}
      {$IFOPT N+}
      procedure ReadExt(Prompt : string; Row, Col, Width, Places : Byte;
                        ELo, EHi : Extended; var E : Extended);
        {-Edit an extended}
      procedure ReadDbl(Prompt : string; Row, Col, Width, Places : Byte;
                        DLo, DHi : Double; var D : Double);
        {-Edit a double}
      procedure ReadSgl(Prompt : string; Row, Col, Width, Places : Byte;
                        SLo, SHi : Single; var S : Single);
        {-Edit a single}
      procedure ReadComp(Prompt : string; Row, Col, Width : Byte;
                         CLo, CHi : Comp; var C : Comp);
        {-Edit a comp}
      {$ENDIF}
      {--------------------------------------Read routines--with picture mask}
      procedure EditString(Prompt : string; Row, Col : Byte; Picture : string;
                           Width : Byte; var St : string);
        {-Edit a string}
      procedure EditArray(Prompt : string; Row, Col : Byte;
                          Picture : string; Width : Byte; var A);
        {-Edit an array of char}
      procedure EditChar(Prompt : string; Row, Col : Byte; Picture : string;
                         ChLo, ChHi : Char; var Ch : Char);
        {-Edit an array of char}
      procedure EditBoolean(Prompt : string; Row, Col : Byte;
                            Picture : string; var B : Boolean);
        {-Edit a boolean variable}
      procedure EditYesNo(Prompt : string; Row, Col : Byte;
                          Picture : string; var YN : Boolean);
        {-Edit a yes-no variable}
      procedure EditLongInt(Prompt : string; Row, Col : Byte;
                            Picture : string; LLo, LHi : LongInt;
                            var L : LongInt);
        {-Edit a long integer}
      procedure EditInteger(Prompt : string; Row, Col : Byte;
                            Picture : string; NLo, NHi : Integer;
                            var N : Integer);
        {-Edit an integer}
      procedure EditWord(Prompt : string; Row, Col : Byte;
                         Picture : string; WLo, WHi : Word;
                         var W : Word);
        {-Edit a word}
      procedure EditByte(Prompt : string; Row, Col : Byte;
                         Picture : string; BLo, BHi : Byte;
                         var B : Byte);
        {-Edit a byte}
      procedure EditShortInt(Prompt : string; Row, Col : Byte;
                             Picture : string; SLo, SHi : ShortInt;
                             var SI : Shortint);
        {-Edit a ShortInt}
      procedure EditReal(Prompt : string; Row, Col : Byte;
                         Picture : string; Places : Byte;
                         RLo, RHi : Real; var R : Real);
        {-Edit a real}
      {$IFDEF UseBcd}
      procedure EditBcd(Prompt : string; Row, Col : Byte; Picture : string;
                        Places : Byte; BLo, BHi : Bcd; var B : BCD);
        {-Edit a BCD real}
      {$ENDIF}
      {$IFOPT N+}
      procedure EditExt(Prompt : string; Row, Col : Byte; Picture : string;
                        Places : Byte; ELo, EHi : Extended; var E : Extended);
        {-Edit an extended}
      procedure EditDbl(Prompt : string; Row, Col : Byte; Picture : string;
                        Places : Byte; DLo, DHi : Double; var D : Double);
        {-Edit a double}
      procedure EditSgl(Prompt : string; Row, Col : Byte; Picture : string;
                        Places : Byte; SLo, SHi : Single; var S : Single);
        {-Edit a single}
      procedure EditComp(Prompt : string; Row, Col : Byte; Picture : string;
                         CLo, CHi : Comp; var C : Comp);
        {-Edit a comp}
      {$ENDIF}
      {$IFDEF UseDates}
      procedure EditDate(Prompt : string; Row, Col : Byte; Picture : string;
                         DLo, DHi : Date; var D : Date);
        {-Edit a date variable}
      procedure EditDateSt(Prompt : string; Row, Col : Byte;
                           Picture : DateString; var DS : DateString);
        {-Edit a date string}
      procedure EditTime(Prompt : string; Row, Col : Byte; Picture : DateString;
                         TLo, THi : Time; var T : Time);
        {-Edit a time variable}
      {$ENDIF}
      {--------------------------------------Read routines--numeric editor}
      procedure EditNumericLongInt(Prompt : string; Row, Col : Byte;
                                   Picture : string; LLo, LHi : LongInt;
                                   var L : LongInt);
        {-Edit a long integer (numeric)}
      procedure EditNumericInteger(Prompt : string; Row, Col : Byte;
                                   Picture : string; NLo, NHi : Integer;
                                   var N : Integer);
        {-Edit an integer (numeric)}
      procedure EditNumericWord(Prompt : string; Row, Col : Byte;
                                Picture : string; WLo, WHi : Word;
                                var W : Word);
        {-Edit a word (numeric)}
      procedure EditNumericByte(Prompt : string; Row, Col : Byte;
                                Picture : string; BLo, BHi : Byte;
                                var B : Byte);
        {-Edit a byte (numeric)}
      procedure EditNumericShortInt(Prompt : string; Row, Col : Byte;
                                    Picture : string; SLo, SHi : ShortInt;
                                    var SI : Shortint);
        {-Edit a ShortInt (numeric)}
      procedure EditNumericReal(Prompt : string; Row, Col : Byte;
                                Picture : string; Places : Byte;
                                RLo, RHi : Real; var R : Real);
        {-Edit a real (numeric)}
      {$IFDEF UseBcd}
      procedure EditNumericBcd(Prompt : string; Row, Col : Byte; Picture : string;
                               Places : Byte; BLo, BHi : Bcd; var B : BCD);
        {-Edit a BCD real (numeric)}
      {$ENDIF}
      {$IFOPT N+}
      procedure EditNumericExt(Prompt : string; Row, Col : Byte; Picture : string;
                               Places : Byte; ELo, EHi : Extended; var E : Extended);
        {-Edit an extended (numeric)}
      procedure EditNumericDbl(Prompt : string; Row, Col : Byte; Picture : string;
                               Places : Byte; DLo, DHi : Double; var D : Double);
        {-Edit a double (numeric)}
      procedure EditNumericSgl(Prompt : string; Row, Col : Byte; Picture : string;
                               Places : Byte; SLo, SHi : Single; var S : Single);
        {-Edit a single}
      procedure EditNumericComp(Prompt : string; Row, Col : Byte; Picture : string;
                                CLo, CHi : Comp; var C : Comp);
        {-Edit a comp (numeric)}
      {$ENDIF}
      {--------------------------------------Other read routines}
      procedure EditChoice(Prompt : string; Row, Col : Byte; Picture : string;
                           DataSize : Byte; Increment : IncChoiceProc; var MC);
        {-Edit a multiple choice variable}
      procedure EditMultiLine(Prompt : string; Row, Col : Byte;
                              Width, Height : Byte; var StArray);
        {-Edit a multiline field}
{.Z+}
      {+++ internal methods +++}
      procedure leInitPrim(var Prompt : string; var pRow, pCol, fRow, fCol : Byte;
                           var fWidth : Byte; MaxLen : Byte);
      procedure leInitPicture(var Picture : string; PChar : Char; var Width : Byte);
      procedure leReadPrim(var EF : EntryField);
      procedure leInitError;
{.Z-}
    end;

{.Z+}
  {$IFDEF UseStreams}
  procedure LineEditorStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing line editors}
  {$ENDIF}
{.Z-}

var
  {$IFDEF UseDrag}                 {!!.03}
  EditCommands : DragProcessor;    {!!.03}
  {$ELSE}                          {!!.03}
  EditCommands : CommandProcessor; {command processor used for line editor}
  {$ENDIF}                         {!!.03}

  {==========================================================================}

implementation

type
  XY = record
         X, Y : Byte;
       end;

  constructor LineEditor.Init(var Colors : ColorSet);
    {-Sets default values for attributes, etc.}
  begin
    {initialize parent}
    Root.Init;

    {initialize line editor data fields}
    leCmd := ccNone;
    leKey := 0;
    leErrorProc := DefaultErrorProc;
    leError := 0;
    leUnitCode := ucEdit;
    lePassword := DefPasswordChar;
    leCmdPtr := @EditCommands;
    lePadChar := DefPadChar;
    leOptions := DefEditOptions;
    leFlags   := DefSecEditOptions;
    leInsertMode := LongFlagIsSet(DefSecEditOptions, sleInsertByDefault);
    leColors := Colors;
    leCFF := DefClearFirstFunc;
  end;

  function LineEditor.GetLastCommand : Word;
    {-Return last command entered by user}
  begin
    GetLastCommand := leCmd;
  end;

  procedure LineEditor.leEditOptionsOn(OptionFlags : LongInt);
    {-Activate multiple options}
  begin
    SetLongFlag(leOptions, OptionFlags and not BadEditOptions);
  end;

  procedure LineEditor.leEditOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple options}
  begin
    ClearLongFlag(leOptions, OptionFlags and not BadEditOptions);
  end;

  function LineEditor.leEditOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified options are on}
  begin
    leEditOptionsAreOn := (leOptions and OptionFlags) = OptionFlags;
  end;

  procedure LineEditor.leSecEditOptionsOn(OptionFlags : LongInt);
    {-Activate multiple options}
  begin
    SetLongFlag(leFlags, OptionFlags and not BadSecEditOptions);
  end;

  procedure LineEditor.leSecEditOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple options}
  begin
    ClearLongFlag(leFlags, OptionFlags and not BadSecEditOptions);
  end;

  function LineEditor.leSecEditOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified options are on}
  begin
    leSecEditOptionsAreOn := (leFlags and OptionFlags) = OptionFlags;
  end;

  procedure LineEditor.SetHelpIndex(Index : Word);
    {-Set the help index}
  begin
    leHelpIndex := Index;
  end;

  procedure LineEditor.leInitError;
    {-Called when an error occurs while initializing an EntryField}
  begin
    if InitStatus <> 0 then
      leError := InitStatus
    else
      leError := epFatal+ecOutOfMemory;
    leErrorProc(leUnitCode, leError, emInsufficientMemory);
    leCmd := ccError;
  end;

  function LineEditor.GetLastKey : Word;
    {-Return last keystroke entered by user}
  begin
    GetLastKey := leKey;
  end;

  function LineEditor.GetLastError : Word;
    {-Get last error code}
  begin
    GetLastError := leError;
    leError := 0;
  end;

  procedure LineEditor.SetCommandProcessor(var CP : CommandProcessor);
    {-Set command processor to use}
  begin
    leCmdPtr := @CP;
  end;

  procedure LineEditor.SetErrorProc(EEP : ErrorProc);
    {-Set procedure to call on error}
  begin
    leErrorProc := EEP;
  end;

  procedure LineEditor.SetClearFirstFilter(CFF : ClearFirstFunc);
    {-Set ClearFirstFunc}
  begin
    leCFF := CFF;
  end;

  procedure LineEditor.SetPadChar(Ch : Char);
    {-Set character used to pad strings being edited}
  begin
    lePadChar := Ch;
  end;

  procedure LineEditor.SetPasswordChar(Ch : Char);
    {-Set character used to represent text in password mode}
  begin
    lePassword := Ch;
  end;

  procedure LineEditor.leInitPrim(var Prompt : string;
                                  var pRow, pCol, fRow, fCol : Byte;
                                  var fWidth : Byte;
                                  MaxLen : Byte);
    {-Prepare to instantiate an entry field}
  begin
    {adjust coordinates if necessary}
    if LongFlagIsSet(leFlags, sleWindowRelative) then begin
      Inc(pRow, XY(WindMin).Y);
      Inc(pCol, XY(WindMin).X);
    end;

    {set field coordinates based on prompt}
    fRow := pRow;
    fCol := pCol+Length(Prompt);

    {check field width}
    if (fWidth = 0) or (fWidth > MaxLen) then
      fWidth := MaxLen;

    InitStatus := 0;
  end;

  procedure LineEditor.leInitPicture(var Picture : string;
                                     PChar : Char;
                                     var Width : Byte);
    {-Check picture mask}
  begin
    if Width = 0 then
      Width := 1;
    if Length(Picture) < Width then
      Picture := CharStr(PChar, Width);
    if Length(Picture) = 255 then        {!!.02}
      Picture[0] := #254;                {!!.02}
  end;

  procedure LineEditor.leReadPrim(var EF : EntryField);
    {-Primitive routine to edit a field}
  var
    SaveXY : Word;
    SaveCFF : ClearFirstFunc;
    Flags : PictureFlags;
    FA, CA : Byte;
    {$IFDEF UseMouse}
    SaveMouse   : Boolean;
    {$ENDIF}
  begin
    with EF do begin
      {draw the prompt for the edit field}
      if sfPWidth <> 0 then begin
        {$IFDEF UseMouse}
        HideMousePrim(SaveMouse);
        {$ENDIF}

        FastWrite(efPrompt^, sfPRow, sfPCol, ColorMono(sfSelPromptColor, sfSelPromptMono));

        {$IFDEF UseMouse}
        ShowMousePrim(SaveMouse);
        {$ENDIF}
      end;

      {force insert mode}
      if LongFlagIsSet(leFlags, sleInsertByDefault) then
        leInsertMode := True;

      {call conversion routine}
      Convert(False);

      {save cursor position}
      SaveXY := WhereXY;

      {reset last command}
      leCmd := ccNone;

      {get attributes}
      FA := ColorMono(sfSelFieldColor, sfSelFieldMono);
      CA := ColorMono(sfCtrlColor, sfCtrlMono);

      {call editor}
      SaveCFF := feClearFirstFunc;
      feClearFirstFunc := leCFF;
      Edit(                                    {!!.02}
        sfFRow, sfFCol, FA, CA, lePassWord, 0, LongFlagIsSet(sfOptions, leReadOnly),
        leCmd, leKey, leInsertMode, leErrorProc, leUnitCode, leCmdPtr^);
      feClearFirstFunc := SaveCFF;

      {restore cursor position}
      GotoXYAbs(Lo(SaveXY), Hi(SaveXY));

      {trim blanks?}
      if LongFlagIsSet(sfOptions, leTrimBlanks) then
        efTrimSpaces;

      {call conversion routine if <Esc> not pressed}
      if leCmd <> ccQuit then begin
        {call conversion routine}
        Convert(True);

        {exit if simple char editor is in use and ShowReadChar is off} {!!.01}
        if (efDataSize = 1) and LongFlagIsSet(sfFlags, ifSimple) and   {!!.01}
           not LongFlagIsSet(sfOptions, leShowReadChar) then           {!!.01}
             Exit;                                                     {!!.01}

        {and again, so we can redraw}
        Convert(False);

        {initialize picture flags}
        InitPictureFlags(Flags);

        {redraw the field}
        Draw(efEditSt^, sfFRow, sfFCol, FA, CA, 0, lePassword, Flags);
      end;
    end;
  end;

  procedure LineEditor.ReadString(Prompt : string; Row, Col, MaxLen, Width : Byte;
                                  var St : string);
    {-Edit a string}
  begin
    ReadSpecial(Prompt, Row, Col, MaxLen, Width, AnyChar, St);
  end;

  procedure LineEditor.ReadSpecial(Prompt : string; Row, Col, MaxLen, Width : Byte;
                                   PictureChar : Char; var St : string);
    {-Edit a string}
  var
    fRow, fCol : Byte;
    SEF : StringField;
  begin
    if MaxLen = 255 then  {!!.02}
      MaxLen := 254;      {!!.02}
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, MaxLen);

    if not SEF.InitSim(
      leHelpIndex, Prompt, Row, Col, PictureChar, fRow, fCol, Width, MaxLen,
      leHelpIndex, St, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(SEF);

    SEF.Done;
  end;

  procedure LineEditor.ReadLongInt(Prompt : string; Row, Col, Width : Byte;
                                   LLo, LHi : LongInt; var L : LongInt);
    {-Edit a long integer}
  var
    fRow, fCol : Byte;
    LEF : LongIntField;
  begin
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Width);

    if not LEF.InitSim(
      leHelpIndex, Prompt, Row, Col, DigitOnly, fRow, fCol, Width, leHelpIndex,
      LLo, LHi, L, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(LEF);

    LEF.Done;
  end;

  procedure LineEditor.ReadInteger(Prompt : string; Row, Col, Width : Byte;
                                   NLo, NHi : Integer; var N : Integer);
    {-Edit an integer}
  var
    L : LongInt;
  begin
    L := LongInt(N);
    if NLo = NHi then begin
      NLo := -32768;
      NHi := MaxInt;
    end;
    ReadLongInt(Prompt, Row, Col, Width, NLo, NHi, L);
    N := Integer(L);
  end;

  procedure LineEditor.ReadWord(Prompt : string; Row, Col, Width : Byte;
                                WLo, WHi : Word; var W : Word);
    {-Edit a word}
  var
    L : LongInt;
  begin
    L := LongInt(W);
    if WLo = WHi then begin
      WLo := $0000;
      WHi := $FFFF;
    end;
    ReadLongInt(Prompt, Row, Col, Width, WLo, WHi, L);
    W := Word(L);
  end;

  procedure LineEditor.ReadByte(Prompt : string; Row, Col, Width : Byte;
                                BLo, BHi : Byte; var B : Byte);
    {-Edit a byte}
  var
    L : LongInt;
  begin
    L := LongInt(B);
    if BLo = BHi then begin
      BLo := $00;
      BHi := $FF;
    end;
    ReadLongInt(Prompt, Row, Col, Width, BLo, BHi, L);
    B := Byte(L);
  end;

  procedure LineEditor.ReadShortInt(Prompt : string;     Row, Col, Width : Byte;
                                    SLo, SHi : ShortInt; var SI : Shortint);
    {-Edit a ShortInt}
  var
    L : LongInt;
  begin
    L := LongInt(SI);
    if SLo = SHi then begin
      SLo := -128;
      SHi := 127;
    end;
    ReadLongInt(Prompt, Row, Col, Width, SLo, SHi, L);
    SI := ShortInt(L);
  end;

  procedure LineEditor.ReadReal(Prompt : string; Row, Col, Width, Places : Byte;
                                RLo, RHi : Real; var R : Real);
    {-Edit a real}
  var
    fRow, fCol : Byte;
    REF : RealField;
  begin
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Width);

    if not REF.InitSim(
      leHelpIndex, Prompt, Row, Col, Scientific, fRow, fCol, Width,
      leHelpIndex, RLo, RHi, Places, R, lePadChar, leOptions, leFlags,
      leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(REF);

    REF.Done;
  end;

{$IFDEF UseBcd}

  procedure LineEditor.ReadBcd(Prompt : string; Row, Col, Width, Places : Byte;
                               BLo, BHi : Bcd;  var B : BCD);
    {-Edit a BCD real}
  var
    fRow, fCol : Byte;
    BEF : BcdField;
  begin
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Width);

    if not BEF.InitSim(
      leHelpIndex, Prompt, Row, Col, Scientific, fRow, fCol, Width,
      leHelpIndex, BLo, BHi, Places, B, lePadChar, leOptions, leFlags,
      leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(BEF);

    BEF.Done;
  end;

{$ENDIF}

{$IFOPT N+}

  procedure LineEditor.ReadExt(Prompt : string; Row, Col, Width, Places : Byte;
                               ELo, EHi : Extended; var E : Extended);
    {-Edit an extended}
  var
    fRow, fCol : Byte;
    EEF : ExtendedField;
  begin
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Width);

    if not EEF.InitSim(
      leHelpIndex, Prompt, Row, Col, Scientific, fRow, fCol, Width,
      leHelpIndex, ELo, EHi, Places, E, lePadChar, leOptions, leFlags,
      leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(EEF);

    EEF.Done;
  end;

  procedure LineEditor.ReadDbl(Prompt : string; Row, Col, Width : Byte;
                               Places : Byte;   DLo, DHi : Double;
                               var D : Double);
    {-Edit a double}
  var
    E : Extended;
  begin
    E := D;
    if DLo = DHi then begin
      DLo := -1.7e+308;
      DHi := +1.7e+308;
    end;
    ReadExt(Prompt, Row, Col, Width, Places, DLo, DHi, E);
    if E = BadExt then   {!!.03}
      D := BadDbl        {!!.03}
    else                 {!!.03}
      D := E;
  end;

  procedure LineEditor.ReadSgl(Prompt : string; Row, Col, Width : Byte;
                               Places : Byte;   SLo, SHi : Single;
                               var S : Single);
    {-Edit a single}
  var
    E : Extended;
  begin
    E := S;
    if SLo = SHi then begin
      SLo := -3.4e+38;
      SHi := +3.4e+38;
    end;
    ReadExt(Prompt, Row, Col, Width, Places, SLo, SHi, E);
    if E = BadExt then   {!!.03}
      S := BadSgl        {!!.03}
    else                 {!!.03}
      S := E;
  end;

  procedure LineEditor.ReadComp(Prompt : string; Row, Col, Width : Byte;
                                CLo, CHi : Comp; var C : Comp);
    {-Edit a comp}
  var
    E : Extended;
  begin
    E := C;
    if CLo = CHi then begin
      CLo := -9.2e+18;
      CHi := +9.2e+18;
    end;
    ReadExt(Prompt, Row, Col, Width, 0, CLo, CHi, E);
    if E = BadExt then   {!!.03}
      C := BadComp       {!!.03}
    else                 {!!.03}
      C := E;
  end;

  {$ENDIF}

  procedure LineEditor.ReadChar(Prompt : string;      Row, Col : Byte;
                                ValidChars : CharSet; var Ch : Char);
    {-Display a prompt and wait for a key in ValidChars}
  var
    fRow, fCol, Width : Byte;
    CEF : CharField;
    SaveCase : CaseChange;
  begin
    {allow acceptance of default?}
    if (Ch in ValidChars) and LongFlagIsSet(leOptions, leDefaultAccepted) then
      Prompt := Prompt+' ['+Ch+'] ';

    {use User8 for valid char set}
    ExchangeStructs(ValidChars, UserSet8, SizeOf(CharSet));
    SaveCase := ForceCaseUser['8']; {!!.13}
    ForceCaseUser['8'] := NoChange;

    Width := 1;
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Width);

    if not CEF.InitSim(
      leHelpIndex, Prompt, Row, Col, '8', fRow, fCol, leHelpIndex, #0, #0,
      Ch, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(CEF);

    CEF.Done;

    {restore User8}
    ExchangeStructs(ValidChars, UserSet8, SizeOf(CharSet));
    ForceCaseUser['8'] := SaveCase;
  end;

  function LineEditor.YesOrNo(Prompt : string; Row, Col : Byte;
                              Default : Char) : Boolean;
    {-Return true for yes, false for no}
  var
    fRow, fCol, Width : Byte;
    YNEF : YesNoField;
    SaveCase : CaseChange;
    Options : LongInt;
    YN : Boolean;
  begin
    {if default answer is Y or N, allow <Enter> to accept it}
    Default := Upcase(Default);
    Options := leOptions or leShowReadChar;
    YN := (Default = YesChar);
    if Default in YesNoSet then begin
      SetLongFlag(Options, efDefaultAccepted);
      Prompt := Prompt+' ['+Default+'] ';
    end;

    Width := 1;
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Width);

    if not YNEF.InitSim(
      leHelpIndex, Prompt, Row, Col, fRow, fCol, leHelpIndex, YN, lePadChar,
      Options, leFlags or ifSuppressFirst, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(YNEF);

    YNEF.Done;

    {Yes if 'Y' entered and <Escape> not pressed}
    YesOrNo := YN and (leCmd <> ccQuit);
  end;

  procedure LineEditor.EditString(Prompt : string;  Row, Col : Byte;
                                  Picture : string; Width : Byte;
                                  var St : string);
    {-Edit a string}
  var
    fRow, fCol : Byte;
    SEF : StringField;
  begin
    leInitPicture(Picture, AnyChar, Width);

    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not SEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, Width, leHelpIndex,
      St, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(SEF);

    SEF.Done;
  end;

  procedure LineEditor.EditArray(Prompt : string;  Row, Col : Byte;
                                 Picture : string; Width : Byte; var A);
    {-Edit an array of char}
  var
    fRow, fCol : Byte;
    AEF : ArrayField;
  begin
    leInitPicture(Picture, AnyChar, Width);

    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not AEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, Width, leHelpIndex,
      A, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(AEF);

    AEF.Done;
  end;

  procedure LineEditor.EditChar(Prompt : string;  Row, Col : Byte;
                                Picture : string; ChLo, ChHi : Char;
                                var Ch : Char);
    {-Edit an array of char}
  var
    fRow, fCol : Byte;
    CEF : CharField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPicture(Picture, AnyChar, Width);

    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not CEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex,
      ChLo, ChHi, Ch, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(CEF);

    CEF.Done;
  end;

  procedure LineEditor.EditBoolean(Prompt : string;  Row, Col : Byte;
                                   Picture : string; var B : Boolean);
    {-Edit a boolean variable}
  var
    fRow, fCol : Byte;
    BEF : BooleanField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPicture(Picture, BooleanOnly, Width);

    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not BEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex, B,
      lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(BEF);

    BEF.Done;
  end;

  procedure LineEditor.EditYesNo(Prompt : string;  Row, Col : Byte;
                                 Picture : string; var YN : Boolean);
    {-Edit a yes-no variable}
  var
    fRow, fCol : Byte;
    YNEF : YesNoField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPicture(Picture, YesNoOnly, Width);

    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not YNEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex, YN,
      lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(YNEF);

    YNEF.Done;
  end;

  procedure LineEditor.EditLongInt(Prompt : string; Row, Col : Byte;
                                   Picture : string; LLo, LHi : LongInt;
                                   var L : LongInt);
    {-Edit a long integer}
  var
    fRow, fCol : Byte;
    LEF : LongIntField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not LEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex,
      LLo, LHi, L, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(LEF);

    LEF.Done;
  end;

  procedure LineEditor.EditInteger(Prompt : string; Row, Col : Byte;
                                   Picture : string; NLo, NHi : Integer;
                                   var N : Integer);
    {-Edit an integer}
  var
    L : LongInt;
  begin
    L := LongInt(N);
    if NLo = NHi then begin
      NLo := -32768;
      NHi := MaxInt;
    end;
    EditLongInt(Prompt, Row, Col, Picture, NLo, NHi, L);
    N := Integer(L);
  end;

  procedure LineEditor.EditWord(Prompt : string; Row, Col : Byte;
                                Picture : string; WLo, WHi : Word;
                                var W : Word);
    {-Edit a word}
  var
    L : LongInt;
  begin
    L := LongInt(W);
    if WLo = WHi then begin
      WLo := $0000;
      WHi := $FFFF;
    end;
    EditLongInt(Prompt, Row, Col, Picture, WLo, WHi, L);
    W := Word(L);
  end;

  procedure LineEditor.EditByte(Prompt : string; Row, Col : Byte;
                                Picture : string; BLo, BHi : Byte;
                                var B : Byte);
    {-Edit a byte}
  var
    L : LongInt;
  begin
    L := LongInt(B);
    if BLo = BHi then begin
      BLo := $00;
      BHi := $FF;
    end;
    EditLongInt(Prompt, Row, Col, Picture, BLo, BHi, L);
    B := Byte(L);
  end;

  procedure LineEditor.EditShortInt(Prompt : string; Row, Col : Byte;
                                    Picture : string; SLo, SHi : ShortInt;
                                    var SI : Shortint);
    {-Edit a ShortInt}
  var
    L : LongInt;
  begin
    L := LongInt(SI);
    if SLo = SHi then begin
      SLo := -128;
      SHi := 127;
    end;
    EditLongInt(Prompt, Row, Col, Picture, SLo, SHi, L);
    SI := ShortInt(L);
  end;

  procedure LineEditor.EditReal(Prompt : string; Row, Col : Byte;
                                Picture : string; Places : Byte;
                                RLo, RHi : Real; var R : Real);
    {-Edit a real}
  var
    fRow, fCol : Byte;
    REF : RealField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not REF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex,
      RLo, RHi, Places, R, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(REF);

    REF.Done;
  end;

{$IFDEF UseBcd}

  procedure LineEditor.EditBcd(Prompt : string;  Row, Col : Byte;
                               Picture : string; Places : Byte;
                               BLo, BHi : Bcd;   var B : BCD);
    {-Edit a BCD real}
  var
    fRow, fCol : Byte;
    BEF : BcdField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not BEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex,
      BLo, BHi, Places, B, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(BEF);

    BEF.Done;
  end;

{$ENDIF}

{$IFOPT N+}

  procedure LineEditor.EditExt(Prompt : string;     Row, Col : Byte;
                               Picture : string;    Places : Byte;
                               ELo, EHi : Extended; var E : Extended);
    {-Edit an extended}
  var
    fRow, fCol : Byte;
    EEF : ExtendedField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not EEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex,
      ELo, EHi, Places, E, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(EEF);

    EEF.Done;
  end;

  procedure LineEditor.EditDbl(Prompt : string;   Row, Col : Byte;
                               Picture : string;  Places : Byte;
                               DLo, DHi : Double; var D : Double);
    {-Edit a double}
  var
    E : Extended;
  begin
    E := D;
    if DLo = DHi then begin
      DLo := -1.7e+308;
      DHi := +1.7e+308;
    end;
    EditExt(Prompt, Row, Col, Picture, Places, DLo, DHi, E);
    if E = BadExt then   {!!.03}
      D := BadDbl        {!!.03}
    else                 {!!.03}
      D := E;
  end;

  procedure LineEditor.EditSgl(Prompt : string;   Row, Col : Byte;
                               Picture : string;  Places : Byte;
                               SLo, SHi : Single; var S : Single);
    {-Edit a single}
  var
    E : Extended;
  begin
    E := S;
    if SLo = SHi then begin
      SLo := -3.4e+38;
      SHi := +3.4e+38;
    end;
    EditExt(Prompt, Row, Col, Picture, Places, SLo, SHi, E);
    if E = BadExt then   {!!.03}
      S := BadSgl        {!!.03}
    else                 {!!.03}
      S := E;
  end;

  procedure LineEditor.EditComp(Prompt : string;  Row, Col : Byte;
                                Picture : string; CLo, CHi : Comp;
                                var C : Comp);
    {-Edit a comp}
  var
    E : Extended;
  begin
    E := C;
    if CLo = CHi then begin
      CLo := -9.2e+18;
      CHi := +9.2e+18;
    end;
    EditExt(Prompt, Row, Col, Picture, 0, CLo, CHi, E);
    if E = BadExt then   {!!.03}
      C := BadComp       {!!.03}
    else                 {!!.03}
      C := E;
  end;

{$ENDIF}

{$IFDEF UseDates}

  procedure LineEditor.EditDate(Prompt : string;
                                   Row, Col : Byte;
                                   Picture : string;
                                   DLo, DHi : Date;
                                   var D : Date);
    {-Edit a date variable}
  var
    fRow, fCol : Byte;
    DEF : DateField;
    Width : Byte;
  begin
    if Length(Picture) = 0 then
      Picture := 'mm/dd/yy';

    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not DEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex,
      DLo, DHi, D, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(DEF);

    DEF.Done;
  end;

  procedure LineEditor.EditDateSt(Prompt : string; Row, Col : Byte;
                                  Picture : DateString; var DS : DateString);
    {-Edit a date string}
  var
    fRow, fCol : Byte;
    DEF : DateStField;
    Width : Byte;
  begin
    if Length(Picture) = 0 then
      Picture := 'mm/dd/yy';

    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not DEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex, DS,
      lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(DEF);

    DEF.Done;
  end;

  procedure LineEditor.EditTime(Prompt : string;
                                   Row, Col : Byte;
                                   Picture : DateString;
                                   TLo, THi : Time;
                                   var T : Time);
    {-Edit a time variable}
  var
    fRow, fCol : Byte;
    TEF : TimeField;
    Width : Byte;
  begin
    if Length(Picture) = 0 then
      Picture := 'hh:mm:ss';

    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not TEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex,
      TLo, THi, T, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(TEF);

    TEF.Done;
  end;

{$ENDIF}

  procedure LineEditor.EditChoice(Prompt : string;
                                     Row, Col : Byte;
                                     Picture : string;
                                     DataSize : Byte;
                                     Increment : IncChoiceProc;
                                     var MC);
    {-Edit a multiple choice variable}
  var
    fRow, fCol : Byte;
    CEF : ChoiceField;
    Width : Byte;
  begin
    if DataSize > SizeOf(RangeType) then begin
      leError := epFatal+ecBadParam;
      leCmd := ccError;
      Exit;
    end;

    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not CEF.Init(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex, DataSize,
      Increment, MC, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(CEF);

    CEF.Done;
  end;

  procedure LineEditor.EditNumericLongInt(Prompt : string; Row, Col : Byte;
                                          Picture : string; LLo, LHi : LongInt;
                                          var L : LongInt);
    {-Edit a long integer (numeric)}
  var
    fRow, fCol : Byte;
    LEF : LongIntField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not LEF.InitNum(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex,
      LLo, LHi, L, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(LEF);

    LEF.Done;
  end;

  procedure LineEditor.EditNumericInteger(Prompt : string; Row, Col : Byte;
                                          Picture : string; NLo, NHi : Integer;
                                          var N : Integer);
    {-Edit an integer (numeric)}
  var
    L : LongInt;
  begin
    L := LongInt(N);
    if NLo = NHi then begin
      NLo := -32768;
      NHi := MaxInt;
    end;
    EditNumericLongInt(Prompt, Row, Col, Picture, NLo, NHi, L);
    N := Integer(L);
  end;

  procedure LineEditor.EditNumericWord(Prompt : string; Row, Col : Byte;
                                       Picture : string; WLo, WHi : Word;
                                       var W : Word);
    {-Edit a word (numeric)}
  var
    L : LongInt;
  begin
    L := LongInt(W);
    if WLo = WHi then begin
      WLo := $0000;
      WHi := $FFFF;
    end;
    EditNumericLongInt(Prompt, Row, Col, Picture, WLo, WHi, L);
    W := Word(L);
  end;

  procedure LineEditor.EditNumericByte(Prompt : string; Row, Col : Byte;
                                       Picture : string; BLo, BHi : Byte;
                                       var B : Byte);
    {-Edit a byte (numeric)}
  var
    L : LongInt;
  begin
    L := LongInt(B);
    if BLo = BHi then begin
      BLo := $00;
      BHi := $FF;
    end;
    EditNumericLongInt(Prompt, Row, Col, Picture, BLo, BHi, L);
    B := Byte(L);
  end;

  procedure LineEditor.EditNumericShortInt(Prompt : string; Row, Col : Byte;
                                           Picture : string; SLo, SHi : ShortInt;
                                           var SI : Shortint);
    {-Edit a ShortInt (numeric)}
  var
    L : LongInt;
  begin
    L := LongInt(SI);
    if SLo = SHi then begin
      SLo := -128;
      SHi := 127;
    end;
    EditNumericLongInt(Prompt, Row, Col, Picture, SLo, SHi, L);
    SI := ShortInt(L);
  end;

  procedure LineEditor.EditNumericReal(Prompt : string; Row, Col : Byte;
                                       Picture : string; Places : Byte;
                                       RLo, RHi : Real; var R : Real);
    {-Edit a real (numeric)}
  var
    fRow, fCol : Byte;
    REF : RealField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not REF.InitNum(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex, RLo, RHi,
      Places, R, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(REF);

    REF.Done;
  end;

{$IFDEF UseBcd}

  procedure LineEditor.EditNumericBcd(Prompt : string;  Row, Col : Byte;
                                      Picture : string; Places : Byte;
                                      BLo, BHi : Bcd;   var B : BCD);
    {-Edit a BCD real (numeric)}
  var
    fRow, fCol : Byte;
    BEF : BcdField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not BEF.InitNum(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex, BLo, BHi,
      Places, B, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(BEF);

    BEF.Done;
  end;

{$ENDIF}

{$IFOPT N+}

  procedure LineEditor.EditNumericExt(Prompt : string; Row, Col : Byte;
                                  Picture : string;    Places : Byte;
                                  ELo, EHi : Extended; var E : Extended);
    {-Edit an extended (numeric)}
  var
    fRow, fCol : Byte;
    EEF : ExtendedField;
    Width : Byte;
  begin
    Width := Length(Picture);
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Length(Picture));

    if not EEF.InitNum(
      leHelpIndex, Prompt, Row, Col, Picture, fRow, fCol, leHelpIndex, ELo, EHi,
      Places, E, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(EEF);

    EEF.Done;
  end;

  procedure LineEditor.EditNumericDbl(Prompt : string;   Row, Col : Byte;
                                      Picture : string;  Places : Byte;
                                      DLo, DHi : Double; var D : Double);
    {-Edit a double (numeric)}
  var
    E : Extended;
  begin
    E := D;
    if DLo = DHi then begin
      DLo := -1.7e+308;
      DHi := +1.7e+308;
    end;
    EditNumericExt(Prompt, Row, Col, Picture, Places, DLo, DHi, E);
    if E = BadExt then   {!!.03}
      D := BadDbl        {!!.03}
    else                 {!!.03}
      D := E;
  end;

  procedure LineEditor.EditNumericSgl(Prompt : string;   Row, Col : Byte;
                                      Picture : string;  Places : Byte;
                                      SLo, SHi : Single; var S : Single);
    {-Edit a single (numeric)}
  var
    E : Extended;
  begin
    E := S;
    if SLo = SHi then begin
      SLo := -3.4e+38;
      SHi := +3.4e+38;
    end;
    EditNumericExt(Prompt, Row, Col, Picture, Places, SLo, SHi, E);
    if E = BadExt then   {!!.03}
      S := BadSgl        {!!.03}
    else                 {!!.03}
      S := E;
  end;

  procedure LineEditor.EditNumericComp(Prompt : string;  Row, Col : Byte;
                                       Picture : string; CLo, CHi : Comp;
                                       var C : Comp);
    {-Edit a comp (numeric)}
  var
    E : Extended;
  begin
    E := C;
    if CLo = CHi then begin
      CLo := -9.2e+18;
      CHi := +9.2e+18;
    end;
    EditNumericExt(Prompt, Row, Col, Picture, 0, CLo, CHi, E);
    if E = BadExt then   {!!.03}
      C := BadComp       {!!.03}
    else                 {!!.03}
      C := E;
  end;

{$ENDIF}

  procedure LineEditor.EditMultiLine(Prompt : string; Row, Col : Byte;
                                     Width, Height : Byte; var StArray);
    {-Edit a multiline field}
  var
    fRow, fCol : Byte;
    MLF : MultiLineField;
  begin
    leInitPrim(Prompt, Row, Col, fRow, fCol, Width, Width);

    if not MLF.Init(
      leHelpIndex, Prompt, Row, Col, AnyChar, fRow, fCol, Width, Height,
      leHelpIndex, StArray, lePadChar, leOptions, leFlags, leColors) then begin
        leInitError;
        Exit;
    end;

    leReadPrim(MLF);

    MLF.Done;
  end;

{$IFDEF UseStreams}

  constructor LineEditor.Load(var S : IdStream);
    {-Load a line editor from a stream}
  begin
    {initialize parent}
    Root.Init;

    {initialize fields not stored in stream}
    leCmd := ccNone;
    leKey := 0;
    leError := 0;
    leCFF := DefClearFirstFunc;

    {Read data specific to the line editor}
    leCmdPtr := S.ReadUserPointer(@EditCommands);
    @leErrorProc := S.ReadUserPointer(@DefaultErrorProc);
    S.ReadRange(lePassword, leCFF);

    {check status}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure LineEditor.Store(var S : IdStream);
    {-Store a line editor in a stream}
  begin
    S.WriteUserPointer(leCmdPtr, ptEditCommands);
    S.WriteUserPointer(@leErrorProc, ptDefaultErrorProc);
    S.WriteRange(lePassword, leCFF);
  end;

  procedure LineEditorStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing line editors}
  begin
    with SPtr^ do begin
      RegisterType(otLineEditor, veLineEditor, TypeOf(LineEditor),
                   @LineEditor.Store, @LineEditor.Load);
      RegisterPointer(ptEditCommands, @EditCommands);
    end;
  end;

{$ENDIF}

begin
  {initialize command processor}
  EditCommands.Init(@EditKeySet, EditKeyMax);
end.
