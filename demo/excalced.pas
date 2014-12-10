{$R-,S-,I-,V-,B-}

program ExCalcEd;

{$I OPDEFINE.INC}

uses
  Dos,
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpAbsFld,
  OpCmd,
  OpFEdit,
  OpField,
  OpFrame,
  OpWindow,
  OpSelect,
  OpEntry;

  {$IFDEF UseMouse}
const
  MouseChar  : Char = #04;
  {$ENDIF}

{Entry field constants}
const
  idnLongint             = 0;
  idnHex                 = idnLongint + 1;
  idnReal                = idnHex + 1;
  idnParens              = idnReal + 1;
  idnDollar              = idnParens + 1;
  idnCurrency            = idnDollar + 1;

type
  UserRecord =
    record
      nLongint             : LongInt;
      nHex                 : LongInt;
      nReal                : Real;
      nParens              : Real;
      nDollar              : Real;
      nCurrency            : Real;
    end;

{Color set used by entry screen}
const
  EsColors : ColorSet = (
    TextColor       : $1E; TextMono        : $0F;
    CtrlColor       : $1E; CtrlMono        : $0F;
    FrameColor      : $1F; FrameMono       : $07;
    HeaderColor     : $3F; HeaderMono      : $70;
    ShadowColor     : $08; ShadowMono      : $0F;
    HighlightColor  : $4F; HighlightMono   : $70;
    PromptColor     : $17; PromptMono      : $07;
    SelPromptColor  : $17; SelPromptMono   : $07;
    ProPromptColor  : $17; ProPromptMono   : $07;
    FieldColor      : $1E; FieldMono       : $07;
    SelFieldColor   : $31; SelFieldMono    : $0F;
    ProFieldColor   : $17; ProFieldMono    : $07;
    ScrollBarColor  : $13; ScrollBarMono   : $07;
    SliderColor     : $13; SliderMono      : $0F;
    HotSpotColor    : $30; HotSpotMono     : $70;
    BlockColor      : $3E; BlockMono       : $0F;
    MarkerColor     : $3F; MarkerMono      : $70;
    DelimColor      : $1E; DelimMono       : $0F;
    SelDelimColor   : $31; SelDelimMono    : $0F;
    ProDelimColor   : $1E; ProDelimMono    : $0F;
    SelItemColor    : $3E; SelItemMono     : $70;
    ProItemColor    : $17; ProItemMono     : $07;
    HighItemColor   : $1F; HighItemMono    : $0F;
    AltItemColor    : $1F; AltItemMono     : $0F;
    AltSelItemColor : $3F; AltSelItemMono  : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono   : $0F;
    FlexBHelpColor  : $1F; FlexBHelpMono   : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono   : $70;
    UnselXrefColor  : $1E; UnselXrefMono   : $09;
    SelXrefColor    : $3F; SelXrefMono     : $70;
    MouseColor      : $4F; MouseMono       : $70
  );

var
  ES     : EntryScreen;
  UR     : UserRecord;
  Status : Word;
  I      : Word;

  procedure InitUserRecord(var UR : UserRecord);
  begin
    FillChar(UR, SizeOf(UR), 0);
    UR.nReal := BadReal;
    UR.nParens := BadReal;
    UR.nDollar := BadReal;
    UR.nCurrency := BadReal;
  end;

  function InitEntryScreen(var ES : EntryScreen;
                           var UR : UserRecord;
                           var EsColors : ColorSet) : Word;
  const
    Frame1 = 'ÚÀ¿ÙÄÄ³³';
    WinOptions = wBordered+wClear+wUserContents;
  begin
    with ES do begin
      if not InitCustom(22, 9, 57, 16, EsColors, WinOptions) then begin
        InitEntryScreen := InitStatus;
        Exit;
      end;

      wFrame.SetFrameType(Frame1);
      wFrame.AddHeader(' Calculator-style Editing Demo ', heTC);
      esFieldOptionsOn(efClearFirstChar+efRightJustify);
      (*esSecFieldOptionsOn(sefSuppressZero);*)
      SetWrapMode(WrapAtEdges);

    {idnLongint:}
      AddNumericLongField(
        'LongInt:', 2, 11,
        '###,###,###', 2, 20,
        1, $80000000, $7FFFFFFF, UR.nLongint);

    {idnHex:}
      AddNumericLongField(
        'Hex:', 3, 15,
        'KKKKKKKK', 3, 20,
        2, $80000000, $7FFFFFFF, UR.nHex);

    {idnReal:}
      AddNumericRealField(
        'Real:', 4, 14,
        '###,###,###.##', 4, 20,
        3, -1.5E+38,  1.5E+38, 0, UR.nReal);

    {idnParens:}
      esFieldOptionsOn(efParensForMinus);
      AddNumericRealField(
        'Parens for -:', 5, 6,
        '###,###,###.## ', 5, 20,
        4, -1.5E+38,  1.5E+38, 0, UR.nParens);
      esFieldOptionsOff(efParensForMinus);

    {idnDollar:}
      AddNumericRealField(
        'Floating dollar:', 6, 3,
        '$###,###,###.##', 6, 20,
        5, -1.5E+38,  1.5E+38, 0, UR.nDollar);

    {idnCurrency:}
      AddNumericRealField(
        'Fixed dollar:', 7, 6,
        'c###,###,###.##', 7, 20,
        6, -1.5E+38,  1.5E+38, 0, UR.nCurrency);

      InitEntryScreen := RawError;
    end;
  end;

begin
  ClrScr;

  {$IFDEF UseMouse}
  if MouseInstalled then
    with EsColors do begin
      {activate mouse cursor}
      SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+
                             Byte(MouseChar));
      ShowMouse;
      {enable mouse support}
      EntryCommands.cpOptionsOn(cpEnableMouse);
    end;
  {$ENDIF}

  {initialize user record}
  InitUserRecord(UR);

  {initialize entry screen}
  Status := InitEntryScreen(ES, UR, EsColors);
  if Status <> 0 then begin
    WriteLn('Error initializing entry screen: ', Status);
    Halt(1);
  end;

{$IFNDEF UseCalcEdit}
  for I := 0 to ES.asCount-1 do
    EntryFieldPtr(ES.FindField(I))^.efEditor := CalcEditor;
{$ENDIF}

  {test entry screen}
  ES.Process;
  ES.Erase;

  WriteLn(UR.nReal = BadReal);

  {$IFDEF UseMouse}
  HideMouse;
  {$ENDIF}

  {show exit command}
  WriteLn('Exit command = ', ES.GetLastCommand);
  ES.Done;
end.
