{$V-,F+}
program EntryExample; {EXENTRY3.PAS}
uses
  Use32,
  OpCrt,
  OpString,
  OpInline,
  OpConst,    {!!.20}
  OpRoot,
  OpCmd,
  OpFrame,
  OpWindow,
  OpAbsFld,
  OpField,
  OpFEdit,
  OpSelect,
  OpEntry;
type
  InfoRec = record
              Name : String[30];
              Bin : Byte;
            end;
const
  BinLo : RangeType = (rtByte : 0);
  BinHi : RangeType = (rtByte : 255);
var
  Info : InfoRec;
  ExitCommand : Word;
  ES : EntryScreen;
  Finished : Boolean;
  function Str2Bin(S : String; var B : Byte) : Boolean;
  const
    BinMag : array[1..8] of Byte = ($80,$40,$20,$10,$08,$04,$02,$01);
  var
    I, Bin : Byte;
  begin
    Str2Bin := False;
    Bin := 0;
    S := TrimSpaces(S);
    if S = '' then
      Exit;
    S := LeftPadCh(S, '0', 8);
    for I := 1 to 8 do
      if S[I] = '1' then
        Inc(Bin, BinMag[I])
      else if S[I] <> '0' then
        Exit;
    Str2Bin := True;
    B := Bin;
  end;
  function ValidateBin(EFP : EntryFieldPtr; var ErrCode : Word;
                       var ErrorSt : StringPtr) : Boolean;
  var
    B : Byte;
    S : string[80];
  begin
    ValidateBin := False;
    with EFP^ do begin
      StripPicture(efEditSt^, S);
      if not Str2Bin(S, B) then begin
        ErrCode := ecBadFormat;
        ErrorSt := @emInvalidNumber;
      end
      else if (B < efRangeLo.rtByte) or (B > efRangeHi.rtByte) then begin
        ErrCode := ecOutOfRange;
        ErrorSt := @emOutOfRange;
      end
      else
        ValidateBin := True
    end;
  end;
  procedure BinConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  var
    S : string[80];
  begin
    with EFP^ do
      if PostEdit then begin
        StripPicture(efEditSt^, S);
        if not Str2Bin(S, Byte(efVarPtr^)) then
          Byte(efVarPtr^) := 0;
      end
      else begin
        S := BinaryB(Byte(efVarPtr^));
        MergePicture(S, efEditSt^);
      end;
  end;
begin
  ClrScr;
  FillChar(Info, SizeOf(Info), 0);
  if not ES.InitCustom(25, 5, 55, 9, DefaultColorSet,
                       DefWindowOptions OR wBordered) then
    Halt;
  UserSet1 := ['0', '1', ' '];
  ForceCaseUser['1'] := NoChange;
  ES.esFieldOptionsOn(efClearFirstChar);
  ES.AddStringField('Name:', 2, 1, '', 2, 9, 20, 0, Info.Name);
  ES.AddUserField('Bin:', 3, 1,        {prompt, coordinates}
                  '11111111', 3, 9,    {picture mask, field coordinates}
                  8,                   {field width}
                  0,                   {help index}
                  BinLo, BinHi,        {range}
                  1,                   {data size}
                  0,                   {decimal places}
                  ValidateBin,         {validation routine}
                  BinConversion,       {conversion routine}
                  DrawString,          {standard drawing routine}
                  StringEditor,        {standard field editor}
                  Info.Bin);           {data being edited}
  Finished := False;
  repeat
    ES.Process;
    case ES.GetLastCommand of
      ccQuit, ccDone, ccError : Finished := True;
      {Process user exit commands}
    end;
  until Finished;
  ES.Done;
end.
