{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPABSFLD.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1988,1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpAbsFld;
  {-Abstract fields, picture mask stuff}


interface

uses
  Use32,
  Dos,
  OpInline,
  OpString,
  OpRoot
  {$IFDEF UseDates}
  ,OpDate
  {$ENDIF}
  {$IFDEF UseBcd}
  ,OpBcd
  {$ENDIF}
  ;

  {------------ picture masks ---------------}

const
  {the following characters are meaningful in Picture strings}
  AnyChar     = 'X';         {allows any character}
  ForceUp     = '!';         {allows any character, forces upper case}
  ForceLo     = 'L';         {allows any character, forces lower case}
  ForceMixed  = 'x';         {allows any character, forces mixed case}
  AlphaOnly   = 'a';         {allows alphas only}
  UpperAlpha  = 'A';         {allows alphas only, forces upper case}
  LowerAlpha  = 'l';         {allows alphas only, forces lower case}
  NumberOnly  = '9';         {allows numbers and spaces only}
  DigitOnly   = '#';         {allows numbers, spaces, minus, period}
  Scientific  = 'E';         {allows numbers, spaces, minus, period, 'e'}
  HexOnly     = 'K';         {allows 0-9 and A-F, forces upper case}
  BooleanOnly = 'B';         {allows T, t, F, f}
  YesNoOnly   = 'Y';         {allows Y, y, N, n}

  User1       = '1';         {for user-defined character sets 1..8}
  User2       = '2';
  User3       = '3';
  User4       = '4';
  User5       = '5';
  User6       = '6';
  User7       = '7';
  User8       = '8';

  Subst1      = #1;          {for user-defined substitutions} {!!.02}
  Subst2      = #2;
  Subst3      = #3;
  Subst4      = #4;
  Subst5      = #5;
  Subst6      = #6;
  Subst7      = #7;
  Subst8      = #8;

const
  {other special characters allowed in Picture strings}
  FloatDollar = '$';         {floating dollar sign}
  CurrencyLt  = 'c';         {currency to left of the amount}
  CurrencyLtStr : string[5] = '$'; {corresponding string}
  CurrencyRt  = 'C';         {currency to right of the amount}
  CurrencyRtStr : string[5] = '';
  DecimalPt   = '.';         {insert decimal point}
  DecimalChar : Char = '.';  {character used for decimal point}
  Comma       = ',';         {character used to separate numbers}
  CommaChar   : Char = ',';  {character used for comma}
  {NOTE: Comma and FloatDollar are allowed only in fields containing
   fixed decimal points and/or numeric fields in which there can be no
   decimal point}

  {character sets corresponding to the first group of constants shown above}
  AnyCharSet    : CharSet = [#0..#255]; {AnyChar, ForceUp, ForceLo, ForceMixed}
  AlphaOnlySet  : CharSet = {AlphaOnly, UpperAlpha, LowerAlpha}
    ['A'..'Z', 'a'..'z', #128..#154, #160..#167, ' ', '-', '.', ','];
  NumberOnlySet : CharSet = ['0'..'9', ' '];
  DigitOnlySet  : CharSet = ['0'..'9', ' ', '-', '.'];
  ScientificSet : CharSet = ['0'..'9', ' ', '-', '+', '.', 'e', 'E'];
  HexOnlySet    : CharSet = ['0'..'9', ' ', 'A'..'F', 'a'..'f'];

  BooleanSet    : CharSet = ['T','t','F','f'];
  TrueChar      : Char    = 'T';
  FalseChar     : Char    = 'F';

  YesNoSet      : CharSet = ['Y','y','N','n'];
  YesChar       : Char    = 'Y';
  NoChar        : Char    = 'N';

  UserSet1      : CharSet = [#0..#255]; {user-defined sets}
  UserSet2      : CharSet = [#0..#255];
  UserSet3      : CharSet = [#0..#255];
  UserSet4      : CharSet = [#0..#255];
  UserSet5      : CharSet = [#0..#255];
  UserSet6      : CharSet = [#0..#255];
  UserSet7      : CharSet = [#0..#255];
  UserSet8      : CharSet = [#0..#255];

type
  CaseChange   = (NoChange, UpperCase, LowerCase, MixedCase);
const
  {these determine whether or not upper/lower case is forced for User1..User8}
  ForceCaseUser : array[User1..User8] of CaseChange = (
    NoChange, NoChange, NoChange, NoChange,
    NoChange, NoChange, NoChange, NoChange);
  {for user-defined substitutions} {!!.02}
  SubstChars : array[Subst1..Subst8] of Char = (#1, #2, #3, #4, #5, #6, #7, #8);

const
  BadReal : Real     = -1.501e+38;      {!!.02}
{$IFOPT N+}
const
  BadExt  : Extended = -1.101e+4931;    {!!.02}
const
  BadComp : Comp     = -9.201e+18;      {!!.02}
const
  BadDbl  : Double   = -1.701e+308;     {!!.02}
const
  BadSgl  : Single   = -3.401e+38;      {!!.02}
{$ENDIF}
{$IFDEF UseBcd}
var
  BadBcd  : Bcd absolute MinBcd;        {!!.02}
{$ENDIF}

type
  PictureFlags  = array[1..255] of Boolean;

  AbstractField =
    object(DoubleListNode)
      procedure InitPictureFlags(var PFlags : PictureFlags);
        {-Initialize a picture flags table}
      procedure MergePicture(St : string; var S : string);
        {-Merge St with Picture and return result in S}
      procedure StripPicture(var S1, S2 : string);
        {-Strip picture characters out of S1 and return in S2}
      procedure CalcWidthAndPlaces(var Width, Places : Word);
        {-Calculate width and decimal places for a numeric field}
{.Z+}
      {+++ internal methods +++}
      function afWidth : Byte; virtual; {!!.22}
      function afMaxLen : Byte; virtual;
      function afPicture : String; virtual;
      function afDPlaces : Byte; virtual;
      function afNoLiterals : Boolean; virtual;
      function afNumeric : Boolean; virtual;
      function afHexadecimal : Boolean; virtual;
      function afRightJustified : Boolean; virtual;
      function afIsReal : Boolean; virtual;
      function afIsNumber : Boolean;
{.Z-}
    end;

  {-------miscellaneous routines-------}

procedure ChangeDecimalChar(DC : Char);
  {-Reset DecimalChar to DC}

function InternationalCurrency(FormChar : Char; MaxDigits : Byte;
                               FloatIfPossible : Boolean;
                               AddCommas : Boolean) : string;
  {-Return a picture mask for a currency string, based on DOS's country info}

  {-------intended for internal use------}

{.Z+}
procedure FixCase(PicChar : Char; var Ch : Char; PrevCh : Char);
  {-Fix the case of Ch based on PicChar}

procedure TrimSpacesPrim(var S : string);
  {-Return a string with leading and trailing blanks removed}

procedure TrimTrailingZeros(var S : string);
  {-Trim trailing zeros from a numeric string. It is assumed that there is a
    decimal point prior to the zeros. Also strips leading spaces}

procedure TrimLeadingZeros(var S : string);
  {-Trim leading zeros from a numeric string. Also strips leading spaces}
{.Z-}

  {==========================================================================}

implementation

  procedure TrimSpacesPrim(var S : string);
    {-Return a string with leading and trailing blanks removed}
  var
    I : Word;
    SLen : Byte absolute S;
  begin
    while (SLen > 0) and (S[SLen] = ' ') do
      Dec(SLen);
    I := 1;
    while (I <= SLen) and (S[I] = ' ') do
      Inc(I);
    if I > 1 then
      Delete(S, 1, I-1);
  end;

  procedure TrimTrailingZeros(var S : string);
    {-Trim trailing zeros from a numeric string. It is assumed that there is a
      decimal point prior to the zeros. Also strips leading spaces}
  var
    SLen : Byte absolute S;
  begin
    while S[SLen] = '0' do
      Dec(SLen);
    if S[SLen] = '.' then
      Dec(SLen);
    TrimSpacesPrim(S);
  end;

  procedure TrimLeadingZeros(var S : string);
    {-Trim leading zeros from a numeric string. Also strips leading spaces}
  var
    SLen : Byte absolute S;
  begin
    TrimSpacesPrim(S);
    while (SLen > 1) and (S[1] = '0') do
      Delete(S, 1, 1);
  end;

  procedure FixCase(PicChar : Char; var Ch : Char; PrevCh : Char);
    {-Fix the case of Ch based on PicChar}
  begin
    case PicChar of
      {$IFDEF UseDates}
      NameOnlyU,
      {$ENDIF}
      ForceUp,
      UpperAlpha,
      BooleanOnly,
      YesNoOnly,
      Scientific,
      HexOnly :
        Ch := Upcase(Ch);
      ForceLo,
      LowerAlpha :
        Ch := Locase(Ch);
      ForceMixed :
        case PrevCh of
          ' ', '-' :
            Ch := Upcase(Ch);
        end;
      {$IFDEF UseDates}
      TimeOnly,
      EmOnly :
        if UpcaseTime then
          Ch := Upcase(Ch);
      {$ENDIF}

      User1..User8 :
        case ForceCaseUser[PicChar] of
          UpperCase :
            Ch := Upcase(Ch);
          LowerCase :
            Ch := Locase(Ch);
          MixedCase :
            case PrevCh of
              ' ', '-' :
                Ch := Upcase(Ch);
            end;
        end;
    end;
  end;

  procedure FixDecimalPoint(var S : string);
    {-Fix decimal points for real numbers before merging}
  var
    I : Word;
  begin
    I := Pos('.', S);
    if I <> 0 then
      S[I] := DecimalChar;
  end;

  procedure AbstractField.InitPictureFlags(var PFlags : PictureFlags);
    {-Initialize a picture flags table}
  const
    PictureChars : set of Char = [
      AnyChar, ForceUp, ForceLo, ForceMixed, AlphaOnly, UpperAlpha,
      LowerAlpha, NumberOnly, DigitOnly, Scientific, HexOnly,
      BooleanOnly, YesNoOnly,
      {$IFDEF UseDates}
      NameOnly, NameOnlyU,
      MonthOnly, DayOnly, YearOnly, HourOnly, MinOnly, SecOnly,
      MonthOnlyU, DayOnlyU, HourOnlyU, MinOnlyU, SecOnlyU, TimeOnly, EmOnly,
      {$ENDIF}
      User1..User8];
  var
    I : Word;
    P : String;
  begin
    FillChar(PFlags[afMaxLen+1], SizeOf(PFlags)-afMaxLen, False);
    if afNoLiterals then
      FillChar(PFlags, afMaxLen, True)
    else begin
      P := afPicture;
      for I := 1 to afMaxLen do
        PFlags[I] := (P[I] in PictureChars);
    end;
  end;

  function AbstractField.afWidth : Byte; {!!.22}
    {-Returns width of field}
  begin
    Abstract;
  end;

  function AbstractField.afMaxLen : Byte;
    {-Returns maximum length of field}
  begin
    Abstract;
  end;

  function AbstractField.afPicture : String;
    {-Returns picture mask}
  begin
    Abstract;
  end;

  function AbstractField.afDPlaces : Byte;
    {-Returns default number of decimal places}
  begin
    afDPlaces := 0;
  end;

  function AbstractField.afNoLiterals : Boolean;
    {-Returns True if there are no literals in the picture mask}
  begin
    afNoLiterals := False;
  end;

  function AbstractField.afNumeric : Boolean;
    {-Returns True if the field is "numeric"}
  begin
    afNumeric := False;
  end;

  function AbstractField.afHexadecimal : Boolean;
    {-Returns True if the field holds hexadecimal numbers}
  begin
    afHexadecimal := False;
  end;

  function AbstractField.afRightJustified : Boolean;
    {-Returns True if field is right justified}
  begin
    afRightJustified := False;
  end;

  function AbstractField.afIsReal : Boolean;
    {-Returns True if field is a real type}
  begin
    afIsReal := False;
  end;

  function AbstractField.afIsNumber : Boolean;
    {-Returns True if field is treated as numeric by MergePicture}
  var
    S : string;
  begin
    {is it a numeric string?}
    S := afPicture;
    afIsNumber := (Pos(DecimalPt, S) <> 0) or
                  (Pos(FloatDollar, S) <> 0) or
                  (Pos(Comma, S) <> 0) or
                  (Pos(CurrencyLt, S) <> 0) or
                  (Pos(CurrencyRt, S) <> 0) or
                  afNumeric or
                  afHexadecimal;
  end;

  procedure AbstractField.CalcWidthAndPlaces(var Width, Places : Word);
    {-Calculate width and decimal places for a numeric field}
  var
    Flags : PictureFlags;
    I, DotPos : Word;
    Picture : String;
  begin
    {initialize flags table}
    InitPictureFlags(Flags);

    {find position of period}
    Picture := afPicture;
    DotPos := Pos(DecimalPt, Picture);

    {calculate decimal places}
    if DotPos = 0 then
      Places := afDPlaces
    else begin
      I := DotPos+1;
      Places := 0;
      while Flags[I] do begin
        Inc(Places);
        Inc(I);
      end;
    end;

    {calculate width}
    I := 1;
    while not Flags[I] do
      Inc(I);
    Width := 0;
    while Flags[I] or (Picture[I] = Comma) do begin
      Inc(Width, Ord(Flags[I]));
      Inc(I);
    end;

    {add decimal places and period}
    if (DotPos <> 0) and (Places <> 0) then
      Inc(Width, Places+1);
  end;

  procedure AbstractField.StripPicture(var S1, S2 : string);
    {-Strip picture characters out of S1 and return in S2}
  var
    SLen : Byte absolute S2;
    Flags : PictureFlags;
    I, J, FDP : Word;
    Picture : String;
  begin
    Picture := afPicture;
    if Length(S1) <> Length(Picture) then begin
      S2 := S1;
      Exit;
    end;

    InitPictureFlags(Flags);

    I := Pos(DecimalPt, Picture);
    if I <> 0 then
      Flags[I] := True;

    FDP := Pos(FloatDollar, Picture);
    if FDP <> 0 then begin
      while (Picture[FDP] = FloatDollar) do begin
        Flags[FDP] := True;
        Inc(FDP);
      end;
    end;

    for J := 1 to Length(Picture) do
      if Picture[J] = Comma then
        Flags[J] := True;

    SLen := 0;
    for I := 1 to Length(Picture) do
      if Flags[I] then begin
        Inc(SLen);
        S2[SLen] := S1[I];
      end;

    if FDP <> 0 then begin
      I := Pos(CurrencyLtStr, S2);
      if I <> 0 then
        Delete(S2, I, Length(CurrencyLtStr));
    end;

    if Pos(Comma, Picture) <> 0 then
      repeat
        I := Pos(CommaChar, S2);
        if I <> 0 then
          Delete(S2, I, 1);
      until (I = 0);

    if Pos(DecimalPt, Picture) <> 0 then begin
      I := Pos(DecimalChar, S2);
      if I <> 0 then
        S2[I] := '.';
    end;
  end;

  procedure AbstractField.MergePicture(St : string; var S : string);
    {-Merge St with Picture and return result in S}
  var
    SLen : Byte absolute S;
    StLen : Byte absolute St;
    Width,
    Places,
    DotPosP,
    DotPosS,
    FloatPos,
    I, J, K, N : Word;
    Flags : PictureFlags;
    IsNumber,
    NeedMinus,
    NeedFloat : Boolean;
    TempCurrency : string[5];
    TClen : Byte absolute TempCurrency;
    Picture : String;
  begin
    Picture := afPicture;
    S := Picture;
    DotPosP := Pos(DecimalPt, Picture);
    InitPictureFlags(Flags);

    {is it a numeric string?}
    IsNumber := afIsNumber;

    {take care of currency strings}
    I := Pos(CurrencyLt, Picture);
    if I <> 0 then begin
      K := I;
      while (K < SLen) and (S[K+1] = CurrencyLt) do
        Inc(K);
      J := Length(CurrencyLtStr);
      for N := K downto I do
        if J > 0 then begin
          S[N] := CurrencyLtStr[J];
          Dec(J);
        end
        else
          S[N] := ' ';
    end;
    I := Pos(CurrencyRt, Picture);
    if I <> 0 then begin
      J := 1;
      while (I <= SLen) and (Picture[I] = CurrencyRt) do begin
        if J <= Length(CurrencyRtStr) then
          S[I] := CurrencyRtStr[J]
        else
          S[I] := ' ';
        Inc(I);
        Inc(J);
      end;
    end;

    if IsNumber then begin
      {see if we need to strip picture characters}
      if (StLen = SLen) and (Pos(DecimalPt, St) {DotPosS} = DotPosP) then {!!.11}
        StripPicture(St, St);

      {we need to fill in the FloatDollar positions too, if any}
      FloatPos := Pos(FloatDollar, Picture);
      if FloatPos <> 0 then begin
        TempCurrency := CurrencyLtStr;
        while Picture[FloatPos] = FloatDollar do begin
          Flags[FloatPos] := True;
          Inc(FloatPos);
        end;
        Dec(FloatPos);
      end
      else
        TClen := 0;

      {trim leading and trailing blanks}
      TrimSpacesPrim(St);

      {check for a minus sign}
      NeedMinus := (StLen > 0) and (St[1] = '-');
      if NeedMinus then
        Delete(St, 1, 1);

      {it's a numeric field--align the decimal points}
      DotPosS := Pos(DecimalPt, St);

      {see if we need a floating dollar sign}
      if StLen = 0 then
        NeedFloat := False
      else
        NeedFloat := TClen <> 0;

      {if there's no tail, pretend there's a dot beyond the end of St}
      if DotPosS = 0 then
        K := StLen+1
      else
        K := DotPosS;

      {copy the tail of the string}
      if DotPosP = 0 then
        I := SLen + 1
      else
        I := DotPosP+1;
      J := K+1;
      while (J <= StLen) and (I <= SLen) and Flags[I] do begin
        S[I] := St[J];
        Inc(I);
        Inc(J);
      end;

      {pad to end with 0's}
      while (I <= SLen) and Flags[I] do begin
        S[I] := '0';
        Inc(I);
      end;

      {handle substitution characters}                      {!!.14}
      while (I <= SLen) and not Flags[I] do begin           {!!.14}
        case Picture[I] of                                  {!!.14}
          Subst1..Subst8 : S[I] := SubstChars[Picture[I]];  {!!.14}
        end;                                                {!!.14}
        Inc(I);                                             {!!.14}
      end;                                                  {!!.14}

      {copy the front of the string}
      if DotPosP = 0 then
        J := SLen
      else
        J := DotPosP;
      if DotPosS <> 0 then
        StLen := DotPosS-1;
      for I := J downto 1 do
        if Flags[I] then begin
          if (StLen <> 0) and (I > FloatPos) then begin
            S[I] := St[StLen];
            Dec(StLen);
          end
          else if NeedFloat then begin
            S[I] := TempCurrency[TClen];
            Dec(TClen);
            NeedFloat := TClen <> 0;
          end
          else if NeedMinus then begin
            S[I] := '-';
            NeedMinus := False;
          end
          else
            S[I] := ' ';
        end
        else case Picture[I] of
          Subst1..Subst8 :                  {!!.02}
            S[I] := SubstChars[Picture[I]]; {!!.02}
          DecimalPt :
            S[I] := DecimalChar;
          Comma :
            if (StLen <> 0) then
              S[I] := CommaChar
            else if NeedFloat then begin
              S[I] := TempCurrency[TClen];
              Dec(TClen);
              NeedFloat := TClen <> 0;
            end
            else if NeedMinus then begin
              S[I] := '-';
              NeedMinus := False;
            end
            else
              S[I] := ' ';
        end;

      {put in a 0 before the dot if necessary}
      if DotPosP <> 0 then begin
        I := DotPosP-1;
        if (S[I] = ' ') then
          S[I] := '0';
      end;
    end
    else begin
      {deal with problem w/ reals w/ variable # of places} {!!.12}
      if afIsReal and (StLen > SLen) then                  {!!.12}
        if Pos(DecimalPt, St) <> 0 then begin              {!!.12}
          StLen := SLen;                                   {!!.12}
          TrimTrailingZeros(St);                           {!!.12}
        end;                                               {!!.12}

      if afRightJustified then begin
        {fill in the characters from St}
        J := StLen;
        for I := SLen downto 1 do
          if Flags[I] then
            if (J >= 1) then begin
              S[I] := St[J];
              FixCase(Picture[I], S[I], #255);
              Dec(J);
            end
            else
              S[I] := ' '                     {!!.02}
          else case Picture[I] of             {!!.02}
            Subst1..Subst8 :                  {!!.02}
              S[I] := SubstChars[Picture[I]]; {!!.02}
          end;                                {!!.02}
      end
      else begin
        {fill in the characters from St}
        J := 1;
        for I := 1 to SLen do
          if Flags[I] then
            if (J <= StLen) then begin
              S[I] := St[J];
              FixCase(Picture[I], S[I], #255);
              Inc(J);
            end
            else
              S[I] := ' '                     {!!.02}
          else case Picture[I] of             {!!.02}
            Subst1..Subst8 :                  {!!.02}
              S[I] := SubstChars[Picture[I]]; {!!.02}
          end;                                {!!.02}
      end;
      if afIsReal then
        FixDecimalPoint(S);
    end;
  end;

{$IFNDEF UseDates}

type
  CountryInfoPtr = ^CountryInfo;
  CountryInfo =
    record
      DateFormat : Word;     {0=US (mdy), 1=Europe (dmy), 2 = Japan (ymd)}
      case Byte of
        2 : (                  {DOS 2.x}
          CurrencySym : Char;  {'$' for US}
          Unused1 : Byte;      {0}
          CommaSym1 : Char;    {',' for US}
          Unused2 : Byte;      {0}
          DecimalSym1 : Char); {'.' for US}

        3 : (                  {DOS 3.x or higher}
          CurrencyStr : array[1..5] of Char; {ASCIIZ string}
          CommaSym2 : Char;    {',' for US}
          Unused3 : Byte;      {0}
          DecimalSym2 : Char;  {'.' for US}
          Unused4 : Byte;      {0}
          DateSym : Char;      {'-' for US}
          Unused5 : Byte;      {0}
          TimeSym : Char;      {':' for US}
          Unused6 : Byte;      {0}
          CurrencyForm : Byte; {0-4}
          Decimals : Byte;     {# of digits after decimal point}
          TimeForm : Byte;     {bit 0 = 0 for 12-hour clock; 1 for 24-hour}
          Unused7 : array[1..14] of Byte);
    end;

  function GetCountryInfo(var Dos2 : Boolean; var Info : CountryInfo) : Boolean;
    {-Return a country information table in Info}
  var
    Regs : Registers;
  begin
    with Regs do begin
      {get DOS version}
      AX := $3000;
      Intr($21, Regs);
      Dos2 := (AL = 2);

      {get pointer to country information table}
      AX := $3800;
      DS := Seg(Info);
      DX := Ofs(Info);
      Intr($21, Regs);
      GetCountryInfo := not Odd(Flags);
    end;
  end;

{$ENDIF}

  function InternationalCurrency(FormChar : Char; MaxDigits : Byte;
                                 FloatIfPossible : Boolean;
                                 AddCommas : Boolean) : string;
    {-Return a picture mask for a currency string, based on DOS's country info}
  var
    Info : CountryInfo;
    Dos2 : Boolean;
    S : string;
    SLen : Byte absolute S;
    I, J : Word;
    CTemp : string[5];
    CTlen : Byte absolute CTemp;
    CLSlen : Byte absolute CurrencyLtStr;
    CRSlen : Byte absolute CurrencyRtStr;
  begin
    {get country information table}
    if (MaxDigits = 0) or not GetCountryInfo(Dos2, Info) then begin
      InternationalCurrency[0] := #0;
      Exit;
    end;

    {initialize S with the numeric part of the string to left of decimal point}
    I := Pred(MaxDigits) div 3 ;
    J := Word(MaxDigits)+(I*Ord(AddCommas));
    if J > 247 then
      SLen := 247
    else
      SLen := J;
    FillChar(S[1], SLen, FormChar);
    if AddCommas then begin
      {insert commas at appropriate points}
      J := 0;
      for I := SLen downto 1 do
        if J = 3 then begin
          S[I] := Comma;
          J := 0;
        end
        else
          Inc(J);
    end;

    {wipe out the current currency strings}
    CLSlen := 0;
    CRSlen := 0;

    with Info do begin

      if Dos2 then begin
        {limited information available}
        CommaChar := CommaSym1;
        ChangeDecimalChar(DecimalSym1);

        {use US format since we don't know any better}
        CurrencyLtStr := CurrencySym;
        CurrencyForm := 0;

        {assume two decimal points}
        Decimals := 2;
      end
      else begin
        {get the easy stuff first}
        CommaChar := CommaSym2;
        ChangeDecimalChar(DecimalSym2);

        {extract the currency string}
        CTlen := 1;
        while (CurrencyStr[CTlen] <> #0) and (CTlen <= 4) do begin
          CTemp[CTlen] := CurrencyStr[CTlen];
          Inc(CTlen);
        end;
        Dec(CTlen);

        {use US format if we don't know any better}
        if CurrencyForm > 3 then
          CurrencyForm := 0;

        {now fix up the currency strings}
        case CurrencyForm of
          0 :  {symbol leads currency, no space}
           CurrencyLtStr := CTemp;
          1 :  {symbol follows currency, no space}
           CurrencyRtStr := CTemp;
          2 :  {symbol leads currency, one space}
           CurrencyLtStr := CTemp+' ';
          3 :  {symbol follows currency, one space}
           CurrencyRtStr := ' '+CTemp;
        end;
      end;

      {add in the decimals}
      if Decimals > 0 then begin
        Inc(SLen);
        S[SLen] := DecimalPt;
        FillChar(S[SLen+1], Decimals, FormChar);
        Inc(SLen, Decimals);
      end;

      {see if we can do a floating currency symbol}
      if FloatIfPossible then
        FloatIfPossible := (CurrencyForm = 0) or (CurrencyForm = 2);

      {plug in the picture characters for the currency symbol}
      if FloatIfPossible then
        S := CharStr(FloatDollar, CLSlen)+S
      else if CLSlen <> 0 then
        S := CharStr(CurrencyLt, CLSlen)+S
      else
        S := S+CharStr(CurrencyRt, CRSlen);
    end;

    InternationalCurrency := S;
  end;

  procedure ChangeDecimalChar(DC : Char);
    {-Reset DecimalChar to DC}
  begin
    if DC <> DecimalChar then begin
      DigitOnlySet := DigitOnlySet-[DecimalChar];
      ScientificSet := ScientificSet-[DecimalChar];
      DecimalChar := DC;
      DigitOnlySet := DigitOnlySet+[DecimalChar];
      ScientificSet := ScientificSet+[DecimalChar];
    end;
  end;

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
