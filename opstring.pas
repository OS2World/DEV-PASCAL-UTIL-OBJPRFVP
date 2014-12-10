{$IFDEF Windows}                {!!.20}
  {$S-,R-,V-,I-,B-,F-,A-}       {!!.20}
  {$DEFINE PMode}               {!!.20}
{$ELSE}                         {!!.20}
  {$S-,R-,V-,I-,B-,F-,O-,A-}    {!!.20}
  {$IFDEF Dpmi}                 {!!.20}
    {$DEFINE PMode}             {!!.20}
  {$ENDIF}                      {!!.20}
{$ENDIF}                        {!!.20}

{$IFDEF VIRTUALPASCAL}
{$OrgName+,Cdecl-,AlignRec-}
{$ENDIF VIRTUALPASCAL}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPSTRING.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OPString;
  {-Basic string manipulation routines}

interface

{$IFDEF Windows}         {!!.20 begin}
uses
  WinProcs,
  WsDpmi,
  WinDos;

const
  DpmiInUse : Boolean = True;
type
  ExtStr  = String[3];
{$ELSE}                  {!!.20 end}
uses
{$IFDEF VirtualPascal}
  VpSysLow,
{$ENDIF}
  Use32,
  Dpmi,                  {!!.20}
{$IFDEF Dpmi}            {!!.20}
  WinApi,                {!!.20}
{$ENDIF}                 {!!.20}
  Dos;                   {!!.01}
{$ENDIF}                 {!!.20}

const
  ExtLen = 3;
{$IFDEF VIRTUALPASCAL}
  DpmiInUse : Boolean = False;
{$ENDIF}

type
  StringPtr = ^String;
  {ExtStr = string[ExtLen];}   {!!.01}
  CharSet = set of Char;
  CompareType = (Less, Equal, Greater);
  BTable = array[0..255] of Byte; {For Boyer-Moore searching}
  {$IFOPT N+}
   Float = Extended;
  {$ELSE}
   Float = Real;
  {$ENDIF}

type                            {!!.20 begin}
  {for International upcase support}
  CaseTable = Array[128..165] of Char;

const
  DefUCTable : CaseTable = (
      'Ä', 'ö', 'E', 'A', 'é', 'A', 'è', 'Ä', 'E', 'E',
      'E', 'I', 'I', 'I', 'é', 'è', 'ê', 'í', 'í', 'O',
      'ô', 'O', 'U', 'U', 'Y', 'ô', 'ö', 'ù', 'ú', 'ù',
      'û', 'ü', 'A', 'I', 'O', 'U', '•', '•'
    );

const
  LCTable : CaseTable = (
      'á', 'Å', 'Ç', 'É', 'Ñ', 'Ö', 'Ü', 'á', 'à', 'â',
      'ä', 'ã', 'å', 'ç', 'Ñ', 'Ü', 'Ç', 'ë', 'ë', 'ì',
      'î', 'ï', 'ñ', 'ó', 'ò', 'î', 'Å', 'õ', 'ú', 'õ',
      'û', 'ü', '†', '°', '¢', '£', '§', '§'
    );
var
  UCTable : CaseTable;
  UpcaseFunc : Pointer;         {!!.20 end}

const
  {used by CompareLetterSets for estimating word similarity}
  LetterValues : array['A'..'Z'] of Byte = (
    3 {A} , 6 {B} , 5 {C} , 4 {D} , 3 {E} , 5 {F} , 5 {G} , 4 {H} , 3 {I} ,
    8 {J} , 7 {K} , 4 {L} , 5 {M} , 3 {N} , 3 {O} , 5 {P} , 7 {Q} , 4 {R} ,
    3 {S} , 3 {T} , 4 {U} , 6 {V} , 5 {W} , 8 {X} , 8 {Y} , 9 {Z} );
const
  MoneySign : Char = '$';    {Used by Form for floating dollar sign}
  CommaForPeriod : Boolean = False; {replace '.' with ',' in Form masks}

  {-------- Numeric conversion -----------}

  procedure ReplacePrim(var Buffer; BufLength, Posn, SrcLen : Word;
                        var Repl  ; RepLength : Word);

function HexB(B : Byte) : string;
  {-Return hex string for byte}

function HexW(W : Word) : string;
  {-Return hex string for word}

function HexL(L : LongInt) : string;
  {-Return hex string for longint}

function HexPtr(P : Pointer) : string;
  {-Return hex string for pointer}

function BinaryB(B : Byte) : string;
  {-Return binary string for byte}

function BinaryW(W : Word) : string;
  {-Return binary string for word}

function BinaryL(L : LongInt) : string;
  {-Return binary string for longint}

function OctalB(B : Byte) : string;
  {-Return octal string for byte}

function OctalW(W : Word) : string;
  {-Return octal string for word}

function OctalL(L : LongInt) : string;
  {-Return octal string for longint}

function Str2Int(S : string; var I : Integer) : Boolean;
  {-Convert a string to an integer, returning true if successful}

function Str2Word(S : string; var I : Word) : Boolean;
  {-Convert a string to a word, returning true if successful}

function Str2Long(S : string; var I : LongInt) : Boolean;
  {-Convert a string to an longint, returning true if successful}

function Str2Real(S : string; var R : Float) : Boolean;
  {-Convert a string to a real, returning true if successful}

function Long2Str(L : LongInt) : string;
  {-Convert a longint/word/integer/byte/shortint to a string}

function Real2Str(R : Float; Width : Byte; Places : ShortInt) : string;
  {-Convert a real to a string}

function Form(Mask : string; R : Float) : string;
  {-Returns a formatted string with digits from R merged into the Mask}

function LongIntForm(Mask : string; L : LongInt) : string;
  {-Returns a formatted string with digits from L merged into the Mask}

  {-------- General purpose string manipulation --------}

function UpCaseMac(Ch : Char) : Char;
  {-Uppercase character macro, no international character support}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {POP  AX}
    $3C/$61/                 {CMP    AL,'a'}
    $72/$06/                 {JB     No}
    $3C/$7A/                 {CMP    AL,'z'}
    $77/$02/                 {JA     No}
    $24/$DF);                {AND    AL,$DF}
                             {No:}
  {$ENDIF}

function LoCaseMac(Ch : Char) : Char;
  {-Lowercase character macro, no international character support}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {POP  AX}
    $3C/$41/                 {CMP    AL,'A'}
    $72/$06/                 {JB     No}
    $3C/$5A/                 {CMP    AL,'Z'}
    $77/$02/                 {JA     No}
    $0C/$20);                {OR     AL,$20}
                             {No:}
  {$ENDIF}

function Upcase(Ch : Char) : Char;
  {-Return uppercase of char, with international character support}

procedure SetInternationalUpcase; {!!.02}
  {-Activate DOS international uppercase function for current code page}

procedure SetDefaultUpcase; {!!.02}
  {-Activate default uppercase function}

function StUpcase(S : string) : string;
  {-Convert lower case letters in string to uppercase, with intl chars}

function LoCase(Ch : Char) : Char;
  {-Return lowercase of char, with international character support}

function StLocase(S : string) : string;
  {-Convert upper case letters in string to lowercase, with intl chars}

function CharStr(Ch : Char; Len : Byte) : string;
  {-Return a string of length len filled with ch}

function PadCh(S : string; Ch : Char; Len : Byte) : string;
  {-Return a string right-padded to length len with ch}

function Pad(S : string; Len : Byte) : string;
  {-Return a string right-padded to length len with blanks}

function LeftPadCh(S : string; Ch : Char; Len : Byte) : string;
  {-Return a string left-padded to length len with ch}

function LeftPad(S : string; Len : Byte) : string;
  {-Return a string left-padded to length len with blanks}

function TrimLead(S : string) : string;
  {-Return a string with leading white space removed}

function TrimTrail(S : string) : string;
  {-Return a string with trailing white space removed}

function Trim(S : string) : string;
  {-Return a string with leading and trailing white space removed}

function TrimSpaces(S : string) : string;
  {-Return a string with leading and trailing spaces removed}

function CenterCh(S : string; Ch : Char; Width : Byte) : string;
  {-Return a string centered in a string of Ch with specified width}

function Center(S : string; Width : Byte) : string;
  {-Return a string centered in a blank string of specified width}

function Entab(S : string; TabSize : Byte) : string;
  {-Convert blanks in a string to tabs on spacing TabSize}

function Detab(S : string; TabSize : Byte) : string;
  {-Expand tabs in a string to blanks on spacing TabSize}

procedure MoveFast(var Src, Dest; Count : Word); {!!.01}
  {-Move Count bytes from Src to Dest, moving by word where possible}

function Substitute(S, FromStr, ToStr : String) : String; {!!.02}
  {-Return string S after mapping characters found in FromStr to the
    corresponding ones in ToStr}

function Filter(S : String; CS : CharSet) : String; {!!.11}
  {-Return string S after removing all characters in CS from it}

  {--------------- Word manipulation -------------------------------}

function WordCount(S : string; WordDelims : CharSet) : Byte;
  {-Given a set of word delimiters, return number of words in S}

function WordPosition(N : Byte; S : string; WordDelims : CharSet) : Byte;
  {-Given a set of word delimiters, return start position of N'th word in S}

function ExtractWord(N : Byte; S : string; WordDelims : CharSet) : string;
  {-Given a set of word delimiters, return the N'th word in S}

function AsciiCount(S : string; WordDelims : CharSet; Quote : Char) : Byte;
  {!!.13-Given a set of word delimiters, return number of words in S}

function AsciiPosition(N : Byte; S : string; WordDelims : CharSet; Quote : Char) : Byte;
  {!!.13-Given a set of word delimiters, return start position of N'th word in S}

function ExtractAscii(N : Byte; S : string; WordDelims : CharSet; Quote : Char) : string;
  {!!.13-Given a set of word delimiters, return the N'th word in S}

procedure WordWrap(InSt : string; var OutSt, Overlap : string;
                   Margin : Byte; PadToMargin : Boolean);
 {-Wrap InSt at Margin, storing the result in OutSt and the remainder in
   Overlap}

  {--------------- String comparison and searching -----------------}

function CompString(S1, S2 : string) : CompareType;
  {-Return less, equal, greater if s1<s2, s1=s2, or s1>s2}

function CompUCString(S1, S2 : string) : CompareType;
  {-Compare two strings in a case insensitive manner}

function CompStruct(var S1, S2; Size : Word) : CompareType;
  {-Compare two fixed size structures}

function Search(var Buffer; BufLength : Word;
                var Match; MatLength : Word) : Word;
 {-Search through Buffer for Match. BufLength is length of range to search.
   MatLength is length of string to match. Returns number of bytes searched
   to find Match, $FFFF if not found.}

function SearchUC(var Buffer; BufLength : Word;
                  var Match; MatLength : Word) : Word;
 {-Search through Buffer for Match, CASE-INSENSITIVE.
   Otherwise same as Search.}

{!!.30 - New}
function Replace(var Buffer; BufLength : Word;
                 var Match; MatLength : Word;
                 var Repl; RepLength : Word;
                 var NewLength : Word;
                 MaxLength : Word; All : Boolean) : Word;
  {-Replace all occurrences of 'Match' in Buffer with the data
    in Repl }

{!!.30 - New}
function ReplaceUC(var Buffer; BufLength : Word;
                   var Match; MatLength : Word;
                   var Repl; RepLength : Word;
                   var NewLength : Word;
                   MaxLength : Word; All : Boolean) : Word;
  {-Replace all occurrences of 'Match' in Buffer with the data
    in Repl--case INsensitive }

procedure BMMakeTable(MatchString : string; var BT : BTable);
  {-Build Boyer-Moore link table}

function BMSearch(var Buffer; BufLength : Word;
                  BT : BTable; MatchString : string) : Word;
  {-Search Buffer for MatchString. BufLength is length of range to search.
    Returns number of bytes searched to find MatchString, $FFFF if not found}

function BMSearchUC(var Buffer; BufLength : Word;
                    BT : BTable; MatchString : string) : Word;
 {-Search Buffer for MatchString, CASE-INSENSITIVE.
   Assumes MatchString is already in uppercase.
   Otherwise same as BMSearch}

function Soundex(S : string) : string;
  {-Return 4 character soundex of input string}

function MakeLetterSet(S : string) : LongInt;
  {-Return a bit-mapped long storing the individual letters contained in S}

function CompareLetterSets(Set1, Set2 : LongInt) : Word;
  {-Returns the sum of the values of the letters common to Set1 and Set2}

  {----- Strings dynamically allocated on the heap ------}

function StringToHeap(S : string) : StringPtr;
  {-Allocate space for s and return pointer}

function StringFromHeap(P : StringPtr) : string;
  {-Return string at p}

procedure DisposeString(P : StringPtr);
  {-Deallocate space for string at p}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtension(Name : string; Ext : ExtStr) : string;
  {-Return a file name with a default extension attached}

function ForceExtension(Name : string; Ext : ExtStr) : string;
  {-Force the specified extension onto the file name}

function JustFilename(PathName : string) : string;
  {-Return just the filename and extension of a pathname}

function JustName(PathName : string) : string;
  {-Return just the name (no extension, no path) of a pathname}

function JustExtension(Name : string) : ExtStr;
  {-Return just the extension of a pathname}

function JustPathname(PathName : string) : string;
  {-Return just the drive:directory portion of a pathname}

function AddBackSlash(DirName : string) : string;
  {-Add a default backslash to a directory name}

function CleanPathName(PathName : string) : string;
  {-Return a pathname cleaned up as DOS will do it}

function FullPathName(FName : string) : string;
  {-Given FName (known to exist), return a full pathname}

  {the following procedures are for internal use only}

{.Z+}
procedure UpCasePrim;
procedure LoCasePrim;
{.Z-}

  {==========================================================================}

implementation

type
  OS =
    record
      O, S : Word;
    end;
  Long =
    record
      LowWord, HighWord : SmallWord;
    end;
const
  Digits : array[0..$F] of Char = '0123456789ABCDEF';
const
  DosDelimSet : set of Char = ['\', ':', #0];
{const}                         {!!.21}
  {AssumeDP : Boolean = False;} {!!.21}

{$IFDEF VIRTUALPASCAL}
  {$L VPTAB.OBJ}
  {$L VPCOMP.OBJ}
  {$L VPCASE.OBJ}
  {$L VPSEARCH.OBJ}
  {$L VPBM.OBJ}
{$ELSE}
  {$L OPCASE.OBJ}
  {$L OPCOMP.OBJ}
  {$L OPSEARCH.OBJ}
  {$L OPTAB.OBJ}
  {$L OPBM.OBJ}
{$ENDIF}

  function HexB(B : Byte) : string;
    {-Return hex string for byte}
  begin
    HexB[0] := #2;
    HexB[1] := Digits[B shr 4];
    HexB[2] := Digits[B and $F];
  end;

  function HexW(W : Word) : string;
    {-Return hex string for word}
  begin
    HexW[0] := #4;
    HexW[1] := Digits[hi(W) shr 4];
    HexW[2] := Digits[hi(W) and $F];
    HexW[3] := Digits[lo(W) shr 4];
    HexW[4] := Digits[lo(W) and $F];
  end;

  function HexL(L : LongInt) : string;
    {-Return hex string for LongInt}
  begin
    with Long(L) do
      HexL := HexW(HighWord)+HexW(LowWord);
  end;

  function HexPtr(P : Pointer) : string;
    {-Return hex string for pointer}
  begin
    {HexPtr := HexW(Seg(P^))+':'+HexW(Ofs(P^));}   {!!.20}
    {$IFDEF VIRTUALPASCAL}
    HexPtr := HexL( Longint( P ) );
    {$ELSE}
    HexPtr := HexW(OS(P).S)+':'+HexW(OS(P).O);     {!!.20}
    {$ENDIF}
  end;

  function BinaryB(B : Byte) : string;
    {-Return binary string for byte}
  var
    I, N : Word;
  begin
    N := 1;
    BinaryB[0] := #8;
    for I := 7 downto 0 do begin
      BinaryB[N] := Digits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
      Inc(N);
    end;
  end;

  function BinaryW(W : Word) : string;
    {-Return binary string for word}
  var
    I, N : Word;
  begin
    N := 1;
    BinaryW[0] := #16;
    for I := 15 downto 0 do begin
      BinaryW[N] := Digits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
      Inc(N);
    end;
  end;

  function BinaryL(L : LongInt) : string;
    {-Return binary string for LongInt}
  var
    I : LongInt;
    N : Byte;
  begin
    N := 1;
    BinaryL[0] := #32;
    for I := 31 downto 0 do begin
      BinaryL[N] := Digits[Ord(L and LongInt(1 shl I) <> 0)]; {0 or 1}
      Inc(N);
    end;
  end;

  function OctalB(B : Byte) : string;
    {-Return octal string for byte}
  var
    I : Word;
  begin
    OctalB[0] := #3;
    for I := 0 to 2 do begin
      OctalB[3-I] := Digits[B and 7];
      B := B shr 3;
    end;
  end;

  function OctalW(W : Word) : string;
    {-Return octal string for word}
  var
    I : Word;
  begin
    OctalW[0] := #6;
    for I := 0 to 5 do begin
      OctalW[6-I] := Digits[W and 7];
      W := W shr 3;
    end;
  end;

  function OctalL(L : LongInt) : string;
    {-Return octal string for word}
  var
    I : Word;
  begin
    OctalL[0] := #12;
    for I := 0 to 11 do begin
      OctalL[12-I] := Digits[L and 7];
      L := L shr 3;
    end;
  end;

  function Str2Int(S : string; var I : Integer) : Boolean;
    {-Convert a string to an integer, returning true if successful}
  var
    code : Word;
    SLen : Byte absolute S;
  begin
    while S[SLen] = ' ' do
      Dec(SLen);
    if (SLen > 1) and (Upcase(S[SLen]) = 'H') then begin          {!!.21 begin}
      Move(S[1], S[2], SLen-1);
      S[1] := '$';
    end
    else if (SLen > 2) and (S[1] = '0') and (Upcase(S[2]) = 'X') then begin
      Dec(SLen);
      Move(S[3], S[2], SLen-1);
      S[1] := '$';
    end;                                                          {!!.21 end}
    Val(S, I, code);
    if code <> 0 then begin
      I := code;
      Str2Int := False;
    end else
      Str2Int := True;
  end;

  function Str2Word(S : string; var I : Word) : Boolean;
    {-Convert a string to a word, returning true if successful}
  var
    code : Word;
    SLen : Byte absolute S;
  begin
    while S[SLen] = ' ' do
      Dec(SLen);
    if (SLen > 1) and (Upcase(S[SLen]) = 'H') then begin          {!!.21 begin}
      Move(S[1], S[2], SLen-1);
      S[1] := '$';
    end
    else if (SLen > 2) and (S[1] = '0') and (Upcase(S[2]) = 'X') then begin
      Dec(SLen);
      Move(S[3], S[2], SLen-1);
      S[1] := '$';
    end;                                                          {!!.21 end}
    Val(S, I, code);
    if code <> 0 then begin
      I := code;
      Str2Word := False;
    end else
      Str2Word := True;
  end;

  function Str2Long(S : string; var I : LongInt) : Boolean;
    {-Convert a string to a longint, returning true if successful}
  var
    code : Word;
    SLen : Byte absolute S;
  begin
    while S[SLen] = ' ' do
      Dec(SLen);
    if (SLen > 1) and (Upcase(S[SLen]) = 'H') then begin          {!!.21 begin}
      Move(S[1], S[2], SLen-1);
      S[1] := '$';
    end
    else if (SLen > 2) and (S[1] = '0') and (Upcase(S[2]) = 'X') then begin
      Dec(SLen);
      Move(S[3], S[2], SLen-1);
      S[1] := '$';
    end;                                                          {!!.21 end}
    Val(S, I, code);
    if code <> 0 then begin
      I := code;
      Str2Long := False;
    end else
      Str2Long := True;
  end;

  function Str2Real(S : string; var R : Float) : Boolean;
    {-Convert a string to a real, returning true if successful}
  var
    Code, I : Word;
    SLen : Byte absolute S;
  begin
    {trim trailing blanks}
    while S[SLen] = ' ' do
      Dec(SLen);

    Val(S, R, Code);
    if Code <> 0 then begin
      R := Code;
      Str2Real := False;
    end else
      Str2Real := True;
  end;

  function Long2Str(L : LongInt) : string;
    {-Convert a long/word/integer/byte/shortint to a string}
  var
    S : string;
  begin
    Str(L, S);
    Long2Str := S;
  end;

  function Real2Str(R : Float; Width : Byte; Places : ShortInt) : string;
    {-Convert a real to a string}
  var
    S : string;
  begin
    Str(R:Width:Places, S);
    Real2Str := S;
  end;

  function FormPrim(Mask : string; R : Float; AssumeDP : Boolean) : string; {!!.21}
    {-Returns a formatted string with digits from R merged into the Mask}
  type
    FillType = (Blank, Asterisk, Zero);
  const
  {$IFOPT N+}          {!!.22}
    MaxPlaces = 18;    {!!.22}
  {$ELSE}              {!!.22}
    MaxPlaces = 11;    {!!.22}
  {$ENDIF}             {!!.22}
    FormChars : string[8] = '#@*$-+,.';
    PlusArray : array[Boolean] of Char = ('+', '-');
    MinusArray : array[Boolean] of Char = (' ', '-');
    FillArray : array[FillType] of Char = (' ', '*', '0');
  var
    S : string;              {temporary string}
    Filler : FillType;       {char for unused digit slots: ' ', '*', '0'}
    WontFit,                 {true if number won't fit in the mask}
    AddMinus,                {true if minus sign needs to be added}
    Dollar,                  {true if floating dollar sign is desired}
    Negative : Boolean;      {true if B is negative}
    StartF,                  {starting point of the numeric field}
    EndF : Word;             {end of numeric field}
    DotPos,                  {position of '.' in Mask}
    Digits,                  {total # of digits}
    Places,                  {# of digits after the '.'}
    Blanks,                  {# of blanks returned by Str}
    FirstDigit,              {pos. of first digit returned by Str}
    Extras,                  {# of extra digits needed for special cases}
    DigitPtr : Byte;         {pointer into temporary string of digits}
    I : Word;
  label
    EndFound,
    RedoCase,
    Done;
  begin
    {assume decimal point at end?}
    if AssumeDP and (Mask <> '') and (Length(Mask) < 255) then begin
      Inc(Mask[0]);
      Mask[Length(Mask)] := '.';
    end;                      {!!.21}
    {else}                    {!!.21}
      {AssumeDP := False;}    {!!.21}

    {check for empty string}
    if Length(Mask) = 0 then
      goto Done;

    {initialize variables}
    Filler := Blank;
    DotPos := 0;
    Places := 0;
    Digits := 0;
    Dollar := False;
    AddMinus := True;
    StartF := 1;

    {store the sign of the real and make it positive}
    Negative := (R < 0);
    R := Abs(R);

    {find the starting point for the field}
    while (StartF <= Length(Mask)) and (Pos(Mask[StartF], FormChars) = 0) do
      Inc(StartF);
    if StartF > Length(Mask) then
      goto Done;

    {find the end point for the field}
    for EndF := StartF to Length(Mask) do
      case Mask[EndF] of
        '*' : Filler := Asterisk;
        '@' : Filler := Zero;
        '$' : Dollar := True;
        '-',
        '+' : AddMinus := False;
        '#' : {ignore} ;
        ',' :
          begin
            DotPos := EndF;
            if CommaForPeriod then
              Mask[EndF] := '.';
          end;
        '.' :
          begin
            DotPos := EndF;
            if CommaForPeriod then
              Mask[EndF] := ',';
          end;
      else
        goto EndFound;
      end;

    {if we get here at all, the last char was part of the field}
    Inc(EndF);

EndFound:
    {if we jumped to here instead, it wasn't}
    Dec(EndF);

    {disallow Dollar if Filler is Zero}
    if Filler = Zero then
      Dollar := False;

    {we need an extra slot if Dollar is True}
    Extras := Ord(Dollar);

    {get total # of digits and # after the decimal point}
    for I := StartF to EndF do
      case Mask[I] of
        '#', '@',
        '*', '$' :
          begin
            Inc(Digits);
            if (I > DotPos) and (DotPos <> 0) then
              Inc(Places);
          end;
      end;

    {need one more 'digit' if Places > 0}
    Inc(Digits, Ord(Places > 0));

    {also need an extra blank if (1) Negative is true, and (2) Filler is Blank,
     and (3) AddMinus is true}
    if Negative and AddMinus and (Filler = Blank) then
      Inc(Extras)
    else
      AddMinus := False;

    {translate the real to a string}
    Str(R:Digits:Places, S);

    {add zeros that Str may have left out}               {!!.22}
    if Places > MaxPlaces then begin                     {!!.22}
      FillChar(S[Length(S)+1], Places-MaxPlaces, '0');   {!!.22}
      inc(S[0], Places-MaxPlaces);                       {!!.22}
      while (Length(S) > Digits) and (S[1] = ' ') do     {!!.22}
        Delete(S, 1, 1);                                 {!!.22}
    end;                                                 {!!.22}

    {count number of initial blanks}
    Blanks := 1;
    while S[Blanks] = ' ' do
      Inc(Blanks);
    FirstDigit := Blanks;
    Dec(Blanks);

    {the number won't fit if (a) S is longer than Digits or (b) the number of
     initial blanks is less than Extras}
    WontFit := (Length(S) > Digits) or (Blanks < Extras);

    {if it won't fit, fill decimal slots with '*'}
    if WontFit then begin
      for I := StartF to EndF do
        case Mask[I] of
          '#', '@', '*', '$' : Mask[I] := '*';
          '+' : Mask[I] := PlusArray[Negative];
          '-' : Mask[I] := MinusArray[Negative];
        end;
      goto Done;
    end;

    {fill initial blanks in S with Filler; insert floating dollar sign}
    if Blanks > 0 then begin
      FillChar(S[1], Blanks, FillArray[Filler]);

      {put floating dollar sign in last blank slot if necessary}
      if Dollar then begin
        S[Blanks] := MoneySign;
        Dec(Extras);
        Dec(Blanks);
      end;

      {insert a minus sign if necessary}
      if AddMinus then
        S[Blanks] := '-';
    end;

    {put in the digits / signs}
    DigitPtr := Length(S);
    for I := EndF downto StartF do begin
RedoCase:
      case Mask[I] of
        '#', '@', '*', '$' :
          if DigitPtr <> 0 then begin
            Mask[I] := S[DigitPtr];
            Dec(DigitPtr);
            if (S[DigitPtr] = '.') and (DigitPtr <> 0) then
              Dec(DigitPtr);
          end
          else
            Mask[I] := FillArray[Filler];
        ',', '.' :
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Mask[I] := '#';
            goto RedoCase;
          end;
        '+' : Mask[I] := PlusArray[Negative];
        '-' : Mask[I] := MinusArray[Negative];
      end;
    end;

Done:
    if AssumeDP then
      Dec(Mask[0]);
    FormPrim := Mask; {!!.21}
  end;

  function LongIntForm(Mask : string; L : LongInt) : string;
    {-Returns a formatted string with digits from L merged into the Mask}
  begin
    {!!.21 AssumeDP now passed to FormPrim}
    LongIntForm := FormPrim(Mask, L, True);
  end;

  function Form(Mask : string; R : Float) : string;
  begin
    {!!.21 guts moved to FormPrim}
    Form := FormPrim(Mask, R, False);
  end;

  {In OPCASE.OBJ}
  procedure UpcasePrim; external;
  procedure LocasePrim; external;
  function Upcase(Ch : Char) : Char; external;
  function LoCase(Ch : Char) : Char; external;
  function StUpcase(S : string) : string; external;
  function StLocase(S : string) : string; external;

  {$IFDEF VIRTUALPASCAL}
  procedure SetInternationalUpcasePrim;
  var
    B : Byte;
  begin
    for B := 128 to 165 do
      UCTable[B] := Chr(B);
    SysGetCaseMap(Sizeof(UCTable), @UCTable);
  end;

  procedure MoveFast(var Src, Dest; Count : Word);
    begin
      { No gain in rewriting this - already optimised in SYSTEM }
      move( Src, Dest, Count );
    end;

  {$ELSE}
    procedure SetInternationalUpcasePrim; external; {!!.02}
    procedure MoveFast(var Src, Dest; Count : Word); external;
  {$ENDIF VIRTUALPASCAL}

  function CharStr(Ch : Char; Len : Byte) : string;
    {-Return a string of length len filled with ch}
  var
    S : string;
  begin
    if Len = 0 then
      CharStr[0] := #0
    else begin
      S[0] := Chr(Len);
      FillChar(S[1], Len, Ch);
      CharStr := S;
    end;
  end;

  function PadCh(S : string; Ch : Char; Len : Byte) : string;
    {-Return a string right-padded to length len with ch}
  var
    o : string;
    SLen : Byte absolute S;
  begin
    if Length(S) >= Len then
      PadCh := S
    else begin
      o[0] := Chr(Len);
      MoveFast(S[1], o[1], SLen); {!!.01}
      if SLen < 255 then
        FillChar(o[Succ(SLen)], Len-SLen, Ch);
      PadCh := o;
    end;
  end;

  function Pad(S : string; Len : Byte) : string;
    {-Return a string right-padded to length len with blanks}
  begin
    Pad := PadCh(S, ' ', Len);
  end;

  function LeftPadCh(S : string; Ch : Char; Len : Byte) : string;
    {-Return a string left-padded to length len with ch}
  var
    o : string;
    SLen : Byte absolute S;
  begin
    if Length(S) >= Len then
      LeftPadCh := S
    else if SLen < 255 then begin
      o[0] := Chr(Len);
      MoveFast(S[1], o[Succ(Word(Len))-SLen], SLen); {!!.01}
      FillChar(o[1], Len-SLen, Ch);
      LeftPadCh := o;
    end;
  end;

  function LeftPad(S : string; Len : Byte) : string;
    {-Return a string left-padded to length len with blanks}
  begin
    LeftPad := LeftPadCh(S, ' ', Len);
  end;

  function TrimLead(S : string) : string;
    {-Return a string with leading white space removed}
  var
    I : Word;
  begin
    I := 1;
    while (I <= Length(S)) and (S[I] <= ' ') do
      Inc(I);
    Dec(I);
    if I > 0 then
      Delete(S, 1, I);
    TrimLead := S;
  end;

  function TrimTrail(S : string) : string;
    {-Return a string with trailing white space removed}
  var
    SLen : Byte absolute S;
  begin
    while (SLen > 0) and (S[SLen] <= ' ') do
      Dec(SLen);
    TrimTrail := S;
  end;

  function Trim(S : string) : string;
    {-Return a string with leading and trailing white space removed}
  var
    I : Word;
    SLen : Byte absolute S;
  begin
    while (SLen > 0) and (S[SLen] <= ' ') do
      Dec(SLen);

    I := 1;
    while (I <= SLen) and (S[I] <= ' ') do
      Inc(I);
    Dec(I);
    if I > 0 then
      Delete(S, 1, I);

    Trim := S;
  end;

  function TrimSpaces(S : string) : string;
    {-Return a string with leading and trailing spaces removed}
  var
    I : Word;
    SLen : Byte absolute S;
  begin
    while (SLen > 0) and (S[SLen] = ' ') do
      Dec(SLen);

    I := 1;
    while (I <= SLen) and (S[I] = ' ') do
      Inc(I);
    Dec(I);
    if I > 0 then
      Delete(S, 1, I);

    TrimSpaces := S;
  end;

  function CenterCh(S : string; Ch : Char; Width : Byte) : string;
    {-Return a string centered in a string of Ch with specified width}
  var
    o : string;
    SLen : Byte absolute S;
  begin
    if SLen >= Width then
      CenterCh := S
    else if SLen < 255 then begin
      o[0] := Chr(Width);
      FillChar(o[1], Width, Ch);
      MoveFast(S[1], o[Succ((Width-SLen) shr 1)], SLen); {!!.01}
      CenterCh := o;
    end;
  end;

  function Center(S : string; Width : Byte) : string;
    {-Return a string centered in a blank string of specified width}
  begin
    Center := CenterCh(S, ' ', Width);
  end;

  function Substitute(S, FromStr, ToStr : String) : String; {!!.02}
    {-Return string S after mapping characters found in FromStr to the
      corresponding ones in ToStr}
  var
    I : Byte;
    P : Byte;
  begin
    if Length(FromStr) = Length(ToStr) then
      for I := 1 to Length(S) do begin
        P := Pos(S[I], FromStr);
        if P <> 0 then
          S[I] := ToStr[P];
      end;
    Substitute := S;
  end;

  function Filter(S : String; CS : CharSet) : String; {!!.11}
    {-Return string S after removing all characters in CS from it}
  var
    I : Word;
    Len : Word;
  begin
    Len := 0;
    for I := 1 to Length(S) do
      if not(S[I] in CS) then begin
        inc(Len);
        Filter[Len] := S[I];
      end;
    Filter[0] := Char(Len);
  end;

  function WordCount(S : string; WordDelims : CharSet) : Byte;
    {-Given a set of word delimiters, return number of words in S}
  var
    {I,} Count : Byte;         {!!.02}
    I : Word;                  {!!.02}
    SLen : Byte absolute S;
  begin
    Count := 0;
    I := 1;

    while I <= SLen do begin
      {skip over delimiters}
      while (I <= SLen) and (S[I] in WordDelims) do
        Inc(I);

      {if we're not beyond end of S, we're at the start of a word}
      if I <= SLen then
        Inc(Count);

      {find the end of the current word}
      while (I <= SLen) and not(S[I] in WordDelims) do
        Inc(I);
    end;

    WordCount := Count;
  end;

  function WordPosition(N : Byte; S : string; WordDelims : CharSet) : Byte;
    {-Given a set of word delimiters, return start position of N'th word in S}
  var
    {I,} Count : Byte;         {!!.02}
    I : Word;                  {!!.02}
    SLen : Byte absolute S;
  begin
    Count := 0;
    I := 1;
    WordPosition := 0;

    while (I <= SLen) and (Count <> N) do begin
      {skip over delimiters}
      while (I <= SLen) and (S[I] in WordDelims) do
        Inc(I);

      {if we're not beyond end of S, we're at the start of a word}
      if I <= SLen then
        Inc(Count);

      {if not finished, find the end of the current word}
      if Count <> N then
        while (I <= SLen) and not(S[I] in WordDelims) do
          Inc(I)
      else
        WordPosition := I;
    end;
  end;

  function ExtractWord(N : Byte; S : string; WordDelims : CharSet) : string;
    {-Given a set of word delimiters, return the N'th word in S}
  var
    I : Word; {!!.12}
    Len : Byte;
    SLen : Byte absolute S;
  begin
    Len := 0;
    I := WordPosition(N, S, WordDelims);
    if I <> 0 then
      {find the end of the current word}
      while (I <= SLen) and not(S[I] in WordDelims) do begin
        {add the I'th character to result}
        Inc(Len);
        ExtractWord[Len] := S[I];
        Inc(I);
      end;
    ExtractWord[0] := Char(Len);
  end;

  function AsciiCount(S : string; WordDelims : CharSet; Quote : Char) : Byte;
    {!!.13-Given a set of word delimiters, return number of words in S}
  var
    {I,} Count : Byte;         {!!.02}
    I : Word;                  {!!.02}
    InQuote : Boolean;         {!!.13}
    SLen : Byte absolute S;
  begin
    Count := 0;
    I := 1;
    InQuote := False;

    while I <= SLen do begin
      {skip over delimiters}
      while (I <= SLen) and (S[i] <> Quote) and (S[I] in WordDelims) do begin
        if I = 1 then
          Inc(Count);
        Inc(I);
      end;

      {if we're not beyond end of S, we're at the start of a word}
      if I <= SLen then
        Inc(Count);

      {find the end of the current word}
      while (I <= SLen) and ((InQuote) or (not(S[I] in WordDelims))) do begin
        if S[I] = Quote then
          InQuote := not(InQuote);
        Inc(I);
      end;
    end;

    AsciiCount := Count;
  end;

  function AsciiPosition(N : Byte; S : string; WordDelims : CharSet; Quote : Char) : Byte;
    {!!.13-Given a set of word delimiters, return start position of N'th word in S}
  var
    {I,} Count : Byte;         {!!.02}
    I : Word;                  {!!.02}
    InQuote : Boolean;         {!!.13}
    SLen : Byte absolute S;
  begin
    Count := 0;
    I := 1;
    InQuote := False;
    AsciiPosition := 0;

    while (I <= SLen) and (Count <> N) do begin
      {skip over delimiters}
      if (I <= SLen) and (S[I] <> Quote) and (S[I] in WordDelims) then begin
        if I = 1 then
          Inc(Count);
        Inc(I);
      end;

      {if we're not beyond end of S, we're at the start of a word}
      if I <= SLen then
        Inc(Count);

      {if not finished, find the end of the current word}
      if Count <> N then
        while (I <= SLen) and ((InQuote) or (not(S[I] in WordDelims))) do begin
          if S[I] = Quote then
            InQuote := not(InQuote);
          Inc(I);
        end
      else
        AsciiPosition := I;
    end;
  end;

  function ExtractAscii(N : Byte; S : string; WordDelims : CharSet; Quote : Char) : string;
    {!!.13-Given a set of word delimiters, return the N'th word in S}
  var
    I : Word; {!!.12}
    Len : Byte;
    SLen : Byte absolute S;
    InQuote : Boolean;
  begin
    Len := 0;
    InQuote := False;
    I := AsciiPosition(N, S, WordDelims, Quote);
    if I <> 0 then
      {find the end of the current word}
      while (I <= SLen) and ((InQuote) or (not(S[I] in WordDelims))) do begin
        {add the I'th character to result}
        Inc(Len);
        if S[I] = Quote then
          InQuote := Not(InQuote);
        ExtractAscii[Len] := S[I];
        Inc(I);
      end;
    ExtractAscii[0] := Char(Len);
  end;

  procedure WordWrap(InSt : string; var OutSt, Overlap : string;
                     Margin : Byte; PadToMargin : Boolean);
    {-Wrap InSt at Margin, storing the result in OutSt and the remainder
      in Overlap}
  var
    InStLen : Byte absolute InSt;
    OutStLen : Byte absolute OutSt;
    OvrLen : Byte absolute Overlap;
    EOS, BOS : Word;
  begin
    {find the end of the output string}
    if InStLen > Margin then begin
      {find the end of the word at the margin, if any}
      EOS := Margin;
      while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
        Inc(EOS);
      if EOS > InStLen then
        EOS := InStLen;

      {trim trailing blanks}
      while (InSt[EOS] = ' ') and (EOS > 0) do
        Dec(EOS);

      if EOS > Margin then begin
        {look for the space before the current word}
        while (EOS > 0) and (InSt[EOS] <> ' ') do
          Dec(EOS);

        {if EOS = 0 then we can't wrap it}
        if EOS = 0 then
          EOS := Margin
        else
          {trim trailing blanks}
          while (InSt[EOS] = ' ') and (EOS > 0) do
            Dec(EOS);
      end;
    end
    else
      EOS := InStLen;

    {copy the unwrapped portion of the line}
    OutStLen := EOS;
    MoveFast(InSt[1], OutSt[1], OutStLen); {!!.01}

    {find the start of the next word in the line}
    BOS := EOS+1;
    while (BOS <= InStLen) and (InSt[BOS] = ' ') do
      Inc(BOS);

    if BOS > InStLen then
      OvrLen := 0
    else begin
      {copy from the start of the next word to the end of the line}
      OvrLen := Succ(InStLen-BOS);
      MoveFast(InSt[BOS], Overlap[1], OvrLen); {!!.01}
    end;

    {pad the end of the output string if requested}
    if PadToMargin and (OutStLen < Margin) then begin
      FillChar(OutSt[OutStLen+1], Margin-OutStLen, ' ');
      OutStLen := Margin;
    end;
  end;

  {In OPTAB.OBJ or VPTAB.OBJ}
  function Entab(S : string; TabSize : Byte) : string; external;
  function Detab(S : string; TabSize : Byte) : string; external;

  {In OPCOMP.OBJ or VPCOMP.OBJ}
  function CompString(S1, S2 : string) : CompareType; external;
  function CompUCString(S1, S2 : string) : CompareType; external;
  function CompStruct(var S1, S2; Size : Word) : CompareType; external;
  function Soundex(S : string) : string; external;
  function MakeLetterSet(S : string) : LongInt; external;
  function CompareLetterSets(Set1, Set2 : LongInt) : Word; external;

  {In OPSEARCH.OBJ}
  function  Search(var Buffer; BufLength : Word; var Match; MatLength : Word) : Word; external;
  function  SearchUC(var Buffer; BufLength : Word; var Match; MatLength : Word) : Word; external;
  procedure ReplacePrim(var Buffer; BufLength, Posn, SrcLen : Word;
                        var Repl  ; RepLength : Word); external;

  {In OPBM.OBJ}
  procedure BMMakeTable(MatchString : string; var BT : BTable); external;
  function BMSearch(var Buffer; BufLength : Word; BT : BTable; MatchString : string) : Word; external;
  function BMSearchUC(var Buffer; BufLength : Word; BT : BTable; MatchString : string) : Word; external;

  function StringToHeap(S : string) : StringPtr;
    {-Allocate space for s and return pointer}
  var
    L : Word;
    P : Pointer;
  begin
    L := Succ(Word(Length(S)));
    if MaxAvail < L then
      StringToHeap := nil
    else begin
      GetMem(P, L);
      MoveFast(S, P^, L); {!!.01}
      StringToHeap := P;
    end;
  end;

  function StringFromHeap(P : StringPtr) : string;
    {-Return string at p}
  begin
    if P = nil then
      StringFromHeap[0] := #0
    else
      StringFromHeap := P^;
  end;

  procedure DisposeString(P : StringPtr);
    {-Deallocate space for string at p}
  begin
    if P <> nil then
      FreeMem(P, Succ(Word(Byte(P^[0]))));
  end;

  function HasExtension(Name : string; var DotPos : Word) : Boolean;
    {-Return whether and position of extension separator dot in a pathname}
  var
    I : Word;
  begin
    DotPos := 0;
    for I := Length(Name) downto 1 do
      if (Name[I] = '.') and (DotPos = 0) then
        DotPos := I;
    HasExtension := (DotPos > 0) and (Pos('\', Copy(Name, Succ(DotPos), 64)) = 0);
  end;

  function DefaultExtension(Name : string; Ext : ExtStr) : string;
    {-Return a pathname with the specified extension attached}
  var
    DotPos : Word;
  begin
    if HasExtension(Name, DotPos) then
      DefaultExtension := Name
    else if Name = '' then
      DefaultExtension := ''
    else
      DefaultExtension := Name+'.'+Ext;
  end;

 function ForceExtension(Name : string; Ext : ExtStr) : string;
    {-Return a pathname with the specified extension attached}
  var
    DotPos : Word;
  begin
    if HasExtension(Name, DotPos) then
      ForceExtension := Copy(Name, 1, DotPos)+Ext
    else if Name = '' then
      ForceExtension := ''
    else
      ForceExtension := Name+'.'+Ext;
  end;

  function JustExtension(Name : string) : ExtStr;
    {-Return just the extension of a pathname}
  var
    DotPos : Word;
  begin
    if HasExtension(Name, DotPos) then
      JustExtension := Copy(Name, Succ(DotPos), ExtLen)
    else
      JustExtension := '';
  end;

  function JustFilename(PathName : string) : string;
    {-Return just the filename of a pathname}
  var
    I : Word;
  begin
    I := Succ(Word(Length(PathName)));
    repeat
      Dec(I);
    until (PathName[I] in DosDelimSet) or (I = 0);
    JustFilename := Copy(PathName, Succ(I), 64);
  end;

  function JustName(PathName : string) : string;
    {-Return just the name (no extension, no path) of a pathname}
  var
    DotPos : Byte;
  begin
    PathName := JustFileName(PathName);
    DotPos := Pos('.', PathName);
    if DotPos > 0 then
      PathName := Copy(PathName, 1, DotPos-1);
    JustName := PathName;
  end;

  function JustPathname(PathName : string) : string;
    {-Return just the drive:directory portion of a pathname}
  var
    I : Word;
  begin
    I := Succ(Word(Length(PathName)));
    repeat
      Dec(I);
    until (PathName[I] in DosDelimSet) or (I = 0);

    if I = 0 then
      {Had no drive or directory name}
      JustPathname[0] := #0
    else if I = 1 then
      {Either the root directory of default drive or invalid pathname}
      JustPathname := PathName[1]
    else if (PathName[I] = '\') then begin
      if PathName[Pred(I)] = ':' then
        {Root directory of a drive, leave trailing backslash}
        JustPathname := Copy(PathName, 1, I)
      else
        {Subdirectory, remove the trailing backslash}
        JustPathname := Copy(PathName, 1, Pred(I));
    end else
      {Either the default directory of a drive or invalid pathname}
      JustPathname := Copy(PathName, 1, I);
  end;

  function AddBackSlash(DirName : string) : string;
    {-Add a default backslash to a directory name}
  begin
    if DirName[Length(DirName)] in DosDelimSet then
      AddBackSlash := DirName
    else
      AddBackSlash := DirName+'\';
  end;

  function CleanFileName(FileName : string) : string;
    {-Return filename with at most 8 chars of name and 3 of extension}
  var
    DotPos : Word;
    namelen : Word;
  begin
    if HasExtension(FileName, DotPos) then begin
      {Take the first 8 chars of name and first 3 chars of extension}
      namelen := Pred(DotPos);
      if namelen > 8 then
        namelen := 8;
      CleanFileName := Copy(FileName, 1, namelen)+Copy(FileName, DotPos, 4);
    end else
      {Take the first 8 chars of name}
      CleanFileName := Copy(FileName, 1, 8);
  end;

  function CleanPathName(PathName : string) : string;
    {-Return a pathname cleaned up as DOS will do it}
  var
    I : Word;
    oname : string;
  begin
    oname[0] := #0;

    {!!.03} {Remove repeated backslashes}
    I := Succ(Word(Length(PathName)));
    repeat
      dec(I);
      if I > 2 then
        if (PathName[I] = '\') and (PathName[I-1] = '\') then
          if (PathName[I-2] <> ':') then
            Delete(PathName, I, 1);
    until I <= 0;

    I := Succ(Word(Length(PathName)));
    repeat
      {Get the next directory or drive portion of pathname}
      repeat
        Dec(I);
      until (PathName[I] in DosDelimSet) or (I = 0);

      {Clean it up and prepend it to output string}
      oname := CleanFileName(Copy(PathName, Succ(I), 64))+oname;
      if I > 0 then begin
        oname := PathName[I]+oname;
        Delete(PathName, I, 255);
      end;
    until I <= 0;

    CleanPathName := oname;
  end;

  function FullPathName(FName : string) : string;
    {-Given FName (known to exist), return a full pathname}
  var
    CurDir : string[64];
    Cpos : Byte;
  begin
    Cpos := Pos(':', FName);
    if Cpos <> 0 then begin
      {Drive letter specified}
      if FName[Succ(Cpos)] = '\' then
        {Complete path already specified}
        FullPathName := FName
      else begin
        {Drive specified, but incomplete path}
        GetDir(Pos(Upcase(FName[1]), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'), CurDir);
        FullPathName := AddBackSlash(CurDir)+Copy(FName, Succ(Cpos), 100);
      end;
    end else begin
      {No drive specified}
      GetDir(0, CurDir);
      if FName[1] = '\' then
        {Complete path but no drive}
        FullPathName := Copy(CurDir, 1, 2)+FName
      else
        {No drive, incomplete path}
        FullPathName := AddBackSlash(CurDir)+FName;
    end;
  end;

  procedure SetDefaultUpcase;   {!!.20}
  begin
    UCTable := DefUCTable;
  end;

  function FixupUCChar(C : Char) : Char;   {!!.20}
{$IFDEF PMode}
  var
    DR : DpmiRegisters;
    W : Word;
  begin
    FillChar(DR, SizeOf(DR), 0);
    with DR do begin
  {$IFDEF Windows}
      drAX := Word(C);
      drCS := OS(UpcaseFunc).S;
      drIP := OS(UpcaseFunc).O;
      W := CallFarRealModeProc(0, nil, DR);
      FixupUCChar := Chr(drAX);
  {$ELSE}
      AX := Word(C);
      CS := OS(UpcaseFunc).S;
      IP := OS(UpcaseFunc).O;
      W := CallFarRealModeProc(0, nil, DR);
      FixupUCChar := Chr(AX);
  {$ENDIF}
    end;
{$ELSE}
  {$IFDEF VIRTUALPASCAL}
  begin
    asm
      mov al,Byte Ptr C
    end;
  {$ELSE}
  begin
    Inline(
      $8A/$46/<C/            {mov al,[bp+<C]}
      $FF/$1E/>UpcaseFunc/   {call dword ptr [>UpcaseFunc]}
      $88/$46/<FixupUCChar); {mov [bp+<FixupUCChar],al}
  {$ENDIF}
{$ENDIF}
  end;

{$IFDEF PMode}
  procedure SetInternationalUpcasePrimPM;
  label
    Skip;
  type
    HiLo = record
      LoWord, HiWord : Word;
    end;
  var
    R : DPMIRegisters;
    L : LongInt;
  begin
    FillChar(R, SizeOf(R), 0);
{$IFDEF Windows}
    R.drAX := $3000;
    if SimulateRealModeInt($21, R) <> 0 then exit;
    {R.drAX := R.drAX or $00FF;}       {!!.22}
    {if R.drAX >= 2 then exit;}        {!!.22}
    if lo(R.drAX) < 3 then exit;       {!!.22}

    L := GlobalDosAlloc(34);
    R.drAX := $3800;
    R.drDX := 0;
    R.drDS := HiLo(L).HiWord;
    if SimulateRealModeInt($21, R) <> 0 then goto Skip;
    Move(Ptr(HiLo(L).LoWord, $12)^, Pointer(UpcaseFunc), 4);
{$ELSE}
    R.ax := $3000;
    if SimulateRealModeInt($21, R) <> 0 then exit;
    {R.ax := R.ax or $00FF;}           {!!.22}
    {if R.ax >= 2 then exit;}          {!!.22}
    if lo(R.ax) < 3 then exit;         {!!.22}

    L := GlobalDosAlloc(34);
    R.ax := $3800;
    R.dx := 0;
    R.ds := HiLo(L).HiWord;
    if SimulateRealModeInt($21, R) <> 0 then goto Skip;
    Move(Ptr(HiLo(L).LoWord, $12)^, Pointer(UpcaseFunc), 4);
{$ENDIF}

Skip:
    L := GlobalDosFree(HiLo(L).LoWord);
  end;
{$ENDIF}

  procedure SetInternationalUpcase;  {!!.20}
  var
    B : Byte;
  begin
    SetDefaultUpcase;
    UpcaseFunc := Nil;
{$IFDEF PMode}
    SetInternationalUpcasePrimPM;
{$ELSE}
    SetInternationalUpcasePrim;
{$ENDIF}
    if UpcaseFunc <> nil then
      for B := 128 to 165 do
        UCTable[B] := FixupUCChar(Chr(B));
  end;

type                                                           {!!.30}
  FinditFunc = function(var Buffer; BufLength : Word;          {!!.30}
                        var Match ; MatLength : Word) : Word;  {!!.30}

  {!!.30 - New}
  function ReplaceIt(var Buffer; BufLength : Word;
                     var Match; MatLength : Word;
                     var Repl; RepLength : Word;
                     var NewLength : Word; MaxLength : Word;
                         FindFunc : FinditFunc; All : Boolean) : Word;
  type
    SearchBuf = array[0..$FFF0] of Byte;
    SearchPtr = ^SearchBuf;

  var
    FoundPos    : Word;
    TempPos     : Word;
    TempLength  : LongInt;
    BufferDelta : Integer;
    SearchStart : Word;
    Done        : Boolean;
    SBuf        : SearchBuf absolute Buffer;

  begin
    { calculate how much the buffer will grow or shrink with each replacement }
    BufferDelta := RepLength - MatLength;

    NewLength := BufLength;
    FoundPos  := FindFunc(SBuf, NewLength, Match, MatLength);
    Done      := False;
    ReplaceIt := Word(-1);
    while not Done and (FoundPos <> Word(-1)) do begin
      ReplaceIt := FoundPos;
      TempLength := LongInt(BufferDelta) + NewLength;
      if (TempLength > MaxLength) then begin
        ReplaceIt := Word(-1);
        Exit;
      end;

      if not All then
        Done := True;

      ReplacePrim(SBuf, NewLength, FoundPos, MatLength, Repl, RepLength);
      NewLength   := TempLength;
      SearchStart := FoundPos + RepLength;
      TempPos     :=
        FindFunc( SBuf[SearchStart],
                  NewLength - SearchStart,
                  Match,
                  MatLength);
      if (TempPos <> Word(-1)) then
        FoundPos := TempPos + SearchStart
      else
        FoundPos := Word(-1);
    end;
  end;

  {!!.30 - New}
  function Replace(var Buffer; BufLength : Word;
                   var Match; MatLength : Word;
                   var Repl; RepLength : Word;
                   var NewLength : Word;
                   MaxLength : Word;
                   All : Boolean) : Word;
  begin
    Replace := ReplaceIt( Buffer, BufLength, Match, MatLength, Repl,
                          RepLength, NewLength, MaxLength, Search, All);
  end;

  {!!.30 - New}
  function ReplaceUC(var Buffer; BufLength : Word;
                     var Match; MatLength : Word;
                     var Repl; RepLength : Word;
                     var NewLength : Word;
                     MaxLength : Word;
                     All : Boolean) : Word;
  begin
    ReplaceUC := ReplaceIt( Buffer, BufLength, Match, MatLength, Repl,
                            RepLength, NewLength, MaxLength, SearchUC, All);
  end;

{$IFDEF VIRTUALPASCAL}
function UpCaseMac(Ch : Char) : Char; assembler; {$FRAME-} {$USES None}
  {-Uppercase character macro, no international character support}
  asm;
    POP    AX
    CMP    AL,'a'
    JB     @No
    CMP    AL,'z'
    JA     @No
    AND    AL,$DF
    @No:
  end;

function LoCaseMac(Ch : Char) : Char; assembler; {$FRAME-} {$USES None}
  {-Lowercase character macro, no international character support}
  asm;
    POP    AX
    CMP    AL,'A'
    JB     @No
    CMP    AL,'Z'
    JA     @No
    OR     AL,$20
    @No:
  end;

{$ENDIF VIRTUALPASCAL}

begin
  {Initialize international uppercasing function for current code page} {!!.02}
  SetInternationalUpcase;   {!!.02}
end.
