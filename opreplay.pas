{*********************************************************}
{*                  OPREPLAY.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{* Portions Copyright (c) Sunny Hill Software 1985, 1986 *}
{*     and used under license to TurboPower Software     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$IFDEF VIRTUALPASCAL}
  !! ERROR: This unit is not compatible with Virtual Pascal !!
{$ENDIF}

{$S-,R-,V-,I-,B-,F-,O-,A-}

{$IFDEF Dpmi}                                                         {!!.20}
  {$C FIXED PRELOAD PERMANENT}                                        {!!.20}
{$ENDIF}                                                              {!!.20}

{$I OPDEFINE.INC}

unit OpReplay;
  {-Macro playback}

interface

const
  MaxKeysInMacro = 1000;     {arbitrary}
  EndOfMacro = $0FFFF;       {signals end of a macro}
type
  MacroRecPtr = ^MacroRec;
  MacroRec =
    record
      NumKeys  : Word;       {number of keys except the last, always $0FFFF}
      KeyArray : array[1..MaxKeysInMacro] of Word;
      Overflow : Word;       {may be needed to mark end of macro}
    end;
const
  ScrapMacroSize     : Word = 0;  {maximum # of keys in ScrapMacroPtr^}
  ScrapMacroPtr      : MacroRecPtr = nil; {pointer to scrap macro}
var
  MacroInProgressPtr : ^Boolean;  {macro is being played back}
  CurrentMacroPtr    : ^Pointer;  {pointer to address of current macro}

procedure StartMacro(Macro : MacroRecPtr);
  {-Starts execution of the specified macro}

function CharToMacro(MacroChar : Char) : Word;
  {-Converts a character to a keyboard code/char representation. This table
    holds all scan code values for ASCII characters from 0 to 63. All alpha
    characters can be mapped to this table by subtracting a number divisible
    by 32 that would put that alpha in the range of 0..63.}

procedure StringToScrapMacro(S : string);
  {-Initialize ScrapMacro from a string}

procedure StringToMacro(S : string; MRP : MacroRecPtr; MaxKeys : Word);
  {-Initialize MRP^ from a string}

procedure Int16;
  {-The macro engine used to replay keystrokes. It is the user's
    responsibility to install and uninstall this interrupt handler.}

procedure SetEndOfMacroProc(P : Pointer);
  {-Set pointer to procedure to call when end of macro reached}

procedure InitScrapMacroPtr;
  {-Initialize ScrapMacroPtr}

procedure SetWaitCount(Count : Byte);           {!!.13}
  {-Set a wait count. This count is the number of times int 16 function 1 will
    pause before returning the next character in the macro. This fixes
    compatibility problems with some applications, such as Clipper programs.
    The default wait count is 0. A Wait count of 5 is recommended for Clipper
    based programs.}
  {==========================================================================}

implementation

var
  CurrentMacroOfs : ^Word absolute CurrentMacroPtr;
  CSToDSInc : Word; {!!.20}

  {$L OPREPLAY.OBJ}
  procedure Int16; external;
  procedure Int16Init; external;
  procedure SetEndOfMacroProc(P : Pointer); external;
  procedure SetWaitCount(Count : Byte); external; {!!.13} {!!.20}

  {$L OPSCRAP.OBJ}
  procedure InitScrapMacroPtr; external;

  procedure InterruptsOn;
    {-Turn interrupts on}
    inline($FB); {sti}

  procedure InterruptsOff;
    {-Turn interrupts off}
    inline($FA); {cli}

  procedure StartMacro(Macro : MacroRecPtr);
    {-Starts execution of the specified macro. If a macro is already in
      progress it will immediately be replaced by the macro passed.}
  begin
    if Macro <> nil then begin
      {turn interrupts off while doing this}
      InterruptsOff;

      {make this macro the current macro}
      CurrentMacroPtr^ := Macro;

      {skip past the length word}
      Inc(CurrentMacroOfs^, 2);

      {Turn this macro on}
      MacroInProgressPtr^ := True;

      {restore interrupts}
      InterruptsOn;
    end;
  end;

  function CharToMacro(MacroChar : Char) : Word;
   {-Converts a character to a keyboard code/char representation. This table
     holds all scan code values for ASCII characters from 0 to 63. All alpha
     characters can be mapped to this table by subtracting a number divisible
     by 32 that would put that alpha in the range of 0..63.}
  const
    ScanCodes : array[0..63] of Byte = (
      $03, {@} $1E, {A} $30, {B} $2E, {C} $20, {D} $12, {E} $21, {F} $22, {G}
      $23, {H} $17, {I} $24, {J} $25, {K} $26, {L} $32, {M} $31, {N} $18, {O}
      $19, {P} $10, {Q} $13, {R} $1f, {S} $14, {T} $16, {U} $2f, {V} $11, {W}
      $2d, {X} $15, {Y} $2c, {Z} $1a, {[} $2b, {\} $1b, {]} $07, {^} $0c, {_}
      $39, { } $02, {!} $28, {"} $04, {#} $5, { $} $06, {%} $08, {&} $28, {'}
      $0A, {(} $0b, {)} $09, {*} $0d, {+} $33, {,} $0c, {-} $34, {.} $35, {/}
      $0b, {0} $02, {1} $03, {2} $04, {3} $05, {4} $06, {5} $07, {6} $08, {7}
      $09, {8} $0A, {9} $27, {:} $27, {;} $33, {<} $0d, {=} $34, {>} $35  {?}
    );
  var
    CharCode : Byte absolute MacroChar;
  begin
    case MacroChar of
      ^M :
        CharToMacro := $1C0D;
      ^[ :                     {!!.13}
        CharToMacro := $011B;  {!!.13}
      #0..'?' :
        CharToMacro := (ScanCodes[CharCode] shl 8) or CharCode;
      '@'..'_' :
        CharToMacro := (ScanCodes[CharCode-64] shl 8) or CharCode;
      '`', '~' :
        CharToMacro := $2900 or CharCode;
      'a'..'}' :
        CharToMacro := (ScanCodes[CharCode-96] shl 8) or CharCode;
      #127 :
        CharToMacro := $0EFF;
      else
        CharToMacro := CharCode;
    end;
  end;

  procedure StringToMacro(S : string; MRP : MacroRecPtr; MaxKeys : Word);
    {-Initialize MRP^ from a string}
  var
    SLen : Byte absolute S;
    I : Word;
  begin
    if (MaxKeys > 0) and (MRP <> nil) then begin
      if SLen > MaxKeys then
        SLen := MaxKeys;
      with MRP^ do begin
        NumKeys := SLen;
        for I := 1 to SLen do
          KeyArray[I] := CharToMacro(S[I]);
        KeyArray[I+1] := EndOfMacro;
      end;
    end;
  end;

  procedure StringToScrapMacro(S : string);
    {-Initialize ScrapMacro from a string}
  begin
    StringToMacro(S, ScrapMacroPtr, ScrapMacroSize);
  end;

begin
  {$IFDEF Dpmi}                 {!!.20}
  CSToDSInc := SelectorInc;     {!!.20}
  {$ELSE}                       {!!.20}
  CSToDSInc := 0;               {!!.20}
  {$ENDIF}                      {!!.20}

  Int16Init;
end.
