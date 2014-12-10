program VirtScreenExample;  {EXVSCR.PAS}
uses OpRoot, OpString, OpCrt, Opframe, OpWindow;
const
  Rows = 100;
  Cols = 200;
var
  V : VirtScreen;
  R, C, KW : Word;
  S : String;
  Finished : Boolean;
begin
  {Clear the physical screen}
  TextAttr := $70;
  ClrScr;
  {Allocate a virtual screen}
  if not V.Alloc(Rows, Cols) then begin
    WriteLn('Unable to Init VirtScreen, InitStatus ', InitStatus);
    Halt;
  end;
  {Initialize the virtual screen with an interesting pattern}
  V.Clear($07, ' ');
  V.Activate;
  for R := 1 to Rows do begin
    Str(R:3, S);
    FastWrite(S, R, 1, $07);
    C := (R mod 10)+5;
    while C <= Cols do
      if (C mod 20 = 0) and (C < Cols-5) then begin
        FastWrite(S, R, C+1, $07);
        Inc(C, 5);
      end else begin
        FastWrite(Char(Byte('a')+((C-R-5) mod 26)), R, C, (C-R) and $7F);
        Inc(C);
      end;
  end;
  V.Deactivate;
  {Scroll around the virtual screen}
  R := 1;
  C := 1;
  Finished := False;
  repeat
    V.CopyToScreen(R, C);
    case ReadKeyWord of
      $4800 :                     {Up arrow}
        if R > 1 then
          Dec(R);
      $4B00 :                     {Left Arrow}
        if C > 1 then
          Dec(C);
      $4D00 :                     {Right Arrow}
        Inc(C);
      $5000 :                     {Down arrow}
        Inc(R);
      $011B :                     {Escape}
        Finished := True;
    end;
  until Finished;
  V.Done;
end.
