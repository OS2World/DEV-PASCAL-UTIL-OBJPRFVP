Program TestCrt;

{ Unit to test the functionality of OpCrt }

Uses
  Use32, OpCrt;

Var
  Win1 : Pointer;
  Win2 : Pointer;
  s : String;
  Flex : FlexAttrs;

Procedure TestOut;
begin
  FastWrite('FastWrite $18', 2, 5, $18 );
  FastText ('FastText ', 3, 5 );
  FastVert ('FastVert $71', 2, 2, $71 );
  FastFill ( 20, 'x', 4, 5, $20 );
  FastFillVert ( 20, 'x', 2, 3, $02 );
  FastCenter( 'FastCenter $81', 5, $81 );
  FastFlush('FastFlush $91', 6, $91 );
  s := #$10#$20#$30#$40#$50#$60#$70#$0F;
  WriteAttribute( s, 2, 8 );
  ChangeAttribute( 5, 3, 8, $27 );
  FastWriteAttr( 'FastWriteAttr', 7, 5, #$10#$20#$30#$40#$50#$60#$70#$0F#10#11#12#$E6#14 );
  Flex[0] := 7; Flex[1] := 8; Flex[2] := 9; Flex[3] := 10;
  s := 'Flex'#1'Wri'#2'te '#3'Test'#3'...';
  FlexWrite( s, 8, 5, Flex );
  If FlexLen( s ) <> 17 then
    Writeln(#7'Error!');
  FastWriteCtrl( #27'FastWriteCtrl'#10':'+s, 9, 5, $07, $1F );

  FastRead( 10, 2, 5, S );
  If s <> 'FastWrite ' then
    Writeln(#7'Error!');
  ReadAttribute( 10, 2, 5, S );
  If s <> #24#24#24#$10#$20#$30#$40#$50#$60#$70 then
    Writeln(#7'Error');
end;

Var
  i : Integer;

begin
  writeln('Hello!');
  Writeln('I AM here...');
  Writeln('The Third...');
  for i := 1 to 29 do
    writeln(i);
  { Save original window }
  SaveWindow( 1, 1, 80, 25, True, Win1 );

  { Test Output functions }
  TestOut;

  { Save modified window }
  SaveWindow( 1, 1, 80, 25, True, Win2 );

  For i := 1 to 5 do
    begin
      ScrollWindowDown( 2, 2, 20, 10, 1 );
      Delay( 100 );
      ScrollWindowUp( 2, 2, 20, 10, 1 );
      Delay( 100 );
    end;

  { Display original window }
  RestoreWindow( 1, 1, 80, 25, False, Win1 );

  { Repeat test in smaller window }
  Window( 40, 10, 75, 22 );
  TextAttr := $17;
  ClrScr;
  TestOut;
  TextAttr := MakeHiddenAttr( TextAttr );

  For i := 1 to 5 do
    begin
      ScrollWindowDown( 2, 2, 20, 10, 1 );
      Delay( 100 );
      ScrollWindowUp( 2, 2, 20, 10, 1 );
      Delay( 100 );
    end;

  { Display our window }
  RestoreWindow( 1, 1, 80, 25, True, Win2 );

  { Restore original window }
  RestoreWindow( 1, 1, 80, 25, True, Win1 );

  FastWrite('FastWrite Middle', 12, 30, $18 );
  TextAttr := $07;
  For i := 1 to 12 do
    begin
      ScrollWindowDown( 1, i, 80, 24-i, 1 );
      Delay( 80 );
      ScrollWindowUp( 1, i, 80, 24-i, 1 );
      Delay( 80 );
    end;
end.
