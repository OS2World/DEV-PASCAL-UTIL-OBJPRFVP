program BitSetExample;  {EXBITSET.PAS}
uses
  OpRoot;
var
  DivisibleBy9 : BitSet;
  I, Cnt : Integer;
begin
  {Create a BitSet for flagging 1000 items (this clears all bits)}
  if not DivisibleBy9.Init(1000) then begin
    Writeln('Failed to Init BitSet,  Status = ', InitStatus);
    Halt;
  end;
  {Loop through first 1000 integers, set bit if integer is divisible by 9}
  for I := 1 to 1000 do
    if (I mod 9) = 0 then
      DivisibleBy9.SetBit(I);
  {How many, total, are divisible by 9?}
  Writeln(DivisibleBy9.BitsSet, ' are divisible by 9');
  {How many, between 500 and 600, are divisible by 9?}
  Cnt := 0;
  for I := 500 to 600 do
    if DivisibleBy9.BitIsSet(I) then
      Inc(Cnt);
  WriteLn(Cnt, ' between 500 and 600 are divisible by 9');
  DivisibleBy9.Done;
end.
