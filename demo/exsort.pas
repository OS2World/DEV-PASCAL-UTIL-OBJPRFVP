program SortInt;  {EXSORT.PAS}
  {-Sort a random set of integers}
uses
  OpSort;
const
  NumToSort = 1000;
var
  Status : SortStatus;

{$F+}
procedure GetInts;
  {-Load the sort data structures with elements to be sorted}
var
  N : Word;
  Value : Integer;
begin
  WriteLn('Loading Sort...');
  for N := 1 to NumToSort do begin
    Value := Random(MaxInt);
    if not PutElement(Value) then
      {Sort error occurred - most likely out of memory}
      Exit;
  end;
end;

function Less(var X, Y) : Boolean;
  {-Compare two integers}
var
  XI : Integer absolute X;
  YI : Integer absolute Y;
begin
  Less := (XI < YI);
end;

procedure WriteInts;
  {-Write the sorted output}
var
  Value : Integer;
begin
  while GetElement(Value) do
    WriteLn(Value);
end;
{$F-}

begin
  Status := Sort(NumToSort, SizeOf(Integer), GetInts, Less, WriteInts);
  case Status of
    SortSuccess : WriteLn(ElementsSorted, ' elements sorted');
    SortOutOfMemory : WriteLn('Insufficient memory');
    SortTooManyElements : WriteLn('Too many elements to sort');
  end;
end.
