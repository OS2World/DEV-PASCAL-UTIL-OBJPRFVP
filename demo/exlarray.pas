{$I OPDEFINE.INC}    {!!.14}
program OpArrayExample;  {EXLARRAY.PAS}
uses
  OpRoot, OpLarray;
type
  Str19 = String[19]; {!!.15} {Change to 20 byte element so XMS gets a chance}
  Str19Array =
    object(OpArray)
      constructor Init(Rows : Word; FileName : String; HeapToUse : LongInt);
      procedure SetVal(Row : Word; Val : Str19);
      function GetVal(Row : Word) : Str19;
    end;
constructor Str19Array.Init(Rows : Word; FileName : String;
                            HeapToUse : LongInt);
const
  {$IFDEF Dpmi}                                                    {!!.20}
  Priority  : AutoPriority = (lRamArray,lVirtualArray);            {!!.20}
  {$ELSE}                                                          {!!.20}
  {$IFDEF SupportXms}                                              {!!.14}
  Priority : AutoPriority = (lRamArray, lEmsArray,                 {!!.14}
                             lXmsArray, lVirtualArray);            {!!.14}
  {$ELSE}                                                          {!!.14}
  Priority : AutoPriority = (lRamArray, lEmsArray, lVirtualArray);
  {$ENDIF}                                                         {!!.14}
  {$ENDIF}                                                         {!!.20}
var
  ClearEl : Str19;
begin
  if not OpArray.Init(Rows, 1,          {Rows, columns}
                      SizeOf(Str19),    {Size of one element}
                      FileName,         {Virtual array file}
                      HeapToUse,        {Heap space to use}
                      0,                {Array options}
                      Priority)         {Allocation priority}
  then
     Fail;
  {Clear the array quickly}
  ClearEl := '';
  ClearA(ClearEl, FastInit);
end;
procedure Str19Array.SetVal(Row : Word; Val : Str19);
begin
  SetA(Row, 0, Val);
end;
function Str19Array.GetVal(Row : Word) : Str19;
var
  S : Str19;
begin
  RetA(Row, 0, S);
  GetVal := S;
end;
var
  OA : Str19Array;
begin
  if not OA.Init(1000,               {Rows}
                 'TEMP.ARR',         {Virtual array file}
                 MaxAvail div 2)     {Heap space to use}
  then begin
    WriteLn('Failed to init Str19Array. Status = ', InitStatus);
    Halt;
  end;
  case OA.TypeOfArray of
    lRamArray : WriteLn('RAM array');
    lEmsArray : WriteLn('EMS array');
    lXmsArray : WriteLn('XMS array'); {!!.20}
    lVirtualArray : WriteLn('Virtual array');
  end;
  {Set the first element of the array}
  OA.SetVal(0, 'Object');
  {Write out the element}
  Writeln('OA[0] = ', OA.GetVal(0));
  {Dispose of the array}
  OA.Done;
end.
