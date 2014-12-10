program PizzaListExample;  {EXPICK3.PAS}
{$UNDEF UseStreams}

uses Use32, OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpPick;
type
  MultiPizzaList =
    object(PickList)
      constructor Init(X1, Y1, X2, Y2 : Byte);
      procedure ItemString(Item : Word; Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
    end;
constructor MultiPizzaList.Init(X1, Y1, X2, Y2 : Byte);
const NumPizzaToppings = 5;
var   PickWindowOptions : LongInt;
begin
  PickWindowOptions := DefWindowOptions or wBordered;
  if not PickList.InitAbstract(X1, Y1, X2, Y2,
                               DefaultColorSet,   {ColorSet to use}
                               PickWindowOptions, {Window options}
                               11,                {Column width per item}
                               NumPizzaToppings,  {Number of picklist items}
                               PickVertical,      {Orientation procedure}
    {--------> }               MultipleChoice)    {Command handler}
  then
    Fail;
  SetSelectMarker(#251, '');
end;
procedure MultiPizzaList.ItemString(Item : Word; Mode : pkMode;
                                    var IType : pkItemType;
                                    var IString : String);
begin
  case Item of
    1 : IString := 'Pepperoni';
    2 : IString := 'Sausage';
    3 : IString := 'Mushrooms';
    4 : IString := 'Anchovies';
    5 : IString := 'Onions';
  end;
end;
var
  PizzaTop : MultiPizzaList;
  SelectedItem : Word;
begin
  {Make a MultiPizzaList}
  if not PizzaTop.Init(35, 5, 45, 9) then begin
    WriteLn('Failed to Init MultiPizzaList,  Status = ', InitStatus);
    Halt;
  end;
  {Pick an item}
  PizzaTop.Process;
  PizzaTop.Erase;
  if PizzaTop.GetLastCommand = ccSelect then
    if PizzaTop.GetSelectedCount > 0 then begin
      WriteLn('You selected:');
      PizzaTop.InitSequence(SelectedItem);
      while PizzaTop.HaveSelected(SelectedItem) do begin
        WriteLn(PizzaTop.GetItemString(SelectedItem));
        PizzaTop.NextSelected(SelectedItem);
      end;
    end else
      Writeln('No selections');
  PizzaTop.Done;
end.
