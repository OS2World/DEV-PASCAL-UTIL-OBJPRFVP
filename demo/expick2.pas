program PizzaListExample;  {EXPICK2.PAS}
uses
  Use32,
  OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpPick;
type
  PizzaList =
    object(PickList)
      constructor Init(X1, Y1, X2, Y2 : Byte);
      procedure ItemString(Item : Word; Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
    end;
constructor PizzaList.Init(X1, Y1, X2, Y2 : Byte);
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
                               SingleChoice)      {Command handler}
  then
    Fail;
end;
procedure PizzaList.ItemString(Item : Word; Mode : pkMode;
                               var IType : pkItemType;
                               var IString : String);
begin
  case Item of     {!!.11}
    1 : IString := 'Pepperoni';
    2 : IString := 'Sausage';
    3 : IString := 'Mushrooms';
    4 : IString := 'Anchovies';
    5 : IString := 'Onions';
  end;
end;
var
  PizzaTop : PizzaList;
begin
  {Make a PizzaList}
  if not PizzaTop.Init(35, 5, 45, 10) then begin
    WriteLn('Failed to Init PizzaList,  Status = ', InitStatus);
    Halt;
  end;
  {Set some PickList and Frame features}
  PizzaTop.SetSearchMode(PickCharSearch);
  PizzaTop.EnableExplosions(20);
  PizzaTop.SetPadSize(1, 0);
  with PizzaTop.wFrame do begin
    AddShadow(shBR, shOverWrite);
    AddHeader(' PICK ', heTC);
  end;
  {Pick an item}
  PizzaTop.Process;
  PizzaTop.Erase;
  if PizzaTop.GetLastCommand = ccSelect then
    WriteLn('You chose item ', PizzaTop.GetLastChoice,
            ' -  ', PizzaTop.GetLastChoiceString)
  else
    Writeln('No choice was made');
  PizzaTop.Done;
end.
