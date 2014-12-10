program PickListExample;  {EXPICK1.PAS}
uses
  Use32,
  OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpPick;
const
  NumPizzaToppings = 11;       {Number of items on pick list} {!!.22}
var
  PizzaTop : PickList;
  PickWindowOptions : LongInt;
  {$F+}
  procedure PizzaTopping(Item : Word; Mode : pkMode;
                         var IType : pkItemType; var IString : String;
                         PickPtr : PickListPtr);
    {-Sets item string and item options (if any)}
  begin
    case Item of     {!!.11} {!!.22}
      1 : IString := 'Pepperoni';
      2 : IString := 'Sausage';
      3 : IString := 'Mushrooms';
      4 : IString := 'Anchovies';
      5 : IString := 'Onions';
      6 : IString := 'Olives';
      7 : IString := 'Eggs';
      8 : IString := 'Pineapple';
      9 : IString := 'Spices';
     10 : IString := 'Artichoke';
     11 : IString := 'Ham';
    end;
  end;
  {$F-}
begin
  {Make a PickList with custom window options}
  PickWindowOptions := DefWindowOptions or wBordered;
  if not PizzaTop.InitCustom(35, 5, 45, 10,     {Initial window coordinates}
                             DefaultColorSet,   {ColorSet to use}
                             PickWindowOptions, {Window options}
                             11,                {Column width per item}
                             NumPizzaToppings,  {Number of picklist items}
                             PizzaTopping,      {Item string function}
                             PickVertical,      {Orientation procedure}
                             SingleChoice)      {Command handler}
  then begin
    WriteLn('Failed to Init PickList,  Status = ', InitStatus);
    Halt;
  end;
  {Set some PickList and Frame features}
  PizzaTop.SetSearchMode(PickCharSearch);
  PizzaTop.EnableExplosions(20);
  PizzaTop.SetPadSize(1, 0); {!!.11}
  with PizzaTop, wFrame do begin {!!.22}
    AddShadow(shBR, shOverWrite);
    AddHeader(' PICK ', heTC);
    AddMoreHeader(' ', heBR, ^X, ^Y, ^R, 1, 1, 1); {!!.22}
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
