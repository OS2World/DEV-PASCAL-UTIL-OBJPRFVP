{$V-}
program Pizza; {EXENTRY2.PAS}
uses
  Use32, OpCrt, OpRoot, OpCmd, OpFrame, OpWindow, OpPick, OpField, OpSelect, OpEntry;
const NumPizzaToppings = 5;
type  InfoRec = record
                  Name : String[30];
                  PTop : String[10];
                end;
var
  Info : InfoRec;
  ES : EntryScreen;
  PL : PickList;
  {$F+}
  procedure PizzaTopping(Item : Word; Mode : pkMode; var IType : pkItemType;
                         var IString : String; PickPtr : PickListPtr);
  const
    ToppingStrings : array[1..5] of string[9] = (
      'Pepperoni', 'Sausage', 'Mushrooms', 'Anchovies', 'Onions');
  begin
    IString := ToppingStrings[Item];
    if Mode <> pkSearch then IString := ' '+IString+' ';
  end;
  {$F-}
begin
  if not PL.InitCustom(35, 5, 45, 10, DefaultColorSet,
                    DefWindowOptions OR wBordered, 20, NumPizzaToppings,
                    PizzaTopping, PickVertical, SingleChoice) then
    Halt;
  if not ES.InitCustom(25, 5, 55, 11, DefaultColorSet,
                       DefWindowOptions+wBordered) then
    Halt;
  ClrScr;
  FillChar(Info, SizeOf(Info), 0);
  { ES.esFieldOptionsOn(efExitAtEdges); }
  ES.AddSimpleStringField('Name:', 2, 1, 'A', 2, 7, 10, 20, 0, Info.Name);
  ES.AddPickStringField('Pizza Topping', 3, 1, 3, 7, 10, 0, Info.PTop, PL);

  ES.Process;
  ES.Done;
  PL.Done;
end.
