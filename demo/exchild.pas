program ChildExample; {EXCHILD.PAS}
uses
  OpInline, OpString, OpRoot, OpCrt, OpFrame, OpWindow;
var
  P : StackWindow;
  C1 : StackWindowPtr; {!!.20}
  C2 : StackWindowPtr; {!!.20}

procedure CheckError(var P : StackWindow);
begin
  if P.RawError <> 0 then begin
    WriteLn('error ', P.RawError);
    Halt;
  end;
end;

procedure Wait;
var
  KW : word;
begin
  KW := ReadKeyWord;
end;

begin
  {All windows will have a border and an alternate frame}
  SetLongFlag(DefWindowOptions, wBordered+wAltFrame);

  {Initialize the parent window, full screen}
  P.Init(2, 2, ScreenWidth-1, ScreenHeight-1);

  {No child window will allocate a covers buffer}
  SetLongFlag(DefWindowOptions, wNoCoversBuffer);

  {Initialize the first child window}
  New(C1, Init(6, 4, 29, 9));                   {!!.20}
  C1^.wFrame.AddHeader('child1', heTC);         {!!.20}
  C1^.wFrame.AddShadow(shBR, shOverwrite);      {!!.20}
  C1^.aFrame.AddHeader('alt child1', heTC);     {!!.20}

  {Initialize the second child window}
  New(C2, Init(41, 4, 69, 9));                  {!!.20}
  C2^.wFrame.AddHeader('child2', heTC);         {!!.20}
  C2^.wFrame.AddShadow(shBR, shOverwrite);      {!!.20}
  C2^.aFrame.AddHeader('alt child2', heTC);     {!!.20}

  {Attach the children to the parent}
  P.AddChild(C1);                               {!!.20}
  CheckError(P);
  P.AddChild(C2);                               {!!.20}
  CheckError(P);

  {Draw the parent and children}
  P.Draw;
  Wait;

  {Make the first child active}
  P.SetActiveChild(C1);                         {!!.20}
  Wait;

  {Make the second child active}
  P.SetActiveChild(C2);                         {!!.20}
  Wait;

  {Write within the second child}
  P.wFastWrite('hello to child 2', 1, 1, $07);
  Wait;

  {Make the parent active and write within it}
  P.SetActiveChild(nil);
  Wait;
  P.wFastCenter('hello to main window', 22, $70);
  Wait;

  {Erase and dispose of parent and children}
  P.Erase;
  P.Done;
end.
