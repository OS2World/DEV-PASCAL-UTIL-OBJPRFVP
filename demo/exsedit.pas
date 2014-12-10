{$V-}
program SimpleEditorExample;  {EXSEDIT.PAS}
uses
  OpCrt, OpRoot, OpCmd, OpSEdit;
var
  SLE  : SimpleLineEditor;
  Name : string[40];
  Age  : string[3];
begin
  ClrScr;
  Name := '';
  Age := '';
  SLE.Init(DefaultColorSet);
  SLE.ReadString('Name: ', 4, 2, 40, 20, Name);
  if SLE.GetLastCommand <> ccQuit then
    SLE.ReadString('Age:  ', 6, 2, 3, 3, Age);
  SLE.Done;
end.
