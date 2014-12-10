{$V-}
program LineEditorExample; {EXLEDIT.PAS}
uses
  Use32,
  OpCrt, OpRoot, OpCmd, OpEdit;
var
  LE : LineEditor;
  Name : String[40];
  Age : Integer;
begin
  ClrScr;
  Name := '';
  Age := 0;
  LE.Init(DefaultColorSet);
  LE.ReadString('Name: ', 4, 2, 40, 20, Name);
  if (LE.GetLastCommand <> ccQuit) and (LE.GetLastCommand <> ccError) then
    LE.ReadInteger('Age:  ', 6, 2, 3, 1, 150, Age);
  LE.Done;
end.
