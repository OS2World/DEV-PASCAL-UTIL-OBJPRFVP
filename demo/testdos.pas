Program TestDos;

Uses
  OpDos;

Var
  c : Char;

begin
  GetDiskClass( 'C', c );
  GetDiskClass( 'G', c );
  GetDiskClass( 'A', c );
  GetDiskClass( 'J', c );
  GetDiskClass( 'H', c );
  GetDiskClass( 'Z', c );
end.
