{$R-,S-,V-}
program XmsTest;

uses Crt, OpString, OpXms;

const
  XmsStr : String[15] = 'Hello XMS world';

procedure MoveTest;
var
  Err : Byte;
  Handle : Word;
  ps, pd : ExtMemPtr;
  S : String[15];

begin
  WriteLn('Performing Move Test');
  Err := AllocateExtMem(1, Handle);
  WriteLn('AllocateExtMem: ', XmsErrorString(Err));
  if Err <> 0 then
    Exit;
  ps.RealPtr := @XmsStr;
  pd.ProtectedPtr := 0;
  Err := MoveExtMemBlock(Succ(Length(XmsStr)), 0, ps, Handle, pd);
  WriteLn('MoveExtMemBlock: ', XmsErrorString(Err));
  if Err <> 0 then begin
    Err := FreeExtMem(Handle);
    Exit;
  end;
  FillChar(S, SizeOf(S), 0);
  pd.RealPtr := @S;
  ps.ProtectedPtr := 0;
  Err := MoveExtMemBlock(Succ(Length(XmsStr)), Handle, ps, 0, pd);
  if Err = 0 then
    WriteLn(S);
  WriteLn('MoveExtMemBlock: ', XmsErrorString(Err));
  Err := FreeExtMem(Handle);
  WriteLn('FreeExtMem: ', XmsErrorString(Err));
end;

procedure AllocateTest;
var
  Total, Largest, Handle : Word;
  Err : Byte;
begin
  WriteLn('Performing Allocate Test');
  Err := QueryFreeExtMem(Total, Largest);
  WriteLn('Total   = ', Total);
  WriteLn('Largest = ', Largest);
  Err := AllocateExtMem(Largest, Handle);
  WriteLn('Attempt to allocate ', Largest,'K of XMS returned: ', XmsErrorString(Err));
  Err := FreeExtMem(Handle);
  WriteLn('Attempt to free extended memory block returned: ', XmsErrorString(Err));
  Err := AllocateExtMem(Largest+1, Handle);
  WriteLn('Attempt to allocate ', Largest+1,'K of XMS returned: ', XmsErrorString(Err));
end;

procedure HMATest;
var
  Err : Byte;
begin
  WriteLn('Performing HMA Test');
  Err := RequestHMA($FFFF);
  WriteLn('RequestHMA returned: ', XmsErrorString(Err));
  if Err = 0 then begin
    Err := ReleaseHMA;
    WriteLn('ReleaseHMA returned: ', XmsErrorString(Err));
  end;
end;

procedure LockTest;
var
  Handle : Word;
  Err : Byte;
  LinearPtr : ExtMemPtr;

begin
  WriteLn('Performing Lock Test');
  Err := AllocateExtMem(1, Handle);
  WriteLn('Attempt to allocate 1K of XMS returned: ', XmsErrorString(Err));
  Err := LockExtMemBlock(Handle, LinearPtr);
  WriteLn('LockExtMemBlock returned: ', XmsErrorString(Err));
  if Err = 0 then
    WriteLn('Block''s 32 bit linear address = $', HexL(LinearPtr.ProtectedPtr));
  Err := UnlockExtMemBlock(Handle);
  WriteLn('UnlockExtMemBlock returned: ', XmsErrorString(Err));
  Err := FreeExtMem(Handle);
  WriteLn('Attempt to free extended memory block returned: ', XmsErrorString(Err));
end;

procedure ResizeTest;
var
  Handle : Word;
  Err : Byte;
begin
  WriteLn('Performing Resize Test');
  Err := AllocateExtMem(1, Handle);
  WriteLn('Attempt to allocate 1K of XMS returned: ', XmsErrorString(Err));

  Err := ResizeExtMemBlock(Handle, 2);
  WriteLn('ResizeExtMemBlock returned: ', XmsErrorString(Err));

  Err := FreeExtMem(Handle);
  WriteLn('Attempt to free extended memory block returned: ', XmsErrorString(Err));
end;

procedure UMBTest;
var
  Largest, SegBase : Word;
  Err : Byte;
  P : ^String;
begin
  WriteLn('Performing Upper Memory Block Test');
  Err := AllocUpperMemBlock($FFFF, SegBase, Largest);
  if Err = FuncNotImplemented then begin
    WriteLn('Upper memory block functions are not implemented by this XMS driver');
    Exit;
  end;
  WriteLn('The largest upper memory block available is ', Largest*16);
  Err := AllocUpperMemBlock(Largest, SegBase, Largest);
  WriteLn('Attempt to allocate ', Largest*16,' byte UMB: ', XmsErrorString(Err));
  if Err <> 0 then
    Exit;
  P := Ptr(SegBase, 0);
  P^ := XmsStr;
  WriteLn(P^);
  Err := FreeUpperMemBlock(SegBase);
  WriteLn('FreeUpperMemBlock returned: ', XmsErrorString(Err));
end;

procedure Pause;

begin
  Write('Press any key to continue');
  if ReadKey = #0 then ;
  WriteLn;
end;

begin
  if not XmsInstalled then begin
    WriteLn('There is no XMS memory manager installed.');
    Halt;
  end;
  WriteLn('XMS Control function at ', HexPtr(XmsControl));
  HMATest;
  Pause;
  AllocateTest;
  Pause;
  MoveTest;
  Pause;
  LockTest;
  Pause;
  ResizeTest;
  Pause;
  UMBTest;
end.
