unit Long32;


// This unit is intended for use with Virtual Pascal, and provides
// some very basic functions for dividing, multiplying and displaying
// Longints/cardinals interpreted as 32-bit unsigned integers.

type
  PCardinal = ^Cardinal;

// Unsigned long division
function udiv(Operand, Divisor: Cardinal): Cardinal; {&frame-} {&uses none}
asm
      mov   eax, Operand
      xor   edx, edx
      div   dword ptr Divisor
end;

// Unsigned long modulus
function umod(Operand, Divisor: Cardinal): Cardinal; {&frame-} {&uses none}
asm
      mov   eax, Operand
      xor   edx, edx
      div   dword ptr Divisor
      mov   eax, edx
end;

// Unsigned long multiplication
function umul(Op1, Op2: Cardinal; HighLong: PCardinal): Cardinal; {&frame-} {&uses none}
asm
      mov   eax, Op1
      mul   dword ptr Op2
      mov   ecx,HighLong
      jecxz @@1           // if HighLong <> nil, assign value
      mov   [ecx],edx
    @@1:
end;

// Unsigned long division and modulus
function udivmod(Operand, Divisor: Cardinal; var Modulus: Cardinal): Cardinal; {&frame-} {&uses none}
asm
      mov   eax, Operand
      mov   ecx, Divisor
      xor   edx, edx
      div   ecx
      mov   ecx,Modulus
      mov   [ecx],edx
end;

// Convert unsigned 32-bit integer to string format
function ulong2str(Value: Cardinal): ShortString; {&frame+} {&uses ebx,edi}
asm
      mov   eax, Value
      mov   ebx, 0Ah
      xor   ecx, ecx
   @@1:
      xor   edx, edx
      div   ebx
      add   dl, 30h        // convert to ascii
      push  edx
      inc   ecx
      test  eax, eax
      jne   @@1

      mov   edi, @Result
      mov   eax, ecx
      stosb               // set the pascal string length byte
    @@2:
      dec   ecx
      pop   eax
      stosb               // now finish the string
      jne   @@2
end;

var
  a,b,c,d: Cardinal;

begin
  a := $9000000;
  a := a shl 4;
  b := 17;
  c := udiv(a,b);
  c := umod(a,b);
  c := udivmod(a,b,d);
  writeln(ulong2str(a));
  writeln(ulong2str(b));
  writeln(ulong2str(c));
  writeln(ulong2str(d));
  a := umul(c,b,@d);
  a := umul(4,3,@b);
end.


