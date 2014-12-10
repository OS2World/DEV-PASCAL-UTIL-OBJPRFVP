Program Test;

{$DEFINE TSTOPMOUSE}
{$DEFINE TSTOPCRT}
{$DEFINE TSTOPSTRING}
{$DEFINE TSTOPDOS}
{$DEFINE TSTOPSTRDEV}
{$DEFINE TSTOPROOT}
{$DEFINE TSTMISC}

Uses
  Use32
  ,Os2Base
  ,OpString
  {$IFDEF TSTOPCRT}
  ,OpCrt
  {$ENDIF}
  {$IFDEF TSTOPDOS}
  ,OpDos
  {$ENDIF}
  {$IFDEF TSTOPSTRDEV}
  ,OpStrDev
  {$ENDIF}
  {$IFDEF TSTOPROOT}
  ,OpRoot
  {$ENDIF}
  {$IFDEF TSTOPMOUSE}
  ,OpMouse
  {$ENDIF}
  {$IFDEF TSTMISC}
  ,OpDate
  ,OpSort
  {$ENDIF}
  ;

Const
  BufLen = 10000;
  ct : Array[CompareType] of String[10]
     = ('Less','Equal','Greater') ;

Var
  s1, s2, s3 : String;
  b1, b2, b3 : Byte;
  w1, w2, w3 : Word;
  l1, l2, l3 : Longint;
{  p1, p2, p3 : RootPtr;}
  Buffer     : Array[ 1..BufLen ] of Byte;

{$IFDEF TSTOPSTRING}
Procedure TstOpString;
  Const
    Up1 : String
        = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`'+
          'ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~ÄöEAéAèÄEEEIIIéèêííOôOUUYôöùúùûüAI'+
          'OU••¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ‡·‚'+
          '„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ˜¯˘˙˚¸˝˛';
  Var
    _ebx, _esi, _edi : Word;
    ch1,ch : Char;
    s, s1,s2 : String;
    RegOK : Boolean;
    NewLen : Word;

    BT : BTable;

  begin
    RegOK := false;
    Writeln( 'OPSTRING:' );
    { Check that everything works, AND that vital registers are }
    { preserved: ebx. esi. edi }
    asm
      mov _ebx,ebx
      mov _esi,esi
      mov _edi,edi
    end;
    Write( '  UpcaseMac     : ' );
    s := '';
    for ch := ' ' to #127 do
      s := s + upcaseMac(ch);
    If s = Copy(Up1,1,length(s)) then Writeln( 'OK' ) else Writeln( 'Error' );

    Write( '  OpStr.Upcase  : ' );
    s := '';
    for ch := ' ' to #254 do
      s := s + OpString.Upcase(ch);
    If s = Copy(Up1,1,length(s)) then Writeln( 'OK' ) else Writeln( 'Error' );

    Writeln( '  Hex           : ', HexB(17),' ',HexW(200),' ',HexL(800));
    Writeln( '  HexPtr        : ', hexPtr( nil ));
    Writeln( '  Binary        : ', BinaryB(17),' ',BinaryW(200),' ',BinaryL(800));
    Writeln( '  Octal         : ', OctalB(17),' ',OctalW(200),' ',OctalL(800));

    Writeln( '  StUpCase      : ', StUpCase( 'Hello, I am Danish ëõÜ!' ));
    Writeln( '  StLoCase      : ', StLoCase( 'Hello, I am Danish íùè!' ));

    s := 'This    is a        test  string';
    Writeln( '  EnTab 8       : ', EnTab( s, 8 ) );
    Writeln( '  EnTab 2       : ', EnTab( s, 2 ) );
    Writeln( '  Detab( En 8 ) : ', DeTab( EnTab( s, 8 ), 8 ) );
    Writeln( '  Detab( En 2 ) : ', DeTab( EnTab( s, 2 ), 2 ) );

    Writeln( '  CompString    : ', ct[CompString( 'Allan' , 'Peter' )] );
    Writeln( '  CompString    : ', ct[CompString( 'Allan' , 'ALLAN' )] );
    Writeln( '  CompUCString  : ', ct[CompUCString( 'Allan' , 'ALLAN' )] );
    Writeln( '  CompUCString  : ', ct[CompUCString( 'Peter' , 'ALLAN' )] );

    Writeln( '  Soundex       : ', Soundex( 'Allan' ),' ',Soundex( 'Anna' ) );
    Writeln( '  Soundex       : ', Soundex( 'vpascal' ),' ',Soundex( 'Hargreaves' ) );

    Writeln( '  MakeLetterSet : ', HexL( MakeLetterSet( 'vpascal' ) ));
    Writeln( '  MakeLetterSet : ', HexL( MakeLetterSet( 'Allan' ) ));
    Writeln( '  CompareLetSet : ', HexL( CompareLetterSets( MakeLetterSet( 'vpascal' ), MakeLetterSet( 'Allan' ))));

    s := 'Allan works on Virtual Pascal all day long';
    s1 := 'work';
    Writeln( '  Search        : ', Search(s[1],length(s),s1[1],length(s1) ));
    s1 := 'not!';
    Writeln( '  Search        : ', Search(s[1],length(s),s1[1],length(s1) ));
    s1 := 'PASCAL';
    Writeln( '  SearchUC      : ', SearchUC(s[1],length(s),s1[1],length(s1) ));
    s1 := 'not!';
    Writeln( '  SearchUC      : ', SearchUC(s[1],length(s),s1[1],length(s1) ));

    s1 := 'work';
    s2 := 'sleep';
    Writeln( '  Replace       : ', Replace(s[1],length(s),s1[1],length(s1),s2[1],length(s2),NewLen, 255, true));
    Writeln( '   s, NewLen is : ', s, ' ',NewLen );
    s1 := 'a';
    s2 := '<Old a>';
    Writeln( '  ReplaceUC     : ', ReplaceUC(s[1],length(s),s1[1],length(s1),s2[1],length(s2),NewLen, 255, true));
    Writeln( '   s, NewLen is : ', s, ' ',NewLen );

    BMMakeTable( s2, BT );
    Writeln( '  BMSearch      : ', BMSearch( s[1], length(s),BT,s2 ) );
    Writeln( '  BMSearch      : ', BMSearch( s[2], length(s),BT,s2 ) );
    Writeln( '  BMSearchUC    : ', BMSearch( s[1], length(s),BT,s2 ) );
    Writeln( '  BMSearchUC    : ', BMSearch( s[2], length(s),BT,s2 ) );

    { Check registers }
    asm
      cmp ebx,_ebx
      jne @@1
      cmp esi,_esi
      jne @@1
      cmp edi,_edi
      jne @@1
      mov byte ptr RegOk,True   { OK... }
    @@1:
    end;
    Writeln( '  Registers     : ',RegOK );
  end;
{$ENDIF}

{$IFDEF TSTOPSTRDEV}
Procedure TstOpStrDev;
  begin
    Writeln( 'OPSTRDEV:' );

    l1 := -3; l2 := 100;
    Write( TPStr, 'l1 = ', l1:5, ' l2 = ',l2:5 );
    ReadStr( s1 );
    Writeln( '  Write & ReadStr   : ',s1 );

    Write( TPStr, 'l1 = ', l1:5, ' l2 = ',l2:5, ' StringTest' );
    Writeln( '  Write & ReturnStr : ',ReturnStr);
  end;
{$ENDIF}

{$IFDEF TSTOPROOT}
Procedure TstOpRoot;
  Var
    pr1, pr2 : RootPtr;

  Procedure TstPointerStack;
    Var
      psp : PointerStackPtr;

    begin
      Writeln( '  PointerStack:');
      {$IFDEF OS2}
      pr1 := Ptr( $1234567 );
      pr2 := Ptr( $FEDCBA9 );
      {$ELSE}
      pr1 := Ptr( $1234, $567 );
      pr2 := Ptr( $FEDC, $BA9 );
      {$ENDIF}
      psp := New( PointerStackPtr, Init( 100 ) );
      With psp^ do
        begin
          Push( pr1 );
          Push( pr2 );
          Writeln( '    Size, SP, Status : ', Size,' ',SP,' ',GetStatus );
          Writeln( '    Peek (1,2)       : ',HexPtr( Peek(1) ),' ',HexPtr( Peek( 2 ) ));
          Writeln( '    PeekTop          : ',HexPtr( PeeKTop ) );
          Resize( 20 );
          {$IFDEF OS2}
          Pr2 := Ptr( $ABCD1234 );
          {$ELSE}
          Pr2 := Ptr( $ABCD, $1234 );
          {$ENDIF}
          Poke( Pr2, 2 );
          Writeln( '    Size, PeekTop    : ',Size,' ',HexPtr( PeekTop ) );
        end;
      Dispose( psp, Done );
    end;

  Procedure TstStaticQueue;
    Var
      qsp : StaticQueuePtr;
      s   : String;
      OK  : Boolean;

    begin
      Writeln( '  StaticQueue:');
      qsp := New( StaticQueuePtr, Init( 100, SizeOf( String ), True ) );
      With qsp^ do
        begin
          s1 := 'First String';
          s2 := 'Second String';
          s3 := 'Last String';
          PushHead( s1 );
          PushHead( s2 );
          PushHead( s3 );
          OK := False;
          PopTail( s );
          If s = s1 then
            begin
              PopTail( s );
              If s = s2 then
                begin
                  PopTail( s );
                  If s = s3 then
                    OK := True;
                end;
            end;
          Write( '    PushHead/PopTail - ' );
          If OK Then
            Writeln( 'OK' )
          else
            Writeln( 'Error' );

          PushTail( s1 );
          PushTail( s2 );
          PushTail( s3 );
          OK := False;
          PopHead( s );
          If s = s1 then
            begin
              PopHead( s );
              If s = s2 then
                begin
                  PopHead( s );
                  If s = s3 then
                    OK := True;
                end;
            end;
          Write( '    PushTail/PopHead - ' );
          If OK Then
            Writeln( 'OK' )
          else
            Writeln( 'Error' );
        end;
      Dispose( qsp, Done );
    end;

  Procedure TstBitSet;
    Const
      BitSetSize = 10000;
    Var
      bsp : BitSetPtr;
      i,x : Longint;

    begin
      Writeln( '  BitSet:' );
      bsp := New( BitSetPtr, Init( 10000 ) );
      With bsp^ do
        begin
          Writeln( '    MaxBits        : ',MaxBits );
          SetAll;
          Writeln( '    Set,   BitsSet : ',BitsSet );
          ClearAll;
          Writeln( '    Clear, BitsSet : ',BitsSet );
          For i := 0 to BitSetSize do
            If i mod 13 = 0 then
              SetBit( i );
          Writeln( '    Divisible by 13: ',BitsSet );
          For i := 0 to BitSetSize do
            If i mod 7 = 0 then
              ClearBit( i );
          Writeln( '    - but not by 7 : ',BitsSet );
          Writeln( '    First Clear,Set: ',FirstClear, ' ', FirstSet );
          Writeln( '    Last Clear,Set : ',LastClear, ' ', LastSet );
          Write  ( '    First Bits set : ' );
          x := FirstSet;
          For i := 1 to 10 do
            begin
              Write( x,' ' );
              x := NextSet( x );
            end;
          Writeln;
          Write  ( '    Last Bits Clear: ' );
          x := LastClear;
          For i := 1 to 8 do
            begin
              Write( x,' ' );
              x := PrevClear( x );
            end;
          Writeln;
          For i := 0 to BitSetSize do
            ToggleBit( i );
          Writeln( '    Toggled - Set  : ',BitsSet );
        end;
      Dispose( bsp, Done );
    end;

  Procedure TstLargeBitSet;
    Const
      BitSetSize = 10000;
    Var
      lbsp : LargeBitSetPtr;
      i,x : Longint;

    begin
      Writeln( '  LargeBitSet:' );
      lbsp := New( LargeBitSetPtr, Init( BitSetSize ) );
      With lbsp^ do
        begin
          Writeln( '    MaxBits        : ',MaxBits );
          SetAll;
          Writeln( '    Set,   BitsSet : ',BitsSet );
          ClearAll;
          Writeln( '    Clear, BitsSet : ',BitsSet );
          For i := 0 to BitSetSize do
            If i mod 13 = 0 then
              SetBit( i );
          Writeln( '    Divisible by 13: ',BitsSet );
          For i := 0 to BitSetSize do
            If i mod 7 = 0 then
              ClearBit( i );
          Writeln( '    - but not by 7 : ',BitsSet );
          Writeln( '    First Clear,Set: ',FirstClear, ' ', FirstSet );
          Writeln( '    Last Clear,Set : ',LastClear, ' ', LastSet );
          Write  ( '    First Bits set : ' );
          x := FirstSet;
          For i := 1 to 10 do
            begin
              Write( x,' ' );
              x := NextSet( x );
            end;
          Writeln;
          Write  ( '    Last Bits Clear: ' );
          x := LastClear;
          For i := 1 to 8 do
            begin
              Write( x,' ' );
              x := PrevClear( x );
            end;
          Writeln;
          For i := 0 to BitSetSize do
            ToggleBit( i );
          Writeln( '    Toggled - Set  : ',BitsSet );
        end;
      Dispose( lbsp, Done );
    end;

  Procedure TstStringDict;
    Var
      sdp : StringDictPtr;

    begin
      Writeln( '  StringDict:' );
      sdp := New( StringDictPtr, Init );
      With sdp^ do
        begin
          Add( 'Allan', 28 );
          Add( 'Sara', 26 );
          Add( 'Anna', 1 );
          Add( 'Olde Anna', 80 );
          Write( '    GetString(26)  : ' );
          If GetString( 26, s1 ) then
            Writeln( s1 )
          else
            Writeln( '- failed.');
          Update( 'Anna', 2 );
          Write( '    GetString(2)   : ' );
          If GetString( 2, s1 ) then
            Writeln( s1 )
          else
            Writeln( '- failed.');
          Remove( 'Allan' );
          Write( '    Remove/Status: ', GetStatus );
          If GetString( 28, s1 ) then
            Writeln( ' - found; should havd been removed!' )
          else
            Writeln( ' - ok');
        end;
      Dispose( sdp, Done );
    end;

  Procedure TstDosIdStream;
    Var
      disp : DosIdStreamPtr;

    begin
      Writeln( '  DosIdStream - Write:' );
      disp := New( DosIdStreamPtr, Init( 'TestFile', sCreate ) );
      With disp^ do
        begin
          s1 := 'The First Test String';
          s2 := 'The next test string...';

          Writeln( '    Status      : ', GetStatus );
          Write( S1[1], Length( S1 ) );
          Writeln( '    Write, Pos  : ', GetStatus,' ',GetPos );
          Seek( 100 );
          Writeln( '    Seek, Pos   : ', GetStatus,' ',GetPos );
          WriteString( S2 );
          Writeln( '    GetPos      : ', GetPos );
        end;
      Dispose( Disp, Done );

      Writeln( '  DosIdStream - Read:' );
      disp := New( DosIdStreamPtr, Init( 'TestFile', sOpenRead ) );
      With disp^ do
        begin
          Writeln( '    Status      : ', GetStatus );
          Read( S2[1], 18 ); s2[0] := #18;
          Writeln( '    Read, Pos   : ', GetStatus,' ',GetPos );
          Writeln( '    String2     : ', S2 );
          Seek( 100 );
          Writeln( '    Seek, Pos   : ', GetStatus,' ',GetPos );
          S1 := ReadString;
          Writeln( '    GetPos, S1  : ', GetPos,' ',S1 );
          Read( S3[1], 10 );
          Writeln( '    ReadError   : ',GetStatus );
        end;
      Dispose( Disp, Done );
    end;

  Procedure TstBufIdStream;
    Var
      bisp : BufIdStreamPtr;

    begin
      Writeln( '  BufIdStream - Write:' );
      bisp := New( BufIdStreamPtr, Init( 'TestFile', sCreate, 1024 ) );
      With bisp^ do
        begin
          s1 := 'The First Test String';
          s2 := 'The next test string...';

          Writeln( '    Status      : ', GetStatus );
          Write( S1[1], Length( S1 ) );
          Writeln( '    Write, Pos  : ', GetStatus,' ',GetPos );
          Seek( 100 );
          Writeln( '    Seek, Pos   : ', GetStatus,' ',GetPos );
          WriteString( S2 );
          Writeln( '    GetPos      : ', GetPos );
        end;
      Dispose( bisp, Done );

      Writeln( '  BufIdStream - Read:' );
      bisp := New( BufIdStreamPtr, Init( 'TestFile', sOpenRead, 1024 ) );
      With bisp^ do
        begin
          Writeln( '    Status      : ', GetStatus );
          Read( S2[1], 18 ); s2[0] := #18;
          Writeln( '    Read, Pos   : ', GetStatus,' ',GetPos );
          Writeln( '    String2     : ', S2 );
          Seek( 100 );
          Writeln( '    Seek, Pos   : ', GetStatus,' ',GetPos );
          S1 := ReadString;
          Writeln( '    GetPos, S1  : ', GetPos,' ',S1 );
          Read( S3[1], 10 );
          Writeln( '    ReadError   : ',GetStatus );
        end;
      Dispose( bisp, Done );
    end;

  Procedure TstPutGet;
    Const
      BitSetSize = 10000;
    Var
      isp : IdStreamPtr;
      sdp : RootPtr;
      bsp : RootPtr;
      i,x : Longint;
      ok  : Boolean;

    begin
      Writeln( '  Put objects on Stream:');
      isp := New( DosIdStreamPtr, Init( 'TestFile', SCreate ) );
      With isp^ do
        begin
          { Prepare the String Dictionary }
          StringDictStream( isp );
          sdp := New( StringDictPtr, Init );
          With StringDictPtr(sdp)^ do
            begin
              Add( 'Allan', 28 );
              Add( 'Sara', 26 );
              Add( 'Anna', 1 );
              Add( 'Olde Anna', 80 );
            end;
          isp^.PutPtr( sdp );

          { Prepare the BitSet }
          BitSetStream( isp );
          bsp := New( BitSetPtr, Init( BitSetSize ) );
          With BitSetPtr(bsp)^ do
            begin
              ClearAll;
              For i := 0 to BitSetSize do
                If i mod 13 = 0 then
                  SetBit( i );
              For i := 0 to BitSetSize do
                If i mod 7 = 0 then
                  ClearBit( i );
            end;
          isp^.PutPtr( bsp );

          Writeln( '    Put        : ',GetStatus ) ;
          Writeln( '    Size       : ',GetSize );
          Dispose( sdp, Done );
          Dispose( bsp, Done );
        end;
      Dispose( isp, Done );

      Writeln( '  Get off the Stream:');
      isp := New( DosIdStreamPtr, Init( 'TestFile', SOpenRead ) );
      With isp^ do
        begin
          StringDictStream( isp );
          BitSetStream( isp );
          sdp := isp^.GetPtr;
          bsp := isp^.GetPtr;

          Writeln( ' Get        : ',GetStatus );

          System.Write('    GetString  : ');
          ok := False;
          If sdp <> nil then
            If typeof( sdp^ ) = TypeOf( StringDict ) then
              If StringDictPtr( sdp )^.GetString( 28, s1 ) then
                ok := True;
          If OK then
            Writeln( 'OK : ',s1 )
          else
            Writeln( 'Failed.');

          System.Write  ( '    BitSet     : ' );
          ok := False;
          If bsp <> nil then
            If typeof( bsp^ ) = TypeOf( BitSet ) then
              ok := True;
          If OK then
            Writeln( 'OK : ', BitSetPtr( bsp )^.BitsSet )
          else
            Writeln( 'Failed.');

        end;
      Dispose( isp, Done );
    end;

  Procedure TstOpLibrary;
    Var
      olp  : OpLibraryPtr;
      sqp  : RootPtr;
      lbsp : RootPtr;

    begin
      Writeln( '  OpLibrary - Create:');
      olp := New( OpLibraryPtr, Create( 'TestLib.vp', 1024, 'TESTLIB', 100 ) );
      With olp^ do
        begin
          RegisterHier( LargeBitSetStream );
          RegisterHier( StaticQueueStream );

          lbsp := New( LargeBitSetPtr, Init( 10000 ) );
          With LargeBitSetPtr( lbsp )^ do
            begin
              SetAll;
              PutEntry( 'FULLBIT.SET', lbsp^ );
              ClearAll;
              PutEntry( 'EMPTYBIT.SET', lbsp^ );
            end;
          Dispose( lbsp, Done );
          Writeln( '    Put BitSet     : ', GetStatus );

          sqp := New( StaticQueuePtr, Init( 100, SizeOf( String ), False ) );
          With StaticQUeuePtr( sqp )^ do
            begin
              s1 := 'Tail Queue String';
              PushTail( s1 );
              s1 := 'Head Queue String';
              PushHead( s1 );
              PutEntry( 'QUEUE', sqp^ );
            end;
          Dispose( sqp, Done );

          Writeln( '    Put SQueue     : ', GetStatus );
        end;
      Dispose( olp, Done );

      Writeln( '  OpLibrary - Read:');
      olp := New( OpLibraryPtr, Init( 'TestLib.vp', sOpenRead, 1024, 'TESTLIB' ) );
      If ( olp = NIL ) or ( olp^.GetStatus <> 0 ) Then
        Writeln( '    Error opening library!' )
      else
        With olp^ do
          begin
            RegisterHier( LargeBitSetStream );
            RegisterHier( StaticQueueStream );

            lbsp := GetEntryPtr( 'EMPTYBIT.SET' );
            With LargeBitSetPtr( lbsp )^ do
              Writeln( '    Bitset1/St     : ', BitsSet,' ', GetStatus );
            Dispose( lbsp, Done );

            lbsp := GetEntryPtr( 'FULLBIT.SET' );
            With LargeBitSetPtr( lbsp )^ do
              Writeln( '    Bitset1/St     : ', BitsSet,' ', GetStatus );
            Dispose( lbsp, Done );

            sqp := GetEntryPtr( 'QUEUE' );
            With StaticQUeuePtr( sqp )^ do
              begin
                poptail( s1 );
                Writeln( '    Squeue/s1      : ',s1,' ',GetStatus );
                popHead( s1 );
                Writeln( '    Squeue/s2      : ',s1,' ',GetStatus );
              end;
            Dispose( sqp, Done );
          end;
      If olp <> nil then
        Dispose( olp, Done );

      Writeln( '  OpLibrary - Change and Pack:' );
      olp := New( OpLibraryPtr, Init( 'TestLib.vp', sOpen, 1024, 'TESTLIB' ) );
      If ( olp = NIL ) or ( olp^.GetStatus <> 0 ) Then
        Writeln( ' Error opening library!' )
      else
        With olp^ do
          begin
            RegisterHier( LargeBitSetStream );
            RegisterHier( StaticQueueStream );

            DeleteEntry( 'EMPTYBIT.SET' );

            Writeln( '    Needs Packing  : ', FileNeedsPacking );
            Writeln( '    FileSize       : ', PackedFileSize );
            Writeln( '    Current/Del-d  : ', CurrentEntries,' ',DeletedEntries );
            Pack;
            Writeln( '    Pack           : ', GetStatus );
            Writeln( '    Current/Del-d  : ', CurrentEntries,' ',DeletedEntries );

          end;
      Dispose( olp, Done );
    end;

  begin
    Writeln( 'OPROOT:' );

    { From OPROOT.IN2 }
    TstPointerStack;
    TstStaticQueue;
    TstBitSet;
    TstLargeBitSet;

    { From OPROOT.IN3 }
    TstStringDict;
    TstDosIdStream;
    TstBufIdStream;
    TstPutGet;

    { From OPROOT.IN4 }
    TstOpLibrary;
  end;
{$ENDIF TSTOPROOT}

{$IFDEF TSTOPDOS}
Procedure TstOpDos;

  Procedure TstDriveInfo;
    Var
      cAvail, cTotal, bps, spc : Word;
      Class : DiskClass;
      Ltr   : Char;

    begin
      Writeln( 'OPDOS:' );
      Writeln( '  Version Info:' );
      Writeln( '    Dos Version     : ', HexL( DosVersion ) );
      Writeln( '  Drive Info:' );
      Writeln( '    Logical Drives  : ', NumberOfDrives );
      SelectDrive( 'C' );
      Writeln( '    Default Drive C : ', DefaultDrive );
      SelectDrive( 'F' );
      Writeln( '    Default Drive F : ', DefaultDrive );
      Writeln( '    Drive t: Valid? : ', ValidDrive( 't' ) );
      Writeln( '    Drive R: Valid? : ', ValidDrive( 'R' ) );
      Writeln( '    Drive B: Phantom: ', IsPhantom( 'B' ) );
      Writeln( '    Phantoms Exist? : ', PhantomExists );
      Write(   '    DiskInfo/ C:    : ' );
      If GetDiskInfo( 3, cAvail, cTotal, bps, spc ) then
        Writeln( 'Clust Avl: ',cAvail:5, ' Clst: ',cTotal:5, ' Byt/Sct: ',bps, ' Sct/Clst: ',spc )
      else
        Writeln( '- failed!' );
      Write(   '    DiskInfo/ F:    : ' );
      If GetDiskInfo( ord('F')-Ord('A')+1, cAvail, cTotal, bps, spc ) then
        Writeln( 'Clust Avl: ',cAvail:5, ' Clst: ',cTotal:5, ' Byt/Sct: ',bps, ' Sct/Clst: ',spc )
      else
        Writeln( '- failed!' );
      Write(   '    DiskInfo/ R:    : ' );
      If GetDiskInfo( ord('R')-Ord('A')+1, cAvail, cTotal, bps, spc ) then
        Writeln( 'Clust Avl: ',cAvail:5, ' Clst: ',cTotal:5, ' Byt/Sct: ',bps, ' Sct/Clst: ',spc )
      else
        Writeln( '- failed!' );
      Write(   '    DiskInfo/ T:    : ' );
      If GetDiskInfo( ord('T')-Ord('A')+1, cAvail, cTotal, bps, spc ) then
        Writeln( 'Clust Avl: ',cAvail:5, ' Clst: ',cTotal:5, ' Byt/Sct: ',bps, ' Sct/Clst: ',spc )
      else
        Writeln( '- failed!' );
      Class := GetDiskClass( 'C', Ltr );
      Writeln( '    Disk Class C:   : "',Ltr,'" - ',ord( Class ) );
      Class := GetDiskClass( 'R', Ltr );
      Writeln( '    Disk Class R:   : "',Ltr,'" - ',ord( Class ) );
    end; { TstDriveInfo }

  Procedure TstFileInfo;
    Var
      Attr : Word;
      Res  : Byte;
      f    : File;
      s    : String;

    begin
      Writeln( '  File Information:' );
      Res := GetFileMode( 'c:\autoexec.bat', Attr );
      Writeln( '    GetFileMode ok  : ',Res:3, ' ', HexL( Attr ) );
      Res := GetFileMode( 'c:\does not.txt', Attr );
      Writeln( '    GetFileMode bad : ',Res:3, ' ', HexL( Attr ) );
      Assign( f, 'TestFile.vp' );
      Rewrite( f, 1 ); BlockWrite( f, Buffer, Sizeof( Buffer ) );
      Writeln( '    FlushDosBuffers : ',FlushDosBuffers( f ) );
      Close( f );
      Writeln( '    FileHandlesLeft : ',FileHandlesLeft );
      Writeln( '    FileHandlesOpen1: ',FileHandlesOpen( True ) );
      Writeln( '    FileHandlesOpen2: ',FileHandlesOpen( False ) );
      Write(   '    ParsePath Test1 : ' );
      s := '\OPRO\Test';
      If ParsePath( s, s1, s2 ) then
        Writeln( s1, ' ', s2 )
      else
        Writeln( '- Failed!' );
      Write(   '    ParsePath Test2 : ' );
      s := 'C:\fPrint.40\*.exe';
      If ParsePath( s, s1, s2 ) then
        Writeln( s1, ' ', s2 )
      else
        Writeln( '- Failed!' );
    end;

  Procedure TstPrint;
    begin
      Writeln( '  Print functions:' );
      Writeln( '    Print Installed : ',PrintInstalled );
      Writeln( '    SubmitPrintFile : ',SubmitPrintFile( 'Test.txt' ) );
    end;

  Procedure TstEnvironment;
    Var
      Env : EnvRec;
    begin
      Writeln( '  Environment:' );
      Writeln( '    GetEnvString 1  : ',GetEnvironmentString( 'OS2_Shell' ) );
      Writeln( '    GetEnvString 2  : ',GetEnvironmentString( 'RUNWORKPLACE' ) );
      Writeln( '    GetEnvString 3  : ',GetEnvironmentString( 'DoesNotExist' ) );

      MasterEnv( Env );
      Writeln( '    GetProgramStr   : ',GetProgramStr( Env ) );
      Writeln( '    SetProgramStr   : ',SetProgramStr( Env, 'ALLAN' ) );
      Writeln( '    SetEnvStr       : ',SetEnvStr( Env, 'PROMPT', 'TestPrompt' ) );
      Writeln( '    SetEnvStr       : ',SetEnvStr( Env, 'PROMPT', '$p!') );
      Writeln( '    GetEnvString    : ',GetEnvironmentString( 'PROMPT' ) );
{      DumpEnv( Env );}
      Writeln( '    ShellWithPrompt : ',
        ShellWithPrompt( '$_Press EXIT to return to program$_[$p]', nil ) );
    end;

  Procedure TstExecute;
    begin
      Writeln( '  Program Execution:' );
      Writeln( '    ExecDos ""     : ',ExecDos( '', True, nil ) );
      Writeln( '    ExecDos CMD    : ',ExecDos( 'Dir *.exe', True, nil ) );
      Writeln( '    ExecDos CMD    : ',ExecDos( 'xcopy *.pas *.src', True, nil ) );
    end;

  Procedure TstTextFile;
    Var
      f : Text;
    begin
      Writeln( '  Text File Handling:' );
      Assign( f, 'TextFile.vp' );
      Rewrite( f );
      Writeln( f, 'This is a test of a text file that is used for seeking' );
      Writeln( f, 'This is the second line of the file' );
      Writeln( '    File Position   : ', TextPos( f ) );
      Writeln( '    TextFlush/Write : ', TextFlush( f ) );
      Writeln( f, '- and this is the last line of the file - maybe.  It all depends.');
      Writeln( f );
      Writeln( f, '(Well, almost :-)' );
      Close (f );

      Reset( f );
      Readln( f, s1 );
      Writeln( '    TextSeek 30     : ', TextSeek( f, 30 ) );
      Readln( f, s1 );
      Writeln( '    Read String     : ', s1 );
      Writeln( '    File Position   : ', TextPos( f ) );
      Writeln( '    TextFlush/Read  : ', TextFlush( f ) );
      Writeln( '    TextSeek 169    : ', TextSeek( f, 169 ) );
      Readln( f, s1 );
      Writeln( '    Read String     : ', s1 );

      Writeln( '    Size of File    : ', TextFileSize( f ) );

      Close( f );
    end;

  Procedure TstConsoleIO;
    Var
      f : Text;

    begin
      Writeln( '  Console IO: ');
      Writeln( '    OpenStdDev      : ', OpenStdDev( f, StdOutHandle ) );
      Writeln( f, '    Test writing to StdOut' );
      Writeln( '    HandleIsCon In  : ', HandleIsConsole( StdInHandle ) );
      Writeln( '    HandleIsCon Out : ', HandleIsConsole( StdOutHandle ) );
      Writeln( '    HandleIsCon Err : ', HandleIsConsole( StdErrHandle ) );
      Writeln( '    HandleIsCon 100 : ', HandleIsConsole( 100 ) );
    end;

  Procedure TstFileCheck;
    begin
      Writeln( '  File Checking: ');
      Writeln( '    ExistFile 1 ok  : ', ExistFile( 'C:\autoexec.bat' ) );
      Writeln( '    ExistFile 2 no  : ', ExistFile( '\autoexec.bat' ) );
      Writeln( '    ExistFile 3 ok  : ', ExistFile( 'd:\os2\xcopy.exe' ) );
      Writeln( '    ExistFile 4 no  : ', ExistFile( 'C:\' ) );
      Writeln( '    ExistOnPath 1   : ', ExistOnPath( 'Xcopy.exe', s1 ) );
      Writeln( '    ExistOnPath 2   : ', ExistOnPath( 'copy.exe', s1 ) );
      Writeln( '    IsDirectory 1   : ', IsDirectory( 'C:\' ) );
      Writeln( '    IsDirectory 2   : ', IsDirectory( 'D:\OS2' ) );
      Writeln( '    IsDirectory 3   : ', IsDirectory( 'DOS' ) );
      Writeln( '    SameFile 1      : ', SameFile( 'd:\autoexec.bat', 'c:\autoexec.bat', w1 ) );
      Writeln( '    SameFile 2      : ', SameFile( 'c:\autoexec.bat', 'c:\autoexec.bat', w1 ) );
      Writeln( '    SameFile 3      : ', SameFile( 'Test', 'Hello.Not', w1 ) );
      Writeln( '    CopyFile 1      : ', CopyFile( 'c:\command.com', 'test.com', @Buffer, BufLen ) );
      Writeln( '    CopyFile 2      : ', CopyFile( 'c:\command.com', 'c:\test.com', @Buffer, BufLen ) );
    end;

  Procedure TstTime;
    Var
      i : Integer;
    begin
      Writeln( '  Time Functions: ');
      For i := 1 to 10 do
        Writeln( '    TimeMS is       : ', TimeMs );
    end;

  Procedure TstVolumeInfo;
    Var
      Vol : VolumeNameStr;
      mID : MediaIDType;

    begin
      Writeln( '  Volume Label Functions: ');
      w1 := GetVolumeLabel( 'C', Vol );
      Writeln( '    Volume Label C: : ',Vol,' (rc=',w1,')');
      w1 := GetVolumeLabel( 'F', Vol );
      Writeln( '    Volume Label F: : ',Vol,' (rc=',w1,')');
      w1 := GetVolumeLabel( 'X', Vol );
      Writeln( '    Volume Label X: : ',Vol,' (rc=',w1,')');
      Writeln( '    DeleteVolLbl C: : ', DeleteVolumeLabel( 'C' ) );
      w1 := GetVolumeLabel( 'C', Vol );
      Writeln( '    Volume Label C: : ',Vol,' (rc=',w1,')');
      Writeln( '    Set VolLbl C:   : ',SetVolumeLabel( 'C', 'MS-DOS_622' ) );
      w1 := GetVolumeLabel( 'C', Vol );
      Writeln( '    Volume Label C: : ',Vol,' (rc=',w1,')');

      w1 := GetMediaID( 'C', mID );
      Writeln( '    GetMediaID C:     ',
        mID.VolumeLabel,' ',HexL( mID.SerialNumber ),' (rc=',w1,')' );

      mID.VolumeLabel := 'Hello      ';
      mID.SerialNumber := $12345678;
      Writeln( '    SetMediaID C:     ',SetMediaID( 'C', mID ) );
      w1 := GetMediaID( 'C', mID );
      Writeln( '    GetMediaID C:     ',
        mID.VolumeLabel,' ',HexL( mID.SerialNumber ),' (rc=',w1,')' );
    end;

  begin
    TstDriveInfo;
    TstVolumeInfo;
    TstFileInfo;
    TstPrint;
    TstEnvironment;
    TstExecute;
    TstTextFile;
    TstConsoleIO;
    TstFileCheck;
    TstTime;
  end; { TstOpDos }

Procedure TstOpDate;
  begin
    Writeln( 'OPDATE:' );
    Writeln( '  ValidDate(30/10/95) : ', ValidDate(30,10,95) );
    Writeln( '  DMYToDate(30/10/95) : ', DMYToDate(30,10,95) );
    DateToDMY(144573, w1, w2, w3);
    Writeln( '  DatetoDMY(144573)   : ',w1,'/',w2,'/',w3  );
    Writeln( '  InternationalDate   : ',InternationalDate( True, True ) );
    Writeln( '  InternationalDate   : ',InternationalDate( False, False ) );
    Writeln( '  InternationalTime   : ',InternationalTime( True, True, True, True ) );
    Writeln( '  InternationalTime   : ',InternationalTime( True, True, False, False ) );
    Writeln( '  InternationalTime   : ',InternationalTime( False, True, False, True ) );
  end;

{$ENDIF TSTOPDOS}

{$IFDEF TSTOPCRT}
Procedure TstOpCrt;
  Procedure TstCursor;
    Var
      ch : Char;
      At : Byte;

    begin
      Writeln( '  Cursor Positioning:' );
      Writeln( '    GetCrtMode          : ',GetCrtMode );
{      Writeln( '    WhereXY             : ',HexL( WhereXY ) );
      Writeln( '    WhereXY             : ',HexL( WhereXY ) );}
      Writeln( '    WhereXAbs           : ',WhereXAbs );
      Writeln( '    WhereYAbs           : ',WhereYAbs );
      Writeln( '    WhereYAbs           : ',WhereYAbs );
      Gotoxy( 10,10 );
      Writeln( 'Gotoxy(10,10) is here');
      GotoxyABS( 10,11 );
      Writeln( 'GotoxyABS(10,11) is here');
      GotoxyABS( 10,11 );
      ch := ReadCharAtCursor;
      At := ReadAttrAtCursor;
      Writeln;
      Writeln( '    ReadChar at 10,11   : ',Ch );
      Writeln( '    ReadAttr at 10,11   : ',At );
      Writeln( '    CursorTypeSL        : ',HexL( CursorTypeSL ) );
      Writeln( '    CursorStartLine     : ',CursorStartLine );
      Writeln( '    CursorEndLine       : ',CursorEndLine );
      SetCursorSize( 10, 15 );
      Writeln( '    CursorTypeSL        : ',HexL( CursorTypeSL ) );
      SetCursorSize( 20, 10 );
      Writeln( '    CursorTypeSL        : ',HexL( CursorTypeSL ) );
      SetCursorSize( 15, 15 );
      Writeln( '    CursorTypeSL        : ',HexL( CursorTypeSL ) );
    end;

  Procedure TstKeyboard;
    Var
      x,y : Word;
      Stat : Byte;
    begin
      Writeln( '  Keyboard functions:' );
      Writeln( '    Readkey             : ',ReadKey );
      Writeln( '    Readkey             : ',ReadKey );
      Writeln( '    ReadkeyWord         : ',HexL( ReadKeyWord ) );
      Writeln( '    ReadkeyWord         : ',HexL( ReadKeyWord ) );
      Write(   '    KbdFlags (ALT=End)  : ' );
      x := WhereX;Y := WhereY;
      Repeat
        Gotoxy( x,y );
        Stat := KbdFlags;
        Write( HexB( Stat ) );
        Delay( 10 );
      Until Stat and kbdstf_Alt <> 0;
      Writeln;
    end;

  Procedure TstFastWrite;
    Const
      Attrs : FlexAttrs = ( $7, $F, $E, $70 );
    begin
      Writeln( '  FastWrite routines:' );
      FastWrite( 'This is FastWritten to 8,3', 3, 8, 06 );
      FastWrite( 'This is FastWritten to 11,4', 4, 11, $6E );
      FastFill( 50, '!', 5, 3, $1F );
      FastFillVert( 10, '!', 6, 1, $71 );
      FastFillVert( 10, '?', 6, 3, $30 );
      FastVert( 'FastVert to 8,7', 7, 8, $1F );

      FastFill( 50, '!', 7, 9, $1F );
      ChangeAttribute( 50, 7, 12, $F1 );

      ReadAttribute( 70, 7, 1, S1 );
      WriteAttribute( S1, 8, 2 );
      FastCenter( 'This is centered on line 10', 10, $06 );
      FastFlush( 'Right aligned on line 1', 1, $16 );
      FastFlush( 'Help', 2, $16 );
      Window( 10, 11, 20, 14 );
      TextAttr := $10; ClrScr;
      FastCenter( 'Center 2!', 2, $37 );
      FastCenter( 'This Is Too Long', 3, $37 );
      FastFlush( '1', 1, $4F );
      FastFlush( 'Help', 4, $4F );
      Window( 22, 11, 65, 14 );
      TextAttr := $20; ClrScr;
      FastCenter( '.', 1, $37 );
      FastCenter( 'Center 2!', 2, $37 );
      FastCenter( '- - - -Center 2!- - - -', 3, $37 );
      FastCenter( 'xxxxx- - - -Center 2!- - - -xxxxx', 4, $37 );
      FastFlush( 'Hello', 1, $16 );
      FastFlush( 'Help', 2, $16 );
      Window( 1,80,1,25 );

      FlexWrite( 'This '^A'is'^A' '^B'a'^B' '^C'test', 21, 3, Attrs );
      FlexWrite( ^A'This'^B' is '^C' another'^C' test'^B' of'^A' FlexWrite', 21, 3, Attrs );

      FastWriteAttr( 'TEST', 21, 50, #$1F#$2F#$3F#$4F );
    end;

  begin
    Writeln( 'OPCRT:' );
    TstCursor;
    Write( ' ... Press ENTER' );Readln;
    ClrScr;
    Window( 20,13, 60,24 );
    TstCursor;
    Window( 1,1, 80,25 );
    ClrScr;
    Writeln( 'OPCRT:' );
    TstKeyboard;
    ClrScr;
    Writeln( 'OPCRT:' );
    TstFastWrite;
  end;
{$ENDIF TSTOPCRT}

Procedure TstOpMouse;
  begin
  end;

Procedure TstOpInline;
  Var
    _ebx, _esi, _edi : Word;
    RegOK : Boolean;

  begin
    RegOK := false;
    Writeln( 'OPINLINE:' );
    { Check that everything works, AND that vital registers are }
    { preserved: ebx. esi. edi }
    asm
      mov _ebx,ebx
      mov _esi,esi
      mov _edi,edi
    end;



    { Check registers }
    asm
      cmp ebx,_ebx
      jne @@1
      cmp esi,_esi
      jne @@1
      cmp edi,_edi
      jne @@1
      mov byte ptr RegOk,True   { OK... }
    @@1:
    end;
    Writeln( '  Registers     : ',RegOK );
  end;

begin
{  TstOpInline;}
{  TstOpString;
  TstOpStrDev;
  TstOpRoot;
  TstOpDate;
  TstOpDos;
  TstOpCrt;
  TstOpMouse;}
end.


