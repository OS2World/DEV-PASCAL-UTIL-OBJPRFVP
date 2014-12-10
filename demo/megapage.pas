{========================================================================
 MEGAPAGE:
  Program for adding manual page numbers to OPRO help text
  The input index file is based on a text dump of the actual manual
  identifier index. Each line of this file is assumed to be in the form:

  IdName (UnitObjName) PgNum, PgNum, ...

  where the UnitObjName and its surrounding parentheses are optional.
  Up to 255 characters of page number information are stored. All characters
  of value SubstChar (default '#') are converted to spaces before adding the
  name to the Id table.

  This version reads all of the OP*.TXT help files and also reads a
  file named OPROIND.TXT that contains the index information. All
  of these should be in the current directory when MEGAPAGE is run. It
  creates a series of files with the .NEW extension that correspond to
  the OP*.TXT files. You should run MAKEHELP OPRO.NEW after MEGAPAGE
  is done to create a new version of OPRO.HLP.

  Based on Scott Samet's MEGAPAGE program.
  Modified by TurboPower Software, 4/94.
========================================================================}

{$R-,I+,S-,V-}
{$M 16384,150000,655360}
{.$DEFINE Debug}     {Define to create OPRO.ERR listing unmatched topics}

program MEGAPAGE;

uses
  DOS, OPCrt, OPRoot, OPString;

const
  BuffSize = 16384;
  SubstChar = '#';

type
  RefPtr = ^RefRec;
  RefRec = {Allocated at actual needed size}
    record
      Topic : Word;
      Page : String;
    end;

var
  Dict : StringDict;
  Bias : Word;
  ShortCnt : Word;
  LongCnt : Word;
  UnitObjCnt : Word;
  UnusedCnt : Word;
  OrigMem : LongInt;
{$IFDEF Debug}
  ErrF : Text;
{$ENDIF}

  procedure AllocTextBuff(var F : Text; Size : LongInt);
  const
    MaxSize = 65531;
  var
    P : Pointer;
  begin
    if Size > MaxSize then
      Size := MaxSize;
    Size := Size and (not 511);
    if Size <= 0 then
      Exit;
    GetMem(P, Size);
    if P = nil then
      Exit;
    SetTextBuf(F, P^, Size);
  end;

  procedure FreeTextBuff(var F : Text);
  begin
    with TextRec(F) do
      if BufPtr <> @Buffer then
        FreeMem(BufPtr, BufSize);
  end;

  procedure Abort(S : String);
  begin
    WriteLn(S);
    Halt(255);
  end;

  procedure CheckOpen(FN : PathStr);
  begin
    if InOutRes <> 0 then
      Abort('Unable to Open '+FN+'; Error='+Long2Str(IoResult));
  end;

  procedure LogLineNo(S : String; var N : Word; Force : Boolean);
  begin
    if Force or (N = 1) or (N mod 100 = 0) then
      Write(N:10, '   ', S);
    if Force then
      WriteLn
    else
      Write(^M);
    Inc(N);
  end;

  procedure LoadRefs(FN : PathStr);
  type
    RefStr = String[79];
  var
    LineCnt : Word;
    Test : RefPtr;
    RefFile : Text;
    Top : RefStr;
    UnitObjName : RefStr;
    ILine : String;
    Pg : String;

    procedure ParseLine(var ILine, Pg : string;
                        var Top, UnitObjName : RefStr);
    var
      CPos : Word;
      I : Word;
    begin
      CPos := pos(' ', ILine);
      Top := copy(ILine, 1, CPos-1);
      for I := 1 to Length(Top) do
        if Top[I] = SubstChar then
          Top[I] := ' ';
      Delete(ILine, 1, CPos);
      ILine := Trim(ILine);
      CPos := pos('(', ILine);
      if CPos = 0 then
        UnitObjName := ''
      else begin
        delete(ILine, 1, CPos);
        CPos := pos(')', ILine);
        UnitObjName := TrimLead(copy(ILine, 1, CPos-1));
        delete(ILine, 1, CPos);
        ILine := TrimLead(ILine);
      end;
      Pg := ILine;
    end;

    function MakeRef(Pg : String) : RefPtr;
    var
      Ref : RefPtr;
    begin
      GetMem(Ref, SizeOf(RefRec)-SizeOf(String)+Length(Pg)+1);
      New(Ref);
      if Ref = nil then
        RunError(203);
      with Ref^ do begin
        Topic := 0;
        Page := Pg;
      end;
      MakeRef := Ref;
    end;

    procedure CheckDictStatus(Name : String);
    var
      I : Word;
    begin
      I := Dict.GetStatus;
      if I = 10008 then
        RunError(203);
      if (0 < I) and (I < 20000) then
        Abort('Unable to add '+Name);
    end;

  begin
    WriteLn;
    WriteLn('Loading Identifiers:');
    Assign(RefFile, FN);
    AllocTextBuff(RefFile, 2*BuffSize);
    {$I-}
    Reset(RefFile);
    {$I+}
    if IoResult <> 0 then
      Abort('Unable to open '+FN);

    LineCnt := 1;
    while not Eof(RefFile) do begin
      ReadLn(RefFile, ILine);
      LogLineNo(FN, LineCnt, False);
      if (ILine <> '') and (ILine[1] <> ';') then begin
        ParseLine(ILine, Pg, Top, UnitObjName);
        if Top <> '' then begin
          { Attempt to add to Short Dictionary }
          if Dict.Member(Top, LongInt(Test)) then begin
            if Test <> nil then begin
              {Duplicate Top, second time nils the RefPtr}
              FreeMem(Test, SizeOf(RefRec)-SizeOf(String)+Length(Test^.Page)+1);
              Dict.Update(Top, LongInt(nil));
            end;
          end else begin
            { Unique reference - add to table }
            Dict.Add(Top, LongInt(MakeRef(Pg)));
            CheckDictStatus(Top);
            Inc(ShortCnt);
          end;
          if UnitObjName <> '' then begin
            { Add Object Name to Short Dictionary }
            UnitObjName := UnitObjName+'.';
            if not Dict.Member(UnitObjName, LongInt(Test)) then begin
              Dict.Add(UnitObjName, LongInt(nil));
              CheckDictStatus(UnitObjName);
              Inc(UnitObjCnt);
            end;
          end;
        end;
      end;
    end;
    Close(RefFile);
    FreeTextBuff(RefFile);
    LogLineNo(FN, LineCnt, False);

    Assign(RefFile, FN);
    AllocTextBuff(RefFile, 2*BuffSize);
    {$I-}
    Reset(RefFile);
    {$I+}
    if IoResult <> 0 then
      Abort('Unable to open '+FN);

    LineCnt := 1;
    while not Eof(RefFile) do begin
      ReadLn(RefFile, ILine);
      LogLineNo(FN, LineCnt, False);
      if (ILine <> '') and (ILine[1] <> ';') then begin
        ParseLine(ILine, Pg, Top, UnitObjName);
        if Top <> '' then
          if UnitObjName <> '' then
            if Dict.Member(Top, LongInt(Test)) then
              if Test = nil then begin
                { Duplicate entries in short dictionary - create long entry }
                Inc(LongCnt);
                Dict.Add(UnitObjName+'.'+Top, LongInt(MakeRef(Pg)));
                CheckDictStatus(UnitObjName+'.'+Top);
              end;
      end;
    end;
    Close(RefFile);
    FreeTextBuff(RefFile);
    LogLineNo(FN, LineCnt, True);
  end;

  function FormatReference(Ref : RefPtr) : String;
  begin
    if Ref^.Page = '' then
      FormatReference := ''
    else
      FormatReference := ^M^J';!MANPAGE'^M^J'Reference: Pg '+Ref^.Page;
  end;

  function ShowUnused(SPtr : StringPtr; Value : LongInt;
                      SDPtr : StringDictPtr) : Boolean; far;
  begin
    if Value <> 0 then
      with RefPtr(Value)^ do
        if Topic = 0 then
          if SPtr^[Length(SPtr^)] <> '.' then begin
{$IFDEF Debug}
            WriteLn(ErrF, SPtr^);
{$ENDIF}
            inc(UnusedCnt);
          end;
    ShowUnused := True;
  end;

  procedure ScanFile(FNIn : PathStr; NestLvl : Word);
  label
    NextLine;
  var
    Match : RefPtr;
    I : Word;
    J : Word;
    TopicNo : Word;
    LineNo : Word;
    IFile : Text;
    OFile : Text;
    ILine : String;
    RefLine : String;
    Temp : String;
    ScanTop : String;
    FNOut : PathStr;
    ShowFN : PathStr;
  begin
    FNIn := FExpand(FNIn);
    FNOut := JustPathName(FNIn)+'\'+JustName(FNIn)+'.NEW';
    ShowFN := LeftPad(JustFileName(FNIn), 12)+' -> '+JustFileName(FNOut);

    Assign(IFile, FNIn);
    Assign(OFile, FNOut);
    if NestLvl > 0 then begin
      AllocTextBuff(IFile, BuffSize);
      AllocTextBuff(OFile, BuffSize);
    end;
    {$I-}
    Reset(IFile);
    {$I+}
    CheckOpen(FNIn);
    Rewrite(OFile);
    CheckOpen(FNOut);

    ScanTop := '';
    RefLine := '';
    LineNo := 1;

    while not Eof(IFile) do begin
      ReadLn(IFile, ILine);
      LogLineNo(ShowFN, LineNo, False);
      Temp := StUpCase(ILine);
      if (ILine <> '') and (Temp[1] = ';') then begin
        if RefLine <> '' then begin
          WriteLn(OFile, RefLine);
          RefLine := '';
        end;
        if Temp = ';!MANPAGE' then begin
          {filter out existing manual page references}
          if not Eof(IFile) then begin
            ReadLn(IFile, ILine);
            LogLineNo(ShowFN, LineNo, False);
          end;
          goto NextLine;
        end;
      end;

      if Copy(Temp, 1, 6) = '!BIAS ' then begin
        Val(Trim(Copy(Temp, 6, 255)), I, J);
        if J = 0 then
          Bias := I;

      end else if Copy(Temp, 1, 9) = '!INCLUDE ' then begin
        Temp := Trim(Copy(Temp, 10, 255));
        ILine := Copy(ILine, 1, 9)+JustPathName(Temp)+JustName(Temp)+'.NEW';
        ScanFile(Temp, NestLvl+1);

      end else if Copy(Temp, 1, 7) = '!TOPIC ' then begin
        if RefLine <> '' then begin
          WriteLn(OFile, RefLine);
          RefLine := '';
        end;
        ScanTop := '';
        Temp := Trim(Copy(ILine, 8, 255));
        I := Pos(' ', Temp);
        if I > 0 then begin
          Val(Copy(Temp, 1, I-1), TopicNo, J);
          Inc(TopicNo, Bias);
          Temp := Trim(Copy(Temp, I, 255));
          if Dict.Member(Temp, LongInt(Match)) then begin
            if Match = nil then
              ScanTop := '.'+StUpCase(Temp)
            else begin
              RefLine := FormatReference(Match);
              Match^.Topic := TopicNo;
              ScanTop := '';
            end;
          end else begin
            I := Pos(',', Temp);
            if I <> 0 then begin
              {Topic name of form Method,Object}
              Temp := Copy(Temp, I+1, 255)+'.'+Copy(Temp, 1, I-1);
              if Dict.Member(Temp, LongInt(Match)) then begin
                RefLine := FormatReference(Match);
                Match^.Topic := TopicNo;
                ScanTop := '';
              end;
            end;
          end;
        end;

      end else if (ScanTop <> '') then begin
        I := Pos(ScanTop, Temp);
        if I > 0 then begin
          J := I-1;
          if Temp[J] = ^E then begin
            Delete(Temp, J, 1);
            Dec(J);
          end;
          while (J > 0) and (Temp[J] in ['A'..'Z', 'a'..'z', '0'..'9', '.']) do
            Dec(J);
          while (I <= Length(Temp)) and (Temp[I] in ['A'..'Z', 'a'..'z', '0'..'9', '.']) do
            Inc(I);
          Temp := Copy(Temp, J+1, I-J-1);
          if Dict.Member(Temp, LongInt(Match)) then begin
            RefLine := FormatReference(Match);
            Match^.Topic := TopicNo;
            ScanTop := '';
          end;
        end;
      end;
      WriteLn(OFile, ILine);
NextLine:
    end;
    if RefLine <> '' then
      WriteLn(OFile, RefLine);

    Close(IFile);
    LogLineNo(ShowFN, LineNo, True);
    FreeTextBuff(IFile);
    Close(OFile);
    FreeTextBuff(OFile);

    if NestLvl = 0 then begin
{$IFDEF Debug}
      Assign(ErrF, ForceExtension(FNIn, 'ERR'));
      AllocTextBuff(ErrF, BuffSize);
      Rewrite(ErrF);
{$ENDIF}
      UnusedCnt := 0;
      Dict.VisitAll(ShowUnused);
{$IFDEF Debug}
      Close(ErrF);
{$ENDIF}
      WriteLn;
      WriteLn('Summary:');
      WriteLn(ShortCnt:10,         '  Identifiers');
      WriteLn(LongCnt:10,          '  Qualified identifiers');
      WriteLn(UnitObjCnt:10,       '  Object or unit names');
      WriteLn(OrigMem-MemAvail:10, '  Bytes used for identifiers');
      WriteLn(MemAvail:10,         '  Bytes left');
      WriteLn(UnusedCnt:10,        '  Unmatched dictionary entries');
      WriteLn;
      WriteLn('Now run MAKEHELP ', JustFileName(FNOut));
    end;
  end;

begin
  WriteLn('MEGAPAGE - Add manual page info to POPHELP files for Object Professional');
  WriteLn('Based on Scott Samet''s original version of MEGAPAGE');

  OrigMem := MemAvail;
  Dict.InitCustom(8192);
  ShortCnt := 0;
  LongCnt := 0;
  UnitObjCnt := 0;
  LoadRefs('OPROIND.TXT');

  WriteLn;
  WriteLn('Converting Help files:');
  Bias := 0;
  ScanFile('OPRO.TXT', 0);
end.


