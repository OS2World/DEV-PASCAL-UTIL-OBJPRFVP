{$R-,S-,I-,V-,F-,B-}

{*********************************************************}
{*                  MAKEHELP.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987,1992.      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{Thanks to:
 Craig L. Roberts for his original work on the topic cross-reference report.
 Mitch Milam for contributing the available topic numbers, topic locations,
 and used files reports.
 Jonathan Ide for his work on topic constants.}

program MakeHelp;
  {-Build indexed binary help file from text file}
uses
  Use32,
  {$IFDEF VIRTUALPASCAL}
  VpSysLow,
  {$ENDIF}
  Dos,
  OpDos,
  OpConst,    {!!.20}
  OpRoot,
  OpString,
  OpInline,
  OpCrt,
  OpWindow,
  OpPick,
  OpHelp;

const
  FileBuffSize = 4096;       {Size of input and output file buffers}
  CommandMark = '!';         {Marks help metacommand in text file}
  CommentMark = ';';         {At start of line, marks comment in text file}
  MaxCompIndex = 14;         {Xlate table is 0..14}
  MaxIncludeNest = 2;        {Maximum depth of include nesting} {!!.03}
  MaxFontStack = 10;         {Maximum nesting of fonts}

type
  FileBuff = array[1..FileBuffSize] of Byte;
  String80 = string[80];
  CountArray = array[0..255] of Word;
  StringPtr = ^string;
  FileArray = array[0..MaxIncludeNest] of Text;
  LineArray = array[0..MaxIncludeNest] of LongInt;
  FontStack = array[1..MaxFontStack] of Char;

  XrefRec   =
    record
      FrTopic : Word;
      ToTopic : Word;
    end;
  XrefArray = array[1..(65520 div SizeOf(XrefRec))] of XrefRec;
  XrefArrayPtr = ^XrefArray;
  XlateTable = array[#0..#255] of Byte;    {!!.12}

var
  FOutFile : Text;           {Files used file}               {!!.03}
  LOutFile : Text;           {Topic locations file}          {!!.03}
  FOutName : String80;       {FIles used file name}          {!!.03}
  LOutName : String80;       {Topic locations file name}     {!!.03}
  InName : String80;         {Input file name}
  OutName : String80;        {Output file name}
  InF : FileArray;           {Input files (with include files)}
  TxtF : Text absolute InF;  {Text output file used for reports}
  OutF : file;               {Output file}
  InBuff : FileBuff;         {Buffer for input text}
  OutBuff : FileBuff;        {Buffer for binary output}
  OutPos : Word;             {Bytes used in output buffer}
  IncLev : Word;             {Include nesting level}
  LineNum : LineArray;       {Current input line number}
  TotLines : LongInt;        {Total number of lines}

  Hdr : HelpHeader;          {Header of help file}
  OP : NameIndexPtr;         {Name buffer offsets and flags}
  CP : CharArrayPtr;         {Packed topic name buffer}
  TM : WordArrayPtr;         {Points to sorted topic map}
  FI : HelpIndexPtr;         {Points to help index}
  IK : WordArrayPtr;         {Points to index key override}
  XR : XrefArrayPtr;         {Points to xref array}
  LT : WordArrayPtr;         {Points to topic size array}

  C : String80;              {Command or command parameter}
  Clen : Byte absolute C;    {Length of parameter}
  S : string;                {Raw input line}
  Slen : Byte absolute S;    {Length of input line}
  Spos : Word;               {Position in input line} {!!.03}

  SectPos : LongInt;         {File offset of current section}
  CurSect : Word;            {Current section number}
  TextWid : Word;            {Max characters in a line} {!!.03}
  LineLen : Word;            {Current line width}       {!!.03}
  SectLen : Word;            {Bytes in current section}
  CompLen : Word;            {Compressed bytes in current section}
  NameOfs : Word;            {Current offset in name buffer}
  XrefCnt : Word;            {Total number of xrefs}
  CurXref : Word;            {Current xref}
  TopicBias : LongInt;       {Added to topic number references}
  CompMargin : Word;         {Buffer size margin for decompression} {!!.11}
  BufferWithMargin : Word;   {Max buffer size including margin}     {!!.11}
  HighestTopicPass2 : LongInt; {Highest topic number for 2nd pass}  {!!.12}

  PickTopicPending : Boolean; {Last topic possibly will be shown on pick list}
  MaxUncompSect : Word;      {Section of biggest topic, uncompressed}
  MaxUncompSize : Word;      {Size of biggest topic, uncompressed}
  MaxCompSect : Word;        {Section of biggest topic, compressed}
  MaxCompSize : Word;        {Size of largest compressed topic}
  MaxLineSect : Word;        {Section of longest line}
  TotalSect : LongInt;       {Total uncompressed bytes}
  TotalComp : LongInt;       {Total compressed bytes}

  MaxXrefs : Word;           {Maximum xrefs in one section}
  XrefsInSect : Word;        {Xrefs in current section}
  MaxXrefSect : Word;        {Section with most xrefs}

  Count : CountArray;        {Count for each character}
  I : Word;                  {Index used for compression}
  B : Word;                  {Index used for count table position}
  MFB : Word;                {Most frequent byte}
  TranslateTable : XlateArray; {Easy to access xlate table}
  TranslateTable2 : XlateTable; {Expanded translation table} {!!.12}

  Warnings : Word;           {Number of warnings reported}
  WriteWarnings : Boolean;   {True to write wrap warnings}
  Nibble : Boolean;          {True when a nibble is pending}
  InOrder : Boolean;         {True when a sort section is in sort order}
  InXref : Boolean;          {True while in midst of xref item}
  Wrapping : Boolean;        {True when word wrapping}
  StoreXrefs : Boolean;      {True when Xref array is maintained}
  XrefList : Boolean;        {True when xref report requested}
  SortXref : Boolean;        {True when xref report should be sorted}
  UnreachList : Boolean;     {True when unreachable topics report requested}
  SearchList : Boolean;      {True when searchable topics report requested}
  SizeList : Boolean;        {True when topic size report requested}
  AvailList : Boolean;       {True when available topic number report requested} {!!.03}
  WhereList : Boolean;       {True when topic location list report requested}    {!!.03}
  FilesList : Boolean;       {True when file list report requested}              {!!.03}

  Stack : FontStack;         {Stack of attributes currently active}
  StackP : Word;             {Current stack pointer}

  ConstDict : StringDict;    {String dictionary for const definitions}
  DoingDict : Boolean;       {True while we're in const section of file} {!!.03}
const
  MaxConstNameLength = 32;   {Arbitrary limit} {!!.03}

  procedure WriteCopyright;
    {-Write the copyright line}
  begin
    WriteLn('Help Compiler. Copyright (c) 1987,92 by TurboPower Software. Version 1.30');
    WriteLn;
  end;

  procedure Error(Msg : string);
    {-Write error message and halt}
  begin
    WriteLn(^M'ERROR: ', Msg);
    Halt(1);
  end;

  procedure EraseOutFile;
    {-Close and erase output file}
  var
    IO : Word;
  begin
    Close(OutF);
    Erase(OutF);
    IO := IoResult;
  end;

  function FileName(var F : Text) : String;
    {-Return name of file}
  var
    NLen : Byte;
  begin
    with TextRec(F) do begin
      NLen := 0;
      while Name[NLen] <> #0 do begin
        FileName[NLen+1] := Name[NLen];
        inc(NLen);
      end;
      FileName[0] := Char(NLen);
    end;
  end;

  procedure WarnLine(Msg : string);
    {-Report error message, but continue}
  var
    KW : Word;
  begin
    if WriteWarnings then begin
      WriteLn(^M'WARNING: ', Msg);
      WriteLn('File: ', FileName(Inf[IncLev]));
      WriteLn('Line number: ', LineNum[IncLev]);
      WriteLn(S);
      Inc(Warnings);
      Write('Press any key to continue, <Esc> to quit');
      KW := ReadKeyWord;
      if KW = $011B then begin
        WriteLn;
        EraseOutFile;
        Halt(1);
      end else begin
        Write(^M);
        ClrEol;
      end;
    end;
  end;

  procedure ErrorLine(Msg : string);
    {-Report error position and message}
  begin
    WriteLn(^M'ERROR: ', Msg);
    WriteLn('File: ', FileName(Inf[IncLev]));
    WriteLn('Line number: ', LineNum[IncLev]);
    WriteLn(S);
    EraseOutFile;
    Halt(1);
  end;

  procedure FlushTextIn(var F : Text);
    {-Flush text file opened for reading}
{$IFDEF VIRTUALPASCAL}
  var
    Bytes : LongInt;
  begin
    with TextRec(F) do begin
      Bytes := TextPos( F );

      SysFileSeek(Handle, Bytes, 0, Bytes);
{$ELSE}
  type
    LH = record L, H : Word; end;
  var
    Bytes : LongInt;
    Regs : Registers;
  begin
    with TextRec(F), Regs do begin
      Bytes := LongInt(BufPos)-BufEnd;
      if Bytes = 0 then
        Exit;

      {Position file pointer past last data used}
      AX := $4201;
      BX := Handle;
      CX := LH(Bytes).H;
      DX := LH(Bytes).L;
      MsDos(Regs);
{$ENDIF}

      {Mark buffer empty}
      BufPos := 0;
      BufEnd := 0;
    end;
  end;

  procedure OpenInf(Name : String80);
    {-Open input file}
  begin
    if IncLev > 0 then
      FlushTextIn(InF[IncLev-1]);
    Assign(InF[IncLev], Name);
    Reset(InF[IncLev]);
    if IoResult <> 0 then
      Error(Name+' not found');
    LineNum[IncLev] := 0;
    Write(^M, CharStr(' ', 64), ^M, Pad(StUpcase(Name), 13), 0:5);
  end;

  procedure CloseInf;
    {-Close input file}
  begin
    WriteLn(^H^H^H^H^H, LineNum[IncLev]:5);
    Close(InF[IncLev]);
    inc(TotLines, LineNum[IncLev]);
    if IncLev > 0 then begin
      dec(IncLev);
      SetTextBuf(InF[IncLev], InBuff, FileBuffSize);
      Write(Pad(StUpcase(FileName(InF[IncLev])), 13), LineNum[IncLev]:5);
    end;
  end;

  procedure Initialize;
    {-Prepare for analysis of help file}
  var
    I : Word;
    Arg : string[127];
  begin
    WriteWarnings := True;
    XrefList := False;
    SortXref := False;
    StoreXrefs := False;
    UnreachList := False;
    SearchList := False;
    SizeList := False;
    InName := '';
    OutName := '';
    AvailList := False;         {!!.03}
    WhereList := False;         {!!.03}
    FilesList := False;         {!!.03}
    LOutName := '';             {!!.03}
    FOutName := '';             {!!.03}

    for I := 1 to ParamCount do begin
      Arg := ParamStr(I);
      if (Arg[1] = '/') or (Arg[1] = '-') then begin
        if Length(Arg) <> 2 then
          Error('Invalid command line option');
        case Upcase(Arg[2]) of
          'A' :                       {!!.03}
            AvailList := True;
          'B' :
            SizeList := True;
          'F' :
            FilesList := True;        {!!.03}
          'L' :
            begin
              StoreXrefs := True;
              XrefList := True;
            end;
          'Q' :
            WriteWarnings := False;
          'S' :
            SortXref := True;
          'T' :
            SearchList := True;
          'U' :
            begin
              StoreXrefs := True;
              UnreachList := True;
            end;
          'W' :                       {!!.03}
            WhereList := True;
        else
          Error('Invalid command line option');
        end;
      end else if Length(InName) = 0 then
        InName := DefaultExtension(StUpcase(CleanPathName(Arg)), 'TXT')
      else if Length(OutName) = 0 then
        OutName := StUpcase(CleanPathName(Arg))
      else
        Error('Too many filenames specified');
    end;

    if Length(InName) = 0 then begin
      WriteLn('Usage: MAKEHELP [Options] InFile [OutFile]'^M^J);
      WriteLn('  An extension of .TXT is assumed for InFile.');
      WriteLn('  If OutFile is not specified, MAKEHELP writes to InFile.HLP.');
      WriteLn;
      WriteLn('Options:');
      WriteLn('  /A  write list of available topic numbers to InFile.AVL');   {!!.03}
      WriteLn('  /B  write list of biggest topics to InFile.SIZ');
      WriteLn('  /F  write list of files used in creating the help file to InFile.FIL');   {!!.03}
      WriteLn('  /L  write topic cross-reference listing to InFile.LST');
      WriteLn('  /Q  no warnings');
      WriteLn('  /S  sort cross-reference list by index order');
      WriteLn('  /T  write list of searchable topics to InFile.TOP');
      WriteLn('  /U  write list of unreachable topics to InFile.UNR');
      WriteLn('  /W  write list of topic file locations to InFile.LOC');      {!!.03}
      Halt(1);
    end;

    if Length(OutName) = 0 then
      OutName := ForceExtension(InName, 'HLP');

    if InName = OutName then
      Error('Input and output filenames must differ');

    Assign(OutF, OutName);
    Rewrite(OutF, 1);
    if IoResult <> 0 then
      Error('Cannot create '+OutName);

    WriteLn('Pass 1 ...........');
    IncLev := 0;
    OpenInf(InName);

    {Default help header}
    FillChar(Hdr, SizeOf(HelpHeader), 0);

    with Hdr do begin
      ID := LongInt(HelpId);
      WindowWidth := 40;
    end;

    {No pick topic pending}
    PickTopicPending := False;

    {No topic bias yet}
    TopicBias := 0;
    CurSect := 0;        {!!.11}

    {No warnings or xrefs yet}
    Warnings := 0;
    TotLines := 0;
    XrefCnt := 0;
    CurXref := 0;

    {Initialize character frequency count array}
    FillChar(Count, SizeOf(Count), 0);

    ConstDict.Init;     {!!.03}
    DoingDict := False; {!!.03}
  end;

  procedure ReadTextLine;
    {-Read next line from help text}
  begin
    Inc(LineNum[IncLev]);
    ReadLn(InF[IncLev], S);
    if IoResult <> 0 then
      ErrorLine('Error reading from '+InName);
    if Slen = 0 then
      S[1] := #0;
    if LineNum[IncLev] and $0F = 0 then
      Write(^H^H^H^H^H, LineNum[IncLev]:5);
  end;

  procedure SkipWhite;
    {-Advance Spos past white space}
  begin
    while (Spos <= Slen) and (S[Spos] <= ' ') do
      Inc(Spos);
  end;

  procedure ParseWord(var C : string; MaxLen : Byte);
    {-Parse next word from S, returning it in C}
  var
    Clen : Byte absolute C;
  begin
    SkipWhite;
    Clen := 0;
    while (Spos <= Slen) and (S[Spos] > ' ') and (S[Spos] <> CommentMark)
    do begin
      if Clen < MaxLen then begin
        Inc(Clen);
        C[Clen] := S[Spos];
      end;
      Inc(Spos);
    end;
  end;

  function ParseNumber(Name : string) : Word;
    {-Parse a word from the line}
  var
    C : string[MaxConstNameLength]; {!!.03}
    L : LongInt;                    {!!.03}
    N : Word;
  begin
    ParseWord(C, MaxConstNameLength); {!!.03}
    if Length(C) = 0 then
      ErrorLine(Name+' expected');
    if not Str2Word(C, N) then
      if ConstDict.Member(StUpcase(C), L) then  {!!.03}
         N := L                                 {!!.03}
      else                                      {!!.03}
        ErrorLine('Invalid '+Name+' specified');
    ParseNumber := N;
  end;

  function ParseLongInt(Name : string) : LongInt;
    {-Parse a word from the line}
  var
    C : string[MaxConstNameLength]; {!!.03}
    N : LongInt;
  begin
    ParseWord(C, MaxConstNameLength); {!!.03}
    if Length(C) = 0 then
      ErrorLine(Name+' expected');
    if not Str2Long(C, N) then
      if not ConstDict.Member(StUpcase(C), N) then {!!.03}
        ErrorLine('Invalid '+Name+' specified');
    ParseLongInt := N;
  end;

  procedure ParseConstDef; {!!.03}
    {-Parse a constant definition for string dictionary}
  var
    ConstName : string[MaxConstNameLength];
    ConstValue : LongInt;
    Dummy : string;
    SaveSPos : Word;
  begin
    SPos := 1;

    ParseWord(ConstName, MaxConstNameLength);
    if ConstName = '' then
      Exit;
    ConstName := StUpcase(ConstName);

    SaveSPos := SPos;
    ParseWord(Dummy, 255);
    if (Dummy <> '=') and (Dummy <> ':=') then
      SPos := SaveSPos;
    ConstValue := ParseLongInt('Constant definition for ' + ConstName);

    {check for existing constant with same value}
    if ConstDict.GetString(ConstValue, Dummy) then
      if ConstName = Dummy then
        {don't add it again}
        Exit
      else
        {not a duplicate, a conflict}
        ErrorLine(ConstName+' constant has same value as '+Dummy);

    {add it to the dictionary}
    ConstDict.Add(ConstName, ConstValue);
    case ConstDict.GetStatus mod 10000 of                         {!!.13}
      ecDupString :                                               {!!.13}
        ErrorLine(ConstName+' used more than once as constant');  {!!.13}
      ecOutOfMemory :                                             {!!.13}
        ErrorLine(emInsufficientMemory);                          {!!.13}
    end;                                                          {!!.13}
  end;

  function ClassifyCommand(C : string) : Word;
    {-Classify valid help metacommands}
  const
    NumCommands = 13; {!!.03}
    CommandNames : array[1..NumCommands] of string[5] =
    ('TOPIC', 'LINE', 'PAGE', 'WIDTH', 'INDEX', 'NOIND',
     'INCLU', 'WRAP', 'NOWRA', 'SCROL', 'BIAS', 'NOSEA',
     'CONST'); {!!.03}
  var
    I : Integer;
  begin
    C := StUpcase(Copy(C, 1, 5));
    for I := 1 to NumCommands do
      if C = CommandNames[I] then begin
        ClassifyCommand := I;
        Exit;
      end;
    ClassifyCommand := 0;
  end;

  procedure BlockWriteChk(var B; Bytes : Word);
    {-Write a block to output and error check}
  var
    BytesWritten : Word;
  begin
    BlockWrite(OutF, B, Bytes, BytesWritten);
    if (IoResult <> 0) or (BytesWritten <> Bytes) then
      Error('Error writing to '+OutName);
  end;

  procedure WriteHeaders;
    {-Write the binary header structures to the help file}
  begin
    with Hdr do begin
      BlockWriteChk(Hdr, SizeOf(HelpHeader));
      BlockWriteChk(OP^, HighestTopic*SizeOf(HelpNameRec));
      BlockWriteChk(CP^, SizeOfNames);
      BlockWriteChk(TM^, PickTopics*SizeOf(Word));
      BlockWriteChk(FI^, HighestTopic*SizeOf(HelpIndexRec));
    end;
  end;

  procedure FindMostFrequent;
    {-Find the most frequently occurring characters}
  begin
    with Hdr do
      for I := 0 to 15 do begin
        MFB := 0;
        {Find most frequently occurring byte in Count table}
        for B := 0 to 255 do
          if Count[B] > Count[MFB] then
            MFB := B;
        {Store it in XlateTable}
        XlateTable[I] := MFB;
        {Mark it out so we can find next MFB}
        Count[MFB] := 0;
      end;
  end;

  procedure AllocWorkSpace;
    {-Allocate space for work arrays and initialize them}
  const
    InitNameIndex : HelpNameRec = (hnOfs : NoTopicName {; hnFlags : 0} );
  var                     {!!.20}
    NASize : LongInt;     {!!.20}
  begin
    with Hdr do begin
      {Finalize count of pick topics}
      if PickTopicPending then
        Inc(PickTopics);

      {Allocate space for name index} {!!.20 - rewritten}
      NASize := LongInt(HighestTopic)*SizeOf(HelpNameRec);
      if NASize > 65520 then
        Error('Topic name index exceeds 64K')
      else if NASize = 0 then
        {Perform dummy allocation}
        NASize := 4;
      if not GetMemCheck(OP, NASize) then
        Error('Insufficient memory for topic name index');

      {Allocate space for names} {!!.20 - rewritten}
      if SizeOfNames = 0 then
        NASize := 4
      else
        NASize := SizeOfNames;
      if not GetMemCheck(CP, NASize) then
        Error('Insufficient memory for name array');

      {Allocate space for topic map}
      if not GetMemCheck(TM, HighestTopic*SizeOf(Word)) then
        Error('Insufficient memory for topic map');

      {Allocate space for file index}
      if LongInt(HighestTopic)*SizeOf(HelpIndexRec) > 65520 then
        Error('File index array exceeds 64K')
      else if not GetMemCheck(FI, HighestTopic*SizeOf(HelpIndexRec)) then
        Error('Insufficient memory for index array');

      {Allocate space for index key}
      if not GetMemCheck(IK, HighestTopic*SizeOf(Word)) then
        Error('Insufficient memory for index key');

      {Initialize the arrays}
      FillStruct(OP^, HighestTopic, InitNameIndex, SizeOf(HelpNameRec));
      FillChar(CP^, SizeOfNames, 0);
      FillChar(TM^, HighestTopic*SizeOf(Word), 0);
      FillChar(FI^, HighestTopic*SizeOf(HelpIndexRec), lo(NoHelpAvailable));
      FillChar(IK^, HighestTopic*SizeOf(Word), $77);

      {Optionally allocate space for xref list}
      if StoreXrefs then
        if XrefCnt = 0 then begin
          {No cross-references to report on}
          StoreXrefs := False;
          XrefList := False;
          UnreachList := False;
        end else begin
          if LongInt(XrefCnt)*SizeOf(XrefRec) > 65520 then
            Error('Number of cross-references exceeds '+
                  Long2Str(65520 div SizeOf(XrefRec)))
          else if not GetMemCheck(XR, XrefCnt*SizeOf(XrefRec)) then
            Error('Insufficient memory for cross-reference array');
          FillChar(XR^, XrefCnt*SizeOf(XrefRec), 0);
        end;

      {Optionally allocate space for topic size list}
      if SizeList then begin
        if not GetMemCheck(LT, HighestTopic*SizeOf(Word)) then
          Error('Insufficient memory for topic size array');
        FillChar(LT^, HighestTopic*SizeOf(Word), 0);
      end;
    end;
  end;

  procedure CountFile;
    {-Scan input file once to determine counts, sizes, frequencies}
  var
    Cnt : Word;
    Ch : Char;
    IncName : String80;
    Command : Word; {!!.03}
  begin
    with Hdr do begin
      SetTextBuf(InF[IncLev], InBuff, FileBuffSize);

      while not eof(InF[IncLev]) do begin
        ReadTextLine;
        case S[1] of
          CommandMark :      {Line is a help metacommand}
            begin
              Spos := 2;
              ParseWord(C, 8);
              Command := ClassifyCommand(C);             {!!.03}
              if (Command <> 13) and (Command <> 7) then {!!.03}
                DoingDict := False;                      {!!.03}
              case Command of                            {!!.03}
                1 :          {TOPIC}
                  begin
                    {New section, get section number}
                    CurSect := ParseNumber('Topic number');
                    if (LongInt(CurSect)+TopicBias <= 0) or
                       (LongInt(CurSect)+TopicBias > 65521 div SizeOf(HelpIndexRec)) then
                      ErrorLine('Invalid topic number');
                    {Bias the topic number}
                    inc(CurSect, TopicBias);
                    {Update topic number maximums}
                    if CurSect > HighestTopic then
                      HighestTopic := CurSect;
                    if PickTopicPending then
                      Inc(PickTopics);
                    {Get optional topic name}
                    SkipWhite;
                    C := Copy(S, Spos, 80);

                    if WhereList then                        {!!.03}
                      WriteLn(LOutFile, CurSect:5, '  ', C); {!!.03}

                    if Length(C) > 0 then begin
                      if Length(C) >= 65520-SizeOfNames then
                        ErrorLine('Topic names exceed 64K');
                      inc(SizeOfNames, Length(C)+1);
                      if Length(C) > LongestName then
                         LongestName := Length(C);
                      {Current section may affect picktopics if it is indexed}
                      PickTopicPending := True;
                    end else
                      {Untitled topic won't affect pick list}
                      PickTopicPending := False;
                  end;

                6 :          {NOINDEX}
                  if CurSect = 0 then
                    ErrorLine('NOINDEX statement must follow TOPIC statement')
                  else
                    {Most recent topic won't appear in pick list}
                    PickTopicPending := False;

                7 :          {INCLUDE}
                  if IncLev = MaxIncludeNest then
                    Error('Too many nested files')
                  else begin
                    {Include file, get filename}
                    ParseWord(IncName, 79);
                    inc(IncLev);

                    if WhereList then begin                                    {!!.03}
                      WriteLn(LOutFile, ^M^J'Include File: ', IncName, ^M^J);  {!!.03}
                      if IoResult <> 0 then                                    {!!.03}
                        Error('Writing to '+LOutName);                         {!!.03}
                    end;                                                       {!!.03}

                    if FilesList then begin                                    {!!.03}
                      WriteLn(FOutFile, IncName);                              {!!.03}
                      if IoResult <> 0 then                                    {!!.03}
                        Error('Writing to '+FOutName);                         {!!.03}
                    end;                                                       {!!.03}

                    OpenInf(IncName);
                    CountFile;
                  end;

                11 :         {BIAS}
                  begin
                    SkipWhite;                          {!!.12}
                    if Spos > Slen then                 {!!.12}
                      TopicBias := HighestTopic         {!!.12}
                    else                                {!!.12}
                      TopicBias := ParseLongInt('Topic bias');
                    if (TopicBias < -65000) or (TopicBias > 65000) then
                      ErrorLine('Topic bias out of range');
                  end;

                13:            {CONST}  {!!.03}
                  DoingDict := True;    {!!.03}

                {Ignore other metacommands this pass}
              end;
            end;
          CommentMark :
            {Ignore comment lines}
            ;
        else
          if DoingDict then         {!!.03}
            {A constant definition} {!!.03}
            ParseConstDef           {!!.03}
          else if CurSect = 0 then                                     {!!.11}
            ErrorLine('Topic text must follow first TOPIC statement')  {!!.11}
          else begin                {!!.03}
            {Part of help text, keep count for compression}
            Spos := 1;
            while Spos <= Slen do begin
              Ch := S[Spos];
              Cnt := Count[Byte(Ch)];
              if Cnt < $FFFF then
                Count[Byte(Ch)] := Cnt+1;
              case Ch of
                IndexMarker : {Skip over the cross-reference topic number}
                  begin
                    repeat
                      Inc(Spos);
                    until (Spos > Slen) or (S[Spos] = XrefToggle);
                    Inc(XrefCnt);
                  end;
                LiteralMarker : {Skip over the literal character}
                  repeat
                    Inc(Spos);
                  until (Spos > Slen) or (S[Spos] < '0') or (S[Spos] > '9');
              else
                Inc(Spos);
              end;
            end;
          end; {!!.03}

          {Approximate the number of linebreak markers}
          Inc(Count[Byte(LineBrkMark)]);
        end;
      end;

      CloseInf;
    end;
  end;

  function TranslateIndex(Ch : Char) : Byte;
    {-If Ch is in translate table, return index, else return $0F}
{$IFDEF VIRTUALPASCAL}
  assembler; {$FRAME-} {$USES esi,edi}
  asm
    lea   edi,TranslateTable
    mov   esi,edi
    mov   al,&Ch
    mov   ecx,MaxCompIndex+1
    cld
    repne scasb
    jne   @fail
    dec   edi
  @fail:
    sub   edi,esi
    mov   eax,edi
  end;
{$ELSE}
  inline
  ($8C/$D8/                  {mov ax,ds}
    $8E/$C0/                 {mov es,ax}
    $BF/>TranslateTable/     {mov di,>TranslateTable  ;es:di -> TranslateTable}
    $89/$FE/                 {mov si,di               ;save start offset}
    $58/                     {pop ax                  ;al = Ch}
    $B9/>MaxCompIndex+1/     {mov cx,>MaxCompIndex+1  ;cx = bytes to scan}
    $FC/                     {cld                     ;forward}
    $F2/                     {repne                   ;search}
    $AE/                     {scasb                   ;  until match}
    $75/$01/                 {jne fail                ;jump if not found}
    $4F/                     {dec di                  ;point to match if found}
    {fail:}
    $29/$F7/                 {sub di,si               ;compute index of match}
    $89/$F8);                {mov ax,di               ;return index in al}
{$ENDIF}

  procedure FlushBuffer;
    {-Write the output buffer to file}
  begin
    if OutPos > 0 then begin
      BlockWriteChk(OutBuff, OutPos);
      OutPos := 0;
      Nibble := False;
    end;
  end;

  procedure NibbleOut(N : Byte);
    {-Send next nibble to output}
  begin
    if Nibble then begin
      OutBuff[OutPos] := OutBuff[OutPos] or (N shl 4);
      Nibble := False;
      if OutPos = FileBuffSize then
        FlushBuffer;
    end else begin
      Inc(OutPos);
      Inc(CompLen);
      OutBuff[OutPos] := N;
      Nibble := True;
    end;
  end;

  procedure CharOut(Ch : Char);
    {-Compress a single character and write it}
  var
    T : Byte;
    Overlap : Integer;
  begin
    T := TranslateTable2[Ch];                 {!!.12}
    if T > MaxCompIndex then begin
      {Not compressible - output 3 nibbles}
      NibbleOut($0F);
      NibbleOut(Ord(Ch) and $0F);
      NibbleOut(Ord(Ch) shr 4);
    end else
      {Store compression code in 1 nibble}
      NibbleOut(T);
    Inc(SectLen);
    {Keep track of decompression margin}      {!!.11}
    if SectLen > CompLen then                 {!!.11}
      if SectLen-CompLen > CompMargin then    {!!.11}
        CompMargin := SectLen-CompLen;        {!!.11}
  end;

  procedure PushFont(FChar : Char);
    {-Activate a new font, pushing it onto the font stack}
  begin
    if StackP >= MaxFontStack then
      ErrorLine('Font stack overflow');
    inc(StackP);
    Stack[StackP] := FChar;
  end;

  procedure PopFont;
    {-Remove top font from font stack}
  begin
    if StackP < 1 then
      ErrorLine('Font stack underflow');
    dec(StackP);
  end;

  procedure NewLine;
    {-End the current line}
  begin
    if InXref then
      {Line break in xref}
      WarnLine('Cross-reference straddles line break');

    {Keep track of longest line}
    if LineLen > Hdr.MaxLineLen then begin
      Hdr.MaxLineLen := LineLen;
      MaxLineSect := CurSect;
    end;

    CharOut(LineBrkMark);
    LineLen := 0;
  end;

  procedure NewPage;
    {-End the current page}
  begin
    CharOut(PageBrkMark);
    LineLen := 0;
  end;

  procedure NewSection;
    {-End the current section and prepare for the new}
  begin
    {Finish previous line}
    if LineLen > 0 then
      NewLine;
    CharOut(SectEndMark);
    FI^[CurSect].CompLen := CompLen;

    {Keep track of largest topic}
    if CompLen > MaxCompSize then begin
      MaxCompSize := CompLen;
      MaxCompSect := CurSect;
    end;
    if SectLen > MaxUncompSize then begin
      MaxUncompSize := SectLen;
      MaxUncompSect := CurSect;
    end;
    if CompLen+CompMargin > BufferWithMargin then    {!!.11}
      BufferWithMargin := CompLen+CompMargin;        {!!.11}
    if SizeList then
      LT^[CurSect] := SectLen;

    {Keep track of maximum xrefs in one section}
    if XrefsInSect > MaxXrefs then begin
      MaxXrefs := XrefsInSect;
      MaxXrefSect := CurSect;
    end;
    XrefsInSect := 0;

    Inc(TotalComp, CompLen);
    Inc(TotalSect, SectLen);

    Inc(SectPos, CompLen);
    SectLen := 0;
    CompLen := 0;
    CompMargin := 0; {!!.11}
    LineLen := 0;
    Nibble := False;
    if OutPos = FileBuffSize then
      FlushBuffer;
  end;

  function LenCount(Ch : Char) : Byte;
    {-Return length to count for character}
  begin
    case Ch of
      Attr1Toggle..XrefToggle :
        LenCount := 0;
    else
      LenCount := 1;
    end;
  end;

  procedure WordOut(var Spos : Word; Tpos : Word); {!!.03}
    {-Write line starting at Spos and continuing to Tpos}
  var
    Topic : Word;
    Code : Word;
    Ch : Char;
    Finished : Boolean;
    L : LongInt;
  begin
    while Spos < Tpos do begin
      Ch := S[Spos];
      case Ch of
        IndexMarker : {Convert cross-reference topic number to binary}
          begin
            CharOut(Ch);

            {Collect the topic number}
            Clen := 0;
            repeat
              Inc(Spos);
              Inc(Clen);
              C[Clen] := S[Spos];
            until (Spos >= Tpos) or (S[Spos] = XrefToggle);
            Dec(Clen);
            Val(C, Topic, Code);

            if Code <> 0 then
              if ConstDict.Member(StUpcase(C), L) then {!!.03}
                Topic := L                             {!!.03}
              else                                     {!!.03}
                ErrorLine('Invalid cross-reference topic number');
            {Bias the topic number}
            inc(Topic, TopicBias);
            if Topic = CurSect then
              WarnLine('Topic cross-references itself');

            {Write binary version of cross reference topic to output}
            CharOut(Char(lo(Topic)));
            CharOut(Char(hi(Topic)));

            Inc(XrefsInSect);

            {Store information about the cross-reference for reporting later}
            if StoreXrefs then begin
              inc(CurXref);
              with XR^[CurXref] do begin
                FrTopic := CurSect;
                ToTopic := Topic;
              end;
            end;
          end;

        LiteralMarker : {Output literal character following}
          begin
            Clen := 0;
            Finished := False;
            repeat
              Inc(Spos);
              Inc(Clen);
              C[Clen] := S[Spos];
              if Spos >= Tpos then
                Finished := True
              else
                case S[Spos] of
                  '0'..'9' : ;
                else
                  Finished := True;
                end;
            until Finished;
            Dec(Clen);
            Val(C, Topic, Code);
            if (Code <> 0) or (Topic > 255) then
              ErrorLine('Invalid literal character');
            CharOut(Char(lo(Topic)));
            Inc(LineLen);
          end;

        Attr1Toggle,
        Attr2Toggle,
        Attr3Toggle,
        XrefToggle :
          begin
            if (StackP > 0) and (Stack[StackP] = Ch) then
              PopFont
            else
              PushFont(Ch);
            CharOut(Ch);
            if Ch = XrefToggle then
              InXref := not InXref;
            Inc(Spos);
          end;

      else
        CharOut(Ch);
        Inc(LineLen, LenCount(Ch));
        Inc(Spos);
      end;
    end;
  end;

  procedure LineOut;
    {-Wrap and write text lines}
  var
    Tpos : Word; {!!.03}
    Tlen : Word; {!!.03}
  begin
    if not Wrapping then begin
      {Write entire line without wrapping}
      SPos := 1;
      WordOut(SPos, Word(Slen)+1); {!!.03}
      NewLine;
      Exit;
    end;

    if Slen = 0 then begin
      {Empty line, finish previous line}
      if LineLen > 0 then
        NewLine;
      {Insert blank line}
      NewLine;
      Exit;
    end;

    {Non-empty line}
    if (S[1] = ' ') then
      {Finish previous line}
      if LineLen > 0 then
        NewLine;

    Spos := 1;
    repeat

      {Write white space}
      while (Spos <= Slen) and (S[Spos] = ' ') do begin
        if LineLen < TextWid then begin
          CharOut(' ');
          Inc(LineLen);
        end;
        Inc(Spos);
      end;
      if Spos > Slen then
        Exit;

      {See if next word fits on line}
      Tpos := Spos;
      Tlen := 0;
      repeat
        case S[Tpos] of
          IndexMarker : {Skip over the cross-reference topic number}
            repeat
              Inc(Tpos);
            until (Tpos > Slen) or (S[Tpos] = XrefToggle);

          LiteralMarker : {Skip over the character number}
            begin
              repeat
                Inc(Tpos);
              until (Tpos > Slen) or (S[Tpos] < '0') or (S[Tpos] > '9');
              Inc(Tlen);
            end;

        else
          Inc(Tlen, LenCount(S[Tpos]));
          Inc(Tpos);
        end;
      until (Tpos > Slen) or (S[Tpos] = ' ');

      if LineLen+Tlen > TextWid then
        {Word won't fit on line, start a new one}
        NewLine;

      {Write the word}
      WordOut(Spos, TPos);

    until Spos > Slen;

    {End line with blank}
    if LineLen < TextWid then begin
      CharOut(' ');
      Inc(LineLen);
    end;
  end;

  procedure GetTopicName(Item : Word; var Name : String);
    {-Return topic name}
  var
    NameOfs : Word;
  begin
    NameOfs := OP^[Item].hnOfs;
    if NameOfs = NoTopicName then
      Name := ''
    else begin
      Name[0] := Char(Byte(CP^[NameOfs]) and $7F);
      move(CP^[NameOfs+1], Name[1], Byte(Name[0]));
    end;
  end;

  function SearchableTopic(Item : Word) : Boolean;
    {-Return True if Topic is searchable}
  var
    NameOfs : Word;
    LenByte : Byte;
  begin
    NameOfs := OP^[Item].hnOfs;
    if NameOfs = NoTopicName then
      SearchableTopic := False
    else begin
      LenByte := Byte(CP^[NameOfs]);
      SearchableTopic := not ByteFlagIsSet(LenByte, $80);
    end;
  end;

  procedure StoreTopicName(C : string);
    {-Store topic name for CurSect}
  begin
    OP^[CurSect].hnOfs := NameOfs;
    Move(C, CP^[NameOfs], Length(C)+1);
    inc(NameOfs, Length(C)+1);
  end;

  procedure Swap(I, J : Word);
    {-Swap topic map for I and J}
  var
    Tmp : Word;
  begin
    Tmp := TM^[J];
    TM^[J] := TM^[I];
    TM^[I] := Tmp;
    InOrder := False;
  end;

{$IFDEF VIRTUALPASCAL}
  Function BiosTicks : Word;
  begin
    BiosTicks := SysSysMsCount;
  end;
{$ENDIF}

  procedure SortTopicMap;
    {-Sort the topic map into alphabetical order}
  const
    SortMsg = 'Sorting .';
  var
{$IFNDEF VIRTUALPASCAL}
    BiosTicks : LongInt absolute $40:$6C; {BIOS time}
{$ENDIF}
    StartTime : LongInt;
    Dots : Word;
    I : Word;
    J : Word;
    K : Word;
    Offset : Word;
    IM : Word;
    JM : Word;
    IStr : String;
    JStr : String;
  begin
    with Hdr do begin
      Write(SortMsg);

      {Initialize topic map}
      for I := 1 to HighestTopic do
        TM^[I] := I;

      {Sort topic names via topic map}
      Dots := 1;
      StartTime := BiosTicks;
      Offset := HighestTopic;
      while Offset > 1 do begin
        Offset := Offset shr 1;
        K := HighestTopic-Offset;
        repeat
          InOrder := True;
          for J := 1 to K do begin
            if BiosTicks > StartTime then begin
              if Dots < ScreenWidth-Length(SortMsg) then begin
                Write('.');
                inc(Dots);
              end;
              StartTime := BiosTicks+18;
            end;
            I := J+Offset;
            IM := TM^[I];
            JM := TM^[J];
            GetTopicName(IM, IStr);
            GetTopicname(JM, JStr);
            if Length(IStr) = 0 then
              {"I" is greater - sort empty names to end of list}
            else if Length(JStr) = 0 then
              {"J" is greater - sort empty names to end of list}
              Swap(I, J)
            else if IK^[IM] > IK^[JM] then
              {"I" is greater - no swap needed}
            else if IK^[IM] < IK^[JM] then
              {"J" is greater by index override}
              Swap(I, J)
            else if CompUCString(IStr, JStr) = Less then
              {"J" is greater alphabetically}
              Swap(I, J)
          end;
        until InOrder;
      end;

      Write(^M, CharStr(' ', Length(SortMsg)+Dots), ^M);

      {Find longest indexed topic name}
      for I := 1 to HighestTopic do begin
        IM := TM^[I];
        GetTopicName(IM, IStr);
        if (IK^[IM] = $FFFF) or (Length(IStr) = 0) then
          {End of indexed topics}
          Exit;
        {Keep track of longest pick name}
        if Length(IStr) > LongestPick then
          LongestPick := Length(IStr);
      end;
    end;

  end;

  procedure InitForScan;
    {-Initialize for the scan file pass}
  var          {!!.12}
    C : Char;  {!!.12}
  begin
    with Hdr do begin
      TextWid := WindowWidth-2; {Correct text dimensions for default spacing}

      {File position for first help section}
      SectPos := SizeOf(HelpHeader)+
                 LongInt(HighestTopic)*(SizeOf(HelpNameRec)+SizeOf(HelpIndexRec))+
                 SizeOfNames+
                 LongInt(PickTopics)*SizeOf(Word);

      {No warnings yet}
      Warnings := 0;
      TotLines := 0;

      {Initialize counters}
      TotalSect := 0;
      TotalComp := 0;
      CurSect := 0;
      LineLen := 0;
      SectLen := 0;
      CompLen := 0;
      OutPos := 0;
      NameOfs := 0;
      MaxXrefs := 0;
      XrefsInSect := 0;
      MaxXrefSect := 0;
      Nibble := False;
      InXref := False;
      Wrapping := True;
      TranslateTable := XlateTable;
      for C := #0 to #255 do                     {!!.12}
        TranslateTable2[C] := TranslateIndex(C); {!!.12}
      TopicBias := 0;
      MaxUncompSect := 0;
      MaxUncompSize := 0;
      MaxCompSect := 0;
      MaxCompSize := 0;
      MaxLineSect := 0;
      StackP := 0;
      CompMargin := 0;       {!!.11}
      BufferWithMargin := 0; {!!.11}
      HighestTopicPass2 := 0; {!!.12}

      WriteLn('Pass 2 ...........');
      TotLines := 0;
      IncLev := 0;
      OpenInf(InName);
    end;
  end;

  procedure ScanFile;
    {-Scan input file to create help text}
  var
    IncName : String80;
    Command : Word;     {!!.03}
  begin
    with Hdr do begin
      LineNum[IncLev] := 0;
      SetTextBuf(InF[IncLev], InBuff, FileBuffSize);

      while not eof(InF[IncLev]) do begin
        ReadTextLine;
        case S[1] of
          CommandMark :      {A help metacommand}
            begin
              Spos := 2;
              ParseWord(C, 8);
              Command := ClassifyCommand(C);             {!!.03}
              if (Command <> 13) and (Command <> 7) then {!!.03}
                DoingDict := False;                      {!!.03}
              case Command of                            {!!.03}
                1 :          {TOPIC}
                  begin
                    if CurSect <> 0 then
                      {Complete previous section}
                      NewSection;
                    if StackP > 0 then
                      WarnLine('Help attributes left on stack from previous topic');
                    {Get section number}
                    CurSect := ParseNumber('Topic number');
                    {Bias the topic number}
                    inc(CurSect, TopicBias);
                    {update topic maximum}              {!!.12}
                    if CurSect > HighestTopicPass2 then {!!.12}
                      HighestTopicPass2 := CurSect;     {!!.12}
                    {Error check}
                    if FI^[CurSect].Start <> NoHelpAvailable then
                      ErrorLine('Duplicate help topic number');
                    {Store file index}
                    FI^[CurSect].Start := SectPos;
                    {Get optional topic name}
                    SkipWhite;
                    C := Copy(S, Spos, 80);
                    if Length(C) > 0 then
                      {Store topic name}
                      StoreTopicName(C);
                  end;

                2 :          {LINE}
                  NewLine;

                3 :          {PAGE}
                  NewPage;

                4 :          {WIDTH}
                  if CurSect <> 0 then
                    ErrorLine('WIDTH statement must precede first help topic')
                  else begin
                    {Parse width}
                    WindowWidth := ParseNumber('Width');
                    {Correct dimension for default spacing}
                    TextWid := WindowWidth-2;
                  end;

                5 :          {INDEX}
                  if CurSect = 0 then
                    ErrorLine('INDEX statement must follow TOPIC statement')
                  else
                    IK^[CurSect] := ParseNumber('Index number');

                6 :          {NOINDEX}
                  if CurSect = 0 then
                    ErrorLine('NOINDEX statement must follow TOPIC statement')
                  else
                    IK^[CurSect] := $FFFF;

                7 :          {INCLUDE}
                  if IncLev = MaxIncludeNest then
                    Error('Too many nested files')
                  else begin
                    {Include file, get filename}
                    ParseWord(IncName, 79);
                    inc(IncLev);
                    OpenInf(IncName);
                    ScanFile;
                  end;

                 8 :         {WRAP}
                   Wrapping := True;

                 9 :         {NOWRAP}
                   Wrapping := False;

                10 :         {SCROLL}
                   Scrolling := True;

                11 :         {BIAS}
                  begin
                    SkipWhite;                          {!!.12}
                    if Spos > Slen then                 {!!.12}
                      TopicBias := HighestTopicPass2    {!!.12}
                    else                                {!!.12}
                      TopicBias := ParseLongInt('Topic bias');
                    if (TopicBias < -65000) or (TopicBias > 65000) then
                      ErrorLine('Topic bias out of range');
                  end;

                12 :         {NOSEARCH}
                  if CurSect = 0 then
                    ErrorLine('NOSEARCH statement must follow TOPIC statement')
                  else
                    SetByteFlag(Byte(CP^[OP^[CurSect].hnOfs]), $80);

                13:          {CONST}                                  {!!.03}
                  DoingDict := True;                                  {!!.03}

              else
                ErrorLine('Unrecognized directive');
              end;
            end;
          CommentMark :
            {Ignore line} ;
        else
          {ignore const definitions on this pass}  {!!.03}
          if not DoingDict then                    {!!.03}
            {A text line - wrap and output}
            LineOut;
        end;
      end;

      CloseInf;
    end;
  end;

  procedure InvertTopicNames;
    {-Perform simple encryption on topic names}
  var
    N : Word;
  begin
    for N := 0 to Hdr.SizeOfNames-1 do
      CP^[N] := Char(not Byte(CP^[N]));
  end;

  procedure ScanDone;
    {-Clean up when scan pass is done}
  begin
    with Hdr do begin
      {Finalize status}
      if Warnings <> 0 then begin
        Write(^M);
        ClrEol;
        WriteLn;
      end;
      WriteLn('Summary ..........');
      WriteLn(TotLines:6, ' total lines in help file');

      {Store last section}
      {if SectLen > 0 then} {!!.21}
        NewSection;
      {Assure output goes to disk}
      FlushBuffer;

      {Sort the topic map}
      SortTopicMap;

      {Leave extra room for worst section's decompression}
      BiggestTopic := MaxWord(BufferWithMargin, MaxUncompSize)+16; {!!.11}
      {BiggestTopic := MaxWord(MaxCompSize, MaxUncompSize)+16;}    {!!.11}

      {Write the updated header and indexes}
      InvertTopicNames;
      Seek(OutF, 0);
      WriteHeaders;
      Close(OutF);
      InvertTopicNames;
    end;
  end;

  procedure OpenTextOutput(OName : String);
    {-Open a text file for output, reusing the InF[0] global variable}
  begin
    {Store global copy of name for error checking}
    OutName := OName;
    Assign(TxtF, OutName);
    Rewrite(TxtF);
    if IoResult <> 0 then
      Error('Cannot create '+OutName);
    SetTextBuf(TxtF, InBuff, FileBuffSize);
  end;

  procedure CheckIO;
    {-Check IoResult while creating output reports}
  var
    IO : Word;
  begin
    IO := IoResult;
    if IO <> 0 then
      Error('Error ('+Long2Str(IO)+') writing to '+OutName);
  end;

  procedure WriteXrefList;
    {-Write a cross-reference listing}
  const
    XrefIndent = 19;
  var
    Counter : Word;
    Xref : Word;
    Topic : Word;
    XrefOnLine : Word;
    XrefTopic : Word;
    Name : String;
  begin
    Write('Writing cross-references ...    1');

    {Open list file}
    OpenTextOutput(ForceExtension(InName, 'LST'));

    {Write a report title}
    if SortXref then
      Write(TxtF, ' Sorted');
    WriteLn(TxtF, ' Topic Cross-Reference'^M^J);
    WriteLn(TxtF, 'Topic    Index  Topic Name');
    CheckIO;

    {Scan all the topics}
    for Counter := 1 to Hdr.HighestTopic do begin

      {Entertaining status during this slow process}

      if Counter mod 50 = 0 then
        Write(^H^H^H^H^H, Counter:5);

      {Choose topic depending on sort order}
      if SortXref then
        Topic := TM^[Counter]
      else
        Topic := Counter;

      if FI^[Topic].Start <> NoHelpAvailable then begin
        {There is help for this topic number}

        {Write the index position}
        case IK^[Topic] of
          $7777 : {Alpha order}
            Name := '<alpha>';
          $FFFF : {No index}
            Name := '       ';
        else
          {Index position specified explicitly}
          Name := LeftPad(Long2Str(IK^[Topic]), 7);
        end;
        Write(TxtF, Topic:5, '  ', Name, '  ');

        {Write the topic name}
        GetTopicName(Topic, Name);
        if Length(Name) = 0 then
          Name := '<no topic name>';
        WriteLn(TxtF, Name);

        {Write the xref TO's}
        XrefOnLine := 0 ;
        for Xref := 1 to XrefCnt do
          if XR^[Xref].FrTopic = Topic then begin
            XrefTopic := XR^[Xref].ToTopic;
            if XrefOnLine = 0 then begin
              Write(TxtF, '':XrefIndent, '  to:');
              XrefOnLine := 1;
            end else if XrefOnLine > 8 then begin
              Write(TxtF, ^M^J, '':XrefIndent, '     ');
              XrefOnLine := 1;
            end else
              Inc(XrefOnLine);
            Write(TxtF, XrefTopic:6);
          end;

        if XrefOnLine > 0 then begin
          WriteLn(TxtF);
          XrefOnLine := 0;
        end;

        {Write the xref FROM's}
        for Xref := 1 to XrefCnt do
          if XR^[Xref].ToTopic = Topic then begin
            XrefTopic := XR^[Xref].FrTopic;
            if XrefOnLine = 0 then begin
              Write(TxtF, '':XrefIndent, 'from:');
              XrefOnLine := 1;
            end else if XrefOnLine > 8 then begin
              Write(TxtF, ^M^J, '':XrefIndent, '     ');
              XrefOnLine := 1;
            end else
              Inc(XrefOnLine);
            Write(TxtF, XrefTopic:6);
          end;

        if XrefOnLine > 0 then
          WriteLn(TxtF);

        CheckIO;
      end; {if HelpAvailable}
    end; {for Counter}

    {Check for cross-references to nowhere}
    WriteLn(TxtF, ^M^J'Questionable Cross-References'^M^J);
    WriteLn(TxtF, 'Topic  Link to  Topic Name');
                  {nnnnn  xxxxx    nnnnnnnnnnnnnn}
    CheckIO;

    XrefOnLine := 0;
    for Xref := 1 to XrefCnt do
      with XR^[Xref] do
        if (ToTopic = 0) or
           (ToTopic > Hdr.HighestTopic) or
           (FI^[ToTopic].Start = NoHelpAvailable) then begin
          GetTopicName(FrTopic, Name);
          if Length(Name) = 0 then
            Name := '<no topic name>';
          WriteLn(TxtF, FrTopic:5, '  ', Totopic:5, '     ', Name);
          CheckIO;
          inc(XrefOnLine);
        end;
    if XrefOnLine = 0 then
      WriteLn(TxtF, ' none');

    Write(^M, '                                 ', ^M);

    Close(TxtF);
    CheckIO;
  end;

  procedure WriteUnreachableTopics;
    {-Report topics that aren't reachable from the master index}
  const
    Asterisk : array[Boolean] of Char = (' ', '*');
  var
    Counter : Word;
    XRef : Word;
    Changed : Boolean;
    Name : String;
  begin
    Write('Checking unreachable topics ', 1:5);

    {Open output file}
    OpenTextOutput(ForceExtension(InName, 'UNR'));

    {Reuse the FI array's CompLen field for "reachable" flags}
    for Counter := 1 to Hdr.HighestTopic do
      with FI^[Counter] do
        if Start = NoHelpAvailable then
          {Don't report topics that are empty}
          CompLen := 1
        else
          CompLen := 0;

    {Pass 1 -- mark topics in the master index reachable}
    for Counter := 1 to Hdr.PickTopics do
      FI^[TM^[Counter]].CompLen := 1;

    {Pass 2 -- extend reachable topics via Xref list}
    Counter := 1;
    repeat
      Changed := False;
      for Xref := 1 to XrefCnt do
        with XR^[Xref] do
          if FI^[FrTopic].CompLen <> 0 then begin
            {From topic has already been reached, mark To topic reached}
            with FI^[ToTopic] do
              if CompLen = 0 then begin
                Changed := True;
                inc(CompLen);
              end;
          end;
      Inc(Counter);
      Write(^H^H^H^H^H, Counter:5);
    until not Changed;

    {Pass 3 -- report unreachable topics}
    WriteLn(TxtF, 'Topics Unreachable from Master Index'^M^J);
    Changed := False;
    for Counter := 1 to Hdr.HighestTopic do
      if FI^[Counter].CompLen = 0 then begin
        GetTopicName(Counter, Name);
        if Length(Name) = 0 then
          Name := '<no topic name>';
        WriteLn(TxtF,
                Counter:5,
                ' ', Asterisk[not SearchableTopic(Counter)], ' ',
                Name);
        CheckIO;
        Changed := True;
      end;
    if Changed then
      WriteLn(TxtF, ^M^J'* topic also specifies !NOSEARCH')
    else
      WriteLn(TxtF, '  none');

    Write(^M, '                                 ', ^M);

    Close(TxtF);
    CheckIO;
  end;

  procedure WriteSearchableTopics;
    {-Write a report listing the searchable topics}
  var
    Counter : Word;
    Name : String;
    Changed : Boolean;
  begin
    Write('Writing searchable topics ...');

    {Open output file}
    OpenTextOutput(ForceExtension(InName, 'TOP'));

    WriteLn(TxtF, 'Searchable Topics'^M^J);

    {Scan all the topics}
    Changed := False;
    for Counter := 1 to Hdr.HighestTopic do
      if FI^[Counter].Start <> NoHelpAvailable then
        if SearchableTopic(Counter) then begin
          GetTopicName(Counter, Name);
          if Length(Name) > 0 then begin
            WriteLn(TxtF, Counter:5, ' ', Name);
            CheckIO;
            Changed := True;
          end;
        end;
    if not Changed then
      WriteLn(TxtF, '  none');

    Write(^M, '                             ', ^M);

    Close(TxtF);
    CheckIO;
  end;

  procedure WriteAvailableTopics;                            {!!.03}
    {-Write a report listing the available topic numbers}
  var
    Counter : Word;
    Name : String;
    Changed : Boolean;
  begin
    Write('Writing available topic numbers ...');

    {Open output file}
    OpenTextOutput(ForceExtension(InName, 'AVL'));

    WriteLn(TxtF, 'Available Topic Numbers'^M^J);

    {Scan all the topics}
    Changed := False;
    for Counter := 1 to Hdr.HighestTopic do
      if FI^[Counter].Start = NoHelpAvailable then begin
        WriteLn(TxtF, Counter:5);
        CheckIO;
        Changed := True;
      end;
    if not Changed then
      WriteLn(TxtF, '  none');

    Write(^M, '                                   ', ^M);

    Close(TxtF);
    CheckIO;
  end;

  procedure OpenLocFile;                            {!!.03}
    {-Write a report listing the topic locations}
  begin
    LOutName := ForceExtension(InName, 'LOC');
    Assign(LOutFile, LOutName);
    Rewrite(LOutFile);
    if IoResult <> 0 then
      Error('Opening '+LOutName);

    WriteLn(LOutFile, 'Topic Number Locations by File'^M^J^M^J);
    WriteLn(LOutFile, 'Main File: ', InName, ^M^J);
   end;

  procedure CloseLocFile;                                   {!!.03}
    {-Close the file used to list the topic locations}
  begin
    Close(LOutFile);
    if IoResult <> 0 then
      Error('Closing '+LOutName);
  end;

  procedure OpenUsedFile;                            {!!.03}
    {-Open a report showing files used to build help file}
  begin
    FOutName := ForceExtension(InName, 'FIL');
    Assign(FOutFile, FOutName);
    Rewrite(FOutFile);
    if IoResult <> 0 then
      Error('Opening '+FOutName);

    WriteLn(FOutFile, InName);
   end;

  procedure CloseUsedFile;                                   {!!.03}
    {-Close a report showing files used to build help file}
  begin
    Close(FOutFile);
    if IoResult <> 0 then
      Error('Closing '+FOutName);
  end;

  procedure WriteBiggestTopics;
    {-Write a report listing the biggest topics}
  const
    TopicsToList = 20;
  var
    Counter : Word;
    Listed : Word;
    BiggestTopic : Word;
    BiggestSize : Word;
    Name : String;
  begin
    Write('Writing biggest topics ...');

    {Open output file}
    OpenTextOutput(ForceExtension(InName, 'SIZ'));

    WriteLn(TxtF, 'Biggest Topics'^M^J);
                  {sssss  ttttt  nnnnnnnnnnnnnnnnnnnnnnnnn}
    WriteLn(TxtF, ' Size  Topic  Name');

    Listed := 0;
    repeat
      BiggestSize := 0;
      for Counter := 1 to Hdr.HighestTopic do
        if LT^[Counter] > BiggestSize then begin
          BiggestSize := LT^[Counter];
          BiggestTopic := Counter;
        end;
      if BiggestSize <> 0 then begin
        {Write out the information}
        GetTopicName(BiggestTopic, Name);
        WriteLn(TxtF, BiggestSize:5, '  ', BiggestTopic:5, '  ', Name);
        CheckIO;
        {Mark out topic so it won't be repeated}
        LT^[BiggestTopic] := 0;
        inc(Listed);
      end;
    until (Listed >= TopicsToList) or (BiggestSize = 0);

    Write(^M, '                          ', ^M);
    Close(TxtF);
    CheckIO;
  end;

  procedure ShowStatistics;
    {-Show some interesting statistics}
  begin
    with Hdr do begin
      WriteLn(HighestTopic:6,
              ' topic slots used');
      WriteLn(PickTopics:6,
              ' indexed topics');
      WriteLn(XrefCnt:6,
              ' cross-references');

      WriteLn((HighestTopic*SizeOf(HelpNameRec)):6,
              ' bytes in topic name index');
      WriteLn(SizeOfNames:6,
              ' bytes in packed topic name table');
      WriteLn((PickTopics*SizeOf(Word)):6,
              ' bytes in sorted topic map');
      WriteLn((HighestTopic*SizeOf(HelpIndexRec)):6,
              ' bytes in help index');

      WriteLn(MaxUncompSize:6,
              ' bytes in largest uncompressed topic (topic ', MaxUncompSect, ')');
      WriteLn(MaxCompSize:6,
              ' bytes in largest compressed topic (topic ', MaxCompSect, ')');
      WriteLn(BiggestTopic:6,                              {!!.11}
              ' bytes for topic decompression buffer');    {!!.11}
      WriteLn(MaxLineLen:6,
              ' bytes in longest line of help (topic ', MaxLineSect, ')');
      WriteLn(MaxXrefs:6,
              ' most cross-refs in a topic (topic ', MaxXrefSect, ')');

      WriteLn(TotalSect:6,
              ' uncompressed help bytes');
      WriteLn(TotalComp:6,
              ' compressed help bytes');
      WriteLn((TotalComp/TotalSect):6:2,
              ' compression factor');
    end;
  end;

begin
  {Force MAKEHELP to honor ExecWin's window}
  Assign(Output, '');
  Rewrite(Output);

  WriteCopyRight;

  {Initialize globals and parse command line}
  Initialize;

  if WhereList then     {!!.03}
    OpenLocFIle;        {!!.03}
  if FilesList then     {!!.03}
    OpenUsedFile;       {!!.03}

  {Perform counting pass}
  CountFile;

  if WhereList then     {!!.03}
    CloseLocFile;       {!!.03}
  if FilesList then     {!!.03}
    CloseUsedFile;      {!!.03}

  if Hdr.HighestTopic = 0 then
    Error('No help topics specified');

  {Allocate heap space for various index structures}
  AllocWorkSpace;

  {Build the xlate table for compression}
  FindMostFrequent;

  {Reserve disk space for help indexes}
  WriteHeaders;

  {Prepare to reread the input file}
  InitForScan;

  {Perform the word wrap pass and write the compressed help topics to disk}
  ScanFile;

  {Complete and write the help indexes to disk}
  ScanDone;

  {Write cross-reference listing if enabled}
  if XrefList then
    WriteXrefList;

  {Write unreachable topics listing if enabled}
  if UnreachList then
    WriteUnreachableTopics;

  {Write searchable topics listing if enabled}
  if SearchList then
    WriteSearchableTopics;

  {Write list of biggest topics if enabled}
  if SizeList then
    WriteBiggestTopics;

  {Write list of available topics if enabled} {!!.03}
  if AvailList then                           {!!.03}
    WriteAvailableTopics;                     {!!.03}

  {Show a summary}
  ShowStatistics;
end.
