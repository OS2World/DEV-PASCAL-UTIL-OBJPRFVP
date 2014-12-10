
{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPCLONE.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpClone;
  {-Clone typed constants into a program}

interface

uses         {!!.20}
  Use32,
  Dos,
  OpConst,   {!!.20}
  OpInline,
  OpString,
  OpRoot;

const
  MinBufSize = 512;
{$IFDEF VIRTUALPASCAL}
  DefBufSize = 65536;
  MinusOne   = $FFFFFFFF;
{$ELSE}
  DefBufSize = 4096;
  MinusOne   = $FFFF;
{$ENDIF}
type
  DateUpdateType = (UpdateNone, UpdateDate, UpdateAll);
  SearchBuffer   = array[0..DefBufSize-1] of Char;
  Cloner =
    object(Root)
      cFile    : File;
      cTime    : LongInt;
      cPos     : LongInt;
      cBufSize : Word;
      cUpdate  : DateUpdateType;
      cError   : Word;
      cBufPtr  : ^SearchBuffer;

      constructor Init(FName : string; DUT : DateUpdateType);
        {-Open file for cloning}
      constructor InitCustom(FName : string; DUT : DateUpdateType;
                             BufSize : Word);
        {-Open file for cloning}
      constructor InitAndFind(FName : string; DUT : DateUpdateType;
                              var ID; IdSize : Word);
        {-Open file and find ID. Uses FindDefaultsEnd}
      destructor Done; virtual;
        {-Close clone file, adjust time stamp, and deallocate buffer}

      function FindDefaultsEnd(var ID; IdSize : Word; Skip : LongInt) : Boolean;
        {-Find the ID in the clone file, searching from the end backward}
      function FindDefaultsStart(var ID; IdSize : Word; Skip : LongInt) : Boolean;
        {-Find the ID in the clone file, searching from the start forward}

      function GetLastError : Word;
        {-Get the code for the last error}
      function GetPos : LongInt;
        {-Get position where an ID string was found (offset returned is the
          start of the ID string)}

      procedure LoadDefaults(FileOfs : LongInt; var Defaults; Bytes : Word);
        {-Seek to position FileOfs and read defaults there}
      procedure StoreDefaults(FileOfs : LongInt; var Defaults; Bytes : Word);
        {-Seek to position FileOfs and store defaults there}
    end;

  {=================================================================}

implementation

  constructor Cloner.Init(FName : string; DUT : DateUpdateType);
    {-Open file for cloning}
  begin
    if not Cloner.InitCustom(FName, DUT, DefBufSize) then
      Fail;
  end;

  constructor Cloner.InitCustom(FName : string; DUT : DateUpdateType;
                                BufSize : Word);
    {-Open file for cloning}
  begin
    cUpdate := DUT;
    cPos := 0;
    cBufPtr := nil;
    cError := 0;
    cBufSize := BufSize;

    {check buffer size}
    if (BufSize < MinBufSize) or not GetMemCheck(cBufPtr, BufSize) then begin
      InitStatus := epFatal+ecOutOfMemory;
      Fail;
    end;

    {Open file}
    Assign(cFile, FName);
    Reset(cFile, 1);
    InitStatus := IoResult;
    if InitStatus <> 0 then begin
      Inc(InitStatus, epFatal);
      FreeMemCheck(cBufPtr, cBufSize);
      Fail;
    end;

    {Save the original date/time}
    GetFTime(cFile, cTime);
  end;

  constructor Cloner.InitAndFind(FName : string; DUT : DateUpdateType;
                                 var ID; IdSize : Word);
    {-Open file and find ID. Uses FindDefaultsEnd}
  begin
    if not Cloner.InitCustom(FName, DUT, DefBufSize) then
      Fail;

    if not FindDefaultsEnd(ID, IdSize, 0) then begin
      if cError = 0 then
        InitStatus := epFatal+ecStringNotFound
      else
        InitStatus := epFatal+(cError mod 10000);
      Fail;
    end;
  end;

  destructor Cloner.Done;
    {-Close clone file, adjust time stamp, and deallocate buffer}
  var
    Status : Word;
    DT : DateTime;
  begin
    {dispose of buffer}
    FreeMemCheck(cBufPtr, cBufSize);

    case cUpdate of
      UpdateNone : {Set original date/time}
        SetFTime(cFile, cTime);
      UpdateDate : {Change the date but not the time}
        begin
          UnpackTime(cTime, DT);
          with DT do
            GetDate(Year, Month, Day, Status);
          PackTime(DT, cTime);
          SetFTime(cFile, cTime);
        end;
      UpdateAll :  {Let new date and time take effect}
        ;
    end;

    Close(cFile);
    cError := IoResult;
  end;

  function Cloner.FindDefaultsEnd(var ID; IdSize : Word;
                                  Skip : LongInt) : Boolean;
    {-Find the ID string in the clone file}
  var
    BufLessId : Word;
    BufPos : Word;
    BytesRead : Word;
  begin
    FindDefaultsEnd := False;

    {Initialize for search}
    BufLessId := cBufSize-IdSize;

    {Initialize file position}
    cPos := FileSize(cFile)-Skip-cBufSize;
    if cPos < 0 then
      cPos := 0;
    Seek(cFile, cPos);
    cError := IoResult;
    if cError <> 0 then begin
      Inc(cError, epNonFatal);
      Exit;
    end;

    {Fill the buffer}
    BlockRead(cFile, cBufPtr^, cBufSize, BytesRead);
    cError := IoResult;
    if cError <> 0 then begin
      Inc(cError, epNonFatal);
      Exit;
    end;

    {Search the buffer}
    BufPos := Search(cBufPtr^, BytesRead, ID, IdSize);

    {Loop until Id found or beginning of file reached}
    while (BufPos = MinusOne) and (cPos > 0) do begin
      {Move the front end of the buffer to the tail of the buffer}
      MoveFast(cBufPtr^, cBufPtr^[BufLessId], IdSize);  {!!.01}

      {Back up the file pointer}
      Dec(cPos, BufLessId);
      if cPos < 0 then
        cPos := 0;
      Seek(cFile, cPos);
      cError := IoResult;
      if cError <> 0 then begin
        Inc(cError, epNonFatal);
        Exit;
      end;

      {Fill the front part of the buffer}
      BlockRead(cFile, cBufPtr^, BufLessId, BytesRead);
      cError := IoResult;
      if cError <> 0 then begin
        Inc(cError, epNonFatal);
        Exit;
      end;

      if BytesRead < BufLessId then
        {Move things forward if necessary}
        MoveFast(cBufPtr^[BufLessId], cBufPtr^[BytesRead], IdSize);  {!!.01}

      if BytesRead > 0 then begin
        {Adjust BytesRead to indicate the actual number of bytes in the buffer}
        Inc(BytesRead, IdSize);
        {Search the buffer for Id}
        BufPos := Search(cBufPtr^, BytesRead, ID, IdSize);
      end;
    end;

    if BufPos <> MinusOne then begin
      {Calculate the actual position in the file}
      Inc(cPos, BufPos);
      FindDefaultsEnd := True;
    end;
  end;

  function Cloner.FindDefaultsStart(var ID; IdSize : Word;
                                    Skip : LongInt) : Boolean;
    {-Find the ID string in the clone file}
  var
    BufPos : Word;
    BytesRead : Word;
  begin
    FindDefaultsStart := False;

    {Initialize for search}
    Seek(cFile, Skip);
    cError := IoResult;
    if cError <> 0 then begin
      Inc(cError, epNonFatal);
      Exit;
    end;

    {Read the first bufferful}
    BlockRead(cFile, cBufPtr^, cBufSize, BytesRead);
    cError := IoResult;
    if cError <> 0 then begin
      Inc(cError, epNonFatal);
      Exit;
    end;

    cPos := BytesRead;

    {Search the buffer}
    BufPos := Search(cBufPtr^, BytesRead, ID, IdSize);

    {Loop until ID found or end of file reached}
    while (BufPos = MinusOne) and (BytesRead >= IdSize) do begin
      {Move the tail end of the buffer to the front of the buffer}
      MoveFast(cBufPtr^[BytesRead-IdSize], cBufPtr^, IdSize);  {!!.01}

      {Read the next bufferful}
      BlockRead(cFile, cBufPtr^[IdSize], cBufSize-IdSize, BytesRead);
      if BytesRead > 0 then begin
        Inc(cPos, BytesRead);
        Inc(BytesRead, IdSize);
        BufPos := Search(cBufPtr^, BytesRead, ID, IdSize);
      end;
    end;

    if BufPos <> MinusOne then begin
      FindDefaultsStart := True;
      cPos := cPos-BytesRead+BufPos;
    end;
  end;

  procedure Cloner.LoadDefaults(FileOfs : LongInt; var Defaults; Bytes : Word);
    {-Seek to position FileOfs and read defaults there}
  begin
    Seek(cFile, FileOfs);
    cError := IoResult;
    if cError = 0 then begin
      {Read defaults}
      BlockRead(cFile, Defaults, Bytes);
      cError := IoResult;
    end;
    if cError <> 0 then
      Inc(cError, epNonFatal);
  end;

  procedure Cloner.StoreDefaults(FileOfs : LongInt; var Defaults; Bytes : Word);
    {-Seek to position FileOfs and store defaults there}
  begin
    Seek(cFile, FileOfs);
    cError := IoResult;
    if cError = 0 then begin
      {Write defaults}
      BlockWrite(cFile, Defaults, Bytes);
      cError := IoResult;
    end;
    if cError <> 0 then
      Inc(cError, epNonFatal);
  end;

  function Cloner.GetLastError : Word;
    {-Get the code for the last error}
  begin
    GetLastError := cError;
    cError := 0;
  end;

  function Cloner.GetPos : LongInt;
    {-Get position where an ID string was found (offset returned is the
      start of the ID string), or 0 if an error is pending}
  begin
    if cError <> 0 then
      GetPos := 0
    else
      GetPos := cPos;
  end;

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
