{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                    OPDATE.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1988, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpDate;
  {-Date/time routines for use by/with OPENTRY.}
  {-Based in part on the public domain units DATES.PAS by Scott Bussinger and
    JULIAN.PAS by Carley Phillips. Many thanks to both.}

interface

uses
  Use32,
{$IFDEF Dpmi}
  Dpmi,
  WinApi,
{$ENDIF}
{$IFDEF VIRTUALPASCAL}
  Strings,
  VpSysLow,
{$ENDIF}
  Dos,
  OpInline,
  OpString;

const
  DateLen = 40;              {maximum length of Picture strings}
type
  {$IFDEF FourByteDates}
    Date = LongInt;
  {$ELSE}
    Date = Word;
  {$ENDIF}

  DayType = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
  DateString = string[DateLen];
  Time = LongInt;
  DateTimeRec =
    record
      D : Date;
      T : Time;
    end;
const
  {$IFDEF FourByteDates}
    MinYear = 1600;
    MaxYear = 3999;
    MinDate = $00000000;     {= 01/01/1600}
    MaxDate = $000D6025;     {= 12/31/3999}
    Date1900 = $0001AC05;    {= 01/01/1900}
    Date1980 = $00021E28;    {= 01/01/1980}
    Date2000 = $00023AB1;    {= 01/01/2000}
    BadDate = $FFFFFFFF;
  {$ELSE}
    MinYear  = 1900;
    MaxYear  = 2078;
    MinDate  = $0000;        {= 01/01/1900}
    MaxDate  = $FF62;        {= 12/31/2078}
    Date1900 = $0000;        {= 01/01/1900}
    Date1980 = $7223;        {= 01/01/1980}
    Date2000 = $8EAC;        {= 01/01/2000}
    BadDate  = $FFFF;
  {$ENDIF}

  Threshold2000 : Integer = 1900;

  MinTime = 0;               {= 00:00:00 am}
  MaxTime = 86399;           {= 23:59:59 pm}
  BadTime = $FFFFFFFF;

  SecondsInDay = 86400;      {number of seconds in a day}
  SecondsInHour = 3600;      {number of seconds in an hour}
  SecondsInMinute = 60;      {number of seconds in a minute}
  HoursInDay = 24;           {number of hours in a day}
  MinutesInHour = 60;        {number of minutes in an hour}

var
  DefaultYear : Integer;     {default year--used by DateStringToDMY}
  DefaultMonth : Byte;       {default month}

const
  {the following characters are meaningful in date Picture strings}
  MonthOnly = 'm';           {these are for date/time pictures, and allow}
  DayOnly = 'd';             {  numbers and spaces only}
  YearOnly = 'y';
  {if uppercase letters are used, numbers are padded with ' ' rather than '0'}
  MonthOnlyU = 'M';
  DayOnlyU = 'D';
  DateSlash = '/';
  SlashChar : Char = '/';

  {'n'/'N' may be used in place of 'm'/'M' when the name of the month is
   desired instead of its number. E.g., 'dd/nnn/yyyy' -> '01-Jan-1980'.
   'dd/NNN/yyyy' -> '01-JAN-1980' (if SlashChar = '-'). The abbreviation used
   is based on the width of the subfield (3 in the example) and the current
   contents of the MonthString array.}
  NameOnly = 'n';
  NameOnlyU = 'N';

  {'w'/'W' may be used to include the day of the week in a date string. E.g.,
  'www dd nnn yyyy' -> 'Mon 01 Jan 1989'. The abbreviation used is based on
  the width of the subfield (3 in the example) and the current contents of the
  DayString array. Note that OPFIELD/OPENTRY will not allow the user to enter
  text into a subfield containing 'w' or 'W'. The day of the week will be
  supplied automatically when a valid date is entered.}
  WeekDayOnly = 'w';
  WeekDayOnlyU = 'W';

const
  {the following characters are meaningful in time Picture strings}
  HourOnly = 'h';
  MinOnly = 'm';
  SecOnly = 's';
  {if uppercase letters are used, numbers are padded with ' ' rather than '0'}
  HourOnlyU = 'H';
  MinOnlyU = 'M';
  SecOnlyU = 'S';
  {'hh:mm:ss te' -> '12:00:00 pm', 'hh:mmt' -> '12:00p'}
  TimeOnly = 't';            {generates 'p', 'P', 'a', or 'A'}
  EmOnly = 'e';              {optional--generates 'm' or 'M'}
  TimeColon = ':';
  ColonChar : Char = ':';
  UpCaseTime : Boolean = False; {if true, 't' and 'e' force upper case}

const
  MonthString : array[1..12] of string[12] = (
    'January', 'February', 'March', 'April', 'May', 'June', 'July',
    'August', 'September', 'October', 'November', 'December');
const
  DayString : array[DayType] of string[15] = (
    'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');

  {-------julian date routines---------------}

function ValidDate(Day, Month, Year : Integer) : Boolean;
  {-Verify that day, month, year is a valid date}

function DMYtoDate(Day, Month, Year : Integer) : Date;
  {-Convert from day, month, year to a julian date}

procedure DateToDMY(Julian : Date; var Day, Month, Year : Integer);
  {-Convert from a julian date to day, month, year}

function IncDate(Julian : Date; Days, Months, Years : Integer) : Date;
  {-Add (or subtract) the number of days, months, and years to a date}

function IncDateTrunc(Julian : Date; Months, Years : Integer) : Date;
  {-Add (or subtract) the specified number of months and years to a date}

procedure DateDiff(Date1, Date2 : Date; var Days, Months, Years : Integer);
  {-Return the difference in days,months,years between two valid julian dates}

function DayOfWeek(Julian : Date) : DayType;
  {-Return the day of the week for the date}

function DayOfWeekDMY(Day, Month, Year : Integer) : DayType;
  {-Return the day of the week for the day, month, year}

function DateStringToDate(Picture, S : DateString) : Date;
  {-Convert S, a string of the form indicated by Picture, to a julian date.
    Picture and S must be of equal lengths}

function DateToDateString(Picture : DateString; Julian : Date) : DateString;
  {-Convert Julian to a string of the form indicated by Picture}

function Today : Date;
  {-Returns today's date as a julian}

function DateToSortString(Julian : Date) : string;
  {-Convert a date to a sortable string}

function SortStringToDate(S : string) : Date;
  {-Convert a sortable string form to a date}

function IsLeapYear(Year : Integer) : Boolean;
  {-Return True if Year is a leap year}

function DaysInMonth(Month, Year : Integer) : Integer;
  {-Return the number of days in the specified month of a given year}

  {-------date string routines---------------}

function DateStringToDMY(Picture, S : DateString; var D, M, Y : Integer) : Boolean;
  {-Extract day, month, and year from S, returning true if string is valid}

function DMYtoDateString(Picture : DateString; Day, Month, Year : Integer) : DateString;
  {-Merge the month, day, and year into the picture}

function TodayString(Picture : DateString) : DateString;
  {-Returns today's date as a string of the specified form}

  {-------time routines---------------}

function ValidTime(Hours, Minutes, Seconds : Integer) : Boolean;
  {-Return true if Hours:Minutes:Seconds is a valid time}

procedure TimeToHMS(T : Time; var Hours, Minutes, Seconds : Byte);
  {-Convert a Time variable to Hours, Minutes, Seconds}

function HMStoTime(Hours, Minutes, Seconds : Byte) : Time;
  {-Convert Hours, Minutes, Seconds to a Time variable}

function TimeStringToHMS(Picture, St : DateString; var H, M, S : Integer) : Boolean;
  {-Extract Hours, Minutes, Seconds from St, returning True if string is valid}

function TimeStringToTime(Picture, S : DateString) : Time;
  {-Convert S, a string of the form indicated by Picture, to Time}

function TimeToTimeString(Picture : DateString; T : Time) : DateString;
  {-Convert T to a string of the form indicated by Picture}

function TimeToAmPmString(Picture : DateString; T : Time) : DateString;
  {-Convert T to a string of the form indicated by Picture. Times are always
    displayed in am/pm format.}

function CurrentTime : Time;
  {-Returns current time in seconds since midnight}

function CurrentTimeString(Picture : DateString) : DateString;
  {-Returns current time as a string of the specified form}

procedure TimeDiff(Time1, Time2 : Time; var Hours, Minutes, Seconds : Byte);
  {-Return the difference in hours,minutes,seconds between two times}

function IncTime(T : Time; Hours, Minutes, Seconds : Byte) : Time;
  {-Add the specified hours,minutes,seconds to T and return the result}

function DecTime(T : Time; Hours, Minutes, Seconds : Byte) : Time;
  {-Subtract the specified hours,minutes,seconds from T and return the result}

function TimeToSortString(T : Time) : string;
  {-Convert a time variable to a sortable string}

function RoundToNearestHour(T : Time; Truncate : Boolean) : Time;
  {-Round T to the nearest hour, or Truncate minutes and seconds from T}

function RoundToNearestMinute(T : Time; Truncate : Boolean) : Time;
  {-Round T to the nearest minute, or Truncate seconds from T}

function SortStringToTime(S : string) : Time;
  {-Convert a sortable string to a time variable}

  {-------- routines for DateTimeRec records ---------}

procedure DateTimeDiff(DT1, DT2 : DateTimeRec; var Days : Word; var Secs : LongInt);
  {-Return the difference in days and seconds between two points in time}

procedure IncDateTime(var DT1, DT2 : DateTimeRec; Days : Integer; Secs : LongInt);
  {-Increment (or decrement) DT1 by the specified number of days and seconds
    and put the result in DT2}

{!!.30 - New}
procedure DosToDateTime(var DT : DateTime; var DTR : DateTimeRec);
  {-Convert a DOS DateTime to an OPDATE DateTimeRec }

{!!.30 - New}
procedure DateTimeToDos(var DTR : DateTimeRec; var DT : DateTime);
  {-Convert an OPDATE DateTimeRec to a DOS DateTime }

{!!.30 - New}
procedure PackedToDateTime(PackedTime : LongInt; var DT : DateTimeRec);
  {-Convert a packed date/time to a DateTimeRec }

{!!.30 - New}
procedure DateTimeToPacked(var DT : DateTimeRec; var PackedTime : LongInt);
  {-Convert a DateTimeRec to a packed date/time }

  {-------- routines for international date/time strings ---------}

function InternationalDate(WholeYear, ZeroPad : Boolean) : DateString;
  {-Return a picture mask for a date string, based on DOS's country info}

function InternationalTime(WithSeconds, ZeroPad : Boolean;
                           ExtraSpace, WithEm : Boolean) : DateString;
  {-Return a picture mask for a time string, based on DOS's country info}

  {-------- the following are interfaced for use by OPFIELD -------}

{.Z+}

type
  CountryInfoPtr = ^CountryInfo;
  CountryInfo =
    record
      DateFormat : Word;     {0=US (mdy), 1=Europe (dmy), 2 = Japan (ymd)}
      case Byte of
        2 : (                  {DOS 2.x}
          CurrencySym : Char;  {'$' for US}
          Unused1 : Byte;      {0}
          CommaSym1 : Char;    {',' for US}
          Unused2 : Byte;      {0}
          DecimalSym1 : Char); {'.' for US}

        3 : (                  {DOS 3.x or higher}
          CurrencyStr : array[1..5] of Char; {ASCIIZ string}
          CommaSym2 : Char;    {',' for US}
          Unused3 : Byte;      {0}
          DecimalSym2 : Char;  {'.' for US}
          Unused4 : Byte;      {0}
          DateSym : Char;      {'-' for US}
          Unused5 : Byte;      {0}
          TimeSym : Char;      {':' for US}
          Unused6 : Byte;      {0}
          CurrencyForm : Byte; {0-4}
          Decimals : Byte;     {# of digits after decimal point}
          TimeForm : Byte;     {bit 0 = 0 for 12-hour clock; 1 for 24-hour}
          Unused7 : array[1..18] of Byte);  {!!.20}
    end;

function GetCountryInfo(var Dos2 : Boolean; var Info : CountryInfo) : Boolean;
  {-Return a country information table in Info}

function DateStringIsBlank(Picture, S : DateString) : Boolean;
  {-Return True if the month, day, and year in S are all blank}

{.Z-}

  {==========================================================================}

implementation

const
  {$IFDEF FourByteDates}
    First2Months = 59;         {1600 was a leap year}
    FirstDayOfWeek = Saturday; {01/01/1600 was a Saturday}
  {$ELSE}
    First2Months = 58;         {1900 was not a leap year}
    FirstDayOfWeek = Monday;   {01/01/1900 was a Monday}
  {$ENDIF}

  function IsLeapYear(Year : Integer) : Boolean;
    {-Return True if Year is a leap year}
  begin
    IsLeapYear := (Year mod 4 = 0) and (Year mod 4000 <> 0) and
      ((Year mod 100 <> 0) or (Year mod 400 = 0));
  end;

  function DaysInMonth(Month, Year : Integer) : Integer;
    {-Return the number of days in the specified month of a given year}
  begin
    if Word(Year) < 100 then begin
      Inc(Year, 1900);
      if Year < Threshold2000 then
        Inc(Year, 100);
    end;

    case Month of
      1, 3, 5, 7, 8, 10, 12 :
        DaysInMonth := 31;
      4, 6, 9, 11 :
        DaysInMonth := 30;
      2 :
        DaysInMonth := 28+Ord(IsLeapYear(Year));
    else
      DaysInMonth := 0;
    end;
  end;

  function ValidDate(Day, Month, Year : Integer) : Boolean;
    {-Verify that day, month, year is a valid date}
  begin
    if Word(Year) < 100 then begin
      Inc(Year, 1900);
      if Year < Threshold2000 then
        Inc(Year, 100);
    end;

    if (Day < 1) or (Year < MinYear) or (Year > MaxYear) then
      ValidDate := False
    else case Month of
      1..12 :
        ValidDate := Day <= DaysInMonth(Month, Year);
    else
      ValidDate := False;
    end
  end;

  function DMYtoDate(Day, Month, Year : Integer) : Date;
    {-Convert from day, month, year to a julian date}
  begin
    if Word(Year) < 100 then begin
      Inc(Year, 1900);
      if Year < Threshold2000 then
        Inc(Year, 100);
    end;

    if not ValidDate(Day, Month, Year) then
      DMYtoDate := BadDate
    else if (Year = MinYear) and (Month < 3) then
      if Month = 1 then
        DMYtoDate := Pred(Day)
      else
        DMYtoDate := Day+30
    else begin
      if Month > 2 then
        Dec(Month, 3)
      else begin
        Inc(Month, 9);
        Dec(Year);
      end;
      Dec(Year, MinYear);
      DMYtoDate :=
        {$IFDEF FourByteDates}
          ((LongInt(Year div 100)*146097) div 4)+
          ((LongInt(Year mod 100)*1461) div 4)+
        {$ELSE}
          ((LongInt(Year)*1461) div 4)+
        {$ENDIF}
          (((153*Month)+2) div 5)+Day+First2Months;
    end;
  end;

  procedure DateToDMY(Julian : Date; var Day, Month, Year : Integer);
    {-Convert from a julian date to month, day, year}
  var
    I, J : LongInt;
  begin
    if Julian = BadDate then begin
      Day := 0;
      Month := 0;
      Year := 0;
    end
    else if Julian <= First2Months then begin
      Year := MinYear;
      if Julian <= 30 then begin
        Month := 1;
        Day := Succ(Julian);
      end
      else begin
        Month := 2;
        Day := Julian-30;
      end;
    end
    else begin
      I := (4*LongInt(Julian-First2Months))-1;
      {$IFDEF FourByteDates}
      J := (4*((I mod 146097) div 4))+3;
      Year := (100*(I div 146097))+(J div 1461);
      I := (5*(((J mod 1461)+4) div 4))-3;
      {$ELSE}
      Year := I div 1461;
      I := (5*((I mod 1461) div 4)) + 2;
      {$ENDIF}
      Month := I div 153;
      Day := ((I mod 153)+5) div 5;
      if Month < 10 then
        Inc(Month, 3)
      else begin
        Dec(Month, 9);
        Inc(Year);
      end;
      Inc(Year, MinYear);
    end;
  end;

  function IncDate(Julian : Date; Days, Months, Years : Integer) : Date;
    {-Add (or subtract) the number of months, days, and years to a date.
      Months and years are added before days. No overflow/underflow
      checks are made}
  var
    Day, Month, Year, Day28Delta : Integer;
  begin
    DateToDMY(Julian, Day, Month, Year);
    Day28Delta := Day-28;
    if Day28Delta < 0 then
      Day28Delta := 0
    else
      Day := 28;

    Inc(Year, Years);
    Inc(Year, Months div 12);
    Inc(Month, Months mod 12);
    if Month < 1 then begin
      Inc(Month, 12);
      Dec(Year);
    end
    else if Month > 12 then begin
      Dec(Month, 12);
      Inc(Year);
    end;

    Julian := DMYtoDate(Day, Month, Year);
    if Julian <> BadDate then begin
      Inc(Julian, Days);
      Inc(Julian, Day28Delta);
    end;
    IncDate := Julian;
  end;

  function IncDateTrunc(Julian : Date; Months, Years : Integer) : Date;
    {-Add (or subtract) the specified number of months and years to a date}
  var
    Day, Month, Year : Integer;
    MaxDay, Day28Delta : Integer;
  begin
    DateToDMY(Julian, Day, Month, Year);
    Day28Delta := Day-28;
    if Day28Delta < 0 then
      Day28Delta := 0
    else
      Day := 28;

    Inc(Year, Years);
    Inc(Year, Months div 12);
    Inc(Month, Months mod 12);
    if Month < 1 then begin
      Inc(Month, 12);
      Dec(Year);
    end
    else if Month > 12 then begin
      Dec(Month, 12);
      Inc(Year);
    end;

    Julian := DMYtoDate(Day, Month, Year);
    if Julian <> BadDate then begin
      MaxDay := DaysInMonth(Month, Year);
      if Day+Day28Delta > MaxDay then
        Inc(Julian, MaxDay-Day)
      else
        Inc(Julian, Day28Delta);
    end;
    IncDateTrunc := Julian;
  end;

  procedure DateDiff(Date1, Date2 : Date; var Days, Months, Years : Integer);
    {-Return the difference in days,months,years between two valid julian dates}
  var
    Day1, Day2, Month1, Month2, Year1, Year2 : Integer;
  begin
    {we want Date2 > Date1}
    if Date1 > Date2 then
      ExchangeStructs(Date1, Date2, SizeOf(Date));

    {convert dates to day,month,year}
    DateToDMY(Date1, Day1, Month1, Year1);
    DateToDMY(Date2, Day2, Month2, Year2);

    {days first}
    if Day2 < Day1 then begin
      Dec(Month2);
      if Month2 = 0 then begin
        Month2 := 12;
        Dec(Year2);
      end;
      Inc(Day2, DaysInMonth(Month2, Year2));
    end;
    Days := Day2-Day1;

    {now months and years}
    if Month2 < Month1 then begin
      Inc(Month2, 12);
      Dec(Year2);
    end;
    Months := Month2-Month1;
    Years := Year2-Year1;
  end;

  function DayOfWeek(Julian : Date) : DayType;
    {-Return the day of the week for the date. Returns DayType(7) if Julian =
      BadDate.}
  var
    B : Byte;
  begin
    if Julian = BadDate then begin
      B := 7;
      DayOfWeek := DayType(B);
    end
    else
      DayOfWeek := DayType( (Julian+Ord(FirstDayOfWeek)) mod 7 );
  end;

  function DayOfWeekDMY(Day, Month, Year : Integer) : DayType;
    {-Return the day of the week for the day, month, year}
  begin
    DayOfWeekDMY := DayOfWeek( DMYtoDate(Day, Month, Year) );
  end;

  function MonthStringToMonth(MSt : DateString; Width : Byte) : Byte;
    {-Convert the month name in MSt to a month (1..12)}
  var
    MLen : Byte absolute MSt;
    S : DateString;
    SLen : Byte absolute S;
    I : Word;
  begin
    if Width > MLen then
      FillChar(MSt[MLen+1], Width-MLen, ' ');
    MLen := Width;
    MSt := StUpcase(MSt);

    for I := 1 to 12 do begin
      S := Stupcase(MonthString[I]);
      if Width > SLen then
        FillChar(S[SLen+1], Width-SLen, ' ');
      SLen := Width;
      if MSt = S then begin
        MonthStringToMonth := I;
        Exit;
      end;
    end;

    MonthStringToMonth := 0;
  end;

  procedure ExtractFromPicture(var Picture, S : DateString;
                               Ch : Char; var I : Integer;
                               Blank, Default : Integer);
    {-Extract the value of the subfield specified by Ch from S and return in
      I. I will be set to -1 in case of an error, Blank if the subfield exists
      in Picture but is empty, Default if the subfield doesn't exist in
      Picture.}
  var
    Tmp : DateString;
    TLen : Byte absolute Tmp;
    PLen : Byte absolute Picture;
    J, K, W : Integer;
    Code : Word;
  begin
    {find the start of the subfield}
    I := Default;
    J := Pos(Ch, Picture);
    Ch := Upcase(Ch);
    K := Pos(Ch, Picture);
    if (J = 0) or ((K > 0) and (K < J)) then
      J := K;
    if (J = 0) or (Length(S) <> Length(Picture)) then
      Exit;

    {extract the substring}
    TLen := 0;
    W := 0;
    while (Upcase(Picture[J]) = Ch) and (J <= PLen) do begin
      Inc(W);
      if S[J] <> ' ' then begin
        Inc(TLen);
        Tmp[TLen] := S[J];
      end;
      Inc(J);
    end;

    if TrimSpaces(Tmp) = '' then
      I := Blank
    else if Ch = NameOnlyU then begin
      I := MonthStringToMonth(Tmp, W);
      if I = 0 then
        I := -1;
    end
    else begin
      {convert to a value}
      Val(Tmp, I, Code);
      if Code <> 0 then
        I := -1;
    end;
  end;

  function DateStringToDMY(Picture, S : DateString; var D, M, Y : Integer) : Boolean;
    {-Extract day, month, and year from S, returning true if string is valid}
  begin
    ExtractFromPicture(Picture, S, NameOnly, M, -1, 0);
    if M = 0 then
      ExtractFromPicture(Picture, S, MonthOnly, M, -1, DefaultMonth);
    ExtractFromPicture(Picture, S, DayOnly, D, -1, 1);
    ExtractFromPicture(Picture, S, YearOnly, Y, -1, DefaultYear);
    DateStringToDMY := ValidDate(D, M, Y);
  end;

  function DateStringIsBlank(Picture, S : DateString) : Boolean;
    {-Return True if the month, day, and year in S are all blank}
  var
    M, D, Y : Integer;
  begin
    ExtractFromPicture(Picture, S, NameOnly,  M, -2, 0);
    if M = 0 then
      ExtractFromPicture(Picture, S, MonthOnly, M, -2, -2);
    ExtractFromPicture(Picture, S, DayOnly,   D, -2, -2);
    ExtractFromPicture(Picture, S, YearOnly,  Y, -2, -2);
    DateStringIsBlank := (M = -2) and (D = -2) and (Y = -2);
  end;

  function DateStringToDate(Picture, S : DateString) : Date;
    {-Convert S, a string of the form indicated by Picture, to a julian date.
      Picture and S must be of equal lengths}
  var
    Month, Day, Year : Integer;
  begin
    {extract day, month, year from S}
    if DateStringToDMY(Picture, S, Day, Month, Year) then
      {convert to julian date}
      DateStringToDate := DMYtoDate(Day, Month, Year)
    else
      DateStringToDate := BadDate;
  end;

  procedure SubstChar(var Picture : DateString; OldCh, NewCh : Char);
    {-Replace all instances of OldCh in Picture with NewCh}
  var
    I : Byte;
    UpCh : Char;
  begin
    UpCh := Upcase(OldCh);
    if (Pos(OldCh, Picture) <> 0) or (Pos(UpCh, Picture) <> 0) then
      for I := 1 to Length(Picture) do
        if Upcase(Picture[I]) = UpCh then
          Picture[I] := NewCh;
  end;

  {!!.30 Was MergeIntoPicture, modified to take ReplStr parameter }
  procedure MergeIntoPicturePrim( Picture : DateString; var ReplStr : DateString;
                                  Ch : Char; I : Integer);

    {-Merge I into location in Picture indicated by format character Ch}
  var
    Tmp : DateString;
    TLen : Byte absolute Tmp;
    PLen : Byte absolute Picture;
    J, K : Integer;
    UCh, CPJ, CTI : Char;
  begin
    {find the start of the subfield}
    J := Pos(Ch, Picture);
    UCh := Upcase(Ch);
    if J = 0 then begin
      J := Pos(UCh, Picture);
      if J = 0 then
        Exit;
    end;

    {find the end of the subfield}
    K := J;
    while (J < PLen) and (Upcase(Picture[J+1]) = UCh) do
      Inc(J);

    if (UCh = WeekDayOnlyU) or (UCh = NameOnlyU) then begin
      if UCh = WeekDayOnlyU then
        case I of
          Ord(Sunday)..Ord(Saturday) :
            Tmp := DayString[DayType(I)];
          else
            Tmp := '';
        end
      else
        case I of
          1..12 :
            Tmp := MonthString[I];
          else
            Tmp := '';
        end;
      K := Succ(J-K);
      if K > TLen then
        FillChar(Tmp[TLen+1], K-TLen, ' ');
      TLen := K;
    end
    else
      {convert I to a string}
      Str(I:DateLen, Tmp);

    {now merge}
    I := TLen;
    CPJ := Picture[J];
    while (Upcase(CPJ) = UCh) and (J > 0) and (I > 0) do begin
      CTI := Tmp[I];
      if (UCh = NameOnlyU) or (UCh = WeekDayOnlyU) then begin
        case CPJ of
          NameOnlyU, WeekDayOnlyU :
            CTI := Upcase(CTI);
        end;
      end
      {change spaces to 0's if desired}
      else if (CPJ >= 'a') and (CTI = ' ') then
        CTI := '0';
      ReplStr[J] := CTI;
      Dec(J);
      Dec(I);
      CPJ := Picture[J];
    end;
  end;

  procedure MergeIntoPicture(var Picture, ReplStr : DateString); {!!.30}
  var                                                            {!!.30}
    Len : Byte absolute Picture;                                 {!!.30}
    I   : Byte;                                                  {!!.30}
                                                                 {!!.30}
  begin                                                          {!!.30}
    for I := 1 to Len do                                         {!!.30}
      if (ReplStr[I] <> #0) then                                 {!!.30}
        Picture[I] := ReplStr[I];                                {!!.30}
  end;                                                           {!!.30}

  function DMYtoDateString(Picture : DateString; Day, Month, Year : Integer) : DateString;
    {-Merge the month, day, and year into the picture}
  var
    DOW     : Integer;
    ReplStr : DateString;

  begin
    { initialize the replacement string }
    FillChar(ReplStr, SizeOf(DateString), 0);
    ReplStr[0] := Picture[0];

    if Word(Year) < 100 then begin
      Inc(Year, 1900);
      if Year < Threshold2000 then
        Inc(Year, 100);
    end;

    DOW := Integer( DayOfWeekDMY(Day, Month, Year) );
    MergeIntoPicturePrim(Picture, ReplStr, MonthOnly,   Month); {!!.30}
    MergeIntoPicturePrim(Picture, ReplStr, DayOnly,     Day);   {!!.30}
    MergeIntoPicturePrim(Picture, ReplStr, YearOnly,    Year);  {!!.30}
    MergeIntoPicturePrim(Picture, ReplStr, NameOnly,    Month); {!!.30}
    MergeIntoPicturePrim(Picture, ReplStr, WeekDayOnly, DOW);   {!!.30}

    {map slashes}
    SubstChar(Picture, DateSlash, SlashChar);

    MergeIntoPicture(Picture, ReplStr);                         {!!.30}

    DMYtoDateString := Picture;
  end;

  function DateToDateString(Picture : DateString; Julian : Date) : DateString;
    {-Convert Julian to a string of the form indicated by Picture}
  var
    Month, Day, Year : Integer;
  begin
    if Julian = BadDate then begin
      {map picture characters to spaces}
      SubstChar(Picture, MonthOnly,   ' ');
      SubstChar(Picture, NameOnly,    ' ');
      SubstChar(Picture, DayOnly,     ' ');
      SubstChar(Picture, YearOnly,    ' ');
      SubstChar(Picture, WeekDayOnly, ' ');

      {map slashes}
      SubstChar(Picture, DateSlash, SlashChar);

      DateToDateString := Picture;
    end
    else begin
      {convert Julian to day/month/year}
      DateToDMY(Julian, Day, Month, Year);

      {merge the month, day, and year into the picture}
      DateToDateString := DMYtoDateString(Picture, Day, Month, Year);
    end;
  end;

  function Today : Date;
    {-Returns today's date as a julian}
  var
    Year, Month, Day, DayOfWeek : Word;
  begin
    GetDate(Year, Month, Day, DayOfWeek);
    Today := DMYtoDate(Day, Month, Year);
  end;

  function TodayString(Picture : DateString) : DateString;
    {-Returns today's date as a string of the specified form}
  begin
    TodayString := DateToDateString(Picture, Today);
  end;

  function DateToSortString(Julian : Date) : string;
    {-Convert a date to a sortable string }
    {$IFDEF FourByteDates}
      const
        Res :
          record case Byte of
              0 : (Len : Byte; W1, W2 : SmallWord);
              1 : (Str : string[4]);
          end = (Str : '    ');
      var
        DRec :
          record
            D1, D2 : SmallWord;
          end absolute Julian;
    {$ELSE}
      const
        Result :
          record case Byte of
            0 : (Len : Byte; W : SmallWord);
            1 : (Str : String[2]);
          end = (Str : '  ');
    {$ENDIF}
  begin
    {$IFDEF FourByteDates}
      Res.W1 := Swap(DRec.D2);
      Res.W2 := Swap(DRec.D1);
    {$ELSE}
      Res.W := Swap(Julian);
    {$ENDIF}
    DateToSortString := Res.Str;
  end;

  function SortStringToDate(S : string) : Date;
    {-Convert a sortable string to a date}
    {$IFDEF FourByteDates}
      var
        Temp :
          record case Byte of
              0 : (Len : Byte; W1, W2 : SmallWord);
              1 : (X : Byte; D : LongInt);
          end absolute S;
    {$ELSE}
      var
        Temp :
          record
            Len : Byte; W : SmallWord;
          end absolute S;
    {$ENDIF}
  begin
    {$IFDEF FourByteDates}
      Temp.W1 := Swap(Temp.W1);
      Temp.W2 := Swap(Temp.W2);
      SortStringToDate := SwapWord(Temp.D);
    {$ELSE}
      SortStringToDate := Swap(Temp.W);
    {$ENDIF}
  end;

  procedure TimeToHMS(T : Time; var Hours, Minutes, Seconds : Byte);
    {-Convert a Time variable to Hours, Minutes, Seconds}
  begin
    if T = BadTime then begin
      Hours := 0;
      Minutes := 0;
      Seconds := 0;
    end
    else begin
      Hours := T div SecondsInHour;
      Dec(T, LongInt(Hours)*SecondsInHour);
      Minutes := T div SecondsInMinute;
      Dec(T, LongInt(Minutes)*SecondsInMinute);
      Seconds := T;
    end;
  end;

  function HMStoTime(Hours, Minutes, Seconds : Byte) : Time;
    {-Convert Hours, Minutes, Seconds to a Time variable}
  var
    T : Time;
  begin
    Hours := Hours mod HoursInDay;
    T := (LongInt(Hours)*SecondsInHour)+(LongInt(Minutes)*SecondsInMinute)+Seconds;
    HMStoTime := T mod SecondsInDay;
  end;

  function ValidTime(Hours, Minutes, Seconds : Integer) : Boolean;
    {-Return true if Hours:Minutes:Seconds is a valid time}
  begin
    if (Hours < 0)   or (Hours > 23) or
       (Minutes < 0) or (Minutes > 60) or
       (Seconds < 0) or (Seconds > 60) then
      ValidTime := False
    else
      ValidTime := True;
  end;

  function TimeStringToHMS(Picture, St : DateString; var H, M, S : Integer) : Boolean;
    {-Extract Hours, Minutes, Seconds from St, returning true if string is valid}
  var
    I : Word;
  begin
    {extract hours, minutes, seconds from St}
    ExtractFromPicture(Picture, St, HourOnly, H, -1, 0);
    ExtractFromPicture(Picture, St, MinOnly,  M, -1, 0);
    ExtractFromPicture(Picture, St, SecOnly,  S, -1, 0);
    if (H = -1) or (M = -1) or (S = -1) then begin
      TimeStringToHMS := False;
      Exit;
    end;

    {check for TimeOnly}
    I := Pos(TimeOnly, Picture);
    if I <> 0 then
      case Upcase(St[I]) of
        'P' :
          if (H < 12) then
            Inc(H, 12)
          else if (H = 0) or (H > 12) then
            {force BadTime}
            H := -1;
        'A', ' ' :           {treat space like 'A'}
          if H = 12 then
            H := 0
          else if (H = 0) or (H > 12) then
            {force BadTime}
            H := -1;
      else
        {force BadTime}
        H := -1;
      end;

    {check for em}
    I := Pos(EmOnly, Picture);
    if I <> 0 then
      case Upcase(St[I]) of
        'M', ' ' : {ok} ;
        else H := -1; {force BadTime result}
      end;

    TimeStringToHMS := ValidTime(H, M, S);
  end;

  function TimeStringToTime(Picture, S : DateString) : Time;
    {-Convert S, a string of the form indicated by Picture, to a Time variable}
  var
    Hours, Minutes, Seconds : Integer;
  begin
    if TimeStringToHMS(Picture, S, Hours, Minutes, Seconds) then
      TimeStringToTime := HMStoTime(Hours, Minutes, Seconds)
    else
      TimeStringToTime := BadTime;
  end;

  function TimeToTimeString(Picture : DateString; T : Time) : DateString;
    {-Convert T to a string of the form indicated by Picture}
  const
    AmPm : array[Boolean] of Char = ('a', 'p');
  var
    Hours, Minutes, Seconds : Byte;
    TPos, EPos : Byte;
    Ch1, Ch2 : Char;
    ReplStr : String;

  begin
    { initialize the replacement string }
    FillChar(ReplStr, SizeOf(DateString), 0);
    ReplStr[0] := Picture[0];

    {merge the hours, minutes, and seconds into the picture}
    TimeToHMS(T, Hours, Minutes, Seconds);

    {check for TimeOnly}
    TPos := Pos(TimeOnly, Picture);
    if TPos <> 0 then begin
      Ch1 := AmPm[Hours >= 12];
      Ch2 := 'm';
      if UpCaseTime then begin
        Ch1 := Upcase(Ch1);
        Ch2 := 'M';
      end;

      {plug in the 'p' or 'a'}
      Picture[TPos] := Ch1;

      {get position of EmOnly}
      EPos := Pos(EmOnly, Picture);

      {adjust hours}
      case Hours of
        0 : Hours := 12;
        13..23 : Dec(Hours, 12);
      end;
    end
    else
      EPos := 0;

    if T = BadTime then begin
      {map picture characters to spaces}
      SubstChar(Picture, HourOnly, ' ');
      SubstChar(Picture, MinOnly, ' ');
      SubstChar(Picture, SecOnly, ' ');
    end
    else begin
      {merge the numbers into the picture}
      MergeIntoPicturePrim(Picture, ReplStr, HourOnly, Hours);   {!!.30}
      MergeIntoPicturePrim(Picture, ReplStr, MinOnly, Minutes);  {!!.30}
      MergeIntoPicturePrim(Picture, ReplStr, SecOnly, Seconds);  {!!.30}
    end;

    {map colons}
    SubstChar(Picture, TimeColon, ColonChar);

    {plug in the em now--if we do it earlier it looks like MinOnly}
    if EPos <> 0 then
      Picture[EPos] := Ch2;

    MergeIntoPicture(Picture, ReplStr);                          {!!.30}

    TimeToTimeString := Picture;
  end;

  function TimeToAmPmString(Picture : DateString; T : Time) : DateString;
    {-Convert T to a string of the form indicated by Picture. Times are always
      displayed in am/pm format.}
  var
    SaveLen : Byte;
  begin
    SaveLen := Length(Picture);
    if Pos(TimeOnly, Picture) = 0 then
      Picture := Picture+TimeOnly;
    TimeToAmPmString := TimeToTimeString(Picture, T);
    TimeToAmPmString[0] := Char(SaveLen);
  end;

  function CurrentTime : Time;
    {-Returns current time in seconds since midnight}
  var
    Hours, Minutes, Seconds, Sec100 : Word;
  begin
    GetTime(Hours, Minutes, Seconds, Sec100);
    CurrentTime := HMStoTime(Hours, Minutes, Seconds);
  end;

  function CurrentTimeString(Picture : DateString) : DateString;
    {-Returns current time as a string of the specified form}
  begin
    CurrentTimeString := TimeToTimeString(Picture, CurrentTime);
  end;

  procedure TimeDiff(Time1, Time2 : Time; var Hours, Minutes, Seconds : Byte);
    {-Return the difference in hours,minutes,seconds between two times}
  var
    T : Time;
  begin
    if Time1 > Time2 then
      T := Time1-Time2
    else
      T := Time2-Time1;
    TimeToHMS(T, Hours, Minutes, Seconds);
  end;

  function IncTime(T : Time; Hours, Minutes, Seconds : Byte) : Time;
    {-Add the specified hours,minutes,seconds to T and return the result}
  begin
    Inc(T, HMStoTime(Hours, Minutes, Seconds));
    IncTime := T mod SecondsInDay;
  end;

  function DecTime(T : Time; Hours, Minutes, Seconds : Byte) : Time;
    {-Subtract the specified hours,minutes,seconds from T and return the result}
  begin
    Hours := Hours mod HoursInDay;
    Dec(T, HMStoTime(Hours, Minutes, Seconds));
    if T < 0 then
      DecTime := T+SecondsInDay
    else
      DecTime := T;
  end;

  function RoundToNearestHour(T : Time; Truncate : Boolean) : Time;
    {-Round T to the nearest hour, or Truncate minutes and seconds from T}
  var
    Hours, Minutes, Seconds : Byte;
  begin
    TimeToHMS(T, Hours, Minutes, Seconds);
    Seconds := 0;
    if not Truncate then
      if Minutes >= (MinutesInHour div 2) then
        Inc(Hours);
    Minutes := 0;
    RoundToNearestHour := HMStoTime(Hours, Minutes, Seconds);
  end;

  function RoundToNearestMinute(T : Time; Truncate : Boolean) : Time;
    {-Round T to the nearest minute, or Truncate seconds from T}
  var
    Hours, Minutes, Seconds : Byte;
  begin
    TimeToHMS(T, Hours, Minutes, Seconds);
    if not Truncate then
      if Seconds >= (SecondsInMinute div 2) then
        Inc(Minutes);
    Seconds := 0;
    RoundToNearestMinute := HMStoTime(Hours, Minutes, Seconds);
  end;

  function TimeToSortString(T : Time) : string;
    {-Convert a time variable to a sortable string}
  const
    Res :
      record case Byte of
          0 : (Len : Byte; W1, W2 : Word);
          1 : (Str : string[4]);
      end = (Str : '    ');
  var
    TRec :
      record
        T1, T2 : Word;
      end absolute T;
  begin
    Res.W1 := Swap(TRec.T2);
    Res.W2 := Swap(TRec.T1);
    TimeToSortString := Res.Str;
  end;

  function SortStringToTime(S : string) : Time;
    {-Convert a sortable string to a time variable}
  var
    Temp :
      record case Byte of
          0 : (Len : Byte; W1, W2 : Word);
          1 : (X : Byte; T : LongInt);
      end absolute S;
  begin
    Temp.W1 := Swap(Temp.W1);
    Temp.W2 := Swap(Temp.W2);
    SortStringToTime := SwapWord(Temp.T);
  end;

  procedure DateTimeDiff(DT1, DT2 : DateTimeRec; var Days : Word; var Secs : LongInt);
    {-Return the difference in days and seconds between two points in time}
  begin
    {swap if DT1 later than DT2}
    if (DT1.D > DT2.D) or ((DT1.D = DT2.D) and (DT1.T > DT2.T)) then
      ExchangeStructs(DT1, DT2, SizeOf(DateTimeRec));

    {the difference in days is easy}
    Days := DT2.D-DT1.D;

    {difference in seconds}
    if DT2.T < DT1.T then begin
      {subtract one day, add 24 hours}
      Dec(Days);
      Inc(DT2.T, SecondsInDay);
    end;
    Secs := DT2.T-DT1.T;
  end;

  procedure IncDateTime(var DT1, DT2 : DateTimeRec; Days : Integer; Secs : LongInt);
    {-Increment (or decrement) DT1 by the specified number of days and seconds
      and put the result in DT2}
  begin
    DT2 := DT1;

    {date first}
    {$IFDEF FourByteDates}
      Inc(DT2.D, LongInt(Days));
    {$ELSE}
      Inc(Integer(DT2.D), Days);
    {$ENDIF}

    if Secs < 0 then begin
      {change the sign}
      Secs := -Secs;

      {adjust the date}
      Dec(DT2.D, Secs div SecondsInDay);
      Secs := Secs mod SecondsInDay;

      if Secs > DT2.T then begin
        {subtract a day from DT2.D and add a day's worth of seconds to DT2.T}
        Dec(DT2.D);
        Inc(DT2.T, SecondsInDay);
      end;

      {now subtract the seconds}
      Dec(DT2.T, Secs);
    end
    else begin
      {increment the seconds}
      Inc(DT2.T, Secs);

      {adjust date if necessary}
      Inc(DT2.D, DT2.T div SecondsInDay);

      {force time to 0..SecondsInDay-1 range}
      DT2.T := DT2.T mod SecondsInDay;
    end;
  end;

  {!!.30 - New}
  procedure DosToDateTime(var DT : DateTime; var DTR : DateTimeRec);
    {-Convert a DOS DateTime to an OPDATE DateTimeRec }
  begin
    DTR.D := DMYToDate(DT.Day, DT.Month, DT.Year);
    DTR.T := HMSToTime(DT.Hour, DT.Min, DT.Sec);
  end;

  {!!.30 - New}
  procedure DateTimeToDos(var DTR : DateTimeRec; var DT : DateTime);
    {-Convert an OPDATE DateTimeRec to a DOS DateTime }
  var
    D, M, Y   : Integer;
    H, Min, S : Byte;

  begin
    FillChar(DT, SizeOf(DT), 0);

    DateToDMY(DTR.D, D, M, Y);
    DT.Day   := D;
    DT.Month := M;
    DT.Year  := Y;

    TimeToHMS(DTR.T, H, Min, S);
    DT.Hour := H;
    DT.Min  := Min;
    DT.Sec  := S;
  end;

  {!!.30 - New}
  procedure PackedToDateTime(PackedTime : LongInt; var DT : DateTimeRec);
    {-Convert a packed date/time to a DateTimeRec }
  var
    DosDate : DateTime;

  begin
    UnpackTime(PackedTime, DosDate);
    DosToDateTime(DosDate, DT);
  end;

  {!!.30 - New}
  procedure DateTimeToPacked(var DT : DateTimeRec; var PackedTime : LongInt);
    {-Convert a DateTimeRec to a packed date/time }
  var
    DosDate : DateTime;

  begin
    DateTimeToDos(DT, DosDate);
    PackTime(DosDate, PackedTime);
  end;

  function GetCountryInfo(var Dos2 : Boolean; var Info : CountryInfo ) : Boolean;
    {-Return a country information table in Info}
{$IFDEF VIRTUALPASCAL}
  var
    AMStr, PMStr, UnusedStr: Array[0..50] of Char;
  begin
    with Info do
      begin
        SysGetCurrencyFormat( PChar(@CurrencyStr), CurrencyForm, Unused4 ,Decimals, CommaSym2, DecimalSym2);

        SysGetTimeFormat(TimeSym, AMStr, PMStr, UnusedStr, UnusedStr);
        if (AMStr <> '') or (PMStr <> '') then
          TimeForm := 0
        else
          TimeForm := 1;
        SysGetDateFormat(DateSym, UnusedStr, UnusedStr);
      end;
    Dos2 := False;
    GetCountryInfo := True
  end;
{$ELSE}
  var
    Regs : Registers;
{$IFDEF Dpmi}
  type
    HiLo = record
             LoWord, HiWord : Word;
           end;
  var
    R : DpmiRegisters;
    L : LongInt;
{$ENDIF}
  begin
    with Regs do begin
      {get DOS version}
      AX := $3000;
      Intr($21, Regs);
      Dos2 := (AL = 2);

{$IFNDEF Dpmi}
      {get pointer to country information table}
      AX := $3800;
      DS := Seg(Info);
      DX := Ofs(Info);
      Intr($21, Regs);
      GetCountryInfo := not Odd(Flags);
{$ELSE}
      L := GlobalDosAlloc(SizeOf(CountryInfo));                     {!!.22}
      FillChar(R, SizeOf(R), 0);                                    {!!.22}
      R.ax := $3800;
      R.dx := 0;
      R.ds := HiLo(L).HiWord;
      GetCountryInfo := False;
      if SimulateRealModeInt($21, R) = 0 then                       {!!.22}
        if not Odd(R.Flags) then begin                              {!!.22}
          GetCountryInfo := True;
          Move(Ptr(HiLo(L).LoWord, 0)^, Info, SizeOf(CountryInfo)); {!!.22}
        end;
      L := GlobalDosFree(HiLo(L).LoWord);
{$ENDIF}
    end;
  end;
{$ENDIF}

  function InternationalDate(WholeYear, ZeroPad : Boolean) : DateString;
    {-Return a picture mask for a date string, based on DOS's country info}
  var
    Info : CountryInfo;
    Dos2 : Boolean;
  begin
    {assume failure}
    InternationalDate[0] := #0;

    {get country information table}
    if not GetCountryInfo(Dos2, Info) then
      Exit;

    {get date string format}
    with Info do begin
      {use US format if number is out of known bounds}
      if DateFormat > 2 then
        DateFormat := 0;
      case DateFormat of
        0 :                  {US}
          begin
            InternationalDate := 'mm/dd/yyyy';
            if not ZeroPad then begin
              InternationalDate[1] := 'M';
              InternationalDate[2] := 'M';
            end;
            if not WholeYear then
              InternationalDate[0] := #8;
          end;
        1 :                  {Europe}
          begin
            InternationalDate := 'dd/mm/yyyy';
            if not ZeroPad then begin
              InternationalDate[1] := 'D';
              InternationalDate[2] := 'D';
            end;
            if not WholeYear then
              InternationalDate[0] := #8;
          end;
        2 :                  {Japan}
          if WholeYear then
            InternationalDate := 'yyyy/mm/dd'
          else
            InternationalDate := 'yy/mm/dd';
      end;

      {set SlashChar}
      if not Dos2 then
        SlashChar := DateSym;
    end;
  end;

  function InternationalTime(WithSeconds, ZeroPad : Boolean;
                             ExtraSpace, WithEm : Boolean) : DateString;
    {-Return a picture mask for a time string, based on DOS's country info}
  var
    Info : CountryInfo;
    Dos2 : Boolean;
    TS : string[10];
    TSlen : Byte absolute TS;
  begin
    {assume failure}
    InternationalTime[0] := #0;

    {get pointer to country info}
    if not GetCountryInfo(Dos2, Info) then
      Exit;

    {format the default string}
    TS := 'hh:mm:ss';
    if not ZeroPad then begin
      TS[1] := 'H';
      TS[2] := 'H';
    end;
    if not WithSeconds then
      TS[0] := #5;

    {DOS 2.x doesn't provide any information about time strings}
    if not Dos2 then
      with Info do begin
        {set ColonChar}
        ColonChar := TimeSym;

        {if bit 0 not set, it's a 12-hour clock}
        if (TimeForm and $01) = 0 then begin
          if ExtraSpace then begin
            Inc(TSlen);
            TS[TSlen] := ' ';
          end;
          Inc(TSlen);
          TS[TSlen] := 't';
          if WithEm then begin
            Inc(TSlen);
            TS[TSlen] := 'e';
          end;
        end;
      end;

    InternationalTime := TS;
  end;

  procedure SetDefaultYear;
    {-Initialize DefaultYear and DefaultMonth}
  var
    Month, Day, DayOfWeek : Word;
  begin
    GetDate(Word(DefaultYear), Month, Day, DayOfWeek);
    DefaultMonth := Month;
  end;

begin
  {initialize DefaultYear and DefaultMonth}
  SetDefaultYear;
end.
