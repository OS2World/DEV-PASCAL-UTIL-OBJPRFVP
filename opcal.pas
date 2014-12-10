{$IFDEF Windows}
  !! ERROR - This unit is not compatible with Windows !!
{$ENDIF}

{$R-,S-,I-,V-,B-,F+,O+,A-} {!!.01}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPCAL.PAS 1.30                      *}
{*                   Popup Calendar                      *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpCal;
  {-Popup calendar derived from pick list}

interface

uses
  Use32,
  opinline,
  opstring,
  opconst,  {!!.20}
  oproot,
  opcrt,
  {$IFDEF UseMouse}
  opmouse,
  {$ENDIF}
  opcmd,
  opframe,
  opwindow,
  {$IFDEF UseDrag} {!!.03}
  opdrag,          {!!.03}
  {$ENDIF}         {!!.03}
  oppick,
  opdate;

  {$I OPCAL.ICD}  {configuration data}

const
  CalendarWidth  = 29;     {width of calendar}
  CalendarHeight = 11;     {height of calendar}
  DefCrossBar    : Char = 'Ä';
  DefLeftTee     : Char = 'Ã';
  DefRightTee    : Char = '´';
  DaysOfTheWeek  : string[CalendarWidth] = ' Sun Mon Tue Wed Thu Fri Sat ';
type
  CalArray = array[1..42] of Byte;

  CalendarPtr = ^Calendar;
  Calendar =
    object(PickList)
      clYear     : Integer;  {current year}
      clMonth    : Integer;  {current month}
      clFirst    : Byte;     {index for first day in current month}
      clLast     : Byte;     {index for last day in current month}
      clCalendar : CalArray; {the current month}
      {...methods...}
      constructor Init(X1, Y1 : Byte);
        {-Initialize the calendar using default colors and options}
      constructor InitCustom(X1, Y1 : Byte; LeftTee, CrossBar, RightTee : Char;
                             var Colors : ColorSet; Options : LongInt);
        {-Initialize the calendar with custom options, colors}
      procedure ProcessSelf; virtual; {!!.01}
        {-Display calendar until a selection/exit command is made}
      {...}
      procedure SetCurrentDate(NewDate : Date);
        {-Change the currently selected date}
      procedure SetCurrentDMY(Day, Month, Year : Integer);
        {-Change the currently selected date}
      function GetCurrentDate : Date;
        {-Return the currently selected date}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a calendar from a stream}
      procedure Store(var S : IdStream);
        {-Store a calendar in a stream}
    {$ENDIF}
{.Z+}
      {+++ internal methods +++}
      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
      procedure UpdateContents; virtual;
      procedure clIncCurrentDate(Months, Years : Integer);
{.Z-}
    end;

{.Z+}
  {$IFDEF UseStreams}
  procedure CalendarStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing calendars}
  {$ENDIF}
{.Z-}

var
  {$IFDEF UseDrag}                 {!!.03}
  CalCommands : DragProcessor;     {!!.03}
  {$ELSE}                          {!!.03}
  CalCommands : CommandProcessor;
  {$ENDIF}                         {!!.03}

  {===========================================================}

implementation

  constructor Calendar.Init(X1, Y1 : Byte);
    {-Initialize the calendar using default colors and options}
  begin
    if not Calendar.InitCustom(X1, Y1, DefLeftTee, DefCrossBar, DefRightTee,
                               DefaultColorSet, DefWindowOptions) then
      Fail;
  end;

  constructor Calendar.InitCustom(X1, Y1 : Byte;
                                  LeftTee, CrossBar, RightTee : Char;
                                  var Colors : ColorSet;
                                  Options : LongInt);
    {-Initialize the calendar with custom options, colors}
  var
    X2, Y2 : Byte;
  begin
    {won't work if screen is too small}
    if (ScreenWidth < CalendarWidth+2) or (ScreenHeight < CalendarHeight) then begin
      InitStatus := epFatal+ecBadCoordinates;
      Fail;
    end;

    {calculate X2 and Y2}
    X2 := X1+Pred(CalendarWidth);
    Y2 := Y1+Pred(CalendarHeight);

    {make sure there's a frame}
    SetLongFlag(Options, wBordered);

    {initialize the pick list}
    if not PickList.InitAbstract(X1, Y1+3, X2, Y2, Colors, Options, 4, 42,
                                 PickHorizontal, SingleChoice) then
      Fail;

    {install alternate command processor}
    SetCommandProcessor(CalCommands);

    {change unit code}
    cwUnitCode := ucCal;

    {make sure highlighted choice stays highlighted}
    pkOptionsOn(pkDrawActive);

    {make sure that the window isn't resized}
    ClearLongFlag(wFlags, wResizeable);

    {adjust frame coordinates}
    with wFrame do
      AdjustFrameCoords(frXL, frYL-3, frXH, frYH);

    {add window divider}
    if RawError = 0 then                                      {!!.01}
      wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, 3, frTT);

    if RawError <> 0 then begin                               {!!.01}
      InitStatus := RawError;                                 {!!.01}
      Done;
      Fail;
    end;

    {initialize the calendar}
    SetCurrentDate(Today);
  end;

  procedure Calendar.ItemString(Item : Word;
                                Mode : pkMode;
                                var IType : pkItemType;
                                var IString : String);
    {-Return a string representing the specified item}
  var
    ILen : Byte absolute IString;
  begin
    if (Item < clFirst) or (Item > clLast) then
      IType := pkProtected
    else
      IType := pkNormal;

    IString := ' '+Long2Str(clCalendar[Item]);
    if ILen = 2 then
      IString[3] := ' ';
    IString[4] := ' ';
    ILen := 4;
  end;

  procedure Calendar.UpdateContents;
    {-Update the contents of the currently displayed window}
  var
    S : string[20];
    A : Byte;
    {$IFDEF UseMouse}          {!!.03}
    SaveMouse : Boolean;       {!!.03}
    {$ENDIF}                   {!!.03}
  begin
    {draw the top of the calendar window}
    A := ColorMono(wTextColor, wTextMono);
    S := MonthString[clMonth] + ' ' + Long2Str(clYear);

    {$IFDEF UseMouse}          {!!.03}
    HideMousePrim(SaveMouse);  {!!.03}
    {$ENDIF}                   {!!.03}

    FastWrite(Center(S, CalendarWidth), wYL-3, wXL, A);
    FastWrite(DaysOfTheWeek, wYL-2, wXL, A);

    {$IFDEF UseMouse}          {!!.03}
    ShowMousePrim(SaveMouse);  {!!.03}
    {$ENDIF}                   {!!.03}

    {draw the pick list}
    PickList.UpdateContents;
  end;

  procedure Calendar.SetCurrentDate(NewDate : Date);
    {-Change the currently selected date}
  var
    Day1 : Date;
    I, J : Word;
    Day : Integer;
  begin
    {verify the date}
    if NewDate = BadDate then begin
      GotError(ecBadParam, emNullError);
      Exit;
    end;

    {convert the date to a day, month, and year}
    DateToDMY(NewDate, Day, clMonth, clYear);

    {get the first day of the current month and year}
    Day1 := DMYtoDate(1, clMonth, clYear);

    {find its index}
    clFirst := Byte(DayOfWeek(Day1))+1;

    {find the index of the last day in the month}
    clLast := clFirst+DaysInMonth(clMonth, clYear)-1;

    {initialize the first part of the calendar}
    if clMonth = 1 then
      J := DaysInMonth(12, clYear-1)
    else
      J := DaysInMonth(clMonth-1, clYear);
    for I := clFirst-1 downto 1 do begin
      clCalendar[I] := J;
      Dec(J);
    end;

    {initialize the rest of the calendar}
    J := 1;
    for I := clFirst to 42 do begin
      clCalendar[I] := J;
      if I = clLast then
        J := 1
      else
        Inc(J);
    end;

    {initialize the pick list's selection pointer}
    SetChoice(clFirst+Pred(Day), 1);
  end;

  procedure Calendar.SetCurrentDMY(Day, Month, Year : Integer);
    {-Change the currently selected date}
  begin
    SetCurrentDate(DMYtoDate(Day, Month, Year));
  end;

  function Calendar.GetCurrentDate : Date;
    {-Return the currently selected date}
  var
    Day : Integer;
  begin
    Day := GetLastChoice-Pred(clFirst);
    GetCurrentDate := DMYtoDate(Day, clMonth, clYear);
  end;

  procedure Calendar.clIncCurrentDate(Months, Years : Integer);
    {-Increment the current date by the specified number of months, years}
  var
    NewDate : Date;
  begin
    NewDate := IncDateTrunc(GetCurrentDate, Months, Years);
    if NewDate <> BadDate then
      SetCurrentDate(NewDate);
  end;

  procedure Calendar.ProcessSelf; {!!.01}
    {-Display calendar until a selection/exit command is made}
  var
    AllDone : Boolean;
    {$IFDEF UseMouse}
    {SaveMouse : Boolean;}    {!!.01}
    {$ENDIF}
  begin
    (*
    {$IFDEF UseMouse}
    SaveMouse := MouseCursorOn;
    if cwCmdPtr^.MouseEnabled then
      ShowMouse;
    {$ENDIF}
    *)

    AllDone := False;
    repeat
      {make a selection}
      PickList.ProcessSelf;  {!!.01}

      {process special exit commands}
      case GetLastCommand of
        ccIncMonth :
          clIncCurrentDate(+1, 0);
        ccDecMonth :
          clIncCurrentDate(-1, 0);
        ccIncYear  :
          clIncCurrentDate(0, +1);
        ccDecYear  :
          clIncCurrentDate(0, -1);
        ccToday :
          SetCurrentDate(Today);
        else
          AllDone := True;
      end;
    until AllDone;

    {$IFDEF UseMouse}
    {ShowMousePrim(SaveMouse);}  {!!.01}
    {$ENDIF}
  end;

{$IFDEF UseStreams}

  constructor Calendar.Load(var S : IdStream);
    {-Load a calendar from a stream}
  var
    D : Date;
  begin
    {load the pick list}
    if not PickList.Load(S) then
      Fail;

    {read the date}
    S.Read(D, SizeOf(D));
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {initialize for the specified date}
    SetCurrentDate(D);
  end;

  procedure Calendar.Store(var S : IdStream);
    {-Store a calendar in a stream}
  var
    D : Date;
  begin
    {store the pick list}
    PickList.Store(S);
    if S.PeekStatus <> 0 then
      Exit;

    {write the current date}
    D := GetCurrentDate;
    S.Write(D, SizeOf(Date));
  end;

  procedure CalendarStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing calendars}
  begin
    {register the pick list}
    PickListStream(SPtr);

    with SPtr^ do begin
      RegisterType(otCalendar, veCalendar,
                   TypeOf(Calendar),
                   @Calendar.Store, @Calendar.Load);
      RegisterPointer(ptPickHorizontal, @PickHorizontal);
      RegisterPointer(ptCalCommands, @CalCommands);
      RegisterPointer(ptPickSingleChoice, @SingleChoice); {!!.11}
    end;
  end;

{$ENDIF}

begin
  {Initialize command processor}
  CalCommands.Init(@CalKeySet, CalKeyMax);
end.
