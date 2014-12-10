{$IFDEF Windows}
  !! ERROR - This program is not compatible with Windows !!
{$ENDIF}

{$S-,I-}
{$V-}                        {<- required for OPENTRY}
{$M 16384,16384,600000}

{*********************************************************}
{*                   ENTRY.PAS 1.30                      *}
{*     An example program for Object Professional 1.0    *}
{*     Copyright (c) TurboPower Software 1988, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program requires that OPDEFINE.INC activate the following defines:
   UseDates, PickListFields
 This program will use features activated with the following defines:
   UseMouse, UseScrollBars, UseHotSpots
 ***************************************************************************}

{$IFNDEF UseDates}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF PickListFields}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

program OpEntryDemo;
  {-Demonstrates use of OPENTRY unit}

uses
  Use32,
  {$IFDEF VIRTUALPASCAL}
  VpSysLow,
  {$ENDIF}
  OpString,
  OpConst,            {!!.20}
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  {$IFDEF UseDrag}    {!!.11}
  OpDrag,             {!!.11}
  {$ENDIF}            {!!.11}
  OpCmd,
  OpDate,
  OpFrame,
  OpWindow,
  OpPick,
  OpHelp,
  OpMemo,
  OpAbsFld,
  OpField,
  OpSelect,
  OpEntry;

{$IFDEF VIRTUALPASCAL}
{$IFDEF USEBCD}
type
  Real = Extended;
{$ENDIF}
{$ENDIF}

const
  SliderChar       = '²';
  ScrollBarChar    = '°';
  OurColorSet : ColorSet = (
    TextColor       : $1B; TextMono       : $07;
    CtrlColor       : $5F; CtrlMono       : $70;
    FrameColor      : $1D; FrameMono      : $0F;
    HeaderColor     : $5F; HeaderMono     : $70;
    ShadowColor     : $07; ShadowMono     : $07;
    HighlightColor  : $1B; HighlightMono  : $07;
    PromptColor     : $0B; PromptMono     : $0F;
    SelPromptColor  : $0B; SelPromptMono  : $0F;
    ProPromptColor  : $0B; ProPromptMono  : $0F;
    FieldColor      : $1F; FieldMono      : $70;
    SelFieldColor   : $5F; SelFieldMono   : $70;
    ProFieldColor   : $0F; ProFieldMono   : $07;
    ScrollBarColor  : $07; ScrollBarMono  : $07;
    SliderColor     : $07; SliderMono     : $07;
    HotSpotColor    : $5F; HotSpotMono    : $07;
    BlockColor      : $0F; BlockMono      : $0F;
    MarkerColor     : $0F; MarkerMono     : $70;
    DelimColor      : $0F; DelimMono      : $0F;
    SelDelimColor   : $70; SelDelimMono   : $70;
    ProDelimColor   : $07; ProDelimMono   : $07;
    SelItemColor    : $5B; SelItemMono    : $70;
    ProItemColor    : $1B; ProItemMono    : $07;
    HighItemColor   : $1F; HighItemMono   : $0F;
    AltItemColor    : $1F; AltItemMono    : $0F;
    AltSelItemColor : $5F; AltSelItemMono : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono  : $0F;
    FlexBHelpColor  : $1F; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono  : $70;
    UnselXrefColor  : $1E; UnselXrefMono  : $09;
    SelXrefColor    : $5F; SelXrefMono    : $70;
    MouseColor      : $4F; MouseMono      : $70
  );

  MainTextColor : Byte = $0B;
  MainTextMono  : Byte = $07;
  TitleLine   = 02;
  StatusLine  = 04;
  HelpLine    = 22;
  KeyInfoLine = 24;
  Title       : string[38] = 'Demonstration Program for OPENTRY 1.30';
  KeyInfoText : string[78] =
  ' <F1> Help '^G' '^[^X^Y^Z' move cursor '^G' <Enter> Accept '^G' <Esc> Cancel '^G' <^Enter> Quit ';
type
  GenderType  = (Unknown, Male, Female);
  MemoField   = array[1..2048] of Char;
  Info =
    record
      Name    : string[30];  {string field}
      Address : string[30];  {string field}
      City    : string[25];  {string field}
      State   : string[02];  {string field w/ special validation}
      Zip     : string[10];  {string field w/ special validation}
      WPhone  : string[14];  {string field w/ special validation}
      HPhone  : string[14];  {string field w/ special validation}
      Gender  : GenderType;  {multiple choice field}
      Married : Boolean;     {yes/no field}
      Born    : Date;        {date field}
      Age     : Byte;        {calculated field, based on Born}
      Wage    : Real;        {numeric field w/ range checking}
      Weekly  : Real;        {calculated field (= Wage * Hours)}
      Hours   : Byte;        {multiple choice field, incremental}
      Yearly  : Real;        {calculated field (= Weekly * 52)}
      Notes   : MemoField;   {a memo field}
    end;

  {this custom help window allows us to highlight specific items in the
   help index}
  CustomHelpWindow =
    object(PagedHelpWindow)
      procedure ItemString(Item : Word; Mode : pkMode; var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
    end;
const
  {ID numbers - main entry screen}
  idRecNum  = 00;
  idName    = 01;
  idAddress = 02;
  idCity    = 03;
  idState   = 04;
  idZip     = 05;
  idPhones  = 06;
  idGender  = 07;
  idMarried = 08;
  idBorn    = 09;
  idAge     = 10;
  idWage    = 11;
  idWeekly  = 12;
  idHours   = 13;
  idYearly  = 14;
  idNotes   = 15;
  {ID numbers - second entry screen}
  idWPhone  = 00;
  idHPhone  = 01;

  MaxRec      = 10;
  PhoneMask   : string[14] = '(999) 999-9999';
  ValidPhone  : string[14] = '(ppp) uuu-uuuu';
  ZipMask     : string[10] = '99999-9999';
  ValidZip    : string[10] = 'uuuuu-pppp';
  Genders     : array[GenderType] of string[7] = ('Unknown', 'Male   ', 'Female ');
  NotesMsg    : string[1] = #14;
var
  InfoRecs    : array[1..MaxRec] of Info; {the "database"}
  Scrap       : Info;        {blank record used for editing}
  CurrentRec  : Byte;        {current index into InfoRecs}
  ES1         : EntryScreen; {our main entry screen}
  ES2         : EntryScreen; {our nested entry screen}
  MW          : Memo;        {memo window}
  HW          : CustomHelpWindow; {help window}
  StateList   : PickList;    {pick list}
  AllDone     : Boolean;     {done with demo program}
  DateMask    : string[10];  {picture mask for date strings}
  TimeMask    : string[11];  {picture mask for time strings}
  WageMask    : string[10];  {picture mask for wage field}
  CurrMask    : string[15];  {picture mask for totals based on wages}

const
  StateStrings : array[1..51] of string[19] = (
    {01} 'AK Alaska',         {02} 'AL Alabama',          {03} 'AR Arkansas',
    {04} 'AZ Arizona',        {05} 'CA California',       {06} 'CO Colorado',
    {07} 'CT Connecticut',    {08} 'DC Dist of Columbia', {09} 'DE Delaware',
    {10} 'FL Florida',        {11} 'GA Georgia',          {12} 'HI Hawaii',
    {13} 'IA Iowa',           {14} 'ID Idaho',            {15} 'IL Illinois',
    {16} 'IN Indiana',        {17} 'KS Kansas',           {18} 'KY Kentucky',
    {19} 'LA Louisana',       {20} 'MA Massachusetts',    {21} 'MD Maryland',
    {22} 'ME Maine',          {23} 'MI Michigan',         {24} 'MN Minnesota',
    {25} 'MO Missouri',       {26} 'MS Mississippi',      {27} 'MT Montana',
    {28} 'NC North Carolina', {29} 'ND North Dakota',     {30} 'NE Nebraska',
    {31} 'NH New Hampshire',  {32} 'NJ New Jersey',       {33} 'NM New Mexico',
    {34} 'NV Nevada',         {35} 'NY New York',         {36} 'OH Ohio',
    {37} 'OK Oklahoma',       {38} 'OR Oregon',           {39} 'PA Pennsylvania',
    {40} 'RI Rhode Island',   {41} 'SC South Carolina',   {42} 'SD South Dakota',
    {43} 'TN Tennessee',      {44} 'TX Texas',            {45} 'UT Utah',
    {46} 'VA Virginia',       {47} 'VT Vermont',          {48} 'WA Washington',
    {49} 'WI Wisconsin',      {50} 'WV West Virginia',    {51} 'WY Wyoming');

  procedure CustomHelpWindow.ItemString(Item : Word;
                                        Mode : pkMode;
                                        var IType : pkItemType;
                                        var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  begin
    {call parent's ItemString routine}
    PagedHelpWindow.ItemString(Item, Mode, IType, IString);

    {highlight general topics}
    if Item <= 7 then
      IType := pkAlternate;
  end;

  {$F+}
  function ValidatePhone(EFP : EntryFieldPtr;
                         var ErrCode : Word;
                         var ErrorSt : StringPtr) : Boolean;
    {-Validate a phone number}
  begin
    ValidatePhone := ValidateSubfields(ValidPhone, EFP, ErrCode, ErrorSt);
  end;

  function ValidateZip(EFP : EntryFieldPtr;
                       var ErrCode : Word;
                       var ErrorSt : StringPtr) : Boolean;
    {-Validate a zip code}
  begin
    ValidateZip := ValidateSubfields(ValidZip, EFP, ErrCode, ErrorSt);
  end;

  {Note: The following validation routine is not used in ENTRY. It is
   provided as an example of a validation routine that must compare data
   entered by the user against a list of valid choices.}

  function ValidateState(EFP : EntryFieldPtr;
                         var ErrCode : Word;
                         var ErrorSt : StringPtr) : Boolean;
    {-Validate a state abbreviation}
  const
    BadState : String[37] = 'Not a valid abbreviation for a state.';
  var
    I : Word;
    S : String[2];
  begin
    {don't validate if user clicked on the field}
    if (ES1.GetLastCommand = ccClickExit) then
      ValidateState := True
    {check for partial entry}
    else if not ValidateNotPartial(EFP, ErrCode, ErrorSt) then
      ValidateState := False
    else begin
      {not partial--is it empty?}
      ValidateState := True;
      S := Trim(EFP^.efEditSt^);

      {empty string is OK here}
      if Length(S) = 0 then
        Exit;

      {not empty--check list of valid abbreviations}
      for I := 1 to 51 do
        {exit if it's a match}
        if (S[1] = StateStrings[I][1]) and (S[2] = StateStrings[I][2]) then
          Exit;

      {not a valid abbreviation}
      ErrCode := 5000; {arbitrary}
      ErrorSt := @BadState;
      ValidateState := False;
    end;
  end;

  procedure StateChoice(Item : Word; Mode : pkMode;
                       var IType : pkItemType;
                       var IString : string;
                       PickPtr : PickListPtr);
    {-Return a state string given an index}
  begin
    if Mode = pkSearch then begin
      {return only the first two characters -- the abbreviation}
      IString := StateStrings[Item];
      IString[0] := #2;
    end
    else begin
      IString := ' '+StateStrings[Item]+' ';
      if Item = PickPtr^.GetDefaultChoice then
        IType := pkAlternate;
    end;
  end;

  {$F-}

  procedure DisplayCentered(S : string; Row : Byte);
    {-Display S centered on the specified Row}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
    {$ENDIF}

    with OurColorSet do
      FastWrite(Center(S, 78), Row, 2, ColorMono(HighlightColor, HighlightMono));

    {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure ClearHelpLine;
    {-Clear the help line}
  begin
    DisplayCentered('', HelpLine);
  end;


  {$F+}
  {$IFDEF UseDrag}                            {!!.11 begin}
  procedure UpdateClock;
  var
    A : Byte;
  begin
    {display the current date and time}
    with OurColorSet do
      A := ColorMono(FieldColor, FieldMono);
    FastWrite(TodayString(DateMask), StatusLine, 38, A);
    FastWrite(CurrentTimeString(TimeMask), StatusLine, 57, A);
  end;

  function DragKeyPressed : Boolean;
  begin
    {make sure TSR's can pop up}
{$IFDEF VIRTUALPASCAL}
    DosSleep(1);
{$ELSE}
    inline($CD/$28);
{$ENDIF}
    UpdateClock;
    DragKeyPressed := OpCrt.KeyPressed;
  end;

  function GetKey : Word;
    {-Display current date and time while waiting for keypress}
  begin
    while not EntryCommands.cpKeyPressed do begin
      {make sure TSR's can pop up}
{$IFDEF VIRTUALPASCAL}
    DosSleep(1);
{$ELSE}
      inline($CD/$28);
{$ENDIF}
      UpdateClock;
    end;
    GetKey := EntryCommands.cpGetKey;
  end;

  {$ELSE}                                  {!!.11 end}
  function GetKey : Word;
    {-Display current date and time while waiting for keypress}
  var
    A : Byte;
  begin
    {$IFDEF UseMouse}
    while not KeyOrButtonPressed do begin
    {$ELSE}
    while not KeyPressed do begin
    {$ENDIF}
      {make sure TSR's can pop up}
{$IFDEF VIRTUALPASCAL}
      SysCtrlSleep(1);
{$ELSE}
      inline($CD/$28);
{$ENDIF}

      {display the current date and time}
      with OurColorSet do
        A := ColorMono(FieldColor, FieldMono);
      FastWrite(TodayString(DateMask), StatusLine, 38, A);
      FastWrite(CurrentTimeString(TimeMask), StatusLine, 57, A);
    end;

    {$IFDEF UseMouse}
      GetKey := ReadKeyOrButton;
    {$ELSE}
      GetKey := ReadKeyWord;
    {$ENDIF}
  end;
  {$ENDIF}                                    {!!.11}

  procedure UpdateWeeklyAndYearly;
    {-Update the Weekly and Yearly fields}
  begin
    with ES1, Scrap do begin
      {calculate weekly and yearly earnings}
      if Wage = BadReal then
        Weekly := 0
      else
        Weekly := Wage*Hours;
      Yearly := Weekly*52;

      {redraw Weekly}
      DrawField(idWeekly);

      {redraw Yearly}
      DrawField(idYearly);
    end;
  end;

  procedure IncChoice(var Value; ID : Word; Factor : Integer; var St : string);
    {-Increment a multiple choice field value and convert it to a string}
  var
    Gender : GenderType absolute Value;
    Hours : Byte absolute Value;
  begin
    if ID = idGender then begin
      case Factor of
        01 :                 {increment}
          if Gender = Female then
            Gender := Unknown
          else
            Inc(Gender);
        -1 :                 {decrement}
          if Gender = Unknown then
            Gender := Female
          else
            Dec(Gender);
      end;
      St := Genders[Gender];
    end
    else { ID = idHours } begin
      case Factor of
        01 :                 {increment}
          if Hours < 99 then
            Inc(Hours);
        -1 :                 {decrement}
          if Hours > 0 then
            Dec(Hours);
      end;
      Str(Hours:2, St);
      UpdateWeeklyAndYearly;
    end;
  end;

  procedure DisplayErrorMessage(Msg : string);
    {-Display an error message}
  var
    W : Word;
    CursorSL, CursorXY : Word;
    P : Pointer;
  begin
    {try to save screen}
    if not SaveWindow(2, HelpLine, 79, HelpLine, True, P) then begin
      RingBell;
      Exit;
    end;

    {Store cursor position and shape, then make it a fat cursor}
    GetCursorState(CursorXY, CursorSL);
    FatCursor;

    {add to default message, if possible}
    if Length(Msg) < 60 then
      Msg := Msg+'. Press any key...';

    {display error message and ring bell}
    DisplayCentered(Msg, HelpLine);
    RingBell;

    {flush keyboard buffer}
    while KeyPressed do
      W := GetKey;

    {wait for keypress}
    W := GetKey;

    {Restore cursor position and shape}
    RestoreCursorState(CursorXY, CursorSL);

    {restore screen}
    RestoreWindow(2, HelpLine, 79, HelpLine, True, P);
  end;

  procedure ErrorHandler(UnitCode : Byte; var ErrCode : Word; Msg : string);
    {-Display messages for errors reported by OPENTRY}
  begin
    {display the error message}
    DisplayErrorMessage(Msg);
  end;

  procedure HideAgeField;
    {-Hide the Age field if it is 0}
  begin
    with ES1 do
      if Scrap.Age = 0 then
        {hide the entire field}
        ChangeHidden(10, On)
      else
        {unhide the field}
        ChangeHidden(10, Off);
  end;

  procedure PostEdit(ESP : EntryScreenPtr);
    {-Called after a field has been edited}
  var
    Days, Months, Years : Integer;
    ThisDate : Date;         {today's date in julian format}
  begin
    ThisDate := Today;
    with Scrap, ESP^ do
      case GetCurrentID of
        idBorn :             {Born}
          begin
            {calculate Age field}
            if (Born = BadDate) or (Born > ThisDate) then
              Age := 0
            else begin
              DateDiff(Born, ThisDate, Days, Months, Years);
              Age := Years;
            end;

            {hide/unhide and redraw the Age field}
            HideAgeField;
            DrawField(idAge);
          end;
        idWage :             {Wage}
          {update related fields}
          UpdateWeeklyAndYearly;

        idHours :            {Hours}
          {do nothing: IncChoice handles call to UpdateWeeklyAndYearly} ;
      end;
  end;

  procedure DisplayHelpPrompt(ESP : EntryScreenPtr);
    {-Display a help prompt for the current field}
  var
    S : string[80];
  begin
    case ESP^.GetCurrentID of
      {--Field 0 is the record number (protected)--}
      idName    : S := 'Enter first name, middle initial, last name';
      idAddress : S := 'Enter street address or post office box';
      idCity    : S := 'Enter city of residence';
      idState   : S := 'Move highlight and press <Enter> to select state of residence'; {!!.11}
      idZip     : S := 'Enter a five- or nine-digit zip code';
      idPhones  : S := 'Press <Enter> to edit work and home phone numbers';
      idGender  : S := 'Press space bar, "+" or "-" to select gender';
      idMarried : S := 'Enter "N" if marital status is unknown, else "N" or "Y"';
      idBorn    : S := 'Enter date of birth';
      {--Field 10 is Age (protected, calculated)--}
      idWage    : S := 'Enter hourly wage ($0-$99.99)';
      {--Field 12 is Weekly (protected, calculated)--}
      idHours   : S := 'Press "+" or "-" to adjust hours worked per week';
      {--Field 14 is Yearly (protected, calculated)--}
      idNotes   : S := 'Press <Enter> to edit notes field';
    end;
    DisplayCentered(S, HelpLine);
  end;

  procedure DisplayHelpPrompt2(ESP : EntryScreenPtr);
    {-Display a help prompt for the current field}
  var
    S : string[80];
  begin
    case ESP^.GetCurrentID of
      idWPhone : S := 'Enter work phone number (area code is optional)';
      idHPhone : S := 'Enter home phone number (area code is optional)';
    end;
    DisplayCentered(S, HelpLine);
  end;

  procedure DisplayHelp(UnitCode : Byte; IdPtr : Pointer; HelpIndex : Word);
    {-Display context sensitive help}
  begin
    {do nothing if help index is illegal}
    if HelpIndex <> 0 then
      with HW do begin
        {display the help screen}
        SetTopic(HelpIndex);
        Process;
        Erase;
      end;
  end;

  procedure MemoFieldStatus(MP : MemoPtr);
    {-Display status line for memo field}
                              {         1         2         }
  const                       {12345678901234567890123456789}
    StatusLine : string[29] = ' Line: xxx Column: xxx 100% ';
  var
    S : string[5];
    SaveMouse : Boolean;
  begin
    with MP^ do begin
      {insert line number}
      S := Long2Str(meCurLine);
      S := Pad(S, 3);
      Move(S[1], StatusLine[8], 3);

      {insert column number}
      S := Long2Str(meCurCol);
      S := Pad(S, 3);
      Move(S[1], StatusLine[20], 3);

      {insert percentage of buffer used}
      S := Real2Str(Trunc((meTotalBytes*100.0)/(meBufSize-2)), 3, 0);
      Move(S[1], StatusLine[24], 3);

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      {display status line}
      with OurColorSet do
        FastWrite(StatusLine, 19, 27, ColorMono(HighlightColor, HighlightMono));

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;
  end;
  {$F-}

  procedure EditMemoField;
    {-Edit a memo field}
  begin
    with MW do begin
      {reinitialize}
      ReinitBuffer;

      {display instructions}
      DisplayCentered('Press <Esc> when finished entering notes', HelpLine);

      {edit}
      Process;

      {erase the memo window}
      Erase;

      {clear the help line}
      ClearHelpLine;
    end;
  end;

  function ConfirmQuitting : Boolean;
    {-Confirm that the user wants to quit}
  var
    ChWord : Word;
    Ch : Char absolute ChWord;
  begin
    {$IFDEF UseMouse}
      while KeyOrButtonPressed do
        ChWord := ReadKeyOrButton;
    {$ELSE}
      while KeyPressed do
        ChWord := ReadKeyWord;
    {$ENDIF}

    HiddenCursor;
    DisplayCentered(
      'Are you sure you want to quit? (Press "Y" or <Esc> to confirm.)', HelpLine);
    ChWord := GetKey;

    {$IFDEF UseMouse}
      ConfirmQuitting := (Upcase(Ch) = 'Y') or (Ch = #27) or (ChWord = MouseRt);
    {$ELSE}
      ConfirmQuitting := (Upcase(Ch) = 'Y') or (Ch = #27);
    {$ENDIF}

    ClearHelpLine;
    NormalCursor;
  end;

  procedure DrawMainScreen;
    {-Draw the outline of the screen. Fields filled in later}
  var
    FA, TA : Byte;

    procedure DrawBox(Row : Byte);
      {-Draw a divided box starting at the specified Row}
    var
      I : Word;
    begin
      {draw the main box}
      for I := Row to Row+4 do
        FastFill(80, ' ', I, 1, FA);
      FrameWindow(1, Row, 80, Row+4, FA, FA, '');
      FastWrite('Ã'+CharStr('Ä', 78)+'´', Row+2, 1, FA);
    end;

  begin
    ClrScr;

    FA := ColorMono(OurColorSet.FrameColor, OurColorSet.FrameMono);
    TA := ColorMono(OurColorSet.HighlightColor, OurColorSet.HighlightMono);

    {draw the box at the top of the screen}
    DrawBox(TitleLine-1);
    DisplayCentered(Title, TitleLine);
    FastWrite('Date', StatusLine, 32, TA);
    FastWrite('Time', StatusLine, 51, TA);

    {draw the box at the bottom of the screen}
    DrawBox(HelpLine-1);
    DisplayCentered(KeyInfoText, KeyInfoLine);
  end;

  procedure OpenHelp;
    {-Open ENTRY.HLP}
  var
    Status : Word;
  begin
    {open the help file}
    if not HW.InitCustom(9, 8, 72, 18, {2, 2, 69, 13,}
                         OurColorSet, DefWindowOptions or wBordered,
                        'ENTRY.HLP', PickVertical) then begin
      Status := InitStatus mod 10000;
      case Status of
        ecFileNotFound :
          WriteLn('Help file ENTRY.HLP not found');
        ecHelpInvalid :
          WriteLn('Help file has invalid format');
        ecOutOfMemory :
          WriteLn(emInsufficientMemory);
        else
          WriteLn('Help initialization error ', Status);
      end;
      Halt(1);
    end;

    with HW do begin
      AddTopicHeader(1, 60, heTC);
      AddMoreHelpHeader(' PgUp/PgDn for more ', heBR,
                        'PgUp', 'PgDn', '/', 2, 7, 6);
      wFrame.AddHeader(' Help Topic Index ', heTC);
      AddMoreHeader(' || for more ', heBR, #24, #25, '', 2, 3, 0);

      {$IFDEF UseHotSpots}
      hwFrame.AddHotSpot(frTL, #4, OurColorSet);
      SetPrevTopicHotSpot(hsSpot, frTL);
      {$ENDIF}
      {$IFDEF UseScrollBars}
        {add vertical scroll bars}
        wFrame.AddCustomScrollBar(
          frRR, 0, MaxLongInt, 1, 1, SliderChar, ScrollBarChar, OurColorSet);
        hwFrame.AddCustomScrollBar(
          frRR, 0, MaxLongInt, 1, 1, SliderChar, ScrollBarChar, OurColorSet);
      {$ENDIF}
    end;
  end;

  function SecondaryEditScreen : Boolean;
    {-Display secondary entry screen. Returns True to advance cursor for main
    entry screen forward, False for backward.}
  var
    AllDone : Boolean;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    with ES2 do begin
      AllDone := False;
      repeat
        {start editing}
        Process;

        {copy the edited data back if ESC wasn't pressed}
        if GetLastCommand <> ccQuit then begin
          InfoRecs[CurrentRec].WPhone := Scrap.WPhone;
          InfoRecs[CurrentRec].HPhone := Scrap.HPhone;
        end;

        {see if we need to edit another record}
        case GetLastCommand of
          ccUser0 :            {toggle Bell on/off}
            if esFieldOptionsAreOn(efBeepOnError) then begin
              ES1.SetBeepOnError(Off);
              ES2.SetBeepOnError(Off);
            end
            else begin
              ES1.SetBeepOnError(On);
              ES2.SetBeepOnError(On);
            end;
          ccError,
          ccExitAtTop,
          ccExitAtBot,
          ccNextRec,
          ccPrevRec,
          ccQuit,
          ccDone :
            begin
              AllDone := True;
              SecondaryEditScreen := (GetLastCommand <> ccPrevRec) and
                                     (GetLastCommand <> ccExitAtTop);
            end;
        end;
      until AllDone;

      {$IFDEF UseMouse}
        {hide the mouse cursor}
        HideMousePrim(SaveMouse);
      {$ENDIF}

      {erase the entry screen}
      Erase;

      {$IFDEF UseMouse}
        {reveal the mouse cursor}
        ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;
  end;

  procedure Initialize;
    {-Miscellaneous initialization code}
  begin
    {initialize the database}
    FillChar(Scrap, SizeOf(Scrap), 0);
    FillChar(InfoRecs, SizeOf(InfoRecs), 0);
    for CurrentRec := 1 to MaxRec do begin
      InfoRecs[CurrentRec].Born := BadDate;
      InfoRecs[CurrentRec].Wage := BadReal;
      InfoRecs[CurrentRec].Hours := 40;
      InfoRecs[CurrentRec].Notes[1] := ^Z;
    end;

    {get international picture mask formats}
    DateMask := InternationalDate(False, False);
    TimeMask := InternationalTime(True, False, True, True);
    WageMask := InternationalCurrency('9', 2, True, False);
    CurrMask := InternationalCurrency('#', 6, True, True);

    {break checking off}
    CheckBreak := False;

    {make sure we're in 80*25 mode}
    case CurrentMode of
      0..1 : TextMode(CurrentMode+2);
      else
       if Hi(LastMode) <> 0 then
         SelectFont8x8(False);
    end;

    {set TextAttr}
    TextAttr := ColorMono(MainTextColor, MainTextMono);

    {adjust color set a bit for B&W monitors}
    if CurrentMode <> 7 then
      OurColorSet.UnselXrefMono := $E;
  end;

  procedure Abort;
    {-Abort the program}
  begin
    WriteLn(emInsufficientMemory);
    Halt(1);
  end;

begin
  {miscellaneous initialization code}
  Initialize;

  {open the help file}
  OpenHelp;

  {draw basic outline of the screen}
  DrawMainScreen;

  {$IFDEF UseMouse}
  if MouseInstalled then begin
    {use a red diamond for our mouse cursor}
    with OurColorSet do
      SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+$04);
    ShowMouse;

    {enable mouse support}
    EntryCommands.cpOptionsOn(cpEnableMouse);
    MemoCommands.cpOptionsOn(cpEnableMouse);
    PickCommands.cpOptionsOn(cpEnableMouse);
    HelpCommands.cpOptionsOn(cpEnableMouse);
  end;
  {$ENDIF}

  {set up for background task}
  {$IFDEF UseDrag}                                  {!!.11}
  EntryCommands.SetKeyPressedProc(DragKeyPressed);  {!!.11}
  MemoCommands.SetKeyPressedProc(DragKeyPressed);   {!!.11}
  PickCommands.SetKeyPressedProc(DragKeyPressed);   {!!.11}
  HelpCommands.SetKeyPressedProc(DragKeyPressed);   {!!.11}
  {$ELSE}                                           {!!.11}
  EntryCommands.SetGetKeyProc(GetKey);
  MemoCommands.SetGetKeyProc(GetKey);
  PickCommands.SetGetKeyProc(GetKey);
  HelpCommands.SetGetKeyProc(GetKey);
  {$ENDIF}                                          {!!.11}

  {set up for context sensitive help}
  EntryCommands.SetHelpProc(DisplayHelp);
  MemoCommands.SetHelpProc(DisplayHelp);
  PickCommands.SetHelpProc(DisplayHelp);

  {set up user exit keys}
  with EntryCommands do begin
    {<AltB> turns bell on/off}
    AddCommand(ccUser0, 1, $3000, 0);

    {^PgUp selects previous record}
    AddCommand(ccPrevRec, 1, $8400, 0);

    {^PgDn selects next record}
    AddCommand(ccNextRec, 1, $7600, 0);
  end;

  {initialize pick list}
  with StateList do begin
    if not InitCustom(
      9, 8, 72, 18, OurColorSet, DefWindowOptions or wBordered, 21, 51,
      StateChoice, PickSnaking, SingleChoice) then
        Abort;

    {add a header centered at the top of the frame}
    wFrame.AddHeader(' Abbreviated State Names ', heTC);

    {$IFDEF UseScrollBars}
    {add horizontal scroll bar}
    wFrame.AddCustomScrollBar(
      frBB, 0, MaxLongInt, 1, 1, SliderChar, ScrollBarChar, OurColorSet);
    {$ENDIF}

    SetHelpIndex(4);
  end;

  {initialize memo window}
  with MW do begin
    if not InitCustom(
      9, 8, 72, 18,                 {window coordinates}
      OurColorSet,                  {color set}
      DefWindowOptions or wBordered,  {window options}
      SizeOf(MemoField),            {size of memo buffer}
      @Scrap.Notes) then            {address of buffer}
        Abort;

    {add a header centered at the top of the frame}
    wFrame.AddHeader(' Notes ', heTC);

    {$IFDEF UseScrollBars}
    {add vertical scroll bar}
    wFrame.AddCustomScrollBar(
      frRR, 0, MaxLongInt, 1, 1, SliderChar, ScrollBarChar, OurColorSet);
    {$ENDIF}

    {change the color for control characters}
    SetCtrlAttr(OurColorSet.HighlightColor, OurColorSet.HighlightMono);

    SetStatusProc(MemoFieldStatus);
    SetErrorProc(ErrorHandler);
    SetHelpIndex(15);
  end;

  with ES1, Scrap do begin
    {initialize the edit screen record}
    if not InitCustom(
      1, 1, 80, 25,                          {window coordinates}
      OurColorSet,                           {colors}
      DefWindowOptions and not wClear) then  {window options}
        Abort;

    {change attribute for text}
    SetTextAttr(MainTextColor, MainTextMono);

    {install user-written event handlers}
    SetPreEditProc(DisplayHelpPrompt);
    SetPostEditProc(PostEdit);
    SetErrorProc(ErrorHandler);

    {set edit screen options}
    SetWrapMode(WrapAtEdges);
    SetBeepOnError(On);

    {set field editing options}
    esFieldOptionsOn(efClearFirstChar);

    {add each of the fields in order: left to right, top to bottom}
    {                Prompt              Field  Fld Hlp  Range   Dec.  Field}
    { Prompt         Row Col Picture    Row Col Wid Ndx Low High Place Value}

    {a protected field}
    esFieldOptionsOn(efProtected);
    AddByteField(
      'Record',      04, 17, '99',      04, 25,     00, 00, 00,        CurrentRec);
                                                   {not part of Scrap--^}
    esFieldOptionsOff(efProtected);
    with OurColorSet do begin
      ChangeProtectedPromptAttr(idRecNum, HighlightColor, HighlightMono);
      ChangeProtectedFieldAttr( idRecNum, FieldColor, FieldMono);
    end;

    AddStringField(
      'Name',        07, 19, CharStr('x', 30),
                                        07, 25, 30, 01,                Name);

    {a required field}
    esFieldOptionsOn(efRequired);
    AddStringField(
      'Address',     08, 16, '',        08, 25, 30, 02,                Address);
    esFieldOptionsOff(efRequired);

    esFieldOptionsOff(efInsertPushes);
    AddStringField(
      'City',        09, 19, '',        09, 25, 25, 03,                City);
    esFieldOptionsOn(efInsertPushes);

    {a field linked to a pick list}
    esFieldOptionsOff(efAllowEscape);
    AddPickStringField(
      'State',       10, 18,            10, 25, 02, 04,                State,
      StateList);
    esFieldOptionsOn(efAllowEscape);

    AddStringField(
      'Zip',         10, 52, ZipMask,   10, 57, 10, 05,                Zip);
    ChangeValidation(idZip, ValidateZip);

    {a field linked to a nested entry screen}
    AddNestedField(
      'Phones',      11, 17, '',        11, 25, 02, 06);

    {multiple-choice field}
    AddChoiceField(
      'Gender',      13, 17, 'XXXXXXX', 13, 25,     07, 01, IncChoice, Gender);

    {a yes-no field}
    AddYesNoField(
      'Married',     13, 48, '',        13, 57,     08,                Married);

    {a date field}
    AddDateField(
      'Born',        14, 19, DateMask,  14, 25,     09, 00, 00,        Born);

    {a calculated field}
    esFieldOptionsOn(efProtected+efHidden);
    AddByteField(
      'Age',         14, 52, '999',     14, 57,     10, 00, 00,        Age);
    esFieldOptionsOff(efProtected+efHidden);

    {a numeric field}
    AddNumericRealField(
      'Hourly wage', 16, 12, WageMask,  16, 25,     11, 00, 999.99, 0, Wage);

    {a calculated field}
    esFieldOptionsOn(efProtected);
    SetPadChar('*');
    AddRealField(
      'Weekly',      16, 49, CurrMask,  16, 57,     12, 00, 00,     0, Weekly);
    SetPadChar(' ');
    esFieldOptionsOff(efProtected);

    {multiple-choice field}
    AddChoiceField(
      'Hours/week',  17, 13, '99',      17, 25,     13, 01, IncChoice, Hours);

    {a calculated field}
    esFieldOptionsOn(efProtected);
    SetPadChar('*');
    AddRealField(
      'Yearly',      17, 49, CurrMask,  17, 57,     14, 00, 00,     0, Yearly);
    SetPadChar(' ');
    esFieldOptionsOff(efProtected);

    {a field linked to a nested memo window}
    esFieldOptionsOff(efMapCtrls);
    AddNestedStringField(
      'Notes',       19, 18, '',        19, 25, SizeOf(NotesMsg)-1, 15, NotesMsg);
  end;

  {initialize the second entry screen}
  with ES2 do begin
    if not InitCustom(
      18, 13, 62, 14,                   {window coordinates}
      OurColorSet,                      {colors}
      wClear+wBordered) then            {window options}
        Abort;

    wFrame.AddHeader(' Phone Numbers ', heTC);

    SetPreEditProc(DisplayHelpPrompt2);
    SetErrorProc(ErrorHandler);

    SetWrapMode(ExitAtEdges);
    SetBeepOnError(On);
    esFieldOptionsOn(efAutoAdvance);
    esSecFieldOptionsOn(sefPadCurrentOnly);
    SetPadChar('_');
    SetPromptAttr(OurColorSet.HighlightColor, OurColorSet.HighlightMono);
    SetSelectedPromptAttr(OurColorSet.HighlightColor, OurColorSet.HighlightMono);

    AddStringField(
      'Work phone number', 01, 08, PhoneMask, 01, 26, 14, 16, Scrap.WPhone);
    ChangeValidation(idWPhone, ValidatePhone);

    AddStringField(
      'Home phone number', 02, 08, PhoneMask, 02, 26, 14, 17, Scrap.HPhone);
    ChangeValidation(idHPhone, ValidatePhone);
  end;

  CurrentRec := 1;
  AllDone := False;
  with ES1 do
    repeat
      {copy the current record into the scrap record used for editing}
      Scrap := InfoRecs[CurrentRec];

      {reset Age field}
      HideAgeField;

      {start editing}
      Process;

      {did user ask to quit?}
      if GetLastCommand = ccQuit then
        {confirm that the user wants to quit}
        if not ConfirmQuitting then
          {cancel the command}
          SetLastCommand(ccNone);

      {copy the edited record back if ESC wasn't pressed}
      if GetLastCommand <> ccQuit then
        InfoRecs[CurrentRec] := Scrap;

      {see if we need to edit another record}
      case GetLastCommand of
        ccError,               {an error occurred}
        ccDone,                {^Enter, ^KD, or ^KQ}
        ccQuit :               {ESC}
          AllDone := True;
        ccNextRec :            {next record}
          if CurrentRec < MaxRec then
            Inc(CurrentRec);
        ccPrevRec :            {previous record}
          if CurrentRec > 1 then
            Dec(CurrentRec);
        ccUser0 :              {toggle Bell on/off}
          if esFieldOptionsAreOn(efBeepOnError) then begin
            ES1.SetBeepOnError(Off);
            ES2.SetBeepOnError(Off);
          end
          else begin
            ES1.SetBeepOnError(On);
            ES2.SetBeepOnError(On);
          end;
        ccNested :             {handle nested entry screen}
          if GetCurrentID = idNotes then begin
            {edit the notes field}
            EditMemoField;

            {copy the notes field}
            InfoRecs[CurrentRec].Notes := Scrap.Notes;
          end
          {switch to secondary edit screen}
          else if SecondaryEditScreen then
            {advance to Gender field}
            SetNextField(idGender)
          else
            {back up to State field}
            SetNextField(idState);
      end;
    until AllDone;

  {$IFDEF UseMouse}
  {hide the mouse cursor}
  HideMouse;
  {$ENDIF}

  {these calls are unnecessary in this case}
  ES1.Done;
  ES2.Done;
  MW.Done;
  HW.Done;

  {clean up display}
  NormVideo;
  ClrScr;
end.
