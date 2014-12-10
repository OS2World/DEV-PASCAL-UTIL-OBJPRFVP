{$R-,S-,I-,V-,B-,F-,O-}

{*********************************************************}
{*                   MSMAIN.PAS 1.30                     *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program requires that OPDEFINE.INC activate the following defines:
   UseScrollBars, UseHotSpots, UseAdjustableWindows, UseShadows, UseStreams
 This program will use features activated with the following defines:
   UseMouse, UseBcd, N+, UseDates,
 ***************************************************************************}

{$IFNDEF UseScrollBars}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseHotSpots}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseShadows}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

{$IFNDEF UseStreams}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}


unit MSMain;
  {-Data entry screen designer for Object Professional 1.0}

interface

uses
  Use32,
  dos,                            {DOS/BIOS stuff}
  opinline,                       {inline macros}
  opconst,                        {!!.20}
  oproot,                         {base objects, error codes, etc.}
  opcmd,                          {command processing}
  opcrt,                          {low-level screen handling}
  {$IFDEF UseMouse}
  opmouse,                        {mouse handling}
  {$ENDIF}
  opstring,                       {string handling}
  opdos,                          {misc. DOS/BIOS routines}
  opframe,                        {frames, shadows, etc.}
  opwindow,                       {window management}
  {$IFDEF UseBcd}
  opbcd,                          {BCD reals}
  {$ENDIF}
  {$IFDEF UseDates}
  opdate,                         {date/time arithmetic}
  {$ENDIF}
  opabsfld,                       {abstract field, picture masks}
  opfield,                        {field definitions}
  opedit,                         {line editor}
  opselect,                       {selectors}
  opentry,                        {entry screens}
  oppick,                         {pick lists}
  opdir,                          {directory lists}
  opmenu,                         {menus}
  makemisc,                       {miscellaneous routines, objects}
  makeces,                        {custom entry screens}
  msutil;                         {utility routines}

  procedure Main;
    {-Main program block}

  {===========================================================}

implementation

type
  EntryScreenInfo =
    object(StackWindow)
      esiBrightColor : Byte;
      esiBrightMono  : Byte;
      esiDimColor    : Byte;
      esiDimMono     : Byte;

      constructor Init(X1, Y1 : Byte);
        {-Initialize the entry screen info window using default colors and options}
      constructor InitCustom(X1, Y1 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt);
        {-Initialize the entry screen info window with custom options, colors}
      procedure UpdateContents; virtual;
        {-Redraw the entry screen info window}
      procedure Process; virtual;
        {-Display the entry screen info window and wait for a keypress}
    end;
const
  FirstCmd   : Byte = ccSelect; {first commands to execute}

  TabDelta   = 8;
  WordDelims : CharSet = [^I, ' '..'"', '.', ',', ':', ';', '?', '!',
                          '*','(', ')', '[', ']', '{', '}', '<', '>',
                          '+', '-', '/', '\', '''', '$', '=', '^', '#'];

var
  MainMenu   : Menu;         {main menu}
  Item       : Word;         {last menu item selected}
  Cmd        : Word;         {last menu command}
  AllDone    : Boolean;      {True when ready to quit}

const
  {Keystroke to command mapping}
  MakeScrnKeyMax = 200;   {last available slot in MakeScrnKeySet}
  MakeScrnKeySet : array[0..MakeScrnKeyMax] of Byte = (
   {length keys         command type      key sequence}
    3,     $00, $0F,    ccBackTab,       {Shift-Tab}
    3,     $00, $3B,    ccHelp,          {F1}
    3,     $00, $40,    ccNextField,     {F6}    {!!.01}
    3,     $00, $63,    ccPrevField,     {^F6}   {!!.01}
    3,     $00, $47,    ccHome,          {Home}
    3,     $00, $48,    ccUp,            {Up}
    3,     $00, $49,    ccPageUp,        {PgUp}
    3,     $00, $4B,    ccLeft,          {Left}
    3,     $00, $4D,    ccRight,         {Right}
    3,     $00, $4F,    ccEnd,           {End}
    3,     $00, $50,    ccDown,          {Down}
    3,     $00, $51,    ccPageDn,        {PgDn}
    3,     $00, $52,    ccIns,           {Ins}
    3,     $00, $53,    ccDel,           {Del}
    3,     $00, $73,    ccWordLeft,      {^Left}
    3,     $00, $74,    ccWordRight,     {^Right}
    3,     $00, $75,    ccScreenBot,     {^End}
    3,     $00, $76,    ccEndOfFile,     {^PgDn}
    3,     $00, $77,    ccScreenTop,     {^Home}
    3,     $00, $84,    ccTopOfFile,     {^PgUp}
    2,     $01,         ccWordLeft,      {^A}
    2,     $03,         ccPageDn,        {^C}
    2,     $04,         ccRight,         {^D}
    2,     $05,         ccUp,            {^E}
    2,     $06,         ccWordRight,     {^F}
    2,     $07,         ccDel,           {^G}
    2,     $08,         ccBack,          {^H, Bksp}
    2,     $09,         ccTab,           {^I, Tab}
    2,     $0D,         ccSelect,        {^M, Enter}
    2,     $0E,         ccInsertLine,    {^N}
    2,     $12,         ccPageUp,        {^R}
    2,     $13,         ccLeft,          {^S}
    2,     $14,         ccDelWord,       {^T}
    2,     $17,         ccUp,            {^W}
    2,     $18,         ccDown,          {^X}
    2,     $19,         ccDelLine,       {^Y}
    2,     $1A,         ccDown,          {^Z}
    2,     $1B,         ccQuit,          {Esc}
    2,     $7F,         ccBack,          {^Bksp}
    3,     $11, $03,    ccEndOfFile,     {^Q^C}
    3,     $11, $04,    ccEnd,           {^Q^D}
    3,     $11, $05,    ccScreenTop,     {^Q^E}
    3,     $11, $12,    ccTopOfFile,     {^Q^R}
    3,     $11, $13,    ccHome,          {^Q^S}
    3,     $11, $18,    ccScreenBot,     {^Q^X}
    3,     $11, $19,    ccDelEol,        {^Q^Y}
  {$IFDEF UseMouse}
    3,     $00, $EF,    ccMouseSel,      {click left  = mouse select}
    3,     $00, $EE,    ccQuit,          {click right = ESC}
    3,     $00, $ED,    MainMenuCmd,     {click both  = help}
  {$ELSE}
                0, 0, 0, 0, 0, 0,        {170} {!!.01}
    0, 0, 0, 0, 0, 0,                    {180} {!!.01}

  {$ENDIF}
    {-----------pad to end of array----------}
                      0, 0, 0, 0,        {180}  {!!.01}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {190}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0);       {200}

  {Keystroke to command mapping}
  HotKeyMax = 50;
  HotKeySet : array[0..HotKeyMax] of Byte = (
  {length keys         command type      key sequence}
  3,      $00, $13,    ResizeWindowCmd,  {AltR}
  3,      $00, $14,    TestCmd,          {AltT}
  3,      $00, $17,    InfoCmd,          {AltI}
  3,      $00, $1F,    StatusCmd,        {AltS}
  3,      $00, $2D,    ExitCmd,          {AltX}
  3,      $00, $32,    MoveWindowCmd,    {AltM}
  3,      $00, $3C,    SaveFileCmd,      {F2}
  3,      $00, $3D,    NewFileCmd,       {F3}
  3,      $00, $44,    MainMenuCmd,      {F10}
                 0, 0, 0, 0, 0,          {40}
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0           {50}
  );
{.F+}

var
  MakeScrnCommands : CommandProcessor;

{$IFDEF VIRTUALPASCAL}
  {$OrgName+}
  {$L VPMSMAIN.OBJ}
{$ELSE}
  {$L MSMAIN.OBJ}
{$ENDIF}

  {$F+}
  function GetPicture(B : Byte) : StringPtr; external;
    {-Used to display sample picture masks}
  {$F-}

  {$I MSMAIN.IN1}    {miscellaneous}
  {$I MSMAIN.IN2}    {adding/editing fields}

  procedure DoTab(Delta : Integer);
    {-Tab left or right}
  var
    I : Integer;
  begin
    with CES do
      if (Delta = 1) then
        cesX := Succ(Succ(Pred(cesX) div TabDelta) * Word(TabDelta))
      else if (Delta = -1) then begin
        I := Pred(cesX) mod TabDelta;
        if I > 0 then
          Dec(cesX, I)
        else if (cesX > 1) then
          Dec(cesX, TabDelta);
      end;
  end;

  procedure EndOfFile;
    {-Go to end of last line with text on it}
  var
    I, J, LastRow : Word;
  begin
    with CES do begin
      LastRow := sesVS.vRows;
      for I := LastRow downto 1 do begin
        J := Length(cesReadRow(I));
        if J > 0 then begin
          cesGotoXY(J+1, I);
          Exit;
        end;
      end;
      cesGotoXY(1, 1);
    end;
  end;

  procedure WordLeft;
    {-Move one word to the left}
  var
    S : string;
    SLen : Byte absolute S;
  begin
    with CES do begin
      S := cesReadRow(cesY);
      if cesX > 1 then begin
        if cesX > SLen then
          cesX := Succ(SLen);
        Dec(cesX);
        while (cesX > 0) and not IsAlpha(S[cesX]) do
          Dec(cesX);
        while (cesX > 0) and IsAlpha(S[cesX]) do
          Dec(cesX);
        Inc(cesX);
      end
      else if cesY > 1 then begin
        Dec(cesY);
        cesX := Length(cesReadRow(cesY))+1;
      end;
    end;
  end;

  procedure WordRight;
    {-Move one word to the right}
  var
    S : string;
    SLen : Byte absolute S;
  begin
    with CES do begin
      S := cesReadRow(cesY);
      if cesX <= SLen then begin
        while (cesX <= SLen) and IsAlpha(S[cesX]) do
          Inc(cesX);
        while (cesX <= SLen) and not IsAlpha(S[cesX]) do
          Inc(cesX);
      end
      else if (cesY < cesMaxRow) then begin
        cesX := 1;
        Inc(cesY);
      end;
    end;
  end;

  {$IFDEF UseMouse}
  function MouseSelect : Boolean;
    {-Process mouse click}
  var
    FramePos : FramePosType;
    L : LongInt;
    HotCode : Byte;
    MouseAbsX, MouseAbsY : Integer;

    procedure ScrollHorizontally(TargetCol : Integer);
      {-Scroll horizontally when mouse clicked on horizontal scroll bar}
    begin
      {compute ideal column}
      with CES do
        cesX := TweakSlider(FramePos, MouseAbsX, TargetCol, 1);
    end;

    procedure ScrollVertically(TargetRow : Integer);
      {-Scroll vertically when mouse clicked on vertical scroll bar}
    begin
      {compute ideal row}
      with CES do
        cesY := TweakSlider(FramePos, MouseAbsY, TargetRow, 1);
    end;

  begin
    MouseSelect := False;
    with CES do begin
      {determine position of mouse}
      EvaluateMousePos;
      L := PosResults(FramePos, HotCode);
      MouseAbsX := MouseKeyWordX+MouseXLo;
      MouseAbsY := MouseKeyWordY+MouseYLo;

      case FramePos of
        frTL..frRR :       {on the frame}
          case HotCode of
            hsDecV :       {the decrement fixture of a vertical scroll bar}
             if ByteFlagIsSet(asOptions, esMousePage) then
               asPageUpOrDown(-1)
             else
               cesMoveXY(0, -1);
            hsDecH :       {the decrement fixture of a horizontal scroll bar}
              DoTab(-1);
            hsIncV :       {the increment fixture of a vertical scroll bar}
             if ByteFlagIsSet(asOptions, esMousePage) then
               asPageUpOrDown(+1)
             else
               cesMoveXY(0, +1);
            hsIncH :       {the increment fixture of a horizontal scroll bar}
              DoTab(1);
            hsBar :            {the slider portion of a scroll bar}
              case FramePos of
                frLL, frRR :   {vertical scroll bar}
                  ScrollVertically(L);
                else           {horizontal scroll bar}
                  ScrollHorizontally(L);
              end;
          end;

        frInsideActive :   {inside window}
          if (MouseAbsX = WhereXabs) and (MouseAbsY = WhereYabs) then
            MouseSelect := True
          else
            cesMoveXY(MouseAbsX-WhereXabs, MouseAbsY-WhereYabs);
      end;
    end;
  end;
  {$ENDIF}

  function MoveCursor(XL, YL, XH, YH : Word) : Boolean; {!!.03} {rewritten}
    {-Move the cursor within the specified window}
  var
    Cmd, Key : Word;
    DoingLine : Boolean;
    Finished : Boolean;
    OldX, OldY : Word;
  begin
    EraseMainMenu;
    MoveCursor := False;
    Status.Display;
    with CES do begin
      SetCursor(cuBlock);

      OldX := cesX;
      OldY := cesY;

      Finished := False;
      DoingLine := FlagIsSet(CES.cesFlags, cesMakeLine+cesMakeBox);
      repeat
        if cesX < XL then
          cesX := XL
        else if cesX > XH then
          cesX := XH;
        if cesY < YL then
          cesY := YL
        else if cesY > YH then
          cesY := YH;
        cesGotoXY(cesX, cesY);
        if (cesX <> OldX) or (cesY <> OldY) then begin
          Status.FastUpdate(MakeScrnCommands.cpKeyPressedProc);
          OldX := cesX;
          OldY := cesY;
        end;
        Cmd := MakeScrnCommands.GetCommand(Key);

        case Cmd of
          ccLeft :
            Dec(cesX);
          ccRight :
            Inc(cesX);
          ccUp :
            Dec(cesY);
          ccDown :
            Inc(cesY);
          ccHome :
            cesX := XL;
          ccEnd :
            cesX := Length(cesReadRow(cesY))+1;
          ccQuit :
            Finished := True;
          ccSelect :
            begin
              MoveCursor := True;
              Finished := True;
            end;
          {$IFDEF UseMouse}
          ccMouseSel :
            if MouseSelect then begin
              MoveCursor := True;
              Finished := True;
            end;
          {$ENDIF}
          StatusCmd :
            Status.Toggle;
          else if not DoingLine then
            case Cmd of
              ccPageUp :
                asPageUpOrDown(-1);
              ccPageDn :
                asPageUpOrDown(+1);
              ccWordLeft :
                WordLeft;
              ccWordRight :
                WordRight;
              ccScreenBot :
                cesGotoXY(cesX, cesRowAtBottom);
              ccScreenTop :
                cesGotoXY(cesX, cesRowAtTop);
              ccTopOfFile :
                cesGotoXY(1, 1);
              ccEndOfFile :
                EndOfFile;
              ccTab :
                DoTab(+1);
              ccBackTab :
                DoTab(-1);
            end;
        end;
      until Finished;
      SetCursor(cuNormal);
    end;
  end;

  procedure MoveTextField(TFP : TextFieldPtr);
    {-Move a text field}
  var
    D : Word;
  begin
    with TFP^ do begin
      {move cursor to start of text field}
      CES.cesX := tfCol;
      CES.cesY := tfRow;

      D := Length(tfString^)-1;
    end;

    {move the cursor to new position}
    if MoveCursor(1, 1, cesMaxCol-D, cesMaxRow) then
      {move the text field}
      if CES.MoveTextFieldToCursor(TFP) then begin
        {mark as modified and redraw}
        Modified := True;
        RedrawCES(False);
      end;
  end;

  procedure CopyTextField(TFP : TextFieldPtr);
    {-Copy a text field}
  var
    D : Word;
  begin
    with CES, TFP^ do begin
      {move cursor to start of text field}
      cesX := tfCol;
      cesY := tfRow;

      D := Length(tfString^)-1;

      {move the cursor to new position}
      if MoveCursor(1, 1, cesMaxCol-D, cesMaxRow) then
        if (cesX <> tfCol) or (cesY <> tfRow) then
          {copy the text field}
          if CES.CopyTextFieldToCursor(TFP) then begin
            {mark as modified and redraw}
            Modified := True;
            RedrawCES(False);
          end;
    end;
  end;

  procedure MoveEntryField(EFP : EntryFieldPtr);
    {-Move a text field}
  var
    XL, YL, XH, YH : Integer;
    XD1, YD1, XD2, YD2 : Integer;
  begin
    with EFP^ do begin
      {move cursor to start of prompt}
      CES.cesX := sfPCol;
      CES.cesY := sfPRow;

      {calculate deltas for min/max coordinates}
      CES.GetFieldCoordinates(EFP, XL, YL, XH, YH);
      XD1 := sfPCol-XL;
      YD1 := sfPRow-YL;
      if XH = XL then
        XD2 := 0
      else
        XD2 := (XH-XL)-(sfPCol-XL);
      YD2 := YH-sfPRow;
    end;

    {move the cursor to new position}
    if MoveCursor(1+XD1, 1+YD1, cesMaxCol-XD2, cesMaxRow-YD2) then
      {move the entry field}
      if CES.MoveEntryFieldToCursor(EFP) then begin
        {mark as modified and redraw}
        Modified := True;
        RedrawCES(False);
      end;
  end;

  function CopyEntryFieldToCursor(EFP : EntryFieldPtr) : Boolean;
    {-Make a copy of the specified field at the cursor}
  var
    XD, YD : Integer;
  begin
    CopyEntryFieldToCursor := False;

    {initialize field record}
    InitFieldRecFromEFP(EFP);

    with FR do begin
      {calculate Delta}
      XD := Integer(CES.cesX)-Integer(frPCol);
      YD := Integer(CES.cesY)-Integer(frPRow);

      {does field need to be copied?}
      if (XD = 0) and (YD = 0) then
        Exit;

      {adjust the coordinates}
      Inc(frPCol, XD);
      Inc(frPRow, YD);
      Inc(frFCol, XD);
      Inc(frFRow, YD);
    end;

    {add the data field to the entry screen}
    if AddDataFieldPrim then begin
      {sort the field list}
      CES.SortEntryFields;

      Modified := True;
      CES.ForceReset;
      RedrawCES(False);

      CopyEntryFieldToCursor := True;
    end
    else
      PopupErrorMessage('Error copying field');
  end;

  procedure RemoveEntryField(EFP : EntryFieldPtr);
    {-Remove an entry field}
  begin
    if ConfirmAction('Delete Entry Field') then begin
      CES.DeleteEntryField(EFP);
      Modified := True;
      RedrawCES(False);
    end;
  end;

  procedure CopyEntryField(EFP : EntryFieldPtr);
    {-Copy an entry field}
  var
    XL, YL, XH, YH : Integer;
    XD1, YD1, XD2, YD2 : Integer;
  begin
    with EFP^ do begin
      {move cursor to start of prompt}
      CES.cesX := sfPCol;
      CES.cesY := sfPRow;

      {calculate deltas for min/max coordinates}
      CES.GetFieldCoordinates(EFP, XL, YL, XH, YH);
      XD1 := sfPCol-XL;
      YD1 := sfPRow-YL;
      if XH = XL then
        XD2 := 0
      else
        XD2 := (XH-XL)-(sfPCol-XL);
      YD2 := YH-sfPRow;
    end;

    {move the cursor to new position}
    if MoveCursor(1+XD1, 1+YD1, cesMaxCol-XD2, cesMaxRow-YD2) then
      {copy the entry field}
      if CopyEntryFieldToCursor(EFP) then begin
        {mark as modified and redraw}
        Modified := True;
        RedrawCES(False);
      end;
  end;

  procedure RemoveTextField(TFP : TextFieldPtr);
    {-Remove a text field}
  begin
    if ConfirmAction('Delete Text Field') then begin
      CES.DeleteTextField(TFP);
      RedrawCES(False);
      Modified := True;
    end;
  end;

  procedure EditTextField(TFP : TextFieldPtr);
    {-Edit an existing text field}
  var
    S : string;
    I : Word;
    Redraw : Boolean;
    Color, Mono : Byte;
  begin
    {edit the textstring}
    S := TFP^.tfString^;
    I := MinWord(Succ(cesMaxCol-TFP^.tfCol), 255);
    if not PopupGetString('Text Field', 'Text: ', False, False, I, S) then
      Exit;

    {delete it if the new string is empty}
    if S = '' then
      RemoveTextField(TFP)
    else begin
      {modify the string}
      Redraw := False;
      if (S <> TFP^.tfString^) then
        if CES.ModifyTextField(TFP, S) then begin
          Modified := True;
          Redraw := True;
        end
        else begin
          InsufficientMemory;
          Exit;
        end;

      {change the colors}
      with TFP^ do begin
        Color := tfColorAttrs[0];
        Mono := tfMonoAttrs[0];
        if EditColor('', Color, Mono) then begin {!!.03}
          CES.ChangeTextFieldAttrs(TFP, Color, Mono);
          Modified := True;
          Redraw := True;
        end;
      end;

      if Redraw then
        RedrawCES(False);
    end;
  end;

  procedure AddATextField;
    {-Add a new text field}
  var
    S : string;
    I : Word;
    Color, Mono : Byte;
    TFP : TextFieldPtr;
    EFP : EntryFieldPtr;
  begin
    {don't add a new one on top of an existing one}
                                    {!!.03}
    if CES.FindTextFieldAtCursor(TFP, True) or CES.FindEntryFieldAtCursor(EFP) then
      OverlapError
    else begin
      {get the text for the field}
      S := '';
      I := MinWord(Succ(cesMaxCol-CES.cesX), 255);
      if not PopupGetString('Text Field', 'Text: ', False, False, I, S) then
        Exit;
      if S = '' then
        Exit;

      {try to add it}
      TFP := CES.AddTextFieldAtCursor(S); {!!.03}
      if TFP = nil then                   {!!.03}
        InsufficientMemory
      else begin
        {change the colors if desired}
        {if CES.FindTextFieldAtCursor(TFP, True) then} {!!.03}
        with TFP^ do begin
          Color := tfColorAttrs[0];
          Mono := tfMonoAttrs[0];
          if EditColor('', Color, Mono) then {!!.03}
            CES.ChangeTextFieldAttrs(TFP, Color, Mono);
        end;

        Modified := True;
        RedrawCES(False);
      end;
    end;
  end;

  function EditLineChars(var Ch1, Ch2, Ch3 : Char) : Boolean; {!!.03}
    {-Edit characters that make up a line field}
  const
    LFst : string[12] = 'Line Field: ';
  begin
    EditLineChars := False;
    if not EditChar(LFst+'Head', Ch1, False) then
      Exit;
    if not EditChar(LFst+'Middle', Ch2, False) then
      Exit;
    if EditChar(LFst+'Tail', Ch3, False) then
      EditLineChars := True;
  end;

  procedure AddALineField(Vert : Boolean);    {!!.03}
    {-Add a new line field}
  var
    Ch1, Ch2, Ch3 : Char;
    Color, Mono : Byte;
    MaxRow, MaxCol : Word;
    EFP : EntryFieldPtr;
    TLFP : TextLineFieldPtr;
  begin
    {don't add a new one on top of an existing entry field}
    if CES.FindEntryFieldAtCursor(EFP) then begin
      OverlapError;
      Exit;
    end;

    {get the three characters that will make up the line}
    if Vert then begin
      Ch1 := '³';
      Ch2 := '³';
      Ch3 := '³';
    end
    else begin
      Ch1 := 'Ä';
      Ch2 := 'Ä';
      Ch3 := 'Ä';
    end;
    if not EditLineChars(Ch1, Ch2, Ch3) then
      Exit;

    with CES do begin
      {try to add the line field}
      TLFP := AddLineFieldAtCursor(Ch1, Ch2, Ch3, 1, Vert);
      if TLFP = nil then begin
        InsufficientMemory;
        Exit;
      end;

      {calculate max row and col}
      if Vert then begin
        MaxRow := cesMaxRow;
        MaxCol := cesX;
      end
      else begin
        MaxRow := cesY;
        MaxCol := cesMaxCol;
      end;

      {set end-of-line}
      SetFlag(cesFlags, cesMakeLine);
      if MoveCursor(cesX, cesY, MaxCol, MaxRow) then begin
        {change the colors if desired}
        with TLFP^ do begin
          Color := tfColorAttrs[0];
          Mono := tfMonoAttrs[0];
          if EditColor('', Color, Mono) then
            ChangeTextFieldAttrs(TLFP, Color, Mono);
        end;

        Modified := True;
      end
      else
        {remove the text field}
        asTextFields.Delete(TLFP);
      DoneAddingLineOrBox;

      ForceReset;
    end;
    RedrawCES(False);
  end;

  procedure EditLineField(TFP : TextFieldPtr);
    {-Edit an existing line field}
  var
    TLFP : TextLineFieldPtr absolute TFP;
    MaxRow, MaxCol : Word;
    Color, Mono : Byte;
    SaveLen : Word;
  begin
    with CES, TLFP^ do begin
      {edit the three characters that make up the line}
      if not EditLineChars(tfString^[1], tfString^[2], tfString^[3]) then
        Exit;

      {calculate max row and col}
      if FlagIsSet(tfFlags, tfVertical) then begin
        MaxRow := cesMaxRow;
        MaxCol := tfCol;
        cesX := MaxCol;
        cesY := tfRow+Pred(tlfLength);
      end
      else begin
        MaxRow := tfRow;
        MaxCol := cesMaxCol;
        cesX := tfCol+Pred(tlfLength);
        cesY := MaxRow;
      end;

      {set end-of-line}
      SetFlag(cesFlags, cesMakeLine);
{$IFDEF VIRTUALPASCAL}
      SetFlag16(tfFlags, tfMarkEnd);
{$ELSE}
      SetFlag(tfFlags, tfMarkEnd);
{$ENDIF}
      SaveLen := tlfLength;
      if MoveCursor(tfCol, tfRow, MaxCol, MaxRow) then begin
        {change the colors if desired}
        Color := tfColorAttrs[0];
        Mono := tfMonoAttrs[0];
        if EditColor('', Color, Mono) then
          ChangeTextFieldAttrs(TLFP, Color, Mono);
      end
      else
        tlfLength := SaveLen;

      DoneAddingLineOrBox;
      Modified := True;
      ForceReset;
    end;
    RedrawCES(False);
  end;

  procedure EditBoxField(TFP : TextFieldPtr);
    {-Edit an existing box field}
  var
    BColor, BMono : Byte;
    SColor, SMono : Byte;
    MaxRow, MaxCol : Word;
    SaveRow, SaveCol : Word;
    FR : FrameArray;
    TBFP : TextBoxFieldPtr absolute TFP;
  begin
    with CES, TBFP^ do begin
      {select a frame type}
      Move(tfString^[1], FR, SizeOf(FrameArray));
      if not EditFramePrim(FR, 56) then
        Exit;
      Move(FR, tfString^[1], SizeOf(FrameArray));

      {calculate max row and col}
      MaxRow := cesMaxRow;
      MaxCol := cesMaxCol;
      if FlagIsSet(tfFlags, tfShadowed) then begin
        SetFlag(cesFlags, cesMakeShBox);
        Dec(MaxRow);
        Dec(MaxCol);
      end;
      cesX := tbfColH;
      cesY := tbfRowH;
      SaveRow := cesY;
      SaveCol := cesX;

      {set bottom right corner}
      SetFlag(cesFlags, cesMakeBox);
{$IFDEF VIRTUALPASCAL}
      SetFlag16(tfFlags, tfMarkEnd);
{$ELSE}
      SetFlag(tfFlags, tfMarkEnd);
{$ENDIF}
      if MoveCursor(tfCol, tfRow, MaxCol, MaxRow) then begin
        {change the colors if desired}
        BColor := tfColorAttrs[0];
        BMono := tfMonoAttrs[0];
        SColor := tfColorAttrs[1];
        SMono := tfMonoAttrs[1];
        if EditColor(' Box', BColor, BMono) then {};
        if FlagIsSet(tfFlags, tfShadowed) then
          if EditColor(' Shadow', SColor, SMono) then {};
        ChangeBoxFieldAttrs(TBFP, BColor, BMono, SColor, SMono);
      end
      else begin
        tbfRowH := SaveRow;
        tbfColH := SaveCol;
      end;

      DoneAddingLineOrBox;
      Modified := True;
      ForceReset;
    end;
    RedrawCES(False);
  end;

  procedure AddABoxField(Shadowed : Boolean); {!!.03}
    {-Add a new box field}
  var
    BColor, BMono : Byte;
    SColor, SMono : Byte;
    MaxRow, MaxCol : Word;
    FR : FrameArray;
    EFP : EntryFieldPtr;
    TBFP : TextBoxFieldPtr;
  begin
    {don't add a new one on top of an existing entry field}
    if CES.FindEntryFieldAtCursor(EFP) then begin
      OverlapError;
      Exit;
    end;

    {select a frame type}
    FR := SglWindowFrame;
    if not EditFramePrim(FR, 56) then
      Exit;

    with CES do begin
      {try to add the box field}
      TBFP := AddBoxFieldAtCursor(FR, 1, 1, Shadowed);
      if TBFP = nil then begin
        InsufficientMemory;
        Exit;
      end;

      {calculate max row and col}
      MaxRow := cesMaxRow;
      MaxCol := cesMaxCol;
      if Shadowed then begin
        Dec(MaxRow);
        Dec(MaxCol);
      end;

      {set bottom right corner}
      SetFlag(cesFlags, cesMakeBox);
      if Shadowed then
        SetFlag(cesFlags, cesMakeShBox);
      if MoveCursor(cesX, cesY, MaxCol, MaxRow) then begin
        {change the colors if desired}
        with TBFP^ do begin
          BColor := tfColorAttrs[0];
          BMono := tfMonoAttrs[0];
          SColor := tfColorAttrs[1];
          SMono := tfMonoAttrs[1];
          if EditColor(' Box', BColor, BMono) then {};
          if Shadowed then
            if EditColor(' Shadow', SColor, SMono) then {};
          ChangeBoxFieldAttrs(TBFP, BColor, BMono, SColor, SMono);
        end;

        Modified := True;
      end
      else
        {remove the text field}
        asTextFields.Delete(TBFP);
      DoneAddingLineOrBox;

      ForceReset;
    end;
    RedrawCES(False);
  end;

  function EditFieldAtCursor : Boolean;   {!!.03}
    {-Edit the text or entry field at cursor, if any}
  var
    TFP : TextFieldPtr;
    EFP : EntryFieldPtr;
  begin
    EditFieldAtCursor := True;
    with CES do
      if FindTextFieldAtCursor(TFP, False) then
        case TextFieldType(TFP) of
          cesHLine, cesVLine :
            EditLineField(TFP);
          cesBox, cesShBox :
            EditBoxField(TFP);
          else
            EditTextField(TFP);
        end
      else if FindEntryFieldAtCursor(EFP) then
        EditDataField(EFP)
      else
        EditFieldAtCursor := False;
  end;

  procedure ScrollAround;
    {-Allow user to position cursor, execute design commands, etc.}
  var
    Cmd, Key   : Word;
    Finished   : Boolean;
    TFP        : TextFieldPtr;
    EFP        : EntryFieldPtr;
    OldX, OldY : Word;
  begin
    EraseMainMenu;

    Status.Display;
    with CES do begin
      SetCursor(cuNormal);

      OldX := cesX;
      OldY := cesY;

      Finished := False;
      repeat
        cesGotoXY(cesX, cesY);

        if (cesX <> OldX) or (cesY <> OldY) then begin
          Status.FastUpdate(MakeScrnCommands.cpKeyPressedProc);
          OldX := cesX;
          OldY := cesY;
        end;
        Cmd := MakeScrnCommands.GetCommand(Key);
        case Cmd of
          ccLeft :
            cesMoveXY(-1, 0);
          ccRight :
            cesMoveXY(+1, 0);
          ccUp :
            cesMoveXY(0, -1);
          ccDown :
            cesMoveXY(0, +1);
          ccPageUp :
            asPageUpOrDown(-1);
          ccPageDn :
            asPageUpOrDown(+1);
          ccHome :
            cesMoveXY(-cesX, 0);
          ccEnd :
            cesGotoXY(Length(cesReadRow(cesY))+1, cesY);
          ccWordLeft :
            WordLeft;
          ccWordRight :
            WordRight;
          ccScreenBot :
            cesGotoXY(cesX, cesRowAtBottom);
          ccScreenTop :
            cesGotoXY(cesX, cesRowAtTop);
          ccTopOfFile :
            cesGotoXY(1, 1);
          ccEndOfFile :
            EndOfFile;
          ccTab :
            DoTab(+1);
          ccBackTab :
            DoTab(-1);
          ccSelect :
            if not EditFieldAtCursor then {!!.03}
              RingBell;
          ccNextField,     {!!.01}
          ccPrevField :    {!!.01}
            if HaveDummyField then
              RingBell
            else begin
              if not FindEntryFieldAtCursor(EFP) then begin
                GuessCurrentField;
                if asNext = nil then
                  asNext := asCurrent;
              end
              else if Cmd = ccNextField then begin
                if EFP^.dlNext <> nil then
                  asNext := Pointer(EFP^.dlNext)
                else
                  asNext := Pointer(asFields.Head);
              end
              else if EFP^.dlPrev <> nil then
                asNext := Pointer(EFP^.dlPrev)
              else
                asNext := Pointer(asFields.Tail);
              asCurrent := asNext;
              sesBind;
              asFixWindow(True, False);
              cesBind;
              with asCurrent^ do
                cesGotoXY(sfFCol, sfFRow);
            end;
          ccInsertLine :
            begin
              InsertLineAtCursor;
              UpdateContents;
              Modified := True;
            end;
          ccDelLine :
            if (not HaveFieldOnCurrentLine) or
               ConfirmAction('Delete All Fields On Line') then begin
                 DeleteLineAtCursor;
                 UpdateContents;
                 Modified := True;
               end;
          MoveWindowCmd :
            begin
              if Status.IsActive then
                Status.EraseHidden;
              if not MoveOrResize(CES, True, Modified) then
                InsufficientMemory;
              Status.Display;
            end;
          ResizeWindowCmd :
            begin
              if Status.IsActive then
                Status.EraseHidden;
              if not MoveOrResize(CES, False, Modified) then
                InsufficientMemory;
              Status.Display;
            end;
          ExitCmd :
            if OkToQuit then begin
              Finished := True;
              AllDone := True;
            end;
          SaveFileCmd :
            SaveEntryScreen;
          NewFileCmd :
            begin
              LoadLibrary;
              Finished := not HaveCES;
            end;
          TestCmd :
            begin
              TestEntryScreen;
              Finished := ClassifyError(cwGetLastError) = etFatal;
            end;
          InfoCmd :
            ShowEntryScreenInfo;
          StatusCmd :
            Status.Toggle;
          MainMenuCmd,
          ccQuit :
            Finished := True;
          {$IFDEF UseMouse}
          ccMouseSel :
            if MouseSelect then
              {same as ccSelect}
              if not EditFieldAtCursor then {ignore click}; {!!.03}
          {$ENDIF}
        end;
      until Finished;
    end;
  end;

  procedure MoveResizePrim(MoveOnly : Boolean);
    {-Move or resize the window}
  begin
    EraseMainMenu;
    if Status.IsActive then
      Status.EraseHidden;
    if not MoveOrResize(CES, MoveOnly, Modified) then
      InsufficientMemory;
    Status.Display;
  end;

  function ProcessMenuItem(Item : Word) : Boolean;
    {-Process the specified menu choice}
  const
    NotOnField = 'Cursor is not on a field';
  var
    TFP : TextFieldPtr;
    EFP : EntryFieldPtr;
    Dummy : Byte;
    SaveModified : Boolean;

    procedure PropagateCommand;
      {-Propagate changes throughout the system}

      function ApplyGlobally : Boolean;
        {-See if user wants changes applied globally}
      var
        Esc : Boolean;
      begin
        ApplyGlobally :=
          PopupYesNo('', 'Apply changes globally?', NoChar, Esc) and not Esc;
      end;

      function ApplyOptionGlobally : Boolean;
        {-See if user wants field option changes applied globally}
      begin
        if CES.HasFields then begin
          MainMenu.UpdateContents;
          ApplyOptionGlobally := ApplyGlobally;
        end
        else
          ApplyOptionGlobally := False;
      end;

      procedure GlobalFieldOption(Mask : LongInt);
        {-Apply a field option change globally if desired}
      begin
        if ApplyOptionGlobally then
          CES.VisitAllEntryFields(ChangeFieldOption, Mask);
      end;

      procedure GlobalSecFieldOption(Mask : LongInt);
        {-Apply a field option change globally if desired}
      begin
        if ApplyOptionGlobally then
          CES.VisitAllEntryFields(ChangeSecFieldOption, Mask);
      end;

    begin
      with CES, wFrame do
        case Item of
          mmTextColor :
            begin
              wTextColor := asColors.TextColor;
              wTextMono := asColors.TextMono;
              if (asTextFields.Head <> nil) and ApplyGlobally then
                ResetTextFieldColor;
              ResetScreen;
            end;
          mmBorderColor :
            begin
              frFrameColor := asColors.FrameColor;
              frFrameMono := asColors.FrameMono;
            end;
          mmHeaderColor :
            begin
              frHeaderColor := asColors.HeaderColor;
              frHeaderMono  := asColors.HeaderMono;
              if (frHeaders.Head <> nil) and ApplyGlobally then
                ResetHeaderColor;
            end;
          mmShadowColor :
            ResetShadowColor;
          mmScrollColor,
          mmSliderColor,
          mmHotColor :
            ResetScrollBarColor;
          mmMouseColor :
            Exit;
          mmPromptColor :
            if ApplyGlobally then
              ResetPromptColor;
          mmSelPromptColor :
            if ApplyGlobally then
              ResetSelectedPromptColor;
          mmProPromptColor :
            if ApplyGlobally then
              ResetProtectedPromptColor;
          mmFieldColor :
            if ApplyGlobally then
              ResetFieldColor;
          mmSelFieldColor :
            if ApplyGlobally then
              ResetSelectedFieldColor;
          mmProFieldColor :
            if ApplyGlobally then
              ResetProtectedFieldColor;
          mmControlColor :
            if ApplyGlobally then
              ResetCtrlColor;
          mmDelimiterColor,
          mmSelDelimColor,
          mmProDelimColor:
            ResetScreen;
          mmScrollSlider :
            if HaveScrollBar(frRR) or HaveScrollBar(frBB) then begin
              if HaveScrollBar(frRR) then
                frBars[frRR]^.sbSliChar := cesSliderChar;
              if HaveScrollBar(frBB) then
                frBars[frBB]^.sbSliChar := cesSliderChar;
            end
            else
              Exit;
          mmScrollBarChar :
            if HaveScrollBar(frRR) or HaveScrollBar(frBB) then begin
              if HaveScrollBar(frRR) then
                frBars[frRR]^.sbBarChar := cesBarChar;
              if HaveScrollBar(frBB) then
                frBars[frBB]^.sbBarChar := cesBarChar;
            end
            else
              Exit;
          mmScrollArrowTop :
            if HaveScrollBar(frRR) then
              frBars[frRR]^.sbDecChar := cesArrows[arrowUp]
            else
              Exit;
          mmScrollArrowBottom :
            if HaveScrollBar(frRR) then
              wFrame.frBars[frRR]^.sbIncChar := cesArrows[arrowDown]
            else
              Exit;
          mmScrollArrowLeft :
            if HaveScrollBar(frBB) then
              frBars[frBB]^.sbDecChar := cesArrows[arrowLeft]
            else
              Exit;
          mmScrollArrowRight :
            if HaveScrollBar(frBB) then
              frBars[frBB]^.sbIncChar := cesArrows[arrowRight]
            else
              Exit;
        {mmFieldOptions}
          mmAutoAdvanceChar :
            GlobalFieldOption(efAutoAdvanceChar);
          mmAutoAdvanceCursor :
            GlobalFieldOption(efAutoAdvanceCursor);
          mmAutoNumLock :
            GlobalFieldOption(efAutoNumLock);
          mmBeepOnError :
            GlobalFieldOption(efBeepOnError);
          mmClearFirstChar :
            GlobalFieldOption(efClearFirstChar);
          mmCursorToEnd :
            GlobalFieldOption(efCursorToEnd);
          mmExitOnClick :
            GlobalFieldOption(efClickExit);
          mmForceCase :
            GlobalFieldOption(efForceUpper+efForceLower);
          mmForceTypingMode :
            GlobalFieldOption(efForceOvertype+efForceMode);
          mmHouseCursor :
            GlobalFieldOption(efHouseCursorAtEnd);
          mmInsertPushes :
            GlobalFieldOption(efInsertPushes);
          mmMapCtrls :
            GlobalFieldOption(efMapCtrls);
          mmPadChar :
            if ApplyOptionGlobally then
              VisitAllEntryFields(ChangePadChar, Dummy);
          mmParensForMinus :
            GlobalFieldOption(efParensForMinus);
          mmPasswordMode :                      {!!.11}
            GlobalFieldOption(efPasswordMode);  {!!.11}
          mmProtected :                         {!!.11}
            GlobalFieldOption(efProtected);     {!!.11}
          mmRequired :
            GlobalFieldOption(efRequired);
          mmRightJustify :
            GlobalFieldOption(efRightJustify);
          mmSuppressZeros :
            GlobalSecFieldOption(sefSuppressZero);
          mmTrimBlanks :
            GlobalFieldOption(efTrimBlanks);
        end;

      case Item of
        mmAutoAdvanceChar..mmTrimBlanks :
          {don't redraw the screen} ;
        else
          RedrawCES(False);
      end;
    end;

    procedure ToggleBoolean(var B : Boolean);
      {-Toggle a boolean variable}
    begin
      B := not B;
      Modified := True;
    end;

    procedure ToggleWindowOption(Flags : LongInt);
      {-Toggle a window option}
    begin
      with CES do
        if LongFlagIsSet(wFlags, Flags) then
          ClearLongFlag(wFlags, Flags)
        else
          SetLongFlag(wFlags, Flags);
      Modified := True;
    end;

    procedure ToggleFieldOption(Flags : LongInt);
      {-Toggle a primary field option}
    begin
      with CES do
        if esFieldOptionsAreOn(Flags) then
          esFieldOptionsOff(Flags)
        else
          esFieldOptionsOn(Flags);
      PropagateCommand;
      Modified := True;
    end;

    procedure ToggleSecFieldOption(Flags : LongInt);
      {-Toggle a secondary field option}
    begin
      with CES do
        if esSecFieldOptionsAreOn(Flags) then
          esSecFieldOptionsOff(Flags)
        else
          esSecFieldOptionsOn(Flags);
      PropagateCommand;
      Modified := True;
    end;

    procedure ChangeColors;
      {-Change a color}

      procedure EditColorPrim(var Color, Mono : Byte);
        {-Edit a color combination and propagate command}
      begin
        if EditColor('', Color, Mono) then {!!.03}
          PropagateCommand;
      end;

    begin
      with CES, asColors do
        case Item of
          mmTextColor      : EditColorPrim(TextColor, TextMono);
          mmBorderColor    : EditColorPrim(FrameColor, FrameMono);
          mmHeaderColor    : EditColorPrim(HeaderColor, HeaderMono);
          mmShadowColor    : EditColorPrim(ShadowColor, ShadowMono);
          mmScrollColor    : EditColorPrim(ScrollBarColor, ScrollBarMono);
          mmSliderColor    : EditColorPrim(SliderColor, SliderMono);
          mmHotColor       : EditColorPrim(HotSpotColor, HotSpotMono);
          mmMouseColor     : EditColorPrim(MouseColor, MouseMono);
          mmPromptColor    : EditColorPrim(PromptColor, PromptMono);
          mmSelPromptColor : EditColorPrim(SelPromptColor, SelPromptMono);
          mmProPromptColor : EditColorPrim(ProPromptColor, ProPromptMono);
          mmFieldColor     : EditColorPrim(FieldColor, FieldMono);
          mmSelFieldColor  : EditColorPrim(SelFieldColor, SelFieldMono);
          mmProFieldColor  : EditColorPrim(ProFieldColor, ProFieldMono);
          mmControlColor   : EditColorPrim(CtrlColor, CtrlMono);
          mmDelimiterColor : EditColorPrim(DelimColor, DelimMono);
          mmSelDelimColor  : EditColorPrim(SelDelimColor, SelDelimMono);
          mmProDelimColor  : EditColorPrim(ProDelimColor, ProDelimMono);
        end;
    end;

    procedure IncScrollByPage;
    var
      SBP, MP : Boolean;
    begin
      with CES do begin
        SBP := esOptionsAreOn(esScrollByPage);
        MP := esOptionsAreOn(esMousePage);
        if SBP and MP then
          esOptionsOff(esScrollByPage+esMousePage)
        else if not(SBP or MP) then
          esOptionsOn(esScrollByPage)
        else if SBP then begin
          esOptionsOff(esScrollByPage);
          esOptionsOn(esMousePage);
        end
        else
          esOptionsOn(esScrollbyPage+esMousePage);
        Modified := True;
      end;
    end;

    procedure IncCase;
    begin
      with CES do
        if esFieldOptionsAreOn(efForceUpper) then begin
          esFieldOptionsOff(efForceUpper);
          esFieldOptionsOn(efForceLower);
        end
        else if esFieldOptionsAreOn(efForceLower) then
          esFieldOptionsOff(efForceLower)
        else
          esFieldOptionsOn(efForceUpper);
      PropagateCommand;
      Modified := True;
    end;

    procedure IncTypingMode;
    begin
      with CES do
       if not esFieldOptionsAreOn(efForceMode) then
         esFieldOptionsOn(efForceMode+efForceOverType)
       else if esFieldOptionsAreOn(efForceOverType) then
         esFieldOptionsOff(efForceOverType)
       else
         esFieldOptionsOff(efForceMode);
       PropagateCommand;
       Modified := True;
    end;

    procedure EditCharPrim(Header : PathStr; var Ch : Char);
    begin                   {!!.03}
      if EditChar(Header, Ch, True) then
        PropagateCommand;
    end;

    function GetColorSet(var CS : ColorSet) : Boolean; {!!.03}
      {-Load a color set into CS}
    const
      TypeCodes = [otLoadableColorSet];
    var
      CSS : ColorSetSelector;
      LCS : LoadableColorSet;
      I : Word;
    begin
      GetColorSet := False;
      if not HaveLib then
        Exit;

      {initialize the color set selector}
      if not CSS.Init(30, 6, 51, 21, Lib, TypeCodes, True) then begin
        InsufficientMemory;
        Exit;
      end;

      {add a shadow}
      CSS.wFrame.AddShadow(shBR, shSeeThru);

      {change the attribute of the column labels}
      with MakeScrnColors do
        CSS.SetLabelAttr(HighItemColor, HighItemMono);

      {make a selection}
      CSS.Process;
      case CSS.GetLastCommand of
        ccQuit, ccError :
          begin
            CSS.Erase;
            CSS.Done;
            Exit;
          end;
      end;

      {is it an existing object or a new one?}
      I := CSS.GetLastObjectChoice;
      if I = 0 then begin
        CS := DefColors;
        GetColorSet := True;
      end
      else with Lib do begin
        {load the color set}
        GetEntry(FindEntryByIndex(I)^.GetEntryName, LCS);
        I := GetStatus;
        if I = 0 then begin
          CS := LCS.lcsColors;
          GetColorSet := True;
        end
        else
          PopupErrorMessage('Internal error '+Long2Str(I)+' loading color set');
      end;

      CSS.Erase;
      CSS.Done;
    end;

    procedure LoadColorSet; {!!.03}
      {-Load a color set from a library}
    var
      Esc : Boolean;
    begin
      with CES, wFrame do
        if GetColorSet(asColors) then begin
          wTextColor := asColors.TextColor;
          wTextMono := asColors.TextMono;
          frFrameColor := asColors.FrameColor;
          frFrameMono := asColors.FrameMono;
          frHeaderColor := asColors.HeaderColor;
          frHeaderMono  := asColors.HeaderMono;
          ResetScreen;
          if PopupYesNo('', 'Apply new colors to existing fields?', YesChar, Esc) and not Esc then
            ResetAllColors;
          RedrawCES(False);
          Modified := True;
        end;
    end;

    procedure SaveColorSet; {!!.03}
      {-Save a color set in a library}
    const
      TypeCodes = [otLoadableColorSet];
      Prompt1 = 'Library''s directory is full. Proceed?';
      Prompt2 = 'Name of color set: ';
      MaxLen = SizeOf(DirEntryName)-1;
    var
      LS : OpLibrarySelector;
      LCS : LoadableColorSet;
      CSName : DirEntryName;
      AllowNew, OK, Esc, Proceed : Boolean;
      I : Word;
    begin
      if not HaveLib then
        Exit;

      {any room left in library's directory for new objects?}
      AllowNew := Lib.AvailableEntries > 0;
      if not AllowNew then begin
        Proceed := PopupYesNo('Warning', Prompt1, YesChar, Esc);
        if Esc or not Proceed then
          Exit;
      end;

      {initialize the color set selector}
      if not LS.Init(30, 6, 51, 21, Lib, TypeCodes, AllowNew) then begin
        InsufficientMemory;
        Exit;
      end;

      {add a shadow}
      LS.wFrame.AddShadow(shBR, shSeeThru);

      {change the attribute of the column labels}
      with MakeScrnColors do
        LS.SetLabelAttr(HighItemColor, HighItemMono);

      {make a selection}
      LS.Process;
      case LS.GetLastCommand of
        ccQuit, ccError :
          begin
            LS.Erase;
            LS.Done;
            Exit;
          end;
      end;

      {initialize loadable color set}
      LCS.Init(CES.asColors);

      {is it an existing object or a new one?}
      I := LS.GetLastObjectChoice;
      if I = 0 then begin
        {prompt for a name}
        CSName := '';
        repeat
          if (not PopupGetString('New object', Prompt2, True, True, MaxLen, CSName)) or
             (CSName = '') then begin
               LS.Erase;
               LS.Done;
               Exit;
             end;

          {check for conflict with existing name}
          OK := Lib.FindAnyDirectoryIndex(CSName) = 0;
          if not OK then
            PopupErrorMessage('Name is already in use');
        until OK;
      end
      else
        CSName := Lib.FindEntryByIndex(I)^.GetEntryName;

      with Lib do begin
        {save the color set}
        PutEntry(CSName, LCS);
        I := GetStatus;
        if I <> 0 then
          PopupErrorMessage('Internal error '+Long2Str(I)+' storing color set')
        else
          if FlushDosBuffers(Handle) then {};
      end;

      LS.Erase;
      LS.Done;
    end;

  begin
    ProcessMenuItem := False;
    case Item of
      mmFieldAdd..mmFieldRemove :
        begin
          SaveModified := Modified;
          Modified := False;
        end;
    end;
    with CES do
      case Item of
        {mmDefaults}
          mmBackdrop :
            ToggleBackdrop;
          mmStatusLine :
            Status.Toggle;
          mmColorConsts :
            GenColorNames := not GenColorNames;
          mmHelpConsts :
            GenHelpNames := not GenHelpNames;
          mmUserHooks :
            GenUserHooks := not GenUserHooks;
          mmStreamCode :
            GenStreamCode := not GenStreamCode;
          mmFieldNames :                         {!!.01}
            GenFieldNames := not GenFieldNames;  {!!.01}
        {mmWindow}
          {mmColors}
            mmTextColor..mmProDelimColor :
              ChangeColors;
            mmLoadColors :                {!!.03}
              LoadColorSet;               {!!.03}
            mmSaveColors :                {!!.03}
              SaveColorSet;               {!!.03}
          {mmEffects}
            mmDelay :
              if PopupGetWord('Stage Delay', 'New value [0-1000]: ',
                              cesStageDelay, 0, 1000) then begin
                Modified := True;
                if IsExploding then begin
                  wStageDelay := cesStageDelay;
                  RedrawCES(True);
                end;
              end;
            mmExplosions :
              begin
                if IsExploding then
                  EnableNormalOpen
                else begin
                  EnableExplosions(cesStageDelay);
                  RedrawCES(True);
                end;
                Modified := True;
              end;
            mmShadows :
              ToggleShadows;
            mmSound :
              begin
                ToggleWindowOption(wSoundEffects);
                if wOptionsAreOn(wSoundEffects) and IsExploding then
                  RedrawCES(True);
              end;
          mmBorder :
            EditFrame;
          {mmHeaders}
            mmHeaderAdd :
              AddHeader;
            mmHeaderEdit :
              begin
                EditHeader;
                if not CES.HasHeaders then
                  MainMenu.SelectItem(mmHeaderAdd);
              end;
            mmHeaderRemove :
              begin
                RemoveHeader;
                if not CES.HasHeaders then
                  MainMenu.SelectItem(mmHeaderAdd);
              end;
          mmMove :
            MoveResizePrim(True);
          mmResize :
            MoveResizePrim(False);
          {mmScrollBars}
            mmScrollBottom :
              ToggleScrollBar(frBB);
            mmScrollRight :
              ToggleScrollBar(frRR);
            mmScrollSlider :
              EditCharPrim('Slider Char', cesSliderChar);
            mmScrollBarChar :
              EditCharPrim('Scroll Bar Char', cesBarChar);
            mmScrollArrowTop :
              EditCharPrim('Arrow Top', cesArrows[arrowUp]);
            mmScrollArrowBottom :
              EditCharPrim('Arrow Bottom', cesArrows[arrowDown]);
            mmScrollArrowLeft :
              EditCharPrim('Arrow Left', cesArrows[arrowLeft]);
            mmScrollArrowRight :
              EditCharPrim('Arrow Right', cesArrows[arrowRight]);
        {mmEntryScreen}
          mmDelimiters :
            if EditDelims then begin
              ForceReset;
              RedrawCES(False);
            end;
          {mmFieldOptions}
            mmAutoAdvanceChar :
              ToggleFieldOption(efAutoAdvanceChar);
            mmAutoAdvanceCursor :
              ToggleFieldOption(efAutoAdvanceCursor);
            mmAutoNumLock :
              ToggleFieldOption(efAutoNumLock);
            mmBeepOnError :
              ToggleFieldOption(efBeepOnError);
            mmClearFirstChar :
              ToggleFieldOption(efClearFirstChar);
            mmCursorToEnd :
              ToggleFieldOption(efCursorToEnd);
            mmExitOnClick :
              ToggleFieldOption(efClickExit);
            mmForceCase :
              IncCase;
            mmForceTypingMode :
              IncTypingMode;
            mmHouseCursor :
              ToggleFieldOption(efHouseCursorAtEnd);
            mmInsertPushes :
              ToggleFieldOption(efInsertPushes);
            mmMapCtrls :
              ToggleFieldOption(efMapCtrls);
            mmPadChar :
              EditCharPrim('Pad Character', esPadChar);
            mmParensForMinus :
              ToggleFieldOption(efParensForMinus);
            mmPasswordMode :                      {!!.11}
              ToggleFieldOption(efPasswordMode);  {!!.11}
            mmProtected :                         {!!.11}
              ToggleFieldOption(efProtected);     {!!.11}
            mmRequired :
              ToggleFieldOption(efRequired);
            mmRightJustify :
              ToggleFieldOption(efRightJustify);
            mmSuppressZeros :
              ToggleSecFieldOption(sefSuppressZero);
            mmTrimBlanks :
              ToggleFieldOption(efTrimBlanks);
          mmMouseSupport :
            ToggleBoolean(cesMouseSupport);
          mmPasswordChar :                             {!!.03}
            if EditChar('Password Char', esPasswordChar, True) then begin
              RedrawCES(False);
              Modified := True;
            end;
          mmScrollByPage :
            IncScrollByPage;
          mmWrapMode :
            begin
              if asWrapMode = ExitAtBot then {!!.01}
                asWrapMode := StopAtEdges
              else
                Inc(asWrapMode);
              Modified := True;
            end;
        {mmFields}
          {mmFieldAdd}
            {mmAddTextOnly}                {!!.03}
              mmAddText :                  {!!.03}
                AddATextField;             {!!.03}
              mmAddHorizLine :             {!!.03}
                AddALineField(False);      {!!.03}
              mmAddVertLine :              {!!.03}
                AddALineField(True);       {!!.03}
              mmAddBox :                   {!!.03}
                AddABoxField(False);       {!!.03}
              mmAddShadowBox :             {!!.03}
                AddABoxField(True);        {!!.03}
            mmAddString..mmAddTime :
              AddDataField(Item);
          mmFieldCopy :                 {!!.03}
            if FindTextFieldAtCursor(TFP, False) then
              CopyTextField(TFP)
            else if FindEntryFieldAtCursor(EFP) then
              CopyEntryField(EFP)
            else
              PopupErrorMessage(NotOnField);
          mmFieldEdit :
            if not EditFieldAtCursor then                   {!!.03}
              PopupErrorMessage(NotOnField);
          mmFieldMove :                 {!!.03}
            if FindTextFieldAtCursor(TFP, False) then
              MoveTextField(TFP)
            else if FindEntryFieldAtCursor(EFP) then
              MoveEntryField(EFP)
            else
              PopupErrorMessage(NotOnField);
          mmFieldRemove :               {!!.03}
            if FindTextFieldAtCursor(TFP, False) then
              RemoveTextField(TFP)
            else if FindEntryFieldAtCursor(EFP) then
              RemoveEntryField(EFP)
            else
              PopupErrorMessage(NotOnField);
        {mmObject}
          mmObjLoad :
            LoadEntryScreen;
          mmSave :
            SaveEntryScreen;
          mmRename :
            RenameObject(False);
          mmSaveAs :
            RenameObject(True);
          mmObjInfo :
            ShowEntryScreenInfo;
          mmTest :
            TestEntryScreen;
          mmGenerateSource :
            GenerateSource;
          mmDocument :
            DocumentEntryScreen;
        {mmLibrary}
          mmLoad :
            LoadLibrary;
          mmNew :
            NewLibrary;
          mmDelete :
            DeleteObjects;
          mmInfo :
            ShowLibraryInfo;
          mmPack :
            PackLibrary;
          mmQuit :
            AllDone := OkToQuit;
      end;
    case Item of
      mmFieldAdd..mmFieldRemove :
        begin
          ProcessMenuItem := Modified;
          Modified := Modified or SaveModified;
        end;
    end;
  end;

  procedure Main;
    {-Main program block}
  begin
    {initialize main menu, enable mouse, clear screen, etc.}
    Initialize;

    with MainMenu do begin
      {did user specify a library on the command line?}
      if (ParamCount <> 0) and OpenLibrary(ParamStr(1)) then
        {see if user wants to load an entry screen}
        LoadEntryScreen
      else begin
        {simulate selection of Load item when first starting}
        DefaultPath(mmLoad);
        MenuCommands.SetCommandList(@FirstCmd, SizeOf(FirstCmd));
      end;

      AllDone := False;
      repeat
        {protect menu items as necessary}
        ProtectMenuItems;

        {get menu choice}
        Process;
        Item := MenuChoice;

        {get last command}
        Cmd := GetLastCommand;

        case Cmd of
          ccSelect :
            if ProcessMenuItem(Item) then
              ScrollAround;
          MoveWindowCmd :
            if HaveCES then
              MoveResizePrim(True);
          ResizeWindowCmd :
            if HaveCES then
              MoveResizePrim(False);
          SaveFileCmd :
            if HaveCES then
              SaveEntryScreen;
          NewFileCmd :
            LoadLibrary;
          TestCmd :
            TestEntryScreen;
          InfoCmd :
            if HaveCES then
              ShowEntryScreenInfo
            else
              ShowLibraryInfo;
          StatusCmd :
            if HaveCES then
              Status.Toggle;
          MainMenuCmd :
            if HaveCES then
              ScrollAround;
          ccQuit :
            if HaveCES then
              ScrollAround
            else
              AllDone := OkToQuit;
          ExitCmd :
            AllDone := OkToQuit;
          ccError :
            AllDone := True;
        end;

      until AllDone;

      {erase the menu}
      EraseMainMenu;
    end;

    {close the library if it is open}
    CloseLibrary;

    {$IFDEF UseMouse}
    HideMouse;
    {$ENDIF}

    {clean up the screen}
    Window(1, 1, ScreenWidth, ScreenHeight);
    TextAttr := 7;
    ClrScr;
    NormalCursor;
  end;

{begin}
end.
