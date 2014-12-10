{$R-,S-,I-,V-,B-,F+,O+}

{$I OPDEFINE.INC}

{***************************************************************************
 This unit requires that OPDEFINE.INC activate the following defines:
   UseScrollBars, UseHotSpots, UseShadows, UseAdjustableWindows
 This program will use features activated with the following defines:
   UseMouse
 ***************************************************************************}
{$IFNDEF UseScrollBars}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}
{$IFNDEF UseHotSpots}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}
{$IFNDEF UseShadows}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}
{$IFNDEF UseAdjustableWindows}
  !! The settings in OPDEFINE.INC are not compatible with this unit.
{$ENDIF}

{*********************************************************}
{*                   MMUTIL.PAS 1.30                     *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit MMUtil;
  {-Miscellaneous utility routines for MAKEMENU}

interface

uses
  Use32,
  dos,                            {directory operations}
  opinline,                       {inline macros}
  opdos,                          {advanced dos operations}
  opconst,                        {!!.20}
  oproot,                         {base objects, error codes, etc.}
  opcmd,                          {command processing}
  opcrt,                          {low-level screen handling}
  {$IFDEF UseMouse}
  opmouse,                        {mouse handling}
  {$ENDIF}
  opstring,                       {string handling}
  opsedit,                        {simple line editor}
  opframe,                        {frames, shadows, etc.}
  opwindow,                       {window management}
  oppick,                         {pick lists}
  opdir;                          {directory lists}

const
  YesChar    = 'Y';
  NoChar     = 'N';

  SliderChar    = '²';
  ScrollBarChar = '°' {'±'};
  BackdropAttr  = $07;
  BackdropChar  = '°' {'±'};

  {colors}
  MainColors : ColorSet = (
    TextColor       : $1A; TextMono       : $07;
    CtrlColor       : $1C; CtrlMono       : $0F;
    FrameColor      : $1A; FrameMono      : $07;
    HeaderColor     : $1F; HeaderMono     : $70;
    ShadowColor     : $08; ShadowMono     : $70;
    HighlightColor  : $1F; HighlightMono  : $0F;
    PromptColor     : $1A; PromptMono     : $07;
    SelPromptColor  : $1A; SelPromptMono  : $07;
    ProPromptColor  : $1A; ProPromptMono  : $07;
    FieldColor      : $1A; FieldMono      : $07;
    SelFieldColor   : $1F; SelFieldMono   : $0F;
    ProFieldColor   : $17; ProFieldMono   : $07;
    ScrollBarColor  : $13; ScrollBarMono  : $07;
    SliderColor     : $13; SliderMono     : $0F;
    HotSpotColor    : $30; HotSpotMono    : $70;
    BlockColor      : $3E; BlockMono      : $0F;
    MarkerColor     : $5F; MarkerMono     : $70;
    DelimColor      : $31; DelimMono      : $0F;
    SelDelimColor   : $31; SelDelimMono   : $0F;
    ProDelimColor   : $31; ProDelimMono   : $0F;
    SelItemColor    : $2F; SelItemMono    : $70;
    ProItemColor    : $17; ProItemMono    : $07;
    HighItemColor   : $1F; HighItemMono   : $0F;
    AltItemColor    : $1F; AltItemMono    : $0F;
    AltSelItemColor : $2F; AltSelItemMono : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono  : $0F;
    FlexBHelpColor  : $1F; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono  : $70;
    UnselXrefColor  : $1E; UnselXrefMono  : $09;
    SelXrefColor    : $5F; SelXrefMono    : $70;
    MouseColor      : $4F; MouseMono      : $70
  );


type
  CursorStateRec =
    record
      CursorXY : Word;              {original cursor position}
      CursorSL : Word;
      {$IFDEF UseMouse}
      MouseX : Byte;                {original mouse position}
      MouseY : Byte;
      MWC : WindowCoordinates;      {saved window coordinates for mouse}
      SaveMouse : Boolean;          {saved state of mouse visibility}
      {$ENDIF}
    end;

  procedure PopupErrorMessage(Msg : string);
    {-Display an error message in a popup window}

  procedure InsufficientMemory;
    {-Display an 'insufficient memory' error message in a popup window}

  procedure PopupMessage(Header, Msg : string);
    {-Display a non-error message in a popup window}

  procedure PopupDelayMessage(Msg : string);
    {-Popup a message and delay for 750 milliseconds}

  function PopupGetString(Header, Prompt : string;
                          ForceUp, TrimBlanks : Boolean;
                          MaxLen : Byte;
                          var S : string) : Boolean;
    {-Prompt for a string within a popup window}

  function GetFileByList(var S : String) : Word;
    {-Let the user choose a filename from a list}

  function PopupGetFileName(Header, Prompt : string; MaxLen : Byte;
                            DefExt : string; var S : string) : Boolean;
    {-Prompt for a filename within a popup window}

  function PopupGetExistingFile(Header, Prompt : string; MaxLen : Byte;
                                DefExt : string; var S : string) : Boolean;
    {-Prompt for name of an existing file within a popup window}

  function PopupGetLong(Header, Prompt : string; var L : LongInt;
                        LLo, LHi : LongInt) : Boolean;
    {-Prompt for a longint within a popup window}

  function PopupGetWord(Header, Prompt : string; var W : Word;
                        WLo, WHi : Word) : Boolean;
    {-Prompt for a word within a popup window}

  function PopupGetByte(Header, Prompt : string; var B : Byte;
                        BLo, BHi : Byte) : Boolean;
    {-Prompt for a byte within a popup window}

  function PopupYesNo(Header, Prompt : string; Default : Char;
                      var Escaped : Boolean) : Boolean;
    {-Prompt for a response to a yes-no question within a popup window}

  function ConfirmAction(Header : string) : Boolean;
    {-Confirm an action (Y/N) within a popup window}

  procedure SaveAllCursors(var State : CursorStateRec);
    {-Save the state of normal and mouse cursor and window. Hides both cursors}

  procedure RestoreAllCursors(var State : CursorStateRec);
    {-Restore cursor state as saved by SaveAllCursors}

const
  SglCrossBar = 'Ä';
  SglLeftTee  = 'Ã';
  SglRightTee = '´';
  DblCrossBar = 'Í';
  DblLeftTee  = 'Æ';
  DblRightTee = 'µ';
  DefCrossBar : Char = SglCrossBar;
  DefLeftTee  : Char = SglLeftTee;
  DefRightTee : Char = SglRightTee;

type
  SpanSelector =
    object(PickList)
      ssHeaders    : DoubleList;
      ssLabelColor : Byte;
      ssLabelMono  : Byte;
      ssVertSpan   : Boolean;

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       var F : Frame;
                       VertSpan : Boolean);
        {-Initialize the span selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc;
                             CommandHandler : pkGenlProc;
                             var F : Frame;
                             VertSpan : Boolean;
                             LeftTee, CrossBar, RightTee : Char);
        {-Initialize the span selector with custom options, colors}
      procedure UpdateContents; virtual;
        {-Redraw the header selector}

      function GetLastHeader : HeaderPtr;
        {-Get last choice}

      procedure SetLabelAttr(Color, Mono : Byte);
        {-Set color for column labels}

      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
    end;

  SpanCharArray = array[1..3] of Char;
  SpanCharSelectorPtr = ^SpanCharSelector;
  SpanCharSelector =
    object(PickList)
      scBoxColor : Byte;
      scBoxMono : Byte;

      constructor Init(X1, Y1, X2, Y2 : Byte; SpanTypes : Word);
        {-Initialize the span char selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc;
                             SpanTypes : Word;
                             LeftTee, CrossBar, RightTee : Char);
        {-Initialize the span char selector with custom options, colors}

      procedure SetInitialSpanChoice(var SCA : SpanCharArray); virtual;
        {-Set initial choice without scrolling if possible}
      procedure GetLastSpanChoice(var SCA : SpanCharArray); virtual;
        {-Get last choice}

      procedure UpdateContents; virtual;
        {-Redraw the span char selector}
      procedure SetBoxAttr(Color, Mono : Byte);
        {-Set color for selection box}
    end;

  VertSpanCharSelectorPtr = ^VertSpanCharSelector;
  VertSpanCharSelector =
    object(SpanCharSelector)
      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the span char selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc;
                             LeftTee, CrossBar, RightTee : Char);
        {-Initialize the span char selector with custom options, colors}

      procedure SetInitialSpanChoice(var SCA : SpanCharArray); virtual;
        {-Set initial choice without scrolling if possible}
      procedure GetLastSpanChoice(var SCA : SpanCharArray); virtual;
        {-Get last choice}

      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
      procedure PreMove; virtual;
        {-Called just prior to getting each keyboard command}
    end;

  HorizSpanCharSelectorPtr = ^HorizSpanCharSelector;
  HorizSpanCharSelector =
    object(SpanCharSelector)
      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the span char selector using default colors and options}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             Orientation : pkGenlProc;
                             LeftTee, CrossBar, RightTee : Char);
        {-Initialize the span char selector with custom options, colors}

      procedure SetInitialSpanChoice(var SCA : SpanCharArray); virtual;
        {-Set initial choice without scrolling if possible}
      procedure GetLastSpanChoice(var SCA : SpanCharArray); virtual;
        {-Get last choice}

      procedure ItemString(Item : Word;
                           Mode : pkMode;
                           var IType : pkItemType;
                           var IString : String); virtual;
        {-Supplies each item string when the list is displayed or searched}
      procedure PreMove; virtual;
        {-Called just prior to getting each keyboard command}
    end;

  {===========================================================}

implementation

const
  MaxHorizSpanTypes = 30;
  HorizSpanTypes : array[1..MaxHorizSpanTypes] of SpanCharArray = (
    'ÃÄ´',               {single}
    'ÌÍ¹',               {double}
    'ÆÍµ',               {mixed #1}
    'ÇÄ¶',               {mixed #2}

    '³Ä³',               {single unjoined}
    'ºÍº',               {double unjoined}
    '³Í³',               {mixed unjoined #1}
    'ºÄº',               {mixed unjoined #2}

    'ÚÄ¿',               {downward single join}
    'ÉÍ»',               {downward double join}
    'ÕÍ¸',               {downward mixed join #1}
    'ÖÄ·',               {downward mixed join #2}

    'ÀÄÙ',               {upward single join}
    'ÈÍ¼',               {upward double join}
    'ÔÍ¾',               {upward mixed join #1}
    'ÓÄ½',               {upward mixed join #2}

    'ÄÄÄ',               {single all horizontal}
    'ÍÍÍ',               {double all horizontal}

    '+-+',               {printable ASCII}

    'ÛÛÛ',               {solid block}
    '°°°',               {polka-dot block #1}
    '±±±',               {polka-dot block #2}
    '²²²',               {polka-dot block #3}

    'þþþ',               {squares}
    '***',               {asterisks}
    ^O^O^O,              {snowflakes}
    ^D^D^D,              {diamonds}
    ^G^G^G,              {dots 1}
    'ùùù',               {dots 2}
    'úúú');              {dots 3}

  MaxVertSpanTypes = 30;
  VertSpanTypes : array[1..MaxVertSpanTypes] of SpanCharArray = (
    'Â³Á',               {single}
    'ËºÊ',               {double}
    'ÒºÐ',               {mixed #1}
    'Ñ³Ï',               {mixed #2}

    'Ä³Ä',               {single unjoined}
    'ÍºÍ',               {double unjoined}
    'ÄºÄ',               {mixed unjoined #1}
    'Í³Í',               {mixed unjoined #2}

    'Ú³À',               {rightward single join}
    'ÉºÈ',               {rightward double join}
    'ÖºÓ',               {rightward mixed #1}
    'Õ³Ô',               {rightward mixed #2}

    '¿³Ù',               {leftward single join}
    '»º¼',               {leftward double join}
    '·º½',               {leftward mixed #1}
    '¸³¾',               {leftward mixed #2}

    '³³³',               {single all vertical}
    'ººº',               {double all vertical}

    '+|+',               {printable ASCII}

    'ÛÛÛ',               {solid block}
    '°°°',               {polka-dot block #1}
    '±±±',               {polka-dot block #2}
    '²²²',               {polka-dot block #3}

    'þþþ',               {squares}
    '***',               {asterisks}
    ^O^O^O,              {snowflakes}
    ^D^D^D,              {diamonds}
    ^G^G^G,              {dots 1}
    'ùùù',               {dots 2}
    'úúú');              {dots 3}

var
  Popup : StackWindow;       {Window used for getting responses}

  function InitPopup(Header : String; Wid : Byte) : Boolean;
    {-Initialize popup window}
  var
    XL, YL, XH : Byte;
  begin
    InitPopup := False;

    if Header <> '' then
      Header := ' '+Header+' ';
    Wid := MaxWord(Wid, Length(Header));

    XL := ((ScreenWidth-Wid) shr 1)+1;
    XH := XL+Pred(Wid);
    YL := ScreenHeight shr 1;

    with Popup do begin
      if not Init(XL, YL, XH, YL) then
        Exit;
      if Header <> '' then begin
        {add the header}
        wFrame.AddHeader(Header, heTC);
        if RawError <> 0 then begin {!!.01}
          Done;
          Exit;
        end;
      end;
      wFrame.AddShadow(shBR, shSeeThru);
      if RawError <> 0 then begin {!!.01}
        Done;                     {!!.01}
        Exit;                     {!!.01}
      end;                        {!!.01}
      Draw;
    end;

    InitPopup := True;
  end;

  procedure ClosePopup;
    {-Close popup window}
  begin
    Popup.Erase;
    Popup.Done;
  end;

  function GetTextAttr : Byte;
    {-Get default text attribute}
  begin
    with MainColors do
      GetTextAttr := ColorMono(TextColor, TextMono);
  end;

  procedure PopupMessagePrim(Header, Msg : string;
                             Bell : Boolean; DelayCount : Word);
    {-Display a message in a popup window}
  const
    PressKeyMsg = '. Press any key...';
  var
    I : Word;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    if DelayCount = 0 then
      if Length(Msg)+Length(PressKeyMsg) <= ScreenWidth-6 then
        Msg := Msg+PressKeyMsg;

    if InitPopup(Header, Length(Msg)+2) then begin
      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      {calculate column offset}
      I := ((Popup.Width-Length(Msg)) shr 1)+1;

      {display the message centered in the window}
      Popup.wFastWrite(Msg, 1, I, GetTextAttr);

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}

      {ring bell if requested}
      if Bell then
        RingBell;

      {move the cursor to the end of the string}
      GotoXY(I+Length(Msg), 1);

      if DelayCount <> 0 then
        {Delay for specified time or until keypressed}
        repeat
          Delay(50);
          if SimpEditCommands.cpKeyPressed then
            DelayCount := 0
          else if DelayCount >= 50 then
            dec(DelayCount, 50)
          else
            DelayCount := 0;
        until DelayCount = 0
      else
        I := SimpEditCommands.cpGetKey;

      {close popup window}
      ClosePopup;
    end else
      RingBell;
  end;

  procedure PopupErrorMessage(Msg : string);
    {-Display an error message in a popup window}
  const
    ErrorHdr    = 'Error';
  begin
    PopupMessagePrim(ErrorHdr, Msg, True, 0);
  end;

  procedure InsufficientMemory;
    {-Display an 'insufficient memory' error message in a popup window}
  begin
    PopupErrorMessage(emInsufficientMemory);
  end;

  procedure PopupMessage(Header, Msg : string);
    {-Display a non-error message in a popup window}
  begin
    PopupMessagePrim(Header, Msg, False, 0);
  end;

  procedure PopupDelayMessage(Msg : string);
    {-Popup a message and delay for 750 milliseconds}
  begin
    PopupMessagePrim('', Msg, False, 750);
  end;

  function PopupGetString(Header, Prompt : string;
                          ForceUp, TrimBlanks : Boolean;
                          MaxLen : Byte;
                          var S : string) : Boolean;
    {-Prompt for a string within a popup window}
  var
    SE : SimpleLineEditor;
    Wid : Word;
  begin
    PopupGetString := False;

    if Prompt <> '' then
      Prompt := ' '+Prompt;
    if Length(Prompt)+MaxLen > (ScreenWidth-5) then
      Wid := (ScreenWidth-5)-Length(Prompt)-1
    else
      Wid := MaxLen;

    if not InitPopup(Header, Length(Prompt)+Wid+1) then begin
      RingBell;
      Exit;
    end;

    with SE do begin
      Init(MainColors);
      seOptionsOn(seWindowRelative);
      seOptionsOff(seMapCtrls);

      if ForceUp then
        seOptionsOn(seForceUpper)
      else
        seOptionsOff(seForceUpper);
      if TrimBlanks then
        seOptionsOn(seTrimBlanks)
      else
        seOptionsOff(seTrimBlanks);

      ReadString(Prompt, 1, 1, MaxLen, Wid, S);

      PopupGetString := (GetLastCommand <> ccQuit);
    end;

    {close popup window}
    ClosePopup;
  end;

  function GetFileByList(var S : String) : Word;
    {-Let the user choose a filename from a list}
  var
    DL : DirList;
    O : Word;
  begin
    GetFileByList := 0;

    with DL do begin
      {initialize the directory list}
      O := DefWindowOptions or wBordered;
      if not InitCustom(15, 12, 66, 23, MainColors, O, 6000,
                        PickSnaking, SingleFile) then begin
        GetFileByList := InitStatus;
        Exit;
      end;

      {add scroll bar}
      wFrame.AddCustomScrollBar(
        frRR, 0, MaxLongInt, 1, 1, SliderChar, ScrollBarChar, MainColors);

      {set basic options}
      diOptionsOn(diOptimizeSize+diExitIfOne);
      pkOptionsOn(pkDrawActive+pkMousePage);

      {set display options}
      SetPadSize(1, 1);

      {set format, sort, search options}
      SetNameFormat;
      SetSortOrder(SortDirName);
      SetSearchMode(PickCharSearch);

      {add header}
      AddMaskHeader(True, 1, 40, heTC);

      {get a filename}
      GetFileByList := GetFileName(S, Directory, S);

      {deallocate}
      Done;
    end;
  end;

  function PopupGetFilePrim(Header, Prompt : string; MaxLen : Byte;
                            DefExt : string; var S : string;
                            MustExist : Boolean) : Boolean;
    {-Prompt for a filename within a popup window}
  var
    Status : Word;
  begin
    PopupGetFilePrim := False;
    if S = '' then
      if DefExt <> '' then
        S := '*.'+DefExt
      else
        S := '*.*';
    if PopupGetString(Header, Prompt, True, True, MaxLen, S) then begin
      if S <> '' then
        S := DefaultExtension(S, DefExt);

      Status := GetFileByList(S);
      case Status mod 10000 of
        0 : PopupGetFilePrim := True;
        ecFileNotFound :
          if MustExist then
            PopupErrorMessage(emFileNotFound)
          else
            PopupGetFilePrim := True;
        ecNoPickSelection : ; {No selection made}
        ecOutOfMemory : InsufficientMemory;
        ecNoMoreFiles : PopupErrorMessage(emNoMoreFiles);
        ecPathNotFound : PopupErrorMessage(emPathNotFound);
      else
        PopupErrorMessage('Error '+Long2Str(Status)+' getting filename');
      end;
    end;
  end;

  function PopupGetFileName(Header, Prompt : string; MaxLen : Byte;
                            DefExt : string; var S : string) : Boolean;
    {-Prompt for a filename within a popup window}
  begin
    PopupGetFileName :=
      PopupGetFilePrim(Header, Prompt, MaxLen, DefExt, S, False);
  end;

  function PopupGetExistingFile(Header, Prompt : string; MaxLen : Byte;
                                DefExt : string; var S : string) : Boolean;
    {-Prompt for name of an existing file within a popup window}
  begin
    PopupGetExistingFile :=
      PopupGetFilePrim(Header, Prompt, MaxLen, DefExt, S, True);
  end;

  function PopupGetLong(Header, Prompt : string; var L : LongInt;
                        LLo, LHi : LongInt) : Boolean;
    {-Prompt for a longint within a popup window}
  var
    Code : Word;
    Finished : Boolean;
    SE : SimpleLineEditor;
    S : String[19];
  begin
    PopupGetLong := False;

    Prompt := ' '+Prompt;
    if not InitPopup(Header, Length(Prompt)+11) then begin
      RingBell;
      Exit;
    end;

    with SE do begin
      Init(MainColors);
      seOptionsOn(seWindowRelative);

      seOptionsOn(seForceUpper);
      seOptionsOn(seTrimBlanks);

      Finished := False;
      repeat
        S := Long2Str(L);
        ReadString(Prompt, 1, 1, 10, 10, S);
        if GetLastCommand = ccQuit then
          Finished := True
        else if not Str2Long(S, L) then begin
          {Invalid number}
          RingBell;
          L := 0;
        end else if (L < LLo) or (L > LHi) then
          RingBell
        else
          Finished := True;
      until Finished;

      PopupGetLong := (GetLastCommand <> ccQuit);
    end;

    {close popup window}
    ClosePopup;
  end;

  function PopupGetWord(Header, Prompt : string; var W : Word;
                        WLo, WHi : Word) : Boolean;
  var
    L : LongInt;
  begin
    L := W;
    PopupGetWord := PopupGetLong(Header, Prompt, L, WLo, WHi);
    W := L;
  end;

  function PopupGetByte(Header, Prompt : string; var B : Byte;
                        BLo, BHi : Byte) : Boolean;
  var
    L : LongInt;
  begin
    L := B;
    PopupGetByte := PopupGetLong(Header, Prompt, L, BLo, BHi);
    B := L;
  end;

  function PopupYesNo(Header, Prompt : string; Default : Char;
                      var Escaped : Boolean) : Boolean;
    {-Prompt for a response to a yes-no question within a popup window}
  var
    KW : Word;
    Finished : Boolean;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    PopupYesNo := False;
    Escaped := True;

    Prompt := ' '+Prompt;

    if not InitPopup(Header, Length(Prompt)+7) then begin
      RingBell;
      Exit;
    end;

    {space for default response and cursor}
    Prompt := Prompt+' ['+Upcase(Default)+'] ';

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {display the message centered in the window}
    Popup.wFastWrite(Prompt, 1, 1, GetTextAttr);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}

    {move the cursor to the end of the string}
    GotoXY(Length(Prompt)+1, 1);

    Escaped := False;
    repeat
      KW := SimpEditCommands.cpGetKey;
      Finished := True;
      case Upcase(Char(Lo(KW))) of
        #13     : PopupYesNo := (UpCase(Default) = YesChar);
        #27     : Escaped := True;
        YesChar : PopupYesNo := True;
        NoChar  : PopupYesNo := False;
        {$IFDEF UseMouse}
        #00     :
          case Hi(KW) of
            $EF : PopupYesNo := (UpCase(Default) = YesChar);
            $EE : Escaped := True;
          else
            Finished := False;
          end;
        {$ENDIF}
      else
        Finished := False;
      end;
    until Finished;

    {close popup window}
    ClosePopup;
  end;

  function ConfirmAction(Header : string) : Boolean;
    {-}
  var
    Esc : Boolean;
  begin
    ConfirmAction :=
      PopupYesNo(Header, 'Are you sure?', 'N', Esc) and not Esc;
  end;

  procedure SaveAllCursors(var State : CursorStateRec);
    {-Save the state of normal and mouse cursor and window. Hides both cursors}
  begin
    with State do begin
      {$IFDEF UseMouse}
      if MouseInstalled then begin
        {get mouse state and hide cursor}
        HideMousePrim(SaveMouse);

        {save mouse cursor position and window coordinates}
        MouseX := MouseWhereX;
        MouseY := MouseWhereY;
        StoreMouseCoordinates(MWC);
      end;
      {$ENDIF}

      {Save the hardware cursor and hide it}
      GetCursorState(CursorXY, CursorSL);
      HiddenCursor;
    end;
  end;

  procedure RestoreAllCursors(var State : CursorStateRec);
    {-Restore cursor state as saved by SaveAllCursors}
  begin
    with State do begin
      {Restore the hardware cursor}
      RestoreCursorState(CursorXY, CursorSL);

      {$IFDEF UseMouse}
      if MouseInstalled then begin
        {Restore the mouse state}
        ShowMousePrim(SaveMouse);
        SetMickeyToPixelRatio(8, 16);
        RestoreMouseCoordinates(MWC);
        MouseGotoXY(MouseX, MouseY);
      end;
      {$ENDIF}
    end;
  end;

  {------------ SpanSelector methods ---------}
  constructor SpanSelector.Init(X1, Y1, X2, Y2 : Byte;
                                var F : Frame;
                                VertSpan : Boolean);
    {-Initialize the header selector using default colors and options}
  begin
    if not SpanSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions, PickVertical,
      SingleChoice, F, VertSpan, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;
  end;

  constructor SpanSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                      var Colors : ColorSet;
                                      Options : LongInt;
                                      Orientation : pkGenlProc;
                                      CommandHandler : pkGenlProc;
                                      var F : Frame;
                                      VertSpan : Boolean;
                                      LeftTee, CrossBar, RightTee : Char);
    {-Initialize the header selector with custom options, colors}
  const
    Wid = 15;    {width of each display line}
  var
    P : HeaderPtr;
    Spans : Word;
    I : Word;
  begin
    {make sure the header list isn't empty}
    if F.frHeaders.Head = nil then
      Fail;

    {get a copy of the header list}
    Move(F.frHeaders, ssHeaders, SizeOf(DoubleList));

    {count number of spans}
    Spans := 0;
    P := HeaderPtr(ssHeaders.Head);
    for I := 1 to ssHeaders.Size do begin
      if P^.heType = heSpan then
        inc(Spans);
      P := HeaderPtr(P^.dlNext);
    end;
    if Spans = 0 then
      Fail;

    {Increase requested width if needed}
    if X2-X1 < Wid-1 then
      X2 := X1+Wid-1;

    {initialize the pick list}
    if not PickList.InitAbstract(X1, Y1+2, X2, Y2, Colors, Options,
                                 Wid, Spans,
                                 Orientation, CommandHandler) then
      Fail;

    {adjust frame coordinates}
    with wFrame do
      AdjustFrameCoords(frXL, frYL-2, frXH, frYH);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {add window divider}
    wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, 2, frTT);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {set attribute for box}
    ssLabelColor := Colors.TextColor;
    ssLabelMono := Colors.TextMono;

    {save which display offset matters}
    ssVertSpan := VertSpan;
  end;

  procedure SpanSelector.SetLabelAttr(Color, Mono : Byte);
    {-Set color for column labels}
  begin
    ssLabelColor := Color;
    ssLabelMono := MapMono(Color, Mono);
  end;

  function SpanSelector.GetLastHeader : HeaderPtr;
    {-Get last choice}
  var
    P : HeaderPtr;
    I, J : Word;
  begin
    J := GetLastChoice;
    I := 0;
    P := HeaderPtr(ssHeaders.Head);
    repeat
      if P^.heType = heSpan then
        inc(I);
      if I = J then begin
        GetLastHeader := P;
        Exit;
      end;
      P := HeaderPtr(P^.dlNext);
    until P = nil;
    GetLastHeader := nil;
  end;

  procedure SpanSelector.UpdateContents;
    {-Redraw the header selector}
  const       {123456789012345}
    HeaderR = '  Row   Chars  ';
    HeaderC = '  Col   Chars  ';
  var
    Wid : Word;
    S : string;
    SLen : Byte absolute S;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw the column headers}
    Wid := wXH-wXL+1;
    if ssVertSpan then
      S := Pad(HeaderC, Wid)
    else
      S := Pad(HeaderR, Wid);
    if SLen > Wid then
      SLen := Wid;
    FastWrite(S, wYL-2, wXL, ColorMono(ssLabelColor, ssLabelMono));

    {update the pick list}
    PickList.UpdateContents;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure SpanSelector.ItemString(Item : Word;
                                    Mode : pkMode;
                                    var IType : pkItemType;
                                    var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  var
    P : HeaderPtr;
    I : Word;
    DOfs : Byte;
    DOfsSt : String[3];
  begin
    I := 0;
    P := HeaderPtr(ssHeaders.Head);
    repeat
      with P^ do begin
        if heType = heSpan then
          inc(I);
        if I = Item then begin
          if ssVertSpan then
            DOfs := heDX
          else
            DOfs := heDY;
          IString := CharStr(' ', Width);
          Str(DOfs:2, DOfsSt);
          move(DofsSt[1], IString[4], 2);
          if Length(heName^) < 3 then
            IString[11] := heName^[1]
          else begin
            IString[09] := heName^[1];
            IString[11] := heName^[2];
            IString[13] := heName^[Length(heName^)];
          end;
          Exit;
        end;
      end;
      P := HeaderPtr(P^.dlNext);
    until P = nil;
    IString := '';
  end;

  {------------------------------------------------------------}

  constructor SpanCharSelector.Init(X1, Y1, X2, Y2 : Byte; SpanTypes : Word);
    {-Initialize the span char selector using default colors and options}
  begin
    if not SpanCharSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, SpanTypes, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;
  end;

  constructor SpanCharSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                          var Colors : ColorSet;
                                          Options : LongInt;
                                          Orientation : pkGenlProc;
                                          SpanTypes : Word;
                                          LeftTee, CrossBar, RightTee : Char);
    {-Initialize the span char selector with custom options, colors}
  begin
    {make sure there's a frame and that the window isn't resized}
    ClearLongFlag(Options, wResizeable);
    SetLongFlag(Options, wBordered+wClear);

    {initialize the pick list}
    if not PickList.InitAbstract(
      X1, Y1+4, X2, Y2, Colors, Options, 7, SpanTypes,
      Orientation, SingleChoice) then
        Fail;

    {adjust frame coordinates}
    with wFrame do
      AdjustFrameCoords(frXL, frYL-4, frXH, frYH);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {add window divider}
    wFrame.AddSpanHeader(LeftTee, CrossBar, RightTee, 4, frTT);
    if RawError <> 0 then begin {!!.01}
      InitStatus := RawError;   {!!.01}
      Done;
      Fail;
    end;

    {set attribute for box}
    scBoxColor := Colors.FrameColor;
    scBoxMono := Colors.FrameMono;
  end;

  procedure SpanCharSelector.SetInitialSpanChoice(var SCA : SpanCharArray);
    {-Set initial choice without scrolling if possible}
  begin
    Abstract;
  end;

  procedure SpanCharSelector.GetLastSpanChoice(var SCA : SpanCharArray);
    {-Get last choice}
  begin
    Abstract;
  end;

  procedure SpanCharSelector.UpdateContents;
    {-Redraw the span char selector}
  {$IFDEF UseMouse}
  var
    SaveMouse : Boolean;
  {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {clear the top part of the window}
    OpCrt.ClearWindow(wXL, wYL-4, wXH, wYL-2, wBackChar,
      ColorMono(wTextColor, wTextMono));

    {update the pick list}
    PickList.UpdateContents;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure SpanCharSelector.SetBoxAttr(Color, Mono : Byte);
    {-Set color for selection box}
  begin
    scBoxColor := Color;
    scBoxMono := MapMono(Color, Mono);
  end;

  {------------------------------------------------------------}

  constructor VertSpanCharSelector.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize the span char selector using default colors and options}
  begin
    if not VertSpanCharSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;
  end;

  constructor VertSpanCharSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                              var Colors : ColorSet;
                                              Options : LongInt;
                                              Orientation : pkGenlProc;
                                              LeftTee, CrossBar, RightTee : Char);
    {-Initialize the span char selector with custom options, colors}
  begin
    if not SpanCharSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, MaxVertSpanTypes, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;
  end;

  procedure VertSpanCharSelector.SetInitialSpanChoice(var SCA : SpanCharArray);
    {-Set initial choice without scrolling if possible}
  var
    I : Word;
  begin
    for I := 1 to MaxVertSpanTypes do
      if SCA = VertSpanTypes[I] then begin
        SetInitialChoice(I);
        Exit;
      end;
    {no luck--set to first choice}
    SetInitialChoice(1);
  end;

  procedure VertSpanCharSelector.GetLastSpanChoice(var SCA : SpanCharArray);
    {-Get last choice}
  var
    I : Word;
  begin
    I := GetLastChoice;
    if I > MaxVertSpanTypes then
      I := 1;
    SCA := VertSpanTypes[I];
  end;

  procedure VertSpanCharSelector.ItemString(Item : Word;
                                            Mode : pkMode;
                                            var IType : pkItemType;
                                            var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  begin
    IString := '  '+VertSpanTypes[Item]+'  ';
    if pkOptionsAreOn(pkSetDefault) then
      if Item = GetDefaultChoice then
        IType := pkAlternate;
  end;

  procedure VertSpanCharSelector.PreMove;
    {-Called just prior to getting each keyboard command}
  var
    Attr : Byte;
    Row : Byte;
    Col : Byte;
    SCA : SpanCharArray;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {calculate coordinates for span}
    Col := (wXH+wXL) shr 1;
    Row := wYL-4;

    {get attribute to use}
    Attr := ColorMono(scBoxColor, scBoxMono);

    {get span to draw}
    GetLastSpanChoice(SCA);

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw the span}
    FastWrite(SCA[1], Row, Col, Attr);
    FastWrite(SCA[2], Row+1, Col, Attr);
    FastWrite(SCA[3], Row+2, Col, Attr);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  {------------------------------------------------------------}

  constructor HorizSpanCharSelector.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize the span char selector using default colors and options}
  begin
    if not HorizSpanCharSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;
  end;

  constructor HorizSpanCharSelector.InitCustom(X1, Y1, X2, Y2 : Byte;
                                              var Colors : ColorSet;
                                              Options : LongInt;
                                              Orientation : pkGenlProc;
                                              LeftTee, CrossBar, RightTee : Char);
    {-Initialize the span char selector with custom options, colors}
  begin
    if not SpanCharSelector.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions,
      PickVertical, MaxHorizSpanTypes, DefLeftTee, DefCrossBar, DefRightTee) then
        Fail;
  end;

  procedure HorizSpanCharSelector.SetInitialSpanChoice(var SCA : SpanCharArray);
    {-Set initial choice without scrolling if possible}
  var
    I : Word;
  begin
    for I := 1 to MaxHorizSpanTypes do
      if SCA = HorizSpanTypes[I] then begin
        SetInitialChoice(I);
        Exit;
      end;
    {no luck--set to first choice}
    SetInitialChoice(1);
  end;

  procedure HorizSpanCharSelector.GetLastSpanChoice(var SCA : SpanCharArray);
    {-Get last choice}
  var
    I : Word;
  begin
    I := GetLastChoice;
    if I > MaxHorizSpanTypes then
      I := 1;
    SCA := HorizSpanTypes[I];
  end;

  procedure HorizSpanCharSelector.ItemString(Item : Word;
                                             Mode : pkMode;
                                             var IType : pkItemType;
                                             var IString : String);
    {-Supplies each item string when the list is displayed or searched}
  begin
    IString := '  '+HorizSpanTypes[Item]+'  ';
    if pkOptionsAreOn(pkSetDefault) then
      if Item = GetDefaultChoice then
        IType := pkAlternate;
  end;

  procedure HorizSpanCharSelector.PreMove;
    {-Called just prior to getting each keyboard command}
  var
    Attr : Byte;
    Row : Byte;
    Col : Byte;
    SCA : SpanCharArray;
    St : String[5];
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {calculate coordinates for span}
    Col := ((wXH+wXL) shr 1)-2;
    Row := wYL-3;

    {get attribute to use}
    Attr := ColorMono(scBoxColor, scBoxMono);

    {get span to draw}
    GetLastSpanChoice(SCA);

    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {draw the span}
    St := CharStr(SCA[2], 5);
    St[1] := SCA[1];
    St[5] := SCA[3];
    FastWrite(St, Row, Col, Attr);

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
