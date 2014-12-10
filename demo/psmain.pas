{$S-,R-,V-,I-,B-,F-}

{*********************************************************}
{*                   PSMAIN.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1987,1992.      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program will use features activated with the following defines:
   ThwartSideKick
 This program is not designed to work with the following define:
   UseDrag
 ***************************************************************************}

{$IFDEF UseDrag} {!!.10}
  !! The settings in OPDEFINE.INC are not compatible with this program.
{$ENDIF}

unit PSMain;
  {-Utility for saving and displaying packed windows}

interface

uses
  Use32,
  Dos,
  OpInline,
  OpConst,  {!!.20}
  OpRoot,
  OpDos,
  OpCrt,
  OpString,
  OpCmd,
  OpFrame,
  OpWindow,
  OpSEdit,
  OpSwap1;

  procedure InitializePScreen;
    {-Initialize PScreen}

  {==========================================================================}

implementation

type
  String64       = String[64];
  VideoWord =
    record
      Ch             : Char;
      Attr           : Byte;
    end;
  ScreenType     = array[1..50, 1..80] of VideoWord; {50 rows * 80 columns}
const
  Version        = '1.30';
  ModuleName     : String[7] = 'PSCREEN'; {module name for standard interface}
  OurHotKey      : Word = $0A19;  {Alt + LeftShift, 'P'}
  Header         : String[14] = ' PSCREEN '+Version+' ';
  ProgName       : String[64] = 'PSCREEN '+Version+': A Utility for Saving and Displaying Packed Screens';
  Copyright      : String[41] = 'Copyright (c) 1992 by TurboPower Software';
  LoadError      : String[25] = 'Unable to install PSCREEN';
  SwapPathName   : String[64] = 'C:\';
  SwapName1      : String[12] = 'PSCREEN1.$$$';
  SwapName2      : String[12] = 'PSCREEN2.$$$';
  SwappingOn     : Boolean = True;
  Disable        : Boolean = False;
  WaitForKey     : Boolean = True;
  DefExt         : String[3] = 'PWF';
  MaxCols        = 80;

  {colors}
  BrightColor  = $1F;
  DimColor     = $1B;
  ReverseColor = $21;
  BorderColor  = $1A;
  BrightMono   = $0F;
  DimMono      = $07;
  ReverseMono  = $70;
  BorderMono   = $0F;

  PScreenColors : ColorSet = (
    TextColor       : $1B; TextMono       : $1B;
    CtrlColor       : $1B; CtrlMono       : $1B;
    FrameColor      : $1A; FrameMono      : $1A;
    HeaderColor     : $21; HeaderMono     : $70;
    ShadowColor     : $00; ShadowMono     : $00;
    HighlightColor  : $00; HighlightMono  : $00;
    PromptColor     : $1F; PromptMono     : $0F;
    SelPromptColor  : $1F; SelPromptMono  : $0F;
    ProPromptColor  : $1F; ProPromptMono  : $0F;
    FieldColor      : $1B; FieldMono      : $07;
    SelFieldColor   : $1B; SelFieldMono   : $07;
    ProFieldColor   : $1B; ProFieldMono   : $07
  );

var
  PW             : PackedWindow;
  MainBufPtr     : Pointer;
  Bright,                         {video attributes}
  Dim,
  Border,
  Reverse        : Byte;
  MaxRows        : Word;
  MaxParas       : Word;          {maximum space needed for saving the screen}
  TempRow        : array[1..MaxCols] of VideoWord;

  function TwiddleColor(B : Byte) : Byte;
    {-Twiddle a color attribute}
  inline(
    $58/                          {POP     AX}
    $34/$11);                     {XOR     AL,$11  ;twiddle the back/fore colors}

  function TwiddleMono(B : Byte) : Byte;
    {-Twiddle a monochrome attribute}
  inline(
    $58/                          {POP     AX}
    $24/$7F/                      {AND     AL,$7F  ;clear high bit}
    $3C/$70/                      {CMP     AL,$70  ;already reverse video?}
    $74/$08/                      {JE      FAMono}
    $3C/$78/                      {CMP     AL,$78}
    $74/$04/                      {JE      FAMono}
    $B0/$70/                      {MOV     AL,$70  ;set reverse video}
    $EB/$02/                      {JMP     SHORT FAexit}
    {                              FAMono:}
    $B0/$0F);                     {MOV     AL,$0F  ;select white on black}
    {                              FAexit:}

  procedure SetAttributes;
    {-Set video attribute variables based on the current video mode}
  begin
    {set video attributes}
    Bright := ColorMono(BrightColor, BrightMono);
    Dim := ColorMono(DimColor, DimMono);
    Reverse := ColorMono(ReverseColor, ReverseMono);
    Border := ColorMono(BorderColor, BorderMono);
    TextAttr := Dim;
  end;

  {$F+}
  function GetKey : Word;
    {-Routine to return next keystroke}
  var
    ChWord         : Word;
  begin
    ChWord := ReadKeyWord;
    {check for Alt-U}
    if ChWord = $1600 then begin
      {translate to ESC and set flag to disable the TSR}
      ChWord := $001B;
      Disable := True;
    end;
    GetKey := ChWord;
  end;
  {$F-}

  function GetFileName(var FName : String64) : Boolean;
    {-Prompt for a file name}
  const
    Prompt = 'File to write [.PWF]: ';
  var
    SLE : SimpleLineEditor;
  begin
    with SLE do begin
      Init(PScreenColors);
      seOptionsOn(seForceUpper);
      ReadString(Prompt, 2, 3, 64, 76-Length(Prompt), FName);
      if (GetLastCommand <> ccQuit) and (Length(FName) <> 0) then begin
        GetFileName := True;
        FName := DefaultExtension(FName, DefExt);
      end
      else
        GetFileName := False;
    end;
  end;

  procedure ErrorMessage(Msg : String);
    {-Display an error message and wait for a keypress}
  const
    PressAnyKey    = '. Press any key...';
  begin
    if Length(Msg)+Length(PressAnyKey)+4 <= ScreenWidth then
      Msg := Msg+PressAnyKey;
    FastWrite(Pad(Msg, ScreenWidth-4), 2, 3, Bright);
    if ReadKeyWord = 0 then {} ;
  end;

  {$F+}
  procedure PopupEntryPoint;
    {-This is the entry point for the popup}
  const
    FName          : String64 = '';
  var
    ScreenPtr      : ^ScreenType;
    ScreenBufPtr   : ^ScreenType absolute MainBufPtr;
    SaveXY, SaveSL : Word;        {for storing cursor position and shape}
    CurRow, CurCol,               {current cursor coordinates}
    StartRow, StartCol,           {start of marked block}
    ChWord         : Word;
    Ch             : Char absolute ChWord;
    Highlight,                    {true if initial point has been marked}
    WinSelected    : Boolean;     {true after window was selected}
    NewRow         : Word;

    procedure MarkBlock(TopRow, BotRow, LeftCol, RightCol : Byte);
      {-Mark the specified block}
    var
      Row, Col, Cols : Word;
    begin
      Cols := Succ(RightCol-LeftCol);
      for Row := TopRow to BotRow do begin
        {copy row into temporary row}
        Move(ScreenBufPtr^[Row, LeftCol], TempRow, Cols*2);

        {twiddle attributes}
        for Col := 1 to Cols do
          with TempRow[Col] do
            if not UseColor then
              Attr := TwiddleMono(Attr)
            else
              Attr := TwiddleColor(Attr);

        {move modified row to screen}
        MoveScreen(TempRow, ScreenPtr^[Row, LeftCol], Cols);
      end;
    end;

    procedure RestoreBlock(TopRow, BotRow, LeftCol, RightCol : Byte);
      {-Unmark the specified block}
    var
      Row, Cols      : Word;
    begin
      Cols := Succ(RightCol-LeftCol);
      for Row := TopRow to BotRow do
        MoveScreen(ScreenBufPtr^[Row, LeftCol], ScreenPtr^[Row, LeftCol], Cols);
    end;

    procedure IncRow(N : Word);
      {-Move the cursor N rows down}
    var
      I              : Word;
    begin
      for I := 1 to N do begin
        {make sure we don't go too far down}
        if CurRow = ScreenHeight then
          Exit;

        Inc(CurRow);
        if Highlight then
          if (CurRow > StartRow) and (CurCol >= StartCol) then
            MarkBlock(Pred(CurRow), CurRow, StartCol, CurCol);
      end;
    end;

    procedure DecRow(N : Integer);
      {-Move the cursor N rows up}
    var
      OldRow, I      : Word;
    begin
      for I := 1 to N do begin
        {make sure we don't go too far up}
        if CurRow = 1 then
          Exit;

        OldRow := CurRow;
        Dec(CurRow);
        if Highlight then
          if CurRow < StartRow then begin
            CurRow := StartRow;
            Exit;
          end
          else
            RestoreBlock(OldRow, OldRow, StartCol, CurCol);
      end;
    end;

    procedure IncCol(N : Word);
      {-Move the cursor N columns to the right}
    var
      I              : Word;
    begin
      for I := 1 to N do begin
        {make sure we don't go too far right}
        if CurCol = ScreenWidth then
          Exit;

        Inc(CurCol);
        if Highlight then
          if (CurCol > StartCol) {and (CurCol >= StartCol)} then {!!.14}
            MarkBlock(StartRow, CurRow, Pred(CurCol), CurCol);
      end;
    end;

    procedure DecCol(N : Word);
      {-Move the cursor N columns to the left}
    var
      OldCol, I      : Word;
    begin
      for I := 1 to N do begin
        {make sure we don't go too far left}
        if CurCol = 1 then
          Exit;

        OldCol := CurCol;
        Dec(CurCol);
        if Highlight then
          if CurCol < StartCol then begin
            CurCol := StartCol;
            Exit;
          end
          else
            RestoreBlock(StartRow, CurRow, OldCol, OldCol);
      end;
    end;

    procedure TabRight;
      {-Moves the cursor to the next tab stop}
    var
      NewCol         : Word;
    begin
      if CurCol < ScreenWidth then begin
        NewCol := Succ(Succ(Pred(CurCol) shr 3) shl 3); {shr 3 = div 8}
        IncCol(NewCol-CurCol);
      end;
    end;

    procedure TabLeft;
      {-Moves the cursor back to the last tab stop}
    var
      NewCol         : Word;
    begin
      NewCol := CurCol;
      if (Pred(NewCol) and 7) = 0 then
        if NewCol > 8 then
          Dec(NewCol, 8)
        else
          NewCol := 1
      else
        NewCol := Succ(Pred(NewCol) and $F8);
      DecCol(CurCol-NewCol);
    end;

    procedure DrawOurWindow;
      {-Draw our window}
    begin
      Window(1, 1, ScreenWidth, 3);
      ClrScr;
      FrameWindow(1, 1, ScreenWidth, 3, Border, Reverse, Header);
    end;

    procedure SaveWholeScreen;
      {-Save the whole screen}
    begin
      {save the screen}
      if SaveWindow(1, 1, ScreenWidth, ScreenHeight, True, MainBufPtr) then
        {won't fail} ;
      ScreenPtr := Ptr(VideoSegment, 0);
    end;

    procedure RestoreWholeScreen;
      {-Restore the whole screen}
    begin
      RestoreWindow(1, 1, ScreenWidth, ScreenHeight, True, MainBufPtr);
    end;

  begin
    {re-initialize CRT}
    ReInitCrt;

    if InTextMode and (ScreenWidth = MaxCols) and (ScreenHeight <= MaxRows) then begin
      {initialize screen stuff}
      SetAttributes;
      GetCursorState(SaveXY, SaveSL);

      {save the screen}
      SaveWholeScreen;

      WinSelected := False;       {Window is not selected now}
      Highlight := False;
      CurCol := WherexAbs;        {Get cursor pos to start with}
      CurRow := WhereyAbs;
      BlockCursor;

      repeat
        {Move to position}
        GotoxyAbs(CurCol, CurRow);
        ChWord := GetKey;
        if Ch = #0 then
          case Hi(ChWord) of
            30 :                  {AltA}
              begin
                StartCol := 1;
                CurCol := ScreenWidth;
                StartRow := 1;
                CurRow := ScreenHeight;
                Highlight := True;
                MarkBlock(StartRow, CurRow, StartCol, CurCol);
                WinSelected := True;
              end;
            72 :                  {Up}
              DecRow(1);
            80 :                  {Down}
              IncRow(1);
            75 :                  {Left}
              DecCol(1);
            77 :                  {Right}
              IncCol(1);
            115,                  {^Left}
            15 :                  {Shift-Tab}
              TabLeft;
            116 :                 {^Right}
              TabRight;
            119,                  {^Home}
            132 :                 {^PgUp}
              DecRow(Pred(ScreenHeight));
            117,                  {^End}
            118 :                 {^PgDn}
              IncRow(Pred(ScreenHeight));
            73 :                  {PgUp}
              begin
                NewRow := CurRow;
                if (CurRow mod 5) = 0 then
                  Dec(NewRow, 5)
                else
                  Dec(NewRow, CurRow mod 5);
                DecRow(CurRow-NewRow);
              end;
            81 :                  {PgDn}
              begin
                NewRow := Succ(CurRow div 5)*5;
                IncRow(NewRow-CurRow);
              end;
            71 :                  {Home}
              DecCol(ScreenWidth);
            79 :                  {End}
              IncCol(ScreenWidth);
          end
        else
          case Ch of
            ^H :                  {BkSp}
              DecCol(1);
            ' ' :                 {space}
              IncCol(1);
            ^I :                  {Tab}
              TabRight;
            #27 :                 {Esc}
              begin
                Highlight := False;
                WinSelected := True;
              end;
            ^M :                  {Enter}
              if not Highlight then begin
                {save starting point}
                StartCol := CurCol;
                StartRow := CurRow;
                Highlight := True;
              end
              else
                WinSelected := True;
          end;
      until WinSelected;

      if Highlight then
        {draw our window}
        DrawOurWindow;

      {get name of file to save screen in}
      if Highlight and GetFileName(FName) then begin
        {restore the screen}
        RestoreWholeScreen;

        {save the packed window}
        if PW.Init(StartCol, StartRow, CurCol, CurRow) then begin
          {try to write the packed window to disk}
          if PW.Write(FName) <> 0 then begin
            PW.Done;
            SaveWholeScreen;
            DrawOurWindow;
            ErrorMessage('Error writing packed window to disk');
            RestoreWholeScreen;
          end
          else
            {dispose of the packed window}
            PW.Done;
        end;
      end
      else begin
        {restore the screen}
        RestoreWholeScreen;

        {try to disable TSR if requested}
        if Disable then
          if not DisableTSR then begin
            Disable := False;
            RingBell;
          end;
      end;

      {restore cursor state}
      RestoreCursorState(SaveXY, SaveSL);
    end
    else
      RingBell;
  end;
  {$F-}

  procedure DisplayFile(FileName : String);
  var
    FName          : String64;
  begin
    {get the filename and display it}
    FName := DefaultExtension(FileName, DefExt);

    HiddenCursor; {!!.12}

    if PW.Read(FName) then
      PW.Display
    else
      WriteLn('Error reading ', FName);

    if WaitForKey then
      if ReadKeyWord = 0 then ;

    NormalCursor; {!!.12}

    Halt;
  end;

  procedure Abort(Msg : String);
    {-Display an error message and halt}
  begin
    WriteLn(Msg);
    Halt(1);
  end;

  procedure Warning(Message : String);
    {-Display warning message, wait for keypress, if key is ESC, then Abort}
  var
    C : Char;
  begin
    WriteLn('Warning: ', Message);
    WriteLn;
    Write('Press ESC to abort, any other key to continue...');
    C := ReadKey;
    Write(^M);
    ClrEOL;
    if C = ^[ then
      Halt;
  end;

  procedure DisableYourself;
    {-Unload resident copy of PSCREEN (if possible) and report results}
  var
    IFC : IfcPtr;
    Save : Boolean;
  begin
    RestoreAllVectors;
    IFC := ModulePtrByName(ModuleName);   {get the IFCPtr for this module}
    if IFC <> nil then begin              {make sure it is already installed}
      Save := IFC^.CSDataPtr^.SwapMsgOn;  {save state of swap messages}
      IFC^.CSDataPtr^.SwapMsgOn := False; {disable swap messages}
      IFC^.CmdEntryPtr;                   {call the CmdEntryPtr}
      if LongInt(IFC^.UserData) = 1 then  {check status of Unload attempt}
        WriteLn('PSCREEN unloaded')
      else
        WriteLn('Unable to unload PSCREEN');
      IFC^.CSDataPtr^.SwapMsgOn := Save;  {restore state of swap messages}
    end
    else
      WriteLn('PSCREEN not installed.');
    Halt;
  end;

  procedure ShowHelp;
    {-Displays help message with PSCREEN options}
  begin
    WriteLn(^M^J'Usage: PSCREEN [Options]'^M^J);
    WriteLn('Options are:');
    WriteLn('  /U          Unload PSCREEN from memory');
    WriteLn('  /N          No swapping');
    WriteLn('  /E          don''t use EMS');
    WriteLn('  /M          squelch swapping Messages');
    WriteLn('  /F          do not make swap files as hidden files');
    WriteLn('  /Ppathname  specify Pathname to use for swapping');
{$IFDEF SupportXms}  {!!.02}
    WriteLn('  /X          use XMS memory if available');
    WriteLn('  /1          single swap file (for RAM disks or XMS)');
{$ELSE}
    WriteLn('  /1          single swap file (for RAM disks)');
{$ENDIF}
    WriteLn('  /?          display this help screen');
    WriteLn(
    '  FileName    display the indicated packed window (non-resident mode)');
    Halt(0);
  end;

  procedure Initialize;
    {-Initialize and check for command line parameters}
  var
    I              : Word;
    Opt            : String;

    procedure InvalidOption;
      {-displays error message and aborts}
    begin
      WriteLn(Opt+' is an invalid option');
      ShowHelp;
    end;

  begin
    {initialize}
    SimpEditCommands.SetGetKeyProc(GetKey);

    {resident mode if no parameters specified}
    if ParamCount = 0 then
      Exit;

    for I := 1 to ParamCount do begin
      Opt := ParamStr(I);
      if (Opt[1] in ['/','-']) and (Length(Opt) >= 2) then begin
        case UpCase(Opt[2]) of
          'U' : DisableYourself;
          'E' : SwapUseEMS := False;
          'M' : SetSwapMsgOn(False);
          'F' : SetSwapFileAttr(False);
          '1' : SetSingleSwapFile(True);
          'Q' : begin
                  SetQuickFixMode(True);
                  WriteLn('In QuickFix mode');
                end;
          'N' : SwappingOn := False;
          'P' : begin
                  SwapPathName := StUpcase(Copy(Opt,3,Length(Opt)));
                  if SwapPathName[Length(SwapPathName)] <> '\' then
                    SwapPathName := SwapPathName + '\';
                end;
       {$IFDEF SupportXms}  {!!.02}
          'X' : begin
                  SwapUseXms := True;
                  EmsOverXms := False;
                end;
       {$ENDIF}

          '?' : ShowHelp;
          else InvalidOption;
        end;
      end
      else
        DisplayFile(Opt);
    end;
  end;

  function DriveIsFixed(Drive : Char) : Boolean;
    {-Return true if drive is not removable}
  var
    SubDrive : Char;
  begin
    case GetDiskClass(Drive,SubDrive) of
      Floppy360,Floppy720,
      Floppy12,Floppy144,
      OtherFloppy       : DriveIsFixed := False;
      else DriveIsFixed := True;
    end;
  end;

  function PathIsValidFixedDisk(Path : String) : Boolean;
    {-Return true if drive specified by Path is a valid fixed disk}
  var
    Drive : Char;
    F : File;
    E : Integer;
  begin
    Assign(F,SwapPathName+SwapName1);
    Reset(F, 1);                        {!!.13}
    E := IoResult;                      {!!.13}
    if E <> 0 then begin                {!!.13}
      Rewrite(F,1);
      E := IoResult;
    end;                                {!!.13}
    case E of
      0 : begin
            Close(F);
            if IoResult <> 0 then
              Abort('Error closing swap file');
          end;
      5 : begin
            {swap files exists, but access denied, probably still
             hidden/system file. Not an error, swap file will be deleted
             by swap system}
          end;
      else
        Abort('Cannot create swap file. Invalid path or drive not ready');
    end;
    if Path[2] = ':' then
      Drive := UpCase(Path[1])
    else
      Drive := DefaultDrive;
    PathIsValidFixedDisk := DriveIsFixed(Drive);
  end;

  procedure DeleteSwapFile(Name : String);             {!!.03}
    {-Deletes the swap file if it already exists}
  var
    F : File;
  begin
    Assign(F, Name);
    {first insure it has a file attribute of 0}
    SetFAttr(F, 0);
    {if we fail, then no such file exists}
    if DosError = 0 then begin
      Erase(F);
      if IoResult <> 0 then ;
    end;
  end;

  procedure InitializePScreen;
    {-Initialize PScreen}
  var
    SwapToEms, SwapToXms : Boolean;   {!!.02}
  begin
    {see if there is a file to display}
    Initialize;

    {determine whether to use special $FF msg row}
    case CurrentDisplay of
      MCGA,EGA,VGA : SetSwapMsgRow($FF);
    end;

    {calculate amount of heap space to set aside}
    case EnhancedDisplay of
      EGA : MaxRows := 43;
      VGA : MaxRows := 50;
      else  MaxRows := 25;
    end;

    MaxParas := (MaxRows*MaxCols*2)+SizeOf(PackedScreen);
    MaxParas := (MaxParas+$F) div 16;

    SwapToEms := WillSwapUseEms(ParagraphsToKeep+MaxParas);
{$IFDEF SupportXms}  {!!.02}
    SwapToXms := WillSwapUseXms(ParagraphsToKeep+MaxParas);
{$ELSE}
    SwapToXms := False;
{$ENDIF}

    {signon message}
    HighVideo;
    WriteLn(^M^J, ProgName, ^M^J, Copyright, ^M^J);
    LowVideo;

    {check to make sure the swap path refers to a valid FIXED disk}
    if (not (SwapToEms or SwapToXms)) and
       (not PathIsValidFixedDisk(SwapPathName)) then
      Warning('The selected swap path refers to a removable drive!');

    {check to see if we're already installed}
    if ModuleInstalled(ModuleName) then
      Abort('PSCREEN is already loaded. Aborting...');

    {install the module with special external interface to allow disabling}
    InstallModule(ModuleName,DisablePopup);

    {check to see if SideKick is loaded}
    if SideKickLoaded then
      Abort('Can''t be loaded after SideKick!');

    {go resident}
    if DefinePop(OurHotKey, PopupEntryPoint, Ptr(SSeg, SPtr)) then begin
      WriteLn('PSCREEN loaded. Press Alt-LeftShift-P to activate.');

      {Enable popups}
      PopupsOn;
      if SwappingOn then begin
        if SwapToEms then begin
          WriteLn('Using EMS memory for swap');
          SetSwapMsgOn(False);
        end
      {$IFDEF SupportXms}  {!!.02}
        else if SwapToXms then begin
          WriteLn('Using XMS memory for swap');
          SetSwapMsgOn(False);
        end
      {$ENDIF}
        else begin
          WriteLn('Swapping to ', SwapPathName, SwapName1);
          SetSwapMsgOn(True);
          DeleteSwapFile(SwapName1);                           {!!.03}
        end;
      end
      else
        WriteLn('Swapping disabled');


      {terminate and stay resident}
      StayResSwap(ParagraphsToKeep+MaxParas, 0,
                  SwapPathName+SwapName1,
                  SwapPathName+SwapName2, SwappingOn);
    end;
    {if we get here we failed}
    Abort(LoadError);
  end;

end.
