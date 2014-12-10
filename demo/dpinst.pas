{$S-,R-,V-,I-,B-,F-,A-} {!!.01}
{$M 16384,16384,655360} {!!.01}

{*********************************************************}
{*                   DPINST.PAS 1.30                     *}
{*    An example program for Object Professional 1.0     *}
{*    Copyright (c) TurboPower Software 1989, 1992.      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I OPDEFINE.INC}

{***************************************************************************
 This program requires that OPDEFINE.INC activate the following defines:
   UseShadows, UseScrollBars, UseHotSpots, UseAdjustableWindows,
 This program will use features activated with the following defines:
   UseMouse
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

program DPInst;
  {-Installation program for DESKPOP}

uses
  Use32,
  {$IFDEF VIRTUALPASCAL}
  Os2Base,
  {$ENDIF}
  Dos,
  OpInline,
  OpString,
  OpDos,
  OpConst,    {!!.20}
  OpRoot,
  OpClone,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpAbsFld,
  OpCmd,
  OpField,
  OpFrame,
  OpWindow,
  OpSelect,
  OpPick,
  OpEntry,
  OpMenu,
  MakeMisc;

const
  Copyright        : string[74] = 'DESKPOP Installation Program 1.30 '^G' '+
                                  'Copyright (c) 1992 TurboPower Software';
  ExeName          : PathStr = 'DESKPOP.EXE';
  MainFrame        : FrameArray = 'ÕÔ¸¾ÍÍ³³';
  BackdropChar     = '±';
  BackdropAttr     = $07;
  {$IFDEF UseMouse}
  MouseChar        : Char = #04;
  {$ENDIF}

const
  dpiColors : ColorSet = (
    TextColor       : $1F; TextMono        : $0F;
    CtrlColor       : $1E; CtrlMono        : $0F;
    FrameColor      : $1A; FrameMono       : $07;
    HeaderColor     : $2F; HeaderMono      : $70;
    ShadowColor     : $08; ShadowMono      : $0F;
    HighlightColor  : $4F; HighlightMono   : $09;
    PromptColor     : $1A; PromptMono      : $07;
    SelPromptColor  : $1A; SelPromptMono   : $07;
    ProPromptColor  : $17; ProPromptMono   : $07;
    FieldColor      : $1F; FieldMono       : $0F;
    SelFieldColor   : $2F; SelFieldMono    : $70;
    ProFieldColor   : $17; ProFieldMono    : $07;
    ScrollBarColor  : $13; ScrollBarMono   : $07;
    SliderColor     : $13; SliderMono      : $0F;
    HotSpotColor    : $30; HotSpotMono     : $70;
    BlockColor      : $1F; BlockMono       : $70;
    MarkerColor     : $3F; MarkerMono      : $70;
    DelimColor      : $31; DelimMono       : $0F;
    SelDelimColor   : $31; SelDelimMono    : $0F;
    ProDelimColor   : $31; ProDelimMono    : $0F;
    SelItemColor    : $2F; SelItemMono     : $70;
    ProItemColor    : $17; ProItemMono     : $07;
    HighItemColor   : $1F; HighItemMono    : $0F;
    AltItemColor    : $1F; AltItemMono     : $0F;
    AltSelItemColor : $3F; AltSelItemMono  : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono   : $0F;
    FlexBHelpColor  : $1F; FlexBHelpMono   : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono   : $70;
    UnselXrefColor  : $1E; UnselXrefMono   : $09;
    SelXrefColor    : $3F; SelXrefMono     : $70;
    MouseColor      : $4F; MouseMono       : $70
  );

  {Entry field constants}
  idHotKey1          = 00;
  idHotKey1Str       = 01;
  idHotKey2          = 02;
  idHotKey3          = 03; {!!.03}
  idHotKey3Str       = 04; {!!.03}
  idSwapPathName     = 05;
  idSwapName1        = 06;
  idSwapName2        = 07;
  idUseEms           = 08;
  idShowSwapMsg      = 09;
  idDefMouseSupport  = 10;
  idDefColorMap      = 11;
  idBrowseDefExt     = 12;
  idBrowseHexMode    = 13;
  idBrowseExpandTabs = 14;
  idBrowseStripHigh  = 15;
  idBrowsePages      = 16;
  idEditDefExt       = 17;
  idEditAutoIndent   = 18;
  idEditDeleteJoins  = 19;
  idEditIndentIsPara = 20;
  idEditSmartTabs    = 21;
  idEditTabDelta     = 22;
  idEditWordWrap     = 23;
  idEditWrapAtLeft   = 24;
  idEditMakeBackups  = 25;
  idEditReadPartial  = 26;
  idEditBufferSize   = 27;
  idDirBufferSize    = 28;
  idDirWinConfirm    = 29;
  idDirWinFormatAll  = 30;
  idCurDirSortType   = 31;
  idPhoneBookName    = 32;
  idDefPhoneExt      = 33;
  idHaveModem        = 34;
  idDefComPort       = 35;
  idDefBaudRate      = 36;
  idDialString       = 37;
  idHangUpString     = 38;

  {Color selector field constants}
  idWindowTextColor      = 0;
  idWindowTextMono       = 1;
  idWindowFrameColor     = 2;
  idWindowFrameMono      = 3;
  idHeaderActiveColor    = 4;
  idHeaderActiveMono     = 5;
  idHeaderInactiveColor  = 6;
  idHeaderInactiveMono   = 7;
  idShadowColor          = 8;
  idShadowMono           = 9;
  idScrollBarColor       = 10;
  idScrollBarMono        = 11;
  idSliderColor          = 12;
  idSliderMono           = 13;
  idHotSpotColor         = 14;
  idHotSpotMono          = 15;
  idMouseColor           = 16;
  idMouseMono            = 17;
  idStatusTextColor      = 18;
  idStatusTextMono       = 19;
  idStatusPromptColor    = 20;
  idStatusPromptMono     = 21;
  idStatusFieldColor     = 22;
  idStatusFieldMono      = 23;
  idMenuFrameColor       = 24;
  idMenuFrameMono        = 25;
  idMenuItemColor        = 26;
  idMenuItemMono         = 27;
  idMenuHighlightColor   = 28;
  idMenuHighlightMono    = 29;
  idMenuSelItemColor     = 30;
  idMenuSelItemMono      = 31;
  idMenuProItemColor     = 32;
  idMenuProItemMono      = 33;
  idPickSelItemColor     = 34;
  idPickSelItemMono      = 35;
  idPickProItemColor     = 36;
  idPickProItemMono      = 37;
  idDirItemColor         = 38;
  idDirItemMono          = 39;
  idDirAltItemColor      = 40;
  idDirAltItemMono       = 41;
  idDirAltSelItemColor   = 42;
  idDirAltSelItemMono    = 43;
  idBrowseTextColor      = 44;
  idBrowseTextMono       = 45;
  idBlockColor           = 46;
  idBlockMono            = 47;
  idMarkerColor          = 48;
  idMarkerMono           = 49;
  idCalcTextColor        = 50;
  idCalcTextMono         = 51;
  idCalcHighlightColor   = 52;
  idCalcHighlightMono    = 53;
  idCalcSubheadColor     = 54;
  idCalcSubheadMono      = 55;
  idEditTextColor        = 56;
  idEditTextMono         = 57;
  idEditCtrlColor        = 58;
  idEditCtrlMono         = 59;
  idEditHighlightColor   = 60;
  idEditHighlightMono    = 61;
  idPhoneItemColor       = 62;
  idPhoneItemMono        = 63;
  idPhoneSelItemColor    = 64;
  idPhoneSelItemMono     = 65;
  idPhonePromptColor     = 66;
  idPhonePromptMono      = 67;
  idPhoneSelPromptColor  = 68;
  idPhoneSelPromptMono   = 69;
  idPhoneSelFieldColor   = 70;
  idPhoneSelFieldMono    = 71;
  idPhoneCtrlColor       = 72;
  idPhoneCtrlMono        = 73;
  idQkRefDimColor        = 74;
  idQkRefDimMono         = 75;
  idQkRefHighlightColor  = 76;
  idQkRefHighlightMono   = 77;

  {Main menu item constants}
  miBrowser          = 01;
  miCalculator       = 02;
  miCalendar         = 03;
  miEditor           = 04;
  miFile             = 05;
  miMenus            = 06;
  miQuickRef         = 07;
  miPhoneBook        = 08;
  miPhoneEditor      = 09;
  miHotKeys          = 10;
  miColors           = 11;
  miOptions          = 12;
  miQuit             = 13;

  {$I DESKMAIN.ICD}  {configuration data for DESKPOP}
  {$I TPUI.ICD}      {configuration data for TPUI}

  {$I OPBROWSE.ICD}  {configuration data for OPBROWSE}
  {$I OPCAL.ICD}     {configuration data for OPCAL}
  {$I OPCALC.ICD}    {configuration data for OPCALC}
  {$I OPEDITOR.ICD}  {configuration data for OPEDITOR}
  {$I OPQKREF.ICD}   {configuration data for OPQKREF}
  {$I OPENTRY.ICD}   {configuration data for OPEDITOR}
  {$I OPMENU.ICD}    {configuration data for OPMENU}

  {$I DPINST.IN1}   {key selector, key editor, help file generation}

type
  BytePtr            = ^Byte;
const
  Modified           : Boolean = False;
var
  ES                 : ScrollingEntryScreen;
  SS                 : ScrollingSelector;
  CS                 : ColorSelector;
  DPC                : Cloner;
  CP                 : CommandPacker;
  KS                 : KeySelector;
  KE                 : KeyEditor;
  UK                 : UnpackedCmdTable;
  MainMenu           : Menu;
  Cmd                : Word;
  DeskPos            : LongInt;
  TpUiPos            : LongInt;
  BrowsePos          : LongInt;
  CalPos             : LongInt;
  CalcPos            : LongInt;
  EditorPos          : LongInt;
  EntryPos           : LongInt;
  QkRefPos           : LongInt;
  MenuPos            : LongInt;

  {---------------------- configuration options -----------------------}

  {$F+}

  procedure PreEdit(ESP : EntryScreenPtr);
    {-Called just before a field is edited}
  var
    S : string[80];
  begin
    with ESP^ do
      case GetCurrentID of
        idHotKey1,
        idHotKey3          : S := 'Enter primary hotkey, or <F10> to select'; {!!.03}
        idHotKey1Str,
        idHotKey3Str       : S := 'Enter hotkey prompt to display when going resident'; {!!.03}
        idHotKey2          : S := 'Enter secondary hotkey, or <F10> to select';
        idSwapPathName     : S := 'Enter path for swap files';
        idSwapName1        : S := 'Enter name for first swap file';
        idSwapName2        : S := 'Enter name for second swap file';
        idUseEms           : S := 'Use EMS for swapping if possible?';
        idShowSwapMsg      : S := 'Display "Swapping in..." message?';
        idDefMouseSupport  : S := 'Use mouse if one is installed?';
        idDefColorMap,
        idBrowseHexMode,
        idDirWinFormatAll,
        idCurDirSortType,
        idDefComPort,
        idDefBaudRate      : S := 'Press <+>, <->, or <Space> to select';
        idBrowseDefExt,
        idEditDefExt,
        idDefPhoneExt      : S := 'Enter default file extension';
        idBrowseExpandTabs : S := 'Expand tabs when displaying files?';
        idBrowseStripHigh  : S := 'Strip high bits when displaying files?';
        idBrowsePages      : S := 'Enter number of file buffers (2-20)';
        idEditAutoIndent   : S := 'Auto-indent when starting a new line?';
        idEditDeleteJoins  : S := 'Allow <Del> to join lines?';
        idEditIndentIsPara : S := 'Should indentation signal a new paragraph (when reformatting)?';
        idEditSmartTabs    : S := 'Use smart tabs rather than fixed tabs?';
        idEditTabDelta     : S := 'Enter size for fixed tabs';
        idEditWordWrap     : S := 'Enable word wrap at right margin?';
        idEditWrapAtLeft   : S := 'Wrap to previous line from column 1 on <Left>?';
        idEditMakeBackups  : S := 'Make backup files when saving?';
        idEditReadPartial  : S := 'Read partial file if too large for buffer?';
        idEditBufferSize   : S := 'Enter size for edit buffer (8192-65521)';
        idDirBufferSize    : S := 'Enter size for filename buffer (4096-65521)';
        idDirWinConfirm    : S := 'Confirm file operations (delete, copy, move)?';
        idPhoneBookName    : S := 'Enter name for default phone book';
        idHaveModem        : S := 'Is a modem installed?';
        idDialString       : S := 'Enter prefix string to use when dialing';
        idHangUpString     : S := 'Enter string used to hang up modem';
      end;
    DisplayCentered(S, ScreenHeight);
  end;

  procedure PostEdit(ESP : EntryScreenPtr);
    {-Called just after a field has been edited}
  var
    ID, Cmd : Word;
    EFP : EntryFieldPtr;
  begin
    with ES do begin
      {was current field modified?}
      if CurrentFieldModified then
        Modified := True
      else begin
        {is current command a mouse click?}
        Cmd := GetLastCommand;
        if Cmd = ccMouseSel then begin
          {evaluate the command}
          ID := EvaluateCommand(Cmd);

          {if Cmd was changed to ccIncChoice, the field is about to be modified}
          if Cmd = ccIncChoice then
            Modified := True;
        end;
      end;
    end;
  end;

  procedure IncChoice(var Value; ID : Word; Factor : Integer; var St : string);
    {-Increment a multiple choice field value and convert it to a string}
  const
    ColorChoiceSt : array[ColorChoice] of string[5] = ('Auto', 'Color', 'Mono');
    AsciiHexSt    : array[Boolean] of string[5] = ('ASCII', 'Hex');
    FormatSt      : array[Boolean] of string[4] = ('Name', 'All');
    ComPortSt     : array[ComPortType] of string[4] = ('COM1', 'COM2');
    BaudRateSt    : array[BaudRateType] of string[4] = ('300', '1200', '2400');
    DirSortSt     : array[DirSortType] of string[6] = (
                    'Name', 'Ext', 'Date', 'Size', 'MS-DOS'); {!!.03}
  var
    DST : DirSortType absolute Value;
    CPT : ComPortType absolute Value;
    BRT : BaudRateType absolute Value;
    CC  : ColorChoice absolute Value;
    B   : Boolean absolute Value;
  begin
    case ID of
      idDefColorMap :
        begin
          if Factor <> 0 then
            if Factor = -1 then           {!!.13}
              if CC = UseDefault then     {!!.13}
                CC := ForceMono           {!!.13}
              else                        {!!.13}
                Dec(CC)                   {!!.13}
            else if CC = ForceMono then   {!!.13}
              CC := UseDefault
            else
              Inc(CC);
          St := ColorChoiceSt[CC];
        end;
      idBrowseHexMode :
        begin
          if Factor <> 0 then
            B := not B;
          St := AsciiHexSt[B];
        end;
      idDirWinFormatAll :
        begin
          if Factor <> 0 then
            B := not B;
          St := FormatSt[B];
        end;
      idCurDirSortType :
        begin
          if Factor <> 0 then
            if Factor = -1 then           {!!.13}
              if DST = dstName then       {!!.13}
                DST := dstDos             {!!.13}
              else                        {!!.13}
                Dec(DST)                  {!!.13}
            else if DST = dstDos then     {!!.13}
              DST := dstName
            else
              Inc(DST);
          St := DirSortSt[DST];
        end;
      idDefComPort :
        begin
          if Factor <> 0 then
            B := not B;
          St := ComPortSt[CPT];
        end;
      idDefBaudRate :
        begin
          if Factor <> 0 then
            if Factor = -1 then           {!!.13}
              if BRT = br300 then         {!!.13}
                BRT := br2400             {!!.13}
              else                        {!!.13}
                Dec(BRT)                  {!!.13}
            else if BRT = br2400 then     {!!.13}
              BRT := br300
            else
              Inc(BRT);
          St := BaudRateSt[BRT];
        end;
      else
        St := '';
    end;
  end;

  procedure ErrorHandler(UnitCode : Byte; var ErrCode : Word; Msg : string);
    {-Report errors}
  begin
    if Msg = '' then
      Msg := 'Internal error '+Long2Str(ErrCode);
    ErrorMessage(Msg);
  end;

  function ValidateHotKey(EFP : EntryFieldPtr;
                          var ErrCode : Word;
                          var ErrorSt : StringPtr) : Boolean;
    {-Validate a hotkey}
  const
    HotKeyError : string[54] = 'High byte must be $01-$0F, low byte $01..$54, $57..$58';
  var
    L : LongInt;
    W : Word absolute L;
    LW : Byte absolute L;
    S : string[80];

    function HotKeyOK : Boolean;
    begin
      HotKeyOK := False;
      case Hi(W) of
        0, $10..$FF : Exit;
      end;
      case LW of
        1..$54, $57..$58 : HotKeyOK := True;
      end;
    end;

  begin
    ValidateHotKey := False;
    with EFP^ do begin
      {format OK?}
      StripPicture(efEditSt^, S);
      if not efStr2Long(S, L) then begin
        ErrCode := ecBadFormat;
        ErrorSt := @emInvalidNumber;
      end
      else if HotKeyOK then
        ValidateHotKey := True
      else begin
        ErrCode := ecOutOfRange;
        ErrorSt := @HotKeyError;
      end;
    end;
  end;


  {$F-}

  function InitEntryScreen : Word; {!!.03} {added two new fields}
    {-Initialize entry screen}
  const
    WinOptions = wBordered+wClear+wUserContents;
  begin
    with ES do begin
      if not InitCustom(
        12, 4, 68, ScreenHeight-3, dpiColors, WinOptions) then begin
          InitEntryScreen := InitStatus;
          Exit;
      end;

      wFrame.SetFrameType(MainFrame);
      wFrame.AddShadow(shBR, shSeeThru);
      wFrame.AddHeader(' Configuration Options ', heTC);
      wFrame.AddCustomScrollBar(frRR, 1, MaxLongInt, 1, 1, '²', '°', dpiColors);

      esFieldOptionsOn(efClearFirstChar+efBeepOnError);
      SetWrapMode(StopAtEdges);

      SetPreEditProc(PreEdit);
      SetPostEditProc(PostEdit);
      SetErrorProc(ErrorHandler);

      AddTextField('TSR Related Options',  01, 2);
      AddTextField('Display Options',      13, 2);
      AddTextField('Browser Options',      17, 2);
      AddTextField('Editor Options',       24, 2);
      AddTextField('File Manager Options', 37, 2);
      AddTextField('Phone Book Options',   43, 2);

    {idHotKey1:}
      esFieldOptionsOn(efRightJustify);
      AddWordField(
        'Primary hotkey', 2, 2, 'cKKKK', 2, 25, 1, 0, 65535, HotKey1);
      esFieldOptionsOff(efRightJustify);

    {idHotKey1Str:}
      AddStringField(
        'Hotkey string', 3, 2, '', 3, 25, 32, 2, HotKey1Str);

    {idHotKey2:}
      AddWordField(
        'Secondary hotkey', 4, 2, 'cKKKK', 4, 25, 3, 0, 65535, HotKey2);

    {idHotKey3:}
      esFieldOptionsOn(efRightJustify);
      AddWordField(
        'Dialer hotkey', 5, 2, 'cKKKK', 5, 25, 1, 0, 65535, HotKey3);
      esFieldOptionsOff(efRightJustify);

    {idHotKey3Str:}
      AddStringField(
        'Dialer hotkey string', 6, 2, '', 6, 25, 32, 2, HotKey3Str);

    {idSwapPathName:}
      AddStringField(
        'Path for swap files', 7, 2, CharStr('!', 64), 7, 25, 32, 4, SwapPathName);

    {idSwapName1:}
      AddStringField(
        'Swap name #1', 8, 2, '!!!!!!!!!!!!', 8, 25, 12, 5, SwapName1);

    {idSwapName2:}
      AddStringField(
        'Swap name #2', 9, 2, '!!!!!!!!!!!!', 9, 25, 12, 6, SwapName2);

    {idUseEms:}
      AddYesNoField(
        'Use EMS if available', 10, 2, 'Y', 10, 25, 7, UseEmsIfAvail);

    {idShowSwapMsg:}
      AddYesNoField(
        'Show swap message', 11, 2, 'Y', 11, 25, 8, ShowSwapMsg);

    {idDefMouseSupport:}
      AddYesNoField(
        'Enable mouse support', 14, 2, 'Y', 14, 25, 9, DefMouseSupport);

    {idDefColorMap:}
      AddChoiceField(
        'Color selection', 15, 2, 'XXXXX', 15, 25, 10, 1, IncChoice, DefColorMap);

    {idBrowseDefExt:}
      AddStringField(
        'Default extension', 18, 2, '!!!', 18, 25, 3, 11, BrowseDefExt);

    {idBrowseHexMode:}
      AddChoiceField(
        'Default mode', 19, 2, 'XXXXX', 19, 25, 12, 1, IncChoice, BrowseHexMode);

    {idBrowseExpandTabs:}
      AddYesNoField(
        'Tab expansion', 20, 2, 'Y', 20, 25, 13, BrowseExpandTabs);

    {idBrowseStripHigh:}
      AddYesNoField(
        'Strip high bits', 21, 2, 'Y', 21, 25, 14, BrowseStripHigh);

    {idBrowsePages:}
      AddByteField(
        'File buffers', 22, 2, '99', 22, 25, 15, 2, 20, BrowsePages);

    {idEditDefExt:}
      AddStringField(
        'Default extension', 25, 2, '!!!', 25, 25, 3, 16, EditDefExt);

    {idEditAutoIndent:}
      AddYesNoField(
        'Auto indent', 26, 2, 'Y', 26, 25, 17, EditAutoIndent);

    {idEditDeleteJoins:}
      AddYesNoField(
        'Delete joins lines', 27, 2, 'Y', 27, 25, 18, EditDeleteJoins);

    {idEditIndentIsPara:}
      AddYesNoField(
        'Indent starts para', 28, 2, 'Y', 28, 25, 19, EditIndentIsPara);

    {idEditSmartTabs:}
      AddYesNoField(
        'Smart tabs', 29, 2, 'Y', 29, 25, 20, EditSmartTabs);

    {idEditTabDelta:}
      AddByteField(
        'Tab size', 30, 2, '99', 30, 25, 21, 1, 72, EditTabDelta);

    {idEditWordWrap:}
      AddYesNoField(
        'Word wrap', 31, 2, 'Y', 31, 25, 22, EditWordWrap);

    {idWrapAtLeft:}
      AddYesNoField(
        'Wrap at column 1', 32, 2, 'Y', 32, 25, 23, EditWrapAtLeft);

    {idEditMakeBackups:}
      AddYesNoField(
        'Make backup files', 33, 2, 'Y', 33, 25, 24, EditMakeBackups);

    {idEditReadPartial:}
      AddYesNoField(
        'Read partial files', 34, 2, 'Y', 34, 25, 25, EditReadPartial);

    {idEditBufferSize:}
      AddWordField(
        'Buffer size', 35, 2, '99999', 35, 25, 26, 8192, 65521, EditBufferSize);

    {idDirBufferSize:}
      AddWordField(
        'Buffer size', 38, 2, '99999', 38, 25, 27, 4096, 65521, DirBufferSize);

    {idDirWinConfirm:}
      AddYesNoField(
        'Confirm changes', 39, 2, 'Y', 39, 25, 28, DirWinConfirm);

    {idDirWinFormatAll:}
      AddChoiceField(
        'Filename format', 40, 2, 'XXXX', 40, 25, 29, 1, IncChoice, DirWinFormatAll);

    {idCurDirSortType:}
      AddChoiceField(
        'Sort order', 41, 2, 'XXXXXX', 41, 25, 30, 1, IncChoice, CurDirSortType);

    {idPhoneBookName:}
      AddStringField(
        'Default phone book', 44, 2, CharStr('!', 64), 44, 25, 32, 31, PhoneBookName);

    {idDefPhoneExt:}
      AddStringField(
        'Default extension', 45, 2, '!!!', 45, 25, 3, 32, DefPhoneExt);

    {idHaveModem:}
      AddYesNoField(
        'Modem installed', 46, 2, 'Y', 46, 25, 33, HaveModem);

    {idDefComPort:}
      AddChoiceField(
        'Communications port', 47, 2, 'XXXX', 47, 25, 34, 1, IncChoice, DefComPort);

    {idDefBaudRate:}
      AddChoiceField(
        'Baud rate', 48, 2, 'XXXX', 48, 25, 35, 1, IncChoice, DefBaudRate);

    {idDialString:}
      AddStringField(
        'Dial modem string', 49, 2, '', 49, 25, 15, 36, DialString);

    {idHangUpString:}
      AddStringField(
        'Hang up modem string', 50, 2, '', 50, 25, 10, 37, HangUpString);

      AllocateScreen;

      {set up special validation routine for hotkeys}
      ChangeValidation(idHotKey1, ValidateHotKey);
      ChangeValidation(idHotKey2, ValidateHotKey);
      ChangeValidation(idHotKey3, ValidateHotKey);

      InitEntryScreen := GetLastError;
    end;
  end;

  procedure EditOptions;
    {-Edit configuration options}
  var
    Status : Word;
    Cmd    : Word;
  begin
    {initialize entry screen}
    Status := InitEntryScreen;
    if Status <> 0 then begin
      InsufficientMemory;
      Exit;
    end;

    {F10: Special popup hotkey selector}
    EntryCommands.AddCommand(ccUser0, 1, $4400, 0);

    repeat
      ES.Process;
      Cmd := ES.GetLastCommand;
      if Cmd = ccUser0 then
        case ES.GetCurrentID of
          idHotKey1 : if EditHotKey(HotKey1) then
                        Modified := True;
          idHotKey2 : if EditHotKey(HotKey2) then
                        Modified := True;
          idHotKey3 : if EditHotKey(HotKey3) then  {!!.03}
                        Modified := True;          {!!.03}
        end;

    until (Cmd = ccError) or (Cmd = ccDone);

    {cancel F10}
    EntryCommands.AddCommand(ccNone, 1, $4400, 0);

    {get rid of it}
    ES.Erase;
    ES.Done;
  end;

  {---------------------- color settings -----------------------}

  function InitColorSelector : Word;
    {-Initialize the color selector}
  begin
    if not CS.InitCustom(5, 4, dpiColors, DefWindowOptions, False) then
      InitColorSelector := epFatal+ecOutOfMemory
    else with CS do begin
      SetErrorProc(ErrorHandler);
      wFrame.AddShadow(shBR, shSeeThru);
      SetBoxAttr($1F, $0F);
      SetColor(0);
      Draw;
      InitColorSelector := GetLastError;
    end;
  end;

  procedure EditAttr(var B : Byte);
    {-"Edit" the specified color attribute}
  begin
    with CS do begin
      Select;

      DisplayCentered('Press <Enter> to select, <Esc> to cancel', ScreenHeight);

      SetColor(B);
      Process;
      if (GetLastCommand <> ccQuit) and (GetLastCommand <> ccError) then
        if GetColor <> B then begin
          B := GetColor;
          Modified := True;
        end;

      SS.Select;
    end;
  end;

  function GetColorPtr(ID : Word) : BytePtr;
    {-Point to the attribute pair corresponding to the field}
  var
    O : Word;
  begin
    case ID of
      idWindowTextColor     : O := Ofs(TpuiColors.TextColor);
      idWindowTextMono      : O := Ofs(TpuiColors.TextMono);
      idWindowFrameColor    : O := Ofs(TpuiColors.FrameColor);
      idWindowFrameMono     : O := Ofs(TpuiColors.FrameMono);
      idHeaderActiveColor   : O := Ofs(TpuiColors.HeaderColor);
      idHeaderActiveMono    : O := Ofs(TpuiColors.HeaderMono);
      idHeaderInactiveColor : O := Ofs(InactiveColor);
      idHeaderInactiveMono  : O := Ofs(InactiveMono);
      idShadowColor         : O := Ofs(TpuiColors.ShadowColor);
      idShadowMono          : O := Ofs(TpuiColors.ShadowMono);
      idScrollBarColor      : O := Ofs(TpuiColors.ScrollBarColor);
      idScrollBarMono       : O := Ofs(TpuiColors.ScrollBarMono);
      idSliderColor         : O := Ofs(TpuiColors.SliderColor);
      idSliderMono          : O := Ofs(TpuiColors.SliderMono);
      idHotSpotColor        : O := Ofs(TpuiColors.HotSpotColor);
      idHotSpotMono         : O := Ofs(TpuiColors.HotSpotMono);
      idMouseColor          : O := Ofs(TpuiColors.MouseColor);
      idMouseMono           : O := Ofs(TpuiColors.MouseMono);
      idStatusTextColor     : O := Ofs(StatusColor);
      idStatusTextMono      : O := Ofs(StatusMono);
      idStatusPromptColor   : O := Ofs(TpuiColors.SelPromptColor);
      idStatusPromptMono    : O := Ofs(TpuiColors.SelPromptMono);
      idStatusFieldColor    : O := Ofs(TpuiColors.SelFieldColor);
      idStatusFieldMono     : O := Ofs(TpuiColors.SelFieldMono);
      idMenuFrameColor      : O := Ofs(TpuiMenuColors.FrameColor);
      idMenuFrameMono       : O := Ofs(TpuiMenuColors.FrameMono);
      idMenuItemColor       : O := Ofs(TpuiMenuColors.TextColor);
      idMenuItemMono        : O := Ofs(TpuiMenuColors.TextMono);
      idMenuHighlightColor  : O := Ofs(TpuiMenuColors.HighItemColor);
      idMenuHighlightMono   : O := Ofs(TpuiMenuColors.HighItemMono);
      idMenuSelItemColor    : O := Ofs(TpuiMenuColors.SelItemColor);
      idMenuSelItemMono     : O := Ofs(TpuiMenuColors.SelItemMono);
      idMenuProItemColor    : O := Ofs(TpuiMenuColors.ProItemColor);
      idMenuProItemMono     : O := Ofs(TpuiMenuColors.ProItemMono);
      idPickSelItemColor    : O := Ofs(TpuiColors.SelItemColor);
      idPickSelItemMono     : O := Ofs(TpuiColors.SelItemMono);
      idPickProItemColor    : O := Ofs(TpuiColors.ProItemColor);
      idPickProItemMono     : O := Ofs(TpuiColors.ProItemMono);
      idDirItemColor        : O := Ofs(DirItemColor);
      idDirItemMono         : O := Ofs(DirItemMono);
      idDirAltItemColor     : O := Ofs(TpuiColors.AltItemColor);
      idDirAltItemMono      : O := Ofs(TpuiColors.AltItemMono);
      idDirAltSelItemColor  : O := Ofs(TpuiColors.AltSelItemColor);
      idDirAltSelItemMono   : O := Ofs(TpuiColors.AltSelItemMono);
      idBrowseTextColor     : O := Ofs(BrowseTextColor);
      idBrowseTextMono      : O := Ofs(BrowseTextMono);
      idBlockColor          : O := Ofs(TpuiColors.BlockColor);
      idBlockMono           : O := Ofs(TpuiColors.BlockMono);
      idMarkerColor         : O := Ofs(TpuiColors.MarkerColor);
      idMarkerMono          : O := Ofs(TpuiColors.MarkerMono);
      idCalcTextColor       : O := Ofs(CalcTextColor);
      idCalcTextMono        : O := Ofs(CalcTextMono);
      idCalcHighlightColor  : O := Ofs(CalcHighlightColor);
      idCalcHighlightMono   : O := Ofs(CalcHighlightMono);
      idCalcSubheadColor    : O := Ofs(CalcSubheadColor);
      idCalcSubheadMono     : O := Ofs(CalcSubheadMono);
      idEditTextColor       : O := Ofs(EditTextColor);
      idEditTextMono        : O := Ofs(EditTextMono);
      idEditCtrlColor       : O := Ofs(TpuiColors.CtrlColor);
      idEditCtrlMono        : O := Ofs(TpuiColors.CtrlMono);
      idEditHighlightColor  : O := Ofs(TpuiColors.HighlightColor);
      idEditHighlightMono   : O := Ofs(TpuiColors.HighlightMono);
      idPhoneItemColor      : O := Ofs(PhoneItemColor);
      idPhoneItemMono       : O := Ofs(PhoneItemMono);
      idPhoneSelItemColor   : O := Ofs(PhoneSelItemColor);
      idPhoneSelItemMono    : O := Ofs(PhoneSelItemMono);
      idPhonePromptColor    : O := Ofs(PhonePromptColor);
      idPhonePromptMono     : O := Ofs(PhonePromptMono);
      idPhoneSelPromptColor : O := Ofs(PhoneSelPromptColor);
      idPhoneSelPromptMono  : O := Ofs(PhoneSelPromptMono);
      idPhoneSelFieldColor  : O := Ofs(PhoneSelFieldColor);
      idPhoneSelFieldMono   : O := Ofs(PhoneSelFieldMono);
      idPhoneCtrlColor      : O := Ofs(PhoneCtrlColor);
      idPhoneCtrlMono       : O := Ofs(PhoneCtrlMono);
      idQkRefDimColor       : O := Ofs(QkRefDimColor);
      idQkRefDimMono        : O := Ofs(QkRefDimMono);
      idQkRefHighlightColor : O := Ofs(QkRefHighlightColor);
      idQkRefHighlightMono  : O := Ofs(QkRefHighlightMono);
    end;
{$IFDEF VIRTUALPASCAL}
    GetColorPtr := Ptr( O );
{$ELSE}
    GetColorPtr := Ptr(DSeg, O);
{$ENDIF}
  end;

  {$F+}

  procedure PreSelect(SP : SelectorPtr; ID : Word);
    {-Our pre-select routine}
  begin
    DisplayCentered(
      'Press <Enter> to select, <Esc> to return to main menu', ScreenHeight);
  end;

  procedure GetAction(SP : SelectorPtr; ID : Word);
    {-Our action routine}
  begin
    EditAttr(GetColorPtr(ID)^);
    PreSelect(SP, ID);
  end;

  procedure GetField(ID : Word; NeedPrompt : Boolean; var S : string);
    {-Our GetField routine}
  var
    B : Byte;
  begin
    if NeedPrompt then
      case ID of
        idWindowTextColor     : S := 'Text';
        idWindowFrameColor    : S := 'Frame';
        idHeaderActiveColor   : S := 'Header, active window';
        idHeaderInactiveColor : S := 'Header, inactive window';
        idShadowColor         : S := 'Shadow';
        idScrollBarColor      : S := 'Scroll bar';
        idSliderColor         : S := 'Scroll bar slider';
        idHotSpotColor        : S := 'Hot spots, scroll bar arrows';
        idMouseColor          : S := 'Mouse cursor';
        idStatusTextColor     : S := 'Text';
        idStatusPromptColor   : S := 'Edit prompt';
        idStatusFieldColor    : S := 'Edit field';
        idMenuFrameColor      : S := 'Frame';
        idMenuItemColor       : S := 'Unselected item';
        idMenuHighlightColor  : S := 'Highlighted char';
        idMenuSelItemColor    : S := 'Selected item';
        idMenuProItemColor    : S := 'Unselected item';
        idPickSelItemColor    : S := 'Selected item';
        idPickProItemColor    : S := 'Protected item';
        idDirItemColor        : S := 'Filename, unselected';
        idDirAltItemColor     : S := 'Directory name, unselected';
        idDirAltSelItemColor  : S := 'Directory name, selected';
        idBrowseTextColor     : S := 'Text';
        idBlockColor          : S := 'Marked block (Editor also)';
        idMarkerColor         : S := 'Text marker (Editor also)';
        idCalcTextColor       : S := 'Normal text';
        idCalcHighlightColor  : S := 'Highlighted text';
        idCalcSubheadColor    : S := 'Subheads';
        idEditTextColor       : S := 'Text';
        idEditCtrlColor       : S := 'Control characters';
        idEditHighlightColor  : S := 'Found text';
        idPhoneItemColor      : S := 'Unselected entry';
        idPhoneSelItemColor   : S := 'Selected entry';
        idPhonePromptColor    : S := 'Prompt, unselected';
        idPhoneSelPromptColor : S := 'Prompt, selected';
        idPhoneSelFieldColor  : S := 'Field, selected';
        idPhoneCtrlColor      : S := 'Control characters';
        idQkRefDimColor       : S := 'Dim text';
        idQkRefHighlightColor : S := 'Highlighted text';
        else                    S := '';
      end
    else
      {return the attribute's value in hex}
      S := HexB(GetColorPtr(ID)^);
  end;

  {$F-}

  function InitSelector : Word;
    {-Initialize the selector}
  const
    WinOptions = wBordered+wClear+wUserContents+wFullMouseWindow;
  begin
    with SS do begin
      if not InitCustom(38, 4, 76, ScreenHeight-4, dpiColors, WinOptions) then begin
        InitSelector := InitStatus;
        Exit;
      end;

      wFrame.SetFrameType(MainFrame);
      wFrame.AddShadow(shBR, shSeeThru);
      wFrame.AddHeader(' Colors ', heTC);
      wFrame.AddHeaderColor(' Color Mono ', heTR, $1F, $0F);
      wFrame.AddCustomScrollBar(frRR, 1, MaxLongInt, 1, 1, '²', '°', dpiColors);
      SetWrapMode(StopAtEdges);

      SetActionProc(GetAction);
      SetGetFieldProc(GetField);
      SetErrorProc(ErrorHandler);
      SetPreSelectProc(PreSelect);

      AddTextField('Windows, General', 1, 2);
      AddTextField('Status line', 12, 2);
      AddTextField('Menus', 17, 2);
      AddTextField('Pick list, Calendar', 24, 2);
      AddTextField('Directory list', 28, 2);
      AddTextField('Browser', 33, 2);
      AddTextField('Calculator', 38, 2);
      AddTextField('Editor', 43, 2);
      AddTextField('Phone book (list)', 48, 2);
      AddTextField('Phone book (editor)', 52, 2);
      AddTextField('Quick reference chart', 58, 2);

    {idWindowTextColor:}
      AddField(02, 02, 04, 02, 32, 02, 01);

    {idWindowTextMono:}
      AddField(02, 35, 00, 02, 35, 02, 02);

    {idWindowFrameColor:}
      AddField(03, 02, 05, 03, 32, 02, 03);

    {idWindowFrameMono:}
      AddField(03, 35, 00, 03, 35, 02, 04);

    {idHeaderActiveColor:}
      AddField(04, 02, 21, 04, 32, 02, 05);

    {idHeaderActiveMono:}
      AddField(04, 35, 00, 04, 35, 02, 06);

    {idHeaderInactiveColor:}
      AddField(05, 02, 23, 05, 32, 02, 07);

    {idHeaderInactiveMono:}
      AddField(05, 35, 00, 05, 35, 02, 08);

    {idShadowColor:}
      AddField(06, 02, 06, 06, 32, 02, 09);

    {idShadowMono:}
      AddField(06, 35, 00, 06, 35, 02, 10);

    {idScrollBarColor:}
      AddField(07, 02, 10, 07, 32, 02, 11);

    {idScrollBarMono:}
      AddField(07, 35, 00, 07, 35, 02, 12);

    {idSliderColor:}
      AddField(08, 02, 17, 08, 32, 02, 13);

    {idSliderMono:}
      AddField(08, 35, 00, 08, 35, 02, 14);

    {idHotSpotColor:}
      AddField(09, 02, 28, 09, 32, 02, 15);

    {idHotSpotMono:}
      AddField(09, 35, 00, 09, 35, 02, 16);

    {idMouseColor:}
      AddField(10, 02, 12, 10, 32, 02, 17);

    {idMouseMono:}
      AddField(10, 35, 00, 10, 35, 02, 18);

    {idStatusTextColor:}
      AddField(13, 02, 04, 13, 32, 02, 19);

    {idStatusTextMono:}
      AddField(13, 35, 00, 13, 35, 02, 20);

    {idStatusPromptColor:}
      AddField(14, 02, 11, 14, 32, 02, 21);

    {idStatusPromptMono:}
      AddField(14, 35, 00, 14, 35, 02, 22);

    {idStatusFieldColor:}
      AddField(15, 02, 10, 15, 32, 02, 23);

    {idStatusFieldMono:}
      AddField(15, 35, 00, 15, 35, 02, 24);

    {idMenuFrameColor:}
      AddField(18, 02, 05, 18, 32, 02, 25);

    {idMenuFrameMono:}
      AddField(18, 35, 00, 18, 35, 02, 26);

    {idMenuItemColor:}
      AddField(19, 02, 15, 19, 32, 02, 27);

    {idMenuItemMono:}
      AddField(19, 35, 00, 19, 35, 02, 28);

    {idMenuHighlightColor:}
      AddField(20, 02, 16, 20, 32, 02, 29);

    {idMenuHighlightMono:}
      AddField(20, 35, 00, 20, 35, 02, 30);

    {idMenuSelItemColor:}
      AddField(21, 02, 13, 21, 32, 02, 31);

    {idMenuSelItemMono:}
      AddField(21, 35, 00, 21, 35, 02, 32);

    {idMenuProItemColor:}
      AddField(22, 02, 15, 22, 32, 02, 33);

    {idMenuProItemMono:}
      AddField(22, 35, 00, 22, 35, 02, 34);

    {idPickSelItemColor:}
      AddField(25, 02, 13, 25, 32, 02, 35);

    {idPickSelItemMono:}
      AddField(25, 35, 00, 25, 35, 02, 36);

    {idPickProItemColor:}
      AddField(26, 02, 14, 26, 32, 02, 37);

    {idPickProItemMono:}
      AddField(26, 35, 00, 26, 35, 02, 38);

    {idDirItemColor:}
      AddField(29, 02, 20, 29, 32, 02, 39);

    {idDirItemMono:}
      AddField(29, 35, 00, 29, 35, 02, 40);

    {idDirAltItemColor:}
      AddField(30, 02, 26, 30, 32, 02, 41);

    {idDirAltItemMono:}
      AddField(30, 35, 00, 30, 35, 02, 42);

    {idDirAltSelItemColor:}
      AddField(31, 02, 24, 31, 32, 02, 43);

    {idDirAltSelItemMono:}
      AddField(31, 35, 00, 31, 35, 02, 44);

    {idBrowseTextColor:}
      AddField(34, 02, 04, 34, 32, 02, 45);

    {idBrowseTextMono:}
      AddField(34, 35, 00, 34, 35, 02, 46);

    {idBlockColor:}
      AddField(35, 02, 26, 35, 32, 02, 47);

    {idBlockMono:}
      AddField(35, 35, 00, 35, 35, 02, 48);

    {idMarkerColor:}
      AddField(36, 02, 25, 36, 32, 02, 49);

    {idMarkerMono:}
      AddField(36, 35, 00, 36, 35, 02, 50);

    {idCalcTextColor:}
      AddField(39, 02, 11, 39, 32, 02, 51);

    {idCalcTextMono:}
      AddField(39, 35, 00, 39, 35, 02, 52);

    {idCalcHighlightColor:}
      AddField(40, 02, 16, 40, 32, 02, 53);

    {idCalcHighlightMono:}
      AddField(40, 35, 00, 40, 35, 02, 54);

    {idCalcSubheadColor:}
      AddField(41, 02, 08, 41, 32, 02, 55);

    {idCalcSubheadMono:}
      AddField(41, 35, 00, 41, 35, 02, 56);

    {idEditTextColor:}
      AddField(44, 02, 04, 44, 32, 02, 57);

    {idEditTextMono:}
      AddField(44, 35, 00, 44, 35, 02, 58);

    {idEditCtrlColor:}
      AddField(45, 02, 18, 45, 32, 02, 59);

    {idEditCtrlMono:}
      AddField(45, 35, 00, 45, 35, 02, 60);

    {idEditHighlightColor:}
      AddField(46, 02, 10, 46, 32, 02, 61);

    {idEditHighlightMono:}
      AddField(46, 35, 00, 46, 35, 02, 62);

    {idPhoneItemColor:}
      AddField(49, 02, 16, 49, 32, 02, 63);

    {idPhoneItemMono:}
      AddField(49, 35, 00, 49, 35, 02, 64);

    {idPhoneSelItemColor:}
      AddField(50, 02, 14, 50, 32, 02, 65);

    {idPhoneSelItemMono:}
      AddField(50, 35, 00, 50, 35, 02, 66);

    {idPhonePromptColor:}
      AddField(53, 02, 18, 53, 32, 02, 67);

    {idPhonePromptMono:}
      AddField(53, 35, 00, 53, 35, 02, 68);

    {idPhoneSelPromptColor:}
      AddField(54, 02, 16, 54, 32, 02, 69);

    {idPhoneSelPromptMono:}
      AddField(54, 35, 00, 54, 35, 02, 70);

    {idPhoneSelFieldColor:}
      AddField(55, 02, 15, 55, 32, 02, 71);

    {idPhoneSelFieldMono:}
      AddField(55, 35, 00, 55, 35, 02, 72);

    {idPhoneCtrlColor:}
      AddField(56, 02, 18, 56, 32, 02, 73);

    {idPhoneCtrlMono:}
      AddField(56, 35, 00, 56, 35, 02, 74);

    {idQkRefDimColor:}
      AddField(59, 02, 08, 59, 32, 02, 75);

    {idQkRefDimMono:}
      AddField(59, 35, 00, 59, 35, 02, 76);

    {idQkRefHighlightColor:}
      AddField(60, 02, 16, 60, 32, 02, 77);

    {idQkRefHighlightMono:}
      AddField(60, 35, 00, 60, 35, 02, 78);

      AllocateScreen;

      InitSelector := GetLastError;
    end;
  end;

  procedure EditColors;
    {-Edit colors}
  var
    Status : Word;
    Cmd    : Word;
  begin
    {initialize selector}
    Status := InitSelector;
    if Status <> 0 then begin
      InsufficientMemory;
      Exit;
    end;
    Status := InitColorSelector;
    if Status <> 0 then begin
      InsufficientMemory;
      Exit;
    end;

    repeat
      SS.Process;
      Cmd := SS.GetLastCommand;
    until (Cmd = ccQuit) or (Cmd = ccError) or (Cmd = ccDone);

    {get rid of selector and color selector}
    SS.Erase;
    CS.Erase;
    SS.Done;
    CS.Done;
  end;

  {------------------------------------------------------}

  function CheckModifiedFlags(NumCmds : Word) : Boolean;
    {-Check to see if any of the Modified flags are set in UK}
  var
    I : Word;
  begin
    {assume success}
    CheckModifiedFlags := False;

    {check for modified keys}
    for I := 1 to NumCmds do
      if UK[I].Modified then begin
        CheckModifiedFlags := True;
        Exit;
      end;
  end;

  function ConflictsWereFound : Boolean;
    {-Any conflicts?}
  var
    I, ID : Word;
  begin
    ConflictsWereFound := False;

    {see if any modifications were made}
    if CheckModifiedFlags(ccUser55*3) then begin
      {check for conflicts}
      DisplayCentered('Checking for conflicts...', ScreenHeight);
      ConflictsWereFound := CP.ConflictsFound;
    end;
  end;

  {$F+}

  function GetCommandName(Cmd : Word; UnitCode : Byte) : string;
    {-Get the name corresponding to Cmd}

    function WinChar : Char;
    begin
      WinChar := Char( Ord('1')+(Cmd-SelectWindow1) );
    end;

  begin
    GetCommandName := '';
    case Cmd of
      ccQuit           :
        case UnitCode of
          ucEntry      : GetCommandName := 'Cancel edits';
          ucMenu       : GetCommandName := 'Exit menu';
          else           GetCommandName := 'Close window';
        end;
      ccSelect         :
        case UnitCode of
          ucCalc       : GetCommandName := 'Accept number';
          ucEditor     : GetCommandName := 'New line';
          ucDir        : GetCommandName := 'Select directory';
          ucPick       : GetCommandName := 'Edit entry';
          ucEntry      : GetCommandName := 'Accept field';
        end;
      ccRestore        :
        case UnitCode of
          ucEditor     : GetCommandName := 'Restore line';
          ucDir        : GetCommandName := 'Clear file markers';
        end;
      ccHome           :
        case UnitCode of
          ucCal        : GetCommandName := 'Beginning of month';
          ucDir        : GetCommandName := 'Cursor to first file';
          ucPick       : GetCommandName := 'First entry';
        end;
      ccEnd            :
        case UnitCode of
          ucCal        : GetCommandName := 'End of month';
          ucDir        : GetCommandName := 'Cursor to last file';
          ucPick       : GetCommandName := 'Last entry';
        end;
      ccWordLeft       :
        case UnitCode of
          ucBrowse,
          ucQkRef      : GetCommandName := 'Cursor left 10 cols';
        end;
      ccWordRight      :
        case UnitCode of
          ucBrowse,
          ucQkRef      : GetCommandName := 'Cursor right 10 cols';
        end;
      ccTopOfFile      : if UnitCode = ucQkRef then
                           GetCommandName := 'Top of chart';
      ccEndOfFile      : if UnitCode = ucQkRef then
                           GetCommandName := 'End of chart';
      ccDel            : if UnitCode = ucDir then
                           GetCommandName := 'Deselect file';
      ccBlkToggle      : if UnitCode = ucDir then
                           GetCommandName := 'Mark/unmark all files';
      ccToggle         : if UnitCode = ucDir then
                           GetCommandName := 'Toggle file selection';
      ccTab            : if UnitCode = ucEntry then
                           GetCommandName := 'Next field';
      ccBackTab        : if UnitCode = ucEntry then
                           GetCommandName := 'Previous field';
      MoveWindowCmd    : GetCommandName := 'Move window';
      ResizeWindowCmd  : GetCommandName := 'Resize window';
      ZoomWindowCmd    : GetCommandName := 'Zoom window';
      ExitTsrCmd       : GetCommandName := 'Exit TSR';
      UnloadTsrCmd     : GetCommandName := 'Unload TSR';
      MainMenuCmd      : GetCommandName := 'Invoke main menu';
      LocalMenuCmd     : GetCommandName := 'Invoke local menu';
      NextWindowCmd    : GetCommandName := 'Next window';
      PrevWindowCmd    : GetCommandName := 'Previous window';
      SelectWindow1..SelectWindow8 :
                         GetCommandName := 'Select window '+WinChar;
      ShowMemoryCmd    : GetCommandName := 'Show available memory';
      ccInsertNumber   : GetCommandName := 'Insert number in editor';
      ccToggleMode     : GetCommandName := 'Toggle display mode';
      ccNewMask        :
        case UnitCode of
          ucPick       : GetCommandName := 'New phone book';
          ucDir        : GetCommandName := 'New file mask';
        end;
      ccDeleteItem     :
        case UnitCode of
          ucPick       : GetCommandName := 'Delete entry';
          ucDir        : GetCommandName := 'Delete file';
        end;
      ccInsertItem     : GetCommandName := 'Insert entry';
      ccCopyFiles      : GetCommandName := 'Copy marked files';
      ccMoveFiles      : GetCommandName := 'Move marked files';
      ccDeleteFiles    : GetCommandName := 'Delete marked files';
      ccRenameFile     : GetCommandName := 'Rename file';
      ccLoadEditor     : GetCommandName := 'Load file into editor';
      ccLoadBrowser    : GetCommandName := 'Load file into browser';
      ccDialPhone      : GetCommandName := 'Dial phone number';
      ccHangUpModem    : GetCommandName := 'Hang up modem';

      ccBlinkToggle    : GetCommandName := 'Toggle attribute blinking';

      ccIncMonth       : GetCommandName := 'Next month';
      ccDecMonth       : GetCommandName := 'Previous month';
      ccIncYear        : GetCommandName := 'Next year';
      ccDecYear        : GetCommandName := 'Previous year';
      ccToday          : GetCommandName := 'Move cursor to today';

      ccAdd            : GetCommandName := 'Add';
      ccSubtract       : GetCommandName := 'Subtract';
      ccMultiply       : GetCommandName := 'Multiply';
      ccDivide         : GetCommandName := 'Divide';
      ccHexA           : GetCommandName := 'Hex $A';
      ccHexB           : GetCommandName := 'Hex $B';
      ccHexC           : GetCommandName := 'Hex $C';
      ccHexD           : GetCommandName := 'Hex $D';
      ccHexE           : GetCommandName := 'Hex $E';
      ccHexF           : GetCommandName := 'Hex $F';
      ccBinaryMode     : GetCommandName := 'Binary mode';
      ccDecimalMode    : GetCommandName := 'Decimal mode';
      ccFloatMode      : GetCommandName := 'Floating point mode';
      ccExpMode        : GetCommandName := 'Exponential mode';
      ccAnd            : GetCommandName := 'And';
      ccShl            : GetCommandName := 'Shl';
      ccMod            : GetCommandName := 'Mod';
      ccNot            : GetCommandName := 'Not';
      ccOr             : GetCommandName := 'Or';
      ccShr            : GetCommandName := 'Shr';
      ccXor            : GetCommandName := 'Xor';
      ccClearAll       : GetCommandName := 'Clear all';
      ccClearEntry     : GetCommandName := 'Clear entry';
      ccInsertValue    : GetCommandName := 'Insert saved value';
      ccSaveValue      : GetCommandName := 'Save value';
      ccDecimalPt      : GetCommandName := 'Decimal point';
    end;
  end;

  procedure EditKeyPreSelect(SP : SelectorPtr; ID : Word);
    {-Pre-select routine}
  const
    Prompt =
      'Press <Enter> to select, <^R> to restore, <Esc> to exit';
  begin
    DisplayCentered(Prompt, ScreenHeight);
  end;

  procedure EditKeyProc(SP : SelectorPtr; ID : Word);
    {-Action routine}
  const
    Prompt =
      '<^C> Clear  <^R> Restore  <ScrollLock> Mode  <Enter> Accept  <Esc> Cancel';
  begin
    DisplayCentered(Prompt, ScreenHeight);

    KE.SetKeyRec(KS.FindKeyRec(ID)^);
    KE.Process;
    KE.Erase;

    EditKeyPreSelect(SP, ID);
  end;

  {$F-}

  procedure EditKeys(Title : string;
                     var Keys; MaxIndex : Word;
                     UnitCode : Byte;
                     DescCount : Word; var Descriptor);
    {-Edit keys}
  const
    WinOptions = wBordered+wClear+wUserContents+wFullMouseWindow;
  var
    Cmd      : Word;
    SaveUK   : UnpackedCmdTable;
    Finished : Boolean;
  begin
    if not KE.InitCustom(9, 12, 72, 12, dpiColors, WinOptions, UK[1]) then begin
      InsufficientMemory;
      Exit;
    end;

    KE.wFrame.AddShadow(shBR, shSeeThru);
    if KE.GetLastError <> 0 then begin
      InsufficientMemory;
      KE.Done;
      Exit;
    end;

    if not CP.Init(@Keys, ccUser55, MaxIndex, @UK, 3, False) then begin
      InsufficientMemory;
      KE.Done;
      Exit;
    end;

    if not KS.InitCustom(4, 4, 76, ScreenHeight-4, dpiColors, WinOptions,
                              CP, DescCount, Descriptor, UnitCode,
                              2, 14, 30, 45, 60) then begin
      InsufficientMemory;
      CP.Done;
      KE.Done;
      Exit;
    end;

    with KS do begin
      SetErrorProc(ErrorHandler);
      wFrame.SetFrameType(MainFrame);
      wFrame.AddShadow(shBR, shSeeThru);
      wFrame.AddHeader(' '+Title+' ', heTC);
      wFrame.AddCustomScrollBar(frRR, 1, MaxLongInt, 1, 1, '²', '°', dpiColors);
      SetWrapMode(StopAtEdges);

      SetModifiedAttr($1E, $07);
      SetCommandNameFunc(GetCommandName);
      SetActionProc(EditKeyProc);
      SetPreSelectProc(EditKeyPreSelect);

      if GetLastError <> 0 then begin
        KS.Done;
        CP.Done;
        KE.Done;
        Exit;
      end;

      SelectCommands.AddCommand(ccUser0, 1, Ord(^R), 0);

      SaveUK := UK;

      Finished := False;
      repeat
        Process;
        Cmd := GetLastCommand;
        case Cmd of
          ccUser0 :
            begin
              UK := SaveUK;
              ResetScreen;
            end;
          ccQuit, ccDone :
            if ConflictsWereFound then begin
              PositionOnFirstConflict;
              ResetScreen;
              Draw;
              Cmd := ccNone;
              ErrorMessage('Conflicts found');
            end
            else if not CP.PackKeys then begin
              ResetScreen;
              Cmd := ccNone;
              ErrorMessage('Keys won''t fit in installation area');
            end
            else
              Finished := True;
          ccError :
            Finished := True;
        end;
      until Finished;

      if CheckModifiedFlags(ccUser55*3) then
        Modified := True;

      SelectCommands.AddCommand(ccNone, 1, Ord(^R), 0);

      Erase;
      Done;
      CP.Done;
      KE.Done;
    end;
  end;

  {---------------------- main menu -----------------------}

  {$F+}

  procedure UpdateHelpLine(CurrentItem : MenuItemNodePtr; MPtr : MenuPtr);
    {-Update HelpLine for each menu item}
  const
    EKF : string[25] = 'Edit key assignments for ';
  var
    S : String;
  begin
    case CurrentItem^.inMenuKey of
      miBrowser     : S := EKF+'file browser';
      miCalculator  : S := EKF+'calculator';
      miCalendar    : S := EKF+'calendar';
      miEditor      : S := EKF+'editor';
      miFile        : S := EKF+'file manager';
      miMenus       : S := EKF+'menus';
      miQuickRef    : S := EKF+'quick reference chart';
      miPhoneBook   : S := EKF+'phone book';
      miPhoneEditor : S := EKF+'phone editor';
      miHotKeys     : S := 'Edit hotkey assignments';
      miColors      : S := 'Change colors';
      miOptions     : S := 'Edit configuration options';
      miQuit        : S := 'Save changes and quit';
    end;
    DisplayCentered(S, ScreenHeight);
  end;

  {$F-}

  procedure InitMainMenu;
    {-Initialize menu system}
  const
    {Frame constants}
    WO = wBordered+wClear+wUserContents;
  begin
    with MainMenu do begin
      if not InitCustom(31, 5, 47, 19, dpiColors, WO, Vertical) then begin
        WriteLn(emInsufficientMemory);
        Halt(1);
      end;
      mnOptionsOn(mnAlphaMatch+mnArrowSelect+mnAllHotSpots);
      mnOptionsOff(mnSelectOnMatch+mnAllowPending+mnPopOnSelect+mnUseItemForTopic);
      wFrame.SetFrameType(MainFrame);
      AddShadow(shBR, shSeeThru);
      AddItem('Browser',          01, 1, miBrowser);
      AddItem('Calculator',       02, 1, miCalculator);
      AddItem('Calendar',         03, 1, miCalendar);
      AddItem('Editor',           04, 1, miEditor);
      AddItem('File manager',     05, 1, miFile);
      AddItem('Menus',            06, 1, miMenus);
      AddItem('Quick reference',  07, 1, miQuickRef);
      AddItem('Phone book',       08, 1, miPhoneBook);
      AddItem('Phone editor',     09, 1, miPhoneEditor);
      AddItem('Hotkeys',          10, 1, miHotKeys);
      AddSeparator('Æ', 'Í', 'µ', 11);
      AddItem('Colors',           12, 1, miColors);
      AddItem('Options',          13, 1, miOptions);
      AddSeparator('Æ', 'Í', 'µ', 14);
      AddItem('Quit',             15, 1, miQuit);
      ItemsDone;

      if GetLastError <> 0 then begin
        WriteLn(emInsufficientMemory);
        Halt(1);
      end;

      SetErrorProc(ErrorHandler);

      SetCurrentItemProc(UpdateHelpLine)
    end;
  end;

  {---------------------- cloning -----------------------}

  procedure InitClonePrim(var ID : string; var FPos : LongInt);
    {-Primitive routine to locate an installation area}
  begin
    {locate the ID string}
    if not DPC.FindDefaultsEnd(ID, Length(ID)+1, 0) then begin
      WriteLn('ID string (', ID, ') not found');
      Halt(1);
    end;

    {skip over ID string}
    FPos := DPC.GetPos+Length(ID)+1;
  end;

  procedure OpenExeFile;
    {-Open the EXE file for installation}
  begin
    {locate the file}
    WriteLn('Locating ', ExeName, '...');
    if not ExistOnPath(ExeName, ExeName) then begin
      WriteLn(ExeName, ' not found');
      Halt(1);
    end;

    {open the file for cloning}
    if not DPC.Init(ExeName, UpdateDate) then begin
      WriteLn('Unable to open ', ExeName);
      Halt(1);
    end;

    {find the ID strings}
    WriteLn('Finding identification strings...');
    InitClonePrim(DeskPopID, DeskPos);
    InitClonePrim(TpUiKeyID, TpUiPos);
    InitClonePrim(BrowseKeyID, BrowsePos);
    InitClonePrim(CalKeyID, CalPos);
    InitClonePrim(CalcKeyID, CalcPos);
    InitClonePrim(EditorKeyID, EditorPos);
    InitClonePrim(EntryKeyID, EntryPos);
    InitClonePrim(QkRefKeyID, QkRefPos);
    InitClonePrim(MenuKeyID, MenuPos);
  end;

  procedure LoadPrim(FPos : LongInt; var Defaults; DefSize : Word);
    {-Primitive routine to load defaults for a unit}
  begin
    {load defaults}
    DPC.LoadDefaults(FPos, Defaults, DefSize);

    {check for errors}
    if DPC.GetLastError <> 0 then begin
      WriteLn('Error reading from ', ExeName);
      Halt(1);
    end;
  end;

  procedure LoadRange(FPos : LongInt; var DefStart, DefEnd1);
    {-Load a range of data}
  begin
    LoadPrim(FPos, DefStart, Ofs(DefEnd1)-Ofs(DefStart));
  end;

  procedure LoadExeDefaults;
    {-Load the default settings}
  begin
    LoadRange(DeskPos, HotKey1, DeskMainCfgEnd);
    LoadRange(TpUiPos, TpUiKeySet, TpUiCfgEnd);
    LoadRange(BrowsePos, BrowseKeySet, BrowseCfgEnd);
    LoadRange(CalPos, CalKeySet, CalCfgEnd);
    LoadRange(CalcPos, CalcKeySet, CalcCfgEnd);
    LoadRange(EditorPos, EditorKeySet, EditorCfgEnd);
    LoadRange(EntryPos, EntryKeySet, EntryCfgEnd);
    LoadRange(QkRefPos, QkRefKeySet, QkRefCfgEnd);
    LoadRange(MenuPos, MenuKeySet, MenuCfgEnd);
    {$IFDEF VirtualPascal}
    if ord(BrowseHexMode) > 1 then  // Should be 0 or 1, as boolean
      // DeskPop.Exe compiled with EXEPACK!  This is not allowed.
      begin
        Writeln( 'Error: Please re-link DESKPOP.EXE without exepacking' );
        Writeln( 'DPINST halted' );
        Halt(1);
      end;
    {$ENDIF}
  end;

  procedure StorePrim(FPos : LongInt; var Defaults; DefSize : Word);
    {-Primitive routine to store the packed commands for a unit}
  begin
    {store modified defaults}
    DPC.StoreDefaults(FPos, Defaults, DefSize);
  end;

  procedure StoreRange(FPos : LongInt; var DefStart, DefEnd1);
    {-Store a range of data}
  begin
    StorePrim(FPos, DefStart, Ofs(DefEnd1)-Ofs(DefStart));
  end;

  procedure UpdateExeFile;
    {-Store the new default settings}
  label
    ExitPoint;

    function CheckIoError : Boolean;
    begin
      if IoResult <> 0 then begin
        CheckIoError := False;
        ErrorMessage('Error writing to help file');
      end
      else
        CheckIoError := True;
    end;

  begin
    if not YesNo('Save changes to disk?', 'Y') then
      Exit;

    DisplayCentered('Storing new defaults...', ScreenHeight);
    StoreRange(DeskPos, HotKey1, DeskMainCfgEnd);
    StoreRange(TpUiPos, TpUiKeySet, TpUiCfgEnd);
    StoreRange(BrowsePos, BrowseKeySet, BrowseCfgEnd);
    StoreRange(CalPos, CalKeySet, CalCfgEnd);
    StoreRange(CalcPos, CalcKeySet, CalcCfgEnd);
    StoreRange(EditorPos, EditorKeySet, EditorCfgEnd);
    StoreRange(EntryPos, EntryKeySet, EntryCfgEnd);
    StoreRange(QkRefPos, QkRefKeySet, QkRefCfgEnd);
    StoreRange(MenuPos, MenuKeySet, MenuCfgEnd);

    {check for errors}
    if DPC.GetLastError <> 0 then
      ErrorMessage('Error writing to EXE file');

    {close the file}
    DPC.Done;

    DisplayCentered('Generating help file...', ScreenHeight);
    if not OpenHelpFile(ExeName) then begin
      ErrorMessage('Unable to open help file'); {!!.01}
      Exit;                                     {!!.01}
    end;

    WriteHelp('Browser', BrowseKeySet, BrowseKeyMax, UK, CP, ucBrowse,
              BrowseCount, BrowseDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('Calculator', CalcKeySet, CalcKeyMax, UK, CP, ucCalc,
              CalcCount, CalcDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('Calendar', CalKeySet, CalKeyMax, UK, CP, ucCal,
              CalCount, CalDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('Editor', EditorKeySet, EditorKeyMax, UK, CP, ucEditor,
              EditorCount, EditorDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('File Manager', DirKeySet, DirKeyMax, UK, CP, ucDir,
              DirCount, DirDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('Menus', MenuKeySet, MenuKeyMax, UK, CP, ucMenu,
              MenuCount, MenuDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('Quick Reference Chart', QkRefKeySet, QkRefKeyMax, UK, CP,
              ucQkRef, QkRefCount, QkRefDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('Phone Book', PhoneKeySet, PhoneKeyMax, UK, CP, ucPick,
              PhoneCount, PhoneDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('Phone Editor', EntryKeySet, EntryKeyMax, UK, CP, ucEntry,
              EntryCount, EntryDescriptor, GetCommandName);
    if not CheckIoError then
      goto ExitPoint;

    WriteHelp('Hotkeys', TpuiKeySet, TpuiKeyMax, UK, CP, ucNone,
              HotKeyCount, HotKeyDescriptor, GetCommandName);
    if not CheckIoError then ;

ExitPoint:
    CloseHelpFile;
  end;

  procedure InitScreen;
    {-Initialize screen, mouse stuff}
  begin
    {clear the screen}
    TextChar := BackdropChar;
    TextAttr := BackdropAttr;
    ClrScr;

    {$IFDEF UseMouse}
    if MouseInstalled then
      with dpiColors do begin
        {activate mouse cursor}
        SoftMouseCursor($0000, (ColorMono(MouseColor, MouseMono) shl 8)+
                               Byte(MouseChar));
        ShowMouse;

        {enable mouse support}
        MenuCommands.cpOptionsOn(cpEnableMouse);
        EntryCommands.cpOptionsOn(cpEnableMouse);
        SelectCommands.cpOptionsOn(cpEnableMouse);
        PickCommands.cpOptionsOn(cpEnableMouse);
      end;
    {$ENDIF}

    {reassign <Esc> and <ClickRight> to ccDone in entry screens}
    EntryCommands.AddCommand(ccDone, 1, $001B, 0);
    {$IFDEF UseMouse}
    EntryCommands.AddCommand(ccDone, 1, $EE00, 0);
    {$ENDIF}
  end;

begin
  {read in configuration data}
  OpenExeFile;
  LoadExeDefaults;

  {initialize mouse, screen stuff}
  InitScreen;

  {initialize the main menu}
  InitMainMenu;

  with MainMenu do begin

    repeat
      {display copyright notice at top of screen}
      DisplayCopyright;

      {get a menu selection}
      Process;

      Cmd := GetLastCommand;
      if Cmd = ccSelect then
        case MenuChoice of
          miBrowser :
            EditKeys('Browser', BrowseKeySet, BrowseKeyMax,
                     ucBrowse, BrowseCount, BrowseDescriptor);
          miCalculator :
            EditKeys('Calculator', CalcKeySet, CalcKeyMax,
                     ucCalc, CalcCount, CalcDescriptor);
          miCalendar :
            EditKeys('Calendar', CalKeySet, CalKeyMax,
                     ucCal, CalCount, CalDescriptor);
          miEditor :
            EditKeys('Editor', EditorKeySet, EditorKeyMax,
                     ucEditor, EditorCount, EditorDescriptor);
          miFile :
            EditKeys('File Manager', DirKeySet, DirKeyMax,
                     ucDir, DirCount, DirDescriptor);
          miMenus :
            EditKeys('Menus', MenuKeySet, MenuKeyMax,
                     ucMenu, MenuCount, MenuDescriptor);
          miQuickRef :
            EditKeys('Quick Reference Chart', QkRefKeySet, QkRefKeyMax,
                     ucQkRef, QkRefCount, QkRefDescriptor);
          miPhoneBook :
            EditKeys('Phone Book', PhoneKeySet, PhoneKeyMax,
                     ucPick, PhoneCount, PhoneDescriptor);
          miPhoneEditor :
            EditKeys('Phone Editor', EntryKeySet, EntryKeyMax,
                     ucEntry, EntryCount, EntryDescriptor);
          miHotKeys :
            EditKeys('Hotkeys', TpuiKeySet, TpuiKeyMax,
                     ucNone, HotKeyCount, HotKeyDescriptor);
          miColors :
            begin
              Erase;
              EditColors;
            end;
          miOptions :
            EditOptions;
          miQuit :
            begin
              if Modified then
                UpdateExeFile;
              Cmd := ccDone;
            end;
        end;

    until (Cmd = ccDone) or (Cmd = ccError);

    {$IFDEF UseMouse}
    HideMouse;
    {$ENDIF}

    Erase;

    ClrScr;
    NormalCursor;
  end;
end.
