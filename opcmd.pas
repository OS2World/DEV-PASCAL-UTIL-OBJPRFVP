{$S-,R-,V-,I-,B-,F-,O-,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                     OPCMD.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpCmd;
  {-Convert keystrokes to commands}

interface

uses
  Use32,
  OpConst,  {!!.20}
  OpInline,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpString, {!!.03}
  OpRoot;

  {.F-}
const
  ccNone        = 000; {Not a command}
  ccChar        = 001; {A regular character}
  ccCtrlChar    = 002; {Accept control character}
  ccSelect      = 003; {Select/accept item or field}
  ccQuit        = 004; {Restore default string and quit}
  ccError       = 005; {Quit due to I/O error or out-of-memory}
  ccMouseSel    = 006; {Mouse select}
  ccRestore     = 007; {Restore default and continue}
  ccHome        = 008; {Cursor to beginning of line}
  ccEnd         = 009; {Cursor to end of line}
  ccLeft        = 010; {Cursor left by one character}
  ccRight       = 011; {Cursor right by one character}
  ccUp          = 012; {Up}
  ccDown        = 013; {Down}
  ccScrollUp    = 014; {Scroll up}
  ccScrollDn    = 015; {Scroll down}
  ccWordLeft    = 016; {Cursor left one word}
  ccWordRight   = 017; {Cursor right one word}
  ccPageUp      = 018; {Page up}
  ccPageDn      = 019; {Page down}
  ccScreenTop   = 020; {Top of screen}
  ccScreenBot   = 021; {Bottom of screen}
  ccTopOfFile   = 022; {Top of file}
  ccEndOfFile   = 023; {End of file}
  ccBack        = 024; {Backspace one character}
  ccDel         = 025; {Delete current character}
  ccDelEol      = 026; {Delete from cursor to end of line}
  ccDelBol      = 027; {Delete from beginning of line to cursor}
  ccDelLine     = 028; {Delete entire line}
  ccDelWord     = 029; {Delete word to right of cursor}
  ccIns         = 030; {Toggle insert mode}
  ccHelp        = 031; {Invoke help routine}
  ccTab         = 032; {Next tab stop}
  ccBackTab     = 033; {Previous tab stop}

  {special OPMEMO commands}
  ccIndent      = 034; {Toggle indent mode}
  ccWordWrap    = 035; {Toggle word wrap}
  ccReformatP   = 036; {Reformat paragraph}
  ccReformatG   = 037; {Global reformat}

  {special OPENTRY commands}
  ccIncChoice   = 038; {increment choice}
  ccDecChoice   = 039; {decrement choice}
  ccNextField   = 040; {Next field in order}
  ccPrevField   = 041; {Previous field in order}
  ccNextRec     = 042; {Next record}
  ccPrevRec     = 043; {Previous record}
  ccFirstFld    = 044; {First field}
  ccLastFld     = 045; {Last field}
  ccNested      = 046; {exit for nested form}
  ccDone        = 047; {completely finished with editing}

  {special commands generated internally--should not be assigned to keys}
  ccClickExit   = 048; {double mouse click produces an exit}
  ccAutoAdvance = 049; {auto advance to next field}
  ccExitAtTop   = 050; {exit at top edge of window}
  ccExitAtBot   = 051; {exit at bottom edge of window}
  ccExitLeft    = 052; {exit at left edge of window}
  ccExitRight   = 053; {exit at right edge of window}
  ccAltKey      = 054; {Alt key in special char set pressed} {!!.03}

  {special OPHELP commands}
  ccPrevTopic   = 055; {Display the most recent help topic}
  ccIndex       = 056; {Display an index of all help topics}
  ccNextXref    = 057; {Jump to the next cross-reference}
  ccPrevXref    = 058; {Jump to the previous cross-reference}

  {special OPBROWSE/OPEDITOR commands}
  ccSetMark0    = 060; {set marker 0..9}
  ccSetMark1    = 061;
  ccSetMark2    = 062;
  ccSetMark3    = 063;
  ccSetMark4    = 064;
  ccSetMark5    = 065;
  ccSetMark6    = 066;
  ccSetMark7    = 067;
  ccSetMark8    = 068;
  ccSetMark9    = 069;
  ccJmpMark0    = 070; {jump to marker 0..9}
  ccJmpMark1    = 071;
  ccJmpMark2    = 072;
  ccJmpMark3    = 073;
  ccJmpMark4    = 074;
  ccJmpMark5    = 075;
  ccJmpMark6    = 076;
  ccJmpMark7    = 077;
  ccJmpMark8    = 078;
  ccJmpMark9    = 079;
  ccMarkToggle  = 080; {toggle marker display}
  ccBlkBegin    = 081; {mark start of block}
  ccBlkEnd      = 082; {mark end of block}
  ccBlkBottom   = 083; {mark end of block (bottom of screen)}
  ccBlkToggle   = 084; {toggle block display}
  ccBlkRead     = 085; {read block}
  ccBlkWrite    = 086; {write block}
  ccBlkCopy     = 087; {copy block}
  ccBlkMove     = 088; {move block}
  ccBlkDelete   = 089; {delete block}
  ccBlkWord     = 090; {mark current word as block}
  ccBlkUCase    = 091; {change characters in block to upper case}
  ccBlkLCase    = 092; {change characters in block to lower case}
  ccBlkTCase    = 093; {toggle case of characters in block}
  ccBlkIndent   = 094; {indent marked block}
  ccBlkUnindent = 095; {unindent marked block}
  ccSetIndent   = 096; {set indentation level for block un/indent}
  ccPrevPos     = 097; {cursor to previous position}
  ccJmpBegin    = 098; {jump to block begin}
  ccJmpEnd      = 099; {jump to block end}
  ccJmpLine     = 100; {jump to line}
  ccInsertLine  = 101; {insert line at cursor}
  ccCenterLine  = 102; {center current line}
  ccSearch      = 103; {search}
  ccReplace     = 104; {search and replace}
  ccReSearch    = 105; {search again}
  ccHexMode     = 106; {toggle hex mode}
  ccTabExpand   = 107; {toggle tab expansion}
  ccTabToggle   = 108; {toggle smart tabs/fixed tabs}
  ccTabSize     = 109; {set size of fixed tabs}
  ccRtMargin    = 110; {set right margin}
  ccStripHigh   = 111; {toggle stripping of high bits}
  ccNewFile     = 112; {read a new file}
  ccSaveFile    = 113; {save a file}
  ccSaveNamed   = 114; {save a file under a new name}
  ccSaveExit    = 115; {save file and exit}
  ccSaveSwitch  = 116; {save file and load new one}
  ccAbandonFile = 117; {abandon file}
  ccBlkPrint    = 118; {print marked block}

  {special OPPICK multi-item selection commands}
  ccToggle      = 119; {toggle selection of an item}

  {special OPMENU submenu selection commands}
  ccSubPrev     = 120; {move to previous submenu}
  ccSubNext     = 121; {move to next submenu}
  ccSubQuit     = 122; {exit current submenu}

  {diagonal cursor movement commands}
  ccUpLeft      = 130; {cursor up and to the left}
  ccDownLeft    = 131; {cursor down and to the left}
  ccUpRight     = 132; {cursor up and to the right}
  ccDownRight   = 133; {cursor down and to the right}

  {mouse down commands, used by OPDRAG}
  ccMouseDown   = 134; {left mouse button pressed (not released)}   {!!.03}
  ccMouseAuto   = 135; {left button down: autorepeat or mouse move} {!!.03}

  {special OPDIALOG commands}                                       {!!.11}
  ccItemChange  = 140; {cluster item selected/changed}              {!!.11}

  {special OPEDITOR commands}                                       {!!.30}
  ccBlkAppend   = 141;                                              {!!.30}

  ccUser0       = 200; {user-defined exit commands}
  ccUser1       = 201;
  ccUser2       = 202;
  ccUser3       = 203;
  ccUser4       = 204;
  ccUser5       = 205;
  ccUser6       = 206;
  ccUser7       = 207;
  ccUser8       = 208;
  ccUser9       = 209;
  ccUser10      = 210;
  ccUser11      = 211;
  ccUser12      = 212;
  ccUser13      = 213;
  ccUser14      = 214;
  ccUser15      = 215;
  ccUser16      = 216;
  ccUser17      = 217;
  ccUser18      = 218;
  ccUser19      = 219;
  ccUser20      = 220;
  ccUser21      = 221;
  ccUser22      = 222;
  ccUser23      = 223;
  ccUser24      = 224;
  ccUser25      = 225;
  ccUser26      = 226;
  ccUser27      = 227;
  ccUser28      = 228;
  ccUser29      = 229;
  ccUser30      = 230;
  ccUser31      = 231;
  ccUser32      = 232;
  ccUser33      = 233;
  ccUser34      = 234;
  ccUser35      = 235;
  ccUser36      = 236;
  ccUser37      = 237;
  ccUser38      = 238;
  ccUser39      = 239;
  ccUser40      = 240;
  ccUser41      = 241;
  ccUser42      = 242;
  ccUser43      = 243;
  ccUser44      = 244;
  ccUser45      = 245;
  ccUser46      = 246;
  ccUser47      = 247;
  ccUser48      = 248;
  ccUser49      = 249;
  ccUser50      = 250;
  ccUser51      = 251;
  ccUser52      = 252;
  ccUser53      = 253;
  ccUser54      = 254;
  ccUser55      = 255; {** reserved for internal use **}
  {...}
  ccUser65335   = 65535; {values > ccUser55 valid only if using word commands}
{.F+}

  {option codes}
  cpMapWordStar    = $0001; {WordStar mapping on?}
  cpEnableMouse    = $0002; {mouse support enabled?}
  cpUpcase         = $0004; {convert keys to uppercase before evaluating}
  cpWordCmds       = $0008; {commands are words rather than bytes}
  cpSwitchPriority = $0010; {give secondary commands priority when searching?}
  cpCtrlsAreChars  = $0020; {treat unassigned control chars as regular chars}
  cpMouseDrag      = $0040; {used by OPDRAG unit} {!!.03}
  cpDoingChars     = $2000; {for internal use}
  cpDeallocP       = $4000; {for internal use}
  cpDeallocS       = $8000; {for internal use}

  DefCmdOptions : Word = cpMapWordStar;
  BadCmdOptions : Word = cpDoingChars+cpDeallocP+cpDeallocS;

type
  MatchType = (NoMatch, PartMatch, FullMatch);
  CmdTable = array[0..32000] of Byte;
  CmdTablePtr = ^CmdTable;
  GetKeyProc = function : Word;
  KeyPressedProc = function : Boolean;
  GetHelpProc = procedure (UnitCode : Byte; IdPtr : Pointer; HelpIndex : Word);
  CommandProcessorPtr = ^CommandProcessor;
  UserHookProc = procedure (CPP : CommandProcessorPtr; MT : MatchType; Key : Word);
  UserMatchFunc = function (    CPP : CommandProcessorPtr;     {!!.30}
                            var Key, Cmd : Word) : MatchType;  {!!.30}
  CommandProcessor =
    object(Root)
      cpCmdList    : Pointer;      {list of commands to be auto-executed}
      cpCmdsLeft   : Word;         {number left to be auto-executed}
      cpOptions    : Word;         {option flags}
      cpPLast      : Word;         {index of last element in primary table}
      cpSLast      : Word;         {index of last element in secondary table}
      cpPKeys      : CmdTablePtr;  {pointer to the primary key table}
      cpSKeys      : CmdTablePtr;  {pointer to the secondary key table}
      cpGetKeyProc : GetKeyProc;   {function to get a key}
      cpKeyPressedProc : KeyPressedProc; {returns True if key pressed}
      cpGetHelpProc : GetHelpProc; {help procedure}
      cpUserHookProc : UserHookProc; {called by GetCommand for each key evaluated}
      cpUserMatchFunc : UserMatchFunc; {allow user to match keystroke} {!!.30}
      cpError      : Word;         {last error}
      cpAltKeys    : ^CharSet;     {pointer to char set for Alt keys} {!!.03}
      {...methods...}
      constructor Init(KeysPtr : CmdTablePtr; MaxIndex : Word);
        {-Initialize a command processor with only one command table}
      constructor InitCustom(PrimaryKeys : CmdTablePtr;
                             PrimaryMaxIndex : Word;
                             SecondaryKeys : CmdTablePtr;
                             SecondaryMaxIndex : Word;
                             Options : Word);
        {-Initialize a command processor with custom options}
      destructor Done; virtual;
        {-Dispose of any memory that was allocated by the command processor}
      {...}
      function GetCommand(var Key : Word) : Word;
        {-Get next command or character}
      procedure AddCommand(Cmd : Word; NumKeys : Byte; Key1, Key2 : Word);
        {-Add a new primary command key assignment or change an existing one}
      procedure AddSecondaryCommand(Cmd : Word; NumKeys : Byte; Key1, Key2 : Word);
        {-Add a new secondary command key assignment or change an existing one}
      function GetLastError : Word;
        {-Returns last error code and clears it}
      {...}
      procedure SetGetKeyProc(GKP : GetKeyProc);
        {-Change the cpGetKey routine}
      procedure SetKeyPressedProc(KPP : KeyPressedProc);
        {-Change the cpKeyPressed routine}
      procedure SetHelpProc(GHP : GetHelpProc);
        {-Change the HelpProc field}
      procedure SetUserHookProc(UHP : UserHookProc);
        {-Specify a routine to be called each time a keystroke is evaluated}
      procedure SetUserMatchFunc(UMF : UserMatchFunc);                   {!!.30}
        {-Assign routine which allows user to match keystroke to command}{!!.30}
      {...}
      procedure cpOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure cpOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function cpOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return true if all specified options are on}
      {...}
      function cpGetKey : Word; virtual;
        {-Called to get next keystroke}
      function cpKeyPressed : Boolean; virtual;
        {-Called to determine if a key has been pressed}
      procedure cpGetHelp(UnitCode : Byte; IdPtr : Pointer; HelpIndex : Word); virtual;
        {-Called when help is requested}
      procedure cpUserHook(MT : MatchType; Key : Word); virtual;
        {-Called each time a keystroke is processed}
      function  cpUserMatch(var Key, Cmd : Word) : MatchType; virtual;  {!!.30}
        {-Allow user to match keystroke to command}                     {!!.30}
      {...}
      procedure SetKeyPtr(KeysPtr : CmdTablePtr; MaxIndex : Word);
        {-Change the primary command table}
      function GetKeyPtr(var MaxIndex : Word) : CmdTablePtr;
        {-Return a pointer to the primary command table}
      procedure SetSecondaryKeyPtr(KeysPtr : CmdTablePtr; MaxIndex : Word);
        {-Change the secondary command table}
      function GetSecondaryKeyPtr(var MaxIndex : Word) : CmdTablePtr;
        {-Return a pointer to the secondary command table}
      procedure SetAltKeySet(var CS : CharSet);  {!!.03}
        {-Specify set of Alt keys to recognize}  {!!.03}
      {...}
      procedure GetKeyAssignment(Cmd : Word; var NumKeys : Byte;
                                 var Key1, Key2 : Word);
        {-Return one set of key(s) assigned to the specified command.
          NumKeys = 0 if none found.}
      function GetCmdAssignment(NumKeys : Byte; Key1, Key2 : Word;
                                var Primary : Boolean) : Word;
        {-Return command associated with specified key combination, or ccNone
          if none found. Primary is False only if match found in secondary
          key set.}
      function CheckForConflict(NumKeys : Byte; Key1, Key2 : Word) : MatchType;
        {-Check to see if the specified key combination conflicts with an
          existing one}
      {...}
      procedure SetCommandList(CmdList : Pointer; NumCmds : Word);
        {-Specify a list of commands to be auto-executed}
      function CommandStringPending : Boolean;
        {-Returns True if a text string is pending in the list of
          auto-executed commands}
      function GetCommandString : string;
        {-Returns a text string when CommandStringPending is True}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a command processor from a stream}
      procedure Store(var S : IdStream);
        {-Store a command processor in a stream}
    {$ENDIF}
      function MouseEnabled : Boolean;
        {-Return True if mouse support enabled}
{.Z+}
      {+++ internal methods +++}
      procedure cpEnableMouseSupport;
      procedure cpAddPrim(KAP : CmdTablePtr; Last, Cmd : Word;
                          NumKeys : Byte; Key1, Key2 : Word);
      function cpCmdPeek : Word;
      function cpNextCmd(var Key : Word) : Word;
      procedure cpFixPriority(var Changed : Boolean);
{.Z-}
    end;

  {--- the following routines, etc. are for installation programs ---}

const
  MaxCommands = 255*3;
  KeyLength = 6;
type
  KeyString = string[KeyLength];
  KeyRecPtr = ^KeyRec;
  KeyRec =
    record
      Modified : Boolean;
      Conflict : Boolean;
      CmdCode : Word;
      Keys : KeyString;
    end;
  UnpackedCmdTable = array[1..MaxCommands] of KeyRec;
  UnpackedCmdPtr = ^UnpackedCmdTable;
  CommandPacker =
    object(Root)
      cpPKeys : CmdTablePtr;    {pointer to packed command table}
      cpUKeys : UnpackedCmdPtr; {pointer to unpacked command table}
      cpCols : Byte;            {number of columns of commands in cpUKeys^}
      cpMaxCmd : Word;          {highest command in cpPKeys^ * cpCols}
      cpMaxIndex : Word;        {last index into cpPKeys^}
      cpUseWords : Boolean;     {if True, commands are words, not bytes}
      {...methods...}
      constructor Init(PKeys : CmdTablePtr;
                       MaxCmd : Word;
                       MaxIndex : Word;
                       UKeys : UnpackedCmdPtr;
                       Columns : Byte;
                       CmdsAreWords : Boolean);
        {-Initialize pointers to packed/unpacked command tables}
      function PackKeys : Boolean;
        {-Convert unpacked array into a packed list of keys again. Returns
          False if keys won't all fit.}
      function SizeKeys : Word;
        {-Return number of bytes in packed version of UnpackedKeys}
      function GetKeyRecPtr(Cmd : Word; Col : Byte) : KeyRecPtr;
        {-Returns a pointer to the KeyRec for the Col'th instance of Cmd}
      function ConflictsFound : Boolean;
        {-Check unpacked commands for conflicts. Returns True if conflicts found}
{.Z+}
      {+++ internal methods +++}
      procedure cpUnpack;
{.Z-}
    end;

  {--- other types ---}
const
  BellPitch     : Word = 700;
  BellDuration  : Word = 100;

procedure SetBell(Pitch, Duration : Word);
  {-Set pitch and duration for bell}

procedure RingBell;
  {-Ring the bell}

procedure DefaultErrorProc(UnitCode : Byte; var ErrorCode : Word; ErrorMsg : string);
  {-Default error handler--just beeps}

{.Z+}
{$IFDEF UseStreams}
procedure CommandProcessorStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing command processors}
{$ENDIF}
{.Z-}

  {======================================================}

implementation

type
  CmdBuffArray = array[0..5] of Byte;

  procedure SetBell(Pitch, Duration : Word);
    {-Set pitch and duration for bell}
  begin
    BellPitch := Pitch;
    BellDuration := Duration;
  end;

  procedure RingBell;
    {-Ring the bell}
  begin
    {$IFDEF VIRTUALPASCAL}
    PlaySound( BellPitch, BellDuration );
    {$ELSE}
    Sound(BellPitch);
    Delay(BellDuration);
    NoSound;
    {$ENDIF}
  end;

  procedure DefaultErrorProc(UnitCode : Byte; var ErrorCode : Word; ErrorMsg : string);
    {-Default error handler--just beeps}
  begin
    if ErrorCode <> 0 then
      RingBell;
  end;

  function WordStarCommand(K : Byte) : Byte;
    {-Return ^C, 'C', or 'c' as ^C, etc.}
  var
    C : Char absolute K;
  begin
    C := Upcase(C);
    case C of
      'A'..'_' :
        WordStarCommand := K-64;
      else
        WordStarCommand := K;
    end;
  end;

  function CommandAt(var Location; UseWords : Boolean) : Word;
    {-Return the command at Location}
  var
    WC : Word absolute Location;
    BC : Byte absolute Location;
  begin
    if UseWords then
      CommandAt := WC
    else
      CommandAt := BC;
  end;

  procedure SetCommand(Cmd : Word; var Location; UseWords : Boolean);
    {-Set the command at Location to Cmd}
  var
    WC : Word absolute Location;
    BC : Byte absolute Location;
  begin
    if UseWords then
      WC := Cmd
    else
      BC := Lo(Cmd);
  end;

  function ScanCommands(K : CmdTablePtr;
                        var CmdBuffer : CmdBuffArray;
                        BufNext : Word;
                        var Cmd : Word;
                        var FoundAt : Word;
                        UseWords : Boolean) : MatchType;
    {-Scan K^ for a match on CmdBuffer}
  var
    BufIndex : Word;
    CmdIndex : Word;
    CmdLen : Byte;
    Matching : Boolean;
    D : Word;
  begin
    Cmd := ccNone;
    CmdIndex := 0;
    if K = nil then
      CmdLen := 0
    else
      CmdLen := K^[CmdIndex];

    {Scan the command list}
    D := Ord(UseWords)+1;
    while CmdLen <> 0 do begin
      FoundAt := CmdIndex;
      Inc(CmdIndex);
      BufIndex := 0;
      Matching := True;
      while Matching and (BufIndex < BufNext) and (BufIndex < CmdLen-D) do
        if CmdBuffer[BufIndex] = K^[CmdIndex+BufIndex] then
          Inc(BufIndex)
        else
          Matching := False;
      if not Matching then begin
        {No match, try next command}
        Inc(CmdIndex, CmdLen);
        CmdLen := K^[CmdIndex];
      end
      else begin
        if BufNext = CmdLen-D then begin
          {Complete match}
          ScanCommands := FullMatch;
          Cmd := CommandAt(K^[CmdIndex+BufIndex], UseWords);
        end
        else
          ScanCommands := PartMatch;
        Exit;
      end;
    end;

    {No match if we get here}
    ScanCommands := NoMatch;
  end;

  procedure InitCmdBuffer(var CmdBuffer : CmdBuffArray;
                          NumKeys       : Byte;
                          Key1, Key2    : Word;
                          var BufNext   : Word);
    {-Initialize a CmdBuffArray}
  begin
    if Lo(Key1) = 0 then begin
      CmdBuffer[0] := 0;
      CmdBuffer[1] := Hi(Key1);
      BufNext := 2;
    end
    else begin
      CmdBuffer[0] := Lo(Key1);
      BufNext := 1;
    end;
    if NumKeys = 2 then
      if Lo(Key2) = 0 then begin
        CmdBuffer[BufNext] := 0;
        Inc(BufNext);
        CmdBuffer[BufNext] := Hi(Key2);
        Inc(BufNext);
      end
      else begin
        CmdBuffer[BufNext] := Lo(Key2);
        Inc(BufNext);
      end;
  end;

  constructor CommandProcessor.Init(KeysPtr : CmdTablePtr; MaxIndex : Word);
    {-Initialize a command processor}
  begin
    if not CommandProcessor.InitCustom(KeysPtr, MaxIndex, nil, 0,
                                       DefCmdOptions) then
      Fail;
  end;

  constructor CommandProcessor.InitCustom(PrimaryKeys : CmdTablePtr;
                                          PrimaryMaxIndex : Word;
                                          SecondaryKeys : CmdTablePtr;
                                          SecondaryMaxIndex : Word;
                                          Options : Word);
    {-Initialize a command processor with custom options}
  begin
    Root.Init;
    cpPKeys := PrimaryKeys;
    cpPLast := PrimaryMaxIndex;
    cpSKeys := SecondaryKeys;
    if SecondaryKeys = nil then
      cpSLast := 0
    else
      cpSLast := SecondaryMaxIndex;
    cpOptions := Options;
    cpCmdList := nil;
    cpCmdsLeft := 0;
    @cpUserHookProc := nil;
    @cpGetHelpProc := nil;
    cpError := 0;
    cpEnableMouseSupport;
    cpAltKeys := nil;        {!!.03}
    @cpUserMatchFunc := nil; {!!.30}
  end;

  destructor CommandProcessor.Done;
    {-Dispose of any memory that was allocated by the command processor}
  begin
    if FlagIsSet(cpOptions, cpDeallocP) then
      FreeMemCheck(cpPKeys, cpPLast+1);
    if FlagIsSet(cpOptions, cpDeallocS) then
      FreeMemCheck(cpSKeys, cpSLast+1);
  end;

  procedure CommandProcessor.cpEnableMouseSupport;
    {-Enable or disable mouse support as appropriate}
  begin
    {$IFDEF UseMouse}
      if MouseInstalled and FlagIsSet(cpOptions, cpEnableMouse) then begin
        EnableEventHandling;
        cpGetKeyProc := ReadKeyOrButton;
        cpKeyPressedProc := KeyOrButtonPressed;
      end
      else begin
        cpGetKeyProc := ReadKeyWord;
        cpKeyPressedProc := KeyPressed;
      end;
    {$ELSE}
      ClearFlag(cpOptions, cpEnableMouse);
      cpGetKeyProc := ReadKeyWord;
      cpKeyPressedProc := KeyPressed;
    {$ENDIF}
  end;

  procedure CommandProcessor.cpOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    SetFlag(cpOptions, OptionFlags and not BadCmdOptions);
    if FlagIsSet(OptionFlags, cpEnableMouse) then
      cpEnableMouseSupport;
  end;

  procedure CommandProcessor.cpOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    ClearFlag(cpOptions, OptionFlags and not BadCmdOptions);
    if FlagIsSet(OptionFlags, cpEnableMouse) then
      cpEnableMouseSupport;
  end;

  function CommandProcessor.cpOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return true if all specified options are on}
  begin
    cpOptionsAreOn := (cpOptions and OptionFlags) = OptionFlags;
  end;

  procedure CommandProcessor.SetGetKeyProc(GKP : GetKeyProc);
    {-Change the cpGetKey routine}
  begin
    cpGetKeyProc := GKP;
  end;

  procedure CommandProcessor.SetKeyPressedProc(KPP : KeyPressedProc);
    {-Change the cpKeyPressed routine}
  begin
    cpKeyPressedProc := KPP;
  end;

  procedure CommandProcessor.SetHelpProc(GHP : GetHelpProc);
    {-Change the HelpProc field}
  begin
    cpGetHelpProc := GHP;
  end;

  procedure CommandProcessor.SetUserHookProc(UHP : UserHookProc);
    {-Specify a routine to be called each time a keystroke is evaluated}
  begin
    cpUserHookProc := UHP;
  end;

  {!!.30 - New}
  procedure CommandProcessor.SetUserMatchFunc(UMF : UserMatchFunc);
  begin
    cpUserMatchFunc := UMF;
  end;

  function CommandProcessor.MouseEnabled : Boolean;
    {-Return True if mouse support enabled}
  begin
    MouseEnabled := FlagIsSet(cpOptions, cpEnableMouse);
  end;

  procedure CommandProcessor.SetKeyPtr(KeysPtr : CmdTablePtr; MaxIndex : Word);
    {-Change the primary command table}
  begin
    cpPKeys := KeysPtr;
    cpPLast := MaxIndex;
  end;

  function CommandProcessor.GetKeyPtr(var MaxIndex : Word) : CmdTablePtr;
    {-Return a pointer to the primary command table}
  begin
    GetKeyPtr := cpPKeys;
    MaxIndex := cpPLast;
  end;

  procedure CommandProcessor.SetSecondaryKeyPtr(KeysPtr : CmdTablePtr;
                                                MaxIndex : Word);
    {-Change the secondary command table}
  begin
    cpSKeys := KeysPtr;
    if KeysPtr = nil then
      cpSLast := 0
    else
      cpSLast := MaxIndex;
  end;

  function CommandProcessor.GetSecondaryKeyPtr(var MaxIndex : Word) : CmdTablePtr;
    {-Return a pointer to the secondary command table}
  begin
    GetSecondaryKeyPtr := cpSKeys;
    MaxIndex := cpSLast;
  end;

  procedure CommandProcessor.SetAltKeySet(var CS : CharSet);  {!!.03}
    {-Specify set of Alt keys to recognize}
  begin
    cpAltKeys := @CS;
  end;

  function CommandProcessor.cpGetKey : Word;
    {-Called to get next keystroke}
  begin
    cpGetKey := cpGetKeyProc;
  end;

  function CommandProcessor.cpKeyPressed : Boolean;
    {-Called to determine if a key has been pressed}
  begin             {vvvvvvvvvvvvvvvvvv} {!!.01}
    cpKeyPressed := (cpCmdsLeft > 0) or cpKeyPressedProc;
  end;

  procedure CommandProcessor.cpGetHelp(UnitCode : Byte; IdPtr : Pointer; HelpIndex : Word);
    {-Called when help is requested}
  begin
    if @cpGetHelpProc <> nil then
      cpGetHelpProc(UnitCode, IdPtr, HelpIndex);
  end;

  procedure CommandProcessor.cpUserHook(MT : MatchType; Key : Word);
    {-Called each time a keystroke is processed}
  begin
    if @cpUserHookProc <> nil then
      cpUserHookProc(@Self, MT, Key);
  end;

  {!!.30 - New}
  function CommandProcessor.cpUserMatch(var Key, Cmd : Word) : MatchType;
  begin
    if (@cpUserMatchFunc <> nil) then
      cpUserMatch := cpUserMatchFunc(@Self, Key, Cmd)
    else
      cpUserMatch := NoMatch;
  end;

  procedure CommandProcessor.cpFixPriority(var Changed : Boolean);
    {-Adjust searching priority temporarily}
  begin
    if FlagIsSet(cpOptions, cpSwitchPriority) and (cpSKeys <> nil) then begin
      ExchangeWords(cpPLast, cpSLast);
      ExchangeStructs(cpPKeys, cpSKeys, SizeOf(Pointer));
      Changed := True;
    end
    else
      Changed := False;
  end;

  function CommandProcessor.GetCommand(var Key : Word) : Word;
    {-Get next command or character. Returns ccNone for no matching command,
      ccChar for alphabetic character.}
  var
    KeyCh : Char absolute Key;
    LCh : Byte;
    Ch : Char absolute LCh;
    Cmd : Word;
    BufNext : Word;
    Dummy, Finished : Boolean;
    CmdBuffer : CmdBuffArray;
    MT : MatchType;
    UseWords : Boolean;
    SkipPrimary : Boolean;
    {$IFDEF UseMouse}
    SaveWaitState : Boolean;
    {$ENDIF}

    function AltChar : Char;
      {-Convert an Alt keycode to its ASCII equivalent, or return #0}
    const
      AlphaAltCodes : array[$10..$32] of Char =
        'QWERTYUIOP'#0#0#0#0'ASDFGHJKL'#0#0#0#0#0'ZXCVBNM';
      NumericAltCodes : array[$78..$81] of Char = '1234567890';
    begin
      AltChar := #0;
      if Lo(Key) = 0 then
        case Hi(Key) of
          $10..$32: AltChar := AlphaAltCodes[Hi(Key)];
          $78..$81: AltChar := NumericAltCodes[Hi(Key)];
        end;
    end;

    procedure CheckForChar;  {!!.03} {rewritten}
      {-Return alphanumeric character if it isn't a command}
    var
      CAC : Boolean;
    begin
      CAC := FlagIsSet(cpOptions, cpCtrlsAreChars);
      if (BufNext = 2) and (KeyCh = #0) and (cpAltKeys <> nil) then begin
        KeyCh := AltChar;
        if KeyCh in cpAltKeys^ then
          Cmd := ccAltKey
        else
          KeyCh := #0;
      end
      else if (BufNext = 1) then
        if (Key = 0) and CAC then
          Cmd := ccChar
        else case KeyCh of
          #001..#031 :
            if CAC then
              Cmd := ccChar;
          #032..#126, #128..#255 :
            Cmd := ccChar;
        end;
    end;

    function ScanPrim(K : CmdTablePtr) : MatchType;
      {-Scan K^ for a match on CmdBuffer}
    var
      Junk : Word;
    begin
      ScanPrim := ScanCommands(K, CmdBuffer, BufNext, Cmd, Junk, UseWords);
    end;

  begin
    if cpCmdsLeft <> 0 then begin
      GetCommand := cpNextCmd(Key);
      if FlagIsSet(cpOptions, cpUpcase) then
        KeyCh := Upcase(KeyCh);
      Exit;
    end;

    BufNext := 0;
    Cmd := ccNone;
    Finished := False;
    SkipPrimary := False;
    UseWords := FlagIsSet(cpOptions, cpWordCmds);

    {adjust priority}
    cpFixPriority(Dummy);

    repeat
      {$IFDEF UseMouse}
      SaveWaitState := WaitForButtonRelease;
      WaitForButtonRelease := True;
      {$ENDIF}

      {Get the next keystroke}
      Key := cpGetKey;

      {$IFDEF UseMouse}
      WaitForButtonRelease := SaveWaitState;
      {$ENDIF}

      LCh := Lo(Key);
      if LCh = 0 then begin
        {Extended keystroke}
        CmdBuffer[BufNext] := 0;
        Inc(BufNext);
        LCh := Hi(Key);
      end
      else begin
        if FlagIsSet(cpOptions, cpUpcase) then
          Ch := Upcase(Ch);
        if (BufNext > 0) and FlagIsSet(cpOptions, cpMapWordStar) then
          {Map WordStar keystrokes}
          LCh := WordStarCommand(LCh);
      end;
      CmdBuffer[BufNext] := LCh;
      Inc(BufNext);

      {Map to a command}
      if SkipPrimary then
        MT := NoMatch
      else
        MT := ScanPrim(cpPKeys);
      case MT of
        FullMatch :
          Finished := True;
        PartMatch :
          if (cpSKeys <> nil) and (ScanPrim(cpSKeys) = FullMatch) then begin
            Finished := True;
            MT := FullMatch;
          end;
        NoMatch :
          if cpSKeys <> nil then begin
            MT := ScanPrim(cpSKeys);
            case MT of
              FullMatch :
                Finished := True;
              PartMatch :
                SkipPrimary := True;
              NoMatch :
                begin
                  CheckForChar;
                  Finished := True;
                end;
            end;
          end
          else begin
            CheckForChar;
            Finished := True;
          end;
      end;

      {if not a character command, then allow user to provide match} {!!.30}
      if not (Cmd in [ccChar, ccAltKey, ccCtrlChar]) then
        MT := cpUserMatch(Key, Cmd);

      {call user hook}
      cpUserHook(MT, Key);
    until Finished;

    {readjust priority}
    cpFixPriority(Dummy);

    GetCommand := Cmd;
  end;

  procedure CommandProcessor.SetCommandList(CmdList : Pointer; NumCmds : Word);
    {-Specify a list of commands to be auto-executed}
  begin
    ClearFlag(cpOptions, cpDoingChars);
    if (CmdList = nil) or (NumCmds = 0) then begin
      cpCmdList := nil;
      cpCmdsLeft := 0;
    end
    else begin
      cpCmdList := CmdList;
      cpCmdsLeft := NumCmds;
    end;
  end;

  function CommandProcessor.CommandStringPending : Boolean;
    {-Returns True if a text string is pending in the list of
      auto-executed commands}
  begin
    CommandStringPending := (cpCmdPeek = ccChar) and (cpCmdsLeft > 1) and
                            not FlagIsSet(cpOptions, cpDoingChars);
  end;

  function CommandProcessor.GetCommandString : string;
    {-Returns a text string when CommandStringPending is True}
  var
    S : String;
    SLen : Byte absolute S;
{$IFNDEF VIRTUALPASCAL}
    CmdOS : OS absolute cpCmdList;
{$ENDIF}
    Cmd, Key : Word;
  begin
    SLen := 0;
    if CommandStringPending then begin
      repeat
        Cmd := cpNextCmd(Key);
        if SLen < 255 then begin
          Inc(SLen);
          S[SLen] := Char(Key);
        end;
      until (cpCmdPeek = ccChar) or (cpCmdsLeft = 0);

      if (cpCmdsLeft > 0) then begin
        if FlagIsSet(cpOptions, cpWordCmds) then
{$IFDEF VIRTUALPASCAL}
          Inc( Longint( cpCmdList ), 2 )
        else
          Inc( Longint( cpCmdList ) );
{$ELSE}
          Inc(CmdOS.O, 2)
        else
          Inc(CmdOS.O);
{$ENDIF}
        Dec(cpCmdsLeft);
      end;
      ClearFlag(cpOptions, cpDoingChars);
    end;
    GetCommandString := S;
  end;

  function CommandProcessor.cpCmdPeek : Word;
    {-Peek at next command in list}
  begin
    if (cpCmdsLeft = 0) then
      cpCmdPeek := ccNone
    else if FlagIsSet(cpOptions, cpWordCmds) then
      cpCmdPeek := Word(cpCmdList^)
    else
      cpCmdPeek := Byte(cpCmdList^);
  end;

  function CommandProcessor.cpNextCmd(var Key : Word) : Word;
    {-Get next command in list}
  var
    C : Word;
    UseWords : Boolean;
{$IFNDEF VIRTUALPASCAL}
    CmdOS : OS absolute cpCmdList;
{$ENDIF}
  label
    Restart;
  begin
    UseWords := FlagIsSet(cpOptions, cpWordCmds);
Restart:
    if (cpCmdsLeft = 0) then begin
      cpNextCmd := ccNone;
      cpCmdList := nil;
    end
    else begin
      if UseWords then begin
        C := Word(cpCmdList^);
{$IFDEF VIRTUALPASCAL}
        Inc( Longint( cpCmdList ), 2 );
{$ELSE}
        Inc(CmdOS.O, 2);
{$ENDIF}
      end
      else begin
        C := Byte(cpCmdList^);
{$IFDEF VIRTUALPASCAL}
        Inc( Longint( cpCmdList ) );
{$ELSE}
        Inc(CmdOS.O );
{$ENDIF}
      end;
      Dec(cpCmdsLeft);

      if FlagIsSet(cpOptions, cpDoingChars) then begin
        if C = ccChar then begin
          ClearFlag(cpOptions, cpDoingChars);
          goto Restart;
        end;

        cpNextCmd := ccChar;
        Key := C;
      end
      else begin
        if C = ccChar then begin
          SetFlag(cpOptions, cpDoingChars);
          goto Restart;
        end;
        cpNextCmd := C;
      end;
    end;
  end;

  procedure CommandProcessor.cpAddPrim(KAP : CmdTablePtr; Last, Cmd : Word;
                                       NumKeys : Byte; Key1, Key2 : Word);
    {-Add a new command key assignment or change an existing one}
  var
    CTmp : Word;
    SlotFound : Boolean;
    CmdLen,
    FoundAt : Word;
    MT : MatchType;
    NextCmdIndex : Word;
    BufNext : Word;
    CmdBuffer : CmdBuffArray;
    UseWords : Boolean;
    D : Byte absolute UseWords;
  begin
    if cpError <> 0 then
      Exit;

    if (NumKeys < 1) or (NumKeys > 2) then begin
      cpError := epNonFatal+ecBadParam;
      Exit;
    end;

    {set up for the search}
    InitCmdBuffer(CmdBuffer, NumKeys, Key1, Key2, BufNext);

    {check for duplicate}
    UseWords := FlagIsSet(cpOptions, cpWordCmds);
    MT := ScanCommands(KAP, CmdBuffer, BufNext, CTmp, FoundAt, UseWords);
    case MT of
      FullMatch :
        begin
          {change the command}
          CmdLen := KAP^[FoundAt];
          if Cmd = ccNone then
            {Disable the keystrokes}
            FillChar(KAP^[FoundAt+1], CmdLen, $FF);
          SetCommand(Cmd, KAP^[FoundAt+CmdLen-D], UseWords);
          Exit;
        end;
      PartMatch :
        begin
          cpError := epNonFatal+ecPartialMatch;
          Exit;
        end;
    end;

    {find next available command slot}
    NextCmdIndex := 0;
    SlotFound := False;
    CmdLen := KAP^[NextCmdIndex];
    while (not SlotFound) and (CmdLen <> 0) do begin
      if CommandAt(KAP^[NextCmdIndex+CmdLen-D], UseWords) = ccNone then
        {Command slot is available for reuse}
        if BufNext+D+1 = CmdLen then
          {Slot is the right size}
          SlotFound := True;
      if not SlotFound then begin
        Inc(NextCmdIndex, CmdLen+1);
        CmdLen := KAP^[NextCmdIndex];
      end;
    end;

    {make sure it will fit}
    if (BufNext+D+2) <= (Last-NextCmdIndex) then begin
      {plug in the sequence length}
      KAP^[NextCmdIndex] := BufNext+D+1;
      Inc(NextCmdIndex);

      {plug in the key}
      Move(CmdBuffer, KAP^[NextCmdIndex], BufNext);
      Inc(NextCmdIndex, BufNext);

      {plug in the command}
      SetCommand(Cmd, KAP^[NextCmdIndex], UseWords);
    end
    else
      cpError := epNonFatal+ecKeyTableFull;
  end;

  procedure CommandProcessor.AddCommand(Cmd : Word; NumKeys : Byte;
                                        Key1, Key2 : Word);
    {-Add a new primary command key assignment or change an existing one}
  begin
    cpAddPrim(cpPKeys, cpPLast, Cmd, NumKeys, Key1, Key2);
  end;

  procedure CommandProcessor.AddSecondaryCommand(Cmd : Word; NumKeys : Byte;
                                                 Key1, Key2 : Word);
    {-Add a new secondary command key assignment or change an existing one}
  begin
    cpAddPrim(cpSKeys, cpSLast, Cmd, NumKeys, Key1, Key2);
  end;

  function CommandProcessor.GetLastError : Word;
    {-Returns last error code and clears it}
  begin
    GetLastError := cpError;
    cpError := 0;
  end;

  procedure CommandProcessor.GetKeyAssignment(Cmd : Word; var NumKeys : Byte;
                                              var Key1, Key2 : Word);
    {-Search command table(s) for Cmd, returning first set of matching keys.
      NumKeys = 0 if no match found}
  var
    Changed : Boolean;

    procedure GetAssignmentPrim(Cmd : Word; KAP : CmdTablePtr);
      {-Search the specified command table for Cmd}
    var
      KO : Word;
      Key : Word;
      Len : Integer;
      UseWords : Boolean;
      D : Byte absolute UseWords;
    begin
      if KAP = nil then
        Exit;
      KO := 0;
      UseWords := FlagIsSet(cpOptions, cpWordCmds);
      repeat
        Len := KAP^[KO];
        if Len <> 0 then
          {does it match the command?}
          if CommandAt(KAP^[KO+Len-D], UseWords) = Cmd then begin
            {Reduce length to avoid Cmd}
            Dec(Len, D+1);
            repeat
              {Get next key byte}
              Inc(KO);
              Dec(Len);
              if KAP^[KO] = 0 then begin
                {Extended keystroke}
                Inc(KO);
                Dec(Len);
                Key := Word(KAP^[KO]) shl 8;
              end
              else
                {Normal keystroke}
                Key := KAP^[KO];

              {Store the keys}
              Inc(NumKeys);
              if NumKeys = 1 then
                Key1 := Key
              else if NumKeys = 2 then
                Key2 := Key;
            until Len <= 0;

            {Don't allow more than two keys}
            if NumKeys > 2 then
              NumKeys := 2;
            Exit;
          end;
        Inc(KO, Len+1);
      until (Len = 0);
    end;

  begin
    {adjust priority}
    cpFixPriority(Changed);

    NumKeys := 0;
    GetAssignmentPrim(Cmd, cpPKeys);
    if NumKeys = 0 then
      GetAssignmentPrim(Cmd, cpSKeys);

    {adjust priority}
    cpFixPriority(Changed);
  end;

  function CommandProcessor.GetCmdAssignment(NumKeys : Byte;
                                             Key1, Key2 : Word;
                                             var Primary : Boolean) : Word;
    {-Return command associated with specified key combination, or ccNone
      if none found. Primary is False only if match found in secondary
      key set.}
  var
    Cmd, BufNext : Word;
    CmdBuffer : CmdBuffArray;
    MT : MatchType;
    Changed : Boolean;

    function ScanPrim(K : CmdTablePtr) : Boolean;
      {-Scan K^ for a match on CmdBuffer}
    var
      Junk : Word;
    begin
      ScanPrim := ScanCommands(K, CmdBuffer, BufNext, Cmd, Junk,
                               FlagIsSet(cpOptions, cpWordCmds)) = FullMatch;
    end;

  begin
    GetCmdAssignment := ccNone;

    {adjust priority}
    cpFixPriority(Changed);

    Primary := not Changed;
    if NumKeys <> 0 then begin
      {set up for the search}
      InitCmdBuffer(CmdBuffer, NumKeys, Key1, Key2, BufNext);

      {scan for command assignment}
      if ScanPrim(cpPKeys) then
        GetCmdAssignment := Cmd
      else if ScanPrim(cpSKeys) then begin
        Primary := Changed;
        GetCmdAssignment := Cmd;
      end;
    end;

    {adjust priority}
    cpFixPriority(Changed);
  end;

  function CommandProcessor.CheckForConflict(NumKeys : Byte;
                                             Key1, Key2 : Word) : MatchType;
    {-Check to see if the specified key combination conflicts with an existing
      one}
  var
    BufNext : Word;
    CmdBuffer : CmdBuffArray;
    MT : MatchType;

    function ScanPrim(K : CmdTablePtr) : MatchType;
      {-Scan K^ for a match on CmdBuffer}
    var
      Cmd, Junk : Word;
    begin
      ScanPrim := ScanCommands(K, CmdBuffer, BufNext, Cmd, Junk,
                               FlagIsSet(cpOptions, cpWordCmds));
    end;

  begin
    if NumKeys = 0 then
      CheckForConflict := NoMatch
    else begin
      {set up for the search}
      InitCmdBuffer(CmdBuffer, NumKeys, Key1, Key2, BufNext);

      {check for duplicate}
      MT := ScanPrim(cpPKeys);
      if MT = NoMatch then
        CheckForConflict := ScanPrim(cpSKeys)
      else
        CheckForConflict := MT;
    end;
  end;

{$IFDEF UseStreams}

  constructor CommandProcessor.Load(var S : IdStream);
    {-Load a command processor from a stream}
  begin
    cpPKeys := nil;
    cpSKeys := nil;
    Root.Init;
    cpCmdList := nil;
    cpCmdsLeft := 0;
    cpError := 0;
    cpAltKeys := nil;        {!!.03}
    @cpUserMatchFunc := nil; {!!.30}

    S.ReadRange(cpOptions, cpPKeys);

    cpPKeys := S.ReadPointer;
    if cpPKeys <> nil then
      ClearFlag(cpOptions, cpDeallocP)
    else if GetMemCheck(cpPKeys, cpPLast+1) then begin
      SetFlag(cpOptions, cpDeallocP);
      S.Read(cpPKeys^, cpPLast+1);
      if S.PeekStatus <> 0 then begin
        Done;
        Fail;
      end;
    end
    else begin
      InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    if cpSLast <> 0 then begin
      cpSKeys := S.ReadPointer;
      if cpSKeys <> nil then
        ClearFlag(cpOptions, cpDeallocS)
      else if GetMemCheck(cpSKeys, cpSLast+1) then begin
        SetFlag(cpOptions, cpDeallocS);
        S.Read(cpSKeys^, cpSLast+1);
        if S.PeekStatus <> 0 then begin
          Done;
          Fail;
        end;
      end
      else begin
        InitStatus := epFatal+ecOutOfMemory;
        Done;
        Fail;
      end;
   end;

    {$IFDEF UseMouse}
      if FlagIsSet(cpOptions, cpEnableMouse) then begin
        (*                                                           {!!.22}
        @cpGetKeyProc := S.ReadUserPointer(@ReadKeyWord);            {!!.22}
        @cpKeyPressedProc := S.ReadUserPointer(@KeyPressed);         {!!.22}
        *)                                                           {!!.22}
        @cpGetKeyProc := S.ReadUserPointer(@ReadKeyOrButton);        {!!.22}
        @cpKeyPressedProc := S.ReadUserPointer(@KeyOrButtonPressed); {!!.22}
      end
      else begin
        (*                                                           {!!.22}
        @cpGetKeyProc := S.ReadUserPointer(@ReadKeyOrButton);        {!!.22}
        @cpKeyPressedProc := S.ReadUserPointer(@KeyOrButtonPressed); {!!.22}
        *)                                                           {!!.22}
        @cpGetKeyProc := S.ReadUserPointer(@ReadKeyWord);            {!!.22}
        @cpKeyPressedProc := S.ReadUserPointer(@KeyPressed);         {!!.22}
      end;
    {$ELSE}
      @cpGetKeyProc := S.ReadUserPointer(@ReadKeyWord);
      @cpKeyPressedProc := S.ReadUserPointer(@KeyPressed);
    {$ENDIF}

    @cpGetHelpProc := S.ReadPointer;
    @cpUserHookProc := S.ReadPointer;

    {check status}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {$IFDEF UseMouse}
    if MouseInstalled and FlagIsSet(cpOptions, cpEnableMouse) then
      EnableEventHandling;
    {$ENDIF}
  end;

  procedure CommandProcessor.Store(var S : IdStream);
    {-Store a command processor in a stream}
  begin
    S.WriteRange(cpOptions, cpPKeys);
    S.WriteUserPointer(cpPKeys, ptNil);
    if not S.PointerRegistered(cpPKeys) then
      S.Write(cpPKeys^, cpPLast+1);

    if cpSKeys = nil then
      cpSLast := 0;
    if cpSLast <> 0 then begin
      S.WriteUserPointer(cpSKeys, ptNil);
      if not S.PointerRegistered(cpSKeys) then
        S.Write(cpSKeys^, cpSLast+1);
    end;

    S.WriteUserPointer(@cpGetKeyProc, ptNil);
    S.WriteUserPointer(@cpKeyPressedProc, ptNil);
    S.WriteUserPointer(@cpGetHelpProc, ptNil);
    S.WriteUserPointer(@cpUserHookProc, ptNil);
  end;

  procedure CommandProcessorStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing command processors}
  begin
    with SPtr^ do begin
      RegisterType(otCommandProcessor, veCommandProcessor,
                   TypeOf(CommandProcessor),
                   @CommandProcessor.Store, @CommandProcessor.Load);
      RegisterPointer(ptReadKeyWord, @ReadKeyWord);
      RegisterPointer(ptKeyPressed, @KeyPressed);
      {$IFDEF UseMouse}
      RegisterPointer(ptReadKeyOrButton, @ReadKeyOrButton);
      RegisterPointer(ptKeyOrButtonPressed, @KeyOrButtonPressed);
      {$ENDIF}
    end;
  end;

{$ENDIF}

  {---------------------------------------------------------}

  constructor CommandPacker.Init(PKeys : CmdTablePtr;
                                 MaxCmd : Word;
                                 MaxIndex : Word;
                                 UKeys : UnpackedCmdPtr;
                                 Columns : Byte;
                                 CmdsAreWords : Boolean);
    {-Initialize pointers to packed/unpacked command tables}
  begin
    cpPKeys := PKeys;
    cpUKeys := UKeys;
    cpMaxIndex := MaxIndex;
    if Columns = 0 then
      cpCols := 1
    else
      cpCols := Columns;
    cpMaxCmd := MaxCmd*cpCols;
    cpUseWords := CmdsAreWords;

    cpUnpack;
  end;

  procedure CommandPacker.cpUnpack;
    {-Unpack keys into a fixed element array}
  var
    Cmd, CmdNum, KeyOfs : Word;
    I, Len : Word;
    Delta : Word;
    MaxCmd : Word;
  label
    ExitPoint;
  begin
    {initialize unpacked command table with 0}
    FillChar(cpUKeys^, cpMaxCmd*SizeOf(KeyRec), 0);

    {fill in the command codes}
    for CmdNum := 1 to cpMaxCmd do
      with cpUKeys^[CmdNum] do
        CmdCode := (Pred(CmdNum) div cpCols)+1;

    KeyOfs := 0;
    Delta := Ord(cpUseWords);

    Len := cpPKeys^[KeyOfs];
    MaxCmd := cpMaxCmd div cpCols;
    while Len <> 0 do begin
      {find an unused entry in the proper row}
      Cmd := CommandAt(cpPKeys^[KeyOfs+Len-Delta], cpUseWords);
                           {!!.01}
      if (Cmd > 0) and (Cmd <= MaxCmd) then begin
        CmdNum := (Pred(Cmd)*cpCols)+1;
        for I := 1 to cpCols do
          with cpUKeys^[CmdNum] do
            if Length(Keys) = 0 then begin
              Move(cpPKeys^[KeyOfs], Keys, Len);
              Dec(Keys[0], Delta+1);
              goto ExitPoint;
            end
            else
              Inc(CmdNum);
      end;

ExitPoint:
      Inc(KeyOfs, Len+1);
      Len := cpPKeys^[KeyOfs];
    end;
  end;

  function CommandPacker.PackKeys : Boolean;
    {-Convert unpacked array into a packed list of keys again. Returns False
      if keys won't all fit.}
  var
    Len : Byte;
    CmdNum : Word;
    KeyOfs : Word;
    KeyNew : Word;
    Col : Word;
    Delta : Word;
  begin
    PackKeys := False;

    FillChar(cpPKeys^, cpMaxIndex+1, 0);

    KeyOfs := 0;
    Delta := Ord(cpUseWords)+1;

    for CmdNum := 1 to cpMaxCmd do
      with cpUKeys^[CmdNum] do
        if Length(Keys) <> 0 then begin
          Len := Length(Keys)+1;

          {will the keys fit?}
          KeyNew := KeyOfs+Len+Delta;
          if KeyNew > cpMaxIndex then
            {nope--we're through}
            Exit
          else begin
            {plug in the keys}
            Inc(Keys[0], Delta);
            Move(Keys, cpPKeys^[KeyOfs], Len);
            SetCommand(CmdCode, cpPKeys^[KeyNew-Delta], cpUseWords);

            Dec(Keys[0], Delta);

            KeyOfs := KeyNew;
          end;
        end;

    PackKeys := True;
  end;

  function CommandPacker.SizeKeys : Word;
    {-Return number of bytes in packed version of cpUKeys}
  var
    CmdNum, Size, Delta : Word;
  begin
    Delta := Ord(cpUseWords)+2;
    Size := 0;
    for CmdNum := 1 to cpMaxCmd do
      with cpUKeys^[CmdNum] do
        if Length(Keys) <> 0 then
          Inc(Size, Length(Keys)+Delta);
    SizeKeys := Size;
  end;

  function CommandPacker.GetKeyRecPtr(Cmd : Word; Col : Byte) : KeyRecPtr;
    {-Returns a pointer to the KeyRec for the Col'th instance of Cmd}
  begin
    if (Col = 0) or (Col > cpCols) or (Cmd > cpMaxCmd div cpCols) then
      GetKeyRecPtr := nil
    else
      GetKeyRecPtr := @cpUKeys^[(Pred(Cmd)*cpCols)+Col];
  end;

function CommandPacker.ConflictsFound : Boolean;
    {-Check cpUKeys for conflicts. Returns False if Conflicts found}
  var
    I, J : Word;
    JP : KeyRecPtr;
  begin
    {assume success}
    ConflictsFound := False;

    {turn off all Conflict flags}
    for I := 1 to cpMaxCmd do
      cpUKeys^[I].Conflict := False;

    {check for conflicts}
    for I := 1 to cpMaxCmd do
      with cpUKeys^[I] do
        {anything in Keys?}
        if Length(Keys) <> 0 then
          {check all other key assignments for conflict}
          for J := 1 to cpMaxCmd do
            {ignore the current one}
            if (J <> I) then begin
              {get a pointer to the J'th key rec}
              JP := @cpUKeys^[J];

              {any keys in it?}
              if (Length(JP^.Keys) <> 0) then
                {partial or complete match?}
                if Pos(JP^.Keys, Keys) = 1 then
                begin
                  {mark both key recs as having a conflict}
                  Conflict := True;
                  JP^.Conflict := True;

                  {we have at least one conflict}
                  ConflictsFound := True;
                end;
            end;
  end;

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
