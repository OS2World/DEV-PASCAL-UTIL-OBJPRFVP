{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPCTRL.PAS 1.30                     *}
{*      Copyright (c) TurboPower Software 1988, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

unit OpCtrl;
  {-Controls for dialog boxes}

interface

uses
  Use32,
  OpConst,    {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpCmd,
  {$IFDEF UseBcd}
  OpBcd,
  {$ENDIF}
  {$IFDEF UseDates}
  OpDate,
  {$ENDIF}
  OpAbsFld,
  OpSelect,   {!!.30}
  OpFEdit,
  OpField,
  OpFrame,
  OpWindow;

{.F-}
type
  DialogColorSet =
    object
      HiPromptColor, HiPromptMono : Byte;     {highlight chars in prompts}
      ButtonColor, ButtonMono : Byte;         {buttons}
      DefButtonColor, DefButtonMono : Byte;   {default button when unselected}
      HiButtonColor, HiButtonMono : Byte;     {highlight chars in buttons}
      SelButtonColor, SelButtonMono : Byte;   {selected buttons}
      ProButtonColor, ProButtonMono : Byte;   {protected buttons}
      BtnShadowColor, BtnShadowMono : Byte;   {button shadows/boxes}
      ClusterColor, ClusterMono : Byte;       {cluster items}
      ProClusterColor, ProClusterMono : Byte; {protected cluster items}
      HiClusterColor, HiClusterMono : Byte;   {highlight chars in cluster items}
      SelClusterColor, SelClusterMono : Byte; {selected cluster items}

      procedure SetHighlightPromptAttr(Color, Mono : Byte);
        {-Set attributes for highlight chars in prompts}
      procedure SetButtonAttr(Color, Mono : Byte);
        {-Set attributes for unselected buttons}
      procedure SetDefaultButtonAttr(Color, Mono : Byte);
        {-Set attributes for default button when unselected}
      procedure SetHighlightButtonAttr(Color, Mono : Byte);
        {-Set attributes for highlighted chars in buttons}
      procedure SetSelectedButtonAttr(Color, Mono : Byte);
        {-Set attributes for selected buttons}
      procedure SetProtectedButtonAttr(Color, Mono : Byte);
        {-Set attributes for protected buttons}
      procedure SetButtonShadowAttr(Color, Mono : Byte);
        {-Set attributes for button shadows/boxes}
      procedure SetClusterAttr(Color, Mono : Byte);
        {-Set attributes for cluster items}
      procedure SetProtectedClusterAttr(Color, Mono : Byte);
        {-Set attributes for protected cluster items}
      procedure SetHighlightClusterAttr(Color, Mono : Byte);
        {-Set attributes for highlighted chars in cluster items}
      procedure SetSelectedClusterAttr(Color, Mono : Byte);
        {-Set attributes for selected cluster items}
    end;

const
  dcSelectLocally   = $00000001;
  dcButtonShadow    = $00000002;
  dcButtonBox       = $00000004;
  dcItemChangeExit  = $00000008;                   {!!.11}
  dcForceExit       = $04000000; {internal flags}  {!!.11}
  dcButtonDown      = $08000000;
  dcSelectByChar    = $10000000;
  dcIsDefault       = $20000000;
  dcIsButton        = $40000000;
  dcIsCluster       = $80000000;

  DefControlOptions : LongInt = dcButtonShadow;
  BadControlOptions : LongInt = dcButtonDown+dcSelectByChar+dcIsDefault+
                                dcIsButton+dcIsCluster+dcForceExit; {!!.11}

  CurControlMarker1 : Char = '¯';
  CurControlMarker2 : Char = '®';
  DefButtonMarker1  : Char = #26;
  DefButtonMarker2  : Char = #27;

  DefButtonBox      : FrameArray = 'ÉÈ»¼ÍÍºº';
  DefButtonBoxSel   : FrameArray = 'ÉÈ»¼ÍÍºº';
  DefButtonBoxDown  : FrameArray = 'ÚÀ¿ÙÄÄ³³';

type
  PointerPtr = ^Pointer;
  ControlPtr = ^Control;
  Control =                 {abstract object}
    object(EntryField)
      dcFlags      : LongInt;
      dcDefCmd     : Word;
      dcHighPColor : Byte;
      dcHighPMono  : Byte;

      constructor Init(ID : Word;           var Prompt : string;
                       pRow, pCol : Word;   var Picture : string;
                       fRow, fCol : Word;   fWidth, fHeight : Byte;
                       HlpNdx : Word;       var RangeLo, RangeHi : RangeType;
                       DataSize : Word;     DecimalPlaces : Byte;
                       VF : ValidationFunc; CP : ConversionProc;
                       DP : DrawProc;       EP : EditProc;
                       var EditVar;         PadChar : Char;
                       prOptions, seOptions, dcOptions : LongInt;
                       var Colors : ColorSet;
                       var dColors : DialogColorSet);
        {-Initialize a control}
      constructor InitNPP(ID : Word;           var Prompt : string;
                          pRow, pCol : Word;   var Picture : string;
                          fRow, fCol : Word;   fWidth, fHeight : Byte;  HlpNdx : Word;
                          var RangeLo, RangeHi : RangeType;
                          DataSize : Word;     DecimalPlaces : Byte;
                          var EditVar;         PadChar : Char;
                          prOptions, seOptions, dcOptions : LongInt;
                          var Colors : ColorSet;
                          var dColors : DialogColorSet);
        {-Initialize a control (No Procedure Pointers)}

      procedure dcOptionsOn(OptionFlags : LongInt);
        {-Activate control options}
      procedure dcOptionsOff(OptionFlags : LongInt);
        {-Deactivate control options}
      function dcOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified control options are on}
      procedure SetDefCommand(Cmd : Word);
        {-Set default command for this control}
      function SelectWithChar(Ch : Char) : Boolean; virtual;
        {-Return True if Ch is a valid selection character for this control}
      function FindSelectionChar(Ch : Char) : ControlPtr;
        {-Return a pointer to Control with Ch as selection char, or nil}
      function efOKtoAdvance(CC : Word) : Boolean; virtual;
        {-Verify that it's OK to go to next/previous field if requested}
      {$IFDEF UseMouse}
      function MouseInControl(Row, Col : Word) : Boolean;
        {-Return True if the mouse is inside the control}
      {$ENDIF}
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a window control from a stream}
      procedure Store(var S : IdStream);
        {-Store a window control in a stream}
      function cpFixUserVars(var IdS : IdStream; UserRecPtr : Pointer;
                             UserRecSize : Word) : Boolean;
        {-Fix up user variable pointers}
      function cpRestoreUserVars(var IdS : IdStream; UserRecPtr : Pointer;
                                 UserRecSize : Word) : Boolean;
        {-Restore user variable pointers}
      procedure cpGetUserVars(var P : Pointer; var UVP : PointerPtr); virtual;
        {-Get user variable pointers}
      {$ENDIF}
      procedure cpAddAltKeys(var CS : CharSet); virtual;
        {-Add to list of acceptable Alt keys}
    end;

  PushButtonPtr = ^PushButton;
  PushButton =           {OK, Cancel, Help, etc.}
    object(Control)
      buShadowColor : Byte;
      buShadowMono  : Byte;
      buDefColor    : Byte;
      buDefMono     : Byte;

      constructor Init(ID : Word; St : string; fRow, fCol : Word;
                       fWidth : Byte; HlpNdx : Word; PadChar : Char;
                       prOptions, seOptions, dcOptions : LongInt;
                       var Colors : ColorSet;
                       var dColors : DialogColorSet; DefCmd : Word);
        {-Initialize a pushbutton}
      procedure Draw(var St : string;
                     Row, Col : Word;
                     FA, CA, POffset : Byte;
                     PasswordChar : Char;
                     var Flags : PictureFlags); virtual;
      procedure Edit(Row, Col : Word;     FA, CA : Byte;
                     PasswordChar : Char; PosCode : Byte;
                     ReadOnly : Boolean;  var CC : Word;
                     var ChWord : Word;   var InsertMode : Boolean;
                     EP : ErrorProc;      UnitCode : Byte;
                     var CP : CommandProcessor); virtual;
      function SelectWithChar(Ch : Char) : Boolean; virtual;
        {-Return True if Ch is a valid selection character for this control}
      procedure cpAddAltKeys(var CS : CharSet); virtual;
        {-Add to list of acceptable Alt keys}
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a window control from a stream}
      procedure Store(var S : IdStream);
        {-Store a window control in a stream}
      {$ENDIF}
    end;

  ClusterItemPtr = ^ClusterItem;
  ClusterItem =
    object(DoubleListNode)
      ciId         : Byte;
      ciSelValue   : Byte;
      ciRow, ciCol : Byte;
      ciVarPtr     : ^Byte;
      ciText       : StringPtr;
      ciProtected  : Boolean;         {!!.30}

      constructor Init(Id : Byte; S : string; Row, Col : Byte;
                       VarPtr : Pointer; SelValue : Byte);
      destructor Done; virtual;

      procedure Select;
        {-Select this item}
      procedure Deselect;
        {-Deselect this item}
      function IsSelected : Boolean;
        {-Return True is item is selected}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a window control from a stream}
      procedure Store(var S : IdStream);
        {-Store a window control in a stream}
      {$ENDIF}
    end;

  ClusterPtr = ^Cluster;
  Cluster =
    object(Control)         {for radio buttons, check boxes}
      clSelFColor : Byte;
      clSelFMono  : Byte;
      clItemWidth : Byte;
      clList      : DoubleList;
      clCurrent   : ClusterItemPtr;

      constructor Init(ID : Word;          var Prompt : string;
                       pRow, pCol : Word;  fRow, fCol : Word;
                       fWidth, fHeight, iWidth : Byte;
                       HlpNdx : Word;      var CVar;
                       prOptions, seOptions, dcOptions : LongInt;
                       var Colors : ColorSet;
                       var dColors : DialogColorSet);
        {-Initialize a cluster}
      destructor Done; virtual;
      procedure Draw(var St : string;
                     Row, Col : Word;
                     FA, CA, POffset : Byte;
                     PasswordChar : Char;
                     var Flags : PictureFlags); virtual;
      procedure Edit(Row, Col : Word;     FA, CA : Byte;
                     PasswordChar : Char; PosCode : Byte;
                     ReadOnly : Boolean;  var CC : Word;
                     var ChWord : Word;   var InsertMode : Boolean;
                     EP : ErrorProc;      UnitCode : Byte;
                     var CP : CommandProcessor); virtual;

      function FindItem(Id : Byte) : ClusterItemPtr;
        {-Return a pointer to the specified item}
      function GetCurrentItem : Byte;
        {-Return ID for current item}
      procedure SetCurrentItem(Id : Byte; Direct : ShortInt); {!!.30}
        {-Set current item}                                   {!!.30}
      procedure SelectItem(Id : Byte);
        {-Select specified item}
      procedure RedrawItems(Row, Col : Word; FA, HA : Byte);
        {-Redraw all items in the cluster}
      function AddItem(S : string; Row, Col : Byte;
                       VarPtr : Pointer; SelValue : Byte) : Word;
        {-Add a new item to the cluster, returning an error code}
      procedure GetIconAndMarker(var Icon : string;
                                 var Marker : Char;
                                 var MarkerOfst : Byte); virtual;
        {-Get the icon and selection marker for this cluster}
      function HandleSelection(Cmd, Key, Row, Col : Word) : Boolean; virtual;
        {-Handle selection commands, returning True if editor should exit}
      {$IFDEF UseMouse}
      function MouseInCluster(var Id : Byte; Row, Col : Word) : Boolean;
        {-Return True if the mouse is in the cluster, and the ID for the item}
      {$ENDIF}
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a cluster from a stream}
      procedure Store(var S : IdStream);
        {-Store a cluster in a stream}
      procedure cpGetUserVars(var P : Pointer; var UVP : PointerPtr); virtual;
        {-Get user variable pointers}
      {$ENDIF}

      function SelectWithChar(Ch : Char) : Boolean; virtual;
        {-Return True if Ch is a valid selection character for this control}
      function AllItemsProtected : Boolean;                             {!!.30}
        {-Return True if all items in cluster are protected }           {!!.30}
      function clFindSelectionChar(Ch : Char) : ClusterItemPtr;
        {-Return pointer to item with Ch as selection char, or nil}
      procedure clSetCurrent; virtual;
        {-Initialize clCurrent}
      procedure clNextUnprotected(var Item : ClusterItemPtr); virtual;  {!!.30}
        {-Set Item to the next unprotected item }                       {!!.30}
      procedure clPrevUnprotected(var Item : ClusterItemPtr); virtual;  {!!.30}
        {-Set Item to the previous unprotected item }                   {!!.30}
      procedure cpAddAltKeys(var CS : CharSet); virtual;
        {-Add to list of acceptable Alt keys}
    end;

  RadioButtonsPtr = ^RadioButtons;
  RadioButtons =
    object(Cluster)
      function HandleSelection(Cmd, Key, Row, Col : Word) : Boolean; virtual;
        {-Handle selection commands, returning True if editor should exit}
      procedure GetIconAndMarker(var Icon : string;
                                 var Marker : Char;
                                 var MarkerOfst : Byte); virtual;
        {-Get the icon and selection marker for this cluster}
      procedure clSetCurrent; virtual;
        {-Initialize clCurrent}
    end;

  CheckBoxesPtr = ^CheckBoxes;
  CheckBoxes =
    object(Cluster)
      function HandleSelection(Cmd, Key, Row, Col : Word) : Boolean; virtual;
        {-Handle selection commands, returning True if editor should exit}
      procedure GetIconAndMarker(var Icon : string;
                                 var Marker : Char;
                                 var MarkerOfst : Byte); virtual;
        {-Get the icon and selection marker for this cluster}
    end;

procedure NullConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  {-Do-nothing conversion routine}

procedure opcFlexWrite(var S : string; Row, Col : Word; A1, A2 : Byte);
  {-Converts A1 and A2 to a FlexAttrs array and calls FlexWrite}

const
  DefaultDialogColors : DialogColorSet = (
    HiPromptColor   : $0F; HiPromptMono    : $0F;
    ButtonColor     : $70; ButtonMono      : $70;
    DefButtonColor  : $78; DefButtonMono   : $70;
    HiButtonColor   : $7F; HiButtonMono    : $70;
    SelButtonColor  : $7F; SelButtonMono   : $70;
    ProButtonColor  : $70; ProButtonMono   : $07;
    BtnShadowColor  : $08; BtnShadowMono   : $07;
    ClusterColor    : $70; ClusterMono     : $70;
    ProClusterColor : $07; ProClusterMono  : $07;
    HiClusterColor  : $7F; HiClusterMono   : $70;
    SelClusterColor : $7F; SelClusterMono  : $70
  );

{$IFDEF UseStreams}

  {------- stream registration routines ------------}

procedure ControlStream(SPtr : IdStreamPtr);
  {-Register all types for controls}

procedure PushButtonStream(SPtr : IdStreamPtr);
  {-Register all types for push buttons}

procedure ClusterStream(SPtr : IdStreamPtr);
  {-Register all types for clusters}

procedure RadioButtonsStream(SPtr : IdStreamPtr);
  {-Register all types for radio buttons}

procedure CheckBoxesStream(SPtr : IdStreamPtr);
  {-Register all types for check boxes}

{$ENDIF}

  {==========================================================================}

implementation

  {$I OPCTRL.IN1}

const
  Ampersand = '&';

  {------- Miscellaneous -----}

  procedure NullConversion(EFP : EntryFieldPtr; PostEdit : Boolean);
  begin
  end;

  procedure opcFlexWrite(var S : string; Row, Col : Word; A1, A2 : Byte);
    {-Converts A1 and A2 to a FlexAttrs array and calls FlexWrite}
  var
    FA : FlexAttrs;
  begin
    FA[0] := A1;
    FA[1] := A2;
    FA[2] := A1;
    FA[3] := A1;
    FlexWrite(S, Row, Col, FA);
  end;

  function FixAmpersand(S : string) : string;
  var
    SLen : Byte absolute S;
    I : Word;
  begin
    I := 1;
    if SLen > 0 then
      repeat
        if S[I] = Ampersand then begin
          S[I] := ^A;
          Insert(^A, S, I+2);
          I := SLen+1;
        end
        else
          Inc(I);
      until (I > SLen);
    FixAmpersand := S;
  end;

  function GetSelectChar(var S : string; var Ch : Char) : Boolean;
  var
    I : Word;
  begin
    I := Pos(^A, S);
    if (I = 0) then
      GetSelectChar := False
    else begin
      GetSelectChar := True;
      Ch := Upcase(S[I+1]);
    end;
  end;

  function HasSelectChar(var S : string; Ch : Char) : Boolean;
  var
    C : Char;
  begin
    if not GetSelectChar(S, C) then
      HasSelectChar := False
    else
      HasSelectChar := C = Upcase(Ch);
  end;

{$IFDEF UseStreams}

  function FixUserVar(var IdS : IdStream; var VarPtr : Pointer;
                      UserRecPtr : Pointer; UserRecSize : Word) : Boolean;
    {-Fix up one user variable pointer}
  var
    IP : IdNodePtr;
    MinO, MaxO : Word;
  begin
    FixUserVar := False;

    with OS(VarPtr) do begin
      {is the variable within the user record?}
      MinO := OS(UserRecPtr).O;
      MaxO := MinO+UserRecSize-1;
      if (UserRecPtr <> nil) and (UserRecSize > 0) and
{$IFNDEF VIRTUALPASCAL}
         (S = OS(UserRecPtr).S) and
{$ENDIF}
         (O >= MinO) and (O <= MaxO) then begin
        {store ptEntryUserRec as the segment, and adjust the offset}
{$IFDEF VIRTUALPASCAL}
        { !!More work! }
        O := ( ptEntryUserRec shl 16 ) or ( O-MinO );
{$ELSE}
        S := ptEntryUserRec;
        Dec(O, MinO);
{$ENDIF}
      end
      else begin
        {is the variable's address registered?}
        IP := IdS.idRegistered.FindPointer(VarPtr);
        if IP = nil then
          Exit
        else with IP^ do begin
          {store the id code in the segment, the version in the offset}
{$IFDEF VIRTUALPASCAL}
          O := idCode shl 16 or idVer;
{$ELSE}
          S := idCode;
          O := idVer;
{$ENDIF}
        end;
      end;
    end;

    FixUserVar := True;
  end;

  function RestoreUserVar(var IdS : IdStream; var VarPtr : Pointer;
                          UserRecPtr : Pointer; UserRecSize : Word) : Boolean;
    {-Restore one user variable pointer}
  var
    IP : IdNodePtr;
  begin
    RestoreUserVar := False;

    with OS(VarPtr) do
      {segment has the type code}
{$IFDEF VIRTUALPASCAL}
      if O shr 16 = ptEntryUserRec then
{$ELSE}
      if S = ptEntryUserRec then
{$ENDIF}
        {a code of ptEntryUserRec means it's part of the user record}
        if UserRecPtr = nil then
          {no user record set up}
          Exit
        else begin
          {offset is relative to start of user record}
{$IFDEF VIRTUALPASCAL}
          { !!more work }
{$ELSE}
          S := OS(UserRecPtr).S;
          Inc(O, OS(UserRecPtr).O);
{$ENDIF}
        end
      else begin
        {segment has the code, offset has the version}
{$IFDEF VIRTUALPASCAL}
        IP := IdS.idRegistered.FindByCodeVer(O shr 16, O and $FFFF);
{$ELSE}
        IP := IdS.idRegistered.FindByCodeVer(S, O);
{$ENDIF}
        if IP = nil then
          Exit
        else
          VarPtr := IP^.StorePtr;
      end;

    RestoreUserVar := True;
  end;

{$ENDIF}

  {------- Control -----------}

  constructor Control.Init(ID : Word;           var Prompt : string;
                           pRow, pCol : Word;   var Picture : string;
                           fRow, fCol : Word;   fWidth, fHeight : Byte;
                           HlpNdx : Word;       var RangeLo, RangeHi : RangeType;
                           DataSize : Word;     DecimalPlaces : Byte;
                           VF : ValidationFunc; CP : ConversionProc;
                           DP : DrawProc;       EP : EditProc;
                           var EditVar;         PadChar : Char;
                           prOptions, seOptions, dcOptions : LongInt;
                           var Colors : ColorSet;
                           var dColors : DialogColorSet);
    {-Initialize a control}
  begin
    if not Control.InitNPP(
      ID, Prompt, pRow, pCol, Picture, fRow, fCol, fWidth, fHeight,
      HlpNdx, RangeLo, RangeHi, DataSize, DecimalPlaces, EditVar, PadChar,
      prOptions, seOptions, dcOptions, Colors, dColors) then
        Fail;
    efValidate := VF;
    efConvert  := CP;
    efDraw     := DP;
    efEditor   := EP;
  end;

  constructor Control.InitNPP(ID : Word;           var Prompt : string;
                              pRow, pCol : Word;   var Picture : string;
                              fRow, fCol : Word;   fWidth, fHeight : Byte;
                              HlpNdx : Word;
                              var RangeLo, RangeHi : RangeType;
                              DataSize : Word;     DecimalPlaces : Byte;
                              var EditVar;         PadChar : Char;
                              prOptions, seOptions, dcOptions : LongInt;
                              var Colors : ColorSet;
                              var dColors : DialogColorSet);
    {-Initialize a control (No Procedure Pointers)}
  var
    PrSt : string[80];
  begin
    PrSt := FixAmpersand(Prompt);
    if not EntryField.InitNPP(
      ID, PrSt, pRow, pCol, Picture, fRow, fCol, fWidth, fHeight,
      HlpNdx, RangeLo, RangeHi, DataSize, DecimalPlaces, EditVar, PadChar,
      prOptions, seOptions, Colors) then
        Fail;
    efConvert := NullConversion;
    if not LongFlagIsSet(dcOptions, dcIsCluster) then
      ClearLongFlag(dcOptions, dcSelectLocally);
    dcFlags  := dcOptions;
    dcDefCmd := ccSelect;

    dcHighPColor := dColors.HiPromptColor;
    dcHighPMono  := dColors.HiPromptMono;

    sfPWidth := FlexLen(Prompt);
  end;

  procedure Control.dcOptionsOn(OptionFlags : LongInt);
    {-Activate control options}
  begin
    ClearLongFlag(OptionFlags, dcButtonShadow+dcButtonBox);
    SetLongFlag(dcFlags, OptionFlags and not BadControlOptions);
  end;

  procedure Control.dcOptionsOff(OptionFlags : LongInt);
    {-Deactivate control options}
  begin
    ClearLongFlag(OptionFlags, dcButtonShadow+dcButtonBox);
    ClearLongFlag(dcFlags, OptionFlags and not BadControlOptions);
  end;

  function Control.dcOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified control options are on}
  begin
    dcOptionsAreOn := (dcFlags and OptionFlags) = OptionFlags;
  end;

  procedure Control.SetDefCommand(Cmd : Word);
    {-Set default command for this control}
  begin
    dcDefCmd := Cmd;
  end;

  function Control.SelectWithChar(Ch : Char) : Boolean;
    {-Return True if Ch is a valid selection character for this control}
  begin
    if efPrompt = nil then
      SelectWithChar := False
    else
      SelectWithChar := HasSelectChar(efPrompt^, Ch);
  end;

  function Control.FindSelectionChar(Ch : Char) : ControlPtr;
    {-Return a pointer to Control with Ch as selection char, or nil}
  label
    ExitPoint;
  var
    CP : ControlPtr;
  begin
    if SelectWithChar(Ch) then
      CP := @Self
    else begin
      {search forward}
      CP := ControlPtr(dlNext);
      while CP <> nil do begin
        if not LongFlagIsSet(CP^.sfOptions, efProtected+efHidden+efInvisible) then  {!!.10}
          if CP^.SelectWithChar(Ch) then
            goto ExitPoint;
        CP := ControlPtr(CP^.dlNext);
      end;

      {search backward}
      CP := ControlPtr(dlPrev);
      while CP <> nil do begin
        if not LongFlagIsSet(CP^.sfOptions, efProtected+efHidden+efInvisible) then  {!!.10}
          if CP^.SelectWithChar(Ch) then
            goto ExitPoint;
        CP := ControlPtr(CP^.dlPrev);
      end;
    end;

ExitPoint:
    FindSelectionChar := CP;
  end;

  procedure Control.cpAddAltKeys(var CS : CharSet);
    {-Add to list of acceptable Alt keys}
  var
    Ch : Char;
  begin
    if not LongFlagIsSet(sfOptions, efProtected+efHidden+efInvisible) then  {!!.10}
      if efPrompt <> nil then
        if GetSelectChar(efPrompt^, Ch) then
          CS := CS+[Ch];
  end;

  function Control.efOKtoAdvance(CC : Word) : Boolean;
    {-Verify that it's OK to go to next/previous field if requested}
  begin
    case CC of
      ccNone, ccLeft, ccWordLeft, ccRight,
      ccWordRight, ccUp, ccDown {, ccMouseAuto} : {!!.13}
        efOKtoAdvance := False;
      else
        efOKtoAdvance := True;
    end;
  end;

{$IFDEF UseMouse}
  function Control.MouseInControl(Row, Col : Word) : Boolean;
    {-Return True if the mouse is inside the control}
  var
    MX, MY : Word;
  begin
    MX := MouseKeyWordX+MouseXLo;
    MY := MouseKeyWordY+MouseYLo;
    if (MY >= Row) and (MX >= Col) and (MY <= Row+Pred(sfFHeight)) and
       (MX <= Col+Pred(sfFWidth)) then
         MouseInControl := True
    else
      MouseInControl := False;
  end;
{$ENDIF}

{$IFDEF UseStreams}
  constructor Control.Load(var S : IdStream);
    {-Load a window control from a stream}
  begin
    if not EntryField.Load(S) then
      Fail;

    S.Read(dcFlags, Ofs(dcHighPMono)-Ofs(dcFlags)+SizeOf(dcHighPMono));
  end;

  procedure Control.Store(var S : IdStream);
    {-Store a window control in a stream}
  begin
    EntryField.Store(S);
    if S.PeekStatus = 0 then
      S.Write(dcFlags, Ofs(dcHighPMono)-Ofs(dcFlags)+SizeOf(dcHighPMono));
  end;

  procedure Control.cpGetUserVars(var P : Pointer; var UVP : PointerPtr);
    {-Get user variable pointers}
  begin
    UVP := nil;
  end;

  function Control.cpFixUserVars(var IdS : IdStream; UserRecPtr : Pointer;
                                 UserRecSize : Word) : Boolean;
    {-Fix up user variable pointers}
  var
    UserP : PointerPtr;
    NextP, LastP : Pointer;
  begin
    cpFixUserVars := False;
    if FixUserVar(IdS, efVarPtr, UserRecPtr, UserRecSize) then begin
      NextP := nil;
      cpGetUserVars(NextP, UserP);
      LastP := NextP;

      while UserP <> nil do
        if FixUserVar(IdS, UserP^, UserRecPtr, UserRecSize) then begin
          cpGetUserVars(NextP, UserP);
          LastP := NextP;
        end
        else begin
          if RestoreUserVar(IdS, efVarPtr, UserRecPtr, UserRecSize) then ;
          NextP := nil;
          cpGetUserVars(NextP, UserP);
          while NextP <> LastP do begin
            if RestoreUserVar(IdS, UserP^, UserRecPtr, UserRecSize) then ;
            cpGetUserVars(NextP, UserP);
          end;
          Exit;
        end;

      cpFixUserVars := True;
    end;
  end;

  function Control.cpRestoreUserVars(var IdS : IdStream; UserRecPtr : Pointer;
                                     UserRecSize : Word) : Boolean;
    {-Restore user variable pointers}
  var
    UserP : PointerPtr;
    NextP : Pointer;
  begin
    cpRestoreUserVars := False;
    if RestoreUserVar(IdS, efVarPtr, UserRecPtr, UserRecSize) then begin
      NextP := nil;

      cpGetUserVars(NextP, UserP);
      while UserP <> nil do
        if RestoreUserVar(IdS, UserP^, UserRecPtr, UserRecSize) then
          cpGetUserVars(NextP, UserP)
        else
          Exit;

      cpRestoreUserVars := True;
    end;
  end;
{$ENDIF}

  {------- PushButton --------}

  constructor PushButton.Init(ID : Word; St : string; fRow, fCol : Word;
                              fWidth : Byte; HlpNdx : Word; PadChar : Char;
                              prOptions, seOptions, dcOptions : LongInt;
                              var Colors : ColorSet;
                              var dColors : DialogColorSet;
                              DefCmd : Word);
    {-Initialize a pushbutton}
  var
    StLen : Byte absolute St;
    W, fHeight : Word;
  begin
    if LongFlagIsSet(dcOptions, dcButtonBox) then begin
      ClearLongFlag(dcOptions, dcButtonShadow);
      fHeight := 3;
      W := fWidth-2;
    end
    else begin
      if LongFlagIsSet(dcOptions, dcButtonShadow) then {!!.20}
        fHeight := 2
      else
        fHeight := 1;
      W := fWidth-(fHeight+1);                         {!!.20}
    end;

    ClearLongFlag(prOptions, efRequired+efTrimBlanks);
    if not Control.InitNPP(
      ID, emNullError, fRow, fCol, emNullError, fRow, fCol, fWidth, fHeight,
      HlpNdx, BlankRange, BlankRange, 0, 0, NullMsgLen, PadChar,
      prOptions, seOptions, dcOptions or dcSelectByChar+dcIsButton,
      Colors, dColors) then
        Fail;

    {use picture field to store text of button}
    DisposeString(efPicture);
    St := FixAmpersand(St);
    St := Center(St, W+(StLen-FlexLen(St)));
    efPicture := StringToHeap(St);
    if efPicture = nil then begin
      InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    with dColors do begin
      sfFieldColor    := ButtonColor;
      sfFieldMono     := ButtonMono;
      sfSelFieldColor := SelButtonColor;
      sfSelFieldMono  := SelButtonMono;
      sfProFieldColor := ProButtonColor;
      sfProFieldMono  := ProButtonMono;
      sfCtrlColor     := HiButtonColor;
      sfCtrlMono      := HiButtonMono;
      buDefColor      := DefButtonColor;
      buDefMono       := DefButtonMono;
      buShadowColor   := BtnShadowColor;
      buShadowMono    := BtnShadowMono;
    end;

    dcDefCmd := DefCmd;
  end;

  procedure PushButton.Draw(var St : string;
                            Row, Col : Word;
                            FA, CA, POffset : Byte;
                            PasswordChar : Char;
                            var Flags : PictureFlags);
  var
    FAP : ^FrameArray;
    ButtonDown, Mono, Editing : Boolean;
    Boxed, Shadowed : Boolean;
    Ch1, Ch2 : Char;
    XL, XH : Word;
    I, ShCol : Word;
    SA, Wid : Byte;
    S : string[80];
    SLen : Byte absolute S;
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {is it hidden?}
    if LongFlagIsSet(sfOptions, sfHidden) then begin
      for I := 1 to sfFHeight do {!!.20}
        FastFill(sfFWidth, efPadChar, Row+Pred(I), Col, FA);
    end
    else begin
      {adjust column}
      Inc(Col);

      {is the mouse button down? ignore if shadows or boxes not in use}
      Shadowed := LongFlagIsSet(dcFlags, dcButtonShadow);
      Boxed := LongFlagIsSet(dcFlags, dcButtonBox);
      ButtonDown := LongFlagIsSet(dcFlags, dcButtonDown) and (Shadowed or Boxed);
      Editing := LongFlagIsSet(sfFlags, ifEditing);

      {using color?}
      Mono := not UseColor;
      SA := ColorMono(buShadowColor, buShadowMono);

      {if protected, no special highlight}
      if LongFlagIsSet(sfOptions, sfProtected) then
        CA := FA;

      {draw the button}
      Wid := sfFWidth-(2+Ord(Shadowed))+Ord(Boxed); {!!.20}
      S := efPicture^;
      if Mono and not (ButtonDown or Boxed) then begin
        S[1] := '[';
        S[SLen] := ']';
      end;
      opcFlexWrite(S, Row+Ord(Boxed), Col+Ord(ButtonDown and not Boxed), FA, CA);

      if Boxed then begin
        if ButtonDown then
          FAP := @DefButtonBoxDown
        else if Editing then
          FAP := @DefButtonBoxSel
        else
          FAP := @DefButtonBox;

        {draw the box}
        XL := Col-1;
        XH := XL+sfFWidth-1;
        ExchangeStructs(FAP^, FrameChars, SizeOf(FrameArray));
        FrameWindow(XL, Row, XH, Row+2, SA, SA, '');
        ExchangeStructs(FAP^, FrameChars, SizeOf(FrameArray));
      end
      else begin
        ShCol := Col+Wid;
        if Mono then begin
          if Editing then begin
            Ch1 := CurControlMarker1;
            Ch2 := CurControlMarker2;
          end
          else if LongFlagIsSet(dcFlags, dcIsDefault) then begin
            Ch1 := DefButtonMarker1;
            Ch2 := DefButtonMarker2;
          end
          else begin
            Ch1 := efPadChar;
            Ch2 := Ch1;
          end;
          if ButtonDown then
            FastFill(1, efPadChar, Row, Col-1, SA)
          else if Shadowed then                     {!!.20}
            FastFill(1, efPadChar, Row, ShCol+1, SA);
          FastFill(1, Ch1, Row, Pred(Col)+Ord(ButtonDown), SA);
          FastFill(1, Ch2, Row, ShCol+Ord(ButtonDown), SA);
          Ch2 := efPadChar;
        end
        else begin
          {draw button shadows}
          if ButtonDown or not Shadowed then begin
            Ch1 := efPadChar;
            Ch2 := Ch1;
          end
          else begin
            Ch1 := 'Ü';
            Ch2 := 'ß';
          end;
          if ButtonDown then
            FastFill(1, Ch1, Row, Col, SA)
          else
            FastFill(1, Ch1, Row, ShCol, SA);
          if Shadowed then                     {!!.20}
            FastFill(1, efPadChar, Row, ShCol+1, SA);
        end;
        if Shadowed then                     {!!.20}
          FastFill(Wid, Ch2, Row+1, Col+1, SA);
      end;
    end;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  procedure PushButton.Edit(Row, Col : Word;     FA, CA : Byte;
                            PasswordChar : Char; PosCode : Byte;
                            ReadOnly : Boolean;  var CC : Word;
                            var ChWord : Word;   var InsertMode : Boolean;
                            EP : ErrorProc;      UnitCode : Byte;
                            var CP : CommandProcessor);
  var
    CursorSL : Word;
    Ch : Char absolute ChWord;
    SaveBreak : Boolean;
    Finished : Boolean;
    FirstTime : Boolean;
    CtP : ControlPtr;
    Flags : PictureFlags;
    {$IFDEF UseMouse}
    MIC : Boolean;
    {$ENDIF}

    procedure Error;
      {-Ring bell on invalid input if desired}
    begin
      if LongFlagIsSet(sfOptions, efBeepOnError) then
        RingBell;
    end;

    procedure RedrawButton;
      {-Redraw the button}
    begin
      Draw(efPicture^, Row, Col, FA, CA, 0, ' ', Flags);
    end;

  begin
    {we're "editing"}
    SetLongFlag(sfFlags, ifEditing);

    {store cursor shape and set it}
    CursorSL := CursorTypeSL;
    HiddenCursor;
    GotoXYabs(Col+1, Row+Ord(LongFlagIsSet(dcFlags, dcButtonBox)));

    {save break checking state}
    SaveBreak := CheckBreak;
    CheckBreak := False;

    {draw the button}
    RedrawButton;

    {loop reading keys}
    Finished := False;
    {$IFDEF UseMouse}
    case CC of
      ccMouseSel, ccMouseDown :
        if not MouseInControl(Row, Col) then
          CC := ccNone;
    end;
    {$ENDIF}
    FirstTime := (CC <> ccNone);
    repeat
      {get next command and validate it}
      if FirstTime then
        FirstTime := False
      else
        CC := CP.GetCommand(ChWord);

      {$IFDEF UseMouse}
      if LongFlagIsSet(dcFlags, dcButtonDown) then
        if ((CC <> ccMouseDown) and (CC <> ccMouseAuto)) or
           not MouseInControl(Row, Col) then begin
             ClearLongFlag(dcFlags, dcButtonDown);
             RedrawButton;
           end;
      {$ENDIF}

      case CC of
        ccAltKey,
        ccChar :
          begin
            CtP := FindSelectionChar(Ch);
            Finished := (CtP <> nil);
            if (CtP = @Self) then
              CC := dcDefCmd;
          end;

        ccSelect :
          begin
            CC := dcDefCmd;
            Finished := True;
          end;

        ccTab,               {tab to next field}
        ccBackTab,           {tab to previous field}
        ccNextField, ccPrevField,                      {!!.12}
        ccDone, ccQuit,      {various exit commands}
        ccUser0..ccUser65335 :
          Finished := True;

        {$IFDEF UseMouse}
        ccMouseAuto,
        ccMouseDown,
        ccMouseSel :
          if not MouseInControl(Row, Col) then
            Finished := (CC <> ccMouseAuto)
          else if (CC = ccMouseSel) then begin
            CC := dcDefCmd;
            Finished := True;
          end
          else if not LongFlagIsSet(dcFlags, dcButtonDown) then begin
            SetLongFlag(dcFlags, dcButtonDown);
            RedrawButton;
          end;
        {$ENDIF}

        ccHelp :             {Invoke help system}
          CP.cpGetHelp(UnitCode, nil, sfHelpIndex);

        ccToggle:                    {!!.22}
          if (Ch = ' ') then begin   {!!.22}
            CC := dcDefCmd;          {!!.22}
            Finished := True;        {!!.22}
          end;                       {!!.22}

        else
          Error;
      end;

      if Finished then
        if CC = ccHelp then begin
          {process help request here}
          FirstTime := True;
          Finished := False;
        end
        else
          {make sure it's OK to advance}
          Finished := efOKtoAdvance(CC);
    until Finished;

    {$IFDEF UseMouse}
    if LongFlagIsSet(dcFlags, dcButtonDown) then begin
      ClearLongFlag(dcFlags, dcButtonDown);
      RedrawButton;
    end;
    {$ENDIF}

    {restore break checking status}
    CheckBreak := SaveBreak;

    {restore cursor shape}
    SetCursorSize(Hi(CursorSL), Lo(CursorSL));

    {we're not editing}
    ClearLongFlag(sfFlags, ifEditing);
  end;

  function PushButton.SelectWithChar(Ch : Char) : Boolean;
    {-Return True if Ch is a valid selection character for this control}
  begin
    SelectWithChar := HasSelectChar(efPicture^, Ch);
  end;

  procedure PushButton.cpAddAltKeys(var CS : CharSet);
    {-Add to list of acceptable Alt keys}
  var
    Ch : Char;
  begin
    if not LongFlagIsSet(sfOptions, efProtected+efHidden+efInvisible) then  {!!.10}
      if GetSelectChar(efPicture^, Ch) then
        CS := CS+[Ch];
  end;

{$IFDEF UseStreams}
  constructor PushButton.Load(var S : IdStream);
    {-Load a window control from a stream}
  begin
    if not Control.Load(S) then
      Fail;

    S.Read(buShadowColor, Ofs(buDefMono)-Ofs(buShadowColor)+SizeOf(buDefMono));
  end;

  procedure PushButton.Store(var S : IdStream);
    {-Store a window control in a stream}
  begin
    Control.Store(S);
    if S.PeekStatus = 0 then
      S.Write(buShadowColor, Ofs(buDefMono)-Ofs(buShadowColor)+SizeOf(buDefMono));
  end;
{$ENDIF}

  {------- ClusterItem -------}

  constructor ClusterItem.Init(Id : Byte; S : string; Row, Col : Byte;
                               VarPtr : Pointer; SelValue : Byte);
  begin
    DoubleListNode.Init;
    ciId := Id;
    ciVarPtr := VarPtr;
    ciSelValue := SelValue;
    ciRow := Row;
    ciCol := Col;
    ciText := StringToHeap(FixAmpersand(S));
    if ciText = nil then begin
      InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    ciProtected := False;     {!!.30}
  end;

  destructor ClusterItem.Done;
  begin
    DoubleListNode.Done;
    DisposeString(ciText);
  end;

  procedure ClusterItem.Select;
    {-Select this item}
  begin
    ciVarPtr^ := ciSelValue;
  end;

  procedure ClusterItem.Deselect;
    {-Deselect this item}
  begin
    ciVarPtr^ := 0;
  end;

  function ClusterItem.IsSelected : Boolean;
    {-Return True is item is selected}
  begin
    IsSelected := (ciVarPtr^ = ciSelValue);
  end;

{$IFDEF UseStreams}
  constructor ClusterItem.Load(var S : IdStream);
    {-Load a window control from a stream}
  var
    St : string[80];
    StLen : Byte absolute St;
  begin
    ciText := nil;
    if not DoubleListNode.Init then
      Fail;

    S.ReadRange(ciID, ciText);

    St := S.ReadString;
    if S.PeekStatus = 0 then
      ciText := StringToHeap(St);
    if (ciText = nil) then begin
      if S.PeekStatus = 0 then
        InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;
  end;

  procedure ClusterItem.Store(var S : IdStream);
    {-Store a window control in a stream}
  begin
    S.WriteRange(ciID, ciText);
    if S.PeekStatus = 0 then
      S.WriteString(ciText^);
  end;
{$ENDIF}

  {------- Cluster -------}

  constructor Cluster.Init(ID : Word;          var Prompt : string;
                           pRow, pCol : Word;  fRow, fCol : Word;
                           fWidth, fHeight, iWidth : Byte;
                           HlpNdx : Word;      var CVar;
                           prOptions, seOptions, dcOptions : LongInt;
                           var Colors : ColorSet;
                           var dColors : DialogColorSet);
    {-Initialize a cluster}
  begin
    ClearLongFlag(prOptions, efRequired+efTrimBlanks);
    if not Control.InitNPP(
      ID, Prompt, pRow, pCol, emNullError, fRow, fCol, fWidth, fHeight,
      HlpNdx, BlankRange, BlankRange, 0, 0, CVar, ' ', prOptions, seOptions,
      dcOptions or dcIsCluster+dcSelectByChar, Colors, dColors) then
        Fail;

    clList.Init;
    clCurrent := nil;
    clItemWidth := iWidth;

    sfFieldColor    := dColors.ClusterColor;
    sfFieldMono     := dColors.ClusterMono;
    sfSelFieldColor := sfFieldColor;
    sfSelFieldMono  := sfFieldMono;
    sfProFieldColor := dColors.ProClusterColor;
    sfProFieldMono  := dColors.ProClusterMono;
    sfCtrlColor     := dColors.HiClusterColor;
    sfCtrlMono      := dColors.HiClusterMono;
    clSelFColor     := dColors.SelClusterColor;
    clSelFMono      := dColors.SelClusterMono;
  end;

  destructor Cluster.Done;
    {-Deallocate item list}
  begin
    clList.Done;
    Control.Done;
  end;

  procedure Cluster.Draw(var St : string;
                         Row, Col : Word;
                         FA, CA, POffset : Byte;
                         PasswordChar : Char;
                         var Flags : PictureFlags);
    {-Draw all the items in the cluster}
  begin
    RedrawItems(Row, Col, FA, CA);
    clSetCurrent;                    {!!.20}
    if clCurrent^.ciProtected then   {!!.30}
      clNextUnprotected(clCurrent);  {!!.30}
  end;

  procedure Cluster.Edit(Row, Col : Word;     FA, CA : Byte;
                         PasswordChar : Char; PosCode : Byte;
                         ReadOnly : Boolean;  var CC : Word;
                         var ChWord : Word;   var InsertMode : Boolean;
                         EP : ErrorProc;      UnitCode : Byte;
                         var CP : CommandProcessor);
  var
    CursorSL : Word;
    Ch : Char absolute ChWord;
    SaveBreak : Boolean;
    Finished : Boolean;
    FirstTime : Boolean;
    CtP : ControlPtr;
    CIP : ClusterItemPtr;
    ColDelta, ID : Byte;

    function GetColDelta : Byte;
    var
      Icon : string;
      Marker : Char;
      MarkerOfst : Byte;
    begin
      GetIconAndMarker(Icon, Marker, MarkerOfst);
      GetColDelta := MarkerOfst-1;
    end;

    procedure Error;
      {-Ring bell on invalid input if desired}
    begin
      if LongFlagIsSet(sfOptions, efBeepOnError) then
        RingBell;
    end;

    function IsHorizontal : Boolean;
    var
      CIP1, CIP2 : ClusterItemPtr;
    begin
      CIP1 := ClusterItemPtr(clList.Head);
      CIP2 := ClusterItemPtr(CIP1^.dlNext);
      IsHorizontal := (CIP2 <> nil) and (CIP1^.ciRow = CIP2^.ciRow);
    end;

    function CommandOK(CC : Word) : Boolean;
    begin
      case CC of
        ccUp, ccDown :
          CommandOK := not IsHorizontal;
        ccLeft, ccRight :
          CommandOK := IsHorizontal;
        else
          CommandOK := True;
      end;
    end;

  begin
    {we're "editing"}
    SetLongFlag(sfFlags, ifEditing);

    {set clCurrent}
    clSetCurrent;
    if clCurrent^.ciProtected then          {!!.30}
      clNextUnprotected(clCurrent);         {!!.30}

    {store cursor shape and set it}
    CursorSL := CursorTypeSL;
    NormalCursor;

    {save break checking state}
    SaveBreak := CheckBreak;
    CheckBreak := False;

    {clear modified flag}
    ClearLongFlag(sfFlags, ifModified);

    {$IFDEF UseMouse}
    case CC of
      ccMouseSel, ccMouseDown :
        if not MouseInCluster(ID, Row, Col) then
          CC := ccNone;
    end;
    {$ENDIF}

    if (CC = ccChar) then {!!.21}
      if LongFlagIsSet(dcFlags, dcSelectLocally) or (clFindSelectionChar(Ch) = nil) then {!!.21}
        CC := ccNone;     {!!.21}

    ColDelta := GetColDelta;

    {loop reading keys}
    Finished := False;
    FirstTime := (CC <> ccNone);

    repeat
      {make sure dcForceExit flag is off}              {!!.11}
      ClearLongFlag(dcFlags, dcForceExit);             {!!.11}

      {redraw}
      RedrawItems(Row, Col, FA, CA);

      {position cursor}
      with clCurrent^ do
        GotoXYabs(Col+Pred(ciCol)+ColDelta, Row+Pred(ciRow));

      {get next command and validate it}
      if FirstTime then
        FirstTime := False
      else
        CC := CP.GetCommand(ChWord);

      case CC of
        ccAltKey,
        ccChar :
          begin
            CIP := clFindSelectionChar(Ch);
            if CIP <> nil then
              Finished := HandleSelection(CC, ChWord, Row, Col)
            else begin
              CtP := FindSelectionChar(Ch);
              if CtP <> nil then
                Finished := True;
            end;
          end;

        ccSelect :
          begin
            CC := dcDefCmd;
            Finished := True;
          end;

        ccTab,               {tab to next field}
        ccBackTab,           {tab to previous field}
        ccNextField, ccPrevField,                      {!!.12}
        ccDone, ccQuit,      {various exit commands}
        ccUser0..ccUser65335 :
          Finished := True;

        ccUp, ccDown, ccLeft, ccRight, ccToggle,
        ccMouseSel, ccMouseDown, ccMouseAuto :
          if CommandOK(CC) then
            Finished := HandleSelection(CC, ChWord, Row, Col);

        ccHelp :             {Invoke help system}
          CP.cpGetHelp(UnitCode, nil, sfHelpIndex);

        else
          Error;
      end;

      {see if we need to exit due to item change}        {!!.11}
      if LongFlagIsSet(dcFlags, dcForceExit) then begin  {!!.11}
        Finished := True;                                {!!.11}
        CC := ccItemChange;                              {!!.11}
        ClearLongFlag(dcFlags, dcForceExit);             {!!.11}
      end;                                               {!!.11}

      {make sure it's OK to go to next/previous field}
      if Finished then
        Finished := efOKtoAdvance(CC);
    until Finished;

    {restore break checking status}
    CheckBreak := SaveBreak;

    {restore cursor shape}
    SetCursorSize(Hi(CursorSL), Lo(CursorSL));

    {we're not editing}
    if CC <> ccItemChange then                 {!!.11}
      ClearLongFlag(sfFlags, ifEditing);
  end;

  function Cluster.FindItem(Id : Byte) : ClusterItemPtr;
    {-Return a pointer to the specified item}
  var
    I : Integer;
    CIP : ClusterItemPtr;
  begin
    CIP := ClusterItemPtr(clList.Head);
    if Id < clList.Size then
      for I := 0 to Integer(Id)-1 do
        CIP := ClusterItemPtr(CIP^.dlNext);
    FindItem := CIP;
  end;

  function Cluster.GetCurrentItem : Byte;
    {-Return ID for current item}
  begin
    GetCurrentItem := clCurrent^.ciId;
  end;

  procedure Cluster.SetCurrentItem(Id : Byte; Direct : ShortInt);  {!!.30}
    {-Set current item}
  var
    CIP : ClusterItemPtr;
  begin
    CIP := FindItem(Id);
    if (CIP <> nil) then
      clCurrent := CIP;
    if clCurrent^.ciProtected then        {!!.30}
      if (Direct < 0) then                {!!.30}
        clPrevUnprotected(clCurrent)      {!!.30}
      else if (Direct > 0) then           {!!.30}
        clNextUnprotected(clCurrent)      {!!.30}
      else                                {!!.30}
        clCurrent := nil;                 {!!.30}
  end;

  procedure Cluster.SelectItem(Id : Byte);
    {-Select specified item}
  var
    CIP : ClusterItemPtr;
  begin
    CIP := FindItem(Id);
    if (CIP <> nil) and not CIP^.ciProtected then   {!!.30}
      CIP^.Select;
  end;

  procedure Cluster.RedrawItems(Row, Col : Word; FA, HA : Byte);
    {-Redraw all items in the cluster}
  var
    I, N, L : Word;
    CIP : ClusterItemPtr;
    Icon : string[20];
    S : string[132];
    SLen : Byte absolute S;
    Marker : Char;
    A, MarkerOfst : Byte;
    Hidden : Boolean;
    HiAttr : Byte;             {!!.30}
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(SaveMouse);
    {$ENDIF}

    {is it hidden?}
    if LongFlagIsSet(sfOptions, sfHidden) then
      ClearWindow(Col, Row, Col+Pred(sfFWidth), Row+Pred(sfFHeight), ' ', FA)
    else begin
      {if protected, no highlight}
      if LongFlagIsSet(sfOptions, sfProtected) then
        HA := FA;

      {if selecting only locally and not editing, no highlight}  {!!.21}
      if LongFlagIsSet(dcFlags, dcSelectLocally) then            {!!.21}
        if not LongFlagIsSet(sfFlags, ifEditing) then            {!!.21}
          HA := FA;                                              {!!.21}

      CIP := ClusterItemPtr(clList.Head);
      N := clList.Size;
      GetIconAndMarker(Icon, Marker, MarkerOfst);

      for I := 1 to N do
        with CIP^ do begin
          S := Icon+ciText^;
          if IsSelected then
            S[MarkerOfst] := Marker;
          if LongFlagIsSet(sfFlags, ifEditing) and (clCurrent = CIP) then begin
            if not UseColor then
              S[1] := CurControlMarker1;
            A := ColorMono(clSelFColor, clSelFMono);
          end
          else
            A := FA;

          HiAttr := HA;                                       {!!.30}
                                                              {!!.30}
          if ciProtected then begin                           {!!.30}
            A := ColorMono(sfProFieldColor, sfProFieldMono);  {!!.30}
            HiAttr := A;                                      {!!.30}
          end;                                                {!!.30}

          if clItemWidth <> 0 then begin
            L := FlexLen(S);
            if L < clItemWidth then
              FillChar(S[SLen+1], sfFWidth-L, ' ');
            SLen := clItemWidth+(SLen-L);
          end;
          opcFlexWrite(S, Row+Pred(ciRow), Col+Pred(ciCol), A, HiAttr);  {!!.30}
          CIP := ClusterItemPtr(dlNext);
        end;
    end;

    {$IFDEF UseMouse}
    ShowMousePrim(SaveMouse);
    {$ENDIF}
  end;

  function Cluster.AddItem(S : string; Row, Col : Byte; VarPtr : Pointer;
                           SelValue : Byte) : Word;
    {-Add a new item to the cluster}
  var
    CIP : ClusterItemPtr;
  begin
    New(CIP, Init(clList.Size, S, Row, Col, VarPtr, SelValue) );
    if CIP = nil then
      AddItem := epFatal+ecOutOfMemory
    else begin
      clList.Append(CIP);
      if clList.Size = 1 then
        clCurrent := CIP;
      AddItem := 0;
    end;
  end;

  procedure Cluster.clSetCurrent;
    {-Initialize clCurrent}
  begin
    {hook}
  end;

  {!!.30 - New}
  procedure Cluster.clNextUnprotected(var Item : ClusterItemPtr);
  var
    SaveCur : ClusterItemPtr;

  begin
    SaveCur := Item;

    while (Item <> nil) and Item^.ciProtected do
      Item := ClusterItemPtr(Item^.dlNext);
    if (Item = nil) then begin
      Item := ClusterItemPtr(clList.Head);
      while (Item <> SaveCur) and Item^.ciProtected do
        Item := ClusterItemPtr(Item^.dlNext);
    end;
  end;

  {!!.30 - New }
  procedure Cluster.clPrevUnprotected(var Item : ClusterItemPtr);
  var
    SaveCur : ClusterItemPtr;

  begin
    SaveCur := Item;

    while (Item <> nil) and Item^.ciProtected do
      Item := ClusterItemPtr(Item^.dlPrev);
    if (Item = nil) then begin
      Item := ClusterItemPtr(clList.Tail);
      while (Item <> SaveCur) and Item^.ciProtected do
        Item := ClusterItemPtr(Item^.dlPrev);
    end;
  end;

  procedure Cluster.GetIconAndMarker(var Icon : string;
                                     var Marker : Char;
                                     var MarkerOfst : Byte);
  begin
    Abstract;
  end;

  function Cluster.HandleSelection(Cmd, Key, Row, Col : Word) : Boolean;
    {-Handle selection commands, returning True if editor should exit}
  begin
    Abstract;
  end;

  function Cluster.clFindSelectionChar(Ch : Char) : ClusterItemPtr;
    {-Return pointer to item with Ch as selection char, or nil}
  var
    CIP : ClusterItemPtr;
  begin
    CIP := ClusterItemPtr(clList.Head);
    while CIP <> nil do
      with CIP^ do
        if HasSelectChar(ciText^, Ch) and not ciProtected then begin  {!!.30}
          clFindSelectionChar := CIP;
          Exit;
        end
        else
          CIP := ClusterItemPtr(dlNext);
    clFindSelectionChar := nil;
  end;

  function Cluster.SelectWithChar(Ch : Char) : Boolean;
    {-Return True if Ch is a valid selection character for this control}
  begin
    if Control.SelectWithChar(Ch) then
      SelectWithChar := True
    else if not LongFlagIsSet(dcFlags, dcSelectLocally) then
      SelectWithChar := clFindSelectionChar(Upcase(Ch)) <> nil
    else
      SelectWithChar := False;
  end;

  {!!.30 - New }
  function Cluster.AllItemsProtected : Boolean;
    {-Return True if all items in cluster are protected }
  var
    OnItem : ClusterItemPtr;

  begin
    AllItemsProtected := False;
    OnItem := ClusterItemPtr(clList.Head);
    while (OnItem <> nil) do begin
      if not OnItem^.ciProtected then
        Exit;
      OnItem := ClusterItemPtr(OnItem^.dlNext);
    end;
    AllItemsProtected := True;
  end;

  procedure Cluster.cpAddAltKeys(var CS : CharSet);
    {-Add to list of acceptable Alt keys}
  var
    Ch : Char;
    CIP : ClusterItemPtr;
  begin
    if LongFlagIsSet(sfOptions, efProtected+efHidden+efInvisible) then  {!!.10}
      Exit;                                                             {!!.10}

    Control.cpAddAltKeys(CS);

    if LongFlagIsSet(dcFlags, dcSelectLocally) and
      not LongFlagIsSet(sfFlags, ifEditing) then
        Exit;

    CIP := ClusterItemPtr(clList.Head);
    while CIP <> nil do
      with CIP^ do begin
        if GetSelectChar(ciText^, Ch) then
          CS := CS+[Ch];
        CIP := ClusterItemPtr(dlNext);
      end;
  end;

{$IFDEF UseMouse}
  function Cluster.MouseInCluster(var Id : Byte; Row, Col : Word) : Boolean;
    {-Return True if the mouse is in the cluster, and the ID for the item.
      Row,Col is the top left corner of the cluster}
  var
    CIP : ClusterItemPtr;
    L, IX, MX, MY : Word;
    Icon : string[20];
    Marker : Char;
    MarkerOfst : Byte;
  begin
    MouseInCluster := False;
    MX := MouseKeyWordX+MouseXLo;
    MY := MouseKeyWordY+MouseYLo;
    if (MX < Col) or (MY < Row) then
      Exit;
    GetIconAndMarker(Icon, Marker, MarkerOfst);
    CIP := ClusterItemPtr(clList.Head);

    {!!.30 - Reworked entire loop }
    while CIP <> nil do begin
      with CIP^ do
        if not CIP^.ciProtected then begin
          if MY = Row+Pred(ciRow) then begin
            IX := Col+Pred(ciCol);
            if clItemWidth = 0 then
              L := FlexLen(Icon+ciText^)
            else
              L := clItemWidth;
            if (MX >= IX) and (MX <= IX+Pred(L)) then begin
              Id := ciID;
              MouseInCluster := True;
              Exit;
            end;
          end;
        end;
      CIP := ClusterItemPtr(CIP^.dlNext);
    end;

  end;
{$ENDIF}

{$IFDEF UseStreams}
  constructor Cluster.Load(var S : IdStream);
    {-Load a cluster from a stream}
  begin
    clList.Init;

    if not Control.Load(S) then
      Fail;

    S.ReadRange(clSelFColor, clList);
    S.Get(clList);
    clCurrent := ClusterItemPtr(clList.Head);
  end;

  procedure Cluster.Store(var S : IdStream);
    {-Store a cluster in a stream}
  begin
    Control.Store(S);
    if S.PeekStatus <> 0 then
      Exit;

    S.WriteRange(clSelFColor, clList);
    S.Put(clList);
  end;

  procedure Cluster.cpGetUserVars(var P : Pointer; var UVP : PointerPtr);
    {-Get user variable pointers}
  var
    CIP : ClusterItemPtr absolute P;
  begin
    if P = nil then
      P := clList.Head
    else
      P := CIP^.dlNext;
    if P = nil then
      UVP := nil
    else
      UVP := @CIP^.ciVarPtr;
  end;

{$ENDIF}

  {------- RadioButtons -------}

  function RadioButtons.HandleSelection(Cmd, Key, Row, Col : Word) : Boolean;
    {-Handle selection commands, returning True if editor should exit}
  var  {!!.11} {numerous changes}
    CurItem : Byte;
    NextItem : Byte;
    LastItem : Byte;
    Ch : Char absolute Key;
    CIP : ClusterItemPtr;
    Changed : Boolean;
  begin
    HandleSelection := False;
    CurItem := GetCurrentItem;
    LastItem := clList.Size-1;

    Changed := False;
    case Cmd of
      ccAltKey,
      ccChar :
        begin
          CIP := clFindSelectionChar(Ch);
          NextItem := CIP^.ciId;
          if (CIP <> nil) and (NextItem <> CurItem) then begin
            SetCurrentItem(NextItem, 0);        {!!.30}
            SelectItem(NextItem);
            SetLongFlag(sfFlags, ifModified);
            Changed := True;
          end;
        end;

      ccLeft,
      ccUp :
        begin
          if CurItem = 0 then
            SetCurrentItem(LastItem, -1)        {!!.30}
          else
            SetCurrentItem(CurItem-1, -1);      {!!.30}
          SelectItem(GetCurrentItem);
          SetLongFlag(sfFlags, ifModified);
          Changed := True;
        end;

      ccRight,
      ccDown :
        begin
          if CurItem = LastItem then
            SetCurrentItem(0, +1)               {!!.30}
          else
            SetCurrentItem(CurItem+1, +1);      {!!.30}
          SelectItem(GetCurrentItem);
          SetLongFlag(sfFlags, ifModified);
          Changed := True;
        end;

    {$IFDEF UseMouse}
      ccMouseAuto,
      ccMouseDown,
      ccMouseSel :
        if not MouseInCluster(NextItem, Row, Col) then
          HandleSelection := not MouseInControl(Row, Col)
        else if NextItem <> CurItem then begin
          SetCurrentItem(NextItem, 0);          {!!.30}
          SelectItem(NextItem);
          SetLongFlag(sfFlags, ifModified);
          Changed := True;
        end;
    {$ENDIF}
    end;

    if Changed and LongFlagIsSet(dcFlags, dcItemChangeExit) then
      SetLongFlag(dcFlags, dcForceExit);
  end;

  procedure RadioButtons.GetIconAndMarker(var Icon : string;
                                          var Marker : Char;
                                          var MarkerOfst : Byte);
    {-Get the icon and selection marker for this cluster}
  begin
    Icon := ' ( ) ';
    Marker := ^G;
    MarkerOfst := 3;
  end;

  procedure RadioButtons.clSetCurrent;
    {-Initialize clCurrent}
  var
    CIP : ClusterItemPtr;

  begin
    CIP := ClusterItemPtr(clList.Head);
    while CIP <> nil do begin
      if not CIP^.ciProtected and (CIP^.ciSelValue = Byte(efVarPtr^)) then begin {!!.30}
        clCurrent := CIP;
        Exit;
      end;
      CIP := ClusterItemPtr(CIP^.dlNext);
    end;
    if CIP  = nil then begin
      clCurrent := ClusterItemPtr(clList.Head);
      clNextUnprotected(clCurrent);                                     {!!.30}
      Byte(efVarPtr^) := clCurrent^.ciSelValue;
    end;
  end;

  {------- CheckBoxes -------}

  function CheckBoxes.HandleSelection(Cmd, Key, Row, Col : Word) : Boolean;
    {-Handle selection commands, returning True if editor should exit}
  var                 {!!.11} {numerous changes}
    CurItem : Byte;
    LastItem : Byte;
    NextItem : Byte;
    Ch : Char absolute Key;
    CIP : ClusterItemPtr;
    Changed : Boolean;

    procedure Toggle;
      {-Toggle the value of the current item}
    begin
      with clCurrent^ do
        if IsSelected then
          Deselect
        else
          Select;
      SetLongFlag(sfFlags, ifModified);
      Changed := True;
    end;

  begin
    HandleSelection := False;
    CurItem := GetCurrentItem;
    LastItem := clList.Size-1;

    Changed := False;
    case Cmd of
      ccAltKey,
      ccChar :
        begin
          CIP := clFindSelectionChar(Ch);
          if (CIP <> nil) then begin
            SetCurrentItem(CIP^.ciId, 0);      {!!.30}
            Toggle;
          end;
        end;

      ccToggle :
        Toggle;

      ccLeft,
      ccUp :
        begin
          if CurItem = 0 then
            SetCurrentItem(LastItem, -1)       {!!.30}
          else
            SetCurrentItem(CurItem-1, -1);     {!!.30}
          SetLongFlag(sfFlags, ifModified);
          Changed := True;
        end;

      ccRight,
      ccDown :
        begin
          if CurItem = LastItem then
            SetCurrentItem(0, +1)              {!!.30}
          else
            SetCurrentItem(CurItem+1, +1);     {!!.30}
          SetLongFlag(sfFlags, ifModified);
          Changed := True;
        end;

      {$IFDEF UseMouse}
      ccMouseAuto,
      ccMouseDown,
      ccMouseSel :
        if MouseInCluster(NextItem, Row, Col) then begin
          SetCurrentItem(NextItem, 0);         {!!.30}
          Changed := (NextItem <> CurItem);
          if (Cmd = ccMouseSel) then
            Toggle;
        end
        else
          HandleSelection := not MouseInControl(Row, Col);
      {$ENDIF}
    end;

    if Changed and LongFlagIsSet(dcFlags, dcItemChangeExit) then
      SetLongFlag(dcFlags, dcForceExit);
  end;

  procedure CheckBoxes.GetIconAndMarker(var Icon : string;
                                        var Marker : Char;
                                        var MarkerOfst : Byte);
    {-Get the icon and selection marker for this cluster}
  begin
    Icon := ' [ ] ';
    Marker := 'X';
    MarkerOfst := 3;
  end;

{$IFDEF UseStreams}

  procedure ControlStream(SPtr : IdStreamPtr);
    {-Register all types for controls}
  begin
    EntryFieldStream(SPtr);

    with SPtr^ do begin
      RegisterType(otControl, veControl, TypeOf(Control),
                   @Control.Store, @Control.Load);
      RegisterPointer(ptNullConversion, @NullConversion);
    end;
  end;

  procedure PushButtonStream(SPtr : IdStreamPtr);
    {-Register all types for push buttons}
  begin
    ControlStream(SPtr);

    with SPtr^ do
      RegisterType(otPushButton, vePushButton, TypeOf(PushButton),
                   @PushButton.Store, @PushButton.Load);
  end;

  procedure ClusterStream(SPtr : IdStreamPtr);
    {-Register all types for clusters}
  begin
    ControlStream(SPtr);
    DoubleListStream(SPtr);

    with SPtr^ do begin
      RegisterType(otCluster, veCluster, TypeOf(Cluster),
                   @Cluster.Store, @Cluster.Load);
      RegisterType(otClusterItem, veClusterItem, TypeOf(ClusterItem),
                   @ClusterItem.Store, @ClusterItem.Load);
    end;
  end;

  procedure RadioButtonsStream(SPtr : IdStreamPtr);
    {-Register all types for radio buttons}
  begin
    ClusterStream(SPtr);

    with SPtr^ do
      RegisterType(otRadioButtons, veRadioButtons, TypeOf(RadioButtons),
                   @RadioButtons.Store, @RadioButtons.Load);
  end;

  procedure CheckBoxesStream(SPtr : IdStreamPtr);
    {-Register all types for check boxes}
  begin
    ClusterStream(SPtr);

    with SPtr^ do
      RegisterType(otCheckBoxes, veCheckBoxes, TypeOf(CheckBoxes),
                   @CheckBoxes.Store, @CheckBoxes.Load);
  end;
{$ENDIF}

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
