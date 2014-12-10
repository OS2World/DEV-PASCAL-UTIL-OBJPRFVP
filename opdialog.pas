{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPDIALOG.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1989, 1992.    *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpDialog;
  {-Dialog box routines}


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
  OpAbsFld,
  OpFEdit,
  OpField,
  OpFrame,
  OpWindow,
  {$IFDEF UseDrag}
  OpDrag,
  {$ENDIF}
  OpSelect,
  OpCtrl;

  {$I OPDIALOG.ICD}  {configuration data}

{.F-}
const
  {option codes for dialog boxes}
  dgSamePosition      = $08; {same cursor position for current control as before?}
  dgEnterToTab        = $20; {used internally}                          {!!.21}
  dgDeallocUserRec    = $40; {used internally}
  dgFastUpdates       = $80; {used internally}

  DefDialogOptions    : Byte = 0;
  BadDialogOptions    : Byte = dgEnterToTab+dgFastUpdates+dgDeallocUserRec; {!!.21}

  DefDgFieldOptions   : LongInt = efInsertPushes+efAutoAdvanceChar+
                                  efTrimBlanks+efMapCtrls+efAllowEscape+
                                  efDefaultAccepted+efShowReadChar;
  BadDgFieldOptions   : LongInt = efMultChoice+efAutoAdvanceCursor;
type
  DialogBoxPtr = ^DialogBox;
  dgUserProc = procedure(DBP : DialogBoxPtr);
  dgControlProc = procedure(CP : ControlPtr; var D; DBP : DialogBoxPtr);
  DialogBox =
    object(AbstractSelector)
      {-----------------------------Procedure pointers}
      dgPreFocusProc  : dgUserProc; {called before focus is moved}
      dgPostFocusProc : dgUserProc; {called after focus is moved}
      {-----------------------------Miscellaneous}
      dgPasswordChar  : Char;       {character used in password mode} {!!.20}
      dgColors        : DialogColorSet; {special dialog box attributes}
      dgPadChar       : Char;       {character used to pad strings}
      dgInsertMode    : Boolean;    {insert mode on?}
      dgFieldFlags    : LongInt;    {internal flags for fields}
      dgControlFlags  : LongInt;    {internal flags for controls}
      {-----------------------------User record data}
      dgUserRecSize   : Word;       {size of the record}
      dgUserRecPtr    : Pointer;    {address of the record}
      {-----------------------------Other}
      dgCFF           : ClearFirstFunc; {routine called by field editor}
      {... methods ...}
      constructor Init(X1, Y1, X2, Y2 : Byte);
        {-Initialize the dialog box}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             var dColors : DialogColorSet);
        {-Initialize the dialog box with custom window options}
      destructor Done; virtual;
        {-Dispose of the dialog box}
      procedure ProcessSelf; virtual;
        {-Process commands}
      {-----------------------------------Add new controls}
      procedure AddPushButton(St : string; Row, Col : Word; cWidth : Byte;
                              HelpIndex : Word; Cmd : Word; Default : Boolean);
        {-Add a pushbutton}
      procedure AddRadioButtons(Prompt : string;  pRow, pCol : Word;
                                cRow, cCol: Word; cWidth, cHeight : Byte;
                                iWidth : Byte; HelpIndex : Word; var rbVar);
        {-Add a radio button box}
      procedure AddRadioButton(S : string; SelValue : Byte);
        {-Add a radio button}
      procedure AddRadioButtonAt(S : string; Row, Col, SelValue : Byte);
        {-Add a radio button at a specific Row,Col}
      procedure AddCheckBoxes(Prompt : string;  pRow, pCol : Word;
                              cRow, cCol: Word; cWidth, cHeight : Byte;
                              iWidth : Byte; HelpIndex : Word);
        {-Add a box for checkboxes}
      procedure AddCheckBox(S : string; var cbVar : Boolean);
        {-Add a check box}
      procedure AddCheckBoxAt(S : string; Row, Col : Byte; var cbVar : Boolean);
        {-Add a check box at a specific Row, Col}
      procedure AddWindowControl(Prompt : string;  pRow, pCol : Word;
                                 cRow, cCol: Word; HelpIndex : Word;
                                 Cmd : Word;       var CW : CommandWindow);
        {-Add a child window to be treated as a control}
      procedure AddEditControl(Prompt : string;  pRow, pCol : Word;
                               Picture : string; cRow, cCol : Word;
                               cWidth : Byte;    HelpIndex : Word;
                               var EditSt : string);
        {-Add an editing control}
      procedure AddSimpleEditControl(Prompt : string;   pRow, pCol : Word;
                                     PicChar : Char;    cRow, cCol : Word;
                                     cWidth, MaxLen : Byte; HelpIndex : Word;
                                     var EditSt : string);
        {-Add an editing control (simple string editor)}
      procedure AddUserEditControl(Prompt : string;     pRow, pCol : Word;
                                   Picture : string;    cRow, cCol : Word;
                                   cWidth : Byte;       HelpIndex : Word;
                                   RangeLo : RangeType; RangeHi : RangeType;
                                   DataSize : Word;     DecimalPlaces : Byte;
                                   Validate: ValidationFunc;
                                   Convert : ConversionProc;
                                   FDraw   : DrawProc;
                                   Editor  : EditProc;
                                   var EditVar);
        {-Add an editing control of a user-defined type}
      {-----------------------------------Procedure pointers}
      procedure SetPreFocusProc(PFP : dgUserProc);
        {-Specify routine to call just before a control receives the focus}
      procedure SetPostFocusProc(PFP : dgUserProc);
        {-Specify routine to call after a control receives the focus}
      {----------------------------Dialog box options}
      procedure dgOptionsOn(OptionFlags : Byte);
        {-Activate multiple options}
      procedure dgOptionsOff(OptionFlags : Byte);
        {-Deactivate multiple options}
      function dgOptionsAreOn(OptionFlags : Byte) : Boolean;
        {-Return true if all specified options are on}
      {----------------------------Control/field options}
      procedure dgControlOptionsOn(OptionFlags : LongInt);
        {-Activate multiple control options}
      procedure dgControlOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple control options}
      function dgControlOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified control options are on}
      procedure dgFieldOptionsOn(OptionFlags : LongInt);
        {-Activate multiple field options}
      procedure dgFieldOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple field options}
      function dgFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified field options are on}
      procedure dgSecFieldOptionsOn(OptionFlags : LongInt);
        {-Activate multiple secondary field options}
      procedure dgSecFieldOptionsOff(OptionFlags : LongInt);
        {-Deactivate multiple secondary field options}
      function dgSecFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
        {-Return true if all specified secondary field options are on}
      {-----------------------------------Colors}
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
        {-Set attributes for button shadows}
      procedure SetClusterAttr(Color, Mono : Byte);
        {-Set attributes for cluster items}
      procedure SetProtectedClusterAttr(Color, Mono : Byte);
        {-Set attributes for protected cluster items}
      procedure SetHighlightClusterAttr(Color, Mono : Byte);
        {-Set attributes for highlighted chars in cluster items}
      procedure SetSelectedClusterAttr(Color, Mono : Byte);
        {-Set attributes for selected cluster items}
      {-----------------------------------Other options}
      procedure SetForceMode(Force, Overtype : Boolean);
        {-Force insert or overtype mode, else use previous setting}
      procedure SetPasswordChar(Ch : Char);     {!!.20}
        {-Set character used in password mode}
      procedure SetPadChar(Ch : Char);
        {-Set character used to pad ends of strings}
      procedure SetBeepOnError(IsOn : Boolean);
        {-Activate/deactivate beeping on error option for all controls}
      procedure SetEnterToTab(IsOn : Boolean);                            {!!.21}
        {-Set EnterToTab option on/off}                                 {!!.21}
      procedure ProtectClusterItem(ClusterID : Word; ItemID : Byte);    {!!.30}
        {-Protect one item in a cluster }                               {!!.30}
      procedure UnprotectClusterItem(ClusterID : Word; ItemID : Byte);  {!!.30}
        {-Unprotect one item in a cluster }                             {!!.30}
      {$IFDEF UseStreams}
      {-----------------------------------Streams}
      constructor Load00(var S : IdStream); {!!.20}
        {-Load a dialog box from a stream}
      constructor Load(var S : IdStream);
        {-Load a dialog box from a stream}
      procedure Store(var S : IdStream);
        {-Store a dialog box in a stream}
      procedure SetUserRecord(var UserRec; UserRecSize : Word);
        {-Set the address and size of the user record}
      function GetUserRecord : Pointer;
        {-Return a pointer to the user record}
      function GetUserRecordSize : Word;
        {-Return the size of the user record}
      {$ENDIF}
      {-----------------------------------Miscellaneous}
      function FindControl(ID : Word) : ControlPtr;
        {-Find the control with the specified ID}
      function GetCurrentItemID : Word;             {!!.11}
        {-Get the ID for the current cluster item}  {!!.11}
      procedure ChangeValidation(ID : Word; VF : ValidationFunc);
        {-Change the validation routine for the specified control}
      procedure ChangeConversion(ID : Word; CP : ConversionProc);
        {-Change the conversion routine for the specified control}
      function EvaluateCommand(var Cmd : Word) : Word;
        {-Given a command, return the ID for the control the cursor will move
          to next. Cmd may be modified on return. This routine is intended to
          be called only from within a post-focus routine!}
      procedure VisitAllControls(FProc : dgControlProc; var D);
        {-Call the specified procedure for all controls in the dialog box}
{.Z+}
      {+++ internal methods +++}
      procedure PositionCursorAt(SFP : SelectFieldPtr); virtual;
      procedure SetWrapMode(WM : WrapMode); virtual;
      procedure asDrawKnownField(SFP : SelectFieldPtr); virtual;
      procedure asResetFlags; virtual;
      procedure dgPreFocus; virtual;
      procedure dgPostFocus; virtual;
      procedure dgCalcAttrs(CP : ControlPtr; var FA, CA, PA : Byte);
      procedure dgDrawControlPrim(CP : ControlPtr; ConvertIt : Boolean);
      procedure dgAddClusterItem(var S : string; Row, Col : Byte;
                                 VarPtr : Pointer; SelValue : Byte);
      procedure dgAppendControl(P : Pointer);
      procedure dgProcessChar;
      function  dgFindDefaultButton : PushButtonPtr;
      function  dgCheckCoords(var Prompt : string; pRow : Word; var pCol : Word;
                              fRow, fCol, fWidth, fHeight : Word) : Boolean;
      function  dgParamsOK(var Prompt : string; pRow : Word; var pCol : Word;
                           var Picture : string; fRow, fCol : Word;
                           var fWidth : Byte; fHeight : Byte) : Boolean;
      function dgRequiredFieldsOK(var CP : ControlPtr;
                                  var ErrCode : Word;
                                  var ErrMsg : StringPtr) : Boolean;
      {$IFDEF UseStreams}
      function dgFixOneUserVar(var IdS : IdStream;
                               CP : ControlPtr) : Boolean;
      function dgFixUserVars(var IdS : IdStream;
                             var CP : ControlPtr) : Boolean;
      function dgRestoreOneUserVar(var IdS : IdStream;
                                   CP : ControlPtr) : Boolean;
      function dgRestoreUserVars(var IdS : IdStream;
                                 Last : ControlPtr) : Boolean;
      {$ENDIF}
      {$IFDEF UseMouse}
      function dgEvaluateMouseCommand(var Cmd : Word;
                                      var ScrollByPage : Boolean;
                                      Execute : Boolean) : Boolean;
      {$ENDIF}
{.Z-}
    end;

  WindowControlPtr = ^WindowControl;
  WindowControl =
    object(Control)         {for embedded command windows}
      wcWin : CommandWindowPtr;

      constructor Init(ID : Word;              var Prompt : string;
                       pRow, pCol : Word;      cRow, cCol : Word;
                       cWidth, cHeight : Byte; HelpIndex : Word;
                       prOptions, seOptions, dcOptions : LongInt;
                       var Colors : ColorSet;  var dColors : DialogColorSet;
                       DefCmd : Word;          var CW : CommandWindow);
        {-Initialize a command window control}
      procedure Convert(PostEdit : Boolean); virtual;
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
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a window control from a stream}
      procedure Store(var S : IdStream);
        {-Store a window control in a stream}
      {$ENDIF}
    end;

{.F+}

var
  {$IFDEF UseDrag}
  DialogCommands : DragProcessor;
  {$ELSE}
  DialogCommands : CommandProcessor;
  {$ENDIF}

{----------- streams ------------}

{.Z+}
{$IFDEF UseStreams}
const
  WindowControlParent : WindowPtr = nil;

procedure DialogBoxStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing dialog boxes}

procedure WindowControlStream(SPtr : IdStreamPtr);
  {-Register all types needed for a WindowControl}

procedure EditControlStream(SPtr : IdStreamPtr);
  {-Register all types needed for an editing control}

procedure SimpleEditControlStream(SPtr : IdStreamPtr);
  {-Register all types needed for a simple editing control}
{$ENDIF}
{.Z-}

  {==========================================================================}

implementation

  {$I OPDIALOG.IN1}

  constructor DialogBox.Init(X1, Y1, X2, Y2 : Byte);
    {-Initialize the dialog box}
  begin
    if not DialogBox.InitCustom(
      X1, Y1, X2, Y2, DefaultColorSet, DefWindowOptions, DefaultDialogColors) then
        Fail;
  end;

  constructor DialogBox.InitCustom(X1, Y1, X2, Y2 : Byte;
                                   var Colors : ColorSet;
                                   Options : LongInt;
                                   var dColors : DialogColorSet);
    {-Initialize the dialog box with custom window options}
  begin
    if not AbstractSelector.InitCustom(X1, Y1, X2, Y2, Colors, Options,
                                       DialogCommands, ucDialog) then
      Fail;

    SetCursor(cuHidden);

    asWrapMode := WrapAtEdges;
    @dgPreFocusProc := nil;
    @dgPostFocusProc := nil;
    dgColors := dColors;
    dgPadChar := DefPadChar;
    dgPasswordChar := DefPasswordChar; {!!.20}
    dgInsertMode := True;
    asOptions := DefDialogOptions;
    asFieldOptions := DefDgFieldOptions;
    dgFieldFlags := DefSEFieldOptions;
    dgControlFlags := DefControlOptions;
    dgUserRecPtr := nil;
    dgUserRecSize := 0;
    dgCFF := DefClearFirstFunc;
  end;

  destructor DialogBox.Done;
    {-Dispose of the dialog box}
  begin
    if ByteFlagIsSet(asOptions, dgDeallocUserRec) then
      FreeMemCheck(dgUserRecPtr, dgUserRecSize);
    AbstractSelector.Done;
  end;

  procedure DialogBox.SetWrapMode(WM : WrapMode);
    {-Set the wrap mode for the DialogBox}
  begin
    {just making sure wrap mode doesn't change }
  end;

  procedure DialogBox.AddPushButton(St : string; Row, Col : Word; cWidth : Byte;
                                    HelpIndex : Word; Cmd : Word; Default : Boolean);
    {-Add a pushbutton}
  var
    CFlags : LongInt;
    H : Word;
  begin
    CFlags := dgControlFlags;
    if Default then
      SetLongFlag(CFlags, dcIsDefault);

    Dec(Col);
    if Col = 0 then begin
      GotError(epFatal+ecBadCoordinates, emNullError);
      Exit;
    end;

    if LongFlagIsSet(CFlags, dcButtonBox) then begin
      H := 3;
      Dec(Row);
      if Row = 0 then begin
        GotError(epFatal+ecBadCoordinates, emNullError);
        Exit;
      end;
      Inc(cWidth, 2);
    end
    else begin
      if LongFlagIsSet(CFlags, dcButtonShadow) then
        H := 2
      else
        H := 1;
      Inc(cWidth, H+1); {!!.20}
    end;

    if dgParamsOK(emNullError, Row, Col, emNullError, Row, Col, cWidth, H) then
      {allocate control and append it to the linked list}
      dgAppendControl(
        New(PushButtonPtr,
            Init(asCount, St, Row, Col, cWidth, HelpIndex, wBackChar,
                 asFieldOptions, dgFieldFlags, CFlags,
                 asColors, dgColors, Cmd) ) );
  end;

  procedure DialogBox.AddRadioButtons(Prompt : string;  pRow, pCol : Word;
                                      cRow, cCol: Word; cWidth, cHeight : Byte;
                                      iWidth : Byte; HelpIndex : Word;
                                      var rbVar);
    {-Add a radio button box}
  begin
    if dgParamsOK(Prompt, pRow, pCol, emNullError, cRow, cCol, cWidth, cHeight) then
      {allocate control and append it to the linked list}
      dgAppendControl(
        New(RadioButtonsPtr,
            Init(asCount, Prompt, pRow, pCol, cRow, cCol, cWidth, cHeight,
                 iWidth, HelpIndex, rbVar, asFieldOptions, dgFieldFlags,
                 dgControlFlags, asColors, dgColors) ) );
  end;

  procedure DialogBox.AddRadioButton(S : string; SelValue : Byte);
    {-Add a radio button}
  var
    CP  : ClusterPtr;
    CIP : ClusterItemPtr;
    Row, Col : Byte;                              {!!.10}
  begin
    CP := ClusterPtr(asFields.Tail);
    if CP <> nil then
      with CP^ do begin
        CIP := ClusterItemPtr(clList.Tail);
        if CIP = nil then begin                   {!!.10}
          Row := 1;
          Col := 1;                               {!!.10}
        end                                       {!!.10}
        else with CIP^ do begin                   {!!.10}
          Row := ciRow+1;
          Col := ciCol;                           {!!.10}
        end;                                      {!!.10}
        AddRadioButtonAt(S, Row, Col, SelValue);  {!!.10}
      end;
  end;

  procedure DialogBox.dgAddClusterItem(var S : string; Row, Col : Byte;
                                       VarPtr : Pointer; SelValue : Byte);
    {-Primitive routine to add a radio button or check box}
  var
    CP : ClusterPtr;
    I : Word;
  begin
    CP := ClusterPtr(asFields.Tail);
    if CP <> nil then
      with CP^ do
        if LongFlagIsSet(dcFlags, dcIsCluster) then begin
          I := AddItem(S, Row, Col, VarPtr, SelValue);
          if I = epFatal+ecOutOfMemory then
            GotError(I, emInsufficientMemory)
          else if I <> 0 then
            GotError(I, emNullError);
        end;
  end;

  procedure DialogBox.AddRadioButtonAt(S : string; Row, Col, SelValue : Byte);
    {-Add a radio button at a specific Row,Col}
  begin
    dgAddClusterItem(S, Row, Col, ClusterPtr(asFields.Tail)^.efVarPtr, SelValue);
  end;

  procedure DialogBox.AddCheckBoxes(Prompt : string;  pRow, pCol : Word;
                                    cRow, cCol: Word; cWidth, cHeight : Byte;
                                    iWidth : Byte; HelpIndex : Word);
    {-Add a box for checkboxes}
  begin
    if dgParamsOK(Prompt, pRow, pCol, emNullError, cRow, cCol, cWidth, cHeight) then
      {allocate control and append it to the linked list}
      dgAppendControl(
        New(CheckBoxesPtr,
            Init(asCount, Prompt, pRow, pCol, cRow, cCol, cWidth, cHeight,
                 iWidth, HelpIndex, emNullError, asFieldOptions, dgFieldFlags,
                 dgControlFlags, asColors, dgColors) ) );
  end;

  procedure DialogBox.AddCheckBox(S : string; var cbVar : Boolean);
    {-Add a check box}
  var
    CP  : ClusterPtr;
    CIP : ClusterItemPtr;
    Row, Col : Byte;                                  {!!.10}
  begin
    CP := ClusterPtr(asFields.Tail);
    if CP <> nil then
      with CP^ do begin
        CIP := ClusterItemPtr(clList.Tail);
        if CIP = nil then begin                       {!!.10}
          Row := 1;
          Col := 1;                                   {!!.10}
        end                                           {!!.10}
        else with CIP^ do begin                       {!!.10}
          Row := ciRow+1;
          Col := ciCol;                               {!!.10}
        end;                                          {!!.10}
        AddCheckBoxAt(S, Row, Col, cbVar);            {!!.10}
      end;
  end;

  procedure DialogBox.AddCheckBoxAt(S : string; Row, Col : Byte;
                                    var cbVar : Boolean);
    {-Add a check box at a specific Row, Col}
  begin
    dgAddClusterItem(S, Row, Col, @cbVar, Ord(True));
  end;

  procedure DialogBox.AddWindowControl(Prompt : string;  pRow, pCol : Word;
                                       cRow, cCol: Word; HelpIndex : Word;
                                       Cmd : Word;       var CW : CommandWindow);
    {-Add a child window to be treated as a control}
  var
    Row, Col, W, H : Integer;
    X1, Y1, X2, Y2 : Byte;
  begin
    {make sure CW isn't a child window and isn't active}
    if (CW.ParentPtr <> nil) or (CW.IsActive) then begin
      GotError(epFatal+ecBadChildError, emNullError);
      Exit;
    end;

    {calculate desired absolute coordinates}
    Row := wYL+Pred(cRow);
    Col := wXL+Pred(cCol);

    {get the current dimensions of the window}
    CW.Extent(X1, Y1, X2, Y2, True);

    {is the window at the right location?}
    if (Row <> Y1) or (Col <> X1) then begin
      {$IFDEF UseAdjustableWindows}
        {move it to the proper position}
        CW.MoveWindow(Col-X1, Row-Y1);
        CW.Extent(X1, Y1, X2, Y2, True);
      {$ELSE}
        {we can't move it, so it's a fatal error}
        GotError(epFatal+ecBadCoordinates, emNullError);
        Exit;
      {$ENDIF}
    end;

    {make sure the window fits within the parent}
    if (X1 < wXL) or (Y1 < wYL) or (X2 > wXH) or (Y2 > wYH) or
       (CW.RawError <> 0) then begin
         GotError(epFatal+ecBadCoordinates, emNullError);
         Exit;
       end;

    {calculate width and height of the child window}
    W := Succ(X2-X1);
    H := Succ(Y2-Y1);

    {check coordinates before adding the control}
    if dgCheckCoords(Prompt, pRow, pCol, cRow, cCol, W, H) then
      {allocate control and append it to the linked list}
      dgAppendControl(
        New(WindowControlPtr,
            Init(asCount, Prompt, pRow, pCol, cRow, cCol, W, H,
                 HelpIndex, asFieldOptions, dgFieldFlags, dgControlFlags,
                 asColors, dgColors, Cmd, CW) ) );

    {add the child window if appropriate}
    if cwGetLastError = 0 then
      AddChild(@CW);
  end;

  procedure DialogBox.AddEditControl(Prompt : string;  pRow, pCol : Word;
                                     Picture : string; cRow, cCol : Word;
                                     cWidth : Byte;    HelpIndex : Word;
                                     var EditSt : string);
    {-Add an editing control}
  begin
    if Picture = '' then
      Picture := CharStr(AnyChar, cWidth);
    if dgParamsOK(Prompt, pRow, pCol, Picture, cRow, cCol, cWidth, 1) then
      {allocate control and append it to the linked list}
      dgAppendControl(
        New(ControlPtr,
            Init(asCount, Prompt, pRow, pCol, Picture, cRow, cCol, cWidth, 1,
                 HelpIndex, BlankRange, BlankRange, Length(Picture)+1, 0,
                 NullValidation, StringConversion, DrawString, StringEditor,
                 EditSt, dgPadChar, asFieldOptions, dgFieldFlags,
                 dgControlFlags, asColors, dgColors) ) );
  end;

  procedure DialogBox.AddSimpleEditControl(Prompt : string;   pRow, pCol : Word;
                                           PicChar : Char;    cRow, cCol : Word;
                                           cWidth, MaxLen : Byte; HelpIndex : Word;
                                           var EditSt : string);
    {-Add an editing control (simple string editor)}
  var
    Picture : string;
  begin
    Picture := CharStr(PicChar, MaxLen);
    if dgParamsOK(Prompt, pRow, pCol, Picture, cRow, cCol, cWidth, 1) then
      {allocate control and append it to the linked list}
      dgAppendControl(
        New(ControlPtr,
            Init(asCount, Prompt, pRow, pCol, Picture, cRow, cCol, cWidth, 1,
                 HelpIndex, BlankRange, BlankRange, Length(Picture)+1, 0,
                 NullValidation, SimpleStringConversion, SimpleDrawString,
                 SimpleStringEditor, EditSt, dgPadChar, asFieldOptions,
                 dgFieldFlags, dgControlFlags, asColors, dgColors) ) );
  end;

  procedure DialogBox.AddUserEditControl(Prompt : string;     pRow, pCol : Word;
                                         Picture : string;    cRow, cCol : Word;
                                         cWidth : Byte;       HelpIndex : Word;
                                         RangeLo : RangeType; RangeHi : RangeType;
                                         DataSize : Word;     DecimalPlaces : Byte;
                                         Validate: ValidationFunc;
                                         Convert : ConversionProc;
                                         FDraw   : DrawProc;
                                         Editor  : EditProc;
                                         var EditVar);
    {-Add an editing control of a user-defined type}
  begin
    if dgParamsOK(Prompt, pRow, pCol, Picture, cRow, cCol, cWidth, 1) then
      {allocate control and append it to the linked list}
      dgAppendControl(
        New(ControlPtr,
            Init(asCount, Prompt, pRow, pCol, Picture, cRow, cCol, cWidth, 1,
                 HelpIndex, RangeLo, RangeHi, DataSize, DecimalPlaces,
                 Validate, Convert, FDraw, Editor, EditVar, dgPadChar,
                 asFieldOptions, dgFieldFlags, dgControlFlags, asColors,
                 dgColors) ) );
  end;

  procedure DialogBox.SetPreFocusProc(PFP : dgUserProc);
    {-Specify routine to call just before a control receives the focus}
  begin
    dgPreFocusProc := PFP;
  end;

  procedure DialogBox.SetPostFocusProc(PFP : dgUserProc);
    {-Specify routine to call after a control receives the focus}
  begin
    dgPostFocusProc := PFP;
  end;

  procedure DialogBox.dgOptionsOn(OptionFlags : Byte);
    {-Activate multiple options}
  begin
    SetByteFlag(asOptions, OptionFlags and not BadDialogOptions);
  end;

  procedure DialogBox.dgOptionsOff(OptionFlags : Byte);
    {-Deactivate multiple options}
  begin
    ClearByteFlag(asOptions, OptionFlags and not BadDialogOptions);
  end;

  function DialogBox.dgOptionsAreOn(OptionFlags : Byte) : Boolean;
    {-Return true if all specified options are on}
  begin
    dgOptionsAreOn := (asOptions and OptionFlags) = OptionFlags;
  end;

  procedure DialogBox.dgControlOptionsOn(OptionFlags : LongInt);
    {-Activate multiple control options}
  begin
    SetLongFlag(dgControlFlags, OptionFlags and not BadControlOptions);
  end;

  procedure DialogBox.dgControlOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple control options}
  begin
    ClearLongFlag(dgControlFlags, OptionFlags and not BadControlOptions);
  end;

  function DialogBox.dgControlOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified control options are on}
  begin
    dgControlOptionsAreOn := (dgControlFlags and OptionFlags) = OptionFlags;
  end;

  procedure DialogBox.dgFieldOptionsOn(OptionFlags : LongInt);
    {-Activate multiple field options}
  begin
    SetLongFlag(asFieldOptions, OptionFlags and not BadDgFieldOptions);
  end;

  procedure DialogBox.dgFieldOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple field options}
  begin
    ClearLongFlag(asFieldOptions, OptionFlags and not BadDgFieldOptions);
  end;

  function DialogBox.dgFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified field options are on}
  begin
    dgFieldOptionsAreOn := (asFieldOptions and OptionFlags) = OptionFlags;
  end;

  procedure DialogBox.dgSecFieldOptionsOn(OptionFlags : LongInt);
    {-Activate multiple secondary field options}
  begin
    SetLongFlag(dgFieldFlags, OptionFlags and not BadSEFieldOptions);
  end;

  procedure DialogBox.dgSecFieldOptionsOff(OptionFlags : LongInt);
    {-Deactivate multiple secondary field options}
  begin
    ClearLongFlag(dgFieldFlags, OptionFlags and not BadSEFieldOptions);
  end;

  function DialogBox.dgSecFieldOptionsAreOn(OptionFlags : LongInt) : Boolean;
    {-Return true if all specified secondary field options are on}
  begin
    dgSecFieldOptionsAreOn := (dgFieldFlags and OptionFlags) = OptionFlags;
  end;

  procedure DialogBox.SetHighlightPromptAttr(Color, Mono : Byte);
    {-Set attributes for highlight chars in prompts}
  begin
    dgColors.SetHighlightPromptAttr(Color, Mono);
  end;

  procedure DialogBox.SetButtonAttr(Color, Mono : Byte);
    {-Set attributes for unselected buttons}
  begin
    dgColors.SetButtonAttr(Color, Mono);
  end;

  procedure DialogBox.SetDefaultButtonAttr(Color, Mono : Byte);
    {-Set attributes for default button when unselected}
  begin
    dgColors.SetDefaultButtonAttr(Color, Mono);
  end;

  procedure DialogBox.SetHighlightButtonAttr(Color, Mono : Byte);
    {-Set attributes for highlighted chars in buttons}
  begin
    dgColors.SetHighlightButtonAttr(Color, Mono);
  end;

  procedure DialogBox.SetSelectedButtonAttr(Color, Mono : Byte);
    {-Set attributes for selected buttons}
  begin
    dgColors.SetSelectedButtonAttr(Color, Mono);
  end;

  procedure DialogBox.SetProtectedButtonAttr(Color, Mono : Byte);
    {-Set attributes for protected buttons}
  begin
    dgColors.SetProtectedButtonAttr(Color, Mono);
  end;

  procedure DialogBox.SetButtonShadowAttr(Color, Mono : Byte);
    {-Set attributes for button shadows}
  begin
    dgColors.SetButtonShadowAttr(Color, Mono);
  end;

  procedure DialogBox.SetClusterAttr(Color, Mono : Byte);
    {-Set attributes for cluster items}
  begin
    dgColors.SetClusterAttr(Color, Mono);
  end;

  procedure DialogBox.SetProtectedClusterAttr(Color, Mono : Byte);
    {-Set attributes for protected cluster items}
  begin
    dgColors.SetProtectedClusterAttr(Color, Mono);
  end;

  procedure DialogBox.SetHighlightClusterAttr(Color, Mono : Byte);
    {-Set attributes for highlighted chars in cluster items}
  begin
    dgColors.SetHighlightClusterAttr(Color, Mono);
  end;

  procedure DialogBox.SetSelectedClusterAttr(Color, Mono : Byte);
    {-Set attributes for selected cluster items}
  begin
    dgColors.SetSelectedClusterAttr(Color, Mono);
  end;

  procedure DialogBox.SetForceMode(Force, Overtype : Boolean);
    {-Force insert or overtype mode, else use previous setting}
  begin
    if Force then begin
      dgFieldOptionsOn(efForceMode);
      if Overtype then
        dgFieldOptionsOn(efForceOvertype)
      else
        dgFieldOptionsOff(efForceOvertype);
    end
    else
      dgFieldOptionsOff(efForceMode+efForceOvertype);
  end;

  procedure DialogBox.SetPasswordChar(Ch : Char);     {!!.20}
    {-Set character used in password mode}
  begin
    dgPasswordChar := Ch;
  end;

  procedure DialogBox.SetPadChar(Ch : Char);
    {-Set character used to pad ends of strings}
  begin
    dgPadChar := Ch;
  end;

  procedure DialogBox.SetBeepOnError(IsOn : Boolean);
    {-Activate/deactivate beeping on error option for all controls}
  var
    SFP : SelectFieldPtr;
  begin
    {set it for the dialog box}
    if IsOn then
      SetLongFlag(asFieldOptions, efBeepOnError)
    else
      ClearLongFlag(asFieldOptions, efBeepOnError);

    {any controls to change the setting for?}
    if asCount > 0 then begin
      SFP := Pointer(asFields.Head);
      while SFP <> nil do begin
        if IsOn then
          SetLongFlag(SFP^.sfOptions, efBeepOnError)
        else
          ClearLongFlag(SFP^.sfOptions, efBeepOnError);
        SFP := Pointer(SFP^.dlNext);
      end;
    end;
  end;

  {!!.21 - New}
  procedure DialogBox.SetEnterToTab(IsOn : Boolean);
    {-Set EnterToTab option for this DialogBox}
  begin
    if IsOn then
      SetByteFlag(asOptions, dgEnterToTab)
    else
      ClearByteFlag(asOptions, dgEnterToTab);
  end;

  {!!.30 - New}
  procedure DialogBox.ProtectClusterItem(ClusterID : Word; ItemID : Byte);
    {-Protect one item in a cluster }
  var
    AControl : ControlPtr;
    ACluster : ClusterPtr absolute AControl;
    ClusterI : ClusterItemPtr;

  begin
    AControl := FindControl(ClusterID);
    if (AControl = nil) then
      Exit;

    if (TypeOf(AControl^) <> TypeOf(CheckBoxes))   and
       (TypeOf(AControl^) <> TypeOf(RadioButtons)) then
      Exit;

    ClusterI := ACluster^.FindItem(ItemID);
    if (ClusterI = nil) then
      Exit;

    ClusterI^.ciProtected := True;
    if ACluster^.AllItemsProtected then
      ChangeProtection(ClusterID, True);
  end;

  {!!.30 - New}
  procedure DialogBox.UnprotectClusterItem(ClusterID : Word; ItemID : Byte);
    {-Unprotect one item in a cluster }
  var
    AControl : ControlPtr;
    ACluster : ClusterPtr absolute AControl;
    ClusterI : ClusterItemPtr;

  begin
    AControl := FindControl(ClusterID);
    if (AControl = nil) then
      Exit;

    if (TypeOf(AControl^) <> TypeOf(CheckBoxes))   and
       (TypeOf(AControl^) <> TypeOf(RadioButtons)) then
      Exit;

    ClusterI := ACluster^.FindItem(ItemID);
    if (ClusterI = nil) then
      Exit;

    ClusterI^.ciProtected := False;
    ChangeProtection(ClusterID, False);
  end;

  function DialogBox.FindControl(ID : Word) : ControlPtr;
    {-Find the control with the specified ID}
  begin
    FindControl := ControlPtr( FindField(ID) );
  end;

  function DialogBox.GetCurrentItemID : Word;   {!!.11}
    {-Get the ID for the current cluster item}  {!!.11}
  var
    CP : ClusterPtr absolute asCurrent;
  begin
    with CP^ do
      if (CP = nil) or not LongFlagIsSet(dcFlags, dcIsCluster) then
        GetCurrentItemID := 0
      else
        GetCurrentItemID := GetCurrentItem;
  end;

  procedure DialogBox.ChangeValidation(ID : Word; VF : ValidationFunc);
    {-Change the validation routine for the specified control}
  var
    EFP : EntryFieldPtr;
  begin
    {find the control}
    EFP := Pointer(FindField(ID));
    if EFP <> nil then
      EFP^.efValidate := VF;
  end;

  procedure DialogBox.ChangeConversion(ID : Word; CP : ConversionProc);
    {-Change the conversion routine for the specified control}
  var
    EFP : EntryFieldPtr;
  begin
    {find the control}
    EFP := Pointer(FindField(ID));
    if EFP <> nil then
      EFP^.efConvert := CP;
  end;

  function DialogBox.EvaluateCommand(var Cmd : Word) : Word;
    {-Given a command, return the ID for the control the cursor will move
      to next. Cmd may be modified on return. This routine is intended to
      be called only from within a post-focus routine!}
  var
    dgCurrent : ControlPtr absolute asCurrent;
    P : Pointer;
    SaveCmd : Word;
    Dummy : Boolean;
  begin
    EvaluateCommand := dgCurrent^.sfID;

    {save asNext and reset}
    P := asNext;
    asNext := nil;
    SaveCmd := cwCmd;

    {move from control to control if appropriate}
    case Cmd of
      ccAltKey,
      ccChar :
        begin
          Cmd := ccChar;
          dgProcessChar;
        end;

      ccBackTab,
      ccPrevField :
        if not asIDisValid(dgCurrent^.sfPrevID) then
          (*asGotoPrevField*)    {!!.30}
          asGotoPrevFieldVisit;  {!!.30}

      ccTab,
      ccNextField,
      ccAutoAdvance :
        if not asIDisValid(dgCurrent^.sfNextID) then
          (*asGotoNextField*)    {!!.30}
          asGotoNextFieldVisit;  {!!.30}

      {$IFDEF UseMouse}
      ccMouseDown,
      ccMouseSel :          {user clicked left mouse button}
        if cwCmdPtr^.MouseEnabled then
          Dummy := dgEvaluateMouseCommand(cwCmd, Dummy, False); {!!.12}
      {$ENDIF}
    end;

    if asNext <> nil then
      EvaluateCommand := asNext^.sfID;
    asNext := P;
    Cmd := cwCmd;                                               {!!.12}
    cwCmd := SaveCmd;
  end;

  procedure DialogBox.VisitAllControls(FProc : dgControlProc; var D);
    {-Call the specified procedure for all controls in the dialog box}
  var
    CP : ControlPtr;
  begin
    CP := Pointer(asFields.Head);
    while CP <> nil do begin
      FProc(CP, D, @Self);
      CP := Pointer(CP^.dlNext);
    end;
  end;

  procedure DialogBox.PositionCursorAt(SFP : SelectFieldPtr);
    {-Puts the cursor on the beginning of the specified control}
  var
    Row, Col : Integer;
    StBgn : Byte;
    Flags : PictureFlags;
    SaveCurrent : SelectFieldPtr;
  begin
    with ControlPtr(SFP)^ do begin
      {make sure the control is on the screen}
      SaveCurrent := asCurrent;
      asCurrent := SFP;
      asFixWindow(True, False);
      asCurrent := SaveCurrent;

      if asFixCoordinates(sfFRow, sfFCol, afWidth, sfFHeight, Row, Col) then begin {!!.22}
        {find the first non-literal}
        InitPictureFlags(Flags);
        for StBgn := 1 to MaxWord(1, Length(efPicture^)) do
          if Flags[StBgn] then begin
            GoToXYabs(Col+Pred(StBgn), Row);
            {$IFDEF UseScrollBars}
            asUpdateScrollBars;
            {$ENDIF}
            Exit;
          end;
      end;

      GotError(epFatal+ecFieldNotFound, emNullError);
    end;
  end;

  procedure DialogBox.asDrawKnownField(SFP : SelectFieldPtr);
    {-Draw a field/control when its address is known}
  begin
    dgDrawControlPrim(ControlPtr(SFP), True);
  end;

  procedure DialogBox.asResetFlags;
    {-Reset internal flags}
  var
    SFP, SFPL : SelectFieldPtr;
    LowCol, HighCol : Word;
    I : Word;
  begin
    {do nothing if first/last row already known}
    if (asMinRow <> 0) and (asMaxRow <> 0) then
      Exit;

    {no fast updates}
    ClearByteFlag(asOptions, dgFastUpdates);

    {find the first unprotected control}
    SFP := Pointer(asFields.Head);
    while asFieldIsProtected(SFP) do
      SFP := Pointer(SFP^.dlNext);
    if SFP = nil then begin                       {!!.14}
      GotError(epFatal+ecNoFields, emNullError);  {!!.14}
      Exit;                                       {!!.14}
    end;                                          {!!.14}
    asMinRow := SFP^.sfFRow;

    {find the last unprotected control}
    SFPL := Pointer(asFields.Tail);
    while asFieldIsProtected(SFPL) do
      SFPL := Pointer(SFPL^.dlPrev);
    asMaxRow := SFPL^.sfFRow;

    {do nothing if min/max column already known}
    if (asMinCol <> 0) and (asMaxCol <> 0) then begin
      {$IFDEF UseScrollBars}
      {set up for scroll bars and exit}
      asSetupForScrollBars;
      {$ENDIF}
      Exit;
    end;

    LowCol := $FFFF;
    HighCol := 0;
    SFP := Pointer(asFields.Head);
    while SFP <> nil do
      with SFP^ do begin
        {check control}
        LowCol := MinWord(LowCol, sfFCol);
        HighCol := MaxWord(HighCol, sfFCol);

        {follow link}
        SFP := Pointer(SFP^.dlNext);
      end;

    asMinCol := LowCol;
    asMaxCol := HighCol;

    {$IFDEF UseScrollBars}
    {setup for scroll bars}
    asSetupForScrollBars;
    {$ENDIF}
  end;

  procedure DialogBox.dgPreFocus;
    {-Called before a new control is given the focus}
  begin
    if @dgPreFocusProc <> nil then
      dgPreFocusProc(@Self);
  end;

  procedure DialogBox.dgPostFocus;
    {-Called after a control has given up the focus}
  begin
    if @dgPostFocusProc <> nil then
      dgPostFocusProc(@Self);
  end;

  procedure DialogBox.dgCalcAttrs(CP : ControlPtr; var FA, CA, PA : Byte);
    {-Determine attributes for controls, ctrl/highlight characters, and prompts}
  var
    PBP : PushButtonPtr absolute CP;
  begin
    with PBP^ do begin
      {do nothing if control is invisible}
      if LongFlagIsSet(sfOptions, sfInvisible) then
        Exit;

      {is control hidden?}
      if LongFlagIsSet(sfOptions, sfHidden) then begin
        FA := ColorMono(wTextColor, wTextMono);
        PA := FA;
        CA := FA;
      end
      else begin
        if asFieldIsProtected(CP) then begin
          FA := ColorMono(sfProFieldColor, sfProFieldMono);
          PA := ColorMono(sfProPromptColor, sfProPromptMono);
        end
        else if (sfID = asCurID) or LongFlagIsSet(sfFlags, ifEditing) then begin
          FA := ColorMono(sfSelFieldColor, sfSelFieldMono);
          PA := ColorMono(sfSelPromptColor, sfSelPromptMono);
        end
        else begin
          if dcOptionsAreOn(dcIsButton+dcIsDefault) then
            FA := ColorMono(buDefColor, buDefMono)
          else
            FA := ColorMono(sfFieldColor, sfFieldMono);
          PA := ColorMono(sfPromptColor, sfPromptMono);
        end;

        {determine attribute for control characters}
        CA := ColorMono(sfCtrlColor, sfCtrlMono);
      end;
    end;
  end;

  procedure DialogBox.dgDrawControlPrim(CP : ControlPtr; ConvertIt : Boolean);
    {-Redraw the specified control}
  var
    FRow, FCol : Integer;
    PRow, PCol : Integer;
    PFlags : PictureFlags;
    FA, PA, CA, PHA : Byte; {!!.10}
    Offset : Integer;         {!!.20}
    Hidden, SemiHidden, Editing : Boolean;
    LD, RD : Char;
    HaveP, HaveF : Boolean;
    S : string[127];
    {$IFDEF UseMouse}
    SaveMouse : Boolean;
    {$ENDIF}
  begin
    if CP = nil then
      Exit;
    with CP^ do begin
      {don't convert if editing}
      Editing := LongFlagIsSet(sfFlags, ifEditing);
      ConvertIt := ConvertIt and not Editing;

      {don't draw it if control is invisible}
      if LongFlagIsSet(sfOptions, sfInvisible) then
        Exit;

      {adjust coordinates}
      HaveP := asFixCoordinates(sfPRow, sfPCol, sfPWidth, 1, PRow, PCol);
      HaveF := asFixCoordinates(sfFRow, sfFCol, afWidth, sfFHeight, FRow, FCol); {!!.22}
      if not (HaveP or HaveF) then
        Exit;

      {do the conversion if necessary}
      if ConvertIt then
        Convert(False);

      {set up for Draw routine}
      InitPictureFlags(PFlags);

      {is the control hidden or semi-hidden?}
      Hidden := LongFlagIsSet(sfOptions, sfHidden);
      SemiHidden := LongFlagIsSet(sfFlags, ifSemiHidden);

      {determine attributes to use for the control}
      dgCalcAttrs(CP, FA, CA, PA);

      {$IFDEF UseMouse}
      HideMousePrim(SaveMouse);
      {$ENDIF}

      {draw the prompt}
      if HaveP then
        if Hidden then
          FastFill(sfPWidth, ' ', PRow, PCol, PA)
        else begin
          S := efPrompt^;
          if not UseColor then
            if Editing or (sfID = asCurID) then
              S[1] := CurControlMarker1;
          if LongFlagIsSet(sfOptions, efProtected) then  {!!.10}
            PHA := PA                                    {!!.10}
          else                                           {!!.10}
            PHA := ColorMono(dcHighPColor, dcHighPMono); {!!.10}
          opcFlexWrite(S, PRow, PCol, PA, PHA);          {!!.10}
        end;

      if HaveF then begin
        if SemiHidden and not Hidden then
          {make it look hidden to the Draw routine}
          SetLongFlag(sfOptions, sfHidden);

        {draw the string}
        if (sfID = asCurID) or LongFlagIsSet(sfFlags, ifEditing) then {!!.20}
          Offset := efHOffset                                         {!!.20}
        else                                                          {!!.20}
          Offset := 0;                                                {!!.20}
        Draw(efEditSt^, FRow, FCol, FA, CA, Offset, dgPasswordChar, PFlags); {!!.20}

        if SemiHidden and not Hidden then
          {clear the phony hidden flag}
          ClearLongFlag(sfOptions, sfHidden);
      end;

      {$IFDEF UseMouse}
      ShowMousePrim(SaveMouse);
      {$ENDIF}
    end;
  end;

  procedure DialogBox.dgAppendControl(P : Pointer);
    {-Append the control pointed to by P to the list of controls}
  begin
    {check for pending error}
    if cwGetLastError <> 0 then
      Exit;

    if P = nil then
      GotError(epFatal+ecOutOfMemory, emInsufficientMemory)
    else begin
      {append it to the linked list}
      asFields.Append(P);

      {set pointers if this is the first control}
      if asCount = 0 then begin
        asCurrent := P;
        asKnown := P;
        asNext := P;
      end;

      {increment control count}
      Inc(asCount);
    end;
  end;

  function DialogBox.dgCheckCoords(var Prompt : string; pRow : Word; var pCol : Word;
                                   fRow, fCol, fWidth, fHeight : Word) : Boolean;
    {-Check the coordinates of a control about to be added}
  var
    PLen : Byte absolute Prompt;
  begin
    {make sure the prompt coordinates are OK}
    dgCheckCoords := False;
    if (PLen > 0) then
      if PCol = 1 then
        Exit
      else begin
        Prompt := wBackChar+Prompt;
        Dec(pCol);
        if not asCoordsOK(pRow, pCol, PLen-Ord(Pos('&', Prompt) <> 0), 1) then {!!.21}
          Exit;
      end;

    {make sure the control coordinates are OK}
    if fWidth = 0 then
      fWidth := 1;
    dgCheckCoords := asCoordsOK(fRow, fCol, fWidth, fHeight);
  end;

  function DialogBox.dgParamsOK(var Prompt : string; pRow : Word; var pCol : Word;
                                var Picture : string; fRow, fCol : Word;
                                var fWidth : Byte; fHeight : Byte) : Boolean;
    {-Return True if the specified values are OK for a Control}
  var
    PLen : Byte absolute Picture;
    SaveLen : Byte;
  begin
    {make sure picture mask isn't too long}
    if PLen = 255 then
      Dec(PLen);

    {make sure that 0 > fWidth <= PLen}
    if (PLen > 0) then
      if (fWidth = 0) or (fWidth > PLen) then
        fWidth := PLen;

    {make sure the coordinates are OK}
    dgParamsOK := dgCheckCoords(Prompt, pRow, pCol, fRow, fCol, fWidth, fHeight);
  end;

{$IFDEF UseMouse}
  function DialogBox.dgEvaluateMouseCommand(var Cmd : Word;
                                            var ScrollByPage : Boolean;
                                            Execute : Boolean) : Boolean;
    {-Evaluate ccMouseSel command. Parameters:
        Cmd may be changed to ccClickExit or ccNested on return.
        ScrollByPage may be forced to True on return.
        Execute should be False if goal is only to evaluate the command.
     }
  var
    cCurrent : ControlPtr absolute asCurrent;
  begin
    dgEvaluateMouseCommand := False;
    with cCurrent^ do
      case asProcessMouseCommand(Cmd, ScrollByPage) of
        1 : dgEvaluateMouseCommand := True;
        2 : {user clicked on current control}
            if (Cmd = ccMouseSel) then
              if LongFlagIsSet(sfOptions, efClickExit) then begin
                Cmd := ccClickExit;
                dgEvaluateMouseCommand := True;
              end
              else if LongFlagIsSet(sfFlags, ifNested) then begin
                Cmd := ccNested;
                dgEvaluateMouseCommand := True;
              end;
        3 : {user clicked on another control}
            if LongFlagIsSet(asNext^.sfFlags, ifNested) then begin
              {jump right to the nested field and exit}
              if Execute then begin
                asCurrent := asNext;
                asCurID := asNext^.sfID;
              end;
              if (Cmd = ccMouseSel) then {begin}   {!!.12}
                Cmd := ccNested;
                dgEvaluateMouseCommand := True;
              {end;}                               {!!.11}
            end;
      end;
  end;
{$ENDIF}

{$IFDEF UseStreams}

  function DialogBox.dgFixOneUserVar(var IdS : IdStream;
                                     CP : ControlPtr) : Boolean;
    {-Fix up one user variable pointer}
  begin
    if CP = nil then
      dgFixOneUserVar := False
    else
      dgFixOneUserVar := CP^.cpFixUserVars(IdS, dgUserRecPtr, dgUserRecSize);
  end;

  function DialogBox.dgFixUserVars(var IdS : IdStream;
                                   var CP : ControlPtr) : Boolean;
    {-Fix up all user variable pointers}
  begin
    dgFixUserVars := True;
    CP := ControlPtr(asFields.Head);
    while CP <> nil do
      if dgFixOneUserVar(IdS, CP) then
        CP := ControlPtr(CP^.dlNext)
      else begin
        dgFixUserVars := False;
        Exit;
      end;
  end;

  function DialogBox.dgRestoreOneUserVar(var IdS : IdStream;
                                         CP : ControlPtr) : Boolean;
    {-Restore one user variable pointer}
  begin
    if CP = nil then
      dgRestoreOneUserVar := False
    else
      dgRestoreOneUserVar :=
        CP^.cpRestoreUserVars(IdS, dgUserRecPtr, dgUserRecSize);
  end;

  function DialogBox.dgRestoreUserVars(var IdS : IdStream;
                                       Last : ControlPtr) : Boolean;
    {-Restore all user variable pointers}
  var
    CP : ControlPtr;
  begin
    dgRestoreUserVars := False;
    CP := ControlPtr(asFields.Head);

    while CP <> Last do
      if dgRestoreOneUserVar(IdS, CP) then
        CP := ControlPtr(CP^.dlNext)
      else
        Exit;

    dgRestoreUserVars := True;
  end;

  constructor DialogBox.Load00(var S : IdStream); {!!.20}
    {-Load a dialog box from a stream}
  var
    WFP : WindowPtr;
    F : Boolean;
  begin
    dgPasswordChar := DefPasswordChar; {!!.20}
    dgUserRecPtr := nil;
    dgCFF := DefClearFirstFunc;

    {Load the underlying raw selector}
    WFP := WindowControlParent;
    WindowControlParent := @Self;
    F := not AbstractSelector.Load(S);
    WindowControlParent := WFP;
    if F then
      Fail;

    {set the command processor if necessary}
    if cwCmdPtr = nil then
      SetCommandProcessor(DialogCommands);

    {read data specific to the dialog box}
    @dgPreFocusProc := S.ReadPointer;
    @dgPostFocusProc := S.ReadPointer;
    S.ReadRange(dgColors, dgUserRecPtr);
    dgUserRecPtr := S.ReadPointer;

    {check the error status}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {allocate the user's record if necessary}
    if (dgUserRecSize <> 0) and (dgUserRecPtr = nil) then begin
      if not GetMemCheck(dgUserRecPtr, dgUserRecSize) then begin
        InitStatus := epFatal+ecOutOfMemory;
        Done;
        Fail;
      end
      else
        SetByteFlag(asOptions, dgDeallocUserRec);
    end
    else
      ClearByteFlag(asOptions, dgDeallocUserRec);

    {fix up the addresses in the controls}
    if not dgRestoreUserVars(S, nil) then begin
      InitStatus := epFatal+ecIdUnknown;
      Done;
      Fail;
    end;
  end;

  constructor DialogBox.Load(var S : IdStream); {!!.20}
    {-Load a dialog box from a stream}
  begin
    if not DialogBox.Load00(S) then
      Fail;
    S.ReadRange(dgPasswordChar, dgColors);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure DialogBox.Store(var S : IdStream);
    {-Store a dialog box in a stream}
  var
    CP : ControlPtr;
  begin
    {try to fix up the addresses in the controls}
    if not dgFixUserVars(S, CP) then begin
      {we failed--undo what we did}
      if not dgRestoreUserVars(S, CP) then {internal error} ;

      {report the error}
      S.Error(epNonFatal+ecIdNotRegistered);
      Exit;
    end;

    {Store the abstract selector}
    AbstractSelector.Store(S);

    {restore the addresses in the controls}
    if not dgRestoreUserVars(S, CP) then {internal error} ;

    {Write data specific to the dialog box}
    S.WriteUserPointer(@dgPreFocusProc, ptNil);
    S.WriteUserPointer(@dgPostFocusProc, ptNil);
    S.WriteRange(dgColors, dgUserRecPtr);
    S.WriteUserPointer(dgUserRecPtr, ptNil);
    S.WriteRange(dgPasswordChar, dgColors);  {!!.20}
  end;

  procedure DialogBox.SetUserRecord(var UserRec; UserRecSize : Word);
    {-Set the address and size of the user record}
  begin
    dgUserRecPtr := @UserRec;
    dgUserRecSize := UserRecSize;
  end;

  function DialogBox.GetUserRecord : Pointer;
    {-Return a pointer to the user record}
  begin
    GetUserRecord := dgUserRecPtr;
  end;

  function DialogBox.GetUserRecordSize : Word;
    {-Return the size of the user record}
  begin
    GetUserRecordSize := dgUserRecSize;
  end;

  procedure DialogBoxStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing dialog boxes}
  begin
    {register the abstract selector}
    AbstractSelectorStream(SPtr);

    {register the push button type}
    PushButtonStream(SPtr);

    with SPtr^ do begin
      RegisterType(otDialogBox, veDialogBox, TypeOf(DialogBox),
                   @DialogBox.Store, @DialogBox.Load);
      RegisterOldVersion(otDialogBox, 00, TypeOf(DialogBox),   {!!.20}
                         @DialogBox.Load00);                   {!!.20}
      RegisterPointer(ptDialogCommands, @DialogCommands);
      RegisterPointer(ptNestedFieldVar, @NullMsgLen);
    end;
  end;
{$ENDIF}

  function DialogBox.dgRequiredFieldsOK(var CP : ControlPtr;
                                        var ErrCode : Word;
                                        var ErrMsg : StringPtr) : Boolean;
    {-Make sure all required fields are filled in}
  var
    SFP : SelectFieldPtr absolute CP;
  begin
    {start with first unprotected field}
    SFP := Pointer(asFields.Head);
    while (SFP <> nil) do begin
      with CP^ do
        {is it a required field?}
        if LongFlagIsSet(sfOptions, efRequired) then
          {check only unprotected fields}
          if not asFieldIsProtected(SFP) then
            {it can't be empty}
            if efFieldIsEmpty then begin
              dgRequiredFieldsOK := False;
              ErrCode := epWarning+ecFieldRequired;
              ErrMsg := @emFieldRequired;
              Exit;
            end;

      {next field}
      SFP := Pointer(SFP^.dlNext);
    end;

    dgRequiredFieldsOK := True;
  end;

  procedure DialogBox.dgProcessChar;
    {-Process ccChar command}
  var
    cCurrent : ControlPtr absolute asCurrent;
    CP : ControlPtr;
    Ch : Char absolute cwKey;
  begin
    CP := cCurrent^.FindSelectionChar(Ch);
    if CP <> nil then
      asNext := CP;
  end;

  function DialogBox.dgFindDefaultButton : PushButtonPtr;
    {-Return a pointer to the default pushbutton}
  var
    PBP : PushButtonPtr;
  begin
    dgFindDefaultButton := nil;
    PBP := PushButtonPtr(asFields.Head);
    while PBP <> nil do
      with PBP^ do
        if dcOptionsAreOn(dcIsButton+dcIsDefault) then begin
          dgFindDefaultButton := PBP;
          Exit;
        end
        else
          PBP := PushButtonPtr(dlNext)
  end;

  procedure DialogBox.ProcessSelf;
    {-Process commands}
  var
    cCurrent : ControlPtr absolute asCurrent;
    CP : ControlPtr;
    SavePBP : PushButtonPtr;
    ErrCode : Word;
    ErrMsg : StringPtr;
    Finished, OK, Stop : Boolean;
    Modified, Dummy : Boolean;
    PosCode : Byte;
    Row, Col : Integer;
    FA, CA, PA : Byte;
    Cmd, SaveCmd : Word;
    SaveCurrent : ControlPtr;
    SaveCFF : ClearFirstFunc;
    SaveFlags : LongInt;            {!!.11}
    AltKeys : CharSet;
    SaveAltKeys : ^CharSet;         {!!.20}

  label
    TopOfLoop, ExitPoint;

    function IsCancelButton(CP : ControlPtr) : Boolean; {!!.20} {new}
    begin
      IsCancelButton :=
        LongFlagIsSet(CP^.dcFlags, dcIsButton) and (CP^.dcDefCmd = ccQuit);
    end;

    procedure BuildAltKeySet;
      {-Build Alt key set}
    var
      CP : ControlPtr;
    begin
      FillChar(AltKeys, SizeOf(AltKeys), 0);
      SetLongFlag(cCurrent^.sfFlags, ifEditing);
      CP := ControlPtr(asFields.Head);
      while CP <> nil do
        with CP^ do begin
          cpAddAltKeys(AltKeys);
          CP := ControlPtr(dlNext)
        end;
      ClearLongFlag(cCurrent^.sfFlags, ifEditing);
    end;

    function ReadOnly : Boolean; {!!.21}
    begin
      ReadOnly := LongFlagIsSet(cCurrent^.sfOptions, efReadOnly);
    end;

  begin
    {do nothing if there are no controls}
    if asCount = 0 then begin
      GotError(epFatal+ecNoFields, emNullError);
      Exit;
    end;

    { establish default visitation order }        {!!.30}
    asEstablishDefaultLinks;                      {!!.30}

    {make sure asNext <> nil}
    if asNext = nil then
      if asCurrent = nil then
        asNext := Pointer(asFields.Head)
      else
        asNext := asCurrent;
    asCurrent := asNext;

    {don't start on a protected field}
    if not asNextFieldOK then begin               {!!.13}
      GotError(epFatal+ecNoFields, emNullError);  {!!.13}
      Exit;
    end;                                          {!!.13}

    {draw everything}
    asCurID := BadFieldID;

    {Draw initial screen if not already done}
    Draw;
    if RawError <> 0 then
      Exit;

    SaveAltKeys := Pointer(cwCmdPtr^.cpAltKeys);  {!!.20}
    cwCmdPtr^.SetAltKeySet(AltKeys);

    Finished := False;
    SaveCurrent := nil;
    SavePBP := dgFindDefaultButton;
    repeat

TopOfLoop:
      {make sure the next control is OK}
      if asNext = nil then
        asNext := asCurrent
      else if not asNextFieldOK then begin
        GotError(epFatal+ecNoFields, emNullError);
        goto ExitPoint;
      end;

      {reset current control}
      asCurrent := asNext;
      asCurID := asCurrent^.sfID;
      asNext := nil;

      with cCurrent^ do begin
        {update entire screen if necessary}
        if not ByteFlagIsSet(asOptions, dgFastUpdates) then
          UpdateContents;

        {do the conversion}
        Convert(False);

        repeat
          {save the last command temporarily}
          SaveCmd := cwCmd;
          cwCmd := ccNone;

          {call the pre-focus routine}
          dgPreFocus;

          {make sure asNext wasn't changed}
          if asNext <> nil then
            goto TopOfLoop;

          {set position code if necessary}
          if ByteFlagIsSet(asOptions, dgSamePosition) then begin
            ClearByteFlag(asOptions, dgSamePosition);
            PosCode := 4;
          end
          else if SaveCurrent = cCurrent then
            PosCode := 4
          else
            PosCode := 0;
          SaveCurrent := cCurrent;

          {make sure the default button is highlighted correctly}
          if LongFlagIsSet(dcFlags, dcIsButton) then
            if (ControlPtr(SavePBP) <> cCurrent) then begin
              ClearLongFlag(SavePBP^.dcFlags, dcIsDefault);
              dgDrawControlPrim(SavePBP, False);
              SetLongFlag(cCurrent^.dcFlags, dcIsDefault);
            end;

          {draw the current control}
          dgDrawControlPrim(cCurrent, False);

          {position cursor on the current control}
          if PosCode <> 4 then
            PositionCursorAt(asCurrent);

          {get video attributes for this control}
          dgCalcAttrs(cCurrent, FA, CA, PA);

          {get screen coordinates for the current control}
          if asFixCoordinates(sfFRow, sfFCol, afWidth, sfFHeight, Row, Col) then {}; {!!.22}

          {does the control need to process a command on entry?}
          if LongFlagIsSet(dcFlags, dcSelectByChar) then
            case SaveCmd of
              ccAltKey, ccChar, ccMouseSel, ccMouseDown :
                cwCmd := SaveCmd;
            end;

          {build Alt key set}
          BuildAltKeySet;

          {call the editor for the current control}
          SaveCFF := feClearFirstFunc;
          feClearFirstFunc := dgCFF;
          Modified := False;
          repeat
            Stop := True;
            SaveFlags := sfFlags;                           {!!.11}
            ClearLongFlag(sfFlags, ifSemiHidden);           {!!.11}
            Edit(                                       {vvvvvvvv} {!!.21}
              Row, Col, FA, CA, dgPasswordChar, PosCode, ReadOnly, cwCmd, cwKey, {!!.20}
              dgInsertMode, cwErrorProc, cwUnitCode, cwCmdPtr^);
            if LongFlagIsSet(SaveFlags, ifSemiHidden) then  {!!.11}
              SetLongFlag(sfFlags, ifSemiHidden);           {!!.11}
           {$IFDEF UseMouse}
           case cwCmd of                                              {!!.20}
             ccMouseSel, ccMouseDown, ccMouseAuto:                    {!!.20}
               if (cwCmdPtr^.MouseEnabled) then begin
                 Cmd := cwCmd;
                 Stop := dgEvaluateMouseCommand(Cmd, Dummy, False) or {!!.12}
                         (Cmd = ccIncChoice) or                       {!!.12}
                         ((asNext <> asCurrent) and (asNext <> nil));
                 if Stop and (asNext <> nil) then                       {!!.20}
                   if not IsCancelButton(ControlPtr(asNext)) then       {!!.20}
                     if LongFlagIsSet(sfOptions, efRequired) and        {!!.20}
                       efFieldIsEmpty then begin                        {!!.20}
                         {display error message}                        {!!.20}
                         if Cmd <> ccMouseAuto then                     {!!.20}
                           GotError(epWarning+ecFieldRequired, emFieldRequired); {!!.20}
                         ClearErrors;                                   {!!.20}
                         Stop := False;                                 {!!.20}
                       end;                                             {!!.20}
                 if not Stop then begin
                   cwCmd := ccNone;
                   PosCode := 4;
                 end;
               end;
           end;                                                        {!!.20}
           {$ENDIF}
           Modified := Modified or LongFlagIsSet(sfFlags, ifModified);
          until Stop;
          feClearFirstFunc := SaveCFF;

          {do the conversion if the control was modified}
          if Modified then begin
            SetLongFlag(sfFlags, ifModified);
            {trim leading and trailing blanks if desired}
            if LongFlagIsSet(sfOptions, efTrimBlanks) then
              efTrimSpaces;
            Convert(True);
          end;

          {if Escape not pressed, validate and convert}
          OK := True;
          if (cwCmd <> ccQuit) then begin
            {if it's a required field, it can't be empty}
            case cwCmd of
              ccNested,
              ccClickExit,
              ccMouseSel,
              ccMouseDown,
              ccUser0..ccUser65335 :
                {don't check} ;
              else
                if LongFlagIsSet(sfOptions, efRequired) and efFieldIsEmpty then begin
                  {display error message}
                  GotError(epWarning+ecFieldRequired, emFieldRequired);
                  OK := False;
                end;
            end;
          end;

          {make sure the default button is highlighted correctly}
          if LongFlagIsSet(dcFlags, dcIsButton) then
            if (ControlPtr(SavePBP) <> cCurrent) then begin
              SetLongFlag(SavePBP^.dcFlags, dcIsDefault);
              dgDrawControlPrim(SavePBP, False);
              ClearLongFlag(cCurrent^.dcFlags, dcIsDefault);
            end;

          {redraw the control}
          DrawField(asCurID);
        until OK;

        {call post-focus routine}
        dgPostFocus;

        {if command was ccSelect, return command assoc. with default button}
        if cwCmd = ccSelect then begin                                                                  {!!.21}
          if (not LongFlagIsSet(dcFlags, dcIsButton)) and (ByteFlagIsSet(asOptions, dgEnterToTab)) then {!!.21}
            cwCmd := ccTab                                                                              {!!.21}
          else if (not LongFlagIsSet(dcFlags, dcIsButton)) and (SavePBP <> nil) then {!!.11}            {!!.21}
            cwCmd := SavePBP^.dcDefCmd
          else
            cwCmd := dcDefCmd;
        end;                                                                                            {!!.21}

        {move from control to control if appropriate}
        case cwCmd of
          ccAltKey,
          ccChar :
            begin
              cwCmd := ccChar;
              dgProcessChar;
            end;

          ccBackTab,
          ccPrevField :
            if not asIDisValid(sfPrevID) then
              asGotoPrevField;

          ccTab,
          ccNextField,
          ccAutoAdvance :
            if not asIDisValid(sfNextID) then
              asGotoNextField;

          ccSelect,
          ccNested,
          ccDone,
          ccQuit,
          ccUser0..ccUser65335 :
            Finished := True;

          {$IFDEF UseMouse}
          ccMouseDown,
          ccMouseSel :          {user clicked left mouse button}
            if cwCmdPtr^.MouseEnabled then
              Finished := dgEvaluateMouseCommand(cwCmd, Dummy, True);
          {$ENDIF}
        end;
      end;

      {if we're exiting, make sure all required fields are filled in}
      if Finished then
        case cwCmd of
          ccItemChange, {!!.11}
          ccNested,
          ccQuit,
          ccClickExit,
          ccUser0..ccUser65335 :
            {don't check} ;
          else if not dgRequiredFieldsOK(CP, ErrCode, ErrMsg) then begin
            {put the cursor on the control that caused the error}
            PositionCursorAt(CP);

            {update screen}
            if not ByteFlagIsSet(asOptions, dgFastUpdates) then
              UpdateContents;

            {display error message}
            GotError(ErrCode, ErrMsg^);
            ClearErrors;

            {move to the control with the error}
            asNext := CP;

            {don't exit}
            Finished := False;
          end;
        end;

    until Finished or (cwCmd = ccError);

ExitPoint:
    cwCmdPtr^.cpAltKeys := Pointer(SaveAltKeys);    {!!.20}

    {save window state}
    rwSaveWindowState;
  end;

begin
  {initialize command processor}
  DialogCommands.Init(@DialogKeySet, DialogKeyMax);
end.
