{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   FDIALOG.PAS 1.30                    *}
{*      Copyright (c) TurboPower Software 1988,1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit FDialog;
  {-File DialogBox routines}

interface

uses
  Dos,
  OpInline,
  OpString,
  OpDos,
  OpConst,                                                 {!!.20}
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
  OpSelect,
  OpCtrl,
  OpPick,
  OpDir,
  OpDialog;

type
  HelpType = (hHidden, hProtected, hVisible);
  FileDialogPtr = ^FileDialog;
  FileDialog =
    object(DialogBox)
      fdFilePtr, fdDirPtr  : DirListPtr;
      fdFName,   fdDirName : PathStr;
      {... methods ...}
      constructor Init(X1, Y1 : Byte; HT : HelpType; Mask : PathStr);
        {-Initialize the file dialog box}
      constructor InitCustom(X1, Y1 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             var dColors : DialogColorSet;
                             HT : HelpType;
                             Mask : PathStr);
        {-Initialize the file dialog box with custom window options}
      destructor Done; virtual;
        {-Dispose of the file dialog box}
      procedure dgPostFocus; virtual;
        {-Called just after a control has given up the focus}
      procedure SetFileName(FN : PathStr);
        {-Set default filename (call after Init)}
      function GetFileName : PathStr;
        {-Return selected filename}
      {+++ internal methods +++}
      procedure fdSetFName(FN : PathStr);
    end;

const
  {ID's for controls in a file dialog box}
  idFileName     = 0;
  idDirName      = 1;
  idFileWin      = 2;
  idDirWin       = 3;
  idOK           = 4;
  idCancel       = 5;
  idHelp         = 6;

  hiFileName     = 1;
  hiDirName      = 2;
  hiFileWin      = 3;
  hiDirWin       = 4;
  hiOK           = 5;
  hiCancel       = 6;
  hiHelp         = 7;

  {==========================================================================}

implementation

  function HasWildCards(fdFName : PathStr) : Boolean;
    {-Return True if fdFName contains wildcards}
  begin
    HasWildCards := (Pos('?', fdFName) <> 0) or (Pos('*', fdFName) <> 0);
  end;

  procedure FileListMoveProc(P : PickListPtr);
    {-Called each time the cursor is moved in the file list}
  begin
    with DirListPtr(P)^ do
      if (pkChoice <> pkInitChoice) and
         not FlagIsSet(pkSecFlags, pkFakingOneItem) then begin
        FileDialogPtr(ParentPtr)^.fdSetFName(StLocase(GetMultiFile(pkChoice)));
        pkInitChoice := pkChoice;
      end;
  end;

  procedure DirListMoveProc(P : PickListPtr);
    {-Called each time the cursor is moved in the directory list}
  var
    FDBP : FileDialogPtr;
    Mask : PathStr;
  begin
    with DirListPtr(P)^, diDPtr^[pkChoice] do begin
      FDBP := FileDialogPtr(ParentPtr);
      Mask := diMask;
      if Mask <> FDBP^.fdFilePtr^.diMask then begin
        FDBP^.fdDirName := JustPathName(Mask);
        with FDBP^.fdFilePtr^ do begin
          SetMask(Mask, AnyFile-Directory);
          PreLoadDirList;
          UpdateContents;
        end;
        FDBP^.fdSetFName(JustFileName(Mask));
        FDBP^.DrawField(idDirName);
      end
      else if pkChoice <> pkInitChoice then begin
        Mask := JustFilename(FDBP^.fdFName);
        if Attr = diDriveAttr then
          Mask := Name+':'+Mask
        else
          Mask := AddBackslash(Name)+Mask;
        FDBP^.fdSetFName(Mask);
        pkInitChoice := pkChoice;
      end;
    end;
  end;

  constructor FileDialog.Init(X1, Y1 : Byte; HT : HelpType; Mask : PathStr);
    {-Initialize the file dialog box}
  begin
    if not FileDialog.InitCustom(
      X1, Y1, DefaultColorSet, DefWindowOptions,
      DefaultDialogColors, HT, Mask) then
        Fail;
  end;

  constructor FileDialog.InitCustom(X1, Y1 : Byte;
                                    var Colors : ColorSet;
                                    Options : LongInt;
                                    var dColors : DialogColorSet;
                                    HT : HelpType;
                                    Mask : PathStr);
    {-Initialize the file dialog box with custom window options}
  const
    ChildWinOptions = wClear+wUserContents+wNoCoversBuffer;
  var
    X, Y : Word;

    procedure SetupDirList(DP : DirListPtr);
      {-Perform customization common to both DirLists}
    begin
      with DP^, dColors do begin
        {reset video attributes}
        SetTextAttr(ClusterColor, ClusterMono);
        SetPickAttr(pkNormal, False, ClusterColor, ClusterMono);

        {$IFDEF UseScrollBars}
        with wFrame do
          AdjustFrameCoords(frXL, frYL, frXH+1, frYH);
        wFrame.AddCustomScrollBar(frRR, 0, MaxLongInt, 0, 0, 'þ', '°', Colors);
        SetPadSize(1, 1);
        {$ELSE}
        SetPadSize(1, 2);
        {$ENDIF}

        pkOptionsOn(pkAltCurrent+pkProcessZero);
        pkOptionsOff(pkDrawActive);
        SetSortOrder(SortNameDirDrive);
        SetSearchMode(PickAnyCharExit);
      end;
    end;

  begin
    fdFilePtr := nil;
    fdDirPtr := nil;

    if Mask = '' then
      Mask := '*.*';

    {instantiate dialog box}
    if not DialogBox.InitCustom(
      X1, Y1, X1+46, Y1+14, Colors, Options, dColors) then
        Fail;

    {instantiate file list}
    X := X1+1;
    Y := Y1+6;
    New(fdFilePtr,
       {$IFDEF UseScrollBars}
        InitCustom(X, Y, X+14, Y+8,
       {$ELSE}
        InitCustom(X, Y, X+15, Y+8,
       {$ENDIF}
                   Colors, ChildWinOptions, 20000, PickVertical, SingleFile) );
    if fdFilePtr = nil then begin
      if InitStatus = 0 then
        InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    with fdFilePtr^, dColors do begin
      SetupDirList(fdFilePtr);
      SetMoveProc(FileListMoveProc);

      Mask := FExpand(Mask);
      fdDirName := JustPathName(Mask);
      fdFName := JustFileName(Mask);

      SetMask(Mask, AnyFile-Directory);
      PreLoadDirList;

      {check for errors}
      if RawError <> 0 then begin
        InitStatus := RawError;
        Self.Done;
        Fail;
      end;
    end;

    {instantiate directory list}
    Inc(X, 18);
    New(fdDirPtr,
       {$IFDEF UseScrollBars}
        InitCustom(X, Y, X+14, Y+8,
       {$ELSE}
        InitCustom(X, Y, X+15, Y+8,
       {$ENDIF}
                   Colors, ChildWinOptions, 3000, PickVertical, SingleFile) );
    if fdDirPtr = nil then begin
      if InitStatus = 0 then
        InitStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    with fdDirPtr^, dColors do begin
      SetupDirList(fdDirPtr);
      SetDirCategory(pkNormal);
      diOptionsOn(diShowDrives);
      SetRejectFunc(RejectFiles);
      SetMoveProc(DirListMoveProc);

      SetMask(Mask, Directory);
      PreLoadDirList;

      {check for errors}
      if RawError <> 0 then begin
        InitStatus := RawError;
        Self.Done;
        Fail;
      end;
    end;

    {set field/control options}
    dgFieldOptionsOn(efClearFirstChar);
    dgSecFieldOptionsOn(sefSwitchCommands);

    {idFileName:}
      AddSimpleEditControl('File &name:', 2, 2, 'X', 2, 13, 34, 79, hiFileName, fdFName);

    {idDirName:}
      dgFieldOptionsOn(efProtected);
      AddSimpleEditControl('Directory:', 4, 2, 'X', 4, 13, 34, 79, hiDirName, fdDirName);
      dgFieldOptionsOff(efProtected);

    {idFileWin:}
      AddWindowControl('&Files', 6, 2, 7, 2, hiFileWin, ccSelect, fdFilePtr^);

    {idDirWin:}
      AddWindowControl('&Directories', 6, 20, 7, 20, hiDirWin, ccSelect, fdDirPtr^);

    {idOK:}
      AddPushButton('O&K',      07, 38, 8, hiOK,     ccSelect, True);

    {idCancel:}
      AddPushButton('Cancel',   10, 38, 8, hiCancel, ccQuit,   False);

    {idHelp:}
      case HT of
        hHidden    : dgFieldOptionsOn(efHidden);
        hProtected : dgFieldOptionsOn(efProtected);
      end;
      AddPushButton('Help',     13, 38, 8, hiHelp,   ccHelp,   False);
      dgFieldOptionsOff(efHidden+efProtected);

    if RawError <> 0 then begin
      InitStatus := RawError;
      Done;
      Fail;
    end;
  end;

  destructor FileDialog.Done;
    {-Dispose of the file dialog box}
  begin
    {dispose of children if necessary}
    if wChildList = nil then begin
      if fdFilePtr <> nil then
        Dispose(fdFilePtr, Done);
      if fdDirPtr <> nil then
        Dispose(fdDirPtr, Done);
    end;

    {dispose of dialog box}
    DialogBox.Done;
  end;

  procedure FileDialog.fdSetFName(FN : PathStr);
  begin
    fdFName := FN;
    DrawField(idFileName);
  end;

  procedure FileDialog.dgPostFocus;
    {-Called just after a control has given up the focus}

    procedure FixFName;
    begin
      if (JustFileName(fdFName) = fdFName) or
         (Pos(':', fdFName) = 0) and (fdFName[1] <> '\') then
        fdFName := AddBackslash(fdDirName)+fdFName
      else if (fdFName[1] = '\') then
        fdFName := Copy(fdDirName, 1, 2)+fdFName;
    end;

    procedure ResetFilename;
    begin
      fdFilePtr^.SetMask(fdFName, AnyFile-Directory);
      fdFilePtr^.PreLoadDirList;
      fdDirName := JustPathName(fdFilePtr^.diMask);
      if fdDirName <> JustPathName(fdDirPtr^.diMask) then begin
        fdDirPtr^.SetMask(fdFName, Directory);
        fdDirPtr^.PreLoadDirList;
      end;
      fdDirPtr^.diMask := fdFilePtr^.diMask;
      fdFName := JustFilename(fdFName);
      ResetScreen;
      cwCmd := ccNone
    end;

    procedure DirectoryCheck;
    begin
      FixFName;
      if IsDirectory(fdFName) then begin
        fdfName := AddBackSlash(fdfName)+JustFileName(fdFilePtr^.diMask);
        ResetFilename;
      end;
    end;

  begin
    if cwCmd <> ccSelect then
      Exit;
    case GetCurrentID of
      idFileName :
        if fdFName = '' then
          cwCmd := ccNone
        else if HasWildCards(fdFName) then begin
          FixFName;
          ResetFilename;
        end
        else
          DirectoryCheck;
      idFileWin :
        with fdFilePtr^ do
          if FlagIsSet(pkSecFlags, pkFakingOneItem) then
            Self.cwCmd := ccNone
          else
            fdFName := GetMultiPath(pkChoice);
      idDirWin :
        cwCmd := ccNone;
      idOK :
        begin
          if HasWildCards(fdFName) or (fdFName = '') then
            cwCmd := ccNone
          else
            DirectoryCheck;
          if cwCmd = ccNone then
            SetNextField(idFileWin);
        end;
      else
        Exit;
    end;
    if GetLastCommand = ccSelect then
      FixFName;
  end;

  procedure FileDialog.SetFileName(FN : PathStr);
    {-Set default filename (call after Init)}
  begin
    fdFName := FN;
  end;

  function FileDialog.GetFileName : PathStr;
    {-Return selected filename}
  begin
    GetFileName := StUpcase(fdFName);
  end;

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
