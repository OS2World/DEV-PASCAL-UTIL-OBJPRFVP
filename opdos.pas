{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                    OPDOS.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{* Portions Copyright (c) Sunny Hill Software 1985, 1986 *}
{*     and used under license to TurboPower Software     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpDos;
  {-Miscellaneous DOS/BIOS call routines}

interface

uses
  Dpmi,                                                            {!!.20}
{$IFDEF Dpmi}                                                      {!!.21}
  WinApi,                                                          {!!.21}
{$ENDIF}                                                           {!!.21}
  Use32,
{$IFDEF VIRTUALPASCAL}
  {$OrgName+,Delphi+}
  strings,
  VpSysLow,
  {$IFDEF OS2}
  Os2Base,
  {$ENDIF}
{$ENDIF}
  Dos,
  OpString;

const
  ExecSaveScreen    = 0;
  ExecShowMemory    = 1;
  ExecPauseAfterRun = 2;
  ExecRestoreScreen = 3;
type
  OS =                 {!!.20}
    record             {!!.20}
      O, S : Word;     {!!.20}
    end;               {!!.20}
  ActionCodeType = ExecSaveScreen..ExecRestoreScreen;
  DiskClass = (
    Floppy360, Floppy720, Floppy12, Floppy144, OtherFloppy, Bernoulli,
    HardDisk, RamDisk, SubstDrive, UnknownDisk, InvalidDrive,
    NovellDrive, CDRomDisk, LANDisk); {!!.13}{!!.20}

type
  EnvRec =
    record
      EnvSeg : Word;              {Segment of the environment}
{$IFDEF Dpmi}                                                          {!!.20}
      EnvLen : LongInt;                                                {!!.20}
{$ELSE}                                                                {!!.20}
      EnvLen : Word;              {Usable length of the environment}
{$ENDIF}                                                               {!!.20}
      EnvPtr : Pointer;           {Nil except when allocated on heap}
{$IFDEF Dpmi}                                                          {!!.20}
      EnvRealMode : Boolean;      {True for a real mode environment}
{$ENDIF}                                                               {!!.20}
    end;

  ExecDosProc = procedure(ActionCode : ActionCodeType; Param : Word);
  VolumeNameStr = String[11];     {string type for volume labels}    {!!.14}
  MediaIDType =                   {record type for Get/SetMediaID}   {!!.14}
    record
      InfoLevel : Word;
      SerialNumber : LongInt;
      VolumeLabel : Array[0..10] of Char;
      FileSystemID : Array[0..7] of Char;
    end;

const
  StackSafetyMargin : Word = 1000;
  MinSpaceForDos : Word = 20000; {Minimum bytes for DOS shell to run}

  StdInHandle = 0;           {handle numbers for OpenStdDev}
  StdOutHandle = 1;
  StdErrHandle = 2;
  StdPrnHandle = 4;

function DosVersion : Word;
  {-Returns the DOS version number. High byte has major version number,
    low byte has minor version number. Eg., DOS 3.1 => $0301.}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $B4/$30/                 {mov ah,$30}
    $CD/$21/                 {int $21}
    $86/$C4);                {xchg ah,al}
  {$ENDIF}

function NumberOfDrives : Byte;
  {-Returns the number of logical drives}

procedure SelectDrive(Drive : Char);
  {-Selects the specified drive as default if possible}

function DefaultDrive : Char;
  {-Returns the default drive as an uppercase letter}

function ValidDrive(Drive : Char) : Boolean;
  {-Return True if the specified drive is valid}

function IsPhantom(Drive : Char) : Boolean;                    {!!.30}
  {-Return True if the specified drive is a phantom drive}     {!!.30}

function PhantomExists : Boolean;                              {!!.30}
  {-Return True if there is a phantom drive in the system}     {!!.30}

procedure SelectPhantom(Drive : Char);                         {!!.30}
  {-Select a phantom drive as default if possible}             {!!.30}

function GetDiskInfo(Drive : Byte; var ClustersAvailable, TotalClusters,
                     BytesPerSector, SectorsPerCluster : Word) : Boolean;
  {-Return technical info about the specified drive}

function GetDiskClass(Drive : Char; var SubstDriveChar : Char) : DiskClass;
  {-Return the disk class for the drive with the specified letter}

{$IFNDEF Dpmi}  {DPMI cannot properly handle the DOS INT 25h/26h calls}
function ReadDiskSectors(Drive : Word; FirstSect : Longint;
                         NumSects : Word; var Buf) : Boolean;
  {-Read absolute disk sectors.}

function WriteDiskSectors(Drive : Word; FirstSect : Longint;
                          NumSects : Word; var Buf) : Boolean;
  {-Write absolute disk sectors.}
{$ENDIF}

function GetFileMode(FName : string; var Attr : Word) : Byte;
  {-Returns a file's attribute in Attr and the DOS error code as the function
    result.}

function FlushDosBuffers(var F) : Boolean;
  {-Flush DOS's buffers for the specified file}

function FileHandlesLeft : Byte;
  {-Return the number of available file handles}

function FileHandlesOpen(CountDevices : Boolean) : Byte;
  {-Return the number of open files owned by a program}

{$IFNDEF VIRTUALPASCAL}
procedure SetDta(DTAptr : Pointer);
  {-Set the DOS DTA to point to DTAptr}

procedure GetDta(var DTAptr : Pointer);
  {-Return the DOS DTA pointer}
{$ENDIF}

function ParsePath(var InputPath, SearchPath, LeadInPath : string) : Boolean;
  {-Takes a user entered path, trims blanks, and returns a valid global
    search path and a valid lead-in path.}

function PrintInstalled : Boolean;
  {-Returns True if PRINT.COM is installed}

function SubmitPrintFile(FileName : string) : Byte;
  {-This procedure submits a file to the PC DOS 3.0 or greater concurrent
   print utility.}

procedure CancelPrintFile(FileMask : string);
  {-Cancels the files matched by the file mask passed in FileMask.}

procedure CancelAllPrintFiles;
  {-Cancels all files in the print queue}

function GetPrintStatus(var QPtr : Pointer) : Byte;
 {-Halts printing, returns current error status, puts pointer to the filename
   queue in the QPtr variable. Filenames in the queue are 64-byte ASCIIZ
   strings. The end of the queue is marked by a name starting with a null.}

procedure EndPrintStatus;
  {-Releases the spooler from the GetPrintStatus procedure.}

function GetEnvironmentString(SearchString : string) : string;
  {-Return a string from the environment}

function SetBlock(var Paragraphs : Word) : Boolean;
  {-Change size of DOS memory block allocated to this program}

procedure NoExecDosProc(ActionCode : ActionCodeType; Param : Word);
  {-Do-nothing ExecDosProc}

function ExecDos(Command : string; UseSecond : Boolean; EDP : ExecDosProc) : Integer;
 {-Execute any DOS command. Call with Command = '' for a new shell.
   If UseSecond is false, Command must be the full pathname of a program to be
   executed. EDP is a procedure to display status, save/restore the screen,
   etc. ExecDos return codes are as follows:
         0 : Success
        -1 : Insufficient memory to store free list
        -2 : DOS setblock error before EXEC call
        -3 : DOS setblock error after EXEC call  -- critical error!
        -4 : Insufficient memory to run DOS command
      else   a DOS error code
  }

function TextSeek(var F : Text; Target : LongInt) : Boolean;
 {-Do a Seek for a text file opened for input. Returns False in case of I/O
   error.}

function TextFileSize(var F : Text) : LongInt;
  {-Return the size of text file F. Returns -1 in case of I/O error.}

function TextPos(var F : Text) : LongInt;
 {-Return the current position of the logical file pointer (that is,
   the position of the physical file pointer, adjusted to account for
   buffering). Returns -1 in case of I/O error.}

function TextFlush(var F : Text) : Boolean;
  {-Flush the buffer(s) for a text file. Returns False in case of I/O error.}

function OpenStdDev(var F : Text; StdHandle : Word) : Boolean;
  {-Assign the text file to a standard DOS device: 0, 1, 2, or 4}

function HandleIsConsole(Handle : Word) : Boolean;
  {-Return true if handle is the console device}

procedure SetRawMode(var F : Text; IsOn : Boolean);
  {-Set "raw" mode on or off for the specified text file (must be a device)}

function ExistFile(FName : string) : Boolean;
  {-Return true if file is found}

function ExistOnPath(FName : string; var FullName : string) : Boolean;
 {-Return true if fname is found in
   a) current directory
   b) program's directory (DOS 3.X only)
   c) any DOS path directory
  and return full path name to file}

function IsDirectory(FName : String) : Boolean;
  {-Return true if FName is a directory}

function SameFile(FilePath1,FilePath2 : String;
                  var ErrorCode : Word) : Boolean;
  {-Return true if FilePath1 and FilePath2 refer to the same physical file.
    Error codes:
      0 - Success (no error)
      1 - Invalid FilePath
  }

function CopyFile(SrcPath, DestPath : String;
                  Buffer : Pointer;
                  BufferSize : Word) : Word;
  {-Copy the file specified by SrcPath into DestPath. DestPath must specify
    a complete filename, it may not be the name of a directory without the
    file portion.  This a low level routine, and the input pathnames are not
    checked for validity. Buffer must already be allocated, and must be no
    less than BufferSize.}

function TimeMs : LongInt;
  {-Return time of day in milliseconds since midnight}

{-------- routines from TPENV --------}

procedure MasterEnv(var Env : EnvRec);
  {-Return master environment record}

procedure CurrentEnv(var Env : EnvRec);
  {-Return current environment record}

procedure ParentEnv(var Env : EnvRec);
  {-Return environment record of program's parent}

procedure CommandEnv(var Env : EnvRec); {!!.12}
  {-Return environment record for last copy of COMMAND.COM in memory}

procedure NewEnv(var Env : EnvRec; Size : Word);
  {-Allocate a new environment on the heap}

procedure DisposeEnv(var Env : EnvRec);
  {-Deallocate an environment previously allocated on heap}

procedure SetCurrentEnv(Env : EnvRec);
  {-Specify a different environment for the current program}

procedure CopyEnv(Src, Dest : EnvRec);
  {-Copy contents of Src environment to Dest environment}

function EnvFree(Env : EnvRec) : Word;
  {-Return bytes free in environment}

function GetEnvStr(Env : EnvRec; Search : string) : string;
  {-Return a string from the environment}

function SetEnvStr(Env : EnvRec; Search, Value : string) : Boolean;
  {-Set environment string, returning true if successful}

procedure DumpEnv(Env : EnvRec);
  {-Dump the environment to the screen}

function GetProgramStr(Env : EnvRec) : string;
  {-Return the name of the program that owns Env, '' if DOS < 3.0 or unknown}

function SetProgramStr(Env : EnvRec; Path : string) : Boolean;
  {-Add a program name to the end of an environment if sufficient space}

function ShellWithPrompt(Prompt : string; EDP : ExecDosProc) : Integer;
  {-Shell to DOS with a new prompt}

function GetVolumeLabel(Drive : Char;
                        var VolName : VolumeNameStr) : Word; {!!.14}
  {-Gets the volume label for Drive. Returns 0 for success, or DOS error
    code.}

function DeleteVolumeLabel(Drive : Char) : Word;            {!!.20}
  {-Deletes an existing volume label on Drive. Returns 0 for success,
    or DOS error code.}

function SetVolumeLabel(Drive : Char; VolName : VolumeNameStr) : Word; {!!.14}
  {-Set the volume label for Drive to VolName. All 11 characters in VolName
    are significant, so the string should always be padded to 11 characters.
    Returns 0 for success, or DOS error code.}

function GetMediaID(Drive : Char;
                    var MediaIDRec : MediaIDType) : Word; {!!.14}
  {-Returns the DOS 5 Media ID record which includes the disk's serial number
    and volume label. Returns 0 for success, or DOS error code.}

function SetMediaID(Drive : Char;
                    var MediaIDRec : MediaIDType) : Word; {!!.14}
  {-Sets the DOS 5 Media ID record which includes the disk's serial number
    and volume label. Returns 0 for success, or DOS error code.}

  {============================================================================}

implementation

type
  SegOfs = record
             O, S : Word;
           end;
  LongRec = record
              LowWord, HighWord : SmallWord; {structure of a LongInt}
            end;

  EnvArray = array[0..32767] of Char;
  EnvArrayPtr = ^EnvArray;

  {text buffer}
  TextBuffer = array[0..65520] of Byte;
{$IFNDEF VIRTUALPASCAL}
var
  Regs : Registers;
const                         {!!.02}
  DSReadDrive  : Byte = $FF;  {!!.02}
  DSWriteDrive : Byte = $FF;  {!!.02}
  DSReadBig    : Byte = $00;  {!!.02}
  DSWriteBig   : Byte = $00;  {!!.02}
{$ENDIF}

{$IFDEF Dpmi}                                    {!!.21}
  {temp selector used in PRINT.COM calls}        {!!.21}
  TempSele : Word = $0000;                       {!!.21}
{$ENDIF}                                         {!!.21}

  {$IFNDEF VIRTUALPASCAL}
  {$L OPDISK.OBJ}
  function NumberOfDrives : Byte; external;
  procedure SelectDrive(Drive : Char); external;
  function DefaultDrive : Char; external;
  function IsPhantom(Drive : Char) : Boolean; external;
  function GetDiskInfo(Drive : Byte; var ClustersAvailable, TotalClusters,
                       BytesPerSector, SectorsPerCluster : Word) : Boolean;
    external;
  {$ENDIF}


{$IFNDEF Dpmi}
  function ReadDiskSectors(Drive : Word; FirstSect : Longint;
                           NumSects : Word; var Buf) : Boolean;
    external;
  function WriteDiskSectors(Drive : Word; FirstSect : Longint;
                            NumSects : Word; var Buf) : Boolean;
    external;
{$ENDIF}

{$IFDEF Dpmi}               {!!.20}
  {$I OPDOS.IN2}            {!!.20}
{$ELSE}                     {!!.20}
  {$I OPDOS.IN1}
{$ENDIF}                    {!!.20}

  function ValidDrive(Drive : Char) : Boolean;
    {-Return True if the specified drive is valid}
  var
    CurDrive : Char;
  begin
    if IsPhantom(Drive) then                   {!!.30}
      ValidDrive := False                      {!!.30}
    else begin                                 {!!.30}
      Drive := UpCase(Drive);  {!!.11}
      CurDrive := DefaultDrive;
      SelectDrive(Drive);
      ValidDrive := (DefaultDrive = Drive);
      SelectDrive(CurDrive);
    end;                                       {!!.30}
  end;

{$IFDEF VIRTUALPASCAL}

  function GetDiskClass(Drive : Char; var SubstDriveChar : Char) : DiskClass;
    {-Return the disk class for the drive with the specified letter}
{  DiskClass = (
    Floppy360, Floppy720, Floppy12, Floppy144, OtherFloppy, Bernoulli,
    HardDisk, RamDisk, SubstDrive, UnknownDisk, InvalidDrive,
    NovellDrive, CDRomDisk);}
  var
    Drv: Byte;
    DiskSize: Longint;
  begin
    SubstDriveChar := Drive;
    Drv := ord(Drive)-ord('A')+1;
    case SysGetDriveType(Drive) of
      dtFloppy:
        begin
          DiskSize := SysDiskSize(Drv) div 1024;
          if ( DiskSize > 340000 ) and ( DiskSize < 370000 ) then
            GetDiskClass := Floppy360
          else if ( DiskSize > 700 ) and ( DiskSize < 730 ) then
            GetDiskClass := Floppy720
          else if ( DiskSize > 1180 ) and ( DiskSize < 1210 ) then
            GetDiskClass := Floppy12
          else if ( DiskSize > 1410 ) and ( DiskSize < 1450 ) then
            GetDiskClass := Floppy144
          else
            getDiskClass := OtherFloppy;
        end;
      dtHDFAT,
      dtHDHPFS,
      dtHDExt2,
      dtHDNTFS:    GetDiskClass := HardDisk;
      dtNovellNet: GetDiskClass := NovellDrive;
      dtCDROM:     GetDiskClass := CDRomDisk;
      dtLAN:       GetDiskClass := LANDisk;
      dtTVFS:      GetDiskClass := SubstDrive; // Best approximation
      dtUnknown:   GetDiskClass := UnknownDisk;
      dtInvalid:   GetDiskClass := InvalidDrive;
    end;
  end;

{$ELSE}

{$IFDEF Dpmi}  {!!.20 - Additions for protected mode}

  function GetDiskClass(Drive : Char; var SubstDriveChar : Char) : DiskClass;
    {-Return the disk class for the drive with the specified letter}
    {-This routine uses two undocumented DOS function ($32 and $52).
      Information about these functions was obtained from Terry Dettmann's DOS
      Programmer's Reference (Que, 1988) and Undocumented DOS (Addison-Wesley,
      1990).}
  type
    NovellTable = array[1..32] of Byte;
    DriveParameterBlock =
      record
        DriveNum, DeviceDriverUnit : Byte;
        BytesPerSector : Word;
        SectorsPerCluster, ShiftFactor : Byte;
        BootSectors : Word;
        FatCopies : Byte;
        RootDirEntries, FirstDataSector, HighestCluster : Word;
        case Byte of
          0 : (SectorsPerFat23 : Byte;
               FirstDirSector23 : Word;
               DeviceDriver23 : Pointer;
               MediaDescriptor23 : Byte;
               AccessFlag23 : Byte;
               Next23 : Pointer;
               Reserved23 : LongInt);
          1 : (SectorsPerFat45 : Word;
               FirstDirSector45 : Word;
               DeviceDriver45 : Pointer;
               MediaDescriptor45 : Byte;
               AccessFlag45 : Byte;
               Next45 : Pointer;
               Reserved45 : LongInt);
      end;
  var
    Ver, DN : Byte;
    MediaDescriptor : Byte;
    SectorsPerFat : Word;
    Found : WordBool;
    pDPBP, DPBP : ^DriveParameterBlock;
    NTP : ^NovellTable;
    Sele : Word;
    Continue : WordBool;
    Regs : DPMIRegisters;
    CDRI : Boolean;
    CDRN : Word;
  Label
    WalkSFT, Evaluate, ExitPoint;

    function CDRomInstalled : Boolean;
    var
      R : DpmiRegisters;
    begin
      FillChar(R, SizeOf(R), 0);
      with R do begin
        AX := $1500;
        if SimulateRealModeInt($2F, R) <> 0 then ;
        CDRomInstalled := (DoubleWord(AX).LoWord <> $FFFF) and         {!!.21}
                          (DoubleWord(BX).LoWord <> 0);                {!!.21}
      end;
    end;

    {!!.21 - Rewritten}
    function IsDriveCDROM(Drive : Char) : Boolean;
    var
      R : DpmiRegisters;
      D : Word;
    begin
      IsDriveCDROM := False;
      FillChar(R, SizeOf(R), 0);
      with R do begin
        AX := $1500;
        if SimulateRealModeInt($2F, R) <> 0 then ;
        {if AX=$FFFF, GRAPHICS.COM interfered with things}
        if DoubleWord(AX).LoWord = $FFFF then
          Exit;
        if BX <> 0 then begin
          D := Ord(Upcase(Drive))-Ord('A');
          IsDriveCDROM := (D >= CX) and (D <= (CX+BX-1));
        end;
      end;
    end;

  begin
    {assume failure}
    GetDiskClass := InvalidDrive;
    {assume that this is not a SUBSTituted drive}
    SubstDriveChar := Drive;

    {convert drive letter to drive number}
    Drive := Upcase(Drive);
    if IsPhantom(Drive) then
      Exit;

    case Drive of
      'A'..'Z' : DN := Ord(Drive)-Ord('A');
      else Exit;
    end;

    {check for CDRom installed}
    CDRI := CDRomInstalled;

    {we need a selector for the following}
    if GetSelectorForRealMem(nil, $FFFF, Sele) <> 0 then
      Exit;

    {get DOS version number}
    Ver := Hi(DosVersion);

    {get pointer to drive parameter block}
    asm
      mov     ah,32h
      mov     dl,DN
      inc     dl
      push    ds
      int     21h
      mov     dx,ds
      pop     ds
      cmp     al,0FFh
      je      WalkSFT
      mov     word ptr pDPBP,bx
      mov     word ptr pDPBP+2,dx
      jmp     Evaluate
    end;
WalkSFT:
    {DPMI16BI doesn't support this call, so virtualize it}
    FillChar(Regs, SizeOf(Regs), 0);
    Regs.AX := $5200;
    if SimulateRealModeInt($21, Regs) <> 0 then
      goto ExitPoint;
    if SetSegmentBaseAddr(Sele, Linear(Ptr(Regs.ES, Regs.BX))) <> 0 then
      Goto ExitPoint;
    DPBP := Ptr(MemW[Sele:2], MemW[Sele:0]);
    if SetSegmentBaseAddr(Sele, Linear(DPBP)) <> 0 then
      Goto ExitPoint;
    Continue := Word(DPBP) <> $FFFF;
    pDPBP := Ptr(Sele, 0);

    Found := False;
    while (Word(DPBP) <> $FFFF) and not Found do begin
      if pDPBP^.DriveNum = DN then
        Found := True
      else if (Ver < 4) or (Ver >= 10) then                           {!!.21}
        Move(pDPBP^.Next23, DPBP, SizeOf(DPBP))
      else
        Move(pDPBP^.Next45, DPBP, SizeOf(DPBP));

      Continue := Word(DPBP) <> $FFFF;
      if (not Found) and Continue then begin
        if SetSegmentBaseAddr(Sele, Linear(DPBP)) <> 0 then
          Goto ExitPoint;
      end;
    end;
    if not Found then begin
      if (CDRI) and (IsDriveCDRom(Drive)) then begin    {moved up}   {!!.22}
        GetDiskClass := CDRomDisk;                                   {!!.22}
        goto ExitPoint;                                              {!!.22}
      end;                                                           {!!.22}
      {386MAX 6.01 doesn't support this call, so virtualize it}
      FillChar(Regs, SizeOf(Regs), 0);
      Regs.SI := 0;
      Regs.ES := 0;
      Regs.AX := $EF01;
      if SimulateRealModeInt($21, Regs) <> 0 then
        goto ExitPoint;
      if (Regs.ES = 0) and (Regs.SI = 0) then
        goto ExitPoint;
      if SetSegmentBaseAddr(Sele, Linear(Ptr(Regs.ES, Regs.SI))) <> 0 then
        goto ExitPoint;
      NTP := Ptr(Sele, 0);
      if NTP^[DN+1] and $03 <> 0 then
        GetDiskClass := NovellDrive;                                 {!!.22}
      Goto ExitPoint;
    end;
  Evaluate:
      with pDPBP^ do begin
        if (Ver < 4) or (Ver >= 10) then begin                      {!!.21}
          MediaDescriptor := MediaDescriptor23;
          SectorsPerFat := SectorsPerFat23;
        end
        else begin
          MediaDescriptor := MediaDescriptor45;
          SectorsPerFat := SectorsPerFat45;
        end;

        {check for SUBSTituted drive}
        if (DriveNum <> DN) then begin
          GetDiskClass := SubstDrive;
          SubstDriveChar := Char(Ord('A')+DriveNum);
        end
        else if (FatCopies = 1) then
          {RAM disks have one copy of File Allocation Table}
          GetDiskClass := RamDisk
        else if (MediaDescriptor = $F8) then
          {MediaDescriptor of $F8 indicates hard disk}
          GetDiskClass := HardDisk
        else if (MediaDescriptor >= $FC) and (SectorsPerFat <> 2) then
          {Bernoulli drives have more than 2 sectors per FAT}
          GetDiskClass := Bernoulli
        else if (MediaDescriptor >= $F9) then
          {media descriptors >= $F9 are for floppy disks}
          case HighestCluster of
             355 : GetDiskClass := Floppy360;
             714,
            1423 : GetDiskClass := Floppy720;
            2372 : GetDiskClass := Floppy12;
            else   GetDiskClass := OtherFloppy;
          end
        else if (MediaDescriptor = $F0) and (HighestCluster = 2848) then
          {it's a 1.44 meg floppy}
          GetDiskClass := Floppy144
        else if (CDRI) and (IsDriveCDRom(Drive)) then
          {it's a CDRom}
          GetDiskClass := CDRomDisk
        else
          {unable to classify disk/drive}
          GetDiskClass := UnknownDisk;
      end;
    ExitPoint:
      if FreeLDTDescriptor(Sele) = 0 then ;
  end;

{$ELSE}

  function GetDiskClass(Drive : Char; var SubstDriveChar : Char) : DiskClass;
    {-Return the disk class for the drive with the specified letter}
    {-This routine uses two undocumented DOS function ($32 and $52).
      Information about these functions was obtained from Terry Dettmann's DOS
      Programmer's Reference (Que, 1988) and Undocumented DOS (Addison-Wesley,
      1990).}
  type                                {!!.13} {rewritten}
    NovellTable = array[1..32] of Byte;
    DriveParameterBlock =
      record
        DriveNum, DeviceDriverUnit : Byte;
        BytesPerSector : Word;
        SectorsPerCluster, ShiftFactor : Byte;
        BootSectors : Word;
        FatCopies : Byte;
        RootDirEntries, FirstDataSector, HighestCluster : Word;
        case Byte of
          0 : (SectorsPerFat23 : Byte;
               FirstDirSector23 : Word;
               DeviceDriver23 : Pointer;
               MediaDescriptor23 : Byte;
               AccessFlag23 : Byte;
               Next23 : Pointer;
               Reserved23 : LongInt);
          1 : (SectorsPerFat45 : Word;
               FirstDirSector45 : Word;
               DeviceDriver45 : Pointer;
               MediaDescriptor45 : Byte;
               AccessFlag45 : Byte;
               Next45 : Pointer;
               Reserved45 : LongInt);
      end;
  var
    Ver, DN : Byte;
    MediaDescriptor : Byte;
    SectorsPerFat : Word;
    DPBP : ^DriveParameterBlock;
    NTP : ^NovellTable;
    CDRI : Boolean;                       {!!.20 - begin}
    CDRN : Word;
    Regs : Registers;

    function CDRomInstalled : Boolean;
    var
      R : Registers;
    begin
      FillChar(R, SizeOf(R), 0);
      with R do begin
        AX := $1500;
        Intr($2F, R);
        CDRomInstalled := (AX <> $FFFF) and (BX <> 0);               {!!.21}
      end;
    end;

    {!!.21 - Rewritten}
    function IsDriveCDROM(Drive : Char) : Boolean;
    var
      R : Registers;
      D : Word;
    begin
      IsDriveCDROM := False;
      FillChar(R, SizeOf(R), 0);
      with R do begin
        AX := $1500;
        Intr($2F, R);
        {if AX=$FFFF, GRAPHICS.COM interfered with things}
        if AX = $FFFF then
          Exit;
        if BX <> 0 then begin
          D := Ord(Upcase(Drive))-Ord('A');
          IsDriveCDRom := (D >= CX) and (D <= CX+BX-1);
        end;
      end;
    end;                                  {!!.20 - end}

  begin
    {assume failure}
    GetDiskClass := InvalidDrive;

    {assume that this is not a SUBSTituted drive}
    SubstDriveChar := Drive;

    {convert drive letter to drive number}
    Drive := Upcase(Drive);
    if IsPhantom(Drive) then                        {!!.30}
      Exit;                                         {!!.30}
    case Drive of
      'A'..'Z' : DN := Ord(Drive)-Ord('A');
      else Exit;
    end;

    {check for CDRom driver installed}              {!!.20}
    CDRI := CDRomInstalled;                         {!!.20}

    {get DOS version number}
    Ver := Hi(DosVersion);

    with Regs do begin
      {get pointer to drive parameter block}
      AH := $32;
      DL := DN+1;
      MsDos(Regs);

      if (AL <> $FF) then
        DPBP := Ptr(DS,BX)
      else if (CDRI) and (IsDriveCDRom(Drive)) then begin    {!!.21 - begin}
        GetDiskClass := CDRomDisk;
        Exit;
      end
      else begin
        {see if it's a Novell drive}
        AX := $EF01;
        ES := 0;
        SI := 0;
        MsDos(Regs);
        NTP := Ptr(ES,SI);
        if NTP <> nil then
          if NTP^[DN+1] and $03 <> 0 then
            GetDiskClass := NovellDrive;
        Exit;                                                {!!.21 - end}
      end;

      with DPBP^ do begin
        if (Ver < 4)or (Ver >= 10) then begin                {!!.21}
          MediaDescriptor := MediaDescriptor23;
          SectorsPerFat := SectorsPerFat23;
        end
        else begin
          MediaDescriptor := MediaDescriptor45;
          SectorsPerFat := SectorsPerFat45;
        end;

        {check for SUBSTituted drive}
        if (DriveNum <> DN) then begin
          GetDiskClass := SubstDrive;
          SubstDriveChar := Char(Ord('A')+DriveNum);
        end
        else if (FatCopies = 1) then
          {RAM disks have one copy of File Allocation Table}
          GetDiskClass := RamDisk
        else if (MediaDescriptor = $F8) then
          {MediaDescriptor of $F8 indicates hard disk}
          GetDiskClass := HardDisk
        else if (MediaDescriptor >= $FC) and (SectorsPerFat <> 2) then
          {Bernoulli drives have more than 2 sectors per FAT}
          GetDiskClass := Bernoulli
        else if (MediaDescriptor >= $F9) then
          {media descriptors >= $F9 are for floppy disks}
          case HighestCluster of
             355 : GetDiskClass := Floppy360;
             714,
            1423 : GetDiskClass := Floppy720;
            2372 : GetDiskClass := Floppy12;
            else   GetDiskClass := OtherFloppy;
          end
        else if (MediaDescriptor = $F0) and (HighestCluster = 2848) then
          {it's a 1.44 meg floppy}
          GetDiskClass := Floppy144
        else if (CDRI) and (IsDriveCDRom(Drive)) then              {!!.20}
          {it's a CDRom}                                           {!!.20}
          GetDiskClass := CDRomDisk                                {!!.20}
        else
          {unable to classify disk/drive}
          GetDiskClass := UnknownDisk;
      end;
    end;
  end;

{$ENDIF (DOS)}
{$ENDIF (Not Virtual Pascal/2)}

  function GetFileMode(FName : string; var Attr : Word) : Byte;
    {-Returns a file's attribute in Attr and the DOS error code as the function
      result.}
  var
    F : file;
  begin
    Assign(F, FName);
    {call routine in Turbo's DOS unit to get the attribute}
    GetFAttr(F, Attr);
    GetFileMode := DosError;
  end;

  function FlushDosBuffers(var F) : Boolean;
    {-Flush DOS's buffers for the specified file}
  {$IFDEF VIRTUALPASCAL}
  Var
    Rc : Longint;

  begin
    rc := SysFileFlushBuffers( FileRec( F ).Handle );
    FlushDosBuffers := ( rc = 0 );
  end;
  {$ELSE}
  var
    Handle : Word absolute F;
  begin
    FlushDosBuffers := False;
    with Regs do begin
      {dupe the file handle}
      AH := $45;
      BX := Handle;
      MsDos(Regs);
      if Odd(Flags) then
        Exit;

      {close the duped file}
      BX := AX;
      AH := $3E;
      MsDos(Regs);
      if Odd(Flags) then
        Exit;
    end;
    FlushDosBuffers := True;
  end;
  {$ENDIF}

  {$IFNDEF VIRTUALPASCAL}
  procedure SetDta(DTAptr : Pointer);
    {-Set the DOS DTA to point to DTA}
  type
    OS =
      record
        O, S : Word;
      end;
  {$IFDEF DPMI} {!!.21 new section for DPMI}
  var
    Regs : DpmiRegisters;
  begin
    FillChar(Regs, SizeOf(DpmiRegisters), 0);
    with Regs do begin
      AX := $1A00;
      DS := OS(DTAptr).S;
      DX := OS(DTAptr).O;
      if SimulateRealModeInt($21, Regs) <> 0 then ;
    end;
  end;
  {$ELSE}
  begin
    with Regs do begin
      AH := $1A;
      DS := OS(DTAptr).S;    {!!.20}
      DX := OS(DTAptr).O;    {!!.20}
      MsDos(Regs);
    end;
  end;
  {$ENDIF}

  procedure GetDta(var DTAptr : Pointer);
    {-Return the DOS DTA pointer}
  begin
    with Regs do begin
      AH := $2F;
      MsDos(Regs);
      DTAptr := Ptr(ES, BX);
    end;
  end;
  {$ENDIF}

  function ParsePath(var InputPath, SearchPath, LeadInPath : string) : Boolean;
    {-Takes a user entered path, trims blanks, and returns a valid global
      search path and a valid lead-in path.}
  var
    S : string;
    SLen : Byte absolute S;
    Attr : Word;

    function IsPath(S : string) : Boolean;
      {-Return True if S is empty or ends with ':' or '\'}
    var
      SLen : Byte absolute S;
    begin
      {check last character in S}
      case S[SLen] of
        ':' : IsPath := (SLen = 2);
        '\' : IsPath := True;
        '.' : IsPath := (SLen = 1) or (S = '..') or
                        ((SLen > 2) and                      {!!.03}
                         ((Pos('\..', S) = (SLen - 2)) or    {!!.03}
                          (Pos(':.', S)  = (SLen - 1)) or    {!!.10}
                          (Pos(':..', S) = (SLen - 2)) )     {!!.03}
                        );                                   {!!.03}
        else IsPath := (SLen = 0); {True if string is empty}
      end;
    end;

    function NameIsValid(S : string) : Boolean; {!!.10}
    var
      I : Word;
    begin
      NameIsValid := False;
      for I := 1 to Length(S) do
        if S[I] in [#0..#31, '"', '[', ']', '|', ';'..'>', '+', ','] then
          Exit;
      NameIsValid := True;
    end;

  begin
    {Assume success}
    ParsePath := True;

    {Get working copy of InputPath; convert to uppercase and trim blanks}
    S := StUpCase(Trim(InputPath));

    if (Pos(':', S) > 2) or (Pos('/', S) <> 0) or
       (Pos('::', S) <> 0) or (Pos('\\', S) <> 0) then
      ParsePath := False
    {if S is just a path name, add "*.*" to search path}
    else if IsPath(S) then begin
      if S[SLen] = '.' then
        S := S+'\';
      LeadInPath := S;
      SearchPath := S+'*.*';
    end
    else
      if SLen >= 77 then
        ParsePath := False
      else
        {test validity of pathname by calling routine to get file attribute}
        case GetFileMode(S, Attr) of

          0 : if (Attr and Directory {= $10} ) <> 0 then begin
                {Input path is valid directory name}
                SearchPath := S+'\*.*';
                LeadInPath := S+'\';
              end
              else begin
                {Input path is the name of a file}
                SearchPath := S;

                {trim end of string until only a path is left}
                while not IsPath(S) do
                  Dec(SLen);
                LeadInPath := S
              end;

          2, 3 {$IFDEF VIRTUALPASCAL}
               , 253 { error_Invalid_Path }
               {$ENDIF}
               : begin {!!.20}
                {path not found}

                {reject clearly invalid names}
                ParsePath := NameIsValid(S);

                SearchPath := S;

                {trim end of string until only a path is left}
                while not IsPath(S) do
                  Dec(SLen);

                if (S[SLen] <> ':') or (SLen = 2) then
                  LeadInPath := S
                else
                  ParsePath := False;
              end;
        else
          ParsePath := False;
        end;
  end;

{$IFDEF Dpmi}   {!!.21 begin - rewritten for pmode}

  function PrintInstalled : Boolean;
    {-Returns True if PRINT.COM is installed}
    var
      DR : DpmiRegisters;
  begin
    {INT $2F functions available only in DOS 3}
    if DosVersion >= $300 then
      with DR do begin
        FillChar(DR, SizeOf(DR), 0);
        AX := $0100;
        if SimulateRealModeInt($2F, DR) <> 0 then
          PrintInstalled := False
        else
          PrintInstalled := ((AX and $00FF) = $FF);
      end
    else
      PrintInstalled := False;
  end;

  function SubmitPrintFile(FileName : string) : Byte;
    {-This procedure submits a file to the PC DOS 3.0 or greater concurrent
      print utility.}
  type
    PSubmitPacket = ^SubmitPacket;
    SubmitPacket = record
                     Level : Byte;
                     FilenamePtr : PChar;
                   end;
  var
    S : string;
    SLen : Byte absolute S;
    L1, L2 : LongInt;
    DR : DpmiRegisters;
  begin
    S := Trim(FileName);
    S := FullPathName(S);
    if SLen = 0 then
      SubmitPrintFile := 2  {file not found}
    else begin
      if SLen > 64 then
        SLen := 64;
      S[Succ(SLen)] := #0;

      {allocate DOS memory for pathname}
      L1 := GlobalDosAlloc(65);
      if L1 = 0 then begin
        SubmitPrintFile := 8; {out of memory}
        exit;
      end;
      Move(S[1], Ptr(LongRec(L1).LowWord, 0)^, Succ(SLen));

      {allocate DOS memory for submitpacket}
      L2 := GlobalDosAlloc(SizeOf(SubmitPacket));
      if L2 = 0 then begin
        if GlobalDosFree(LongRec(L1).LowWord) = 0 then ;
        SubmitPrintFile := 8; {out of memory}
        exit;
      end;

      with PSubmitPacket(Ptr(LongRec(L2).LowWord, 0))^ do begin
        FileNamePtr := Ptr(LongRec(L1).HighWord, 0);
        Level := 0;
      end;

      with DR do begin
        FillChar(DR, SizeOf(DR), 0);
        DS := LongRec(L2).HighWord;
        AX := $0101;                                {submit file to be printed}
        if SimulateRealModeInt($2F, DR) <> 0 then   {print spool control int}
          SubmitPrintFile := 1                      {invalid function if DPMI error}
        else if Odd(Flags) then                     {check carry flag}
          SubmitPrintFile := Byte(AX and $00FF)     {carry set, return code in AL}
        else
          SubmitPrintFile := 0;
      end;
      if GlobalDosFree(LongRec(L2).LowWord) <> 0 then ;
      if GlobalDosFree(LongRec(L1).LowWord) <> 0 then ;
    end;
  end;

  procedure CancelPrintFile(FileMask : string);
    {-Cancels the files matched by the file mask passed in FileMask.}
  var
    Len : Byte absolute FileMask;
    L : LongInt;
    DR : DpmiRegisters;
  begin
    if Len > 64 then
      Len := 64;                {truncate filenames longer than 64 characters}
    L := GlobalDosAlloc(65);    {allocate a real-mode block}
    if L = 0 then
      Exit;
    with DR do begin
      FileMask[Succ(Len)] := #0;            {make FileMask an ASCIIZ string}
      Move(FileMask[1], Ptr(LongRec(L).LowWord, 0)^, Succ(Len));
      FillChar(DR, SizeOf(DR), 0);
      DS := LongRec(L).HighWord;            {DS:DX points to the ASCIIZ string}
      AX := $0102;
      if SimulateRealModeInt($2F, DR) <> 0 then ;{print spool control interrupt}
      if GlobalDosFree(LongRec(L).LowWord) <> 0 then ;
    end;
  end;

  procedure CancelAllPrintFiles;
    {-Cancels all files in the print queue}
  var
    DR : DpmiRegisters;
  begin
    FillChar(DR, SizeOf(DR), 0);
    DR.AX := $0103;                             {cancel all files function}
    if SimulateRealModeInt($2F, DR) <> 0 then ; {print spool control interrupt}
  end;

  function GetPrintStatus(var QPtr : Pointer) : Byte;
    {-Halts printing, returns current error status, puts pointer to the filename
      queue in the QPtr variable. Filenames in the queue are 64-byte ASCIIZ
      strings. The end of the queue is marked by a name starting with a null.}
  var
    DR : DpmiRegisters;
  begin
    with DR do begin
      TempSele := $0000;
      QPtr := nil;
      FillChar(DR, SizeOf(DR), 0);
      AX := $0104;                               {access print queue function}
      if SimulateRealModeInt($2F, DR) <> 0 then  {print spool control interrupt}
        GetPrintStatus := 1
      else if Odd(Flags) then                    {check carry flag}
        GetPrintStatus := Byte(AX and $00FF)
      else if GetSelectorForRealMem(Ptr(DS, SI), $FFFF, TempSele) <> 0 then
        GetPrintStatus := 1                      {couldn't alloc selector}
      else begin
        QPtr := Ptr(TempSele, 0);
        GetPrintStatus := 0;
      end;
    end;
  end;

  procedure EndPrintStatus;
    {-Releases the spooler from the GetPrintStatus procedure.}
  var
    DR : DpmiRegisters;
  begin
    if TempSele <> $0000 then
      if FreeLDTDescriptor(TempSele) <> 0 then
        Exit;
    FillChar(DR, SizeOf(DR), 0);
    DR.AX := $0105;                              {unfreeze queue function}
    if SimulateRealModeInt($2F, DR) <> 0 then ;  {print spool control interrupt}
  end;

{$ELSE}

  function PrintInstalled : Boolean;
    {-Returns True if PRINT.COM is installed}
  begin
    {$IFDEF VIRTUALPASCAL}
    PrintInstalled := False;
    {$ELSE}
    {INT $2F functions available only in DOS 3}
    if DosVersion >= $300 then
      with Regs do begin
        AX := $0100;         {get PRINT installed status}
        Intr($2F, Regs);     {print spool control interrupt}
        PrintInstalled := (AL = $FF); {DOS returns $FF in AL if PRINT installed}
      end
    else
      PrintInstalled := False;
    {$ENDIF}
  end;

  function SubmitPrintFile(FileName : string) : Byte;
    {-This procedure submits a file to the PC DOS 3.0 or greater concurrent
      print utility.}
  type
    AsciiZ = array[1..65] of Char;
    SubmitPacket = record
                     Level : Byte;
                     FilenamePtr : ^AsciiZ;
                   end;
  var
    SubPack : SubmitPacket;
    S : string;
    SLen : Byte absolute S;
  begin
    {$IFNDEF VIRTUALPASCAL}
    S := Trim(FileName);
    S := FullPathName(S);    {!!.12}
    if SLen <> 0 then
      with SubPack, Regs do begin
        Level := 0;          {set level code}
        if SLen > 64 then
          SLen := 64;        {truncate filenames longer than 64 characters}
        S[Succ(SLen)] := #0; {add null to end of string}
        FilenamePtr := @S[1]; {point to first character in S}
        DS := Seg(SubPack);  {DS:DX points to the packet}
        DX := Ofs(SubPack);
        AX := $0101;         {submit file to be printed}
        Intr($2F, Regs);     {print spool control interrupt}
        if Odd(Flags) then   {check carry flag}
          SubmitPrintFile := AL {carry set, return code in AL}
        else
          SubmitPrintFile := 0;
      end
    else
    {$ENDIF}
      SubmitPrintFile := 2;  {return the code for a file not found error}
  end;

  procedure CancelPrintFile(FileMask : string);
    {-Cancels the files matched by the file mask passed in FileMask.}
  var
    Len : Byte absolute FileMask;
  begin
    {$IFNDEF VIRTUALPASCAL}
    if Len > 64 then
      Len := 64;             {truncate filenames longer than 64 characters}
    with Regs do begin
      FileMask[Succ(Len)] := #0; {make FileMask an ASCIIZ string}
      DS := Seg(FileMask);   {DS:DX points to the ASCIIZ string}
      DX := Ofs(FileMask[1]);
      AX := $0102;           {cancel print file}
      Intr($2F, Regs);       {print spool control interrupt}
    end;
    {$ENDIF}
  end;

  procedure CancelAllPrintFiles;
    {-Cancels all files in the print queue}
  begin
    {$IFNDEF VIRTUALPASCAL}
    Regs.AX := $0103;        {cancel all files function}
    Intr($2F, Regs);         {print spool control interrupt}
    {$ENDIF}
  end;

  function GetPrintStatus(var QPtr : Pointer) : Byte;
    {-Halts printing, returns current error status, puts pointer to the filename
      queue in the QPtr variable. Filenames in the queue are 64-byte ASCIIZ
      strings. The end of the queue is marked by a name starting with a null.}
  begin
    {$IFDEF VIRTUALPASCAL}
    GetPrintStatus := 1;
    {$ELSE}
    with Regs do begin
      AX := $0104;           {access print queue function}
      Intr($2F, Regs);       {print spool control interrupt}
      {check carry flag}
      if Odd(Flags) then begin
        {carry set, return code in AL}
        QPtr := nil;
        GetPrintStatus := AL;
      end
      else begin
        {DS:SI points to the queue}
        QPtr := Ptr(DS, SI);
        GetPrintStatus := 0;
      end;
    end;
    {$ENDIF}
  end;

  procedure EndPrintStatus;
    {-Releases the spooler from the GetPrintStatus procedure.}
  begin
    {$IFNDEF VIRTUALPASCAL}
    Regs.AX := $0105;        {unfreeze queue function}
    Intr($2F, Regs);         {print spool control interrupt}
    {$ENDIF}
  end;

{$ENDIF}     {!!.21 end}

  function GetEnvironmentString(SearchString : string) : string;
    {-Return a string from the environment}
  begin
    while SearchString[Length(SearchString)] = '=' do
      Dec(SearchString[0]);
    GetEnvironmentString := Dos.GetEnv(SearchString);
  end;

  function PtrDiff(H, L : Pointer) : LongInt;
    {-Return the number of bytes between H^ and L^. H is the higher address}
  var
    High : SegOfs absolute H;
    Low : SegOfs absolute L;
  begin
    {$IFDEF VIRTUALPASCAL}
    PtrDiff := Word(H) - Word(L);
    {$ELSE}
    PtrDiff := (LongInt(High.S) shl 4+High.O)-(LongInt(Low.S) shl 4+Low.O);
    {$ENDIF}
  end;

  function SetBlock(var Paragraphs : Word) : Boolean;
    {-Change size of DOS memory block allocated to this program}
  begin
    {$IFDEF VIRTUALPASCAL}
    SetBlock := True;
    {$ELSE}
    with Regs do begin
      AH := $4A;
      ES := PrefixSeg;
      BX := Paragraphs;
      MsDos(Regs);
      Paragraphs := BX;
      SetBlock := not Odd(Flags);
    end;
    {$ENDIF}
  end;

  function UsingEmulator : Boolean;
    {-Return True if floating point emulator in use}
  type
    Array3 = array[1..3] of Char;
  const
    EmuSignature : Array3 = 'emu';
  var
    A3P : ^Array3;
  begin
    {$IFDEF VIRTUALPASCAL}
    UsingEmulator := False;
    {$ELSE}
    A3P := Ptr(SSeg, $E0);
    {using emulator if Test8087 is 0 and emulator's signature is found in SS}
    UsingEmulator := (Test8087 = 0) and (A3P^ = EmuSignature);
    {$ENDIF}
  end;

{$IFDEF Heap6} {!!.10} {New version of ExecDos follows} {!!.20}

  function ExecDos(Command : string; UseSecond : Boolean; EDP : ExecDosProc) : Integer;
    {-Execute any DOS command. Call with Command = '' for a new shell. If
      UseSecond is false, Command must be the full pathname of a program to be
      executed}
  var
    {Variables for saving and restoring state of system}
    OurInt23 : Pointer;
    OurInt24 : Pointer;
    SaveDta : Pointer;  {!!.01}

    {Variables for managing the heap compression}
    ParasWeHave : Word;
    ParasForDos : Word;
    ParasToKeep : Word;
    OldHeapEnd : Pointer;

    {Variables for parsing the command line}
    BlankPos : Word;
    PathName : string[127];
    CommandTail : string[127];
  begin
    {Current DOS memory allocation read from memory control block}
{$IFNDEF Dpmi}  {no heap compression needed in protected mode}         {!!.20}
    ParasWeHave := MemW[Pred(PrefixSeg):3];

    {Calculate amount of memory to give up}
    ParasForDos := Pred(PtrDiff(HeapEnd, HeapPtr) shr 4);

    {Calculate amount of memory to keep while in shell}
    ParasToKeep := ParasWeHave-ParasForDos;

    {See if enough memory to run DOS}
    if (ParasForDos > 0) and (ParasForDos < (MinSpaceForDos shr 4)) then begin
      ExecDos := -4;
      Exit;
    end;

    {Deallocate memory for DOS}
    if not SetBlock(ParasToKeep) then begin
      ExecDos := -2;
      Exit;
    end;

    {Save old end of heap and set new one}
    OldHeapEnd := HeapEnd;
    HeapEnd := HeapPtr;
{$ENDIF}                                                                {!!.20}

    {get parameters for Execute}
    if Command = '' then
      UseSecond := True;
    CommandTail := '';
    if not UseSecond {command processor} then begin
      {Command is assumed to be a full pathname for a program}
      BlankPos := Pos(' ', Command);
      if BlankPos = 0 then
        PathName := Command
      else begin
        CommandTail := Copy(Command, BlankPos, Length(Command));
        PathName := Copy(Command, 1, Pred(BlankPos));
      end;
    end
    else begin
      {Pathname is the full pathname for COMMAND.COM}
      PathName := GetEnvironmentString('COMSPEC');

      {if Command is empty, we're doing a shell}
      if Command <> '' then
        {we're asking COMMAND.COM to execute the command}
        CommandTail := '/C '+Command;
    end;

    {Let user routine store and clear the physical screen}
    EDP(ExecSaveScreen, 0);

    {let user routine show status info if entering DOS shell}
    if (Command = '') then
      {Pass user routine the approximate memory available in KB}
      EDP(ExecShowMemory, (ParasForDos-240) shr 6);

    {get current DTA}    {!!.01}
    GetDta(SaveDta);     {!!.01}

    {switch vectors}
    SwapVectors;

    {Call Turbo's EXEC function}
    Exec(PathName, CommandTail);
{$IFDEF Dpmi}                                                         {!!.20}
    {accomodate known problem with RTM using 386^Max as DPMI server}  {!!.20}
    if DosError = $4B00 then                                          {!!.20}
      DosError := 0;                                                  {!!.20}
{$ENDIF}                                                              {!!.20}

    {restore vectors}
    SwapVectors;

    {restore DTA}        {!!.01}
    SetDta(SaveDta);     {!!.01}

{$IFNDEF Dpmi}                                                         {!!.20}
    {Reallocate memory from DOS}
    if not SetBlock(ParasWeHave) then begin
      ExecDos := -3;
      Exit;
    end;

    {Put heap end back where it was}
    HeapEnd := OldHeapEnd;
{$ENDIF}                                                               {!!.20}

    {if not in shell, let user routine allow time to see result}
    if (Command <> '') or (DosError <> 0) then
      EDP(ExecPauseAfterRun, 0);

    {give user routine a chance to restore the screen}
    EDP(ExecRestoreScreen, 0);

    {If we get to here, our function result is in DosError}
    ExecDos := DosError;
  end;

{$ELSE}

  {$IFDEF VIRTUALPASCAL}
  function ExecDos(Command : string; UseSecond : Boolean; EDP : ExecDosProc) : Integer;
    Var
      BlankPos    : Integer;
      PathName : string[127];
      CommandTail : string[127];
    begin
      {get parameters for Execute}
      if Command = '' then
        UseSecond := True;
      CommandTail := '';
      if not UseSecond {command processor} then begin
        {Command is assumed to be a full pathname for a program}
        BlankPos := Pos(' ', Command);
        if BlankPos = 0 then
          PathName := Command
        else begin
          CommandTail := Copy(Command, BlankPos, Length(Command));
          PathName := Copy(Command, 1, Pred(BlankPos));
        end;
      end
      else begin
        {Pathname is the full pathname for COMMAND.COM}
        PathName := GetEnvironmentString('OS2_SHELL');

        {if Command is empty, we're doing a shell}
        if Command <> '' then
          {we're asking COMMAND.COM to execute the command}
          CommandTail := '/C '+Command;
      end;

      Dos.Exec( PathName, CommandTail );
      ExecDos := DosExitCode;
    end;
  {$ELSE}

  function EndOfHeap : Pointer;
    {-Returns a pointer to the end of the free list}
  var
    FreeSegOfs : SegOfs absolute FreePtr;
  begin
    if FreeSegOfs.O = 0 then
      {the free list is empty, add $1000 to the segment}
      EndOfHeap := Ptr(FreeSegOfs.S+$1000, 0)
    else
      EndOfHeap := Ptr(FreeSegOfs.S+(FreeSegOfs.O shr 4), 0);
  end;

  function ExecDos(Command : string; UseSecond : Boolean; EDP : ExecDosProc) : Integer;
    {-Execute any DOS command. Call with Command = '' for a new shell. If
      UseSecond is false, Command must be the full pathname of a program to be
      executed}
  label
    ExitPoint;
  var
    PathName,
    CommandTail : string[127];
    OurInt23,
    OurInt24,
    OldEndOfHeap,
    NewEndOfHeap,
    TopOfHeap : Pointer;
    BlankPos,
    Allocated,
    SizeOfFreeList,
    ParasToKeep,
    ParasWeHave,
    ParasForDos : Word;
    SaveDta : Pointer;  {!!.01}
  begin
    {Calculate number of bytes to save}
    TopOfHeap := Ptr(SegOfs(FreePtr).S+$1000, 0);
    SizeOfFreeList := PtrDiff(TopOfHeap, EndOfHeap);

    {get current DTA}    {!!.01}
    GetDta(SaveDta);     {!!.01}

    {If enough space available, use stack to store the free list}
    if (not UsingEmulator) and
       (LongInt(SizeOfFreeList)+StackSafetyMargin < LongInt(SPtr)) then begin
      NewEndOfHeap := Ptr(SSeg, 0);
      Allocated := 0;
    end
    else begin
      {Check for sufficient memory}
      if MaxAvail < LongInt(SizeOfFreeList) then begin
        {Insufficient memory to store free list}
        ExecDos := -1;
        Exit;
      end;

      {Allocate memory for a copy of free list}
      Allocated := SizeOfFreeList;
      if Allocated > 0 then
        GetMem(NewEndOfHeap, Allocated);

      {Recalculate the size of the free list}
      SizeOfFreeList := Word(PtrDiff(TopOfHeap, EndOfHeap));
    end;

    {Save the current pointer to the end of the free list}
    OldEndOfHeap := EndOfHeap;

    {Current DOS memory allocation read from memory control block}
    ParasWeHave := MemW[Pred(PrefixSeg):3];

    {Calculate amount of memory to give up}
    ParasForDos := Pred(PtrDiff(TopOfHeap, HeapPtr) shr 4);

    {Calculate amount of memory to keep while in shell}
    ParasToKeep := ParasWeHave-ParasForDos;

    {See if enough memory to run DOS}
    if (ParasForDos > 0) and (ParasForDos < (MinSpaceForDos shr 4)) then begin
      ExecDos := -4;
      goto ExitPoint;
    end;

    {Deallocate memory for DOS}
    if not SetBlock(ParasToKeep) then begin
      ExecDos := -2;
      goto ExitPoint;
    end;

    {get parameters for Execute}
    if Command = '' then
      UseSecond := True;
     CommandTail := '';
    if not UseSecond {command processor} then begin
      {Command is assumed to be a full pathname for a program}
      BlankPos := Pos(' ', Command);
      if BlankPos = 0 then
        PathName := Command
      else begin
        CommandTail := Copy(Command, BlankPos, Length(Command));
        PathName := Copy(Command, 1, Pred(BlankPos));
      end;
    end
    else begin
      {Pathname is the full pathname for COMMAND.COM}
      PathName := GetEnvironmentString('COMSPEC');

      {if Command is empty, we're doing a shell}
      if Command <> '' then
        {we're asking COMMAND.COM to execute the command}
        CommandTail := '/C '+Command;
    end;

    {Let user routine store and clear the physical screen}
    EDP(ExecSaveScreen, 0);

    {let user routine show status info if entering DOS shell}
    if (Command = '') then
      {Pass user routine the approximate memory available in KB}
      EDP(ExecShowMemory, (ParasForDos-240) shr 6);

    {Copy the free list to a safe location}
    Move(OldEndOfHeap^, NewEndOfHeap^, SizeOfFreeList);

    {switch vectors}
    SwapVectors;

    {Call Turbo's EXEC function}
    Exec(PathName, CommandTail);

    {restore vectors}
    SwapVectors;

    {restore DTA}        {!!.01}
    SetDta(SaveDta);     {!!.01}

    {Reallocate memory from DOS}
    if not SetBlock(ParasWeHave) then begin
      ExecDos := -3;
      goto ExitPoint;
    end;

    {Put free list back where it was}
    Move(NewEndOfHeap^, OldEndOfHeap^, SizeOfFreeList);

    {if not in shell, let user routine allow time to see result}
    if (Command <> '') or (DosError <> 0) then
      EDP(ExecPauseAfterRun, 0);

    {give user routine a chance to restore the screen}
    EDP(ExecRestoreScreen, 0);

    {If we get to here, our function result is in DosError}
    ExecDos := DosError;

ExitPoint:
    {Deallocate any dynamic memory used}
    if Allocated <> 0 then
      FreeMem(NewEndOfHeap, Allocated);
  end;
  {$ENDIF}

{$ENDIF}

  function DosBlockWrite(H : Word; var Src; N : Word) : Word;
    {-Calls DOS's BlockWrite routine. Returns 0 if successful, else the DOS
      error code.}
{$IFDEF VIRTUALPASCAL}
  Var
    rc    : Longint;
    Test  : Word;
  begin
    rc := SysFileWrite( H, Src, N, Test );
    If ( rc <> 0 ) or ( N <> Test ) then
      DosBlockWrite := rc
    else
      DosBlockWrite := 0;
{$ELSE}
  begin
    with Regs do begin
      AH := $40;             {write to file}
      BX := H;               {file handle}
      CX := N;               {Number of bytes to write}
      DS := Seg(Src);        {DS:DX points to buffer}
      DX := Ofs(Src);
      MsDos(Regs);           {returns bytes written in AX}

      {check carry flag, also the number of bytes written}
      if Odd(Flags) or (AX <> N) then
        DosBlockWrite := AX
      else
        DosBlockWrite := 0;
    end;
{$ENDIF}
  end;

  function TextSeek(var F : Text; Target : LongInt) : Boolean;
    {-Do a Seek for a text file opened for input. Returns False in case of I/O
      error.}
  var
    {$IFDEF VIRTUALPASCAL}
    rc  : Longint;
    {$ENDIF}
    T : LongRec absolute Target;
    Pos : LongInt;
  begin
    TextSeek := false;
    with {$IFNDEF VIRTUALPASCAL} Regs, {$ENDIF} TextRec(F) do begin

      {check for file opened for input}
      if Mode <> fmInput then
        Exit;

      {get current position of the file pointer}
      {$IFDEF VIRTUALPASCAL}
      rc := SysFileSeek( Handle, 0, 1, Pos );

      If rc <> 0 then
        Exit;
      {$ELSE}
      AX := $4201;           {move file pointer function}
      BX := Handle;          {file handle}
      CX := 0;               {if CX and DX are both 0, call returns the..}
      DX := 0;               {current file pointer in DX:AX}
      MsDos(Regs);

      {check for I/O error}
      if Odd(Flags) then
        Exit;

      {calculate current position for the start of the buffer}
      LongRec(Pos).HighWord := DX;
      LongRec(Pos).LowWord := AX;
      {$ENDIF}

      Dec(Pos, BufEnd);

      {see if the Target is within the buffer}
      Pos := Target-Pos;
      if (Pos >= 0) and (Pos < BufEnd) then
        {it is--just move the buffer pointer}
        BufPos := Pos
      else begin
        {have DOS seek to the Target-ed offset}
        {$IFDEF VIRTUALPASCAL}
        rc := SysFileSeek( Handle, Target, 0, Pos );

        If rc <> 0 then
          Exit;
        {$ELSE}
        AX := $4200;         {move file pointer function}
        BX := Handle;        {file handle}
        CX := T.HighWord;    {CX has high word of Target offset}
        DX := T.LowWord;     {DX has low word}
        MsDos(Regs);

        {check for I/O error}
        if Odd(Flags) then
          Exit;
        {$ENDIF}

        {tell Turbo its buffer is empty}
        BufEnd := 0;
        BufPos := 0;
      end;
    end;

    {if we get to here we succeeded}
    TextSeek := True;
  end;

  function TextFileSize(var F : Text) : LongInt;
    {-Return the size of text file F. Returns -1 in case of I/O error.}
  var
    OldHi, OldLow : Integer;
    {$IFDEF VIRTUALPASCAL}
    rc : Longint;
    OldPos, NewPos : Word;
    {$ENDIF}

  begin
    TextFileSize := -1;
    with {$IFNDEF VIRTUALPASCAL} Regs, {$ENDIF} TextRec(F) do begin
      {check for open file}
      if Mode = fmClosed then begin
        TextFileSize := -1;
        Exit;
      end;

      {$IFDEF VIRTUALPASCAL}
      { Get position }
      rc := SysFileSeek( Handle, 0, 1, OldPos );

      If rc <> 0 then
        Exit;

      { Move to end of file }
      rc := SysFileSeek( Handle, 0, 2, NewPos );

      If rc <> 0 then
        Exit;

      { Move to original position }
      rc := SysFileSeek( Handle, OldPos, 0, NewPos );
      If rc <> 0 then
        Exit;

      TextFileSize := NewPos;
      {$ELSE}
      {get current position of the file pointer}
      AX := $4201;           {move file pointer function}
      BX := Handle;          {file handle}
      CX := 0;               {if CX and DX are both 0, call returns the..}
      DX := 0;               {current file pointer in DX:AX}
      MsDos(Regs);

      {check for I/O error}
      if Odd(Flags) then begin
        TextFileSize := -1;
        Exit;
      end;

      {save current position of the file pointer}
      OldHi := DX;
      OldLow := AX;

      {have DOS move to end-of-file}
      AX := $4202;           {move file pointer function}
      BX := Handle;          {file handle}
      CX := 0;               {if CX and DX are both 0, call returns the...}
      DX := 0;               {current file pointer in DX:AX}
      MsDos(Regs);           {call DOS}

      {check for I/O error}
      if Odd(Flags) then begin
        TextFileSize := -1;
        Exit;
      end;

      {calculate the size}
      TextFileSize := LongInt(DX) shl 16+AX;

      {reset the old position of the file pointer}
      AX := $4200;           {move file pointer function}
      BX := Handle;          {file handle}
      CX := OldHi;           {high word of old position}
      DX := OldLow;          {low word of old position}
      MsDos(Regs);           {call DOS}

      {check for I/O error}
      if Odd(Flags) then
        TextFileSize := -1;
      {$ENDIF}
    end;
  end;

  function TextPos(var F : Text) : LongInt;
    {-Return the current position of the logical file pointer (that is,
      the position of the physical file pointer, adjusted to account for
      buffering). Returns -1 in case of I/O error.}
  var
    Position : LongInt;
    {$IFDEF VIRTUALPASCAL}
    rc : Longint;
    {$ENDIF}
  begin
    TextPos := -1;

    with {$IFNDEF VIRTUALPASCAL} Regs, {$ENDIF} TextRec(F) do begin
      {check for open file}
      if Mode = fmClosed then begin
        TextPos := -1;
        Exit;
      end;

      {get current position of the physical file pointer}
      {$IFDEF VIRTUALPASCAL}
      rc := SysFileSeek( Handle, 0, 1, Position );

      If rc <> 0 then
        Exit;
      {$ELSE}
      AX := $4201;           {move file pointer function}
      BX := Handle;          {file handle}
      CX := 0;               {if CX and DX are both 0, call returns the...}
      DX := 0;               {current file pointer in DX:AX}
      MsDos(Regs);           {call DOS}

      {check for I/O error}
      if Odd(Flags) then begin
        TextPos := -1;
        Exit;
      end;

      {calculate the position of the logical file pointer}
      LongRec(Position).HighWord := DX;
      LongRec(Position).LowWord := AX;
      {$ENDIF}

      if Mode = fmOutput then
        {writing}
        Inc(Position, BufPos)
      else
        {reading}
        if BufEnd <> 0 then
          Dec(Position, BufEnd-BufPos);

      {return the calculated position}
      TextPos := Position;
    end;
  end;

  function TextFlush(var F : Text) : Boolean;
    {-Flush the buffer(s) for a text file. Returns False in case of I/O error.}
  var
    Position : LongInt;
    P : LongRec absolute Position;
    Code : Word;
  begin
    {assume failure}
    TextFlush := False;

    with {$IFNDEF VIRTUALPASCAL} Regs, {$ENDIF} TextRec(F) do begin
      {check for open file}
      if Mode = fmClosed then
        Exit;

      {see if file is opened for reading or writing}
      if Mode = fmInput then begin
        {get current position of the logical file pointer}
        Position := TextPos(F);

        {exit in case of I/O error}
        if Position = -1 then
          Exit;

        {set the new position of the physical file pointer}
        {$IFDEF VIRTUALPASCAL}
        Code := SysFileSeek( Handle, Position, 0, Position);

        If Code <> 0 then
         Exit;
        {$ELSE}

        AX := $4200;         {move file pointer function}
        BX := Handle;        {file handle}
        CX := P.HighWord;    {CX has high word of offset}
        DX := P.LowWord;     {DX has low word}
        MsDos(Regs);         {call DOS}

        {check for I/O error}
        if Odd(Flags) then
          Exit;
        {$ENDIF}
      end
      else begin
        {write the current contents of the buffer, if any}
        if BufPos <> 0 then begin
          Code := DosBlockWrite(Handle, BufPtr^, BufPos);
          if Code <> 0 then
            Exit;
        end;

        {flush DOS's buffers}
        if not FlushDosBuffers(F) then
          Exit;
      end;

      {tell Turbo its buffer is empty}
      BufEnd := 0;
      BufPos := 0;
    end;

    {if we get to here we succeeded}
    TextFlush := True;
  end;

  function OpenStdDev(var F : Text; StdHandle : Word) : Boolean;
    {-Assign the text file to the specified standard DOS device}
  begin
    OpenStdDev := False;
    case StdHandle of
      StdInHandle,
      StdOutHandle,
      StdErrHandle,
      StdPrnHandle :
        begin
          {Initialize the file variable}
          Assign(F, '');
          if StdHandle = StdInHandle then {!!.11}
            Reset(F)                      {!!.11}
          else                            {!!.11}
            Rewrite(F);                   {!!.11}
          if IoResult = 0 then begin
            TextRec(F).Handle := StdHandle;
            if StdHandle = StdErrHandle then
              TextRec(F).BufSize := 1;
            OpenStdDev := True;
          end;
        end;
    end;
  end;

  function HandleIsConsole(Handle : Word) : Boolean;
    {-Return true if handle is the console device (input or output)}
{$IFDEF VIRTUALPASCAL}
  begin
    HandleIsConsole := SysFileIsDevice(Handle) = 1;
{$ELSE}
  begin
    with Regs do begin
      AX := $4400;
      BX := Handle;
      MsDos(Regs);
      if (DX and $80) = 0 then
        HandleIsConsole := False
      else
        HandleIsConsole := (DX and $02 <> 0) or (DX and $01 <> 0);
    end;
{$ENDIF}
  end;

  procedure SetRawMode(var F : Text; IsOn : Boolean);
    {-Set "raw" mode on or off for the specified text file (must be a device)}
  begin
    {$IFNDEF VIRTUALPASCAL}
    with TextRec(F), Regs do begin
      {check for open file}
      if (Mode < fmInput) or (Mode > fmInOut) then begin
        {Turbo's file not found error code}
        DosError := 103;
        Exit;
      end;

      DosError := 0;

      FillChar(Regs, SizeOf(Regs), 0);                              {!!.21}
      AX := $4400;           {Get device information}
      BX := Handle;
      MsDos(Regs);           {returns device info in DX}

      if not Odd(Flags) then begin
        {check bit 7 for device flag}
        if DL and $80 = 0 then
          Exit;

        {clear unwanted bits}
        DX := DX and $00AF;

        {select raw/cooked mode}
        if IsOn then
          {set bit 5 of DX}
          DL := DL or $20
        else
          {clear bit 5 of DX}
          DL := DL and $DF;

        AX := $4401;           {Set device information}
        BX := Handle;          {BX has file handle}
        MsDos(Regs);
      end;

      if Odd(Flags) then
        DosError := AX
      else
        DosError := 0;
    end;
    {$ENDIF}
  end;

  function FileHandlesOpen(CountDevices : Boolean) : Byte; {!!.01}
    {-Return the number of open files owned by a program}
{$IFDEF VIRTUALPASCAL}
  begin
    {$IFDEF OS2}
    Result := SysFileIncHandleCount(0) - FileHandlesLeft;
    IF NOT CountDevices THEN
      Dec(Result, 6); {subtract the device handle count}
    {$ELSE}
    Result := 1; // Cannot get information in Win32
    {$ENDIF}
  end;
{$ELSE}
  type
    HandleTable = array[0..254] of Byte;
  var
    HandlesPtr : ^HandleTable;
    I, N, Max : Byte;
{$IFDEF Dpmi}                                                        {!!.21}
    Sele : Word;                                                     {!!.21}
    P : Pointer;                                                     {!!.21}
{$ENDIF}                                                             {!!.21}
  begin
{$IFDEF Dpmi}                                                        {!!.21}
    LongInt(P) := MemL[PrefixSeg:$34];                               {!!.21}
    if not GetSelectorForRealMem(P, $100, Sele) = 0 then             {!!.21}
      Exit;                                                          {!!.21}
    HandlesPtr := Ptr(Sele, 0);                                      {!!.21}
{$ELSE}                                                              {!!.21}
    {pointer to file handles table at PrefixSeg:$34}
    HandlesPtr := Pointer(MemL[PrefixSeg:$34]);
{$ENDIF}                                                             {!!.21}

    {size of file handles table at PrefixSeg:$32}
    Max := Mem[PrefixSeg:$0032]-1;

    N := 0;
    for I := 0 to Max do
      if HandlesPtr^[I] <> $FF then
        case I of
          0..4 : Inc(N, Ord(CountDevices));
          else   Inc(N);
        end;

    FileHandlesOpen := N;
{$IFDEF Dpmi}                                                        {!!.21}
    if FreeLDTDescriptor(Sele) = 0 then ;                            {!!.21}
{$ENDIF}                                                             {!!.21}
  end;
{$ENDIF}

  function FileHandlesLeft : Byte;    {!!.01}  {revised}  {!!.11}
    {-Return the number of available file handles}
{$IFDEF VIRTUALPASCAL}
  var
    fhstate : Longint;
    i       : Longint;
  begin
    {$IFDEF OS2}
    Result := 0;
    {query each possible file handle; count the bad (closed/free) ones}
    for I := 6 to SysFileIncHandleCount(0) do
      {skip devices}
      If DosQueryFHState(I, fhstate) <> 0 then
        // Increase, if API returns error, indicating unused handle
        Inc(Result);
    {if no handles were free, then clean up the count}
    if Result > 0 then
      dec(Result); // Return 1 less than actual figure
    {$ELSE}
    Result := 1; // Cannot get this data in Win32
    {$ENDIF}
  end;
{$ELSE}
  const
    NullName : array[1..4] of Char = 'NUL'#0;
    MaxHandles = 255;
  var
    Handles : array[1..MaxHandles] of Word;
    N : Byte;
  begin
    inline(
      $30/$ED/               {xor ch,ch            ;CX = MaxHandles}
      $B1/<MaxHandles/       {mov cl,<MaxHandles}
      $BA/>NullName/         {mov dx,>NullName     ;DS:DX => NullName}
      $8D/$B6/>Handles/      {lea si,[bp+>Handles] ;SI has offset for Handles[1]}
                             {Next:}
      $B8/$02/$3D/           {mov ax,$3D02         ;DOS open file function}
      $CD/$21/               {int $21              ;call DOS}
      $72/$07/               {jc Close             ;start closing if CF set}
      $36/                   {ss:}
      $89/$04/               {mov [si],ax          ;save the Handle}
      $46/                   {inc si               ;inc pointer into Handles}
      $46/                   {inc si}
      $E2/$F2/               {loop Next            ;repeat if CX > 0}
                             {Close:}
      $F6/$D1/               {not cl               ;flip bits in cl}
      $88/$8E/>N/            {mov [bp+>N],cl       ;save handle count in N}
      $E3/$0C/               {jcxz Done            ;done if count is 0}
                             {CloseOne:}
      $4E/                   {dec si               ;dec pointer into Handles}
      $4E/                   {dec si}
      $36/                   {ss:}
      $8B/$1C/               {mov bx,[si]          ;get the handle into BX}
      $B8/$00/$3E/           {mov ax,$3E00         ;DOS close file function}
      $CD/$21/               {int $21              ;call DOS, ignore error}
      $E2/$F4);              {loop CloseOne        ;do it again}
                             {Done:}

    FileHandlesLeft := N;
  end;
{$ENDIF}

  function ExistFile(FName : string) : Boolean;
    {-Return true if file is found}
{$IFDEF VIRTUALPASCAL}
  var
    PathZ : array [0..SizeOf(PathStr)-1] of Char;
    SR: TOSSearchRec;
    rc: Longint;
  begin
    StrPCopy(PathZ, FName);
    rc := SysFindFirst(PathZ, $3F, SR, False);
    if rc = 0 then
      begin
        ExistFile := True;
        SysFindClose( SR );
      end
    else
      ExistFile := False;
{$ELSE}
  var
    FLen : Byte absolute FName;
  begin
    {check for empty string}
    if Length(FName) = 0 then
      ExistFile := False
    else with Regs do begin
      if (Length(FName) > 3) and (FName[2] = ':') then  {!!.30}
        if IsPhantom(UpCase(FName[1])) then begin       {!!.30}
          ExistFile := False;                           {!!.30}
          Exit;                                         {!!.30}
        end;                                            {!!.30}

      Inc(FLen);
      FName[FLen] := #0;
      AX := $4300;           {get file attribute}
      DS := Seg(FName);
      DX := Ofs(FName[1]);
      MsDos(Regs);
      ExistFile := (not Odd(Flags)) {and (IoResult = 0)} and {!!.10}
                   (CX and (VolumeID+Directory) = 0);
    end;
{$ENDIF}
  end;

  function ExistOnPath(FName : string; var FullName : string) : Boolean;
   {-Return true if FName is found in
      a) current directory
      b) program's directory (DOS 3.X only)
      c) any DOS path directory
    and return path name to file}
{$IFDEF VIRTUALPASCAL}
  Var
    PathZ   : array [0..SizeOf(PathStr)-1] of Char;
    Buffer  : Array [0..Sizeof(PathStr)] of Char;
    DosPath : String;

  begin
    ExistOnPath := False;

    StrPCopy( PathZ, FName );
    DosPath := GetEnv('PATH');
    if SysFileSearch(Buffer, PathZ, @DosPath[1]) <> nil then
      begin
        FullName := StrPas( Buffer );
        ExistOnPath := True;
      end;
{$ELSE}
  var
    Ppos, Fpos : Word;
    DosPath : string;
  begin
    {string empty?}
    if Length(FName) = 0 then begin
      ExistOnPath := False;
      Exit;
    end;

    {Assume success}
    ExistOnPath := True;

    {Check current directory}
    if ExistFile(FName) then begin
      FullName := FName;
      Exit;
    end;

    {If DOS 3 or higher, check the directory where the program was found}
    if DosVersion >= $300 then begin
      FullName := JustPathname(ParamStr(0));
      FullName := AddBackSlash(FullName)+FName;
      if ExistFile(FullName) then
        Exit;
    end;

    {Check the path}
    DosPath := Dos.GetEnv('PATH');
    Ppos := 1;
    while Ppos < Length(DosPath) do begin

      {Find the termination of the current path entry}
      Fpos := Ppos;
      while (Fpos <= Length(DosPath)) and (DosPath[Fpos] <> ';') do
        Inc(Fpos);

      if Fpos > Ppos then begin
        {A path entry found}
        FullName[0] := Char(Fpos-Ppos);
        Move(DosPath[Ppos], FullName[1], Fpos-Ppos);
        FullName := AddBackSlash(FullName)+FName;
        if ExistFile(FullName) then
          Exit;
      end;

      {Prepare to look at next item}
      Ppos := Succ(Fpos);
    end;

    {Not found, even on the path}
    ExistOnPath := False;
    FullName := FName;
{$ENDIF}
  end;

  function TimeMs : LongInt;
    {-Return time of day in milliseconds since midnight}
{$IFDEF VIRTUALPASCAL}
  begin
    TimeMs := SysSysMsCount;
{$ELSE}
  begin
    with Regs do begin
      AH := $2C;
      MsDos(Regs);
      TimeMs := 1000*(LongInt(DH)+60*(LongInt(CL)+60*LongInt(CH)))+10*LongInt(DL);
    end;
{$ENDIF}
  end;

  {!!.10} {Rewritten}
  function IsDirectory(FName : String) : Boolean;
    {-Return true if FName is a directory}
  var
    IO : Word;
    CurDir : PathStr;
    CurDestDir : PathStr;
    DiffDrive : Boolean;
  begin
    GetDir(0, CurDir);

    if (Length(FName) >= 2) and (FName[2] = ':') and (FName[1] <> CurDir[1])
    then begin
      {Checking on a different drive}
      if IsPhantom(UpCase(FName[1])) then begin    {!!.30}
        IsDirectory := False;                      {!!.30}
        Exit;                                      {!!.30}
      end;                                         {!!.30}

      DiffDrive := True;
      ChDir(FName[1]+':');
      if IoResult <> 0 then begin
        {Restore current drive and directory just in case} {!!.22}
        ChDir(CurDir);                                     {!!.22}
        IsDirectory := False;
        Exit;
      end;
      GetDir(0, CurDestDir);
    end else
      DiffDrive := False;

    ChDir(FName);
    IsDirectory := (IoResult = 0);

    if DiffDrive then begin
      ChDir(CurDestDir);
      IO := IoResult;
    end;

    ChDir(CurDir);
    IO := IoResult;
  end;

  function SameFile(FilePath1, FilePath2 : String;
                    var ErrorCode : Word) : Boolean;
    {-Return true if FilePath1 and FilePath2 refer to the same physical file.
      Error codes:
        0 - Success (no error)
        1 - Invalid FilePath1
        2 - Invalid FilePath2
        3 - Error on Dos Set/GetFAttr
    }
  var
    F1, F2 : File;
    Attr1, Attr2, NewAttr : Word;
  begin
    SameFile := False;
    ErrorCode := 0;
    Assign(F1,FilePath1);
    Assign(F2,FilePath2);
    GetFAttr(F1,Attr1);
    if DosError <> 0 then begin
      ErrorCode := 1;
      Exit;
    end;
    GetFAttr(F2,Attr2);
    if DosError <> 0 then begin
      {leave ErrorCode at 0 if file not found but path is valid}
      if DosError <> 2 then
        ErrorCode := 2;
      Exit;
    end;
    if Attr1 <> Attr2 then
      Exit;
    if ((Attr1 and Archive) = 0) then
      NewAttr := Attr1 or Archive
    else
      NewAttr := Attr1 and (not Archive);
    SetFAttr(F1,NewAttr);
    if DosError <> 0 then begin
      ErrorCode := 3;
      Exit;
    end;
    GetFAttr(F2,Attr2);
    if DosError <> 0 then
      ErrorCode := 3;

    SameFile := Attr2 = NewAttr;

    SetFAttr(F1,Attr1);
    if DosError <> 0 then
      ErrorCode := 3;
  end;

  function CopyFile(SrcPath, DestPath : String;
                    Buffer : Pointer;
                    BufferSize : Word) : Word;
    {-Copy the file specified by SrcPath into DestPath. DestPath must specify
      a complete filename, it may not be the name of a directory without the
      file portion.  This a low level routine, and the input pathnames are not
      checked for validity. Buffer must already be allocated, and must be no
      less than BufferSize.}
  var
    ErrorCode,BytesRead,BytesWritten : Word;
    Time : LongInt;
    Src,Dest : File;
    SaveFileMode : Word;                                 {!!.03}
    SaveFAttr : Word;                                    {!!.12}

    procedure UnDo(CloseAndDeleteDest : Boolean);
    begin
      Close(Src);
      if IoResult <> 0 then ;
      if CloseAndDeleteDest then begin
        Close(Dest);
        if IoResult <> 0 then ;
        Erase(Dest);
        if IoResult <> 0 then ;
      end;
    end;

  begin
    SaveFileMode := FileMode;                            {!!.03}
    Assign(Src,SrcPath);
    GetFAttr(Src, SaveFAttr);                            {!!.12}
    if DosError <> 0 then begin                          {!!.12}
      CopyFile := 1;                                     {!!.12}
      Exit;                                              {!!.12}
    end;                                                 {!!.12}
    FileMode := FileMode and $F0;                        {!!.03} {!!.14 moved}
    Reset(Src,1);
    FileMode := SaveFileMode;                            {!!.03}
    if IoResult <> 0 then begin
      CopyFile := 1;                   {unable to open SrcPath}
      Exit;
    end;
    Assign(Dest,DestPath);
    Rewrite(Dest,1);
    if IoResult <> 0 then begin
      CopyFile := 2;                   {unable to open DestPath}
      Undo(False);
      Exit;                                              {!!.12}
    end;
    while not EOF(Src) do begin
      BlockRead(Src,Buffer^,BufferSize,BytesRead);
      if IoResult <> 0 then begin
        CopyFile := 3;                 {error reading SrcPath}
        UnDo(True);
        Exit;
      end;
      BlockWrite(Dest,Buffer^,BytesRead,BytesWritten);
      if (IoResult <> 0) or (BytesWritten <> BytesRead) then begin
        CopyFile := 4;                 {error reading SrcPath}
        UnDo(True);                        {error writing DestPath}
        Exit;
      end;
    end;
    GetFTime(Src,Time);
    if DosError <> 0 then begin
      CopyFile := 5;                   {error getting SrcPath's Date/Time}
      UnDo(True);
      Exit;
    end;
    SetFTime(Dest,Time);
    if DosError <> 0 then begin
      CopyFile := 6;                   {error getting DestPath's Date/Time}
      UnDo(True);
      Exit;
    end;
    Close(Dest);
    if IoResult <> 0 then begin        {!!.02}
      CopyFile := 7;
      Close(Src);                      {!!.02}
      if IoResult <> 0 then ;          {!!.02}
    end                                {!!.02}
    else begin                         {!!.02}
      Close(Src);                      {!!.02}
      if IoResult <> 0 then ;          {!!.02}
      CopyFile := 0;                   {!!.02}
    end;                               {!!.02}
    SetFAttr(Dest, SaveFAttr);         {!!.12}
  end;

  procedure NoExecDosProc(ActionCode : ActionCodeType; Param : Word);
    {-Do-nothing ExecDosProc}
  begin
  end;

  {!!.21 many changes made in the following for disk label stuff in pmode}
  type
    XFCBrec =
      record
        Flag : Byte;   {should be $FF}
        Reserved0 : Array[1..5] of Byte; {should be all zeroes}
        AttrByte : Byte; {should be 8}
        DriveCode : Byte;
        FileSpec : Array[1..11] of Char;
        Reserved1 : Array[1..25] of Byte;
      end;
    XFCBPtr = ^XFCBRec;
    DTABuf = Array[0..63] of Char;
    DosMemRec =
      record
        Sele, Segm : Word;
      end;

  function GetMediaID(Drive : Char;
                      var MediaIDRec : MediaIDType) : Word; {!!.14}
{$IFDEF VIRTUALPASCAL}
begin
  // !!!
  GetMediaID := -1;
end;
{
  Var
    rc          : Longint;
    DriveNumber : Word;
    Buf         : Record
      SerialNum : Word;
      VolLabel  : String;
    end;

  Begin
    DriveNumber := Ord( Drive ) - Ord( 'A' ) + 1;

    rc := DosQueryFSInfo( DriveNumber, fsil_VolSer, Buf, Sizeof( Buf ));
    If rc = 0 then
      begin
        FillChar( MediaIDRec, Sizeof( MediaIDRec ), 0 );
        MediaIDRec.SerialNumber := Buf.SerialNum;
        Move( Buf.VolLabel[1], MediaIDRec.VolumeLabel, Length( Buf.VolLabel ));

//       Note: FileSystemID uninitialized
        MediaIDRec.InfoLevel := fsil_VolSer;
        GetMediaID := 0;
      end
    else
      GetMediaID := rc;
}
{$ELSE}
  {$IFDEF DPMI}
  var
    Status : Word;
    M : DosMemRec;
    Regs : DpmiRegisters;
  begin
    LongInt(M) := GlobalDosAlloc(SizeOf(MediaIDType));
    if LongInt(M) = 0 then begin
      GetMediaID := 8;
      Exit;
    end;
    with Regs do begin
      FillChar(Regs, SizeOf(DpmiRegisters), 0);
      DS := M.Segm; {DX already set to zero}
      AX := $440D;
      BX := Byte(Upcase(Drive))-Byte('A')+1;
      CX := $0866;
      Status := SimulateRealModeInt($21, Regs);
      if Status = 0 then
        if Odd(Flags) then
          Status := AX
        else begin
          Status := 0;
          move(Mem[M.Sele:0], MediaIDRec, SizeOf(MediaIDType));
        end;
      GetMediaID := Status;
      Status := GlobalDosFree(M.Sele);
    end;
  end;
  {$ELSE}
  begin
    with Regs do begin
      DS := Seg(MediaIDRec);
      DX := Ofs(MediaIDRec);
      AX := $440D;
      BX := Byte(Upcase(Drive))-Byte('A')+1;
      CX := $0866;
      MsDos(Regs);
      if Odd(Flags) then
        GetMediaID := AX
      else
        GetMediaID := 0;
    end;
  end;
  {$ENDIF}
{$ENDIF}

  function SetMediaID(Drive : Char;
                      var MediaIDRec : MediaIDType) : Word; {!!.14}
{$IFDEF VIRTUALPASCAL}
begin
  SetMediaID := -1;
end;
{
  Var
    rc          : Longint;
    DriveNumber : Word;
    Buf         : Record
      SerialNum : Word;
      VolLabel  : String[12];
    end;

  Begin
    DriveNumber := Ord( Drive ) - Ord( 'A' ) + 1;
    fillchar( Buf, Sizeof( Buf ), #0 );
    Buf.SerialNum := MediaIDRec.SerialNumber;
    Buf.VolLabel  := MediaIDRec.VolumeLabel;
    Buf.VolLabel[ Length( Buf.VolLabel)+1 ] := #0;

//    rc := DosSetFSInfo( DriveNumber, fsil_VolSer, Buf, Sizeof( Buf ));
    rc := DosSetFSInfo( DriveNumber, fsil_VolSer, Buf.VolLabel, Sizeof( Buf.VolLabel ));
    If rc = 0 then
      SetMediaID := 0
    else
      SetMediaID := rc;
  end;
}
{$ELSE}
  {$IFDEF DPMI}
  var
    Status : Word;
    M : DosMemRec;
    Regs : DpmiRegisters;
  begin
    LongInt(M) := GlobalDosAlloc(SizeOf(MediaIDType));
    if LongInt(M) = 0 then begin
      SetMediaID := 8;
      Exit;
    end;
    move(MediaIDRec, Mem[M.Sele:0], SizeOf(MediaIDType));
    with Regs do begin
      FillChar(Regs, SizeOf(DpmiRegisters), 0);
      DS := M.Segm; {DX already set to zero}
      AX := $440D;
      BX := Byte(Upcase(Drive))-Byte('A')+1;
      CX := $0846;
      Status := SimulateRealModeInt($21, Regs);
      if Status = 0 then
        if Odd(Flags) then
          Status := AX
        else
          Status := 0;
      SetMediaID := Status;
      Status := GlobalDosFree(M.Sele);
    end;
  end;
  {$ELSE}
  begin
    with Regs do begin
      DS := Seg(MediaIDRec);
      DX := Ofs(MediaIDRec);
      AX := $440D;
      BX := Byte(Upcase(Drive))-Byte('A')+1;
      CX := $0846;
      MsDos(Regs);
      if Odd(Flags) then
        SetMediaID := AX
      else
        SetMediaID := 0;
    end;
  end;
  {$ENDIF}
{$ENDIF}
  procedure InitVolXFCB(XFCBP : XFCBPtr; Drive : Char);
    {-Initialize XFCB to find a volume label}
  begin
    FillChar(XFCBP^, SizeOf(XFCBRec), 0);
    with XFCBP^ do begin
      FillChar(FileSpec, 11, '?');
      Flag := $FF;
      AttrByte := VolumeID;
      DriveCode := Byte(Upcase(Drive))-Byte('A')+1;
    end;
  end;

  function GetVolumeLabel(Drive : Char; var VolName : VolumeNameStr) : Word; {!!.14}
    {-Gets the volume label for Drive. Returns 0 for success, or Dos error
      code.}
{$IFDEF VIRTUALPASCAL}
  Var
    mID :MediaIDType;
  begin
    GetVolumeLabel := GetMediaID( Drive, mID );
    VolName := StrPas(mID.VolumeLabel);
{$ELSE}
  var
    SearchR : SearchRec;
    P : Byte;
  begin
    FindFirst(Drive+':\*.*', VolumeID, SearchR);
    if DosError = 0 then begin
      P := Pos('.', SearchR.Name);
      if (P = 0) or (P = 9) then begin
        if P = 9 then
          Delete(SearchR.Name, P, 1);
        VolName := SearchR.Name;
      end else
        VolName := Pad(Copy(SearchR.Name, 1, P-1), 8) +
                       Copy(SearchR.Name, P+1, 11);
    end else begin
      VolName := '';
      if DosError = 18 then
        DosError := 0;
    end;
    GetVolumeLabel := DosError;
{$ENDIF}
  end;

  function DeleteVolumeLabel(Drive : Char) : Word; {!!.20}
    {-Deletes an existing volume label on Drive. Returns 0 for success,
      or DOS error code.}
{$IFDEF VIRTUALPASCAL}
begin
  if SysSetVolumeLabel(Drive, '') then
    Result := 0
  else
    Result := 8;
end;
{$ELSE}
  {$IFDEF DPMI}
  var
    Status : Word;
    MFCB : DosMemRec;
    MDTA : DosMemRec;
    XFCBP : XFCBPtr;
    SaveDTAP : Pointer;
    Regs : DpmiRegisters;
    MediaID : MediaIDType;
  begin
    {Allocate DOS memory blocks}
    DeleteVolumeLabel := 8;
    LongInt(MDTA) := GlobalDosAlloc(SizeOf(DTABuf));
    if LongInt(MDTA) = 0 then
      Exit;
    LongInt(MFCB) := GlobalDosAlloc(SizeOf(XFCBRec));
    if LongInt(MFCB) = 0 then begin
      Status := GlobalDosFree(MDTA.Sele);
      Exit;
    end;

    {Save DTA and switch to ours}
    GetDTA(SaveDTAP);
    SetDta(Ptr(MDTA.Segm, 0));

    {Delete label via FCB}
    with Regs do begin
      XFCBP := Ptr(MFCB.Sele, 0);
      InitVolXFCB(XFCBP, Drive);
      FillChar(Regs, SizeOf(DpmiRegisters), 0);
      DS := MFCB.Segm; {DX already zero}
      AX := $1300;
      Status := SimulateRealModeInt($21, Regs);
      if Status = 0 then
        Status := Lo(AX);
    end;

    if Status = 0 then
      {Change DOS 4 media ID to all blanks}
      if GetMediaID(Drive, MediaID) = 0 then begin
        FillChar(MediaID.VolumeLabel, 11, ' ');
        if SetMediaID(Drive, MediaID) = 0 then ;
      end;

    {Restore previous DTA}
    SetDTA(SaveDTAP);

    {Return status code}
    DeleteVolumeLabel := Status;

    {Free DOS memory}
    Status := GlobalDosFree(MFCB.Sele);
    Status := GlobalDosFree(MDTA.Sele);
  end;

  {$ELSE}

  var
    Status : Word;
    XFCB : XFCBRec;
    DTA  : DTABuf;
    MediaID : MediaIDType;
    SaveDTAP : Pointer;
  begin
    {Save existing DTA and point to ours}
    GetDTA(SaveDTAP);
    SetDTA(@DTA);

    {Delete label via FCB}
    with Regs do begin
      InitVolXFCB(@XFCB, Drive);
      DS := Seg(XFCB);
      DX := Ofs(XFCB);
      AH := $13;
      MsDos(Regs);
      Status := AL;
    end;

    if Status = 0 then
      {Change DOS 4 media ID to all blanks}
      if GetMediaID(Drive, MediaID) = 0 then begin
        FillChar(MediaID.VolumeLabel, 11, ' ');
        if SetMediaID(Drive, MediaID) = 0 then ;
      end;

    {Restore DTA}
    SetDTA(SaveDTAP);

    {Return status}
    DeleteVolumeLabel := Status;
  end;
  {$ENDIF}
{$ENDIF}

  function SetVolumeLabel(Drive : Char; VolName : VolumeNameStr) : Word; {!!.14}
    {-Set the volume label for Drive to VolName. All 11 characters in VolName
      are significant. If VolName contains less than 11 characters, it will be
      padded with spaces to 11 characters. Returns 0 for success, or DOS
      error code.}
{$IFDEF VIRTUALPASCAL}
  begin
    if SysSetVolumeLabel(Drive, VolName) then
      Result := 0
    else
      Result := 8; // Error
  end;
{$ELSE}
  {$IFDEF DPMI}
  var
    Status : Word;
    MFCB : DosMemRec;
    MDTA : DosMemRec;
    XFCBP : XFCBPtr;
    SaveDTAP : Pointer;
    Regs : DpmiRegisters;
    MediaID : MediaIDType;
  begin
    {Allocate DOS memory blocks}
    SetVolumeLabel := 8;
    LongInt(MDTA) := GlobalDosAlloc(SizeOf(DTABuf));
    if LongInt(MDTA) = 0 then
      Exit;
    LongInt(MFCB) := GlobalDosAlloc(SizeOf(XFCBRec));
    if LongInt(MFCB) = 0 then begin
      Status := GlobalDosFree(MDTA.Sele);
      Exit;
    end;

    {Pad volume label to full length}
    VolName := Pad(VolName, 11);

    {Save DTA and switch to ours}
    GetDTA(SaveDTAP);
    SetDta(Ptr(MDTA.Segm, 0));

    with Regs do begin
      {Find existing label}
      XFCBP := Ptr(MFCB.Sele, 0);
      InitVolXFCB(XFCBP, Drive);
      FillChar(Regs, SizeOf(DpmiRegisters), 0);
      DS := MFCB.Segm; {DX already zero}
      AX := $1100;
      Status := SimulateRealModeInt($21, Regs);
      if Status = 0 then begin
        if Lo(AX) = 0 then begin
          {Rename existing label}
          FillChar(Regs, SizeOf(DpmiRegisters), 0);
          Move(VolName[1], DTABuf(Ptr(MDTA.Sele, $18)^), 11);
          DS := MDTA.Segm; {DX already zero}
          AX := $1700;
          Status := SimulateRealModeInt($21, Regs);
          if Status = 0 then
            Status := Lo(AX);
        end else begin
          {Create new label}
          FillChar(Regs, SizeOf(DpmiRegisters), 0);
          Move(VolName[1], XFCBP^.FileSpec, 11);
          DS := MFCB.Segm; {DX already zero}
          AX := $1600;
          Status := SimulateRealModeInt($21, Regs);
          if Status = 0 then
            Status := Lo(AX);
          if Status = 0 then begin
            FillChar(Regs, SizeOf(DpmiRegisters), 0);
            Move(VolName[1], XFCBP^.FileSpec, 11);
            DS := MFCB.Segm; {DX already zero}
            AX := $1000;
            Status := SimulateRealModeInt($21, Regs);
          end;
        end;
      end;
    end;
    if Status = 0 then
      {Set DOS 4 media ID too}
      if GetMediaID(Drive, MediaID) = 0 then begin
        Move(VolName[1], MediaID.VolumeLabel, 11);
        if SetMediaID(Drive, MediaID) = 0 then ;
      end;

    {Restore previous DTA}
    SetDTA(SaveDTAP);

    {Return status code}
    SetVolumeLabel := Status;

    {Free DOS memory}
    Status := GlobalDosFree(MFCB.Sele);
    Status := GlobalDosFree(MDTA.Sele);
  end;

  {$ELSE}

  var
    XFCB : XFCBRec;
    DTA  : DTABuf;
    MediaID : MediaIDType;
    SaveDTAP : Pointer;
    Status : Word;
  begin
    {Save existing DTA and point to ours}
    GetDTA(SaveDTAP);
    SetDTA(@DTA);

    {Pad new volume name to full length}
    VolName := Pad(VolName, 11);

    with Regs do begin
      {Find existing label, if any}
      InitVolXFCB(@XFCB, Drive);
      DS := Seg(XFCB);
      DX := Ofs(XFCB);
      AH := $11;
      MsDos(Regs);
      if AL = 0 then begin
        {Label exists, rename it}
        Move(VolName[1], DTA[$18], 11);
        DS := Seg(DTA);
        DX := Ofs(DTA);
        AH := $17;
        MsDos(Regs);
        Status := AL;
      end else begin
        {No label exists, create it}
        DS := Seg(XFCB);
        DX := Ofs(XFCB);
        Move(VolName[1], XFCB.FileSpec, 11);
        AH := $16;
        MsDos(Regs);
        Status := AL;
        if AL = 0 then begin                                           {!!.21}
          DS := Seg(XFCB);                                             {!!.21}
          DX := Ofs(XFCB);                                             {!!.21}
          AH := $10;                                                   {!!.21}
          MsDos(Regs);                                                 {!!.21}
        end;                                                           {!!.21}
      end;
    end;

    if Status = 0 then
      {Change DOS 4 media ID too}
      if GetMediaID(Drive, MediaID) = 0 then begin
        Move(VolName[1], MediaID.VolumeLabel, 11);
        if SetMediaID(Drive, MediaID) = 0 then ;
      end;

    {Restore DTA}
    SetDTA(SaveDTAP);

    {Return status}
    SetVolumeLabel := Status;
  end;
  {$ENDIF}
{$ENDIF}

  {!!.30 - New}
  function PhantomExists : Boolean;
  begin
    PhantomExists := IsPhantom('A') or IsPhantom('B');
  end;

  {!!.30 - New}
  procedure SelectPhantom(Drive : Char);
    {-Select a phantom drive as default if possible}
  var
    N : Byte;

  begin
{$IFNDEF VIRTUALPASCAL}
    If (DosVersion < $0302) then
      Exit;

    Drive := UpCase(Drive);
    N     := Ord(Drive) - Ord('A') + 1;

    FillChar(Regs, SizeOf(Regs), 0);
    Regs.BL := N;
    Regs.AX := $330F; { set to next logical drive entry }
    MsDos(Regs);
{$ENDIF}
  end;

  {$IFDEF VIRTUALPASCAL}
  function DosVersion : Word;
  begin
    DosVersion := SysOSVersion;
  end;

  {&Delphi+}
  function NumberOfDrives : Byte;
  Var
    Drives : Longint;

  begin
    Result := 0;
    Drives := SysGetValidDrives;
    { Count bits = drives }
    While Drives <> 0 do
      begin
        If odd(Drives) then
          Inc( Result );
        Drives := Drives shl 1;
      end;
  end;

  procedure SelectDrive(Drive : Char);
  const
    Buffer: Array[0..2] of Char = 'X:'#0;
  begin
    Buffer[0] := Drive;
    SysDirSetCurrent(Buffer);
  end;

  function DefaultDrive : Char;
  Var
    Buffer: array[0..259] of Char;
  begin
    if SysDirGetCurrent(0, Buffer) = 0 then
      Result := Buffer[0]
    else
      Result := 'C'; // Default if error
  end;

  function IsPhantom(Drive : Char) : Boolean;                    {!!.30}
  begin
    IsPhantom := False;
  end;

  function GetDiskInfo(Drive : Byte; var ClustersAvailable, TotalClusters,
                       BytesPerSector, SectorsPerCluster : Word) : Boolean;
{  Var
    rc        : Longint;
    DriveNum  : Word;
    FSInfo    : FSAllocate;
!!!}
  begin
{    DriveNum := Drive;
    rc := DosQueryFSInfo( DriveNum, fsil_Alloc, FSInfo, Sizeof( FSInfo ) );
    If rc = 0 then
      begin
        SectorsPerCluster := FSInfo.cSectorUnit;
        ClustersAvailable := FSInfo.cUnitAvail;
        TotalClusters     := FSInfo.cUnit;
        BytesPerSector    := FSInfo.cbSector;
        GetDiskInfo := True;
      end
    else}
      GetDiskInfo := False;
  end;
  {$ENDIF}


{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
