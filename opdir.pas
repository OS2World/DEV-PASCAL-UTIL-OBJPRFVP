{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$R-,S-,I-,V-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                    OPDIR.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1987,1992.      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpDir;
  {-Use a pick window to select filenames}


interface

uses
  Use32,
  Dos,
  OpConst,    {!!.20}
  OpInline,
  OpString,
  OpDos,
  OpRoot,
  OpCrt,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  {$IFDEF UseDates}
  OpDate,
  {$ENDIF}
  OpCmd,
  OpFrame,
  OpWindow,
  OpPick;

{.F-}
const
  {---- Option codes for DirList ----}
  diDirsUpcase      = $0001;      {Raise directory names to uppercase}
  diFilesUpcase     = $0002;      {Raise filenames to uppercase}
  diExitIfOne       = $0004;      {Autoselect in GetFileName
                                   if one matching file}
  diSetFirstFile    = $0008;      {Position select bar on first non-directory}
  diOptimizeSize    = $0010;      {Optimize window size when directory is read}
  diUserNewHdr      = $0020;      {Set for user move routine
                                   when header is changed}
  diShowDrives      = $0040;      {Set to display drives in the dir list}
  {.... Internal flags ....}
  diUpdating        = $0100;      {Set while UpdateContents active}
  diReadDir         = $0200;      {Set when file directory must be searched}
  diUpdateHdr       = $0400;      {Set when header is to be updated}
  diResize          = $0800;      {Set when window resizing may be needed}
  diReposition      = $1000;      {Set when cursor repositioning is needed}
  diResort          = $2000;      {Set when resorting is needed}
  diReformat        = $4000;      {Set when strings must be reformatted}
  diFullPathHdr     = $8000;      {Set when mask header shows full path}

  {---- Default options for DirList ----}
  DefDirOptions     : Word = diSetFirstFile;
  BadDirOptions     : Word = diUpdating+diReadDir+diUpdateHdr+
                             diResize+diReposition+diResort+
                             diReformat+diFullPathHdr;

const
  diReadOnlyChar    : string[1] = 'R'; {Characters to display file attributes}
  diHiddenChar      : string[1] = 'H';
  diSystemChar      : string[1] = 'S';
  diArchiveChar     : string[1] = 'A';

  diDriveAttr       = $F0;        {Fake file attribute used for a drive letter}
  diFirstDrive      = 'A';        {First drive to consider}
  diLastDrive       = 'Z';        {Last drive to consider}

type
  FileStr           = String[12]; {Room for just a filename}

  DispRec           =             {Variable length record for display entry}
    record
      Cat           : Byte;       {Pick category}
      Disp          : String;     {String to display -- partially allocated}
    end;
  DispRecPtr        = ^DispRec;   {Pointer to a DispRec}

  DirRec            =             {Data from DOS SearchRec}
    record
      Attr          : Byte;       {File attribute}
      Time          : LongInt;    {Date/time stamp}
      Size          : LongInt;    {Size of file}
      Name          : FileStr;    {Name and extension of file}
    end;
  DirRecPtr         = ^DirRec;
  DirRecArray       = array[1..($FFF1 div SizeOf(DirRec))] of DirRec;
  DirArrayPtr       = ^DirRecArray;

  DriveSet          = set of diFirstDrive..diLastDrive; {Possible drives}

  DirListPtr        = ^DirList;   {Pointer to directory list object}

  diLessFunc        = function (var X, Y : DirRec) : Boolean;
  diSortProc        = procedure (D : DirListPtr);
  diRejectFunc      = function (var X : DirRec; D : DirListPtr) : Boolean;
  diFormatProc      = procedure (var X : DirRec; var pkCat : Byte;
                                 var S : String; D : DirListPtr);
  diInitCommandProc = procedure (D : DirListPtr;
                                 var StringProc : pkStringProc;
                                 var CommandInitProc : pkGenlProc);

  DirList           =
    object(PickList)
      diDPtr        : DirArrayPtr;{Pointer to array of fixed length records}
      diSPtr        : DispRecPtr; {Pointer to array of variable length strings}

      diFlags       : Word;       {Bit-mapped flags}
      diMaxFmatLen  : Byte;       {Maximum formatted length of DispName}
      diItemWidth   : Byte;       {Width of each pick column}
      diHeapToUse   : Word;       {Maximum bytes of heap space for file array}
      diMinFiles    : Word;       {Minimum acceptable files to fit in list}
      diMaxFiles    : Word;       {Maximum number of files stored}
      diCurFiles    : Word;       {Current number of files stored}
      diCurItem     : Word;       {Current working directory item}
      diFileAttr    : Byte;       {File search attribute}
      diMaskHdr     : Byte;       {Header number for file mask}
      diHdrPadSize  : Byte;       {Spaces around mask header}
      diMaxHdrLen   : Byte;       {Longest header length}
      diRejectCnt   : Word;       {Number of strings to reject}
      diDirStr      : String[9];  {String used for "<dir>"}
      diCurDir      : DirStr;     {Last selected directory string}
      diMask        : PathStr;    {File search mask}
      diSkip        : DriveSet;   {Set of drives to skip when diShowDrives on}
      diLeftDrive   : String[5];  {Delimiter to left of drive letter}
      diRightDrive  : String[5];  {Delimiter to right of driver letter}
      {$IFDEF UseDates}
      diDatePict    : String[13]; {Picture mask for date}
      diTimePict    : String[13]; {Picture mask for time}
      {$ENDIF}
      diRejectSet   : StringSet;  {Set of strings to reject}

      diLess        : diLessFunc;   {Less function used for sorting}
      diSort        : diSortProc;   {Control procedure for sorting}
      diReject      : diRejectFunc; {Function for choosing from files}
      diFormat      : diFormatProc; {Function for formatting file names}

      diDirCat      : pkItemType;   {Pick category used for drives/dirs} {!!.03}

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       HeapToUse : LongInt; {!!.01}
                       PickOrientation : pkGenlProc;
                       CommandInit : diInitCommandProc);
        {-Initialize a directory list}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HeapToUse : LongInt; {!!.01}
                             PickOrientation : pkGenlProc;
                             CommandInit : diInitCommandProc);
        {-Initialize a directory list with custom window options}
      destructor Done; virtual;
        {-Dispose of directory list}

      procedure AddMaskHeader(FullPath : Boolean;
                              PadSize : Byte;
                              MaxLen : Byte;
                              Posn : HeaderPosType);
        {-Add a header to display the search mask}
      procedure SetMask(Mask : String; FileAttr : Byte);
        {-Set search mask and attribute and reset list}
      procedure ResetList;
        {-Force reread of directory when it may have changed}
      procedure PreloadDirList;
        {-Read directory in advance, in order to optimize size before drawing}

      procedure ProcessSelf; virtual; {!!.01}
        {-Process pick commands leading to a file choice}
      procedure UpdateContents; virtual;
        {-Redraw the contents of the directory window}
      procedure SortList; virtual;
        {-Sort the list of directory entries}
      function Reject(var X : DirRec) : Boolean; virtual;
        {-Return True to exclude a particular entry from pick}
      procedure Format(var X : DirRec; var pkCat : Byte;
                       var S : String); virtual;
        {-Return a formatted string and category for a directory entry}

      function GetFileName(Mask : String; FileAttr : Byte;
                           var Name : PathStr) : Word;
        {-Set a new mask, pick a name and return. Returns status}
      procedure ChangeDirectory(Mask : String; ChangeCode : Byte);
        {-Change to specified directory, or let user browse around to do so}

      function GetMatchingFileCount : Word;
        {-Return number of files matched in last directory}
      function GetMatchingSize(ClusterSize : Word) : LongInt;
        {-Return number of bytes in all matched files}
      function GetSelectedFile : FileStr;
        {-Return 12-character name of selected file}
      function GetSelectedPath : PathStr;
        {-Return full pathname of selected file}
      function GetSelectedDir : DirStr;
        {-Return directory of selected file}
      procedure GetSelectedDirRec(var D : DirRec);
        {-Return full DirRec for selected file}
      function SelectedFileInCurDir : Boolean;
        {-Return true if selected file is in current directory}
      procedure RemoveSelectedFile;
        {-Remove current file from pick list}

      procedure diOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure diOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function diOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}
      function FileCapacity : Word;
        {-Return the maximum number of files list can hold}

      procedure SetNameFormat;
        {-Initialize for compact nnnnnnnn.xxx format}
      procedure SetNameSizeFormat(DirStr : String);
        {-Initialize for nnnnnnn.xxx sssssssss format}
      procedure SetNameSizeKFormat(DirStr : String);
        {-Initialize for nnnnnnn.xxx ssssssk format}
      {$IFDEF UseDates}
      procedure SetNameTimeFormat(DatePicture, TimePicture : String);
        {-Initialize for name date time format}
      procedure SetNameSizeTimeFormat(DirStr,
                                      DatePicture, TimePicture : String);
        {-Initialize for name size date time format}
      procedure SetNameSizeKTimeFormat(DirStr,
                                       DatePicture, TimePicture : String);
        {-Initialize for name size date time format}
      procedure SetAllFormat(DirStr, DatePicture, TimePicture : String);
        {-Initialize for name size date time attr format}
      {$ENDIF}
      procedure SetUserFormat(MaxFmatLen : Byte; FF : diFormatProc);
        {-Set up for a user-defined format function}

      procedure SetFileAttr(Color, Mono : Byte);
        {-Set video attributes for files}
      procedure SetDirAttr(Color, Mono : Byte);
        {-Set video attributes for directories}
      procedure SetSelectFileAttr(Color, Mono : Byte);
        {-Set video attributes for selected file}
      procedure SetSelectDirAttr(Color, Mono : Byte);
        {-Set video attributes for selected directory}

      procedure SetDirCategory(DirCat : pkItemType);             {!!.03}
        {-Set the pick list category for directories and drives} {!!.03}

      procedure SetSortOrder(SP : diSortProc);
        {-Set alternate routine for sorting}
      procedure SetRejectFunc(RF : diRejectFunc);
        {-Set alternate reject function for file selection}

      procedure AddRejectString(S : String);
        {-Add a string to the set to reject}
      procedure ClearRejectStrings;
        {-Clear the set of strings to reject}

      procedure SkipDrives(Drives : DriveSet);
        {-Specify drives to skip when drive letters show}
      procedure SetDriveDelim(Left, Right : String);
        {-Specify delimiter strings to appear left and right of drive letters}

      function GetMultiFile(N : Word) : FileStr;
        {-Return 12-character name of specified file}
      function GetMultiPath(N : Word) : PathStr;
        {-Return full pathname of specified file}
      procedure GetMultiDirRec(N : Word; var D : DirRec);
        {-Return full DirRec for specified file}
      function GetSelectedSize(ClusterSize : Word) : LongInt;
        {-Return total size of all selected files}
      procedure RemoveMultiFile(N : Word);
        {-Remove specified file from list}
      procedure RemoveSelectedFiles;
        {-Remove all selected files from list}
      function GetMultiDirPtr(N : Word) : DirRecPtr;
        {-Return pointer to DirRec for specified file}
      procedure FormatMultiFile(N : Word);
        {-Rebuild the format string for specified file}

      procedure SelectItem(N : Word); virtual;
        {-Select an item. Overrides PickList.SelectItem to avoid
          selecting directories}

    {$IFDEF UseStreams}
      constructor Load00(var S : IdStream);       {!!.03}
        {-Load a list from a stream - version 00} {!!.03}
      constructor Load(var S : IdStream);
        {-Load a list from a stream}
      procedure Store(var S : IdStream);
        {-Store a list in a stream}
    {$ENDIF}

      {++++++++ for internal use ++++++++}
      {.Z+}
      function diRecPtr(Item : Word) : DispRecPtr;
      procedure diQuickSort(L, R : Word);
      procedure diSetItemWidth;
      procedure diNewFormat;
      procedure diPartitionHeapSpace;
      procedure diReformatAll;
      procedure diUpdateMaskHdr;
      procedure diSetupMasks;
      procedure diSelectDir(Name : FileStr); virtual; {!!.10}
      procedure diSelectDrive(Name : FileStr);
      function diAddDRec(Rejectable : Boolean;
                         var DRec : DirRec) : Boolean; {!!.01}
      procedure diFindPrim(SearchString : PathStr; FileAttr : Byte); virtual; {!!.03}
      procedure diAddDrives;
      procedure diAddParent; {!!.01}
      procedure diFindFiles; virtual; {!!.03}
      procedure diCheckZeroFiles; {!!.01}
      function diMatchingFileCnt(var FirstFile : Word) : Word;
      procedure diSetChoiceFirstFile;
      procedure diUpdateFormatting(ReportError : Boolean);
      procedure diUpdateDirList(ReportError : Boolean);
      function diSelect : Boolean;
      {.Z-}
    end;

const
  {---- Option codes for PathList ----}
  paUpcase          = $0001; {Display directories in uppercase}
  paSetCurDir       = $0002; {Position select bar over current directory}
  paShowSize        = $0004; {Show the total bytes stored in each directory}
  paOptimizeSize    = $0008; {Optimize window size when directory is read}
  paAltCurDir       = $0010; {Display current directory in alternate attribute}
  {.... Internal flags ....}
  paUpdating        = $2000; {Set when UpdateContents is active}
  paFullPathDisp    = $4000; {Set when the full path function is used}
  paReadDir         = $8000; {Set when the DOS directory must be read}

  {---- Default options for PathList ----}
  DefPathOptions    : Word = paSetCurDir;
  BadPathOptions    : Word = paReadDir+paFullPathDisp+paUpdating;

  {---- Index to characters used for line drawing ----}
  paTee             = 1;          {Â}
  paPipe            = 2;          {³}
  paArrow           = 3;          {Ã}
  paBar             = 4;          {Ä}
  paEll             = 5;          {À}

type
  PathDrawArray     = array[paTee..paEll] of Char;

  PathRec           =             {Data from DOS SearchRec}
    record
      PCat          : Byte;       {Pick category}
      PLevel        : Byte;       {Nesting level of directory. Root = 0}
      PSize         : LongInt;    {Total bytes stored in directory}
      PName         : FileStr;    {Name and extension of file}
    end;
  PathRecArray      = array[1..($FF00 div SizeOf(PathRec))] of PathRec;
  PathArrayPtr      = ^PathRecArray;

  PathListPtr       = ^PathList;

  paInitCommandProc = procedure (P : PathListPtr;
                                 var StringProc : pkStringProc;
                                 var CommandInitProc : pkGenlProc);

  PathList          =
    object(PickList)
      paDPtr        : PathArrayPtr; {Pointer to array of path records}
      paFlags       : Word;       {Bit-mapped flags}
      paMaxLen      : Byte;       {Maximum length of a path string}
      paItemWidth   : Byte;       {Width of each pick column}
      paHeapToUse   : Word;       {Maximum bytes of heap space for path array}
      paMaxFiles    : Word;       {Maximum number of files to store}
      paCurFiles    : Word;       {Current number of files stored}
      paCurItem     : Word;       {Current working path}
      paDrive       : Char;       {Drive being searched}
      paClusterSize : Word;       {Bytes in a disk cluster}
      paLine        : PathDrawArray; {Line drawing characters}

      constructor Init(X1, Y1, X2, Y2 : Byte;
                       HeapToUse : LongInt; {!!.01}
                       PickOrientation : pkGenlProc;
                       CommandInit : paInitCommandProc);
        {-Initialize a path list}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             Options : LongInt;
                             HeapToUse : LongInt; {!!.01}
                             PickOrientation : pkGenlProc;
                             CommandInit : paInitCommandProc);
        {-Initialize a path list with custom window options}
      destructor Done; virtual;
        {-Dispose of path list}

      procedure SetDrive(Drive : Char);
        {-Specify drive for path list}
      procedure ResetList;
        {-Force reread of directories when they may have changed}
      procedure UpdateContents; virtual;
        {-Redraw the complete pick window}
      procedure ProcessSelf; virtual; {!!.01}
        {-Display list if any, get one selection}
      procedure PreloadPathList;
        {-Read paths in advance, in order to optimize size before drawing}

      procedure SavePathList(FName : PathStr);      {!!.30}
        {-Save path names to a file}                {!!.30}
      procedure LoadPathList(FName : PathStr);      {!!.30}
        {-Load a list of path names from a file}    {!!.30}

      function GetPathName(Drive : Char) : PathStr;
        {-Set drive, pick a path, and return it. Returns empty string if error.}

      function GetSelectedPath : PathStr;
        {-Return selected pathname}

      procedure paOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure paOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function paOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}

      procedure SetClusterSize(Size : Word);
        {-Set cluster size for adjusting files to DOS boundaries}
      procedure SetDrawingChars(P : PathDrawArray);
        {-Set the characters used for line graphics}
      procedure SetLineDrawMode;
        {-Display hierarchy with line draw characters (default)}
      procedure SetFullPathMode;
        {-Display hierarchy with full path names}

      procedure SetPathAttr(Color, Mono : Byte);
        {-Set video attributes for paths}
      procedure SetSelectPathAttr(Color, Mono : Byte);
        {-Set video attributes for selected path}

      function GetMultiPath(N : Word) : PathStr;
        {-Return pathname of specified path}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a list from a stream}
      procedure Store(var S : IdStream);
        {-Store a list in a stream}
    {$ENDIF}

      {++++++++ for internal use ++++++++}
      {.Z+}
      function paMaxNumFiles : Word;
      procedure paSetItemWidth;
      procedure paFindPaths(Level : Byte; StartDir : PathStr); virtual; {!!.03}
      procedure paFindAllPaths; virtual; {!!.03}
      procedure paSetChoiceCurDir;
      {.Z-}
    end;

const
  paSingleDraw      : PathDrawArray = 'Â³ÃÄÀ';
const
  paDoubleDraw      : PathDrawArray = 'ËºÌÍÈ';

{.F+}

  {---- single vs multi file selection ----}
  procedure SingleFile(DirPtr : DirListPtr;
                       var StringProc : pkStringProc;
                       var CommandInitProc : pkGenlProc);
    {-Initialize for picking single files}
  procedure MultipleFile(DirPtr : DirListPtr;
                         var StringProc : pkStringProc;
                         var CommandInitProc : pkGenlProc);
    {-Initialize for picking multiple files}

  {---- sort options ----}
  procedure SortNone(DirPtr : DirListPtr);
    {-No sort}
  procedure SortName(DirPtr : DirListPtr);
    {-Sort alphabetically by name, then by extension}
  procedure SortDirName(DirPtr : DirListPtr);
    {-Sort directories first, then alphabetically by name and extension}
  procedure SortSize(DirPtr : DirListPtr);
    {-Sort by size, then by name and extension}
  procedure SortTime(DirPtr : DirListPtr);
    {-Sort by date, then by name/extension}
  procedure SortNameDirDrive(DirPtr : DirListPtr);      {!!.03}
    {-Sort files first, then directories, then drives}  {!!.03}

  {---- formatting routines ----}
  procedure NameFormat(var D : DirRec; var pkCat : Byte;
                       var S : String; DirPtr : DirListPtr);
    {-Simple default format for directory lists}
  procedure NameSizeKFormat(var D : DirRec; var pkCat : Byte;
                            var S : String; DirPtr : DirListPtr);
    {-Show name and size in Kilobytes}
  procedure NameSizeFormat(var D : DirRec; var pkCat : Byte;
                           var S : String; DirPtr : DirListPtr);
    {-Show name and size in bytes}
  {$IFDEF UseDates}
  procedure NameTimeFormat(var D : DirRec; var pkCat : Byte;
                           var S : String; DirPtr : DirListPtr);
    {-Show name and date/time}
  procedure NameSizeKTimeFormat(var D : DirRec; var pkCat : Byte;
                                var S : String; DirPtr : DirListPtr);
    {-Show name, size(K), time}
  procedure NameSizeTimeFormat(var D : DirRec; var pkCat : Byte;
                               var S : String; DirPtr : DirListPtr);
    {-Show name, size, time}
  procedure AllFormat(var D : DirRec; var pkCat : Byte;
                      var S : String; DirPtr : DirListPtr);
    {-Show name, size, time, attr}
  {$ENDIF}

  {---- reject routines ----}
  function RejectNone(var D : DirRec; DirPtr : DirListPtr) : Boolean;
    {-Don't reject any files}
  function RejectExtensions(var D : DirRec; DirPtr : DirListPtr) : Boolean;
    {-Reject files with extensions in RejectSet}
  function RejectFiles(var D : DirRec; DirPtr : DirListPtr) : Boolean;
    {-Reject all but subdirectories}

  {---- single vs multi path selection ----}
  procedure SinglePath(PathPtr : PathListPtr;
                       var StringProc : pkStringProc;
                       var CommandInitProc : pkGenlProc);
    {-Initialize for picking single paths}
  procedure MultiplePath(PathPtr : PathListPtr;
                         var StringProc : pkStringProc;
                         var CommandInitProc : pkGenlProc);
    {-Initialize for picking multiple paths}

  {---- stream initialization ----}
{$IFDEF UseStreams}
  procedure DirListStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing directory lists}
  procedure PathListStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing path lists}
{$ENDIF}

  {---- string procs for PickList ----}
  {.Z+}
  procedure DirStringProc(Item : Word; Mode : pkMode;
                          var ItemType : pkItemType;
                          var S : String; PickPtr : PickListPtr);
    {-Return filename string for pick list}

  procedure MultiStringProc(Item : Word; Mode : pkMode;
                            var ItemType : pkItemType;
                            var S : String; PickPtr : PickListPtr);
    {-Return filename string for multiple pick list}
  {.Z-}

  {---- formatting routines for pathnames ----}
  {.Z+}
  procedure PathLineDrawProc(Item : Word; Mode : pkMode;
                             var ItemType : pkItemType;
                             var S : String; PickPtr : PickListPtr);
    {-Return linedraw pathname string for pick list}
  procedure PathFullDrawProc(Item : Word; Mode : pkMode;
                             var ItemType : pkItemType;
                             var S : String; PickPtr : PickListPtr);
    {-Return full pathname string for pick list}
  {.Z-}

  {====================================================================}

implementation

var
  Pivot : DirRec;                 {Temporary storage for sort pivot}

  {$I OPDIR.IN1}                  {Low level routines}
  {$I OPDIR.IN2}                  {DirList}
  {$I OPDIR.IN3}                  {PathList}

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
