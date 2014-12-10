{$S-,R-,V-,I-,B-,F+,O+,A-}
{!!.21 overlays allowed}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}
{.$DEFINE Debug}

{*********************************************************}
{*                   OPROOT.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*          Compatibility with Virtual Pascal v2.1:       *}
{*             Copyright (c) 1995-2000 vpascal.com       *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpRoot;
  {-Base objects used in Object Professional}

interface

uses
  Use32,              {Redefine 32 bit data types}
  Dpmi,               {!!.20}
  Dos,
  {$IFDEF VIRTUALPASCAL}
  VpSysLow,
  {$ENDIF}
  OpConst,            {!!.20}
  OpInline,
  OpString;

type
  ByteArrayPtr = ^ByteArray;    {General memory mask}
{$IFDEF VirtualPascal}
  ByteArray    = array[0..1000000] of Byte;
{$ELSE}
  ByteArray    = array[0..65520] of Byte;
{$ENDIF}

  {Abstract object guarantees VMT pointer is in first word of all descendants}
  RootPtr = ^Root;
  Root    =
    object
      {!! DON'T ADD DATA FIELDS TO THIS OBJECT !!}
      constructor Init;
      destructor Done; virtual;
    end;

{$IFDEF UseStreams}

  {--------------- streams -----------------}

const
  {DOS file access modes}
  SCreate    = $3C00;           {Create new file, read/write access}
  SOpenRead  = $3D00;           {Read access only}
  SOpenWrite = $3D01;           {Write access only}
  SOpen      = $3D02;           {Read and write access}

  {SetPos positioning modes}
  PosAbs     = 0;               {Relative to beginning}
  PosCur     = 1;               {Relative to current position}
  PosEnd     = 2;               {Relative to end}

  {"Minus One", word style}
  {$IFDEF VIRTUALPASCAL}
  MinusOne   = $FFFFFFFF;
  {$ELSE}
  MinusOne   = $FFFF;
  {$ENDIF}
  {Reserved version number codes for ID streams}
  PtrVersion = $FFFF;      {Special version code used for storing pointers}

type
  {This specialized list is kept separate from the normal singly linked list
   in order to minimize the use of pointer parameters in the IdStream type}

  IdNodePtr = ^IdNode;
  IdNode =          {Nodes in list of types registered with IdStream}
    object
      idNext   : IdNodePtr;       {Pointer to next node in list}
      idCode   : SmallWord;       {Code associated with type}
      idVer    : SmallWord;       {Version of type, PtrVersion for pointer}
      idVmt    : Word;            {Offset of VMT in DS, PtrVersion for pointer}
      StorePtr : Pointer;         {@Method to write object to IdStream,
                                   or pointer}
      LoadPtr  : Pointer;         {@Constructor to read object from IdStream}

      {.Z+}
      constructor Init(Code, Ver : SmallWord; Vmt : Word; Store, Load : Pointer);
        {-Initialize an IdNode}
      {.Z-}
    end;

  IdList =
    object
      idHead : IdNodePtr;     {Start of list}
      {.Z+}
      procedure Clear;
        {-Initialize an empty list of items}
      procedure Deallocate;
        {-Dispose of list of items}
      procedure Append(I : IdNodePtr);
        {-Add element to end of list}
      function Head : IdNodePtr;
        {-Return pointer to head of list}
      function Tail : IdNodePtr;
        {-Return pointer to tail of list}
      function Next(P : IdNodePtr) : IdNodePtr;
        {-Returns a pointer to the next node}
      function FindByVmt(Vmt : Word) : IdNodePtr;
        {-Return pointer to matching IdNode, nil for none. Used by Put}
      function FindByVmtVer(Vmt, Ver : Word) : IdNodePtr;
        {-Return pointer to matching IdNode, nil for none. Used by Put}
      function FindPointer(Ptr : Pointer) : IdNodePtr;
        {-Return pointer to matching IdNode, nil for none. Used by Put}
      function FindByCodeVer(Code, Ver : SmallWord) : IdNodePtr;
        {-Return pointer to matching IdNode, nil for none. Used by Get}
      function Add(Code, Ver : SmallWord; Vmt : Word; StoreP, LoadP : Pointer) : Word;
        {-Add new Id to list, returning zero for success, non-zero for error.
          Ignores requests for duplicate entries.}
      function AddPointer(Code : SmallWord; Ptr : Pointer) : Word;
        {-Add new pointer to list}
      {.Z-}
    end;

  IdStreamPtr = ^IdStream;

  StreamRegisterProc = procedure (SPtr : IdStreamPtr);

  IdStream =                      {Abstract ID stream}
    object
      idRegistered : IdList;      {List of ID's and procedure pointers
                                  registered with this stream}
      idStatus     : Word;        {Status of last operation}
      idVmtError   : Word;        {Vmt offset when ecIdNotRegistered error occurred} {!!.03}

      {... For opening and closing streams}
      {.Z+}
      constructor Init;
        {-Clears Registered list and Status variable}
      destructor Done; virtual;
        {-Disposes IdList}
      {.Z-}

      {... For general input, output and control of streams}
      {.Z+}
      procedure Read(var Buf; Count : Word); virtual;
        {-Must be overridden}
      procedure Write(var Buf; Count : Word); virtual;
        {-Must be overridden}
      {.Z-}
      function ReadString : String;
        {-Read next variable length string from stream}
      procedure ReadRange(var BufStart, BufBeyond);
        {-Read Ofs(BufBeyond)-Ofs(BufStart) bytes into BufStart}
      procedure WriteString(S : String);
        {-Write a variable length string to stream}
      procedure WriteRange(var BufStart, BufBeyond);
        {-Write Ofs(BufBeyond)-Ofs(BufStart) bytes to stream}
      {.Z+}
      procedure Flush; virtual;
        {-Does nothing unless overridden}
      procedure Truncate; virtual;
        {-Must be overridden}
      {.Z-}
      function GetSize : Longint;
        {-Determine size of stream}
      procedure Seek(Pos : Longint);
        {-Set the current stream position to Pos,
          relative to the beginning of stream}
      function GetPos : Longint; virtual;
        {-Return the current stream position}
      function GetStatus : Word;
        {-Return status of stream}
      function PeekStatus : Word;
        {-Return status and leave internal variable unchanged}

      {... For I/O of objects}
      procedure RegisterType(Code, Version : SmallWord;
                             TypePtr, StorePtr, LoadPtr : Pointer);
        {-Registers one object type of specified version for IdStream I/O.
          May set Status to non-zero}
      procedure RegisterOldVersion(Code, Version : SmallWord;
                                   TypePtr, LoadPtr : Pointer);
        {-Registers an alternate loader for an old version of an object type}
      function TypeRegistered(TypePtr : Pointer) : Boolean;
        {-Return True if specified type is registered for store and load}
      function TypeVerRegistered(TypePtr : Pointer; Version : Word) : Boolean;
        {-Return True if specified type and version is registered for load}
      function CodeVerRegistered(Code, Version : SmallWord) : Boolean;
        {-Return True if specified code and version number are registered}
      procedure RegisterHier(RProc : StreamRegisterProc);
        {-Call a supplied routine to register a hierarchy of object types}

      procedure PutPtr(RPtr : RootPtr);
        {-Calls polymorphic Store method to write object at RPtr^}
      function GetPtr : RootPtr;
        {-Calls polymorphic Load constructor to allocate and read object}
      procedure Put(var R : Root);
        {-Calls polymorphic Store method to write object R}
      procedure Get(var R : Root);
        {-Calls polymorphic Load constructor to read static object R}

      {... For I/O of pointers}
      procedure RegisterPointer(Code : SmallWord; Ptr : Pointer);
        {-Registers one pointer for IdStream I/O}
      function PointerRegistered(Ptr : Pointer) : Boolean;
        {-Return True if specified pointer is registered with stream}
      procedure WritePointer(Ptr : Pointer);
        {-Writes identifier for pointer to stream}
      procedure WriteUserPointer(Ptr : Pointer; DefaultCode : SmallWord);
        {-Writes Ptr code if registered, else DefaultCode}
      function ReadPointer : Pointer;
        {-Reads pointer from stream}
      function ReadUserPointer(DefaultPtr : Pointer) : Pointer;
        {-Reads pointer and assigns default instead of nil}

      procedure Error(Code : Word); virtual;
        {-Report an error}

      {++++ For internal use ++++}
      {.Z+}
      procedure SetPos(Pos : Longint; Mode : Byte); virtual;
      function GetPrim(NewSelf : RootPtr) : RootPtr;
      {.Z-}
    end;

  DosIdStreamPtr = ^DosIdStream;
  DosIdStream =                   {Unbuffered DOS ID stream}
    object(IdStream)
      Handle       : Word;        {DOS file handle}

      {... For opening and closing streams}
      constructor Init(FileName : String; Mode : Word);
        {-Open DOS file in specified mode and initialize stream}
      destructor Done; virtual;
        {-Close file and dispose stream}

      {... For general input, output and control of streams}
      procedure Read(var Buf; Count : Word); virtual;
        {-Read bytes from DOS file}
      procedure Write(var Buf; Count : Word); virtual;
        {-Write bytes to DOS file}
      procedure Truncate; virtual;
        {-Truncate file at current position}

      {++++ for internal use ++++}
      {.Z+}
      function GetPos : Longint; virtual;
      procedure SetPos(Pos : Longint; Mode : Byte); virtual;
      procedure Close; virtual; {!!.01}
      procedure Open(var Name; Mode : Word); virtual; {!!.01}
      {.Z-}
    end;

  BufIdStreamPtr = ^BufIdStream;
  BufIdStream =
    object(DosIdStream)
      Buffer  : ByteArrayPtr;       {Address of character buffer}
      BufSize : Word;               {Bytes in buffer}
      BufPtr  : Word;               {Positions within buffer. Meaning depends}
      BufEnd  : Word;               {... on read or write mode}

      constructor Init(FileName : String; Mode, Size : Word);
        {-Open DOS file in mode, and allocate buffer of Size bytes}
      destructor Done; virtual;
        {-Flush and close file, deallocate buffer, dispose stream}
      procedure Flush; virtual;
        {-Flush buffer}
      procedure Read(var Buf; Count : Word); virtual;
        {-Read, using buffer}
      procedure Write(var Buf; Count : Word); virtual;
        {-Write, using buffer}

      {++++ for internal use ++++}
      {.Z+}
      function GetPos : Longint; virtual;
        {-Return position in DOS file after flushing}
      function FlushBuffer(FlushMode : Byte) : Boolean;
      {.Z-}
    end;

  MemIdStreamPtr = ^MemIdStream;
  MemIdStream =                   {ID stream already in memory}
    object(IdStream)
      MPtr        : ByteArrayPtr; {Pointer to base of stream data}
      MPos        : Word;         {Current position within stream}
      MSize       : Word;         {Maximum size of stream}

      {... For opening and closing streams}
      constructor Init(BasePtr : Pointer; Size : Word);
        {-Initialize stream to point to a location in memory}

      {... For general input, output and control of streams}
      procedure Read(var Buf; Count : Word); virtual;
        {-Read bytes from memory stream}
      procedure Write(var Buf; Count : Word); virtual;
        {-Write bytes to memory stream}
      procedure Truncate; virtual;
        {-Truncate at current position. Does nothing but reduce Size.}

      {++++ for internal use ++++}
      {.Z+}
      function GetPos : Longint; virtual;
      procedure SetPos(Pos : Longint; Mode : Byte); virtual;
      {.Z-}
    end;

  {--------------- libraries ----------------}

const
  {signature for directory entries}
  DirectorySig = Ord('O')+(Ord('P') shl 8); {'OP' => $504F}
  TempLibExt : string[3] = '$$$';

  {directory flag bits}
  deUnused  = $0001;
  deDeleted = $0002;
type
  DirEntryName = string[12];
  DirEntryPtr = ^DirEntry;
  DirEntry =
    object(Root)
      deSig        : SmallWord;    {used to flag valid directory entries}
      deName       : DirEntryName; {name of the entry}
      deStart      : LongInt;      {index into file for start of entry}
      deLength     : LongInt;      {size of the entry}
      deFlags      : Word;         {flag bits}
      deCode       : SmallWord;    {type code for entry}
      deVer        : SmallWord;    {version number for entry}
      deUser       : Pointer;      {set aside for user's use at run time}
      deCustom     : record end;   {for custom directory entries}
      {!! additional fields may not contain pointers or variably-sized data !!}

      constructor Init;
        {-Initialize an empty directory entry}
      {.Z+}
      constructor Load(var S : IdStream);
        {-Load a directory entry from a stream}
      procedure Store(var S : IdStream);
        {-Store a directory entry in a stream}
      {.Z-}

      procedure Rename(Name : DirEntryName);
        {-Change the name of the directory entry}
      procedure Update(Start, Len : LongInt; Code, Ver : SmallWord); virtual;
        {-Update the start and length data}
      procedure SetUserPointer(P : Pointer);
        {-Set the user pointer to P}

      procedure deOptionsOn(Options : Word);
        {-Set the specified flag(s)}
      procedure deOptionsOff(Options : Word);
        {-Clear the specified flag(s)}
      function deOptionsAreOn(Options : Word) : Boolean;
        {-Returns True if the specified flag(s) are on}

      function GetEntryName : DirEntryName;
        {-Return the name of the entry}
      function GetEntryStart : LongInt;
        {-Return the starting point for the entry}
      function GetEntryLength : LongInt;
        {-Return the length of the entry}
      function GetUserPointer : Pointer;
        {-Return the user pointer}
    end;
  DirEntries = array[1..512] of DirEntryPtr; {size is arbitrary}
  DirPtr = ^DirEntries;

    {NOTE: Effective with TP7, the word "Library" is reserved.  As a result,
     all references to Library have been replaced with OpLibrary.}

  OpLibrarySig = string[8];                                             {!!.20}
  OpLibraryPtr = ^OpLibrary;                                            {!!.20}
  OpLibrary =                                                           {!!.20}
    object(BufIdStream)
      liFileName   : PathStr;    {name of the file}
      liDirStart   : Word;  {index into file for start of directory}
      liDirectory  : DirPtr;     {pointer to the directory}

      liSig        : OpLibrarySig; {signature}                          {!!.20}
      liEntries    : Word;  {number of entries in the directory}
      liEntrySize  : Word;  {size of one directory entry}
      liCustom     : record end; {additional data for custom libraries}

      constructor Init(FileName : PathStr; Mode, BufferSize : Word;     {!!.20}
                       Sig : OpLibrarySig);
        {-Open an existing library}
      constructor Create(FileName : PathStr; BufferSize : Word;         {!!.20}
                         Sig : OpLibrarySig; Entries : Word);
        {-Create a new library}
      destructor Done; virtual;
        {-Flush and close library file, and dispose of directory and stream}
      {...}
      procedure PutEntry(Name : DirEntryName; var R : Root);
        {-Calls polymorphic Store method to write object R}
      procedure GetEntry(Name : DirEntryName; var R : Root);
        {-Calls polymorphic Load constructor to read static object R}
      function GetEntryPtr(Name : DirEntryName) : RootPtr;
        {-Calls polymorphic Load constructor to allocate and read object}
      {...}
      procedure DeleteEntry(Name : DirEntryName); virtual;
        {-Mark the entry of the specified name as deleted}
      procedure UndeleteEntry(Name : DirEntryName); virtual;
        {-Remove deletion mark from the specified entry}
      procedure KillEntry(Name : DirEntryName);
        {-Permanently delete the entry of the specified name}
      procedure RenameEntry(OldName, NewName : DirEntryName); virtual;
        {-Rename the specified entry}
      procedure GetCodeAndVersion(Name : DirEntryName; var Code, Ver : SmallWord);
        {-Return the code and version for the specified entry}
      {...}
      procedure Pack;
        {-Pack the library}
      function PackedFileSize : LongInt;
        {-Returns size of library after packing}
      function FileNeedsPacking : Boolean;
        {-Returns True if library needs packing}
      {...}
      function FindEntryByIndex(N : Word) : DirEntryPtr;
        {-Return a pointer to the N'th entry in the directory}
      function FindDirectoryEntry(Name : DirEntryName) : DirEntryPtr;
        {-Return a pointer to a directory entry; nil if not found or deleted}
      function FindAnyDirectoryEntry(Name : DirEntryName) : DirEntryPtr;
        {-Return a pointer to a directory entry; nil if not found}
      {...}
      function MaxEntries : Word;
        {-Return the maximum number of entries in the library's directory}
      function CurrentEntries : Word;
        {-Return the number of used entries in the library's directory,
          not including deleted entries}
      function DeletedEntries : Word;
        {-Return the number of deleted entries in the library's directory}
      function AvailableEntries : Word;
        {-Return number of entries available in library's directory}

      {+++ internal methods +++}
      {.Z+}
      function FindDirectoryIndex(Name : DirEntryName) : Word; virtual;
        {-Return the directory index for Name; 0 if not found or deleted}
      function FindAnyDirectoryIndex(Name : DirEntryName) : Word; virtual;
        {-Return the directory index for Name; 0 if not found}
      procedure UpdateHeader; virtual;
        {-Write the contents of the library header to disk}
      procedure UpdateDirectoryEntry(N : Word); virtual;
        {-Write the specified directory entry to disk}
      procedure UpdateDirectory;
        {-Update the entire directory}
      procedure RegisterEntryType; virtual;
        {-Register the type of the directory entry and set liEntrySize}
      procedure InitEntry(N : Word); virtual;
        {-Allocate and initialize directory entry N}
      function WriteEntryWhenPacking(N : Word) : Boolean; virtual;
        {-Return True if entry will be written to library when packing}
      function FindAvailableEntry : Word;
        {-Return index of first available entry, 0 if none}
      {.Z-}
    end;

  MemOpLibraryPtr = ^MemOpLibrary;
  MemOpLibrary =
    object(MemIdStream)
      mlDirStart   : Word;       {index into stream for start of directory}

      mlSig        : OpLibrarySig; {signature}
      mlEntries    : Word;       {number of entries in the directory}
      mlEntrySize  : Word;       {size of one directory entry}
      mlCustom     : record end; {additional data for custom libraries}

      constructor Init(BasePtr : Pointer; Size : Word);
        {-Open an existing library in memory}
      destructor Done; virtual; {!!.03}
        {-Close the library}
      {...}
      procedure GetEntry(Name : DirEntryName; var R : Root);
        {-Calls polymorphic Load constructor to read static object R}
      function GetEntryPtr(Name : DirEntryName) : RootPtr;
        {-Calls polymorphic Load constructor to allocate and read object}
      {...}
      function FindDirectoryEntry(Name : DirEntryName) : DirEntryPtr; virtual;
        {-Return a pointer to a directory entry; nil if not found or deleted}
      function FindAnyDirectoryEntry(Name : DirEntryName) : DirEntryPtr;
        virtual;
        {-Return a pointer to a directory entry; nil if not found}
      procedure GetCodeAndVersion(Name : DirEntryName; var Code, Ver : SmallWord);
        {-Return the code and version for the specified entry}

      {+++ internal methods +++}
      {.Z+}
      procedure RegisterEntryType; virtual;
        {-Register the type of the directory entry}
      procedure InitEntry(var DE); virtual;
        {-Initialize the VMT pointer for a directory entry}
      function GetDirEntryPtr(N : Word) : DirEntryPtr;
        {-Return a pointer to the N'th directory entry}
      procedure Write(var Buf; Count : Word); virtual;
        {-Write bytes to memory stream}
      {.Z-}
    end;

{$ENDIF}

  {--------------- pointer stack -----------------}

const
  MaxStackSize = 65520 div SizeOf(RootPtr);
  InitStackSize = 8;
type
  PointerArray = array[1..MaxStackSize] of RootPtr;
  PointerArrayPtr = ^PointerArray;
  PointerStackPtr = ^PointerStack;

  PointerStack =
    object(Root)
      psTop   : Word;
      psSize  : Word;
      psStatus : Word;
      psBase : PointerArrayPtr;

      constructor Init(Max : Word);
        {-Allocate a stack of pointers that can manage up to Max items}
      destructor Done; virtual;
        {-Deallocate the stack}
      function Size : Word;
        {-Return the size of the stack}
      procedure Push(P : RootPtr);
        {-Push P onto the stack}
      function Pop : RootPtr;
        {-Pop the topmost RootPtr off the stack}
      function SP : Word;
        {-Return the current top of stack index}
      function Peek(Index : Word) : RootPtr;
        {-Return stack element at position Index}
      function PeekTop : RootPtr;
        {-Return pointer currently at the top of stack}
      procedure Poke(P : RootPtr; Index : Word);
        {-Directly set a stack element}
      procedure Clear;
        {-Remove all elements from the stack}
      function GetStatus : Word;
        {-Return latest status code and reset internal status to zero}
      function Resize(Max : Word) : Boolean;
        {-Expand or shrink stack to hold Max pointers}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a stack from a stream}
      procedure Store(var S : IdStream);
        {-Store a stack in a stream}
    {$ENDIF}

      {++++ for internal use ++++}
      {.Z+}
      procedure Error(Code : Word); virtual;
      {.Z-}
    end;

  {----------------- static queue ------------------}

  StaticQueuePtr = ^StaticQueue;
  StaticQueue =
    object(Root)
      sqHead : Word;
      sqTail : Word;
      sqElSize : Word;
      sqSize : Word;
      sqStatus : Word;
      sqReject : Boolean;
      sqBase : ByteArrayPtr;

      constructor Init(NumElements, ElementSize : Word;
                       RejectIfFull : Boolean);
        {-Allocate space for static queue}
      destructor Done; virtual;
        {-Deallocate space for the queue}
      procedure PushTail(var Element);
        {-Add an element to the tail of the queue}
      procedure PopTail(var Element);
        {-Remove an element from the tail of the queue}
      procedure PeekTail(var Element);
        {-Look at the element at the tail of the queue}
      procedure PushHead(var Element); {!!.02}
        {-Add an element to the head of the queue}
      procedure PopHead(var Element);
        {-Remove an element from the front of the queue}
      procedure PeekHead(var Element);
        {-Look at the element at the front of the queue}
      procedure Clear;
        {-Flush all elements from the queue}
      function Elements : Word;
        {-Return the number of elements currently in the queue}
      function Nth(W : Word) : Pointer;                                 {!!.13}
        {-Returns a pointer to the Nth item in the Queue}
      function GetStatus : Word;
        {-Return latest status code and reset internal status to zero}
      procedure Error(Code : Word); virtual;
        {-Report error}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a static queue from a stream}
      procedure Store(var S : IdStream);
        {-Store a static queue to a stream}
    {$ENDIF}
      {++++ for internal use ++++}
      {.Z+}
      procedure sqInc(var SP : Word);
      procedure sqDec(var SP : Word);
      {.Z-}
    end;

  {--------------- singly linked list ----------------}

  SingleNodePtr = ^SingleListNode;
  SingleListNode =
    object(Root)
      slNext : SingleNodePtr;

      constructor Init;
        {-Initialize a singly linked list node}
    end;

  SingleListPtr = ^SingleList;
  SingleList =
    object(Root)
      slHead : SingleNodePtr;     {Start of list}
      slTail : SingleNodePtr;     {End of list}
      slSize : LongInt;           {Number of elements} {!!.21}

      constructor Init;
        {-Initialize an empty list of items}
      destructor Done; virtual;
        {-Destroy list}
      procedure Clear;                                                 {!!.20}
        {-Remove all items from list}
      procedure Reset;                                                 {!!.30}
        {-Clear the list without disposing of any items }              {!!.30}
      function Size : LongInt;    {!!.21}
        {-Return the size of the current list}
      procedure Append(P : SingleNodePtr);
        {-Add element to end of list}
      procedure Insert(P : SingleNodePtr);
        {-Insert element at start of list}
      procedure Place(P, L : SingleNodePtr);
        {-Place element P into list _after_ existing element L}
      procedure PlaceBefore(P, L : SingleNodePtr);
        {-Place element P into list _before_ existing element L}
      procedure Delete(P : SingleNodePtr);
        {-Delete existing element in list, dispose of its contents}
      procedure Remove(P : SingleNodePtr);
        {-Remove existing element from list without disposing of it}
      function Head : SingleNodePtr;
        {-Return pointer to head of list}
      function Tail : SingleNodePtr;
        {-Return pointer to tail of list}
      function Next(P : SingleNodePtr) : SingleNodePtr;
        {-Returns a pointer to the next node}
      function Nth(N : LongInt) : SingleNodePtr; {!!.03}
        {-Returns pointer to Nth node in list, starting at 1; nil if N > Size}
      function Posn(P : SingleNodePtr) : LongInt;                       {!!.13}
        {-Returns ordinal position of P in list; 0 if P=nil or not in list}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a list from a stream}
      procedure Store(var S : IdStream);
        {-Store a list in a stream}
    {$ENDIF}
      {++++ for internal use ++++}
      {.Z+}
      procedure slRemove(P : SingleNodePtr; DisposeIt : Boolean);
      {.Z-}
    end;

  {--------------- doubly linked list ----------------}

  DoubleNodePtr = ^DoubleListNode;
  DoubleListNode =
    object(Root)
      dlNext : DoubleNodePtr;
      dlPrev : DoubleNodePtr;
      constructor Init;
        {-Initialize a doubly linked list node}
    end;

  DoubleListPtr = ^DoubleList;
  DoubleList =
    object(Root)
      dlHead : DoubleNodePtr;     {Start of list}
      dlTail : DoubleNodePtr;     {End of list}
      dlSize : LongInt;           {Size of list} {!!.21}

      constructor Init;
        {-Initialize an empty list of items}
      destructor Done; virtual;
        {-Destroy list}
      procedure Clear;                                                 {!!.20}
        {-Remove all items from list}
      procedure Reset;                                                 {!!.30}
        {-Clear the list without disposing of any items }              {!!.30}
      function Size : LongInt;    {!!.21}
        {-Return the size of the current list}
      procedure Append(P : DoubleNodePtr);
        {-Add element to end of list}
      procedure Insert(P : DoubleNodePtr);
        {-Insert element at start of list}
      procedure Place(P, L : DoubleNodePtr);
        {-Place element P into list _after_ existing element L}
      procedure PlaceBefore(P, L : DoubleNodePtr);
        {-Place element P into list _before_ existing element L}
      procedure Delete(P : DoubleNodePtr);
        {-Delete existing element in list, disposing of its contents}
      procedure Remove(P : DoubleNodePtr);
        {-Remove existing element from list without disposing of it}
      function Head : DoubleNodePtr;
        {-Return pointer to head of list}
      function Tail : DoubleNodePtr;
        {-Return pointer to tail of list}
      function Next(P : DoubleNodePtr) : DoubleNodePtr; virtual;
        {-Returns a pointer to the next node}
      function Prev(P : DoubleNodePtr) : DoubleNodePtr; virtual;
        {-Returns a pointer to the previous node}
      function Nth(N : LongInt) : DoubleNodePtr; {!!.03}
        {-Returns pointer to Nth node in list, starting at 1; nil if N > Size}
      function Posn(P : DoubleNodePtr) : LongInt;                      {!!.13}
        {-Returns ordinal position of P in list; 0 if P=nil or not in list}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a list from a stream}
      procedure Store(var S : IdStream);
        {-Store a list in a stream}
    {$ENDIF}
      {++++ for internal use ++++}
      {.Z+}
      procedure dlRemove(P : DoubleNodePtr; DisposeIt : Boolean);
      {.Z-}
    end;

  {----------- circular linked list -------------}

  CircularListPtr = ^CircularList;
  CircularList =
    object(DoubleList)
      function Next(P : DoubleNodePtr) : DoubleNodePtr; virtual;
        {-Returns a pointer to the next node (wraps at end)}
      function Prev(P : DoubleNodePtr) : DoubleNodePtr; virtual;
        {-Returns a pointer to the previous node (wraps at begin)}
    {$IFDEF UseStreams}
      procedure Store(var S : IdStream);
        {-Store a list in a stream}
    {$ENDIF}
    end;

  {--------------- bit sets ----------------}

const
  MaxBitCount = 65534;              {Max bits in a bitset}
  NoMoreBits = $FFFFFFFF;           {Constant indicating no more bits}
type
  BitSetPtr = ^Bitset;
  BitSet =                          {Packed boolean set of up to 64K elements}
    object(Root)
      biMax : LongInt;              {Maximum element number}
      biBase : ByteArrayPtr;        {Pointer to data area}

      constructor Init(Max : LongInt);
        {-Allocate space for elements}
      destructor Done; virtual;
        {-Deallocate data area}
      procedure ClearAll; virtual;
        {-Clear all bits}
      procedure SetAll; virtual;
        {-Set all bits}
      function MaxBits : LongInt;
        {-Return capacity of bitset}

      procedure SetBit(Element : LongInt); virtual;
        {-Set bit}
      procedure ClearBit(Element : LongInt); virtual;
        {-Clear bit}
      procedure ToggleBit(Element : LongInt);
        {-Toggle bit}
      procedure ControlBit(Element : LongInt; State : Boolean);
        {-Set or clear bit depending on State}
      function TestSetBit(Element : LongInt) : Boolean;
        {-Return current state of bit and set it}
      function TestClearBit(Element : LongInt) : Boolean;
        {-Return current state of bit and clear it}

      function BitIsSet(Element : LongInt) : Boolean; virtual;
        {-Return True if bit is set}
      function NextSet(Element : LongInt) : LongInt; virtual;
        {-Return index of next set element, NoMoreBits if none}
      function NextClear(Element : LongInt) : LongInt; virtual;
        {-Return index of next clear element, NoMoreBits if none}
      function PrevSet(Element : LongInt) : LongInt; virtual;
        {-Return index of previous set element, NoMoreBits if none}
      function PrevClear(Element : LongInt) : LongInt; virtual;
        {-Return index of previous clear element, NoMoreBits if none}
      function BitsSet : LongInt; virtual;
        {-Return number of bits set}

      function FirstSet : LongInt;
        {-Return index of first set bit, NoMoreBits if none}
      function LastSet : LongInt;
        {-Return index of last set bit, NoMoreBits if none}
      function FirstClear : LongInt;
        {-Return index of first clear bit, NoMoreBits if none}
      function LastClear : LongInt;
        {-Return index of last clear bit, NoMoreBits if none}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a bitset from a stream}
      procedure Store(var S : IdStream);
        {-Store a bitset to a stream}
    {$ENDIF}
    end;

const
  MaxLargeBitCount = 1024*1024*8;   {Largest number of bits}
  BitPageSize = 512;                {Bytes allocated for each page of bits}
  BitPageShift = 9;                 {Shift count for bit page size}
type
  BytePointerArray = array[0..(MaxLargeBitCount div 8) div BitPageSize]
                     of ByteArrayPtr;
  LargeBitSetPtr = ^LargeBitset;
  LargeBitSet =                     {Packed boolean set of up to ~8M elements}
    object(BitSet)
      biPageCnt : Word;        {Number of pages allocated}
      biPages : ^BytePointerArray;  {Array of pointers to pages}
      biStatus : Word;         {Status variable}

      constructor Init(Max : LongInt);
        {-Allocate space for elements}

      {.Z+}
      destructor Done; virtual;
        {-Deallocate data area}
      procedure ClearAll; virtual;
        {-Clear all bits}
      procedure SetAll; virtual;
        {-Set all bits}
      procedure SetBit(Element : LongInt); virtual;
        {-Set bit}
      procedure ClearBit(Element : LongInt); virtual;
        {-Clear bit}
      function BitIsSet(Element : LongInt) : Boolean; virtual;
        {-Return True if bit is set}
      function NextSet(Element : LongInt) : LongInt; virtual;
        {-Return index of next set element past the current one,
          NoMoreBits if none}
      function NextClear(Element : LongInt) : LongInt; virtual;
        {-Return index of next clear element past the current one,
          NoMoreBits if none}
      function PrevSet(Element : LongInt) : LongInt; virtual;
        {-Return index of previous set element before the current one,
          NoMoreBits if none}
      function PrevClear(Element : LongInt) : LongInt; virtual;
        {-Return index of previous clear element before the current one,
          NoMoreBits if none}
      {.Z-}

      function Resize(Max : LongInt) : Boolean;
        {-Expand or shrink large bitset to hold Max bits}
      function GetStatus : Word;
        {-Return latest status code and reset internal status to zero}
      procedure Error(Code : Word); virtual;
        {-Report an error}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a bitset from a stream}
      procedure Store(var S : IdStream);
        {-Store a bitset to a stream}
    {$ENDIF}
      {++++ for internal use ++++}
      {.Z+}
      function biNext(Element : LongInt; SetClear : Boolean) : LongInt;
      function biPrev(Element : LongInt; SetClear : Boolean) : LongInt;
      {.Z-}
    end;

  {-------------- string dictionary ----------------}

const
  InitPoolSize = 16;              {Initial pool size - must be a power of two}

type
  HashPool = array[0..16376] of StringPtr;
  HashPoolPtr = ^HashPool;

  StringDictPtr = ^StringDict;

  StringDictFunc = function (SPtr : StringPtr; Value : LongInt;{!!.02}
                             SDPtr : StringDictPtr) : Boolean; {!!.02}

  StringDict =                    {Dictionary of strings (not case sensitive)}
    object(Root)
      sdSize : Word;         {Maximum elements in hash pool}
      sdUsed : Word;         {Current elements in hash pool}
      sdPool : HashPoolPtr;       {Pointer to hash pool}
      sdStatus : Word;       {Status variable}

      constructor Init;
        {-Allocate hash pool for a string dictionary}
      constructor InitCustom(PoolSize : Word); {!!.10}
        {-Allocate hash pool}
      destructor Done; virtual;
        {-Deallocate pool}
      procedure Add(S : String; Value : LongInt);
        {-Add string and associated value to dictionary}
      function Member(S : String; var Value : LongInt) : Boolean;
        {-Return true and associated value if S is element of dictionary}
      procedure Update(S : String; Value : LongInt);
        {-Change the data value associated with a string}
      procedure Remove(S : String);
        {-Remove string from dictionary}
      procedure Clear;
        {-Remove all strings from dictionary}
      function GetStatus : Word;
        {-Return status code and reset internal result to zero}
      procedure VisitAll(SDF : StringDictFunc);                        {!!.02}
        {-Visit all elements in dictionary, calling SDF for each one}  {!!.02}
      function GetString(Value : LongInt; var S : String) : Boolean;   {!!.02}
        {-Return the first string that has given Value, False if none} {!!.02}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a string dictionary from a stream}
      procedure Store(var S : IdStream);
        {-Store a string dictionary to a stream}
    {$ENDIF}
      function Hash(var S : String) : Word; virtual;
        {-Return a hash code for the specified string}
      function Equal(var S, T : String) : Boolean; virtual;
        {-Return True if the two strings are considered equal}
      procedure Error(Code : Word); virtual;
        {-Report an error}

      {++++ for internal use ++++}
      {.Z+}
      procedure StoreData(H : Word; var S : string; var D); virtual;
      procedure FreeData(H : Word); virtual;
      function Expand : Boolean;
      {.Z-}
    end;

  {--------- string set ---------}

  StringSetPtr = ^StringSet;
  StringSet =
    object(StringDict)
      procedure Add(S : String);
        {-Add string to set}
      function Member(S : String) : Boolean;
        {-Return true if S is element of set}
    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a list from a stream}
      procedure Store(var S : IdStream);
        {-Store a list in a stream}
    {$ENDIF}
      {++++ for internal use ++++}
      {.Z+}
      procedure StoreData(H : Word; var S : string; var D); virtual;
      procedure FreeData(H : Word); virtual;
      {.Z-}
    end;

  {--------- packed string array ---------}

const
  paMaxIndexSize     = 65520 div SizeOf(Word);
  paGrowIndex : Word = 100;       {Number of elements to expand index by}
  paGrowInfo  : Word = 1000;      {Number of bytes to grow string storage area by}

type
  paIndexArrayPtr = ^paIndexArray;
  paIndexArray = array[1..paMaxIndexSize] of Word;

  StringArrayPtr = ^StringArray;
  StringArray =
    object(Root)
      paIndex : paIndexArrayPtr;  {Pointer to index area}
      paInfo  : ByteArrayPtr;     {Pointer to string storage area}
      paCount : Word;             {Number of strings in index}
      paISize : Word;             {Allocated index size}
      paUsed  : Word;             {Total size of used strings}
      paAmount: Word;             {Allocated info size}

      constructor Init(StrMax, Amount : Word);
        {-Allocate space for StrMax strings in Amount space}
      constructor ReadText(FileName : PathStr);
        {-Initialize a packed array from a text file}
      destructor Done; virtual;
        {-Deallocate packed array}
      procedure WriteText(FileName : PathStr);
        {-Write a packed array to a text file}
      procedure Clear; {!!.02}
        {-Remove all strings from array} {!!.02}

    {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a binary packed array from a stream}
      procedure Store(var S : IdStream);
        {-Write a packed array to a stream}
    {$ENDIF}

      function GetString(Which : Word) : String; virtual;
        {-Return string from packed array}
      function GetStringPtr(Which : Word) : StringPtr;
        {-Return pointer to string from packed array}

      function Shrink : Boolean;
        {-Remove excess packed array space}
      function AddString(St : String) : Word; virtual; {!!.10}
        {-Add a new string, returning its index, or 0 if error}
      procedure RemoveString(Which : Word); {!!.03} virtual; {!!.10}
        {-Remove specified string from array and pack character table}
      function NumStrings : Word;
        {-Return number of strings in the packed array}
      function Size : Word;
        {-Return the number of bytes used by strings}

      {++++ for internal use ++++}
      {.Z+}
      procedure paZeroOut;
      function ExpandIndex(Delta : Integer) : Boolean;
      function ExpandInfo(Delta : LongInt) : Boolean;
      {.Z-}
    end;

  {--------- memory allocation ---------}

{.Z+}
function GetMemCheck(var P; Bytes : Word) : Boolean;
  {-Allocate heap space, returning true if successful}

procedure FreeMemCheck(var P; Bytes : Word);
  {-Deallocate heap space}

  {--------- error handling ---------}

procedure Abstract;
  {-Call to generate runtime error if abstract method hasn't been overridden}
{.Z-}

{$IFDEF UseStreams}

  {--------- stream registration ----------}

procedure PointerStackStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing PointerStacks}

procedure StaticQueueStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing StaticQueues}

procedure SingleListStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing SingleLists}

procedure DoubleListStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing DoubleLists}

procedure CircularListStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing CircularLists}

procedure BitSetStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing BitSets}

procedure LargeBitSetStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing LargeBitSets}

procedure StringDictStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing StringDicts}

procedure StringSetStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing StringSets}

procedure StringArrayStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing StringArrays}

{$ENDIF}

type
  ErrorProc = procedure (UnitCode : Byte; var ErrorCode : Word;
                         ErrorMsg : string);

  {-------- status variable used by constructors ---------}
const
  InitStatus : Word = 0;

  {======================================================================}

implementation

  {$I OPROOT.IN2}            {Stacks, lists, sets, heap management}
  {$I OPROOT.IN3}            {Streams, string dictionaries}
{$IFDEF UseStreams}
  {$I OPROOT.IN4}            {Libraries}
{$ENDIF}

{$IFDEF Debug}
var
  sep : pointer;

  procedure OurExitProc;
    {-}
  var
    I : Word;
  begin
    ExitProc := sep;
    if fmreturn <> nil then begin
      Write('fmReturn = ', HexPtr(fmReturn));
      ReadLn;
    end;
  end;
{$ENDIF}

begin
  HeapError := @OpConst.HeapFunc; {!!.21}
  {$IFDEF Debug}
  sep := ExitProc;
  ExitProc := @OurExitProc;
  {$ENDIF}
end.
