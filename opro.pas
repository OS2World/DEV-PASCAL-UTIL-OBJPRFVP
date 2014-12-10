Library Opro;

// This file is used to build a DLL containing all of the OPro units
// Virtual Pascal for OS/2 only

// To use the DLL in programs, insert
//  {$Dynamic Opro.Lib}
// after the uses clause of the program and recompile (Ver 1.1 and later).

// Add description string to DLL
{$D Object Professional v1.30 - OS/2 DLL version}

{$S-,Delphi+,R-}
{$IFNDEF VER10}
  // Version 1.1 and later supports Location Information, allowing the RTL
  // to display correct line number and module information when run-time
  // errors occur in a program - even in a DLL.  To enable this in the DLL,
  // all units must be compiled in the $D+,LocInfo+ state.
  // The overhead of this is about 40kB for all of OPro.
  {$,D+,LocInfo+}
{$ENDIF}

Uses
  OpAbsFld, OpAsciiz, OpAbsWin, OpBcd, OpBrowse, OpCal, OpCalc, OpConst,
  OpCmd, OpCrt, OpCtrl, OpDate, OpDialog, OpDir, OpDos, OpDrag, OpEdit,
  OpEditor, OpEntry, OpFEdit, OpField, OpFrame, OpHelp, OpInline, OplArray,
  OpMemo, OpMenu, OpMouse, OpPick, OpQkRef, OpRoot, OpSEdit, OpSelect,
  OpSort, OpStrDev, OpString, OpWindow, OpClone;

{$Export OpAbsFld,OpAbsWin,OpAsciiz,OpBcd,OpBrowse,OpCal,OpCalc,OpConst,OpCmd}
{$Export OpCrt,OpCtrl,OpDate,OpDialog,OpDir,OpDos,OpDrag,OpEdit,OpEditor}
{$Export OpEntry,OpFEdit,OpField,OpFrame,OpHelp,OpInline,OpMemo,OpMenu}
{$Export OpMouse,OpPick,OpQkRef,OpRoot,OpSEdit,OpSelect,OpSort,OpStrDev}
{$Export OpString,OpWindow,OplArray,OpClone}

{$Export:NoName}

begin
end.


