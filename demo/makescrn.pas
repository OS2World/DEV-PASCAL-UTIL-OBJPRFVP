{$S-,R-,V-,I-,B-,F-}

{$IFNDEF VIRTUALPASCAL}
{$M 8192,32767,655360}
{$ENDIF}

{*********************************************************}
{*                  MAKESCRN.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

program MakeScreen;
  {-Data entry screen designer for Object Professional 1.0}

uses
  msmain; {<- actual MAKESCRN code is in here}

{$IFDEF DYNAMIC_VERSION}
  {$Dynamic System, Dos, Strings}
  {$Dynamic OpAbsFld,OpAbsWin,OpBcd,OpConst,OpCmd}
  {$Dynamic OpCrt,OpDate,OpDir,OpDos,OpEdit}
  {$Dynamic OpEntry,OpFEdit,OpField,OpFrame,OpInline,OpMenu}
  {$Dynamic OpMouse,OpPick,OpRoot,OpSelect}
  {$Dynamic OpString,OpWindow}
  {$L VPRTL.LIB}
  {$L OPRO.LIB}
{$ENDIF}


begin
  Main;
end.
