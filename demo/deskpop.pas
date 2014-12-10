{$IFDEF Windows}
  !! This program is not compatible with Windows !!
{$ENDIF}

{$IFDEF Dpmi}
  !! This program is not compatible with protected mode !!
{$ENDIF}

{$S-,R-,V-,I-,B-}
{$IFDEF VIRTUALPASCAL}
  {$DEFINE NOPOP}
{$ELSE}
  {$M 8192,98000,655360}
  {$DEFINE NoPop}            {If the following directive is defined, DESKPOP
                             will be compiled as a non-resident program. The
                             state of this define MUST agree with the
                             identically named define in DESKMAIN.PAS}
{$ENDIF}

{*********************************************************}
{*                  DESKPOP.PAS 1.30                     *}
{*    An example program for Object Professional 1.0     *}
{*    Copyright (c) TurboPower Software 1989, 1992.      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program DeskPop;
  {-Super-duper swapping TSR written with Object Professional}

  { Note: Using Virtual Pascal for OS/2, DeskPop *MUST* be linked }
  { without ExePack in order to work.  Configuration data that is }
  { not compressible is stored in the executable itself. }

uses
  deskmain
{$IFNDEF VIRTUALPASCAL}
  {$IFNDEF NoPop}       {!!.01}
  , opswap              {!!.01}
  {$ENDIF}              {!!.01}
{$ENDIF}
  ;

begin
  {initialize and try to go resident}
  InitializePopup;
end.
