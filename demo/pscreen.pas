{$IFDEF Windows}  {!!.20}
  !! This program is not compatible with Windows !!
{$ENDIF}

{$IFDEF Dpmi}     {!!.20}
  !! This program is not compatible with Protected Mode !!
{$ENDIF}

{$IFDEF VIRTUALPASCAL}
  !! This program is not compatible with Virtual Pascal for OS/2 !!
{$ENDIF}


{$S-,R-,V-,I-,B-,F-}
{$IFNDEF VIRTUALPASCAL}
{$M 4096,0,20000}
{$ENDIF}

{*********************************************************}
{*                  PSCREEN.PAS 1.30                     *}
{*    An example program for Object Professional 1.0     *}
{*     Copyright (c) TurboPower Software 1987,1992.      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program PackedScreenUtility;
  {-Utility for saving and displaying packed windows}

uses
  PSMain, OpSwap;

begin
  InitializePScreen;
end.
