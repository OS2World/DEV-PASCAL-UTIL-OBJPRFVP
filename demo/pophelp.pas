{$IFDEF Windows}
  !! This program is not compatible with Windows !!
{$ENDIF}

{$IFNDEF VIRTUALPASCAL}
{$M 4000,0,655360}
{$V-,S-,R-,E-,N-,I-,F+}
{$ENDIF}

{*********************************************************}
{*                  POPHELP.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

{.$DEFINE NoPop}  {!!.03}   {If the following directive is defined, POPHELP
                             will be compiled as a non-resident program. The
                             state of this define MUST agree with the
                             identically named define in PHMAIN.PAS}

{$IFDEF Dpmi}     {!!.20}   {Force non-resident status if in protected mode}
  {$DEFINE NoPop}
{$ENDIF}

{$IFNDEF NoPop}   {!!.03}
  {$DEFINE EnableCutPaste}  {Define to enable block cut/paste from help to
                             an underlying editor.  Increases resident kernel
                             by approximately 2K bytes.  The state of this
                             define MUST agree with the identically named
                             define in PHMAIN.PAS}
{$ENDIF}          {!!.03}

program PopHelp;

uses
  Dos,
  PHMain
  {$IFNDEF NoPop}       {!!.03}
  , OpSwap              {!!.03}
  {$ENDIF}              {!!.03}
  {$IFDEF EnableCutPaste}
  ,OpReplay
  {$ENDIF}
  ;

{$IFDEF EnableCutPaste}
  {$L PHTIMER.OBJ}
  procedure WaitInit(OneMsec, DelayCnt : Word; UserDataPtr : Pointer); external;
    {-Sets Wait values}
  procedure GetMoreBytes; external;
    {-Waits for DelayCnt msec or BufferReady}

  procedure MacrosOff;
    {-Turn macro processing off}
  begin
    SetIntVec($16, OrigInt16Addr);
  end;

  procedure StartChaining;
  begin
    SetEndOfMacroProc(@GetMoreBytes);
  end;

  procedure StopChaining;
  begin
    SetEndOfMacroProc(Nil);
  end;

  procedure StartMacroPlain(P : Pointer);
    {-Stub for starting the macro pointed to by P}
  begin
    StartMacro(MacroRecPtr(P));
  end;
{$ENDIF}

begin
 {$IFDEF EnableCutPaste}
  {Grap Int16 for the macro engine (must be after OpSwap's)}
  GetIntVec($16, OrigInt16Addr);
  SetIntVec($16, @OpReplay.Int16);

  {Initialize the hooks into OpReplay}
  CallStartMacro := StartMacroPlain;
  CallStartChaining := StartChaining;
  CallStopChaining := StopChaining;
  CallWaitInit := WaitInit;
  CallCharToMacro := CharToMacro;
 {$ENDIF}

  {Go load the popup}
  LoadPopHelp;
end.
