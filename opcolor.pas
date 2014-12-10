{$S-,R-,V-,I-,B-,F-,O-,A-}

{$I OPDEFINE.INC}

{*********************************************************}
{*                   OPCOLOR.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpColor;
  {-Color definitions. Thanks to Bill Madison of W.G. Madison and Associates
    for the original suggestion for this unit.}

interface

const
  {Black background}
  BlackOnBlack         = $00;
  BlueOnBlack          = $01;
  GreenOnBlack         = $02;
  CyanOnBlack          = $03;
  RedOnBlack           = $04;
  MagentaOnBlack       = $05;
  BrownOnBlack         = $06;
  LtGrayOnBlack        = $07;
  DkGrayOnBlack        = $08;
  LtBlueOnBlack        = $09;
  LtGreenOnBlack       = $0A;
  LtCyanOnBlack        = $0B;
  LtRedOnBlack         = $0C;
  LtMagentaOnBlack     = $0D;
  YellowOnBlack        = $0E;
  WhiteOnBlack         = $0F;

  {Blue background}
  BlackOnBlue          = $10;
  BlueOnBlue           = $11;
  GreenOnBlue          = $12;
  CyanOnBlue           = $13;
  RedOnBlue            = $14;
  MagentaOnBlue        = $15;
  BrownOnBlue          = $16;
  LtGrayOnBlue         = $17;
  DkGrayOnBlue         = $18;
  LtBlueOnBlue         = $19;
  LtGreenOnBlue        = $1A;
  LtCyanOnBlue         = $1B;
  LtRedOnBlue          = $1C;
  LtMagentaOnBlue      = $1D;
  YellowOnBlue         = $1E;
  WhiteOnBlue          = $1F;

  {Green background}
  BlackOnGreen         = $20;
  BlueOnGreen          = $21;
  GreenOnGreen         = $22;
  CyanOnGreen          = $23;
  RedOnGreen           = $24;
  MagentaOnGreen       = $25;
  BrownOnGreen         = $26;
  LtGrayOnGreen        = $27;
  DkGrayOnGreen        = $28;
  LtBlueOnGreen        = $29;
  LtGreenOnGreen       = $2A;
  LtCyanOnGreen        = $2B;
  LtRedOnGreen         = $2C;
  LtMagentaOnGreen     = $2D;
  YellowOnGreen        = $2E;
  WhiteOnGreen         = $2F;

  {Cyan background}
  BlackOnCyan          = $30;
  BlueOnCyan           = $31;
  GreenOnCyan          = $32;
  CyanOnCyan           = $33;
  RedOnCyan            = $34;
  MagentaOnCyan        = $35;
  BrownOnCyan          = $36;
  LtGrayOnCyan         = $37;
  DkGrayOnCyan         = $38;
  LtBlueOnCyan         = $39;
  LtGreenOnCyan        = $3A;
  LtCyanOnCyan         = $3B;
  LtRedOnCyan          = $3C;
  LtMagentaOnCyan      = $3D;
  YellowOnCyan         = $3E;
  WhiteOnCyan          = $3F;

  {Red background}
  BlackOnRed           = $40;
  BlueOnRed            = $41;
  GreenOnRed           = $42;
  CyanOnRed            = $43;
  RedOnRed             = $44;
  MagentaOnRed         = $45;
  BrownOnRed           = $46;
  LtGrayOnRed          = $47;
  DkGrayOnRed          = $48;
  LtBlueOnRed          = $49;
  LtGreenOnRed         = $4A;
  LtCyanOnRed          = $4B;
  LtRedOnRed           = $4C;
  LtMagentaOnRed       = $4D;
  YellowOnRed          = $4E;
  WhiteOnRed           = $4F;

  {Magenta background}
  BlackOnMagenta       = $50;
  BlueOnMagenta        = $51;
  GreenOnMagenta       = $52;
  CyanOnMagenta        = $53;
  RedOnMagenta         = $54;
  MagentaOnMagenta     = $55;
  BrownOnMagenta       = $56;
  LtGrayOnMagenta      = $57;
  DkGrayOnMagenta      = $58;
  LtBlueOnMagenta      = $59;
  LtGreenOnMagenta     = $5A;
  LtCyanOnMagenta      = $5B;
  LtRedOnMagenta       = $5C;
  LtMagentaOnMagenta   = $5D;
  YellowOnMagenta      = $5E;
  WhiteOnMagenta       = $5F;

  {Brown background}
  BlackOnBrown         = $60;
  BlueOnBrown          = $61;
  GreenOnBrown         = $62;
  CyanOnBrown          = $63;
  RedOnBrown           = $64;
  MagentaOnBrown       = $65;
  BrownOnBrown         = $66;
  LtGrayOnBrown        = $67;
  DkGrayOnBrown        = $68;
  LtBlueOnBrown        = $69;
  LtGreenOnBrown       = $6A;
  LtCyanOnBrown        = $6B;
  LtRedOnBrown         = $6C;
  LtMagentaOnBrown     = $6D;
  YellowOnBrown        = $6E;
  WhiteOnBrown         = $6F;

  {Light gray background}
  BlackOnLtGray        = $70;
  BlueOnLtGray         = $71;
  GreenOnLtGray        = $72;
  CyanOnLtGray         = $73;
  RedOnLtGray          = $74;
  MagentaOnLtGray      = $75;
  BrownOnLtGray        = $76;
  LtGrayOnLtGray       = $77;
  DkGrayOnLtGray       = $78;
  LtBlueOnLtGray       = $79;
  LtGreenOnLtGray      = $7A;
  LtCyanOnLtGray       = $7B;
  LtRedOnLtGray        = $7C;
  LtMagentaOnLtGray    = $7D;
  YellowOnLtGray       = $7E;
  WhiteOnLtGray        = $7F;

  {==========================================================================}

implementation

{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.
