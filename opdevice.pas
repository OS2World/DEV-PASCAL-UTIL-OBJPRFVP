
{$IFDEF Windows}                                                      {!!.20}
  !! ERROR: This unit is not compatible with Windows applications !!  {!!.20}
{$ENDIF}                                                              {!!.20}

{$IFDEF VIRTUALPASCAL}
  !! ERROR: This unit is not compatible with Virtual Pascal !!
{$ENDIF}

{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}

{*********************************************************}
{*                  OPDEVICE.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1989, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{
  This unit contains code that is specific to various printer devices.
}
Unit OpDevice;

interface

uses
  Dos,
  OpConst,    {!!.20}
  OpInline,
  OpString,
  OpRoot,
  OpDos,
  OpPrnLow,
  OpPrint;

const
  {ID numbers for assorted known printers}
  pLaserjet           = 1;
  pDeskJet            = 2;
  pLaserjetIII        = 3;               {!!.20}
  pEpsonMX            = 4;
  pEpsonFX            = 5;
  pEpsonLQ            = 6;
  pGenericEpson       = 7;

  pDiablo             = 10;
  pProprinterII       = 11;
  pKXP1093            = 12;
  pToshiba321         = 13;
  pStarMicronics      = 14;
  pNec3510            = 15;
  pNec8800            = 16;
  pProwriter          = 17;
  pFacit              = 18;
  pDataProducts       = 19;
  pQuickWriter5204    = 20;                                   {!!.21}
  pBrotherM1709EpsonMode = 21;                                {!!.21}
  pBrotherM1709IBMmode= 22;                                   {!!.21}
  pPanasonicKXP2124   = 23;                                   {!!.22}


{hardware specific constants and routines}
const
  HalfInch         = 0.5;
  LJVC             = #27'&a#R';
  LJHC             = #27'&a#C';
  LJVD             = #27'&a#V';
  LJHD             = #27'&a#H';
  LJLineOn         = #27'(0B';
  LJLandLineOn     = #27'&l0O';
  LJDefaultFont    = #27'&l0O'#27'(0U'#27'(s0p10h12v0s0b3T'; {Courier}
  LJHRuleDot       = #27'*c#A';
  LJHRuleDeci      = #27'*c#H';
  LJVRuleDot       = #27'*c#B';
  LJVRuleDeci      = #27'*c#V';
  LJRulePattern    = #27'*c#G';
  LJPrintRule      = #27'*c0P';
  LJPrintPattern   = #27'*c2P';
  LJHPPattern      = #27'*c3P';

  LJDefLPI         = 6.0;
  LJDefCPI         = 10.0;
  LJCompressedCPI  = 16.66;
  LJDefPoint       = 12.0;
  LJDefLPP         = 60.0;
  LJDeciUnit       = 1.0 / 720.0;
  LJDefTopMargin   = HalfInch;
  LJPointUnit      = 1.0 / 72.0;
  LJMaskChar       : Char = '#';

  {Predefined PrintMode ID codes}

                      {PrintMode IDs for IBM QuickWriter (5204)}     {!!.21}
                      {and Brother M-1709 printers}                  {!!.21}
  SuperScript      = 241;                                            {!!.21}
  SubScript        = 240;                                            {!!.21}
  DoubleHighWide   = 239;                                            {!!.21}
  BeepPrinter      = 238;                                            {!!.21}
  SetTopOfForm     = 237;                                            {!!.21}
  ChangePageLen    = 236;                                            {!!.21}
  OverLine         = 235;                                            {!!.21}
                      {PrintMode IDs for  LJCustomRegister}
  LJLineDraw       = 231;
  LinePrinterCompressed
                   = 230;
  Courier          = 229;

  CourierBold      = 228;
  LandscapeCourier = 227;
  HelvBold         = 226;
  Prestige16_66Landscape
                   = 225;

  LJFeedFromTray   = 223;
  LJManualFeed     = 222;
  LJEnvelopeFeed   = 221;

  LJIIIUpperBin    = 220;                        {!!.13}
  LJIIILowerBin    = 219;                        {!!.13}
  LJIIIUpperTray   = 218;                        {!!.13}
  LJIIILowerTray   = 217;                        {!!.13}
  LJIIIManualFeedPaper
                   = 216;                        {!!.13}
  LJIIIManualFeedEnv
                   = 215;                        {!!.13}
  LJIIIEnvelopeFeeder
                   = 214;                        {!!.13}
                      {Printmode IDs for Panasonic KX-P2124}
  SuperLtrQuality  = 213;                                            {!!.22}
  Pica10cpi        = 212;                                            {!!.22}
  Elite12cpi       = 211;                                            {!!.22}
  Micron15cpi      = 210;                                            {!!.22}
  clrRed           = 209;                                            {!!.22}
  clrBlue          = 208;                                            {!!.22}
  clrViolet        = 207;                                            {!!.22}
  clrYellow        = 206;                                            {!!.22}
  clrOrange        = 205;                                            {!!.22}
  clrGreen         = 204;                                            {!!.22}

const
  {LJ Symbol Sets}
  ljssUSAscii      = '0U';   {normal US Ascii}
  ljssIBMUS        = '10U';  {special Ascii + IBM extended (avail. on LJII)}
  ljssUSLegal      = '1U';   {US Legal}
  ljssRoman        = '0E';   {Roman extensions}

  SymbolSet        : String[4] = ljssUSAscii;

procedure Deregister(P : PrinterPtr);
  {-Deregisters a Printer of any previous registration}

{.Z+}
function LJBuildEscapeSeq(Dimen : Dimension;
                          Mask : String;
                          MaskChar : Char;
                          rtType : RelativeType) : String;
  {-Build a LJ escape sequence by replacing MaskChar with appropriate
    string}

function LaserJetPosition(Row, Col : Dimension;
                          Relative : RelativeType;
                          P : PrinterPtr) : Word;
  {-The pPositionPrim function for HP Laserjets}

{.Z-}

procedure LJRegister(P : PrinterPtr);
  {-Register this printer as an HP Laserjet compatible}

procedure LJPlusRegister(P : PrinterPtr);
  {-Registers the printer as an HP Laserjet Plus}

procedure LJIIRegister(P : PrinterPtr);
  {-Registers the printer as an HP Laserjet II}

procedure LJIIIsiRegister(P : PrinterPtr);             {!!.13}
  {-Registers a printer as an HP Laserjet IIIsi.}

procedure LJFontGRegister(P : PrinterPtr);
  {-Registers cartridge font G for LJ}

procedure LJFontFRegister(P : PrinterPtr);
  {-Registers cartridge font F for LJ}

function LJDeci2Row(Decipoints : Dimension; P : PrinterPtr) : Dimension;
  {-Convert decipoint to Row number}

function LJDeci2Col(Decipoints : Dimension; P : PrinterPtr) : Dimension;
  {-Convert decipoint to Col number}

function LJRow2Deci(Row : Dimension; P : PrinterPtr) : Dimension;
  {-Convert a Row number to a Decipoint}

function LJCol2Deci(Col : Dimension; P : PrinterPtr) : Dimension;
  {-Convert a Row number to a Decipoint}

procedure DiabloRegister(P : PrinterPtr);
  {-Register this printer as a Diablo compatible}

procedure EpsonMXRegister(P : PrinterPtr);
  {-Registers P as an Epson MX compatible printer}

procedure EpsonFXRegister(P : PrinterPtr);
  {-Registers P as an Epson FX printer}

procedure EpsonLQRegister(P : PrinterPtr);
  {-Registers P as an Epson LQ printer}

procedure ProPrinterIIRegister(P : PrinterPtr);
  {-Registers P as an IBM Proprinter II printer}

procedure GenericEpsonRegister(P : PrinterPtr);
  {-Registers P as a Generic Epson compatible printer}

procedure HPDeskJetRegister(P : PrinterPtr);
  {-Registers P as an HP Deskjet compatible}

procedure KXP1093Register(P : PrinterPtr);
  {-Registers P as a Panasonic KX-P1093 printer}

procedure ToshibaP321Register(P : PrinterPtr);
  {-Registers P as Toshiba P321SL or P341SL}

procedure StarMicronicsRegister(P : PrinterPtr);
  {-Registers P as a Star Micronics printer}

procedure Nec3510Register(P : PrinterPtr);
  {-Registers P as a Nec 3510/3530/7710/7730/8830/8810 printer}

procedure Nec8800Register(P : PrinterPtr);
  {-Registers P as a Nec 3510/3530/7710/7730/8830/8810 printer}

procedure ProwriterRegister(P : PrinterPtr);
  {-Registers P as a C. Itoh 8510 Prowriter printer}

procedure FacitRegister(P : PrinterPtr);
  {-Registers P as a Facit 4512 printer}

procedure DataProductsRegister(P : PrinterPtr);
  {-Registers P as a DataProducts DP printer}

{$IFDEF UseStreams}

procedure LJDeviceStream(SPtr : IdStreamPtr);
  {-Registers the LJ specific procedure and function pointers}

{$ENDIF}

procedure QuickWriter5204Register(P : PrinterPtr);                   {!!.21}
  {-Registers P as an IBM QuickWriter (5204) printer}                {!!.21}

procedure BrotherM1709IBMmodeRegister(P : PrinterPtr);               {!!.21}
  {-Registers P as a Brother M-1709 printer in IBM Mode}             {!!.21}

procedure BrotherM1709EpsonModeRegister(P : PrinterPtr);             {!!.21}
  {-Registers P as a Brother M-1709 printer in Epson Mode}           {!!.21}

procedure PanasonicKXP2124Register(P : PrinterPtr);                  {!!.22}
  {-Registers P as a Panasonic KX-P2124 printer}                     {!!.22}


  {==========================================================================}

implementation

{...hardware specific routines...}

{...HP Laserjet (and LJ+) specific routines...}

  function LJDeci2Row(Decipoints : Dimension; P : PrinterPtr) : Dimension;
    {-Convert decipoint to Row number}

  begin
    LJDeci2Row  := (DeciPoints * LJDeciUnit) * P^.GetLPI;
  end;

  function LJDeci2Col(Decipoints : Dimension; P : PrinterPtr) : Dimension;
    {-Convert decipoint to Col number}

  begin
    LJDeci2Col := (DeciPoints * LJDeciUnit) * P^.GetCPI;
  end;

  function LJRow2Deci(Row : Dimension; P : PrinterPtr) : Dimension;
    {-Convert a Row number to a Decipoint}
  begin
    LJRow2Deci := (Row / P^.GetLPI) * 720.0;
  end;

  function LJCol2Deci(Col : Dimension; P : PrinterPtr) : Dimension;
    {-Convert a Row number to a Decipoint}

  begin
    LJCol2Deci := (Col / P^.GetCPI) * 720.0;
  end;

  function LJBuildEscapeSeq(Dimen : Dimension;
                               Mask : String;
                               MaskChar : Char;
                               rtType : RelativeType) : String;
    {-Build a LJ escape sequence by replacing MaskChar with appropriate
      string}
  var
    S : String;
    I : Byte;

  begin
    S := '';
    I := Pos(MaskChar,Mask);
    if I > 0 then begin
      Str(Dimen:8:0,S);
      S := Trim(S);
      if (rtType = rtCurrent) then
        if (Dimen > 0.0) then
          S := '+' + S
        else
          if S[1] <> '-' then
            S := '-' + S;
      Delete(Mask,I,1);
      Insert(S,Mask,I);
      S := Mask;
    end;
    LJBuildEscapeSeq := S;
  end;

  function LaserJetPosition(Row, Col : Dimension;
                            Relative : RelativeType;
                            P : PrinterPtr) : Word;
    {-The pPositionPrim function for HP Laserjets}
  var
    S : String;

  begin
    LaserJetPosition := 0;
    with P^ do begin                                     {!!.03}
      if Relative = rtAbsolute then begin                {!!.13}
        pLastY := Row;                                   {!!.03}
        pLastX := Col;
      end                                                {!!.13}
      else begin                                         {!!.13}
        pLastY := pLastY + Row;                          {!!.13}
        pLastX := pLastX + Col;                          {!!.13}
      end;                                               {!!.13}
      if FlagIsSet(pOptions, pUseDeciPoints) then begin
        {positioning via Decipoints (1/720")}
        S := LJBuildEscapeSeq(Col,pDeciHoriz^,LJMaskChar,Relative);
        PrintStrPrim(S);
        if pLastError <> 0 then
          Exit;
        S := LJBuildEscapeSeq(Row,pDeciVert^,LJMaskChar,Relative);
        PrintStrPrim(S);
      end
      else begin
        {positioning via Row/Col}
        if FlagIsSet(pOptions, pLJColAdjust) then        {!!.13}
          Col := Col - PosUnit;                          {!!.13}
        S := LJBuildEscapeSeq(Col,pHorizCursor^,LJMaskChar,Relative);
        PrintStrPrim(S);
        if pLastError <> 0 then
          Exit;
        S := LJBuildEscapeSeq(Row,pVertCursor^,LJMaskChar,Relative);
        PrintStrPrim(S);
      end;
    end;                                                  {!!.03}
  end;

  procedure LJGraphicsProc(On : Boolean; P : PrinterPtr);
    {-Graphics not currently supported}
  begin
  end;

  procedure LJRegister(P : PrinterPtr);
    {-Registers the printer as an HP Laserjet}
  var                                                      {!!.13}
    DefFont : String;                                      {!!.13}
    Posit : Byte;                                          {!!.13}
  begin
    with P^ do begin
      P^.SetPosData(LJVC,LJHC,LJVD,LJHD);
      SetPrnOptions(GetPrnOptions or (pPositionable or pSupportsHPRules));
      SetPositionPrim(LaserjetPosition);
      (* graphics not currently implemented
      SetGraphicsProc(LJGraphicsProc);
      *)
      SetPrinterInfo(LJDefLPI,LJDefCPI,LJDefPoint,LJDefLPP);
      SetPhysicalTopMargin(LJDefTopMargin);
      AddPrintMode('Underline',#27'&dD',#27'&d@',0,0,Underline,pmFont);
      DefFont := LJDefaultFont;                            {!!.13}
      Posit := Pos(ljssUSAscii, DefFont);                  {!!.13}
      if Posit > 0 then begin                              {!!.13}
        Delete(DefFont, Posit, Length(ljssUSAscii));       {!!.13}
        Insert(SymbolSet, DefFont, Posit);                 {!!.13}
      end;                                                 {!!.13}
      AddPrintMode('DefaultFont',DefFont,'',LJDefCPI,LJDefLPI,     {!!.13}
                   DefaultFont,NewFont);
      AddPrintMode('FeedFromTray', #27#38#108#49#72, '', 0, 0,
                   LJFeedFromTray, 0);
      AddPrintMode('ManualFeed',#27#38#108#50#72, '', 0, 0,
                   LJManualFeed, 0);
      AddPrintMode('EnvelopeFeed',#27#38#108#51#72, '', 0, 0,
                   LJEnvelopeFeed, 0);
      AddPrintMode('Courier', #27'&l0O'#27'(8U'#27'(s0p10h12v0s0b3T',
                   '', 10.0, 12.0, Courier, NewFont);               {!!.03}
      AddPrintMode('LandscapeCourier',
                   #27'&l1O'#27'(8U'#27'(s0p10h12v0s0b3T', '', 10.0, 12.0,
                   LandscapeCourier, NewFont+pmChangesLandscape);

      SetDefaultFont(DefaultFont);
      EnableDefaultFont;

      AddPrintMode('PrinterReset',#27'E','',0,0,PrinterReset,0);
      SetRegisteredType(pLaserjet);
    end;
  end;

  procedure LJPlusRegister(P : PrinterPtr);
    {-Registers the printer as an HP Laserjet Plus}

  begin
    LJRegister(P);
    with P^ do begin
      AddPrintMode('LinePrinterCompressed',
                   #27'&l0O'#27'('+SymbolSet+#27'(s0p16.66h8.5v0s0b0T', '',
                   16.66, 8.5, LinePrinterCompressed,
                   NewFont + pmChangesLPI);              {!!.13}
    end;
  end;

  procedure LJIIRegister(P : PrinterPtr);
    {-Registers the printer as an HP Laserjet II}

  begin
    LJPlusRegister(P);
    with P^ do begin
      AddPrintMode('Bold', #27'(s3B', #27'(s0B', 0, 0, Bold, NewFont);  {!!.22}
      AddPrintMode('CourierBold', #27'&l0O'#27'(8U'#27'(s0p10h12v0s3b3T',
                   '', 10.0, 12.0, CourierBold, NewFont);
    end;
  end;

  procedure LJIIIsiRegister(P : PrinterPtr);
    {-Registers a printer as an HP Laserjet IIIsi.}
  begin
    LJRegister(P);
    with P^ do begin
      AddPrintMode('UpperBin', #27'&l1G', '', 0, 0,
                   LJIIIUpperBin, 0);
      AddPrintMode('LowerBin', #27'&l2G', '', 0, 0,
                   LJIIILowerBin, 0);
      AddPrintMode('UpperTray', #27'&l1G', '', 0, 0,
                   LJIIIUpperTray, 0);
      AddPrintMode('ManualFeedPaper', #27'&l2G', '', 0, 0,
                   LJIIIManualFeedPaper, 0);
      AddPrintMode('ManualFeedEnv', #27'&l3G', '', 0, 0,
                   LJIIIManualFeedEnv, 0);
      AddPrintMode('LowerTray', #27'&l4G', '', 0, 0,
                   LJIIILowerTray, 0);
      AddPrintMode('EnvelopeFeeded', #27'&l6G', '', 0, 0,
                   LJIIIEnvelopeFeeder, 0);
    end;
  end;

  procedure LJFontGRegister(P : PrinterPtr);
    {-Registers cartridge font G for LJ}

  begin
    with P^ do begin
      AddPrintMode('PrestigeElite',
                   #27'&l0O'#27'('+SymbolSet+#27'(s0p12h10v0s0b8T', '',
                   12.0, 10.0, Normal, NewFont);
      AddPrintMode('PrestigeBold',
                   #27'&l0O'#27'('+SymbolSet+#27'(s0p12h10v0s3b8T', '',
                   12.0, 10.0, Bold, NewFont);
      AddPrintMode('PrestigeItalic',
                   #27'&l0O'#27'('+SymbolSet+#27'(s0p12h10v1s0b8T', '',
                   12.0, 10.0, Italics, NewFont);
      AddPrintMode('Prestige16_66',
                   #27'&l0O'#27'('+SymbolSet+#27'(s0p16.66h7v0s0b8T', '',
                   16.66, 7.0, Compressed, NewFont);
      AddPrintMode('LJLineDraw',
                   #27'&l0O'#27'(0B'#27'(s0p12h12v0s0B', '',
                   12.0, 12.0, LJLineDraw, NewFont);
      AddPrintMode('Prestige16_66Landscape',
                   #27'&l1O'#27'('+SymbolSet+#27'(s0p16.66h7v0s0b8T', '',
                   16.66, 7.0, Prestige16_66Landscape, NewFont+pmChangesLandscape);
    end;
  end;

  procedure LJFontFRegister(P : PrinterPtr);
    {-Registers cartridge font F for LJ}
  const
    PropFont = NewFont + pmProportional;
  begin
    with P^ do begin
      AddPrintMode('TmsRmn',#27'&l0O'#27'('+SymbolSet+#27'(s1p10v0s0b5T','',
                   LJDefCPI,10.0,Normal,PropFont);
      AddPrintMode('TmsRmnBold',#27'&l0O'#27'('+SymbolSet+#27'(s1p10v0s3b5T','',
                   LJDefCPI,10.0,Bold,PropFont);
      AddPrintMode('TmsRmnItalics',#27'&l0O'#27'('+SymbolSet+#27'(s1p10v1s0b5T','',
                   LJDefCPI,10.0,Italics,PropFont);
      AddPrintMode('TmsRmn8',#27'&l0O'#27'('+SymbolSet+#27'(s1p8v0s0b5T', '',
                   LJDefCPI, 8.0, Compressed, PropFont);
      AddPrintMode('HelvBold',#27'&l0O'#27'('+SymbolSet+#27'(s1p14.4v0s3b5T', '',
                   LJDefCPI, 14.4, HelvBold, PropFont);
      {Look to see if LinePrinterCompressed mode is already defined, if not,
       add it.}
      if GetPrintModeByName('LinePrinterCompressed') = Nil then
        AddPrintMode('LinePrinterCompressed',
                     #27'&l0O'#27'('+SymbolSet+#27'(s0p16.66h8.5v0s0b0T', '',
                     16.66, 8.5, LinePrinterCompressed, NewFont);
    end;
  end;

  procedure StandardBoxReplace(P : PrinterPtr);

  begin
    with P^, GetBasePrinter^ do begin
      XlatCharItem('Ú','+');
      XlatCharItem('À','+');
      XlatCharItem('¿','+');
      XlatCharItem('Ù','+');
      XlatCharItem('Ä','-');
      XlatCharItem('³','|');

      XlatCharItem('É','+');
      XlatCharItem('È','+');
      XlatCharItem('»','+');
      XlatCharItem('¼','+');
      XlatCharItem('Í','-');
      XlatCharItem('º','|');
    end;
  end;

  procedure EpsonBoxReplace(P : PrinterPtr);

  begin
    with P^, GetBasePrinter^ do begin
      XlatCharItem('Ú',#134);
      XlatCharItem('À',#153);
      XlatCharItem('¿',#149);
      XlatCharItem('Ù',#154);
      XlatCharItem('Ä',#157);
      XlatCharItem('³',#156);

      XlatCharItem('É',#134);
      XlatCharItem('È',#153);
      XlatCharItem('»',#149);
      XlatCharItem('¼',#154);
      XlatCharItem('Í',#157);
      XlatCharItem('º',#156);
    end;
  end;


  {...HP Deskjet...}

  procedure HPDeskJetRegister(P : PrinterPtr);
    {-Registers P as an HP Deskjet compatible}
  var
    Opts : Word;
  begin
    with P^ do begin
      SetPosData(LJVC,LJHC,LJVD,LJHD);                     {!!.10 begin}
      SetPrnOptions(GetPrnOptions or (pPositionable or pSupportsHPRules));
      SetPositionPrim(LaserjetPosition);
      (* graphics not currently implemented
      SetGraphicsProc(LJGraphicsProc);
      *)
      SetPrinterInfo(LJDefLPI,LJDefCPI,LJDefPoint,LJDefLPP);
      SetPhysicalTopMargin(LJDefTopMargin);                {!!.10 end}
      AddPrintMode('Bold',#27#40#115#51#66,#27#40#115#48#66,0,0,Bold,0);
      AddPrintMode('Compressed',#27#40#115#49#54#46#54#72,
                                #27#40#115#49#48#72,0,0,Compressed,0);
      AddPrintMode('Underline',#27#38#100#52#68,#27#38#100#64,0,0,Underline,0);
      AddPrintMode('PrinterReset',#27#40#115#48#66,'',0,0,PrinterReset,0);
      SetRegisteredType(pDeskJet);
    end;
  end;

  {...Diablo...}

  procedure DiabloRegister(P : PrinterPtr);
    {-Registers P as a Diablo compatible}
  var
    Opts : Word;
  begin
    with P^ do begin
      SetPrnOptions(GetPrnOptions or pBackSpaceAllowed);
      AddPrintMode('DoubleStrike','','',0,0,DoubleStrike,0);
      AddPrintMode('Underline',#27#69,#27#82,0,0,Underline,0);
      StandardBoxReplace(P);
      SetRegisteredType(pDiablo);
    end;
  end;

  {...Epson...}

  procedure EpsonMXRegister(P : PrinterPtr);
    {-Registers P as an Epson MX compatible printer}
  var
    Opts : Word;
  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Italics',#27'4',#27'5',0,0,Italics,0);
      AddPrintMode('Bold',#27'E',#27'F',0,0,Bold,0);
      AddPrintMode('Compressed',#15,#18,0,0,Compressed,0);         {!!.11}
      AddPrintMode('Underline',#27'-'#1,#27'-'#0,0,0,Underline,0);
      AddPrintMode('Unidirectional',#27'U'#1,#27'U'#0,0,0,Unidirectional,0);
      AddPrintMode('DoubleWidth',#27'W'#1,#27'W'#0,0,0,DoubleWidth,0);
      AddPrintMode('PrinterReset',#27'@','',0,0,PrinterReset,0);
      EpsonBoxReplace(P);

      SetRegisteredType(pEpsonMX);
    end;
  end;

  procedure EpsonLQRegister(P : PrinterPtr);
    {-Registers P as an Epson LQ printer}
  var
    Opts : Word;
  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Italics',#27#52,#27#53,0,0,Italics,0);
      AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);         {!!.01}
      AddPrintMode('Compressed',#15,#18,0,0,Compressed,0);
      AddPrintMode('Underline',#27#45#1,#27#45#0,0,0,Underline,0);
      AddPrintMode('Unidirectional',#27'U'#1,#27'U'#0,0,0,Unidirectional,0);
      AddPrintMode('DoubleWidth',#27'W'#1,#27'W'#0,0,0,DoubleWidth,0);
      AddPrintMode('PrinterReset',#27#64,'',0,0,PrinterReset,0);
      AddPrintMode('Letter Quality',#27#120#1,#27#120#0,0,0,LetterQuality,0);

      (*EpsonBoxReplace(P);*)                  {!!.12}

      SetRegisteredType(pEpsonLQ);
    end;
  end;

  procedure EpsonFXRegister(P : PrinterPtr);
    {-Registers P as an Epson FX printer}

  begin
    EpsonMXRegister(P);
    P^.SetRegisteredType(pEpsonFX);                {!!.01}
  end;

  procedure GenericEpsonRegister(P : PrinterPtr);
  var
    Opts : Word;
  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Italics',#27#52,#27#53,0,0,Italics,0);
      AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);             {!!.11}
      AddPrintMode('Compressed',#15,#18,0,0,Compressed,0);
      AddPrintMode('Underline',#27#45#1,#27#45#0,0,0,Underline,0);
      AddPrintMode('PrinterReset',#27#64,'',0,0,PrinterReset,0);

      EpsonBoxReplace(P);

      SetRegisteredType(pGenericEpson);
    end;
  end;

  procedure ProPrinterIIRegister(P : PrinterPtr);
    {-Registers P as an IBM Proprinter II printer}
  var
    Opts : Word;
  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Italics',#27#52,#27#53,0,0,Italics,0);       {!!.03}
      AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);
      AddPrintMode('Compressed',#15,#18,0,0,Compressed,0);       {!!.13}
      AddPrintMode('Underline',#27#45#1,#27#45#0,0,0,Underline,0);
      AddPrintMode('PrinterReset',#27#64,'',0,0,PrinterReset,0);
      SetRegisteredType(pProprinterII);
    end;
  end;

  procedure KXP1093Register(P : PrinterPtr);
    {-Registers P as a Panasonic KX-P1093 printer}
  var
    Opts : Word;

  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);
      AddPrintMode('Underline',#27#45#49,#27#45#48,0,0,Underline,0);
      AddPrintMode('PrinterReset',#27#51#36#27#50,'',0,0,PrinterReset,0);
      StandardBoxReplace(P);
      SetRegisteredType(pKXP1093);
    end;
  end;

  procedure ToshibaP321Register(P : PrinterPtr);
    {-Registers P as Toshiba P321SL or P341SL}
  var
    Opts : Word;
  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Italics',#27#18,#27#20,0,0,Italics,0);
      AddPrintMode('Bold',#27#75#3,#27#77,0,0,Bold,0);
      AddPrintMode('Compressed',#27#91,#27#93,0,0,Compressed,0);
      AddPrintMode('Underline',#27#73,#27#74,0,0,Underline,0);
      AddPrintMode('PrinterReset',#27#13#80,'',0,0,PrinterReset,0);
      StandardBoxReplace(P);
      SetRegisteredType(pToshiba321);
    end;
  end;

  procedure StarMicronicsRegister(P : PrinterPtr);
    {-Registers P as a Star Micronics printer}
  var
    Opts : Word;

  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Italics',#28#4,#27#53,0,0,Italics,0);
      AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);
      AddPrintMode('Compressed',#27#15,#18,0,0,Compressed,0);
      AddPrintMode('PrinterReset',#27#64,'',0,0,PrinterReset,0);
      StandardBoxReplace(P);
      SetRegisteredType(pStarMicronics);
    end;
  end;

  procedure Nec3510Register(P : PrinterPtr);
    {-Registers P as a Nec 3510/3530/7710/7730/8830/8810 printer}
  var
    Opts : Word;

  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Bold',#27#42,#27#44,0,0,Bold,0);
      AddPrintMode('Underline',#27#45,#27#39,0,0,Underline,0);
      AddPrintMode('PrinterReset',#27#93#76#27#93#87,'',0,0,PrinterReset,0);
      StandardBoxReplace(P);
      SetRegisteredType(pNec3510);
    end;
  end;

  procedure Nec8800Register(P : PrinterPtr);
    {-Registers P as a Nec 3510/3530/7710/7730/8830/8810 printer}
  var
    Opts : Word;

  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Bold',#27#42,#27#44,0,0,Bold,0);
      AddPrintMode('Underline',#27#45,#27#39,0,0,Underline,0);
      AddPrintMode('PrinterReset',#27#92#70#27#93#87,'',0,0,PrinterReset,0);
      StandardBoxReplace(P);
      SetRegisteredType(pNec8800);
    end;
  end;

  procedure ProwriterRegister(P : PrinterPtr);
    {-Registers P as a C. Itoh 8510 Prowriter printer}
  var
    Opts : Word;

  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Bold',#27#33,#27#34,0,0,Bold,0);
      AddPrintMode('Underline',#27#88,#27#89,0,0,Underline,0);
      AddPrintMode('Compressed',#27#81,#18,0,0,Compressed,0);      {!!.03}
      AddPrintMode('PrinterReset',#27#78#27#65,'',0,0,PrinterReset,0);
      StandardBoxReplace(P);
      SetRegisteredType(pProwriter);
    end;
  end;

  procedure FacitRegister(P : PrinterPtr);
    {-Registers P as a Facit 4512 printer}
  var
    Opts : Word;

  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Bold',#28,#31,0,0,Bold,0);
      AddPrintMode('Underline',#29,#31,0,0,Underline,0);
      AddPrintMode('Compressed',#15,#18,0,0,Compressed,0);
      AddPrintMode('PrinterReset',#27#58#27#33#44#127,'',0,0,PrinterReset,0);
      StandardBoxReplace(P);
      SetRegisteredType(pFacit);
    end;
  end;

  procedure DataProductsRegister(P : PrinterPtr);
    {-Registers P as a DataProducts DP printer}

  var
    Opts : Word;

  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);
      AddPrintMode('Bold',#27#87,#27#38,0,0,Bold,0);
      AddPrintMode('PrinterReset',#27#47#27#31#13#27#30#9,'',
                   0,0,PrinterReset,0);
      StandardBoxReplace(P);
      SetRegisteredType(pDataProducts);
    end;
  end;

{$IFDEF UseStreams}

  procedure LJDeviceStream(SPtr : IdStreamPtr);
    {-Registers the LJ specific procedure and function pointers}
  begin
    with SPtr^ do begin
      RegisterPointer(ptLJPosition,@LaserjetPosition);
      (* graphics not currently implemented
      RegisterPointer(ptLJGraphics,@LJGraphicsProc);
      *)
    end;
  end;

{$ENDIF}

  procedure Deregister(P : PrinterPtr);
    {-Deregisters a Printer of any previous registration}
  begin
    with P^ do begin
      @pPositionPrim := Nil;                       {!!.03}
      @pGraphicsProc := Nil;                       {!!.03}
      @pSpecialProc  := Nil;                       {!!.03}
      pOptions := pOptions and $F000;              {!!.03}{!!.10}
      DisposeAllModes;
      pFontStack.Clear;
      pActiveModes.ClearAll;
      GetBasePrinter^.ClearXlatCharTable;
      SetRegisteredType(pPlain);
      DisposePosData;
    end;
  end;

{Start !!.21}

{*
   NOTE:  IBM QuickWriter (5204):
          The IBM QuickWriter (5204) is a 24-pin dot matrix printer.
*}
  procedure QuickWriter5204Register(P : PrinterPtr);                  {!!.21}
    { Registers P as an IBM QuickWriter (5204) printer }
  const
    PropFont         = NewFont+pmProportional;
    HighWide         = NewFont+pmChangesLPI;
  var
    Opts : Word;
  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);

      { With reset, you don't know what font will be active, because  }
      { it is set by dip switches within the printer.  For consistent }
      { results, you should specifically set the default font to a    }
      { known font. }
      AddPrintMode('PrinterReset',#27#73#0,'',0,0,PrinterReset,PropFont);

      { We'll use cp437Proportional as the default font, because that }
      { corresponds to the factory dip switch settings.  But we'll    }
      { set it specifically, so we won't be surprised if the user     }
      { changed the dip switch settings. }
      AddPrintMode('DefaultFont',#27#73#35,'',12.0,0,DefaultFont,PropFont);

      AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);
      AddPrintMode('DoubleStrike',#27#71,#27#72,0,0,DoubleStrike,0);
      AddPrintMode('Superscript',#27#83#0,#27#84,0,0,SuperScript,0);
      AddPrintMode('Subscript',#27#83#1,#27#84,0,0,SubScript,0);
      AddPrintMode('Underline',#27#45#1,#27#45#0,0,0,Underline,0);
      AddPrintMode('Overline',#27#95#1,#27#95#0,0,0,OverLine,0);
      AddPrintMode('Unidirectional',#27#85#1,#27#85#0,0,0,Unidirectional,0);

      { Fonts & Code Pages: }
      AddPrintMode('cp437Courier10',#27#73#32,'',10.0,0,Normal,NewFontPitch);
      AddPrintMode('cp437Courier12',#27#73#33,'',12.0,0,Normal,NewFontPitch);
      AddPrintMode('cp437Gothic17',#27#73#34,'',17.0,0,Normal,NewFontPitch);
      AddPrintMode('cp437Proportional',#27#73#35,'',12.0,0,Normal,PropFont);

      AddPrintMode('cp850Courier10',#27#73#36,'',10.0,0,Normal,NewFontPitch);
      AddPrintMode('cp850Courier12',#27#73#37,'',12.0,0,Normal,NewFontPitch);
      AddPrintMode('cp850Gothic17',#27#73#38,'',17.0,0,Normal,NewFontPitch);
      AddPrintMode('cp850Proportional',#27#73#39,'',12.0,0,Normal,PropFont);

      { Causes the printer to print the next character as a character,}
      { even if it normally represents a control code: }
      AddPrintMode('PrintNextChar',#27#94,'',0,0,Normal,0);

      { Change the lines per inch: }
      AddPrintMode('Set6lpi',#27#50,'',0,6.0,Normal,pmChangesLPI);
      AddPrintMode('Set8lpi',#27#48,'',0,8.0,Normal,pmChangesLPI);
      AddPrintMode('Set10pt2lpi',#27#49,'',0,10.2,Normal,pmChangesLPI);

      AddPrintMode('DoubleWidth',#27#87#1,#27#87#0,0,0,DoubleWidth,pmFont);
      AddPrintMode('Compressed',#15,#18,0,0,Compressed,pmFont);

      AddPrintMode('DoubleHighWide',#27#91#64#4#0#0#0#34#2,#27#91#64#4#0#0#0#17#1,0,0,DoubleWidth,HighWide);

      AddPrintMode('Draft',#27#73#1,'',0,0,Dim,pmFont);
      AddPrintMode('LetterQuality',#27#73#2,'',0,0,LetterQuality,pmFont);

      { Cause the printer to beep: }
      AddPrintMode('BeepPrinter',#7,'',0,0,BeepPrinter,0);

      { Set the TopOfForm to the current vertical position: }
      AddPrintMode('SetTopOfForm',#27#52,'',0,0,SetTopOfForm,0);

      { Set the page size (page height in inches) as seen by the printer: }
      { These automatically set the TopOfForm to the current vertical position! }

      { 4 inches, for regular envelopes: }
      AddPrintMode('PageLen4in',#27#67#0#4,'',0,0,ChangePageLen,pmChangesLPP);
      { To set some other value for page size, replace the "#4", which }
      { means 4 inches, with the number of inches per page. }

      { 6 inches, for large envelopes: }
      AddPrintMode('PageLen6in',#27#67#0#6,'',0,0,ChangePageLen,pmChangesLPP);

      { 11 inches, for standard (US) paper size: }
      AddPrintMode('PageLen11in',#27#67#0#10,'',0,0,ChangePageLen,pmChangesLPP);

      { 11.625 inches, for European (A4) paper size: }
      { This setting works by setting the number of lines-per-page. }
      { It assumes that you are currently using 6 LPI !! }
      AddPrintMode('PageLenA4',#27#67#55#70,'',0,0,ChangePageLen,pmChangesLPP);
      { To set the page size for some other number of lines-per-page,}
      { replace the "#70" at the end of the string with the number of }
      { lines per page (#70 = 70 LPP). }

      { 12 inches, common size for some label sheets: }
      AddPrintMode('PageLen12in',#27#67#0#12,'',0,0,ChangePageLen,pmChangesLPP);

      { 14 inches, for legal size paper: }
      AddPrintMode('PageLen14in',#27#67#0#14,'',0,0,ChangePageLen,pmChangesLPP);

      SetRegisteredType(pQuickWriter5204);
    end;
  end;  {Procedure QuickWriter5204Register}


{*
   NOTE:  Brother M-1709:
          The Brother M-1709 printer is a 9-pin dot-matrix printer.
          Dip switches enable it to be set for IBM mode or Epson mode.
          Dip switches also switch between IBM and Epson character sets.
          We will assume that if the printer is in IBM mode,it is also
          using the IBM character set.  If it is in Epson mode,it is
          also using the Epson character set.
*}
  procedure BrotherM1709IBMmodeRegister(P : PrinterPtr);              {!!.21}
    { Registers P as a Brother M-1709 printer in IBM Mode }
  const
    PropFont         = NewFont+pmProportional;
  var
    Opts : Word;
  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);

      AddPrintMode('PrinterReset',#27#64,'',0,0,PrinterReset,PropFont);

      AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);
      AddPrintMode('DoubleStrike',#27#71,#27#72,0,0,DoubleStrike,0);
      AddPrintMode('Superscript',#27#83#0,#27#84,0,0,SuperScript,0);
      AddPrintMode('Subscript',#27#83#1,#27#84,0,0,SubScript,0);
      AddPrintMode('Underline',#27#45#1,#27#45#0,0,0,Underline,0);
      AddPrintMode('Overline',#27#95#1,#27#95#0,0,0,OverLine,0);
      AddPrintMode('Unidirectional',#27#85#1,#27#85#0,0,0,Unidirectional,0);

      { Character Sizes (fonts): }
      AddPrintMode('Pica10cpi',#27#80,'',10.0,0,DefaultFont,NewFontPitch);
      AddPrintMode('Elite12cpi',#27#58,#27#80,12.0,0,Normal,NewFontPitch);

      { Causes the printer to print the next character as a character,}
      { even if it normally represents a control code: }
      AddPrintMode('PrintNextChar',#27#94,'',0,0,Normal,0);

      { Change the lines per inch: }
      AddPrintMode('Set6lpi',#27#50,'',0,6.0,Normal,pmChangesLPI);
      AddPrintMode('Set8lpi',#27#48,'',0,8.0,Normal,pmChangesLPI);
      AddPrintMode('Set10pt2lpi',#27#49,'',0,10.2,Normal,pmChangesLPI);

      AddPrintMode('DoubleWidth',#27#87#1,#27#87#0,0,0,DoubleWidth,pmFont);
      AddPrintMode('Compressed',#15,#18,0,0,Compressed,pmFont);

      AddPrintMode('Draft',#27#73#0,'',0,0,Dim,pmFont);
      AddPrintMode('LetterQuality',#27#73#2,'',0,0,LetterQuality,pmFont);

      { Set the TopOfForm to the current vertical position: }
      AddPrintMode('SetTopOfForm',#27#52,'',0,0,SetTopOfForm,0);

      { Set the page size (page height in inches) as seen by the printer: }
      { These automatically set the TopOfForm to the current vertical position! }

      { 4 inches, for regular envelopes: }
      AddPrintMode('PageLen4in',#27#67#0#4,'',0,0,ChangePageLen,pmChangesLPP);
      { To set some other value for page size, replace the "#4", which }
      { means 4 inches, with the number of inches per page. }

      { 6 inches, for large envelopes: }
      AddPrintMode('PageLen6in',#27#67#0#6,'',0,0,ChangePageLen,pmChangesLPP);

      { 11 inches, for standard (US) paper size: }
      AddPrintMode('PageLen11in',#27#67#0#10,'',0,0,ChangePageLen,pmChangesLPP);

      { 11.625 inches, for European (A4) paper size: }
      { This setting works by setting the number of lines-per-page. }
      { It assumes that you are currently using 6 LPI !! }
      AddPrintMode('PageLenA4',#27#67#70,'',0,0,ChangePageLen,pmChangesLPP);
      { To set the page size for some other number of lines-per-page,}
      { replace the "#70" at the end of the string with the number of }
      { lines per page (#70 = 70 LPP). }

      { 12 inches, common size for some label sheets: }
      AddPrintMode('PageLen12in',#27#67#0#12,'',0,0,ChangePageLen,pmChangesLPP);

      { 14 inches, for legal size paper: }
      AddPrintMode('PageLen14in',#27#67#0#14,'',0,0,ChangePageLen,pmChangesLPP);

      SetRegisteredType(pBrotherM1709IBMmode);

    end;
  end;  {Procedure BrotherM1709IBMmodeRegister}


  procedure BrotherM1709EpsonModeRegister(P : PrinterPtr);            {!!.21}
    { Registers P as a Brother M-1709 printer in Epson Mode }
  const
    PropFont         = NewFont+pmProportional;
  var
    Opts : Word;
  begin
    with P^ do begin
      Opts := GetPrnOptions;
      Opts := (Opts and (not pPositionable)) or pBackSpaceAllowed;
      SetPrnOptions(Opts);

      AddPrintMode('PrinterReset',#27#64,'',0,0,PrinterReset,PropFont);

      AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);
      AddPrintMode('Italics',#27#52,#27#53,0,0,Italics,0);
      AddPrintMode('DoubleStrike',#27#71,#27#72,0,0,DoubleStrike,0);
      AddPrintMode('Superscript',#27#83#0,#27#84,0,0,SuperScript,0);
      AddPrintMode('Subscript',#27#83#1,#27#84,0,0,SubScript,0);
      AddPrintMode('Underline',#27#45#1,#27#45#0,0,0,Underline,0);
      AddPrintMode('Unidirectional',#27#85#1,#27#85#0,0,0,Unidirectional,0);

      { Character Sizes (fonts): }
      AddPrintMode('Pica10cpi',#27#80,'',10.0,0,DefaultFont,NewFontPitch);
      AddPrintMode('Elite12cpi',#27#58,#27#80,12.0,0,Normal,NewFontPitch);
      AddPrintMode('Proportional',#27#112#1,#27#112#0,12.0,0,Normal,PropFont);

      { Change the lines per inch: }
      AddPrintMode('Set6lpi',#27#50,'',0,6.0,Normal,pmChangesLPI);
      AddPrintMode('Set8lpi',#27#48,'',0,8.0,Normal,pmChangesLPI);
      AddPrintMode('Set10pt2lpi',#27#49,'',0,10.2,Normal,pmChangesLPI);

      AddPrintMode('DoubleWidth',#27#87#1,#27#87#0,0,0,DoubleWidth,pmFont);
      AddPrintMode('Compressed',#15,#18,0,0,Compressed,pmFont);

      AddPrintMode('Draft',#27#120#0,'',0,0,Dim,pmFont);
      AddPrintMode('LetterQuality',#27#120#1,'',0,0,LetterQuality,pmFont);

      { Set the page size (page height in inches) as seen by the printer: }
      { These automatically set the TopOfForm to the current vertical position! }

      { 4 inches, for regular envelopes: }
      AddPrintMode('PageLen4in',#27#67#0#4,'',0,0,ChangePageLen,pmChangesLPP);
      { To set some other value for page size, replace the "#4", which }
      { means 4 inches, with the number of inches per page. }

      { 6 inches, for large envelopes: }
      AddPrintMode('PageLen6in',#27#67#0#6,'',0,0,ChangePageLen,pmChangesLPP);

      { 11 inches, for standard (US) paper size: }
      AddPrintMode('PageLen11in',#27#67#0#10,'',0,0,ChangePageLen,pmChangesLPP);

      { 11.625 inches, for European (A4) paper size: }
      { This setting works by setting the number of lines-per-page. }
      { It assumes that you are currently using 6 LPI !! }
      AddPrintMode('PageLenA4',#27#67#70,'',0,0,ChangePageLen,pmChangesLPP);
      { To set the page size for some other number of lines-per-page, }
      { replace the "#70" at the end of the string with the number of }
      { lines per page (#70 = 70 LPP). }

      { 12 inches, common size for some label sheets: }
      AddPrintMode('PageLen12in',#27#67#0#12,'',0,0,ChangePageLen,pmChangesLPP);

      { 14 inches, for legal size paper: }
      AddPrintMode('PageLen14in',#27#67#0#14,'',0,0,ChangePageLen,pmChangesLPP);

      SetRegisteredType(pBrotherM1709IBMmode);
    end;
  end;  {Procedure BrotherM1709EpsonModeRegister}
{End !!.21}

{Start !!.22}
{*
    NOTE:  The Panasonic KX-P2124 is a 24-pin dot-matrix printer.
           It operates in either Epson LQ-860 mode, or
           IBM Proprinter X24E mode.  The factory-set default is
           Epson LQ-860 mode, so that is the mode handled here.
           Also has optional color printing (kit KX-PCK11).
*}

procedure PanasonicKXP2124Register(P : PrinterPtr);                  {!!.22}
  {-Registers P as a Panasonic KX-P2124 printer}
const
  PropFont    = NewFont + pmProportional;
  HighWide    = NewFont + pmChangesLPI;
var
  Opts : Word;
begin
  with P^ do
  begin
    Opts := GetPrnOptions;
    Opts := (Opts AND (Not pPositionable)) or pBackSpaceAllowed;
    SetPrnOptions(Opts);

    AddPrintMode('PrinterReset',#27#64,'',0,0,PrinterReset,0);
    AddPrintMode('DefaultFont',#27#107#6,'',12.0,0,DefaultFont,PropFont);

    AddPrintMode('Bold',#27#69,#27#70,0,0,Bold,0);
    AddPrintMode('Italic',#27#52,#27#53,0,0,Italics,0);
    AddPrintMode('Underline',#27#45#1,#27#45#0,0,0,Underline,0);
    AddPrintMode('DoubleStrike',#27#71,#27#72,0,0,DoubleStrike,0);
    AddPrintMode('Superscript',#27#83#0,#27#84,0,0,SuperScript,0);
    AddPrintMode('Subscript',#27#83#1,#27#84,0,0,SubScript,0);
    AddPrintMode('Overline',#27#40#45#3#0#1#3#1,#27#40#45#3#0#1#3#0,0,0,OverLine,0);
    AddPrintMode('Unidirectional',#27#85#1,#27#85#0,0,0,Unidirectional,0);

    AddPrintMode('Draft',#27#120#0,'',0,0,Dim,pmFont);
    AddPrintMode('LetterQuality',#27#120#1,'',0,0,LetterQuality,pmFont);
    AddPrintMode('SuperLtrQuality',#27#120#2,'',0,0,SuperLtrQuality,pmFont);

    AddPrintMode('TypeFaceRoman',#27#107#0,'',0,0,LetterQuality,pmFont);
    AddPrintMode('TypeFaceSansSerif',#27#107#1,'',0,0,LetterQuality,pmFont);
    AddPrintMode('TypeFaceCourier',#27#107#2,'',0,0,LetterQuality,pmFont);
    AddPrintMode('TypeFacePrestige',#27#107#3,'',0,0,LetterQuality,pmFont);
    AddPrintMode('TypeFaceScript',#27#107#4,'',0,0,LetterQuality,pmFont);
    AddPrintMode('TypeFaceOCRB',#27#107#5,'',0,0,LetterQuality,pmFont);
    AddPrintMode('TypeFaceBoldPS',#27#107#6,'',0,0,LetterQuality,pmFont);
    AddPrintMode('TypeFaceOrator',#27#107#7,'',0,0,LetterQuality,pmFont);

    AddPrintMode('Pica10cpi',#27#80,'',0,0,Pica10cpi,pmFont);
    AddPrintMode('Elite12cpi',#27#77,'',0,0,Elite12cpi,pmFont);
    AddPrintMode('Micron15cpi',#27#103,'',0,0,Micron15cpi,pmFont);
    AddPrintMode('Proportional',#27#112#1,#27#112#0,0,0,LetterQuality,PropFont);
    AddPrintMode('Compressed',#15,#18,0,0,Compressed,pmFont);

    AddPrintMode('DoubleWidth',#27#87#1,#27#87#0,0,0,DoubleWidth,pmFont);
    AddPrintMode('DoubleHighWide',#27#87#1#27#119#1,#27#119#0#27#87#0,0,0,DoubleHighWide,HighWide);

    {Change the lines per inch:}
    AddPrintMode('Set6lpi',#27#50,'',0,6.0,Normal,pmChangesLPI);
    AddPrintMode('Set8lpi',#27#48,'',0,8.0,Normal,pmChangesLPI);
    AddPrintMode('Set10pt2lpi',#27#43#35,'',0,10.2,Normal,pmChangesLPI);

    {Cause the printer to beep:}
    AddPrintMode('BeepPrinter',#7,'',0,0,BeepPrinter,0);

    {Set the page size (page height in inches) as seen by the printer:}
    {These automatically set the TopOfForm to the current vertical position!}

    {4 inches, for regular envelopes: }
    AddPrintMode('PageLen4in',#27#67#0#4,'',0,0,ChangePageLen,pmChangesLPP);
    {To set some other value for page size, replace the "#4", which}
    {means 4 inches, with the number of inches per page.}

    {6 inches, for large envelopes:}
    AddPrintMode('PageLen6in',#27#67#0#6,'',0,0,ChangePageLen,pmChangesLPP);

    {11 inches, for standard (US) paper size:}
    AddPrintMode('PageLen11in',#27#67#0#10,'',0,0,ChangePageLen,pmChangesLPP);

    {11.625 inches, for European (A4) paper size:}
    {This setting works by setting the number of lines-per-page.}
    {It assumes that you are currently using 6 LPI !!}
    AddPrintMode('PageLenA4',#27#67#70,'',0,0,ChangePageLen,pmChangesLPP);
    {To set the page size for some other number of lines-per-page,}
    {replace the "#70" at the end of the string with the number of}
    {lines per page (#70 = 70 LPP).}

    {12 inches, common size for some label sheets:}
    AddPrintMode('PageLen12in',#27#67#0#12,'',0,0,ChangePageLen,pmChangesLPP);

    {14 inches, for legal size paper:}
    AddPrintMode('PageLen14in',#27#67#0#14,'',0,0,ChangePageLen,pmChangesLPP);

    {COLOR COMMANDS:  For these commands to work, the printer must     }
    {                 have the optional color kit (KX-PCK11) installed!}

    AddPrintMode('clrRed',#27#114#1,#27#114#0,0,0,clrRed,0 );
    AddPrintMode('clrBlue',#27#114#2,#27#114#0,0,0,clrRed,0 );
    AddPrintMode('clrViolet',#27#114#3,#27#114#0,0,0,clrRed,0 );
    AddPrintMode('clrYellow',#27#114#4,#27#114#0,0,0,clrRed,0 );
    AddPrintMode('clrOrange',#27#114#5,#27#114#0,0,0,clrRed,0 );
    AddPrintMode('clrGreen',#27#114#6,#27#114#0,0,0,clrRed,0 );

    SetRegisteredType(pPanasonicKXP2124);
  end;
end;  {Procedure PanasonicKXP2124Register}                           {!!.22}
{End !!.22}


{$IFDEF InitAllUnits}
begin
{$ENDIF}
end.

