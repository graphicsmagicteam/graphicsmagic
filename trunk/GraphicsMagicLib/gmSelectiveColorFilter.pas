{ This filter is written by Ma Xiaoguang and Ma Xiaoming ( gmbros@hotmail.com )
  Copyright (c) Ma Xiaoguang and Ma Xiaoming. All rights reserved.

  Last update: 2012-02-17
 }

unit gmSelectiveColorFilter;

interface

uses
  GR32, GR32_LowLevel;

type
  TgmColorSelector = (gmcsReds,
                      gmcsYellows,
                      gmcsGreens,
                      gmcsCyans,
                      gmcsBlues,
                      gmcsMagentas,
                      gmcsWhites,
                      gmcsNeutrals,
                      gmcsBlacks);

  TgmSelectiveColorAdjustMode = (gmscamRelative,
                                 gmscamAbsolute);

  TgmSelectiveColorFilter = class(TObject)
  private
    FAdjustMode   : TgmSelectiveColorAdjustMode;
    FCyanScales   : array [TgmColorSelector] of Single;
    FMagentaScales: array [TgmColorSelector] of Single;
    FYellowScales : array [TgmColorSelector] of Single;
    FBlackScales  : array [TgmColorSelector] of Single;

    function GetCyanScale(AIndex: TgmColorSelector): Single;
    function GetMagentaScale(AIndex: TgmColorSelector): Single;
    function GetYellowScale(AIndex: TgmColorSelector): Single;
    function GetBlackScale(AIndex: TgmColorSelector): Single;

    procedure SetCyanScale(AIndex: TgmColorSelector; AValue: Single);
    procedure SetMagentaScale(AIndex: TgmColorSelector; AValue: Single);
    procedure SetYellowScale(AIndex: TgmColorSelector; AValue: Single);
    procedure SetBlackScale(AIndex: TgmColorSelector; AValue: Single);

    procedure ResetAdjustScales;
  public
    constructor Create;

    procedure Execute(const ABmp: TBitmap32);

    property AdjustMode                            : TgmSelectiveColorAdjustMode read FAdjustMode     write FAdjustMode;
    property CyanScales[index: TgmColorSelector]   : Single                      read GetCyanScale    write SetCyanScale;
    property MagentaScales[index: TgmColorSelector]: Single                      read GetMagentaScale write SetMagentaScale;
    property YellowScales[index: TgmColorSelector] : Single                      read GetYellowScale  write SetYellowScale;
    property BlackScales[index: TGmColorSelector]  : Single                      read GetBlackScale   write SetBlackScale;
  end;

implementation

uses
{ Standard }
  Math, SysUtils,
{ externals\Little CMS }
  lcms2dll,
{ GraphicsMagic Lib }
  gmColorSpace;

constructor TgmSelectiveColorFilter.Create;
begin
  inherited Create;

  FAdjustMode := gmscamRelative;
  ResetAdjustScales;
end; 

function TgmSelectiveColorFilter.GetCyanScale(AIndex: TgmColorSelector): Single;
begin
  Result := FCyanScales[AIndex];
end;

function TgmSelectiveColorFilter.GetMagentaScale(AIndex: TgmColorSelector): Single;
begin
  Result := FMagentaScales[AIndex];
end;

function TgmSelectiveColorFilter.GetYellowScale(AIndex: TgmColorSelector): Single;
begin
  Result := FYellowScales[AIndex];
end;

function TgmSelectiveColorFilter.GetBlackScale(AIndex: TgmColorSelector): Single;
begin
  Result := FBlackScales[AIndex];
end;

procedure TgmSelectiveColorFilter.SetCyanScale(AIndex: TgmColorSelector;
  AValue: Single);
begin
  FCyanScales[AIndex] := AValue;
end;

procedure TgmSelectiveColorFilter.SetMagentaScale(AIndex: TgmColorSelector;
  AValue: Single);
begin
  FMagentaScales[AIndex] := AValue;
end; 

procedure TgmSelectiveColorFilter.SetYellowScale(AIndex: TgmColorSelector;
  AValue: Single);
begin
  FYellowScales[AIndex] := AValue;
end;

procedure TgmSelectiveColorFilter.SetBlackScale(AIndex: TgmColorSelector;
  AValue: Single);
begin
  FBlackScales[AIndex] := AValue;
end;

procedure TgmSelectiveColorFilter.ResetAdjustScales;
var
  LColorSelector: TgmColorSelector;
begin
  for LColorSelector := gmcsReds to gmcsBlacks do
  begin
    FCyanScales[LColorSelector]    := 0.0;
    FMagentaScales[LColorSelector] := 0.0;
    FYellowScales[LColorSelector]  := 0.0;
    FBlackScales[LColorSelector]   := 0.0;
  end;
end;

// solution 1 -- calculte local color gamut and weight value in HSL color space
{procedure TgmSelectiveColorFilter.Execute(const ABmp: TBitmap32);
var
  i               : Integer;
  a, r, g, b      : Byte;
  rr, gg, bb      : Integer;
  H, L, S         : Integer;
  SH, SL, SS      : Integer;
  FH, FL, FS      : Single;
  FSH, FSL, FSS   : Single;
  LBits           : PColor32;

  hInProfile      : cmsHPROFILE;
  hOutProfile     : cmsHPROFILE;
  hTransform      : cmsHTransform;
  hTransform2     : cmsHTransform;
  RGBArray        : array [0..2] of Double;
  CMYKArray       : array [0..3] of Double;
  LCMYKProfileName: string;

  LDiffH          : Single;
  LDiffS          : Single;
  LDiffL          : Single;
  LDistance       : Single;
  LWeight         : Single;
begin
  RGBToHLS32(clRed32, SH, SL, SS);

  FSH := SH / 360;
  FSS := SS / 255;
  FSL := SL / 255;

  LCMYKProfileName := ExtractFileDir(ParamStr(0)) + '\USWebCoatedSWOP.icc';

  hInProfile  := cmsCreate_sRGBProfile;
  hOutProfile := cmsOpenProfileFromFile(PAnsiChar(LCMYKProfileName), 'r');

  hTransform  := cmsCreateTransform(hInProfile, TYPE_RGB_DBL,
                                    hOutProfile, TYPE_CMYK_DBL,
                                    INTENT_PERCEPTUAL, 0);

  hTransform2 := cmsCreateTransform(hOutProfile, TYPE_CMYK_DBL,
                                    hInProfile, TYPE_RGB_DBL,
                                    INTENT_PERCEPTUAL, 0);

  try
    LBits := @ABmp.Bits[0];
    for i := 0 to (ABmp.Width * ABmp.Height - 1) do
    begin
      RGBToHLS32(LBits^, H, L, S);

      FH := H / 360;
      FS := S / 255;
      FL := L / 255;

      LDiffH := FH - FSH;
      LDiffS := FS - FSS;
      LDiffL := FL - FSL;

      LDistance := Sqrt( (LDiffH * LDiffH) + (LDiffS * LDiffS) + (LDiffL * LDiffL) );

      a := LBits^ shr 24 and $FF;
      r := LBits^ shr 16 and $FF;
      g := LBits^ shr  8 and $FF;
      b := LBits^        and $FF;

      RGBArray[0] := r / 255;
      RGBArray[1] := g / 255;
      RGBArray[2] := b / 255;
      cmsDoTransform(hTransform, @RGBArray, @CMYKArray, 1);

      if LDistance < 1.0 then
      begin
        LWeight := 1.0 - LDistance;

        CMYKArray[0] := CMYKArray[0] * (1 + FCyanScale    * LWeight);
        CMYKArray[1] := CMYKArray[1] * (1 + FMagentaScale * LWeight);
        CMYKArray[2] := CMYKArray[2] * (1 + FYellowScale  * LWeight);
        CMYKArray[3] := CMYKArray[3] * (1 + FBlackScale   * LWeight);

        cmsDoTransform(hTransform2, @CMYKArray, @RGBArray, 1);

        rr := Round(RGBArray[0] * 255);
        gg := Round(RGBArray[1] * 255);
        bb := Round(RGBArray[2] * 255);

        r := Clamp(rr, 0, 255);
        g := Clamp(gg, 0, 255);
        b := Clamp(bb, 0, 255);

        LBits^ := (a shl 24) or (r shl 16) or (g shl 8) or b;
      end;
      
      Inc(LBits);
    end;

  finally
    cmsDeleteTransform(hTransform);
    cmsDeleteTransform(hTransform2);
    cmsCloseProfile(hInProfile);
    cmsCloseProfile(hOutProfile);
  end;
end;
}

{ Solution 2 -- calculte local color gamut and weight value in Lab color space.

  Algorithm:

  This solution is based on following paper which is about
  Selective Color Correction at Google:

  http://www.google.com.hk/patents?hl=zh-CN&lr=&vid=USPAT6058207&id=3z8EAAAAEBAJ&oi=fnd&dq=principle+of+selective+color+correction+by+CMYK&printsec=abstract#v=onepage&q&f=false

  The paper is titled "Selective color correction applied to plurality of 
  local color gamuts" written by Chris Tuijn et al.

  We can hardly understand all the content in the paper. So the algorithm
  in the following applied part of the concept from the paper.

  If you could teach us how to make the "Selective Color Correction", please
  contact us.

  E-mail: gmbros@hotmail.com

  *************************************************************************

  Utilities:
  
  Also note that, for the color space conversion between RGB and CMYK, we 
  employed the wonderful library Little CMS, which is an open source project
  and released under MIT, you could find it at here:

  http://sourceforge.net/projects/lcms/
  http://www.littlecms.com/

  We found that to convert color space between RGB and CMYK properly, we need
  to use ICC profiles, and to work with ICC files Little CMS is a good choice,
  it applies an easy way to read ICC profile files and work with them. The
  Little CMS is written in C/C++, but it applies a DLL file. We could calling
  the API from the DLL file.

  Also, we have tested that to convert RGB to CMYK with USWebCoatedSWOP.icc is
  closest to Photoshop's. So we have downloaded it from
  http://www.adobe.com/support/downloads/product.jsp?product=62&platform=windows.
 }
procedure TgmSelectiveColorFilter.Execute(const ABmp: TBitmap32);
const
  L_RADIUS = 90.0;  // hard coded value for action radius
  C_RADIUS = 90.0;
  H_RADIUS = 90.0;
var
  i                    : Integer;
  a, r, g, b           : Byte;
  rr, gg, bb           : Integer;
  LBits                : PColor32;

  hRGBProfile          : cmsHPROFILE;
  hCMYKProfile         : cmsHPROFILE;
  hXYZProfile          : cmsHPROFILE;

  hRGBToCMYKTransform  : cmsHTransform;
  hCMYKToRGBTransform  : cmsHTransform;
  hRGBToXYZTransform   : cmsHTransform;
  hXYZToRGBTransform   : cmsHTransform;

  PixelRGBArray        : array [0..2] of Double;
  SelectedColorRGBArray: array [0..2] of Double;
  CMYKArray            : array [0..3] of Double;
  XYZArray             : array [0..2] of Double;
  PixelLabArray        : array [0..2] of Double;
  SelectedColorLabArray: array [0..2] of Double;

  LCMYKProfileName     : string;

  LWeight              : Single;
  p, p1, p2, p3        : Single;

  LColorSelector       : TgmColorSelector;
  LAccumRed            : Integer;
  LAccumGreen          : Integer;
  LAccumBlue           : Integer;
  LAccumCounter        : Integer;
begin
  LCMYKProfileName := ExtractFileDir(ParamStr(0)) + '\USWebCoatedSWOP.icc';

  hRGBProfile  := cmsCreate_sRGBProfile;
  hCMYKProfile := cmsOpenProfileFromFile(PAnsiChar(LCMYKProfileName), 'r');
  hXYZProfile  := cmsCreateXYZProfile;

  hRGBToCMYKTransform := cmsCreateTransform(hRGBProfile, TYPE_RGB_DBL,
                                            hCMYKProfile, TYPE_CMYK_DBL,
                                            INTENT_PERCEPTUAL, 0);

  hCMYKToRGBTransform := cmsCreateTransform(hCMYKProfile, TYPE_CMYK_DBL,
                                            hRGBProfile, TYPE_RGB_DBL,
                                            INTENT_PERCEPTUAL, 0);

  hRGBToXYZTransform := cmsCreateTransform(hRGBProfile, TYPE_RGB_DBL,
                                           hXYZProfile, TYPE_XYZ_DBL,
                                           INTENT_PERCEPTUAL, 0);

  hXYZToRGBTransform := cmsCreateTransform(hXYZProfile, TYPE_XYZ_DBL,
                                           hRGBProfile, TYPE_RGB_DBL,
                                           INTENT_PERCEPTUAL, 0);

  try
    // process each pixels on the image
    LBits := @ABmp.Bits[0];

    for i := 0 to (ABmp.Width * ABmp.Height - 1) do
    begin
      LAccumRed     := 0;
      LAccumGreen   := 0;
      LAccumBlue    := 0;
      LAccumCounter := 0;

      a := LBits^ shr 24 and $FF;
      r := LBits^ shr 16 and $FF;
      g := LBits^ shr  8 and $FF;
      b := LBits^        and $FF;

      PixelRGBArray[0] := r / 255;
      PixelRGBArray[1] := g / 255;
      PixelRGBArray[2] := b / 255;

      // convert the color of current pixels to Lab color space
      cmsDoTransform(hRGBToXYZTransform, @PixelRGBArray, @XYZArray, 1);
      cmsXYZ2Lab(nil, @PixelLabArray, @XYZArray);

      // process each selected color
      for LColorSelector := gmcsReds to gmcsMagentas do
      begin
        // check for whether there are some acquired changes for this selected color
        if (FCyanScales[LColorSelector]    <> 0.0) or
           (FMagentaScales[LColorSelector] <> 0.0) or
           (FYellowScales[LColorSelector]  <> 0.0) or
           (FBlackScales[LColorSelector]   <> 0.0) then
        begin
          // determine a main color to calculate the local color gamut
          case LColorSelector of
            gmcsReds:
              begin
                SelectedColorRGBArray[0] := 1.0;
                SelectedColorRGBArray[1] := 0.0;
                SelectedColorRGBArray[2] := 0.0;
              end;

            gmcsYellows:
              begin
                SelectedColorRGBArray[0] := 1.0;
                SelectedColorRGBArray[1] := 1.0;
                SelectedColorRGBArray[2] := 0.0;
              end;

            gmcsGreens:
              begin
                SelectedColorRGBArray[0] := 0.0;
                SelectedColorRGBArray[1] := 1.0;
                SelectedColorRGBArray[2] := 0.0;
              end;

            gmcsCyans:
              begin
                SelectedColorRGBArray[0] := 0.0;
                SelectedColorRGBArray[1] := 1.0;
                SelectedColorRGBArray[2] := 1.0;
              end;

            gmcsBlues:
              begin
                SelectedColorRGBArray[0] := 0.0;
                SelectedColorRGBArray[1] := 0.0;
                SelectedColorRGBArray[2] := 1.0;
              end;

            gmcsMagentas:
              begin
                SelectedColorRGBArray[0] := 1.0;
                SelectedColorRGBArray[1] := 0.0;
                SelectedColorRGBArray[2] := 1.0;
              end;
          end;

          // convert the selected color to XYZ Lab space
          cmsDoTransform(hRGBToXYZTransform, @SelectedColorRGBArray, @XYZArray, 1);
          cmsXYZ2Lab(nil, @SelectedColorLabArray, @XYZArray);

          // if the color of current pixels is within a color local gamut...
          if ( PixelLabArray[0] >= (SelectedColorLabArray[0] - L_RADIUS) ) and
             ( PixelLabArray[0] <= (SelectedColorLabArray[0] + L_RADIUS) ) and
             ( PixelLabArray[1] >= (SelectedColorLabArray[1] - C_RADIUS) ) and
             ( PixelLabArray[1] <= (SelectedColorLabArray[1] + C_RADIUS) ) and
             ( PixelLabArray[2] >= (SelectedColorLabArray[2] - H_RADIUS) ) and
             ( PixelLabArray[2] <= (SelectedColorLabArray[2] + H_RADIUS) ) then
          begin
            cmsDoTransform(hRGBToCMYKTransform, @PixelRGBArray, @CMYKArray, 1);

            case FAdjustMode of
              gmscamRelative:
                begin
                  // calculate weight value
                  p1 := (PixelLabArray[0] - SelectedColorLabArray[0]) / L_RADIUS;
                  p1 := p1 * p1;

                  p2 := (PixelLabArray[1] - SelectedColorLabArray[1]) / C_RADIUS;
                  p2 := p2 * p2;

                  p3 := (PixelLabArray[2] - SelectedColorLabArray[2]) / H_RADIUS;
                  p3 := p3 * p3;

                  p := -(p1 + p2 + p3);

                  LWeight := Exp(p);

                  // result of algorithm for Relative method is similar to Photoshop's, but not good enough
                  CMYKArray[0] := CMYKArray[0] * (1 + FCyanScales[LColorSelector]    * LWeight);
                  CMYKArray[1] := CMYKArray[1] * (1 + FMagentaScales[LColorSelector] * LWeight);
                  CMYKArray[2] := CMYKArray[2] * (1 + FYellowScales[LColorSelector]  * LWeight);
                  CMYKArray[3] := CMYKArray[3] * (1 + FBlackScales[LColorSelector]   * LWeight);
                end;

              gmscamAbsolute:
                begin
                  // seems like the algorithm for Absolute methhod does not works
                  //CMYKArray[0] := CMYKArray[0] + FCyanScales[LColorSelector];
                  //CMYKArray[1] := CMYKArray[1] + FMagentaScales[LColorSelector];
                  //CMYKArray[2] := CMYKArray[2] + FYellowScales[LColorSelector];
                  //CMYKArray[3] := CMYKArray[3] + FBlackScales[LColorSelector];

                  CMYKArray[0] := CMYKArray[0] * (1 + FCyanScales[LColorSelector]);
                  CMYKArray[1] := CMYKArray[1] * (1 + FMagentaScales[LColorSelector]);
                  CMYKArray[2] := CMYKArray[2] * (1 + FYellowScales[LColorSelector]);
                  CMYKArray[3] := CMYKArray[3] * (1 + FBlackScales[LColorSelector]);
                end;
            end;

            cmsDoTransform(hCMYKToRGBTransform, @CMYKArray, @PixelRGBArray, 1);

            rr := Round(PixelRGBArray[0] * 255);
            gg := Round(PixelRGBArray[1] * 255);
            bb := Round(PixelRGBArray[2] * 255);

            r := Clamp(rr, 0, 255);
            g := Clamp(gg, 0, 255);
            b := Clamp(bb, 0, 255);

            LAccumRed   := LAccumRed   + r;
            LAccumGreen := LAccumGreen + g;
            LAccumBlue  := LAccumBlue  + b;

            Inc(LAccumCounter);
          end;
        end;
      end;

      if LAccumCounter > 0 then
      begin
        r := LAccumRed   div LAccumCounter;
        g := LAccumGreen div LAccumCounter;
        b := LAccumBlue  div LAccumCounter;

        LBits^ := (LBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;
      end;

      Inc(LBits);
    end;

  finally
    cmsDeleteTransform(hRGBToCMYKTransform);
    cmsDeleteTransform(hCMYKToRGBTransform);
    cmsDeleteTransform(hRGBToXYZTransform);
    cmsDeleteTransform(hXYZToRGBTransform);

    cmsCloseProfile(hRGBProfile);
    cmsCloseProfile(hCMYKProfile);
    cmsCloseProfile(hXYZProfile);
  end;
end; 

end.
