{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmPaintBucket;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/LGPL 2.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * ***** END LICENSE BLOCK ***** *)

interface

uses
{ Standard }
  Graphics,
{ Graphics32 }
  GR32,
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Lib }
  gmTypes, gmBlendModes;

type
  TgmPaintBucketFillSource = (pbfsForeColor,
                              pbfsBackColor,
                              pbfsPattern,
                              pbfsTransparent);
                            
  TgmPaintBucketFillCondition = (pbfcContiguous, pbfcDiscontiguous);

  TgmPaintBucketColorMode = (pbcmRGB,
                             pbcmPreserveHue,
                             pbcmPreserveSaturation,
                             pbcmPreserveLuminosity,
                             pbcmAddHue,
                             pbcmSubHue,
                             pbcmAddSaturation,
                             pbcmSubSaturation,
                             pbcmAddLuminosity,
                             pbcmSubLuminosity);

  TgmPaintBucket = class(TObject)
  private
    FTolerance           : Byte;
    FOpacity             : Byte;
    FAdjustIntensity     : Double;
    FColor               : TColor32;
    FBlendMode           : TBlendMode32;
    FFillSource          : TgmPaintBucketFillSource;
    FFillCondition       : TgmPaintBucketFillCondition;
    FColorMode           : TgmPaintBucketColorMode;
    FPattern             : TBitmap;
    FPreserveTransparency: Boolean;
    FChannelSet          : TgmChannelSet;

    procedure ContiguousFill(const ASampleBmp, ADestBmp: TBitmap32; const AX, AY: Integer);
    procedure DiscontiguousFill(const ASampleBmp, ADestBmp: TBitmap32; const AX, AY: Integer);
    
    function GetBlendColor(const AForeColor, ABackColor: TColor32): TColor32;
    function GetAdjustedColor(const AForeColor, ABackColor: TColor32): TColor32;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Paint(const ASampleBmp, ADestBmp: TBitmap32; const AX, AY: Integer);

    property Tolerance           : Byte                        read FTolerance            write FTolerance;
    property Opacity             : Byte                        read FOpacity              write FOpacity;
    property AdjustIntensity     : Double                      read FAdjustIntensity      write FAdjustIntensity;
    property Color               : TColor32                    read FColor                write FColor;
    property FillSource          : TgmPaintBucketFillSource    read FFillSource           write FFillSource;
    property FillCondition       : TgmPaintBucketFillCondition read FFillCondition        write FFillCondition;
    property ColorMode           : TgmPaintBucketColorMode     read FColorMode            write FColorMode;
    property BlendMode           : TBlendMode32                read FBlendMode            write FBlendMode;
    property PreserveTransparency: Boolean                     read FPreserveTransparency write FPreserveTransparency;
    property ChannelSet          : TgmChannelSet               read FChannelSet           write FChannelSet;
    property Pattern             : TBitmap                     read FPattern;
  end;

  function GetPaintBucketFillSourceString(const AFillSource: TgmPaintBucketFillSource): string;

implementation

uses
{ Standard }
  Math,
{ Graphics32 }
  GR32_LowLevel,
{ externals }
  GR32_SeedFill,
{ GraphicsMagic Lib }
  gmColorSpace, gmColorRange;
  

function GetTransparentColor(const AColor: TColor32;
  const AFillOpacity: Byte): TColor32;
var
  LAlpha: Byte;
  LRGB  : TColor32;
begin
  LAlpha := AColor shr 24 and $FF;
  LRGB   := AColor and $FFFFFF;

  LAlpha := Round( LAlpha * (1 - AFillOpacity / 255) );
  Result := (LAlpha shl 24) or LRGB;
end; 

function GetPaintBucketFillSourceString(
  const AFillSource: TgmPaintBucketFillSource): string;
var
  s: string;
begin
  case AFillSource of
    pbfsForeColor  : s := 'Foreground Color';
    pbfsBackColor  : s := 'Background Color';
    pbfsPattern    : s := 'Pattern';
    pbfsTransparent: s := 'Transparent';
  end;
  
  Result := s;
end; 

{ TgmPaintBucket }

constructor TgmPaintBucket.Create;
begin
  inherited Create;

  FTolerance            := 82;
  FOpacity              := 255;
  FAdjustIntensity      := 0.1;
  FColor                := clBlack32;
  FBlendMode            := bbmNormal32;
  FFillSource           := pbfsForeColor;
  FFillCondition        := pbfcContiguous;
  FColorMode            := pbcmRGB;
  FPreserveTransparency := False;
  FChannelSet           := [];
  FPattern              := TBitmap.Create;
  FPattern.Width        := 10;
  FPattern.Height       := 10;
end;

destructor TgmPaintBucket.Destroy;
begin
  FPattern.Free;
  inherited Destroy;
end;

function TgmPaintBucket.GetBlendColor(
  const AForeColor, ABackColor: TColor32): TColor32;
var
  LRGBChannelCount    : Integer;
  a, r, g, b          : Cardinal;
  LBlendColor         : TColor32;
  fa, LAdjustedOpacity: Byte;
begin
  Result := ABackColor;

  LRGBChannelCount := 0;

  if csRed in FChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csGreen in FChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csBlue in FChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  fa := AForeColor shr 24 and $FF;

  // do nothing if the foreground is fully transparent
  if fa > 0 then
  begin
    a := ABackColor and $FF000000;

    if a > 0 then
    begin
      // combine
      if csGrayscale in FChannelSet then
      begin
        { If the foreground has semi-transparent pixels, we need to adjust the
          blending opacity. }

        if fa < 255 then
        begin
          LAdjustedOpacity := fa * FOpacity div 255;
        end
        else
        begin
          LAdjustedOpacity := FOpacity;
        end;

        LBlendColor := RGBBlendByMode(AForeColor, ABackColor, LAdjustedOpacity, FBlendMode);
        b           := Intensity(LBlendColor);
        Result      := a or (b shl 16) or (b shl 8) or b;
      end
      else
      begin
        // blend color with blend mode
        if LRGBChannelCount = 3 then
        begin
          LBlendColor := ARGBBlendByMode(AForeColor, ABackColor, FOpacity, FBlendMode);
        end
        else
        begin
          { If the foreground has semi-transparent pixels, we need to adjust the
            blending opacity. }

          if fa < 255 then
          begin
            LAdjustedOpacity := fa * FOpacity div 255;
          end
          else
          begin
            LAdjustedOpacity := FOpacity;
          end;

          LBlendColor := RGBBlendByMode(AForeColor, ABackColor, LAdjustedOpacity, FBlendMode);
        end;

        r := ABackColor and $FF0000;
        g := ABackColor and $FF00;
        b := ABackColor and $FF;

        if csRed in FChannelSet then
        begin
          r := LBlendColor and $FF0000;
        end;

        if csGreen in FChannelSet then
        begin
          g := LBlendColor and $FF00;
        end;

        if csBlue in FChannelSet then
        begin
          b := LBlendColor and $FF;
        end;

        if (FPreserveTransparency) or (LRGBChannelCount < 3) then
        begin
          Result := a or r or g or b;
        end
        else
        begin
          Result := (LBlendColor and $FF000000) or r or g or b;
        end;
      end;
    end
    else
    begin
      if not FPreserveTransparency then
      begin
        if (csRed   in FChannelSet) and
           (csGreen in FChannelSet) and
           (csBlue  in FChannelSet) then
        begin
          a      := fa * FOpacity div 255;
          Result := (a shl 24) or (AForeColor and $FFFFFF);
        end;
      end;
    end;
  end;
end;

function TgmPaintBucket.GetAdjustedColor(
  const AForeColor, ABackColor: TColor32): TColor32;
var
  fH, fL, fS: Integer;
  bH, bL, bS: Integer;
begin
  Result := ABackColor;

  case FColorMode of
    pbcmRGB:
      begin
        Result := AForeColor;
      end;

    pbcmPreserveHue:
      begin
        RGBToHLS32(AForeColor, fH, fL, fS);
        RGBToHLS32(ABackColor, bH, bL, bS);

        fH     := Clamp(fH, 0, 360);
        bL     := Clamp(bL, 0, 255);
        bS     := Clamp(bS, 1, 255);
        Result := HLSToRGB32(255, fH, bL, bS);
      end;

    pbcmPreserveSaturation:
      begin
        RGBToHLS32(AForeColor, fH, fL, fS);
        RGBToHLS32(ABackColor, bH, bL, bS);

        bH     := Clamp(bH, 0, 360);
        bL     := Clamp(bL, 0, 255);
        fS     := Clamp(fS, 1, 255);
        Result := HLSToRGB32(255, bH, bL, fS);
      end;

    pbcmPreserveLuminosity:
      begin
        RGBToHLS32(AForeColor, fH, fL, fS);
        RGBToHLS32(ABackColor, bH, bL, bS);

        bH     := Clamp(bH, 0, 360);
        fL     := Clamp(fL, 0, 255);
        bS     := Clamp(bS, 1, 255);
        Result := HLSToRGB32(255, bH, fL, bS);
      end;

    pbcmAddHue:
      begin
        RGBToHLS32(ABackColor, bH, bL, bS);

        bH     := Round(bH * (1 + FAdjustIntensity));
        bH     := Clamp(bH, 0, 360 );
        bL     := Clamp(bL, 0, 255);
        bS     := Clamp(bS, 1, 255);
        Result := HLSToRGB32(255, bH, bL, bS);
      end;

    pbcmSubHue:
      begin
        RGBToHLS32(ABackColor, bH, bL, bS);

        bH     := Round(bH * (1 - FAdjustIntensity));
        bH     := Clamp(bH, 0, 360 );
        bL     := Clamp(bL, 0, 255);
        bS     := Clamp(bS, 1, 255);
        Result := HLSToRGB32(255, bH, bL, bS);
      end;

    pbcmAddSaturation:
      begin
        RGBToHLS32(ABackColor, bH, bL, bS);

        bH     := Clamp(bH, 0, 360);
        bL     := Clamp(bL, 0, 255);
        bS     := Round(bS * (1 + FAdjustIntensity));
        bS     := Clamp(bS, 1, 255);
        Result := HLSToRGB32(255, bH, bL, bS);
      end;

    pbcmSubSaturation:
      begin
        RGBToHLS32(ABackColor, bH, bL, bS);

        bH     := Clamp(bH, 0, 360);
        bL     := Clamp(bL, 0, 255);
        bS     := Round(bS * (1 - FAdjustIntensity));
        bS     := Clamp(bS, 1, 255);
        Result := HLSToRGB32(255, bH, bL, bS);
      end;

    pbcmAddLuminosity:
      begin
        RGBToHLS32(ABackColor, bH, bL, bS);

        bH     := Clamp(bH, 0, 360);
        bL     := Round(bL * (1 + FAdjustIntensity));
        bL     := Clamp(bL, 0, 255);
        bS     := Clamp(bS, 1, 255);
        Result := HLSToRGB32(255, bH, bL, bS);
      end;

    pbcmSubLuminosity:
      begin
        RGBToHLS32(ABackColor, bH, bL, bS);

        bH     := Clamp(bH, 0, 360);
        bL     := Round(bL * (1 - FAdjustIntensity));
        bL     := Clamp(bL, 0, 255);
        bS     := Clamp(bS, 1, 255);
        Result := HLSToRGB32(255, bH, bL, bS);
      end;
  end;
end; 

procedure TgmPaintBucket.ContiguousFill(const ASampleBmp, ADestBmp: TBitmap32;
  const AX, AY: Integer);
var
  LFiller          : TSeedFill;
  i, j             : Integer;
  LMaskPtr         : PByte;
  LDestPtr         : PColor32Array;
  LFillingColor    : TColor32;
  LPatternFilledBmp: TBitmap32;
begin
{$RANGECHECKS OFF}

  if (not Assigned(ASampleBmp)) or
     (not Assigned(ADestBmp)) then
  begin
    Exit;
  end;

  if (ASampleBmp.Width  <> ADestBmp.Width) or
     (ASampleBmp.Height <> ADestBmp.Height) then
  begin
    Exit;
  end;

  if (AX < 0) or
     (AY < 0) or
     (AX >= ADestBmp.Width) or
     (AY >= ADestBmp.Height) then
  begin
    Exit;
  end;

  LFillingColor := FColor;

  LPatternFilledBmp := TBitmap32.Create;
  LFiller           := TSeedFill.Create(ASampleBmp);
  try
    with LPatternFilledBmp do
    begin
      DrawMode := dmOpaque;
      SetSize(ADestBmp.Width, ADestBmp.Height);
      
      Canvas.Brush.Bitmap := FPattern;
      Canvas.FillRect(Canvas.ClipRect);
    end;

    with LFiller do
    begin
      Min := FTolerance;
      Max := FTolerance;
      
      SetFillPoint(AX, AY);
      Update;
    end;

    for j := 0 to (ADestBmp.Height - 1) do
    begin
      LDestPtr := ADestBmp.ScanLine[j];

      for i := 0 to (ADestBmp.Width - 1) do
      begin
        LMaskPtr := LFiller.ToleranceMaskPtr[i, j];

        if LMaskPtr^ <> 0 then
        begin
          // get filling color
          case FFillSource of
            pbfsForeColor, pbfsBackColor:
              begin
                LFillingColor := FColor;
              end;
              
            pbfsPattern:
              begin
                LFillingColor := LPatternFilledBmp.PixelS[i, j] or $FF000000;
              end;
          end;

          // adjust filling color
          if FColorMode <> pbcmRGB then
          begin
            LFillingColor := GetAdjustedColor(LFillingColor, LDestPtr[i]);
          end;

          // blend
          if FFillSource = pbfsTransparent then
          begin
            LDestPtr[i] := GetTransparentColor(LDestPtr[i], FOpacity);
          end
          else
          begin
            LDestPtr[i] := GetBlendColor(LFillingColor, LDestPtr[i]);
          end;
        end;
      end;
    end;
    
  finally
    LFiller.Free;
    
    LPatternFilledBmp.Canvas.Brush.Bitmap := nil;
    LPatternFilledBmp.Free;
  end;

{$RANGECHECKS ON}
end;

procedure TgmPaintBucket.DiscontiguousFill(
  const ASampleBmp, ADestBmp: TBitmap32; const AX, AY: Integer);
var
  i, j               : Integer;
  LMinR, LMinG, LMinB: Byte;
  LMaxR, LMaxG, LMaxB: Byte;
  r, g, b            : Byte;
  LDestPtr           : PColor32Array;
  LFillingColor      : TColor32;
  LPatternFilledBmp  : TBitmap32;
begin
{$RANGECHECKS OFF}

  if (not Assigned(ASampleBmp)) or
     (not Assigned(ADestBmp)) then
  begin
    Exit;
  end;

  if (ASampleBmp.Width  <> ADestBmp.Width) or
     (ASampleBmp.Height <> ADestBmp.Height) then
  begin
    Exit;
  end;

  if (AX < 0) or
     (AY < 0) or
     (AX >= ADestBmp.Width) or
     (AY >= ADestBmp.Height) then
  begin
    Exit;
  end;

  LFillingColor := FColor;

  RGBColorRange32(ADestBmp.PixelS[AX, AY], FTolerance,
                  LMaxR, LMinR, LMaxG, LMinG, LMaxB, LMinB);

  LPatternFilledBmp := TBitmap32.Create;
  try
    with LPatternFilledBmp do
    begin
      DrawMode := dmOpaque;
      SetSize(ADestBmp.Width, ADestBmp.Height);

      Canvas.Brush.Bitmap := FPattern;
      Canvas.FillRect(Canvas.ClipRect);
    end;

    for j := 0 to (ADestBmp.Height - 1) do
    begin
      LDestPtr := ADestBmp.ScanLine[j];

      for i := 0 to (ADestBmp.Width - 1) do
      begin
        r := LDestPtr[i] shr 16 and $FF;
        g := LDestPtr[i] shr  8 and $FF;
        b := LDestPtr[i]        and $FF;

        if (r in [LMinR..LMaxR]) and
           (g in [LMinG..LMaxG]) and
           (b in [LMinB..LMaxB]) then
        begin
          // get filling color
          case FFillSource of
            pbfsForeColor, pbfsBackColor:
              begin
                LFillingColor := FColor;
              end;
              
            pbfsPattern:
              begin
                LFillingColor := LPatternFilledBmp.PixelS[i, j] or $FF000000;
              end;
          end;

          // adjust filling color
          if FColorMode <> pbcmRGB then
          begin
            LFillingColor := GetAdjustedColor(LFillingColor, LDestPtr[i]);
          end;

          // blend
          LDestPtr[i] := GetBlendColor(LFillingColor, LDestPtr[i]);
        end;
      end;
    end;
    
  finally
    LPatternFilledBmp.Canvas.Brush.Bitmap := nil;
    LPatternFilledBmp.Free;
  end;

{$RANGECHECKS ON}
end;

procedure TgmPaintBucket.Paint(const ASampleBmp, ADestBmp: TBitmap32;
  const AX, AY: Integer);
begin
  case FFillCondition of
    pbfcContiguous:
      begin
        ContiguousFill(ASampleBmp, ADestBmp, AX, AY);
      end;

    pbfcDiscontiguous:
      begin
        DiscontiguousFill(ASampleBmp, ADestBmp, AX, AY);
      end;
  end;
end;

end.
