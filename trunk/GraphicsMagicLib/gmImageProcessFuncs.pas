unit gmImageProcessFuncs;

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

// Update Date: 2016/04/17

{$WARN UNSAFE_CODE OFF}

interface

uses
{ Standard }
  Graphics,
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmTypes;

type
  TStatisticalMethod = (smMean, smMedian);  // by Michael Hansen
  TChannelReplaceStyle = (crsReplaceDest, crsRemainDest);

{ Image Processing }

  // By Michael Hansen.
  procedure FindEdges(SRC, DST: TBitmap32; const ARadius: Byte = 2;
    const ABalance: Double = 1;
    const AStatisticalMethod: TStatisticalMethod = smMedian;
    const BW: Boolean = True);

  procedure FastBlur(ABitmap32: TBitmap32; const ARadius: Integer;
    const APasses: Integer = 3);

  procedure AdjustImageHLS(ASourceBmp, ADestBmp: TBitmap32;
    const AChangedH, AChangedL, AChangedS: Integer);
    
  procedure AdjustImageHSV(ASourceBmp, ADestBmp: TBitmap32;
    const AChangedH, AChangedS, AChangedV: Integer);

  { Some code is based on Lightness, Contrast and Posterize
    from ProEffectImage.pas, written by Babak Sateli (babak_sateli@yahoo.com)
    http://raveland.netfirms.com
    Special thanks to Jan Verhoeven. }

  procedure Brightness32(const ADestBmp: TBitmap32; const AAmount: Integer); overload;

  procedure Brightness32(ASourceBmp, ADestBmp: TBitmap32; const AAmount: Integer;
    const AChannelSet: TgmChannelSet); overload;
    
  procedure BrightnessAdjustment(const ADestBmp: TBitmap32; const AAmount: Integer);
  
  procedure Contrast32(const ADestBmp: TBitmap32; const AAmount: Integer); overload;

  procedure Contrast32(ASourceBmp, ADestBmp: TBitmap32; const AAmount: Integer;
    const AChannelSet: TgmChannelSet); overload;

  procedure Posterize32(ASourceBmp, ADestBmp: TBitmap32; const ALevel: Byte;
    const AChannelSet: TgmChannelSet);

  procedure Desaturate(ABitmap: TBitmap);

  { Different from TBitmap32.CopyRect, this method will modifies the dimension
    of the destination bitmap. The benefit of this method is that it could
    remains full information about the clipped area. TBitmap32.Draw will
    change the RGB channel if the DrawMode is set to dmBlend and the alpha channel
    is not $FF. }
  procedure CopyRect32WithARGB(ADestBmp, ASourceBmp: TBitmap32;
    const AClipRect: TRect; const AFillColor: TColor32);

  procedure CopyBitmap32(const ADestBmp, ASourceBmp: TBitmap32); overload;
  procedure CopyBitmap32(const ADestBmp, ASourceBmp: TBitmap32; const ARect: TRect); overload;

  // cut bitmap according to the Anchor Direction.
  procedure CutBitmap32ByAnchorDirection(const ASourceBmp, ADestBmp: TBitmap32;
    const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
    const AFillColor: TColor32);

  procedure DrawBitmap32ByAnchorDirection(ASourceBmp, ADestBmp: TBitmap32;
    const AAnchor: TgmAnchorDirection);

  { Relace the RGB channels of the destination bitmap with RGB channels of the source bitmap.}
  procedure ReplaceRGB(ADestBmp, ASourceBmp: TBitmap32);

  // this method will replace the RGB channels of destination with the source
  procedure ReplaceRGBChannels(ASourceBmp, ADestBmp, AMaskBmp: TBitmap32;
    const AChannelSet: TgmChannelSet; const AReplaceStyle: TChannelReplaceStyle); overload;

  // this method will replace the RGB channels of destination with the source
  procedure ReplaceRGBChannels(ASourceBmp, ADestBmp: TBitmap32;
    const AChannelSet: TgmChannelSet; const AReplaceStyle: TChannelReplaceStyle); overload;

  // mask bitmap for the Color Range Selection command
  procedure MakeColorRangeShadow32(ASourceBmp, ADestBmp: TBitmap32;
    const ASampleColor: TColor32; const AFuzziness: Integer);

  procedure InvertBitmap32(ADestBmp: TBitmap32; const AChannelSet: TgmChannelSet);
  procedure Desaturate32(ADestBmp: TBitmap32);
  procedure SaturationAdjustment(const ADestBmp: TBitmap32; const AAmount: Integer);

  procedure ThresholdBitmap32(ASourceBmp, ADestBmp: TBitmap32;
    const ALevel: Byte; const AChannelSet: TgmChannelSet);

  procedure ExtractSingleChannelBitmap32(ADestBmp: TBitmap32;
    const AChannelSet: TgmChannelSet); overload;

  procedure ExtractSingleChannelBitmap32(ASourceBmp, ADestBmp: TBitmap32;
    const AChannelSet: TgmChannelSet); overload;
    
  procedure AdjustBitmapChannels32(ADestBmp: TBitmap32; const AChannelSet: TgmChannelSet);

  procedure FlipBitmap(const ABitmap: TBitmap32; const AFlipMode: TgmFlipMode);
  procedure SmoothResize32(ABmp: TBitmap32; const ANewWidth, ANewHeight: Integer);

  // Rotate bitmap32 -- base on the efg2's rotation routine from www.efg2.com .
  procedure RotateBitmap32(const ASourceBmp, ADestBmp: TBitmap32;
    const ARotateDirection: TgmRotateDirection; const ADeg, ADegHundredths: Integer;
    const ACornerColor: TColor32);

  procedure Transpose(const ABmp: TBitmap32);

  procedure MergeBitmapToColoredBackground(ADestBmp: TBitmap32; const AColor: TColor32);

  procedure GetScaledBitmap(const ASourceBmp, ADestBmp: TBitmap32;
    const AWidth, AHeight: Integer); overload;

  procedure GetScaledBitmap(const ASourceBmp, ADestBmp: TBitmap;
    const AWidth, AHeight: Integer); overload;

  // get scaled width/Height according to old width/height and new width/height
  procedure GetScaledDimension(
    const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer;
    var AScaledWidth, AScaledHeight: Integer);

{ Color processing }

  function InvertRGB(const AColor: TColor32): TColor32;

  function GetRGB(const ABmp: TBitmap32; var APixelArray: TArrayOfColor32): Integer;

  function SetRGB(const ABmp: TBitmap32; const AWidth, AHeight: Integer;
    const APixelArray: TArrayOfColor32): Boolean;

  // an algorithm for image sharpening
  procedure RGBIncDef(const ASourceBmp, ADestBmp: TBitmap32;
    const AFuzziness: Byte; const ACoefficient: Double = 1.0);

implementation

uses
{ Standard }
  SysUtils, Math,
{ Graphics32 }
  GR32_LowLevel, GR32_Blend,
{ GraphicsMagic Lib }
  gmColorSpace, gmColorRange;

{ Image Processing }

procedure FindEdges(SRC, DST: TBitmap32; const ARadius: Byte = 2;
  const ABalance: Double = 1;
  const AStatisticalMethod: TStatisticalMethod = smMedian;
  const BW: Boolean = True);
//By Michael Hansen.

var
  x, y, cx, cy             : Integer;
  D, HR, HB, HG, LR, LG, LB: Cardinal;
  SR, SG, SB, v, i         : Double;
begin
  DST.Assign(SRC);

  D := Sqr(ARadius * 2 + 1);

  for x := 0 to (SRC.Width - 1) do
  begin
    for y := 0 to (SRC.Height - 1) do
    begin
      SR := 0;
      SG := 0;
      SB := 0;

      case AStatisticalMethod of
        smMean:
          begin
            for cx := (x - ARadius) to (x + ARadius) do
            begin
              for cy := (y - ARadius) to (y + ARadius) do
              begin
                SR := SR + SRC.PixelS[cx, cy] shr 16 and $FF;
                SG := SG + SRC.PixelS[cx, cy] shr  8 and $FF;
                SB := SB + SRC.PixelS[cx, cy]        and $FF;
              end;
            end;

            SR:= SR / D;
            SG:= SG / D;
            SB:= SB / D;
          end;

        smMedian:
          begin
            HR := 0;
            HG := 0;
            HB := 0;
            LR := 255;
            LG := 255;
            LB := 255;

            for cx := (x - ARadius) to (x + ARadius) do
            begin
              for cy := (y - ARadius) to (y + ARadius) do
              begin
                D := SRC.PixelS[cx, cy] shr 16 and $FF;

                if D > HR then
                begin
                  HR:= D;
                end;

                if D < LR then
                begin
                  LR:= D;
                end;

                D := SRC.PixelS[cx, cy] shr 8 and $FF;

                if D > HG then
                begin
                  HG:= D;
                end;
                
                if D < LG then
                begin
                  LG:= D;
                end;

                D := SRC.PixelS[cx, cy] and $FF;

                if D > HB then
                begin
                  HB:= D;
                end;

                if D < LB then
                begin
                  LB:= D;
                end;
              end;
            end;

            SR:= (LR + HR) / 2;
            SG:= (LG + HG) / 2;
            SB:= (LB + HB) / 2;
          end;
      end;

      if BW then
      begin
        v := (SR + SG + SB)/3;

        i := ( (SRC.Pixel[x, y] shr 16 and $FF)+
               (SRC.Pixel[x, y] shr  8 and $FF)+
               (SRC.Pixel[x, y]        and $FF) ) / 3;

        v:= v - i;

        if v > ABalance then
        begin
          HR:= 0;
        end
        else
        begin
          HR:= 255;
        end;

        HG:= HR;
        HB:= HR;
      end
      else
      begin
        v:= SR - SRC.Pixel[x, y] shr 16 and $FF;

        if v > ABalance then
        begin
          HR:= 0;
        end
        else
        begin
          HR:= 255;
        end;

        v:= SG - SRC.Pixel[x, y] shr 8 and $FF;

        if v > ABalance then
        begin
          HG:= 0;
        end
        else
        begin
          HG:= 255;
        end;

        v:= SB - SRC.Pixel[x, y] and $FF;

        if v > ABalance then
        begin
          HB:= 0;
        end
        else
        begin
          HB:= 255;
        end;
      end;

      DST.Pixel[x, y]:= (SRC[x, y] and $FF000000) or HR shl 16 or HG shl 8 or HB;
    end;
  end;
end;

procedure FastBlur(ABitmap32: TBitmap32; const ARadius: Integer;
  const APasses: Integer = 3);
// Quick box blur algoritm

// aPasses:
// 1: Blur quality too low
// 2: Best speed / quality compromise
// 3: Good quality but impossible to have a small blur radius. Even radius 1 gives a large blur.

// Implementation by Jan Derk, 2006
// Alpha weighted area version by Michael Hansen, 2008

type
  PSumRecord = ^TSumRecord;
  TSumRecord = packed record
    B, G, R, A: cardinal;
  end;

var
  iPass    : Integer;
  lBoxSize : Cardinal;
  lColor32 : TColor32Entry;
  lHeight1 : Integer;
  lSumArray: array of TSumRecord;
  lWidth1  : Integer;
  x        : Integer;
  xBitmap  : Integer;
  y        : Integer;
  yBitmap  : Integer;
  W        : Cardinal;
begin
  if ARadius <= 0 then
  begin
    Exit;
  end;

  lBoxSize := (ARadius * 2) + 1;
  lWidth1  := ABitmap32.Width  - 1;
  lHeight1 := ABitmap32.Height - 1;

  // Process horizontally
  SetLength(lSumArray, ABitmap32.Width + 2 * ARadius + 1);

  for yBitmap := 0 to lHeight1 do
  begin
    for iPass := 1 to aPasses do
    begin
      // First element is zero
      lSumArray[0].A := 0;
      lSumArray[0].R := 0;
      lSumArray[0].G := 0;
      lSumArray[0].B := 0;

      for x := Low(lSumArray) + 1 to High(lSumArray) do
      begin
        xBitmap := x - aRadius - 1;

        if xBitmap < 0 then
        begin
          xBitmap := 0;
        end
        else
        if xBitmap > lWidth1 then
        begin
          xBitmap := lWidth1;
        end;

        lColor32.ARGB := PColor32(ABitmap32.PixelPtr[xBitmap, yBitmap])^;

        with lColor32 do
        begin
          lSumArray[x].A := lSumArray[x - 1].A + A;
          lSumArray[x].R := lSumArray[x - 1].R + (R * A) div 255;
          lSumArray[x].G := lSumArray[x - 1].G + (G * A) div 255;
          lSumArray[x].B := lSumArray[x - 1].B + (B * A) div 255;
        end;
      end;

      for xBitmap := 0 to lWidth1 do
      begin
        x := xBitmap + ARadius + 1;

        with PColor32Entry(ABitmap32.PixelPtr[xBitmap, yBitmap])^ do
        begin
          W := (lSumArray[x + ARadius].A - lSumArray[x - ARadius - 1].A);
          
          if W > 0 then
          begin
            A := W div lBoxSize;
            R := ((lSumArray[x + ARadius].R - lSumArray[x - ARadius - 1].R) * 255) div W;
            G := ((lSumArray[x + ARadius].G - lSumArray[x - ARadius - 1].G) * 255) div W;
            B := ((lSumArray[x + ARadius].B - lSumArray[x - ARadius - 1].B) * 255) div W;
          end
          else
          begin
            ARGB := 0;
          end;
        end;
      end;
    end;
  end;

  // Process vertically
  SetLength(lSumArray, ABitmap32.Height + 2 * ARadius + 1);

  for xBitmap := 0 to lWidth1 do
  begin
    for iPass := 1 to aPasses do
    begin
      // First element is zero
      lSumArray[0].A := 0;
      lSumArray[0].R := 0;
      lSumArray[0].G := 0;
      lSumArray[0].B := 0;

      for y := Low(lSumArray) + 1 to High(lSumArray) do
      begin
        yBitmap := y - aRadius - 1;

        if yBitmap < 0 then
        begin
          yBitmap := 0;
        end
        else if yBitmap > lHeight1 then
        begin
          yBitmap := lHeight1;
        end;

        lColor32.ARGB := PColor32(ABitmap32.PixelPtr[xBitmap, yBitmap])^;
        
        with lColor32 do
        begin
          lSumArray[y].A := lSumArray[y - 1].A + A;
          lSumArray[y].R := lSumArray[y - 1].R + (R * A) div 255;
          lSumArray[y].G := lSumArray[y - 1].G + (G * A) div 255;
          lSumArray[y].B := lSumArray[y - 1].B + (B * A) div 255;
        end;
      end;

      for yBitmap := 0 to lHeight1 do
      begin
        y := yBitmap + aRadius + 1;
        
        with PColor32Entry(ABitmap32.PixelPtr[xBitmap, yBitmap])^ do
        begin
          W := (lSumArray[y + ARadius].A - lSumArray[y - ARadius - 1].A);
          
          if W > 0 then
          begin
            A := W div lBoxSize;
            R := ((lSumArray[y + ARadius].R - lSumArray[y - ARadius - 1].R) * 255) div W;
            G := ((lSumArray[y + ARadius].G - lSumArray[y - ARadius - 1].G) * 255) div W;
            B := ((lSumArray[y + ARadius].B - lSumArray[y - ARadius - 1].B) * 255) div W;
          end
          else
          begin
            ARGB := 0;
          end;
        end;
      end;
    end;
  end;
end;

// brightness
procedure Brightness32(ASourceBmp, ADestBmp: TBitmap32; const AAmount: Integer;
  const AChannelSet: TgmChannelSet);
var
  i, LChangedAmount: Integer;
  r, g, b          : Byte;
  LSrcBits         : PColor32;
  LDstBits         : PColor32;
begin
  if (ASourceBmp.Width  = ADestBmp.Width) and
     (ASourceBmp.Height = ADestBmp.Height) then
  begin
    LSrcBits := @ASourceBmp.Bits[0];
    LDstBits := @ADestBmp.Bits[0];

    for i := 0 to (ASourceBmp.Width * ASourceBmp.Height - 1) do
    begin
      r := LSrcBits^ shr 16 and $FF;
      g := LSrcBits^ shr 8  and $FF;
      b := LSrcBits^        and $FF;

      if csGrayscale in AChannelSet then
      begin
        LChangedAmount := b + AAmount;
        b              := Clamp(LChangedAmount, 0, 255);
        LDstBits^      := (LSrcBits^ and $FF000000) or (b shl 16) or (b shl 8) or b;
      end
      else
      begin
        if csRed in AChannelSet then
        begin
          LChangedAmount := r + AAmount;
          r              := Clamp(LChangedAmount, 0, 255);
        end;

        if csGreen in AChannelSet then
        begin
          LChangedAmount := g + AAmount;
          g              := Clamp(LChangedAmount, 0, 255);
        end;

        if csBlue in AChannelSet then
        begin
          LChangedAmount := b + AAmount;
          b              := Clamp(LChangedAmount, 0, 255);
        end;

        LDstBits^ := (LSrcBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;
      end;

      Inc(LSrcBits);
      Inc(LDstBits);
    end;
  end;
end;

procedure Brightness32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i, tr, tg, tb: Integer;
  r, g, b      : Byte;
  LBits        : PColor32;
begin
  if (ADestBmp.Width  <= 0) or
     (ADestBmp.Height <= 0) then
  begin
    Exit;
  end;

  LBits := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    // get RGB components
    tr := LBits^ shr 16 and $FF;
    tg := LBits^ shr  8 and $FF;
    tb := LBits^        and $FF;

    // change RGB values
    tr := tr + ( (255 - tr) * AAmount ) div 255;
    tg := tg + ( (255 - tg) * AAmount ) div 255;
    tb := tb + ( (255 - tb) * AAmount ) div 255;

    r := Clamp(tr, 0, 255);
    g := Clamp(tg, 0, 255);
    b := Clamp(tb, 0, 255);

    // composite the original alpha channel with the new RGB values
    LBits^ := (LBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;

    Inc(LBits);
  end;
end;

procedure BrightnessAdjustment(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i: Integer;
  p: PColor32;
begin
  p := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    p^ := Lighten(p^, AAmount);
    Inc(p);
  end;

  EMMS;
end;

// contrast
procedure Contrast32(ASourceBmp, ADestBmp: TBitmap32; const AAmount: Integer;
  const AChannelSet: TgmChannelSet);
var
  i         : Integer;
  ir, ig, ib: Integer;
  r, g, b   : Byte;
  LSrcBits  : PColor32;
  LDstBits  : PColor32;
begin
  if (ASourceBmp.Width  = ADestBmp.Width) and
     (ASourceBmp.Height = ADestBmp.Height) then
  begin
    LSrcBits := @ASourceBmp.Bits[0];
    LDstBits := @ADestBmp.Bits[0];

    for i := 0 to (ASourceBmp.Width * ASourceBmp.Height - 1) do
    begin
      r := LSrcBits^ shr 16 and $FF;
      g := LSrcBits^ shr  8 and $FF;
      b := LSrcBits^        and $FF;

      if csGrayscale in AChannelSet then
      begin
        ib := ( Abs(127 - b) * AAmount ) div 255;

        if b > 127 then
        begin
          ib := b + ib;
        end
        else
        begin
          ib := b - ib;
        end;

        b := Clamp(ib, 0, 255);

        LDstBits^ := (LSrcBits^ and $FF000000) or (b shl 16) or (b shl 8) or b;
      end
      else
      begin
        if csRed in AChannelSet then
        begin
          ir := ( Abs(127 - r) * AAmount ) div 255;

          if r > 127 then
          begin
            ir := r + ir;
          end
          else
          begin
            ir := r - ir;
          end;

          r := Clamp(ir, 0, 255);
        end;

        if csGreen in AChannelSet then
        begin
          ig := ( Abs(127 - g) * AAmount ) div 255;

          if g > 127 then
          begin
            ig := g + ig;
          end
          else
          begin
            ig := g - ig;
          end;

          g := Clamp(ig, 0, 255);
        end;

        if csBlue in AChannelSet then
        begin
          ib := ( Abs(127 - b) * AAmount ) div 255;

          if b > 127 then
          begin
            ib := b + ib;
          end
          else
          begin
            ib := b - ib;
          end;

          b := Clamp(ib, 0, 255);
        end;

        LDstBits^ := (LSrcBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;
      end;
      
      Inc(LSrcBits);
      Inc(LDstBits);
    end;
  end;
end;

// HLS
procedure AdjustImageHLS(ASourceBmp, ADestBmp: TBitmap32;
  const AChangedH, AChangedL, AChangedS: Integer);
var
  A                 : Cardinal;
  H, L, S, I        : Integer;
  LSrcBits, LDstBits: PColor32;
begin
  if (ASourceBmp.Width  = ADestBmp.Width) and
     (ASourceBmp.Height = ADestBmp.Height) then
  begin
    LSrcBits := @ASourceBmp.Bits[0];
    LDstBits := @ADestBmp.Bits[0];

    for I := 0 to (ASourceBmp.Width * ASourceBmp.Height - 1) do
    begin
      A := LSrcBits^ shr 24 and $FF;

      RGBToHLS32(LSrcBits^, H, L, S);

      H := Clamp(H + AChangedH, 0, 360);
      L := Clamp(L + AChangedL, 0, 255);
      S := Clamp(S + AChangedS, 1, 255);

      LDstBits^ := HLSToRGB32(A, H, L, S);

      Inc(LSrcBits);
      Inc(LDstBits);
    end;
  end;
end;

// HSV
procedure AdjustImageHSV(ASourceBmp, ADestBmp: TBitmap32;
  const AChangedH, AChangedS, AChangedV: Integer);
var
  A                 : Cardinal;
  H, S, V, I        : Integer;
  LSrcBits, LDstBits: PColor32;
begin
  if (ASourceBmp.Width  = ADestBmp.Width) and
     (ASourceBmp.Height = ADestBmp.Height) then
  begin
    LSrcBits := @ASourceBmp.Bits[0];
    LDstBits := @ADestBmp.Bits[0];

    for I := 0 to (ASourceBmp.Width * ASourceBmp.Height - 1) do
    begin
      A := LSrcBits^ shr 24 and $FF;

      RGBToHSV32(LSrcBits^, H, S, V);

      H := Clamp(H + AChangedH, 0, 360);
      S := Clamp(S + AChangedS, 0, 255);
      V := Clamp(V + AChangedV, 0, 255);

      LDstBits^ := HSVToRGB32(A, H, S, V);
      
      Inc(LSrcBits);
      Inc(LDstBits);
    end;
  end;
end;

procedure Contrast32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  LBits        : PColor32;
  i, rr, gg, bb: Integer;
  r, g, b      : Byte;
begin
  if (ADestBmp.Width  <= 0) or
     (ADestBmp.Height <= 0) then
  begin
    Exit;
  end;

  LBits := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    // get RGB components
    r := LBits^ shr 16 and $FF;
    g := LBits^ shr  8 and $FF;
    b := LBits^        and $FF;

    // get the changing element
    rr := ( Abs(127 - r) * AAmount ) div 255;
    gg := ( Abs(127 - g) * AAmount ) div 255;
    bb := ( Abs(127 - b) * AAmount ) div 255;

    // change RGB on condition
    if r > 127 then
    begin
      rr := r + rr;
    end
    else
    begin
      rr := r - rr;
    end;
    
    if g > 127 then
    begin
      gg := g + gg;
    end
    else
    begin
      gg := g - gg;
    end;

    if b > 127 then
    begin
      bb := b + bb;
    end
    else
    begin
      bb := b - bb;
    end;

    // Limit the RGB range
    r := Clamp(rr, 0, 255);
    g := Clamp(gg, 0, 255);
    b := Clamp(bb, 0, 255);

    // composite the original alpha channel with the new RGB values
    LBits^ := (LBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;

    Inc(LBits);
  end;
end;

procedure Posterize32(ASourceBmp, ADestBmp: TBitmap32; const ALevel: Byte;
  const AChannelSet: TgmChannelSet);
var
  i       : Integer;
  r, g, b : Cardinal;
  LSrcBits: PColor32;
  LDstBits: PColor32;
begin
  if (ALevel in [2..255]) then
  begin
    if ADestBmp.Width <> ASourceBmp.Width then
    begin
      ADestBmp.Width := ASourceBmp.Width;
    end;

    if ADestBmp.Height <> ASourceBmp.Height then
    begin
      ADestBmp.Height := ASourceBmp.Height;
    end;

    LSrcBits := @ASourceBmp.Bits[0];
    LDstBits := @ADestBmp.Bits[0];

    for i := 0 to (ASourceBmp.Width * ASourceBmp.Height - 1) do
    begin
      if csGrayscale in AChannelSet then
      begin
        b := LSrcBits^ and $FF;
        b := Round(b / ALevel) * ALevel;
        b := Clamp(b, 0, 255);

        LDstBits^ := (LSrcBits^ and $FF000000) or (b shl 16) or (b shl 8) or b;
      end
      else
      begin
        r := LSrcBits^ shr 16 and $FF;
        g := LSrcBits^ shr  8 and $FF;
        b := LSrcBits^        and $FF;

        if csRed in AChannelSet then
        begin
          r := Round(r / ALevel) * ALevel;
          r := Clamp(r, 0, 255);
        end;

        if csGreen in AChannelSet then
        begin
          g := Round(g / ALevel) * ALevel;
          g := Clamp(g, 0, 255);
        end;

        if csBlue in AChannelSet then
        begin
          b := Round(b / ALevel) * ALevel;
          b := Clamp(b, 0, 255);
        end;

        LDstBits^ := (LSrcBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;
      end;

      // shifting pointer to next
      Inc(LSrcBits);
      Inc(LDstBits);
    end;
  end;
end;

{ Different from TBitmap32.CopyRect, this method will modifies the dimension
  of the destination bitmap. The benefit of this method is that it could
  remains full information about the clipped area. TBitmap32.Draw will
  change the RGB channel if the DrawMode is set to dmBlend and the alpha channel
  is not $FF. }
procedure CopyRect32WithARGB(ADestBmp, ASourceBmp: TBitmap32;
  const AClipRect: TRect; const AFillColor: TColor32);
var
  LLeft, LTop     : Integer;
  LWidth, LHeight : Integer;
  i, j            : Integer;
  LSrcRow, LDstRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  LSrcRow := nil;
  LDstRow := nil;

  LLeft   := MinIntValue([AClipRect.Left, AClipRect.Right]);
  LTop    := MinIntValue([AClipRect.Top,  AClipRect.Bottom]);
  LWidth  := Abs(AClipRect.Right  - AClipRect.Left);
  LHeight := Abs(AClipRect.Bottom - AClipRect.Top);

  ADestBmp.Width  := LWidth;
  ADestBmp.Height := LHeight;
  
  ADestBmp.FillRect(0, 0, LWidth, LHeight, AFillColor);

  for j := 0 to (LHeight - 1) do
  begin
    if ( (LTop + j) < 0 ) or ( (LTop + j) > (ASourceBmp.Height - 1) ) then
    begin
      Continue;
    end
    else
    begin
      LSrcRow := ASourceBmp.ScanLine[LTop + j];
      LDstRow := ADestBmp.ScanLine[j];
    end;

    for i := 0 to (LWidth - 1) do
    begin
      if ( (LLeft + i) < 0 ) or ( (LLeft + i) > (ASourceBmp.Width - 1) ) then
      begin
        Continue;
      end
      else
      begin
        LDstRow[I] := LSrcRow[LLeft + i];
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

procedure CopyBitmap32(const ADestBmp, ASourceBmp: TBitmap32);
var
  i                : Integer;
  LSrcBit, LDstBit : PColor32;
begin
  if (ADestBmp.Width <> ASourceBmp.Width) or
     (ADestBmp.Height <> ASourceBmp.Height) then
  begin
    ADestBmp.SetSizeFrom(ASourceBmp);
  end;

  LSrcBit := @ASourceBmp.Bits[0];
  LDstBit := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    LDstBit^ := LSrcBit^;
    
    Inc(LSrcBit);
    Inc(LDstBit);
  end;
end;

procedure CopyBitmap32(const ADestBmp, ASourceBmp: TBitmap32; const ARect: TRect);
var
  i, j, w, h, x, y: Integer;
  LSrcRow, LDstRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  if (ARect.Left >= ARect.Right) or
     (ARect.Top  >= ARect.Bottom) then
  begin
    Exit;
  end;

  if (ADestBmp.Width <> ASourceBmp.Width) or
     (ADestBmp.Height <> ASourceBmp.Height) then
  begin
    ADestBmp.SetSizeFrom(ASourceBmp);
  end;

  w := ARect.Right  - ARect.Left + 1;
  h := ARect.Bottom - ARect.Top  + 1;

  for j := 0 to h do
  begin
    y := j + ARect.Top;

    if (y < 0) or (y >= ADestBmp.Height) then
    begin
      Continue;
    end;

    LSrcRow := ASourceBmp.ScanLine[y];
    LDstRow := ADestBmp.ScanLine[y];

    for i := 0 to w do
    begin
      x := i + ARect.Left;

      if (x < 0) or (x >= ADestBmp.Width) then
      begin
        Continue;
      end;

      LDstRow[x] := LSrcRow[x];
    end;
  end;

{$RANGECHECKS ON}
end;

// cut bitmap according to the Anchor Direction.
procedure CutBitmap32ByAnchorDirection(const ASourceBmp, ADestBmp: TBitmap32;
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const AFillColor: TColor32);
var
  LCutRect : TRect;
begin
  if ASourceBmp.Width > ANewWidth then
  begin
    ADestBmp.Width := ANewWidth;

    case AAnchor of
      adTopLeft, adLeft, adBottomLeft:
        begin
          LCutRect.Left := 0;
        end;

      adTop, adCenter, adBottom:
        begin
          LCutRect.Left := (ASourceBmp.Width - ADestBmp.Width) div 2;
        end;
        
      adTopRight, adRight, adBottomRight:
        begin
          LCutRect.Left := ASourceBmp.Width - ADestBmp.Width;
        end;
    end;

    LCutRect.Right := LCutRect.Left + ADestBmp.Width;
  end
  else
  begin
    ADestBmp.Width := ASourceBmp.Width;
    LCutRect.Left  := 0;
    LCutRect.Right := ASourceBmp.Width;
  end;

  if ASourceBmp.Height > ANewHeight then
  begin
    ADestBmp.Height := ANewHeight;

    case AAnchor of
      adTopLeft, adTop, adTopRight:
        begin
          LCutRect.Top := 0;
        end;
        
      adLeft, adCenter, adRight:
        begin
          LCutRect.Top := (ASourceBmp.Height - ADestBmp.Height) div 2;
        end;
        
      adBottomLeft, adBottom, adBottomRight:
        begin
          LCutRect.Top := ASourceBmp.Height - ADestBmp.Height;
        end;
    end;
    
    LCutRect.Bottom := LCutRect.Top + ADestBmp.Height;
  end
  else
  begin
    ADestBmp.Height := ASourceBmp.Height;
    LCutRect.Top    := 0;
    LCutRect.Bottom := ASourceBmp.Height;
  end;

  CopyRect32WithARGB(ADestBmp, ASourceBmp, LCutRect, AFillColor);
end;

procedure DrawBitmap32ByAnchorDirection(ASourceBmp, ADestBmp: TBitmap32;
  const AAnchor: TgmAnchorDirection);
var
  LTop, LLeft: Integer;
  LDrawMode  : TDrawMode;
begin
  LTop  := 0;
  LLeft := 0;

  case AAnchor of
    adTopLeft:
      begin
        LTop  := 0;
        LLeft := 0;
      end;

    adTop:
      begin
        LTop  := 0;
        LLeft := (ADestBmp.Width - ASourceBmp.Width) div 2;
      end;

    adTopRight:
      begin
        LTop  := 0;
        LLeft := ADestBmp.Width - ASourceBmp.Width;
      end;

    adLeft:
      begin
        LTop  := (ADestBmp.Height - ASourceBmp.Height) div 2;
        LLeft := 0;
      end;

    adCenter:
      begin
        LTop  := (ADestBmp.Height - ASourceBmp.Height) div 2;
        LLeft := (ADestBmp.Width  - ASourceBmp.Width)  div 2;
      end;

    adRight:
      begin
        LTop  := (ADestBmp.Height - ASourceBmp.Height) div 2;
        LLeft := ADestBmp.Width - ASourceBmp.Width;
      end;

    adBottomLeft:
      begin
        LTop  := ADestBmp.Height - ASourceBmp.Height;
        LLeft := 0;
      end;

    adBottom:
      begin
        LTop  := ADestBmp.Height - ASourceBmp.Height;
        LLeft := (ADestBmp.Width - ASourceBmp.Width) div 2;
      end;
      
    adBottomRight:
      begin
        LTop  := ADestBmp.Height - ASourceBmp.Height;
        LLeft := ADestBmp.Width  - ASourceBmp.Width;
      end;
  end;

  LDrawMode           := ASourceBmp.DrawMode;
  ASourceBmp.DrawMode := dmOpaque;

  ADestBmp.Draw(LLeft, LTop, ASourceBmp);
  
  ASourceBmp.DrawMode := LDrawMode;
end;

{ Relace the RGB channels of the destination bitmap with RGB channels of the source bitmap. }
procedure ReplaceRGB(ADestBmp, ASourceBmp: TBitmap32);
var
  i                 : Integer;
  LDstBits, LSrcBits: PColor32;
begin
  if (ADestBmp.Width  <> ASourceBmp.Width) or
     (ADestBmp.Height <> ASourceBmp.Height) then
  begin
    Exit;
  end;

  LDstBits := @ADestBmp.Bits[0];
  LSrcBits := @ASourceBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    LDstBits^ := (LDstBits^ and $FF000000) or (LSrcBits^ and $FFFFFF);

    Inc(LDstBits);
    Inc(LSrcBits);
  end;
end;

// this method will replace the RGB channels of destination with the source
procedure ReplaceRGBChannels(ASourceBmp, ADestBmp, AMaskBmp: TBitmap32;
  const AChannelSet: TgmChannelSet; const AReplaceStyle: TChannelReplaceStyle);
var
  i           : Integer;
  LResultColor: TColor32;
  LSrcBits    : PColor32;
  LDstBits    : PColor32;
  LMaskBits   : PColor32;
begin
  if (ASourceBmp.Width  <> ADestBmp.Width) or
     (ASourceBmp.Height <> ADestBmp.Height) then
  begin
    Exit;
  end;

  if (AMaskBmp.Width  <> ADestBmp.Width) or
     (AMaskBmp.Height <> ADestBmp.Height) then
  begin
    Exit;
  end;

  LSrcBits  := @ASourceBmp.Bits[0];
  LDstBits  := @ADestBmp.Bits[0];
  LMaskBits := @AMaskBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    if (LMaskBits^ and $FF) > 0 then
    begin
      LResultColor := LDstBits^ and $FF000000;

      case AReplaceStyle of
        crsReplaceDest:
          begin
            if csRed in AChannelSet then
            begin
              LResultColor := LResultColor or (LSrcBits^ and $FF0000);
            end
            else
            begin
              LResultColor := LResultColor or (LDstBits^ and $FF0000);
            end;

            if csGreen in AChannelSet then
            begin
              LResultColor := LResultColor or (LSrcBits^ and $FF00);
            end
            else
            begin
              LResultColor := LResultColor or (LDstBits^ and $FF00);
            end;

            if csBlue in AChannelSet then
            begin
              LResultColor := LResultColor or (LSrcBits^ and $FF);
            end
            else
            begin
              LResultColor := LResultColor or (LDstBits^ and $FF);
            end;
          end;

        crsRemainDest:
          begin
            if csRed in AChannelSet then
            begin
              LResultColor := LResultColor or (LDstBits^ and $FF0000);
            end
            else
            begin
              LResultColor := LResultColor or (LSrcBits^ and $FF0000);
            end;

            if csGreen in AChannelSet then
            begin
              LResultColor := LResultColor or (LDstBits^ and $FF00);
            end
            else
            begin
              LResultColor := LResultColor or (LSrcBits^ and $FF00);
            end;

            if csBlue in AChannelSet then
            begin
              LResultColor := LResultColor or (LDstBits^ and $FF);
            end
            else
            begin
              LResultColor := LResultColor or (LSrcBits^ and $FF);
            end;
          end;
      end;

      LDstBits^ := LResultColor;
    end;

    Inc(LSrcBits);
    Inc(LDstBits);
    Inc(LMaskBits);
  end;
end;

// this method will replace the RGB channels of destination with the source
procedure ReplaceRGBChannels(ASourceBmp, ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet; const AReplaceStyle: TChannelReplaceStyle);
var
  i           : Integer;
  LResultColor: TColor32;
  LSrcBits    : PColor32;
  LDstBits    : PColor32;
begin
  if (ASourceBmp.Width  <> ADestBmp.Width) or
     (ASourceBmp.Height <> ADestBmp.Height) then
  begin
    Exit;
  end;

  LSrcBits  := @ASourceBmp.Bits[0];
  LDstBits  := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    LResultColor := LDstBits^ and $FF000000;

    case AReplaceStyle of
      crsReplaceDest:
        begin
          if csRed in AChannelSet then
          begin
            LResultColor := LResultColor or (LSrcBits^ and $FF0000);
          end
          else
          begin
            LResultColor := LResultColor or (LDstBits^ and $FF0000);
          end;

          if csGreen in AChannelSet then
          begin
            LResultColor := LResultColor or (LSrcBits^ and $FF00);
          end
          else
          begin
            LResultColor := LResultColor or (LDstBits^ and $FF00);
          end;

          if csBlue in AChannelSet then
          begin
            LResultColor := LResultColor or (LSrcBits^ and $FF);
          end
          else
          begin
            LResultColor := LResultColor or (LDstBits^ and $FF);
          end;
        end;

      crsRemainDest:
        begin
          if csRed in AChannelSet then
          begin
            LResultColor := LResultColor or (LDstBits^ and $FF0000);
          end
          else
          begin
            LResultColor := LResultColor or (LSrcBits^ and $FF0000);
          end;

          if csGreen in AChannelSet then
          begin
            LResultColor := LResultColor or (LDstBits^ and $FF00);
          end
          else
          begin
            LResultColor := LResultColor or (LSrcBits^ and $FF00);
          end;

          if csBlue in AChannelSet then
          begin
            LResultColor := LResultColor or (LDstBits^ and $FF);
          end
          else
          begin
            LResultColor := LResultColor or (LSrcBits^ and $FF);
          end;
        end;
    end;

    LDstBits^ := LResultColor;

    Inc(LSrcBits);
    Inc(LDstBits);
  end;
end;

// mask bitmap of the selection for Color Range selection tool
procedure MakeColorRangeShadow32(ASourceBmp, ADestBmp: TBitmap32;
  const ASampleColor: TColor32; const AFuzziness: Integer);
var
  i                  : Integer;
  LMinR, LMinG, LMinB: Byte;
  LMaxR, LMaxG, LMaxB: Byte;
  LSrcBits, LDstBits : PColor32;
begin
  if ADestBmp.Width <> ASourceBmp.Width then
  begin
    ADestBmp.Width := ASourceBmp.Width;
  end;

  if ADestBmp.Height <> ASourceBmp.Height then
  begin
    ADestBmp.Height := ASourceBmp.Height;
  end;

  RGBColorRange32(ASampleColor, AFuzziness, LMaxR, LMinR, LMaxG, LMinG, LMaxB, LMinB);

  LSrcBits := @ASourceBmp.Bits[0];
  LDstBits := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    if ( RedComponent  (LSrcBits^) in [LMinR .. LMaxR] ) and
       ( GreenComponent(LSrcBits^) in [LMinG .. LMaxG] ) and
       ( BlueComponent (LSrcBits^) in [LMinB .. LMaxB] ) then
    begin
      LDstBits^ := $FFFFFFFF;
    end
    else
    begin
      LDstBits^ := $FF000000;
    end;
    
    Inc(LSrcBits);
    Inc(LDstBits);
  end;
end;

procedure InvertBitmap32(ADestBmp: TBitmap32; const AChannelSet: TgmChannelSet);
var
  a, r, g, b: Cardinal;
  i         : Integer;
  LBits      : PColor32;
begin
  LBits := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := LBits^ and $FF000000;

    if csGrayscale in AChannelSet then
    begin
      b      := 255 - (LBits^ and $FF);
      LBits^ := a or (b shl 16) or (b shl 8) or b;
    end
    else
    begin
      r := LBits^ shr 16 and $FF;
      g := LBits^ shr  8 and $FF;
      b := LBits^        and $FF;

      if csRed in AChannelSet then
      begin
        r := 255 - r;
      end;

      if csGreen in AChannelSet then
      begin
        g := 255 - g;
      end;

      if csBlue in AChannelSet then
      begin
        b := 255 - b;
      end;

      LBits^ := a or (r shl 16) or (g shl 8) or b;
    end;

    Inc(LBits);
  end;
end;

procedure Desaturate32(ADestBmp: TBitmap32);
var
  i         : Integer;
  a, LIntens: Cardinal;
  r, g, b   : Byte;
  LBits     : PColor32;
begin
  LBits := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := LBits^ and $FF000000;

    if a > 0 then
    begin
      r := LBits^ shr 16 and $FF;
      g := LBits^ shr  8 and $FF;
      b := LBits^        and $FF;

      if (r <> g) or (r <> b) or (g <> b) then
      begin
        LIntens := Round(0.299 * r + 0.587 * g + 0.114 * b);
        LIntens := Clamp(LIntens, 0, 255);
        LBits^  := a or (LIntens shl 16) or (LIntens shl 8) or LIntens;
      end;
    end;

    Inc(LBits);
  end;
end;

procedure SaturationAdjustment(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  A         : Cardinal;
  I, H, L, S: Integer;
  P         : PColor32;
begin
  P := @ADestBmp.Bits[0];

  for I := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    A := P^ shr 24 and $FF;

    RGBToHLS32(P^, H, L, S);

    S  := S + AAmount;
    S  := Clamp(S, 1, 255);
    P^ := HLSToRGB32(A, H, L, S);

    Inc(P);
  end;
end;

procedure ThresholdBitmap32(ASourceBmp, ADestBmp: TBitmap32; const ALevel: Byte;
  const AChannelSet: TgmChannelSet);
var
  a               : Cardinal;
  i               : Integer;
  r, g, b, w      : Byte;
  LSrcBit, LDstBit: PColor32;
begin
  if ADestBmp.Width <> ASourceBmp.Width then
  begin
    ADestBmp.Width := ASourceBmp.Width;
  end;

  if ADestBmp.Height <> ASourceBmp.Height then
  begin
    ADestBmp.Height := ASourceBmp.Height;
  end;

  LSrcBit := @ASourceBmp.Bits[0];
  LDstBit := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := LSrcBit^        and $FF000000;
    r := LSrcBit^ shr 16 and $FF;
    g := LSrcBit^ shr  8 and $FF;
    b := LSrcBit^        and $FF;

    if (csRed   in AChannelSet) and
       (csGreen in AChannelSet) and
       (csBlue  in AChannelSet) then
    begin
      w := (r + g + b) div 3;

      if w > ALevel then
      begin
        w := 255;
      end
      else
      begin
        w := 0;
      end;

      LDstBit^ := a or (w shl 16) or (w shl 8) or w;
    end
    else
    if csGrayscale in AChannelSet then
    begin
      if b > ALevel then
      begin
        b := 255;
      end
      else
      begin
        b := 0;
      end;

      LDstBit^ := a or (b shl 16) or (b shl 8) or b;
    end
    else
    begin
      if csRed in AChannelSet then
      begin
        if r > ALevel then
        begin
          r := 255;
        end
        else
        begin
          r := 0;
        end;
      end;

      if csGreen in AChannelSet then
      begin
        if g > ALevel then
        begin
          g := 255;
        end
        else
        begin
          g := 0;
        end;
      end;

      if csBlue in AChannelSet then
      begin
        if b > ALevel then
        begin
          b := 255;
        end
        else
        begin
          b := 0;
        end;
      end;

      LDstBit^ := a or (r shl 16) or (g shl 8) or b;
    end;

    Inc(LSrcBit);
    Inc(LDstBit);
  end;
end;

procedure ExtractSingleChannelBitmap32(ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
var
  i, LChannelCount: Integer;
  LGray           : Byte;
  LBits           : PColor32;
begin
  LGray := 0;

  if csGrayscale in AChannelSet then
  begin
    Desaturate32(ADestBmp);
  end
  else
  begin
    LChannelCount := 0;

    if csRed in AChannelSet then
    begin
      Inc(LChannelCount);
    end;

    if csGreen in AChannelSet then
    begin
      Inc(LChannelCount);
    end;

    if csBlue in AChannelSet then
    begin
      Inc(LChannelCount);
    end;

    // this method could only processes one channel
    if LChannelCount > 1 then
    begin
      Exit;
    end;

    LBits := @ADestBmp.Bits[0];

    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      if csRed in AChannelSet then
      begin
        LGray := LBits^ shr 16 and $FF;
      end
      else
      if csGreen in AChannelSet then
      begin
        LGray := LBits^ shr 8 and $FF;
      end
      else
      if csBlue in AChannelSet then
      begin
        LGray := LBits^ and $FF;
      end;

      LBits^ := (LBits^ and $FF000000) or (LGray shl 16) or (LGray shl 8) or LGray;

      Inc(LBits);
    end;
  end;
end;

procedure ExtractSingleChannelBitmap32(ASourceBmp, ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
var
  i, LChannelCount: Integer;
  LBits           : PColor32;
  LGray           : Byte;
begin
  LGray := 0;
  
  ADestBmp.Assign(ASourceBmp);

  if csGrayscale in AChannelSet then
  begin
    Desaturate32(ADestBmp);
  end
  else
  begin
    LChannelCount := 0;

    if csRed in AChannelSet then
    begin
      Inc(LChannelCount);
    end;
    
    if csGreen in AChannelSet then
    begin
      Inc(LChannelCount);
    end;

    if csBlue in AChannelSet then
    begin
      Inc(LChannelCount);
    end;

    // this method could only processes one channel
    if LChannelCount > 1 then
    begin
      Exit;
    end;

    LBits := @ADestBmp.Bits[0];

    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      if csRed in AChannelSet then
      begin
        LGray := LBits^ shr 16 and $FF;
      end
      else
      if csGreen in AChannelSet then
      begin
        LGray := LBits^ shr 8 and $FF;
      end
      else
      if csBlue in AChannelSet then
      begin
        LGray := LBits^ and $FF;
      end;

      LBits^ := (LBits^ and $FF000000) or (LGray shl 16) or (LGray shl 8) or LGray;

      Inc(LBits);
    end;
  end;
end;

procedure AdjustBitmapChannels32(ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
var
  i, LChannelCount      : Integer;
  LBits                 : PColor32;
  LAlpha, R, G, B, LGray: Cardinal;
begin
  if AChannelSet = [] then
  begin
    Exit;
  end;

  LGray := 0;

  if csGrayscale in AChannelSet then
  begin
    Desaturate32(ADestBmp);
  end
  else
  begin
    LChannelCount := 0;

    if csRed in AChannelSet then
    begin
      Inc(LChannelCount);
    end;

    if csGreen in AChannelSet then
    begin
      Inc(LChannelCount);
    end;

    if csBlue in AChannelSet then
    begin
      Inc(LChannelCount);
    end;

    LBits := @ADestBmp.Bits[0];

    if LChannelCount = 1 then
    begin
      for i := 0 to (ADestBmp.Width * ADestBmp.Height -1) do
      begin
        LAlpha := LBits^ and $FF000000;

        if csRed in AChannelSet then
        begin
          LGray := LBits^ shr 16 and $FF;
        end
        else
        if csGreen in AChannelSet then
        begin
          LGray := LBits^ shr 8 and $FF;
        end
        else
        if csBlue in AChannelSet then
        begin
          LGray := LBits^ and $FF;
        end;

        LBits^ := LAlpha or (LGray shl 16) or (LGray shl 8) or LGray;

        Inc(LBits);
      end;
    end
    else
    begin
      for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
      begin
        LAlpha := LBits^ and $FF000000;

        if csRed in AChannelSet then
        begin
          R := LBits^ and $FF0000;
        end
        else
        begin
          R := 0;
        end;

        if csGreen in AChannelSet then
        begin
          G := LBits^ and $FF00;
        end
        else
        begin
          G := 0;
        end;

        if csBlue in AChannelSet then
        begin
          B := LBits^ and $FF;
        end
        else
        begin
          B := 0;
        end;

        LBits^ := LAlpha or R or G or B;

        Inc(LBits);
      end;
    end;
  end;
end;

procedure Desaturate(ABitmap: TBitmap);
var
  i, j, LIntens: Integer;
  r, g, b      : Integer;
  LRow         : PRGBTripleArray;
begin
  if ABitmap.PixelFormat <> pf24bit then
  begin
    ABitmap.PixelFormat := pf24bit;
  end;

  for j := 0 to (ABitmap.Height - 1) do
  begin
    LRow := ABitmap.ScanLine[j];

    for i := 0 to (ABitmap.Width - 1) do
    begin
      r := LRow[i].rgbtRed;
      g := LRow[i].rgbtGreen;
      b := LRow[i].rgbtBlue;

      if (r <> g) or (r <> b) or (g <> b) then
      begin
        LIntens := Round(0.299 * r + 0.587 * g + 0.114 * b);
        LIntens := Clamp(LIntens, 0, 255);

        LRow[i].rgbtRed   := LIntens;
        LRow[i].rgbtGreen := LIntens;
        LRow[i].rgbtBlue  := LIntens;
      end;
    end;
  end;
end;

procedure FlipBitmap(const ABitmap: TBitmap32; const AFlipMode: TgmFlipMode);
var
  LSrcBmp         : TBitmap32;
  i, j            : Integer;
  LSrcRow, LDstRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  LSrcBmp := TBitmap32.Create;
  try
    LSrcBmp.Assign(ABitmap);

    case AFlipMode of
      fmHorizontal:
        begin
          for j := 0 to (ABitmap.Height - 1) do
          begin
            LSrcRow := LSrcBmp.ScanLine[j];
            LDstRow := ABitmap.ScanLine[j];

            for i := 0 to (ABitmap.Width - 1) do
            begin
              LDstRow[ABitmap.Width - 1 - i] := LSrcRow[i];
            end;
          end;
        end;

      fmVertical:
        begin
          for j := 0 to (ABitmap.Height - 1) do
          begin
            LSrcRow := LSrcBmp.ScanLine[ABitmap.Height - 1 - j];
            LDstRow := ABitmap.ScanLine[j];

            for i := 0 to (ABitmap.Width - 1) do
            begin
              LDstRow[i] := LSrcRow[i];
            end;
          end;
        end;
    end;
  finally
    LSrcBmp.Free;
  end;

{$RANGECHECKS ON}
end;

procedure SmoothResize32(ABmp: TBitmap32; const ANewWidth, ANewHeight: Integer);
var
  xscale, yscale     : Single;
  sfrom_y, sfrom_x   : Single;
  ifrom_y, ifrom_x   : Integer;
  to_y, to_x         : Integer;
  new_red, new_green : Integer;
  new_blue, new_alpha: Integer;
  bTmp               : TBitmap32;
  slo, OriginalRow   : pColor32Array;
  TA, TR, TG, TB     : Integer;
begin
{$RANGECHECKS OFF}

  if (ABmp.Width  = ANewWidth) and
     (ABmp.Height = ANewHeight) then
  begin
    Exit;
  end;

  bTmp := TBitmap32.Create;
  try
    bTmp.DrawMode := dmBlend;
    bTmp.Assign(ABmp);

    bTmp.Width  := ANewWidth;
    bTmp.Height := ANewHeight;
    xscale      := bTmp.Width  / ABmp.Width;   // original code -- xscale := bTmp.Width  /  (abmp.Width - 1);
    yscale      := bTmp.Height / ABmp.Height;  // original code -- yscale := bTmp.Height  / (abmp.Height - 1);

    for to_y := 0 to (bTmp.Height - 1) do
    begin
      sfrom_y     := to_y / yscale;
      ifrom_y     := Trunc(sfrom_y);
      OriginalRow := ABmp.ScanLine[ifrom_y];

      for to_x := 0 to (bTmp.Width - 1) do
      begin
        sfrom_x := to_x / xscale;
        ifrom_x := Trunc(sfrom_x);

        new_alpha := OriginalRow[ifrom_x] shr 24 and $FF;
        new_red   := OriginalRow[ifrom_x] shr 16 and $FF;
        new_green := OriginalRow[ifrom_x] shr  8 and $FF;
        new_blue  := OriginalRow[ifrom_x]        and $FF;

        TA := new_Alpha;
        TR := new_red;
        TG := new_green;
        TB := new_blue;

        slo       := bTmp.ScanLine[to_y];
        slo[to_x] := (TA shl 24) or (TR shl 16) or (TG shl 8) or TB;
      end;
    end;

    ABmp.Assign(bTmp);

  finally
    bTmp.Free;
  end;

{$RANGECHECKS ON}
end;

{ This version of SmoothResize32() may cause divide by zero error when the
  width or height of the original image is one pixel. But if we use the
  following code

  xscale := bTmp.Width  / ABmp.Width;
  yscale := bTmp.Height / ABmp.Height;

  to instead of the original code

  xscale := bTmp.Width  / (ABmp.Width  - 1);
  yscale := bTmp.Height / (ABmp.Height - 1);

  although we fixed the divide by zero error, but the result of this algorithm
  is not optimal.
}
//procedure SmoothResize32(ABmp: TBitmap32; const ANewWidth, ANewHeight: Integer);
//var
//  xscale, yscale     : Single;
//  sfrom_y, sfrom_x   : Single;
//  ifrom_y, ifrom_x   : Integer;
//  to_y, to_x         : Integer;
//  weight_x, weight_y : array[0..1] of Single;
//  weight             : Single;
//  new_alpha, new_red : Integer;
//  new_green, new_blue: Integer;
//  total_alpha        : Single;
//  total_red          : Single;
//  total_green        : Single;
//  total_blue         : Single;
//  ix, iy             : Integer;
//  bTmp               : TBitmap32;
//  sli, slo           : PColor32Array;
//  a, r, g, b         : Byte;
//begin
//{$RANGECHECKS OFF}
//
//  if (ABmp.Width  = ANewWidth) and
//     (ABmp.Height = ANewHeight) then
//  begin
//    Exit;
//  end;
//
//  bTmp := TBitmap32.Create;
//  try
//    bTmp.Assign(ABmp);
//    bTmp.SetSize(ANewWidth, ANewHeight);
//
//    xscale := bTmp.Width  / (ABmp.Width  - 1);
//    yscale := bTmp.Height / (ABmp.Height - 1);
//
//    for to_y := 0 to (bTmp.Height - 1) do
//    begin
//      sfrom_y := to_y / yscale;
//      ifrom_y := Trunc(sfrom_y);
//
//      weight_y[1] := sfrom_y - ifrom_y;
//      weight_y[0] := 1 - weight_y[1];
//
//      for to_x := 0 to (bTmp.Width - 1) do
//      begin
//        sfrom_x := to_x / xscale;
//        ifrom_x := Trunc(sfrom_x);
//
//        weight_x[1] := sfrom_x - ifrom_x;
//        weight_x[0] := 1 - weight_x[1];
//
//        total_alpha := 0.0;
//        total_red   := 0.0;
//        total_green := 0.0;
//        total_blue  := 0.0;
//
//        for ix := 0 to 1 do
//        begin
//          for iy := 0 to 1 do
//          begin
//            sli       := ABmp.Scanline[ifrom_y + iy];
//            new_alpha := sli[ifrom_x + ix] shr 24 and $FF;
//            new_red   := sli[ifrom_x + ix] shr 16 and $FF;
//            new_green := sli[ifrom_x + ix] shr  8 and $FF;
//            new_blue  := sli[ifrom_x + ix]        and $FF;
//
//            weight      := weight_x[ix] * weight_y[iy];
//            total_alpha := total_alpha + new_alpha * weight;
//            total_red   := total_red   + new_red   * weight;
//            total_green := total_green + new_green * weight;
//            total_blue  := total_blue  + new_blue  * weight;
//          end;
//        end;
//
//        slo := bTmp.ScanLine[to_y];
//        a   := Round(total_alpha);
//        r   := Round(total_red);
//        g   := Round(total_green);
//        b   := Round(total_blue);
//
//        slo[to_x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
//      end;
//    end;
//
//    ABmp.Assign(bTmp);
//  finally
//    bTmp.Free;
//  end;
//
//{$RANGECHECKS ON}
//end;

// Rotate bitmap32 -- based on the efg2's rotation routine from www.efg2.com .
procedure RotateBitmap32(const ASourceBmp, ADestBmp: TBitmap32;
  const ARotateDirection: TgmRotateDirection;
  const ADeg, ADegHundredths: Integer; const ACornerColor: TColor32);
var
  SinTheta, CosTheta, Theta {radians}     : Double;
  i, iRotationAxis, iOriginal             : Integer;
  iPrime, iPrimeRotated                   : Integer;
  j, jRotationAxis, jOriginal             : Integer;
  jPrime, jPrimeRotated                   : Integer;
  OldWidth, OldHeight, NewWidth, NewHeight: Integer;
  SourceRow, DestRow                      : PColor32Array;
begin
{$RANGECHECKS OFF}

  Theta := 0.00;

  // The size of Dest is the same as Source.
  // Axis of rotation is normally center of image

  // Convert degrees to radians.  Use minus sign to force clockwise
  // rotation.
  if ARotateDirection = rdClockwise then
  begin
    Theta := -(ADeg + ADegHundredths / 100) * PI / 180;
  end
  else
  if ARotateDirection = rdCounterclockwise then
  begin
    Theta := (ADeg + ADegHundredths / 100) * PI / 180;
  end;

  SinTheta := Sin(Theta);
  CosTheta := Cos(Theta);

  OldWidth  := ASourceBmp.Width;
  OldHeight := ASourceBmp.Height;

  // An easy way to calculate the non-clipping rectangle
  NewWidth  := Abs( Round(OldHeight * SinTheta) ) +
               Abs( Round(OldWidth  * CosTheta) );

  NewHeight := Abs( Round(OldWidth  * SinTheta) ) +
               Abs( Round(OldHeight * CosTheta) );

  ADestBmp.SetSize(NewWidth, NewHeight);

  iRotationAxis := OldWidth  div 2;
  jRotationAxis := OldHeight div 2;

  // Step through each row of rotated image.
  for j := (ADestBmp.Height - 1) downto 0 do
  begin
    DestRow := ADestBmp.Scanline[j];

    { Assume the bitmap has an even number of pixels in both dimensions and
      the axis of rotation is to be the exact middle of the image -- so
      this axis of rotation is not at the middle of any pixel.

      The transformation (i,j) to (iPrime, jPrime) puts the center of each
      pixel at odd-numbered coordinates.  The left and right sides of each
      pixel (as well as the top and bottom) then have even-numbered
      coordinates.

      The point (iRotationAxis, jRotationAxis) identifies the axis of
      rotation.

      For a 640 x 480 pixel image, the center point is (320, 240).  Pixels
      numbered (index i) 0..319 are left of this point along the "X" axis
      and
      pixels numbered 320..639 are right of this point.  Likewise,
      vertically
      pixels are numbered (index j) 0..239 above the axis of rotation and
      240..479 below the axis of rotation.

      The subtraction (i, j) - (iRotationAxis, jRotationAxis) moves the
      axis of
      rotation from (i, j) to (iRotationAxis, jRotationAxis), which is the
      center of the bitmap in this implementation. }

    // offset origin by the growth factor (NewHeight - OldHeight) div 2
    jPrime := 2 * ( j - (NewHeight - OldHeight) div 2 - jRotationAxis ) + 1 ;

    for i := (ADestBmp.Width - 1) downto 0 do
    begin
      // offset origin by the growth factor (NewWidth - OldWidth) div 2
      iPrime := 2 * ( i - (NewWidth - OldWidth) div 2 - iRotationAxis ) + 1;

      { Rotate (iPrime, jPrime) to location of desired pixel
        Note:  There is negligible difference between floating point and
        scaled integer arithmetic here, so keep the math simple (and
        readable).  }
      iPrimeRotated := Round(iPrime * CosTheta - jPrime * SinTheta);
      jPrimeRotated := Round(iPrime * SinTheta + jPrime * CosTheta);

      { Transform back to pixel coordinates of image, including translation
        of origin from axis of rotation to origin of image. }
      iOriginal := (iPrimeRotated - 1) div 2 + iRotationAxis;
      jOriginal := (jPrimeRotated - 1) div 2 + jRotationAxis;
      
      { Make sure (iOriginal, jOriginal) is in Source.  If not,
        assign blue color to corner points. }
      if (iOriginal >= 0) and
         (jOriginal >= 0) and
         (iOriginal <= ASourceBmp.Width  - 1) and
         (jOriginal <= ASourceBmp.Height - 1) then
      begin
        // Assign pixel from rotated space to current pixel in Dest
        SourceRow  := ASourceBmp.Scanline[jOriginal];
        DestRow[i] := SourceRow[iOriginal];
      end
      else
      begin
        // assign "corner" color
        DestRow[i] := ACornerColor;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

procedure MergeBitmapToColoredBackground(ADestBmp: TBitmap32;
  const AColor: TColor32);
var
  a, br, bg, bb: Byte;
  dr, dg, db   : Byte;
  i            : Integer;
  LBits        : PColor32;
begin
  br    := AColor shr 16 and $FF;
  bg    := AColor shr  8 and $FF;
  bb    := AColor        and $FF;
  LBits := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a  := LBits^ shr 24 and $FF;
    dr := LBits^ shr 16 and $FF;
    dg := LBits^ shr  8 and $FF;
    db := LBits^        and $FF;

    // blend
    dr     := ( dr * a + (255 - a) * br ) div 255;
    dg     := ( dg * a + (255 - a) * bg ) div 255;
    db     := ( db * a + (255 - a) * bb ) div 255;
    LBits^ := $FF000000 or (dr shl 16) or (dg shl 8) or db;
    
    Inc(LBits);
  end;
end;

procedure Transpose(const ABmp: TBitmap32);
var
  x, y, LPixelCount    : Integer;
  LOffset, LIndex      : Integer;
  LWidth, LHeight      : Integer;
  LInPixels, LOutPixels: TArrayOfColor32;
begin
  if ( not Assigned(ABmp) ) or
     ( ABmp.Width = 0 ) or
     ( ABmp.Height = 0 ) then
  begin
    Exit;
  end;

  LWidth      := ABmp.Width;
  LHeight     := ABmp.Height;
  LPixelCount := LWidth * LHeight;

  SetLength(LInPixels,  LPixelCount);
  SetLength(LOutPixels, LPixelCount);
  try
    GetRGB(ABmp, LInPixels);

    // transpose...
    for y := 0 to (LHeight - 1) do
    begin
      LIndex  := y;
      LOffset := y * LWidth;

      for x := 0 to (LWidth - 1) do
      begin
        LOutPixels[LIndex] := LInPixels[LOffset + x];
        LIndex             := LIndex + LHeight;
      end;
    end;

    SetRGB(ABmp, LHeight, LWidth, LOutPixels);

  finally
    SetLength(LInPixels,  0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;
  end;
end;

// get scaled width/Height according to old width/height and new width/height
procedure GetScaledDimension(
  const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer;
  var AScaledWidth, AScaledHeight: Integer);
var
  LWidthFactor : Extended;
  LHeightFactor: Extended;
  LScaleFactor : Extended;
  LScaling     : Boolean;
begin
  LScaleFactor := 0.0;
  LScaling     := False;

  AScaledWidth  := AOldWidth;
  AScaledHeight := AOldHeight;

  if (AOldWidth > ANewWidth) and (AOldHeight > ANewHeight) then
  begin
    LWidthFactor  := AOldWidth  / ANewWidth;
    LHeightFactor := AOldHeight / ANewHeight;

    if LWidthFactor >= LHeightFactor then
    begin
      LScaleFactor := LWidthFactor;
    end
    else
    begin
      LScaleFactor := LHeightFactor;
    end;

    LScaling := True;
  end
  else
  if (AOldWidth > ANewWidth) and (AOldHeight <= ANewHeight) then
  begin
    LScaleFactor := AOldWidth / ANewWidth;
    LScaling     := True;
  end
  else
  if (AOldWidth <= ANewWidth) and (AOldHeight > ANewHeight) then
  begin
    LScaleFactor := AOldHeight / ANewHeight;
    LScaling     := True;
  end;

  if LScaling then
  begin
    AScaledWidth  := Round(AOldWidth  / LScaleFactor);
    AScaledHeight := Round(AOldHeight / LScaleFactor);
  end;
end;

procedure GetScaledBitmap(const ASourceBmp, ADestBmp: TBitmap32;
  const AWidth, AHeight: Integer);
var
  sw, sh: Integer;
begin
  GetScaledDimension(ASourceBmp.Width, ASourceBmp.Height, AWidth, AHeight, sw, sh);
  ADestBmp.Assign(ASourceBmp);
  SmoothResize32(ADestBmp, sw, sh);
end;

procedure GetScaledBitmap(const ASourceBmp, ADestBmp: TBitmap;
  const AWidth, AHeight: Integer);
var
  sw, sh: Integer;
begin
  GetScaledDimension(ASourceBmp.Width, ASourceBmp.Height, AWidth, AHeight, sw, sh);

  ADestBmp.Width       := sw;
  ADestBmp.Height      := sh;
  ADestBmp.PixelFormat := pf24bit;
  
  ADestBmp.Canvas.StretchDraw(ADestBmp.Canvas.ClipRect, ASourceBmp);
end;

function InvertRGB(const AColor: TColor32): TColor32;
var
  r, g, b: Byte;
begin
  r      := 255 - (AColor shr 16 and $FF);
  g      := 255 - (AColor shr  8 and $FF);
  b      := 255 - (AColor        and $FF);
  Result := (AColor and $FF000000) or (r shl 16) or (g shl 8) or b;
end;

function GetRGB(const ABmp: TBitmap32; var APixelArray: TArrayOfColor32): Integer;
var
  LPixelCount, i: Integer;
  LBits         : PColor32;
begin
  Result := 0;

  if ( not Assigned(ABmp) ) or
     ( ABmp.Width  = 0 ) or
     ( ABmp.Height = 0 ) then
  begin
    Exit;
  end;

  LPixelCount := ABmp.Width * ABmp.Height;

  if Length(APixelArray) < LPixelCount then
  begin
    SetLength(APixelArray, LPixelCount);
  end;

  LBits  := @ABmp.Bits[0];

  for i := 0 to (LPixelCount - 1) do
  begin
    APixelArray[i] := LBits^;

    Inc(LBits);
    Inc(Result);
  end;
end;

function SetRGB(const ABmp: TBitmap32; const AWidth, AHeight: Integer;
  const APixelArray: TArrayOfColor32): Boolean;
var
  LItemCount : Integer;
  LPixelCount: Integer;
  i          : Integer;
  LBits      : PColor32;
begin
  Result := False;

  LItemCount  := Length(APixelArray);
  LPixelCount := AWidth * AHeight;
  
  if ( not Assigned(ABmp) ) or
     ( LItemCount  <= 0 ) or
     ( LPixelCount <= 0 ) or
     ( LPixelCount > LItemCount ) then
  begin
    Exit;
  end;

  if (ABmp.Width <> AWidth) or (ABmp.Height <> AHeight) then
  begin
    ABmp.SetSize(AWidth, AHeight);
  end;

  LBits := @ABmp.Bits[0];

  for i := 0 to (LPixelCount - 1) do
  begin
    LBits^ := APixelArray[i];
    Inc(LBits);
  end;

  Result := True;
end;

procedure RGBIncDefHorz(const ASourceBmp, ADestBmp: TBitmap32;
  const AFuzziness: Byte; const ACoefficient: Double);
var
  i, j, dr, dg, db            : Integer;
  LIntensity1, LIntensity2    : Byte;
  LIntensityDiff              : Integer;
  r1, g1, b1, r2, g2, b2      : Byte;
  LScaleR1, LScaleG1, LScaleB1: Double;
  LScaleR2, LScaleG2, LScaleB2: Double;
  c1, c2                      : TColor32;
  a1, a2                      : Cardinal;
  LDestRow, LSourceRow        : PColor32Array;
begin
{$RANGECHECKS OFF}

  for j := 0 to (ADestBmp.Height - 1) do
  begin
    LSourceRow := ASourceBmp.ScanLine[j];
    LDestRow   := ADestBmp.ScanLine[j];

    for i := 0 to (ADestBmp.Width - 2) do
    begin
      c1 := LSourceRow[i];
      c2 := LSourceRow[i + 1];
      a1 := c1 and $FF000000;
      a2 := c2 and $FF000000;

      R1 := c1 shr 16 and $FF;
      G1 := c1 shr  8 and $FF;
      B1 := c1        and $FF;

      R2 := c2 shr 16 and $FF;
      G2 := c2 shr  8 and $FF;
      B2 := c2        and $FF;

      // Y = 0.299R + 0.587G + 0.114B
      LIntensity1    := Round(0.299 * r1 + 0.587 * g1 + 0.114 * b1);
      LIntensity2    := Round(0.299 * r2 + 0.587 * g2 + 0.114 * b2);
      //LIntensity1    := (r1 + g1 + b1) div 3;
      //LIntensity2    := (r2 + g2 + b2) div 3;
      LIntensityDiff := LIntensity1 - LIntensity2;

      if Abs(LIntensityDiff) <= AFuzziness then
      begin
        LScaleR1 := r1 / 255;
        LScaleG1 := g1 / 255;
        LScaleB1 := b1 / 255;

        dr := Round(r1 + LScaleR1 * LIntensityDiff * ACoefficient);
        dg := Round(g1 + LScaleG1 * LIntensityDiff * ACoefficient);
        db := Round(b1 + LScaleB1 * LIntensityDiff * ACoefficient);

        r1 := Clamp(dr, 0, 255);
        g1 := Clamp(dg, 0, 255);
        b1 := Clamp(db, 0, 255);

        LScaleR2 := r2 / 255;
        LScaleG2 := g2 / 255;
        LScaleB2 := b2 / 255;

        dr := Round(r2 - LScaleR2 * LIntensityDiff * ACoefficient);
        dg := Round(g2 - LScaleG2 * LIntensityDiff * ACoefficient);
        db := Round(b2 - LScaleB2 * LIntensityDiff * ACoefficient);

        r2 := Clamp(dr, 0, 255);
        g2 := Clamp(dg, 0, 255);
        b2 := Clamp(db, 0, 255);

        LDestRow[i]     := a1 or (r1 shl 16) or (g1 shl 8) or b1;
        LDestRow[i + 1] := a2 or (r2 shl 16) or (g2 shl 8) or b2;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

procedure RGBIncDefVert(const ASourceBmp, ADestBmp: TBitmap32;
  const AFuzziness: Byte; const ACoefficient: Double);
var
  i, j, dr, dg, db              : Integer;
  LIntensity1, LIntensity2      : Byte;
  LIntensityDiff                : Integer;
  r1, g1, b1, r2, g2, b2        : Byte;
  LScaleR1, LScaleG1, LScaleB1  : Double;
  LScaleR2, LScaleG2, LScaleB2  : Double;
  c1, c2                        : TColor32;
  a1, a2                        : Cardinal;
  LSourceRowArray, LDestRowArray: array of PColor32Array;
begin
{$RANGECHECKS OFF}

  SetLength(LSourceRowArray, ASourceBmp.Height);
  SetLength(LDestRowArray, ADestBmp.Height);
  try
    for j := 0 to (ADestBmp.Height - 1) do
    begin
      LSourceRowArray[j] := ASourceBmp.ScanLine[j];
      LDestRowArray[j]   := ADestBmp.ScanLine[j];
    end;

    for i := 0 to (ADestBmp.Width - 1) do
    begin
      for j := 0 to (ADestBmp.Height - 2) do
      begin
        c1 := LSourceRowArray[j,     i];
        c2 := LSourceRowArray[j + 1, i];
        a1 := c1 and $FF000000;
        a2 := c2 and $FF000000;

        r1 := c1 shr 16 and $FF;
        g1 := c1 shr  8 and $FF;
        b1 := c1        and $FF;

        r2 := c2 shr 16 and $FF;
        g2 := c2 shr  8 and $FF;
        b2 := c2        and $FF;

        // Y = 0.299R + 0.587G + 0.114B
        LIntensity1    := Round(0.299 * r1 + 0.587 * g1 + 0.114 * b1);
        LIntensity2    := Round(0.299 * r2 + 0.587 * g2 + 0.114 * b2);
        //LIntensity1    := (R1 + G1 + B1) div 3;
        //LIntensity2    := (R2 + G2 + B2) div 3;
        LIntensityDiff := LIntensity1 - LIntensity2;

        if Abs(LIntensityDiff) <= AFuzziness then
        begin
          LScaleR1 := r1 / 255;
          LScaleG1 := g1 / 255;
          LScaleB1 := b1 / 255;

          dr := Round(r1 + LScaleR1 * LIntensityDiff * ACoefficient);
          dg := Round(g1 + LScaleG1 * LIntensityDiff * ACoefficient);
          db := Round(b1 + LScaleB1 * LIntensityDiff * ACoefficient);

          r1 := Clamp(dr, 0, 255);
          g1 := Clamp(dg, 0, 255);
          b1 := Clamp(db, 0, 255);

          LScaleR2 := r2 / 255;
          LScaleG2 := g2 / 255;
          LScaleB2 := b2 / 255;

          dr := Round(r2 - LScaleR2 * LIntensityDiff * ACoefficient);
          dg := Round(g2 - LScaleG2 * LIntensityDiff * ACoefficient);
          db := Round(b2 - LScaleB2 * LIntensityDiff * ACoefficient);

          r2 := Clamp(dr, 0, 255);
          g2 := Clamp(dg, 0, 255);
          b2 := Clamp(db, 0, 255);

          LDestRowArray[j,     i] := a1 or (r1 shl 16) or (g1 shl 8) or b1;
          LDestRowArray[j + 1, i] := a2 or (r2 shl 16) or (g2 shl 8) or b2;
        end;
      end;
    end;

  finally
    SetLength(LSourceRowArray, 0);
    SetLength(LDestRowArray, 0);
    LSourceRowArray := nil;
    LDestRowArray   := nil;
  end;

{$RANGECHECKS ON}
end;

// an algorithm for image sharpening
// TODO: improve the algorithm and delete unnecessary code lines
procedure RGBIncDef(const ASourceBmp, ADestBmp: TBitmap32;
  const AFuzziness: Byte; const ACoefficient: Double = 1.0);
var
  LHorzProcBmp  : TBitmap32;
  LVertProcBmp  : TBitmap32;
  i             : Integer;
  hp, vp, dp    : PColor32;
  ha, hr, hg, hb: Byte;
  va, vr, vg, vb: Byte;
  da, dr, dg, db: Byte;
begin
{$RANGECHECKS OFF}

  if (ASourceBmp.Width  <> ADestBmp.Width) or
     (ASourceBmp.Height <> ADestBmp.Height) then
  begin
    Exception.Create('Error: The dimension is different between Source and Dest.');
    Exit;
  end;

  LHorzProcBmp := TBitmap32.Create;
  LVertProcBmp := TBitmap32.Create;
  try
    LHorzProcBmp.Assign(ASourceBmp);
    RGBIncDefHorz(ASourceBmp, LHorzProcBmp, AFuzziness, ACoefficient);

    LVertProcBmp.Assign(ASourceBmp);
    RGBIncDefVert(ASourceBmp, LVertProcBmp, AFuzziness, ACoefficient);

    hp := @LHorzProcBmp.Bits[0];
    vp := @LVertProcBmp.Bits[0];
    dp := @ADestBmp.Bits[0];
    
    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      ha := hp^ shr 24 and $FF;
      hr := hp^ shr 16 and $FF;
      hg := hp^ shr  8 and $FF;
      hb := hp^        and $FF;

      va := vp^ shr 24 and $FF;
      vr := vp^ shr 16 and $FF;
      vg := vp^ shr  8 and $FF;
      vb := vp^        and $FF;

      da := (ha + va) div 2;
      dr := (hr + vr) div 2;
      dg := (hg + vg) div 2;
      db := (hb + vb) div 2;

      dp^ := (da shl 24) or (dr shl 16) or (dg shl 8) or db;

      Inc(hp);
      Inc(vp);
      Inc(dp);
    end;
  finally
    LHorzProcBmp.Free;
    LVertProcBmp.Free;
  end;

{$RANGECHECKS ON}
end; 

end.
