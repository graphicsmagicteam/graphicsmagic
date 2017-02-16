{ -- LICENSE -------------------------------------------------------------------

  This unit is written by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
  at 2011-02-22.

  Based on app\paint-funcs\paint-funcs.h and
  app\paint-funcs\paint-funcs.c of GIMP 2.6.0

  GIMP - The GNU Image Manipulation Program
  Copyright (C) 1995 Spencer Kimball and Peter Mattis

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit gmGimpPaintFuncs;

interface

uses
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmGimpBaseEnums;

  procedure ConvolveRegion(const ASourceBmp, ADestBmp: TBitmap32;
    const AMatrix: array of Single; const ASize: Integer; const ADivisor: Double;
    const AMode: TgmGimpConvolutionType; const IsAlphaWeighting: Boolean);

implementation

uses
  Math, GR32_LowLevel;

// convolve the src image using the convolution matrix, writing to dest
procedure ConvolveRegion(const ASourceBmp, ADestBmp: TBitmap32;
  const AMatrix: array of Single; const ASize: Integer; const ADivisor: Double;
  const AMode: TgmGimpConvolutionType; const IsAlphaWeighting: Boolean);
var
  x1, y1          : Integer;
  x2, y2          : Integer;
  x, y            : Integer;
  xx, yy          : Integer;
  i, j            : Integer;
  LMargin         : Integer;
  LOffset         : Integer;
  LIndex          : Integer;
  a, r, g, b      : Byte;
  LSumAlpha       : Double;
  LSumRed         : Double;
  LSumGreen       : Double;
  LSumBlue        : Double;
  LWeightedDivisor: Double;
  LMultAlpha      : Double;
  s, d            : PColor32Array;
  LMode           : TgmGimpConvolutionType;
begin
{$RANGECHECKS OFF}

  if not Assigned(ASourceBmp) then
  begin
    Exit;
  end;

  if not Assigned(ADestBmp) then
  begin
    Exit;
  end;

  if (ASourceBmp.Width  <> ADestBmp.Width) or
     (ASourceBmp.Height <> ADestBmp.Height) then
  begin
    ADestBmp.SetSize(ASourceBmp.Width, ASourceBmp.Height);
  end;

  LMode   := AMode;
  LMargin := ASize div 2;
  x1      := 0;
  y1      := 0;
  x2      := ASourceBmp.Width  - 1;
  y2      := ASourceBmp.Height - 1;

  // if the mode is NEGATIVE_CONVOL, the offset should be 128
  if LMode = gctNegativeConvolve then
  begin
    LOffset := 128;
    LMode   := gctNormalConvolve;
  end
  else
  begin
    LOffset := 0;
  end;

  for y := 0 to (ADestBmp.Height - 1) do
  begin
    d := ADestBmp.ScanLine[y];

    if IsAlphaWeighting then
    begin
      for x := 0 to (ADestBmp.Width - 1) do
      begin
        LIndex := 0;

        LSumAlpha := 0.0;
        LSumRed   := 0.0;
        LSumGreen := 0.0;
        LSumBlue  := 0.0;

        LWeightedDivisor := 0.0;

        for j := (y - LMargin) to (y + LMargin) do
        begin
          for i := (x - LMargin) to (x + LMargin) do
          begin
            xx := Clamp(i, x1, x2);
            yy := Clamp(j, y1, y2);

            s := ASourceBmp.ScanLine[yy];
            
            a := s[xx] shr 24 and $FF;
            r := s[xx] shr 16 and $FF;
            g := s[xx] shr  8 and $FF;
            b := s[xx]        and $FF;

            if a > 0 then
            begin
              LMultAlpha := AMatrix[LIndex] * a;

              LWeightedDivisor := LWeightedDivisor + LMultAlpha;

              LSumAlpha := LSumAlpha + LMultAlpha;
              LSumRed   := LSumRed   + LMultAlpha * r;
              LSumGreen := LSumGreen + LMultAlpha * g;
              LSumBlue  := LSumBlue  + LMultAlpha * b;
            end;
            
            Inc(LIndex);
          end;
        end;

        if LWeightedDivisor = 0.0 then
        begin
          LWeightedDivisor := ADivisor;
        end;

        LSumAlpha := LSumAlpha / ADivisor         + LOffset;
        LSumRed   := LSumRed   / LWeightedDivisor + LOffset;
        LSumGreen := LSumGreen / LWeightedDivisor + LOffset;
        LSumBlue  := LSumBlue  / LWeightedDivisor + LOffset;

        if LMode <> gctNormalConvolve then
        begin
          if LSumAlpha < 0.0 then
          begin
            LSumAlpha := -LSumAlpha;
          end;

          if LSumRed < 0.0 then
          begin
            LSumRed := -LSumRed;
          end;

          if LSumGreen < 0.0 then
          begin
            LSumGreen := -LSumGreen;
          end;

          if LSumBlue < 0.0 then
          begin
            LSumBlue := -LSumBlue;
          end;
        end;

        // range checks for alpha
        if LSumAlpha < 0.0 then
        begin
          a := 0;
        end
        else
        if LSumAlpha > 255.0 then
        begin
          a := 255;
        end
        else
        begin
          a := Ceil(LSumAlpha);
        end;

        // range checks for red
        if LSumRed < 0.0 then
        begin
          r := 0;
        end
        else
        if LSumRed > 255.0 then
        begin
          r := 255;
        end
        else
        begin
          r := Ceil(LSumRed);
        end;

        // range checks for green
        if LSumGreen < 0.0 then
        begin
          g := 0;
        end
        else
        if LSumGreen > 255.0 then
        begin
          g := 255;
        end
        else
        begin
          g := Ceil(LSumGreen);
        end;

        // range checks for blue
        if LSumBlue < 0.0 then
        begin
          b := 0;
        end
        else
        if LSumBlue > 255.0 then
        begin
          b := 255;
        end
        else
        begin
          b := Ceil(LSumBlue);
        end;

        d[x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
      end;
    end
    else  //  IsAlphaWeighting = False
    begin
      for x := 0 to (ADestBmp.Width - 1) do
      begin
        LIndex := 0;

        LSumAlpha := 0.0;
        LSumRed   := 0.0;
        LSumGreen := 0.0;
        LSumBlue  := 0.0;

        for j := (y - LMargin) to (y + LMargin) do
        begin
          for i := (x - LMargin) to (x + LMargin) do
          begin
            xx := Clamp(i, x1, x2);
            yy := Clamp(j, y1, y2);

            s := ASourceBmp.ScanLine[yy];

            a := s[xx] shr 24 and $FF;
            r := s[xx] shr 16 and $FF;
            g := s[xx] shr  8 and $FF;
            b := s[xx]        and $FF;

            LSumAlpha := LSumAlpha + AMatrix[LIndex] * a;
            LSumRed   := LSumRed   + AMatrix[LIndex] * r;
            LSumGreen := LSumGreen + AMatrix[LIndex] * g;
            LSumBlue  := LSumBlue  + AMatrix[LIndex] * b;

            Inc(LIndex);
          end;
        end;

        LSumAlpha := LSumAlpha / ADivisor + LOffset;
        LSumRed   := LSumRed   / ADivisor + LOffset;
        LSumGreen := LSumGreen / ADivisor + LOffset;
        LSumBlue  := LSumBlue  / ADivisor + LOffset;

        if (LMode <> gctNormalConvolve) then
        begin
          if LSumAlpha < 0.0 then
          begin
            LSumAlpha := -LSumAlpha;
          end;

          if LSumRed < 0.0 then
          begin
            LSumRed := -LSumRed;
          end;

          if LSumGreen < 0.0 then
          begin
            LSumGreen := -LSumGreen;
          end;

          if LSumBlue < 0.0 then
          begin
            LSumBlue := -LSumBlue;
          end;
        end;

        // range checks for alpha
        if LSumAlpha < 0.0 then
        begin
          a := 0;
        end
        else
        if LSumAlpha > 255.0 then
        begin
          a := 255;
        end
        else
        begin
          a := Ceil(LSumAlpha);
        end;

        // range checks for red
        if LSumRed < 0.0 then
        begin
          r := 0;
        end
        else
        if LSumRed > 255.0 then
        begin
          r := 255;
        end
        else
        begin
          r := Ceil(LSumRed);
        end;

        // range checks for green
        if LSumGreen < 0.0 then
        begin
          g := 0;
        end
        else
        if LSumGreen > 255.0 then
        begin
          g := 255;
        end
        else
        begin
          g := Ceil(LSumGreen);
        end;

        // range checks for blue
        if LSumBlue < 0.0 then
        begin
          b := 0;
        end
        else
        if LSumBlue > 255.0 then
        begin
          b := 255;
        end
        else
        begin
          b := Ceil(LSumBlue);
        end;

        d[x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

end.
