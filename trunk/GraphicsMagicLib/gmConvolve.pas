{ This library created in April/01/2009
  CopyRight(C) 2009, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on "app\paint\gimpconvolve.h", "app\paint\gimpconvolve.c"
  and "app\paint-funcs\paint-funcs.c" from GIMP 2.6.0
  
  The original source can be found at www.gimp.org.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA. }

unit gmConvolve;

{$WARN UNSAFE_CODE OFF}

interface

uses
  GR32;

type
  TgmConvolveType = (gmctNone, gmctBlur, gmctSharpen);

  TgmConvolver = class(TObject)
  private
    FDivisor: Single;
  public
    FMatrix: array [0..8] of Single;

    constructor Create;
    
    procedure Init;
    function SumMatrix: Double;

    procedure CalculateMatrix(const AType: TgmConvolveType;
      const RadiusX, RadiusY: Integer; const Rate: Double);

    procedure ConvolveRegion(SourceBitmap, DestBitmap: TBitmap32;
      const Size: Integer; const AlphaWeighting: Boolean);
  end;

implementation

uses
  Math, GR32_LowLevel, SysUtils;

const
  MIN_BLUR    = 64;    // (8/9 original pixel)
  MAX_BLUR    = 0.25;  // (1/33 original pixel)
  MIN_SHARPEN = -512;
  MAX_SHARPEN = -64;

constructor TgmConvolver.Create;
begin
  inherited Create;
  Init;
end;

procedure TgmConvolver.Init;
var
  i: Integer;
begin
  FDivisor := 9.0;

  for i := 0 to 8 do
  begin
    FMatrix[i] := 1.0;
  end;
end;

procedure TgmConvolver.CalculateMatrix(const AType: TgmConvolveType;
  const RadiusX, RadiusY: Integer; const Rate: Double);
var
  LPercent: Double;
begin
  LPercent := Min(Rate / 100.0, 1.0);

  if (RadiusX <> 0) and (RadiusY <> 0) then
  begin
    FMatrix[0] := 1.0;
  end
  else
  begin
    FMatrix[0] := 0.0;
  end;

  if RadiusY <> 0 then
  begin
    FMatrix[1] := 1.0;
  end
  else
  begin
    FMatrix[1] := 0.0;
  end;

  if (RadiusX <> 0) and (RadiusY <> 0) then
  begin
    FMatrix[2] := 1.0;
  end
  else
  begin
    FMatrix[2] := 0.0;
  end;

  if RadiusX <> 0 then
  begin
    FMatrix[3] := 1.0;
  end
  else
  begin
    FMatrix[3] := 0.0;
  end;

  // get the appropriate convolution matrix and size and divisor
  case AType of
    gmctBlur:
      begin
        FMatrix[4] := MIN_BLUR + LPercent * (MAX_BLUR - MIN_BLUR);
      end;

    gmctSharpen:
      begin
        FMatrix[4] := MIN_SHARPEN + LPercent * (MAX_SHARPEN - MIN_SHARPEN);
      end;
  end;

  if RadiusX <> 0 then
  begin
    FMatrix[5] := 1.0;
  end
  else
  begin
    FMatrix[5] := 0.0;
  end;

  if (RadiusX <> 0) and (RadiusY <> 0) then
  begin
    FMatrix[6] := 1.0;
  end
  else
  begin
    FMatrix[6] := 0.0;
  end;

  if RadiusY <> 0 then
  begin
    FMatrix[7] := 1.0;
  end
  else
  begin
    FMatrix[7] := 0.0;
  end;

  if (RadiusX <> 0) and (RadiusY <> 0) then
  begin
    FMatrix[8] := 1.0;
  end
  else
  begin
    FMatrix[8] := 0.0;
  end;

  FDivisor := SumMatrix;
end;

function TgmConvolver.SumMatrix: Double;
var
  i: Integer;
begin
  Result := 0.0;

  for i := 0 to 8 do
  begin
    Result := Result + FMatrix[i];
  end;
end;

procedure TgmConvolver.ConvolveRegion(SourceBitmap, DestBitmap: TBitmap32;
  const Size: Integer; const AlphaWeighting: Boolean);
var
  x, y, xx, yy         : Integer;
  LMargin, i, j, LIndex: Integer;
  a, r, g, b           : Byte;
  LWeightedDivisor     : Double;
  LMultAlpha           : Double;
  LTotal               : array [0..3] of Double;  // order is b, g, r, a
  LSrcPixel            : TColor32;
begin
  LMargin := Size div 2;

  for y := 0 to DestBitmap.Height - 1 do
  begin
    for x := 0 to DestBitmap.Width - 1 do
    begin
      if AlphaWeighting then
      begin
        LIndex := 0;

        for i := 0 to 3 do
        begin
          LTotal[i] := 0.0;
        end;

        LWeightedDivisor := 0.0;

        for j := (y - LMargin) to (y + LMargin) do
        begin
          for i := (x - LMargin) to (x + LMargin) do
          begin
            xx        := Clamp(i, 0, SourceBitmap.Width  - 1);
            yy        := Clamp(j, 0, SourceBitmap.Height - 1);
            LSrcPixel := SourceBitmap.PixelS[xx, yy];

            a := LSrcPixel shr 24 and $FF;
            r := LSrcPixel shr 16 and $FF;
            g := LSrcPixel shr  8 and $FF;
            b := LSrcPixel        and $FF;

            if a > 0 then
            begin
              LMultAlpha       := FMatrix[LIndex] * a;
              LWeightedDivisor := LWeightedDivisor + LMultAlpha;

              LTotal[0] := LTotal[0] + LMultAlpha * b;
              LTotal[1] := LTotal[1] + LMultAlpha * g;
              LTotal[2] := LTotal[2] + LMultAlpha * r;
              LTotal[3] := LTotal[3] + LMultAlpha;
            end;

            Inc(LIndex);
          end;
        end;

        if LWeightedDivisor = 0.0 then
        begin
          LWeightedDivisor := FDivisor;
        end;

        LTotal[0] := LTotal[0] / LWeightedDivisor;  // blue
        LTotal[1] := LTotal[1] / LWeightedDivisor;  // green
        LTotal[2] := LTotal[2] / LWeightedDivisor;  // red
        LTotal[3] := LTotal[3] / FDivisor;          // alpha

        // blue
        if LTotal[0] < 0.0 then
        begin
          b := 0;
        end
        else if LTotal[0] > 255.0 then
        begin
          b := 255;
        end
        else
        begin
          b := Round(LTotal[0]);
        end;

        // green
        if LTotal[1] < 0.0 then
        begin
          g := 0;
        end
        else if LTotal[1] > 255.0 then
        begin
          g := 255;
        end
        else
        begin
          g := Round(LTotal[1]);
        end;

        // red
        if LTotal[2] < 0.0 then
        begin
          r := 0;
        end
        else if LTotal[2] > 255.0 then
        begin
          r := 255;
        end
        else
        begin
          r := Round(LTotal[2]);
        end;

        // alpha
        if LTotal[3] < 0.0 then
        begin
          a := 0;
        end
        else if LTotal[3] > 255.0 then
        begin
          a := 255;
        end
        else
        begin
          a := Round(LTotal[3]);
        end;

        DestBitmap.PixelS[x, y] := (a shl 24) or (r shl 16) or (g shl 8) or b;
      end
      else
      begin
        LIndex := 0;

        for i := 0 to 3 do
        begin
          LTotal[i] := 0.0;
        end;

        for j := (y - LMargin) to (y + LMargin) do
        begin
          for i := (x - LMargin) to (x + LMargin) do
          begin
            xx        := Clamp(i, 0, SourceBitmap.Width  - 1);
            yy        := Clamp(j, 0, SourceBitmap.Height - 1);
            LSrcPixel := SourceBitmap.PixelS[xx, yy];

            a := LSrcPixel shr 24 and $FF;
            r := LSrcPixel shr 16 and $FF;
            g := LSrcPixel shr  8 and $FF;
            b := LSrcPixel        and $FF;

            LTotal[0] := LTotal[0] + FMatrix[LIndex] * b;  // blue
            LTotal[1] := LTotal[1] + FMatrix[LIndex] * g;  // green
            LTotal[2] := LTotal[2] + FMatrix[LIndex] * r;  // red
            LTotal[3] := LTotal[3] + FMatrix[LIndex] * a;  // alpha

            Inc(LIndex);
          end;
        end;

        for i := 0 to 3 do
        begin
          LTotal[i] := LTotal[i] / FDivisor;
        end;

        // blue
        if LTotal[0] < 0.0 then
        begin
          b := 0;
        end
        else if LTotal[0] > 255.0 then
        begin
          b := 255;
        end
        else
        begin
          b := Round(LTotal[0]);
        end;

        // green
        if LTotal[1] < 0.0 then
        begin
          g := 0;
        end
        else if LTotal[1] > 255.0 then
        begin
          g := 255;
        end
        else
        begin
          g := Round(LTotal[1]);
        end;

        // red
        if LTotal[2] < 0.0 then
        begin
          r := 0;
        end
        else if LTotal[2] > 255.0 then
        begin
          r := 255;
        end
        else
        begin
          r := Round(LTotal[2]);
        end;

        // alpha
        if LTotal[3] < 0.0 then
        begin
          a := 0;
        end
        else if LTotal[3] > 255.0 then
        begin
          a := 255;
        end
        else
        begin
          a := Round(LTotal[3]);
        end;

        DestBitmap.PixelS[x, y] := (a shl 24) or (r shl 16) or (g shl 8) or b;
      end;
    end;
  end;
end; 

end.
