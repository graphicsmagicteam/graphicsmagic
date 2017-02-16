{ Copyright (c) 2008 by Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on "emboss.c" from GIMP 2.2.10 .
  The original source can be found at www.gimp.org.

  file: emboss/emboss.c

  Copyright (c) 1997 Eric L. Hernes (erich@rrnet.com)
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. The name of the author may not be used to endorse or promote products
     derived from this software withough specific prior written permission

  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  $Id: emboss.c,v 1.50.2.1 2005/02/21 16:13:07 neo Exp $

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit gmGimpEmboss;

interface

uses
{ Standard }
  SysUtils, Math,
{ Graphics32 }
  GR32, GR32_LowLevel;

type
  TgmColorChannel = (ccRed, ccGreen, ccBlue, ccIntensity);
  TgmFunctionMode = (fmBumpmap, fmEmboss);

  TgmEmbossFilter = class(TObject)
  private
    FLx  : Double;
    FLy  : Double;
    FLz  : Double;
    FNz  : Double;
    FNz2 : Double;
    FNzLz: Double;
    Fbg  : Double;

    FAzimuth  : Double;
    FElevation: Double;
    FDepth    : Integer;
    FEmbossP  : TgmFunctionMode;
    FChannel  : TgmColorChannel;
    FPreview  : Boolean;
    
    procedure EmbossInit(const Azimuth, Elevation: Double; const Width45: Word);

    procedure EmbossRow(s1, s2, s3, ATexture, ADst: PColor32Array;
      const xSize: Cardinal);
  public
    constructor Create;
    procedure Emboss(const ADestBmp: TBitmap32);

    property Azimuth  : Double          read FAzimuth   write FAzimuth;
    property Elevation: Double          read FElevation write FElevation;
    property Depth    : Integer         read FDepth     write FDepth;
    property EmbossP  : TgmFunctionMode read FEmbossP   write FEmbossP;
    property Channel  : TgmColorChannel read FChannel   write FChannel;
    property IsPreview: Boolean         read FPreview   write FPreview;
  end;

implementation

const
  PixelScale = 255.9;

function ARGBToGray(const AColor: TColor32): TColor32;
var
  LGray: Byte;
begin
  LGray  := Intensity(AColor);
  Result := (AColor and $FF000000) or (LGray shl 16) or (LGray shl 8) or LGray;
end;

constructor TgmEmbossFilter.Create;
begin
  inherited Create;

  FAzimuth   := 30.0;
  FElevation := 45.0;
  FDepth     := 20;
  FEmbossP   := fmEmboss;
  FPreview   := True;
  FChannel   := ccRed;
end;

procedure TgmEmbossFilter.EmbossInit(const Azimuth, Elevation: Double;
  const Width45: Word);
begin
  { compute the light vector from the input parameters.
    normalize the length to PixelScale for fast shading calculation. }

  FLx := Cos(Azimuth) * Cos(Elevation) * PixelScale;
  FLy := Sin(Azimuth) * Cos(Elevation) * PixelScale;
  FLz := Sin(Elevation) * PixelScale;

  { constant z component of image surface normal - this depends on the
    image slope we wish to associate with an angle of 45 degrees, which
    depends on the width of the filter used to produce the source image. }

  FNz   := (6 * 255) / Width45;
  FNz2  := FNz * FNz;
  FNzLz := FNz * FLz;

  // optimization for vertical normals: L.[0 0 1]
  Fbg := FLz;
end;

{ ANSI C code from the article
  "Fast Embossing Effects on Raster Image Data"
  by John Schlag, jfs@kerner.com
  in "Graphics Gems IV", Academic Press, 1994


  Emboss - shade 24-bit pixels using a single distant light source.
  Normals are obtained by differentiating a monochrome 'bump' image.
  The unary case ('texture' == NULL) uses the shading result as output.
  The binary case multiples the optional 'texture' image by the shade.
  Images are in row major order with interleaved color components (rgbrgb...).
  E.g., component c of pixel x,y of 'dst' is dst[3*(y*xSize + x) + c]. }

procedure TgmEmbossFilter.EmbossRow(s1, s2, s3, ATexture, ADst: PColor32Array;
  const xSize: Cardinal);
var
  Nx, Ny, NdotL  : Integer;
  x, LTempShade  : Integer;
  r, g, b, LShade: Byte;
begin
{$RANGECHECKS OFF}

  Nx := 0;
  Ny := 0;

  // grayscale the first and last pixels of the scanline
  if FEmbossP = fmEmboss then
  begin
    ADst[0]         := ARGBToGray(ADst[0]);
    ADst[xSize - 1] := ARGBToGray(ADst[xSize - 1]);
  end;

  // mung pixels, avoiding edge pixels
  for x := 1 to xSize - 2 do
  begin
    { compute the normal from the src map. the type of the
      expression before the cast is compiler dependent. in
      some cases the sum is unsigned, in others it is
      signed. ergo, cast to signed. }

    case FChannel of
      ccRed: // the defualt
        begin
          Nx := RedComponent(s1[x - 1]) +
                RedComponent(s2[x - 1]) +
                RedComponent(s3[x - 1]) -
                RedComponent(s1[x + 1]) -
                RedComponent(s2[x + 1]) -
                RedComponent(s3[x + 1]);

          Ny := RedComponent(s3[x - 1]) +
                RedComponent(s3[x])     +
                RedComponent(s3[x + 1]) -
                RedComponent(s1[x - 1]) -
                RedComponent(s1[x])     -
                RedComponent(s1[x + 1]);
        end;

      ccGreen:
        begin
          Nx := GreenComponent(s1[x - 1]) +
                GreenComponent(s2[x - 1]) +
                GreenComponent(s3[x - 1]) -
                GreenComponent(s1[x + 1]) -
                GreenComponent(s2[x + 1]) -
                GreenComponent(s3[x + 1]);

          Ny := GreenComponent(s3[x - 1]) +
                GreenComponent(s3[x])     +
                GreenComponent(s3[x + 1]) -
                GreenComponent(s1[x - 1]) -
                GreenComponent(s1[x])     -
                GreenComponent(s1[x + 1]);
        end;

      ccBlue:
        begin
          Nx := BlueComponent(s1[x - 1]) +
                BlueComponent(s2[x - 1]) +
                BlueComponent(s3[x - 1]) -
                BlueComponent(s1[x + 1]) -
                BlueComponent(s2[x + 1]) -
                BlueComponent(s3[x + 1]);

          Ny := BlueComponent(s3[x - 1]) +
                BlueComponent(s3[x])     +
                BlueComponent(s3[x + 1]) -
                BlueComponent(s1[x - 1]) -
                BlueComponent(s1[x])     -
                BlueComponent(s1[x + 1]);
        end;

      ccIntensity:
        begin
          Nx := Intensity(s1[x - 1]) +
                Intensity(s2[x - 1]) +
                Intensity(s3[x - 1]) -
                Intensity(s1[x + 1]) -
                Intensity(s2[x + 1]) -
                Intensity(s3[x + 1]);

          Ny := Intensity(s3[x - 1]) +
                Intensity(s3[x])     +
                Intensity(s3[x + 1]) -
                Intensity(s1[x - 1]) -
                Intensity(s1[x])     -
                Intensity(s1[x + 1]);
        end;
    end;

    NdotL := Round(Nx * FLx + Ny * FLy + FNzLz);

    // shade with distant light source
    if (Nx = 0) and (Ny = 0) then
    begin
      LTempShade := Round(Fbg);
    end
    else if NdotL < 0 then
    begin
      LTempShade := 0;
    end
    else
    begin
      LTempShade := Round( NdotL / Sqrt(Nx * Nx + Ny * Ny + FNz2) );
    end;

    LShade := Clamp(LTempShade, 0, 255);

    // do something with the shading result
    if Assigned(ATexture) then
    begin
      r := ATexture[x] shr 16 and $FF;
      g := ATexture[x] shr  8 and $FF;
      b := ATexture[x]        and $FF;

      r := r * LShade shr 8;
      g := g * LShade shr 8;
      b := b * LShade shr 8;
      
      ADst[x] := (ADst[x] and $FF000000) or (r shl 16) or (g shl 8) or b;
    end
    else
    begin
      ADst[x] := (ADst[x] and $FF000000) or (LShade shl 16) or (LShade shl 8) or LShade;
    end;
  end;

{$RANGECHECKS ON}
end;

procedure TgmEmbossFilter.Emboss(const ADestBmp: TBitmap32);
var
  LSourceBmp                  : TBitmap32;
  x, y                        : Integer;
  LWidth, LHeight             : Cardinal;
  LSrcRow1, LSrcRow2, LSrcRow3: PColor32Array;
  LDstRow, LTextureRow        : PColor32Array;
begin
{$RANGECHECKS OFF}

  LTextureRow := nil;
  
  LWidth  := ADestBmp.Width;
  LHeight := ADestBmp.Height;

  LSourceBmp := TBitmap32.Create;
  try
    LSourceBmp.Assign(ADestBmp);
    EmbossInit(DegToRad(FAzimuth), DegToRad(FElevation), FDepth);

    // first row
    LSrcRow1 := LSourceBmp.ScanLine[0];
    LSrcRow2 := LSourceBmp.ScanLine[1];
    LSrcRow3 := LSourceBmp.ScanLine[2];

    for x := 0 to (LWidth - 1) do
    begin
      LSrcRow1[x] := LSrcRow2[x];
    end;

    case FEmbossP of
      fmBumpmap:
        begin
          LTextureRow := LSrcRow1;
        end;

      fmEmboss:
        begin
          LTextureRow := nil;
        end;
    end;

    LDstRow := ADestBmp.ScanLine[0];

    EmbossRow(LSrcRow1, LSrcRow2, LSrcRow3, LTextureRow, LDstRow, LWidth);

    // last row
    LSrcRow1 := LSourceBmp.ScanLine[LHeight - 3];
    LSrcRow2 := LSourceBmp.ScanLine[LHeight - 2];
    LSrcRow3 := LSourceBmp.ScanLine[LHeight - 1];

    for x := 0 to (LWidth - 1) do
    begin
      LSrcRow3[x] := LSrcRow2[x];
    end;

    case FEmbossP of
      fmBumpmap:
        begin
          LTextureRow := LSrcRow1;
        end;

      fmEmboss:
        begin
          LTextureRow := nil;
        end;
    end;

    LDstRow := ADestBmp.ScanLine[LHeight - 1];

    EmbossRow(LSrcRow1, LSrcRow2, LSrcRow3, LTextureRow, LDstRow, LWidth);

    for y := 0 to (LHeight - 3) do
    begin
      LSrcRow1 := LSourceBmp.ScanLine[y];
      LSrcRow2 := LSourceBmp.ScanLine[y + 1];
      LSrcRow3 := LSourceBmp.ScanLine[y + 2];

      LDstRow  := ADestBmp.ScanLine[y + 1];

      case FEmbossP of
        fmBumpmap:
          begin
            LTextureRow := LSrcRow1;
          end;
          
        fmEmboss:
          begin
            LTextureRow := nil;
          end;
      end;

      EmbossRow(LSrcRow1, LSrcRow2, LSrcRow3, LTextureRow, LDstRow, LWidth);
    end;

  finally
    LSourceBmp.Free;
  end;

{$RANGECHECKS ON}
end; 

end.
