{ CopyRight(C) Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  illusion.c  -- This is a plug-in for the GIMP 1.0
  Copyright (C) 1997  Hirotsuna Mizuno
                      s1041150@u-aizu.ac.jp

  Preview and new mode added May 2000 by tim copperfield
  		       timecop@japan.co.jp
  		       http://www.ne.jp/asahi/linux/timecop

  We have translated the C code to Pascal code for supporting TBitmap32.

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

unit gmIllusion;

interface

uses
  Math, GR32, GR32_LowLevel;

type
  TgmIllusionTypes = (itType1, itType2);

  TgmGimpIllusion = class(TObject)
  private
    FSourceBitmap: TBitmap32;
    FDivision    : Integer;
    FType        : TgmIllusionTypes;
  public
    constructor Create(const ASourceBmp: TBitmap32);
    destructor Destroy; override;

    procedure Execute(ADestBmp: TBitmap32);

    property IllusionDivision: Integer          read FDivision write FDivision;
    property IllusionType    : TgmIllusionTypes read FType     write FType;
  end;

implementation

const
  G_PI   = 3.14159265358979323846;
  G_PI_2 = 1.57079632679489661923;

constructor TgmGimpIllusion.Create(const ASourceBmp: TBitmap32);
begin
  inherited Create;

  FSourceBitmap := TBitmap32.Create;
  FSourceBitmap.Assign(ASourceBmp);

  FDivision := 8;
  FType     := itType1;
end; 

destructor TgmGimpIllusion.Destroy;
begin
  FSourceBitmap.Free;
  inherited Destroy;
end; 

procedure TgmGimpIllusion.Execute(ADestBmp: TBitmap32);
var
  x, y, xx, yy           : Integer;
  LWidth, LHeight, LTemp : Integer;
  LCenterX, LCenterY     : Double;
  LScale, LOffset, cx, cy: Double;
  LAngle, LRadius        : Double;
  a, r, g, b             : Byte;
  LSrcRows, LDstRows     : array of PColor32Array;
begin
{$RANGECHECKS OFF}

  if ADestBmp.Width <> FSourceBitmap.Width then
  begin
    ADestBmp.Width := FSourceBitmap.Width;
  end;

  if ADestBmp.Height <> FSourceBitmap.Height then
  begin
    ADestBmp.Height := FSourceBitmap.Height;
  end;

  SetLength(LSrcRows, FSourceBitmap.Height);
  SetLength(LDstRows, FSourceBitmap.Height);

  for y := 0 to (FSourceBitmap.Height - 1) do
  begin
    LSrcRows[y] := FSourceBitmap.ScanLine[y];
    LDstRows[y] := ADestBmp.ScanLine[y];
  end;

  xx       := 0;
  yy       := 0;
  LWidth   := FSourceBitmap.Width;
  LHeight  := FSourceBitmap.Height;
  LCenterX := LWidth  / 2;
  LCenterY := LHeight / 2;

  LScale  := Sqrt(LWidth * LWidth + LHeight * LHeight) / 2;
  LOffset := Round(LScale / 2);

  for y := 0 to (LHeight - 1) do
  begin
    cy := (y - LCenterY) / LScale;

    for x := 0 to (LWidth - 1) do
    begin
      cx      := (x - LCenterX) / LScale;
      LRadius := Sqrt(cx * cx + cy * cy);

      if FDivision <> 0 then
      begin
        LAngle := Floor( ArcTan2(cy, cx) * FDivision / G_PI_2 ) * G_PI_2 / FDivision + (G_PI / FDivision);

        case FType of
          itType1:
            begin
	            xx := Round( x - LOffset * Cos(LAngle) );
	            yy := Round( y - LOffset * Sin(LAngle) );
            end;

          itType2:
            begin
              xx := Round( x - LOffset * Sin(LAngle) );
	            yy := Round( y - LOffset * Cos(LAngle) );
            end;
        end;

        if xx < 0 then
        begin
          xx := 0;
        end
        else
        if LWidth <= xx then
        begin
          xx := LWidth - 1;
        end;

        if yy < 0 then
        begin
          yy := 0;
        end
        else
        if LHeight <= yy then
        begin
          yy := LHeight - 1;
        end;
      end
      else // if FDivision equals to 0 ...
      begin
        xx := 0;
        yy := 0;
      end;

      LTemp := Round( (1 - LRadius) * (LSrcRows[y, x] shr 24 and $FF) + LRadius * (LSrcRows[yy, xx] shr 24 and $FF) );
      a     := Clamp(LTemp, 255);

      LTemp := Round( (1 - LRadius) * (LSrcRows[y, x] shr 16 and $FF) + LRadius * (LSrcRows[yy, xx] shr 16 and $FF) );
      r     := Clamp(LTemp, 255);

      LTemp := Round( (1 - LRadius) * (LSrcRows[y, x] shr 8 and $FF) + LRadius * (LSrcRows[yy, xx] shr 8 and $FF) );
      g     := Clamp(LTemp, 255);

      LTemp := Round( (1 - LRadius) * (LSrcRows[y, x] and $FF) + LRadius * (LSrcRows[yy, xx] and $FF) );
      b     := Clamp(LTemp, 255);

      LDstRows[y, x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
    end;
  end;

  LSrcRows := nil;
  LDstRows := nil;

{$RANGECHECKS ON}
end; 

end.
