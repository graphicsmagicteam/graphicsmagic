{ This is the FlareFX plug-in for the GIMP 0.99
  Version 1.05

  Original author is Karl-Johan Andersson.
  Copyright (C) 1997-1998 Karl-Johan Andersson (t96kja@student.tdb.uu.se)

  We adapted the source code for supporting Graphics32.
  CopyRight(C) 2001-2008, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

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
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.}

unit gmFlareFX;

interface

uses
  Math, GR32;

  { Filter Function: X, Y are the coordinates of flare center. }
  procedure GimpFlareFX32(const ASourceBmp, ADestBmp: TBitmap32;
    const AX, AY: Integer);

implementation

type

  TgmRGBFloat = record
    r: Double;
    g: Double;
    b: Double;
  end;

  TgmReflect = record
    CCol : TgmRGBFloat;
    Size : Double;
    xp   : Integer;
    yp   : Integer;
    Types: Integer;
  end;

var
  SColor, SGlow, SInner, SOuter, SHalo: Double;
  Color, Glow, Inner, Outer, Halo     : TgmRGBFloat;

  Ref1  : array [0..18] of TgmReflect;
  NumRef: Integer;

procedure FixPix(var AColor: TColor32; const APercent: Double;
  const AColPro: TgmRGBFloat);
var
  r, g, b: Cardinal;
begin
  r := AColor shr 16 and $FF;
  g := AColor shr  8 and $FF;
  b := AColor        and $FF;

  r := r + Trunc( (255 - r) * APercent * AColPro.r );
  g := g + Trunc( (255 - g) * APercent * AColPro.g );
  b := b + Trunc( (255 - b) * APercent * AColPro.b );

  AColor := (AColor and $FF000000) or (r shl 16) or (g shl 8) or b;
end;

procedure MColor(var AColor: TColor32; const h: Double);
var
  LPercent: Double;
begin
  LPercent := SColor - h;
  LPercent := LPercent / SColor;

  if LPercent > 0.0 then
  begin
    LPercent := LPercent * LPercent;
    FixPix(AColor, LPercent, Color);
  end;
end;

procedure MGlow(var AColor: TColor32; const h: Double);
var
  LPercent: Double;
begin
  LPercent := SGlow - h;
  LPercent := LPercent / SGlow;

  if LPercent > 0.0 then
  begin
    LPercent := LPercent * LPercent;
    FixPix(AColor, LPercent, Glow);
  end;
end;

procedure MInner(var AColor: TColor32; const h: Double);
var
  LPercent: Double;
begin
  LPercent := SInner - h;
  LPercent := LPercent / SInner;

  if LPercent > 0.0 then
  begin
    LPercent := LPercent * LPercent;
    FixPix(AColor, LPercent, Inner);
  end;
end;

procedure MOuter(var AColor: TColor32; const h: Double);
var
  LPercent: Double;
begin
  LPercent := SOuter - h;
  LPercent := LPercent / SOuter;

  if LPercent > 0.0 then
  begin
    FixPix(AColor, LPercent, Outer);
  end;
end;

procedure MHalo(var AColor: TColor32; const h: Double);
var
  LPercent: Double;
begin
  LPercent := h - SHalo;
  LPercent := LPercent / (SHalo * 0.07);
  LPercent := Abs(LPercent);

  if LPercent < 1.0 then
  begin
    FixPix(AColor, 1.0 - LPercent, Halo);
  end;
end;

procedure InitRef(const sx, sy, AWidth, AHeight, AMatt: Integer);
var
  xh, yh, dx, dy: Integer;
begin
  xh              := AWidth  div 2;
  yh              := AHeight div 2;
  dx              := xh - sx;
  dy              := yh - sy;
  NumRef          := 19;
  Ref1[0].Types   := 1;
  Ref1[0].Size    := AMatt * 0.027;
  Ref1[0].xp      := Round(0.6699 * dx + xh);
  Ref1[0].yp      := Round(0.6699 * dy + yh);
  Ref1[0].CCol.r  := 0.0;
  Ref1[0].CCol.g  := 14.0 / 255.0;
  Ref1[0].CCol.b  := 113.0 / 255.0;
  Ref1[1].Types   := 1;
  Ref1[1].Size    := AMatt * 0.01;
  Ref1[1].xp      := Round(0.2692 * dx + xh);
  Ref1[1].yp      := Round(0.2692 * dy + yh);
  Ref1[1].CCol.r  := 90.0 / 255.0;
  Ref1[1].CCol.g  := 181.0 / 255.0;
  Ref1[1].CCol.b  := 142.0 / 255.0;
  Ref1[2].Types   := 1;
  Ref1[2].Size    := AMatt * 0.005;
  Ref1[2].xp      := Round(-0.0112 * dx + xh);
  Ref1[2].yp      := Round(-0.0112 * dy + yh);
  Ref1[2].CCol.r  := 56.0 / 255.0;
  Ref1[2].CCol.g  := 140.0 / 255.0;
  Ref1[2].CCol.b  := 106.0 / 255.0;
  Ref1[3].Types   := 2;
  Ref1[3].Size    := AMatt * 0.031;
  Ref1[3].xp      := Round(0.6490 * dx + xh);
  Ref1[3].yp      := Round(0.6490 * dy + yh);
  Ref1[3].CCol.r  := 9.0 / 255.0;
  Ref1[3].CCol.g  := 29.0 / 255.0;
  Ref1[3].CCol.b  := 19.0 / 255.0;
  Ref1[4].Types   := 2;
  Ref1[4].Size    := AMatt * 0.015;
  Ref1[4].xp      := Round(0.4696 * dx + xh);
  Ref1[4].yp      := Round(0.4696 * dy + yh);
  Ref1[4].CCol.r  := 24.0 / 255.0;
  Ref1[4].CCol.g  := 14.0 / 255.0;
  Ref1[4].CCol.b  := 0.0;
  Ref1[5].Types   := 2;
  Ref1[5].Size    := AMatt * 0.037;
  Ref1[5].xp      := Round(0.4087 * dx + xh);
  Ref1[5].yp      := Round(0.4087 * dy + yh);
  Ref1[5].CCol.r  := 24.0 / 255.0;
  Ref1[5].CCol.g  := 14.0 / 255.0;
  Ref1[5].CCol.b  := 0.0;
  Ref1[6].Types   := 2;
  Ref1[6].Size    := AMatt * 0.022;
  Ref1[6].xp      := Round(-0.2003 * dx + xh);
  Ref1[6].yp      := Round(-0.2003 * dy + yh);
  Ref1[6].CCol.r  := 42.0 / 255.0;
  Ref1[6].CCol.g  := 19.0 / 255.0;
  Ref1[6].CCol.b  := 0.0;
  Ref1[7].Types   := 2;
  Ref1[7].Size    := AMatt * 0.025;
  Ref1[7].xp      := Round(-0.4103 * dx + xh);
  Ref1[7].yp      := Round(-0.4103 * dy + yh);
  Ref1[7].CCol.b  := 17.0 / 255.0;
  Ref1[7].CCol.g  := 9.0 / 255.0;
  Ref1[7].CCol.r  := 0.0;
  Ref1[8].Types   := 2;
  Ref1[8].Size    := AMatt * 0.058;
  Ref1[8].xp      := Round(-0.4503 * dx + xh);
  Ref1[8].yp      := Round(-0.4503 * dy + yh);
  Ref1[8].CCol.b  := 10.0 / 255.0;
  Ref1[8].CCol.g  := 4.0 / 255.0;
  Ref1[8].CCol.r  := 0.0;
  Ref1[9].Types   := 2;
  Ref1[9].Size    := AMatt * 0.017;
  Ref1[9].xp      := Round(-0.5112 * dx + xh);
  Ref1[9].yp      := Round(-0.5112 * dy + yh);
  Ref1[9].CCol.r  := 5.0 / 255.0;
  Ref1[9].CCol.g  := 5.0 / 255.0;
  Ref1[9].CCol.b  := 14.0 / 255.0;
  Ref1[10].Types  := 2;
  Ref1[10].Size   := AMatt * 0.2;
  Ref1[10].xp     := Round(-1.496 * dx + xh);
  Ref1[10].yp     := Round(-1.496 * dy + yh);
  Ref1[10].CCol.r := 9.0 / 255.0;
  Ref1[10].CCol.g := 4.0 / 255.0;
  Ref1[10].CCol.b := 0.0;
  Ref1[11].Types  := 2;
  Ref1[11].Size   := AMatt * 0.5;
  Ref1[11].xp     := Round(-1.496 * dx + xh);
  Ref1[11].yp     := Round(-1.496 * dy + yh);
  Ref1[11].CCol.r := 9.0 / 255.0;
  Ref1[11].CCol.g := 4.0 / 255.0;
  Ref1[11].CCol.b := 0.0;
  Ref1[12].Types  := 3;
  Ref1[12].Size   := AMatt * 0.075;
  Ref1[12].xp     := Round(0.4487 * dx + xh);
  Ref1[12].yp     := Round(0.4487 * dy + yh);
  Ref1[12].CCol.r := 34.0 / 255.0;
  Ref1[12].CCol.g := 19.0 / 255.0;
  Ref1[12].CCol.b := 0.0;
  Ref1[13].Types  := 3;
  Ref1[13].Size   := AMatt * 0.1;
  Ref1[13].xp     := dx + xh;
  Ref1[13].yp     := dy + yh;
  Ref1[13].CCol.r := 14.0 / 255.0;
  Ref1[13].CCol.g := 26.0 / 255.0;
  Ref1[13].CCol.b := 0.0;
  Ref1[14].Types  := 3;
  Ref1[14].Size   := AMatt * 0.039;
  Ref1[14].xp     := Round(-1.301 * dx + xh);
  Ref1[14].yp     := Round(-1.301 * dy + yh);
  Ref1[14].CCol.r := 10.0 / 255.0;
  Ref1[14].CCol.g := 25.0 / 255.0;
  Ref1[14].CCol.b := 13.0 / 255.0;
  Ref1[15].Types  := 4;
  Ref1[15].Size   := AMatt * 0.19;
  Ref1[15].xp     := Round(1.309 * dx + xh);
  Ref1[15].yp     := Round(1.309 * dy + yh);
  Ref1[15].CCol.r := 9.0 / 255.0;
  Ref1[15].CCol.g := 0.0;
  Ref1[15].CCol.b := 17.0 / 255.0;
  Ref1[16].Types  := 4;
  Ref1[16].Size   := AMatt * 0.195;
  Ref1[16].xp     := Round(1.309 * dx + xh);
  Ref1[16].yp     := Round(1.309 * dy + yh);
  Ref1[16].CCol.r := 9.0 / 255.0;
  Ref1[16].CCol.g := 16.0 / 255.0;
  Ref1[16].CCol.b := 5.0 / 255.0;
  Ref1[17].Types  := 4;
  Ref1[17].Size   := AMatt * 0.20;
  Ref1[17].xp     := Round(1.309 * dx + xh);
  Ref1[17].yp     := Round(1.309 * dy + yh);
  Ref1[17].CCol.r := 17.0 / 255.0;
  Ref1[17].CCol.g := 4.0 / 255.0;
  Ref1[17].CCol.b := 0.0;
  Ref1[18].Types  := 4;
  Ref1[18].Size   := AMatt * 0.038;
  Ref1[18].xp     := Round(-1.301 * dx + xh);
  Ref1[18].yp     := Round(-1.301 * dy + yh);
  Ref1[18].CCol.r := 17.0 / 255.0;
  Ref1[18].CCol.g := 4.0 / 255.0;
  Ref1[18].CCol.b := 0.0;
end;

procedure mrt1(var AColor: TColor32; const i, ACol, ARow: Integer);
var
  LPercent: Double;
begin
  LPercent := Ref1[i].Size - Hypot(Ref1[i].xp - ACol, Ref1[i].yp - ARow);
  LPercent := LPercent / Ref1[i].Size;

  if LPercent > 0.0 then
  begin
    LPercent := LPercent * LPercent;
    FixPix(AColor, LPercent, Ref1[i].CCol);
  end;
end;

procedure mrt2(var AColor: TColor32; const i, ACol, ARow: Integer);
var
  LPercent: Double;
begin
  LPercent := Ref1[i].Size - Hypot(Ref1[i].xp - ACol, Ref1[i].yp - ARow);
  LPercent := LPercent / (Ref1[i].Size * 0.15);

  if LPercent > 0.0 then
  begin
    if LPercent > 1.0 then
    begin
      LPercent := 1.0;
    end;
    
    FixPix(AColor, LPercent, Ref1[i].CCol);
  end;
end;

procedure mrt3(var AColor: TColor32; const i, ACol, ARow: Integer);
var
  LPercent: Double;
begin
  LPercent := Ref1[i].Size - Hypot(Ref1[i].xp - ACol, Ref1[i].yp - ARow);
  LPercent := LPercent / (Ref1[i].Size * 0.12);

  if LPercent > 0.0 then
  begin
    if LPercent > 1.0 then
    begin
      LPercent := 1.0 - (LPercent * 0.12);
    end;

    FixPix(AColor, LPercent, Ref1[i].CCol);
  end;
end;

procedure mrt4(var AColor: TColor32; const i, ACol, ARow: Integer);
var
  LPercent: Double;
begin
  LPercent := Hypot(Ref1[i].xp - ACol, Ref1[i].yp - ARow) - Ref1[i].Size;
  LPercent := LPercent / (Ref1[i].Size * 0.04);
  LPercent := Abs(LPercent);

  if LPercent < 1.0 then
  begin
    FixPix(AColor, 1.0 - LPercent, Ref1[i].CCol);
  end;
end;

{ Filter Function: X, Y are the coordinates of flare center. }
procedure GimpFlareFX32(const ASourceBmp, ADestBmp: TBitmap32;
  const AX, AY: Integer);
var
  LCol, LRow, LMatt, i: Integer;
  LWidth, LHeight     : Integer;
  LHyp                : Double;
  LColor              : TColor32;
  r, g, b             : Byte;
  LSrcRow, LDstRow    : PColor32Array;
begin
{$RANGECHECKS OFF}

  if ADestBmp.Width <> ASourceBmp.Width then
  begin
    ADestBmp.Width := ASourceBmp.Width;
  end;

  if ADestBmp.Height <> ASourceBmp.Height then
  begin
    ADestBmp.Height := ASourceBmp.Height;
  end;

  LMatt   := ASourceBmp.Width;
  LWidth  := ASourceBmp.Width;
  LHeight := ASourceBmp.Height;

  SColor := LMatt * 0.0375;
  SGlow  := LMatt * 0.078125;
  SInner := LMatt * 0.1796875;
  SOuter := LMatt * 0.3359375;
  SHalo  := LMatt * 0.084375;

  Color.r := 239.0/255.0;
  Color.g := 239.0/255.0;
  Color.b := 239.0/255.0;
  Glow.r  := 245.0/255.0;
  Glow.g  := 245.0/255.0;
  Glow.b  := 245.0/255.0;
  Inner.r := 255.0/255.0;
  Inner.g := 38.0/255.0;
  Inner.b := 43.0/255.0;
  Outer.r := 69.0/255.0;
  Outer.g := 59.0/255.0;
  Outer.b := 64.0/255.0;
  Halo.r  := 80.0/255.0;
  Halo.g  := 15.0/255.0;
  Halo.b  := 4.0/255.0;

  InitRef(AX, AY, LWidth, LHeight, LMatt);

  {Loop through the rows}
  for LRow := 0 to (ASourceBmp.Height - 1) do
  begin
    LSrcRow := ASourceBmp.ScanLine[LRow];
    LDstRow := ADestBmp.ScanLine[LRow];
    
    for LCol := 0 to (ASourceBmp.Width - 1) do
    begin
      LColor := LSrcRow[LCol];
      LHyp   := Hypot(LCol - AX, LRow - AY);
      
      MColor(LColor, LHyp);  // make color
      MGlow (LColor, LHyp);  // make glow
      MInner(LColor, LHyp);  // make inner
      MOuter(LColor, LHyp);  // make outer
      MHalo (LColor, LHyp);  // make halo

      for i := 0 to (NumRef - 1) do
      begin
        case Ref1[i].Types of
          1:
            begin
              mrt1(LColor, i, LCol, LRow);
            end;

          2:
            begin
              mrt2(LColor, i, LCol, LRow);
            end;
            
          3:
            begin
              mrt3(LColor, i, LCol, LRow);
            end;

          4:
            begin
              mrt4(LColor, i, LCol, LRow);
            end;
        end;
      end;

      r := LColor shr 16 and $FF;
      g := LColor shr  8 and $FF;
      b := LColor        and $FF;

      LDstRow[LCol] := (LSrcRow[LCol] and $FF000000) or (r shl 16) or (g shl 8) or b;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
