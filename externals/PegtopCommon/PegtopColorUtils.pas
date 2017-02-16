////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorUtils.pas
// Version:    1.02
// Date:       27 Oct 2004 created 1.00
//             27 Jan 2005 modified 1.01 ("pink hue problem" fixed)
//             29 Jan 2005 modified 1.02 (TPegtopColor64 added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// Some simple functions to convert / transform RGB values.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorUtils;

interface

type
  TPegtopColorChannel = (pchRed, pchGreen, pchBlue, pchAlpha);
  TPegtopColorChannels = set of TPegtopColorChannel;

  TPegtopColor = packed record
    case Integer of
      0: (Def: Longword);
      1: (B, G, R, A: Byte);
      2: (Channel: packed array[0..3] of Byte);
  end;
  PPegtopColor = ^TPegtopColor;

  TPegtopColor64 = packed record
    case Integer of
      0: (Def: Int64);
      1: (B, G, R, A: Word);
      2: (Channel: packed array[0..3] of Word);
  end;
  PPegtopColor64 = ^TPegtopColor64;

  TPegtopColorRow = packed array[0..32767] of TPegtopColor;
  PPegtopColorRow = ^TPegtopColorRow;

  TPegtopGrayTransformationTable = array[0..255] of Byte;
  TPegtopRGBTransformationTable = array[0..2] of TPegtopGrayTransformationTable;
  TPegtopRGBATransformationTable = array[0..3] of TPegtopGrayTransformationTable;

  TPegtopColorPalette = array of TPegtopColor;

  TPegtopColorComponent = (pccHue, pccSaturation, pccBrightness, pccRed, pccGreen, pccBlue);

function PegtopColor(Color: Longword): TPegtopColor;

function ConvertRGBToColor(const R, G, B: Byte): TPegtopColor;
function ConvertRGBAToColor(const R, G, B, A: Byte): TPegtopColor;
procedure ConvertColorToRGB(const Color: TPegtopColor; out R, G, B: Byte); overload;
procedure ConvertColorToRGB(const Color: TPegtopColor; out R, G, B: Integer); overload;

function GetSaturatedColor(const Hue: Integer): TPegtopColor;
function GetSaturatedColor64(const Hue: Integer): TPegtopColor64;
function GetValueColor(const Hue, Val: Integer): TPegtopColor;
function ConvertHSBToColor(const Hue, Sat, Bri: Integer): TPegtopColor;
function ConvertHSBToColor64(const Hue, Sat, Bri: Integer): TPegtopColor64;
procedure ConvertColorToHSB(const Color: TPegtopColor; out Hue, Sat, Bri: Integer);
function GetHue(const Color: TPegtopColor): Integer;
function GetSaturation(const Color: TPegtopColor): Integer;
function GetBrightness(const Color: TPegtopColor): Integer;
function ShiftHue(const Color: TPegtopColor; const DeltaHue: Integer): TPegtopColor;
function ShiftChannels(const Color: TPegtopColor; const Delta: Integer): TPegtopColor;

function ContrastColor(const Color: TPegtopColor; const Factor: Integer; const FixValue: Integer = $80): TPegtopColor;
function BrightenColor(const Color: TPegtopColor; const Delta: Integer): TPegtopColor;

function SwapColorBytes(const Color: TPegtopColor): TPegtopColor;
function SwapColorWords(const Color: TPegtopColor64): TPegtopColor64;

function TransformColor(const Color: TPegtopColor; const Table: TPegtopGrayTransformationTable): TPegtopColor; overload;
function TransformColor(const Color: TPegtopColor; const Table: TPegtopRGBTransformationTable): TPegtopColor; overload;

procedure CalcGammaTable(const Gamma: Double; out Table: TPegtopGrayTransformationTable); overload;
procedure CalcGammaTable(const GammaR, GammaG, GammaB: Double; out Table: TPegtopRGBTransformationTable); overload;

function GammaColor(const Color: TPegtopColor; const Gamma: Double): TPegtopColor; overload;
function GammaColor(const Color: TPegtopColor; const GammaR, GammaG, GammaB: Double): TPegtopColor; overload;

function GetGrayLevel(const Color: TPegtopColor): Byte;
function GetContrastColor(const Color: TPegtopColor): TPegtopColor;

function MixColors(const Color1, Color2: TPegtopColor; const Alpha: Integer): TPegtopColor;

function ColorFrom15(Color15: Word): TPegtopColor;
function ColorFrom16(Color16: Word): TPegtopColor;

const
  Hue000 = 0;
  Hue360 = 65536;
  Hue060 = Hue360 div 6;
  Hue120 = Hue360 div 3;
  Hue180 = Hue360 div 2;
  Hue240 = Hue360 * 2 div 3;
  Hue300 = Hue360 * 5 div 6;
  HueMask = 65535;

  Sat100 = 65536;
  SatMask = 65535;

  Bri100 = 65536;
  BriMask = 65535;

implementation

function PegtopColor(Color: Longword): TPegtopColor;
begin
  Result.Def := Color;
end;

function ConvertRGBToColor(const R, G, B: Byte): TPegtopColor;
begin
  Result.B := B;
  Result.G := G;
  Result.R := R;
  Result.A := 0;
end;

function ConvertRGBAToColor(const R, G, B, A: Byte): TPegtopColor;
begin
  Result.B := B;
  Result.G := G;
  Result.R := R;
  Result.A := A;
end;

procedure ConvertColorToRGB(const Color: TPegtopColor; out R, G, B: Byte); overload;
begin
  R := Color.R;
  G := Color.G;
  B := Color.B;
end;

procedure ConvertColorToRGB(const Color: TPegtopColor; out R, G, B: Integer); overload;
begin
  R := Color.R;
  G := Color.G;
  B := Color.B;
end;

function GetSaturatedColor(const Hue: Integer): TPegtopColor;
const
  AddTab: array[0..2] of Word = (Hue240, Hue000, Hue120);
  HueMul = 255 * 6;
var
  I: Integer;
  S: Integer;
  F: Integer;
begin
  for I := 0 to 2 do begin
    S := (Hue + AddTab[I]) and HueMask;
    if S < Hue060 then begin
      F := (S * HueMul) shr 16;
    end
    else if S <= Hue180 then begin
      F := 255;
    end
    else if S < Hue240 then begin
      F := ((Hue240 - S) * HueMul) shr 16;
    end
    else begin
      F := 0;
    end;
    Result.Channel[I] := F;
  end;
  Result.A := 0;
end;

function GetSaturatedColor64(const Hue: Integer): TPegtopColor64;
const
  AddTab: array[0..2] of Word = (Hue240, Hue000, Hue120);
  HueMul = 255 * 6;
var
  I: Integer;
  S: Integer;
  F: Integer;
begin
  for I := 0 to 2 do begin
    S := (Hue + AddTab[I]) and HueMask;
    if S < Hue060 then begin
      F := (S * HueMul) shr 8;
    end
    else if S <= Hue180 then begin
      F := 255 * 256;
    end
    else if S < Hue240 then begin
      F := ((Hue240 - S) * HueMul) shr 8;
    end
    else begin
      F := 0;
    end;
    Result.Channel[I] := F;
  end;
  Result.A := 0;
end;

function GetValueColor(const Hue, Val: Integer): TPegtopColor;
var
  Sat, Bri: Integer;
begin
  If Val < 32768 then begin
    Sat := Sat100;
    Bri := Val shl 1;
  end
  else begin
    Sat := (32768 - Val) shl 1;
    Bri := Bri100;
  end;
  Result := ConvertHSBToColor(Hue, Sat, Bri);
End;

function ConvertHSBToColor(const Hue, Sat, Bri: Integer): TPegtopColor;
begin
  Result := GetSaturatedColor(Hue);
  Result.R := ((255 * 16) - ((255 - Result.R) * Sat shr 12)) * Bri shr 20;
  Result.G := ((255 * 16) - ((255 - Result.G) * Sat shr 12)) * Bri shr 20;
  Result.B := ((255 * 16) - ((255 - Result.B) * Sat shr 12)) * Bri shr 20;
  Result.A := 0;
end;

function ConvertHSBToColor64(const Hue, Sat, Bri: Integer): TPegtopColor64;
begin
  Result := GetSaturatedColor64(Hue);
  Result.R := ((255 * 64) - (((255 * 256 - Result.R) shr 2) * Sat shr 16)) * Bri shr 14;
  Result.G := ((255 * 64) - (((255 * 256 - Result.G) shr 2) * Sat shr 16)) * Bri shr 14;
  Result.B := ((255 * 64) - (((255 * 256 - Result.B) shr 2) * Sat shr 16)) * Bri shr 14;
  Result.A := 0;
end;

procedure ConvertColorToHSB(const Color: TPegtopColor; out Hue, Sat, Bri: Integer);
const
  PureHues: array[0..2] of Word = (Hue240, Hue120, Hue000);
  NextChannels: array[0..2] of Integer = (2, 0, 1);
var
  StrongestChannel, WeakestChannel, MediumChannel: Integer;
  WeakestValue, MediumValue: Word;
  Diff: LongInt;
begin
  if (Color.Channel[0] = Color.Channel[1])
  and (Color.Channel[0] = Color.Channel[2]) then begin // gray
    Hue := 0;
    Sat := 0;
    Bri := Color.Channel[0] shl 8;
  end

  else begin // not gray
    if (Color.Channel[0] >= Color.Channel[1])
    and (Color.Channel[0] >= Color.Channel[2]) then begin
      StrongestChannel := 0; // blue dominates
      if (Color.Channel[1] <= Color.Channel[2]) then WeakestChannel := 1 else WeakestChannel := 2;
    end
    else if (Color.Channel[1] >= Color.Channel[2]) then begin
      StrongestChannel := 1; // green dominates
      if (Color.Channel[0] <= Color.Channel[2]) then WeakestChannel := 0 else WeakestChannel := 2;
    end
    else begin
      StrongestChannel := 2; // red dominates
      if (Color.Channel[0] <= Color.Channel[1]) then WeakestChannel := 0 else WeakestChannel := 1;
    end;
    Bri := Color.Channel[StrongestChannel] * $10000 div 255;

    if Bri = 0 then begin // black
      Sat := 0;
      Hue := 0;
    end

    else begin // not black: "normal" color
      MediumChannel := 3 - WeakestChannel - StrongestChannel;
      WeakestValue := Color.Channel[WeakestChannel] shl 8;
      MediumValue := Color.Channel[MediumChannel] shl 8;
      Sat := $10000 - ((WeakestValue * 256) div Color.Channel[StrongestChannel]);
      Hue := PureHues[StrongestChannel];
      Diff := (((((MediumValue - WeakestValue) * 256) div Color.Channel[StrongestChannel]) * Hue060)) div Sat;
      if MediumChannel = NextChannels[StrongestChannel] then
        Hue := (Hue + Diff) and HueMask
      else
        Hue := (Hue - Diff) and HueMask;
    end;
  end;
end;

function GetHue(const Color: TPegtopColor): Integer;
var
  Sat, Bri: Integer;
begin
  ConvertColorToHSB(Color, Result, Sat, Bri);
end;

function GetSaturation(const Color: TPegtopColor): Integer;
var
  Hue, Bri: Integer;
begin
  ConvertColorToHSB(Color, Hue, Result, Bri);
end;

function GetBrightness(const Color: TPegtopColor): Integer;
begin
  if (Color.R >= Color.G)
  and (Color.R >= Color.B) then begin
    Result := Color.R shl 8; // green dominates
  end
  else if (Color.G >= Color.B) then begin
    Result := Color.G shl 8; // green dominates
  end
  else begin
    Result := Color.B shl 8; // blue dominates
  end;
end;

function ShiftHue(const Color: TPegtopColor; const DeltaHue: Integer): TPegtopColor;
var
  H, S, B: Integer;
begin
  ConvertColorToHSB(Color, H, S, B);
  Result := ConvertHSBToColor(H + DeltaHue, S, B);
end;

function ShiftChannels(const Color: TPegtopColor; const Delta: Integer): TPegtopColor;
var
  D: Integer;
begin
  if Delta < 0 then begin
    D := (-Delta) mod 3;
    if D = 0 then begin
      Result.Channel[0] := Color.Channel[2];
      Result.Channel[1] := Color.Channel[1];
      Result.Channel[2] := Color.Channel[0];
      Result.Channel[3] := Color.Channel[3];
    end
    else if D = 1 then begin
      Result.Channel[0] := Color.Channel[1];
      Result.Channel[1] := Color.Channel[0];
      Result.Channel[2] := Color.Channel[2];
      Result.Channel[3] := Color.Channel[3];
    end
    else if D = 2 then begin
      Result.Channel[0] := Color.Channel[0];
      Result.Channel[1] := Color.Channel[2];
      Result.Channel[2] := Color.Channel[1];
      Result.Channel[3] := Color.Channel[3];
    end;
  end
  else begin
    D := Delta mod 3;
    if D = 0 then Result := Color
    else if D = 1 then begin
      Result.Channel[0] := Color.Channel[1];
      Result.Channel[1] := Color.Channel[2];
      Result.Channel[2] := Color.Channel[0];
      Result.Channel[3] := Color.Channel[3];
    end
    else if D = 2 then begin
      Result.Channel[0] := Color.Channel[2];
      Result.Channel[1] := Color.Channel[0];
      Result.Channel[2] := Color.Channel[1];
      Result.Channel[3] := Color.Channel[3];
    end;
  end;
end;

function ContrastColor(const Color: TPegtopColor; const Factor: Integer; const FixValue: Integer = $80): TPegtopColor;
var
  R, G, B: Integer;
begin
  R := Color.R;
  G := Color.G;
  B := Color.B;
  R := FixValue + ((R - FixValue) * Factor div $100);
  if R < 0 then R := 0 else if R > 255 then R := 255;
  G := FixValue + ((G - FixValue) * Factor div $100);
  if G < 0 then G := 0 else if G > 255 then G := 255;
  B := FixValue + ((B - FixValue) * Factor div $100);
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := ConvertRGBToColor(R, G, B);
end;

function BrightenColor(const Color: TPegtopColor; const Delta: Integer): TPegtopColor;
var
  R, G, B: Integer;
begin
  R := Color.R;
  G := Color.G;
  B := Color.B;
  Inc(R, Delta);
  if R < 0 then R := 0 else if R > 255 then R := 255;
  Inc(G, Delta);
  if G < 0 then G := 0 else if G > 255 then G := 255;
  Inc(B, Delta);
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := ConvertRGBToColor(R, G, B);
end;

Function SwapColorBytes(const Color: TPegtopColor): TPegtopColor;
Begin
  Result.R := Color.B;
  Result.G := Color.G;
  Result.B := Color.R;
  Result.A := Color.A;
End;

Function SwapColorWords(const Color: TPegtopColor64): TPegtopColor64;
Begin
  Result.R := Color.B;
  Result.G := Color.G;
  Result.B := Color.R;
  Result.A := Color.A;
End;

function TransformColor(const Color: TPegtopColor; const Table: TPegtopGrayTransformationTable): TPegtopColor; overload;
begin
  Result.R := Table[Color.R];
  Result.G := Table[Color.G];
  Result.B := Table[Color.B];
  Result.A := 0;
end;

function TransformColor(const Color: TPegtopColor; const Table: TPegtopRGBTransformationTable): TPegtopColor; overload;
begin
  Result.R := Table[0, Color.R];
  Result.G := Table[1, Color.G];
  Result.B := Table[2, Color.B];
  Result.A := 0;
end;

procedure CalcGammaTable(const Gamma: Double; out Table: TPegtopGrayTransformationTable); overload;
var
  I: Integer;
begin
  if (Gamma > 0) then begin
    Table[0] := 0;
    for I := 1 to 255 do begin
      Table[I] := Round(Exp(Ln(I / 255) / Gamma) * 255);
    end;
  end;
end;

procedure CalcGammaTable(const GammaR, GammaG, GammaB: Double; out Table: TPegtopRGBTransformationTable); overload;
var
  I: Integer;
begin
  if (GammaR > 0) and (GammaG > 0) and (GammaB > 0) then begin
    Table[0, 0] := 0;
    Table[1, 0] := 0;
    Table[2, 0] := 0;
    for I := 1 to 255 do begin
      Table[0, I] := Round(Exp(Ln(I / 255) / GammaR) * 255);
      Table[1, I] := Round(Exp(Ln(I / 255) / GammaG) * 255);
      Table[2, I] := Round(Exp(Ln(I / 255) / GammaB) * 255);
    end;
  end;
end;

function GammaColor(const Color: TPegtopColor; const Gamma: Double): TPegtopColor; overload;
var
  R, G, B: Integer;
begin
  if (Gamma > 0) then begin
    R := Color.R;
    G := Color.G;
    B := Color.B;
    if R > 0 then R := Round(Exp(Ln(R / 255) / Gamma) * 255);
    if G > 0 then G := Round(Exp(Ln(G / 255) / Gamma) * 255);
    if B > 0 then B := Round(Exp(Ln(B / 255) / Gamma) * 255);
    Result.R := R;
    Result.G := G;
    Result.B := B;
    Result.A := 0;
  end;
end;

function GammaColor(const Color: TPegtopColor; const GammaR, GammaG, GammaB: Double): TPegtopColor; overload;
var
  R, G, B: Integer;
begin
  if (GammaR > 0) and (GammaG > 0) and (GammaB > 0) then begin
    R := Color.R;
    G := Color.G;
    B := Color.B;
    if R > 0 then R := Round(Exp(Ln(R / 255) / GammaR) * 255);
    if G > 0 then G := Round(Exp(Ln(G / 255) / GammaG) * 255);
    if B > 0 then B := Round(Exp(Ln(B / 255) / GammaB) * 255);
    Result.R := R;
    Result.G := G;
    Result.B := B;
    Result.A := 0;
  end;
end;

function GetGrayLevel(const Color: TPegtopColor): Byte;
begin
  Result := (77 * Color.R + 151 * Color.G + 28 * Color.B) shr 8;
end;

function GetContrastColor(const Color: TPegtopColor): TPegtopColor;
begin
  if GetGrayLevel(Color) < 128 then Result.Def := $00FFFFFF
  else Result.Def := $00000000;
end;

function MixColors(const Color1, Color2: TPegtopColor; const Alpha: Integer): TPegtopColor;
begin
  Result.R := Color1.R + (Color2.R - Color1.R) * Alpha div 256;
  Result.G := Color1.G + (Color2.G - Color1.G) * Alpha div 256;
  Result.B := Color1.B + (Color2.B - Color1.B) * Alpha div 256;
  Result.A := Color1.A + (Color2.A - Color1.A) * Alpha div 256;
end;

function ColorFrom15(Color15: Word): TPegtopColor;
// from Boris Novgorodov
asm
  mov ecx,eax
  and ecx,$1F       //Blue
  imul ecx,541052   //Scaling
  mov edx,eax
  and ecx,$FF0000   //Blue Mask
  and edx,$3E0      //Green
  imul edx,135263
  and eax,$7C00
  shr edx,11
  imul eax,135263    //Red
  and edx,$FF00     //Green Mask
  shr eax,24
  or eax,ecx
  or eax,edx
end;

function ColorFrom16(Color16: Word): TPegtopColor;
// from Boris Novgorodov
asm
  mov ecx,eax
  and ecx,$1F
  imul ecx,541052
  and ecx,$FF0000
  mov edx,eax
  and edx,$7E0
  imul edx,135263
  shr edx,12
  and eax,$F800
  and edx,$FF00
  imul eax,135263
  shr eax,24
  or eax,ecx
  or eax,edx
end;

end.

