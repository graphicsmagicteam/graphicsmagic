////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorFilters.pas
// Version:    1.00
// Date:       24 Jan 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// Color filters, mainly used for dithering.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorFilters;

interface

uses
  Windows, PegtopColorUtils;

type
  TPegtopColorFilter = class
  public
    constructor Create; virtual;
    procedure Apply(var C: TPegtopColor; X, Y: Integer); virtual; abstract;
  end;

  TPegtopColorFilterClass = class of TPegtopColorFilter;

  TPegtopHighColorFilter = class(TPegtopColorFilter)
  private
    FLookupTable: array[0..32] of Byte;
  public
    constructor Create; override;
    procedure Apply(var C: TPegtopColor; X, Y: Integer); override;
  end;

  TPegtopWebSafeColorFilter = class(TPegtopColorFilter)
  private
    FLookupTable: array[0..255] of Byte;
  public
    constructor Create; override;
    procedure Apply(var C: TPegtopColor; X, Y: Integer); override;
  end;

  TPegtopPaletteColorFilter = class(TPegtopColorFilter)
  private
    FPalette: TPegtopColorPalette;
    function GetNearest(const R, G, B: Integer): Integer;
  public
    constructor Create; override;
    procedure Apply(var C: TPegtopColor; X, Y: Integer); override;
    procedure SetPalette(const Palette: TPegtopColorPalette);
  end;

var
  PegtopHighColorFilter: TPegtopColorFilter;
  PegtopLowColorFilter: TPegtopColorFilter;
  PegtopWebSafeColorFilter: TPegtopColorFilter;
  PegtopDitherTable: array[0..15, 0..15] of Byte;

function GetProperColorFilter(Handle: THandle): TPegtopColorFilter;

implementation

procedure InitDitherTable;
  function FindDitherX(I: Integer): Integer;
  var
    J: Integer;
  begin
    Result := 0;
    for J := 0 to 3 do
      Result := Result or ((((((I shr (J * 2)) and 3) + 1) and 2) shr 1) shl (3 - J));
  end;
  function FindDitherY(I: Integer): Integer;
  var
    J: Integer;
  begin
    Result := 0;
    for J := 0 to 3 do
      Result := Result or (((I shr (J * 2)) and 1) shl (3 - J));
  end;
var
  X, Y, I: Integer;
begin
  for Y := 0 to 15 do begin
    for X := 0 to 15 do begin
      I := Y * 16 + X;
      PegtopDitherTable[FindDitherX(I), FindDitherY(I)] := I;
    end;
  end;
end;

function GetProperColorFilter(Handle: THandle): TPegtopColorFilter;
var
  BitsPerPixel: Integer;
begin
  BitsPerPixel := GetDeviceCaps(Handle, BITSPIXEL) * GetDeviceCaps(Handle, PLANES);
  if BitsPerPixel < 15 then
    Result := PegtopLowColorFilter
  else if BitsPerPixel < 24 then
    Result := PegtopHighColorFilter
  else
    Result := NIL;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorFilter
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorFilter.Create;
begin
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopHighColorFilter
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopHighColorFilter.Create;
var
  I: Integer;
begin
  for I := 0 to 32 do begin
    FLookupTable[I] := I * 255 div 32;
  end;
end;

procedure TPegtopHighColorFilter.Apply(var C: TPegtopColor; X, Y: Integer);
var
  D: Integer;
begin
  D := PegtopDitherTable[X and 15, Y and 15];
  if ((C.R and 7) shl 5) > D then C.R := FLookupTable[(C.R shr 3) + 1] else C.R := FLookupTable[C.R shr 3];
  if ((C.G and 7) shl 5) > D then C.G := FLookupTable[(C.G shr 3) + 1] else C.G := FLookupTable[C.G shr 3];
  if ((C.B and 7) shl 5) > D then C.B := FLookupTable[(C.B shr 3) + 1] else C.B := FLookupTable[C.B shr 3];
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopPaletteColorFilter
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopPaletteColorFilter.Create;
const
  PaletteDef: array[0..15] of Integer = (
    $000000, $800000, $008000, $808000,
    $000080, $800080, $008080, $C0C0C0,
    $808080, $FF0000, $00FF00, $FFFF00,
    $0000FF, $FF00FF, $00FFFF, $FFFFFF
  );
var
  I: Integer;
begin
  SetLength(FPalette, Length(PaletteDef));
  for I := Low(PaletteDef) to High(PaletteDef) do
    FPalette[I - Low(PaletteDef)].Def := PaletteDef[I];
end;

procedure TPegtopPaletteColorFilter.Apply(var C: TPegtopColor; X, Y: Integer);
var
  I, J, M: Integer;
  Color: TPegtopColor;
  Error: array[0..2] of Integer;
begin
  Color := FPalette[GetNearest(C.R, C.G, C.B)];
  if Color.Def <> C.Def then begin
    M := (X xor Y) and 3;
    if (Y and 1) <> 0 then M := 7 - M;
    if (Y and 2) <> 0 then M := M xor 3;
    if M > 0 then begin
      for J := 0 to 2 do Error[J] := 0;
      for I := 1 to M do begin
        for J := 0 to 2 do Inc(Error[J], C.Channel[J] - Color.Channel[J]);
        Color := FPalette[GetNearest(C.R + Error[0], C.G + Error[1], C.B + Error[2])];
      end;
    end;
    C := Color;
  end;
end;

function TPegtopPaletteColorFilter.GetNearest(const R, G, B: Integer): Integer;
var
  I: Integer;
  D, Dist: Integer;
begin
  Dist := Sqr(256) * 3;
  Result := 0;
  for I := 0 to Length(FPalette) - 1 do begin
    D := Sqr(R - FPalette[I].R) + Sqr(G - FPalette[I].G) + Sqr(B - FPalette[I].B);
    if D < Dist then begin
      Result := I;
      Dist := D;
    end;
  end;
end;

procedure TPegtopPaletteColorFilter.SetPalette(const Palette: TPegtopColorPalette);
var
  I: Integer;
begin
  if Length(Palette) > 1 then begin
    SetLength(FPalette, Length(Palette));
    for I := 0 to Length(FPalette) - 1 do
      FPalette[I] := Palette[I];
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopWebSafeColorFilter
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopWebSafeColorFilter.Create;
var
  I: Integer;
begin
  for I := 0 to 255 do begin
    FLookupTable[I] := ((I + $19) div $33) * $33;
  end;
end;

procedure TPegtopWebSafeColorFilter.Apply(var C: TPegtopColor; X, Y: Integer);
begin
  C.R := FLookupTable[C.R];
  C.G := FLookupTable[C.G];
  C.B := FLookupTable[C.B];
end;

initialization
  InitDitherTable;
  PegtopHighColorFilter := TPegtopHighColorFilter.Create;
  PegtopLowColorFilter := TPegtopPaletteColorFilter.Create;
  PegtopWebSafeColorFilter := TPegtopWebSafeColorFilter.Create;
finalization
  PegtopHighColorFilter.Free;
  PegtopLowColorFilter.Free;
  PegtopWebSafeColorFilter.Free;
end.
