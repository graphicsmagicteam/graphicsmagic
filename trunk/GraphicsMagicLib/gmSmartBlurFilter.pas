unit gmSmartBlurFilter;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Implementing Smart Blur in Java
 * http://asserttrue.blogspot.com/2010/08/implementing-smart-blur-in-java.html
 *
 * The Initial Developer of the Original Code is
 * Kas Thomas
 *
 * The Pascal code in this unit is based on the Original Code written by
 * Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
{ Delphi }
  Classes,
{ Graphics32 }
  GR32;

type
  TgmSmartBlurFilter = class(TPersistent)
  private
    FRadius     : Double;
    FRegionSize : Integer;
    FThreshold  : Double;

    procedure SetRadius(AValue: Double);
    procedure SetThreshold(AValue: Double);

    function BlurImage(AImage: TBitmap32; const AOrigColors, ABlurColors: TArrayOfColor32): TArrayOfColor32;
    function GetLerpAmount(const a: Double; const ACutoff: Double): Double;
    function GetRGB(AImage: TBitmap32; const AX, AY, AWidth, AHeight: Integer): TArrayOfColor32;
    function GetSample(AImage: TBitmap32; const AX, AY, ASize: Integer): TArrayOfColor32;
    function Lerp(const a, b, amt: Double): Double;
    function LerpPixel(const AColor1, AColor2: TColor32; const amt: Double): TColor32;
    function RmsError(const APixels: TArrayOfColor32): Double;
  public
    constructor Create;

    procedure Filter(AImage: TBitmap32);

    property Radius    : Double read FRadius    write SetRadius;
    property Threshold : Double read FThreshold write SetThreshold;
  end;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmGimpGaussianBlur;

const
  MIN_RADIUS    = 0.1;
  MAX_RADIUS    = 100.0;
  MIN_THRESHOLD = 0.1;
  MAX_THRESHOLD = 100.0;

function TgmSmartBlurFilter.BlurImage(AImage: TBitmap32;
  const AOrigColors, ABlurColors: TArrayOfColor32): TArrayOfColor32;
var
  LAmt      : Double;
  LLength   : Integer;
  i, w      : Integer;
  LPix      : TArrayOfColor32;
begin
  LPix    := nil;
  LLength := Length(AOrigColors);

  SetLength(Result, LLength);

  w := AImage.Width;
  
  for i := 0 to (LLength - 1) do
  begin
    LPix := GetSample(AImage, i mod w, i div w, FRegionSize);

    if Length(LPix) = 0 then
    begin
      Continue;
    end;

    LAmt      := getLerpAmount( RmsError(LPix), FThreshold );
    Result[i] := LerpPixel(ABlurColors[i], AOrigColors[i], LAmt);
  end;
end;

constructor TgmSmartBlurFilter.Create;
begin
  inherited Create;

  FRadius     := 9.0;
  FRegionSize := 5;
  FThreshold  := 25.0;
end;

procedure TgmSmartBlurFilter.Filter(AImage: TBitmap32);
var
  i             : Integer;
  p             : PColor32;
  LTarget       : TBitmap32;
  LSrcPixels    : TArrayOfColor32;
  LBlurryPixels : TArrayOfColor32;
  LResultPixels : TArrayOfColor32;
  LGaussBlur    : TgmGimpGaussianBlur;
begin
  LTarget    := TBitmap32.Create;
  LGaussBlur := TgmGimpGaussianBlur.Create;
  try
    // clone image into target
    LTarget.Assign(AImage);

    // get source pixels
    LSrcPixels := Self.GetRGB(AImage, 0, 0, AImage.Width, AImage.Height);

    // blur the clone image
    LGaussBlur.HorizontalRadius := FRadius;
    LGaussBlur.VerticalRadius   := FRadius;
    LGaussBlur.Execute(LTarget);

    // get the blurred pixels
    LBlurryPixels := Self.GetRGB(LTarget, 0, 0, LTarget.Width, LTarget.Height);

    // go thru the image and interpolate values
    LResultPixels := BlurImage(AImage, LSrcPixels, LBlurryPixels);

    // replace original pixels with new ones
    p := @AImage.Bits[0];

    for i := 0 to (AImage.Width * AImage.Height - 1) do
    begin
      p^ := LResultPixels[i];
      Inc(p);
    end;
    
  finally
    LTarget.Free;
    LGaussBlur.Free;
  end;
end;

function TgmSmartBlurFilter.GetLerpAmount(
  const a: Double; const ACutoff: Double): Double;
begin
  if a > ACutoff then
  begin
    Result := 1.0;
  end
  else
  begin
    Result := a / ACutoff;
  end;
end;

function TgmSmartBlurFilter.GetRGB(AImage: TBitmap32;
  const AX, AY, AWidth, AHeight: Integer): TArrayOfColor32;
var
  i, j   : Integer;
  x, y   : Integer;
  LIndex : Integer;
  p      : PColor32Array;
begin
{$RANGECHECKS OFF}

  SetLength(Result, AWidth * AHeight);
  LIndex := 0;

  for j := 0 to (AHeight - 1) do
  begin
    y := AY + j;

    // if the Y-coordinate is out the range, skip to next row
    if (y < 0) or (y >= AImage.Height) then
    begin
      Inc(LIndex, AImage.Width);
      Continue;
    end;


    p := AImage.ScanLine[y];

    for i := 0 to (AWidth - 1) do
    begin
      Inc(LIndex);

      x := AX + i;
      
      // if the X-coordinate is out the range, skip to next column
      if (x < 0) or (x >= AImage.Width) then
      begin
        Continue;
      end;

      Result[LIndex - 1] := p[x];
    end;
  end;

{$RANGECHECKS ON}
end;

function TgmSmartBlurFilter.GetSample(AImage: TBitmap32;
  const AX, AY, ASize: Integer): TArrayOfColor32;
begin
  Result := Self.GetRGB(AImage, AX, AY, ASize, ASize);
end;

// linear interpolation between two float-point values
function TgmSmartBlurFilter.Lerp(const a, b, amt: Double): Double;
begin
  Result := a + amt * (b - a);
end;

// linear interpolation between two colors
function TgmSmartBlurFilter.LerpPixel(const AColor1, AColor2: TColor32;
  const amt: Double): TColor32;
var
  a1, r1, g1, b1 : Cardinal;
  a2, r2, g2, b2 : Cardinal;
  na, nr, ng, nb : Cardinal;
begin
  a1 := AColor1 shr 24 and $FF;
  r1 := AColor1 shr 16 and $FF;
  g1 := AColor1 shr  8 and $FF;
  b1 := AColor1        and $FF;

  a2 := AColor2 shr 24 and $FF;
  r2 := AColor2 shr 16 and $FF;
  g2 := AColor2 shr  8 and $FF;
  b2 := AColor2        and $FF;

  na := Clamp( Round(Lerp(a1, a2, amt)), 0, 255 );
  nr := Clamp( Round(Lerp(r1, r2, amt)), 0, 255 );
  ng := Clamp( Round(Lerp(g1, g2, amt)), 0, 255 );
  nb := Clamp( Round(Lerp(b1, b2, amt)), 0, 255 );

  Result := (na shl 24) or (nr shl 16) or (ng shl 8) or nb;
end;

function TgmSmartBlurFilter.RmsError(const APixels: TArrayOfColor32): Double;
var
  LAccum  : Double;
  LAve    : Double;
  LDiff   : Double;
  rms     : Double;
  i       : Integer;
  LLength : Integer;
begin
  LAve    := 0;
  LLength := Length(APixels);

  for i := 0 to (LLength - 1) do
  begin
    LAve := LAve + (APixels[i] shr 8 and $FF);
  end;

  LAve   := LAve / LLength;
  LAccum := 0;

  for i := 0 to (LLength - 1) do
  begin
    LDiff  := (APixels[i] shr 8 and $FF) - LAve;
    LDiff  := LDiff * LDiff;
    LAccum := LAccum + LDiff;
  end;

  rms    := LAccum / LLength;
  Result := Sqrt(rms);
end;

procedure TgmSmartBlurFilter.SetRadius(AValue: Double);
begin
  if AValue < MIN_RADIUS then
  begin
    AValue := MIN_RADIUS;
  end
  else if AValue > MAX_RADIUS then
  begin
    AValue := MAX_RADIUS;
  end;

  if FRadius <> AValue then
  begin
    FRadius := AValue;
  end;
end;

procedure TgmSmartBlurFilter.SetThreshold(AValue: Double);
begin
  if AValue < MIN_THRESHOLD then
  begin
    AValue := MIN_THRESHOLD;
  end
  else if AValue > MAX_THRESHOLD then
  begin
    AValue := MAX_THRESHOLD;
  end;

  if FThreshold <> AValue then
  begin
    FThreshold := AValue;
  end;
end;

end.
