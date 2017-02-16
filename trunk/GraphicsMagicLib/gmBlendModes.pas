{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  This library was introduced in order to provide functions for manipulating
  Alpha Channel of a Bitmap32.

  ------------------------------------------------------------------

  Most of algorithms found in http://www.pegtop.net/

  We have rewrote the GR32_Add_BlendModes.pas library, which could be found on
  http://www.g32.org/graphics32/

  This library is for GR32, but could not apply it to layer blending.
  It's just for Brush blending, Marquee blending, etc.}

unit gmBlendModes;

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
  SysUtils, Classes, Math, GR32, GR32_Add_BlendModes;

  function MultiplyBlend32(const F, B: TColor32): TColor32;
  function ScreenBlend32(const F, B: TColor32): TColor32;
  function OverlayBlend32(const F, B: TColor32): TColor32;
  function BrightLightBlend32(const F, B: TColor32): TColor32;
  function SoftLightXFBlend32(const F, B: TColor32): TColor32;
  function HardLightBlend32(const F, B: TColor32): TColor32;
  function ColorDodgeBlend32(const F, B: TColor32): TColor32;
  function ColorBurnBlend32(const F, B: TColor32): TColor32;
  function DarkenBlend32(const F, B: TColor32): TColor32;
  function LightenBlend32(const F, B: TColor32): TColor32;
  function DifferenceBlend32(const F, B: TColor32): TColor32;
  function NegationBlend32(const F, B: TColor32): TColor32;
  function ExclusionBlend32(const F, B: TColor32): TColor32;
  function HueBlend32(const F, B: TColor32): TColor32;
  function SaturationBlend32(const F, B: TColor32): TColor32;
  function ColorBlend32(const F, B: TColor32): TColor32;
  function LuminosityBlend32(const F, B: TColor32): TColor32;
  function AverageBlend32(const F, B: TColor32): TColor32;
  function InverseColorDodgeBlend32(const F, B: TColor32): TColor32;
  function InverseColorBurnBlend32(const F, B: TColor32): TColor32;
  function SoftColorDodgeBlend32(const F, B: TColor32): TColor32;
  function SoftColorBurnBlend32(const F, B: TColor32): TColor32;
  function ReflectBlend32(const F, B: TColor32): TColor32;
  function GlowBlend32(const F, B: TColor32): TColor32;
  function FreezeBlend32(const F, B: TColor32): TColor32;
  function HeatBlend32(const F, B: TColor32): TColor32;
  function AdditiveBlend32(const F, B: TColor32): TColor32;
  function SubtractiveBlend32(const F, B: TColor32): TColor32;
  function InterpolationBlend32(const F, B: TColor32): TColor32;
  function StampBlend32(const F, B: TColor32): TColor32;
  function XORBlend32(const F, B: TColor32): TColor32;
  function ANDBlend32(const F, B: TColor32): TColor32;
  function ORBlend32(const F, B: TColor32): TColor32;
  function RedBlend32(const F, B: TColor32): TColor32;
  function GreenBlend32(const F, B: TColor32): TColor32;
  function BlueBlend32(const F, B: TColor32): TColor32;
  function DissolveBlend32(const F, B, M: TColor32): TColor32;

  function RGBBlendByMode(const F, B: TColor32; const AOpacity: Byte;
    const AMode: TBlendMode32): TColor32;

implementation

var
  // For bright-light blend mode
  SqrtLUT: array [0..65535] of Byte;

  // Cosine Table used for Interpolation blend mode
  CosineLUT: array [0..255] of Integer;

  // Probability look up table
  ProbLUT: array [0..100, 0..99] of Boolean;

//-- Blend Functions -----------------------------------------------------------

function MultiplyBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  bR := fR * bR div 255;
  bG := fG * bG div 255;
  bB := fB * bB div 255;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function ScreenBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  bR := 255 - (255 - fR) * (255 - bR) div 255;
  bG := 255 - (255 - fG) * (255 - bG) div 255;
  bB := 255 - (255 - fB) * (255 - bB) div 255;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function OverlayBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  if bR < 128 then
  begin
    bR := bR * fR div 128;
  end
  else
  begin
    bR := 255 - (255 - bR) * (255 - fR) div 128;
  end;

  if bG < 128 then
  begin
    bG := bG * fG div 128;
  end
  else
  begin
    bG := 255 - (255 - bG) * (255 - fG) div 128;
  end;

  if bB < 128 then
  begin
    bB := bB * fB div 128;
  end
  else
  begin
    bB := 255 - (255 - bB) * (255 - fB) div 128;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

{ Bright Light - Introduced by Michael Hansen -  much like average }
function BrightLightBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  bR := SqrtLUT[fR * bR];
  bG := SqrtLUT[fG * bG];
  bB := SqrtLUT[fB * bB];

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

{ Soft Light - formula by Jens Gruschel }
function SoftLightXFBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
  C         : Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  C  := bR * fR div 255;
  bR := C + bR * (  255 - ( (255 - bR) * (255 - fR) div 255 ) - C  ) div 255;

  C  := bG * fG div 255;
  bG := C + bG * (  255 - ( (255 - bG) * (255 - fG) div 255 ) - C  ) div 255;
  
  C  := bB * fB div 255;;
  bB := C + bB * (  255 - ( (255 - bB) * (255 - fB) div 255 ) - C  ) div 255;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function HardLightBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  if fR < 128 then
  begin
    bR := bR * fR div 128;
  end
  else
  begin
    bR := 255 - (255 - bR) * (255 - fR) div 128;
  end;

  if fG < 128 then
  begin
    bG := bG * fG div 128;
  end
  else
  begin
    bG := 255 - (255 - bG) * (255 - fG) div 128;
  end;

  if fB < 128 then
  begin
    bB := bB * fB div 128;
  end
  else
  begin
    bB := 255 - (255 - bB) * (255 - fB) div 128;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function ColorDodgeBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  if fR = 255 then
  begin
    bR := 255;
  end
  else
  begin
    bR := bR * 255 div (255 - fR);

    if bR > 255 then
    begin
      bR := 255;
    end;
  end;

  if fG = 255 then
  begin
    bG := 255;
  end
  else
  begin
    bG := bG * 255 div (255 - fG);

    if bG > 255 then
    begin
      bG := 255;
    end;
  end;

  if fB = 255 then
  begin
    bB := 255;
  end
  else
  begin
    bB := bB * 255 div (255 - fB);

    if bB > 255 then
    begin
      bB := 255;
    end;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function ColorBurnBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Byte;
  bR, bG, bB: Byte;
  LTemp     : Integer;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  if fR = 0 then
  begin
    bR := 0;
  end
  else
  begin
    LTemp := 255 - ( (255 - bR) * 255 div fR );

    if LTemp < 0 then
    begin
      bR := 0;
    end
    else
    begin
      bR := LTemp;
    end;
  end;

  if fG = 0 then
  begin
    bG := 0;
  end
  else
  begin
    LTemp := 255 - ( (255 - bG) * 255 div fG );

    if LTemp < 0 then
    begin
      bG := 0;
    end
    else
    begin
      bG := LTemp;
    end;
  end;

  if fB = 0 then
  begin
    bB := 0;
  end
  else
  begin
    LTemp := 255 - ( (255 - bB) * 255 div fB );

    if LTemp < 0 then
    begin
      bB := 0;
    end
    else
    begin
      bB := LTemp;
    end;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function DarkenBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  if fR < bR then
  begin
    bR := fR;
  end;
  
  if fG < bG then
  begin
    bG := fG;
  end;
  
  if fB < bB then
  begin
    bB := fB;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function LightenBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  if fR > bR then
  begin
    bR := fR;
  end;

  if fG > bG then
  begin
    bG := fG;
  end;

  if fB > bB then
  begin
    bB := fB;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function DifferenceBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Byte;
  bR, bG, bB: Byte;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  bR := Abs(bR - fR);
  bG := Abs(bG - fG);
  bB := Abs(bB - fB);

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

{ Negation - introduced by Jens Gruschel }
function NegationBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Byte;
  bR, bG, bB: Byte;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  bR := 255 - Abs(255 - bR - fR);
  bG := 255 - Abs(255 - bG - fG);
  bB := 255 - Abs(255 - bB - fB);

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function ExclusionBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  bR := bR + fR - bR * fR div 128;
  bG := bG + fG - bG * fG div 128;
  bB := bB + fB - bB * fB div 128;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function HueBlend32(const F, B: TColor32): TColor32;
var
  fH, fS, fL: Single;
  bH, bS, bL: Single;
  LNewHSL   : TColor32;
begin
  { Invert RGB to HSL }
  RGBToHSL(F, fH, fS, fL);
  RGBToHSL(B, bH, bS, bL);

  { Combine HSL and invert it to RGB }
  LNewHSL := HSLToRGB(fH, bS, bL);

  { Get result }
  Result := (B and $FF000000) or (LNewHSL and $FFFFFF);
end; 

function SaturationBlend32(const F, B: TColor32): TColor32;
var
  fH, fS, fL: Single;
  bH, bS, bL: Single;
  LNewHSL   : TColor32;
begin
  { Invert RGB to HSL }
  RGBToHSL(F, fH, fS, fL);
  RGBToHSL(B, bH, bS, bL);

  { Combine HSL and invert it to RGB }
  LNewHSL := HSLToRGB(bH, fS, bL);

  { Get result }
  Result := (B and $FF000000) or (LNewHSL and $FFFFFF);
end; 

function ColorBlend32(const F, B: TColor32): TColor32;
var
  fH, fS, fL: Single;
  bH, bS, bL: Single;
  LNewHSL   : TColor32;
begin
  { Invert RGB to HSL }
  RGBToHSL(F, fH, fS, fL);
  RGBToHSL(B, bH, bS, BL);

  { Combine HSL and invert it to RGB }
  LNewHSL := HSLToRGB(fH, fS, bL);
  
  { Get result }
  Result := (B and $FF000000) or (LNewHSL and $FFFFFF);
end; 

function LuminosityBlend32(const F, B: TColor32): TColor32;
var
  fH, fS, fL: Single;
  bH, bS, bL: Single;
  LNewHSL   : TColor32;
begin
  { Invert RGB to HSL }
  RGBToHSL(F, fH, fS, fL);
  RGBToHSL(B, bH, bS, bL);

  { Combine HSL and invert it to RGB }
  LNewHSL := HSLToRGB(bH, bS, fL);

  { Get result }
  Result := (B and $FF000000) or (LNewHSL and $FFFFFF);
end; 

{ Average - useful in some cases - but the same as Normal with MasterAlpha = 128 }
function AverageBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  bR := (bR + fR) div 2;
  bG := (bG + fG) div 2;
  bB := (bB + fB) div 2;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function InverseColorDodgeBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  if bR < 255 then
  begin
    bR := fR * 255 div (255 - bR);

    if bR > 255 then
    begin
      bR := 255;
    end;
  end;

  if bG < 255 then
  begin
    bG := fG * 255 div (255 - bG);

    if bG > 255 then
    begin
      bG := 255;
    end;
  end;

  if bB < 255 then
  begin
    bB := fB * 255 div (255 - bB);

    if bB > 255 then
    begin
      bB := 255;
    end;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function InverseColorBurnBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Byte;
  bR, bG, bB: Byte;
  LTemp     : Integer;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  if bR > 0 then
  begin
    LTemp := 255 - (255 - fR) * 255 div bR;

    if LTemp < 0 then
    begin
      bR := 0;
    end
    else
    begin
      bR := LTemp;
    end;
  end;

  if bG > 0 then
  begin
    LTemp := 255 - (255 - fG) * 255 div bG;

    if LTemp < 0 then
    begin
      bG := 0;
    end
    else
    begin
      bG := LTemp;
    end;
  end;

  if bB > 0 then
  begin
    LTemp := 255 - (255 - fB) * 255 div bB;

    if LTemp < 0 then
    begin
      bB := 0;
    end
    else
    begin
      bB := LTemp;
    end;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function SoftColorDodgeBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
  LTemp     : Integer;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  if (bR + fR) < 256 then
  begin
    if fR = 255 then
    begin
      bR := 255;
    end
    else
    begin
      bR := bR * 128 div (255 - fR);

      if bR > 255 then
      begin
        bR := 255;
      end;
    end;
  end
  else
  begin
    LTemp := 255 - (255 - fR) * 128 div bR;

    if LTemp < 0 then
    begin
      bR := 0;
    end
    else
    begin
      bR := LTemp;
    end;
  end;

  if (bG + fG) < 256 then
  begin
    if fG = 255 then
    begin
      bG := 255;
    end
    else
    begin
      bG := bG * 128 div (255 - fG);

      if bG > 255 then
      begin
        bG := 255;
      end;
    end;
  end
  else
  begin
    LTemp := 255 - (255 - fG) * 128 div bG;

    if LTemp < 0 then
    begin
      bG := 0;
    end
    else
    begin
      bG := LTemp;
    end;
  end;

  if (bB + fB) < 256 then
  begin
    if fB = 255 then
    begin
      bB := 255;
    end
    else
    begin
      bB := bB * 128 div (255 - fB);

      if bB > 255 then
      begin
        bB := 255;
      end;
    end;
  end
  else
  begin
    LTemp := 255 - (255 - fB) * 128 div bB;

    if LTemp < 0 then
    begin
      bB := 0;
    end
    else
    begin
      bB := LTemp;
    end;
  end;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function SoftColorBurnBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
  LTemp     : Integer;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  if (bR + fR) < 256 then
  begin
    if bR < 255 then
    begin
      bR := fR * 128 div (255 - bR);

      if bR > 255 then
      begin
        bR := 255;
      end;
    end;
  end
  else
  begin
    LTemp := 255 - (255 - bR) * 128 div fR;

    if LTemp < 0 then
    begin
      bR := 0;
    end
    else
    begin
      bR := LTemp;
    end;
  end;

  if (bG + fG) < 256 then
  begin
    if bG < 255 then
    begin
      bG := fG * 128 div (255 - bG);

      if bG > 255 then
      begin
        bG := 255;
      end;
    end;
  end
  else
  begin
    LTemp := 255 - (255 - bG) * 128 div fG;

    if LTemp < 0 then
    begin
      bG := 0;
    end
    else
    begin
      bG := LTemp;
    end;
  end;

  if (bB + fB) < 256 then
  begin
    if bB < 255 then
    begin
      bB := fB * 128 div (255 - bB);

      if bB > 255 then
      begin
        bB := 255;
      end;
    end;
  end
  else
  begin
    LTemp := 255 - (255 - bB) * 128 div fB;

    if LTemp < 0 then
    begin
      bB := 0;
    end
    else
    begin
      bB := LTemp;
    end;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

{ Reflect - introduced by Michael Hansen }
function ReflectBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  if fR = 255 then
  begin
    bR := 255;
  end
  else
  begin
    bR := bR * bR div (255 - fR);

    if bR > 255 then
    begin
      bR := 255;
    end;
  end;

  if fG = 255 then
  begin
    bG := 255;
  end
  else
  begin
    bG := bG * bG div (255 - fG);

    if bG > 255 then
    begin
      bG := 255;
    end;
  end;

  if fB = 255 then
  begin
    bB := 255;
  end
  else
  begin
    bB := bB * bB div (255 - fB);

    if bB > 255 then
    begin
      bB := 255;
    end;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function GlowBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  if bR < 255 then
  begin
    bR := fR * fR div (255 - bR);

    if bR > 255 then
    begin
      bR := 255;
    end;
  end;

  if bG < 255 then
  begin
    bG := fG * fG div (255 - bG);

    if bG > 255 then
    begin
      bG := 255;
    end;
  end;

  if bB < 255 then
  begin
    bB := fB * fB div (255 - bB);

    if bB > 255 then
    begin
      bB := 255;
    end;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function FreezeBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Byte;
  bR, bG, bB: Byte;
  LTemp     : Integer;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  if fR = 0 then
  begin
    bR := 0;
  end
  else
  begin
    LTemp := 255 - Sqr(255 - bR) div fR;

    if LTemp < 0 then
    begin
      bR := 0;
    end
    else
    begin
      bR := LTemp;
    end;
  end;

  if fG = 0 then
  begin
    bG := 0;
  end
  else
  begin
    LTemp := 255 - Sqr(255 - bG) div fG;

    if LTemp < 0 then
    begin
      bG := 0;
    end
    else
    begin
      bG := LTemp;
    end;
  end;

  if fB = 0 then
  begin
    bB := 0;
  end
  else
  begin
    LTemp := 255 - Sqr(255 - bB) div fB;

    if LTemp < 0 then
    begin
      bB := 0;
    end
    else
    begin
      bB := LTemp;
    end;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function HeatBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Byte;
  bR, bG, bB: Byte;
  LTemp     : Integer;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  if bR > 0 then
  begin
    LTemp := 255 - Sqr(255 - fR) div bR;

    if LTemp < 0 then
    begin
      bR := 0;
    end
    else
    begin
      bR := LTemp;
    end;
  end;

  if bG > 0 then
  begin
    LTemp := 255 - Sqr(255 - fG) div bG;

    if LTemp < 0 then
    begin
      bG := 0;
    end
    else
    begin
      bG := LTemp;
    end;
  end;

  if bB > 0 then
  begin
    LTemp := 255 - Sqr(255 - fB) div bB;

    if LTemp < 0 then
    begin
      bB := 0;
    end
    else
    begin
      bB := LTemp;
    end;
  end;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function AdditiveBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  bR := bR + fR;

  if bR > 255 then
  begin
    bR := 255;
  end;

  bG := bG + fG;

  if bG > 255 then
  begin
    bG := 255;
  end;

  bB := bB + fB;

  if bB > 255 then
  begin
    bB := 255;
  end;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function SubtractiveBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Byte;
  bR, bG, bB: Byte;
  LTemp     : Integer;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  LTemp := bR + fR - 256;

  if LTemp < 0 then
  begin
    bR := 0;
  end
  else
  begin
    bR := LTemp;
  end;

  LTemp := bG + fG - 256;

  if LTemp < 0 then
  begin
    bG := 0;
  end
  else
  begin
    bG := LTemp;
  end;

  LTemp := bB + fB - 256;

  if LTemp < 0 then
  begin
    bB := 0;
  end
  else
  begin
    bB := LTemp;
  end;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

function InterpolationBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine:

    You should calculate a Cosine Table for this mode first, codes like this:
    for I := 0 to 255 do
      CosineTab[I] := Round( 64 - Cos(I * Pi / 255) * 64 ); 

    The variable CosineTable has already defined in the Initialization part
    of this unit. }
    
  bR := CosineLUT[fR] + CosineLUT[bR];
  bG := CosineLUT[fG] + CosineLUT[bG];
  bB := CosineLUT[fB] + CosineLUT[bB];

  if bR > 255 then
  begin
    bR := 255;
  end;

  if bG > 255 then
  begin
    bG := 255;
  end;

  if bB > 255 then
  begin
    bB := 255;
  end;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function StampBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Byte;
  bR, bG, bB: Byte;
  LTemp     : Integer;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  LTemp := bR + 2 * fR - 255;

  if LTemp < 0 then
  begin
    bR := 0;
  end
  else if LTemp > 255 then
  begin
    bR := 255;
  end
  else
  begin
    bR := LTemp;
  end;

  LTemp := bG + 2 * fG - 255;

  if LTemp < 0 then
  begin
    bG := 0;
  end
  else if LTemp > 255 then
  begin
    bG := 255;
  end
  else
  begin
    bG := LTemp;
  end;

  LTemp := bB + 2 * fB - 255;

  if LTemp < 0 then
  begin
    bB := 0;
  end
  else if LTemp > 255 then
  begin
    bB := 255;
  end
  else
  begin
    bB := LTemp;
  end;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function XORBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  bR := bR xor fR;
  bG := bG xor fG;
  bB := bB xor fB;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function ANDBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  bR := bR and fR;
  bG := bG and fG;
  bB := bB and fB;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function ORBlend32(const F, B: TColor32): TColor32;
var
  fR, fG, fB: Cardinal;
  bR, bG, bB: Cardinal;
begin
  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  
  { Combine }
  bR := bR or fR;
  bG := bG or fG;
  bB := bB or fB;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function RedBlend32(const F, B: TColor32): TColor32;
var
  bR, bG, bB: Cardinal;
begin
  { Combine }
  bR := F shr 16 and $FF; // replace background red component with forground red component
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function GreenBlend32(const F, B: TColor32): TColor32;
var
  bR, bG, bB: Cardinal;
begin
  { Combine }
  bR := B shr 16 and $FF;
  bG := F shr 8  and $FF; // replace background green component with forground green component
  bB := B        and $FF;
  
  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

function BlueBlend32(const F, B: TColor32): TColor32;
var
  bR, bG, bB: Cardinal;
begin
  { Combine }
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := F        and $FF; // replace background blue component with forground blue component

  { Get result }
  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end;

// F = foreground color
// B = background color
// M = blend opacity [0..255]
function DissolveBlend32(const F, B, M: TColor32): TColor32;
var
  LProbIndex   : Cardinal;
  LRandomIndex : Integer;
begin
  LProbIndex   := Round( (M and $FF) / 255 * 100 );
  LRandomIndex := Random(100);

  if ProbLUT[LProbIndex, LRandomIndex] then
  begin
    Result := F;
  end
  else
  begin
    Result := B;
  end;
end;

//------------------------------------------------------------------------------

function RGBBlendByMode(const F, B: TColor32; const AOpacity: Byte;
  const AMode: TBlendMode32): TColor32;
var
  LBlendColor : TColor32;
  LOpacity    : Cardinal;
  fR, fG, fB  : Cardinal;
  bR, bG, bB  : Cardinal;
begin
  LOpacity := AOpacity;

  // RGB blend
  case AMode of
    bbmNormal32           : LBlendColor := F;
    bbmMultiply32         : LBlendColor := MultiplyBlend32          (F, B);
    bbmScreen32           : LBlendColor := ScreenBlend32            (F, B);
    bbmOverlay32          : LBlendColor := OverlayBlend32           (F, B);
    bbmBrightLight32      : LBlendColor := BrightLightBlend32       (F, B);
    bbmSoftLightXF32      : LBlendColor := SoftLightXFBlend32       (F, B);
    bbmHardLight32        : LBlendColor := HardLightBlend32         (F, B);
    bbmColorDodge32       : LBlendColor := ColorDodgeBlend32        (F, B);
    bbmColorBurn32        : LBlendColor := ColorBurnBlend32         (F, B);
    bbmDarken32           : LBlendColor := DarkenBlend32            (F, B);
    bbmLighten32          : LBlendColor := LightenBlend32           (F, B);
    bbmDifference32       : LBlendColor := DifferenceBlend32        (F, B);
    bbmNegation32         : LBlendColor := NegationBlend32          (F, B);
    bbmExclusion32        : LBlendColor := ExclusionBlend32         (F, B);
    bbmHue32              : LBlendColor := HueBlend32               (F, B);
    bbmSaturation32       : LBlendColor := SaturationBlend32        (F, B);
    bbmColor32            : LBlendColor := ColorBlend32             (F, B);
    bbmLuminosity32       : LBlendColor := LuminosityBlend32        (F, B);
    bbmAverage32          : LBlendColor := AverageBlend32           (F, B);
    bbmInverseColorDodge32: LBlendColor := InverseColorDodgeBlend32 (F, B);
    bbmInverseColorBurn32 : LBlendColor := InverseColorBurnBlend32  (F, B);
    bbmSoftColorDodge32   : LBlendColor := SoftColorDodgeBlend32    (F, B);
    bbmSoftColorBurn32    : LBlendColor := SoftColorBurnBlend32     (F, B);
    bbmReflect32          : LBlendColor := ReflectBlend32           (F, B);
    bbmGlow32             : LBlendColor := GlowBlend32              (F, B);
    bbmFreeze32           : LBlendColor := FreezeBlend32            (F, B);
    bbmHeat32             : LBlendColor := HeatBlend32              (F, B);
    bbmAdditive32         : LBlendColor := AdditiveBlend32          (F, B);
    bbmSubtractive32      : LBlendColor := SubtractiveBlend32       (F, B);
    bbmInterpolation32    : LBlendColor := InterpolationBlend32     (F, B);
    bbmStamp32            : LBlendColor := StampBlend32             (F, B);
    bbmXOR32              : LBlendColor := XORBlend32               (F, B);
    bbmAND32              : LBlendColor := ANDBlend32               (F, B);
    bbmOR32               : LBlendColor := ORBlend32                (F, B);
    bbmRed32              : LBlendColor := RedBlend32               (F, B);
    bbmGreen32            : LBlendColor := GreenBlend32             (F, B);
    bbmBlue32             : LBlendColor := BlueBlend32              (F, B);

    bbmDissolve32:
      begin
        LBlendColor := DissolveBlend32(F, B, AOpacity);
        LOpacity    := 255;
      end;

  else
    LBlendColor := $00000000;
  end;

  // blend with opacity
  fR := LBlendColor shr 16 and $FF;
  fG := LBlendColor shr  8 and $FF;
  fB := LBlendColor        and $FF;

  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  bR := (fR * LOpacity + bR * (255 - LOpacity)) div 255;
  bG := (fG * LOpacity + bG * (255 - LOpacity)) div 255;
  bB := (fB * LOpacity + bB * (255 - LOpacity)) div 255;

  Result := (B and $FF000000) or (bR shl 16) or (bG shl 8) or bB;
end; 

procedure InitTables;
var
  i, j        : Integer;
  LSqrtNumber : Integer;
  LCosNumber  : Integer;
  LTmp        : Integer;
begin
  // initialize SqrtLUT -- for Bright-Light blend mode.
  for LSqrtNumber := 0 to 65535 do
  begin
    LTmp := Round(Sqrt(LSqrtNumber));

    if LTmp <= 255 then
    begin
      SqrtLUT[LSqrtNumber] := LTmp;
    end
    else
    begin
      SqrtLUT[LSqrtNumber] := 255;
    end;
  end;

  // initialize a cosine table -- for Interpolation blend mode
  for LCosNumber := 0 to 255 do
  begin
    CosineLUT[LCosNumber] := Round( 64 - Cos(LCosNumber * Pi / 255) * 64 );
  end;

  { Init ProbLUT -- Probability look up table }
  for i := 0 to 100 do
  begin
    for j := 0 to 99 do
    begin
      if j < i then
      begin
        ProbLUT[i, j] := True;
      end
      else
      begin
        ProbLUT[i, j] := False;
      end;
    end;
  end;
end;


initialization
  InitTables;

end.
