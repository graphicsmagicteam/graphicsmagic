{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmColorRange;

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
  GR32;

type
  TgmSelectedColorMode = (scmRGB, scmHLS, scmHSV);

  // HLS Color Range
  procedure HLSColorRange(const AColor: TColor32; const AFuzziness: Integer;
    var AMaxH, AMinH: Word; var AMaxL, AMinL, AMaxS, AMinS: Byte);

  // HSV Color Range
  procedure HSVColorRange(const AColor: TColor32; const AFuzziness: Integer;
    var AMaxH, AMinH: Word; var AMaxS, AMinS, AMaxV, AMinV: Byte);

  // RGB Color Range
  procedure RGBColorRange32(const AColor: TColor32; const ARange: Integer;
    var AMaxR, AMinR, AMaxG, AMinG, AMaxB, AMinB: Byte);

  procedure LightnessRange(const AColor: TColor32; const AFuzziness: Integer;
    var AMinLight, AMaxLight: Byte);

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmColorSpace;

// HLS Color Range
procedure HLSColorRange(const AColor: TColor32; const AFuzziness: Integer;
  var AMaxH, AMinH: Word; var AMaxL, AMinL, AMaxS, AMinS: Byte);
var
  H, L, S: Integer;
begin
  RGBToHLS32(AColor, H, L, S);

  AMinH := Clamp(H - AFuzziness, 0, 360);
  AMaxH := Clamp(H + AFuzziness, 0, 360);
  AMinL := Clamp(L - AFuzziness, 0, 255);
  AMaxL := Clamp(L + AFuzziness, 0, 255);
  AMinS := Clamp(S - AFuzziness, 1, 255);
  AMaxS := Clamp(S + AFuzziness, 1, 255);
end; 

// HSV Color Range
procedure HSVColorRange(const AColor: TColor32; const AFuzziness: Integer;
  var AMaxH, AMinH: Word; var AMaxS, AMinS, AMaxV, AMinV: Byte);
var
  H, S, V: Integer;
begin
  RGBToHSV32(AColor, H, S, V);

  AMinH := Clamp(H - AFuzziness, 0, 360);
  AMaxH := Clamp(H + AFuzziness, 0, 360);
  AMinS := Clamp(S - AFuzziness, 0, 255);
  AMaxS := Clamp(S + AFuzziness, 0, 255);
  AMinV := Clamp(V - AFuzziness, 0, 255);
  AMaxV := Clamp(V + AFuzziness, 0, 255);
end; 

// RGB Color Range
procedure RGBColorRange32(const AColor: TColor32; const ARange: Integer;
  var AMaxR, AMinR, AMaxG, AMinG, AMaxB, AMinB: Byte);
var
  R, G, B: Integer;
begin
  R := AColor shr 16 and $FF;
  G := AColor shr 8  and $FF;
  B := AColor        and $FF;

  AMinR := Clamp(R - ARange, 0, 255);
  AMaxR := Clamp(R + ARange, 0, 255);
  AMinG := Clamp(G - ARange, 0, 255);
  AMaxG := Clamp(G + ARange, 0, 255);
  AMinB := Clamp(B - ARange, 0, 255);
  AMaxB := Clamp(B + ARange, 0, 255);
end; 

procedure LightnessRange(const AColor: TColor32; const AFuzziness: Integer;
  var AMinLight, AMaxLight: Byte);
var
  LLightValue: Integer;
begin
  LLightValue := RGBToLightness32(AColor);
  AMinLight   := Clamp(LLightValue - AFuzziness, 0, 255);
  AMaxLight   := Clamp(LLightValue + AFuzziness, 0, 255);
end; 

end.
