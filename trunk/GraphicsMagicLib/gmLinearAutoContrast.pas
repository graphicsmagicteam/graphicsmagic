unit gmLinearAutoContrast;

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
 *
 * The Original Code is gmLinearAutoContrast.pas.
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2015/06/28

interface

uses
{ Graphics32 }
  GR32;

type
  // This class is based on the article written by Peng Jia Le (≈Ìº—¿÷).
  // The article is at here:
  // http://www.cnblogs.com/Imageshop/archive/2011/11/13/2247614.html
  TgmLinearAutoContrast = class(TObject)
  private
    FShadowsClipPercent    : Single;  // 0.00 to 9.99
    FHighlightsClipPercent : Single;  // 0.00 to 9.99

    procedure SetShadowsClipPercent(AValue: Single);
    procedure SetHighlightsClipPercent(AValue: Single);
  public
    constructor Create;

    procedure Execute(ABitmap: TBitmap32);

    property ShadowsClipPercent    : Single read FShadowsClipPercent    write SetShadowsClipPercent;
    property HighlightsClipPercent : Single read FHighlightsClipPercent write SetHighlightsClipPercent;
  end;

implementation

const 
  MIN_CLIP_PERCENT = 0.00;
  MAX_CLIP_PERCENT = 9.99;

constructor TgmLinearAutoContrast.Create;
begin
  inherited;

  FShadowsClipPercent    := 0.1;
  FHighlightsClipPercent := 0.1;
end;

procedure TgmLinearAutoContrast.Execute(ABitmap: TBitmap32);
var
  LHistRed              : array [0..255] of Cardinal;
  LHistGreen            : array [0..255] of Cardinal;
  LHistBlue             : array [0..255] of Cardinal;
  LMap                  : array [0..255] of Cardinal;
  LShadowsClipAmount    : Cardinal;
  LHighlightsClipAmount : Cardinal;
  i, LSum, LPixelAmount : Cardinal;
  LMinRed, LMaxRed      : Cardinal;
  LMinGreen, LMaxGreen  : Cardinal;
  LMinBlue, LMaxBlue    : Cardinal;
  LMin, LMax            : Cardinal;
  r, g, b               : Byte;
  p                     : PColor32;
begin
  if (not Assigned(ABitmap)) or
     (ABitmap.Width <= 0) or
     (ABitmap.Height <= 0) then
  begin
    Exit;
  end;

  for i := 0 to 255 do
  begin
    LHistRed[i]   := 0;
    LHistGreen[i] := 0;
    LHistBlue[i]  := 0;
  end;

  LPixelAmount := ABitmap.Width * ABitmap.Height;

  // Note: The Shadows/Highlights clip percent is from 0.00% to 9.99%,
  // and both field values are from 0.00 to 9.99, so to get the correct
  // percentage, we have to multiply the both field values with 0.01 .
  LShadowsClipAmount    := Round(LPixelAmount * FShadowsClipPercent * 0.01);
  LHighlightsClipAmount := Round(LPixelAmount * FHighlightsClipPercent * 0.01);

  // count histogram
  p := @ABitmap.Bits[0];
  for i := 1 to LPixelAmount do
  begin
    r := p^ shr 16 and $FF;
    g := p^ shr  8 and $FF;
    b := p^        and $FF;

    LHistRed[r]   := LHistRed[r] + 1;
    LHistGreen[g] := LHistGreen[g] + 1;
    LHistBlue[b]  := LHistBlue[b] + 1;

    Inc(p);
  end;

  // calculate the lower and higher limits for seperate color channels ...

  // red channel
  LSum    := 0;
  LMinRed := 0;
  for i := 0 to 255 do
  begin
    LSum := LSum + LHistRed[i];

    if LSum >= LShadowsClipAmount then
    begin
      LMinRed := i;
      Break;
    end;
  end;

  LSum    := 0;
  LMaxRed := 0;
  for i := 255 downto 0 do
  begin
     LSum := LSum + LHistRed[i];

     if LSum >= LHighlightsClipAmount then
     begin
       LMaxRed := i;
       Break;
     end;
  end;

  // green channel
  LSum      := 0;
  LMinGreen := 0;
  for i := 0 to 255 do
  begin
    LSum := LSum + LHistGreen[i];

    if LSum >= LShadowsClipAmount then
    begin
      LMinGreen := i;
      Break;
    end;
  end;

  LSum      := 0;
  LMaxGreen := 0;
  for i := 255 downto 0 do
  begin
     LSum := LSum + LHistGreen[i];

     if LSum >= LHighlightsClipAmount then
     begin
       LMaxGreen := i;
       Break;
     end;
  end;

  // blue channel
  LSum     := 0;
  LMinBlue := 0;
  for i := 0 to 255 do
  begin
    LSum := LSum + LHistBlue[i];

    if LSum >= LShadowsClipAmount then
    begin
      LMinBlue := i;
      Break;
    end;
  end;

  LSum     := 0;
  LMaxBlue := 0;
  for i := 255 downto 0 do
  begin
     LSum := LSum + LHistBlue[i];

     if LSum >= LHighlightsClipAmount then
     begin
       LMaxBlue := i;
       Break;
     end;
  end;

  if (LMinRed = LMaxRed) and
     (LMinGreen = LMaxGreen) and
     (LMinBlue = LMaxBlue) then
  begin
    Exit;
  end;

  // get minimum limits among three channels
  if LMinBlue < LMinGreen then
  begin
    LMin := LMinBlue;
  end
  else
  begin
    LMin := LMinGreen;
  end;

  if LMin > LMinRed then
  begin
    LMin := LMinRed;
  end;

  // get maximum limits among three channels
  if LMaxBlue > LMaxGreen then
  begin
    LMax := LMaxBlue;
  end
  else
  begin
    LMax := LMaxGreen;
  end;

  if LMax < LMaxRed then
  begin
    LMax := LMaxRed;
  end;

  // calculate a maps for all color channels
  for i := 0 to 255 do
  begin
    if i <= LMin then
    begin
      LMap[i] := 0;
    end
    else if i > LMax then
    begin
      LMap[i] := 255;
    end
    else
    begin
      // linear interpolation
      LMap[i] := Round( (i - LMin) / (LMax - LMin) * 255 );
      
      if LMap[i] > 255 then
      begin
        LMap[i] := 255;
      end;
    end;
  end;

  // do the color mapping ...
  p := @ABitmap.Bits[0];
  for i := 1 to LPixelAmount do
  begin
    r := p^ shr 16 and $FF;
    g := p^ shr  8 and $FF;
    b := p^        and $FF;

    r := LMap[r];
    g := LMap[g];
    b := LMap[b];

    p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;

    Inc(p);
  end;
end;

procedure TgmLinearAutoContrast.SetHighlightsClipPercent(AValue: Single);
begin
  FHighlightsClipPercent := AValue;

  if FHighlightsClipPercent < MIN_CLIP_PERCENT then
  begin
    FHighlightsClipPercent := MIN_CLIP_PERCENT;
  end
  else if FHighlightsClipPercent > MAX_CLIP_PERCENT then
  begin
    FHighlightsClipPercent := MAX_CLIP_PERCENT
  end;
end;

procedure TgmLinearAutoContrast.SetShadowsClipPercent(AValue: Single);
begin
  FShadowsClipPercent := AValue;

  if FShadowsClipPercent < MIN_CLIP_PERCENT then
  begin
    FShadowsClipPercent := MIN_CLIP_PERCENT;
  end
  else if FShadowsClipPercent > MAX_CLIP_PERCENT then
  begin
    FShadowsClipPercent := MAX_CLIP_PERCENT
  end;
end;

end.
