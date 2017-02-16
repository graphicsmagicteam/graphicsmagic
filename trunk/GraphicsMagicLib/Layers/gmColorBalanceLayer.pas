unit gmColorBalanceLayer;

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
 * The Original Code is gmColorBalanceLayer.pas.
 *
 * The Initial Developer of the Original Code are
 * Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2015/11/15

interface

uses
{ Graphics32 }
  GR32,
{ GraphicsMagic lib }
  gmColorBalance,
  gmLayers;

type
  { TgmColorBalanceLayer }

  TgmColorBalanceLayer = class(TgmNonPixelizedLayer)
  private
    FColorBalance : TgmColorBalance;

    // adjusting color balance for single color
    function RGBColorBalance(const AColor: TColor32): TColor32;
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    property ColorBalance : TgmColorBalance read FColorBalance;
  end;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmColorSpace;

{$R gmColorBalanceLayerIcons.res}

{ TgmColorBalanceLayer }

constructor TgmColorBalanceLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Color Balance';
  FLogoThumbEnabled := True;
  FColorBalance     := TgmColorBalance.Create(FLayerBitmap);

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.LoadFromResourceName(HInstance, 'COLORBALANCELAYERLOGO');

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

destructor TgmColorBalanceLayer.Destroy;
begin
  FColorBalance.Free();
  inherited;
end;

function TgmColorBalanceLayer.GetCopy: TgmCustomLayer;
var
  i      : Integer;
  LLayer : TgmColorBalanceLayer;
begin
  LLayer := TgmColorBalanceLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  for i := 0 to 255 do
  begin
    LLayer.ColorBalance.RedLookup[i]   := Self.FColorBalance.RedLookup[i];
    LLayer.ColorBalance.GreenLookup[i] := Self.FColorBalance.GreenLookup[i];
    LLayer.ColorBalance.BlueLookup[i]  := Self.FColorBalance.BlueLookup[i];
  end;

  for i := 0 to 2 do
  begin
    LLayer.ColorBalance.CyanRedArray[i]      := Self.FColorBalance.CyanRedArray[i];
    LLayer.ColorBalance.MagentaGreenArray[i] := Self.FColorBalance.MagentaGreenArray[i];
    LLayer.ColorBalance.YellowBlueArray[i]   := Self.FColorBalance.YellowBlueArray[i];
  end;

  LLayer.ColorBalance.PreserveLuminosity := Self.FColorBalance.PreserveLuminosity;
  LLayer.ColorBalance.TransferMode       := Self.FColorBalance.TransferMode;

  Result := LLayer;
end;

procedure TgmColorBalanceLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
var
  LForeColor : TColor32;
  LAlpha     : Byte;
begin
  LAlpha := B shr 24 and $FF;

  // only process when the background pixel is not transparent
  if LAlpha > 0 then
  begin
    LForeColor := RGBColorBalance(B);
    LForeColor := (LAlpha shl 24) or (LForeColor and $FFFFFF); // only need RGB components

    // blending
    FLayerBlendEvent(LForeColor, B, M);
  end;
end;

// adjusting color balance for single color
function TgmColorBalanceLayer.RGBColorBalance(
  const AColor: TColor32): TColor32;
var
  A          : Cardinal;
  H, L, S    : Integer;
  R, G, B    : Byte;
  rn, gn, bn : Byte;
begin
  // preserving the Alpha channel of passed color
  A := AColor shr 24 and $FF;

  // extracting the RGB values from passed color
  R := AColor shr 16 and $FF;
  G := AColor shr  8 and $FF;
  B := AColor        and $FF;

  // getting the modulated color
  rn := FColorBalance.RedLookup[R];
  gn := FColorBalance.GreenLookup[G];
  bn := FColorBalance.BlueLookup[B];

  Result := (A shl 24) or (rn shl 16) or (gn shl 8) or bn;

  // if preserving the original luminosity of the passed color ...
  if FColorBalance.PreserveLuminosity then
  begin
    RGBToHLS32(Result, H, L, S);
    S      := Clamp(S, 1, 255);
    L      := Clamp( RGBToLightness32(AColor), 0, 255 );
    Result := HLSToRGB32(A, H, L, S);
  end;
end;

end.
