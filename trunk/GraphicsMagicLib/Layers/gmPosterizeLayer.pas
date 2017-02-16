unit gmPosterizeLayer;

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
 * The Original Code is gmPosterizeLayer.pas.
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
  gmLayers;

type
  { TgmPosterizeLayer }

  TgmPosterizeLayer = class(TgmNonPixelizedLayer)
  private
    FLevel : Byte;
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    function GetCopy: TgmCustomLayer; override;

    property Level : Byte read FLevel write FLevel;
  end;

implementation

uses
{ Graphics32 }
  GR32_LowLevel;

{$R gmPosterizeLayerIcons.res}

{ TgmPosterizeLayer }

constructor TgmPosterizeLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Posterize';
  FLevel            := 2;
  FLogoThumbEnabled := True;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.LoadFromResourceName(HInstance, 'POSTERIZELAYERLOGO');

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

function TgmPosterizeLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmPosterizeLayer;
begin
  LLayer := TgmPosterizeLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);
  LLayer.Level := Self.FLevel;

  Result := LLayer;
end;

procedure TgmPosterizeLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
var
  LForeColor     : TColor32;
  LLevel, LAlpha : Byte;
  rr, gg, bb     : Byte;
begin
  LLevel := 255 - FLevel;

  if LLevel < 2 then
  begin
    Exit;
  end;

  LAlpha := B shr 24 and $FF;

  // only process when the background pixel is not transparent
  if LAlpha > 0 then
  begin
    // Channel Separation
    rr := B shr 16 and $FF;
    gg := B shr  8 and $FF;
    bb := B        and $FF;

    // Changing Color
    rr := Clamp( Round(rr / LLevel) * LLevel, 0, 255 );
    gg := Clamp( Round(gg / LLevel) * LLevel, 0, 255 );
    bb := Clamp( Round(bb / LLevel) * LLevel, 0, 255 );

    // get new modulated color
    LForeColor := (B and $FF000000) or (rr shl 16) or (gg shl 8) or bb;

    // blending
    FLayerBlendEvent(LForeColor, B, M);
  end;
end;

end.
