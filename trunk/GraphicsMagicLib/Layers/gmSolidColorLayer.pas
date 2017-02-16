unit gmSolidColorLayer;

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
 * The Original Code is gmSolidColorLayer.pas.
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
  { TgmSolidColorLayer }

  TgmSolidColorLayer = class(TgmNonPixelizedLayer)
  private
    FSolidColor : TColor32;

    procedure DrawLayerLogoInPhotoshopStyle;
    procedure SetSolidColor(const AColor: TColor32);
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    function GetCopy: TgmCustomLayer; override;

    procedure UpdateLogoThumbnail; override;

    property SolidColor: TColor32 read FSolidColor write SetSolidColor;
  end;


implementation

const
  LOGO_BITMAP_WIDTH  = 31;
  LOGO_BITMAP_HEIGHT = 31;

{ TgmSolidColorLayer }

constructor TgmSolidColorLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Color Fill';
  FLogoThumbEnabled := True;
  FSolidColor       := clBlack32;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.SetSize(LOGO_BITMAP_WIDTH, LOGO_BITMAP_HEIGHT);

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

procedure TgmSolidColorLayer.DrawLayerLogoInPhotoshopStyle;
begin
  // drawing layer logo by hard-coded
  with FLogoBitmap do
  begin
    Clear(clWhite32);
    FillRect(0, 0, 31, 22, FSolidColor);

    PenColor := clBlack32;

    MoveTo(0, 22);
    LineToS(31, 22);

    MoveTo(4, 25);
    LineToS(26, 25);

    MoveTo(17, 26);
    LineToS(15, 28);

    MoveTo(17, 26);
    LineToS(19, 28);

    MoveTo(15, 28);
    LineToS(19, 28);
  end;
end;

function TgmSolidColorLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmSolidColorLayer;
begin
  LLayer := TgmSolidColorLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.LayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);
  LLayer.SolidColor := Self.FSolidColor;

  Result := LLayer;
end;

procedure TgmSolidColorLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
begin
  FLayerBlendEvent(FSolidColor, B, M);
end;

procedure TgmSolidColorLayer.SetSolidColor(const AColor: TColor32);
begin
  FSolidColor := $FF000000 or AColor;
end;

procedure TgmSolidColorLayer.UpdateLogoThumbnail;
begin
  DrawLayerLogoInPhotoshopStyle();

  inherited;
end; 

end.
