unit gmGradientMapLayer;

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
 * The Original Code is gmGradientMapLayer.pas.
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
{ GraphicsMagic Package Lib }
  gmGradient,
{ GraphicsMagic lib }
  gmLayers;

type
  { TgmGradientMapLayer }

  TgmGradientMapLayer = class(TgmNonPixelizedLayer)
  private
    FGradient : TgmGradientItem;
    FReversed : Boolean;

    procedure SetGradient(AGradient: TgmGradientItem);
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    property Gradient   : TgmGradientItem read FGradient write SetGradient;
    property IsReversed : Boolean         read FReversed write FReversed;
  end;

implementation

{$R gmGradientMapLayerIcons.res}

{ TgmGradientMapLayer }

constructor TgmGradientMapLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Gradient Map';
  FLogoThumbEnabled := True;

  FGradient := TgmGradientItem.Create(nil);
  with FGradient do
  begin
    GradientLength := 256;
    RefreshColorArray();
  end;

  FReversed := False;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.LoadFromResourceName(HInstance, 'GRADIENTMAPLAYERLOGO');

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

destructor TgmGradientMapLayer.Destroy;
begin
  FGradient.Free();
  inherited;
end;

function TgmGradientMapLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmGradientMapLayer;
begin
  LLayer := TgmGradientMapLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  LLayer.Gradient   := Self.FGradient;
  LLayer.IsReversed := Self.FReversed;

  Result := LLayer;
end;

procedure TgmGradientMapLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
var
  LForeColor : TColor32;
  LAlpha     : Byte;
  LGrayscale : Byte;
  LIndex     : Byte;
  rr, gg, bb : Byte;
begin
  LAlpha := B shr 24 and $FF;

  // only process when the background pixel is not transparent
  if LAlpha > 0 then
  begin
    rr := B shr 16 and $FF;
    gg := B shr  8 and $FF;
    bb := B        and $FF;

    LGrayscale := (rr + gg + bb) div 3;

    if FReversed then
    begin
      LIndex := 255 - LGrayscale;
    end
    else
    begin
      LIndex := LGrayscale;
    end;

    LForeColor := (LAlpha shl 24) or (FGradient.OutputColors[LIndex] and $FFFFFF);

    // blending
    FLayerBlendEvent(LForeColor, B, M);
  end;
end;

procedure TgmGradientMapLayer.SetGradient(AGradient: TgmGradientItem);
begin
  if Assigned(AGradient) then
  begin
    FGradient.Assign(AGradient);
    FGradient.GradientLength := 256;
    FGradient.RefreshColorArray();
  end;
end;

end.
