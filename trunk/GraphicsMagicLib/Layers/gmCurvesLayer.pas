unit gmCurvesLayer;

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
 * The Original Code is gmCurvesLayer.pas.
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
  gmCurvesTool,
  gmLayers;

type
  { TgmCurvesLayer }

  TgmCurvesLayer = class(TgmNonPixelizedLayer)
  private
    FCurvesTool : TgmCurvesTool;
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    procedure CalculateHistogram(ASourceBitmap: TBitmap32);

    property CurvesTool : TgmCurvesTool read FCurvesTool;
  end;

implementation

{$R gmCurvesLayerIcons.res}

{ TgmCurvesLayer }

constructor TgmCurvesLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Curves';
  FLogoThumbEnabled := True;

  FCurvesTool := TgmCurvesTool.Create(FLayerBitmap);
  FCurvesTool.LUTSetup(3);

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.LoadFromResourceName(HInstance, 'CURVESLAYERLOGO');

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

destructor TgmCurvesLayer.Destroy;
begin
  FCurvesTool.Free();
  inherited;
end;

procedure TgmCurvesLayer.CalculateHistogram(ASourceBitmap: TBitmap32);
begin
  FCurvesTool.Hist.gimp_histogram_calculate(ASourceBitmap);
  FCurvesTool.Hist.gimp_histogram_view_expose(FCurvesTool.Channel);
end;

function TgmCurvesLayer.GetCopy: TgmCustomLayer;
var
  i, j, k : Integer;
  LLayer  : TgmCurvesLayer;
begin
  LLayer := TgmCurvesLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  for j := Low(FCurvesTool.LUT.luts) to High(FCurvesTool.LUT.luts) do
  begin
    for i := Low(FCurvesTool.LUT.luts[j]) to High(FCurvesTool.LUT.luts[j]) do
    begin
      LLayer.CurvesTool.LUT.luts[j, i] := Self.FCurvesTool.LUT.luts[j, i];
    end;
  end;

  for i := 0 to 4 do
  begin
    LLayer.CurvesTool.Curves.CurveType[i] := Self.FCurvesTool.Curves.CurveType[i];
  end;

  for i := 0 to 4 do
  begin
    for j := 0 to 16 do
    begin
      for k := 0 to 1 do
      begin
        LLayer.CurvesTool.Curves.Points[i, j, k] := Self.FCurvesTool.Curves.Points[i, j, k];
      end;
    end;
  end;

  for i := 0 to 4 do
  begin
    for j := 0 to 255 do
    begin
      LLayer.CurvesTool.Curves.FCurve[i, j] := Self.FCurvesTool.Curves.FCurve[i, j];
    end;
  end;

  LLayer.CurvesTool.Channel   := Self.FCurvesTool.Channel;
  LLayer.CurvesTool.Scale     := Self.FCurvesTool.Scale;
  LLayer.CurvesTool.CurveType := Self.FCurvesTool.CurveType;

  Result := LLayer;
end;

procedure TgmCurvesLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
var
  LForeColor : TColor32;
  LAlpha     : Byte;
  rr, gg, bb : Byte;
begin
  LAlpha := B shr 24 and $FF;

  // only process when the background pixel is not transparent
  if LAlpha > 0 then
  begin
    rr := B shr 16 and $FF;
    gg := B shr  8 and $FF;
    bb := B        and $FF;

    rr := FCurvesTool.LUT.luts[0, rr];
    gg := FCurvesTool.LUT.luts[1, gg];
    bb := FCurvesTool.LUT.luts[2, bb];

    LForeColor := (LAlpha shl 24) or (rr shl 16) or (gg shl 8) or bb;

    // blending
    FLayerBlendEvent(LForeColor, B, M);
  end;
end;


end.
