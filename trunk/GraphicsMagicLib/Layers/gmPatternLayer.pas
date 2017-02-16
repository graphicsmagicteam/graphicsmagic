unit gmPatternLayer;

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
 * The Original Code is gmPatternLayer.pas.
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
  gmCrop,
  gmLayers,
  gmResamplers,
  gmTypes;

type
  { TgmPatternLayer }

  TgmPatternLayer = class(TgmSpecialPixelizedLayer)
  private
    FPatternBitmap : TBitmap32;
    FScale         : Double;

    procedure DrawLayerLogo;
    procedure SetPatternBitmap(APatternBmp: TBitmap32);
    procedure SetScale(const AScale: Double);
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    procedure FillPatternOnLayer;
    procedure Setup(APatternBmp: TBitmap32; const AScale: Double);
    procedure UpdateLogoThumbnail; override;

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); override;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); override;
    
    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions); override;

    procedure RotateCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32); override;

    property PatternBitmap : TBitmap32 read FPatternBitmap write SetPatternBitmap;
    property Scale         : Double    read FScale         write SetScale;
  end;

implementation

uses
{ Delphi }
  Math,
{ GraphicsMagic Lib }
  gmImageProcessFuncs,
  gmPaintFuncs;

const
  LOGO_BITMAP_SIZE = 31;
  

{ TgmPatternLayer }

constructor TgmPatternLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Pattern Fill';
  FLogoThumbEnabled := True;

  FPatternBitmap := nil;
  FScale         := 1.0;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.SetSize(LOGO_BITMAP_SIZE, LOGO_BITMAP_SIZE);

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

destructor TgmPatternLayer.Destroy;
begin
  FPatternBitmap.Free();
  inherited;
end;

procedure TgmPatternLayer.CropLayer(ACrop: TgmCrop; const ABackColor: TColor32);
var
  LNewWidth  : Integer;
  LNewHeight : Integer;
begin
  if Assigned(ACrop) then
  begin
    LNewWidth  := Abs(ACrop.FCropEnd.X - ACrop.FCropStart.X);
    LNewHeight := Abs(ACrop.FCropEnd.Y - ACrop.FCropStart.Y);

    if ACrop.IsResized then
    begin
      LNewWidth  := ACrop.ResizeW;
      LNewHeight := ACrop.ResizeH;
    end;

    if (LNewWidth > 0) and (LNewHeight > 0) then
    begin
      inherited;
      FillPatternOnLayer();
    end;
  end;
end;

procedure TgmPatternLayer.CropLayerRect(const ACropArea: TRect;
  const ABackgroundColor: TColor32);
begin
  if (ACropArea.Left = ACropArea.Right) or
     (ACropArea.Top = ACropArea.Bottom) then
  begin
    Exit;
  end;

  inherited;
  FillPatternOnLayer();
end;

procedure TgmPatternLayer.DrawLayerLogo;
var
  LLeft, LTop : Integer;
  LScaledBmp  : TBitmap32;
begin
  if not Assigned(FPatternBitmap) then
  begin
    Exit;
  end;

  // draw Photoshop-Style thumbnail for the pattern layer

  with FLogoBitmap do
  begin
    Clear(clWhite32);

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

  LScaledBmp := TBitmap32.Create();
  try
    LScaledBmp.DrawMode := dmBlend;

    GetScaledBitmap(FPatternBitmap, LScaledBmp, 18, 18);

    LLeft := 15 - LScaledBmp.Width  div 2;
    LTop  := 12 - LScaledBmp.Height div 2;

    FLogoBitmap.Draw(LLeft, LTop, LScaledBmp);
  finally
    LScaledBmp.Free();
  end;
end;

procedure TgmPatternLayer.FillPatternOnLayer;
var
  sw, sh     : Integer;
  LScaledBmp : TBitmap32;
begin
  if Assigned(FPatternBitmap) then
  begin
    FLayerBitmap.Clear($00000000);

    sw := Round(FPatternBitmap.Width  * FScale);
    sh := Round(FPatternBitmap.Height * FScale);

    LScaledBmp := TBitmap32.Create();
    try
      LScaledBmp.Assign(FPatternBitmap);
      LScaledBmp.DrawMode := dmBlend;

      SmoothResize32(LScaledBmp, sw, sh);
      DrawPattern(FLayerBitmap, LScaledBmp);
    finally
      LScaledBmp.Free();
    end;
  end;
end;

function TgmPatternLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmPatternLayer;
begin
  LLayer := TgmPatternLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  LLayer.PatternBitmap := Self.FPatternBitmap;
  LLayer.Scale         := Self.FScale;

  Result := LLayer;
end;

procedure TgmPatternLayer.LayerBlend(F: TColor32; var B: TColor32; M: TColor32);
begin
  FLayerBlendEvent(F, B, M);
end;

procedure TgmPatternLayer.ResizeLayerCanvas(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (FLayerBitmap.Width <> ANewWidth) or
       (FLayerBitmap.Height <> ANewHeight) then
    begin
      inherited;
      FillPatternOnLayer();
    end;
  end;
end;

procedure TgmPatternLayer.ResizeLayerImage(
  const ANewWidth, ANewHeight: Integer;
  const ASamplingOptions: TgmResamplingOptions);
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (FLayerBitmap.Width <> ANewWidth) or
       (FLayerBitmap.Height <> ANewHeight) then
    begin
      inherited;
      FillPatternOnLayer();
    end;
  end;
end;

procedure TgmPatternLayer.RotateCanvas(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
begin
  inherited;
  FillPatternOnLayer();
end;

procedure TgmPatternLayer.SetPatternBitmap(APatternBmp: TBitmap32);
begin
  if Assigned(APatternBmp) then
  begin
    if not Assigned(FPatternBitmap) then
    begin
      FPatternBitmap := TBitmap32.Create();
    end;

    FPatternBitmap.Assign(APatternBmp);
    FPatternBitmap.DrawMode := dmBlend;

    FillPatternOnLayer();
  end;
end;

procedure TgmPatternLayer.SetScale(const AScale: Double);
begin
  if FScale <> AScale then
  begin
    FScale := AScale;
    
    FillPatternOnLayer();
  end;
end;

// setting pattern layer up with one method
procedure TgmPatternLayer.Setup(APatternBmp: TBitmap32; const AScale: Double);
begin
  if Assigned(APatternBmp) then
  begin
    if not Assigned(FPatternBitmap) then
    begin
      FPatternBitmap := TBitmap32.Create();
    end;

    FPatternBitmap.Assign(APatternBmp);
    FPatternBitmap.DrawMode := dmBlend;
  end;

  if FScale <> AScale then
  begin
    FScale := AScale;
  end;
end;

procedure TgmPatternLayer.UpdateLogoThumbnail;
begin
  DrawLayerLogo();
  inherited;
end; 

end.
