unit gmGradientFillLayer;

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
 * The Original Code is gmGradientFillLayer.pas.
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
  gmGradientRender,
{ GraphicsMagic lib }
  gmCrop,
  gmLayers,
  gmResamplers,
  gmTypes;

type
  TgmGradientFillSettings = record
    Style          : TgmGradientRenderMode;
    Angle          : Integer;
    Scale          : Double;
    TranslateX     : Integer;
    TranslateY     : Integer;
    Reversed       : Boolean;

    StartPoint     : TPoint;
    EndPoint       : TPoint;
    CenterPoint    : TPoint;

    OriginalCenter : TPoint;
    OriginalStart  : TPoint;
    OriginalEnd    : TPoint;
  end;

  { TgmGradientFillLayer }

  TgmGradientFillLayer = class(TgmSpecialPixelizedLayer)
  private
    FGradient       : TgmGradientItem;
    FStyle          : TgmGradientRenderMode;
    FAngle          : Integer;
    FScale          : Double;
    FTranslateX     : Integer;
    FTranslateY     : Integer;
    FReversed       : Boolean;

    FStartPoint     : TPoint;
    FEndPoint       : TPoint;
    FCenterPoint    : TPoint;

    FOriginalCenter : TPoint;
    FOriginalStart  : TPoint;
    FOriginalEnd    : TPoint;

    procedure DrawLayerLogo;
    procedure SetAngle(const AAngle: Integer);
    procedure SetGradient(AGradient: TgmGradientItem);
    procedure SetReverse(const AReverse: Boolean);
    procedure SetScale(const AScale: Double);
    procedure SetStyle(const AStyle: TgmGradientRenderMode);
    procedure SetTranslateX(const AValue: Integer);
    procedure SetTranslateY(const AValue: Integer);
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    procedure CalculateGradientCoord;
    procedure DrawGradientOnLayer;
    procedure Setup(const ASettings: TgmGradientFillSettings);
    procedure UpdateLogoThumbnail; override;

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); override;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions); override;

    procedure RotateCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32); override;

    property Angle          : Integer               read FAngle          write SetAngle;
    property CenterPoint    : TPoint                read FCenterPoint    write FCenterPoint;
    property EndPoint       : TPoint                read FEndPoint       write FEndPoint;
    property Gradient       : TgmGradientItem       read FGradient       write SetGradient;
    property IsReversed     : Boolean               read FReversed       write SetReverse;
    property OriginalCenter : TPoint                read FOriginalCenter write FOriginalCenter;
    property OriginalEnd    : TPoint                read FOriginalEnd    write FOriginalEnd;
    property OriginalStart  : TPoint                read FOriginalStart  write FOriginalStart;
    property Scale          : Double                read FScale          write SetScale;
    property StartPoint     : TPoint                read FStartPoint     write FStartPoint;
    property Style          : TgmGradientRenderMode read FStyle          write SetStyle;
    property TranslateX     : Integer               read FTranslateX     write SetTranslateX;
    property TranslateY     : Integer               read FTranslateY     write SetTranslateY;
  end;

implementation

uses
{ Standard }
  Math,
{ GraphicsMagic Lib }
  gmMath;

const
  LOGO_BITMAP_SIZE = 31;

{ TgmGradientFillLayer }

constructor TgmGradientFillLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Gradient Fill';
  FLogoThumbEnabled := True;

  FGradient := TgmGradientItem.Create(nil);

  FStyle       := grmLinear;
  FAngle       := 0 - 90;
  FScale       := 1.0;
  FCenterPoint := Point(FLayerBitmap.Width div 2, FLayerBitmap.Height div 2);
  FTranslateX  := 0;
  FTranslateY  := 0;
  FReversed    := False;

  CalculateGradientCoord();

  FOriginalCenter := FCenterPoint;
  FOriginalStart  := FStartPoint;
  FOriginalEnd    := FEndPoint;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.SetSize(LOGO_BITMAP_SIZE, LOGO_BITMAP_SIZE);

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

destructor TgmGradientFillLayer.Destroy;
begin
  FGradient.Free();
  inherited;
end;

procedure TgmGradientFillLayer.CalculateGradientCoord;
var
  LRadius, LRadians : Double;
  LWidth, LHeight   : Integer;
begin
  LWidth  := FLayerBitmap.Width;
  LHeight := FLayerBitmap.Height;
  LRadius := MinValue([LWidth / 2, LHeight / 2]);
  LRadius := LRadius * FScale;

  LRadians       := DegToRad(FAngle);
  FStartPoint    := CalcOffsetPointByRadian(FCenterPoint, LRadians, LRadius);
  FEndPoint      := CalcOffsetPointByRadian(FCenterPoint, LRadians + PI, LRadius);
  FOriginalStart := CalcOffsetPointByRadian(FOriginalCenter, LRadians, LRadius);
  FOriginalEnd   := CalcOffsetPointByRadian(FOriginalCenter, LRadians + PI, LRadius);
end;

procedure TgmGradientFillLayer.CropLayer(ACrop: TgmCrop;
  const ABackColor: TColor32);
var
  LOldWidth  : Integer;
  LOldHeight : Integer;
  LNewWidth  : Integer;
  LNewHeight : Integer;
begin
  if Assigned(ACrop) then
  begin
    LOldWidth  := FLayerBitmap.Width;
    LOldHeight := FLayerBitmap.Height;
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

      FCenterPoint    := ScalePoint(FCenterPoint, LOldWidth, LOldHeight, LNewWidth, LNewHeight);
      FOriginalCenter := Point(LNewWidth div 2, LNewHeight div 2);
      FTranslateX     := FTranslateX * LNewWidth div LOldWidth;
      FTranslateY     := FTranslateY * LNewHeight div LOldHeight;

      CalculateGradientCoord();
      DrawGradientOnLayer();
    end;
  end;
end;

procedure TgmGradientFillLayer.CropLayerRect(const ACropArea: TRect;
  const ABackgroundColor: TColor32);
var
  LCropRect  : TRect;
  LOldWidth  : Integer;
  LOldHeight : Integer;
  LNewWidth  : Integer;
  LNewHeight : Integer;
begin
  if (ACropArea.Left = ACropArea.Right) or
     (ACropArea.Top = ACropArea.Bottom) then
  begin
    Exit;
  end;

  LCropRect.Left   := Min(ACropArea.Left, ACropArea.Right);
  LCropRect.Top    := Min(ACropArea.Top, ACropArea.Bottom);
  LCropRect.Right  := Max(ACropArea.Left, ACropArea.Right);
  LCropRect.Bottom := Max(ACropArea.Top, ACropArea.Bottom);

  LOldWidth  := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;
  LNewWidth  := LCropRect.Right - LCropRect.Left;
  LNewHeight := LCropRect.Bottom - LCropRect.Top;

  inherited;

  FCenterPoint    := ScalePoint(FCenterPoint, LOldWidth, LOldHeight, LNewWidth, LNewHeight);
  FOriginalCenter := Point(LNewWidth div 2, LNewHeight div 2);
  FTranslateX     := FTranslateX * LNewWidth div LOldWidth;
  FTranslateY     := FTranslateY * LNewHeight div LOldHeight;

  CalculateGradientCoord();
  DrawGradientOnLayer();
end;

procedure TgmGradientFillLayer.DrawGradientOnLayer;
begin
  case FStyle of
    grmLinear:
      begin
        DrawLinearGradient(FLayerBitmap, FEndPoint, FStartPoint, FGradient, FReversed);
      end;

    grmRadial:
      begin
        DrawRadialGradient(FLayerBitmap, FCenterPoint, FStartPoint, FGradient, FReversed);
      end;

    grmAngle:
      begin
        DrawAngleGradient(FLayerBitmap, FCenterPoint, FStartPoint, FGradient, FReversed);
      end;

    grmReflected:
      begin
        DrawReflectedGradient(FLayerBitmap, FStartPoint, FEndPoint, FGradient, FReversed);
      end;

    grmDiamond:
      begin
        DrawDiamondGradient(FLayerBitmap, FCenterPoint, FStartPoint, FGradient, FReversed);
      end;
  end;
end;

procedure TgmGradientFillLayer.DrawLayerLogo;
var
  LGradientBmp : TBitmap32;
begin
  if not Assigned(FGradient) then
  begin
    Exit;
  end;

  // draw the Photoshop-style thumbnail for the gradient fill layer
  
  LGradientBmp := TBitmap32.Create();
  try
    LGradientBmp.Assign( FGradient.CachedBitmap(LOGO_BITMAP_SIZE, 22) );
    LGradientBmp.DrawMode := dmBlend;

    with FLogoBitmap do
    begin
      Clear(clWhite32);
      Draw(0, 0, LGradientBmp);

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

  finally
    LGradientBmp.Free();
  end;
end;

function TgmGradientFillLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmGradientFillLayer;
begin
  LLayer := TgmGradientFillLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  LLayer.Gradient.Assign(Self.FGradient);

  LLayer.FStyle          := Self.FStyle;
  LLayer.FAngle          := Self.FAngle;
  LLayer.FScale          := Self.FScale;
  LLayer.FTranslateX     := Self.FTranslateX;
  LLayer.FTranslateY     := Self.FTranslateY;
  LLayer.FReversed       := Self.FReversed;
  LLayer.FStartPoint     := Self.FStartPoint;
  LLayer.FEndPoint       := Self.FEndPoint;
  LLayer.FCenterPoint    := Self.FCenterPoint;
  LLayer.FOriginalCenter := Self.FOriginalCenter;
  LLayer.FOriginalStart  := Self.FOriginalStart;
  LLayer.FOriginalEnd    := Self.FOriginalEnd;

  Result := LLayer;
end;

procedure TgmGradientFillLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
begin
  FLayerBlendEvent(F, B, M);
end;

procedure TgmGradientFillLayer.ResizeLayerCanvas(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
var
  LOffsetVector         : TPoint;
  LOldWidth, LOldHeight : Integer;
begin
  LOldWidth  := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;

  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (LOldWidth <> ANewWidth) or (LOldHeight <> ANewHeight) then
    begin
      inherited;

      LOffsetVector := CalcOffsetCoordinateByAnchorDirection(
        LOldWidth, LOldHeight, ANewWidth, ANewHeight, AAnchor);

      FCenterPoint    := Point(FCenterPoint.X + LOffsetVector.X, FCenterPoint.Y + LOffsetVector.Y);
      FOriginalCenter := Point(ANewWidth div 2, ANewHeight div 2);
      FTranslateX     := FTranslateX + LOffsetVector.X;
      FTranslateY     := FTranslateY + LOffsetVector.Y;

      CalculateGradientCoord();
      DrawGradientOnLayer();
    end;
  end;
end;

procedure TgmGradientFillLayer.ResizeLayerImage(
  const ANewWidth, ANewHeight: Integer;
  const ASamplingOptions: TgmResamplingOptions);
var
  LOldWidth, LOldHeight : Integer;
begin
  LOldWidth  := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;

  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (LOldWidth <> ANewWidth) or (LOldHeight <> ANewHeight) then
    begin
      inherited;

      FCenterPoint    := ScalePoint(FCenterPoint, LOldWidth, LOldHeight, ANewWidth, ANewHeight);
      FOriginalCenter := Point(ANewWidth div 2, ANewHeight div 2);
      FTranslateX     := FTranslateX * ANewWidth div LOldWidth;
      FTranslateY     := FTranslateY * ANewHeight div LOldHeight;

      CalculateGradientCoord();
      DrawGradientOnLayer();
    end;
  end;
end;

procedure TgmGradientFillLayer.RotateCanvas(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
var
  LOldWidth, LOldHeight : Integer;
  LNewWidth, LNewHeight : Integer;
begin
  LOldWidth  := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;

  inherited;

  LNewWidth  := FLayerBitmap.Width;
  LNewHeight := FLayerBitmap.Height;

  if (LOldWidth <> LNewWidth) or (LOldHeight <> LNewHeight) then
  begin
    FCenterPoint    := ScalePoint(FCenterPoint, LOldWidth, LOldHeight, LNewWidth, LNewHeight);
    FOriginalCenter := Point(LNewWidth div 2, LNewHeight div 2);
    FTranslateX     := FTranslateX * LNewWidth div LOldWidth;
    FTranslateY     := FTranslateY * LNewHeight div LOldHeight;

    CalculateGradientCoord();
    DrawGradientOnLayer();
  end;
end;

procedure TgmGradientFillLayer.SetAngle(const AAngle: Integer);
begin
  if FAngle <> AAngle then
  begin
    FAngle := AAngle;
    
    CalculateGradientCoord();
    DrawGradientOnLayer();
  end;
end;

procedure TgmGradientFillLayer.SetGradient(AGradient: TgmGradientItem);
begin
  if Assigned(AGradient) then
  begin
    FGradient.Assign(AGradient);
    DrawGradientOnLayer();
  end;
end;

procedure TgmGradientFillLayer.SetReverse(const AReverse: Boolean);
begin
  if FReversed <> AReverse then
  begin
    FReversed := AReverse;
    DrawGradientOnLayer();
  end;
end;

procedure TgmGradientFillLayer.SetScale(const AScale: Double);
begin
  if FScale <> AScale then
  begin
    FScale := AScale;
    CalculateGradientCoord();
    DrawGradientOnLayer();
  end;
end;

procedure TgmGradientFillLayer.SetStyle(
  const AStyle: TgmGradientRenderMode);
begin
  if FStyle <> AStyle then
  begin
    FStyle := AStyle;
    DrawGradientOnLayer();
  end;
end;

procedure TgmGradientFillLayer.SetTranslateX(const AValue: Integer);
begin
  if FTranslateX <> AValue then
  begin
    FTranslateX    := AValue;
    FCenterPoint.X := FOriginalCenter.X + AValue;
    FStartPoint.X  := FOriginalStart.X  + AValue;
    FEndPoint.X    := FOriginalEnd.X    + AValue;

    DrawGradientOnLayer();
  end;
end;

procedure TgmGradientFillLayer.SetTranslateY(const AValue: Integer);
begin
  if FTranslateY <> AValue then
  begin
    FTranslateY    := AValue;
    FCenterPoint.Y := FOriginalCenter.Y + AValue;
    FStartPoint.Y  := FOriginalStart.Y  + AValue;
    FEndPoint.Y    := FOriginalEnd.Y    + AValue;

    DrawGradientOnLayer();
  end;
end;

// set up several properties with just one call
procedure TgmGradientFillLayer.Setup(const ASettings: TgmGradientFillSettings);
begin
  FStyle          := ASettings.Style;
  FAngle          := ASettings.Angle;
  FScale          := ASettings.Scale;
  FTranslateX     := ASettings.TranslateX;
  FTranslateY     := ASettings.TranslateY;
  FReversed       := ASettings.Reversed;
  FStartPoint     := ASettings.StartPoint;
  FEndPoint       := ASettings.EndPoint;
  FCenterPoint    := ASettings.CenterPoint;
  FOriginalCenter := ASettings.OriginalCenter;
  FOriginalStart  := ASettings.OriginalStart;
  FOriginalEnd    := ASettings.OriginalEnd;
end;

procedure TgmGradientFillLayer.UpdateLogoThumbnail;
begin
  DrawLayerLogo();
  inherited;
end; 

end.
