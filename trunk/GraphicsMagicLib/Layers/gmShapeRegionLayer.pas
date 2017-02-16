unit gmShapeRegionLayer;

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
 * The Original Code is gmShapeRegionLayer.pas.
 *
 * The Initial Developer of the Original Code are
 * Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2015/11/15

{$WARN UNSAFE_CAST OFF}

interface

uses
{ Standard }
  Graphics,
{ Graphics32 }
  GR32,
{ GraphicsMagic lib }
  gmCrop,
  gmLayers,
  gmResamplers,
  gmShapes,
  gmTypes;

type
  { TgmShapeRegionLayer }

  TgmShapeRegionLayer = class(TgmSpecialPixelizedLayer)
  private
    FDismissed        : Boolean;  // indicating whether the region is dissmissing for editing (editing is complete)
    FShapeOutlineList : TgmOutlineList;
    FShapeRegion      : TgmShapeRegion;

    function GetBrushStyle: TBrushStyle;
    function GetRegionColor: TColor;

    procedure DrawLayerLogo;
    procedure SetBrushStyle(const AStyle: TBrushStyle);
    procedure SetRegionColor(const AColor: TColor);
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    procedure DrawRegionOnLayer;
    procedure UpdateLayerThumbnail; override;
    procedure UpdateLogoThumbnail; override;

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); override;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions); override;

    procedure RotateCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32); override;

    property BrushStyle       : TBrushStyle    read GetBrushStyle  write SetBrushStyle;
    property IsDismissed      : Boolean        read FDismissed     write FDismissed;
    property RegionColor      : TColor         read GetRegionColor write SetRegionColor;
    property ShapeOutlineList : TgmOutlineList read FShapeOutlineList;
    property ShapeRegion      : TgmShapeRegion read FShapeRegion;
  end;

implementation

uses
{ Standard }
  Windows,
  Classes,
  Math,
{ GraphicsMagic Lib }
  gmMath;

const
  LOGO_BITMAP_SIZE = 31;

{ TgmShapeRegionLayer }

constructor TgmShapeRegionLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName  := 'Shape Region';
  FLayerThumbEnabled := True;
  FLogoThumbEnabled  := True;

  FShapeOutlineList        := TgmOutlineList.Create();
  FShapeRegion             := TgmShapeRegion.Create();
  FShapeRegion.BrushStyle  := bsSolid;
  FShapeRegion.RegionColor := clBlack;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.SetSize(LOGO_BITMAP_SIZE, LOGO_BITMAP_SIZE);

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  FLayerThumb := TBitmap32.Create();
  with FLayerThumb do
  begin
    SetSize(LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
  end;

  UpdateLogoThumbnail();
  UpdateLayerThumbnail();
end;

destructor TgmShapeRegionLayer.Destroy;
begin
  FShapeOutlineList.Free();
  FShapeRegion.Free();
  
  inherited;
end;

procedure TgmShapeRegionLayer.CropLayer(ACrop: TgmCrop;
  const ABackColor: TColor32);
var
  i, j          : Integer;
  LCropWidth    : Integer;
  LCropHeight   : Integer;
  LNewWidth     : Integer;
  LNewHeight    : Integer;
  LOffsetVector : TPoint;
  LOutline      : TgmShapeOutline;
begin
  if Assigned(ACrop) then
  begin
    LCropWidth  := Abs(ACrop.FCropEnd.X - ACrop.FCropStart.X);
    LCropHeight := Abs(ACrop.FCropEnd.Y - ACrop.FCropStart.Y);
    LNewWidth   := LCropWidth;
    LNewHeight  := LCropHeight;

    if ACrop.IsResized then
    begin
      LNewWidth  := ACrop.ResizeW;
      LNewHeight := ACrop.ResizeH;
    end;

    if (LNewWidth > 0) and (LNewHeight > 0) then
    begin
      inherited;

      if FShapeOutlineList.Count > 0 then
      begin
        LOffsetVector.X := FShapeOutlineList.FBoundaryTL.X - ACrop.FCropStart.X;
        LOffsetVector.Y := FShapeOutlineList.FBoundaryTL.Y - ACrop.FCropStart.Y;

        for i := 0 to (FShapeOutlineList.Count - 1) do
        begin
          LOutline := FShapeOutlineList.Items[i];

          case LOutline.ShapeRegionTool of
            srtRectangle,
            srtRoundedRect,
            srtEllipse:
              begin
                // calculating the offset
                with LOutline do
                begin
                  StartPoint := Point(StartPoint.X - FShapeOutlineList.FBoundaryTL.X + LOffsetVector.X,
                                      StartPoint.Y - FShapeOutlineList.FBoundaryTL.Y + LOffsetVector.Y);

                  EndPoint := Point(EndPoint.X - FShapeOutlineList.FBoundaryTL.X + LOffsetVector.X,
                                    EndPoint.Y - FShapeOutlineList.FBoundaryTL.Y + LOffsetVector.Y);

                  // scaling
                  if ACrop.IsResized then
                  begin
                    StartPoint := ScalePoint(StartPoint, LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                    EndPoint   := ScalePoint(EndPoint,   LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                  end;
                end;
              end;

            srtPolygon,
            srtLine:
              begin
                for j := Low(LOutline.FPolygon) to High(LOutline.FPolygon) do
                begin
                  with LOutline do
                  begin
                    // calculating the offset
                    FPolygon[j].X := FPolygon[j].X - FShapeOutlineList.FBoundaryTL.X + LOffsetVector.X;
                    FPolygon[j].Y := FPolygon[j].Y - FShapeOutlineList.FBoundaryTL.Y + LOffsetVector.Y;

                    // scaling
                    if ACrop.IsResized then
                    begin
                      LOutline.FPolygon[j] := ScalePoint(LOutline.FPolygon[j],
                        LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                    end;
                  end;
                end;
              end;
          end;

          LOutline.BackupCoordinates();
        end;

        FShapeOutlineList.GetShapesBoundary();
        FShapeOutlineList.BackupCoordinates();
      end;

      FShapeRegion.AccumRGN := FShapeOutlineList.GetScaledShapesRegion();

      DrawRegionOnLayer();
      UpdateLayerThumbnail();
    end;
  end;
end;

procedure TgmShapeRegionLayer.CropLayerRect(const ACropArea: TRect;
  const ABackgroundColor: TColor32);
var
  i, j          : Integer;
  LCropRect     : TRect;
  LOffsetVector : TPoint;
  LOutline      : TgmShapeOutline;
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

  inherited;

  if FShapeOutlineList.Count > 0 then
  begin
    LOffsetVector.X := FShapeOutlineList.FBoundaryTL.X - LCropRect.Left;
    LOffsetVector.Y := FShapeOutlineList.FBoundaryTL.Y - LCropRect.Top;

    for i := 0 to (FShapeOutlineList.Count - 1) do
    begin
      LOutline := FShapeOutlineList.Items[i];

      case LOutline.ShapeRegionTool of
        srtRectangle,
        srtRoundedRect,
        srtEllipse:
          begin
            // calculating the offset
            with LOutline do
            begin
              StartPoint := Point(StartPoint.X - FShapeOutlineList.FBoundaryTL.X + LOffsetVector.X,
                                  StartPoint.Y - FShapeOutlineList.FBoundaryTL.Y + LOffsetVector.Y);

              EndPoint := Point(EndPoint.X - FShapeOutlineList.FBoundaryTL.X + LOffsetVector.X,
                                EndPoint.Y - FShapeOutlineList.FBoundaryTL.Y + LOffsetVector.Y);
            end;
          end;

        srtPolygon,
        srtLine:
          begin
            for j := Low(LOutline.FPolygon) to High(LOutline.FPolygon) do
            begin
              with LOutline do
              begin
                // calculating the offset
                FPolygon[j].X := FPolygon[j].X - FShapeOutlineList.FBoundaryTL.X + LOffsetVector.X;
                FPolygon[j].Y := FPolygon[j].Y - FShapeOutlineList.FBoundaryTL.Y + LOffsetVector.Y;
              end;
            end;
          end;
      end;

      LOutline.BackupCoordinates();
    end;

    FShapeOutlineList.GetShapesBoundary();
    FShapeOutlineList.BackupCoordinates();
  end;

  FShapeRegion.AccumRGN := FShapeOutlineList.GetScaledShapesRegion();

  DrawRegionOnLayer();
  UpdateLayerThumbnail();
end;

procedure TgmShapeRegionLayer.DrawLayerLogo;
begin
  // drawing layer logo by hard-coded
  with FLogoBitmap do
  begin
    Clear(clWhite32);
    FillRect( 0, 0, 31, 22, Color32(FShapeRegion.RegionColor) );

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

procedure TgmShapeRegionLayer.DrawRegionOnLayer;
begin
  FShapeRegion.ShowRegion(FLayerBitmap);
end;

function TgmShapeRegionLayer.GetBrushStyle: TBrushStyle;
begin
  Result := FShapeRegion.BrushStyle;
end;

function TgmShapeRegionLayer.GetCopy: TgmCustomLayer;
var
  i, j          : Integer;
  LLayer        : TgmShapeRegionLayer;
  LShapeOutline : TgmShapeOutline;
  LTempOutline  : TgmShapeOutline;
begin
  LLayer := TgmShapeRegionLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  if Self.FShapeOutlineList.Count > 0 then
  begin
    LLayer.ShapeOutlineList.FBoundaryTL := Self.FShapeOutlineList.FBoundaryTL;
    LLayer.ShapeOutlineList.FBoundaryBR := Self.FShapeOutlineList.FBoundaryBR;
    LLayer.ShapeOutlineList.TLBackup    := Self.FShapeOutlineList.TLBackup;
    LLayer.ShapeOutlineList.BRBackup    := Self.FShapeOutlineList.BRBackup;
    LLayer.ShapeOutlineList.IsStretched := Self.FShapeOutlineList.IsStretched;

    for i := 0 to (Self.FShapeOutlineList.Count - 1) do
    begin
      LShapeOutline := nil;
      LTempOutline  := Self.FShapeOutlineList.Items[i];

      case LTempOutline.ShapeRegionTool of
        srtRectangle:
          begin
            LShapeOutline := TgmRectangleOutLine.Create();
          end;

        srtRoundedRect:
          begin
            LShapeOutline := TgmRoundRectOutline.Create();
          end;
          
        srtEllipse:
          begin
            LShapeOutline := TgmEllipseOutline.Create();
          end;

        srtPolygon:
          begin
            LShapeOutline := TgmRegularPolygonOutline.Create();
          end;
          
        srtLine:
          begin
            LShapeOutline := TgmLineOutline.Create();
          end;
      end;

      LShapeOutline.StartPoint := LTempOutline.StartPoint;
      LShapeOutline.EndPoint   := LTempOutline.EndPoint;

      case LShapeOutline.ShapeRegionTool of
        srtRoundedRect:
          begin
            TgmRoundRectOutline(LShapeOutline).CornerRadius := TgmRoundRectOutline(LTempOutline).CornerRadius;
          end;
          
        srtPolygon:
          begin
            TgmRegularPolygonOutline(LShapeOutline).Sides := TgmRegularPolygonOutline(LTempOutline).Sides;
            
            SetLength( LShapeOutline.FPolygon, Length(LTempOutline.FPolygon) );
            SetLength( LShapeOutline.FPolygonBackup, Length(LTempOutline.FPolygonBackup) );

            for j := Low(LTempOutline.FPolygon) to High(LTempOutline.FPolygon) do
            begin
              LShapeOutline.FPolygon[j] := LTempOutline.FPolygon[j];
            end;

            for j := Low(LTempOutline.FPolygonBackup) to High(LTempOutline.FPolygonBackup) do
            begin
              LShapeOutline.FPolygonBackup[j] := LTempOutline.FPolygonBackup[j];
            end;
          end;

        srtLine:
          begin
            TgmLineOutline(LShapeOutline).Weight := TgmLineOutline(LTempOutline).Weight;
            
            SetLength( LShapeOutline.FPolygon, Length(LTempOutline.FPolygon) );
            SetLength( LShapeOutline.FPolygonBackup, Length(LTempOutline.FPolygonBackup) );

            for j := Low(LTempOutline.FPolygon) to High(LTempOutline.FPolygon) do
            begin
              LShapeOutline.FPolygon[j] := LTempOutline.FPolygon[j];
            end;

            for j := Low(LTempOutline.FPolygonBackup) to High(LTempOutline.FPolygonBackup) do
            begin
              LShapeOutline.FPolygonBackup[j] := LTempOutline.FPolygonBackup[j];
            end;
          end;
      end;

      LShapeOutline.StartPointBackup := LTempOutline.StartPointBackup;
      LShapeOutline.EndPointBackup   := LTempOutline.EndPointBackup;
      LShapeOutline.CombineMode      := LTempOutline.CombineMode;
      
      LLayer.ShapeOutlineList.Add(LShapeOutline);
    end;

    LLayer.ShapeRegion.BrushStyle  := Self.FShapeRegion.BrushStyle;
    LLayer.ShapeRegion.RegionColor := Self.FShapeRegion.RegionColor;
    LLayer.IsDismissed             := Self.FDismissed;
    
    LLayer.ShapeRegion.AccumRGN := LLayer.ShapeOutlineList.GetScaledShapesRegion();
  end;

  Result := LLayer;
end;

function TgmShapeRegionLayer.GetRegionColor: TColor;
begin
  Result := FShapeRegion.RegionColor;
end;

procedure TgmShapeRegionLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
begin
  FLayerBlendEvent(F, B, M);
end;

procedure TgmShapeRegionLayer.ResizeLayerCanvas(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
var
  i, j          : Integer;
  LOffsetVector : TPoint;
  LOldWidth     : Integer;
  LOldHeight    : Integer;
  LOutline      : TgmShapeOutline;
begin
  LOldWidth  := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;

  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (LOldWidth <> ANewWidth) or (LOldHeight <> ANewHeight) then
    begin
      inherited;

      if FShapeOutlineList.Count > 0 then
      begin
        LOffsetVector := CalcOffsetCoordinateByAnchorDirection(
          LOldWidth, LOldHeight, ANewWidth, ANewHeight, AAnchor);

        for i := 0 to (FShapeOutlineList.Count - 1) do
        begin
          LOutline := TgmShapeOutline(FShapeOutlineList.Items[i]);

          case LOutline.ShapeRegionTool of
            srtRectangle,
            srtRoundedRect,
            srtEllipse:
              begin
                LOutline.StartPoint := Point(LOutline.StartPoint.X + LOffsetVector.X,
                                             LOutline.StartPoint.Y + LOffsetVector.Y);

                LOutline.EndPoint := Point(LOutline.EndPoint.X + LOffsetVector.X,
                                           LOutline.EndPoint.Y + LOffsetVector.Y);
              end;

            srtPolygon,
            srtLine:
              begin
                for j := Low(LOutline.FPolygon) to High(LOutline.FPolygon) do
                begin
                  LOutline.FPolygon[j] := Point(LOutline.FPolygon[j].X + LOffsetVector.X,
                                                LOutline.FPolygon[j].Y + LOffsetVector.Y);
                end;
              end;
          end;

          LOutline.BackupCoordinates();
        end;

        FShapeOutlineList.FBoundaryTL :=
          Point(FShapeOutlineList.FBoundaryTL.X + LOffsetVector.X,
                FShapeOutlineList.FBoundaryTL.Y + LOffsetVector.Y);

        FShapeOutlineList.FBoundaryBR :=
          Point(FShapeOutlineList.FBoundaryBR.X + LOffsetVector.X,
                FShapeOutlineList.FBoundaryBR.Y + LOffsetVector.Y);

        FShapeOutlineList.BackupCoordinates();
      end;

      FShapeRegion.AccumRGN := FShapeOutlineList.GetScaledShapesRegion();

      DrawRegionOnLayer();
      UpdateLayerThumbnail();
    end;
  end;
end;

procedure TgmShapeRegionLayer.ResizeLayerImage(
  const ANewWidth, ANewHeight: Integer;
  const ASamplingOptions: TgmResamplingOptions);
var
  i, j          : Integer;
  LOldWidth     : Integer;
  LOldHeight    : Integer;
  LOriginalRect : TRect;
  LCurrentRect  : TRect;
  LOutline      : TgmShapeOutline;
begin
  LOldWidth  := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;

  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (LOldWidth <> ANewWidth) or (LOldHeight <> ANewHeight) then
    begin
      inherited;

      if FShapeOutlineList.Count > 0 then
      begin
        LOriginalRect := Rect(0, 0, LOldWidth, LOldHeight);
        LCurrentRect  := Rect(0, 0, ANewWidth, ANewHeight);

        for i := 0 to (FShapeOutlineList.Count - 1) do
        begin
          LOutline := TgmShapeOutline(FShapeOutlineList.Items[i]);

          case LOutline.ShapeRegionTool of
            srtRectangle,
            srtRoundedRect,
            srtEllipse:
              begin
                LOutline.StartPoint := ScalePoint(LOutline.StartPoint,
                                                  LOldWidth, LOldHeight,
                                                  ANewWidth, ANewHeight);

                LOutline.EndPoint := ScalePoint(LOutline.EndPoint,
                                                LOldWidth, LOldHeight,
                                                ANewWidth, ANewHeight);
              end;

            srtPolygon,
            srtLine:
              begin
                for j := Low(LOutline.FPolygon) to High(LOutline.FPolygon) do
                begin
                  LOutline.FPolygon[j] := ScalePoint(LOutline.FPolygon[j],
                                                     LOldWidth, LOldHeight,
                                                     ANewWidth, ANewHeight);
                end;
              end;
          end;

          LOutline.BackupCoordinates();
        end;

        FShapeOutlineList.GetShapesBoundary();
        FShapeOutlineList.BackupCoordinates();
      end;

      FShapeRegion.AccumRGN := FShapeOutlineList.GetScaledShapesRegion();

      DrawRegionOnLayer();
      UpdateLayerThumbnail();
    end;
  end;
end;

procedure TgmShapeRegionLayer.RotateCanvas(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
begin
  inherited;
  DrawRegionOnLayer();
  UpdateLayerThumbnail();
end;

procedure TgmShapeRegionLayer.SetBrushStyle(const AStyle: TBrushStyle);
begin
  if FShapeRegion.BrushStyle <> AStyle then
  begin
    FShapeRegion.BrushStyle := AStyle;
    FShapeRegion.ShowRegion(FLayerBitmap);
  end;
end;

procedure TgmShapeRegionLayer.SetRegionColor(const AColor: TColor);
begin
  if FShapeRegion.RegionColor <> AColor then
  begin
    FShapeRegion.RegionColor := AColor;
    FShapeRegion.ShowRegion(FLayerBitmap);
  end;
end;

procedure TgmShapeRegionLayer.UpdateLayerThumbnail;
var
  LRect         : TRect;
  LRegionBitmap : TBitmap32;
begin
  LRect := FRealThumbRect;
  
  LRegionBitmap := TBitmap32.Create();
  try
    LRegionBitmap.DrawMode := dmOpaque;
    LRegionBitmap.SetSizeFrom(FLayerBitmap);
    LRegionBitmap.Clear(clGray32);
    
    LRegionBitmap.Canvas.Brush.Style := FShapeRegion.BrushStyle;
    LRegionBitmap.Canvas.Brush.Color := clWhite;

    PaintRGN(LRegionBitmap.Canvas.Handle, FShapeRegion.AccumRGN);
    FShapeOutlineList.DrawAllOutlines( LRegionBitmap.Canvas, Point(0, 0), pmCopy );

    FLayerThumb.Clear( Color32(clBtnFace) );
    FLayerThumb.Draw(LRect, LRegionBitmap.BoundsRect, LRegionBitmap);
  finally
    LRegionBitmap.Free();
  end;

  InflateRect(LRect, 1, 1);
  FLayerThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end;
end;

procedure TgmShapeRegionLayer.UpdateLogoThumbnail;
begin
  DrawLayerLogo();

  inherited;
end; 

end.
