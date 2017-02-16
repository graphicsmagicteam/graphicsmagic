unit gmLayerFigureManager;

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

// Update Date: 2016/11/02

interface

{$WARN UNSAFE_CAST OFF}

uses
{ Standard }
  Classes,
  Graphics,
  Windows,
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmFigures,
  gmLayers,
  gmTypes;

type
  TgmFigureInfo = record
    LayerIndex  : Integer;  // indicating which layer the figure is on
    FigureIndex : Integer;  // the figure index in the figure list
  end;

  TgmArrayOfFigureInfo = array of TgmFigureInfo;
  TgmArrayOfLayerIndex = array of Integer;

  { TgmLayerFigureManager }

  TgmLayerFigureManager = class(TObject)
  private
    // Pointer to a layer list which may contains vector layers that
    // this manager will work with.
    FLayerList : TgmLayerList;
  public
    // Used for holding the data about which figure is selected
    // on a vector layer, and what the index of the vector layer is.
    FSelectedFigureInfoArray : TgmArrayOfFigureInfo;

    // used for holding the index of a vector layer that has
    // selected figures
    FSelectedFigureLayerIndexArray: TgmArrayOfLayerIndex;

    FVectorLayerIndexes : TgmArrayOfLayerIndex;

    constructor Create(ALayerList: TgmLayerList);
    destructor Destroy; override;

    procedure ClearSelectedFiguresInfo;
    procedure DeleteSelectedFigures;
    procedure DeselectAllFigures;
    procedure DeselectFiguresOnSelectedLayer;
    procedure DeselectFiguresOnVisibleLayers;
    procedure DrawAllFigures;
    procedure DrawAllFiguresOnSelectedVectorLayers;
    procedure DrawAllFiguresOnSpecifiedVectorLayer(const ALayerIndex: Integer);
    
    procedure DrawHandlesOfSelectedFigures(ABuffer: TBitmap32;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil);

    procedure DrawSelectedFiguresOnCanvas(ACanvas: TCanvas;
      const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);

    procedure DrawUnselectedAndLockedFiguresOnSelectedVectorLayers;
    procedure LockSelectedFigures;
    procedure UnlockSelectedFigures;
    procedure SelectAllFiguresOnVectorLayers;
    procedure SelectFigures(const AShift: TShiftState; const AX, AY: Integer);

    procedure SelectFiguresByRectOnVectorLayers(
      const AIncludeMode: TgmFigureSelectIncludeMode;
      const ARegionStart, ARegionEnd: TPoint);

    procedure TranslateSelectedFigures(const ATranslateVector: TPoint);
    procedure UpdateSelectedFiguresInfo;
    procedure UpdateThumbnailOnSelectedVectorLayers;
    procedure UpdateVectorLayerIndexRecord;

    function GetHandleOfSelectedFigures(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmDrawingHandle;

    function GetOnlyOneSelectedFigure: TgmFigureObject;
    function GetOnlyOneSelectedFigureIndex: Integer;
    function GetOnlyOneSelectedFigureLayerIndex: Integer;
    function HasFiguresOnVectorLayers: Boolean;
    function HasSelectedLockedFigures: Boolean;
    function HasSelectedUnlockedFigures: Boolean;
      
    function PointOnFigure(const ATestPoint: TPoint;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
      
    function PointOnSelectedFigure(const ATestPoint: TPoint;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
      
    function SelectedFigureCount: Integer;
  end;

implementation

uses
{ GraphicsMagic Lib }
  gmConstants,
  gmPaintFuncs,
  gmVectorLayer;

{ TgmLayerFigureManager }

constructor TgmLayerFigureManager.Create(ALayerList: TgmLayerList);
begin
  inherited Create();

  FLayerList := ALayerList;

  FSelectedFigureLayerIndexArray := nil;
  FSelectedFigureInfoArray       := nil;
  FVectorLayerIndexes            := nil;
end;

destructor TgmLayerFigureManager.Destroy;
begin
  SetLength(FSelectedFigureLayerIndexArray, 0);
  SetLength(FSelectedFigureInfoArray, 0);
  SetLength(FVectorLayerIndexes, 0);

  FSelectedFigureLayerIndexArray := nil;
  FSelectedFigureInfoArray       := nil;
  FVectorLayerIndexes            := nil;

  inherited;
end;

procedure TgmLayerFigureManager.ClearSelectedFiguresInfo;
begin
  SetLength(FSelectedFigureLayerIndexArray, 0);
  SetLength(FSelectedFigureInfoArray, 0);

  FSelectedFigureLayerIndexArray := nil;
  FSelectedFigureInfoArray       := nil;
end;

procedure TgmLayerFigureManager.DeleteSelectedFigures;
var
  i            : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LVectorLayer : TgmVectorLayer;
begin
  if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     ( Length(FSelectedFigureInfoArray) > 0 ) then
  begin
    // Must deleting figures in inverse order,
    // an exception will occur, otherwise.
    for i := High(FSelectedFigureInfoArray) downto 0 do
    begin
      LLayerIndex  := FSelectedFigureInfoArray[i].LayerIndex;
      LFigureIndex := FSelectedFigureInfoArray[i].FigureIndex;
      LVectorLayer := TgmVectorLayer(FLayerList.Layers[LLayerIndex]);

      LVectorLayer.FigureList.Delete(LFigureIndex);
    end;
  end;
end;

procedure TgmLayerFigureManager.DeselectAllFigures;
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        LVectorLayer.FigureList.DeselectAllFigures();
      end;
    end;
  end;

  SetLength(FSelectedFigureInfoArray, 0);
  SetLength(FSelectedFigureLayerIndexArray, 0);
end;

procedure TgmLayerFigureManager.DeselectFiguresOnSelectedLayer;
var
  i            : Integer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  if Length(FSelectedFigureInfoArray) > 0 then
  begin
    if Assigned(FLayerList) and
       (FLayerList.Count > 0) and
       (FLayerList.SelectedLayer is TgmVectorLayer) then
    begin
      LVectorLayer := TgmVectorLayer(FLayerList.SelectedLayer);

      for i := 0 to (LVectorLayer.FigureList.Count - 1) do
      begin
        LFigureObj := TgmFigureObject(LVectorLayer.FigureList.Items[i]);

        if LFigureObj.IsSelected then
        begin
          LFigureObj.IsSelected := False;
        end;
      end;
    end;

    UpdateSelectedFiguresInfo();
  end;
end;

procedure TgmLayerFigureManager.DeselectFiguresOnVisibleLayers;
var
  i, j         : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  if Length(FSelectedFigureInfoArray) > 0 then
  begin
    if Assigned(FLayerList) and (FLayerList.Count > 0) then
    begin
      for i := 0 to FLayerList.MaxIndex do
      begin
        LLayer := FLayerList.Layers[i];

        if (LLayer is TgmVectorLayer) and (LLayer.IsLayerVisible) then
        begin
          LVectorLayer := TgmVectorLayer(LLayer);

          if LVectorLayer.FigureList.Count > 0 then
          begin
            for j := 0 to (LVectorLayer.FigureList.Count - 1) do
            begin
              with LVectorLayer do
              begin
                LFigureObj := TgmFigureObject(FigureList.Items[j]);

                if LFigureObj.IsSelected then
                begin
                  LFigureObj.IsSelected := False;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    Self.UpdateSelectedFiguresInfo();
  end;
end;

procedure TgmLayerFigureManager.DrawAllFigures;
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        LVectorLayer.DrawAllFiguresOnLayer();
      end;
    end;
  end;
end;

// draw all figures on vector layers that have selected figures on them
procedure TgmLayerFigureManager.DrawAllFiguresOnSelectedVectorLayers;
var
  i, LLayerIndex : Integer;
  LLayer         : TgmCustomLayer;
  LVectorLayer   : TgmVectorLayer;
begin
  if Length(FSelectedFigureLayerIndexArray) > 0 then
  begin
    if Assigned(FLayerList) and (FLayerList.Count > 0) then
    begin
      for i := 0 to High(FSelectedFigureLayerIndexArray) do
      begin
        LLayerIndex := FSelectedFigureLayerIndexArray[i];

        if FLayerList.IsValidIndex(LLayerIndex) then
        begin
          LLayer := FLayerList.Layers[LLayerIndex];

          if LLayer is TgmVectorLayer then
          begin
            LVectorLayer := TgmVectorLayer(LLayer);
            LVectorLayer.DrawAllFiguresOnLayer();
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.DrawAllFiguresOnSpecifiedVectorLayer(
  const ALayerIndex: Integer);
var
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    if FLayerList.IsValidIndex(ALayerIndex) then
    begin
      LLayer := FLayerList.Layers[ALayerIndex];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        LVectorLayer.DrawAllFiguresOnLayer();
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.DrawHandlesOfSelectedFigures(ABuffer: TBitmap32;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  i, j, k            : Integer;
  LLayer             : TgmCustomLayer;
  LVectorLayer       : TgmVectorLayer;
  LFigureObj         : TgmFigureObject;
  LLayerIndex        : Integer;
  LFigureIndex       : Integer;
  LSelectedCount     : Integer;
  LHandleColor       : TColor32;
  LCurveHandleColor1 : TColor32;
  LCurveHandleColor2 : TColor32;
  LStartPoint        : TPoint;
  LEndPoint          : TPoint;
  LHandlePoint       : TPoint;
  p1, p2, p3, p4     : TPoint;
begin
  LSelectedCount := Length(FSelectedFigureInfoArray);

  if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     (LSelectedCount > 0) then
  begin
    for i := 0 to High(FSelectedFigureInfoArray) do
    begin
      LLayerIndex  := FSelectedFigureInfoArray[i].LayerIndex;
      LFigureIndex := FSelectedFigureInfoArray[i].FigureIndex;

      if FLayerList.IsValidIndex(LLayerIndex) then
      begin
        LLayer := FLayerList.Layers[LLayerIndex];

        if LLayer is TgmVectorLayer then
        begin
          LVectorLayer := TgmVectorLayer(LLayer);
          LFigureObj   := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);

          if LFigureObj.IsLocked then
          begin
            LHandleColor       := clRed32;
            LCurveHandleColor1 := clRed32;
            LCurveHandleColor2 := clRed32;
          end
          else
          begin
            if LSelectedCount > 1 then
            begin
              LHandleColor       := clAqua32;
              LCurveHandleColor1 := clAqua32;
              LCurveHandleColor2 := clAqua32;
            end
            else
            begin
              LHandleColor       := clLime32;
              LCurveHandleColor1 := clYellow32;
              LCurveHandleColor2 := clBlue32;
            end;
          end;

          case LFigureObj.Flag of
            ffStraightLine:
              begin
                if Assigned(ACoordConvertFunc) then
                begin
                  LStartPoint := ACoordConvertFunc(LFigureObj.FStartPoint);
                  LEndPoint   := ACoordConvertFunc(LFigureObj.FEndPoint);
                end
                else
                begin
                  LStartPoint := LFigureObj.FStartPoint;
                  LEndPoint   := LFigureObj.FEndPoint;
                end;

                DrawHandleOnBitmap32(ABuffer, LStartPoint, Point(0, 0),
                                     HANDLE_RADIUS, LHandleColor);

                DrawHandleOnBitmap32(ABuffer, LEndPoint, Point(0, 0),
                                     HANDLE_RADIUS, LHandleColor);
              end;

            ffCurve:
              begin
                ABuffer.SetStipple([$FF000000, $FF000000, $FF000000,
                                    $00FFFFFF, $00FFFFFF, $00FFFFFF]);

                ABuffer.StippleStep := 1.0;

                if Assigned(ACoordConvertFunc) then
                begin
                  p1 := ACoordConvertFunc(LFigureObj.FStartPoint);
                  p2 := ACoordConvertFunc(LFigureObj.FCurvePoint1);
                  p3 := ACoordConvertFunc(LFigureObj.FCurvePoint2);
                  p4 := ACoordConvertFunc(LFigureObj.FEndPoint);
                end
                else
                begin
                  p1 := LFigureObj.FStartPoint;
                  p2 := LFigureObj.FCurvePoint1;
                  p3 := LFigureObj.FCurvePoint2;
                  p4 := LFigureObj.FEndPoint;
                end;

                // drawing lines that between control points
                ABuffer.LineFSP(p1.X, p1.Y, p2.X, p2.Y);
                ABuffer.LineFSP(p2.X, p2.Y, p3.X, p3.Y);
                ABuffer.LineFSP(p3.X, p3.Y, p4.X, p4.Y);

                DrawHandleOnBitmap32(ABuffer, p1, Point(0, 0), HANDLE_RADIUS, LHandleColor);
                DrawHandleOnBitmap32(ABuffer, p4, Point(0, 0), HANDLE_RADIUS, LHandleColor);

                if (LFigureObj.FCurvePoint1.X = LFigureObj.FCurvePoint2.X) and
                   (LFigureObj.FCurvePoint1.Y = LFigureObj.FCurvePoint2.Y) then
                begin
                  case LFigureObj.CurveControl of
                    ccpFirst:
                      begin
                        DrawHandleOnBitmap32(ABuffer, p2, Point(0, 0),
                                             HANDLE_RADIUS, LCurveHandleColor1);
                      end;

                    ccpSecond:
                      begin
                        DrawHandleOnBitmap32(ABuffer, p3, Point(0, 0), 
                                             HANDLE_RADIUS, LCurveHandleColor2);
                      end;
                  end;
                end
                else
                begin
                  DrawHandleOnBitmap32(ABuffer, p2, Point(0, 0),
                                       HANDLE_RADIUS, LCurveHandleColor1);

                  DrawHandleOnBitmap32(ABuffer, p3, Point(0, 0),
                                       HANDLE_RADIUS, LCurveHandleColor2);
                end;
              end;

            ffPolygon,
            ffRegularPolygon:
              begin
                if Length(LFigureObj.FPolygonPoints) > 0 then
                begin
                  for k := 0 to High(LFigureObj.FPolygonPoints) do
                  begin
                    if Assigned(ACoordConvertFunc) then
                    begin
                      LHandlePoint := ACoordConvertFunc(LFigureObj.FPolygonPoints[k]);
                    end
                    else
                    begin
                      LHandlePoint := LFigureObj.FPolygonPoints[k];
                    end;
                    
                    DrawHandleOnBitmap32(ABuffer, LHandlePoint, Point(0, 0),
                                         HANDLE_RADIUS, LHandleColor);
                  end;
                end;
              end;

            ffRectangle,
            ffRoundRectangle,
            ffEllipse:
              begin
                for j := 0 to 7 do
                begin
                  case j of
                    0:
                      begin
                        LHandlePoint := LFigureObj.FStartPoint;
                      end;

                    1:
                      begin
                        LHandlePoint := LFigureObj.FEndPoint;
                      end;

                    2:
                      begin
                        LHandlePoint := Point(LFigureObj.FStartPoint.X,
                                              LFigureObj.FEndPoint.Y);
                      end;

                    3:
                      begin
                        LHandlePoint := Point(LFigureObj.FEndPoint.X,
                                              LFigureObj.FStartPoint.Y);
                      end;

                    4:
                      begin
                        LHandlePoint.X := (LFigureObj.FStartPoint.X +
                                           LFigureObj.FEndPoint.X) div 2;

                        LHandlePoint.Y := LFigureObj.FStartPoint.Y;
                      end;

                    5:
                      begin
                        LHandlePoint.X := (LFigureObj.FStartPoint.X +
                                           LFigureObj.FEndPoint.X) div 2;

                        LHandlePoint.Y := LFigureObj.FEndPoint.Y;
                      end;

                    6:
                      begin
                        LHandlePoint.X := LFigureObj.FStartPoint.X;

                        LHandlePoint.Y := (LFigureObj.FStartPoint.Y +
                                           LFigureObj.FEndPoint.Y) div 2;
                      end;

                    7:
                      begin
                        LHandlePoint.X := LFigureObj.FEndPoint.X;

                        LHandlePoint.Y := (LFigureObj.FStartPoint.Y +
                                           LFigureObj.FEndPoint.Y) div 2;
                      end;
                  end;

                  if Assigned(ACoordConvertFunc) then
                  begin
                    LHandlePoint := ACoordConvertFunc(LHandlePoint);
                  end;

                  DrawHandleOnBitmap32(ABuffer, LHandlePoint, Point(0, 0),
                                       HANDLE_RADIUS, LHandleColor);
                end;
              end;

            ffSquare,
            ffRoundSquare,
            ffCircle:
              begin
                for j := 0 to 7 do
                begin
                  case j of
                    0:
                      begin
                        LHandlePoint := LFigureObj.FStartPoint;
                      end;

                    1:
                      begin
                        LHandlePoint := LFigureObj.FEndPoint;
                      end;

                    2:
                      begin
                        LHandlePoint := Point(LFigureObj.FStartPoint.X,
                                              LFigureObj.FEndPoint.Y);
                      end;

                    3:
                      begin
                        LHandlePoint := Point(LFigureObj.FEndPoint.X,
                                              LFigureObj.FStartPoint.Y);
                      end;
                  end;

                   if Assigned(ACoordConvertFunc) then
                   begin
                     LHandlePoint := ACoordConvertFunc(LHandlePoint);
                   end;

                  DrawHandleOnBitmap32(ABuffer, LHandlePoint, Point(0, 0),
                                       HANDLE_RADIUS, LHandleColor);
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.DrawSelectedFiguresOnCanvas(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  i            : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  if Length(FSelectedFigureInfoArray) > 0 then
  begin
    if Assigned(FLayerList) and (FLayerList.Count > 0) then
    begin
      for i := 0 to High(FSelectedFigureInfoArray) do
      begin
        LLayerIndex  := FSelectedFigureInfoArray[i].LayerIndex;
        LFigureIndex := FSelectedFigureInfoArray[i].FigureIndex;

        if FLayerList.IsValidIndex(LLayerIndex) then
        begin
          LLayer := FLayerList.Layers[LLayerIndex];

          if LLayer is TgmVectorLayer then
          begin
            LVectorLayer := TgmVectorLayer(LLayer);
            LFigureObj   := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);

            if not LFigureObj.IsLocked then
            begin
              LFigureObj.DrawFigure(ACanvas, APenMode, ACoordConvertFunc);
            end;
          end;
        end;
      end;
    end;
  end;
end;

// draw all the unselected and locked figues on the vector layers that
// have selected figures
procedure TgmLayerFigureManager.DrawUnselectedAndLockedFiguresOnSelectedVectorLayers;
var
  i, LLayerIndex : Integer;
  LLayer         : TgmCustomLayer;
  LVectorLayer   : TgmVectorLayer;
begin
  if Length(FSelectedFigureLayerIndexArray) > 0 then
  begin
    if Assigned(FLayerList) and (FLayerList.Count > 0) then
    begin
      for i := 0 to High(FSelectedFigureLayerIndexArray) do
      begin
        LLayerIndex := FSelectedFigureLayerIndexArray[i];

        if FLayerList.IsValidIndex(LLayerIndex) then
        begin
          LLayer := FLayerList.Layers[LLayerIndex];

          if LLayer is TgmVectorLayer then
          begin
            LVectorLayer := TgmVectorLayer(LLayer);
            LVectorLayer.DrawUnselectedAndLockedLayersOnLayer();
          end;
        end;
      end;
    end;
  end;
end;

// get a control handle of a selected figure at point
function TgmLayerFigureManager.GetHandleOfSelectedFigures(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmDrawingHandle;
var
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  Result := dhNone;
  
  if Length(FSelectedFigureInfoArray) > 0 then
  begin
    if Assigned(FLayerList) and (FLayerList.Count > 0) then
    begin
      LLayerIndex  := FSelectedFigureInfoArray[0].LayerIndex;
      LFigureIndex := FSelectedFigureInfoArray[0].FigureIndex;

      if FLayerList.IsValidIndex(LLayerIndex) then
      begin
        LLayer := FLayerList.Layers[LLayerIndex];

        if LLayer is TgmVectorLayer then
        begin
          LVectorLayer := TgmVectorLayer(LLayer);
          LFigureObj   := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);

          Result := LFigureObj.GetHandleAtPoint(AX, AY, FIGURE_HANDLE_RADIUS,
                                                ACoordConvertFunc);
        end;
      end;
    end;
  end;
end;

// If there were only one selected figure, return it,
// return nil, otherwise.
function TgmLayerFigureManager.GetOnlyOneSelectedFigure: TgmFigureObject;
var
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  Result := nil;

  if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     ( Length(FSelectedFigureInfoArray) = 1 ) then
  begin
    LLayerIndex  := FSelectedFigureInfoArray[0].LayerIndex;
    LFigureIndex := FSelectedFigureInfoArray[0].FigureIndex;

    if FLayerList.IsValidIndex(LLayerIndex) then
    begin
      LLayer := FLayerList.Layers[LLayerIndex];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        Result       := LVectorLayer.FigureList.Items[LFigureIndex];
      end;
    end;
  end;
end;

function TgmLayerFigureManager.GetOnlyOneSelectedFigureIndex: Integer;
begin
  Result := -1;

  if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     ( Length(FSelectedFigureInfoArray) = 1 ) then
  begin
    Result := FSelectedFigureInfoArray[0].FigureIndex;
  end;
end;

function TgmLayerFigureManager.GetOnlyOneSelectedFigureLayerIndex: Integer;
begin
  Result := -1;

  if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     ( Length(FSelectedFigureInfoArray) = 1 ) then
  begin
    Result := FSelectedFigureInfoArray[0].LayerIndex;
  end;
end;

// if there are any figures on any of vector layers
function TgmLayerFigureManager.HasFiguresOnVectorLayers: Boolean;
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  Result := False;

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        
        if LVectorLayer.FigureList.Count > 0 then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

// determine whether there are any selected figures that are clocked
function TgmLayerFigureManager.HasSelectedLockedFigures: Boolean;
var
  i            : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  Result := False;

   if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     ( Length(FSelectedFigureInfoArray) > 0 ) then
  begin
    for i := 0 to High(FSelectedFigureInfoArray) do
    begin
      LLayerIndex  := FSelectedFigureInfoArray[i].LayerIndex;
      LFigureIndex := FSelectedFigureInfoArray[i].FigureIndex;
      LVectorLayer := TgmVectorLayer(FLayerList.Layers[LLayerIndex]);
      LFigureObj   := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);
      
      if LFigureObj.IsLocked then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

// determine whether there are any selected figures that are unclocked 
function TgmLayerFigureManager.HasSelectedUnlockedFigures: Boolean;
var
  i            : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  Result := False;

  if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     ( Length(FSelectedFigureInfoArray) > 0 ) then
  begin
    for i := 0 to High(FSelectedFigureInfoArray) do
    begin
      LLayerIndex  := FSelectedFigureInfoArray[i].LayerIndex;
      LFigureIndex := FSelectedFigureInfoArray[i].FigureIndex;
      LVectorLayer := TgmVectorLayer(FLayerList.Layers[LLayerIndex]);
      LFigureObj   := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);
      
      if not LFigureObj.IsLocked then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.LockSelectedFigures;
var
  i            : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     ( Length(FSelectedFigureInfoArray) > 0 ) then
  begin
    for i := 0 to High(FSelectedFigureInfoArray) do
    begin
      LLayerIndex         := FSelectedFigureInfoArray[i].LayerIndex;
      LFigureIndex        := FSelectedFigureInfoArray[i].FigureIndex;
      LVectorLayer        := TgmVectorLayer(FLayerList.Layers[LLayerIndex]);
      LFigureObj          := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);
      LFigureObj.IsLocked := True;
    end;
  end;
end;

// whether or not the point is on any of figures on vector layers
function TgmLayerFigureManager.PointOnFigure(const ATestPoint: TPoint;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  Result := False;

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := (FLayerList.Count - 1) downto 0 do
    begin
      LLayer := FLayerList.Layers[i];
       
      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);

        with LVectorLayer do
        begin
          if FigureList.IfPointOnFigure(ATestPoint.X, ATestPoint.Y, ACoordConvertFunc) then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

// checking for whether or not the point is on any of selected figures
// on vector layers
function TgmLayerFigureManager.PointOnSelectedFigure(const ATestPoint: TPoint;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  Result := False;
  
  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := (FLayerList.Count - 1) downto 0 do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);

        with LVectorLayer do
        begin
          if FigureList.SelectedContainsPoint(ATestPoint, ACoordConvertFunc) then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.SelectAllFiguresOnVectorLayers;
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        LVectorLayer.FigureList.SelectAllFigures();
      end;
    end;
    
    Self.UpdateSelectedFiguresInfo();
  end;
end;

procedure TgmLayerFigureManager.SelectFigures(const AShift: TShiftState;
  const AX, AY: Integer);
var
  i, j, LIndex : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  SetLength(FSelectedFigureInfoArray, 0);
  SetLength(FSelectedFigureLayerIndexArray, 0);

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := (FLayerList.Count - 1) downto 0 do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        LVectorLayer.FigureList.SelectFigures(AShift, AX, AY);

        for j := 0 to (LVectorLayer.FigureList.Count - 1) do
        begin
          LFigureObj := TgmFigureObject(LVectorLayer.FigureList.Items[j]);

          if LFigureObj.IsSelected then
          begin
            SetLength( FSelectedFigureInfoArray, Length(FSelectedFigureInfoArray) + 1 );

            LIndex := High(FSelectedFigureInfoArray);
            FSelectedFigureInfoArray[LIndex].LayerIndex  := i;
            FSelectedFigureInfoArray[LIndex].FigureIndex := j;
          end;
        end;

        if LVectorLayer.FigureList.SelectedFigureCount > 0 then
        begin
          SetLength( FSelectedFigureLayerIndexArray,
                     Length(FSelectedFigureLayerIndexArray) + 1 );

          FSelectedFigureLayerIndexArray[ High(FSelectedFigureLayerIndexArray) ] := i;
        end;

        if not (ssShift in AShift) then
        begin
          if Length(FSelectedFigureInfoArray) > 0 then
          begin
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.SelectFiguresByRectOnVectorLayers(
  const AIncludeMode: TgmFigureSelectIncludeMode;
  const ARegionStart, ARegionEnd: TPoint);
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    DeselectAllFigures;

    for i := (FLayerList.Count - 1) downto 0 do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        LVectorLayer.FigureList.SelectRect(AIncludeMode, ARegionStart, ARegionEnd);
      end;
    end;
  end;
  
  UpdateSelectedFiguresInfo();
end;

function TgmLayerFigureManager.SelectedFigureCount: Integer;
var
  i, LFigureCount : Integer;
  LLayer          : TgmCustomLayer;
  LVectorLayer    : TgmVectorLayer;
begin
  LFigureCount := 0;

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := (FLayerList.Count - 1) downto 0 do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        LFigureCount := LFigureCount + LVectorLayer.FigureList.SelectedFigureCount;
      end;
    end;
  end;
  
  Result := LFigureCount;
end;

procedure TgmLayerFigureManager.TranslateSelectedFigures(
  const ATranslateVector: TPoint);
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := (FLayerList.Count - 1) downto 0 do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);
        
        if LVectorLayer.FigureList.Count > 0 then
        begin
          LVectorLayer.FigureList.TranslateSelectedFigures(ATranslateVector);
        end;
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.UnlockSelectedFigures;
var
  i            : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  if Assigned(FLayerList) and
     (FLayerList.Count > 0) and
     ( Length(FSelectedFigureInfoArray) > 0 )  then
  begin
    for i := 0 to High(FSelectedFigureInfoArray) do
    begin
      LLayerIndex         := FSelectedFigureInfoArray[i].LayerIndex;
      LFigureIndex        := FSelectedFigureInfoArray[i].FigureIndex;
      LVectorLayer        := TgmVectorLayer(FLayerList.Layers[LLayerIndex]);
      LFigureObj          := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);
      LFigureObj.IsLocked := False;
    end;
  end;
end;

procedure TgmLayerFigureManager.UpdateSelectedFiguresInfo;
var
  i, j           : Integer;
  LSelectedCount : Integer;
  LLayer         : TgmCustomLayer;
  LVectorLayer   : TgmVectorLayer;
  LFigureObj     : TgmFigureObject;
begin
  SetLength(FSelectedFigureInfoArray, 0);
  SetLength(FSelectedFigureLayerIndexArray, 0);

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        LVectorLayer := TgmVectorLayer(LLayer);

        if LVectorLayer.FigureList.Count > 0 then
        begin
          LSelectedCount := 0;

          for j := 0 to (LVectorLayer.FigureList.Count - 1) do
          begin
            LFigureObj := TgmFigureObject(LVectorLayer.FigureList.Items[j]);
            
            if LFigureObj.IsSelected then
            begin
              Inc(LSelectedCount);
              SetLength( FSelectedFigureInfoArray, Length(FSelectedFigureInfoArray) + 1 );
              FSelectedFigureInfoArray[High(FSelectedFigureInfoArray)].LayerIndex  := i;
              FSelectedFigureInfoArray[High(FSelectedFigureInfoArray)].FigureIndex := j;
            end;
          end;

          if LSelectedCount > 0 then
          begin
            SetLength( FSelectedFigureLayerIndexArray,
                       Length(FSelectedFigureLayerIndexArray) + 1 );

            FSelectedFigureLayerIndexArray[High(FSelectedFigureLayerIndexArray)] := i;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.UpdateThumbnailOnSelectedVectorLayers;
var
  i, j                 : Integer;
  LLayerIndex          : Integer;
  LLayer               : TgmCustomLayer;
  LUpdatedLayerIndexes : array of Integer;
  LRepetition          : Boolean;
begin
  if Length(FSelectedFigureInfoArray) > 0 then
  begin
    if Assigned(FLayerList) and (FLayerList.Count > 0) then
    begin
      SetLength(LUpdatedLayerIndexes, 1);
      LUpdatedLayerIndexes[0] := -1;  // dummy data for coding easily
      
      for i := 0 to High(FSelectedFigureInfoArray) do
      begin
        LLayerIndex := FSelectedFigureInfoArray[i].LayerIndex;

        if FLayerList.IsValidIndex(LLayerIndex) then
        begin
          LLayer := FLayerList.Layers[LLayerIndex];

          if LLayer is TgmVectorLayer then
          begin
            // checking for repetition
            LRepetition := False;
            for j := 0 to High(LUpdatedLayerIndexes) do
            begin
              if LLayerIndex = LUpdatedLayerIndexes[j] then
              begin
                LRepetition := True;
                Break;
              end;
            end;

            if not LRepetition then
            begin
              LLayer.UpdateLayerThumbnail();

              SetLength( LUpdatedLayerIndexes, Length(LUpdatedLayerIndexes) + 1 );
              LUpdatedLayerIndexes[High(LUpdatedLayerIndexes)] := LLayerIndex;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmLayerFigureManager.UpdateVectorLayerIndexRecord;
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  SetLength(FVectorLayerIndexes, 0);
  FVectorLayerIndexes := nil;

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmVectorLayer then
      begin
        SetLength( FVectorLayerIndexes, Length(FVectorLayerIndexes) + 1 );
        FVectorLayerIndexes[High(FVectorLayerIndexes)] := i;
      end;
    end;
  end;
end;


end.
