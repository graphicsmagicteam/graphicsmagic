unit gmShapeRegionLayerCommands;

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

// Update Date: 2016/11/18

interface

uses
{ Delphi }
  Windows, SysUtils, Graphics,
{ GraphicsMagicLib }
  gmChannelManager,
  gmHistoryCommands,
  gmLayers,
  gmShapes,
  gmShapeRegionLayer;

type
  { TgmCustomShapeRegionCommand }

  TgmCustomShapeRegionCommand = class(TgmCustomCommand)
  protected
    function GetCommandName(AOutline: TgmShapeOutline): string;
  end;

  { TgmNewShapeOnNewShapeRegionLayerCommand }

  TgmNewShapeOnNewShapeRegionLayerCommand = class(TgmCustomShapeRegionCommand)
  private
    FChannelManager   : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList        : TgmLayerList;             // pointer to an external layer list
    FLayerIndex       : Integer;
    FLayerName        : string;
    FShapeRegionLayer : TgmShapeRegionLayer;      // a copy to the newly created shape-region layer
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; AShapeRegionLayer: TgmShapeRegionLayer;
      const AShapeRegionLayerIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmNewShapeOnExistedShapeRegionLayerCommand }

  TgmNewShapeOnExistedShapeRegionLayerCommand = class(TgmCustomShapeRegionCommand)
  private
    FLayerList    : TgmLayerList;             // pointer to an external layer list
    FLayerIndex   : Integer;
    FOutlineIndex : Integer;
    FOutline      : TgmShapeOutline;
  public
    constructor Create(ALayerList: TgmLayerList;
      const AShapeRegionLayerIndex: Integer; AOutline: TgmShapeOutline;
      const AOutlineIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmModifyShapeRegionFillingColorCommand }

  TgmModifyShapeRegionFillingColorCommand = class(TgmCustomCommand)
  private
    FLayerList  : TgmLayerList;  // pointer to an external layer list
    FLayerIndex : Integer;
    FOldColor   : TColor;
    FNewColor   : TColor;
  public
    constructor Create(ALayerList: TgmLayerList;
      const AShapeRegionLayerIndex: Integer;
      const AOldColor, ANewColor: TColor);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmModifyShapeRegionFillingStyleCommand }

  TgmModifyShapeRegionFillingStyleCommand = class(TgmCustomCommand)
  private
    FLayerList  : TgmLayerList;  // pointer to an external layer list
    FLayerIndex : Integer;
    FOldStyle   : TBrushStyle;
    FNewStyle   : TBrushStyle;
  public
    constructor Create(ALayerList: TgmLayerList;
      const AShapeRegionLayerIndex: Integer;
      const AOldStyle, ANewStyle: TBrushStyle);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmTranslateShapeRegionCommand }

  TgmTranslateShapeRegionCommand = class(TgmCustomCommand)
  private
    FLayerList        : TgmLayerList;  // pointer to an external layer list
    FLayerIndex       : Integer;
    FTranslatedVector : TPoint;
  public
    constructor Create(ALayerList: TgmLayerList;
      const AShapeRegionLayerIndex: Integer; const ATranslatedVector: TPoint);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmScaleShapeRegionCommand }

  TgmScaleShapeRegionCommand = class(TgmCustomCommand)
  private
    FLayerList      : TgmLayerList;  // pointer to an external layer list
    FLayerIndex     : Integer;
    FOldTopLeft     : TPoint;
    FOldBottomRight : TPoint;
    FNewTopLeft     : TPoint;
    FNewBottomRight : TPoint;
  public
    constructor Create(ALayerList: TgmLayerList;
      const AShapeRegionLayerIndex: Integer;
      const AOldTopLeft, AOldBottomRight: TPoint;
      const ANewTopLeft, ANewBottomRight: TPoint);

    procedure Execute; override;
    procedure Rollback; override;
  end;


implementation


{ TgmCustomShapeRegionCommand }

function TgmCustomShapeRegionCommand.GetCommandName(
  AOutline: TgmShapeOutline): string;
begin
  Result := '';
  
  if Assigned(AOutline) then
  begin
    if AOutline is TgmEllipseOutline then
    begin
      Result := 'Ellipse Tool';
    end
    else if AOutline is TgmLineOutline then
    begin
      Result := 'Line Tool';
    end
    else if AOutline is TgmRectangleOutline then
    begin
      Result := 'Rectangle Tool';
    end
    else if AOutline is TgmRegularPolygonOutline then
    begin
      Result := 'Regular Polygon Tool';
    end
    else if AOutline is TgmRoundRectOutline then
    begin
      Result := 'Rounded-Corner Rectangle Tool';
    end;
  end;
end;

{ TgmModifyShapeRegionFillingColorCommand }

constructor TgmModifyShapeRegionFillingColorCommand.Create(ALayerList: TgmLayerList;
  const AShapeRegionLayerIndex: Integer; const AOldColor, ANewColor: TColor);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingColorCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not ALayerList.IsValidIndex(AShapeRegionLayerIndex) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingColorCommand.Create(): parameter AShapeRegionLayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[AShapeRegionLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingColorCommand.Create(): the layer at AShapeRegionLayerIndex in the list is not a shape-region layer.');
  end;

  inherited Create('Modify Shape Region Filling Color');

  FLayerList  := ALayerList;
  FLayerIndex := AShapeRegionLayerIndex;
  FOldColor   := AOldColor;
  FNewColor   := ANewColor;
end;

procedure TgmModifyShapeRegionFillingColorCommand.Execute;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingColorCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingColorCommand.Execute(): the layer at FLayerIndex in the list is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  with LShapeRegionLayer do
  begin
    // Setting RegionColor property of a shape-region layer will get the layer
    // to draw the region on itself automatically.
    RegionColor := FNewColor;
    UpdateLogoThumbnail();
  end;
end;

procedure TgmModifyShapeRegionFillingColorCommand.Rollback;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingColorCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingColorCommand.Rollback(): the layer at FLayerIndex in the list is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  with LShapeRegionLayer do
  begin
    // Setting RegionColor property of a shape-region layer will get the layer
    // to draw the region on itself automatically.
    RegionColor := FOldColor;
    UpdateLogoThumbnail();
  end;
end;

{ TgmModifyShapeRegionFillingStyleCommand }

constructor TgmModifyShapeRegionFillingStyleCommand.Create(ALayerList: TgmLayerList;
  const AShapeRegionLayerIndex: Integer;
  const AOldStyle, ANewStyle: TBrushStyle);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingStyleCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not ALayerList.IsValidIndex(AShapeRegionLayerIndex) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingStyleCommand.Create(): parameter AShapeRegionLayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[AShapeRegionLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingStyleCommand.Create(): the layer at AShapeRegionLayerIndex in the list is not a shape-region layer.');
  end;

  inherited Create('Modify Shape Region Filling Style');

  FLayerList  := ALayerList;
  FLayerIndex := AShapeRegionLayerIndex;
  FOldStyle   := AOldStyle;
  FNewStyle   := ANewStyle;
end;

procedure TgmModifyShapeRegionFillingStyleCommand.Execute;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingStyleCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingStyleCommand.Execute(): the layer at FLayerIndex in the list is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  with LShapeRegionLayer do
  begin
    // Setting BrushStyle property of a shape-region layer will get the layer
    // to draw the region on itself automatically.
    BrushStyle := FNewStyle;
    UpdateLayerThumbnail();
  end;
end;

procedure TgmModifyShapeRegionFillingStyleCommand.Rollback;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingStyleCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmModifyShapeRegionFillingStyleCommand.Rollback(): the layer at FLayerIndex in the list is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  with LShapeRegionLayer do
  begin
    // Setting BrushStyle property of a shape-region layer will get the layer
    // to draw the region on itself automatically.
    BrushStyle := FOldStyle;
    UpdateLayerThumbnail();
  end;
end;

{ TgmNewShapeOnExistedShapeRegionLayerCommand }

constructor TgmNewShapeOnExistedShapeRegionLayerCommand.Create(
  ALayerList: TgmLayerList; const AShapeRegionLayerIndex: Integer;
  AOutline: TgmShapeOutline; const AOutlineIndex: Integer);
var
  LCommandName      : string;
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not ALayerList.IsValidIndex(AShapeRegionLayerIndex) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Create(): parameter AShapeRegionLayerIndex is out of the range.');
  end;

  LLayer := ALayerList.Layers[AShapeRegionLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Create(): the layer at AShapeRegionLayerIndex is not a shape-region layer.');
  end;

  if not Assigned(AOutline) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Create(): parameter AOutline is a nil pointer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  if AOutlineIndex <> (LShapeRegionLayer.ShapeOutlineList.Count - 1) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Create(): AOutlineIndex should be at last of the outline list.');
  end;

  LCommandName := GetCommandName(AOutline);
  inherited Create(LCommandName);
  
  FLayerList    := ALayerList;
  FLayerIndex   := AShapeRegionLayerIndex;
  FOutline      := gmShapes.CopyShapeOutline(AOutline);
  FOutlineIndex := AOutlineIndex;
end;

destructor TgmNewShapeOnExistedShapeRegionLayerCommand.Destroy;
begin
  FOutline.Free();
  inherited;
end;

procedure TgmNewShapeOnExistedShapeRegionLayerCommand.Execute;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOutline          : TgmShapeOutline;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Execute(): FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Execute(): the layer at FLayerIndex is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  if FOutlineIndex <> LShapeRegionLayer.ShapeOutlineList.Count then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Execute(): FOutlineIndex is mismatched.');
  end;

  LOutline := gmShapes.CopyShapeOutline(FOutline);

  with LShapeRegionLayer do
  begin
    ShapeOutlineList.Add(LOutline);
    ShapeOutlineList.GetShapesBoundary();
    ShapeRegion.AccumRGN := ShapeOutlineList.GetScaledShapesRegion();
    DrawRegionOnLayer();
    UpdateLayerThumbnail();
  end;
end;

procedure TgmNewShapeOnExistedShapeRegionLayerCommand.Rollback;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Rollback(): FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Rollback(): the layer at FLayerIndex is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  if FOutlineIndex <> (LShapeRegionLayer.ShapeOutlineList.Count - 1) then
  begin
    raise Exception.Create('TgmNewShapeOnExistedShapeRegionLayerCommand.Rollback(): FOutlineIndex is mismatched.');
  end;

  with LShapeRegionLayer do
  begin
    ShapeOutlineList.DeleteOutlineByIndex(FOutlineIndex);
    ShapeOutlineList.GetShapesBoundary();
    ShapeRegion.AccumRGN := ShapeOutlineList.GetScaledShapesRegion();
    DrawRegionOnLayer();
    UpdateLayerThumbnail();
  end;
end;

{ TgmNewShapeOnNewShapeRegionLayerCommand }

constructor TgmNewShapeOnNewShapeRegionLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  AShapeRegionLayer: TgmShapeRegionLayer;
  const AShapeRegionLayerIndex: Integer);
var
  LCommandName : string;
  LOutline     : TgmShapeOutline;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewShapeOnNewShapeRegionLayerCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmNewShapeOnNewShapeRegionLayerCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not Assigned(AShapeRegionLayer) then
  begin
    raise Exception.Create('TgmNewShapeOnNewShapeRegionLayerCommand.Create(): parameter AShapeRegionLayer is a nil pointer.');
  end;

  if not ALayerList.IsValidIndex(AShapeRegionLayerIndex) then
  begin
    raise Exception.Create('TgmNewShapeOnNewShapeRegionLayerCommand.Create(): parameter AShapeRegionLayerIndex is out of range.');
  end;

  if AShapeRegionLayer.ShapeOutlineList.Count <= 0 then
  begin
    raise Exception.Create('TgmNewShapeOnNewShapeRegionLayerCommand.Create(): there is no any shape on the shape-region layer.');
  end;

  LOutline     := AShapeRegionLayer.ShapeOutlineList.Items[0];
  LCommandName := GetCommandName(LOutline);
  inherited Create(LCommandName);

  FChannelManager   := AChannelManager;
  FLayerList        := ALayerList;
  FLayerIndex       := AShapeRegionLayerIndex;
  FLayerName        := AShapeRegionLayer.LayerName;
  FShapeRegionLayer := TgmShapeRegionLayer( AShapeRegionLayer.GetCopy() );
end;

destructor TgmNewShapeOnNewShapeRegionLayerCommand.Destroy;
begin
  FShapeRegionLayer.Free();
  inherited;
end;

procedure TgmNewShapeOnNewShapeRegionLayerCommand.Execute;
var
  LDuplicatedLayer : TgmShapeRegionLayer;
  LLayerCounter    : Integer;
begin
  inherited;

  LDuplicatedLayer           := TgmShapeRegionLayer( FShapeRegionLayer.GetCopy() );
  LDuplicatedLayer.LayerName := Self.FLayerName;

  LDuplicatedLayer.UpdateLayerThumbnail();
  LDuplicatedLayer.UpdateLogoThumbnail();

  LLayerCounter := FLayerList.GetLayerCounter(TgmShapeRegionLayer);

  FLayerList.SimplyInsert(FLayerIndex, LDuplicatedLayer);
  FLayerList.SimplySelectLayer(FLayerIndex);
  FLayerList.SetLayerCounter(TgmShapeRegionLayer, LLayerCounter + 1);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmNewShapeOnNewShapeRegionLayerCommand.Rollback;
var
  LLayer        : TgmCustomLayer;
  LSelectIndex  : Integer;
  LLayerCounter : Integer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmNewShapeOnNewShapeRegionLayerCommand.Rollback(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmNewShapeOnNewShapeRegionLayerCommand.Rollback(): the layer at FLayerIndex is not a shape-region layer.');
  end;

  LLayerCounter := FLayerList.GetLayerCounter(TgmShapeRegionLayer);

  FLayerList.SimplyDeleteLayer(FLayerIndex);
  FLayerList.SetLayerCounter(TgmShapeRegionLayer, LLayerCounter - 1);

  LSelectIndex := FLayerIndex - 1;
  if LSelectIndex < 0 then
  begin
    LSelectIndex := 0;
  end;
  
  FLayerList.SimplySelectLayer(LSelectIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmScaleShapeRegionCommand }

constructor TgmScaleShapeRegionCommand.Create(ALayerList: TgmLayerList;
  const AShapeRegionLayerIndex: Integer;
  const AOldTopLeft, AOldBottomRight: TPoint;
  const ANewTopLeft, ANewBottomRight: TPoint);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmScaleShapeRegionCommand.Create(): parameter ALayerList is a nil');
  end;

  if not ALayerList.IsValidIndex(AShapeRegionLayerIndex) then
  begin
    raise Exception.Create('TgmScaleShapeRegionCommand.Create(): parameter AShapeRegionLayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[AShapeRegionLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmScaleShapeRegionCommand.Create(): the layer at AShapeRegionLayerIndex is not a shape-region layer.');
  end;

  inherited Create('Scale Shape Region');
  
  FLayerList      := ALayerList;
  FLayerIndex     := AShapeRegionLayerIndex;
  FOldTopLeft     := AOldTopLeft;
  FOldBottomRight := AOldBottomRight;
  FNewTopLeft     := ANewTopLeft;
  FNewBottomRight := ANewBottomRight;
end;

procedure TgmScaleShapeRegionCommand.Execute;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmScaleShapeRegionComman.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmScaleShapeRegionComman.Execute(): the layer at FLayerIndex in the list is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  with LShapeRegionLayer do
  begin
    ShapeOutlineList.BackupCoordinates();
    ShapeOutlineList.FBoundaryTL := FNewTopLeft;
    ShapeOutlineList.FBoundaryBR := FNewBottomRight;
    ShapeOutlineList.ScaleShapesCoordinates();

    ShapeRegion.AccumRGN := ShapeOutlineList.GetScaledShapesRegion();
    DrawRegionOnLayer();
    UpdateLayerThumbnail();
  end;
end;

procedure TgmScaleShapeRegionCommand.Rollback;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmScaleShapeRegionComman.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmScaleShapeRegionComman.Rollback(): the layer at FLayerIndex in the list is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  with LShapeRegionLayer do
  begin
    ShapeOutlineList.FBoundaryTL := FOldTopLeft;
    ShapeOutlineList.FBoundaryBR := FOldBottomRight;
    ShapeOutlineList.ScaleShapesCoordinates();

    ShapeRegion.AccumRGN := ShapeOutlineList.GetScaledShapesRegion();
    
    DrawRegionOnLayer();
    UpdateLayerThumbnail();
  end;
end;

{ TgmTranslateShapeRegionCommand }

constructor TgmTranslateShapeRegionCommand.Create(ALayerList: TgmLayerList;
  const AShapeRegionLayerIndex: Integer; const ATranslatedVector: TPoint);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmTranslateShapeRegionCommand.Create(): parameter ALayerList is a nil');
  end;

  if not ALayerList.IsValidIndex(AShapeRegionLayerIndex) then
  begin
    raise Exception.Create('TgmTranslateShapeRegionCommand.Create(): parameter AShapeRegionLayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[AShapeRegionLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmTranslateShapeRegionCommand.Create(): the layer at AShapeRegionLayerIndex is not a shape-region layer.');
  end;

  inherited Create('Move Shape Region');
  
  FLayerList        := ALayerList;
  FLayerIndex       := AShapeRegionLayerIndex;
  FTranslatedVector := ATranslatedVector;

  ChangeCommandIconByResourceName(gmHistoryCommands.MOVE_COMMAND_ICON_RES_NAME);
end;

procedure TgmTranslateShapeRegionCommand.Execute;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmTranslateShapeRegionCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmTranslateShapeRegionCommand.Execute(): the layer at FLayerIndex in the list is not a shape-region layer.');
  end;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  with LShapeRegionLayer do
  begin
    ShapeOutlineList.Translate(FTranslatedVector);
    ShapeRegion.Translate(FTranslatedVector);
    DrawRegionOnLayer();
    UpdateLayerThumbnail();
  end;
end;

procedure TgmTranslateShapeRegionCommand.Rollback;
var
  LLayer            : TgmCustomLayer;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LTranslatedVector : TPoint;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmTranslateShapeRegionCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmShapeRegionLayer) then
  begin
    raise Exception.Create('TgmTranslateShapeRegionCommand.Rollback(): the layer at FLayerIndex in the list is not a shape-region layer.');
  end;

  LTranslatedVector.X := -FTranslatedVector.X;
  LTranslatedVector.Y := -FTranslatedVector.Y;

  LShapeRegionLayer := TgmShapeRegionLayer(LLayer);
  with LShapeRegionLayer do
  begin
    ShapeOutlineList.Translate(LTranslatedVector);
    ShapeRegion.Translate(LTranslatedVector);
    DrawRegionOnLayer();
    UpdateLayerThumbnail();
  end;
end;

end.
