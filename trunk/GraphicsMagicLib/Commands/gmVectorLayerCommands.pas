unit gmVectorLayerCommands;

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

// Update Date: 2016/11/23

{$WARN UNSAFE_CAST OFF}

interface

uses
{ Delphi }
  Windows, SysUtils,
{ Graphics32 }
  GR32,           // TArrayOfInteger
{ GraphicsMagicLib }
  gmChannelManager,
  gmFigures,
  gmLayers,
  gmLayerFigureManager,
  gmHistoryCommands,
  gmVectorLayer;

type
  TgmFigureLockMode = (flmLock, flmUnlock);

  TgmFigureRec = record
    LayerIndex  : Integer;
    FigureIndex : Integer;
    FigureObj   : TgmFigureObject;
  end;

  TgmFigureRecArray        = array of TgmFigureRec;
  TgmArrayOfFigureRecArray = array of TgmFigureRecArray;

  { TgmCustomVectorLayerCommand }

  TgmCustomVectorLayerCommand = class(TgmCustomCommand)
  protected
    procedure SetCommandIcon(const AFigureFlag: TgmFigureFlags);
  end;

  { TgmNewFigureOnNewVectorLayerCommand }

  TgmNewFigureOnNewVectorLayerCommand = class(TgmCustomVectorLayerCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FLayerIndex     : Integer;
    FLayerName      : string;
    FVectorLayer    : TgmVectorLayer;           // a copy to the newly created vector layer
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; AVectorLayer: TgmVectorLayer;
      const AVectorLayerIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmNewFigureOnExistedVectorLayerCommand }

  TgmNewFigureOnExistedVectorLayerCommand = class(TgmCustomVectorLayerCommand)
  private
    FChannelManager   : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList        : TgmLayerList;             // pointer to an external layer list
    FVectorLayerIndex : Integer;
    FAddedFigure      : TgmFigureObject;
    FFigureIndex      : Integer;
    FFigureFlag       : TgmFigureFlags;

    procedure ChangeFigureCounter(const AFigureFlag: TgmFigureFlags;
      const AChangedAmount: Integer);
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const AVectorLayerIndex, ANewFigureIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmModifyFigurePropertiesOnVectorLayerCommand }

  TgmModifyFigurePropertiesOnVectorLayerCommand = class(TgmCustomVectorLayerCommand) 
  private
    FLayerList        : TgmLayerList;  // pointer to an external layer list
    FVectorLayerIndex : Integer;
    FFigureIndex      : Integer;
    FFigureFlag       : TgmFigureFlags;
    FOldProperties    : TgmFigureBasicData;
    FNewProperties    : TgmFigureBasicData;
  public
    constructor Create(ALayerList: TgmLayerList;
      const AVectorLayerIndex, AFigureIndex: Integer;
      const AOldProperties, ANewProperties: TgmFigureBasicData);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmLockUnlockFiguresOnVectorLayersCommand }

  TgmLockUnlockFiguresOnVectorLayersCommand = class(TgmCustomCommand)
  private
    FLayerList       : TgmLayerList;          // pointer to an external layer list
    FLockMode        : TgmFigureLockMode;
    FFigureInfoArray : TgmArrayOfFigureInfo;
  public
    constructor Create(ALayerList: TgmLayerList;
      AFigureInfoArray: TgmArrayOfFigureInfo;
      const ALockMode: TgmFigureLockMode);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmTranslateFiguresOnVectorLayersCommand }

  TgmTranslateFiguresOnVectorLayersCommand = class(TgmCustomCommand)
  private
    FLayerList        : TgmLayerList;          // pointer to an external layer list
    FTranslatedVector : TPoint;
    FFigureInfoArray  : TgmArrayOfFigureInfo;
  public
    constructor Create(ALayerList: TgmLayerList;
      AFigureInfoArray: TgmArrayOfFigureInfo;
      const ATranslatedVector: TPoint);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeleteFiguresOnVectorLayersCommand }

  TgmDeleteFiguresOnVectorLayersCommand = class(TgmCustomCommand)
  private
    FLayerList               : TgmLayerList;       // pointer to an external layer list
    FFigureRecArray          : TgmArrayOfFigureRecArray;
    FDeletedFigureIndexArray : TArrayOfArrayOfInteger;
  public
    constructor Create(ALayerListBeforeDeletion: TgmLayerList;
      ADeletedFigureInfoArray: TgmArrayOfFigureInfo);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmModifyFiguresOnVectorLayersCommand }

  TgmModifyFiguresOnVectorLayersCommand = class(TgmCustomCommand)
  private
    FLayerList          : TgmLayerList;       // pointer to an external layer list
    FOldFigureRecArray  : TgmArrayOfFigureRecArray;
    FNewFigureRecArray  : TgmArrayOfFigureRecArray;
    FOldFiguresRecorded : Boolean;
    FNewFiguresRecorded : Boolean;

    procedure ClearOldRecords;
    procedure ClearNewRecords;
  public
    constructor Create(ALayerList: TgmLayerList);
    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;

    procedure SetOldFiguresData(ALayerList: TgmLayerList);
    procedure SetNewFiguresData(ALayerList: TgmLayerList);
  end;

  { TgmSingleFigureAdjustmentCommand }

  TgmSingleFigureAdjustmentCommand = class(TgmCustomVectorLayerCommand)
  private
    FLayerList        : TgmLayerList;       // pointer to an external layer list
    FVectorLayerIndex : Integer;
    FFigureIndex      : Integer;
    FFigureFlag       : TgmFigureFlags;
    FOldFigure        : TgmFigureObject;
    FNewFigure        : TgmFigureObject;
  public
    constructor Create(ALayerList: TgmLayerList;
      const AVectorLayerIndex, AFigureIndex: Integer;
      const AOldFigure, ANewFigure: TgmFigureObject);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;


const
  STRAIGHT_LINE_ICON_RES_NAME   = 'STRAIGHTLINEICONMASK';
  BEZIER_CURVE_ICON_RES_NAME    = 'BEZIERCURVEICONMASK';
  POLYGON_ICON_RES_NAME         = 'POLYGONICONMASK';
  REGULAR_POLYGON_ICON_RES_NAME = 'REGULARPOLYGONICONMASK';
  RECTANGLE_ICON_RES_NAME       = 'RECTANGLEICONMASK';
  ROUND_RECT_ICON_RES_NAME      = 'ROUNDRECTICONMASK';
  ELLIPSE_ICON_RES_NAME         = 'ELLIPSEICONMASK';


implementation

uses
{ GraphicsMagicLib }
  gmMath;

{$R gmVectorLayerCommandIcons.res }


function GetUnrepeatedVectorLayerIndexes(
  AFigureInfoArray: TgmArrayOfFigureInfo): TArrayOfInteger;
var
  i, j         : Integer;
  LArrayLength : Integer;
  LAccumIndex  : Integer;
  LLayerIndex  : Integer;
  LFound       : Boolean;
begin
  Result       := nil;
  LArrayLength := Length(AFigureInfoArray);

  if LArrayLength > 0 then
  begin
    SetLength( Result, Length(Result) + 1 );

    // add the first layer index to the result array
    LAccumIndex         := 0;
    Result[LAccumIndex] := AFigureInfoArray[0].LayerIndex; 

    if LArrayLength > 1 then
    begin
      // pick the unrepeated layer index out from the passed array,
      // and save it to the result array 
      for i := 1 to (LArrayLength - 1) do
      begin
        LLayerIndex := AFigureInfoArray[i].LayerIndex;
        LFound      := False;
        
        for j := 0 to LAccumIndex do
        begin
          if LLayerIndex = Result[j] then
          begin
            LFound := True;
            Break;
          end;
        end;

        if not LFound then
        begin
          SetLength( Result, Length(Result) + 1 );

          LAccumIndex         := LAccumIndex + 1;
          Result[LAccumIndex] := LLayerIndex;
        end;
      end;
    end;
  end;
end;

{ TgmCustomVectorLayerCommand }

procedure TgmCustomVectorLayerCommand.SetCommandIcon(
  const AFigureFlag: TgmFigureFlags);
begin
  case AFigureFlag of
    ffStraightLine:
      begin
        ChangeCommandIconByResourceName(STRAIGHT_LINE_ICON_RES_NAME);
      end;

    ffCurve:
      begin
        ChangeCommandIconByResourceName(BEZIER_CURVE_ICON_RES_NAME);
      end;

    ffPolygon:
      begin
        ChangeCommandIconByResourceName(POLYGON_ICON_RES_NAME);
      end;

    ffRegularPolygon:
      begin
        ChangeCommandIconByResourceName(REGULAR_POLYGON_ICON_RES_NAME);
      end;

    ffRectangle,
    ffSquare:
      begin
        ChangeCommandIconByResourceName(RECTANGLE_ICON_RES_NAME);
      end;

    ffRoundRectangle,
    ffRoundSquare:
      begin
        ChangeCommandIconByResourceName(ROUND_RECT_ICON_RES_NAME);
      end;

    ffEllipse,
    ffCircle:
      begin
        ChangeCommandIconByResourceName(ELLIPSE_ICON_RES_NAME);
      end;
  end;
end;

{ TgmDeleteFiguresOnVectorLayersCommand }

constructor TgmDeleteFiguresOnVectorLayersCommand.Create(
  ALayerListBeforeDeletion: TgmLayerList;
  ADeletedFigureInfoArray: TgmArrayOfFigureInfo);
var
  i, j             : Integer;
  LArrayLength     : Integer;
  LLayerIndex      : Integer;
  LFigureIndex     : Integer;
  LSecondIndex     : Integer;
  LLayerIndexArray : TArrayOfInteger;
  LLayer           : TgmCustomLayer;
  LVectorLayer     : TgmVectorLayer;
  LFigure          : TgmFigureObject;
begin
  if not Assigned(ALayerListBeforeDeletion) then
  begin
    raise Exception.Create('TgmDeleteFiguresOnVectorLayersCommand.Create(): parameter ALayerListBeforeDeletion is a nil.');
  end;

  LArrayLength := Length(ADeletedFigureInfoArray);
  if LArrayLength <= 0 then
  begin
    raise Exception.Create('TgmDeleteFiguresOnVectorLayersCommand.Create(): the ADeletedFigureInfoArray is empty.');
  end;

  inherited Create('Delete Figures');

  FLayerList := ALayerListBeforeDeletion;

  // back up all of the figures on every vector layer that some of its figures
  // are deleted, for restoring them later...

  LLayerIndexArray := GetUnrepeatedVectorLayerIndexes(ADeletedFigureInfoArray);
  LArrayLength     := Length(LLayerIndexArray);
  
  QuickSort(LLayerIndexArray, 0, LArrayLength - 1);
  
  SetLength(FFigureRecArray, FLayerList.Count);
  for i := 0 to (LArrayLength - 1) do
  begin
    LLayerIndex := LLayerIndexArray[i];
    LLayer      := FLayerList.Layers[LLayerIndex];

    if LLayer is TgmVectorLayer then
    begin
      LVectorLayer := TgmVectorLayer(LLayer);

      if LVectorLayer.FigureList.Count > 0 then
      begin
        SetLength(FFigureRecArray[LLayerIndex], LVectorLayer.FigureList.Count);

        for j := 0 to (LVectorLayer.FigureList.Count - 1) do
        begin
          LFigure := TgmFigureObject(LVectorLayer.FigureList.Items[j]);

          FFigureRecArray[LLayerIndex, j].LayerIndex  := LLayerIndex;
          FFigureRecArray[LLayerIndex, j].FigureIndex := j;
          FFigureRecArray[LLayerIndex, j].FigureObj   := LFigure.GetSelfBackup();
        end;
      end;
    end;
  end;

  // Back up the index of deleted figures for redo ...
  //
  // The first dimension of the array corresponds to layer indexes,
  // the second dimension of the array corresponds to figure indexes.
  SetLength(FDeletedFigureIndexArray, FLayerList.Count);
  
  for i := 0 to High(ADeletedFigureInfoArray) do
  begin
    LLayerIndex  := ADeletedFigureInfoArray[i].LayerIndex;
    LFigureIndex := ADeletedFigureInfoArray[i].FigureIndex;

    // record a new figure index at corresponding layer index (the first dimension)
    SetLength( FDeletedFigureIndexArray[LLayerIndex], Length(FDeletedFigureIndexArray[LLayerIndex]) + 1 );

    LSecondIndex := High(FDeletedFigureIndexArray[LLayerIndex]);
    FDeletedFigureIndexArray[LLayerIndex, LSecondIndex] := LFigureIndex;
  end;

  // sorting ...
  for i := 0 to High(FDeletedFigureIndexArray) do
  begin
    LArrayLength := Length(FDeletedFigureIndexArray[i]);

    if LArrayLength > 0 then
    begin
      QuickSort(FDeletedFigureIndexArray[i], 0, LArrayLength - 1);
    end;
  end;

  SetLength(LLayerIndexArray, 0);
  LLayerIndexArray := nil;
end;

destructor TgmDeleteFiguresOnVectorLayersCommand.Destroy;
var
  i, j    : Integer;
  LLength : Integer;
begin
  for i := 0 to High(FFigureRecArray) do
  begin
    LLength := Length(FFigureRecArray[i]);

    if LLength > 0 then
    begin
      for j := High(FFigureRecArray[i]) downto 0 do
      begin
        FFigureRecArray[i, j].FigureObj.Free();
        FFigureRecArray[i, j].FigureObj := nil;
      end;

      SetLength(FFigureRecArray[i], 0);
      FFigureRecArray[i] := nil;
    end;
  end;
  
  SetLength(FFigureRecArray, 0);
  FFigureRecArray := nil;

  for i := 0 to High(FDeletedFigureIndexArray) do
  begin
    SetLength(FDeletedFigureIndexArray[i], 0);
    FDeletedFigureIndexArray[i] := nil;
  end;
  
  SetLength(FDeletedFigureIndexArray, 0);
  FDeletedFigureIndexArray := nil;

  inherited;
end;

procedure TgmDeleteFiguresOnVectorLayersCommand.Execute;
var
  i, j         : Integer;
  LArrayLength : Integer;
  LFigureIndex : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  inherited;

  // Remove the specified figures from specified vector layers again ...

  for i := 0 to High(FDeletedFigureIndexArray) do
  begin
    LArrayLength := Length(FDeletedFigureIndexArray[i]);

    if LArrayLength > 0 then
    begin
      LLayer := FLayerList.Layers[i];

      if not (LLayer is TgmVectorLayer) then
      begin
        raise Exception.Create('TgmDeleteFiguresOnVectorLayersCommand.Execute(): the specified layer is not a vector layer');
      end;

      LVectorLayer := TgmVectorLayer(LLayer);

      for j := (LArrayLength - 1) downto 0 do
      begin
        LFigureIndex := FDeletedFigureIndexArray[i, j];
        LVectorLayer.FigureList.DeleteFigureByIndex(LFigureIndex);
      end;

      LVectorLayer.DrawAllFiguresOnLayer();
      LVectorLayer.UpdateLayerThumbnail();
    end;
  end;
end;

procedure TgmDeleteFiguresOnVectorLayersCommand.Rollback;
var
  i, j         : Integer;
  LArrayLength : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigureRec   : TgmFigureRec;
begin
  inherited;

  // Remove all of the remained figures from vector layers that have figures
  // been deleted and restore all of the figures that before the deletion on
  // those vector layers ...

  for i := 0 to High(FFigureRecArray) do
  begin
    // The indexes of the first dimension of the array is corresponding to
    // layer indexes.
    LArrayLength := Length(FFigureRecArray[i]);

    // has figures been deleted on this vector layer ...
    if LArrayLength > 0 then
    begin
      LLayer := FLayerList.Layers[i];

      if not (LLayer is TgmVectorLayer) then
      begin
        raise Exception.Create('TgmDeleteFiguresOnVectorLayersCommand.Rollback(): the specified layer is not a vector layer');
      end;

      // deleting remained figures ...
      LVectorLayer := TgmVectorLayer(LLayer);
      LVectorLayer.FigureList.DeleteAllFigures();

      // restoring figures on this layer ...
      for j := 0 to (LArrayLength - 1) do
      begin
        LFigureRec := FFigureRecArray[i, j];

        if LFigureRec.LayerIndex <> i then
        begin
          raise Exception.Create('TgmDeleteFiguresOnVectorLayersCommand.Rollback(): layer index is mismatched');
        end;

        if LFigureRec.FigureIndex <> j then
        begin
          raise Exception.Create('TgmDeleteFiguresOnVectorLayersCommand.Rollback(): figure index is mismatched');
        end;

        LVectorLayer.FigureList.Add( LFigureRec.FigureObj.GetSelfBackup() );
      end;

      // rendering vector layers and updating its thumbnails ...
      LVectorLayer.DrawAllFiguresOnLayer();
      LVectorLayer.UpdateLayerThumbnail();
    end;
  end;
end;

{ TgmLockUnlockFiguresOnVectorLayersCommand }

constructor TgmLockUnlockFiguresOnVectorLayersCommand.Create(
  ALayerList: TgmLayerList; AFigureInfoArray: TgmArrayOfFigureInfo;
  const ALockMode: TgmFigureLockMode);
var
  i            : Integer;
  LArrayLength : Integer;
  LCommandName : string;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmLockUnlockFiguresOnVectorLayersCommand.Create(): parameter ALayerList is a nil.');
  end;

  LArrayLength := Length(AFigureInfoArray);
  if LArrayLength <= 0 then
  begin
    raise Exception.Create('TgmLockUnlockFiguresOnVectorLayersCommand.Create(): the AFigureInfoArray is empty.');
  end;

  case ALockMode of
    flmLock:
      begin
        LCommandName := 'Lock Selected Figures';
      end;
      
    flmUnlock:
      begin
        LCommandName := 'Unlock Selected Figures';
      end;
  end;

  inherited Create(LCommandName);
  
  FLayerList := ALayerList;
  FLockMode  := ALockMode;

  SetLength(FFigureInfoArray, LArrayLength);
  for i := 0 to (LArrayLength - 1) do
  begin
    FFigureInfoArray[i].LayerIndex  := AFigureInfoArray[i].LayerIndex;
    FFigureInfoArray[i].FigureIndex := AFigureInfoArray[i].FigureIndex;
  end;
end;

destructor TgmLockUnlockFiguresOnVectorLayersCommand.Destroy;
begin
  SetLength(FFigureInfoArray, 0);
  FFigureInfoArray := nil;

  inherited;
end;

procedure TgmLockUnlockFiguresOnVectorLayersCommand.Execute;
var
  i            : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigure      : TgmFigureObject;
begin
  inherited;

  for i := 0 to High(FFigureInfoArray) do
  begin
    LLayerIndex  := FFigureInfoArray[i].LayerIndex;
    LFigureIndex := FFigureInfoArray[i].FigureIndex;

    if not ( FLayerList.IsValidIndex(LLayerIndex) ) then
    begin
      raise Exception.Create('TgmLockUnlockFiguresOnVectorLayersCommand.Execute(): the layer index in FFigureInfoArray is out of the range.');
    end;

    LLayer := FLayerList.Layers[LLayerIndex];

    if not (LLayer is TgmVectorLayer) then
    begin
      raise Exception.Create('TgmLockUnlockFiguresOnVectorLayersCommand.Execute(): the layer at the index is not a vector layer.');
    end;

    LVectorLayer := TgmVectorLayer(LLayer);

    if not ( LVectorLayer.FigureList.ValidIndex(LFigureIndex) ) then
    begin
      raise Exception.Create('TgmLockUnlockFiguresOnVectorLayersCommand.Execute(): the figure index in FFigureInfoArray is out of the range.');
    end;

    LFigure := LVectorLayer.FigureList.Items[LFigureIndex];
    
    case FLockMode of
      flmLock:
        begin
          LFigure.IsLocked := True;
        end;

      flmUnlock:
        begin
          LFigure.IsLocked := False;
        end;
    end;
  end;
end;

procedure TgmLockUnlockFiguresOnVectorLayersCommand.Rollback;
var
  i            : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigure      : TgmFigureObject;
begin
  inherited;

  for i := 0 to High(FFigureInfoArray) do
  begin
    LLayerIndex  := FFigureInfoArray[i].LayerIndex;
    LFigureIndex := FFigureInfoArray[i].FigureIndex;

    if not ( FLayerList.IsValidIndex(LLayerIndex) ) then
    begin
      raise Exception.Create('TgmLockUnlockFiguresOnVectorLayersCommand.Rollback(): the layer index in FFigureInfoArray is out of the range.');
    end;

    LLayer := FLayerList.Layers[LLayerIndex];

    if not (LLayer is TgmVectorLayer) then
    begin
      raise Exception.Create('TgmLockUnlockFiguresOnVectorLayersCommand.Rollback(): the layer at the index is not a vector layer.');
    end;

    LVectorLayer := TgmVectorLayer(LLayer);

    if not ( LVectorLayer.FigureList.ValidIndex(LFigureIndex) ) then
    begin
      raise Exception.Create('TgmLockUnlockFiguresOnVectorLayersCommand.Rollback(): the figure index in FFigureInfoArray is out of the range.');
    end;

    LFigure := LVectorLayer.FigureList.Items[LFigureIndex];
    
    case FLockMode of
      flmLock:
        begin
          LFigure.IsLocked := False;
        end;

      flmUnlock:
        begin
          LFigure.IsLocked := True;
        end;
    end;
  end;
end;

{ TgmModifyFigurePropertiesOnVectorLayerCommand }

constructor TgmModifyFigurePropertiesOnVectorLayerCommand.Create(
  ALayerList: TgmLayerList; const AVectorLayerIndex, AFigureIndex: Integer;
  const AOldProperties, ANewProperties: TgmFigureBasicData);
var
  LFigure      : TgmFigureObject;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ( ALayerList.IsValidIndex(AVectorLayerIndex) ) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Create(): parameter AVectorLayerIndex is out of the range.');
  end;

  LLayer := ALayerList.Layers[AVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Create(): the layer at AVectorLayerIndex is not a vector layer.');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  if not ( LVectorLayer.FigureList.ValidIndex(AFigureIndex) ) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Create(): parameter AFigureIndex is out of the range.');
  end;

  inherited Create('Modify Figure Properties');

  LFigure := TgmFigureObject(LVectorLayer.FigureList.Items[AFigureIndex]);

  FLayerList        := ALayerList;
  FVectorLayerIndex := AVectorLayerIndex;
  FFigureIndex      := AFigureIndex;
  FFigureFlag       := LFigure.Flag;
  FOldProperties    := AOldProperties;
  FNewProperties    := ANewProperties;

  SetCommandIcon(FFigureFlag);
end;

procedure TgmModifyFigurePropertiesOnVectorLayerCommand.Execute;
var
  LFigure      : TgmFigureObject;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  inherited;

  if not ( FLayerList.IsValidIndex(FVectorLayerIndex) ) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Execute(): the FVectorLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Execute(): the layer at FVectorLayerIndex is not a vector layer.');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  if not ( LVectorLayer.FigureList.ValidIndex(FFigureIndex) ) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Execute(): the FFigureIndex is out of the range.');
  end;

  LFigure := TgmFigureObject(LVectorLayer.FigureList.Items[FFigureIndex]);
  if LFigure.Flag <> FFigureFlag then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Execute(): the flag of the figure does not match the recorded one.');
  end;

  with LFigure do
  begin
    Name       := FNewProperties.Name;
    PenColor   := FNewProperties.PenColor;
    BrushColor := FNewProperties.BrushColor;
    PenWidth   := FNewProperties.PenWidth;
    PenStyle   := FNewProperties.PenStyle;
    BrushStyle := FNewProperties.BrushStyle;

    if Flag in [ffRegularPolygon, ffSquare, ffRoundSquare, ffCircle] then
    begin
      OriginPixelX := FNewProperties.OriginX;
      OriginPixelY := FNewProperties.OriginY;
      RadiusPixel  := FNewProperties.Radius;
    end;
  end;

  LVectorLayer.DrawAllFiguresOnLayer();
  LVectorLayer.UpdateLayerThumbnail();
end;

procedure TgmModifyFigurePropertiesOnVectorLayerCommand.Rollback;
var
  LFigure      : TgmFigureObject;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  inherited;

  if not ( FLayerList.IsValidIndex(FVectorLayerIndex) ) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Rollback(): the FVectorLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Rollback(): the layer at FVectorLayerIndex is not a vector layer.');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  if not ( LVectorLayer.FigureList.ValidIndex(FFigureIndex) ) then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Rollback(): the FFigureIndex is out of the range.');
  end;

  LFigure := TgmFigureObject(LVectorLayer.FigureList.Items[FFigureIndex]);
  if LFigure.Flag <> FFigureFlag then
  begin
    raise Exception.Create('TgmModifyFigurePropertiesOnVectorLayerCommand.Rollback(): the flag of the figure does not match the recorded one.');
  end;

  with LFigure do
  begin
    Name       := FOldProperties.Name;
    PenColor   := FOldProperties.PenColor;
    BrushColor := FOldProperties.BrushColor;
    PenWidth   := FOldProperties.PenWidth;
    PenStyle   := FOldProperties.PenStyle;
    BrushStyle := FOldProperties.BrushStyle;

    if Flag in [ffRegularPolygon, ffSquare, ffRoundSquare, ffCircle] then
    begin
      OriginPixelX := FOldProperties.OriginX;
      OriginPixelY := FOldProperties.OriginY;
      RadiusPixel  := FOldProperties.Radius;
    end;
  end;

  LVectorLayer.DrawAllFiguresOnLayer();
  LVectorLayer.UpdateLayerThumbnail();
end;

{ TgmModifyFiguresOnVectorLayersCommand }

constructor TgmModifyFiguresOnVectorLayersCommand.Create(
  ALayerList: TgmLayerList);
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Create(): parameter ALayerList is nil.');
  end;

  inherited Create('Figures Modified');

  FLayerList          := ALayerList;
  FOldFiguresRecorded := False;
  FNewFiguresRecorded := False;
end;

destructor TgmModifyFiguresOnVectorLayersCommand.Destroy;
begin
  ClearOldRecords();
  ClearNewRecords();
  
  inherited;
end;

procedure TgmModifyFiguresOnVectorLayersCommand.ClearNewRecords;
var
  i, j    : Integer;
  LLength : Integer;
begin
  for i := 0 to High(FNewFigureRecArray) do
  begin
    LLength := Length(FNewFigureRecArray[i]);

    if LLength > 0 then
    begin
      for j := High(FNewFigureRecArray[i]) downto 0 do
      begin
        FNewFigureRecArray[i, j].FigureObj.Free();
        FNewFigureRecArray[i, j].FigureObj := nil;
      end;

      SetLength(FNewFigureRecArray[i], 0);
      FNewFigureRecArray[i] := nil;
    end;
  end;

  SetLength(FNewFigureRecArray, 0);
  FNewFigureRecArray := nil;
end;

procedure TgmModifyFiguresOnVectorLayersCommand.ClearOldRecords;
var
  i, j    : Integer;
  LLength : Integer;
begin
  for i := 0 to High(FOldFigureRecArray) do
  begin
    LLength := Length(FOldFigureRecArray[i]);

    if LLength > 0 then
    begin
      for j := High(FOldFigureRecArray[i]) downto 0 do
      begin
        FOldFigureRecArray[i, j].FigureObj.Free();
        FOldFigureRecArray[i, j].FigureObj := nil;
      end;

      SetLength(FOldFigureRecArray[i], 0);
      FOldFigureRecArray[i] := nil;
    end;
  end;
  
  SetLength(FOldFigureRecArray, 0);
  FOldFigureRecArray := nil;
end;

procedure TgmModifyFiguresOnVectorLayersCommand.Execute;
var
  i, j         : Integer;
  LArrayLength : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigureRec   : TgmFigureRec;
begin
  inherited;

  if not FNewFiguresRecorded then
  begin
    raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Execute(): new orders have not been recorded.');
  end;

  // Remove all of the figures from vector layers in the layer list,
  // and restore all of the figures that after the modification.

  for i := 0 to High(FNewFigureRecArray) do
  begin
    // The indexes of the first dimension of the array is corresponding to
    // layer indexes.
    LArrayLength := Length(FNewFigureRecArray[i]);

    // this layer is a vector layer and has figures on it ... 
    if LArrayLength > 0 then
    begin
      LLayer := FLayerList.Layers[i];

      if not (LLayer is TgmVectorLayer) then
      begin
        raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Execute(): the specified layer is not a vector layer');
      end;

      // deleting figures ...
      LVectorLayer := TgmVectorLayer(LLayer);
      LVectorLayer.FigureList.DeleteAllFigures();

      // restoring figures on this layer ...
      for j := 0 to (LArrayLength - 1) do
      begin
        LFigureRec := FNewFigureRecArray[i, j];

        if LFigureRec.LayerIndex <> i then
        begin
          raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Execute(): layer index is mismatched');
        end;

        if LFigureRec.FigureIndex <> j then
        begin
          raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Execute(): figure index is mismatched');
        end;

        LVectorLayer.FigureList.Add( LFigureRec.FigureObj.GetSelfBackup() );
      end;

      // rendering vector layers and updating its thumbnails ...
      LVectorLayer.DrawAllFiguresOnLayer();
      LVectorLayer.UpdateLayerThumbnail();
    end;
  end;
end;

procedure TgmModifyFiguresOnVectorLayersCommand.Rollback;
var
  i, j         : Integer;
  LArrayLength : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigureRec   : TgmFigureRec;
begin
  inherited;

  if not FOldFiguresRecorded then
  begin
    raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Rollback(): old orders have not been recorded.');
  end;

  // Remove all of the figures from vector layers in the layer list,
  // and restore all of the figures that before the modification.

  for i := 0 to High(FOldFigureRecArray) do
  begin
    // The indexes of the first dimension of the array is corresponding to
    // layer indexes.
    LArrayLength := Length(FOldFigureRecArray[i]);

    // this layer is a vector layer and has figures on it ... 
    if LArrayLength > 0 then
    begin
      LLayer := FLayerList.Layers[i];

      if not (LLayer is TgmVectorLayer) then
      begin
        raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Rollback(): the specified layer is not a vector layer');
      end;

      // deleting figures ...
      LVectorLayer := TgmVectorLayer(LLayer);
      LVectorLayer.FigureList.DeleteAllFigures();

      // restoring figures on this layer ...
      for j := 0 to (LArrayLength - 1) do
      begin
        LFigureRec := FOldFigureRecArray[i, j];

        if LFigureRec.LayerIndex <> i then
        begin
          raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Rollback(): layer index is mismatched');
        end;

        if LFigureRec.FigureIndex <> j then
        begin
          raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.Rollback(): figure index is mismatched');
        end;

        LVectorLayer.FigureList.Add( LFigureRec.FigureObj.GetSelfBackup() );
      end;

      // rendering vector layers and updating its thumbnails ...
      LVectorLayer.DrawAllFiguresOnLayer();
      LVectorLayer.UpdateLayerThumbnail();
    end;
  end;
end;

procedure TgmModifyFiguresOnVectorLayersCommand.SetNewFiguresData( 
  ALayerList: TgmLayerList);
var
  i, j         : Integer;
  LRecordCount : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigure      : TgmFigureObject;
begin
  FNewFiguresRecorded := False;
  LRecordCount        := 0;

  ClearNewRecords();
  
  if Assigned(ALayerList) then
  begin
    if ALayerList <> FLayerList then
    begin
      raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.SetNewFiguresData(): layer list mismatched.');
    end;

    if ALayerList.Count > 0 then
    begin
      // back up all of the figures on every vector layer in the layer list
      // for restoring them in Redo operation ...
      SetLength(FNewFigureRecArray, FLayerList.Count);
      
      for i := 0 to (FLayerList.Count - 1) do
      begin
        LLayer := FLayerList.Layers[i];

        if LLayer is TgmVectorLayer then
        begin
          LVectorLayer := TgmVectorLayer(LLayer);

          if LVectorLayer.FigureList.Count > 0 then
          begin
            SetLength(FNewFigureRecArray[i], LVectorLayer.FigureList.Count);
            LRecordCount := LRecordCount + LVectorLayer.FigureList.Count;

            for j := 0 to (LVectorLayer.FigureList.Count - 1) do
            begin
              LFigure := TgmFigureObject(LVectorLayer.FigureList.Items[j]);

              FNewFigureRecArray[i, j].LayerIndex  := i;
              FNewFigureRecArray[i, j].FigureIndex := j;
              FNewFigureRecArray[i, j].FigureObj   := LFigure.GetSelfBackup();
            end;
          end;
        end;
      end;
    end; 
  end;

  FNewFiguresRecorded := (LRecordCount > 0);
end;

procedure TgmModifyFiguresOnVectorLayersCommand.SetOldFiguresData(
  ALayerList: TgmLayerList);
var
  i, j         : Integer;
  LRecordCount : Integer;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigure      : TgmFigureObject;
begin
  FOldFiguresRecorded := False;
  LRecordCount        := 0;
  
  ClearOldRecords();
  
  if Assigned(ALayerList) then
  begin
    if ALayerList <> FLayerList then
    begin
      raise Exception.Create('TgmModifyFiguresOnVectorLayersCommand.SetOldFiguresData(): layer list mismatched.');
    end;

    if ALayerList.Count > 0 then
    begin
      // back up all of the figures on every vector layer in the layer list
      // for restoring them in Undo operation ...
      SetLength(FOldFigureRecArray, FLayerList.Count);
      
      for i := 0 to (FLayerList.Count - 1) do
      begin
        LLayer := FLayerList.Layers[i];

        if LLayer is TgmVectorLayer then
        begin
          LVectorLayer := TgmVectorLayer(LLayer);

          if LVectorLayer.FigureList.Count > 0 then
          begin
            SetLength(FOldFigureRecArray[i], LVectorLayer.FigureList.Count);
            LRecordCount := LRecordCount + LVectorLayer.FigureList.Count;

            for j := 0 to (LVectorLayer.FigureList.Count - 1) do
            begin
              LFigure := TgmFigureObject(LVectorLayer.FigureList.Items[j]);

              FOldFigureRecArray[i, j].LayerIndex  := i;
              FOldFigureRecArray[i, j].FigureIndex := j;
              FOldFigureRecArray[i, j].FigureObj   := LFigure.GetSelfBackup();
            end;
          end;
        end;
      end;
    end; 
  end;

  FOldFiguresRecorded := (LRecordCount > 0);
end;

{ TgmNewFigureOnExistedVectorLayerCommand }

constructor TgmNewFigureOnExistedVectorLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const AVectorLayerIndex, ANewFigureIndex: Integer);
var
  LCommandName : string;
  LFigure      : TgmFigureObject;
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(AVectorLayerIndex) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Create(): parameter AVectorLayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[AVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Create(): the layer at AVectorLayerIndex is not a vector layer.');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  if not ( LVectorLayer.FigureList.ValidIndex(ANewFigureIndex) ) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Create(): parameter ANewFigureIndex is out of the range.');
  end;

  LFigure      := TgmFigureObject(LVectorLayer.FigureList.Items[ANewFigureIndex]);
  LCommandName := gmFigures.GetFigureName(LFigure.Flag);
  inherited Create(LCommandName);

  FChannelManager   := AChannelManager;
  FLayerList        := ALayerList;
  FVectorLayerIndex := AVectorLayerIndex;
  FFigureIndex      := ANewFigureIndex;
  FFigureFlag       := LFigure.Flag;
  FAddedFigure      := LFigure.GetSelfBackup();

  SetCommandIcon(LFigure.Flag);
end;

destructor TgmNewFigureOnExistedVectorLayerCommand.Destroy;
begin
  FAddedFigure.Free();
  inherited;
end;

procedure TgmNewFigureOnExistedVectorLayerCommand.ChangeFigureCounter(
  const AFigureFlag: TgmFigureFlags; const AChangedAmount: Integer);
var
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  if not FLayerList.IsValidIndex(FVectorLayerIndex) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.ChangeFigureCounter(): the FVectorLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.ChangeFigureCounter(): the layer at FVectorLayerIndex is not a vector layer');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  
  with LVectorLayer.FigureList do
  begin
    case FFigureFlag of
      ffStraightLine:
        begin
          LineNumber := LineNumber + AChangedAmount;
        end;
        
      ffCurve:
        begin
          CurveNumber := CurveNumber + AChangedAmount;
        end;
        
      ffPolygon:
        begin
          PolygonNumber := PolygonNumber + AChangedAmount;
        end;
        
      ffRegularPolygon:
        begin
          RegularPolygonNumber := RegularPolygonNumber + AChangedAmount;
        end;
        
      ffRectangle:
        begin
          RectangleNumber := RectangleNumber + AChangedAmount;
        end;
        
      ffSquare:
        begin
          SquareNumber := SquareNumber + AChangedAmount;
        end;
        
      ffRoundRectangle:
        begin
          RoundRectangleNumber := RoundRectangleNumber + AChangedAmount;
        end;
        
      ffRoundSquare:
        begin
          RoundSquareNumber := RoundSquareNumber + AChangedAmount;
        end;
        
      ffEllipse:
        begin
          EllipseNumber := EllipseNumber + AChangedAmount;
        end;
        
      ffCircle:
        begin
          CircleNumber := CircleNumber + AChangedAmount;
        end;
    end;
  end;
end;

procedure TgmNewFigureOnExistedVectorLayerCommand.Execute;
var
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigure      : TgmFigureObject;
begin
  inherited;

  if not FLayerList.IsValidIndex(FVectorLayerIndex) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Execute(): the FVectorLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Execute(): the layer at FVectorLayerIndex is not a vector layer');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FVectorLayerIndex then
  begin
    FLayerList.SelectLayer(FVectorLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LFigure      := FAddedFigure.GetSelfBackup();
  LVectorLayer := TgmVectorLayer(LLayer);

  LVectorLayer.FigureList.Insert(FFigureIndex, LFigure);
  LVectorLayer.DrawAllFiguresOnLayer();
  LVectorLayer.UpdateLayerThumbnail();

  ChangeFigureCounter(FFigureFlag, +1);
end;

procedure TgmNewFigureOnExistedVectorLayerCommand.Rollback;
var
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FVectorLayerIndex) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Execute(): the FVectorLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Execute(): the layer at FVectorLayerIndex is not a vector layer');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  if not ( LVectorLayer.FigureList.ValidIndex(FFigureIndex) ) then
  begin
    raise Exception.Create('TgmNewFigureOnExistedVectorLayerCommand.Execute(): the FFigureIndex is out of the range');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FVectorLayerIndex then
  begin
    FLayerList.SelectLayer(FVectorLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LVectorLayer.FigureList.DeleteFigureByIndex(FFigureIndex);
  LVectorLayer.DrawAllFiguresOnLayer();
  LVectorLayer.UpdateLayerThumbnail();

  ChangeFigureCounter(FFigureFlag, -1);
end;

{ TgmNewFigureOnNewVectorLayerCommand }

constructor TgmNewFigureOnNewVectorLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  AVectorLayer: TgmVectorLayer; const AVectorLayerIndex: Integer);
var
  LCommandName : string;
  LFigure      : TgmFigureObject;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewFigureOnNewVectorLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmNewFigureOnNewVectorLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(AVectorLayer) then
  begin
    raise Exception.Create('TgmNewFigureOnNewVectorLayerCommand.Create(): parameter AVectorLayer is nil.');
  end;

  if not ALayerList.IsValidIndex(AVectorLayerIndex) then
  begin
    raise Exception.Create('TgmNewFigureOnNewVectorLayerCommand.Create(): parameter AVectorLayerIndex is out of range.');
  end;

  if AVectorLayer.FigureList.Count <= 0 then
  begin
    raise Exception.Create('TgmNewFigureOnNewVectorLayerCommand.Create(): there is no any figure be included in the vector layer.');
  end;

  LFigure      := TgmFigureObject(AVectorLayer.FigureList.Items[0]);
  LCommandName := gmFigures.GetFigureName(LFigure.Flag);
  inherited Create(LCommandName);

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := AVectorLayerIndex;
  FLayerName      := AVectorLayer.LayerName;
  FVectorLayer    := TgmVectorLayer( AVectorLayer.GetCopy() );

  SetCommandIcon(LFigure.Flag);
end;

destructor TgmNewFigureOnNewVectorLayerCommand.Destroy;
begin
  FVectorLayer.Free();
  inherited;
end;

procedure TgmNewFigureOnNewVectorLayerCommand.Execute;
var
  LDuplicatedLayer : TgmVectorLayer;
  LLayerCounter    : Integer;
begin
  inherited;

  LDuplicatedLayer           := TgmVectorLayer( FVectorLayer.GetCopy() );
  LDuplicatedLayer.LayerName := Self.FLayerName;

  LDuplicatedLayer.UpdateLayerThumbnail();

  LLayerCounter := FLayerList.GetLayerCounter(TgmVectorLayer);

  FLayerList.SimplyInsert(FLayerIndex, LDuplicatedLayer);
  FLayerList.SimplySelectLayer(FLayerIndex);
  FLayerList.SetLayerCounter(TgmVectorLayer, LLayerCounter + 1);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmNewFigureOnNewVectorLayerCommand.Rollback;
var
  LLayer        : TgmCustomLayer;
  LSelectIndex  : Integer;
  LLayerCounter : Integer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmNewFigureOnNewVectorLayerCommand.Rollback(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmNewFigureOnNewVectorLayerCommand.Rollback(): the layer at FLayerIndex is not a vector layer.');
  end;

  LLayerCounter := FLayerList.GetLayerCounter(TgmVectorLayer);

  FLayerList.SimplyDeleteLayer(FLayerIndex);
  FLayerList.SetLayerCounter(TgmVectorLayer, LLayerCounter - 1);

  LSelectIndex := FLayerIndex - 1;
  if LSelectIndex < 0 then
  begin
    LSelectIndex := 0;
  end;
  
  FLayerList.SimplySelectLayer(LSelectIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmSingleFigureAdjustmentCommand }

constructor TgmSingleFigureAdjustmentCommand.Create(ALayerList: TgmLayerList;
  const AVectorLayerIndex, AFigureIndex: Integer;
  const AOldFigure, ANewFigure: TgmFigureObject);
var
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigure      : TgmFigureObject;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(AVectorLayerIndex) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Create(): parameter AVectorLayerIndex is out of the range.');
  end;

  LLayer := ALayerList.Layers[AVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Create(): the layer at AVectorLayerIndex in the list is not a vector layer.');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  if not LVectorLayer.FigureList.ValidIndex(AFigureIndex) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Create(): parameter AFigureIndex in out of the range.');
  end;

  if not Assigned(AOldFigure) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Create(): parameter AOldFigure is nil.');
  end;

  if not Assigned(ANewFigure) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Create(): parameter ANewFigure is nil.');
  end;

  inherited Create('Figure Adjustment');
  
  FLayerList        := ALayerList;
  FVectorLayerIndex := AVectorLayerIndex;
  FFigureIndex      := AFigureIndex;
  LFigure           := TgmFigureObject(LVectorLayer.FigureList.Items[AFigureIndex]);
  FFigureFlag       := LFigure.Flag;
  FOldFigure        := AOldFigure.GetSelfBackup();
  FNewFigure        := ANewFigure.GetSelfBackup();

  SetCommandIcon(FFigureFlag);
end;

destructor TgmSingleFigureAdjustmentCommand.Destroy;
begin
  FOldFigure.Free();
  FNewFigure.Free();

  inherited;
end;

procedure TgmSingleFigureAdjustmentCommand.Execute;
var
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigure      : TgmFigureObject;
begin
  inherited;

  if not FLayerList.IsValidIndex(FVectorLayerIndex) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Execute(): FVectorLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Execute(): the layer at FVectorLayerIndex in the list is not a vector layer.');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  if not LVectorLayer.FigureList.ValidIndex(FFigureIndex) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Execute(): FFigureIndex in out of the range.');
  end;

  LFigure := TgmFigureObject(LVectorLayer.FigureList.Items[FFigureIndex]);

  if LFigure.Flag <> FFigureFlag then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Execute(): the flag of target figure is mismatched.');
  end;

  if LFigure.Flag <> FNewFigure.Flag then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Execute(): the flags between target figure and new figure is mismatched.');
  end;

  LFigure.AssignData(FNewFigure);
  LVectorLayer.DrawAllFiguresOnLayer();
  LVectorLayer.UpdateLayerThumbnail();
end;

procedure TgmSingleFigureAdjustmentCommand.Rollback;
var
  LLayer       : TgmCustomLayer;
  LVectorLayer : TgmVectorLayer;
  LFigure      : TgmFigureObject;
begin
  inherited;

  if not FLayerList.IsValidIndex(FVectorLayerIndex) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Rollback(): FVectorLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FVectorLayerIndex];
  if not (LLayer is TgmVectorLayer) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Rollback(): the layer at FVectorLayerIndex in the list is not a vector layer.');
  end;

  LVectorLayer := TgmVectorLayer(LLayer);
  if not LVectorLayer.FigureList.ValidIndex(FFigureIndex) then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Rollback(): FFigureIndex in out of the range.');
  end;

  LFigure := TgmFigureObject(LVectorLayer.FigureList.Items[FFigureIndex]);

  if LFigure.Flag <> FFigureFlag then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Rollback(): the flag of target figure is mismatched.');
  end;

  if LFigure.Flag <> FOldFigure.Flag then
  begin
    raise Exception.Create('TgmSingleFigureAdjustmentCommand.Rollback(): the flags between target figure and old figure is mismatched.');
  end;

  LFigure.AssignData(FOldFigure);
  LVectorLayer.DrawAllFiguresOnLayer();
  LVectorLayer.UpdateLayerThumbnail();
end;

{ TgmTranslateFiguresOnVectorLayersCommand }

constructor TgmTranslateFiguresOnVectorLayersCommand.Create(
  ALayerList: TgmLayerList; AFigureInfoArray: TgmArrayOfFigureInfo;
  const ATranslatedVector: TPoint);
var
  i            : Integer;
  LArrayLength : Integer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.Create(): parameter ALayerList is a nil.');
  end;

  LArrayLength := Length(AFigureInfoArray);
  if LArrayLength <= 0 then
  begin
    raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.Create(): the AFigureInfoArray is empty.');
  end;

  inherited Create('Move Figures');
  
  FLayerList        := ALayerList;
  FTranslatedVector := ATranslatedVector;

  SetLength(FFigureInfoArray, LArrayLength);
  for i := 0 to (LArrayLength - 1) do
  begin
    FFigureInfoArray[i].LayerIndex  := AFigureInfoArray[i].LayerIndex;
    FFigureInfoArray[i].FigureIndex := AFigureInfoArray[i].FigureIndex;
  end;

  ChangeCommandIconByResourceName(gmHistoryCommands.MOVE_COMMAND_ICON_RES_NAME);
end;

destructor TgmTranslateFiguresOnVectorLayersCommand.Destroy;
begin
  SetLength(FFigureInfoArray, 0);
  FFigureInfoArray := nil;

  inherited;
end;

procedure TgmTranslateFiguresOnVectorLayersCommand.Execute;
var
  i                : Integer;
  LLayerIndex      : Integer;
  LFigureIndex     : Integer;
  LArrayLength     : Integer;
  LLayer           : TgmCustomLayer;
  LVectorLayer     : TgmVectorLayer;
  LFigure          : TgmFigureObject;
  LLayerIndexArray : TArrayOfInteger;
begin
  inherited;

  for i := 0 to High(FFigureInfoArray) do
  begin
    LLayerIndex  := FFigureInfoArray[i].LayerIndex;
    LFigureIndex := FFigureInfoArray[i].FigureIndex;

    if not ( FLayerList.IsValidIndex(LLayerIndex) ) then
    begin
      raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.Execute(): the layer index in FFigureInfoArray is out of the range.');
    end;

    LLayer := FLayerList.Layers[LLayerIndex];

    if not (LLayer is TgmVectorLayer) then
    begin
      raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.Execute(): the layer at the index is not a vector layer.');
    end;

    LVectorLayer := TgmVectorLayer(LLayer);

    if not ( LVectorLayer.FigureList.ValidIndex(LFigureIndex) ) then
    begin
      raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.Execute(): the figure index in FFigureInfoArray is out of the range.');
    end;

    LFigure := LVectorLayer.FigureList.Items[LFigureIndex];
    LFigure.Translate(FTranslatedVector);
  end;

  LLayerIndexArray := GetUnrepeatedVectorLayerIndexes(FFigureInfoArray);
  LArrayLength     := Length(LLayerIndexArray);

  if LArrayLength <= 0 then
  begin
    raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.Execute(): get returned unrepeated layer index array is empty.');
  end;

  for i := 0 to (LArrayLength - 1) do
  begin
    LLayerIndex  := LLayerIndexArray[i];
    LVectorLayer := TgmVectorLayer(FLayerList.Layers[LLayerIndex]);

    LVectorLayer.DrawAllFiguresOnLayer();
    LVectorLayer.UpdateLayerThumbnail();
  end;

  SetLength(LLayerIndexArray, 0);
  LLayerIndexArray := nil;
end;

procedure TgmTranslateFiguresOnVectorLayersCommand.Rollback;
var
  i                : Integer;
  LLayerIndex      : Integer;
  LFigureIndex     : Integer;
  LArrayLength     : Integer;
  LReverseVector   : TPoint;
  LLayer           : TgmCustomLayer;
  LVectorLayer     : TgmVectorLayer;
  LFigure          : TgmFigureObject;
  LLayerIndexArray : TArrayOfInteger;
begin
  inherited;

  LReverseVector.X := -FTranslatedVector.X;
  LReverseVector.Y := -FTranslatedVector.Y;

  for i := 0 to High(FFigureInfoArray) do
  begin
    LLayerIndex  := FFigureInfoArray[i].LayerIndex;
    LFigureIndex := FFigureInfoArray[i].FigureIndex;

    if not ( FLayerList.IsValidIndex(LLayerIndex) ) then
    begin
      raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.RollBack(): the layer index in FFigureInfoArray is out of the range.');
    end;

    LLayer := FLayerList.Layers[LLayerIndex];

    if not (LLayer is TgmVectorLayer) then
    begin
      raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.RollBack(): the layer at the index is not a vector layer.');
    end;

    LVectorLayer := TgmVectorLayer(LLayer);

    if not ( LVectorLayer.FigureList.ValidIndex(LFigureIndex) ) then
    begin
      raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.RollBack(): the figure index in FFigureInfoArray is out of the range.');
    end;

    LFigure := LVectorLayer.FigureList.Items[LFigureIndex];
    LFigure.Translate(LReverseVector);
  end;

  LLayerIndexArray := GetUnrepeatedVectorLayerIndexes(FFigureInfoArray);
  LArrayLength     := Length(LLayerIndexArray);

  if LArrayLength <= 0 then
  begin
    raise Exception.Create('TgmTranslateFiguresOnVectorLayersCommand.RollBack(): get returned unrepeated layer index array is empty.');
  end;

  for i := 0 to (LArrayLength - 1) do
  begin
    LLayerIndex  := LLayerIndexArray[i];
    LVectorLayer := TgmVectorLayer(FLayerList.Layers[LLayerIndex]);

    LVectorLayer.DrawAllFiguresOnLayer();
    LVectorLayer.UpdateLayerThumbnail();
  end;

  SetLength(LLayerIndexArray, 0);
  LLayerIndexArray := nil;
end;

end.
