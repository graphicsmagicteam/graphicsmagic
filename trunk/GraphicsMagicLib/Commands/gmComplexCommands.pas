unit gmComplexCommands;

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

// Update Date: 2017/01/22

interface

uses
{ Delphi }
  Windows, SysUtils, Classes, ComCtrls,
{ Graphics32 }
  GR32,
{ GraphicsMagicLib }
  gmChannelManager,
  gmCrop,
  gmHistoryCommands,
  gmLayers,
  gmPaths,
  gmResamplers,
  gmTypes;

type
  TgmMergeMode = (mmFlatten, mmMergeDown, mmMergeVisible);

  { TgmLayerMergenceCommand }

  TgmLayerMergenceCommand = class(TgmCustomCommand)
  private
    FChannelManager         : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList              : TgmLayerList;             // pointer to an external layer list
    FLayerCount             : Integer;
    FSelectedIndex          : Integer;
    FLayerWidth             : Integer;
    FLayerHeight            : Integer;
    FMergeMode              : TgmMergeMode;
    FLayerFileName          : string;
    FTextEditor             : TRichEdit;                // pointer to an external text editor for loading Type layers
    FVisibleLayerIndexArray : array of Integer;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerListBeforeMergence: TgmLayerList; ATextEditor: TRichEdit;
      const AMergeMode: TgmMergeMode);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCropCommand }

  TgmCropCommand = class(TgmCustomCommand)
  private
    FChannelManager     : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList          : TgmLayerList;             // pointer to an external layer list
    FPathList           : TgmPathList;              // pointer to an external path list
    FTextEditor         : TRichEdit;                // pointer to an external text editor for loading Type layers
    FDataFileName       : string;
    FCropStart          : TPoint;
    FCropEnd            : TPoint;
    FResizedWidth       : Integer;
    FResizedHeight      : Integer;
    FResized            : Boolean;
    FSelectedLayerIndex : Integer;
    FBackgroundColor    : TColor32;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; APathList: TgmPathList; ATextEditor: TRichEdit;
      ACrop: TgmCrop; const ABackColor: TColor32);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmRectCropCommand }

  TgmRectCropCommand = class(TgmCustomCommand)
  private
    FChannelManager     : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList          : TgmLayerList;             // pointer to an external layer list
    FPathList           : TgmPathList;              // pointer to an external path list
    FTextEditor         : TRichEdit;                // pointer to an external text editor for loading Type layers
    FDataFileName       : string;
    FCropRect           : TRect;
    FSelectedLayerIndex : Integer;
    FBackgroundColor    : TColor32;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
      APathList: TgmPathList; ATextEditor: TRichEdit; const ACropRect: TRect;
      const ABackColor: TColor32);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmRotateCanvasCommand }

  TgmRotateCanvasCommand = class(TgmCustomCommand)
  private
    FChannelManager     : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList          : TgmLayerList;             // pointer to an external layer list
    FPathList           : TgmPathList;              // pointer to an external path list
    FTextEditor         : TRichEdit;                // pointer to an external text editor for loading Type layers
    FDataFileName       : string;
    FSelectedLayerIndex : Integer;
    FRotateDegrees      : Integer;
    FRotateDirection    : TgmRotateDirection;
    FBackgroundColor    : TColor32;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; APathList: TgmPathList; ATextEditor: TRichEdit;
      const ARotateDegrees: Integer; const ARotateDirection: TgmRotateDirection;
      const ABackgroundColor: TColor32);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmImageSizeCommand }

  TgmImageSizeCommand = class(TgmCustomCommand)
  private
    FChannelManager     : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList          : TgmLayerList;             // pointer to an external layer list
    FPathList           : TgmPathList;              // pointer to an external path list
    FTextEditor         : TRichEdit;                // pointer to an external text editor for loading Type layers
    FDataFileName       : string;
    FSelectedLayerIndex : Integer;
    FNewWidth           : Integer;
    FNewHeight          : Integer;
    FResamplingOptions  : TgmResamplingOptions;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; APathList: TgmPathList; ATextEditor: TRichEdit;
      const ANewWidth, ANewHeight: Integer;
      const AResamplingOptions: TgmResamplingOptions);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCanvasSizeCommand }

  TgmCanvasSizeCommand = class(TgmCustomCommand)
  private
    FChannelManager     : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList          : TgmLayerList;             // pointer to an external layer list
    FPathList           : TgmPathList;              // pointer to an external path list
    FTextEditor         : TRichEdit;                // pointer to an external text editor for loading Type layers
    FDataFileName       : string;
    FSelectedLayerIndex : Integer;
    FNewWidth           : Integer;
    FNewHeight          : Integer;
    FAnchorDirection    : TgmAnchorDirection;
    FBackgroundColor    : TColor32;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; APathList: TgmPathList; ATextEditor: TRichEdit;
      const ANewWidth, ANewHeight: Integer;
      const AAnchorDirection: TgmAnchorDirection;
      const ABackgroundColor: TColor32);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;
  

implementation

uses
{ GraphicsMagicLib }
  gmChannels,
  gmGMDFile,
  gmLayerIO;


{ TgmCanvasSizeCommand }

constructor TgmCanvasSizeCommand.Create(AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; APathList: TgmPathList; ATextEditor: TRichEdit;
  const ANewWidth, ANewHeight: Integer;
  const AAnchorDirection: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
var
  LGMDManager : TgmGMDManager;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCanvasSizeCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmCanvasSizeCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmCanvasSizeCommand.Create(): parameter APathList is a nil pointer.');
  end;

  if not Assigned(ATextEditor) then
  begin
    raise Exception.Create('TgmCanvasSizeCommand.Create(): parameter ATextEditor is a nil pointer.');
  end;

  if ALayerList.Count <= 0 then
  begin
    raise Exception.Create('TgmCanvasSizeCommand.Create(): there was no any layers in the passed layer list.');
  end;

  if (ANewWidth <= 0) or (ANewHeight <= 0) then
  begin
    raise Exception.Create('TgmCanvasSizeCommand.Create(): both ANewWidth and ANewHeight should be greater than zero.');
  end;

  inherited Create('Canvas Size');

  FChannelManager     := AChannelManager;
  FLayerList          := ALayerList;
  FPathList           := APathList;
  FTextEditor         := ATextEditor;
  FSelectedLayerIndex := FLayerList.SelectedIndex;
  FNewWidth           := ANewWidth;
  FNewHeight          := ANewHeight;
  FAnchorDirection    := AAnchorDirection;
  FBackgroundColor    := ABackgroundColor;

  // saving data
  FDataFileName := COMMAND_DATA_DIR + '\CanvasSize' + IntToStr( GetTickCount() );

  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;

    LGMDManager.SaveToFile(FDataFileName);
  finally
    LGMDManager.Free();
  end;
end;

destructor TgmCanvasSizeCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FDataFileName) then
  begin
    DeleteFile(PChar(FDataFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmCanvasSizeCommand.Execute;
var
  LOffset : TPoint;
begin
  inherited;

  // resize canvas of each layer ...
  FLayerList.ResizeCanvasOfLayers(FNewWidth, FNewHeight,
    FAnchorDirection, FBackgroundColor);

  // change the canvas size of each channel
  FChannelManager.ResizeCanvasOfChannels(FNewWidth, FNewHeight, FAnchorDirection);

  // dealing with paths
  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(FNewWidth, FNewHeight, LOffset);
  end;
end;

procedure TgmCanvasSizeCommand.Rollback;
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
  LGMDManager   : TgmGMDManager;
  LOffset       : TPoint;
begin
  inherited;

  if not FileExists(FDataFileName) then
  begin
    raise Exception.Create('TgmCanvasSizeCommand.Rollback(): the file named in FDataFileName is not existed.');
  end;

  FPathList.DeselectAllPaths();
  FPathList.Clear();

  FChannelManager.AlphaChannelList.Clear();
  FChannelManager.DeleteLayerMaskChannel();
  FChannelManager.DeleteQuickMaskChannel();

  FLayerList.Clear();

  // read the data in...
  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;
    LGMDManager.TextEditor     := FTextEditor;

    if not LGMDManager.LoadFromFile(FDataFileName) then
    begin
      raise Exception.Create('TgmCanvasSizeCommand.Rollback(): error occurs in loading data named in FDataFileName.');
    end;
  finally
    LGMDManager.Free();
  end;

  for i := 0 to (FChannelManager.AlphaChannelList.Count - 1) do
  begin
    LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[i]);
    LAlphaChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.LayerMaskChannel) then
  begin
    FChannelManager.LayerMaskChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.QuickMaskChannel) then
  begin
    FChannelManager.QuickMaskChannel.IsChannelVisible := False;
  end;

  FLayerList.SelectLayer(FSelectedLayerIndex);
  FChannelManager.SelectColorChannel(0, True);

  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(
      FLayerList.SelectedLayer.LayerBitmap.Width,
      FLayerList.SelectedLayer.LayerBitmap.Height, LOffset);
  end;
end;

{ TgmCropCommand }

constructor TgmCropCommand.Create(AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; APathList: TgmPathList; ATextEditor: TRichEdit;
  ACrop: TgmCrop; const ABackColor: TColor32);
var
  LGMDManager : TgmGMDManager;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCropCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmCropCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmCropCommand.Create(): parameter APathList is a nil pointer.');
  end;

  if not Assigned(ATextEditor) then
  begin
    raise Exception.Create('TgmCropCommand.Create(): parameter ATextEditor is a nil pointer.');
  end;

  if not Assigned(ACrop) then
  begin
    raise Exception.Create('TgmCropCommand.Create(): parameter ACrop is a nil pointer.');
  end;

  if ALayerList.Count <= 0 then
  begin
    raise Exception.Create('TgmCropCommand.Create(): there was no any layers in the passed layer list.');
  end;

  inherited Create('Crop');

  FChannelManager     := AChannelManager;
  FLayerList          := ALayerList;
  FPathList           := APathList;
  FTextEditor         := ATextEditor;
  FSelectedLayerIndex := FLayerList.SelectedIndex;
  FBackgroundColor    := ABackColor;

  // saving the data of a crop tool
  FCropStart     := ACrop.FCropStart;
  FCropEnd       := ACrop.FCropEnd;
  FResizedWidth  := ACrop.ResizeW;
  FResizedHeight := ACrop.ResizeH;
  FResized       := ACrop.IsResized;

  // saving data
  FDataFileName := COMMAND_DATA_DIR + '\Crop' + IntToStr( GetTickCount() );

  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;

    LGMDManager.SaveToFile(FDataFileName);
  finally
    LGMDManager.Free();
  end;
end;

destructor TgmCropCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FDataFileName) then
  begin
    DeleteFile(PChar(FDataFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmCropCommand.Execute;
var
  LCrop      : TgmCrop;
  LNewWidth  : Integer;
  LNewHeight : Integer;
  LOffset    : TPoint;
begin
  inherited;

  LCrop := TgmCrop.Create();
  try
    LCrop.FCropStart := FCropStart;
    LCrop.FCropEnd   := FCropEnd;
    LCrop.ResizeW    := FResizedWidth;
    LCrop.ResizeH    := FResizedHeight;
    LCrop.IsResized  := FResized;

    // crop layers ...
    FLayerList.CropLayers(LCrop, Self.FBackgroundColor);

    // Crop channels ...
    FChannelManager.CropChannels(LCrop);

    // dealing with paths
    if LCrop.IsResized then
    begin
      LNewWidth  := LCrop.ResizeW;
      LNewHeight := LCrop.ResizeH;
    end
    else
    begin
      LNewWidth  := LCrop.CropAreaWidth;
      LNewHeight := LCrop.CropAreaHeight;
    end;

    if FPathList.Count > 0 then
    begin
      LOffset.X := 0;
      LOffset.Y := 0;
      
      FPathList.UpdateAllPathThumbnails(LNewWidth, LNewHeight, LOffset);
    end;
    
  finally
    LCrop.Free();
  end;
end;

procedure TgmCropCommand.Rollback;
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
  LGMDManager   : TgmGMDManager;
  LOffset       : TPoint;
begin
  inherited;

  if not FileExists(FDataFileName) then
  begin
    raise Exception.Create('TgmCropCommand.Rollback(): the file named in FDataFileName is not existed.');
  end;

  FPathList.DeselectAllPaths();
  FPathList.Clear();

  FChannelManager.AlphaChannelList.Clear();
  FChannelManager.DeleteLayerMaskChannel();
  FChannelManager.DeleteQuickMaskChannel();

  FLayerList.Clear();

  // read the data in...
  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;
    LGMDManager.TextEditor     := FTextEditor;

    if not LGMDManager.LoadFromFile(FDataFileName) then
    begin
      raise Exception.Create('TgmCropCommand.Rollback(): error occurs in loading data named in FDataFileName.');
    end;
  finally
    LGMDManager.Free();
  end;

  for i := 0 to (FChannelManager.AlphaChannelList.Count - 1) do
  begin
    LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[i]);
    LAlphaChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.LayerMaskChannel) then
  begin
    FChannelManager.LayerMaskChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.QuickMaskChannel) then
  begin
    FChannelManager.QuickMaskChannel.IsChannelVisible := False;
  end;

  FLayerList.SelectLayer(FSelectedLayerIndex);
  FChannelManager.SelectColorChannel(0, True);

  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(
      FLayerList.SelectedLayer.LayerBitmap.Width,
      FLayerList.SelectedLayer.LayerBitmap.Height, LOffset);
  end;
end;

{ TgmImageSizeCommand }

constructor TgmImageSizeCommand.Create(AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; APathList: TgmPathList; ATextEditor: TRichEdit;
  const ANewWidth, ANewHeight: Integer;
  const AResamplingOptions: TgmResamplingOptions);
var
  LGMDManager : TgmGMDManager;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmImageSizeCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmImageSizeCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmImageSizeCommand.Create(): parameter APathList is a nil pointer.');
  end;

  if not Assigned(ATextEditor) then
  begin
    raise Exception.Create('TgmImageSizeCommand.Create(): parameter ATextEditor is a nil pointer.');
  end;

  if ALayerList.Count <= 0 then
  begin
    raise Exception.Create('TgmImageSizeCommand.Create(): there was no any layers in the passed layer list.');
  end;

  if (ANewWidth <= 0) or (ANewHeight <= 0) then
  begin
    raise Exception.Create('TgmImageSizeCommand.Create(): both ANewWidth and ANewHeight should be greater than zero.');
  end;

  inherited Create('Image Size');

  FChannelManager     := AChannelManager;
  FLayerList          := ALayerList;
  FPathList           := APathList;
  FTextEditor         := ATextEditor;
  FSelectedLayerIndex := FLayerList.SelectedIndex;
  FNewWidth           := ANewWidth;
  FNewHeight          := ANewHeight;
  FResamplingOptions  := AResamplingOptions;

  // saving data
  FDataFileName := COMMAND_DATA_DIR + '\ImageSize' + IntToStr( GetTickCount() );

  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;

    LGMDManager.SaveToFile(FDataFileName);
  finally
    LGMDManager.Free();
  end;
end;

destructor TgmImageSizeCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FDataFileName) then
  begin
    DeleteFile(PChar(FDataFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmImageSizeCommand.Execute;
var
  LOffset : TPoint;
begin
  inherited;

  // resize layers
  FLayerList.ResizeImageOfLayers(FNewWidth, FNewHeight, FResamplingOptions);

  // change the size of channels
  FChannelManager.ResizeChannels(FNewWidth, FNewHeight);

  // dealing with paths
  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(FNewWidth, FNewHeight, LOffset);
  end;
end;

procedure TgmImageSizeCommand.Rollback;
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
  LGMDManager   : TgmGMDManager;
  LOffset       : TPoint;
begin
  inherited;

  if not FileExists(FDataFileName) then
  begin
    raise Exception.Create('TgmImageSizeCommand.Rollback(): the file named in FDataFileName is not existed.');
  end;

  FPathList.DeselectAllPaths();
  FPathList.Clear();

  FChannelManager.AlphaChannelList.Clear();
  FChannelManager.DeleteLayerMaskChannel();
  FChannelManager.DeleteQuickMaskChannel();

  FLayerList.Clear();

  // read the data in...
  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;
    LGMDManager.TextEditor     := FTextEditor;

    if not LGMDManager.LoadFromFile(FDataFileName) then
    begin
      raise Exception.Create('TgmImageSizeCommand.Rollback(): error occurs in loading data named in FDataFileName.');
    end;
  finally
    LGMDManager.Free();
  end;

  for i := 0 to (FChannelManager.AlphaChannelList.Count - 1) do
  begin
    LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[i]);
    LAlphaChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.LayerMaskChannel) then
  begin
    FChannelManager.LayerMaskChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.QuickMaskChannel) then
  begin
    FChannelManager.QuickMaskChannel.IsChannelVisible := False;
  end;

  FLayerList.SelectLayer(FSelectedLayerIndex);
  FChannelManager.SelectColorChannel(0, True);

  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(
      FLayerList.SelectedLayer.LayerBitmap.Width,
      FLayerList.SelectedLayer.LayerBitmap.Height, LOffset);
  end;
end;

{ TgmLayerMergenceCommand }

constructor TgmLayerMergenceCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ALayerListBeforeMergence: TgmLayerList;
  ATextEditor: TRichEdit; const AMergeMode: TgmMergeMode);
var
  i             : Integer;
  LIndex        : Integer;
  LCommandName  : string;
  LLayer        : TgmCustomLayer;
  LDataStream   : TMemoryStream;
  LLayersWriter : TgmLayersWriter;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmLayerMergenceCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(ALayerListBeforeMergence) then
  begin
    raise Exception.Create('TgmLayerMergenceCommand.Create(): parameter ALayerListBeforeMergence is a nil pointer.');
  end;

  if not Assigned(ATextEditor) then
  begin
    raise Exception.Create('TgmLayerMergenceCommand.Create(): parameter ATextEditor is a nil pointer.');
  end;

  if ALayerListBeforeMergence.Count <= 0 then
  begin
    raise Exception.Create('TgmLayerMergenceCommand.Create(): there is no any layers in the passed layer list.');
  end;

  case AMergeMode of
    mmFlatten:
      begin
        LCommandName := 'Flatten Image';
      end;

    mmMergeDown:
      begin
        LCommandName := 'Merge Down';
      end;

    mmMergeVisible:
      begin
        if not ALayerListBeforeMergence.CanMergeVisbleLayers() then
        begin
          raise Exception.Create('TgmLayerMergenceCommand.Create(): there was no any visible layers in the passed layer list.');
        end;

        LCommandName := 'Merge Visible';
      end;
  end;

  inherited Create(LCommandName);

  FChannelManager := AChannelManager;
  FLayerList      := ALayerListBeforeMergence;
  FLayerCount     := FLayerList.Count;
  FSelectedIndex  := FLayerList.SelectedIndex;
  FLayerWidth     := FLayerList.SelectedLayer.LayerBitmap.Width;
  FLayerHeight    := FLayerList.SelectedLayer.LayerBitmap.Height;
  FMergeMode      := AMergeMode;
  FTextEditor     := ATextEditor;
  FLayerFileName  := COMMAND_DATA_DIR + '\LayerMergence' + IntToStr( GetTickCount() );

  // save data of layers to disk
  LDataStream := TMemoryStream.Create();
  try
    LLayersWriter := TgmLayersWriter.Create(FLayerList);
    try
      LLayersWriter.SaveToStream(LDataStream);

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FLayerFileName);  
    finally
      LLayersWriter.Free();
    end;
  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;

  // save the index of visible layers in the list
  for i := 0 to (FLayerList.Count - 1) do
  begin
    LLayer := FLayerList.Layers[i];

    if LLayer.IsLayerVisible then
    begin
      SetLength( FVisibleLayerIndexArray, Length(FVisibleLayerIndexArray) + 1 );

      LIndex                          := High(FVisibleLayerIndexArray);
      FVisibleLayerIndexArray[LIndex] := i;
    end;
  end;  
end;

destructor TgmLayerMergenceCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FLayerFileName) then
  begin
    DeleteFile(PChar(FLayerFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  SetLength(FVisibleLayerIndexArray, 0);
  FVisibleLayerIndexArray := nil;

  inherited;
end;

procedure TgmLayerMergenceCommand.Execute;
var
  i           : Integer;
  LArrayCount : Integer;
  LLayerIndex : Integer;
  LLayer      : TgmCustomLayer;
begin
  inherited;

  case FMergeMode of
    mmFlatten:
      begin
        FLayerList.FlattenLayers();
      end;

    mmMergeDown:
      begin
        if not FLayerList.IsValidIndex(FSelectedIndex) then
        begin
          raise Exception.Create('TgmLayerMergenceCommand.Execute(): the FSelectedIndex is out of the range of the layer list.');
        end;

        FLayerList.SelectLayer(FSelectedIndex);
        FLayerList.SelectedLayer.SimplySetLayerVisible(True);
        FLayerList.MergeSelectedLayerDown();
      end;

    mmMergeVisible:
      begin
        LArrayCount := Length(FVisibleLayerIndexArray);

        for i := 0 to (LArrayCount - 1) do
        begin
          LLayerIndex := FVisibleLayerIndexArray[i];

          if not FLayerList.IsValidIndex(LLayerIndex) then
          begin
            raise Exception.Create('TgmLayerMergenceCommand.Execute(): the LLayerIndex is out of the range of the layer list.');
          end;

          LLayer := FLayerList.Layers[LLayerIndex];

          if not LLayer.IsLayerVisible then
          begin
            LLayer.SimplySetLayerVisible(True);
          end;
        end;

        if not FLayerList.IsValidIndex(FSelectedIndex) then
        begin
          raise Exception.Create('TgmLayerMergenceCommand.Execute(): the FSelectedIndex is out of the range of the layer list.');
        end;

        FLayerList.SelectLayer(FSelectedIndex);
        FLayerList.SelectedLayer.SimplySetLayerVisible(True);
        FLayerList.MergeVisibleLayers();
      end;
  end;

  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmLayerMergenceCommand.Rollback;
var
  LDataStream : TMemoryStream;
begin
  inherited;

  if not FileExists(FLayerFileName) then
  begin
    raise Exception.Create('TgmLayerMergenceCommand.Rollback(): the file named in FLayerFileName is not existed.');
  end;

  FLayerList.Clear();

  // load layer data back in
  LDataStream := TMemoryStream.Create();
  try
    LDataStream.LoadFromFile(FLayerFileName);
    LDataStream.Position := 0;

    if LDataStream.Size <= 0 then
    begin
      raise Exception.Create('TgmLayerMergenceCommand.Rollback(): there was no any layer data to load back in.');
    end;

    gmLayerIO.LoadLayersFromStream(LDataStream, FLayerList, FTextEditor,
      gmLayerIO.LAYER_FILE_VER, FLayerCount, FLayerWidth, FLayerHeight)
  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;

  FLayerList.SelectLayer(FSelectedIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmRectCropCommand }

constructor TgmRectCropCommand.Create(const ACommandName: string;
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  APathList: TgmPathList; ATextEditor: TRichEdit; const ACropRect: TRect;
  const ABackColor: TColor32);
var
  LGMDManager : TgmGMDManager;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmRectCropCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmRectCropCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmRectCropCommand.Create(): parameter APathList is a nil pointer.');
  end;

  if not Assigned(ATextEditor) then
  begin
    raise Exception.Create('TgmRectCropCommand.Create(): parameter ATextEditor is a nil pointer.');
  end;

  if ALayerList.Count <= 0 then
  begin
    raise Exception.Create('TgmRectCropCommand.Create(): there was no any layers in the passed layer list.');
  end;

  if (ACropRect.Left = ACropRect.Right) or
     (ACropRect.Top = ACropRect.Bottom) then
  begin
    raise Exception.Create('TgmRectCropCommand.Create(): nothing be covered by the rect in parameter ARectCrop.');
  end;

  inherited Create(ACommandName);

  FChannelManager     := AChannelManager;
  FLayerList          := ALayerList;
  FPathList           := APathList;
  FTextEditor         := ATextEditor;
  FCropRect           := ACropRect;
  FSelectedLayerIndex := FLayerlist.SelectedIndex;
  FBackgroundColor    := ABackColor;
  
  // saving data
  FDataFileName := COMMAND_DATA_DIR + '\RectCrop' + IntToStr( GetTickCount() );

  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;

    LGMDManager.SaveToFile(FDataFileName);
  finally
    LGMDManager.Free();
  end;
end;

destructor TgmRectCropCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FDataFileName) then
  begin
    DeleteFile(PChar(FDataFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmRectCropCommand.Execute;
var
  LOffset : TPoint;
begin
  inherited;

  // crop layers ...
  FLayerList.CropLayers(FCropRect, FBackgroundColor);

  // Crop channels ...
  FChannelManager.CropChannels(FCropRect);

  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(
      FLayerList.SelectedLayer.LayerBitmap.Width,
      FLayerList.SelectedLayer.LayerBitmap.Height, LOffset);
  end;
end;

procedure TgmRectCropCommand.Rollback;
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
  LGMDManager   : TgmGMDManager;
  LOffset       : TPoint;
begin
  inherited;

  if not FileExists(FDataFileName) then
  begin
    raise Exception.Create('TgmRectCropCommand.Rollback(): the file named in FDataFileName is not existed.');
  end;

  FPathList.DeselectAllPaths();
  FPathList.Clear();

  FChannelManager.AlphaChannelList.Clear();
  FChannelManager.DeleteLayerMaskChannel();
  FChannelManager.DeleteQuickMaskChannel();

  FLayerList.Clear();

  // read the data in...
  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;
    LGMDManager.TextEditor     := FTextEditor;

    if not LGMDManager.LoadFromFile(FDataFileName) then
    begin
      raise Exception.Create('TgmRectCropCommand.Rollback(): error occurs in loading data named in FDataFileName.');
    end;
  finally
    LGMDManager.Free();
  end;

  for i := 0 to (FChannelManager.AlphaChannelList.Count - 1) do
  begin
    LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[i]);
    LAlphaChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.LayerMaskChannel) then
  begin
    FChannelManager.LayerMaskChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.QuickMaskChannel) then
  begin
    FChannelManager.QuickMaskChannel.IsChannelVisible := False;
  end;

  FLayerList.SelectLayer(FSelectedLayerIndex);
  FChannelManager.SelectColorChannel(0, True);

  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(
      FLayerList.SelectedLayer.LayerBitmap.Width,
      FLayerList.SelectedLayer.LayerBitmap.Height, LOffset);
  end;
end;

{ TgmRotateCanvasCommand }

constructor TgmRotateCanvasCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  APathList: TgmPathList; ATextEditor: TRichEdit; const ARotateDegrees: Integer;
  const ARotateDirection: TgmRotateDirection; const ABackgroundColor: TColor32);
var
  LGMDManager : TgmGMDManager;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmRotateCanvasCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmRotateCanvasCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmRotateCanvasCommand.Create(): parameter APathList is a nil pointer.');
  end;

  if not Assigned(ATextEditor) then
  begin
    raise Exception.Create('TgmRotateCanvasCommand.Create(): parameter ATextEditor is a nil pointer.');
  end;

  if ALayerList.Count <= 0 then
  begin
    raise Exception.Create('TgmRotateCanvasCommand.Create(): there was no any layers in the passed layer list.');
  end;

  inherited Create('Rotate Canvas');

  FChannelManager     := AChannelManager;
  FLayerList          := ALayerList;
  FPathList           := APathList;
  FTextEditor         := ATextEditor;
  FSelectedLayerIndex := FLayerList.SelectedIndex;
  FRotateDegrees      := ARotateDegrees;
  FRotateDirection    := ARotateDirection;
  FBackgroundColor    := ABackgroundColor;

  // saving data
  FDataFileName := COMMAND_DATA_DIR + '\RotateCanvas' + IntToStr( GetTickCount() );

  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;

    LGMDManager.SaveToFile(FDataFileName);
  finally
    LGMDManager.Free();
  end;
end;

destructor TgmRotateCanvasCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FDataFileName) then
  begin
    DeleteFile(PChar(FDataFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmRotateCanvasCommand.Execute;
var
  LNewWidth  : Integer;
  LNewHeight : Integer;
  LOffset    : TPoint;
  LLayer     : TgmCustomLayer;
begin
  inherited;

  // rotate layers
  FLayerList.RotateCanvasOfLayers(FRotateDegrees, FRotateDirection, FBackgroundColor);

  // get layer width and height
  LLayer     := FLayerList.Layers[0];
  LNewWidth  := LLayer.LayerBitmap.Width;
  LNewHeight := LLayer.LayerBitmap.Height;

  // rotate channels
  FChannelManager.RotateChannels(FRotateDegrees, FRotateDirection);

  // dealing with paths
  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(LNewWidth, LNewHeight, LOffset);
  end;
end;

procedure TgmRotateCanvasCommand.Rollback;
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
  LGMDManager   : TgmGMDManager;
  LOffset       : TPoint;
begin
  inherited;

  if not FileExists(FDataFileName) then
  begin
    raise Exception.Create('TgmRotateCanvasCommand.Rollback(): the file named in FDataFileName is not existed.');
  end;

  FPathList.DeselectAllPaths();
  FPathList.Clear();

  FChannelManager.AlphaChannelList.Clear();
  FChannelManager.DeleteLayerMaskChannel();
  FChannelManager.DeleteQuickMaskChannel();

  FLayerList.Clear();

  // read the data in...
  LGMDManager := TgmGMDManager.Create();
  try
    // link pointers to the gmd manager
    LGMDManager.LayerList      := FLayerList;
    LGMDManager.ChannelManager := FChannelManager;
    LGMDManager.PathList       := FPathList;
    LGMDManager.TextEditor     := FTextEditor;

    if not LGMDManager.LoadFromFile(FDataFileName) then
    begin
      raise Exception.Create('TgmRotateCanvasCommand.Rollback(): error occurs in loading data named in FDataFileName.');
    end;
  finally
    LGMDManager.Free();
  end;

  for i := 0 to (FChannelManager.AlphaChannelList.Count - 1) do
  begin
    LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[i]);
    LAlphaChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.LayerMaskChannel) then
  begin
    FChannelManager.LayerMaskChannel.IsChannelVisible := False;
  end;

  if Assigned(FChannelManager.QuickMaskChannel) then
  begin
    FChannelManager.QuickMaskChannel.IsChannelVisible := False;
  end;

  FLayerList.SelectLayer(FSelectedLayerIndex);
  FChannelManager.SelectColorChannel(0, True);

  if FPathList.Count > 0 then
  begin
    LOffset.X := 0;
    LOffset.Y := 0;

    FPathList.UpdateAllPathThumbnails(
      FLayerList.SelectedLayer.LayerBitmap.Width,
      FLayerList.SelectedLayer.LayerBitmap.Height, LOffset);
  end;
end;

end.
