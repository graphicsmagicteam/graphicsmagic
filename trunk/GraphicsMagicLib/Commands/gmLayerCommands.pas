unit gmLayerCommands;

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

// Update Date: 2016/09/24

interface

uses
{ Delphi }
  Windows, SysUtils, Classes,
{ Graphics32 }
  GR32,
{ GraphicsMagicLib }
  gmChannelManager,
  gmHistoryCommands,
  gmLayers;

type
  { TgmLayerImageProcessCommand }
  
  TgmLayerImageProcessCommand = class(TgmImageProcessCommand)
  private
    FLayerList        : TgmLayerList;
    FTargetLayerIndex : Integer;
  public
    constructor Create(const ACommandName: string;
      const AUndoBmp, ARedoBmp: TBitmap32;
      const ALayerList: TgmLayerList;
      const ATargetLayerIndex: Integer);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmLayerMaskProcessCommand }

  TgmLayerMaskProcessCommand = class(TgmByteMapProcessCommand)
  private
    FLayerList        : TgmLayerList;
    FTargetLayerIndex : Integer;
  public
    constructor Create(const ACommandName: string;
      const AUndoBmp, ARedoBmp: TBitmap32;
      const ALayerList: TgmLayerList;
      const ATargetLayerIndex: Integer);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeleteLayerCommand }

  TgmDeleteLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList         : TgmLayerList;             // pointer to an external layer list
    FDeletedLayerIndex : Integer;
    FDeletedLayer      : TgmCustomLayer;
    FDeletedLayerName  : string;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; ADeletedLayer: TgmCustomLayer;
      const ADeletedLayerIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmNewLayerCommand }

  TgmNewLayerCommand = class(TgmCustomCommand)
  private
    FLayerList     : TgmLayerList;   // pointer to an external layer list
    FNewLayerIndex : Integer;
    FNewLayer      : TgmCustomLayer;
    FNewLayerName  : string;
  public
    constructor Create(ALayerList: TgmLayerList; ANewLayer: TgmCustomLayer;
      const ANewLayerIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmAddLayerMaskCommand }

  TgmAddLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager; // pointer to an external channel manager
    FLayerList      : TgmLayerList;            // pointer to an external layer list
    FLayerIndex     : Integer;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmApplyLayerMaskCommand }

  TgmApplyLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager   : TgmCustomChannelManager;
    FLayerList        : TgmLayerList;
    FLayerIndex       : Integer;
    FLayerBmpFileName : string;    // save the layer that before mask applied to disk with this filename
    FMaskFileName     : string;    // save the mask to disk with this filename 
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeleteLayerMaskCommand }

  TgmDeleteLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager   : TgmCustomChannelManager;
    FLayerList        : TgmLayerList;
    FLayerIndex       : Integer;
    FMaskFileName     : string;    // save the mask to disk with this filename 
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmLayerOrderCommmand }

  TgmLayerOrderCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager; // pointer to an external channel manager
    FLayerList      : TgmLayerList;            // pointer to an external layer list
    FOldIndex       : Integer;
    FNewIndex       : Integer;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
      const AOldIndex, ANewIndex: Integer);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmLayerPropertiesCommand }

  TgmLayerPropertiesCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager; // pointer to an external channel manager
    FLayerList      : TgmLayerList;            // pointer to an external layer list
    FLayerIndex     : Integer;
    FOldLayerName   : ShortString;
    FNewLayerName   : ShortString;
    FRenamedBefore  : Boolean;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      const AOldLayerName, ANewLayerName: ShortString;
      const ARenamedBefore: Boolean);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDuplicateLayerCommand }

  TgmDuplicateLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager      : TgmCustomChannelManager; // pointer to an external channel manager
    FLayerList           : TgmLayerList;            // pointer to an external layer list
    FDuplicatedLayer     : TgmCustomLayer;
    FDuplicatedLayerName : string;
    FTargetLayerIndex    : Integer;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; ADuplicatedLayer: TgmCustomLayer;
      const ATargetLayerIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;


implementation

uses
{ Graphics32 }
  GR32_OrdinalMaps;


{ TgmAddLayerMaskCommand }

constructor TgmAddLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmAddLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmAddLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmAddLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  inherited Create('Add Layer Mask');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;
end;

procedure TgmAddLayerMaskCommand.Execute;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  LLayer := FLayerList.Layers[FLayerIndex];
  LLayer.SimplyEnableMask();
  
  FLayerList.SimplySelectLayer(FLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmAddLayerMaskCommand.Rollback;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  LLayer := FLayerList.Layers[FLayerIndex];
  LLayer.SimplyDiscardMask();
  
  FLayerList.SimplySelectLayer(FLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmApplyLayerMaskCommand }

constructor TgmApplyLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer);
var
  i             : Integer;
  LLayer        : TgmCustomLayer;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LDataStream   : TMemoryStream;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmApplyLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmApplyLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmApplyLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not (LLayer is TgmNormalLayer) then
  begin
    raise Exception.Create('TgmApplyLayerMaskCommand.Create(): error, must be a Normal layer.');
  end;

  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmApplyLayerMaskCommand.Create(): the layer has no mask.');
  end; 

  inherited Create('Apply Layer Mask');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  // Storing the layer bitmap and its mask to disk for saving the system memory.
  // We save the bitmap to a memory stream first, and save the stream to
  // a file that with a random generated filename without a file extension.

  LRandomString     := IntToStr( GetTickCount() );
  FLayerBmpFileName := COMMAND_DATA_DIR + '\ApplyMask_LayerPixel' + LRandomString;
  FMaskFileName     := COMMAND_DATA_DIR + '\ApplyMask_MaskPixel' + LRandomString;

  LDataStream := TMemoryStream.Create();
  try
    LLayer.LayerBitmap.SaveToStream(LDataStream);
    LDataStream.Position := 0;
    LDataStream.SaveToFile(FLayerBmpFileName);
    
    LDataStream.Clear();

    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(LLayer.MaskBitmap.Width, LLayer.MaskBitmap.Height);
      LByteMap.ReadFrom(LLayer.MaskBitmap, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
      // save byte map to a stream
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Write(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FMaskFileName);
    finally
      LByteMap.Free();
    end;

  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmApplyLayerMaskCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FLayerBmpFileName) then
  begin
    DeleteFile(PChar(FLayerBmpFileName));
  end;

  if FileExists(FMaskFileName) then
  begin
    DeleteFile(PChar(FMaskFileName));
  end;
{$WARN UNSAFE_TYPE ON}
  
  inherited;
end;

procedure TgmApplyLayerMaskCommand.Execute;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  LLayer := FLayerList.Layers[FLayerIndex];

  if not (LLayer is TgmNormalLayer) then
  begin
    raise Exception.Create('TgmApplyLayerMaskCommand.Execute -- The target layer is not a Normal layer.');
  end;

  TgmNormalLayer(LLayer).SimplyApplyMask();
  FLayerList.SimplySelectLayer(FLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmApplyLayerMaskCommand.Rollback;
var
  i           : Integer;
  LLayer      : TgmCustomLayer;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
  LDataStream : TMemoryStream;
begin
  inherited;

  LLayer := FLayerList.Layers[FLayerIndex];

  if not (LLayer is TgmNormalLayer) then
  begin
    raise Exception.Create('TgmApplyLayerMaskCommand.Rollback -- The target layer is not a Normal layer.');
  end;

  // restore pixels of the layer
  if FileExists(FLayerBmpFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      LDataStream.LoadFromFile(FLayerBmpFileName);
      LDataStream.Position := 0;

      LLayer.LayerBitmap.LoadFromStream(LDataStream);
      LLayer.UpdateLayerThumbnail();
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;

  // restore the mask of the layer
  if not LLayer.IsMaskEnabled then
  begin
    LLayer.SimplyEnableMask();
  end;

  if FileExists(FMaskFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      LDataStream.LoadFromFile(FMaskFileName);
      LDataStream.Position := 0;

      // read in Byte Map data
      LByteMap := TByteMap.Create();
      try
        LByteMap.SetSize(LLayer.MaskBitmap.Width, LLayer.MaskBitmap.Height);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LLayer.MaskBitmap, ctUniformRGB);
        LLayer.UpdateMaskThumbnail();
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;

  FLayerList.SimplySelectLayer(FLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmDeleteLayerCommand }

constructor TgmDeleteLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  ADeletedLayer: TgmCustomLayer; const ADeletedLayerIndex: Integer);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDeleteLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmDeleteLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ADeletedLayer) then
  begin
    raise Exception.Create('TgmDeleteLayerCommand.Create(): parameter ADeletedLayer is nil.');
  end;

  if not ALayerList.IsValidIndex(ADeletedLayerIndex) then
  begin
    raise Exception.Create('TgmDeleteLayerCommand.Create(): parameter ADeletedLayerIndex is out of range.');
  end;

  inherited Create('Delete Layer');

  FChannelManager    := AChannelManager;
  FLayerList         := ALayerList;
  FDeletedLayerIndex := ADeletedLayerIndex;
  FDeletedLayer      := ADeletedLayer.GetCopy();
  FDeletedLayerName  := ADeletedLayer.LayerName;
end;

destructor TgmDeleteLayerCommand.Destroy;
begin
  FDeletedLayer.Free();
  inherited;
end;

procedure TgmDeleteLayerCommand.Execute;
var
  LSelectIndex : Integer;
begin
  inherited;

  FLayerList.SimplyDeleteLayer(FDeletedLayerIndex);

  LSelectIndex := FDeletedLayerIndex - 1;
  if LSelectIndex < 0 then
  begin
    LSelectIndex := 0;
  end;
  
  FLayerList.SimplySelectLayer(LSelectIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmDeleteLayerCommand.Rollback;
var
  LDuplicatedLayer : TgmCustomLayer;
begin
  inherited;

  LDuplicatedLayer           := FDeletedLayer.GetCopy();
  LDuplicatedLayer.LayerName := Self.FDeletedLayerName;

  case LDuplicatedLayer.PixelFeature of
    lpfNormalPixelized:
      begin
        LDuplicatedLayer.UpdateLayerThumbnail();
      end;

    lpfSpecialPixelized:
      begin
        if LDuplicatedLayer.IsLogoThumbEnabled then
        begin
          LDuplicatedLayer.UpdateLogoThumbnail();
        end;

        if LDuplicatedLayer.IsLayerThumbEnabled then
        begin
          LDuplicatedLayer.UpdateLayerThumbnail();
        end;
      end;

    lpfNonPixelized:
      begin
        LDuplicatedLayer.UpdateLogoThumbnail();
      end;
  end;

  if LDuplicatedLayer.IsMaskEnabled then
  begin
    LDuplicatedLayer.UpdateMaskThumbnail();
  end;

  FLayerList.SimplyInsert(FDeletedLayerIndex, LDuplicatedLayer);
  FLayerList.SimplySelectLayer(FDeletedLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmDeleteLayerMaskCommand }

constructor TgmDeleteLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer);
var
  i             : Integer;
  LLayer        : TgmCustomLayer;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LDataStream   : TMemoryStream;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDeleteLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmDeleteLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmDeleteLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmDeleteLayerMaskCommand.Create(): the layer has no mask.');
  end; 

  inherited Create('Delete Layer Mask');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  LRandomString := IntToStr( GetTickCount() );
  FMaskFileName := COMMAND_DATA_DIR + '\DeleteMaskPixel' + LRandomString;

  LDataStream := TMemoryStream.Create();
  try
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(LLayer.MaskBitmap.Width, LLayer.MaskBitmap.Height);
      LByteMap.ReadFrom(LLayer.MaskBitmap, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
      // save byte map to a stream
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Write(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FMaskFileName);
    finally
      LByteMap.Free();
    end;

  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmDeleteLayerMaskCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FMaskFileName) then
  begin
    DeleteFile(PChar(FMaskFileName));
  end;
{$WARN UNSAFE_TYPE ON}
  
  inherited;
end;

procedure TgmDeleteLayerMaskCommand.Execute;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  LLayer := FLayerList.Layers[FLayerIndex];

  LLayer.SimplyDiscardMask();
  FLayerList.SimplySelectLayer(FLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmDeleteLayerMaskCommand.Rollback;
var
  i           : Integer;
  LLayer      : TgmCustomLayer;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
  LDataStream : TMemoryStream;
begin
  inherited;

  LLayer := FLayerList.Layers[FLayerIndex];

  // restore the mask of the layer
  if not LLayer.IsMaskEnabled then
  begin
    LLayer.SimplyEnableMask();
  end;

  if FileExists(FMaskFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      LDataStream.LoadFromFile(FMaskFileName);
      LDataStream.Position := 0;

      // read in Byte Map data
      LByteMap := TByteMap.Create();
      try
        LByteMap.SetSize(LLayer.MaskBitmap.Width, LLayer.MaskBitmap.Height);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LLayer.MaskBitmap, ctUniformRGB);
        LLayer.UpdateMaskThumbnail();
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;

  FLayerList.SimplySelectLayer(FLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmDuplicateLayerCommand }

constructor TgmDuplicateLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  ADuplicatedLayer: TgmCustomLayer; const ATargetLayerIndex: Integer);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDuplicateLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmDuplicateLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ADuplicatedLayer) then
  begin
    raise Exception.Create('TgmDuplicateLayerCommand.Create(): parameter ADuplicatedLayer is nil.');
  end;

  if not ALayerList.IsValidIndex(ATargetLayerIndex) then
  begin
    raise Exception.Create('TgmDuplicateLayerCommand.Create(): parameter ATargetLayerIndex is out of range.');
  end;

  inherited Create('Duplicate Layer');

  FChannelManager      := AChannelManager;
  FLayerList           := ALayerList;
  FDuplicatedLayer     := ADuplicatedLayer.GetCopy();
  FDuplicatedLayerName := ADuplicatedLayer.LayerName;
  FTargetLayerIndex    := ATargetLayerIndex;
end;

destructor TgmDuplicateLayerCommand.Destroy;
begin
  FDuplicatedLayer.Free();
  inherited;
end;

procedure TgmDuplicateLayerCommand.Execute;
var
  LDuplicatedLayer : TgmCustomLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FTargetLayerIndex) then
  begin
    raise Exception.Create('TgmDuplicateLayerCommand.Execute(): parameter FTargetLayerIndex is out of range.');
  end;

  LDuplicatedLayer           := FDuplicatedLayer.GetCopy();
  LDuplicatedLayer.LayerName := Self.FDuplicatedLayerName;

  case LDuplicatedLayer.PixelFeature of
    lpfNormalPixelized:
      begin
        LDuplicatedLayer.UpdateLayerThumbnail();
      end;

    lpfSpecialPixelized:
      begin
        LDuplicatedLayer.UpdateLogoThumbnail();
      end;

    lpfNonPixelized:
      begin
        LDuplicatedLayer.UpdateLogoThumbnail();
      end;
  end;

  if LDuplicatedLayer.IsMaskEnabled then
  begin
    LDuplicatedLayer.UpdateMaskThumbnail();
  end;

  FLayerList.SimplyInsert(FTargetLayerIndex + 1, LDuplicatedLayer);
  FLayerList.SimplySelectLayer(FTargetLayerIndex + 1);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmDuplicateLayerCommand.Rollback;
var
  LDuplicatedLayerIndex : Integer;
begin
  inherited;

  LDuplicatedLayerIndex := FTargetLayerIndex + 1;
  
  if not FLayerList.IsValidIndex(LDuplicatedLayerIndex ) then
  begin
    raise Exception.Create('TgmDuplicateLayerCommand.Rollback(): parameter LDuplicatedLayerIndex is out of range.');
  end;

  if not FLayerList.IsValidIndex(FTargetLayerIndex) then
  begin
    raise Exception.Create('TgmDuplicateLayerCommand.Rollback(): parameter FTargetLayerIndex is out of range.');
  end;
  
  FLayerList.SimplyDeleteLayer(LDuplicatedLayerIndex);
  FLayerList.SimplySelectLayer(FTargetLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmLayerImageProcessCommand }

constructor TgmLayerImageProcessCommand.Create(const ACommandName: string;
  const AUndoBmp, ARedoBmp: TBitmap32;  const ALayerList: TgmLayerList;
  const ATargetLayerIndex: Integer);
begin
  inherited Create(ACommandName, AUndoBmp, ARedoBmp);

  FLayerList        := ALayerList;
  FTargetLayerIndex := ATargetLayerIndex;
end;

procedure TgmLayerImageProcessCommand.Execute;
var
  LDataStream : TMemoryStream;
  LLayer      : TgmCustomLayer;
begin
  inherited;

  if Assigned(FLayerList) and FLayerList.IsValidIndex(FTargetLayerIndex) then
  begin
    LLayer := FLayerList.Layers[FTargetLayerIndex];

    if not (LLayer is TgmNormalLayer) then
    begin
      raise Exception.Create('TgmLayerImageProcessCommand.Execute -- The target layer is not a normal layer.');
    end;

    if FileExists(FRedoBmpFileName) then
    begin
      LDataStream := TMemoryStream.Create();
      try
        LDataStream.LoadFromFile(FRedoBmpFileName);
        LDataStream.Position := 0;

        LLayer.LayerBitmap.LoadFromStream(LDataStream);
        LLayer.UpdateLayerThumbnail();
      finally
        LDataStream.Clear();
        LDataStream.Free();
      end;
    end;
  end;
end;

procedure TgmLayerImageProcessCommand.Rollback;
var
  LDataStream : TMemoryStream;
  LLayer      : TgmCustomLayer;
begin
  inherited;

  if Assigned(FLayerList) and FLayerList.IsValidIndex(FTargetLayerIndex) then
  begin
    LLayer := FLayerList.Layers[FTargetLayerIndex];

    if not (LLayer is TgmNormalLayer) then
    begin
      raise Exception.Create('TgmLayerImageProcessCommand.Rollback -- The target layer is not a normal layer.');
    end;

    if FileExists(FUndoBmpFileName) then
    begin
      LDataStream := TMemoryStream.Create();
      try
        LDataStream.LoadFromFile(FUndoBmpFileName);
        LDataStream.Position := 0;

        LLayer.LayerBitmap.LoadFromStream(LDataStream);
        LLayer.UpdateLayerThumbnail();
      finally
        LDataStream.Clear();
        LDataStream.Free();
      end;
    end;
  end;
end;

{ TgmLayerMaskProcessCommand }

constructor TgmLayerMaskProcessCommand.Create(const ACommandName: string;
  const AUndoBmp, ARedoBmp: TBitmap32; const ALayerList: TgmLayerList;
  const ATargetLayerIndex: Integer);
begin
  inherited Create(ACommandName, AUndoBmp, ARedoBmp);

  FLayerList        := ALayerList;
  FTargetLayerIndex := ATargetLayerIndex;
end;

procedure TgmLayerMaskProcessCommand.Execute;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LLayer      : TgmCustomLayer;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
begin
  inherited;

  if Assigned(FLayerList) and FLayerList.IsValidIndex(FTargetLayerIndex) then
  begin
    LLayer := FLayerList.Layers[FTargetLayerIndex];

    if not LLayer.IsMaskEnabled then
    begin
      raise Exception.Create('TgmLayerMaskProcessCommand.Execute -- The target layer mask is not available.');
    end;

    if FileExists(FRedoMapFileName) and
       (FRedoMapWidth > 0) and
       (FRedoMapHeight > 0) then
    begin
      LDataStream := TMemoryStream.Create();
      try
        LDataStream.LoadFromFile(FRedoMapFileName);
        LDataStream.Position := 0;

        // read in Byte Map data
        LByteMap := TByteMap.Create();
        try
          LByteMap.SetSize(FRedoMapWidth, FRedoMapHeight);

{$WARN UNSAFE_CODE OFF}
          LByteBits := @LByteMap.Bits[0];
          for i := 0 to (LByteMap.Height - 1) do
          begin
            LDataStream.Read(LByteBits^, LByteMap.Width);
            Inc(LByteBits, LByteMap.Width);
          end;
{$WARN UNSAFE_CODE ON}

          // TODO: prevent from the error that the size of the Layer Mask is
          // different than the map.
          LByteMap.WriteTo(LLayer.MaskBitmap, ctUniformRGB);
          LLayer.UpdateMaskThumbnail();
        finally
          LByteMap.Free();
        end;
      finally
        LDataStream.Clear();
        LDataStream.Free();
      end;
    end;
  end;
end;

procedure TgmLayerMaskProcessCommand.Rollback;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LLayer      : TgmCustomLayer;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
begin
  inherited;

  if Assigned(FLayerList) and FLayerList.IsValidIndex(FTargetLayerIndex) then
  begin
    LLayer := FLayerList.Layers[FTargetLayerIndex];

    if not LLayer.IsMaskEnabled then
    begin
      raise Exception.Create('TgmLayerMaskProcessCommand.Execute -- The target layer mask is not available.');
    end;

    if FileExists(FUndoMapFileName) and
       (FUndoMapWidth > 0) and
       (FUndoMapHeight > 0) then
    begin
      LDataStream := TMemoryStream.Create();
      try
        LDataStream.LoadFromFile(FUndoMapFileName);
        LDataStream.Position := 0;

        // read in Byte Map data
        LByteMap := TByteMap.Create();
        try
          LByteMap.SetSize(FUndoMapWidth, FUndoMapHeight);

{$WARN UNSAFE_CODE OFF}
          LByteBits := @LByteMap.Bits[0];
          for i := 0 to (LByteMap.Height - 1) do
          begin
            LDataStream.Read(LByteBits^, LByteMap.Width);
            Inc(LByteBits, LByteMap.Width);
          end;
{$WARN UNSAFE_CODE ON}

          // TODO: prevent from the error that the size of the Layer Mask is
          // different than the map.
          LByteMap.WriteTo(LLayer.MaskBitmap, ctUniformRGB);
          LLayer.UpdateMaskThumbnail();
        finally
          LByteMap.Free();
        end;
      finally
        LDataStream.Clear();
        LDataStream.Free();
      end;
    end;
  end;
end;

{ TgmLayerOrderCommmand }

constructor TgmLayerOrderCommand.Create(const ACommandName: string;
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const AOldIndex, ANewIndex: Integer);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmLayerOrderCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmLayerOrderCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(AOldIndex) then
  begin
    raise Exception.Create('TgmLayerOrderCommand.Create(): parameter AOldIndex is out of range.');
  end;

  if not ALayerList.IsValidIndex(ANewIndex) then
  begin
    raise Exception.Create('TgmLayerOrderCommand.Create(): parameter ANewIndex is out of range.');
  end;

  if AOldIndex = ANewIndex then
  begin
    raise Exception.Create('TgmLayerOrderCommand.Create(): parameters AOldIndex and ANewIndex are identical.');
  end;

  inherited Create(ACommandName);

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FOldIndex       := AOldIndex;
  FNewIndex       := ANewIndex;
end;

procedure TgmLayerOrderCommand.Execute;
begin
  inherited;
  
  FLayerList.SimplyMove(FOldIndex, FNewIndex);
  FLayerList.SimplySelectLayer(FNewIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmLayerOrderCommand.Rollback;
begin
  inherited;
  
  FLayerList.SimplyMove(FNewIndex, FOldIndex);
  FLayerList.SimplySelectLayer(FOldIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmLayerPropertiesCommand }

constructor TgmLayerPropertiesCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; const AOldLayerName, ANewLayerName: ShortString;
  const ARenamedBefore: Boolean);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmLayerPropertiesCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmLayerPropertiesCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmLayerPropertiesCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  inherited Create('Layer Properties');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;            
  FLayerIndex     := ALayerIndex;
  FOldLayerName   := AOldLayerName;
  FNewLayerName   := ANewLayerName;
  FRenamedBefore  := ARenamedBefore;
end;

procedure TgmLayerPropertiesCommand.Execute;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmLayerPropertiesCommand.Create(): FLayerIndex is out of range.');
  end;

  LLayer           := FLayerList.Layers[FLayerIndex];
  LLayer.LayerName := FNewLayerName;
  LLayer.IsRenamed := True;

  FLayerList.SimplySelectLayer(FLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmLayerPropertiesCommand.Rollback;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmLayerPropertiesCommand.Create(): FLayerIndex is out of range.');
  end;

  LLayer           := FLayerList.Layers[FLayerIndex];
  LLayer.LayerName := FOldLayerName;
  LLayer.IsRenamed := FRenamedBefore;

  FLayerList.SimplySelectLayer(FLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmNewLayerCommand }

constructor TgmNewLayerCommand.Create(ALayerList: TgmLayerList;
  ANewLayer: TgmCustomLayer; const ANewLayerIndex: Integer);
var
  LCommandName : string;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmNewLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ANewLayer) then
  begin
    raise Exception.Create('TgmNewLayerCommand.Create(): parameter ANewLayer is nil.');
  end;

  if ANewLayerIndex < 0 then
  begin
    raise Exception.Create('TgmDeleteLayerCommand.Create(): parameter ANewLayerIndex is less than zero.');
  end;

  if ANewLayer is TgmNormalLayer then
  begin
    LCommandName := 'New Layer';
  end
  else
  begin
    LCommandName := 'New ' + ANewLayer.LayerName;
  end;
  
  inherited Create(LCommandName);

  FLayerList     := ALayerList;
  FNewLayerIndex := ANewLayerIndex;
  FNewLayer      := ANewLayer.GetCopy();
  FNewLayerName  := ANewLayer.LayerName;
end;

destructor TgmNewLayerCommand.Destroy;
begin
  FNewLayer.Free();
  inherited;
end;

procedure TgmNewLayerCommand.Execute;
var
  LDuplicatedLayer : TgmCustomLayer;
  LLayerTypeCount  : Integer;
begin
  inherited;

  LDuplicatedLayer           := FNewLayer.GetCopy();
  LDuplicatedLayer.LayerName := Self.FNewLayerName;
  LLayerTypeCount            := FLayerList.GetLayerCounter(FNewLayer.ClassName) + 1;

  case LDuplicatedLayer.PixelFeature of
    lpfNormalPixelized:
      begin
        LDuplicatedLayer.UpdateLayerThumbnail();
      end;

    lpfSpecialPixelized:
      begin
        LDuplicatedLayer.UpdateLogoThumbnail();
      end;

    lpfNonPixelized:
      begin
        LDuplicatedLayer.UpdateLogoThumbnail();
      end;
  end;

  if LDuplicatedLayer.IsMaskEnabled then
  begin
    LDuplicatedLayer.UpdateMaskThumbnail();
  end;
  
  FLayerList.SimplyInsert(FNewLayerIndex, LDuplicatedLayer);
  FLayerList.SetLayerCounter(LDuplicatedLayer.ClassName, LLayerTypeCount);
  FLayerList.SimplySelectLayer(FNewLayerIndex);
end;

procedure TgmNewLayerCommand.Rollback;
var
  LSelectIndex    : Integer;
  LLayerTypeCount : Integer;
begin
  inherited;

  FLayerList.SimplyDeleteLayer(FNewLayerIndex);

  LSelectIndex := FNewLayerIndex - 1;
  if LSelectIndex < 0 then
  begin
    LSelectIndex := 0;
  end;
  
  FLayerList.SimplySelectLayer(LSelectIndex);

  LLayerTypeCount := FLayerList.GetLayerCounter(FNewLayer.ClassName) - 1;
  FLayerList.SetLayerCounter(FNewLayer.ClassName, LLayerTypeCount);
end;

end.
