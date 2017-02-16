unit gmChannelCommands;

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

// Update Date: 2016/12/21

interface

uses
{ Delphi }
  Windows, SysUtils, Classes,
{ Graphics32 }
  GR32,
{ GraphicsMagicLib }
  gmChannels,
  gmChannelManager,
  gmHistoryCommands,
  gmLayers,
  gmSelection,
  gmSelectionCommands,
  gmTypes;

type
  { TgmAlphaChannelProcessCommand }

  TgmAlphaChannelProcessCommand = class(TgmByteMapProcessCommand)
  private
    FAlphaChannelList   : TgmAlphaChannelList;
    FTargetChannelIndex : Integer;
  public
    constructor Create(const ACommandName: string;
      const AUndoBmp, ARedoBmp: TBitmap32;
      const AAlphaChannelList: TgmAlphaChannelList;
      const ATargetChannelIndex: Integer);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmNewAlphaChannelCommand }

  TgmNewAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager; // pointer to an external channel manager
    FChannelWidth   : Integer;
    FChannelHeight  : Integer;
    FMaskColor      : TColor32;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AChannelWidth, AChannelHeight: Integer);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeleteAlphaChannelCommand }

  TgmDeleteAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager      : TgmCustomChannelManager;  // pointer to an external channel manager
    FDeletedChannelIndex : Integer;

    // data of the deleted alpha channel
    FChannelWidth        : Integer;
    FChannelHeight       : Integer;
    FMaskColor           : TColor32;
    FMaskColorIndicator  : TgmMaskColorIndicator;
    FMaskColorType       : TgmMaskColorType;
    FChannelName         : string;
    FChannelMapFileName  : string;  // save the data of the channel map with this file name for saving memory space
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const ADeletedChannelIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmRearrangeChannelsCommand }

  TgmRearrangeChannelsCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FOldIndex       : Integer;
    FNewIndex       : Integer;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AOldIndex, ANewIndex: Integer);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmAlphaChannelOptionsCommand }

  TgmChannelOptionsCommandData = record
    ChannelIndex      : Integer;

    OldChannelName    : ShortString;
    NewChannelName    : ShortString;

    OldMaskColor      : TColor32;
    NewMaskColor      : TColor32;

    OldOpacityPercent : Double;   // 0 to 1.0
    NewOpacityPercent : Double;   // 0 to 1.0

    OldColorIndicator : TgmMaskColorIndicator;
    NewColorIndicator : TgmMaskColorIndicator;
  end;

  TgmAlphaChannelOptionsCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FChannelOptions : TgmChannelOptionsCommandData;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AChannelOptionsData: TgmChannelOptionsCommandData);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDuplicateChannelCommand }

  TgmDuplicateChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FChannelIndex   : Integer;                  // the index of the duplicated channel

    // data of the duplicated alpha channel
    FChannelWidth        : Integer;
    FChannelHeight       : Integer;
    FMaskColor           : TColor32;
    FMaskColorIndicator  : TgmMaskColorIndicator;
    FMaskColorType       : TgmMaskColorType;
    FChannelName         : string;
    FChannelMapFileName  : string;  // save the data of the channel map with this file name for saving memory space
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const ADuplicatedChannelIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmEnterQuickMaskChannelCommand }

  TgmEnterQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager  : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList       : TgmLayerList;             // pointer to an external layer list
    FLayerIndex      : Integer;
    FOldSelection    : TgmSelection;             // a copy to the selection that before entering to quick mask channel
    FSetEditModeProc : TgmSetEditModeProc;       // an external procedure for setting the Edit Mode

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      AOldSelection: TgmSelection; ASetEditModeProc: TgmSetEditModeProc;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmQuickMaskChannelProcessCommand }

  TgmQuickMaskChannelProcessCommand = class(TgmByteMapProcessCommand)
  private
    FChannelManager : TgmCustomChannelManager;
  public
    constructor Create(const ACommandName: string;
      const AUndoBmp, ARedoBmp: TBitmap32;
      AChannelManager: TgmCustomChannelManager);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmExitFromQuickMaskChannelCommand }

  TgmExitFromQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager     : TgmCustomChannelManager;  // pointer to an external channel manager
    FOldSelection       : TgmSelection;             // a copy to the selection that before exiting from quick mask channel
    FNewSelection       : TgmSelection;             // a copy to the selection that after exited from quick mask channel
    FMaskColor          : TColor32;
    FMaskColorIndicator : TgmMaskColorIndicator;
    FMaskOpacity        : Byte;
    FMapFileName        : string;
    FMapWidth           : Integer;
    FMapHeight          : Integer;
    FSetEditModeProc    : TgmSetEditModeProc;       // an external procedure for setting the Edit Mode

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      AOldSelection, ANewSelection: TgmSelection;
      AOldQuickChannelMap: TBitmap32;
      const AQuickChannelMaskColor: TColor32;
      const AQuickChannelMaskColorIndicator: TgmMaskColorIndicator;
      const AQuickChannelMaskOpacity: Byte;
      ASetEditModeProc: TgmSetEditModeProc;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmQuickMaskChannelOptionsCommand }
  
  TgmQuickMaskChannelOptionsCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FChannelOptions : TgmChannelOptionsCommandData;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AChannelOptionsData: TgmChannelOptionsCommandData);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmLayerMaskChannelOptionsCommand }

  TgmLayerMaskChannelOptionsCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FLayerIndex     : Integer;
    FChannelOptions : TgmChannelOptionsCommandData;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      const AChannelOptionsData: TgmChannelOptionsCommandData);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSaveSelectionAsChannelCommand }

  TgmSaveSelectionAsChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager  : TgmCustomChannelManager;  // pointer to an external channel manager
    FChannelName     : string;
    FChannelIndex    : Integer;
    FChannelWidth    : Integer;
    FChannelHeight   : Integer;
    FChannelFileName : string;                   // for saving the pixels of the channel on disk
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      AChannel: TgmAlphaChannel; const AChannelIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;


implementation

uses
{ Graphics }
  GR32_OrdinalMaps,
{ GraphicsMagicLib }
  gmAlphaFuncs,
  gmImageProcessFuncs;

const
  CREATE_SELECTION_IF_NONE      : Boolean = True;
  DONT_CREATE_SELECTION_IF_NONE : Boolean = False;

{ TgmAlphaChannelOptionsCommand }

constructor TgmAlphaChannelOptionsCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const AChannelOptionsData: TgmChannelOptionsCommandData);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmAlphaChannelOptionsCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AChannelOptionsData.ChannelIndex) then
  begin
    raise Exception.Create('TgmAlphaChannelOptionsCommand.Create(): parameter AChannelOptionsData.ChannelIndex is out of range.');
  end;

  inherited Create('Channel Options');

  FChannelManager := AChannelManager;
  FChannelOptions := AChannelOptionsData;
end;

procedure TgmAlphaChannelOptionsCommand.Execute;
var
  LChannel : TgmAlphaChannel;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelOptions.ChannelIndex) then
  begin
    raise Exception.Create('TgmAlphaChannelOptionsCommand.Execute(): parameter FChannelOptions.ChannelIndex is out of range.');
  end;

  LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelOptions.ChannelIndex]);

  LChannel.ChannelLayer.Bitmap.BeginUpdate();
  try
    LChannel.ChannelName        := FChannelOptions.NewChannelName;
    LChannel.MaskColor          := FChannelOptions.NewMaskColor;
    LChannel.MaskOpacity        := Round(FChannelOptions.NewOpacityPercent * 255);
    LChannel.MaskColorIndicator := FChannelOptions.NewColorIndicator;
  finally
    LChannel.ChannelLayer.Bitmap.EndUpdate();
  end;
end;

procedure TgmAlphaChannelOptionsCommand.Rollback;
var
  LChannel : TgmAlphaChannel;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelOptions.ChannelIndex) then
  begin
    raise Exception.Create('TgmAlphaChannelOptionsCommand.Rollback(): parameter FChannelOptions.ChannelIndex is out of range.');
  end;

  LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelOptions.ChannelIndex]);

  LChannel.ChannelLayer.Bitmap.BeginUpdate();
  try
    LChannel.ChannelName        := FChannelOptions.OldChannelName;
    LChannel.MaskColor          := FChannelOptions.OldMaskColor;
    LChannel.MaskOpacity        := Round(FChannelOptions.OldOpacityPercent * 255);
    LChannel.MaskColorIndicator := FChannelOptions.OldColorIndicator;
  finally
    LChannel.ChannelLayer.Bitmap.EndUpdate();
  end;
end;


{ TgmAlphaChannelProcessCommand }

constructor TgmAlphaChannelProcessCommand.Create(
  const ACommandName: string;
  const AUndoBmp, ARedoBmp: TBitmap32;
  const AAlphaChannelList: TgmAlphaChannelList;
  const ATargetChannelIndex: Integer);
begin
  if not Assigned(AAlphaChannelList) then
  begin
    raise Exception.Create('TgmAlphaChannelCommand.Create(): parameter AAlphaChannelList is nil.');
  end;

  if not AAlphaChannelList.IsValidIndex(ATargetChannelIndex) then
  begin
    raise Exception.Create('TgmAlphaChannelCommand.Create(): parameter ATargetChannelIndex is out of range.');
  end;

  inherited Create(ACommandName, AUndoBmp, ARedoBmp);

  FAlphaChannelList   := AAlphaChannelList;
  FTargetChannelIndex := ATargetChannelIndex;
end;

procedure TgmAlphaChannelProcessCommand.Execute;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LChannel    : TgmAlphaChannel;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
begin
  if not Assigned(FAlphaChannelList) then
  begin
    raise Exception.Create('TgmAlphaChannelCommand.Execute(): FAlphaChannelList is invalid.');
  end;

  if not FAlphaChannelList.IsValidIndex(FTargetChannelIndex) then
  begin
    raise Exception.Create('TgmAlphaChannelCommand.Execute(): FTargetChannelIndex is invalid.');
  end;

  inherited;

  LChannel := TgmAlphaChannel(FAlphaChannelList.Channels[FTargetChannelIndex]);

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

        // TODO: prevent from the error that the size of the channel is
        // different than the map.
        LChannel.ChannelLayer.Bitmap.BeginUpdate();
        try
          LByteMap.WriteTo(LChannel.ChannelLayer.Bitmap, ctUniformRGB);
        finally
          LChannel.ChannelLayer.Bitmap.EndUpdate();
        end;
        
        LChannel.UpdateChannelThumbnail();
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

procedure TgmAlphaChannelProcessCommand.Rollback;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LChannel    : TgmAlphaChannel;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
begin
  if not Assigned(FAlphaChannelList) then
  begin
    raise Exception.Create('TgmAlphaChannelCommand.Rollback(): FAlphaChannelList is invalid.');
  end;

  if not FAlphaChannelList.IsValidIndex(FTargetChannelIndex) then
  begin
    raise Exception.Create('TgmAlphaChannelCommand.Rollback(): FTargetChannelIndex is invalid.');
  end;

  inherited;

  LChannel := TgmAlphaChannel(FAlphaChannelList.Channels[FTargetChannelIndex]);

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

        // TODO: prevent from the error that the size of the channel is
        // different than the map.
        LChannel.ChannelLayer.Bitmap.BeginUpdate();
        try
          LByteMap.WriteTo(LChannel.ChannelLayer.Bitmap, ctUniformRGB);
        finally
          LChannel.ChannelLayer.Bitmap.EndUpdate();
        end;
        
        LChannel.UpdateChannelThumbnail();
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

{ TgmDeleteAlphaChannelCommand }

constructor TgmDeleteAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const ADeletedChannelIndex: Integer);
var
  i             : Integer;
  LChannel      : TgmAlphaChannel;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LDataStream   : TMemoryStream;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDeleteAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(ADeletedChannelIndex) then
  begin
    raise Exception.Create('TgmDeleteAlphaChannelCommand.Create(): parameter ADeletedChannelIndex is out of range.');
  end;

  inherited Create('Delete Channel');

  FChannelManager      := AChannelManager;
  FDeletedChannelIndex := ADeletedChannelIndex;

  LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FDeletedChannelIndex]);

  FChannelName        := LChannel.ChannelName;
  FChannelWidth       := LChannel.ChannelLayer.Bitmap.Width;
  FChannelHeight      := LChannel.ChannelLayer.Bitmap.Height;
  FMaskColor          := LChannel.MaskColor;
  FMaskColorIndicator := LChannel.MaskColorIndicator;
  FMaskColorType      := LChannel.MaskColorType;

  LRandomString       := IntToStr( GetTickCount() );
  FChannelMapFileName := gmHistoryCommands.COMMAND_DATA_DIR + '\DeleteAlphaChannelPixel' + LRandomString;

  LDataStream := TMemoryStream.Create();
  try
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FChannelWidth, FChannelHeight);
      LByteMap.ReadFrom(LChannel.ChannelLayer.Bitmap, ctUniformRGB);

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
      LDataStream.SaveToFile(FChannelMapFileName);
    finally
      LByteMap.Free();
    end;

  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmDeleteAlphaChannelCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FChannelMapFileName) then
  begin
    DeleteFile(PChar(FChannelMapFileName));
  end;
{$WARN UNSAFE_TYPE ON}
  inherited;
end;

procedure TgmDeleteAlphaChannelCommand.Execute;
var
  LSelectedIndex : Integer;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FDeletedChannelIndex) then
  begin
    raise Exception.Create('TgmDeleteAlphaChannelCommand.Execute(): FDeletedChannelIndex is out of range.');
  end;

  LSelectedIndex := FChannelManager.AlphaChannelList.SelectedIndex;
  FChannelManager.SimplyDeleteAlphaChannel(FDeletedChannelIndex);
  
  if FDeletedChannelIndex = LSelectedIndex then
  begin
    FChannelManager.SelectColorChannel(0, True);
  end
  else if FDeletedChannelIndex < LSelectedIndex then
  begin
    LSelectedIndex := LSelectedIndex - 1;
    FChannelManager.SelectAlphaChannel(LSelectedIndex, False);
  end;
end;

procedure TgmDeleteAlphaChannelCommand.Rollback;
var
  i              : Integer;
  LSelectedIndex : Integer;
  LInsertedIndex : Integer;
  LChannel       : TgmAlphaChannel;
  LChannelMap    : TBitmap32;
  LByteMap       : TByteMap;
  LByteBits      : PByte;
  LDataStream    : TMemoryStream;
begin
  inherited;

  if FileExists(FChannelMapFileName) then
  begin
    LSelectedIndex := FChannelManager.AlphaChannelList.SelectedIndex;

    LDataStream := TMemoryStream.Create();
    try
      LDataStream.LoadFromFile(FChannelMapFileName);
      LDataStream.Position := 0;

      // read in Byte Map data
      LByteMap := TByteMap.Create();
      try
        LByteMap.SetSize(FChannelWidth, FChannelHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LChannelMap := TBitmap32.Create();
        try
          LChannelMap.SetSize(FChannelWidth, FChannelHeight);
          LByteMap.WriteTo(LChannelMap, ctUniformRGB);

          LInsertedIndex := FChannelManager.SimplyInsertNewAlphaChannel(
            FDeletedChannelIndex, LChannelMap, False);

          LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[LInsertedIndex]);

          LChannel.MaskColor          := FMaskColor;
          LChannel.MaskColorIndicator := FMaskColorIndicator;
          LChannel.MaskColorType      := FMaskColorType;
          LChannel.ChannelName        := FChannelName;
          LChannel.IsChannelVisible   := False;

          LChannel.UpdateChannelThumbnail();

          if LSelectedIndex >= LInsertedIndex then
          begin
            LSelectedIndex := LSelectedIndex + 1;

            if FChannelManager.AlphaChannelList.IsValidIndex(LSelectedIndex) then
            begin
              FChannelManager.SelectAlphaChannel(LSelectedIndex, False);
            end;
          end;
        finally
          LChannelMap.Free();
        end;

      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

{ TgmDuplicateChannelCommand }

constructor TgmDuplicateChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const ADuplicatedChannelIndex: Integer);
var
  i             : Integer;
  LChannel      : TgmAlphaChannel;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LDataStream   : TMemoryStream;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDuplicateChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(ADuplicatedChannelIndex) then
  begin
    raise Exception.Create('TgmDuplicateChannelCommand.Create(): parameter ADuplicatedChannelIndex is out of range.');
  end;

  inherited Create('Duplicate Channel');

  FChannelManager := AChannelManager;
  FChannelIndex   := ADuplicatedChannelIndex;

  LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelIndex]);

  FChannelName        := LChannel.ChannelName;
  FChannelWidth       := LChannel.ChannelLayer.Bitmap.Width;
  FChannelHeight      := LChannel.ChannelLayer.Bitmap.Height;
  FMaskColor          := LChannel.MaskColor;
  FMaskColorIndicator := LChannel.MaskColorIndicator;
  FMaskColorType      := LChannel.MaskColorType;

  LRandomString       := IntToStr( GetTickCount() );
  FChannelMapFileName := gmHistoryCommands.COMMAND_DATA_DIR + '\DuplicatedAlphaChannelPixel' + LRandomString;

  LDataStream := TMemoryStream.Create();
  try
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FChannelWidth, FChannelHeight);
      LByteMap.ReadFrom(LChannel.ChannelLayer.Bitmap, ctUniformRGB);

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
      LDataStream.SaveToFile(FChannelMapFileName);
    finally
      LByteMap.Free();
    end;

  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmDuplicateChannelCommand.Destroy;
begin
  {$WARN UNSAFE_TYPE OFF}
  if FileExists(FChannelMapFileName) then
  begin
    DeleteFile(PChar(FChannelMapFileName));
  end;
{$WARN UNSAFE_TYPE ON}
  inherited;
end;

procedure TgmDuplicateChannelCommand.Execute;
var
  i              : Integer;
  LInsertedIndex : Integer;
  LChannel       : TgmAlphaChannel;
  LChannelMap    : TBitmap32;
  LByteMap       : TByteMap;
  LByteBits      : PByte;
  LDataStream    : TMemoryStream;
begin
  inherited;

  if FileExists(FChannelMapFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      LDataStream.LoadFromFile(FChannelMapFileName);
      LDataStream.Position := 0;

      // read in Byte Map data
      LByteMap := TByteMap.Create();
      try
        LByteMap.SetSize(FChannelWidth, FChannelHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LChannelMap := TBitmap32.Create();
        try
          LChannelMap.SetSize(FChannelWidth, FChannelHeight);
          LByteMap.WriteTo(LChannelMap, ctUniformRGB);

          LInsertedIndex := FChannelManager.SimplyInsertNewAlphaChannel(
            FChannelIndex, LChannelMap, False);

          LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[LInsertedIndex]);

          LChannel.MaskColor          := FMaskColor;
          LChannel.MaskColorIndicator := FMaskColorIndicator;
          LChannel.MaskColorType      := FMaskColorType;
          LChannel.ChannelName        := FChannelName;
          LChannel.IsChannelVisible   := False;

          LChannel.UpdateChannelThumbnail();
        finally
          LChannelMap.Free();
        end;

      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

procedure TgmDuplicateChannelCommand.Rollback;
var
  LSelectedIndex : Integer;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmDuplicateChannelCommand.Execute(): FChannelIndex is out of range.');
  end;

  LSelectedIndex := FChannelManager.AlphaChannelList.SelectedIndex;
  FChannelManager.SimplyDeleteAlphaChannel(FChannelIndex);
  
  if FChannelIndex = LSelectedIndex then
  begin
    FChannelManager.SelectColorChannel(0, True);
  end;
end;

{ TgmEnterQuickMaskChannelCommand }

constructor TgmEnterQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; AOldSelection: TgmSelection;
  ASetEditModeProc: TgmSetEditModeProc;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmEnterQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmEnterQuickMaskChannelCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmEnterQuickMaskChannelCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(ASetEditModeProc) then
  begin
    raise Exception.Create('TgmEnterQuickMaskChannelCommand.Create(): parameter ASetEditModeProc is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmEnterQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmEnterQuickMaskChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Enter Quick Mask');

  FChannelManager              := AChannelManager;
  FLayerList                   := ALayerList;
  FLayerIndex                  := ALayerIndex;
  FSetEditModeProc             := ASetEditModeProc;
  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;
end;

destructor TgmEnterQuickMaskChannelCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;

  inherited;
end;

procedure TgmEnterQuickMaskChannelCommand.Execute;
var
  LMaskBmp : TBitmap32;
  LLayer   : TgmCustomLayer;
begin
  inherited;

  FSetEditModeProc(emQuickMaskMode);

  LMaskBmp := nil;
  
  if Assigned(FOldSelection) then
  begin
    // make a copy of the original mask of the selection for later use
    LMaskBmp := TBitmap32.Create();
    LMaskBmp.SetSizeFrom(FOldSelection.OriginalMask);

    LMaskBmp.Clear(clBlack32);

    LMaskBmp.Draw(FOldSelection.MaskBorderStart.X,
                  FOldSelection.MaskBorderStart.Y,
                  FOldSelection.ResizedMask);

    ReplaceAlphaChannelWithNewValue(LMaskBmp, 255);

    // remove the external selection
    FDeleteExternalSelectionProc();
  end;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    LLayer := FLayerList.Layers[FLayerIndex];

    FChannelManager.CreateQuickMaskChannel(
      LLayer.LayerBitmap.Width, LLayer.LayerBitmap.Height);
  end;

  if Assigned(LMaskBmp) then
  begin
    with FChannelManager.QuickMaskChannel do
    begin
      CopyBitmap32(ChannelLayer.Bitmap, LMaskBmp);
      ChannelLayer.Bitmap.Changed();
      UpdateChannelThumbnail();
    end;

    LMaskBmp.Free();
  end;
end;

procedure TgmEnterQuickMaskChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  FSetEditModeProc(emStandardMode);
  FChannelManager.DeleteQuickMaskChannel();

  // switching to color channel
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SimplySelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  if Assigned(FOldSelection) then
  begin
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end;
end;

{ TgmExitFromQuickMaskChannelCommand }

constructor TgmExitFromQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  AOldSelection, ANewSelection: TgmSelection;
  AOldQuickChannelMap: TBitmap32;
  const AQuickChannelMaskColor: TColor32;
  const AQuickChannelMaskColorIndicator: TgmMaskColorIndicator;
  const AQuickChannelMaskOpacity: Byte;
  ASetEditModeProc: TgmSetEditModeProc;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
var
  i             : Integer;
  LDataStream   : TMemoryStream;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmExitFromQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASetEditModeProc) then
  begin
    raise Exception.Create('TgmExitFromQuickMaskChannelCommand.Create(): parameter ASetEditModeProc is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmExitFromQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmExitFromQuickMaskChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  if not Assigned(AOldQuickChannelMap) then
  begin
    raise Exception.Create('TgmExitFromQuickMaskChannelCommand.Create(): parameter AOldQuickChannelMap is nil.');
  end;

  if (AOldQuickChannelMap.Width <= 0) or (AOldQuickChannelMap.Height <=0) then
  begin
    raise Exception.Create('TgmExitFromQuickMaskChannelCommand.Create(): the width or height of AOldQuickChannelMap is zero.');
  end;

  inherited Create('Exit Quick Mask');

  FChannelManager              := AChannelManager;
  FMaskColor                   := AQuickChannelMaskColor;
  FMaskColorIndicator          := AQuickChannelMaskColorIndicator;
  FMaskOpacity                 := AQuickChannelMaskOpacity;
  FSetEditModeProc             := ASetEditModeProc;
  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := nil;
  if Assigned(ANewSelection) then
  begin
    FNewSelection := TgmSelection.Create();
    FNewSelection.AssignAllSelectionData(ANewSelection);
  end;

  // Storing the the Quick Mask map to disk for saving the system memory.
  // We convert the bitmap to Byte Map first and then save the Byte Map
  // to disk for saving disk space. 
  // 
  // The file name we are using is randomly generated and without
  // a file extension.

  LRandomString := IntToStr( GetTickCount() );
  FMapFileName  := COMMAND_DATA_DIR + '\Undo' + LRandomString;
  FMapWidth     := AOldQuickChannelMap.Width;
  FMapHeight    := AOldQuickChannelMap.Height;

  LDataStream := TMemoryStream.Create();
  try
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FMapWidth, FMapHeight);
      LByteMap.ReadFrom(AOldQuickChannelMap, ctUniformRGB);

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
      LDataStream.SaveToFile(FMapFileName);
    finally
      LByteMap.Free();
    end;

  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmExitFromQuickMaskChannelCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;

  if Assigned(FNewSelection) then
  begin
    FNewSelection.Free();
  end;

{$WARN UNSAFE_TYPE OFF}
  if FileExists(FMapFileName) then
  begin
    DeleteFile(PChar(FMapFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmExitFromQuickMaskChannelCommand.Execute;
var
  LSelection : TgmSelection;
begin
  inherited;

  FSetEditModeProc(emStandardMode);
  FChannelManager.DeleteQuickMaskChannel();

  // switching to color channel
  if FChannelManager.CurrentChannelType <> ctColorChannel then
  begin
    FChannelManager.SelectColorChannel(0, True);
  end;

  if Assigned(FNewSelection) then
  begin
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FNewSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmExitFromQuickMaskChannelCommand.Rollback;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
  LSelection  : TgmSelection;
begin
  inherited;

  FSetEditModeProc(emQuickMaskMode);

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    FChannelManager.CreateQuickMaskChannel(FMapWidth, FMapHeight);

    with FChannelManager.QuickMaskChannel do
    begin
      MaskColor          := FMaskColor;
      MaskOpacity        := FMaskOpacity;
      MaskColorIndicator := FMaskColorIndicator;
    end;
  end;

  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  if FileExists(FMapFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      LDataStream.LoadFromFile(FMapFileName);
      LDataStream.Position := 0;

      // read in Byte Map data
      LByteMap := TByteMap.Create();
      try
        LByteMap.SetSize(FMapWidth, FMapHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        with FChannelManager.QuickMaskChannel do
        begin
          ChannelLayer.Bitmap.BeginUpdate();
          try
            LByteMap.WriteTo(ChannelLayer.Bitmap, ctUniformRGB);
          finally
            ChannelLayer.Bitmap.EndUpdate();
          end;

          UpdateChannelThumbnail();
        end;
        
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;

  if Assigned(FOldSelection) then
  begin
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

{ TgmLayerMaskChannelOptionsCommand }

constructor TgmLayerMaskChannelOptionsCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  const AChannelOptionsData: TgmChannelOptionsCommandData);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmLayerMaskChannelOptionsCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmLayerMaskChannelOptionsCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmLayerMaskChannelOptionsCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  inherited Create('Channel Options');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;
  FChannelOptions := AChannelOptionsData;
end;

procedure TgmLayerMaskChannelOptionsCommand.Execute;
var
  LLayer           : TgmCustomLayer;
  LMaskChannelName : string;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmLayerMaskChannelOptionsCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmLayerMaskChannelOptionsCommand.Execute(): the mask of the layer is unvailable.');
  end;

  // switching to the layer that has the mask enabled 
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if not Assigned(FChannelManager.LayerMaskChannel) then
  begin
    LMaskChannelName := LLayer.LayerName + ' Mask'; 
    FChannelManager.CreateLayerMaskChannel(LLayer.MaskBitmap, LMaskChannelName);
  end;

  // channel switching ...
  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  with FChannelManager.LayerMaskChannel do
  begin
    ChannelLayer.Bitmap.BeginUpdate();
    try
      MaskColor   := FChannelOptions.NewMaskColor;
      MaskOpacity := Round(FChannelOptions.NewOpacityPercent * 255);
    finally
      ChannelLayer.Bitmap.EndUpdate();
    end;
  end;    
end;

procedure TgmLayerMaskChannelOptionsCommand.Rollback;
var
  LLayer           : TgmCustomLayer;
  LMaskChannelName : string;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmLayerMaskChannelOptionsCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmLayerMaskChannelOptionsCommand.Rollback(): the mask of the layer is unvailable.');
  end;

  // switching to the layer that has the mask enabled 
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if not Assigned(FChannelManager.LayerMaskChannel) then
  begin
    LMaskChannelName := LLayer.LayerName + ' Mask'; 
    FChannelManager.CreateLayerMaskChannel(LLayer.MaskBitmap, LMaskChannelName);
  end;

  // channel switching ...
  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  with FChannelManager.LayerMaskChannel do
  begin
    ChannelLayer.Bitmap.BeginUpdate();
    try
      MaskColor   := FChannelOptions.OldMaskColor;
      MaskOpacity := Round(FChannelOptions.OldOpacityPercent * 255);
    finally
      ChannelLayer.Bitmap.EndUpdate();
    end;
  end;
end;


{ TgmNewAlphaChannelCommand }

constructor TgmNewAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const AChannelWidth, AChannelHeight: Integer);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  inherited Create('New Channel');

  FChannelManager := AChannelManager; 
  FChannelWidth   := AChannelWidth;
  FChannelHeight  := AChannelHeight;
  FMaskColor      := AChannelManager.DefaultMaskColor;
end;

procedure TgmNewAlphaChannelCommand.Execute;
var
  LChannel : TgmAlphaChannel;
  LIndex   : Integer;
begin
  inherited;

  FChannelManager.SimplyAddNewAlphaChannel(FChannelWidth, FChannelHeight);

  LIndex   := FChannelManager.AlphaChannelList.Count - 1;
  LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[LIndex]);

  LChannel.IsChannelVisible := False;
  LChannel.MaskColor        := FMaskColor;
end;

procedure TgmNewAlphaChannelCommand.Rollback;
begin
  inherited;

  with FChannelManager do
  begin
    SimplyDeleteAlphaChannel(FChannelManager.AlphaChannelList.Count - 1);
    AlphaChannelList.AccumulatedCount := AlphaChannelList.AccumulatedCount - 1;
    SelectColorChannel(0, True);
  end;
end;

{ TgmQuickMaskChannelOptionsCommand }

constructor TgmQuickMaskChannelOptionsCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const AChannelOptionsData: TgmChannelOptionsCommandData);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmQuickMaskChannelOptionsCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmQuickMaskChannelOptionsCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  inherited Create('Channel Options');

  FChannelManager := AChannelManager;
  FChannelOptions := AChannelOptionsData;
end;

procedure TgmQuickMaskChannelOptionsCommand.Execute;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmQuickMaskChannelOptionsCommand.Execute(): the Quick Mask channel is unavailable.');
  end;

  with FChannelManager.QuickMaskChannel do
  begin
    ChannelLayer.Bitmap.BeginUpdate();
    try
      MaskColor          := FChannelOptions.NewMaskColor;
      MaskOpacity        := Round(FChannelOptions.NewOpacityPercent * 255);
      MaskColorIndicator := FChannelOptions.NewColorIndicator;
    finally
      ChannelLayer.Bitmap.EndUpdate();
    end;
  end;
end;

procedure TgmQuickMaskChannelOptionsCommand.Rollback;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmQuickMaskChannelOptionsCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  with FChannelManager.QuickMaskChannel do
  begin
    ChannelLayer.Bitmap.BeginUpdate();
    try
      MaskColor          := FChannelOptions.OldMaskColor;
      MaskOpacity        := Round(FChannelOptions.OldOpacityPercent * 255);
      MaskColorIndicator := FChannelOptions.OldColorIndicator;
    finally
      ChannelLayer.Bitmap.EndUpdate();
    end;
  end;
end;

{ TgmQuickMaskChannelProcessCommand }

constructor TgmQuickMaskChannelProcessCommand.Create(
  const ACommandName: string; const AUndoBmp, ARedoBmp: TBitmap32;
  AChannelManager: TgmCustomChannelManager);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmQuickMaskChannelProcessCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmQuickMaskChannelProcessCommand.Create(): the quick mask channel is unavailable.');
  end;

  inherited Create(ACommandName, AUndoBmp, ARedoBmp);

  FChannelManager := AChannelManager;
end;

procedure TgmQuickMaskChannelProcessCommand.Execute;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmQuickMaskChannelProcessCommand.Execute(): the quick mask channel is unavailable.');
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

        // TODO: prevent from the error that the size of the channel is
        // different than the map.
        with FChannelManager.QuickMaskChannel do
        begin
          ChannelLayer.Bitmap.BeginUpdate();
          try
            LByteMap.WriteTo(ChannelLayer.Bitmap, ctUniformRGB);
          finally
            ChannelLayer.Bitmap.EndUpdate();
          end;

          UpdateChannelThumbnail();
        end;
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

procedure TgmQuickMaskChannelProcessCommand.Rollback;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmQuickMaskChannelProcessCommand.Execute(): the quick mask channel is unavailable.');
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

        // TODO: prevent from the error that the size of the channel is
        // different than the map.
        with FChannelManager.QuickMaskChannel do
        begin
          ChannelLayer.Bitmap.BeginUpdate();
          try
            LByteMap.WriteTo(ChannelLayer.Bitmap, ctUniformRGB);
          finally
            ChannelLayer.Bitmap.EndUpdate();
          end;

          UpdateChannelThumbnail();
        end;
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

{ TgmRearrangeChannelsCommand }

constructor TgmRearrangeChannelsCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const AOldIndex, ANewIndex: Integer);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmRearrangeChannelsCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AOldIndex) then
  begin
    raise Exception.Create('TgmRearrangeChannelsCommand.Create(): parameter AOldIndex is out of range.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(ANewIndex) then
  begin
    raise Exception.Create('TgmRearrangeChannelsCommand.Create(): parameter ANewIndex is out of range.');
  end;

  if AOldIndex = ANewIndex then
  begin
    raise Exception.Create('TgmRearrangeChannelsCommand.Create(): parameter AOldIndex is same as ANewIndex.');
  end;

  inherited Create('Rearrange Channels');

  FChannelManager := AChannelManager;
  FOldIndex       := AOldIndex;
  FNewIndex       := ANewIndex;
end;

procedure TgmRearrangeChannelsCommand.Execute;
var
  LSelectedIndex : Integer;
begin
  inherited;

  LSelectedIndex := FChannelManager.AlphaChannelList.SelectedIndex;
  
  if FChannelManager.SimplyMoveAlphaChannel(FOldIndex, FNewIndex) then
  begin
    // select an alpha channel properly ...
    if LSelectedIndex >= 0 then
    begin
      if FOldIndex = LSelectedIndex then
      begin
        FChannelManager.SimplySelectAlphaChannel(FNewIndex, False);
      end
      else if FNewIndex > LSelectedIndex then
      begin
        LSelectedIndex := LSelectedIndex - 1;
        if LSelectedIndex < 0 then
        begin
          LSelectedIndex := 0;
        end;

        FChannelManager.SimplySelectAlphaChannel(LSelectedIndex, False);
      end
      else if FNewIndex < LSelectedIndex then
      begin
        LSelectedIndex := LSelectedIndex + 1;
        if LSelectedIndex >= FChannelManager.AlphaChannelList.Count then
        begin
          LSelectedIndex := FChannelManager.AlphaChannelList.Count - 1;
        end;

        FChannelManager.SimplySelectAlphaChannel(LSelectedIndex, False);
      end;
    end;
  end;
end;

procedure TgmRearrangeChannelsCommand.Rollback;
var
  LSelectedIndex : Integer;
begin
  inherited;

  LSelectedIndex := FChannelManager.AlphaChannelList.SelectedIndex;
  
  if FChannelManager.SimplyMoveAlphaChannel(FNewIndex, FOldIndex) then
  begin
    // select an alpha channel properly ...
    if LSelectedIndex >= 0 then
    begin
      if FNewIndex = LSelectedIndex then
      begin
        FChannelManager.SimplySelectAlphaChannel(FOldIndex, False);
      end
      else if FOldIndex > LSelectedIndex then
      begin
        LSelectedIndex := LSelectedIndex - 1;
        if LSelectedIndex < 0 then
        begin
          LSelectedIndex := 0;
        end;

        FChannelManager.SimplySelectAlphaChannel(LSelectedIndex, False);
      end
      else if FOldIndex < LSelectedIndex then
      begin
        LSelectedIndex := LSelectedIndex + 1;
        if LSelectedIndex >= FChannelManager.AlphaChannelList.Count then
        begin
          LSelectedIndex := FChannelManager.AlphaChannelList.Count - 1;
        end;

        FChannelManager.SimplySelectAlphaChannel(LSelectedIndex, False);
      end;
    end;
  end;
end;

{ TgmSaveSelectionAsChannelCommand }

constructor TgmSaveSelectionAsChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; AChannel: TgmAlphaChannel;
  const AChannelIndex: Integer);
var
  i             : Integer;
  LDataStream   : TMemoryStream;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSaveSelectionAsChannelCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(AChannel) then
  begin
    raise Exception.Create('TgmSaveSelectionAsChannelCommand.Create(): parameter AChannel is a nil pointer.');
  end;

  if AChannelIndex < 0 then
  begin
    raise Exception.Create('TgmSaveSelectionAsChannelCommand.Create(): parameter AChannelIndex is less than zero.');
  end;

  if (AChannel.ChannelLayer.Bitmap.Width <= 0) or
     (AChannel.ChannelLayer.Bitmap.Height <= 0) then
  begin
    raise Exception.Create('TgmSaveSelectionAsChannelCommand.Create(): the width or height of the channel is less than one.');
  end;

  inherited Create('Save Selection');

  FChannelManager := AChannelManager;
  FChannelName    := AChannel.ChannelName;
  FChannelIndex   := AChannelIndex;
  FChannelWidth   := AChannel.ChannelLayer.Bitmap.Width;
  FChannelHeight  := AChannel.ChannelLayer.Bitmap.Height;

  // Storing the channel byte map to disk for saving system memory.
  // The file name we are using is randomly generated and without
  // a file extension.
  LRandomString    := IntToStr( GetTickCount() );
  FChannelFileName := COMMAND_DATA_DIR + '\SaveSelectionAsChannel' + LRandomString;

  LDataStream := TMemoryStream.Create();
  try
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FChannelWidth, FChannelHeight);
      LByteMap.ReadFrom(AChannel.ChannelLayer.Bitmap, ctUniformRGB);

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
      LDataStream.SaveToFile(FChannelFileName);
    finally
      LByteMap.Free();
    end;
  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmSaveSelectionAsChannelCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FChannelFileName) then
  begin
    DeleteFile(PChar(FChannelFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmSaveSelectionAsChannelCommand.Execute;
var
  i              : Integer;
  LDataStream    : TMemoryStream;
  LByteMap       : TByteMap;
  LByteBits      : PByte;
  LChannelBitmap : TBitmap32;
  LChannel       : TgmAlphaChannel;
begin
  inherited;

  if not FileExists(FChannelFileName) then
  begin
    raise Exception.Create('TgmSaveSelectionAsChannelCommand.Execute(): the file named FChannelFileName is not existed.');
  end;

  if (FChannelIndex < 0) or
     (FChannelIndex > FChannelManager.AlphaChannelList.Count) then
  begin
    raise Exception.Create('TgmSaveSelectionAsChannelCommand.Execute(): the FChannelIndex is out of range.');
  end;

  // load pixel data of the alpha channel in
  LDataStream    := TMemoryStream.Create();
  LChannelBitmap := TBitmap32.Create();
  try
    LDataStream.LoadFromFile(FChannelFileName);
    LDataStream.Position := 0;

    // read in Byte Map data
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FChannelWidth, FChannelHeight);

{$WARN UNSAFE_CODE OFF}
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Read(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LChannelBitmap.SetSize(FChannelWidth, FChannelHeight);
      LByteMap.WriteTo(LChannelBitmap, ctUniformRGB);
 
      FChannelManager.SimplyInsertNewAlphaChannel(
        FChannelIndex, LChannelBitmap, False);
        
      LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelIndex]);

      LChannel.IsChannelVisible := False;
      LChannel.ChannelName      := FChannelName;
      LChannel.UpdateChannelThumbnail();
    finally
      LByteMap.Free();
    end;
  finally
    LDataStream.Clear();
    LDataStream.Free();
    LChannelBitmap.Free();
  end;
end;

procedure TgmSaveSelectionAsChannelCommand.Rollback;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmSaveSelectionAsChannelCommand.Rollback(): the FChannelIndex is out of range.');
  end;

  with FChannelManager do
  begin
    SimplyDeleteAlphaChannel(FChannelIndex);
    AlphaChannelList.AccumulatedCount := AlphaChannelList.AccumulatedCount - 1;
    SelectColorChannel(0, True);
  end;
end;

end.
