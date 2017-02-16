unit gmChannelIO;

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

// Update Date: 2015/11/18

interface

{$WARN UNSAFE_CODE OFF}

uses
{ Delphi }
  Classes,
  SysUtils,
{ Graphics32 }
  GR32,
{ GraphicsMagicLib }
  gmChannels,
  gmChannelManager;


type
  { Channels Writer }

  TgmChannelsWriter = class(TObject)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to a channel manager

    procedure SaveChannelData(AChannel: TgmAlphaChannel; AStream: TStream);
  public
    constructor Create(AChannelManager: TgmCustomChannelManager);

    procedure SaveToStream(AStream: TStream);
  end;



// load channels from a '*.gmd' file that is already loaded in a stream
function LoadChannelsFromStream(
  AStream: TStream; AChannelManager: TgmCustomChannelManager;
  const AFileVersion, AChannelCount, AChannelWidth, AChannelHeight: Cardinal): Boolean;


implementation

uses
{ Graphics32 }
  GR32_OrdinalMaps,
{ GraphicsMagicLib }
  gmTypes;

type
  // Used to load/save the layers info from/to a '*.gmd' file.
  // For old versions of '*.gmd' file.
  TgmChannelHeaderVer1 = record
    ChannelType        : Cardinal;
    ChannelName        : ShortString;
    MaskColor          : TColor32;
    MaskOpacityPercent : Single;
    MaskColorType      : Cardinal;
    MaskColorIndicator : Cardinal;
  end;

  // For new version of '*.gmd' file.
  TgmChannelHeaderVer2 = record
    ChannelType        : Cardinal;
    ChannelName        : ShortString;
    MaskColor          : TColor32;
    MaskOpacityPercent : Single;
    MaskColorIndicator : Cardinal;
  end;

  { TgmCustomChannelLoader }

  // The channel loader is used to load channels from a given .gmd file.
  TgmCustomChannelLoader = class(TObject)
  protected
    FFileStream    : TStream;    // pointer to a stream
    FChannelMap    : TBitmap32;  // save the channel map that is loaded from a stream with this field
    FChannelWidth  : Integer;
    FChannelHeight : Integer;

    function LoadChannelData: Boolean; virtual; abstract;
  public
    constructor Create(AStream: TStream;
      const AChannelWidth, AChannelHeight: Integer);

    destructor Destroy; override;

    property ChannelMap : TBitmap32 read FChannelMap;
  end;

  { TgmChannelLoader1 }

  TgmChannelLoader1 = class(TgmCustomChannelLoader)
  private
    FChannelSettings : TgmChannelHeaderVer1;
  protected
    function LoadChannelData: Boolean; override;
  public
    property ChannelSettings : TgmChannelHeaderVer1 read FChannelSettings;
  end;

  { TgmChannelLoader2 }

  TgmChannelLoader2 = class(TgmCustomChannelLoader)
  private
    FChannelSettings : TgmChannelHeaderVer2;
  protected
    function LoadChannelData: Boolean; override;
  public
    property ChannelSettings : TgmChannelHeaderVer2 read FChannelSettings;
  end;


// load channels from a '*.gmd' file that is already loaded in a stream
function LoadChannelsFromStream(
  AStream: TStream; AChannelManager: TgmCustomChannelManager;
  const AFileVersion, AChannelCount, AChannelWidth, AChannelHeight: Cardinal): Boolean;
var
  i, LChannelLayerIndex : Integer;
  LChannelLoader1       : TgmChannelLoader1;
  LChannelLoader2       : TgmChannelLoader2;
  LAlphaChannel         : TgmAlphaChannel;
  LOldChannelType       : TgmWorkingChannelType;
  LNewChannelType       : TgmChannelType;
begin
  Result := False;

  if (not Assigned(AStream)) or
     (AStream.Size <= 0) or
     (not Assigned(AChannelManager)) or
     (AFileVersion   = 0) or
     (AChannelCount  = 0) or
     (AChannelWidth  = 0) or
     (AChannelHeight = 0) then
  begin
    Exit;
  end;

  case AFileVersion of
    1,
    2,
    3: // same data in a .gmd file with version 1, 2, 3
      begin
        LChannelLoader1 := TgmChannelLoader1.Create(AStream, AChannelWidth, AChannelHeight);

        for i := 0 to (AChannelCount - 1) do
        begin
          LChannelLayerIndex := AChannelManager.ChannelLayerBaseIndex;
          LChannelLayerIndex := LChannelLayerIndex + AChannelManager.AlphaChannelList.Count;
          if Assigned(AChannelManager.LayerMaskChannel) then
          begin
            Inc(LChannelLayerIndex);
          end;

          if LChannelLoader1.LoadChannelData() then
          begin
            LOldChannelType := TgmWorkingChannelType(LChannelLoader1.ChannelSettings.ChannelType);

            case LOldChannelType of
              wctAlpha:
                begin
                  LAlphaChannel := TgmAlphaChannel.Create(
                    AChannelManager.Layers,
                    LChannelLayerIndex,
                    AChannelWidth,
                    AChannelHeight,
                    AChannelManager.ChannelLayerLocation,
                    LChannelLoader1.ChannelSettings.MaskColor);

                  with LAlphaChannel do
                  begin
                    OnThumbnailUpdate  := AChannelManager.OnChannelThumbnailUpdate;
                    ChannelName        := LChannelLoader1.ChannelSettings.ChannelName;
                    MaskOpacity        := Round(LChannelLoader1.ChannelSettings.MaskOpacityPercent * 255);
                    MaskColorType      := TgmMaskColorType(LChannelLoader1.ChannelSettings.MaskColorType);
                    MaskColorIndicator := TgmMaskColorIndicator(LChannelLoader1.ChannelSettings.MaskColorIndicator);

                    // get the pixels of the channel map
                    ChannelLayer.Bitmap.Draw(0, 0, LChannelLoader1.ChannelMap);
                    UpdateChannelThumbnail();
                  end;

                  AChannelManager.AlphaChannelList.Add(LAlphaChannel, False);

                  LAlphaChannel.ChannelName := 'Hello';
                end;

              wctQuickMask:
                begin
                  AChannelManager.CreateQuickMaskChannel(
                    AChannelWidth, AChannelHeight, False);

                  if Assigned(AChannelManager.QuickMaskChannel) then
                  begin
                    with AChannelManager.QuickMaskChannel do
                    begin
                      MaskColor          := LChannelLoader1.ChannelSettings.MaskColor;
                      MaskOpacity        := Round(LChannelLoader1.ChannelSettings.MaskOpacityPercent * 255);
                      MaskColorType      := TgmMaskColorType(LChannelLoader1.ChannelSettings.MaskColorType);
                      MaskColorIndicator := TgmMaskColorIndicator(LChannelLoader1.ChannelSettings.MaskColorIndicator);

                      // get the pixels of the channel map
                      ChannelLayer.Bitmap.Draw(0, 0, LChannelLoader1.ChannelMap);
                      UpdateChannelThumbnail();
                    end;
                  end;
                end;
            end;
          end;
        end;

        Result := True;
      end;

    4:
      begin
        LChannelLoader2 := TgmChannelLoader2.Create(AStream, AChannelWidth, AChannelHeight);

        for i := 0 to (AChannelCount - 1) do
        begin
          LChannelLayerIndex := AChannelManager.ChannelLayerBaseIndex;
          LChannelLayerIndex := LChannelLayerIndex + AChannelManager.AlphaChannelList.Count;
          if Assigned(AChannelManager.LayerMaskChannel) then
          begin
            Inc(LChannelLayerIndex);
          end;

          if LChannelLoader2.LoadChannelData() then
          begin
            LNewChannelType := TgmChannelType(LChannelLoader2.ChannelSettings.ChannelType);

            case LNewChannelType of
              ctAlphaChannel:
                begin
                  LAlphaChannel := TgmAlphaChannel.Create(
                    AChannelManager.Layers,
                    LChannelLayerIndex,
                    AChannelWidth,
                    AChannelHeight,
                    AChannelManager.ChannelLayerLocation,
                    LChannelLoader2.ChannelSettings.MaskColor);

                  with LAlphaChannel do
                  begin
                    OnThumbnailUpdate  := AChannelManager.OnChannelThumbnailUpdate;
                    ChannelName        := LChannelLoader2.ChannelSettings.ChannelName;
                    MaskOpacity        := Round(LChannelLoader2.ChannelSettings.MaskOpacityPercent * 255);
                    MaskColorIndicator := TgmMaskColorIndicator(LChannelLoader2.ChannelSettings.MaskColorIndicator);

                    // get the pixels of the channel map
                    ChannelLayer.Bitmap.Draw(0, 0, LChannelLoader2.ChannelMap);
                    UpdateChannelThumbnail();
                  end;

                  AChannelManager.AlphaChannelList.Add(LAlphaChannel, False);
                end;

              ctQuickMaskChannel:
                begin
                  AChannelManager.CreateQuickMaskChannel(
                    AChannelWidth, AChannelHeight, False);

                  if Assigned(AChannelManager.QuickMaskChannel) then
                  begin
                    with AChannelManager.QuickMaskChannel do
                    begin
                      MaskColor          := LChannelLoader2.ChannelSettings.MaskColor;
                      MaskOpacity        := Round(LChannelLoader2.ChannelSettings.MaskOpacityPercent * 255);
                      MaskColorIndicator := TgmMaskColorIndicator(LChannelLoader2.ChannelSettings.MaskColorIndicator);

                      // get the pixels of the channel map
                      ChannelLayer.Bitmap.Draw(0, 0, LChannelLoader2.ChannelMap);
                      UpdateChannelThumbnail();
                    end;
                  end;
                end;
            end;
          end;
        end;

        Result := True;
      end;
  end;
end;


{ TgmCustomChannelLoader }

constructor TgmCustomChannelLoader.Create(AStream: TStream;
  const AChannelWidth, AChannelHeight: Integer);
begin
  if not Assigned(AStream) then
  begin
    raise Exception.Create('[ERROR] TgmCustomChannelLoader.Create(): AStream is nil.');
  end;

  if AStream.Size = 0 then
  begin
    raise Exception.Create('[ERROR] TgmCustomChannelLoader.Create(): There is no data in the AStream.');
  end;

  if (AChannelWidth <= 0) or (AChannelHeight <= 0) then
  begin
    raise Exception.Create('[ERROR] TgmCustomChannelLoader.Create(): Channel size is not correct.');
  end;

  inherited Create();

  Self.FFileStream := AStream;
  FChannelWidth    := AChannelWidth;
  FChannelHeight   := AChannelHeight;

  FChannelMap := TBitmap32.Create;
  FChannelMap.SetSize(FChannelWidth, FChannelHeight);
end;

destructor TgmCustomChannelLoader.Destroy;
begin
  FChannelMap.Free();
  inherited;
end;

{ TgmChannelLoader1 }

function TgmChannelLoader1.LoadChannelData: Boolean;
var
  i         : Integer;
  LByteMap  : TByteMap;
  LByteBits : PByte;
begin
  Result := False;

  if Assigned(FFileStream) then
  begin
    // read channel settings ...
    FFileStream.Read( FChannelSettings, SizeOf(TgmChannelHeaderVer1) );

    // read in the channel map data from the stream
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FChannelWidth, FChannelHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];

      for i := 0 to (FChannelHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FChannelWidth);
        Inc(LByteBits, FChannelWidth);
      end;

      LByteMap.WriteTo(FChannelMap, ctUniformRGB);
    finally
      LByteMap.Free();
    end;

    Result := True;
  end;
end;

{ TgmChannelLoader2 }

function TgmChannelLoader2.LoadChannelData: Boolean;
var
  i         : Integer;
  LByteMap  : TByteMap;
  LByteBits : PByte;
begin
  Result := False;

  if Assigned(FFileStream) then
  begin
    // read channel settings ...
    FFileStream.Read( FChannelSettings, SizeOf(TgmChannelHeaderVer2) );

    // read in the channel map data from the stream
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FChannelWidth, FChannelHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];

      for i := 0 to (FChannelHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FChannelWidth);
        Inc(LByteBits, FChannelWidth);
      end;

      LByteMap.WriteTo(FChannelMap, ctUniformRGB);
    finally
      LByteMap.Free();
    end;

    Result := True;
  end;
end;

{ Channels Writer }

constructor TgmChannelsWriter.Create(AChannelManager: TgmCustomChannelManager);
begin
  inherited Create();

  FChannelManager := AChannelManager;
end;

procedure TgmChannelsWriter.SaveChannelData(AChannel: TgmAlphaChannel;
  AStream: TStream);
var
  i, w, h   : Integer;
  LByteMap  : TByteMap;
  LByteBits : PByte;
begin
  if Assigned(AChannel) and Assigned(AStream) then
  begin
    // write the pixels of channel layer to stream
    LByteMap := TByteMap.Create();
    try
      // Note that, we only need to save single component to stream,
      // because each RGB component of a pixel on an Alpha layer is identical.
      w := AChannel.ChannelLayer.Bitmap.Width;
      h := AChannel.ChannelLayer.Bitmap.Height;

      LByteMap.SetSize(w, h);
      LByteMap.ReadFrom(AChannel.ChannelLayer.Bitmap, ctBlue);

      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (h - 1) do
      begin
        AStream.Write(LByteBits^, w);
        Inc(LByteBits, w);
      end;
    finally
      LByteMap.Free();
    end;
  end;
end;

procedure TgmChannelsWriter.SaveToStream(AStream: TStream);
var
  i              : Integer;
  LAlphaChannel  : TgmAlphaChannel;
  LChannelHeader : TgmChannelHeaderVer2;
begin
  if not Assigned(AStream) then
  begin
    Exit;
  end;

  if Assigned(FChannelManager) then
  begin
    // save Alpha channels first ...
    if FChannelManager.AlphaChannelList.Count > 0 then
    begin
      for i := 0 to (FChannelManager.AlphaChannelList.Count - 1) do
      begin
        LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[i]);

        // save channel header first ...
        LChannelHeader.ChannelType        := Ord(ctAlphaChannel);
        LChannelHeader.ChannelName        := LAlphaChannel.ChannelName;
        LChannelHeader.MaskColor          := LAlphaChannel.MaskColor;
        LChannelHeader.MaskOpacityPercent := LAlphaChannel.MaskOpacity / 255;
        LChannelHeader.MaskColorIndicator := Ord(LAlphaChannel.MaskColorIndicator);

        AStream.Write( LChannelHeader, SizeOf(TgmChannelHeaderVer2) );

        // then save data of the channel
        SaveChannelData(LAlphaChannel, AStream);
      end;
    end;

    // save Quick Mask channel to stream, if any ...
    if Assigned(FChannelManager.QuickMaskChannel) then
    begin
      // save channel header first ...
      with FChannelManager do
      begin
        LChannelHeader.ChannelType        := Ord(ctQuickMaskChannel);
        LChannelHeader.ChannelName        := QuickMaskChannel.ClassName;
        LChannelHeader.MaskColor          := QuickMaskChannel.MaskColor;
        LChannelHeader.MaskOpacityPercent := QuickMaskChannel.MaskOpacity / 255;
        LChannelHeader.MaskColorIndicator := Ord(QuickMaskChannel.MaskColorIndicator);

        AStream.Write( LChannelHeader, SizeOf(TgmChannelHeaderVer2) );

        // then save data of the channel
        SaveChannelData(QuickMaskChannel, AStream);
      end;
    end;
  end;
end;


end.
