unit gmChannelManager;

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
 *
 * The Original Code is igChannelManager.pas.
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2016/12/28

interface

{$WARN UNSAFE_CODE OFF}

uses
{ Standard }
  Classes,
{ Graphics32 }
  GR32,
  GR32_Layers,
{ GraphicsMagic lib }
  gmChannels,
  gmCrop,
  gmTypes;

type
  TgmChannelType = (ctColorChannel,
                    ctAlphaChannel,
                    ctLayerMaskChannel,
                    ctQuickMaskChannel);

  TgmAlphaChannelOrderChangedEvent = procedure (AList: TgmAlphaChannelList; const AOldIndex, ANewIndex: Integer) of object;
  TgmChannelDblClick = procedure (AChannel: TgmCustomChannel; const AChannelType: TgmChannelType) of object;
  TgmChannelVisibleChanged = procedure (const AChannelType: TgmChannelType) of object;
  TgmInsertAlphaChannelEvent = procedure (AList: TgmAlphaChannelList; const AIndex: Integer) of object;
  TgmSelectedChannelChangedEvent = procedure (const ACurrentChannelType: TgmChannelType) of object;


  { TgmCustomChannelManager }

  TgmCustomChannelManager = class(TObject)
  private
    function GetSelectedAlphaChannel: TgmAlphaChannel;
    
    procedure SetChannelLayerBaseIndex(const AValue: Integer);
    procedure SetChannelLayerLocation(const ALocation: TFloatRect);
  protected
    FColorChannelList           : TgmChannelList;  // holding color channels for RGB, Red, Green, Blue, CMYK, Cyan, etc.
    FAlphaChannelList           : TgmAlphaChannelList;
    FLayerMaskChannel           : TgmAlphaChannel;
    FQuickMaskChannel           : TgmAlphaChannel;
    FCurrentChannelType         : TgmChannelType;
    FChannelPreviewSet          : TgmChannelSet;        // indicate which channel will be shown on the channel preview layer
    FSelectedColorChannels      : TgmChannelSet;        // indicate which color channels have been selected
    FAlphaChannelMultiSelect    : Boolean;              // whether we could select multiple alpha channels at a time

    // Pointer to an outside layer collection for holding
    // channel layers for layer mask channel, quick mask channel and
    // alpha channels.
    FLayers                     : TLayerCollection;
    FChannelLayerLocation       : TFloatRect;
    FChannelLayerBaseIndex      : Integer;
    FDefaultMaskColor           : TColor32;              // default mask color for alpha channels layer blending

    FOnInsertAlphaChannel       : TgmInsertAlphaChannelEvent;
    FOnAlphaChannelDelete       : TNotifyEvent;
    FOnAlphaChannelOrderChanged : TgmAlphaChannelOrderChangedEvent;
    FOnChannelDblClick          : TgmChannelDblClick;
    FOnChannelThumbUpdate       : TNotifyEvent;
    FOnChannelVisibleChanged    : TgmChannelVisibleChanged;
    FOnLayerMaskChannelCreate   : TNotifyEvent;
    FOnLayerMaskChannelDelete   : TNotifyEvent;
    FOnQuickMaskChannelCreate   : TNotifyEvent;
    FOnQuickMaskChannelDelete   : TNotifyEvent;
    FOnSelectedChannelChanged   : TgmSelectedChannelChangedEvent;

    procedure ChannelPreviewLayerBlend(F: TColor32; var B: TColor32; M: TColor32); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddNewAlphaChannel(const AChannelBmpWidth, AChannelBmpHeight: Integer;
      const AAccumCount: Boolean = True); overload; virtual;

    procedure AddNewAlphaChannel(AChannelBmp: TBitmap32;
      const AAccumCount: Boolean = True); overload; virtual;

    procedure BlendByColorChannelSettings(ASrcBitmap, ADstBitmap: TBitmap32;
      const ARect: TRect); virtual; abstract;

    procedure CreateLayerMaskChannel(AMaskBmp: TBitmap32; const AChannelName: string); virtual;

    procedure CreateQuickMaskChannel(
      const AChannelWidth, AChannelHeight: Integer;
      const IsInvokeCallback: Boolean = True); virtual;

    procedure CropChannels(ACrop: TgmCrop); overload;
    procedure CropChannels(const ACropArea: TRect); overload;
      
    procedure DeleteAlphaChannel(const AIndex: Integer); virtual;
    procedure DeleteLayerMaskChannel; virtual;
    procedure DeleteQuickMaskChannel; virtual;
    procedure DeleteSelectedAlphaChannels; virtual;

    procedure ResizeChannels(const ANewWidth, ANewHeight: Integer);

    procedure ResizeCanvasOfChannels(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection);

    procedure RotateChannels(const ADegrees: Integer;
      const ADirection: TgmRotateDirection);

    procedure SelectAlphaChannel(const AIndex: Integer; const AMultiSelect: Boolean); virtual; abstract;
    procedure SelectColorChannel(const AIndex: Integer; const AMultiSelect: Boolean); virtual; abstract;
    procedure SelectLayerMaskChannel; virtual; abstract;
    procedure SelectQuickMaskChannel; virtual; abstract;

    procedure SimplyAddNewAlphaChannel(
      const AChannelBmpWidth, AChannelBmpHeight: Integer;
      const AAccumCount: Boolean = True);

    procedure SimplyDeleteAlphaChannel(const AIndex: Integer);
    procedure SimplyDeleteQuickMaskChannel;
    procedure SimplySelectAlphaChannel(const AIndex: Integer; const AMultiSelect: Boolean); virtual; abstract;

    procedure ToggleAlphaChannelVisible(const AIndex: Integer); virtual; abstract;
    procedure ToggleColorChannelVisible(const AIndex: Integer); virtual; abstract;
    procedure ToggleLayerMaskChannelVisible; virtual; abstract;
    procedure ToggleQuickMaskChannelVisible; virtual; abstract;
    
    procedure UpdateColorChannelThumbnails(ABitmap: TCustomBitmap32); virtual;

    function GetColorChannelGrayscaleMap(ABitmap: TCustomBitmap32): TBitmap32; virtual; abstract;

    function InsertNewAlphaChannel(AIndex: Integer; AChannelBmp: TBitmap32;
      const AAccumCount: Boolean = True): Integer;

    function MoveAlphaChannel(const AOldIndex, ANewIndex: Integer): Boolean;

    function SimplyInsertNewAlphaChannel(AIndex: Integer; AChannelBmp: TBitmap32;
      const AAccumCount: Boolean = True): Integer;

    function SimplyMoveAlphaChannel(const AOldIndex, ANewIndex: Integer): Boolean;


    property AlphaChannelMultiSelect    : Boolean                          read FAlphaChannelMultiSelect    write FAlphaChannelMultiSelect;
    property AlphaChannelList           : TgmAlphaChannelList              read FAlphaChannelList;
    property ChannelLayerBaseIndex      : Integer                          read FChannelLayerBaseIndex      write SetChannelLayerBaseIndex;
    property ChannelLayerLocation       : TFloatRect                       read FChannelLayerLocation       write SetChannelLayerLocation;
    property ColorChannelList           : TgmChannelList                   read FColorChannelList;
    property CurrentChannelType         : TgmChannelType                   read FCurrentChannelType;
    property DefaultMaskColor           : TColor32                         read FDefaultMaskColor           write FDefaultMaskColor;
    property Layers                     : TLayerCollection                 read FLayers                     write FLayers;
    property LayerMaskChannel           : TgmAlphaChannel                  read FLayerMaskChannel;
    property OnAlphaChannelDelete       : TNotifyEvent                     read FOnAlphaChannelDelete       write FOnAlphaChannelDelete;
    property OnAlphaChannelOrderChanged : TgmAlphaChannelOrderChangedEvent read FOnAlphaChannelOrderChanged write FOnAlphaChannelOrderChanged;
    property OnChannelDblClick          : TgmChannelDblClick               read FOnChannelDblClick          write FOnChannelDblClick;
    property OnChannelThumbnailUpdate   : TNotifyEvent                     read FOnChannelThumbUpdate       write FOnChannelThumbUpdate;
    property OnChannelVisibleChanged    : TgmChannelVisibleChanged         read FOnChannelVisibleChanged    write FOnChannelVisibleChanged;
    property OnInsertAlphaChannel       : TgmInsertAlphaChannelEvent       read FOnInsertAlphaChannel       write FOnInsertAlphaChannel;
    property OnLayerMaskChannelCreate   : TNotifyEvent                     read FOnLayerMaskChannelCreate   write FOnLayerMaskChannelCreate;
    property OnLayerMaskChannelDelete   : TNotifyEvent                     read FOnLayerMaskChannelDelete   write FOnLayerMaskChannelDelete;
    property OnQuickMaskChannelCreate   : TNotifyEvent                     read FOnQuickMaskChannelCreate   write FOnQuickMaskChannelCreate;
    property OnQuickMaskChannelDelete   : TNotifyEvent                     read FOnQuickMaskChannelDelete   write FOnQuickMaskChannelDelete;
    property OnSelectedChannelChanged   : TgmSelectedChannelChangedEvent   read FOnSelectedChannelChanged   write FOnSelectedChannelChanged;
    property QuickMaskChannel           : TgmAlphaChannel                  read FQuickMaskChannel;
    property SelectedAlphaChannel       : TgmAlphaChannel                  read GetSelectedAlphaChannel;
    property SelectedColorChannels      : TgmChannelSet                    read FSelectedColorChannels;
  end;



function GetChannelMap(ASourceBitmap: TBitmap32;
  const AChannel: TgmChannelSelector; const AInvert: Boolean): TBitmap32;


implementation

uses
{ Standard }
  SysUtils;


function GetChannelMap(ASourceBitmap: TBitmap32;
  const AChannel: TgmChannelSelector; const AInvert: Boolean): TBitmap32;
var
  i             : Integer;
  a, r, g, b, v : Cardinal;
  p1, p2        : PColor32;
begin
  Result := nil;
  
  if not Assigned(ASourceBitmap) then
  begin
    Exit;
  end;

  Result := TBitmap32.Create;
  Result.SetSizeFrom(ASourceBitmap);

  p1 := @ASourceBitmap.Bits[0];
  p2 := @Result.Bits[0];

  for i := 1 to (Result.Width * Result.Height) do
  begin
    case AChannel of
      csRed:
        begin
          v := p1^ shr 16 and $FF;
        end;

      csGreen:
        begin
          v := p1^ shr 8 and $FF;
        end;

      csBlue:
        begin
          v := p1^ and $FF;
        end;

      csGrayscale:
        begin
          r := p1^ shr 16 and $FF;
          g := p1^ shr 8 and $FF;
          b := p1^ and $FF;
          v := (r + g + b) div 3;
        end;

      csAlpha:
        begin
          v := p1^ shr 24 and $FF;
        end;
        
    else
      v := 0;
    end;

    // Take alpha channel into account, make the extracted channel info
    // be against a white background when the alpha channel is not 255.
    a := p1^ shr 24 and $FF;
    v := ( v * a + 255 * (255 - a) ) div 255;

    if AInvert then
    begin
      v := 255 - v;
    end;

    p2^ := $FF000000 or (v shl 16) or (v shl 8) or v;
    
    Inc(p1);
    Inc(p2);
  end;
end;


{ TgmCustomChannelManager }

constructor TgmCustomChannelManager.Create;
begin
  inherited;

  FColorChannelList        := TgmChannelList.Create();
  FAlphaChannelList        := TgmAlphaChannelList.Create();
  FLayerMaskChannel        := nil;
  FQuickMaskChannel        := nil;
  FLayers                  := nil;
  FChannelLayerLocation    := FloatRect(0, 0, 0, 0);
  FChannelLayerBaseIndex   := 0;
  FAlphaChannelMultiSelect := False;

  FCurrentChannelType    := ctColorChannel;
  FChannelPreviewSet     := [];
  FSelectedColorChannels := [];
  FDefaultMaskColor      := clRed32;

  FOnAlphaChannelDelete       := nil;
  FOnAlphaChannelOrderChanged := nil;
  FOnChannelDblClick          := nil;
  FOnChannelVisibleChanged    := nil;
  FOnChannelThumbUpdate       := nil;
  FOnInsertAlphaChannel       := nil;
  FOnLayerMaskChannelCreate   := nil;
  FOnLayerMaskChannelDelete   := nil;
  FOnQuickMaskChannelCreate   := nil;
  FOnQuickMaskChannelDelete   := nil;
  FOnSelectedChannelChanged   := nil;
end;

destructor TgmCustomChannelManager.Destroy;
begin
  FLayers                     := nil;
  FOnAlphaChannelDelete       := nil;
  FOnAlphaChannelOrderChanged := nil;
  FOnChannelDblClick          := nil;
  FOnChannelThumbUpdate       := nil;
  FOnChannelVisibleChanged    := nil;
  FOnInsertAlphaChannel       := nil;
  FOnLayerMaskChannelCreate   := nil;
  FOnLayerMaskChannelDelete   := nil;
  FOnQuickMaskChannelCreate   := nil;
  FOnQuickMaskChannelDelete   := nil;
  FOnSelectedChannelChanged   := nil;

  FAlphaChannelList.Free();
  FLayerMaskChannel.Free();
  FQuickMaskChannel.Free();
  FColorChannelList.Free();

  inherited;
end;

procedure TgmCustomChannelManager.AddNewAlphaChannel(
  const AChannelBmpWidth, AChannelBmpHeight: Integer;
  const AAccumCount: Boolean = True);
var
  LLayerIndex   : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if not Assigned(FLayers) then
  begin
    raise Exception.Create('[Error] TgmCustomChannelManager.AddNewAlphaChannel(): field FLayers is nil. ');
  end;

  LLayerIndex := FChannelLayerBaseIndex;
  
  if Assigned(FLayerMaskChannel) then
  begin
    Inc(LLayerIndex);
  end;

  LLayerIndex := LLayerIndex + FAlphaChannelList.Count;
  
  LAlphaChannel := TgmAlphaChannel.Create(FLayers,
                                          LLayerIndex,
                                          AChannelBmpWidth,
                                          AChannelBmpHeight,
                                          FChannelLayerLocation,
                                          FDefaultMaskColor);

  LAlphaChannel.OnThumbnailUpdate := FOnChannelThumbUpdate;

  FAlphaChannelList.Add(LAlphaChannel, AAccumCount);

  if Assigned(FOnInsertAlphaChannel) then
  begin
    FOnInsertAlphaChannel(FAlphaChannelList, FAlphaChannelList.Count - 1);
  end;
end;

procedure TgmCustomChannelManager.AddNewAlphaChannel(AChannelBmp: TBitmap32;
  const AAccumCount: Boolean = True);
var
  LLayerIndex   : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if not Assigned(FLayers) then
  begin
    raise Exception.Create('[Error] TgmCustomChannelManager.AddNewAlphaChannel(): field FLayers is nil. ');
  end;

  LLayerIndex := FChannelLayerBaseIndex;
  
  if Assigned(FLayerMaskChannel) then
  begin
    Inc(LLayerIndex);
  end;

  LLayerIndex := LLayerIndex + FAlphaChannelList.Count;
  
  LAlphaChannel := TgmAlphaChannel.Create(FLayers,
                                          LLayerIndex,
                                          AChannelBmp,
                                          FChannelLayerLocation,
                                          FDefaultMaskColor);

  LAlphaChannel.OnThumbnailUpdate := FOnChannelThumbUpdate;

  FAlphaChannelList.Add(LAlphaChannel, AAccumCount);

  if Assigned(FOnInsertAlphaChannel) then
  begin
    FOnInsertAlphaChannel(FAlphaChannelList, FAlphaChannelList.Count - 1);
  end;
end;

procedure TgmCustomChannelManager.CreateLayerMaskChannel(AMaskBmp: TBitmap32;
  const AChannelName: string);
begin
  if not Assigned(AMaskBmp) then
  begin
    raise Exception.Create('[Error] TgmCustomChannelManager.CreateLayerMaskChannel(): parameter AMaskBmp is nil. ');
  end;

  if not Assigned(FLayers) then
  begin
    raise Exception.Create('[Error] TgmCustomChannelManager.CreateLayerMaskChannel(): field FLayers is nil. ');
  end;

  if not Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel := TgmAlphaChannel.Create(FLayers,
      FChannelLayerBaseIndex, AMaskBmp, FChannelLayerLocation,
      FDefaultMaskColor);

    FLayerMaskChannel.IsChannelVisible  := False;
    FLayerMaskChannel.ChannelName       := AChannelName;
    FLayerMaskChannel.OnThumbnailUpdate := FOnChannelThumbUpdate;

    if Assigned(FOnLayerMaskChannelCreate) then
    begin
      FOnLayerMaskChannelCreate(FLayerMaskChannel);
    end;
  end;
end;

procedure TgmCustomChannelManager.CreateQuickMaskChannel(
  const AChannelWidth, AChannelHeight: Integer;
  const IsInvokeCallback: Boolean = True);
var
  LLayerIndex : Integer;
begin
  if not Assigned(FQuickMaskChannel) then
  begin
    LLayerIndex := FChannelLayerBaseIndex;

    if Assigned(FLayerMaskChannel) then
    begin
      Inc(LLayerIndex);
    end;

    LLayerIndex := LLayerIndex + FAlphaChannelList.Count;

    FQuickMaskChannel := TgmAlphaChannel.Create(FLayers,
      LLayerIndex, AChannelWidth, AChannelHeight, FChannelLayerLocation,
      FDefaultMaskColor);

    FQuickMaskChannel.ChannelLayer.Bitmap.Clear($FFFFFFFF);
    FQuickMaskChannel.UpdateChannelThumbnail;

    FQuickMaskChannel.ChannelName       := 'Quick Mask';
    FQuickMaskChannel.OnThumbnailUpdate := FOnChannelThumbUpdate;

    if Assigned(FOnQuickMaskChannelCreate) and IsInvokeCallback then
    begin
      FOnQuickMaskChannelCreate(FQuickMaskChannel);
    end;
  end;
end;

procedure TgmCustomChannelManager.CropChannels(ACrop: TgmCrop);
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if Assigned(ACrop) then
  begin
    if Assigned(FLayerMaskChannel) then
    begin
      FLayerMaskChannel.Crop(ACrop, clWhite32);
    end;

    if Assigned(FQuickMaskChannel) then
    begin
      FQuickMaskChannel.Crop(ACrop, clBlack32);
    end;

    if FAlphaChannelList.Count > 0 then
    begin
      for i := 0 to (FAlphaChannelList.Count - 1) do
      begin
        LAlphaChannel := TgmAlphaChannel(FAlphaChannelList.Channels[i]);

        LAlphaChannel.Crop(ACrop, clBlack32);
      end;
    end;
  end;
end;

procedure TgmCustomChannelManager.CropChannels(const ACropArea: TRect);
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if (ACropArea.Left <> ACropArea.Right) and
     (ACropArea.Top <> ACropArea.Bottom) then
  begin
    if Assigned(FLayerMaskChannel) then
    begin
      FLayerMaskChannel.Crop(ACropArea, clWhite32);
    end;

    if Assigned(FQuickMaskChannel) then
    begin
      FQuickMaskChannel.Crop(ACropArea, clBlack32);
    end;

    if FAlphaChannelList.Count > 0 then
    begin
      for i := 0 to (FAlphaChannelList.Count - 1) do
      begin
        LAlphaChannel := TgmAlphaChannel(FAlphaChannelList.Channels[i]);

        LAlphaChannel.Crop(ACropArea, clBlack32);
      end;
    end;
  end;
end;

procedure TgmCustomChannelManager.DeleteAlphaChannel(const AIndex: Integer);
begin
  FAlphaChannelList.DeleteChannel(AIndex);

  if Assigned(FOnAlphaChannelDelete) then
  begin
    FOnAlphaChannelDelete(Self);
  end;
end;

procedure TgmCustomChannelManager.DeleteLayerMaskChannel;
begin
  if Assigned(FLayerMaskChannel) then
  begin
    FreeAndNil(FLayerMaskChannel);

    if Assigned(FOnLayerMaskChannelDelete) then
    begin
      FOnLayerMaskChannelDelete(Self);
    end;
  end;
end;

procedure TgmCustomChannelManager.DeleteQuickMaskChannel;
begin
  if Assigned(FQuickMaskChannel) then
  begin
    FreeAndNil(FQuickMaskChannel);

    if Assigned(FOnQuickMaskChannelDelete) then
    begin
      FOnQuickMaskChannelDelete(Self);
    end;
  end;
end;

procedure TgmCustomChannelManager.DeleteSelectedAlphaChannels;
begin
  FAlphaChannelList.DeleteSelectedChannels();

  if Assigned(FOnAlphaChannelDelete) then
  begin
    FOnAlphaChannelDelete(Self);
  end;
end;

function TgmCustomChannelManager.GetSelectedAlphaChannel: TgmAlphaChannel;
begin
  Result := FAlphaChannelList.SelectedChannel;
end;

function TgmCustomChannelManager.InsertNewAlphaChannel(
  AIndex: Integer; AChannelBmp: TBitmap32;
  const AAccumCount: Boolean = True): Integer;
var
  LLayerIndex   : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if not Assigned(FLayers) then
  begin
    raise Exception.Create('[Error] TgmCustomChannelManager.InsertNewAlphaChannel(): field FLayers is nil. ');
  end;

  LLayerIndex := FChannelLayerBaseIndex;
  
  if Assigned(FLayerMaskChannel) then
  begin
    Inc(LLayerIndex);
  end;

  if AIndex < 0 then
  begin
    AIndex := 0;
  end
  else if AIndex > FAlphaChannelList.Count then
  begin
    AIndex := FAlphaChannelList.Count;
  end;
  
  LLayerIndex := LLayerIndex + AIndex;
  
  LAlphaChannel := TgmAlphaChannel.Create(FLayers,
                                          LLayerIndex,
                                          AChannelBmp,
                                          FChannelLayerLocation,
                                          FDefaultMaskColor);

  LAlphaChannel.OnThumbnailUpdate := FOnChannelThumbUpdate;

  Result := FAlphaChannelList.Insert(AIndex, LAlphaChannel, True);

  if Assigned(FOnInsertAlphaChannel) then
  begin
    FOnInsertAlphaChannel(FAlphaChannelList, Result);
  end;
end;

function TgmCustomChannelManager.MoveAlphaChannel(
  const AOldIndex, ANewIndex: Integer): Boolean;
var
  LChannel       : TgmAlphaChannel;
  LNewLayerIndex : Integer;
begin
  LChannel       := TgmAlphaChannel(FAlphaChannelList.Channels[AOldIndex]);
  LNewLayerIndex := TgmAlphaChannel(FAlphaChannelList.Channels[ANewIndex]).ChannelLayer.Index;

  // move channel panel first ...
  Result := FAlphaChannelList.Move(AOldIndex, ANewIndex);

  // if moving successed, move the corresponding layer of alpha channel
  if Result then
  begin
    LChannel.ChannelLayer.Index := LNewLayerIndex;

    if Assigned(FOnAlphaChannelOrderChanged) then
    begin
      FOnAlphaChannelOrderChanged(FAlphaChannelList, AOldIndex, ANewIndex);
    end;
  end;
end;

procedure TgmCustomChannelManager.ResizeChannels(
  const ANewWidth, ANewHeight: Integer);
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel.ResizeChannel(ANewWidth, ANewHeight);
  end;

  if Assigned(FQuickMaskChannel) then
  begin
    FQuickMaskChannel.ResizeChannel(ANewWidth, ANewHeight);
  end;

  if FAlphaChannelList.Count > 0 then
  begin
    for i := 0 to (FAlphaChannelList.Count - 1) do
    begin
      LAlphaChannel := TgmAlphaChannel(FAlphaChannelList.Channels[i]);
      LAlphaChannel.ResizeChannel(ANewWidth, ANewHeight);
    end;
  end;
end;

procedure TgmCustomChannelManager.ResizeCanvasOfChannels(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection);
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel.ResizeChannelCanvas(ANewWidth, ANewHeight,
                                          AAnchor, $FFFFFFFF);
  end;

  if Assigned(FQuickMaskChannel) then
  begin
    FQuickMaskChannel.ResizeChannelCanvas(ANewWidth, ANewHeight,
                                          AAnchor, $FF000000);
  end;

  if FAlphaChannelList.Count > 0 then
  begin
    for i := 0 to (FAlphaChannelList.Count - 1) do
    begin
      LAlphaChannel := TgmAlphaChannel(FAlphaChannelList.Channels[i]);

      LAlphaChannel.ResizeChannelCanvas(ANewWidth, ANewHeight,
                                        AAnchor, $FF000000);
    end;
  end;
end;

procedure TgmCustomChannelManager.RotateChannels(const ADegrees: Integer;
  const ADirection: TgmRotateDirection);
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel.Rotate(ADegrees, ADirection, clWhite32);
  end;

  if Assigned(FQuickMaskChannel) then
  begin
    FQuickMaskChannel.Rotate(ADegrees, ADirection, clBlack32);
  end;

  if FAlphaChannelList.Count > 0 then
  begin
    for i := 0 to (FAlphaChannelList.Count - 1) do
    begin
      LAlphaChannel := TgmAlphaChannel(FAlphaChannelList.Channels[i]);

      LAlphaChannel.Rotate(ADegrees, ADirection, clBlack32);
    end;
  end;
end;

procedure TgmCustomChannelManager.SetChannelLayerBaseIndex(
  const AValue: Integer);
begin
  if (AValue >= 0) and (AValue <> FChannelLayerBaseIndex) then
  begin
    FChannelLayerBaseIndex := AValue;
  end; 
end;

procedure TgmCustomChannelManager.SetChannelLayerLocation(
  const ALocation: TFloatRect);
var
  i        : Integer;
  LChannel : TgmAlphaChannel;
begin
  if (FChannelLayerLocation.Left   <> ALocation.Left) or
     (FChannelLayerLocation.Top    <> ALocation.Top) or
     (FChannelLayerLocation.Right  <> ALocation.Right) or
     (FChannelLayerLocation.Bottom <> ALocation.Bottom) then
  begin
    FChannelLayerLocation := ALocation;

    if Assigned(FLayerMaskChannel) then
    begin
      FLayerMaskChannel.ChannelLayer.Location := ALocation;
    end;

    if Assigned(FQuickMaskChannel) then
    begin
      FQuickMaskChannel.ChannelLayer.Location := ALocation;
    end;

    if FAlphaChannelList.Count > 0 then
    begin
      for i := 0 to (FAlphaChannelList.Count - 1) do
      begin
        LChannel := TgmAlphaChannel(FAlphaChannelList.Channels[i]);
        LChannel.ChannelLayer.Location := ALocation;
      end;
    end;
  end;
end;

// no callback function calls version of AddNewAlphaChannel() 
procedure TgmCustomChannelManager.SimplyAddNewAlphaChannel(
  const AChannelBmpWidth, AChannelBmpHeight: Integer;
  const AAccumCount: Boolean = True);
var
  LLayerIndex   : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if not Assigned(FLayers) then
  begin
    raise Exception.Create('[Error] TgmCustomChannelManager.SimplyAddNewAlphaChannel(): field FLayers is nil. ');
  end;

  LLayerIndex := FChannelLayerBaseIndex;
  
  if Assigned(FLayerMaskChannel) then
  begin
    Inc(LLayerIndex);
  end;

  LLayerIndex := LLayerIndex + FAlphaChannelList.Count;
  
  LAlphaChannel := TgmAlphaChannel.Create(FLayers,
                                          LLayerIndex,
                                          AChannelBmpWidth,
                                          AChannelBmpHeight,
                                          FChannelLayerLocation,
                                          FDefaultMaskColor);

  LAlphaChannel.OnThumbnailUpdate := FOnChannelThumbUpdate;

  FAlphaChannelList.Add(LAlphaChannel, AAccumCount);
end;

// no callback function calls version of DeleteAlphaChannel()
procedure TgmCustomChannelManager.SimplyDeleteAlphaChannel(
  const AIndex: Integer);
begin
  FAlphaChannelList.DeleteChannel(AIndex);
end;

// no callback function calls version of DeleteQuickMaskChannel()
procedure TgmCustomChannelManager.SimplyDeleteQuickMaskChannel;
begin
  if Assigned(FQuickMaskChannel) then
  begin
    FreeAndNil(FQuickMaskChannel);
  end;
end;

// no callback function calls version of MoveAlphaChannel()
function TgmCustomChannelManager.SimplyMoveAlphaChannel(
  const AOldIndex, ANewIndex: Integer): Boolean;
var
  LChannel       : TgmAlphaChannel;
  LNewLayerIndex : Integer;
begin
  LChannel       := TgmAlphaChannel(FAlphaChannelList.Channels[AOldIndex]);
  LNewLayerIndex := TgmAlphaChannel(FAlphaChannelList.Channels[ANewIndex]).ChannelLayer.Index;

  // move channel panel first ...
  Result := FAlphaChannelList.Move(AOldIndex, ANewIndex);

  // if moving successed, move the corresponding layer of alpha channel
  if Result then
  begin
    LChannel.ChannelLayer.Index := LNewLayerIndex;
  end;
end;

// no callback functions calls version of InsertNewAlphaChannel()
function TgmCustomChannelManager.SimplyInsertNewAlphaChannel(
  AIndex: Integer; AChannelBmp: TBitmap32;
  const AAccumCount: Boolean = True): Integer;
var
  LLayerIndex   : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if not Assigned(FLayers) then
  begin
    raise Exception.Create('[Error] TgmCustomChannelManager.SimplyNewAlphaChannel(): field FLayers is nil. ');
  end;

  LLayerIndex := FChannelLayerBaseIndex;
  
  if Assigned(FLayerMaskChannel) then
  begin
    Inc(LLayerIndex);
  end;

  if AIndex < 0 then
  begin
    AIndex := 0;
  end
  else if AIndex > FAlphaChannelList.Count then
  begin
    AIndex := FAlphaChannelList.Count;
  end;
  
  LLayerIndex := LLayerIndex + AIndex;
  
  LAlphaChannel := TgmAlphaChannel.Create(FLayers,
                                          LLayerIndex,
                                          AChannelBmp,
                                          FChannelLayerLocation,
                                          FDefaultMaskColor);

  LAlphaChannel.OnThumbnailUpdate := FOnChannelThumbUpdate;

  Result := FAlphaChannelList.Insert(AIndex, LAlphaChannel, AAccumCount);
end;

// update color channel thumbnails respectively with the passed bitmap
procedure TgmCustomChannelManager.UpdateColorChannelThumbnails(
  ABitmap: TCustomBitmap32);
var
  i        : Integer;
  LChannel : TgmCustomChannel;
begin
  if Assigned(ABitmap) and (FColorChannelList.Count > 0) then
  begin
    for i := 0 to (FColorChannelList.Count - 1) do
    begin
      LChannel := TgmCustomChannel(FColorChannelList.Channels[i]);
      LChannel.UpdateChannelThumbnail(ABitmap);
    end;

    if Assigned(FOnChannelThumbUpdate) then
    begin
      FOnChannelThumbUpdate(Self);
    end;
  end;
end;


end.
