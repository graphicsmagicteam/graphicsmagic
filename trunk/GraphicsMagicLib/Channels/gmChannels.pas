unit gmChannels;

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
 * The Original Code is igChannels.pas.
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2016/12/27

interface

{$WARN UNSAFE_CODE OFF}

uses
{ Standard }
  Classes,
  Contnrs,
{ Graphics32 }
  GR32,
  GR32_Layers,
{ GraphicsMagicLib }
  gmCrop,
  gmTypes;

type
  TgmMaskColorIndicator = (mciMaskedArea, mciSelectedArea);
  TgmMaskColorType      = (mctColor, mctGrayscale);

  { TgmCustomChannel }
  
  TgmCustomChannel = class(TObject)
  protected
    FChannelThumb   : TBitmap32;
    FSelected       : Boolean;   // indicate whether the channel is selected
    FChannelVisible : Boolean;   // indicate whether the channel is visible
    FChannelName    : string;

    FOnThumbUpdate  : TNotifyEvent;

    function GetRealThumbRect(ASrcBitmap: TCustomBitmap32;
      const AMarginSize: Integer = 4): TRect;

    function GetThumbZoomScale(
      const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer): Single;

    procedure SetChannelVisible(const AValue: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateChannelThumbnail(ABitmap: TCustomBitmap32); virtual; abstract;

    property ChannelName       : string       read FChannelName    write FChannelName;
    property ChannelThumbnail  : TBitmap32    read FChannelThumb;
    property IsChannelVisible  : Boolean      read FChannelVisible write SetChannelVisible;
    property IsSelected        : Boolean      read FSelected       write FSelected;
    property OnThumbnailUpdate : TNotifyEvent read FOnThumbUpdate  write FOnThumbUpdate;
  end;

  { TgmAlphaChannel }

  TgmAlphaChannel = class(TgmCustomChannel)
  private
    FChannelLayer       : TBitmapLayer;
    FMaskColor          : TColor32;
    FMaskColorIndicator : TgmMaskColorIndicator;
    FMaskColorType      : TgmMaskColorType;

    function GetMaskOpacity: Byte;

    procedure ChannelLayerBlend(F: TColor32; var B: TColor32; M: TColor32);
    procedure SetMaskColorIndicator(AValue: TgmMaskColorIndicator);
    procedure SetMaskOpacity(AValue: Byte);
  protected
    procedure SetChannelVisible(const AValue: Boolean); override;
  public
    constructor Create; overload;

    constructor Create(ACollection: TLayerCollection;
      const ALayerIndex, ALayerBmpWidth, ALayerBmpHeight: Integer;
      const ALayerLocation: TFloatRect; const AMaskColor: TColor32); overload;

    constructor Create(ACollection: TLayerCollection;
      const ALayerIndex: Integer; AChannelBmp: TBitmap32;
      const ALayerLocation: TFloatRect; const AMaskColor: TColor32); overload;

    destructor Destroy; override;

    procedure Crop(ACrop: TgmCrop; const ABackColor: TColor32); overload;
    procedure Crop(const ACropArea: TRect; const ABackColor: TColor32); overload;

    procedure ResizeChannel(const ANewWidth, ANewHeight: Integer);

    procedure ResizeChannelCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackColor: TColor32);

    procedure Rotate(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);

    procedure UpdateChannelThumbnail; reintroduce;

    property ChannelLayer       : TBitmapLayer          read FChannelLayer;
    property MaskColor          : TColor32              read FMaskColor          write FMaskColor;
    property MaskColorIndicator : TgmMaskColorIndicator read FMaskColorIndicator write SetMaskColorIndicator;
    property MaskColorType      : TgmMaskColorType      read FMaskColorType      write FMaskColorType;
    property MaskOpacity        : Byte                  read GetMaskOpacity      write SetMaskOpacity;
  end;

  { TgmChannelList }

  TgmChannelList = class(TObject)
  protected
    FItems           : TObjectList;
    FSelectedIndex   : Integer;
    FSelectedChannel : TgmCustomChannel;

    function GetChannel(AIndex: Integer): TgmCustomChannel;
    function GetFirstSelectedChannelIndex: Integer;
    function GetChannelCount: Integer;
    function GetChannelMaxIndex: Integer;
    function GetSelectedChannelCount: Integer;
    function GetVisibleChannelCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AChannel: TgmCustomChannel);
    procedure Clear;
    procedure DeleteChannel(const AIndex: Integer);
    procedure DeleteSelectedChannels;
    procedure DeselectAllChannels;
    procedure SelectAllChannels;
    procedure SelectChannel(const AIndex: Integer; const AMultiSelect: Boolean);
    procedure ShowAllChannels;
    procedure HideAllChannels;

    function IsValidIndex(const AIndex: Integer): Boolean;
    function Move(ACurIndex, ANewIndex: Integer): Boolean; virtual;

    property Count                     : Integer          read GetChannelCount;
    property FirstSelectedIndex        : Integer          read GetFirstSelectedChannelIndex;
    property MaxIndex                  : Integer          read GetChannelMaxIndex;
    property Channels[AIndex: Integer] : TgmCustomChannel read GetChannel;
    property SelectedChannelCount      : Integer          read GetSelectedChannelCount;
    property SelectedIndex             : Integer          read FSelectedIndex;
    property SelectedChannel           : TgmCustomChannel read FSelectedChannel;
    property VisibleChannelCount       : Integer          read GetVisibleChannelCount;
  end;

  { TgmAlphaChannelList }

  TgmAlphaChannelList = class(TgmChannelList)
  private
    FAccumulatedCount : Integer;   // count how many alpha channels have been added to this list

    function GetSelectedAlphaChannel: TgmAlphaChannel;
    procedure SetAccumulatedCount(AValue: Integer);
  public
    constructor Create;

    function GetAlphlaChannelIndex(AChannel: TgmAlphaChannel): Integer;

    function Insert(const AIndex: Integer; AChannel: TgmCustomChannel;
      const AAccumulateCount: Boolean): Integer;

    procedure Add(AChannel: TgmAlphaChannel; const AAccumulateCount: Boolean); reintroduce;
    procedure SetMaskColorTypeForVisibleChannels(const AType: TgmMaskColorType);

    property AccumulatedCount : Integer         read FAccumulatedCount write SetAccumulatedCount;
    property SelectedChannel  : TgmAlphaChannel read GetSelectedAlphaChannel;
  end;

const
  CHANNEL_THUMB_SIZE = 36;

implementation

uses
{ Standard }
  SysUtils,
  Graphics,
{ external lib }
  GR32_Add_BlendModes,
{ GraphicsMagicLib }
  gmImageProcessFuncs;
  

{ TgmCustomChannel }

constructor TgmCustomChannel.Create;
begin
  inherited Create();

  FChannelThumb := TBitmap32.Create();
  with FChannelThumb do
  begin
    SetSize(CHANNEL_THUMB_SIZE, CHANNEL_THUMB_SIZE);
  end;

  FSelected       := True;
  FChannelVisible := True;
  FChannelName    := '';

  FOnThumbUpdate := nil;    
end;

destructor TgmCustomChannel.Destroy;
begin
  FOnThumbUpdate := nil;
  FChannelThumb.Free();

  inherited;
end;

function TgmCustomChannel.GetRealThumbRect(ASrcBitmap: TCustomBitmap32;
  const AMarginSize: Integer = 4): TRect;
var
  LThumbWidth  : Integer;
  LThumbHeight : Integer;
  LScale       : Single;
begin
  LScale := GetThumbZoomScale(ASrcBitmap.Width, ASrcBitmap.Height,
    CHANNEL_THUMB_SIZE - AMarginSize, CHANNEL_THUMB_SIZE - AMarginSize);

  LThumbWidth  := Round(ASrcBitmap.Width  * LScale);
  LThumbHeight := Round(ASrcBitmap.Height * LScale);

  with Result do
  begin
    Left   := (CHANNEL_THUMB_SIZE - LThumbWidth)  div 2;
    Top    := (CHANNEL_THUMB_SIZE - LThumbHeight) div 2;
    Right  := Left + LThumbWidth;
    Bottom := Top  + LThumbHeight;
  end;
end;

function TgmCustomChannel.GetThumbZoomScale(
  const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer): Single;
var
  ws, hs : Single;
begin
  if (ASrcWidth <= AThumbWidth) and (ASrcHeight <= AThumbHeight) then
  begin
    Result := 1.0;
  end
  else
  begin
    ws := AThumbWidth  / ASrcWidth;
    hs := AThumbHeight / ASrcHeight;

    if ws < hs then
    begin
      Result := ws;
    end
    else
    begin
      Result := hs;
    end;
  end;
end;

procedure TgmCustomChannel.SetChannelVisible(const AValue: Boolean);
begin
  if FChannelVisible <> AValue then
  begin
    FChannelVisible := AValue;
  end;
end;

{ TgmAlphaChannel }

constructor TgmAlphaChannel.Create;
begin
  inherited;

  FSelected           := False;
  FMaskColor          := clRed32;
  FMaskColorIndicator := mciMaskedArea;
  FMaskColorType      := mctColor;
end;

constructor TgmAlphaChannel.Create(ACollection: TLayerCollection;
  const ALayerIndex, ALayerBmpWidth, ALayerBmpHeight: Integer;
  const ALayerLocation: TFloatRect; const AMaskColor: TColor32);
begin
  if not Assigned(ACollection) then
  begin
    raise Exception.Create('[Error] TgmAlphaChannel.Create(): parameter ACollection is nil.');
  end;

  if ALayerIndex < 0 then
  begin
    raise Exception.Create('[Error] TgmAlphaChannel.Create(): parameter ALayerIndex less than zero.');
  end;

  Self.Create;
  FMaskColor := AMaskColor;

  ACollection.Insert(ALayerIndex, TBitmapLayer);

  FChannelLayer := TBitmapLayer(ACollection[ALayerIndex]);

  with FChannelLayer do
  begin
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := Self.ChannelLayerBlend;
    Bitmap.MasterAlpha    := 128;
    
    Bitmap.SetSize(ALayerBmpWidth, ALayerBmpHeight);
    Bitmap.Clear($FF000000);
    
    Location := ALayerLocation;
    Scaled   := True;
    Visible  := True;
  end;

  UpdateChannelThumbnail;
end;

constructor TgmAlphaChannel.Create(ACollection: TLayerCollection;
  const ALayerIndex: Integer; AChannelBmp: TBitmap32;
  const ALayerLocation: TFloatRect; const AMaskColor: TColor32);
begin
  if not Assigned(ACollection) then
  begin
    raise Exception.Create('[Error] TgmAlphaChannel.Create(): parameter ACollection is nil.');
  end;

  if not Assigned(AChannelBmp) then
  begin
    raise Exception.Create('[Error] TgmAlphaChannel.Create(): parameter AChannelBmp is nil.');
  end;

  if ALayerIndex < 0 then
  begin
    raise Exception.Create('[Error] TgmAlphaChannel.Create(): parameter ALayerIndex less than zero.');
  end;

  Self.Create;
  FMaskColor := AMaskColor;

  ACollection.Insert(ALayerIndex, TBitmapLayer);

  FChannelLayer := TBitmapLayer(ACollection[ALayerIndex]);

  with FChannelLayer do
  begin
    Bitmap.Assign(AChannelBmp);
    
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := Self.ChannelLayerBlend;
    Bitmap.MasterAlpha    := 128;
    
    Location := ALayerLocation;
    Scaled   := True;
    Visible  := True;
  end;

  UpdateChannelThumbnail;
end;

destructor TgmAlphaChannel.Destroy;
begin
  FChannelLayer.Free();

  inherited;
end;

procedure TgmAlphaChannel.ChannelLayerBlend(
  F: TColor32; var B: TColor32; M: TColor32);
var
  LAlpha   : Cardinal;
  LForeRGB : TColor32;
begin
  LAlpha := ( 255 - (F and $FF) ) shl 24;

  case FMaskColorType of
    mctColor:
      begin
        LForeRGB := LAlpha or (FMaskColor and $FFFFFF);
        
        Blendmode.NormalBlend(LForeRGB, B, M);
      end;

    mctGrayscale:
      begin
        B := F;
      end;
  end;
end;

procedure TgmAlphaChannel.Crop(ACrop: TgmCrop; const ABackColor: TColor32);
var
  LCropRect   : TRect;
  LCropBitmap : TBitmap32;
begin
  if Assigned(ACrop) then
  begin
    LCropBitmap := TBitmap32.Create;
    try
      LCropBitmap.DrawMode := dmBlend;

      LCropRect := Rect(ACrop.FCropStart.X, ACrop.FCropStart.Y,
                        ACrop.FCropEnd.X, ACrop.FCropEnd.Y);

      CopyRect32WithARGB(LCropBitmap, FChannelLayer.Bitmap, LCropRect, ABackColor);

      FChannelLayer.Bitmap.SetSize(LCropBitmap.Width, LCropBitmap.Height);
      CopyBitmap32(FChannelLayer.Bitmap, LCropBitmap);

      if ACrop.IsResized then
      begin
        if (FChannelLayer.Bitmap.Width  <> ACrop.ResizeW) or
           (FChannelLayer.Bitmap.Height <> ACrop.ResizeH) then
        begin
          SmoothResize32(FChannelLayer.Bitmap, ACrop.ResizeW, ACrop.ResizeH);
        end;
      end;

      UpdateChannelThumbnail();
    finally
      LCropBitmap.Free();
    end;
  end;
end;

procedure TgmAlphaChannel.Crop(const ACropArea: TRect;
  const ABackColor: TColor32);
var
  LCropBitmap : TBitmap32;
begin
  LCropBitmap := TBitmap32.Create();
  try
    LCropBitmap.DrawMode := dmBlend;

    CopyRect32WithARGB(LCropBitmap, FChannelLayer.Bitmap, ACropArea, ABackColor);
    FChannelLayer.Bitmap.SetSize(LCropBitmap.Width, LCropBitmap.Height);
    CopyBitmap32(FChannelLayer.Bitmap, LCropBitmap);
    UpdateChannelThumbnail();
  finally
    LCropBitmap.Free();
  end;
end;

function TgmAlphaChannel.GetMaskOpacity: Byte;
begin
  Result := FChannelLayer.Bitmap.MasterAlpha;
end;

procedure TgmAlphaChannel.ResizeChannel(const ANewWidth, ANewHeight: Integer);
begin
  if (ANewWidth <> FChannelLayer.Bitmap.Width) or
     (ANewHeight <> FChannelLayer.Bitmap.Height) then
  begin
    SmoothResize32(FChannelLayer.Bitmap, ANewWidth, ANewHeight);
    UpdateChannelThumbnail();
  end;
end;

procedure TgmAlphaChannel.ResizeChannelCanvas(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const ABackColor: TColor32);
var
  LCutBitmap : TBitmap32;
begin
  if (ANewWidth <> FChannelLayer.Bitmap.Width) or
     (ANewHeight <> FChannelLayer.Bitmap.Height) then
  begin
    LCutBitmap := TBitmap32.Create();
    try
      LCutBitmap.DrawMode := dmBlend;

      CutBitmap32ByAnchorDirection(FChannelLayer.Bitmap, LCutBitmap,
        ANewWidth, ANewHeight, AAnchor, ABackColor);

      FChannelLayer.Bitmap.SetSize(ANewWidth, ANewHeight);
      FChannelLayer.Bitmap.Clear(ABackColor);

      DrawBitmap32ByAnchorDirection(LCutBitmap, FChannelLayer.Bitmap, AAnchor);
      UpdateChannelThumbnail();
    finally
      LCutBitmap.Free();
    end;
  end;
end;

procedure TgmAlphaChannel.Rotate(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
var
  LSrcBmp : TBitmap32;
begin
  LSrcBmp := TBitmap32.Create();
  try
    LSrcBmp.Assign(FChannelLayer.Bitmap);
    RotateBitmap32(LSrcBmp, FChannelLayer.Bitmap, ADirection, ADegrees, 0, ABackgroundColor);
    UpdateChannelThumbnail();
  finally
    LSrcBmp.Free();
  end;
end;

procedure TgmAlphaChannel.SetChannelVisible(const AValue: Boolean);
begin
  inherited;

  FChannelLayer.Visible := FChannelVisible;
end;

procedure TgmAlphaChannel.SetMaskColorIndicator(
  AValue: TgmMaskColorIndicator);
var
  i       : Integer;
  r, g, b : Cardinal;
  p       : PColor32;
begin
  if FMaskColorIndicator <> AValue then
  begin
    FMaskColorIndicator := AValue;

    // invert channel layer map ...
    p := @FChannelLayer.Bitmap.Bits[0];

    for i := 1 to (FChannelLayer.Bitmap.Width * FChannelLayer.Bitmap.Height) do
    begin
      r := 255 - (p^ shr 16 and $FF);
      g := 255 - (p^ shr 8 and $FF);
      b := 255 - (p^ and $FF);

      p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;

      Inc(p);
    end;

    Self.UpdateChannelThumbnail;
  end;
end;

procedure TgmAlphaChannel.SetMaskOpacity(AValue: Byte);
begin
  if FChannelLayer.Bitmap.MasterAlpha <> AValue then
  begin
    FChannelLayer.Bitmap.MasterAlpha := AValue;
  end;
end;

procedure TgmAlphaChannel.UpdateChannelThumbnail;
var
  LRect : TRect;
  LBmp  : TBitmap32;
begin
  if Assigned(FChannelLayer) then
  begin
    LRect := Self.GetRealThumbRect(FChannelLayer.Bitmap);

    FChannelThumb.Clear( Color32(clBtnFace) );

    LBmp := TBitmap32.Create;
    try
      LBmp.Assign(FChannelLayer.Bitmap);
      LBmp.DrawMode := dmOpaque;

      FChannelThumb.Draw(LRect, LBmp.BoundsRect, LBmp);
    finally
      LBmp.Free;
    end;

    InflateRect(LRect, 1, 1);
    FChannelThumb.FrameRectS(LRect, clBlack32);

    if Assigned(FOnThumbUpdate) then
    begin
      FOnThumbUpdate(Self);
    end;
  end;
end;

{ TgmChannelList }

constructor TgmChannelList.Create;
begin
  inherited;

  FItems           := TObjectList.Create();
  FSelectedIndex   := -1;
  FSelectedChannel := nil;
end;

destructor TgmChannelList.Destroy;
begin
  FItems.Free();

  inherited;
end;

procedure TgmChannelList.Add(AChannel: TgmCustomChannel);
begin
  if Assigned(AChannel) then
  begin
    FItems.Add(AChannel);
  end;
end;

procedure TgmChannelList.Clear;
begin
  FItems.Clear();  
end;

procedure TgmChannelList.DeleteChannel(const AIndex: Integer);
begin
  if not IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  FItems.Delete(AIndex);
  DeselectAllChannels;
end;

procedure TgmChannelList.DeleteSelectedChannels;
var
  i : Integer;
begin
  if FItems.Count > 0 then
  begin
    for i := (FItems.Count - 1) downto 0 do
    begin
      if TgmCustomChannel(FItems.Items[i]).IsSelected then
      begin
        FItems.Delete(i);
      end;
    end;

    FSelectedIndex   := -1;
    FSelectedChannel := nil;
  end;
end;

procedure TgmChannelList.DeselectAllChannels;
var
  i        : Integer;
  LChannel : TgmCustomChannel;
begin
  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LChannel           := TgmCustomChannel(FItems.Items[i]);
      LChannel.FSelected := False;
    end;

    FSelectedChannel := nil;
    FSelectedIndex   := -1;
  end;
end;

function TgmChannelList.GetChannel(AIndex: Integer): TgmCustomChannel;
begin
  Result := nil;

  if IsValidIndex(AIndex) then
  begin
    Result := TgmCustomChannel(FItems.Items[AIndex]);
  end;
end;

function TgmChannelList.GetFirstSelectedChannelIndex: Integer;
var
  i : Integer;
begin
  Result := -1;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      if TgmCustomChannel(FItems.Items[i]).FSelected then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function TgmChannelList.GetChannelCount: Integer;
begin
  Result := FItems.Count;
end;

function TgmChannelList.GetChannelMaxIndex: Integer;
begin
  Result := FItems.Count - 1;
end;

function TgmChannelList.GetSelectedChannelCount: Integer;
var
  i : Integer;
begin
  Result := 0;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      if TgmCustomChannel(FItems.Items[i]).IsSelected then
      begin
        Inc(Result);
      end;
    end;
  end;
end;

function TgmChannelList.GetVisibleChannelCount: Integer;
var
  i : Integer;
begin
  Result := 0;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      if TgmCustomChannel(FItems.Items[i]).IsChannelVisible then
      begin
        Inc(Result);
      end;
    end;
  end;
end;

procedure TgmChannelList.HideAllChannels;
var
  i        : Integer;
  LChannel : TgmCustomChannel;
begin
  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LChannel                  := TgmCustomChannel(FItems.Items[i]);
      LChannel.IsChannelVisible := False;
    end;
  end;
end;

function TgmChannelList.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count);
end;

function TgmChannelList.Move(ACurIndex, ANewIndex: Integer): Boolean;
begin
  Result := False;
  
  if IsValidIndex(ACurIndex) and
     IsValidIndex(ANewIndex) and
     (ACurIndex <> ANewIndex) then
  begin
    FItems.Move(ACurIndex, ANewIndex);
    Result := True;
  end;
end;

procedure TgmChannelList.SelectAllChannels;
var
  i        : Integer;
  LChannel : TgmCustomChannel;
begin
  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LChannel           := TgmCustomChannel(FItems.Items[i]);
      LChannel.FSelected := True;
    end;

    FSelectedChannel := nil;
    FSelectedIndex   := -1;
  end;
end;

procedure TgmChannelList.SelectChannel(const AIndex: Integer;
  const AMultiSelect: Boolean);
begin
  if not IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  if not AMultiSelect then
  begin
    DeselectAllChannels;

    FSelectedIndex              := AIndex;
    FSelectedChannel            := TgmCustomChannel(FItems.Items[AIndex]);
    FSelectedChannel.IsSelected := True;
  end
  else
  begin
    TgmCustomChannel(FItems.Items[AIndex]).IsSelected := True;

    if GetSelectedChannelCount = 1 then
    begin
      // if the selected channel above is the only one in the list ...
      FSelectedIndex   := AIndex;
      FSelectedChannel := TgmCustomChannel(FItems.Items[AIndex]);
    end
    else
    begin
      FSelectedIndex   := -1;
      FSelectedChannel := nil;
    end;
  end;
end;

procedure TgmChannelList.ShowAllChannels;
var
  i        : Integer;
  LChannel : TgmCustomChannel;
begin
  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LChannel                  := TgmCustomChannel(FItems.Items[i]);
      LChannel.IsChannelVisible := True;
    end;
  end;
end;

{ TgmAlphaChannelList }

constructor TgmAlphaChannelList.Create;
begin
  inherited;

  FAccumulatedCount := 0;
end;

procedure TgmAlphaChannelList.Add(AChannel: TgmAlphaChannel;
  const AAccumulateCount: Boolean);
begin
  if Assigned(AChannel) then
  begin
    FItems.Add(AChannel);
  end;

  if AAccumulateCount then
  begin
    Inc(FAccumulatedCount);

    // give the added alpha channel a default name
    AChannel.ChannelName := 'Alpha ' + IntToStr(FAccumulatedCount);
  end;
end;

function TgmAlphaChannelList.GetAlphlaChannelIndex(
  AChannel: TgmAlphaChannel): Integer;
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  Result := -1;

  if Assigned(AChannel) then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LAlphaChannel := TgmAlphaChannel(Self.Channels[i]);
      if LAlphaChannel = AChannel then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function TgmAlphaChannelList.GetSelectedAlphaChannel: TgmAlphaChannel;
begin
  Result := nil;

  if Assigned(FSelectedChannel) then
  begin
    Result := TgmAlphaChannel(FSelectedChannel);
  end;
end;

// return the index that the channel inserted at,
// return -1 if failed
function TgmAlphaChannelList.Insert(const AIndex: Integer;
  AChannel: TgmCustomChannel; const AAccumulateCount: Boolean): Integer;
var
  LInsertedIndex : Integer;
begin
  LInsertedIndex := -1;

  if Assigned(AChannel) then
  begin
    LInsertedIndex := AIndex;
    if LInsertedIndex < 0 then
    begin
      LInsertedIndex := 0;
    end
    else if LInsertedIndex > FItems.Count then
    begin
      LInsertedIndex := FItems.Count;
    end;

    if AAccumulateCount then
    begin
      Inc(FAccumulatedCount);

      // give the inserted alpha channel a default name
      AChannel.ChannelName := 'Alpha ' + IntToStr(FAccumulatedCount);
    end;

    FItems.Insert(LInsertedIndex, AChannel);
  end;

  Result := LInsertedIndex;
end;

procedure TgmAlphaChannelList.SetAccumulatedCount(AValue: Integer);
begin
  if AValue >= 0 then
  begin
    FAccumulatedCount := AValue;
  end;
end;

procedure TgmAlphaChannelList.SetMaskColorTypeForVisibleChannels(
  const AType: TgmMaskColorType);
var
  i             : Integer;
  LAlphaChannel : TgmAlphaChannel;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to Self.MaxIndex do
    begin
      LAlphaChannel := TgmAlphaChannel(Self.Channels[i]);

      if LAlphaChannel.IsChannelVisible then
      begin
        LAlphaChannel.MaskColorType := AType;
      end;
    end;
  end;
end;


end.
