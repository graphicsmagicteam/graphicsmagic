unit gmRGBChannelManager;

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
 * The Original Code is igRGBChannelManager.pas.
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2016/02/27

interface

{$WARN UNSAFE_CODE OFF}

uses
{ Standard }
  Classes,
{ Graphics32 }
  GR32,
{ GraphicsMagic lib }
  gmChannels,
  gmChannelManager;

type
  { TgmRGBChannel }

  TgmRGBChannel = class(TgmCustomChannel)
  public
    constructor Create;
    procedure UpdateChannelThumbnail(ABitmap: TCustomBitmap32); override;
  end;

  { TgmRedChannel }

  TgmRedChannel = class(TgmCustomChannel)
  public
    constructor Create;
    procedure UpdateChannelThumbnail(ABitmap: TCustomBitmap32); override;
  end;

  { TgmGreenChannel }
  
  TgmGreenChannel = class(TgmCustomChannel)
  public
    constructor Create;
    procedure UpdateChannelThumbnail(ABitmap: TCustomBitmap32); override;
  end;

  { TgmBlueChannel }

  TgmBlueChannel = class(TgmCustomChannel)
  public
    constructor Create;
    procedure UpdateChannelThumbnail(ABitmap: TCustomBitmap32); override;
  end;

  { TgmRGBChannelManager }

  TgmRGBChannelManager = class(TgmCustomChannelManager)
  private
    function GetChannelPreviewColor(const ASrcColor: TColor32): TColor32;

    procedure DeselectAllAlphaChannels(const AHideChannel: Boolean);
    procedure DeselectAllChannels(const AHideChannel: Boolean);
    procedure DeselectColorChannel(const AIndex: Integer; const AHideChannel: Boolean);
    procedure DeselectLayerMask(const AHideChannel: Boolean);
    procedure DeselectQuickMask(const AHideChannel: Boolean);

    procedure InitColorChannels;
    procedure PreviewAllColorChannels;
    
    procedure SelectAllColorChannels;
    procedure SelectRedChannel;
    procedure SelectGreenChannel;
    procedure SelectBlueChannel;
  public
    constructor Create;

    procedure BlendByColorChannelSettings(ASrcBitmap, ADstBitmap: TBitmap32;
      const ARect: TRect); override;

    procedure SelectAlphaChannel(const AIndex: Integer; const AMultiSelect: Boolean); override;
    procedure SelectColorChannel(const AIndex: Integer; const AMultiSelect: Boolean); override;
    procedure SelectLayerMaskChannel; override;
    procedure SelectQuickMaskChannel; override;

    procedure SimplySelectAlphaChannel(const AIndex: Integer; const AMultiSelect: Boolean); override;

    procedure ToggleAlphaChannelVisible(const AIndex: Integer); override;
    procedure ToggleColorChannelVisible(const AIndex: Integer); override;
    procedure ToggleLayerMaskChannelVisible; override;
    procedure ToggleQuickMaskChannelVisible; override;

    function GetColorChannelGrayscaleMap(ABitmap: TCustomBitmap32): TBitmap32; override;
  end;

implementation

uses
{ Standard }
  SysUtils,
  Graphics,
  Math,
{ external lib}
  GR32_Add_BlendModes,
{ GraphicsMagic lib }
  gmTypes,
  gmPaintFuncs;

const
  HIDE_CHANNEL      : Boolean = True;
  DONT_HIDE_CHANNEL : Boolean = False;


{ TgmRGBChannel }

constructor TgmRGBChannel.Create;
begin
  inherited;

  FChannelName := 'RGB';
end;

procedure TgmRGBChannel.UpdateChannelThumbnail(ABitmap: TCustomBitmap32);
var
  LRect : TRect;
begin
  LRect := Self.GetRealThumbRect(ABitmap);
  
  FChannelThumb.Clear( Color32(clBtnFace) );
  DrawCheckerboardPattern(FChannelThumb, LRect, True);
  FChannelThumb.Draw(LRect, ABitmap.BoundsRect, ABitmap);

  InflateRect(LRect, 1, 1);
  FChannelThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end;
end;

{ TgmRedChannel }

constructor TgmRedChannel.Create;
begin
  inherited;

  FChannelName := 'Red';
end;

procedure TgmRedChannel.UpdateChannelThumbnail(ABitmap: TCustomBitmap32);
var
  LRect      : TRect;
  LScaledBmp : TBitmap32;
  LGrayBmp   : TBitmap32;
  i          : Integer;
  LGrayVal   : Cardinal;
  p1, p2     : PColor32;
begin
  LRect := Self.GetRealThumbRect(ABitmap);
  
  FChannelThumb.Clear( Color32(clBtnFace) );
  DrawCheckerboardPattern(FChannelThumb, LRect, True);

  LScaledBmp := TBitmap32.Create;
  LGrayBmp   := TBitmap32.Create;
  try
    LScaledBmp.SetSize(LRect.Right - LRect.Left + 1, LRect.Bottom - LRect.Top + 1);
    LGrayBmp.SetSizeFrom(LScaledBmp);

    // we can also have semi-transparent thumbnail bitmap
    LScaledBmp.DrawMode    := ABitmap.DrawMode;
    LScaledBmp.CombineMode := ABitmap.CombineMode;
    LGrayBmp.DrawMode      := ABitmap.DrawMode;
    LGrayBmp.CombineMode   := ABitmap.CombineMode;

    // scale the passed bitmap to the actual thumbnail size
    LScaledBmp.Draw(LScaledBmp.BoundsRect, ABitmap.BoundsRect, ABitmap);

    // extract red channel from the passed bitmap for making grayscale bitmap
    p1 := @LScaledBmp.Bits[0];
    p2 := @LGrayBmp.Bits[0];

    for i := 1 to (LScaledBmp.Width * LScaledBmp.Height) do
    begin
      LGrayVal := p1^ shr 16 and $FF; // extract red component as gray value
      p2^      := (p1^ and $FF000000) or (LGrayVal shl 16) or (LGrayVal shl 8) or LGrayVal;

      Inc(p1);
      Inc(p2);
    end;

    FChannelThumb.Draw(LRect.Left, LRect.Top, LGrayBmp);
  finally
    LScaledBmp.Free;
    LGrayBmp.Free;
  end;
  
  InflateRect(LRect, 1, 1);
  FChannelThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end;
end;

{ TgmGreenChannel }

constructor TgmGreenChannel.Create;
begin
  inherited;

  FChannelName := 'Green';
end;
  
procedure TgmGreenChannel.UpdateChannelThumbnail(ABitmap: TCustomBitmap32);
var
  LRect      : TRect;
  LScaledBmp : TBitmap32;
  LGrayBmp   : TBitmap32;
  i          : Integer;
  LGrayVal   : Cardinal;
  p1, p2     : PColor32;
begin
  LRect := Self.GetRealThumbRect(ABitmap);
  
  FChannelThumb.Clear( Color32(clBtnFace) );
  DrawCheckerboardPattern(FChannelThumb, LRect, True);

  LScaledBmp := TBitmap32.Create;
  LGrayBmp   := TBitmap32.Create;
  try
    LScaledBmp.SetSize(LRect.Right - LRect.Left + 1, LRect.Bottom - LRect.Top + 1);
    LGrayBmp.SetSizeFrom(LScaledBmp);

    // we can also have semi-transparent thumbnail bitmap
    LScaledBmp.DrawMode    := ABitmap.DrawMode;
    LScaledBmp.CombineMode := ABitmap.CombineMode;
    LGrayBmp.DrawMode      := ABitmap.DrawMode;
    LGrayBmp.CombineMode   := ABitmap.CombineMode;

    // scale the passed bitmap to the actual thumbnail size
    LScaledBmp.Draw(LScaledBmp.BoundsRect, ABitmap.BoundsRect, ABitmap);

    // extract green channel from the passed bitmap for making grayscale bitmap
    p1 := @LScaledBmp.Bits[0];
    p2 := @LGrayBmp.Bits[0];

    for i := 1 to (LScaledBmp.Width * LScaledBmp.Height) do
    begin
      LGrayVal := p1^ shr 8 and $FF; // extract green component as gray value
      p2^      := (p1^ and $FF000000) or (LGrayVal shl 16) or (LGrayVal shl 8) or LGrayVal;

      Inc(p1);
      Inc(p2);
    end;

    FChannelThumb.Draw(LRect.Left, LRect.Top, LGrayBmp);
  finally
    LScaledBmp.Free;
    LGrayBmp.Free;
  end;
  
  InflateRect(LRect, 1, 1);
  FChannelThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end;
end;

{ TgmBlueChannel }

constructor TgmBlueChannel.Create;
begin
  inherited;

  FChannelName := 'Blue';
end;

procedure TgmBlueChannel.UpdateChannelThumbnail(ABitmap: TCustomBitmap32);
var
  LRect      : TRect;
  LScaledBmp : TBitmap32;
  LGrayBmp   : TBitmap32;
  i          : Integer;
  LGrayVal   : Cardinal;
  p1, p2     : PColor32;
begin
  LRect := Self.GetRealThumbRect(ABitmap);
  
  FChannelThumb.Clear( Color32(clBtnFace) );
  DrawCheckerboardPattern(FChannelThumb, LRect, True);

  LScaledBmp := TBitmap32.Create;
  LGrayBmp   := TBitmap32.Create;
  try
    LScaledBmp.SetSize(LRect.Right - LRect.Left + 1, LRect.Bottom - LRect.Top + 1);
    LGrayBmp.SetSizeFrom(LScaledBmp);

    // we can also have semi-transparent thumbnail bitmap
    LScaledBmp.DrawMode    := ABitmap.DrawMode;
    LScaledBmp.CombineMode := ABitmap.CombineMode;
    LGrayBmp.DrawMode      := ABitmap.DrawMode;
    LGrayBmp.CombineMode   := ABitmap.CombineMode;

    // scale the passed bitmap to the actual thumbnail size
    LScaledBmp.Draw(LScaledBmp.BoundsRect, ABitmap.BoundsRect, ABitmap);

    // extract blue channel from the passed bitmap for making grayscale bitmap
    p1 := @LScaledBmp.Bits[0];
    p2 := @LGrayBmp.Bits[0];

    for i := 1 to (LScaledBmp.Width * LScaledBmp.Height) do
    begin
      LGrayVal := p1^ and $FF; // extract blue component as gray value
      p2^      := (p1^ and $FF000000) or (LGrayVal shl 16) or (LGrayVal shl 8) or LGrayVal;

      Inc(p1);
      Inc(p2);
    end;

    FChannelThumb.Draw(LRect.Left, LRect.Top, LGrayBmp);
  finally
    LScaledBmp.Free;
    LGrayBmp.Free;
  end;
  
  InflateRect(LRect, 1, 1);
  FChannelThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end;
end;

{ TgmRGBChannelManager }

constructor TgmRGBChannelManager.Create;
begin
  inherited;

  // create color channels for RGB color space 
  InitColorChannels;

  FChannelPreviewSet     := [csRed, csGreen, csBlue];
  FSelectedColorChannels := [csRed, csGreen, csBlue];
end;

{$RANGECHECKS OFF}
procedure TgmRGBChannelManager.BlendByColorChannelSettings(
  ASrcBitmap, ADstBitmap: TBitmap32; const ARect: TRect);
var
  x, y      : Integer;
  LRect     : TRect;
  LSrcColor : TColor32;
  sp, dp    : PColor32Array;
begin
  if ( not Assigned(ASrcBitmap) ) or
     ( ASrcBitmap.Width <> ADstBitmap.Width) or
     ( ASrcBitmap.Height <> ADstBitmap.Height) then
  begin
    Exit;
  end;

  LRect.Left   := Min(ARect.Left, ARect.Right);
  LRect.Right  := Max(ARect.Left, ARect.Right);
  LRect.Top    := Min(ARect.Top, ARect.Bottom);
  LRect.Bottom := Max(ARect.Top, ARect.Bottom);

  if (LRect.Left >= ADstBitmap.Width) or
     (LRect.Right <= 0) or
     (LRect.Top >= ADstBitmap.Height) or
     (LRect.Bottom <= 0) then
  begin
    Exit;  // do nothing if the rect is out of the range
  end;

  for y := LRect.Top to (LRect.Bottom - 1) do
  begin
    if (y < 0) or (y >= ADstBitmap.Height) then
    begin
      Continue;
    end;

    sp := PColor32Array(ASrcBitmap.Scanline[y]);
    dp := PColor32Array(ADstBitmap.Scanline[y]);

    for x := LRect.Left to (LRect.Right - 1) do
    begin
      if (x < 0) or (x >= ADstBitmap.Width) then
      begin
        Continue;
      end;

      if (sp[x] shr 24 and $FF) > 0 then
      begin
        LSrcColor := Self.GetChannelPreviewColor(sp[x]);
        BlendMode.NormalBlend(LSrcColor, dp[x], 255);
      end;
    end;
  end;
end;
{$RANGECHECKS ON}

procedure TgmRGBChannelManager.DeselectAllAlphaChannels(
  const AHideChannel: Boolean);
begin
  if FAlphaChannelList.Count > 0 then
  begin
    FAlphaChannelList.DeselectAllChannels();

    if AHideChannel then
    begin
      FAlphaChannelList.HideAllChannels();
    end;
  end;
end;

procedure TgmRGBChannelManager.DeselectAllChannels(
  const AHideChannel: Boolean);
var
  i : Integer;
begin
  // red, green and blue channels ...
  for i := 1 to 3 do
  begin
    DeselectColorChannel(i, AHideChannel);
  end;

  DeselectAllAlphaChannels(AHideChannel);
  DeselectLayerMask(AHideChannel);
  DeselectQuickMask(AHideChannel);
end;

procedure TgmRGBChannelManager.DeselectColorChannel(const AIndex: Integer;
  const AHideChannel: Boolean);
begin
  // only process red, green and blue channels ...
  if (AIndex > 0) and (AIndex < 4) then
  begin
    case AIndex of
      1: // red channel
        begin
          FSelectedColorChannels := FSelectedColorChannels - [csRed];

          if AHideChannel then
          begin
            FChannelPreviewSet := FChannelPreviewSet - [csRed];
          end;
        end;

      2: // green channel
        begin
          FSelectedColorChannels := FSelectedColorChannels - [csGreen];

          if AHideChannel then
          begin
            FChannelPreviewSet := FChannelPreviewSet - [csGreen];
          end;
        end;

      3: // blue channel
        begin
          FSelectedColorChannels := FSelectedColorChannels - [csBlue];

          if AHideChannel then
          begin
            FChannelPreviewSet := FChannelPreviewSet - [csBlue];
          end;
        end;
    end;

    // red or green or blue channel
    ColorChannelList.Channels[AIndex].IsSelected := False;
    if AHideChannel then
    begin
      ColorChannelList.Channels[AIndex].IsChannelVisible := False;
    end;

    with ColorChannelList.Channels[0] do
    begin
      // If any of the red, green and blue channel is deselected,
      // the RGB full-channel should be deselected either.
      IsSelected := False;

      IsChannelVisible := (csRed   in FChannelPreviewSet) and
                          (csGreen in FChannelPreviewSet) and
                          (csBlue  in FChannelPreviewSet);
    end;
  end;
end;

procedure TgmRGBChannelManager.DeselectLayerMask(const AHideChannel: Boolean);
begin
  if Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel.IsSelected := False;

    if AHideChannel then
    begin
      FLayerMaskChannel.IsChannelVisible := False;
    end;
  end;
end;

procedure TgmRGBChannelManager.DeselectQuickMask(const AHideChannel: Boolean);
begin
  if Assigned(FQuickMaskChannel) then
  begin
    FQuickMaskChannel.IsSelected := False;

    if AHideChannel then
    begin
      FQuickMaskChannel.IsChannelVisible := False;
    end;
  end;
end;

function TgmRGBChannelManager.GetChannelPreviewColor(
  const ASrcColor: TColor32): TColor32;
var
  rr, gg, bb    : Cardinal;
  LChannelCount : Integer;
  LResultRGB    : TColor32;
begin
  Result := ASrcColor;

  if (ASrcColor shr 24 and $FF) = $0 then
  begin
    Exit;
  end;

  rr := 0;
  gg := 0;
  bb := 0;

  LResultRGB    := $0;
  LChannelCount := 0;

  if csRed in FChannelPreviewSet then
  begin
    rr := ASrcColor shr 16 and $FF;
    Inc(LChannelCount);
  end;

  if csGreen in FChannelPreviewSet then
  begin
    gg := ASrcColor shr 8 and $FF;
    Inc(LChannelCount);
  end;

  if csBlue in FChannelPreviewSet then
  begin
    bb := ASrcColor and $FF;
    Inc(LChannelCount);
  end;

  if LChannelCount = 0 then
  begin
    Result := $FFFFFFFF;
  end
  else
  begin
    if LChannelCount = 1 then
    begin
      LResultRGB := LResultRGB or (rr shl 16) or (rr shl 8) or rr;
      LResultRGB := LResultRGB or (gg shl 16) or (gg shl 8) or gg;
      LResultRGB := LResultRGB or (bb shl 16) or (bb shl 8) or bb;
    end
    else
    begin
      LResultRGB := (rr shl 16) or (gg shl 8) or bb;
    end;

    Result := (ASrcColor and $FF000000) or LResultRGB;
  end;
end;

function TgmRGBChannelManager.GetColorChannelGrayscaleMap(
  ABitmap: TCustomBitmap32): TBitmap32;
var
  i, LChannelCount : Integer;
  r, g, b, LGray   : Byte;
  LSrcBit          : PColor32;
  LResultBit       : PColor32;
begin
  if not Assigned(ABitmap) then
  begin
    raise Exception.Create('[Error] TgmRGBChannelManager.GetColorChannelGrayscaleMap(): Parameter ABitmap is nil.');
  end;

  if (ABitmap.Width <= 0) or (ABitmap.Height <= 0) then
  begin
    raise Exception.Create('[Error] TgmRGBChannelManager.GetColorChannelGrayscaleMap(): The size of parameter ABitmap is invalid.');
  end;

  LChannelCount := 0;
  
  if csRed in FSelectedColorChannels then
  begin
    Inc(LChannelCount);
  end;

  if csGreen in FSelectedColorChannels then
  begin
    Inc(LChannelCount);
  end;

  if csBlue in FSelectedColorChannels then
  begin
    Inc(LChannelCount);
  end;

  if LChannelCount = 0 then
  begin
    raise Exception.Create('[Error] TgmRGBChannelManager.GetColorChannelGrayscaleMap(): No color channel selected.');
  end;

  Result := TBitmap32.Create;
  Result.SetSizeFrom(ABitmap);
  Result.Clear($FF000000);

  LSrcBit    := @ABitmap.Bits[0];
  LResultBit := @Result.Bits[0];

  for i := 1 to (Result.Width * Result.Height) do
  begin
    r := 0;
    g := 0;
    b := 0;

    if csRed in FSelectedColorChannels then
    begin
      r := LSrcBit^ shr 16 and $FF;
    end;

    if csGreen in FSelectedColorChannels then
    begin
      g := LSrcBit^ shr 8 and $FF;
    end;

    if csBlue in FSelectedColorChannels then
    begin
      b := LSrcBit^ and $FF;
    end;

    LGray       := (r + g + b) div LChannelCount;
    LResultBit^ := $FF000000 or (LGray shl 16) or (LGray shl 8) or LGray;

    Inc(LSrcBit);
    Inc(LResultBit);
  end;
end;

procedure TgmRGBChannelManager.InitColorChannels;
begin
  // create color channels for RGB color space
  FColorChannelList.Add( TgmRGBChannel.Create() );
  FColorChannelList.Add( TgmRedChannel.Create() );
  FColorChannelList.Add( TgmGreenChannel.Create() );
  FColorChannelList.Add( TgmBlueChannel.Create() );
end;

procedure TgmRGBChannelManager.PreviewAllColorChannels;
var
  i : Integer;
begin
  FChannelPreviewSet := [csRed, csGreen, csBlue];

  for i := 0 to 3 do
  begin
    FColorChannelList.Channels[i].IsChannelVisible := True;
  end;

  if Assigned(FOnChannelVisibleChanged) then
  begin
    FOnChannelVisibleChanged(ctColorChannel);
  end;
end;

procedure TgmRGBChannelManager.SelectAllColorChannels;
begin
  FSelectedColorChannels := [csRed, csGreen, csBlue];
  FChannelPreviewSet     := [csRed, csGreen, csBlue];

  FColorChannelList.SelectAllChannels();
  FColorChannelList.ShowAllChannels();
end;

procedure TgmRGBChannelManager.SelectAlphaChannel(const AIndex: Integer;
  const AMultiSelect: Boolean);
var
  i              : Integer;
  LMaskColorType : TgmMaskColorType;
begin
  if not FAlphaChannelList.IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  // if we can select multiple alpha channels at a time ...
  if FAlphaChannelMultiSelect then
  begin
    if not AMultiSelect then
    begin
      DeselectAllChannels(HIDE_CHANNEL);
    end;
  end
  else
  begin
    DeselectAllChannels(HIDE_CHANNEL);
  end;

  if FAlphaChannelMultiSelect and AMultiSelect then
  begin
    for i := 1 to 3 do
    begin
      DeselectColorChannel(i, DONT_HIDE_CHANNEL);
    end;

    DeselectLayerMask(DONT_HIDE_CHANNEL);
    DeselectQuickMask(DONT_HIDE_CHANNEL);

    if FAlphaChannelList.Channels[AIndex].IsSelected then
    begin
      if FAlphaChannelList.SelectedChannelCount > 1 then
      begin
        FAlphaChannelList.Channels[AIndex].IsSelected := False;
      end;
    end
    else
    begin
      FAlphaChannelList.SelectChannel(AIndex, True);
      FAlphaChannelList.Channels[AIndex].IsChannelVisible := True;
    end;
  end
  else
  begin
    FAlphaChannelList.SelectChannel(AIndex, False);
    FAlphaChannelList.Channels[AIndex].IsChannelVisible := True;
  end;

  if (FColorChannelList.VisibleChannelCount > 0) or
     (FAlphaChannelList.VisibleChannelCount > 1) or
     ( Assigned(FLayerMaskChannel) and FLayerMaskChannel.IsChannelVisible ) or
     ( Assigned(FQuickMaskChannel) and FQuickMaskChannel.IsChannelVisible ) then
  begin
    LMaskColorType := mctColor;
  end
  else
  begin
    LMaskColorType := mctGrayscale;
  end;

  // change mask color type for other channels ...

  FAlphaChannelList.SetMaskColorTypeForVisibleChannels(LMaskColorType);

  if Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel.MaskColorType := LMaskColorType;
  end;

  if Assigned(FQuickMaskChannel) then
  begin
    FQuickMaskChannel.MaskColorType := LMaskColorType;
  end;

  FCurrentChannelType := ctAlphaChannel;

  // invoke callback function 
  if Assigned(FOnSelectedChannelChanged) then
  begin
    FOnSelectedChannelChanged(ctAlphaChannel);
  end;
end;

procedure TgmRGBChannelManager.SelectBlueChannel;
begin
  FSelectedColorChannels := FSelectedColorChannels + [csBlue];
  FChannelPreviewSet     := FChannelPreviewSet + [csBlue];

  // blue channel
  FColorChannelList.Channels[3].IsSelected       := True;
  FColorChannelList.Channels[3].IsChannelVisible := True;

  if (csRed   in FSelectedColorChannels) and
     (csGreen in FSelectedColorChannels) and
     (csBlue  in FSelectedColorChannels) then
  begin
    FColorChannelList.Channels[0].IsSelected       := True;
    FColorChannelList.Channels[0].IsChannelVisible := True;
  end;
end;

procedure TgmRGBChannelManager.SelectColorChannel(const AIndex: Integer;
  const AMultiSelect: Boolean);
begin
  if not ColorChannelList.IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  if not AMultiSelect then
  begin
    DeselectAllChannels(HIDE_CHANNEL);
  end;

  case AIndex of
    0: // RGB composite
      begin
        if not FColorChannelList.Channels[0].IsSelected then
        begin
          // Deselect all alpha channels, layer mask and quick mask,
          // even if the parameter AMultiSelect is set to true.
          if AMultiSelect then
          begin
            DeselectAllAlphaChannels(DONT_HIDE_CHANNEL);
            DeselectLayerMask(DONT_HIDE_CHANNEL);
            DeselectQuickMask(DONT_HIDE_CHANNEL);
          end;

          SelectAllColorChannels;
        end;
      end;

    1: // red channel
      begin
        if AMultiSelect then
        begin
          // Deselect all alpha channels, layer mask and quick mask,
          // even if the user pressed the Shift key.
          DeselectAllAlphaChannels(DONT_HIDE_CHANNEL);
          DeselectLayerMask(DONT_HIDE_CHANNEL);
          DeselectQuickMask(DONT_HIDE_CHANNEL);

          if csRed in FSelectedColorChannels then
          begin
            if (csGreen in FSelectedColorChannels) or
               (csBlue  in FSelectedColorChannels) then
            begin
              DeselectColorChannel(1, HIDE_CHANNEL);
            end
            else
            begin
              SelectAllColorChannels;
            end;
          end
          else
          begin
            SelectRedChannel;
          end;
        end
        else
        begin
          SelectRedChannel;
        end;
      end;

    2: // green channel
      begin
        if AMultiSelect then
        begin
          // Deselect all alpha channels, layer mask and quick mask,
          // even if the user pressed the Shift key.
          DeselectAllAlphaChannels(DONT_HIDE_CHANNEL);
          DeselectLayerMask(DONT_HIDE_CHANNEL);
          DeselectQuickMask(DONT_HIDE_CHANNEL);

          if csGreen in FSelectedColorChannels then
          begin
            if (csRed  in FSelectedColorChannels) or
               (csBlue in FSelectedColorChannels) then
            begin
              DeselectColorChannel(2, HIDE_CHANNEL);
            end
            else
            begin
              SelectAllColorChannels;
            end;
          end
          else
          begin
            SelectGreenChannel;
          end;
        end
        else
        begin
          SelectGreenChannel;
        end;
      end;

    3: // blue
      begin
        if AMultiSelect then
        begin
          DeselectAllAlphaChannels(DONT_HIDE_CHANNEL);
          DeselectLayerMask(DONT_HIDE_CHANNEL);
          DeselectQuickMask(DONT_HIDE_CHANNEL);

          if csBlue in FSelectedColorChannels then
          begin
            if (csRed   in FSelectedColorChannels) or
               (csGreen in FSelectedColorChannels) then
            begin
              DeselectColorChannel(3, HIDE_CHANNEL);
            end
            else
            begin
              SelectAllColorChannels;
            end;
          end
          else
          begin
            SelectBlueChannel;
          end;
        end
        else
        begin
          SelectBlueChannel;
        end;
      end;
  end;

  // change mask color type for other channels ...

  FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctColor);

  if Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel.MaskColorType := mctColor;
  end;

  if Assigned(FQuickMaskChannel) then
  begin
    FQuickMaskChannel.MaskColorType := mctColor;
  end;

  FCurrentChannelType := ctColorChannel;

  // invoke callback function 
  if Assigned(FOnSelectedChannelChanged) then
  begin
    FOnSelectedChannelChanged(ctColorChannel);
  end;
end;

procedure TgmRGBChannelManager.SelectGreenChannel;
begin
  FSelectedColorChannels := FSelectedColorChannels + [csGreen];
  FChannelPreviewSet     := FChannelPreviewSet + [csGreen];

  // green channel
  FColorChannelList.Channels[2].IsSelected       := True;
  FColorChannelList.Channels[2].IsChannelVisible := True;

  if (csRed   in FSelectedColorChannels) and
     (csGreen in FSelectedColorChannels) and
     (csBlue  in FSelectedColorChannels) then
  begin
    FColorChannelList.Channels[0].IsSelected       := True;
    FColorChannelList.Channels[0].IsChannelVisible := True;
  end;
end;

procedure TgmRGBChannelManager.SelectRedChannel;
begin
  FSelectedColorChannels := FSelectedColorChannels + [csRed];
  FChannelPreviewSet     := FChannelPreviewSet + [csRed];

  // red channel
  FColorChannelList.Channels[1].IsSelected       := True;
  FColorChannelList.Channels[1].IsChannelVisible := True;

  if (csRed   in FSelectedColorChannels) and
     (csGreen in FSelectedColorChannels) and
     (csBlue  in FSelectedColorChannels) then
  begin
    FColorChannelList.Channels[0].IsSelected       := True;
    FColorChannelList.Channels[0].IsChannelVisible := True;
  end;
end;

procedure TgmRGBChannelManager.SelectLayerMaskChannel;
var
  i : Integer;
begin
  if not Assigned(FLayerMaskChannel) then
  begin
    Exit;
  end;

  for i := 1 to 3 do
  begin
    DeselectColorChannel(i, DONT_HIDE_CHANNEL);
  end;

  DeselectAllAlphaChannels(HIDE_CHANNEL);
  DeselectQuickMask(HIDE_CHANNEL);
  PreviewAllColorChannels;

  FCurrentChannelType          := ctLayerMaskChannel;
  FLayerMaskChannel.IsSelected := True;

  // invoke callback function 
  if Assigned(FOnSelectedChannelChanged) then
  begin
    FOnSelectedChannelChanged(ctLayerMaskChannel);
  end;
end;

procedure TgmRGBChannelManager.SelectQuickMaskChannel;
begin
  if not Assigned(FQuickMaskChannel) then
  begin
    Exit;
  end;

  DeselectAllChannels(DONT_HIDE_CHANNEL);
  
  FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctColor);

  if Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel.MaskColorType := mctColor;
  end;

  FCurrentChannelType                := ctQuickMaskChannel;
  FQuickMaskChannel.IsSelected       := True;
  FQuickMaskChannel.IsChannelVisible := True;
  FQuickMaskChannel.MaskColorType    := mctColor;

  // invoke callback function 
  if Assigned(FOnSelectedChannelChanged) then
  begin
    FOnSelectedChannelChanged(ctQuickMaskChannel);
  end;
end;

// no callback invoke version fo SelectAlphaChannel()
procedure TgmRGBChannelManager.SimplySelectAlphaChannel(const AIndex: Integer;
  const AMultiSelect: Boolean);
var
  i              : Integer;
  LMaskColorType : TgmMaskColorType;
begin
  if not FAlphaChannelList.IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  // if we can select multiple alpha channels at a time ...
  if FAlphaChannelMultiSelect then
  begin
    if not AMultiSelect then
    begin
      DeselectAllChannels(HIDE_CHANNEL);
    end;
  end
  else
  begin
    DeselectAllChannels(HIDE_CHANNEL);
  end;

  if FAlphaChannelMultiSelect and AMultiSelect then
  begin
    for i := 1 to 3 do
    begin
      DeselectColorChannel(i, DONT_HIDE_CHANNEL);
    end;

    DeselectLayerMask(DONT_HIDE_CHANNEL);
    DeselectQuickMask(DONT_HIDE_CHANNEL);

    if FAlphaChannelList.Channels[AIndex].IsSelected then
    begin
      if FAlphaChannelList.SelectedChannelCount > 1 then
      begin
        FAlphaChannelList.Channels[AIndex].IsSelected := False;
      end;
    end
    else
    begin
      FAlphaChannelList.SelectChannel(AIndex, True);
      FAlphaChannelList.Channels[AIndex].IsChannelVisible := True;
    end;
  end
  else
  begin
    FAlphaChannelList.SelectChannel(AIndex, False);
    FAlphaChannelList.Channels[AIndex].IsChannelVisible := True;
  end;

  if (FColorChannelList.VisibleChannelCount > 0) or
     (FAlphaChannelList.VisibleChannelCount > 1) or
     ( Assigned(FLayerMaskChannel) and FLayerMaskChannel.IsChannelVisible ) or
     ( Assigned(FQuickMaskChannel) and FQuickMaskChannel.IsChannelVisible ) then
  begin
    LMaskColorType := mctColor;
  end
  else
  begin
    LMaskColorType := mctGrayscale;
  end;

  // change mask color type for other channels ...

  FAlphaChannelList.SetMaskColorTypeForVisibleChannels(LMaskColorType);

  if Assigned(FLayerMaskChannel) then
  begin
    FLayerMaskChannel.MaskColorType := LMaskColorType;
  end;

  if Assigned(FQuickMaskChannel) then
  begin
    FQuickMaskChannel.MaskColorType := LMaskColorType;
  end;

  FCurrentChannelType := ctAlphaChannel;
end;

procedure TgmRGBChannelManager.ToggleAlphaChannelVisible(
  const AIndex: Integer);
var
  LVisibleAlphaChannelCount : Integer;
  LLayerMaskVisible         : Boolean;
  LQuickMaskVisible         : Boolean; 
  LAlphaChannel             : TgmAlphaChannel;
begin
  if not FAlphaChannelList.IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  LVisibleAlphaChannelCount := FAlphaChannelList.VisibleChannelCount;
  LLayerMaskVisible         := ( Assigned(FLayerMaskChannel) and FLayerMaskChannel.IsChannelVisible );
  LQuickMaskVisible         := ( Assigned(FQuickMaskChannel) and FQuickMaskChannel.IsChannelVisible );

  LAlphaChannel := TgmAlphaChannel(FAlphaChannelList.Channels[AIndex]);

  if LAlphaChannel.IsChannelVisible then
  begin
    if (FChannelPreviewSet <> []) or
       (LVisibleAlphaChannelCount > 1) or
       LQuickMaskVisible or
       LLayerMaskVisible then
    begin
      LAlphaChannel.IsChannelVisible := False;
    end;
  end
  else
  begin
    LAlphaChannel.IsChannelVisible := True;
  end;

  // regain the number of visible alpha channels
  LVisibleAlphaChannelCount := FAlphaChannelList.VisibleChannelCount;

  // change mask color type for layer mask channel
  if LLayerMaskVisible then
  begin
    if (FChannelPreviewSet = []) and
       (LVisibleAlphaChannelCount = 0) and
       (LQuickMaskVisible = False) then
    begin
      FLayerMaskChannel.MaskColorType := mctGrayscale;
    end
    else
    begin
      FLayerMaskChannel.MaskColorType := mctColor;
    end;
  end;

  // change mask color type for visible alpha channels
  if (FChannelPreviewSet = []) and
     (LVisibleAlphaChannelCount = 1) and
     (LQuickMaskVisible = False) and
     (LLayerMaskVisible = False) then
  begin
    FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctGrayscale);
  end
  else
  begin
    FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctColor);
  end;

  // change mask color type for quick mask channel
  if LQuickMaskVisible then
  begin
    if (FChannelPreviewSet = []) and
       (LVisibleAlphaChannelCount = 0) and
       (LLayerMaskVisible = False) then
    begin
      FQuickMaskChannel.MaskColorType := mctGrayscale;
    end
    else
    begin
      FQuickMaskChannel.MaskColorType := mctColor;
    end;
  end;

  if Assigned(FOnChannelVisibleChanged) then
  begin
    FOnChannelVisibleChanged(ctAlphaChannel);
  end;  
end;

procedure TgmRGBChannelManager.ToggleColorChannelVisible(const AIndex: Integer);
var
  LVisibleColorChannelCount : Integer; 
  LVisibleAlphaChannelCount : Integer;
  LQuickMaskVisible         : Boolean;
  LLayerMaskVisible         : Boolean;
begin
  if not FColorChannelList.IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  LVisibleColorChannelCount := Self.FColorChannelList.VisibleChannelCount;
  LVisibleAlphaChannelCount := Self.FAlphaChannelList.VisibleChannelCount;
  LLayerMaskVisible         := ( Assigned(FLayerMaskChannel) and FLayerMaskChannel.IsChannelVisible );
  LQuickMaskVisible         := ( Assigned(FQuickMaskChannel) and FQuickMaskChannel.IsChannelVisible );

  if AIndex = 0 then  // RGB channels
  begin
    FChannelPreviewSet := [csRed, csGreen, csBlue];
  end
  else if AIndex = 1 then  // red channel
  begin
    if csRed in FChannelPreviewSet then
    begin
      // determine whether we can exclude red channel in the preview set
      if (LVisibleColorChannelCount > 1) or
         (LVisibleAlphaChannelCount > 0) or
         LQuickMaskVisible or
         LLayerMaskVisible then
      begin
        FChannelPreviewSet := FChannelPreviewSet - [csRed];
      end;
    end
    else
    begin
      FChannelPreviewSet := FChannelPreviewSet + [csRed];
    end;
  end
  else if AIndex = 2 then  // green channel
  begin
    if csGreen in FChannelPreviewSet then
    begin
      // determine whether we can exclude green channel in the preview set
      if (LVisibleColorChannelCount > 1) or
         (LVisibleAlphaChannelCount > 0) or
         LQuickMaskVisible or
         LLayerMaskVisible then
      begin
        FChannelPreviewSet := FChannelPreviewSet - [csGreen];
      end;
    end
    else
    begin
      FChannelPreviewSet := FChannelPreviewSet + [csGreen];
    end;
  end
  else if AIndex = 3 then  // blue channel
  begin
    if csBlue in FChannelPreviewSet then
    begin
      // determine whether we can exclude blue channel in the preview set
      if (LVisibleColorChannelCount > 1) or
         (LVisibleAlphaChannelCount > 0) or
         LQuickMaskVisible or
         LLayerMaskVisible then
      begin
        FChannelPreviewSet := FChannelPreviewSet - [csBlue];
      end;
    end
    else
    begin
      FChannelPreviewSet := FChannelPreviewSet + [csBlue];
    end;
  end;

  FColorChannelList.Channels[0].IsChannelVisible := (FChannelPreviewSet = [csRed, csGreen, csBlue]);
  FColorChannelList.Channels[1].IsChannelVisible := (csRed   in FChannelPreviewSet);
  FColorChannelList.Channels[2].IsChannelVisible := (csGreen in FChannelPreviewSet);
  FColorChannelList.Channels[3].IsChannelVisible := (csBlue  in FChannelPreviewSet);

  // change mask color type for layer mask channel
  if LLayerMaskVisible then
  begin
    if (FChannelPreviewSet = []) and
       (LVisibleAlphaChannelCount = 0) and
       (LQuickMaskVisible = False) then
    begin
      FLayerMaskChannel.MaskColorType := mctGrayscale;
    end
    else
    begin
      FLayerMaskChannel.MaskColorType := mctColor;
    end;
  end;

  // change mask color type for visible alpha channels
  if (FChannelPreviewSet = []) and
     (LVisibleAlphaChannelCount = 1) and
     (LQuickMaskVisible = False) and
     (LLayerMaskVisible = False) then
  begin
    FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctGrayscale);
  end
  else
  begin
    FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctColor);
  end;

  // change mask color type for quick mask channel
  if LQuickMaskVisible then
  begin
    if (FChannelPreviewSet = []) and
       (LVisibleAlphaChannelCount = 0) and
       (LLayerMaskVisible = False) then
    begin
      FQuickMaskChannel.MaskColorType := mctGrayscale;
    end
    else
    begin
      FQuickMaskChannel.MaskColorType := mctColor;
    end;
  end;

  if Assigned(FOnChannelVisibleChanged) then
  begin
    FOnChannelVisibleChanged(ctColorChannel);
  end;
end;

procedure TgmRGBChannelManager.ToggleLayerMaskChannelVisible;
var
  LVisibleColorChannelCount : Integer; 
  LVisibleAlphaChannelCount : Integer;
  LQuickMaskVisible         : Boolean;
begin
  if not Assigned(FLayerMaskChannel) then
  begin
    Exit;
  end;

  LVisibleColorChannelCount := FColorChannelList.VisibleChannelCount;
  LVisibleAlphaChannelCount := FAlphaChannelList.VisibleChannelCount;
  LQuickMaskVisible         := ( Assigned(FQuickMaskChannel) and FQuickMaskChannel.IsChannelVisible );

  if FLayerMaskChannel.IsChannelVisible then
  begin
    if (LVisibleColorChannelCount > 0) or
       (LVisibleAlphaChannelCount > 0) or
       LQuickMaskVisible then
    begin
      FLayerMaskChannel.IsChannelVisible := False;
    end;
  end
  else
  begin
    FLayerMaskChannel.IsChannelVisible := True;
  end;

  // change mask color type for layer mask channel
  if FLayerMaskChannel.IsChannelVisible then
  begin
    if (FChannelPreviewSet = []) and
       (LVisibleAlphaChannelCount = 0) and
       (LQuickMaskVisible = False) then
    begin
      FLayerMaskChannel.MaskColorType := mctGrayscale;
    end
    else
    begin
      FLayerMaskChannel.MaskColorType := mctColor;
    end;
  end;

  // change mask color type for visible alpha channels
  if (FChannelPreviewSet = []) and
     (LVisibleAlphaChannelCount = 1) and
     (LQuickMaskVisible = False) and
     (FLayerMaskChannel.IsChannelVisible = False) then
  begin
    FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctGrayscale);
  end
  else
  begin
    FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctColor);
  end;

  // change mask color type for quick mask channel
  if LQuickMaskVisible then
  begin
    if (FChannelPreviewSet = []) and
       (LVisibleAlphaChannelCount = 0) and
       (FLayerMaskChannel.IsChannelVisible = False) then
    begin
      FQuickMaskChannel.MaskColorType := mctGrayscale;
    end
    else
    begin
      FQuickMaskChannel.MaskColorType := mctColor;
    end;
  end;

  if Assigned(FOnChannelVisibleChanged) then
  begin
    FOnChannelVisibleChanged(ctLayerMaskChannel);
  end;
end;

procedure TgmRGBChannelManager.ToggleQuickMaskChannelVisible;
var
  LVisibleColorChannelCount : Integer; 
  LVisibleAlphaChannelCount : Integer;
  LLayerMaskVisible         : Boolean;
begin
  if not Assigned(FQuickMaskChannel) then
  begin
    Exit;
  end;

  LVisibleColorChannelCount := FColorChannelList.VisibleChannelCount;
  LVisibleAlphaChannelCount := FAlphaChannelList.VisibleChannelCount;
  LLayerMaskVisible         := ( Assigned(FLayerMaskChannel) and FLayerMaskChannel.IsChannelVisible );

  if FQuickMaskChannel.IsChannelVisible then
  begin
    if (LVisibleColorChannelCount > 0) or
       (LVisibleAlphaChannelCount > 0) or
       LLayerMaskVisible then
    begin
      FQuickMaskChannel.IsChannelVisible := False;
    end;
  end
  else
  begin
    FQuickMaskChannel.IsChannelVisible := True;
  end;

  // change mask color type for quick mask channel
  if FQuickMaskChannel.IsChannelVisible then
  begin
    if (FChannelPreviewSet = []) and
       (LVisibleAlphaChannelCount = 0) and
       (LLayerMaskVisible = False) then
    begin
      FQuickMaskChannel.MaskColorType := mctGrayscale;
    end
    else
    begin
      FQuickMaskChannel.MaskColorType := mctColor;
    end;
  end;

  // change mask color type for visible alpha channels
  if (FChannelPreviewSet = []) and
     (LVisibleAlphaChannelCount = 1) and
     (LLayerMaskVisible = False) and
     (FQuickMaskChannel.IsChannelVisible = False) then
  begin
    FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctGrayscale);
  end
  else
  begin
    FAlphaChannelList.SetMaskColorTypeForVisibleChannels(mctColor);
  end;

  // change mask color type for layer mask channel
  if LLayerMaskVisible then
  begin
    if (FChannelPreviewSet = []) and
       (LVisibleAlphaChannelCount = 0) and
       (FQuickMaskChannel.IsChannelVisible = False) then
    begin
      FLayerMaskChannel.MaskColorType := mctGrayscale;
    end
    else
    begin
      FLayerMaskChannel.MaskColorType := mctColor;
    end;
  end;

  if Assigned(FOnChannelVisibleChanged) then
  begin
    FOnChannelVisibleChanged(ctQuickMaskChannel);
  end;
end;


end.
