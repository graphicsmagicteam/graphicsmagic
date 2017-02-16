unit gmChannelViewer;

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
 * The Original Code is igChannelViewer.pas.
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2017/01/12

interface

uses
{ Standard }
  Windows,
  Classes,
  Controls,
{ Graphics32 }
  GR32,
  GR32_Image,
  GR32_RangeBars,
{ GraphicsMagic lib }
  gmChannels,
  gmChannelManager;

type
  TgmChannelPanelArea = (cpaUnknown,
                         cpaVisibleMark,
                         cpaChannelThumbnail,
                         cpaChannelCaption);

  { TgmChannelPanelCustomTheme }

  TgmChannelPanelCustomTheme = class(TObject)
  private
    procedure SetObjectSpan(AValue: Integer);
  protected
    FObjectSpan : Integer;

    function GetChannelVisibleIconRect(const APanelRect: TRect): TRect; virtual; abstract;

    function GetPanelAreaAtXY(APanel: TgmCustomChannel;
      const APanelRect: TRect; const AX, AY: Integer): TgmChannelPanelArea; virtual; abstract;
  public
    constructor Create;

    procedure Paint(ABuffer: TBitmap32; AChannel: TgmCustomChannel;
      const ARect: TRect); virtual; abstract;

    function GetSnapshot(AChannel: TgmCustomChannel;
      const AWidth, AHeight: Integer): TBitmap32; virtual; abstract;

    property ObjectSpan : Integer read FObjectSpan write SetObjectSpan;
  end;

  { TgmChannelPanelStdTheme }

  TgmChannelPanelStdTheme = class(TgmChannelPanelCustomTheme)
  private
    FChannelVisibleIcon : TBitmap32;
    FDeselectedColor    : TColor32;
    FSelectedColor      : TColor32;
    FSpanColor          : TColor32;
  protected
    function GetChannelVisibleIconRect(const APanelRect: TRect): TRect; override;

    function GetPanelAreaAtXY(AChannel: TgmCustomChannel;
      const APanelRect: TRect; const AX, AY: Integer): TgmChannelPanelArea; override;

    procedure DrawChannelVisibleIcon(ABuffer: TBitmap32; const ARect: TRect; const AVisible: Boolean);
    procedure DrawPanelBorder(ABuffer: TBitmap32; const ARect: TRect);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Paint(ABuffer: TBitmap32; AChannel: TgmCustomChannel;
      const ARect: TRect); override;

    function GetSnapshot(AChannel: TgmCustomChannel;
      const AWidth, AHeight: Integer): TBitmap32; override;
  end;

  { TgmChannelViewer }

  TgmScrollChannelPanelThread = class; // forward declaration

  TgmChannelViewer = class(TCustomPaintBox32)
  private
    FChannelManager          : TgmCustomChannelManager;     // pointer to a channel manager
    FPanelTheme              : TgmChannelPanelCustomTheme;  // render channel panels on the manager in this theme
    FScrollLocked            : Boolean;                     // lock execution of the scroll bars
    FVertScroll              : TRangeBar;
    FViewportOffset          : TPoint;                      // offset of the viewport
    FWorkSize                : TPoint;                      // maximum scrollable area
    FLeftButtonDown          : Boolean;                     // if mouse left button is pressed
    FMouseButtonDown         : Boolean;                     // if the mouse left or right button is pressed
    FWheelDelta              : Integer;
    FEnabled                 : Boolean;                     // if the viewer is enabled

    // for render snapshot of a moving panel by mouse move
    FAlphaPanelIsMoving      : Boolean;
    FCurrentX, FCurrentY     : Integer;                     // for scrolling thread
    FMouseX, FMouseY         : Integer;
    FLastX, FLastY           : Integer;
    FMouseDownX              : Integer;
    FMouseDownY              : Integer;
    FMovingAlphaPanelIndex   : Integer;
    FPanelSnapshot           : TBitmap32;
    FScrollThread            : TgmScrollChannelPanelThread;
    FSnapshotOffsetY         : Integer;
    FSnapshotTopLeft         : TPoint;

    // callbacks
    FBeforeMouseDown         : TNotifyEvent;
    FOnMouseRightButtonClick : TNotifyEvent;

    function CanScrollDown: Boolean;
    function CanScrollUp: Boolean;
    function GetAlphaChannelIndexAtXY(AX, AY: Integer): Integer;
    function GetAlphaChannelPanelRect(const AIndex: Integer): TRect;
    function GetAlphaChannelPanelSnapshot(const APanelIndex: Integer): TBitmap32;
    function GetAlphaChannelValidAreaForSnapshot: TRect; 
    function GetColorChannelIndexAtXY(AX, AY: Integer): Integer;
    function GetColorChannelPanelRect(const AIndex: Integer): TRect;
    function GetLayerMaskChannelPanelRect: TRect;
    function GetQuickMaskChannelPanelRect: TRect;
    function IsRectInViewport(const ARect: TRect): Boolean;  // dertermine if any part of a rect is in the viewport
    function PointOnLayerMaskPanel(AX, AY: Integer): Boolean;
    function PointOnQuickMaskPanel(AX, AY: Integer): Boolean;

    procedure ScrollThreadStop;
    procedure SetChannelManager(const AValue: TgmCustomChannelManager);

    // callbacks
    procedure ScrollHandler(Sender: TObject);
  protected
    procedure CheckLayout;
    procedure DoPaintBuffer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PreparePanelSnapshotRendering(const AMouseX, AMouseY: Integer); virtual;
    procedure Scroll(Dy: Integer); virtual;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function ScrollChannelPanelInViewport(const APanelRect: TRect): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Resize; override;
    function ScrollSelectedChannelPanelInViewport: Boolean;

    property ChannelManager : TgmCustomChannelManager read FChannelManager write SetChannelManager;
    property IsEnabled      : Boolean                 read FEnabled        write FEnabled;
  published
    property BeforeMouseDown         : TNotifyEvent read FBeforeMouseDown         write FBeforeMouseDown;
    property OnMouseRightButtonClick : TNotifyEvent read FOnMouseRightButtonClick write FOnMouseRightButtonClick;
  end;

  { TgmScrollChannelPanelThread }

  TgmScrollChannelPanelThread = class(TThread)
  private
    FChannelViewer : TgmChannelViewer;
  protected
    procedure Execute; override;
  public
    constructor Create(AChannelViewer: TgmChannelViewer);
  end;

implementation

uses
{ Standard }
  SysUtils,
  Forms,
  Graphics,
  Math,
{ Graphics32 }
  GR32_LowLevel,
  GR32_Layers,
{ GraphicsMagic lib }
  gmMath;
  

const
  CHANNEL_PANEL_HEIGHT = 40;
  MAX_OBJECT_SPAN      = 20;
  MIN_OBJECT_SPAN      = 2;


{ TgmChannelPanelCustomTheme }

constructor TgmChannelPanelCustomTheme.Create;
begin
  inherited;

  FObjectSpan := 5;
end;

procedure TgmChannelPanelCustomTheme.SetObjectSpan(AValue: Integer);
begin
  FObjectSpan := Clamp(AValue, MIN_OBJECT_SPAN, MAX_OBJECT_SPAN);
end;

{ TgmChannelPanelStdTheme }

constructor TgmChannelPanelStdTheme.Create;
begin
  inherited;

  FChannelVisibleIcon := TBitmap32.Create;
  FChannelVisibleIcon.LoadFromResourceName(HInstance, 'EYEOPEN');

  FSpanColor       := clSilver32;
  FSelectedColor   := Color32(clHighlight);
  FDeselectedColor := Color32(clBtnFace);
end;

destructor TgmChannelPanelStdTheme.Destroy;
begin
  FChannelVisibleIcon.Free;

  inherited;
end;

procedure TgmChannelPanelStdTheme.DrawPanelBorder(ABuffer: TBitmap32;
  const ARect: TRect);
begin
  ABuffer.LineS(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom, clWhite32);
  ABuffer.LineS(ARect.Left, ARect.Top, ARect.Right, ARect.Top, clWhite32);
  ABuffer.LineS(ARect.Right, ARect.Top, ARect.Right, ARect.Bottom, clGray32);
  ABuffer.LineS(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom, clGray32);
end;

procedure TgmChannelPanelStdTheme.DrawChannelVisibleIcon(ABuffer: TBitmap32;
  const ARect: TRect; const AVisible: Boolean);
var
  LRectSize : TSize;
  LIconRect : TRect;
begin
  LRectSize := gmMath.GetRectSize(ARect);

  LIconRect.Left   := ARect.Left + (LRectSize.cx - FChannelVisibleIcon.Width) div 2;
  LIconRect.Top    := ARect.Top  + (LRectSize.cy - FChannelVisibleIcon.Height) div 2;
  LIconRect.Right  := LIconRect.Left + FChannelVisibleIcon.Width;
  LIconRect.Bottom := LIconRect.Top  + FChannelVisibleIcon.Height;

  if AVisible then
  begin
    ABuffer.Draw(LIconRect, FChannelVisibleIcon.BoundsRect, FChannelVisibleIcon);
  end;
  
  ABuffer.FrameRectS(LIconRect, clGray32);
end;

// calculate an area from ARect for drawing EYE icon
function TgmChannelPanelStdTheme.GetChannelVisibleIconRect(
  const APanelRect: TRect): TRect;
begin
  Result.TopLeft := APanelRect.TopLeft;
  Result.Right   := APanelRect.Left + FChannelVisibleIcon.Width + FObjectSpan;
  Result.Bottom  := APanelRect.Bottom;
end;

function TgmChannelPanelStdTheme.GetPanelAreaAtXY(AChannel: TgmCustomChannel;
  const APanelRect: TRect; const AX, AY: Integer): TgmChannelPanelArea;
var
  LRect      : TRect;
  LTestPoint : TPoint;
  LSpan2     : Integer;
begin
  Result     := cpaUnknown;
  LTestPoint := Point(AX, AY);
  LSpan2     := FObjectSpan * 2;

  // if point on layer visible mark ...

  LRect := GetChannelVisibleIconRect(APanelRect);

  if Windows.PtInRect(LRect, LTestPoint) then
  begin
    Result := cpaVisibleMark;
    Exit;
  end;

  // if point on channel thumbnail ...

  LRect.Left  := LRect.Right + 1;
  LRect.Right := LRect.Left + AChannel.ChannelThumbnail.Width + LSpan2 - 1;

  if Windows.PtInRect(LRect, LTestPoint) then
  begin
    Result := cpaChannelThumbnail;
    Exit;
  end;

  // if point on caption area ...

  LRect.Left   := LRect.Right + 1;
  LRect.Right  := APanelRect.Right;

  if Windows.PtInRect(LRect, LTestPoint) then
  begin
    Result := cpaChannelCaption;
  end;
end;

function TgmChannelPanelStdTheme.GetSnapshot(AChannel: TgmCustomChannel;
  const AWidth, AHeight: Integer): TBitmap32;
var
  LBackColor : TColor32;
begin
  Result := nil;

  if not Assigned(AChannel) then
  begin
    Exit;
  end;

  if (AWidth <= 0) or (AHeight < CHANNEL_PANEL_HEIGHT) then
  begin
    Exit;
  end;

  LBackColor := Color32(clBtnFace);

  Result             := TBitmap32.Create;
  Result.DrawMode    := dmBlend;
  Result.CombineMode := cmMerge;
  
  Result.SetSize(AWidth, AHeight);
  Result.Clear(LBackColor);

  Self.Paint(Result, AChannel, Result.BoundsRect);
  Result.FrameRectS(Result.BoundsRect, LBackColor); // clear border
end;

procedure TgmChannelPanelStdTheme.Paint(ABuffer: TBitmap32;
  AChannel: TgmCustomChannel; const ARect: TRect);
var
  LRect         : TRect;
  LSize         : TSize;
  LCaptionColor : TColor32;
begin
  LSize := gmMath.GetRectSize(ARect);

  ABuffer.BeginUpdate;
  try
    // draw channel visible mark ... 

    LRect := GetChannelVisibleIconRect(ARect);
    DrawChannelVisibleIcon(ABuffer, LRect, AChannel.IsChannelVisible);
    ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);

    // draw channel thumbnail ...

    LRect.Left   := LRect.Right + FObjectSpan;
    LRect.Top    := LRect.Top + (LSize.cy - AChannel.ChannelThumbnail.Height) div 2;
    LRect.Right  := LRect.Left + AChannel.ChannelThumbnail.Width;
    LRect.Bottom := LRect.Top + AChannel.ChannelThumbnail.Height;
    ABuffer.Draw(LRect.Left, LRect.Top, AChannel.ChannelThumbnail);

    LRect.Top    := ARect.Top;
    LRect.Right  := LRect.Right + FObjectSpan;
    LRect.Bottom := ARect.Bottom;
    ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);
    
    // fill background color for the panel
    LRect.Left   := LRect.Right + 1;
    LRect.Top    := ARect.Top;
    LRect.Right  := ARect.Right;
    LRect.Bottom := ARect.Bottom;

    if AChannel.IsSelected then
    begin
      ABuffer.FillRectS(LRect, FSelectedColor);
      LCaptionColor := clWhite32;
    end
    else
    begin
      ABuffer.FillRectS(LRect, FDeselectedColor);
      LCaptionColor := clBlack32;
    end;

    // draw panel caption
    LRect.Left := LRect.Left + FObjectSpan;
    LRect.Top  := LRect.Top + ( LSize.cy - ABuffer.TextHeight(AChannel.ChannelName) ) div 2;

    ABuffer.RenderText(LRect.Left, LRect.Top, AChannel.ChannelName, 0, LCaptionColor);

    // draw panel border
    DrawPanelBorder(ABuffer, ARect);
  finally
    ABuffer.EndUpdate;
  end;
end;

{ TgmChannelViewer }

constructor TgmChannelViewer.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csReplicatable, csOpaque];
    
  Options := [pboAutoFocus, pboWantArrowKeys];
  TabStop := True; //to receive Tabkey and focusable as default

  FPanelTheme := TgmChannelPanelStdTheme.Create;

  FVertScroll := TRangeBar.Create(Self);
  with FVertScroll do
  begin
    Parent       := Self;
    BorderStyle  := bsNone;
    Kind         := sbVertical;
    Align        := alRight;
    Width        := GetSystemMetrics(SM_CYVSCROLL) div 2;
    OnUserChange := ScrollHandler;
  end;

  FChannelManager  := nil;
  FScrollLocked    := False;
  FViewportOffset  := Point(0, 0);
  FWorkSize        := Point(0, 0);
  FLeftButtonDown  := False;
  FMouseButtonDown := False;
  FWheelDelta      := CHANNEL_PANEL_HEIGHT div 2;
  FEnabled         := True;

  // callbacks
  FBeforeMouseDown         := nil;
  FOnMouseRightButtonClick := nil;

  // for render snapshot of a moving panel by mouse move
  FAlphaPanelIsMoving := False;
  FPanelSnapshot      := nil;
  FScrollThread       := nil;
end;

destructor TgmChannelViewer.Destroy;
begin
  ScrollThreadStop();
  
  FChannelManager          := nil;
  FBeforeMouseDown         := nil;
  FOnMouseRightButtonClick := nil;

  FPanelSnapshot.Free();
  FVertScroll.Free();
  FPanelTheme.Free();

  inherited;
end;

function TgmChannelViewer.CanScrollDown: Boolean;
begin
  Result := FViewportOffset.Y + FWorkSize.Y > Self.ClientHeight;
end;

function TgmChannelViewer.CanScrollUp: Boolean;
begin
  Result := FViewportOffset.Y < 0;
end;

procedure TgmChannelViewer.CheckLayout;
begin
  // update WorkSize
  FWorkSize.X := Self.ClientWidth;

  if Assigned(FChannelManager) then
  begin
    FWorkSize.Y := CHANNEL_PANEL_HEIGHT * FChannelManager.ColorChannelList.Count;

    if Assigned(FChannelManager.LayerMaskChannel) then
    begin
      FWorkSize.Y := FWorkSize.Y + CHANNEL_PANEL_HEIGHT;
    end;

    FWorkSize.Y := FWorkSize.Y + FChannelManager.AlphaChannelList.Count * CHANNEL_PANEL_HEIGHT;

    if Assigned(FChannelManager.QuickMaskChannel) then
    begin
      FWorkSize.Y := FWorkSize.Y + CHANNEL_PANEL_HEIGHT;
    end;
  end
  else
  begin
    FWorkSize.Y := 0;
  end;

  FVertScroll.Range := FWorkSize.Y;
end;

function TgmChannelViewer.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := CanScrollDown;

  if Result then
  begin
    Self.Scroll(-FWheelDelta);
    Invalidate;
  end;
end;

function TgmChannelViewer.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := CanScrollUp;

  if Result then
  begin
    Self.Scroll(FWheelDelta);
    Invalidate;
  end;
end;

procedure TgmChannelViewer.DoPaintBuffer;
var
  i, y, LMaxY : Integer;
  LRect       : TRect;
  LChannel    : TgmCustomChannel;
begin
  CheckLayout;
  Buffer.Clear( Color32(clBtnFace) );

  // rendering channel panels ...
  if Assigned(FChannelManager) then
  begin
    // render color channel panels ...
    if FChannelManager.ColorChannelList.Count > 0 then
    begin
      for i := 0 to FChannelManager.ColorChannelList.MaxIndex do
      begin
        LChannel := FChannelManager.ColorChannelList.Channels[i];
        LRect    := GetColorChannelPanelRect(i);

        // only render the panel that is in the viewport area...
        if IsRectInViewport(LRect) then
        begin
          FPanelTheme.Paint(Buffer, LChannel, LRect);
        end;
      end;
    end;

    // render layer mask channel panel ...
    if Assigned(FChannelManager.LayerMaskChannel) then
    begin
      LRect := GetLayerMaskChannelPanelRect;

      // only render the panel that is in the viewport area...
      if IsRectInViewport(LRect) then
      begin
        FPanelTheme.Paint(Buffer, FChannelManager.LayerMaskChannel, LRect);
      end;
    end;

    // render alpha channel panels ...
    if FChannelManager.AlphaChannelList.Count > 0 then
    begin
      for i := 0 to FChannelManager.AlphaChannelList.MaxIndex do
      begin
        LChannel := FChannelManager.AlphaChannelList.Channels[i];
        LRect    := GetAlphaChannelPanelRect(i);

        // only render the panel that is in the viewport area...
        if IsRectInViewport(LRect) then
        begin
          FPanelTheme.Paint(Buffer, LChannel, LRect);
        end;
      end;
    end;

    // render quick mask channel panel ...
    if Assigned(FChannelManager.QuickMaskChannel) then
    begin
      LRect := GetQuickMaskChannelPanelRect;

      // only render the panel that is in the viewport area...
      if IsRectInViewport(LRect) then
      begin
        FPanelTheme.Paint(Buffer, FChannelManager.QuickMaskChannel, LRect);
      end;
    end;

    // render panel snapshot, if any ...
    if FAlphaPanelIsMoving then
    begin
      LMaxY := Min(FWorkSize.Y, Self.ClientHeight) - CHANNEL_PANEL_HEIGHT;
      y     := FMouseY + FSnapshotOffsetY;
      y     := Clamp(y, 0, LMaxY);

      Buffer.Draw(0, y, FPanelSnapshot);

      FSnapshotTopLeft := Point(0, y); // for other use ...
    end;
  end;

  Buffer.FrameRectS(Buffer.BoundsRect, clBlack32);
end;

function TgmChannelViewer.GetAlphaChannelIndexAtXY(AX, AY: Integer): Integer;
var
  LYActual         : Integer;
  LAlphaAreaTop    : Integer;
  LAlphaAreaBottom : Integer;
begin
  Result := -1;

  if Assigned(FChannelManager) then
  begin
    with FChannelManager do
    begin
      if AlphaChannelList.Count > 0 then
      begin
        LYActual := AY + Abs(FViewportOffset.Y);

        LAlphaAreaTop := ColorChannelList.Count * CHANNEL_PANEL_HEIGHT;

        if Assigned(LayerMaskChannel) then
        begin
          LAlphaAreaTop := LAlphaAreaTop + CHANNEL_PANEL_HEIGHT;
        end;

        LAlphaAreaBottom := LAlphaAreaTop + AlphaChannelList.Count * CHANNEL_PANEL_HEIGHT;

        if (LYActual > LAlphaAreaTop) and (LYActual < LAlphaAreaBottom) then
        begin
          Result := (LYActual - LAlphaAreaTop) div CHANNEL_PANEL_HEIGHT;
        end;
      end;
    end;
  end;
end;

function TgmChannelViewer.GetAlphaChannelPanelRect(
  const AIndex: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if Assigned(FChannelManager) then
  begin
    with FChannelManager do
    begin
      if AlphaChannelList.IsValidIndex(AIndex) then
      begin
        Result.Right := Self.ClientWidth - 1;
        Result.Top   := ColorChannelList.Count * CHANNEL_PANEL_HEIGHT + FViewportOffset.Y;

        if Assigned(LayerMaskChannel) then
        begin
          Result.Top := Result.Top + CHANNEL_PANEL_HEIGHT;
        end;

        Result.Top    := Result.Top + AIndex * CHANNEL_PANEL_HEIGHT;
        Result.Bottom := Result.Top + CHANNEL_PANEL_HEIGHT - 1;
      end;
    end;
  end; 
end;

function TgmChannelViewer.GetAlphaChannelPanelSnapshot(
  const APanelIndex: Integer): TBitmap32;
var
  LChannel : TgmCustomChannel;
begin
  Result := nil;

  // we only get the snapshot of an alpha channel panel
  if Assigned(FChannelManager) and
     FChannelManager.AlphaChannelList.IsValidIndex(APanelIndex) then
  begin
    LChannel := FChannelManager.AlphaChannelList.Channels[APanelIndex];
    Result   := FPanelTheme.GetSnapshot(LChannel, Self.ClientWidth, CHANNEL_PANEL_HEIGHT);
  end;
end;

function TgmChannelViewer.GetAlphaChannelValidAreaForSnapshot: TRect;
var
  LTopRect, LBottomRect : TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if Assigned(FChannelManager) and
     (FChannelManager.AlphaChannelList.Count > 0) then
  begin
    LTopRect    := GetAlphaChannelPanelRect(0);
    LBottomRect := GetAlphaChannelPanelRect(FChannelManager.AlphaChannelList.MaxIndex);

    Result.Left  := ClientRect.Left;
    Result.Top   := Max(LTopRect.Top - FSnapshotOffsetY, ClientRect.Top);
    Result.Right := ClientWidth;

    Result.Bottom := Min(ClientHeight, FWorkSize.Y,
      LBottomRect.Bottom - (CHANNEL_PANEL_HEIGHT - Abs(FSnapshotOffsetY)));
  end;
end;

function TgmChannelViewer.GetColorChannelIndexAtXY(AX, AY: Integer): Integer;
var
  LYActual           : Integer;
  LChannelAreaHeight : Integer;
begin
  Result := -1;

  if Assigned(FChannelManager) then
  begin
    with FChannelManager do
    begin
      if ColorChannelList.Count > 0 then
      begin
        LYActual           := AY + Abs(FViewportOffset.Y);
        LChannelAreaHeight := ColorChannelList.Count * CHANNEL_PANEL_HEIGHT;

        if LYActual < LChannelAreaHeight then
        begin
          Result := LYActual div CHANNEL_PANEL_HEIGHT;
        end;
      end;
    end;
  end;
end;

function TgmChannelViewer.GetColorChannelPanelRect(
  const AIndex: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if Assigned(FChannelManager) then
  begin
    with FChannelManager do
    begin
      if ColorChannelList.IsValidIndex(AIndex) then
      begin
        Result.Top    := AIndex * CHANNEL_PANEL_HEIGHT + FViewportOffset.Y;
        Result.Right  := Self.ClientWidth - 1;
        Result.Bottom := Result.Top + CHANNEL_PANEL_HEIGHT - 1;
      end;
    end;
  end;
end;

function TgmChannelViewer.GetLayerMaskChannelPanelRect: TRect;
begin
  Result := Rect(0, FViewportOffset.Y, Self.ClientWidth - 1, 0);

  if Assigned(FChannelManager) and
     Assigned(FChannelManager.LayerMaskChannel) then
  begin
    with FChannelManager do
    begin
      Result.Top    := Result.Top + ColorChannelList.Count * CHANNEL_PANEL_HEIGHT;
      Result.Bottom := Result.Top + CHANNEL_PANEL_HEIGHT - 1;
    end;
  end;
end;

function TgmChannelViewer.GetQuickMaskChannelPanelRect: TRect;
begin
  Result := Rect(0, FViewportOffset.Y, Self.ClientWidth - 1, 0);

  if Assigned(FChannelManager) and
     Assigned(FChannelManager.QuickMaskChannel) then
  begin
    with FChannelManager do
    begin
      Result.Top := Result.Top + ColorChannelList.Count * CHANNEL_PANEL_HEIGHT;

      if Assigned(LayerMaskChannel) then
      begin
        Result.Top := Result.Top + CHANNEL_PANEL_HEIGHT;
      end;

      Result.Top    := Result.Top + AlphaChannelList.Count * CHANNEL_PANEL_HEIGHT;
      Result.Bottom := Result.Top + CHANNEL_PANEL_HEIGHT - 1;
    end;
  end;
end;

// dertermine if any part of a rect is in the viewport
function TgmChannelViewer.IsRectInViewport(const ARect: TRect): Boolean;
begin
  Result := Windows.PtInRect(Self.ClientRect, ARect.TopLeft) or
            Windows.PtInRect(Self.ClientRect, ARect.BottomRight);
end;

procedure TgmChannelViewer.PreparePanelSnapshotRendering(
  const AMouseX, AMouseY: Integer);
var
  LPanelIndex : Integer;
  LPanelRect  : TRect;
  LChannel    : TgmAlphaChannel;
begin
  FMovingAlphaPanelIndex := -1;

  // we can only moving alpha channel panel ...
  if Assigned(FChannelManager) and
     (FChannelManager.AlphaChannelList.Count > 0) then
  begin
    LPanelIndex := GetAlphaChannelIndexAtXY(AMouseX, AMouseY);
    LChannel    := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[LPanelIndex]);

    if Assigned(LChannel) then
    begin
      FMovingAlphaPanelIndex := LPanelIndex;
      LPanelRect             := Self.GetAlphaChannelPanelRect(LPanelIndex);
      FSnapshotOffsetY       := LPanelRect.Top - AMouseY;

      if Assigned(FPanelSnapshot) then
      begin
        FreeAndNil(FPanelSnapshot);
      end;

      FPanelSnapshot := GetAlphaChannelPanelSnapshot(LPanelIndex);
      FPanelSnapshot.MasterAlpha := $7F;
    end;
  end;
end;

procedure TgmChannelViewer.Resize;
var
  LHeight : Integer;
  LDelta  : Integer;
begin
  inherited;

  LHeight := FWorkSize.Y + Self.FViewportOffset.Y;

  if LHeight < Self.ClientHeight then
  begin
    LDelta := Self.ClientHeight - LHeight;

    Inc(FViewportOffset.Y, LDelta);

    if FViewportOffset.Y > 0 then
    begin
      FViewportOffset.Y := 0;
    end;

    FScrollLocked := True;
    try
      FVertScroll.Position := Abs(FViewportOffset.Y);
    finally
      FScrollLocked := False;
    end;
  end;
end;

procedure TgmChannelViewer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LIndex               : Integer;
  LPanelRect           : TRect;
  LChannel             : TgmCustomChannel;
  LClickOnPanel        : Boolean;
  LClickOnAlphaChannel : Boolean;
  LClickOnLayerMask    : Boolean;
  LClickOnQuickMask    : Boolean;
begin
  if Assigned(FBeforeMouseDown) then
  begin
    FBeforeMouseDown(Self);
  end;

  if not FEnabled then
  begin
    Exit;
  end;

  if (Button = mbLeft) or (Button = mbRight) then
  begin
    // prepare for moving a panel that under current mouse position
    PreparePanelSnapshotRendering(X, Y);

    FMouseDownX         := X;
    FMouseDownY         := Y;
    FLastX              := X;
    FLastY              := Y;
    FAlphaPanelIsMoving := False;

    // dealing with double click on a panel
    if ssDouble	in Shift then
    begin
      if Assigned(FChannelManager.OnChannelDblClick) then
      begin
        LChannel             := nil;
        LClickOnAlphaChannel := False;
        LClickOnLayerMask    := False;
        LClickOnQuickMask    := False;

        // if click on any of an alpha channel panel ...
        LIndex := Self.GetAlphaChannelIndexAtXY(X, Y);
        if LIndex >= 0 then
        begin
          LPanelRect           := GetAlphaChannelPanelRect(LIndex);
          LChannel             := FChannelManager.AlphaChannelList.Channels[LIndex];
          LClickOnAlphaChannel := True;
        end
        else if PointOnLayerMaskPanel(X, Y) then // if click on layer mask channel panel ...
        begin
          LChannel          := FChannelManager.LayerMaskChannel;
          LPanelRect        := GetLayerMaskChannelPanelRect;
          LClickOnLayerMask := True;
        end
        else if PointOnQuickMaskPanel(X, Y) then // if click on quick mask channel panel ...
        begin
          LChannel          := FChannelManager.QuickMaskChannel;
          LPanelRect        := GetQuickMaskChannelPanelRect;
          LClickOnQuickMask := True;
        end;

        LClickOnPanel := LClickOnAlphaChannel or
                         LClickOnLayerMask or
                         LClickOnQuickMask;

        if LClickOnPanel then
        begin
          case FPanelTheme.GetPanelAreaAtXY(LChannel, LPanelRect, X, Y) of
            cpaChannelThumbnail,
            cpaChannelCaption:
              begin
                if LClickOnAlphaChannel then
                begin
                  FChannelManager.OnChannelDblClick(
                    FChannelManager.SelectedAlphaChannel, ctAlphaChannel);
                end
                else if LClickOnLayerMask then
                begin
                  FChannelManager.OnChannelDblClick(
                    FChannelManager.LayerMaskChannel, ctLayerMaskChannel);
                end
                else if LClickOnQuickMask then
                begin
                  FChannelManager.OnChannelDblClick(
                    FChannelManager.QuickMaskChannel, ctQuickMaskChannel);
                end;
              end;
          end;
        end;
      end;
    end
    else
    begin
      // If the Double-Click has not been fired, we mark
      // the mouse button is pressed. Doing this is for
      // preventing from the Double-Click opens a dialog and
      // after the dialog is closed, the current panel is still
      // in Moving mode.
      FMouseButtonDown := True;
      FLeftButtonDown  := (Button = mbLeft);
    end;
  end;

  inherited; // respond to OnMouseDown
end;

procedure TgmChannelViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LValidArea : TRect;
begin
  FCurrentX := X;
  FCurrentY := Y;
  FMouseX   := X;
  FMouseY   := Y;

  // only moving selected channel when left mouse button is pressed ... 
  if FLeftButtonDown then
  begin
    // we can only move alpha channel by mouse ...
    if Assigned(FChannelManager) and
       (FChannelManager.CurrentChannelType = ctAlphaChannel) then
    begin
      if Abs(FMouseY - FMouseDownY) > 8 then
      begin
        if not FAlphaPanelIsMoving then
        begin
          FAlphaPanelIsMoving := Assigned(FPanelSnapshot);
        end;
      end;

      if FAlphaPanelIsMoving then
      begin
        if not Assigned(FScrollThread) then
        begin
          FScrollThread := TgmScrollChannelPanelThread.Create(Self);
        end;

        LValidArea := GetAlphaChannelValidAreaForSnapshot;

        if FMouseY < LValidArea.Top then
        begin
          FMouseY := LValidArea.Top;
        end
        else if FMouseY > LValidArea.Bottom then
        begin
          FMouseY := LValidArea.Bottom;
        end;
      
        if Y <> FLastY then
        begin
          Invalidate;
        end;
      end;
    end;

    FLastX := X;
    FLastY := Y;
  end;

  inherited; // respond to OnMouseMove
end;

procedure TgmChannelViewer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LIndex               : Integer;
  LChannel             : TgmCustomChannel;
  LPanelRect           : TRect;
  LValidArea           : TRect;
  LPos                 : TPoint;
  LClickOnPanel        : Boolean;
  LClickOnColorChannel : Boolean;
  LClickOnAlphaChannel : Boolean;
  LClickOnLayerMask    : Boolean;
  LClickOnQuickMask    : Boolean;
begin
  if FMouseButtonDown then
  begin
    FLeftButtonDown  := False;
    FMouseButtonDown := False;

    if Assigned(FChannelManager) then
    begin
      if FAlphaPanelIsMoving then
      begin
        FAlphaPanelIsMoving := False;
        ScrollThreadStop;

        LValidArea := GetAlphaChannelValidAreaForSnapshot;

        if Windows.PtInRect( LValidArea, Point(X, Y) ) then
        begin
          LPos := Point(X, Y);
        end
        else
        begin
          // get center point of the snapshot of a moving alpha channel panel
          LPos.X := FSnapshotTopLeft.X + ClientWidth div 2;
          LPos.Y := FSnapshotTopLeft.Y + CHANNEL_PANEL_HEIGHT div 2;
        end;

        LIndex := GetAlphaChannelIndexAtXY(LPos.X, LPos.Y);

        FChannelManager.MoveAlphaChannel(FMovingAlphaPanelIndex, LIndex);
        FChannelManager.SelectAlphaChannel(LIndex, False);

        // If the order of alpha channel is changed, the external callbacks
        // should to take care of the refreshing of the GUI of channel viewer,
        // otherwise, we should to refresh the viewer by ourselves.
        if ScrollSelectedChannelPanelInViewport or
           (FMovingAlphaPanelIndex = FChannelManager.AlphaChannelList.SelectedIndex) then
        begin
          Invalidate;
        end;
      end
      else
      begin
        LChannel             := nil;
        LClickOnColorChannel := False;
        LClickOnAlphaChannel := False;
        LClickOnLayerMask    := False;
        LClickOnQuickMask    := False;

        // if click on any of a color channel panel ...
        LIndex := Self.GetColorChannelIndexAtXY(X, Y);
        if LIndex >= 0 then
        begin
          LPanelRect           := GetColorChannelPanelRect(LIndex);
          LChannel             := FChannelManager.ColorChannelList.Channels[LIndex];
          LClickOnColorChannel := True;
        end
        else
        begin
          // if click on any of an alpha channel panel ...
          LIndex := Self.GetAlphaChannelIndexAtXY(X, Y);
          if LIndex >= 0 then
          begin
            LPanelRect           := GetAlphaChannelPanelRect(LIndex);
            LChannel             := FChannelManager.AlphaChannelList.Channels[LIndex];
            LClickOnAlphaChannel := True;
          end
          else if PointOnLayerMaskPanel(X, Y) then // if click on layer mask channel panel ...
          begin
            LChannel          := FChannelManager.LayerMaskChannel;
            LPanelRect        := GetLayerMaskChannelPanelRect;
            LClickOnLayerMask := True;
          end
          else if PointOnQuickMaskPanel(X, Y) then // if click on quick mask channel panel ...
          begin
            LChannel          := FChannelManager.QuickMaskChannel;
            LPanelRect        := GetQuickMaskChannelPanelRect;
            LClickOnQuickMask := True;
          end;
        end;

        LClickOnPanel := LClickOnColorChannel or
                         LClickOnAlphaChannel or
                         LClickOnLayerMask or
                         LClickOnQuickMask;

        if LClickOnPanel then
        begin
          case FPanelTheme.GetPanelAreaAtXY(LChannel, LPanelRect, X, Y) of
            cpaVisibleMark:
              begin
                if LClickOnColorChannel then
                begin
                  FChannelManager.ToggleColorChannelVisible(LIndex);
                end
                else if LClickOnLayerMask then
                begin
                  FChannelManager.ToggleLayerMaskChannelVisible;
                end
                else if LClickOnAlphaChannel then
                begin
                  FChannelManager.ToggleAlphaChannelVisible(LIndex);
                end
                else if LClickOnQuickMask then
                begin
                  FChannelManager.ToggleQuickMaskChannelVisible;
                end;
              end;

            cpaChannelThumbnail,
            cpaChannelCaption:
              begin
                if LClickOnColorChannel then
                begin
                  FChannelManager.SelectColorChannel(LIndex, ssShift in Shift);
                end
                else if LClickOnAlphaChannel then
                begin
                  FChannelManager.SelectAlphaChannel(LIndex, ssShift in Shift);
                end
                else if LClickOnLayerMask then
                begin
                  FChannelManager.SelectLayerMaskChannel;
                end
                else if LClickOnQuickMask then
                begin
                  FChannelManager.SelectQuickMaskChannel;
                end;

                // invoke callback function when mouse right button released ...
                if (Button = mbRight) and (not (ssShift in Shift)) then
                begin
                  if Assigned(FOnMouseRightButtonClick) then
                  begin
                    if LClickOnColorChannel then
                    begin
                      FOnMouseRightButtonClick(
                        FChannelManager.ColorChannelList.Channels[LIndex]);
                    end
                    else if LClickOnAlphaChannel then
                    begin
                      FOnMouseRightButtonClick(FChannelManager.SelectedAlphaChannel);
                    end
                    else if LClickOnLayerMask then
                    begin
                      FOnMouseRightButtonClick(FChannelManager.LayerMaskChannel);
                    end
                    else if LClickOnQuickMask then
                    begin
                      FOnMouseRightButtonClick(FChannelManager.QuickMaskChannel);
                    end;
                  end;
                end;
              end;
          end;

          if ScrollChannelPanelInViewport(LPanelRect) then
          begin
            Invalidate;
          end;
        end;
      end;
    end;
  end;

  inherited;  // respond to OnMouseUp
end;

function TgmChannelViewer.PointOnLayerMaskPanel(AX, AY: Integer): Boolean;
var
  LYActual           : Integer;
  LLayerMaskPanelTop : Integer;
begin
  Result := False;

  if Assigned(FChannelManager) and
     Assigned(FChannelManager.LayerMaskChannel) then
  begin
    with FChannelManager do
    begin
      LYActual := AY + Abs(FViewportOffset.Y);

      LLayerMaskPanelTop := ColorChannelList.Count * CHANNEL_PANEL_HEIGHT;

      Result := (LYActual > LLayerMaskPanelTop) and
                (LYActual < (LLayerMaskPanelTop + CHANNEL_PANEL_HEIGHT));
    end;
  end;
end;

function TgmChannelViewer.PointOnQuickMaskPanel(AX, AY: Integer): Boolean;
var
  LYActual           : Integer;
  LQuickMaskPanelTop : Integer;
begin
  Result := False;

  if Assigned(FChannelManager) and
     Assigned(FChannelManager.QuickMaskChannel) then
  begin
    with FChannelManager do
    begin
      LYActual := AY + Abs(FViewportOffset.Y);

      LQuickMaskPanelTop := ColorChannelList.Count * CHANNEL_PANEL_HEIGHT;

      if Assigned(LayerMaskChannel) then
      begin
        LQuickMaskPanelTop := LQuickMaskPanelTop + CHANNEL_PANEL_HEIGHT;
      end;

      LQuickMaskPanelTop := LQuickMaskPanelTop + AlphaChannelList.Count * CHANNEL_PANEL_HEIGHT;

      Result := (LYActual > LQuickMaskPanelTop) and
                (LYActual < (LQuickMaskPanelTop + CHANNEL_PANEL_HEIGHT));
    end;
  end;
end;

procedure TgmChannelViewer.Scroll(Dy: Integer);
var
  LHeight : Integer;
begin
  FViewportOffset.Y := FViewportOffset.Y + Dy;

  // limit the scrolling amount
  LHeight := FViewportOffset.Y + FWorkSize.Y;
  if LHeight < Self.ClientHeight then
  begin
    Inc(FViewportOffset.Y, Self.ClientHeight - LHeight);
  end;

  if FViewportOffset.Y > 0 then
  begin
    FViewportOffset.Y := 0;
  end;

  // update scroll bar
  FScrollLocked := True;
  try
    FVertScroll.Position := Abs(FViewportOffset.Y);
  finally
    FScrollLocked := False;
  end;
end;

function TgmChannelViewer.ScrollChannelPanelInViewport(
  const APanelRect: TRect): Boolean;
begin
  Result := False;
  
  if APanelRect.Top < Self.ClientRect.Top then
  begin
    Self.Scroll(Self.ClientRect.Top - APanelRect.Top);
    Result := True;
  end
  else if APanelRect.Bottom > Self.ClientRect.Bottom then
  begin
    Self.Scroll(Self.ClientRect.Bottom - APanelRect.Bottom);
    Result := True;
  end;
end;

// callbacks
procedure TgmChannelViewer.ScrollHandler(Sender: TObject);
begin
  if Sender = FVertScroll then
  begin
    if not FScrollLocked then
    begin
      FViewportOffset.Y := 0 - Round(FVertScroll.Position);
      Invalidate;
    end;
  end;
end;

function TgmChannelViewer.ScrollSelectedChannelPanelInViewport: Boolean;
var
  LIndex     : Integer;
  LPanelRect : TRect;
begin
  Result := False;

  if Assigned(FChannelManager) then
  begin
    CheckLayout;

    case FChannelManager.CurrentChannelType of
      ctColorChannel:
        begin
          LIndex     := FChannelManager.ColorChannelList.FirstSelectedIndex;
          LPanelRect := GetColorChannelPanelRect(LIndex);
        end;

      ctAlphaChannel:
        begin
          LIndex     := FChannelManager.AlphaChannelList.FirstSelectedIndex;
          LPanelRect := GetAlphaChannelPanelRect(LIndex);
        end;

      ctLayerMaskChannel:
        begin
          LPanelRect := GetLayerMaskChannelPanelRect;
        end;

      ctQuickMaskChannel:
        begin
          LPanelRect := GetQuickMaskChannelPanelRect;
        end;

    else
      LPanelRect := Rect(0, 0, 0, 0);
    end;


    Result := ScrollChannelPanelInViewport(LPanelRect);

    if Result then
    begin
      Self.Invalidate;
    end;
  end;
end;

procedure TgmChannelViewer.ScrollThreadStop;
begin
  if Assigned(FScrollThread) then
  begin
    FScrollThread.Terminate;
    FScrollThread.WaitFor;
    FreeAndNil(FScrollThread);
  end;
end;

procedure TgmChannelViewer.SetChannelManager(
  const AValue: TgmCustomChannelManager);
begin
  FChannelManager := AValue;
  
  // make the selected panel fully showing in the viewport ...
  FViewportOffset := Point(0, 0);
  ScrollSelectedChannelPanelInViewport;

  Invalidate;
end;

{ TgmScrollChannelPanelThread }

constructor TgmScrollChannelPanelThread.Create(
  AChannelViewer: TgmChannelViewer);
begin
  FChannelViewer  := AChannelViewer;
  FreeOnTerminate := False;

  inherited Create(False);
  
  Priority := tpLower;
end;

procedure TgmScrollChannelPanelThread.Execute;
var
  LTopRect    : TRect;
  LBottomRect : TRect;
begin
  if Assigned(FChannelViewer) then
  begin
    while (not Terminated) do
    begin
      with FChannelViewer do
      begin
        if FChannelManager.AlphaChannelList.Count > 0 then
        begin
          LTopRect    := GetAlphaChannelPanelRect(0);
          LBottomRect := GetAlphaChannelPanelRect(FChannelManager.AlphaChannelList.MaxIndex);
        
          if FAlphaPanelIsMoving and
             (FMouseX >= 0) and
             (FMouseX < ClientWidth) then
          begin
            if (FCurrentY < 0) and (LTopRect.Top < 0) then
            begin
              if CanScrollUp then
              begin
                Scroll(CHANNEL_PANEL_HEIGHT);
                Invalidate;
              end;
            end
            else if (FCurrentY > ClientHeight) and (LBottomRect.Bottom > ClientHeight) then
            begin
              if CanScrollDown then
              begin
                Scroll(-CHANNEL_PANEL_HEIGHT);
                Invalidate;
              end;
            end;

            Sleep(500);
          end;
        end;
      end;
    end;
  end;
end;

end.
