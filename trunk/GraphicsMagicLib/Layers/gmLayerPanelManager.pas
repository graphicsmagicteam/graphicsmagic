unit gmLayerPanelManager;

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
 * The Original Code is igLayerPanelManager.pas.
 *
 * This unit is adapted from Original Code for GraphicsMagic.
 *
 * The Initial Developer of the Original Code and this unit is
 * Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2017/01/12

interface

uses
{ Delphi }
  Types, Controls, Classes,
{ Graphics32 }
  GR32, GR32_Image, GR32_RangeBars,
{ GraphicsMagic lib }
  gmLayers;

type
  TgmSelectedPanelArea = (spaUnknown,
                          spaVisibleMark,
                          spaStageMark,
                          spaLogoThumbnail,
                          spaLayerThumbnail,
                          spaMaskLinkageMark,
                          spaMaskThumbnail,
                          spaLayerCaption);

  { TgmLayerPanelCustomTheme }

  TgmLayerPanelCustomTheme = class(TObject)
  private
    procedure SetObjectSpan(AValue: Integer);
  protected
    FObjectSpan : Integer;

    function GetLayerVisibleIconRect(const APanelRect: TRect): TRect; virtual; abstract;

    function GetPanelAreaAtXY(ALayer: TgmCustomLayer;
      const APanelRect: TRect; const AX, AY: Integer): TgmSelectedPanelArea; virtual; abstract;
  public
    constructor Create;

    procedure Paint(ABuffer: TBitmap32; ALayer: TgmCustomLayer;
      const ARect: TRect); virtual; abstract;

    function GetSnapshot(ALayer: TgmCustomLayer;
      const AWidth, AHeight: Integer): TBitmap32; virtual; abstract;

    property ObjectSpan : Integer read FObjectSpan write SetObjectSpan;
  end;

  { TgmLayerPanelStdTheme }

  TgmLayerPanelStdTheme = class(TgmLayerPanelCustomTheme)
  private
    FLayerVisibleIcon : TBitmap32;
    FLayerStageIcon   : TBitmap32;
    FMaskStageIcon    : TBitmap32;
    FMaskLinkedIcon   : TBitmap32;
    FMaskUnlinkedIcon : TBitmap32;
    FSpanColor        : TColor32;
    FSelectedColor    : TColor32;
    FDeselectedColor  : TColor32;
    FDisabledColor    : TColor32;
  protected
    function GetLayerVisibleIconRect(const APanelRect: TRect): TRect; override;

    function GetPanelAreaAtXY(ALayer: TgmCustomLayer;
      const APanelRect: TRect; const AX, AY: Integer): TgmSelectedPanelArea; override;

    procedure DrawLayerVisibleIcon(ABuffer: TBitmap32; const ARect: TRect; const AVisible: Boolean);
    procedure DrawProcessStageIcon(ABuffer: TBitmap32; const ARect: TRect; const AStage: TgmLayerProcessStage);
    procedure DrawMaskLinkIcon(ABuffer: TBitmap32; const ARect: TRect; const ALinked: Boolean);
    procedure DrawPanelBorder(ABuffer: TBitmap32; const ARect: TRect);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Paint(ABuffer: TBitmap32; ALayer: TgmCustomLayer;
      const ARect: TRect); override;

    function GetSnapshot(ALayer: TgmCustomLayer;
      const AWidth, AHeight: Integer): TBitmap32; override;
  end;

  { TgmLayerPanelManager }

  TgmScrollPanelThread = class; // forward declaration
  
  TgmLayerPanelManager = class(TCustomPaintBox32)
  private
    FScrollLocked   : Boolean;                 // lock execution of the scroll bars
    FVertScroll     : TRangeBar;
    FPanelTheme     : TgmLayerPanelCustomTheme;
    FLayerList      : TgmLayerList;
    FViewportOffset : TPoint;                  // offset of the viewport
    FWorkSize       : TPoint;                  // maximum scrollable area
    FLeftButtonDown : Boolean;                 // if mouse left button is pressed
    FWheelDelta     : Integer;
    FEnabled        : Boolean;                 // if the manager is enabled

    // for render snapshot of a moving panel by mouse move
    FMouseX, FMouseY  : Integer;
    FLastX, FLastY    : Integer;
    FMouseDownX       : Integer;
    FMouseDownY       : Integer;
    FSnapshotOffsetY  : Integer;
    FMovingPanelIndex : Integer;
    FIsPanelMoving    : Boolean;
    FSnapshotTopLeft  : TPoint;
    FPanelSnapshot    : TBitmap32;
    FScrollThread     : TgmScrollPanelThread;

    // callbacks
    FBeforeMouseDown : TNotifyEvent;

    procedure SetLayerList(const AValue: TgmLayerList);
    procedure ScrollThreadStop;

    function GetPanelRect(const APanelIndex: Integer): TRect;
    function GetPanelIndexAtXY(AX, AY: Integer): Integer;
    function CanScrollDown: Boolean;
    function CanScrollUp: Boolean;
    function IsRectInViewport(const ARect: TRect): Boolean; // dertermine if any part of a rect is in the viewport

    // callbacks
    procedure ScrollHandler(Sender: TObject);
  protected
    procedure PreparePanelSnapshotRendering(const AMouseX, AMouseY: Integer); virtual;
    procedure CheckLayout; virtual;
    procedure Scroll(Dy: Integer); virtual;
    procedure DoPaintBuffer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    function ScrollPanelInViewport(const APanelIndex: Integer): Boolean; virtual;
    function GetPanelSnapshot(const APanelIndex: Integer): TBitmap32;
    function GetSelectedPanelSnapshot: TBitmap32;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ScrollSelectedPanelInViewport: Boolean;

    procedure Resize; override;

    property IsEnabled : Boolean      read FEnabled   write FEnabled;
    property LayerList : TgmLayerList read FLayerList write SetLayerList;
  published
    property BeforeMouseDown : TNotifyEvent read FBeforeMouseDown write FBeforeMouseDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TgmScrollPanelThread }

  TgmScrollPanelThread = class(TThread)
  private
    FPanelManager : TgmLayerPanelManager;
  protected
    procedure Execute; override;
  public
    constructor Create(APanelManager: TgmLayerPanelManager);
  end;

implementation

uses
{ Delphi }
  Windows, SysUtils, Graphics, Forms, Math,
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic lib }
  gmMath;

{$R gmLayerManagerIcons.res}

const
  MIN_OBJECT_SPAN    = 2;
  MAX_OBJECT_SPAN    = 20;
  LAYER_PANEL_HEIGHT = 40;

  
{ TgmLayerPanelCustomTheme }

constructor TgmLayerPanelCustomTheme.Create;
begin
  inherited;

  FObjectSpan := 5;
end;

procedure TgmLayerPanelCustomTheme.SetObjectSpan(AValue: Integer);
begin
  FObjectSpan := Clamp(AValue, MIN_OBJECT_SPAN, MAX_OBJECT_SPAN);
end;

{ TgmLayerPanelStdTheme }

constructor TgmLayerPanelStdTheme.Create;
begin
  inherited;

  FLayerVisibleIcon := TBitmap32.Create();
  FLayerVisibleIcon.LoadFromResourceName(HInstance, 'EYEOPEN');

  FLayerStageIcon := TBitmap32.Create();
  FLayerStageIcon.LoadFromResourceName(HInstance, 'ONLAYER');

  FMaskStageIcon := TBitmap32.Create();
  FMaskStageIcon.LoadFromResourceName(HInstance, 'ONMASK');

  FMaskLinkedIcon   := TBitmap32.Create();
  FMaskLinkedIcon.LoadFromResourceName(HInstance, 'MASKLINKED');

  FMaskUnlinkedIcon := TBitmap32.Create();
  FMaskUnlinkedIcon.LoadFromResourceName(HInstance, 'MASKUNLINKED');

  FSpanColor       := clSilver32;
  FSelectedColor   := Color32(clHighlight);
  FDeselectedColor := Color32(clBtnFace);
  FDisabledColor   := $FFDFDFDF;
end;

destructor TgmLayerPanelStdTheme.Destroy;
begin
  FMaskLinkedIcon.Free();
  FMaskUnlinkedIcon.Free();
  FMaskStageIcon.Free();
  FLayerStageIcon.Free();
  FLayerVisibleIcon.Free();

  inherited;
end;

procedure TgmLayerPanelStdTheme.DrawLayerVisibleIcon(ABuffer: TBitmap32;
  const ARect: TRect; const AVisible: Boolean);
var
  LRectSize : TSize;
  LIconRect : TRect;
begin
  LRectSize := gmMath.GetRectSize(ARect);

  LIconRect.Left   := ARect.Left + (LRectSize.cx - FLayerVisibleIcon.Width) div 2;
  LIconRect.Top    := ARect.Top  + (LRectSize.cy - FLayerVisibleIcon.Height) div 2;
  LIconRect.Right  := LIconRect.Left + FLayerVisibleIcon.Width;
  LIconRect.Bottom := LIconRect.Top  + FLayerVisibleIcon.Height;

  if AVisible then
  begin
    ABuffer.Draw(LIconRect, FLayerVisibleIcon.BoundsRect, FLayerVisibleIcon);
  end;
  
  ABuffer.FrameRectS(LIconRect, clGray32);
end;

procedure TgmLayerPanelStdTheme.DrawMaskLinkIcon(ABuffer: TBitmap32;
  const ARect: TRect; const ALinked: Boolean);
var
  LRectSize : TSize;
  LIconRect : TRect;
  LBmp      : TBitmap32;
begin
  LRectSize := gmMath.GetRectSize(ARect);

  if ALinked then
  begin
    LBmp := FMaskLinkedIcon;
  end
  else
  begin
    LBmp := FMaskUnlinkedIcon;
  end;

  LIconRect.Left   := ARect.Left + (LRectSize.cx - LBmp.Width) div 2;
  LIconRect.Top    := ARect.Top + (LRectSize.cy - LBmp.Height) div 2;
  LIconRect.Right  := LIconRect.Left + LBmp.Width;
  LIconRect.Bottom := LIconRect.Top  + LBmp.Height;

  ABuffer.Draw(LIconRect, LBmp.BoundsRect, LBmp);
end;

procedure TgmLayerPanelStdTheme.DrawPanelBorder(ABuffer: TBitmap32;
  const ARect: TRect);
begin
  ABuffer.LineS(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom, clWhite32);
  ABuffer.LineS(ARect.Left, ARect.Top, ARect.Right, ARect.Top, clWhite32);
  ABuffer.LineS(ARect.Right, ARect.Top, ARect.Right, ARect.Bottom, clGray32);
  ABuffer.LineS(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom, clGray32);
end;

procedure TgmLayerPanelStdTheme.DrawProcessStageIcon(ABuffer: TBitmap32;
  const ARect: TRect; const AStage: TgmLayerProcessStage);
var
  LRectSize : TSize;
  LIconRect : TRect;
  LBmp      : TBitmap32;
begin
  LRectSize := gmMath.GetRectSize(ARect);
  LBmp      := nil;

  case AStage of
    lpsLayer:
      begin
        LBmp := FLayerStageIcon;
      end;

    lpsMask:
      begin
        LBmp := FMaskStageIcon;
      end;
  end;

  LIconRect.Left   := ARect.Left + (LRectSize.cx - LBmp.Width) div 2;
  LIconRect.Top    := ARect.Top + (LRectSize.cy - LBmp.Height) div 2;
  LIconRect.Right  := LIconRect.Left + LBmp.Width;
  LIconRect.Bottom := LIconRect.Top  + LBmp.Height;

  ABuffer.Draw(LIconRect, LBmp.BoundsRect, LBmp);
  ABuffer.FrameRectS(LIconRect, clGray32);
end;

// calculate an area from ARect for drawing EYE icon
function TgmLayerPanelStdTheme.GetLayerVisibleIconRect(
  const APanelRect: TRect): TRect;
begin
  Result.TopLeft := APanelRect.TopLeft;
  Result.Right   := APanelRect.Left + FLayerVisibleIcon.Width + FObjectSpan;
  Result.Bottom  := APanelRect.Bottom;
end;

function TgmLayerPanelStdTheme.GetPanelAreaAtXY(ALayer: TgmCustomLayer;
  const APanelRect: TRect; const AX, AY: Integer): TgmSelectedPanelArea;
var
  LRect      : TRect;
  LSize      : TSize;
  LTestPoint : TPoint;
  LBmp       : TBitmap32;
  LSpan2     : Integer;
begin
  Result     := spaUnknown;
  LTestPoint := Point(AX, AY);
  LSize      := gmMath.GetRectSize(APanelRect);
  LSpan2     := FObjectSpan * 2;
  LBmp       := nil;

  // if point on layer visible mark ...

  LRect := GetLayerVisibleIconRect(APanelRect);

  if Windows.PtInRect(LRect, LTestPoint) then
  begin
    Result := spaVisibleMark;
    Exit;
  end;

  // if point on stage mark ...

  case ALayer.LayerProcessStage of
    lpsLayer:
      begin
        LBmp := FLayerStageIcon;
      end;

    lpsMask:
      begin
        LBmp := FMaskStageIcon;
      end;
  end;

  LRect.Left  := LRect.Right + 1;
  LRect.Right := LRect.Left + LBmp.Width + FObjectSpan - 1;
  
  if Windows.PtInRect(LRect, LTestPoint) then
  begin
    Result := spaStageMark;
    Exit;
  end;

  // if point on layer logo thumbnail ...
  if ALayer.IsLogoThumbEnabled then
  begin
    LRect.Left  := LRect.Right + 1;
    LRect.Right := LRect.Left + ALayer.LogoThumbnail.Width + LSpan2 - 1;

    if Windows.PtInRect(LRect, LTestPoint) then
    begin
      Result := spaLogoThumbnail;
      Exit;
    end;
  end;

  // if point on layer thumbnail ...
  if ALayer.IsLayerThumbEnabled then
  begin
    LRect.Left  := LRect.Right + 1;
    LRect.Right := LRect.Left + ALayer.LayerThumbnail.Width + LSpan2 - 1;

    if Windows.PtInRect(LRect, LTestPoint) then
    begin
      Result := spaLayerThumbnail;
      Exit;
    end;
  end;

  // if mask enabled ...
  if ALayer.IsMaskEnabled then
  begin
    // if point on Mask-Link mark ...

    if ALayer.IsMaskLinked then
    begin
      LBmp := FMaskLinkedIcon;
    end
    else
    begin
      LBmp := FMaskUnlinkedIcon;
    end;

    LRect.Left  := LRect.Right + 1;
    LRect.Right := LRect.Left + LBmp.Width + FObjectSpan - 1;

    if Windows.PtInRect(LRect, LTestPoint) then
    begin
      Result := spaMaskLinkageMark;
      Exit;
    end;

    // if point on Mask thumbnail ...

    LRect.Left  := LRect.Right + 1;
    LRect.Right := LRect.Left + ALayer.MaskThumbnail.Width + LSpan2 - 1;

    if Windows.PtInRect(LRect, LTestPoint) then
    begin
      Result := spaMaskThumbnail;
      Exit;
    end;
  end;

  // if point on caption area ...

  LRect.Left   := LRect.Right + 1;
  LRect.Right  := APanelRect.Right;

  if Windows.PtInRect(LRect, LTestPoint) then
  begin
    Result := spaLayerCaption;
  end;
end;

function TgmLayerPanelStdTheme.GetSnapshot(ALayer: TgmCustomLayer;
  const AWidth, AHeight: Integer): TBitmap32;
var
  LBackColor : TColor32;
begin
  Result := nil;

  if not Assigned(ALayer) then
  begin
    Exit;
  end;

  if (AWidth <= 0) or (AHeight < LAYER_PANEL_HEIGHT) then
  begin
    Exit;
  end;

  LBackColor := Color32(clBtnFace);

  Result             := TBitmap32.Create;
  Result.DrawMode    := dmBlend;
  Result.CombineMode := cmMerge;
  
  Result.SetSize(AWidth, AHeight);
  Result.Clear(LBackColor);

  Self.Paint(Result, ALayer, Result.BoundsRect);
  Result.FrameRectS(Result.BoundsRect, LBackColor); // clear border
end;

procedure TgmLayerPanelStdTheme.Paint(ABuffer: TBitmap32;
  ALayer: TgmCustomLayer; const ARect: TRect);
var
  LRect         : TRect;
  LSize         : TSize;
  LBmp          : TBitmap32;
  LCaptionColor : TColor32;
begin
  LSize := gmMath.GetRectSize(ARect);
  LBmp  := nil;

  ABuffer.BeginUpdate;
  try
    // draw layer visible mark
    LRect := GetLayerVisibleIconRect(ARect);
    DrawLayerVisibleIcon(ABuffer, LRect, ALayer.IsLayerVisible);
    ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);

    // draw process stage mark
    case ALayer.LayerProcessStage of
      lpsLayer:
        begin
          LBmp := FLayerStageIcon;
        end;

      lpsMask:
        begin
          LBmp := FMaskStageIcon;
        end;
    end;

    LRect.Left  := LRect.Right;
    LRect.Right := LRect.Left + LBmp.Width + FObjectSpan;
    DrawProcessStageIcon(ABuffer, LRect, ALayer.LayerProcessStage);
    ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);

    // draw logo thumbnail
    if ALayer.IsLogoThumbEnabled then
    begin
      LRect.Left   := LRect.Right + FObjectSpan;
      LRect.Top    := LRect.Top + (LSize.cy - ALayer.LogoThumbnail.Height) div 2;
      LRect.Right  := LRect.Left + ALayer.LogoThumbnail.Width;
      LRect.Bottom := LRect.Top + ALayer.LogoThumbnail.Height;
      ABuffer.Draw(LRect.Left, LRect.Top, ALayer.LogoThumbnail);

      LRect.Top    := ARect.Top;
      LRect.Right  := LRect.Right + FObjectSpan;
      LRect.Bottom := ARect.Bottom;
      ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);
    end;

    // draw layer thumbnail
    if ALayer.IsLayerThumbEnabled then
    begin
      LRect.Left   := LRect.Right + FObjectSpan;
      LRect.Top    := LRect.Top + (LSize.cy - ALayer.LayerThumbnail.Height) div 2;
      LRect.Right  := LRect.Left + ALayer.LayerThumbnail.Width;
      LRect.Bottom := LRect.Top + ALayer.LayerThumbnail.Height;
      ABuffer.Draw(LRect.Left, LRect.Top, ALayer.LayerThumbnail);

      LRect.Top    := ARect.Top;
      LRect.Right  := LRect.Right + FObjectSpan;
      LRect.Bottom := ARect.Bottom;
      ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);
    end;

    // draw Mask-Link mark
    if ALayer.IsMaskEnabled then
    begin
      if ALayer.IsMaskLinked then
      begin
        LBmp := FMaskLinkedIcon;
      end
      else
      begin
        LBmp := FMaskUnlinkedIcon;
      end;

      LRect.Left  := LRect.Right;
      LRect.Right := LRect.Left + LBmp.Width + FObjectSpan;
      DrawMaskLinkIcon(ABuffer, LRect, ALayer.IsMaskLinked);
      ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);

      // draw Mask thumbnail
      LRect.Left   := LRect.Right + FObjectSpan;
      LRect.Top    := ARect.Top + (LSize.cy - ALayer.MaskThumbnail.Height) div 2;
      LRect.Right  := LRect.Left + ALayer.MaskThumbnail.Width;
      LRect.Bottom := ARect.Top + ALayer.MaskThumbnail.Height;
      ABuffer.Draw(LRect.Left, LRect.Top, ALayer.MaskThumbnail);

      LRect.Top    := ARect.Top;
      LRect.Right  := LRect.Right + FObjectSpan;
      LRect.Bottom := ARect.Bottom;
      ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);
    end;

    // fill background color for the panel
    LRect.Left   := LRect.Right + 1;
    LRect.Top    := ARect.Top;
    LRect.Right  := ARect.Right;
    LRect.Bottom := ARect.Bottom;

    if ALayer.IsSelected then
    begin
      if ALayer.IsLayerEnabled then
      begin
        ABuffer.FillRectS(LRect, FSelectedColor);
        LCaptionColor := clWhite32;
      end
      else
      begin
        ABuffer.FillRectS(LRect, FDisabledColor);
        LCaptionColor := clBlack32;
      end;
    end
    else
    begin
      ABuffer.FillRectS(LRect, FDeselectedColor);
      LCaptionColor := clBlack32;
    end;

    // draw panel caption
    LRect.Left := LRect.Left + FObjectSpan;
    LRect.Top  := LRect.Top + ( LSize.cy - ABuffer.TextHeight(ALayer.LayerName) ) div 2;

    ABuffer.RenderText(LRect.Left, LRect.Top, ALayer.LayerName, 0, LCaptionColor);

    // draw panel border
    DrawPanelBorder(ABuffer, ARect);
  finally
    ABuffer.EndUpdate;
  end;
end;


{ TgmLayerPanelManager }

constructor TgmLayerPanelManager.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csReplicatable, csOpaque];
    
  Options := [pboAutoFocus, pboWantArrowKeys];
  TabStop := True; //to receive Tabkey and focusable as default 

  FScrollLocked   := False;
  FViewportOffset := Point(0, 0);
  FLeftButtonDown := False;
  FWheelDelta     := LAYER_PANEL_HEIGHT div 2;
  FEnabled        := True;

  FPanelTheme := TgmLayerPanelStdTheme.Create();

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

  // for render snapshot of a moving panel by mouse move
  FPanelSnapshot := nil;
  FScrollThread  := nil;
  FIsPanelMoving := False;

  // callbacks
  FBeforeMouseDown := nil;
end;

destructor TgmLayerPanelManager.Destroy;
begin
  FBeforeMouseDown := nil;

  ScrollThreadStop();
  FPanelSnapshot.Free();
  FVertScroll.Free();
  FPanelTheme.Free();

  inherited;
end;

function TgmLayerPanelManager.CanScrollDown: Boolean;
begin
  Result := FViewportOffset.Y + FWorkSize.Y > Self.ClientHeight;
end;

function TgmLayerPanelManager.CanScrollUp: Boolean;
begin
  Result := FViewportOffset.Y < 0;
end;

procedure TgmLayerPanelManager.CheckLayout;
begin
  if Assigned(FLayerList) then
  begin
    // update WorkSize
    FWorkSize         := Point(Self.ClientWidth, FLayerList.Count * LAYER_PANEL_HEIGHT);
    FVertScroll.Range := FWorkSize.Y;
  end
  else
  begin
    FVertScroll.Range := 0;
  end;
end;

function TgmLayerPanelManager.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := CanScrollDown;

  if Result then
  begin
    Self.Scroll(-FWheelDelta);
    Invalidate();
  end;
end;

function TgmLayerPanelManager.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := CanScrollUp();

  if Result then
  begin
    Self.Scroll(FWheelDelta);
    Invalidate();
  end;
end;

procedure TgmLayerPanelManager.DoPaintBuffer;
var
  i, y, LMaxY : Integer;
  LLayer      : TgmCustomLayer;
  LRect       : TRect;
begin
  CheckLayout;
  Buffer.Clear( Color32(clBtnFace) );

  if Assigned(FLayerList) then
  begin
    if FLayerList.Count > 0 then
    begin
      for i := FLayerList.MaxIndex downto 0 do
      begin
        LLayer := FLayerList.Layers[i];
        LRect  := GetPanelRect(i);

        // only render the panel that in the viewport area...
        if IsRectInViewport(LRect) then
        begin
          FPanelTheme.Paint(Buffer, LLayer, LRect);
        end;
      end;

      // render panel snapshot, if any ...
      if FIsPanelMoving then
      begin
        LMaxY := Min(FWorkSize.Y, Self.ClientHeight) - LAYER_PANEL_HEIGHT;
        y     := FMouseY + FSnapshotOffsetY;
        y     := Clamp(y, 0, LMaxY);

        Buffer.Draw(0, y, FPanelSnapshot);

        FSnapshotTopLeft := Point(0, y); // for other use ...
      end;
    end;
  end;

  Buffer.FrameRectS(Buffer.BoundsRect, clBlack32);
end;

function TgmLayerPanelManager.GetPanelIndexAtXY(AX, AY: Integer): Integer;
var
  LYActual: Integer;
begin
  Result := -1;

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    LYActual := AY + Abs(FViewportOffset.Y);

    if LYActual < FWorkSize.Y then
    begin
      Result := FLayerList.MaxIndex - LYActual div LAYER_PANEL_HEIGHT;
    end;
  end;
end;

function TgmLayerPanelManager.GetPanelRect(const APanelIndex: Integer): TRect;
begin
  Result.Left   := 0;
  Result.Top    := (FLayerList.MaxIndex - APanelIndex) * LAYER_PANEL_HEIGHT + FViewportOffset.Y;
  Result.Right  := Self.ClientWidth - 1;
  Result.Bottom := Result.Top + LAYER_PANEL_HEIGHT - 1;
end;

function TgmLayerPanelManager.GetPanelSnapshot(
  const APanelIndex: Integer): TBitmap32;
var
  LLayer : TgmCustomLayer;
begin
  Result := nil;

  if Assigned(FLayerList) and FLayerList.IsValidIndex(APanelIndex) then
  begin
    LLayer := FLayerList.Layers[APanelIndex];
    Result := FPanelTheme.GetSnapshot(LLayer, Self.ClientWidth, LAYER_PANEL_HEIGHT);
  end;
end;

function TgmLayerPanelManager.GetSelectedPanelSnapshot: TBitmap32;
begin
  Result := nil;

  if Assigned(FLayerList) then
  begin
    Result := GetPanelSnapshot(FLayerList.SelectedIndex);
  end;
end;

// dertermine if any part of a rect is in the viewport
function TgmLayerPanelManager.IsRectInViewport(const ARect: TRect): Boolean;
begin
  Result := Windows.PtInRect(Self.ClientRect, ARect.TopLeft) or
            Windows.PtInRect(Self.ClientRect, ARect.BottomRight);
end;

procedure TgmLayerPanelManager.KeyDown(var Key: Word; Shift: TShiftState);
var
  LCurIndex    : Integer;
  LTargetIndex : Integer;
begin
  if not FEnabled then
  begin
    Exit;
  end;

  if FIsPanelMoving then
  begin
    Exit;
  end;

  if Assigned(FLayerList) and (FLayerList.Count > 1) then
  begin
    case Key of
      VK_UP:
        begin
          LCurIndex    := FLayerList.SelectedIndex;
          LTargetIndex := LCurIndex + 1;

          if LCurIndex < FLayerList.MaxIndex then
          begin
            if ssShift in Shift then
            begin
              FLayerList.Move(LCurIndex, LTargetIndex);
            end
            else
            begin
              FLayerList.SelectLayer(LTargetIndex);
            end;

            ScrollSelectedPanelInViewport;
          end;
        end;
        
      VK_DOWN:
        begin
          LCurIndex    := FLayerList.SelectedIndex;
          LTargetIndex := LCurIndex - 1;

          if LCurIndex > 0 then
          begin
            if ssShift in Shift then
            begin
              FLayerList.Move(LCurIndex, LTargetIndex);
            end
            else
            begin
              FLayerList.SelectLayer(LTargetIndex);
            end;

            ScrollSelectedPanelInViewport;
          end;
        end;
    end;
  end;

  inherited;  // respond to OnKeyDown
end;

procedure TgmLayerPanelManager.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LIndex     : Integer;
  LPanelRect : TRect;
  LLayer     : TgmCustomLayer;
begin
  if Assigned(FBeforeMouseDown) then
  begin
    FBeforeMouseDown(Self);
  end;

  if not FEnabled then
  begin
    Exit;
  end;
  
  if Button = mbLeft then
  begin
    // prepare for moving a panel that under current mouse position
    PreparePanelSnapshotRendering(X, Y);

    FMouseDownX    := X;
    FMouseDownY    := Y;
    FLastX         := X;
    FLastY         := Y;
    FIsPanelMoving := False;

    // dealing with double click on a panel
    if ssDouble	in Shift then
    begin
      LIndex := GetPanelIndexAtXY(X, Y);

      if LIndex >= 0 then
      begin
        LPanelRect := Self.GetPanelRect(LIndex);
        LLayer     := FLayerList.Layers[LIndex];

        case FPanelTheme.GetPanelAreaAtXY(LLayer, LPanelRect, X, Y) of
          spaLayerThumbnail:
            begin
              if Assigned(LLayer.OnLayerThumbDblClick) then
              begin
                LLayer.OnLayerThumbDblClick(LLayer);
              end;
            end;

          spaMaskThumbnail:
            begin
              if Assigned(LLayer.OnMaskThumbDblClick) then
              begin
                LLayer.OnMaskThumbDblClick(LLayer);
              end;
            end;

          spaLogoThumbnail:
            begin
              if Assigned(LLayer.OnLogoThumbDblClick) then
              begin
                LLayer.OnLogoThumbDblClick(LLayer);
              end;
            end;

          spaLayerCaption:
            begin
              if Assigned(LLayer.OnPanelDblClick) then
              begin
                LLayer.OnPanelDblClick(LLayer);
              end;
            end;
        end;
      end;
    end
    else
    begin
      // If the Double-Click has not been fired, we mark
      // the mouse left button is pressed. Doing this is for
      // preventing from the Double-Click opens a dialog and
      // after the dialog is closed, the current panel is still
      // in Moving mode.
      FLeftButtonDown := True;
    end;
  end;

  inherited; // respond to OnMouseDown
end;

procedure TgmLayerPanelManager.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;

  if FLeftButtonDown then
  begin
    if Abs(FMouseY - FMouseDownY) > 8 then
    begin
      if not FIsPanelMoving then
      begin
        FIsPanelMoving := Assigned(FPanelSnapshot);
      end;
    end;

    if FIsPanelMoving then
    begin
      if not Assigned(FScrollThread) then
      begin
        FScrollThread := TgmScrollPanelThread.Create(Self);
      end;
      
      if Y <> FLastY then
      begin
        Invalidate();
      end;
    end;

    FLastX := X;
    FLastY := Y;
  end;
  
  inherited; // respond to OnMouseMove
end;

procedure TgmLayerPanelManager.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LIndex     : Integer;
  LLayer     : TgmCustomLayer;
  LPanelRect : TRect;
  LValidArea : TRect;
  LPos       : TPoint;
begin
  if FLeftButtonDown then
  begin
    FLeftButtonDown := False;

    if Assigned(FLayerList) then
    begin
      if FIsPanelMoving then
      begin
        FIsPanelMoving := False;
        ScrollThreadStop;

        LValidArea.TopLeft := ClientRect.TopLeft;
        LValidArea.Right   := ClientWidth;
        LValidArea.Bottom  := Min(ClientHeight, FWorkSize.Y);
        
        if Windows.PtInRect( LValidArea, Point(X, Y) ) then
        begin
          LPos := Point(X, Y);
        end
        else
        begin
          // get center point of the snapshot of a moving layer panel
          LPos.X := FSnapshotTopLeft.X + ClientWidth div 2;
          LPos.Y := FSnapshotTopLeft.Y + LAYER_PANEL_HEIGHT div 2;
        end;

        LIndex := GetPanelIndexAtXY(LPos.X, LPos.Y);

        FLayerList.Move(FMovingPanelIndex, LIndex);
        FLayerList.SelectLayer(LIndex);

        // If the layer order is changed, the external callbacks should to
        // take care of the refreshing of the GUI of layer manager,
        // otherwise, we should to refresh the view by ourselves.
        if ScrollSelectedPanelInViewport or
           (FMovingPanelIndex = FLayerList.SelectedIndex) then
        begin
          Invalidate;
        end;
      end
      else
      begin
        LIndex := GetPanelIndexAtXY(X, Y);

        if LIndex >= 0 then
        begin
          LPanelRect := Self.GetPanelRect(LIndex);
          LLayer     := FLayerList.Layers[LIndex];

          case FPanelTheme.GetPanelAreaAtXY(LLayer, LPanelRect, X, Y) of
            spaVisibleMark:
              begin
                LLayer.IsLayerVisible := not LLayer.IsLayerVisible;
              end;

            spaStageMark:
              begin
                // do nothing yet
              end;

            spaLayerThumbnail:
              begin
                FLayerList.SelectLayer(LIndex);
                FLayerList.SelectedLayer.LayerProcessStage := lpsLayer;
              end;

            spaMaskLinkageMark:
              begin
                LLayer.IsMaskLinked := not LLayer.IsMaskLinked;
              end;

            spaMaskThumbnail:
              begin
                FLayerList.SelectLayer(LIndex);
                FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
              end;

            spaLogoThumbnail,
            spaLayerCaption:
              begin
                FLayerList.SelectLayer(LIndex);
              end;
          end;

          if ScrollSelectedPanelInViewport then
          begin
            Invalidate;
          end;
        end;
      end;
    end;
  end;

  inherited;  // respond to OnMouseUp
end;

procedure TgmLayerPanelManager.PreparePanelSnapshotRendering(
  const AMouseX, AMouseY: Integer);
var
  LPanelIndex : Integer;
  LPanelRect  : TRect;
  LLayer      : TgmCustomLayer;
begin
  FMovingPanelIndex := -1;

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    LPanelIndex := GetPanelIndexAtXY(AMouseX, AMouseY);
    LLayer      := FLayerList.Layers[LPanelIndex];

    if Assigned(LLayer) then
    begin
      FMovingPanelIndex := LPanelIndex;
      LPanelRect        := Self.GetPanelRect(LPanelIndex);
      FSnapshotOffsetY  := LPanelRect.Top - AMouseY;

      if Assigned(FPanelSnapshot) then
      begin
        FreeAndNil(FPanelSnapshot);
      end;

      FPanelSnapshot := GetPanelSnapshot(LPanelIndex);
      FPanelSnapshot.MasterAlpha := $7F;
    end;
  end;
end;

procedure TgmLayerPanelManager.Resize;
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

procedure TgmLayerPanelManager.Scroll(Dy: Integer);
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

procedure TgmLayerPanelManager.ScrollHandler(Sender: TObject);
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

function TgmLayerPanelManager.ScrollPanelInViewport(
  const APanelIndex: Integer): Boolean;
var
  LRect : TRect;
begin
  Result := False;
  
  if Assigned(FLayerList) and FLayerList.IsValidIndex(APanelIndex) then
  begin
    LRect := GetPanelRect(APanelIndex);

    if LRect.Top < Self.ClientRect.Top then
    begin
      Self.Scroll(Self.ClientRect.Top - LRect.Top);
      Result := True;
    end
    else if LRect.Bottom > Self.ClientRect.Bottom then
    begin
      Self.Scroll(Self.ClientRect.Bottom - LRect.Bottom);
      Result := True;
    end;
  end;
end;

function TgmLayerPanelManager.ScrollSelectedPanelInViewport: Boolean;
var
  LIndex : Integer;
begin
  Result := False;
  
  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    LIndex := FLayerList.SelectedIndex;
    Result := ScrollPanelInViewport(LIndex);
  end;
end;

procedure TgmLayerPanelManager.ScrollThreadStop;
begin
  if Assigned(FScrollThread) then
  begin
    FScrollThread.Terminate;
    FScrollThread.WaitFor;
    FreeAndNil(FScrollThread);
  end;
end;

procedure TgmLayerPanelManager.SetLayerList(const AValue: TgmLayerList);
begin
  FLayerList := AValue;
  CheckLayout();
  
  // make the selected panel fully showing in the viewport ...
  FViewportOffset := Point(0, 0);
  ScrollSelectedPanelInViewport();
  
  Invalidate();
end;


{ TgmScrollPanelThread }

constructor TgmScrollPanelThread.Create(APanelManager: TgmLayerPanelManager);
begin
  FPanelManager   := APanelManager;
  FreeOnTerminate := False;

  inherited Create(False);
  
  Priority := tpLower;
end;

procedure TgmScrollPanelThread.Execute;
begin
  if Assigned(FPanelManager) then
  begin
    while (not Terminated) do
    begin
      with FPanelManager do
      begin
        if FIsPanelMoving and (FMouseX >= 0) and (FMouseX < ClientWidth) then
        begin
          if FMouseY < 0 then
          begin
            if CanScrollUp then
            begin
              Scroll(LAYER_PANEL_HEIGHT);
              Invalidate;
            end;
          end
          else if FMouseY > ClientHeight then
          begin
            if CanScrollDown then
            begin
              Scroll(-LAYER_PANEL_HEIGHT);
              Invalidate;
            end;
          end;

          Sleep(500);
        end;
      end;
    end;
  end;
end;


end.
