unit gmPathManager;

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

// Update Date: 2017/02/14

interface

uses
{ Delphi }
  Classes,
  Controls,
  Types,
{ Graphics32 }
  GR32,
  GR32_Image,
  GR32_RangeBars,
{ GraphicsMagicLib }
  gmPaths;

type
  TgmPathPanelArea = (ppaOutOfPanels,
                      ppaPathThumbnail,
                      ppaPathCaption);


  { TgmPathPanelCustomTheme }

  TgmPathPanelCustomTheme = class(TObject)
  private
    procedure SetObjectSpan(AValue: Integer);
  protected
    FObjectSpan : Integer;

    function GetPanelAreaAtXY(APath: TgmPath; const APanelRect: TRect;
      const AX, AY: Integer): TgmPathPanelArea; virtual; abstract;
  public
    constructor Create;

    function GetSnapshot(APath: TgmPath;
      const AWidth, AHeight: Integer): TBitmap32; virtual; abstract;

    procedure Paint(ABuffer: TBitmap32; APath: TgmPath;
      const ARect: TRect); virtual; abstract;

    property ObjectSpan : Integer read FObjectSpan write SetObjectSpan;
  end;


  { TgmPathPanelStdTheme }

  TgmPathPanelStdTheme = class(TgmPathPanelCustomTheme)
  private
    FSpanColor       : TColor32;
    FSelectedColor   : TColor32;
    FDeselectedColor : TColor32;
  protected
    procedure DrawPanelBorder(ABuffer: TBitmap32; const ARect: TRect);

    function GetPanelAreaAtXY(APath: TgmPath; const APanelRect: TRect;
      const AX, AY: Integer): TgmPathPanelArea; override;
  public
    constructor Create;

    function GetSnapshot(APath: TgmPath;
      const AWidth, AHeight: Integer): TBitmap32; override;

    procedure Paint(ABuffer: TBitmap32; APath: TgmPath;
      const ARect: TRect); override;
  end;


  { TgmPathManager }

  TgmPathManagerMouseRightClick = procedure (APath: TgmPath;
    const AClickedArea: TgmPathPanelArea) of object;

  TgmPathPanelScrollingThread = class; // forward declaration

  // The class TgmPathManager is a component for managing
  // the path list visually.
  TgmPathManager = class(TCustomPaintBox32)
  private
    FVertScrollBar       : TRangeBar;
    FScrollBarLocked     : Boolean;                 // lock up the execution of the scroll bars
    FPathPanelTheme      : TgmPathPanelCustomTheme;
    FPathList            : TgmPathList;             // connect to a path list
    FViewportOffset      : TPoint;                  // offset of the viewport
    FWorkSize            : TSize;                   // maximum scrollable area
    FWheelDelta          : Integer;
    FLeftButtonDown      : Boolean;                 // if mouse left button is pressed
    FMouseButtonDown     : Boolean;                 // if the mouse left or right button is pressed
    FEnabled             : Boolean;                 // if the manager is enabled

    // for rendering snapshot of a moving panel by mouse move
    FMouseX, FMouseY     : Integer;
    FLastX, FLastY       : Integer;
    FMouseDownX          : Integer;
    FMouseDownY          : Integer;
    FSnapshotOffsetY     : Integer;
    FMovingPanelIndex    : Integer;
    FIsPanelMoving       : Boolean;
    FSnapshotTopLeft     : TPoint;
    FPanelSnapshot       : TBitmap32;
    FScrollThread        : TgmPathPanelScrollingThread;

    // callbacks
    FBeforeMouseDown     : TNotifyEvent;
    FOnMouseRightClick   : TgmPathManagerMouseRightClick;
    FOnPathPanelDblClick : TNotifyEvent;
    FOnPathThumbDblClick : TNotifyEvent;

    function CanScrollDown: Boolean;
    function CanScrollUp: Boolean;
    function GetPanelIndexAtXY(AX, AY: Integer): Integer;
    function GetPanelRect(const APanelIndex: Integer): TRect;
    function GetPanelSnapshot(const APanelIndex: Integer): TBitmap32;
    function IsRectInViewport(const ARect: TRect): Boolean;  // dertermine if any part of a rect is in the viewport

    procedure StopScrollThread;
    procedure SetPathList(const AValue: TgmPathList);

    // callbacks
    procedure ScrollHandler(Sender: TObject);
  protected
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function ScrollPanelInViewport(const APanelRect: TRect): Boolean;
    
    procedure CheckLayout; virtual;
    procedure DoPaintBuffer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PreparePanelSnapshotRendering(const AMouseX, AMouseY: Integer); virtual;
    procedure Scroll(Dy: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Resize; override;

    function ScrollSelectedPanelInViewport: Boolean;

    property IsEnabled : Boolean     read FEnabled  write FEnabled;
    property PathList  : TgmPathList read FPathList write SetPathList;
  published
    property BeforeMouseDown     : TNotifyEvent                  read FBeforeMouseDown     write FBeforeMouseDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseRightClick   : TgmPathManagerMouseRightClick read FOnMouseRightClick   write FOnMouseRightClick;
    property OnPathPanelDblClick : TNotifyEvent                  read FOnPathPanelDblClick write FOnPathPanelDblClick;
    property OnPathThumbDblClick : TNotifyEvent                  read FOnPathThumbDblClick write FOnPathThumbDblClick;
  end;

  { TgmPathPanelScrollingThread }

  TgmPathPanelScrollingThread = class(TThread)
  private
    FPathManager : TgmPathManager;  // pointer to an external path manager
  protected
    procedure Execute; override;
  public
    constructor Create(APathManager: TgmPathManager);
  end;

implementation

uses
{ Delphi }
  Windows,
  SysUtils,
  Forms,
  Graphics,
  Math,
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagicLib }
  gmMath;


const 
  MIN_OBJECT_SPAN   = 2;
  MAX_OBJECT_SPAN   = 20;
  PATH_PANEL_HEIGHT = 40;


{ TgmPathPanelCustomTheme }

constructor TgmPathPanelCustomTheme.Create;
begin
  inherited;

  FObjectSpan := 5;
end;

procedure TgmPathPanelCustomTheme.SetObjectSpan(AValue: Integer);
begin
  FObjectSpan := Clamp(AValue, MIN_OBJECT_SPAN, MAX_OBJECT_SPAN);
end;


{ TgmPathPanelStdTheme }

constructor TgmPathPanelStdTheme.Create;
begin
  inherited;

  FSpanColor       := clSilver32;
  FSelectedColor   := Color32(clHighlight);
  FDeselectedColor := Color32(clBtnFace);
end;

procedure  TgmPathPanelStdTheme.DrawPanelBorder(ABuffer: TBitmap32;
  const ARect: TRect);
begin
  ABuffer.LineS(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom, clWhite32);
  ABuffer.LineS(ARect.Left, ARect.Top, ARect.Right, ARect.Top, clWhite32);
  ABuffer.LineS(ARect.Right, ARect.Top, ARect.Right, ARect.Bottom, clGray32);
  ABuffer.LineS(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom, clGray32);
end;

function TgmPathPanelStdTheme.GetPanelAreaAtXY(APath: TgmPath;
  const APanelRect: TRect; const AX, AY: Integer): TgmPathPanelArea;
var
  LRect      : TRect;
  LSize      : TSize;
  LTestPoint : TPoint;
begin
  Result     := ppaOutOfPanels;
  LTestPoint := GR32.Point(AX, AY);
  LSize      := gmMath.GetRectSize(APanelRect);

  // if point on path thumbnail ...
  LRect.TopLeft := APanelRect.TopLeft;
  LRect.Right   := LRect.Left + APath.Thumbnail.Width;
  LRect.Bottom  := APanelRect.Bottom;

  if Windows.PtInRect(LRect, LTestPoint) then
  begin
    Result := ppaPathThumbnail;
    Exit;
  end;

  // if point on caption area ...
  LRect.Left  := LRect.Right + 1;
  LRect.Right := APanelRect.Right;

  if Windows.PtInRect(LRect, LTestPoint) then
  begin
    Result := ppaPathCaption;
  end;
end;

function TgmPathPanelStdTheme.GetSnapshot(APath: TgmPath;
  const AWidth, AHeight: Integer): TBitmap32;
var
  LBackColor : TColor32;
begin
  Result := nil;

  if not Assigned(APath) then
  begin
    Exit;
  end;

  if (AWidth <= 0) or (AHeight < PATH_PANEL_HEIGHT) then
  begin
    Exit;
  end;

  LBackColor := Color32(clBtnFace);

  Result             := TBitmap32.Create();
  Result.DrawMode    := dmBlend;
  Result.CombineMode := cmMerge;

  Result.SetSize(AWidth, AHeight);
  Result.Clear(LBackColor);

  Self.Paint(Result, APath, Result.BoundsRect);
  Result.FrameRectS(Result.BoundsRect, LBackColor);  // clear the border
end;

procedure TgmPathPanelStdTheme.Paint(ABuffer: TBitmap32; APath: TgmPath;
  const ARect: TRect);
var
  LCaptionColor : TColor32;
  LRect         : TRect;
  LSize         : TSize;
begin
  LSize := gmMath.GetRectSize(ARect);

  ABuffer.BeginUpdate();
  try
    // draw path thumbnail ...
    LRect.Left   := ARect.Left + FObjectSpan;
    LRect.Top    := ARect.Top + (LSize.cy - APath.Thumbnail.Height) div 2;
    LRect.Right  := LRect.Left + APath.Thumbnail.Width;
    LRect.Bottom := LRect.Top + APath.Thumbnail.Height;
    ABuffer.Draw(LRect.Left, LRect.Top, APath.Thumbnail);

    LRect.Top    := ARect.Top;
    LRect.Right  := LRect.Right + FObjectSpan;
    LRect.Bottom := ARect.Bottom;
    ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);

    // fill background color for the panel ...
    LRect.Left   := LRect.Right + 1;
    LRect.Top    := ARect.Top;
    LRect.Right  := ARect.Right;
    LRect.Bottom := ARect.Bottom;

    if APath.IsSelected then
    begin
      ABuffer.FillRectS(LRect, FSelectedColor);
      LCaptionColor := clWhite32;
    end
    else
    begin
      ABuffer.FillRectS(LRect, FDeselectedColor);
      LCaptionColor := clBlack32;
    end;

    // draw panel caption ...
    LRect.Left := LRect.Left + FObjectSpan;
    LRect.Top  := LRect.Top + ( LSize.cy - ABuffer.TextHeight(APath.PathName) ) div 2;

    if APath.IsNamed then
    begin
      ABuffer.Font.Style := ABuffer.Font.Style - [fsItalic];
    end
    else
    begin
      ABuffer.Font.Style := ABuffer.Font.Style + [fsItalic];
    end;

    ABuffer.RenderText(LRect.Left, LRect.Top, APath.PathName, 0, LCaptionColor);

    // draw panel border ...
    DrawPanelBorder(ABuffer, ARect);

  finally
    ABuffer.EndUpdate();
  end;
end;


{ TgmPathManager }

constructor TgmPathManager.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csReplicatable, csOpaque];

  Options := [pboAutoFocus, pboWantArrowKeys];
  TabStop := True; //to receive Tabkey and focusable as default

  FPathPanelTheme  := TgmPathPanelStdTheme.Create();
  FViewportOffset  := GR32.Point(0, 0);
  FWorkSize.cx     := 0;
  FWorkSize.cy     := 0;
  FWheelDelta      := PATH_PANEL_HEIGHT div 2;
  FLeftButtonDown  := False;
  FMouseButtonDown := False;
  FScrollBarLocked := False;
  FEnabled         := True;

  FVertScrollBar := TRangeBar.Create(Self);
  with FVertScrollBar do
  begin
    Parent       := Self;
    BorderStyle  := bsNone;
    Kind         := sbVertical;
    Align        := alRight;
    Width        := GetSystemMetrics(SM_CYVSCROLL) div 2;
    OnUserChange := ScrollHandler;
  end;

  FPathList := nil;

  // for rendering snapshot of a moving panel by mouse move
  FIsPanelMoving := False;
  FPanelSnapshot := nil;
  FScrollThread  := nil;

  FBeforeMouseDown     := nil;
  FOnMouseRightClick   := nil;
  FOnPathPanelDblClick := nil;
  FOnPathThumbDblClick := nil;
end;

destructor TgmPathManager.Destroy;
begin
  FPathList            := nil;
  FBeforeMouseDown     := nil;
  FOnMouseRightClick   := nil;
  FOnPathPanelDblClick := nil;
  FOnPathThumbDblClick := nil;

  StopScrollThread();
  FPanelSnapshot.Free();
  FVertScrollBar.Free();
  FPathPanelTheme.Free();
  
  inherited;
end;

function TgmPathManager.CanScrollDown: Boolean;
begin
  Result := (FViewportOffset.Y + FWorkSize.cy) > Self.ClientHeight; 
end;

function TgmPathManager.CanScrollUp: Boolean;
begin
  Result := FViewportOffset.Y < 0;
end;

procedure TgmPathManager.CheckLayout;
begin
  if Assigned(FPathList) then
  begin
    // update Work Size
    FWorkSize.cx         := Self.ClientWidth;
    FWorkSize.cy         := FPathList.Count * PATH_PANEL_HEIGHT;
    FVertScrollBar.Range := FWorkSize.cy;
  end
  else
  begin
    FViewportOffset.Y    := 0;
    FVertScrollBar.Range := 0;
  end;
end;

procedure TgmPathManager.DoPaintBuffer;
var
  i, y, LMaxY : Integer;
  LPath       : TgmPath;
  LRect       : TRect;
begin
  CheckLayout();
  Buffer.Clear( Color32(clBtnFace) );

  if Assigned(FPathList) then
  begin
    if FPathList.Count > 0 then
    begin
      for i := 0 to FPathList.MaxIndex do
      begin
        LPath := FPathList.Paths[i];
        LRect := GetPanelRect(i);

        // only render the panel that is in the viewport area ...
        if IsRectInViewport(LRect) then
        begin
          FPathPanelTheme.Paint(Buffer, LPath, LRect);
        end;
      end;

      // render panel snapshot, if any ...
      if (FIsPanelMoving) and
         (FMovingPanelIndex >= 0) and
         Assigned(FPanelSnapshot) then
      begin
        LMaxY := Min(FWorkSize.cy, Self.ClientHeight) - PATH_PANEL_HEIGHT;
        y     := FMouseY + FSnapshotOffsetY;
        y     := Clamp(y, 0, LMaxY);

        Buffer.Draw(0, y, FPanelSnapshot);

        FSnapshotTopLeft := GR32.Point(0, y);  // for other use ...
      end;
    end;
  end;

  Buffer.FrameRectS(Buffer.BoundsRect, clBlack32);
end;

function TgmPathManager.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := CanScrollDown();

  if Result then
  begin
    Self.Scroll(-FWheelDelta);
    Invalidate();
  end;
end;

function TgmPathManager.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := CanScrollUp();

  if Result then
  begin
    Self.Scroll(FWheelDelta);
    Invalidate();
  end;
end;

function TgmPathManager.GetPanelIndexAtXY(AX, AY: Integer): Integer;
var
  LYActual : Integer;
begin
  Result := -1;

  if Assigned(FPathList) and (FPathList.Count > 0) then
  begin
    LYActual := AY + Abs(FViewportOffset.Y);

    if LYActual < FWorkSize.cy then
    begin
      Result := LYActual div PATH_PANEL_HEIGHT;
    end;
  end;
end;

function TgmPathManager.GetPanelRect(const APanelIndex: Integer): TRect;
begin
  Result.Left   := 0;
  Result.Top    := APanelIndex * PATH_PANEL_HEIGHT + FViewportOffset.Y;
  Result.Right  := Self.ClientWidth - 1;
  Result.Bottom := Result.Top + PATH_PANEL_HEIGHT - 1;
end;

function TgmPathManager.GetPanelSnapshot(const APanelIndex: Integer): TBitmap32;
var
  LPath : TgmPath;
begin
  Result := nil;

  if Assigned(FPathList) and FPathList.IsValidIndex(APanelIndex) then
  begin
    LPath  := FPathList.Paths[APanelIndex];
    Result := FPathPanelTheme.GetSnapshot(LPath, Self.ClientWidth, PATH_PANEL_HEIGHT);
  end;
end;

function TgmPathManager.IsRectInViewport(const ARect: TRect): Boolean;
begin
  Result := Windows.PtInRect(Self.ClientRect, ARect.TopLeft) or
            Windows.PtInRect(Self.ClientRect, ARect.BottomRight);
end;

procedure TgmPathManager.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LIndex     : Integer;
  LPanelRect : TRect;
  LPath      : TgmPath;
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

    FMouseDownX    := X;
    FMouseDownY    := Y;
    FLastX         := X;
    FLastY         := Y;
    FIsPanelMoving := False;

    // dealing with double click on a panel
    if ssDouble in Shift then
    begin
      LIndex := GetPanelIndexAtXY(X, Y);

      if LIndex >= 0 then
      begin
        LPanelRect := GetPanelRect(LIndex);
        LPath      := FPathList.Paths[LIndex];

        case FPathPanelTheme.GetPanelAreaAtXY(LPath, LPanelRect, X, Y) of
          ppaPathThumbnail:
            begin
              if Assigned(FOnPathThumbDblClick) then
              begin
                FOnPathThumbDblClick(LPath);
              end;
            end;

          ppaPathCaption:
            begin
              if Assigned(FOnPathPanelDblClick) then
              begin
                FOnPathPanelDblClick(LPath);
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

  inherited;  // respond to OnMouseDown
end;

procedure TgmPathManager.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;

  // only moving selected path when left mouse button is pressed ...
  if FLeftButtonDown then
  begin
    if Abs(FMouseY - FMouseDownY) > 8 then
    begin
      if not FIsPanelMoving then
      begin
        FIsPanelMoving := (FMovingPanelIndex >= 0) and Assigned(FPanelSnapshot);
      end;
    end;

    if FIsPanelMoving then
    begin
      if not Assigned(FScrollThread) then
      begin
        FScrollThread := TgmPathPanelScrollingThread.Create(Self);
      end;

      if Y <> FLastY then
      begin
        Invalidate();
      end;
    end;

    FLastX := X;
    FLastY := Y;
  end;

  inherited;  // respond to OnMouseMove
end;

procedure TgmPathManager.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LClickedArea : TgmPathPanelArea;
  LIndex       : Integer;
  LPanelRect   : TRect;
  LPath        : TgmPath;
  LPos         : TPoint;
  LValidArea   : TRect;
begin
  if FMouseButtonDown then
  begin
    FLeftButtonDown  := False;
    FMouseButtonDown := False;

    if Assigned(FPathList) then
    begin
      if FIsPanelMoving then
      begin
        FIsPanelMoving := False;
        StopScrollThread();

        LValidArea.TopLeft := ClientRect.TopLeft;
        LValidArea.Right   := ClientWidth;
        LValidArea.Bottom  := Min(ClientHeight, FWorkSize.cy);

        if Windows.PtInRect( LValidArea, GR32.Point(X, Y) ) then
        begin
          LPos := GR32.Point(X, Y);
        end
        else
        begin
          // get center point of the snapshot of a moving path panel
          LPos.X := FSnapshotTopLeft.X + ClientWidth div 2;
          LPos.Y := FSnapshotTopLeft.Y + PATH_PANEL_HEIGHT div 2;
        end;

        LIndex := GetPanelIndexAtXY(LPos.X, LPos.Y);
        LPath  := FPathList.Paths[LIndex];

        if LPath.IsNamed then
        begin
          FPathList.Move(FMovingPanelIndex, LIndex);
          FPathList.SelectPath(LIndex);

          LPanelRect := Self.GetPanelRect(LIndex);

          // If the path order is changed, the external callbacks should to
          // take care of the refreshing of the GUI of path manager,
          // otherwise, we should to refresh the view by ourselves.
          ScrollPanelInViewport(LPanelRect);
          Invalidate();
        end
        else
        begin
          // If the target path is a Work Path,
          // then terminate the panel moving.
          Invalidate();
        end;
      end
      else
      begin
        // if click on any of a path panel ...
        LIndex       := GetPanelIndexAtXY(X, Y);
        LPath        := nil;
        LClickedArea := ppaOutOfPanels;

        if LIndex >= 0 then
        begin
          LPanelRect   := GetPanelRect(LIndex);
          LPath        := FPathList.Paths[LIndex];
          LClickedArea := FPathPanelTheme.GetPanelAreaAtXY(LPath, LPanelRect, X, Y);

          case LClickedArea of
            ppaPathThumbnail,
            ppaPathCaption:
              begin
                FPathList.SelectPath(LIndex);
              end;
          end;

          if ScrollPanelInViewport(LPanelRect) then
          begin
            Invalidate();
          end;
        end
        else
        begin
          // If not clicked on any of path panels, but clicked on the
          // path manager, then deselect all paths, and invoke the
          // callback function.
          if (X >= 0) and (X < Self.ClientWidth) and
             (Y >= 0) and (Y < Self.ClientHeight) then
          begin
            FPathList.DeselectAllPaths();

            if Assigned(FPathList.OnSelectionChanged) then
            begin
              FPathList.OnSelectionChanged(FPathList);
            end;
          end;
        end;

        // invoke callback function when mouse right button released ...
        if (Button = mbRight) and (not (ssShift in Shift) ) then
        begin
          if Assigned(FOnMouseRightClick) then
          begin
            FOnMouseRightClick(LPath, LClickedArea);
          end;
        end;
      end;
    end;
  end;

  inherited;  // respond to OnMouseUp
end;

procedure TgmPathManager.PreparePanelSnapshotRendering(
  const AMouseX, AMouseY: Integer);
var
  LPanelIndex : Integer;
  LPanelRect  : TRect;
  LPath       : TgmPath;
begin
  FMovingPanelIndex := -1;

  if Assigned(FPathList) and (FPathList.Count > 0) then
  begin
    if Assigned(FPanelSnapshot) then
    begin
      FreeAndNil(FPanelSnapshot);
    end;

    LPanelIndex := GetPanelIndexAtXY(AMouseX, AMouseY);
    LPath       := FPathList.Paths[LPanelIndex];

    // We can only move a named path, not a Work Path.
    if Assigned(LPath) and LPath.IsNamed then
    begin
      FMovingPanelIndex          := LPanelIndex;
      LPanelRect                 := Self.GetPanelRect(LPanelIndex);
      FSnapshotOffsetY           := LPanelRect.Top - AMouseY;
      FPanelSnapshot             := GetPanelSnapshot(LPanelIndex);
      FPanelSnapshot.MasterAlpha := $7F;
    end;
  end;
end;

procedure TgmPathManager.Resize;
var
  LDelta  : Integer;
  LHeight : Integer;
begin
  inherited;

  LHeight := FWorkSize.cy + FViewportOffset.Y;

  if LHeight < Self.ClientHeight then
  begin
    LDelta := Self.ClientHeight - LHeight;

    Inc(FViewportOffset.Y, LDelta);

    if FViewportOffset.Y > 0 then
    begin
      FViewportOffset.Y := 0;
    end;

    FScrollBarLocked := True;
    try
      FVertScrollBar.Position := Abs(FViewportOffset.Y);
    finally
      FScrollBarLocked := False;
    end;
  end;
end;

procedure TgmPathManager.Scroll(Dy: Integer);
var
  LHeight : Integer;
begin
  FViewportOffset.Y := FViewportOffset.Y + Dy;

  // limit the scrolling amount
  LHeight := FViewportOffset.Y + FWorkSize.cy;
  if LHeight < Self.ClientHeight then
  begin
    Inc(FViewportOffset.Y, Self.ClientHeight - LHeight);
  end;

  if FViewportOffset.Y > 0 then
  begin
    FViewportOffset.Y := 0;
  end;

  // update scroll bar
  FScrollBarLocked := True;
  try
    FVertScrollBar.Position := Abs(FViewportOffset.Y);
  finally
    FScrollBarLocked := False;
  end;
end;

procedure TgmPathManager.ScrollHandler(Sender: TObject);
begin
  if Sender = FVertScrollBar then
  begin
    if not FScrollBarLocked then
    begin
      FViewportOffset.Y := 0 - Round(FVertScrollBar.Position);
      Invalidate();
    end;
  end;
end;

function TgmPathManager.ScrollPanelInViewport(const APanelRect: TRect): Boolean;
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

function TgmPathManager.ScrollSelectedPanelInViewport: Boolean;
var
  LIndex    : Integer;
  LPathRect : TRect;
begin
  Result := False;

  if Assigned(FPathList) then
  begin
    CheckLayout();

    LIndex := FPathList.SelectedIndex;
    if LIndex >= 0 then
    begin
      LPathRect := GetPanelRect(LIndex);
      Result    := ScrollPanelInViewport(LPathRect);
    end;
  end;
end;

procedure TgmPathManager.SetPathList(const AValue: TgmPathList);
begin
  FPathList := AValue;

  FViewportOffset := GR32.Point(0, 0);

  if Assigned(FPathList) and (FPathList.Count > 0) then
  begin
    if FPathList.SelectedIndex >= 0 then
    begin
      // get the selected panel be shown fully in the viewport ...
      ScrollSelectedPanelInViewport();
    end
    else
    begin
      CheckLayout();
      Self.FVertScrollBar.Position := 0.0;
    end;
  end;

  Invalidate();
end;

procedure TgmPathManager.StopScrollThread;
begin
  if Assigned(FScrollThread) then
  begin
    FScrollThread.Terminate();
    FScrollThread.WaitFor();
    FreeAndNil(FScrollThread);
  end;
end;


{ TgmPathPanelScrollingThread }

constructor TgmPathPanelScrollingThread.Create(APathManager: TgmPathManager);
begin
  FPathManager    := APathManager;
  FreeOnTerminate := False;

  inherited Create(False);

  Priority := tpLower;
end;

procedure TgmPathPanelScrollingThread.Execute;
begin
  if Assigned(FPathManager) then
  begin
    while (not Terminated) do
    begin
      with FPathManager do
      begin
        if FIsPanelMoving and (FMouseX >= 0) and (FMouseX < ClientWidth) then
        begin
          if FMouseY < 0 then
          begin
            if CanScrollUp() then
            begin
              Scroll(PATH_PANEL_HEIGHT);
              Invalidate();
            end;
          end
          else if FMouseY > ClientHeight then
          begin
            if CanScrollDown() then
            begin
              Scroll(-PATH_PANEL_HEIGHT);
              Invalidate();
            end;
          end;

          Sleep(500);
        end;
      end;
    end;
  end;
end;


end.
