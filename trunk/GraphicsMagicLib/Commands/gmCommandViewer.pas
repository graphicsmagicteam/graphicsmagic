unit gmCommandViewer;

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

// Update Date: 2017/01/12

interface

uses
{ Delphi }
  Windows, Classes, Controls,
{ Graphics32 }
  GR32,
  GR32_Image,
  GR32_RangeBars,
{ GraphicsMagicLib }
  gmHistoryCommands;
  

type
  { TgmCustomCommandViewerTheme }

  TgmCustomCommandViewerTheme = class(TObject)
  private
    procedure SetObjectSpan(AValue: Integer);
  protected
    FObjectSpan : Integer;
  public
    constructor Create;

    procedure PaintCommandPanel(ABuffer: TBitmap32; ACommand: TgmCustomCommand;
      const APanelRect: TRect); virtual; abstract;

    procedure PaintSnapshotPanel(ABuffer: TBitmap32; ASnapshot: TgmSnapshot;
      const APanelRect: TRect); virtual; abstract;

    property ObjectSpan : Integer read FObjectSpan write SetObjectSpan;
  end;


  { TgmDefaultCommandViewerTheme }

  TgmDefaultCommandViewerTheme = class(TgmCustomCommandViewerTheme)
  private
    FSpanColor       : TColor32;
    FSelectedColor   : TColor32;
    FDeselectedColor : TColor32;
  protected
    procedure DrawPanelBorder(ABuffer: TBitmap32; const ARect: TRect);
  public
    constructor Create;

    procedure PaintCommandPanel(ABuffer: TBitmap32; ACommand: TgmCustomCommand;
      const APanelRect: TRect); override;

    procedure PaintSnapshotPanel(ABuffer: TBitmap32; ASnapshot: TgmSnapshot;
      const APanelRect: TRect); override;
  end;


  { TgmCommandViewer }
  
  TgmCommandViewer = class(TCustomPaintBox32)
  private
    FCommandManager       : TgmCommandManager;             // pointer to a command manager
    FVertScroller         : TRangeBar;
    FScrollLocked         : Boolean;                       // lock up the execution of the scroll bar
    FRenderTheme          : TgmDefaultCommandViewerTheme;  // for rendering the manager
    FViewportOffset       : TPoint;                        // offset of the viewport
    FWorkSize             : TSize;                         // maximum scrollable area
    FLeftButtonDown       : Boolean;                       // if the left mouse button is pressed
    FMouseButtonDown      : Boolean;                       // if the left/right mouse button is pressed
    FEnabled              : Boolean;                       // if the viewer is enabled
    FWheelDelta           : Integer;
    FBeforeMouseDown      : TNotifyEvent;
    FOnCommandDblClick    : TNotifyEvent;
    FOnCommandRightClick  : TNotifyEvent;
    FOnSnapshotDblClick   : TNotifyEvent;
    FOnSnapshotRightClick : TNotifyEvent;

    function CanScrollDown: Boolean;
    function CanScrollUp: Boolean;
    function GetCommandIndexAtXY(AX, AY: Integer): Integer;
    function GetCommandPanelRect(const AIndex: Integer): TRect;
    function GetSnapshotIndexAtXY(AX, AY: Integer): Integer;
    function GetSnapshotPanelRect(const AIndex: Integer): TRect;
    function IsRectInViewport(const ARect: TRect): Boolean;  // dertermine if any part of a rect is in the viewport

    procedure SetCommandManager(const AValue: TgmCommandManager);

    // callbacks
    procedure ScrollHandler(Sender: TObject);
  protected
    function ScrollPanelInViewport(const APanelRect: TRect): Boolean;

    procedure CheckLayout; virtual;
    procedure DoPaintBuffer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Scroll(Dy: Integer); virtual;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ScrollSelectedCommandPanelInViewport: Boolean;
    function ScrollSelectedSnapshotPanelInViewport: Boolean;

    procedure RefreshViewer;
    procedure Resize; override;

    property CommandManager       : TgmCommandManager read FCommandManager       write SetCommandManager;
    property IsEnabled            : Boolean           read FEnabled              write FEnabled;
    property OnCommandDblClick    : TNotifyEvent      read FOnCommandDblClick    write FOnCommandDblClick;
    property OnCommandRightClick  : TNotifyEvent      read FOnCommandRightClick  write FOnCommandRightClick;
    property OnSnapshotDblClick   : TNotifyEvent      read FOnSnapshotDblClick   write FOnSnapshotDblClick;
    property OnSnapshotRightClick : TNotifyEvent      read FOnSnapshotRightClick write FOnSnapshotRightClick;
  published
    property BeforeMouseDown : TNotifyEvent read FBeforeMouseDown write FBeforeMouseDown;
  end;

implementation

uses
{ Delphi }
  Forms,
  Graphics,
  Math,
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagicLib }
  gmAlphaFuncs,
  gmMath;

const
  SNAPSHOT_PANEL_HEIGHT = 40;
  COMMAND_PANEL_HEIGHT  = 24; 
  MAX_OBJECT_SPAN       = 20;
  MIN_OBJECT_SPAN       = 2;
  

{ TgmCommandViewer }

constructor TgmCommandViewer.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csReplicatable, csOpaque];
    
  Options := [pboAutoFocus, pboWantArrowKeys];
  TabStop := True; // to receive Tabkey and focusable as default

  // create a vertical scroll bar
  FVertScroller := TRangeBar.Create(Self);
  with FVertScroller do
  begin
    Parent       := Self;
    BorderStyle  := bsNone;
    Kind         := sbVertical;
    Align        := alRight;
    Width        := getSystemMetrics(SM_CYVSCROLL) div 2;
    OnUserChange := ScrollHandler;
  end;

  FRenderTheme := TgmDefaultCommandViewerTheme.Create();

  FCommandManager  := nil;
  FViewportOffset  := Point(0, 0);
  FScrollLocked    := False;
  FWorkSize.cx     := 0;
  FWorkSize.cy     := 0;
  FLeftButtonDown  := False;
  FMouseButtonDown := False;
  FEnabled         := True;
  FWheelDelta      := COMMAND_PANEL_HEIGHT;

  // callbacks
  FBeforeMouseDown      := nil;
  FOnCommandDblClick    := nil;
  FOnCommandRightClick  := nil;
  FOnSnapshotDblClick   := nil;
  FOnSnapshotRightClick := nil;
end;

destructor TgmCommandViewer.Destroy;
begin
  FBeforeMouseDown      := nil;
  FOnCommandDblClick    := nil;
  FOnCommandRightClick  := nil;
  FOnSnapshotDblClick   := nil;
  FOnSnapshotRightClick := nil;

  FRenderTheme.Free();
  FVertScroller.Free();

  inherited;
end;

function TgmCommandViewer.CanScrollDown: Boolean;
begin
  Result := ( (FViewportOffset.Y + FWorkSize.cy) > Self.ClientHeight );
end;

function TgmCommandViewer.CanScrollUp: Boolean;
begin
  Result := (FViewportOffset.Y < 0);
end;

procedure TgmCommandViewer.CheckLayout;
begin
  FWorkSize.cx := Self.ClientWidth;
  FWorkSize.cy := 0;

  // update Work Size ...

  if Assigned(FCommandManager) then
  begin
    if FCommandManager.SnapshotList.Count > 0 then
    begin
      FWorkSize.cy := FCommandManager.SnapshotList.Count * SNAPSHOT_PANEL_HEIGHT;
    end;

    if FCommandManager.CommandList.Count > 0 then
    begin
      FWorkSize.cy := FWorkSize.cy + FCommandManager.CommandList.Count * COMMAND_PANEL_HEIGHT;
    end;
  end;

  FVertScroller.Range := FWorkSize.cy;
end;

function TgmCommandViewer.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  if CanScrollDown() then
  begin
    Result := True;
    Self.Scroll(-FWheelDelta);
    Invalidate();
  end
  else
  begin
    Result := False;
  end;
end;

function TgmCommandViewer.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  if CanScrollUp() then
  begin
    Result := True;
    Self.Scroll(FWheelDelta);
    Invalidate();
  end
  else
  begin
    Result := False;
  end;
end;

procedure TgmCommandViewer.DoPaintBuffer;
var
  i         : Integer;
  LRect     : TRect;
  LCommand  : TgmCustomCommand;
  LSnapshot : TgmSnapshot;
begin
  CheckLayout();
  Buffer.Clear( Color32(clBtnFace) );

  // rendering snapshot panels and command panels ...
  if Assigned(FCommandManager) then
  begin
    // rendering the snapshot panels ...
    if FCommandManager.SnapshotList.Count > 0 then
    begin
      for i := 0 to (FCommandManager.SnapshotList.Count - 1) do
      begin
        LSnapshot := FCommandManager.SnapshotList.Snapshots[i];
        LRect     := GetSnapshotPanelRect(i);

        if IsRectInViewport(LRect) then
        begin
          FRenderTheme.PaintSnapshotPanel(Buffer, LSnapshot, LRect);
        end;
      end;
    end;

    // rendering the command panels ...
    if FCommandManager.CommandList.Count > 0 then
    begin
      for i := 0 to (FCommandManager.CommandList.Count - 1) do
      begin
        LCommand := FCommandManager.CommandList.Commands[i];
        LRect    := GetCommandPanelRect(i);

        if IsRectInViewport(LRect) then
        begin
          FRenderTheme.PaintCommandPanel(Buffer, LCommand, LRect);
        end;
      end;
    end;
  end; 

  Buffer.FrameRectS(Buffer.BoundsRect, clBlack32);
end;

function TgmCommandViewer.GetCommandIndexAtXY(AX, AY: Integer): Integer;
var
  LCommandAreaTop    : Integer;
  LCommandAreaBottom : Integer;
  LYActual           : Integer;
begin
  Result := -1;
  
  if Assigned(FCommandManager) then
  begin
    with FCommandManager do
    begin
      if CommandList.Count > 0 then
      begin
        LCommandAreaTop    := SnapshotList.Count * SNAPSHOT_PANEL_HEIGHT;
        LCommandAreaBottom := LCommandAreaTop + CommandList.Count * COMMAND_PANEL_HEIGHT;
        LYActual           := AY + Abs(FViewportOffset.Y);

        if (LYActual > LCommandAreaTop) and (LYActual < LCommandAreaBottom) then
        begin
          Result := (LYActual - LCommandAreaTop) div Command_PANEL_HEIGHT;
        end;
      end;
    end;
  end;
end;

function TgmCommandViewer.GetCommandPanelRect(const AIndex: Integer): TRect;
begin
  if Assigned(FCommandManager) then
  begin
    with FCommandManager do
    begin
      if CommandList.IsValidIndex(AIndex) then
      begin
        Result.Top    := SnapshotList.Count * SNAPSHOT_PANEL_HEIGHT;
        Result.Top    := Result.Top + AIndex * COMMAND_PANEL_HEIGHT;
        Result.Top    := Result.Top + FViewportOffset.Y;
        Result.Right  := Self.ClientWidth - 1;
        Result.Bottom := Result.Top + COMMAND_PANEL_HEIGHT - 1;
      end;
    end;
  end
  else
  begin
    Result := Rect(0, 0, 0, 0);
  end;
end;

function TgmCommandViewer.GetSnapshotIndexAtXY(AX, AY: Integer): Integer;
var
  LSnapshotAreaHeight : Integer;
  LYActual            : Integer;
begin
  Result := -1;
  
  if Assigned(FCommandManager) then
  begin
    with FCommandManager do
    begin
      if SnapshotList.Count > 0 then
      begin
        LSnapshotAreaHeight := SnapshotList.Count * SNAPSHOT_PANEL_HEIGHT;
        LYActual            := AY + Abs(FViewportOffset.Y);

        if LYActual < LSnapshotAreaHeight then
        begin
          Result := LYActual div SNAPSHOT_PANEL_HEIGHT;
        end;
      end;
    end;
  end;
end;

function TgmCommandViewer.GetSnapshotPanelRect(const AIndex: Integer): TRect;
begin
  if Assigned(FCommandManager) then
  begin
    with FCommandManager do
    begin
      if SnapshotList.IsValidIndex(AIndex) then
      begin
        Result.Top    := AIndex * SNAPSHOT_PANEL_HEIGHT + FViewportOffset.Y;
        Result.Right  := Self.ClientWidth - 1;
        Result.Bottom := Result.Top + SNAPSHOT_PANEL_HEIGHT - 1;
      end;
    end;
  end
  else
  begin
    Result := Rect(0, 0, 0, 0);
  end;
end;

// dertermine if any part of a rect is in the viewport
function TgmCommandViewer.IsRectInViewport(const ARect: TRect): Boolean;
var
  LRect : TRect;
begin
  LRect.Left   := Min(ARect.Left, ARect.Right);
  LRect.Top    := Min(ARect.Top, ARect.Bottom);
  LRect.Right  := Max(ARect.Left, ARect.Right);
  LRect.Bottom := Max(ARect.Top, ARect.Bottom);
  
  Result := Windows.PtInRect(Self.ClientRect, LRect.TopLeft) or
            Windows.PtInRect(Self.ClientRect, LRect.BottomRight);
end;

procedure TgmCommandViewer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LIndex : Integer;
begin
  if Assigned(FBeforeMouseDown) then
  begin
    FBeforeMouseDown(Self);
  end;

  if not FEnabled then
  begin
    Exit;
  end;

  if Button in [mbLeft, mbRight] then
  begin
    // dealing with double-click on a panel
    if ssDouble in Shift then
    begin
      // if double-clicked on a snapshot ...
      LIndex := GetSnapshotIndexAtXY(X, Y);
      if LIndex >= 0 then
      begin
        if Assigned(FOnSnapshotDblClick) then
        begin
          FOnSnapshotDblClick(FCommandManager.SnapshotList.Snapshots[LIndex]);
        end;
      end
      else
      begin
        LIndex := GetCommandIndexAtXY(X, Y);
        if LIndex >= 0 then
        begin
          if Assigned(FOnCommandDblClick) then
          begin
            FOnCommandDblClick(FCommandManager.CommandList.Commands[LIndex]);
          end;
        end;
      end;
    end
    else
    begin
      // If the Double-Click has not been fired, we mark
      // the mouse button is pressed. Doing this is for
      // preventing from the Double-Click opens a dialog and
      // after the dialog is closed, the following marks are
      // still in True state.
      FMouseButtonDown := True;
      FLeftButtonDown  := (Button = mbLeft);
    end;
  end;

  inherited;  // respond to OnMouseDown
end;

procedure TgmCommandViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;  // respond to OnMouseMove
end;

procedure TgmCommandViewer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LIndex : Integer;
begin
  if FMouseButtonDown then
  begin
    FLeftButtonDown  := False;
    FMouseButtonDown := False;

    if Assigned(FCommandManager) then
    begin
      // if clicked on a snapshot ...
      LIndex := GetSnapshotIndexAtXY(X, Y);
      if LIndex >= 0 then
      begin
        FCommandManager.SelectSnapshot(LIndex);
        
        if ScrollSelectedSnapshotPanelInViewport() then
        begin
          Invalidate();
        end;

        // invoke callback function when mouse right button is released ...
        if Button = mbRight then
        begin
          if Assigned(FOnSnapshotRightClick) then
          begin
            FOnSnapshotRightClick(FCommandManager.SnapshotList.Snapshots[LIndex]);
          end;
        end;
      end
      else
      begin
        // if clicked on a command ...
        LIndex := GetCommandIndexAtXY(X, Y);
        
        if LIndex >= 0 then
        begin
          FCommandManager.SelectCommand(LIndex);

          if ScrollSelectedSnapshotPanelInViewport() then
          begin
            Invalidate();
          end;

          if Button = mbRight then
          begin
            if Assigned(FOnCommandRightClick) then
            begin
              FOnCommandRightClick(FCommandManager.CommandList.Commands[LIndex]);
            end;
          end;
        end;
      end;
    end;
  end;

  inherited;  // respond to OnMouseUp
end;

// reset the layout of the viewer
procedure TgmCommandViewer.RefreshViewer;
var
  LVisibleHeight : Integer;
begin
  CheckLayout();

  // scroll the viewport down to fillful the client area of the viewer
  LVisibleHeight := FWorkSize.cy + FViewportOffset.Y;
  if Self.ClientHeight > LVisibleHeight then
  begin
    Self.Scroll(Self.ClientHeight - LVisibleHeight);
  end;

  if Assigned(FCommandManager) then
  begin
    if FCommandManager.SnapshotList.SelectedIndex >= 0 then
    begin
      ScrollSelectedSnapshotPanelInViewport();
    end
    else if FCommandManager.CommandList.SelectedIndex >= 0 then
    begin
      ScrollSelectedCommandPanelInViewport();
    end;
  end;

  Invalidate();
end;

procedure TgmCommandViewer.Resize;
var
  LHeight : Integer;
  LDelta  : Integer;
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

    FScrollLocked := True;
    try
      FVertScroller.Position := Abs(FViewportOffset.Y);
    finally
      FScrollLocked := False;
    end;
  end;
end;

procedure TgmCommandViewer.Scroll(Dy: Integer);
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

  // update the scroll bar
  FScrollLocked := True;
  try
    FVertScroller.Position := Abs(FViewportOffset.Y);
  finally
    FScrollLocked := False;
  end;
end;

procedure TgmCommandViewer.ScrollHandler(Sender: TObject);
begin
  if Sender = FVertScroller then
  begin
    if not FScrollLocked then
    begin
      FViewportOffset.Y := 0 - Round(FVertScroller.Position);
      Invalidate();
    end;
  end;
end;

function TgmCommandViewer.ScrollPanelInViewport(
  const APanelRect: TRect): Boolean;
begin
  if APanelRect.Top < Self.ClientRect.Top then
  begin
    Self.Scroll(Self.ClientRect.Top - APanelRect.Top);
    Result := True;
  end
  else if APanelRect.Bottom > Self.ClientRect.Bottom then
  begin
    Self.Scroll(Self.ClientRect.Bottom - APanelRect.Bottom);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TgmCommandViewer.ScrollSelectedCommandPanelInViewport: Boolean;
var
  LIndex     : Integer;
  LPanelRect : TRect;
begin
  if Assigned(FCommandManager) then
  begin
    CheckLayout();

    LIndex     := FCommandManager.CommandList.SelectedIndex;
    LPanelRect := GetCommandPanelRect(LIndex);
    Result     := ScrollPanelInViewport(LPanelRect);

    if Result then
    begin
      Self.Invalidate();
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function TgmCommandViewer.ScrollSelectedSnapshotPanelInViewport: Boolean;
var
  LIndex     : Integer;
  LPanelRect : TRect;
begin
  if Assigned(FCommandManager) then
  begin
    CheckLayout();

    LIndex     := FCommandManager.SnapshotList.SelectedIndex;
    LPanelRect := GetSnapshotPanelRect(LIndex);
    Result     := ScrollPanelInViewport(LPanelRect);

    if Result then
    begin
      Self.Invalidate();
    end;
  end
  else
  begin
    Result := False;
  end;
end;

procedure TgmCommandViewer.SetCommandManager(const AValue: TgmCommandManager);
begin
  FCommandManager := AValue;

  // make the selected panel fully showing in the viewport ...
  FViewportOffset := Point(0, 0);

  if Assigned(FCommandManager) then
  begin
    if FCommandManager.SnapshotList.SelectedIndex >= 0 then
    begin
      ScrollSelectedSnapshotPanelInViewport();
    end
    else if FCommandManager.CommandList.SelectedIndex >= 0 then
    begin
      ScrollSelectedCommandPanelInViewport();
    end;
  end;

  Invalidate();
end;

{ TgmCustomCommandViewerTheme }

constructor TgmCustomCommandViewerTheme.Create;
begin
  inherited;

  FObjectSpan := 5;
end;

procedure TgmCustomCommandViewerTheme.SetObjectSpan(AValue: Integer);
begin
  FObjectSpan := Clamp(AValue, MIN_OBJECT_SPAN, MAX_OBJECT_SPAN);
end;

{ TgmDefaultCommandViewerTheme }

constructor TgmDefaultCommandViewerTheme.Create;
begin
  inherited;

  FSpanColor       := clSilver32;
  FSelectedColor   := Color32(clHighlight);
  FDeselectedColor := Color32(clBtnFace);
end;

procedure TgmDefaultCommandViewerTheme.DrawPanelBorder(ABuffer: TBitmap32;
  const ARect: TRect);
begin
  ABuffer.LineS(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom, clWhite32);
  ABuffer.LineS(ARect.Left, ARect.Top, ARect.Right, ARect.Top, clWhite32);
  ABuffer.LineS(ARect.Right, ARect.Top, ARect.Right, ARect.Bottom, clGray32);
  ABuffer.LineS(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom, clGray32);
end;

procedure TgmDefaultCommandViewerTheme.PaintCommandPanel(ABuffer: TBitmap32;
  ACommand: TgmCustomCommand; const APanelRect: TRect);
var
  LPanelSize    : TSize;
  LRect         : TRect;
  LBackColor    : TColor32;
  LCommandColor : TColor32;
begin
  if ( not Assigned(ABuffer) ) or ( not Assigned(ACommand) ) then
  begin
    Exit;
  end;

  LPanelSize := gmMath.GetRectSize(APanelRect);

  ABuffer.BeginUpdate();
  try
    if ACommand.IsSelected then
    begin
      LBackColor    := FSelectedColor;
      LCommandColor := clWhite32;
    end
    else
    begin
      LBackColor := FDeselectedColor;

      if ACommand.IsExecuted then
      begin
        LCommandColor := clBlack32;
      end
      else
      begin
        LCommandColor := clGray32;
      end;
    end;

    ACommand.Icon.Clear(LCommandColor);
    ReplaceAlphaChannelWithMask(ACommand.Icon, ACommand.IconMask);

    // draw background of the panel ...
    ABuffer.FillRectS(APanelRect, LBackColor);

    // draw icon of the command
    LRect.Left   := APanelRect.Left + FObjectSpan;
    LRect.Top    := APanelRect.Top + (LPanelSize.cy - ACommand.Icon.Height) div 2;
    LRect.Right  := LRect.Left + ACommand.Icon.Width;
    LRect.Bottom := LRect.Top + ACommand.Icon.Height;
    ABuffer.Draw(LRect.Left, LRect.Top, ACommand.Icon);

    // draw panel caption
    LRect.Left   := LRect.Right + FObjectSpan;
    LRect.Top    := APanelRect.Top + ( LPanelSize.cy - ABuffer.TextHeight(ACommand.CommandName) ) div 2;
    LRect.Right  := APanelRect.Right;
    LRect.Bottom := LRect.Top + ABuffer.TextHeight(ACommand.CommandName);
    ABuffer.RenderText(LRect.Left, LRect.Top, ACommand.CommandName, 0, LCommandColor);

    // draw panel border
    DrawPanelBorder(ABuffer, APanelRect);
  finally
    ABuffer.EndUpdate();
  end;
end;

procedure TgmDefaultCommandViewerTheme.PaintSnapshotPanel(ABuffer: TBitmap32;
  ASnapshot: TgmSnapshot; const APanelRect: TRect);
var
  LCaptionColor : TColor32;
  LRect         : TRect;
  LPanelSize    : TSize;
begin
  if ( not Assigned(ABuffer) ) or ( not Assigned(ASnapshot) ) then
  begin
    Exit;
  end;

  LPanelSize := gmMath.GetRectSize(APanelRect);

  ABuffer.BeginUpdate();
  try
    // draw snapshot thumbnail ...
    LRect.Left   := APanelRect.Left + FObjectSpan;
    LRect.Top    := APanelRect.Top + (LPanelSize.cy - ASnapshot.Thumbnail.Height) div 2;
    LRect.Right  := LRect.Left + ASnapshot.Thumbnail.Width;
    LRect.Bottom := LRect.Top + ASnapshot.Thumbnail.Height;
    ABuffer.Draw(LRect.Left, LRect.Top, ASnapshot.Thumbnail);

    LRect.Top    := APanelRect.Top;
    LRect.Right  := LRect.Right + FObjectSpan;
    LRect.Bottom := APanelRect.Bottom;
    ABuffer.LineS(LRect.Right, LRect.Top, LRect.Right, LRect.Bottom, FSpanColor);

    // fill background color for the panel
    LRect.Left   := LRect.Right + 1;
    LRect.Top    := APanelRect.Top;
    LRect.Right  := APanelRect.Right;
    LRect.Bottom := APanelRect.Bottom;

    if ASnapshot.IsSelected then
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
    LRect.Top  := LRect.Top + ( LPanelSize.cy - ABuffer.TextHeight(ASnapshot.Name) ) div 2;
    ABuffer.RenderText(LRect.Left, LRect.Top, ASnapshot.Name, 0, LCaptionColor);

    // draw panel border
    DrawPanelBorder(ABuffer, APanelRect);
  finally
    ABuffer.EndUpdate();
  end;
end;


end.
