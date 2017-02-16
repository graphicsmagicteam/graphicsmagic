////////////////////////////////////////////////////////////////////////////////
// File:       PegtopDesktopMagnifiers.pas
// Classes:    TPegtopDesktopMagnifier
// Version:    1.00
// Date:       18 Jan 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// PegtopDesktopMagnifiers shows the desktop under the cursor magnified.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopDesktopMagnifiers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TPegtopDesktopMagnifierOption = (pmoFollowCursor, pmoDrawCursor, pmoInvertColors);
  TPegtopDesktopMagnifierOptions = set of TPegtopDesktopMagnifierOption;
  TPegtopDesktopMagnifierCrossHairs = (pmcNone, pmcOpenCross, pmcContinuousCross, pmcSmallCross);
  TPegtopDesktopMagnifierIndicator = (pmiNone, pmiCaption, pmiCursorPos, pmiHexCode, pmiZoom);

  TPegtopCustomDesktopMagnifier = class(TGraphicControl)
  private
    FPosition: TPoint;
    FZoom: Integer;
    FTargetColor: TColor;
    FOptions: TPegtopDesktopMagnifierOptions;
    FCrossHairs: TPegtopDesktopMagnifierCrossHairs;
    FIndicator: array[0..3] of TPegtopDesktopMagnifierIndicator;
    FBorderWidth: Integer;
    FOnTargetColorChange: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure WMSetText(var Msg: TMessage); message WM_SETTEXT;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    function GetIndicatorText(Index: Integer): String;
    procedure SetPosition(Value: TPoint);
    procedure SetZoom(Value: Integer);
    procedure SetOptions(Value: TPegtopDesktopMagnifierOptions);
    procedure SetCrossHairs(Value: TPegtopDesktopMagnifierCrossHairs);
    function GetIndicator(Index: Integer): TPegtopDesktopMagnifierIndicator;
    procedure SetIndicator(Index: Integer; Value: TPegtopDesktopMagnifierIndicator);
    procedure SetBorderWidth(Value: Integer);
  protected
    procedure Paint; override;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property TargetColor: TColor read FTargetColor;
    property Position: TPoint read FPosition write SetPosition;
  published
    property Zoom: Integer read FZoom write SetZoom default 5;
    property Options: TPegtopDesktopMagnifierOptions read FOptions write SetOptions;
    property CrossHairs: TPegtopDesktopMagnifierCrossHairs read FCrossHairs write SetCrossHairs;
    property IndicatorTopLeft: TPegtopDesktopMagnifierIndicator index 0 read GetIndicator write SetIndicator;
    property IndicatorTopRight: TPegtopDesktopMagnifierIndicator index 1 read GetIndicator write SetIndicator;
    property IndicatorBottomLeft: TPegtopDesktopMagnifierIndicator index 2 read GetIndicator write SetIndicator;
    property IndicatorBottomRight: TPegtopDesktopMagnifierIndicator index 3 read GetIndicator write SetIndicator;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property OnTargetColorChange: TNotifyEvent read FOnTargetColorChange write FOnTargetColorChange;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Align;
    property Anchors;
    property Caption;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

  TPegtopDesktopMagnifier = class(TPegtopCustomDesktopMagnifier)
  private
    FWindowHandle: THandle;
    FInterval: Integer;
    FIntervalForce: Integer;
    FRecentRefreshTick: Integer;
    procedure TimerWndProc(var Msg: TMessage);
    procedure SetInterval(Value: Integer);
    procedure SetIntervalForce(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Interval: Integer read FInterval write SetInterval; // refresh interval when mouse is moved
    property IntervalForce: Integer read FIntervalForce write SetIntervalForce; // refresh interval when mouse isn't moved (screen might change)
  end;

  TPegtopDesktopMagnifierForm = class(TCustomForm)
  private
    FMagnifier: TPegtopCustomDesktopMagnifier;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure AutoPosition(const Fixspot, Hotspot: TPoint; const Reset: Boolean);
    property Magnifier: TPegtopCustomDesktopMagnifier read FMagnifier;
  end;

function GetDesktopMagnifierForm: TPegtopDesktopMagnifierForm;

implementation

var
  DesktopMagnifierForm: TPegtopDesktopMagnifierForm;

function GetDesktopMagnifierForm: TPegtopDesktopMagnifierForm;
begin
  if DesktopMagnifierForm = NIL then
    DesktopMagnifierForm := TPegtopDesktopMagnifierForm.CreateNew(NIL);
  Result := DesktopMagnifierForm;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomDesktopMagnifier
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomDesktopMagnifier.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  FTargetColor := clNone;
  FZoom := 5;
  FOptions := [pmoFollowCursor];
  FCrossHairs := pmcOpenCross;
  FIndicator[0] := pmiNone;
  FIndicator[1] := pmiNone;
  FIndicator[2] := pmiNone;
  FIndicator[3] := pmiNone;
  FBorderWidth := 1;
  ClientWidth := 17 * FZoom + 2;
  ClientHeight := 17 * FZoom + 2;
  Color := clAppWorkSpace; // was clWindow before
end;

procedure TPegtopCustomDesktopMagnifier.Paint;
var
  CursorPos: TPoint;
  SourceCanvas: TCanvas;
  SourcePos: TPoint;
  SourceSize: TSize;
  DestPos: TPoint;
  DestSize: TSize;
  SelfPos: TPoint;
  Bitmap: TBitmap;
  S: String;
  Extent: TSize;
  OldColor: TColor;
  MouseThreadId: Cardinal;
  CurrentThreadId: Cardinal;
  CursorHandle: THandle;
  CursorInfo: ICONINFO;
  CursorImagePos: TPoint;
  Center: TPoint;
  I: Integer;
begin
  // refresh cursor position:
  GetCursorPos(CursorPos);
  if pmoFollowCursor in FOptions then FPosition := CursorPos;
  OldColor := FTargetColor;
  SelfPos := ClientToScreen(Point(0, 0));

  // magnify desktop:
  SourceSize.CX := (((ClientWidth - FBorderWidth * 2) + FZoom - 1) div (FZoom * 2)) * 2 + 1;
  SourceSize.CY := (((ClientHeight - FBorderWidth * 2) + FZoom - 1) div (FZoom * 2)) * 2 + 1;
  SourcePos.X := FPosition.X - SourceSize.CX div 2;
  SourcePos.Y := FPosition.Y - SourceSize.CY div 2;
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := ClientWidth;
    Bitmap.Height := ClientHeight;
    SourceCanvas := TCanvas.Create;
    try
      SourceCanvas.Handle := GetDC(0);
      try
        if (ClientWidth > FBorderWidth * 2) and (ClientHeight > FBorderWidth * 2) then begin
          DestSize.CX := SourceSize.CX * FZoom;
          DestSize.CY := SourceSize.CY * FZoom;
          DestPos.X := (ClientWidth - DestSize.CX) div 2;
          DestPos.Y := (ClientHeight - DestSize.CY) div 2;
          if pmoInvertColors in FOptions then
            StretchBlt(Bitmap.Canvas.Handle, DestPos.X, DestPos.Y, DestSize.CX, DestSize.CY,
              SourceCanvas.Handle, SourcePos.X, SourcePos.Y, SourceSize.CX, SourceSize.CY, NOTSRCCOPY)
          else
            StretchBlt(Bitmap.Canvas.Handle, DestPos.X, DestPos.Y, DestSize.CX, DestSize.CY,
              SourceCanvas.Handle, SourcePos.X, SourcePos.Y, SourceSize.CX, SourceSize.CY, SRCCOPY);
        end;
        if (FPosition.X >= SelfPos.X) and (FPosition.Y >= SelfPos.Y)
        and (FPosition.X < SelfPos.X + Width) and (FPosition.Y < SelfPos.Y + Height) then
          FTargetColor := clNone
        else
          FTargetColor := SourceCanvas.Pixels[FPosition.X, FPosition.Y];
      finally
        ReleaseDC(0, SourceCanvas.Handle);
      end;
    finally
      SourceCanvas.Free;
    end;

    // avoid recursion:
    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.Brush.Color := Color;
    Bitmap.Canvas.FillRect(Bounds(
      (SelfPos.X + FBorderWidth - SourcePos.X) * FZoom + DestPos.X,
      (SelfPos.Y + FBorderWidth - SourcePos.Y) * FZoom + DestPos.Y,
      FZoom * (ClientWidth - FBorderWidth * 2),
      FZoom * (ClientHeight - FBorderWidth * 2)
    ));

    // magnify cursor:
    if pmoDrawCursor in FOptions then begin
      // get cursor image (of any thread):
      MouseThreadId := GetWindowThreadProcessId(WindowFromPoint(CursorPos), NIL);
      CurrentThreadId := GetCurrentThreadId;
      if (MouseThreadId <> CurrentThreadId)
      and AttachThreadInput(CurrentThreadId, MouseThreadId, True) then begin
        try
          CursorHandle := GetCursor;
        finally
          AttachThreadInput(CurrentThreadId, MouseThreadId, False);
        end;
      end
      else begin
        CursorHandle := GetCursor;
      end;
      // get absolute position of cursor image:
      GetIconInfo(CursorHandle, CursorInfo);
      CursorImagePos.X := CursorPos.X - Integer(CursorInfo.xHotspot);
      CursorImagePos.Y := CursorPos.Y - Integer(CursorInfo.yHotspot);
      // draw cursor:
      DrawIconEx(Bitmap.Canvas.Handle,
        (CursorImagePos.X - SourcePos.X) * FZoom + DestPos.X,
        (CursorImagePos.Y - SourcePos.Y) * FZoom + DestPos.Y,
        CursorHandle,
        GetSystemMetrics(SM_CXCURSOR) * FZoom,
        GetSystemMetrics(SM_CYCURSOR) * FZoom,
        0, 0, DI_NORMAL);
    end;

    // draw indicators:
    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.Font.Color := clNone;
    S := GetIndicatorText(0);
    if S <> '' then begin
      Bitmap.Canvas.TextOut (FBorderWidth + 1, FBorderWidth, S);
    end;
    S := GetIndicatorText(1);
    if S <> '' then begin
      Extent := Bitmap.Canvas.TextExtent(S);
      Bitmap.Canvas.TextOut (ClientWidth - Extent.CX - FBorderWidth - 1, FBorderWidth, S);
    end;
    S := GetIndicatorText(2);
    if S <> '' then begin
      Extent := Bitmap.Canvas.TextExtent(S);
      Bitmap.Canvas.TextOut (FBorderWidth + 1, ClientHeight - Extent.CY - FBorderWidth, S);
    end;
    S := GetIndicatorText(3);
    if S <> '' then begin
      Extent := Bitmap.Canvas.TextExtent(S);
      Bitmap.Canvas.TextOut (ClientWidth - Extent.CX - FBorderWidth - 1, ClientHeight - Extent.CY - FBorderWidth, S);
    end;

    // draw crosshairs:
    Center.X := Bitmap.Width div 2;
    Center.Y := Bitmap.Height div 2;
    Bitmap.Canvas.Pen.Color := $000000;
    if FCrossHairs = pmcOpenCross then begin
      // draw top line:
      Bitmap.Canvas.MoveTo(Center.X, FBorderWidth);
      Bitmap.Canvas.LineTo(Center.X, Center.Y - FZoom);
      // draw bottom line:
      Bitmap.Canvas.MoveTo(Center.X, Bitmap.Height - FBorderWidth - 1);
      Bitmap.Canvas.LineTo(Center.X, Center.Y + FZoom);
      // draw left line:
      Bitmap.Canvas.MoveTo(FBorderWidth, Center.Y);
      Bitmap.Canvas.LineTo(Center.X - FZoom, Center.Y);
      // draw right line:
      Bitmap.Canvas.MoveTo(Bitmap.Width - FBorderWidth - 1, Center.Y);
      Bitmap.Canvas.LineTo(Center.X + FZoom, Center.Y);
    end
    else if FCrossHairs = pmcContinuousCross then begin
      // draw vertical line:
      Bitmap.Canvas.MoveTo(Center.X, FBorderWidth);
      Bitmap.Canvas.LineTo(Center.X, Bitmap.Height - FBorderWidth);
      // draw horizontal line:
      Bitmap.Canvas.MoveTo(FBorderWidth, Center.Y);
      Bitmap.Canvas.LineTo(Bitmap.Width - FBorderWidth, Center.Y);
    end
    else if FCrossHairs = pmcSmallCross then begin
      // draw vertical line:
      Bitmap.Canvas.MoveTo(Center.X, Center.Y - FZoom);
      Bitmap.Canvas.LineTo(Center.X, Center.Y + FZoom + 1);
      // draw horizontal line:
      Bitmap.Canvas.MoveTo(Center.X - FZoom, Center.Y);
      Bitmap.Canvas.LineTo(Center.X + FZoom + 1, Center.Y);
    end;

    // draw border:
    for I := 0 to FBorderWidth - 1 do
      Bitmap.Canvas.Rectangle(I, I, Bitmap.Width - I, Bitmap.Height - I);

    // finally copy drawings to canvas:
    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  // notify:
  if (FTargetColor <> OldColor) and Assigned(FOnTargetColorChange) then FOnTargetColorChange(Self);
end;

procedure TPegtopCustomDesktopMagnifier.WMSetText(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopCustomDesktopMagnifier.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  DoMouseEnter;
end;

procedure TPegtopCustomDesktopMagnifier.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  DoMouseLeave;
end;

procedure TPegtopCustomDesktopMagnifier.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TPegtopCustomDesktopMagnifier.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

function TPegtopCustomDesktopMagnifier.GetIndicatorText(Index: Integer): String;
  function SwapBytes(V: Integer): Integer;
  begin
    Result := ((V shr 16) and $FF) or (V and $00FF00) or ((V and $FF) shl 16);
  end;
begin
  case FIndicator[Index] of
    pmiCaption:   Result := Caption;
    pmiCursorPos: Result := IntToStr(FPosition.X) + ' / ' + IntToStr(FPosition.Y);
    pmiHexCode:   if FTargetColor = clNone then Result := '' else Result := IntToHex(SwapBytes(ColorToRGB(FTargetColor)), 6);
    pmiZoom:      Result := IntToStr(FZoom) + 'x';
    else          Result := '';
  end;
end;

procedure TPegtopCustomDesktopMagnifier.SetPosition(Value: TPoint);
begin
  Exclude(FOptions, pmoFollowCursor);
  if (FPosition.X <> Value.X) or (FPosition.Y <> Value.Y) then begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomDesktopMagnifier.SetZoom(Value: Integer);
begin
  if FZoom <> Value then begin
    FZoom := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomDesktopMagnifier.SetOptions(Value: TPegtopDesktopMagnifierOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomDesktopMagnifier.SetCrossHairs(Value: TPegtopDesktopMagnifierCrossHairs);
begin
  if FCrossHairs <> Value then begin
    FCrossHairs := Value;
    Invalidate;
  end;
end;

function TPegtopCustomDesktopMagnifier.GetIndicator(Index: Integer): TPegtopDesktopMagnifierIndicator;
begin
  Result := FIndicator[Index];
end;

procedure TPegtopCustomDesktopMagnifier.SetIndicator(Index: Integer; Value: TPegtopDesktopMagnifierIndicator);
begin
  if FIndicator[Index] <> Value then begin
    FIndicator[Index] := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomDesktopMagnifier.SetBorderWidth(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 10 then Value := 10;
  if FBorderWidth <> Value then begin
    FBorderWidth := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopDesktopMagnifier
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopDesktopMagnifier.Create(AOwner: TComponent);
begin
  inherited;
  FInterval := 50;
  FIntervalForce := 500;
  FWindowHandle := AllocateHWnd(TimerWndProc);
  SetTimer(FWindowHandle, 1, FInterval, NIL);
end;

destructor TPegtopDesktopMagnifier.Destroy;
begin
  KillTimer(FWindowHandle, 1);
  DeAllocateHWnd(FWindowHandle);
  inherited;
end;

procedure TPegtopDesktopMagnifier.TimerWndProc(var Msg: TMessage);
var
  P: TPoint;
  T: Integer;
begin
  if Msg.Msg = WM_TIMER then begin
    GetCursorPos(P);
    T := GetTickCount;
    if (P.X <> Position.X) or (P.Y <> Position.Y)
    or (FIntervalForce <= FInterval) or (T - FRecentRefreshTick >= FIntervalForce) then begin
      Invalidate;
      FRecentRefreshTick := T;
    end;
  end;
  Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TPegtopDesktopMagnifier.SetInterval(Value: Integer);
begin
  if Value < 1 then Value := 1 else if Value > 10000 then Value := 10000;
  if FInterval <> Value then begin
    KillTimer(FWindowHandle, 1);
    FInterval := Value;
    if FIntervalForce < FInterval then FIntervalForce := FInterval;
    SetTimer(FWindowHandle, 1, FInterval, NIL);
  end;
end;

procedure TPegtopDesktopMagnifier.SetIntervalForce(Value: Integer);
begin
  if Value < FInterval then Value := FInterval;
  FIntervalForce := Value;
end;


////////////////////////////////////////////////////////////////////////////////
// TPegtopDesktopMagnifierForm
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopDesktopMagnifierForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited;
  BorderIcons := [];
  BorderStyle := bsNone;
  Width := 17 * 5 + 2;
  Height := 17 * 5 + 2;
  FormStyle := fsStayOnTop;
  FMagnifier := TPegtopCustomDesktopMagnifier.Create(Self);
  FMagnifier.Align := alClient;
  FMagnifier.Parent := Self;
end;

procedure TPegtopDesktopMagnifierForm.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TPegtopDesktopMagnifierForm.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  if (Win32Platform = VER_PLATFORM_WIN32_NT)
  and ((Win32MajorVersion > 5)
  or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;


procedure TPegtopDesktopMagnifierForm.AutoPosition(const Fixspot, Hotspot: TPoint; const Reset: Boolean);
var
  X, Y, W, H: Integer;
  WorkArea: TRect;
  procedure FindBestPosition;
  begin
    if (Hotspot.X + 10 >= X) and (Hotspot.Y + 10 >= Y)
    and (Hotspot.X - 10 <= X + W) and (Hotspot.Y - 10 <= Y + H) then begin
      // hotspot near window
      if Hotspot.X < Fixspot.X then
        X := Fixspot.X + 16 // place right
      else
        X := Fixspot.X - 16 - W; // place left
      if Hotspot.Y < Fixspot.Y then
        Y := Fixspot.Y + 16 // place below
      else
        Y := Fixspot.Y - 16 - H; // place above
    end;
  end;
begin
  X := Left;
  Y := Top;
  W := Width;
  H := Height;
  if Reset then begin
    X := Fixspot.X + 16;
    Y := Fixspot.Y + 16;
  end;

  FindBestPosition;

  // move window into work area:
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0);
  if X < WorkArea.Left + 8 then
    X := Fixspot.X + 16
  else If X + W > WorkArea.Right - 8 then
    X := Fixspot.X - 16 - W;
  if Y < WorkArea.Top + 8 then
    Y := Fixspot.Y + 16
  else If Y + H > WorkArea.Bottom - 8 then
    Y := Fixspot.Y - 16 - H;

  FindBestPosition;

  // apply window position:
  if (X <> Left) or (Y <> Top) then SetBounds(X, Y, W, H);
end;

initialization
  DesktopMagnifierForm := NIL;
finalization
  DesktopMagnifierForm.Free;
end.
