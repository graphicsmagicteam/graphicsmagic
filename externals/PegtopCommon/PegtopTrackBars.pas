////////////////////////////////////////////////////////////////////////////////
// File:       PegtopTrackBars.pas
// Classes:    TPegtopSlideBar, TPegtopLabelSlideBar, TPegtopTrackBar,
//             TPegtopRangeBar
// Version:    1.02
// Date:       09 Sep 2004 created 1.00
//             19 Jan 2005 modified 1.01 (OnDrawTrack event handler added)
//             23 Mar 2005 modified 1.02 (Font and other properties added,
//                                        optional label hint added,
//                                        more label options added,
//                                        vertical labels implemented,
//                                        constrained rangebar implemented,
//                                        transparency (supports XP themes),
//                                        bugfix: order of properties changed)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004, 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopTrackBar is a better track bar for changing integer values (floating
// point values can be "simulated" by changing the LabelMode property).
// TPegtopColorTrackBar is a track bar with a colored button.
// TPegtopRangeBar works like TPegtopTrackBar, but defines a range (two values).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopTrackBars;

interface

uses
  Windows, Classes, Messages, Graphics, Forms, Controls, PegtopThemes;

const
  WM_PEGTOPSLIDEBAR_EDITVALUE = WM_USER + $E;

type
  TPegtopSlideBarOrientation = (psoHorizontal, psoVertical);

  TPegtopLabelMode = (plmPos, plmMul, plmDiv, plmShl, plmShr, plmBin, plmInv, plmLog, plmExp, plmSqr, plmAdd, plmSub);
  TPegtopLabelOption = (ploVisible, ploPlusMinusZero, ploExplicitSign, ploHint, ploDisableEdit, ploDisableCopy, ploDisablePaste, ploFlip, ploRotate);
  TPegtopLabelOptions = set of TPegtopLabelOption;
  TPegtopRangeOption = (proDisableConstrain);
  TPegtopRangeOptions = set of TPegtopRangeOption;

  TPegtopScrollCode = (pscLineUp, pscLineDown, pscPageUp, pscPageDown, pscPosition, pscTrack, pscTop, pscBottom, pscEndScroll);
  TPegtopScrollEvent = procedure (Sender: TObject; ScrollCode: TPegtopScrollCode; var ScrollPos: Integer) of object;
  TPegtopLabelEvent = procedure (Sender: TObject; var Caption: String) of object;
  TPegtopDrawTrackEvent = procedure (Sender: TObject; Canvas: TCanvas; Orientation: TPegtopSlideBarOrientation; BoundsRect: TRect; Center: TPoint) of object;

  TPegtopSlideBar = class(TCustomControl)
  private
    FOrientation: TPegtopSlideBarOrientation;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: Integer;
    FMouseDown: Boolean;
    FMouseDelta: TPoint;
    FOnChange: TNotifyEvent;
    FOnScroll: TPegtopScrollEvent;
    FOnDrawTrack: TPegtopDrawTrackEvent;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure SetOrientation(V: TPegtopSlideBarOrientation);
    procedure SetMin(V: Integer);
    procedure SetMax(V: Integer);
    procedure SetSmallChange(V: Integer);
    function GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
    procedure SetOnDrawTrack(V: TPegtopDrawTrackEvent);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintTo(const ACanvas: TCanvas); virtual;
    procedure Paint; override;
    procedure DrawTrack(const ACanvas: TCanvas; const AOrientation: TPegtopSlideBarOrientation; const P: TPoint); virtual;
    function TrackEnabled: Boolean; virtual;
    procedure EndScroll;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ExtraMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure ValidatePosition; virtual; abstract;
    procedure DrawText(const ACanvas: TCanvas; const X, Y: Integer; const S: String);
    procedure DrawButton(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState; const WithFocusRect: Boolean = False);
    procedure DrawButtonGrip(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState); virtual;
    function DoStartScroll(const X, Y: Integer): TPoint; virtual; abstract;
    procedure DoScroll(const X, Y: Integer); virtual; abstract;
    procedure DoEndScroll; virtual; abstract;
    procedure PopupContextMenu(const X, Y: Integer); virtual;
    procedure ApplyPosition(const P: Integer); virtual; abstract;
    procedure ChangePosition(const Delta: Integer); virtual; abstract;
    function GetLinePoint: TPoint; virtual;
    function GetScrollableRect: TRect; virtual;
    function GetButtonSize: TSize; virtual; abstract;
    property IsMouseDown: Boolean read FMouseDown;
    property OnDrawTrack: TPegtopDrawTrackEvent read FOnDrawTrack write SetOnDrawTrack;
    property Orientation: TPegtopSlideBarOrientation read FOrientation write SetOrientation default psoHorizontal;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScroll: TPegtopScrollEvent read FOnScroll write FOnScroll;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowContextMenu;
    procedure ResetToDefault; virtual;
  published
    property TabOrder;
    property OnEnter;
    property OnExit;
  end;

  TPegtopLabelSlideBar = class(TPegtopSlideBar)
  private
    FLabelCaption: TCaption;
    FLabelMin: TCaption;
    FLabelMax: TCaption;
    FLabelMode: TPegtopLabelMode;
    FLabelParam: Double;
    FLabelOptions: TPegtopLabelOptions;
    FCaptionRect: TRect;
    FCursor: TCursor;
    FOnLabel: TPegtopLabelEvent;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure ChangeCursor(const NewCursor: TCursor);
    procedure SetLabelCaption(V: TCaption);
    procedure SetLabelMin(V: TCaption);
    procedure SetLabelMax(V: TCaption);
    procedure SetLabelMode(V: TPegtopLabelMode);
    procedure SetLabelParam(V: Double);
    procedure SetLabelOptions(V: TPegtopLabelOptions);
    procedure SetCursor(Value: TCursor);
  protected
    procedure PaintTo(const ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function TransformCaption(const S: String): String; virtual;
    function PositionToString(const V: Integer): String;
    function PositionToValue(const V: Integer): Double;
    function ValueToPosition(const V: Double): Integer;
    function GetLinePoint: TPoint; override;
    function GetCaptionPoint: TPoint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property LabelCaption: TCaption read FLabelCaption write SetLabelCaption;
    property LabelMin: TCaption read FLabelMin write SetLabelMin;
    property LabelMax: TCaption read FLabelMax write SetLabelMax;
    property LabelMode: TPegtopLabelMode read FLabelMode write SetLabelMode;
    property LabelParam: Double read FLabelParam write SetLabelParam;
    property LabelOptions: TPegtopLabelOptions read FLabelOptions write SetLabelOptions;
    property Cursor: TCursor read FCursor write SetCursor;
    property OnLabel: TPegtopLabelEvent read FOnLabel write FOnLabel;
  end;

  TPegtopTrackBar = class(TPegtopLabelSlideBar)
  private
    FPosition: Integer;
    FDefaultPosition: Integer;
    FPositionCaptionRect: TRect;
    FMouseHover: Boolean;
    procedure WMEditValue(var Msg: TMessage); message WM_PEGTOPSLIDEBAR_EDITVALUE;
    procedure NumEditChange(Sender: TObject);
    procedure NumEditClose(Sender: TObject; var Action: TCloseAction);
    function PointToPosition(const X, Y: Integer): Integer;
    procedure MenuItemClick(Sender: TObject);
    procedure SetPosition(V: Integer);
    procedure SetDefaultPosition(V: Integer);
    function GetValue: Double;
    procedure SetValue(V: Double);
  protected
    procedure Loaded; override;
    procedure PaintTo(const ACanvas: TCanvas); override;
    procedure ValidatePosition; override;
    function TransformCaption(const S: String): String; override;
    function DoStartScroll(const X, Y: Integer): TPoint; override;
    procedure DoScroll(const X, Y: Integer); override;
    procedure DoEndScroll; override;
    procedure ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ExtraMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PopupContextMenu(const X, Y: Integer); override;
    procedure ApplyPosition(const P: Integer); override;
    procedure ChangePosition(const Delta: Integer); override;
    function GetButtonSize: TSize; override;
    function GetButtonPoint: TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditValue;
    procedure CopyValue;
    procedure PasteValue;
    procedure ResetToDefault; override;
    property DefaultPosition: Integer read FDefaultPosition write SetDefaultPosition;
  published
    property Value: Double read GetValue write SetValue stored False;
    property Transparent;
    property Orientation;
    property Min;
    property Max;
    property SmallChange;
    property OnChange;
    property OnScroll;
    property Enabled;
    property Visible;
    property OnDrawTrack;
    property Font;
    property ParentFont;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property Position: Integer read FPosition write SetPosition; // should be last property
  end;

  TPegtopColorTrackBar = class(TPegtopTrackBar)
  private
    FButtonColor: TColor;
    procedure SetButtonColor(V: TColor);
  protected
    procedure DrawButtonGrip(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState); override;
  published
    property ButtonColor: TColor read FButtonColor write SetButtonColor;
    property OnDrawTrack;
  end;

  TPegtopRangeBar = class(TPegtopLabelSlideBar)
  private
    FPosition: array[0..1] of Integer;
    FDefaultPosition: array[0..1] of Integer;
    FPositionCaptionRect: array[0..1] of TRect;
    FButtonFocus: Integer;
    FMouseHover: Integer;
    FConstrained: Boolean;
    FRangeOptions: TPegtopRangeOptions;
    procedure WMEditValue(var Msg: TMessage); message WM_PEGTOPSLIDEBAR_EDITVALUE;
    procedure NumEditChange(Sender: TObject);
    procedure NumEditClose(Sender: TObject; var Action: TCloseAction);
    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;
    function PointToPosition(const Index, X, Y: Integer): Integer;
    procedure MenuItemClick(Sender: TObject);
    function GetPosition(Index: Integer): Integer;
    procedure SetPosition(Index: Integer; V: Integer);
    function GetDefaultPosition(Index: Integer): Integer;
    procedure SetDefaultPosition(Index: Integer; V: Integer);
    function GetValue(Index: Integer): Double;
    procedure SetValue(Index: Integer; V: Double);
    procedure SetConstrained(V: Boolean);
  protected
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PaintTo(const ACanvas: TCanvas); override;
    procedure ValidatePosition; override;
    function TransformCaption(const S: String): String; override;
    function DoStartScroll(const X, Y: Integer): TPoint; override;
    procedure DoScroll(const X, Y: Integer); override;
    procedure DoEndScroll; override;
    procedure ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ExtraMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PopupContextMenu(const X, Y: Integer); override;
    procedure ApplyPosition(const P: Integer); override;
    procedure ChangePosition(const Delta: Integer); override;
    function GetButtonSize: TSize; override;
    function GetButtonPoint(Index: Integer): TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditValueMin;
    procedure EditValueMax;
    procedure CopyValueMin;
    procedure CopyValueMax;
    procedure PasteValueMin;
    procedure PasteValueMax;
    procedure ResetToDefault; override;
    property DefaultPositionMin: Integer index 0 read GetDefaultPosition write SetDefaultPosition;
    property DefaultPositionMax: Integer index 1 read GetDefaultPosition write SetDefaultPosition;
  published
    property RangeOptions: TPegtopRangeOptions read FRangeOptions write FRangeOptions;
    property Constrained: Boolean read FConstrained write SetConstrained default False;
    property Transparent;
    property Orientation;
    property Min;
    property Max;
    property SmallChange;
    property OnChange;
    property OnScroll;
    property Enabled;
    property Visible;
    property OnDrawTrack;
    property Font;
    property ParentFont;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property PositionMin: Integer index 0 read GetPosition write SetPosition;    // should be last property
    property PositionMax: Integer index 1 read GetPosition write SetPosition;    // should be last property
    property ValueMin: Double index 0 read GetValue write SetValue stored False; // should be last property
    property ValueMax: Double index 1 read GetValue write SetValue stored False; // should be last property
  end;

  TPegtopHintWindow = class(THintWindow)
  private
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
  end;

implementation

uses
  Menus, SysUtils, Clipbrd, PegtopNumForms, PegtopNumEdits;

{$R *.res}

resourcestring
  PegtopTrackBarReset = '&Reset';
  PegtopTrackBarEdit = '&Edit value...';
  PegtopTrackBarCopy = '&Copy value';
  PegtopTrackBarPaste = '&Paste value';
  PegtopRangeBarReset = '&Reset';
  PegtopRangeBarConstrained = '&Constrained';
  PegtopRangeBarEditMin = 'Edit &lower limit...';
  PegtopRangeBarEditMax = 'Edit &upper limit...';
  PegtopRangeBarCopyMin = 'Copy &lower limit';
  PegtopRangeBarCopyMax = 'Copy &upper limit';
  PegtopRangeBarPasteMin = 'Paste &lower limit';
  PegtopRangeBarPasteMax = 'Paste &upper limit';

const
  ExpTable: array[0..9] of Double =
    (1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);

var
  TrackBarPopupMenu: TPopupMenu;
  RangeBarPopupMenu: TPopupMenu;
  BitmapsLoaded: Boolean;
  Bitmaps: array[0..3] of TBitmap;
  LabelHintWindow: THintWindow;

function GetLabelHintWindow: THintWindow;
begin
  if LabelHintWindow = NIL then begin
    LabelHintWindow := HintWindowClass.Create(Application);
    LabelHintWindow.Visible := False;
  end;
  Result := LabelHintWindow;
end;

function HasExtent(R: TRect): Boolean;
begin
  Result := (R.Right > R.Left) and (R.Bottom > R.Top);
end;

function IsValidFloat(S: String): Boolean;
var
  Dummy: Extended;
begin
  Result := TextToFloat(PChar(S), Dummy, fvExtended);
end;

procedure InitBitmaps;
var
  I: Integer;
begin
  if not BitmapsLoaded then begin
    for I := Low(Bitmaps) to High(Bitmaps) do begin
      Bitmaps[I] := TBitmap.Create;
      Bitmaps[I].LoadFromResourceName(HInstance, 'PEGTOPTRACKBAR' + Chr(48 + I div 10) + Chr(48 + I mod 10));
    end;
    BitmapsLoaded := True;
  end;
end;

procedure CloseBitmaps;
var
  I: Integer;
begin
  if BitmapsLoaded then begin
    for I := Low(Bitmaps) to High(Bitmaps) do Bitmaps[I].Free;
    BitmapsLoaded := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopSlideBar
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopSlideBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls, csOpaque];
  Width := 120;
  Height := 32;
  TabStop := True;
  DoubleBuffered := True;
  FMin := 0;
  FMax := 100;
  FSmallChange := 1;
  FOrientation := psoHorizontal;
end;

procedure TPegtopSlideBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TPegtopSlideBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  // Msg.Result := 1;
  // Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.DC, Msg.Unused);
  inherited;
end;

procedure TPegtopSlideBar.Paint;
begin
  if Transparent then begin
    DefaultTheme.DrawParent(Handle, Canvas.Handle, NIL);
  end
  else begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
  end;
  PaintTo(Canvas);
end;

procedure TPegtopSlideBar.PaintTo(const ACanvas: TCanvas);
var
  P: TPoint;
begin
  P := GetLinePoint;
  DrawTrack(ACanvas, FOrientation, P);
end;

procedure TPegtopSlideBar.DrawTrack(const ACanvas: TCanvas; const AOrientation: TPegtopSlideBarOrientation; const P: TPoint);
var
  P1, P2: Integer;
begin
  if Assigned(FOnDrawTrack) then begin
    FOnDrawTrack(Self, ACanvas, AOrientation, ClientRect, P);
  end
  else begin
    if FOrientation = psoHorizontal then begin
      P1 := 1;
      P2 := ClientWidth - 1;
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.MoveTo(P2 - 1, P.Y - 1);
      ACanvas.LineTo(P1, P.Y - 1);
      ACanvas.LineTo(P1, P.Y + 1);
      ACanvas.Pen.Color := clBtnHighlight;
      ACanvas.LineTo(P2 - 1, P.Y + 1);
      ACanvas.LineTo(P2 - 1, P.Y - 1);
      if TrackEnabled then ACanvas.Pen.Color := clBlack
      else ACanvas.Pen.Color := clBtnFace;
      ACanvas.MoveTo(P1 + 1, P.Y);
      ACanvas.LineTo(P2 - 1, P.Y);
    end
    else begin
      P1 := 1;
      P2 := ClientHeight - 1;
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.MoveTo(P.X - 1, P2 - 1);
      ACanvas.LineTo(P.X - 1, P1);
      ACanvas.LineTo(P.X + 1, P1);
      ACanvas.Pen.Color := clBtnHighlight;
      ACanvas.LineTo(P.X + 1, P2 - 1);
      ACanvas.LineTo(P.X - 1, P2 - 1);
      if TrackEnabled then ACanvas.Pen.Color := clBlack
      else ACanvas.Pen.Color := clBtnFace;
      ACanvas.MoveTo(P.X, P1 + 1);
      ACanvas.LineTo(P.X, P2 - 1);
    end;
  end;
end;

function TPegtopSlideBar.TrackEnabled: Boolean;
begin
  Result := Enabled;
end;

procedure TPegtopSlideBar.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopSlideBar.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopSlideBar.EndScroll;
begin
  if FMouseDown then begin
    FMouseDown := False;
    DoEndScroll;
  end;
end;

procedure TPegtopSlideBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ScrollableRect: TRect;
Begin
  if Visible and Enabled then begin
    if Button = mbLeft then begin
      ScrollableRect := GetScrollableRect;
      if (X >= ScrollableRect.Left) and (X < ScrollableRect.Right)
      and (Y >= ScrollableRect.Top) and (Y < ScrollableRect.Bottom) then begin
        FMouseDelta := DoStartScroll(X, Y);
        FMouseDown := True;
        if CanFocus then begin
          SetFocus;
          Invalidate;
        end;
      end
      else begin
        ExtraMouseDown(Button, Shift, X, Y);
      end;
    end
    else if Button = mbRight then begin
      if FMouseDown then begin
        FMouseDown := False;
        DoEndScroll;
      end;
    end;
  end;
  inherited;
end;

procedure TPegtopSlideBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown then begin
    DoScroll(X - FMouseDelta.X, Y - FMouseDelta.Y);
  end
  else begin
    ExtraMouseMove(Shift, X, Y);
  end;
  inherited;
end;

procedure TPegtopSlideBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown then begin
    FMouseDown := False;
    DoEndScroll;
  end;
  // if Button = mbRight then PopupContextMenu(X, Y);
  inherited;
end;

procedure TPegtopSlideBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT: if FOrientation = psoHorizontal then ChangePosition(-FSmallChange);
    VK_RIGHT: if FOrientation = psoHorizontal then ChangePosition(FSmallChange);
    VK_UP: if FOrientation = psoVertical then ChangePosition(FSmallChange);
    VK_DOWN: if FOrientation = psoVertical then ChangePosition(-FSmallChange);
  end;
  inherited;
end;

function TPegtopSlideBar.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if FOrientation = psoVertical then ChangePosition(FSmallChange)
  else ChangePosition(-FSmallChange);
  Result := True;
end;

function TPegtopSlideBar.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if FOrientation = psoVertical then ChangePosition(-FSmallChange)
  else ChangePosition(FSmallChange);
  Result := True;
end;

procedure TPegtopSlideBar.ShowContextMenu;
var
  P: TPoint;
begin
  P := GetLinePoint;
  Dec(P.Y, 2);
  P := ClientToScreen(P);
  PopupContextMenu(P.X, P.Y);
end;

procedure TPegtopSlideBar.CMWantSpecialKey(var Msg: TWMKey);
begin
  case Msg.CharCode of
    VK_LEFT, VK_RIGHT:
      if FOrientation = psoHorizontal then Msg.Result := 1 else inherited;
    VK_UP, VK_DOWN:
      if FOrientation = psoVertical then Msg.Result := 1 else inherited;
    else
      inherited;
  end;
end;

procedure TPegtopSlideBar.CMEnabledChanged(var Msg: TMessage);
begin
  FMouseDown := False;
  inherited;
  Invalidate;
end;

procedure TPegtopSlideBar.CMMouseLeave(var Msg: TMessage);
begin
  ExtraMouseMove([], -1, -1);
  inherited;
end;

procedure TPegtopSlideBar.ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TPegtopSlideBar.ExtraMouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TPegtopSlideBar.DrawText(const ACanvas: TCanvas; const X, Y: Integer; const S: String);
begin
  if S <> '' then begin
    if Enabled then begin
      // ACanvas.Font.Color := clWindowText;
      ACanvas.TextOut(X, Y, S);
    end
    else begin
      ACanvas.Font.Color := clBtnHighlight;
      ACanvas.TextOut(X + 1, Y + 1, S);
      ACanvas.Font.Color := clBtnShadow;
      ACanvas.TextOut(X, Y, S);
    end;
  end;
end;

procedure TPegtopSlideBar.DrawButton(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState; const WithFocusRect: Boolean);
begin
  // DefaultTheme.Enabled := False;
  SetBkColor(ACanvas.Handle, ColorToRGB(Font.Color));
  SetTextColor(ACanvas.Handle, ColorToRGB(Color));
  DefaultTheme.DrawButton(ACanvas.Handle, ARect, AState, WithFocusRect);
end;

procedure TPegtopSlideBar.DrawButtonGrip(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState);
var
  X, Y: Integer;
begin
  if Enabled then begin
    Y := ARect.Top + 6;
    while Y < ARect.Bottom - 6 do begin
      X := ARect.Left + 6;
      while X < ARect.Right - 6 do begin
        ACanvas.Pixels[X, Y] := clBtnText; // clGray;
        Inc(X, 2);
      end;
      Inc(Y, 2);
    end;
  end;
end;

procedure TPegtopSlideBar.WMContextMenu(var Msg: TWMContextMenu);
begin
  if (Msg.XPos = -1) and (Msg.YPos = -1) then
    ShowContextMenu
  else
    PopupContextMenu(Msg.XPos, Msg.YPos);
  // inherited is not called because we already did popup the context menu
end;

procedure TPegtopSlideBar.PopupContextMenu(const X, Y: Integer);
begin
end;

procedure TPegtopSlideBar.ResetToDefault;
begin
end;

function TPegtopSlideBar.GetLinePoint: TPoint;
begin
  Result.X := ClientWidth div 2;
  Result.Y := ClientHeight div 2;
end;

function TPegtopSlideBar.GetScrollableRect: TRect;
var
  ButtonSize: TSize;
  LinePoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  LinePoint := GetLinePoint;
  if FOrientation = psoHorizontal then
    Result := Bounds(0, LinePoint.Y - ButtonSize.CY div 2, ClientWidth, ButtonSize.CY)
  else
    Result := Bounds(LinePoint.X - ButtonSize.CX div 2, 0, ButtonSize.CX, ClientHeight);
End;

procedure TPegtopSlideBar.SetOrientation(V: TPegtopSlideBarOrientation);
begin
  if FOrientation <> V then begin
    FOrientation := V;
    Invalidate;
  end;
end;

procedure TPegtopSlideBar.SetMin(V: Integer);
begin
  if FMin <> V then begin
    FMin := V;
    if FMax < FMin Then FMax := FMin;
    ValidatePosition;
    Invalidate;
  end;
end;

procedure TPegtopSlideBar.SetMax(V: Integer);
begin
  if FMax <> V then begin
    FMax := V;
    if FMin > FMax Then FMin := FMax;
    ValidatePosition;
    Invalidate;
  end;
end;

procedure TPegtopSlideBar.SetSmallChange(V: Integer);
begin
  if V < 1 then V := 1
  else if V > (FMax - FMin) then V := (FMax - FMin);
  if FSmallChange <> V then begin
    FSmallChange := V;
  end;
end;

function TPegtopSlideBar.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TPegtopSlideBar.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TPegtopSlideBar.SetOnDrawTrack(V: TPegtopDrawTrackEvent);
begin
  if (TMethod(FOnDrawTrack).Code <> TMethod(V).Code)
  or (TMethod(FOnDrawTrack).Data <> TMethod(V).Data) then begin
    FOnDrawTrack := V;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopLabelSlideBar
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopLabelSlideBar.Create(AOwner: TComponent);
begin
  inherited;
  FCursor := Cursor;
  FLabelCaption := Name;
  FLabelMin := '';
  FLabelMax := '';
  FLabelMode := plmPos;
  FLabelParam := 1.0;
  FLabelOptions := [ploVisible];
end;

procedure TPegtopLabelSlideBar.PaintTo(const ACanvas: TCanvas);
var
  S: String;
  CaptionPoint: TPoint;
  CaptionSize: TSize;
  LogFont: TLogFont;
  TextMetric: TTextMetric;
  RotFont, OrgFont: THandle;
begin
  ACanvas.Font.Assign(Font);
  S := TransformCaption(FLabelCaption);
  if ploVisible in FLabelOptions then begin
    if Assigned(FOnLabel) then FOnLabel(Self, S);
    CaptionSize := Canvas.TextExtent(S);
    CaptionPoint := GetCaptionPoint;
    if Orientation = psoHorizontal then begin
      FCaptionRect.Left := CaptionPoint.X - CaptionSize.CX div 2;
      FCaptionRect.Right := FCaptionRect.Left + CaptionSize.CX;
      FCaptionRect.Top := CaptionPoint.Y;
      FCaptionRect.Bottom := FCaptionRect.Top + CaptionSize.CY;
      ACanvas.Brush.Style := bsClear;
      DrawText(ACanvas, FCaptionRect.Left, FCaptionRect.Top, S);
      DrawText(ACanvas, 0, FCaptionRect.Top, FLabelMin);
      DrawText(ACanvas, ClientWidth - ACanvas.TextWidth(FLabelMax), FCaptionRect.Top, FLabelMax);
    end
    else begin
      FCaptionRect.Left := CaptionPoint.X;
      FCaptionRect.Right := FCaptionRect.Left + CaptionSize.CY;
      FCaptionRect.Top := CaptionPoint.Y - CaptionSize.CX div 2;
      FCaptionRect.Bottom := FCaptionRect.Top + CaptionSize.CX;
      ACanvas.Brush.Style := bsClear;
      GetObject(ACanvas.Font.Handle, SizeOf(TLogFont), @LogFont);
      GetTextMetrics(ACanvas.Handle, TextMetric);
      if (TextMetric.tmPitchAndFamily and (TMPF_TRUETYPE or TMPF_VECTOR)) = 0 then // no truetype font
        StrCopy(LogFont.lfFaceName, 'Arial');
      if ploRotate in FLabelOptions then begin
        LogFont.lfEscapement := 2700; // 270.0°
        LogFont.lfOrientation := 2700; // 270.0°
      end
      else begin
        LogFont.lfEscapement := 900; // 90.0°
        LogFont.lfOrientation := 900; // 90.0°
      end;
      RotFont := CreateFontIndirect(LogFont);
      try
        OrgFont := SelectObject(ACanvas.Handle, RotFont);
        try
          if ploRotate in FLabelOptions then begin
            DrawText(ACanvas, FCaptionRect.Right, FCaptionRect.Top, S);
            DrawText(ACanvas, FCaptionRect.Right, ClientHeight - ACanvas.TextWidth(FLabelMin), FLabelMin);
            DrawText(ACanvas, FCaptionRect.Right, 0, FLabelMax);
          end
          else begin
            DrawText(ACanvas, FCaptionRect.Left, FCaptionRect.Bottom, S);
            DrawText(ACanvas, FCaptionRect.Left, ClientHeight, FLabelMin);
            DrawText(ACanvas, FCaptionRect.Left, ACanvas.TextWidth(FLabelMax), FLabelMax);
          end;
        finally
          SelectObject(ACanvas.Handle, OrgFont);
        end;
      finally
        DeleteObject(RotFont);
      end;
    end;
  end;
  inherited;
end;

procedure TPegtopLabelSlideBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HintWindow: THintWindow;
  HintRect: TRect;
  HintPoint: TPoint;
  S: String;
begin
  inherited;
  if FMouseDown and (ploHint in FLabelOptions) then begin
    HintWindow := GetLabelHintWindow;
    if not HintWindow.Visible then begin
      HintWindow.Color := Application.HintColor;
      HintWindow.Visible := True;
    end;
    S := TransformCaption(FLabelCaption);
    if Assigned(FOnLabel) then FOnLabel(Self, S);
    HintRect := HintWindow.CalcHintRect(200, S, NIL);
    HintPoint := GetCaptionPoint;
    HintPoint.X := HintPoint.X - (HintRect.Right - HintRect.Left) div 2;
    HintPoint.Y := HintPoint.Y - 6;
    HintPoint := ClientToScreen(HintPoint);
    OffsetRect(HintRect, HintPoint.X, HintPoint.Y);
    HintWindow.ActivateHint(HintRect, S);
    HintWindow.Update;
  end;
end;

procedure TPegtopLabelSlideBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  HintWindow: THintWindow;
  HintRect: TRect;
  HintPoint: TPoint;
  S: String;
begin
  inherited;
  if FMouseDown and (ploHint in FLabelOptions) then begin
    HintWindow := GetLabelHintWindow;
    if not HintWindow.Visible then begin
      HintWindow.Color := Application.HintColor;
      HintWindow.Visible := True;
    end;
    S := TransformCaption(FLabelCaption);
    if Assigned(FOnLabel) then FOnLabel(Self, S);
    HintRect := HintWindow.CalcHintRect(200, S, NIL);
    HintPoint := GetCaptionPoint;
    HintPoint.X := HintPoint.X - (HintRect.Right - HintRect.Left) div 2;
    HintPoint.Y := HintPoint.Y - 6;
    HintPoint := ClientToScreen(HintPoint);
    OffsetRect(HintRect, HintPoint.X, HintPoint.Y);
{    if (HintWindow.Width <> HintRect.Right - HintRect.Left)
    or (HintWindow.Height <> HintRect.Bottom - HintRect.Top) then
      // deactivate befor resizing to avoid graphic errors
      HintWindow.ReleaseHandle;}
    HintWindow.ActivateHint(HintRect, S);
    HintWindow.Update;
  end;
end;

procedure TPegtopLabelSlideBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HintWindow: THintWindow;
begin
  inherited;
  if not FMouseDown then begin
    HintWindow := LabelHintWindow;
    if (HintWindow <> NIL) and HintWindow.Visible then begin
      HintWindow.Visible := False;
      HintWindow.ReleaseHandle;
    end;
  end;
end;

function TPegtopLabelSlideBar.TransformCaption(const S: String): String;
begin
  Result := S; // no transformation
end;

function TPegtopLabelSlideBar.ValueToPosition(const V: Double): Integer;
begin
  Result := Low(Integer); // undefined
  case FLabelMode of
    plmPos:
      Result := Round(V);
    plmMul:
      Result := Round(V / FLabelParam);
    plmDiv:
      Result := Round(V * FLabelParam);
    plmShl:
      if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then
        Result := Round(V / ExpTable[Trunc(FLabelParam)]);
    plmShr:
      if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then
        Result := Round(V * ExpTable[Trunc(FLabelParam)]);
    plmBin:
      if (Trunc(FLabelParam) >= 0) and (Trunc(FLabelParam) <= 10) then
        Result := Round(Ln(V / Trunc(FLabelParam)) / Ln(2));
    plmInv:
      if (FLabelParam > 0) and (FLabelParam <= 1000000) and (V <> 0) then
        Result := Round(FLabelParam / V);
    plmLog:
      if (FLabelParam > 0) and (FLabelParam <= 1000) then
        Result := Round(Exp(Ln(FLabelParam) * V));
    plmExp:
      if (FLabelParam > 0) and (FLabelParam <= 1000) then
        Result := Round(Ln(V) / Ln(FLabelParam));
    plmSqr:
      Result := Round(Sqrt(V));
    plmAdd:
      Result := Round(V - FLabelParam);
    plmSub:
      Result := Round(FLabelParam - V);
  end;
end;

function TPegtopLabelSlideBar.PositionToValue(const V: Integer): Double;
begin
  Result := -10000000000000.0;
  case FLabelMode of
    plmPos:
      Result := V;
    plmMul:
      Result := V * FLabelParam;
    plmDiv:
      Result := V / FLabelParam;
    plmShl:
      if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then
        Result := Round(V * ExpTable[Trunc(FLabelParam)]);
    plmShr:
      if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then
        Result := V / ExpTable[Trunc(FLabelParam)];
    plmBin:
      if (Trunc(FLabelParam) >= 0) and (FLabelParam <= 10) then
        Result := Trunc(FLabelParam) shl V;
    plmInv:
      if (FLabelParam > 0) and (FLabelParam <= 1000000) and (V <> 0) then
        Result := FLabelParam / V;
    plmLog:
      if (FLabelParam > 0) and (FLabelParam <= 1000) and (V > 0) then
        Result := Ln(V) / Ln(FLabelParam);
    plmExp:
      if (FLabelParam > 0) and (FLabelParam <= 1000) then
        Result := Exp(Ln(FLabelParam) * V);
    plmSqr:
      Result := Sqr(V);
    plmAdd:
      Result := FLabelParam + V;
    plmSub:
      Result := FLabelParam - V;
  end;
end;

function TPegtopLabelSlideBar.PositionToString(const V: Integer): String;
  function AddSign(const S: String; const IsNull, IsPositive: Boolean): String;
  begin
    if IsNull and (ploPlusMinusZero in FLabelOptions) then
      Result := '±' + S
    else if (V > 0) and (ploExplicitSign in FLabelOptions) then
      Result := '+' + S
    else
      Result := S;
  end;
begin
  Result := '***';
  case FLabelMode of
    plmPos:
      Result := AddSign(IntToStr(V), V = 0, V > 0);
    plmMul:
      Result := AddSign(FloatToStrF(V * FLabelParam, ffGeneral, 7, 2), V = 0, V > 0);
    plmDiv:
      Result := AddSign(FloatToStrF(V / FLabelParam, ffNumber, 16, Trunc(Ln(FLabelParam) / Ln(10) + 0.9999)), V = 0, V > 0);
    plmShl:
      if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then
        Result := AddSign(IntToStr(Round(V * ExpTable[Trunc(FLabelParam)])), V = 0, V > 0);
    plmShr:
      if (Trunc(FLabelParam) >= Low(ExpTable)) and (Trunc(FLabelParam) <= High(ExpTable)) then
        Result := AddSign(FloatToStrF(V / ExpTable[Trunc(FLabelParam)], ffNumber, 16, Trunc(FLabelParam)), V = 0, V > 0);
    plmBin:
      if (Trunc(FLabelParam) >= 0) and (Trunc(FLabelParam) <= 10) then
        Result := AddSign(IntToStr(Trunc(FLabelParam) shl V), False, True);
    plmInv:
      if (FLabelParam > 0) and (FLabelParam <= 1000000) then begin
        if V = 0 then
          Result := 'inf.'
        else
          Result := AddSign(FloatToStrF(FLabelParam / V, ffNumber, 16, Trunc(Ln(Abs(V) / FLabelParam) / Ln(10) + 2.4999)), False, V > 0);
      end;
    plmLog:
      if (FLabelParam > 0) and (FLabelParam <= 1000) then begin
        if V <= 0 then
          Result := 'undef.'
        else
          Result := AddSign(FloatToStrF(Ln(V) / Ln(FLabelParam), ffNumber, 16, 2), V = 1, V < 1);
      end;
    plmExp:
      if (FLabelParam >= 1) and (FLabelParam <= 1000) then
        Result := AddSign(FloatToStrF(Exp(Ln(FLabelParam) * V), ffNumber, 16, 0), False, True);
    plmSqr:
      Result := AddSign(IntToStr(Sqr(V)), V = 0, True);
    plmAdd:
      Result := AddSign(FloatToStrF(FLabelParam + V, ffGeneral, 7, 2), FLabelParam + V = 0, FLabelParam + V > 0);
    plmSub:
      Result := AddSign(FloatToStrF(FLabelParam - V, ffGeneral, 7, 2), FLabelParam - V = 0, FLabelParam - V > 0);
  end;
end;

procedure TPegtopLabelSlideBar.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopLabelSlideBar.ChangeCursor(const NewCursor: TCursor);
begin
  inherited Cursor := NewCursor;
end;

procedure TPegtopLabelSlideBar.SetCursor(Value: TCursor);
begin
  if FCursor <> Value then begin
    FCursor := Value;
    ChangeCursor(FCursor);
  end;
end;

function TPegtopLabelSlideBar.GetLinePoint: TPoint;
var
  ButtonSize: TSize;
  CaptionHeight: Integer;
begin
  if not (ploVisible in FLabelOptions) then begin
    Result := inherited GetLinePoint;
  end
  else begin
    ButtonSize := GetButtonSize;
    Canvas.Font.Assign(Font);
    CaptionHeight := Canvas.TextHeight('0');
    if FOrientation = psoHorizontal then begin
      Result.X := ClientWidth div 2;
      if ploFlip in FLabelOptions then
        Result.Y := (ClientHeight - ButtonSize.CY - CaptionHeight) div 2 + ButtonSize.CY div 2
      else
        Result.Y := (ClientHeight - ButtonSize.CY - CaptionHeight) div 2 + CaptionHeight + ButtonSize.CY div 2;
    end
    else begin
      if ploFlip in FLabelOptions then
        Result.X := (ClientWidth - ButtonSize.CX - CaptionHeight) div 2 + ButtonSize.CX div 2
      else
        Result.X := (ClientWidth - ButtonSize.CX - CaptionHeight) div 2 + CaptionHeight + ButtonSize.CX div 2;
      Result.Y := ClientHeight div 2;
    end;
  end;
end;

function TPegtopLabelSlideBar.GetCaptionPoint: TPoint;
var
  ButtonSize: TSize;
  CaptionHeight: Integer;
begin
  ButtonSize := GetButtonSize;
  Canvas.Font.Assign(Font);
  CaptionHeight := Canvas.TextHeight('0');
  if FOrientation = psoHorizontal then begin
    Result.X := ClientWidth div 2;
    if ploFlip in FLabelOptions then
      Result.Y := (ClientHeight - ButtonSize.CY - CaptionHeight) div 2 + ButtonSize.CY
    else
      Result.Y := (ClientHeight - ButtonSize.CY - CaptionHeight) div 2;
  end
  else begin
    if ploFlip in FLabelOptions then
      Result.X := (ClientWidth - ButtonSize.CX - CaptionHeight) div 2 + ButtonSize.CX
    else
      Result.X := (ClientWidth - ButtonSize.CX - CaptionHeight) div 2;
    Result.Y := ClientHeight div 2;
  end;
end;

procedure TPegtopLabelSlideBar.SetLabelCaption(V: TCaption);
begin
  if FLabelCaption <> V then begin
    FLabelCaption := V;
    Invalidate;
  end;
end;

procedure TPegtopLabelSlideBar.SetLabelMin(V: TCaption);
begin
  if FLabelMin <> V then begin
    FLabelMin := V;
    Invalidate;
  end;
end;

procedure TPegtopLabelSlideBar.SetLabelMax(V: TCaption);
begin
  if FLabelMax <> V then begin
    FLabelMax := V;
    Invalidate;
  end;
end;

procedure TPegtopLabelSlideBar.SetLabelMode(V: TPegtopLabelMode);
begin
  if FLabelMode <> V then begin
    FLabelMode := V;
    Invalidate;
  end;
end;

procedure TPegtopLabelSlideBar.SetLabelParam(V: Double);
begin
  if FLabelParam <> V then begin
    FLabelParam := V;
    Invalidate;
  end;
end;

procedure TPegtopLabelSlideBar.SetLabelOptions(V: TPegtopLabelOptions);
begin
  if FLabelOptions <> V then begin
    FLabelOptions := V;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopTrackBar
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopTrackBar.Create(AOwner: TComponent);
const
  MenuCaption: array[0..4] of String = (PegtopTrackBarReset, '-',
    PegtopTrackBarEdit, PegtopTrackBarCopy, PegtopTrackBarPAste);
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  if TrackBarPopupMenu = NIL then begin
    TrackBarPopupMenu := TPopupMenu.Create(Application);
    for I := Low(MenuCaption) to High(MenuCaption) do begin
      MenuItem := TMenuItem.Create(TrackBarPopupMenu);
      MenuItem.Caption := MenuCaption[I];
      MenuItem.Tag := I;
      TrackBarPopupMenu.Items.Add(MenuItem);
    end;
  end;
  FPosition := 0;
  FDefaultPosition := 0;
  inherited;
  LabelCaption := 'Position: <pos>';
end;

procedure TPegtopTrackBar.Loaded;
begin
  inherited;
  FDefaultPosition := FPosition;
end;

procedure TPegtopTrackBar.PaintTo(const ACanvas: TCanvas);
var
  ButtonPoint: TPoint;
  ButtonSize: TSize;
  ButtonRect: TRect;
  State: TPegtopThemeState;
begin
  inherited;
  if not Enabled then
    State := ptsDisabled
  else if FMouseDown then
    State := ptsPushed
  else if FMouseHover then
    State := ptsHot
  else if Focused then
    State := ptsFocused
  else
    State := ptsNormal;
  ButtonSize := GetButtonSize;
  ButtonPoint := GetButtonPoint;
  ButtonRect := Bounds(ButtonPoint.X, ButtonPoint.Y, ButtonSize.CX, ButtonSize.CY);
  DrawButton(ACanvas, ButtonRect, State, Focused);
  DrawButtonGrip(ACanvas, ButtonRect, State);
end;

procedure TPegtopTrackBar.ValidatePosition;
begin
  if FPosition < Min then FPosition := Min
  else if FPosition > Max then FPosition := Max;
  if FDefaultPosition < Min then FDefaultPosition := Min
  else if FDefaultPosition > Max then FDefaultPosition := Max;
end;

function TPegtopTrackBar.DoStartScroll(const X, Y: Integer): TPoint;
var
  ButtonSize: TSize;
  ButtonPoint: TPoint;
  NewPos: Integer;
begin
  ButtonSize := GetButtonSize;
  ButtonPoint := GetButtonPoint;
  if (X < ButtonPoint.X) or (Y < ButtonPoint.Y)
  or (X >= ButtonPoint.X + ButtonSize.CX) or (Y >= ButtonPoint.Y + ButtonSize.CY) then begin
    // clicked outside the button
    ButtonPoint.X := X - (ButtonSize.CX div 2);
    ButtonPoint.Y := Y - (ButtonSize.CY div 2);
    NewPos := PointToPosition(ButtonPoint.X, ButtonPoint.Y);
    if NewPos <> FPosition then begin
      FPosition := NewPos;
      Invalidate;
      if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition);
      if Assigned(FOnChange) then FOnChange(Self);
    end;
    ButtonPoint := GetButtonPoint;
  end;
  Result.X := X - ButtonPoint.X;
  Result.Y := Y - ButtonPoint.Y;
end;

procedure TPegtopTrackBar.DoScroll(const X, Y: Integer);
var
  NewPos: Integer;
begin
  NewPos := PointToPosition(X, Y);
  if NewPos <> FPosition then begin
    FPosition := NewPos;
    Invalidate;
    if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TPegtopTrackBar.DoEndScroll;
begin
  if Assigned(FOnScroll) then FOnScroll(Self, pscEndScroll, FPosition);
  Invalidate;
end;

procedure TPegtopTrackBar.WMEditValue(var Msg: TMessage);
var
  NumEditForm: TPegtopNumEditForm;
  UpperLeft: TPoint;
  MinVal, MaxVal: Double;
begin
  if not (ploDisableEdit in LabelOptions) then begin
    if CanFocus then begin
      SetFocus;
      Invalidate;
    end;
    UpperLeft := ClientToScreen(Point(FCaptionRect.Left, FCaptionRect.Top));
    MinVal := PositionToValue(FMin);
    MaxVal := PositionToValue(FMax);
    case FLabelMode of
      plmPos, plmMul, plmShl, plmBin, plmSqr, plmAdd, plmSub:
        begin
          NumEditForm := TPegtopIntEditForm.CreateNew(Application);
          if MinVal <= MaxVal then begin // could be inversed (plmSub)
            TPegtopIntEdit(NumEditForm.NumEdit).MinValue := Round(MinVal);
            TPegtopIntEdit(NumEditForm.NumEdit).MaxValue := Round(MaxVal);
          end
          else begin
            TPegtopIntEdit(NumEditForm.NumEdit).MinValue := Round(MaxVal);
            TPegtopIntEdit(NumEditForm.NumEdit).MaxValue := Round(MinVal);
          end;
          TPegtopIntEdit(NumEditForm.NumEdit).Value := Round(PositionToValue(FPosition));
        end;
      plmDiv, plmShr, plmInv, plmLog, plmExp:
        begin
          NumEditForm := TPegtopFloatEditForm.CreateNew(Application);
          case FLabelMode of
            plmDiv:
              TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(FLabelParam) / Ln(10) + 0.9999);
            plmShr:
              TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(FLabelParam);
            plmInv:
              if Abs(FMin) > Abs(FMax) then
                TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(Abs(FMin) / FLabelParam) / Ln(10) + 2.4999)
              else
                TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(Abs(FMax) / FLabelParam) / Ln(10) + 2.4999);
            plmLog:
              TPegtopFloatEdit(NumEditForm.NumEdit).Digits := 2;
            plmExp:
              TPegtopFloatEdit(NumEditForm.NumEdit).Digits := 0;
          end;
          if MinVal <= MaxVal then begin // could be inversed (plmInv)
            TPegtopFloatEdit(NumEditForm.NumEdit).MinValue := PositionToValue(FMin);
            TPegtopFloatEdit(NumEditForm.NumEdit).MaxValue := PositionToValue(FMax);
          end
          else begin
            TPegtopFloatEdit(NumEditForm.NumEdit).MinValue := PositionToValue(FMax);
            TPegtopFloatEdit(NumEditForm.NumEdit).MaxValue := PositionToValue(FMin);
          end;
          TPegtopFloatEdit(NumEditForm.NumEdit).Value := PositionToValue(FPosition);
        end;
      else
        NumEditForm := NIL; // NumEditForm must be initialized
    end;
    if Assigned(NumEditForm) then begin
      NumEditForm.NumEdit.Width := Canvas.TextWidth(StringOfChar('0', NumEditForm.NumEdit.GetMaxLength)) + 8;
      NumEditForm.NumEdit.SelectAll;
      NumEditForm.OnEditChange := NumEditChange;
      NumEditForm.OnClose := NumEditClose;
      NumEditForm.Left := UpperLeft.X + ((FPositionCaptionRect.Left + FPositionCaptionRect.Right) div 2) - (NumEditForm.NumEdit.Width div 2);
      NumEditForm.Top := UpperLeft.Y + FPositionCaptionRect.Bottom - NumEditForm.NumEdit.Height + 4;
      NumEditForm.Show;
    end;
    // SendMessage (ParentWindow, WM_NCACTIVATE, 1, 0);
  end;
end;

procedure TPegtopTrackBar.NumEditChange(Sender: TObject);
begin
  if Sender is TPegtopIntEditForm then
    ApplyPosition(ValueToPosition(TPegtopIntEdit(TPegtopIntEditForm(Sender).NumEdit).Value))
  else if Sender is TPegtopFloatEditForm then
    ApplyPosition(ValueToPosition(TPegtopFloatEdit(TPegtopFloatEditForm(Sender).NumEdit).Value));
end;

procedure TPegtopTrackBar.NumEditClose(Sender: TObject; var Action: TCloseAction);
begin
  if CanFocus then begin
    SetFocus;
    Invalidate;
  end;
end;

procedure TPegtopTrackBar.ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not (ploDisableEdit in LabelOptions))
  and (ploVisible in LabelOptions)
  and (X >= FCaptionRect.Left + FPositionCaptionRect.Left) and (X < FCaptionRect.Left + FPositionCaptionRect.Right)
  and (Y >= FCaptionRect.Top + FPositionCaptionRect.Top) and (Y < FCaptionRect.Top + FPositionCaptionRect.Bottom)
  and HasExtent(FPositionCaptionRect) then begin
    EditValue;
  end;
  inherited;
end;

procedure TPegtopTrackBar.ExtraMouseMove(Shift: TShiftState; X, Y: Integer);
var
  ButtonSize: TSize;
  ButtonPoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  ButtonPoint := GetButtonPoint;
  if (X >= ButtonPoint.X) and (Y >= ButtonPoint.Y)
  and (X < ButtonPoint.X + ButtonSize.CX) and (Y < ButtonPoint.Y + ButtonSize.CY) then begin
    if not FMouseHover then begin
      FMouseHover := True;
      Invalidate;
    end;
  end
  else begin
    if FMouseHover then begin
      FMouseHover := False;
      Invalidate;
    end;
  end;
  if (not (ploDisableEdit in LabelOptions))
  and (ploVisible in LabelOptions)
  and (X >= FCaptionRect.Left + FPositionCaptionRect.Left) and (X < FCaptionRect.Left + FPositionCaptionRect.Right)
  and (Y >= FCaptionRect.Top + FPositionCaptionRect.Top) and (Y < FCaptionRect.Top + FPositionCaptionRect.Bottom)
  and HasExtent(FPositionCaptionRect) then begin
    ChangeCursor(crIBeam);
  end
  else begin
    ChangeCursor(FCursor);
  end;
  inherited;
end;

procedure TPegtopTrackBar.PopupContextMenu(const X, Y: Integer);
var
  I: Integer;
begin
  TrackBarPopupMenu.Items[0].Enabled := FPosition <> FDefaultPosition;
  TrackBarPopupMenu.Items[2].Visible := not (ploDisableEdit in LabelOptions);
  TrackBarPopupMenu.Items[2].Enabled := HasExtent(FPositionCaptionRect);
  TrackBarPopupMenu.Items[3].Visible := not (ploDisableCopy in LabelOptions);
  TrackBarPopupMenu.Items[4].Visible := not (ploDisablePaste in LabelOptions);
  TrackBarPopupMenu.Items[4].Enabled := Clipboard.HasFormat(CF_TEXT) and IsValidFloat(Clipboard.AsText);
  for I := 0 to TrackBarPopupMenu.Items.Count - 1 do TrackBarPopupMenu.Items[I].OnClick := MenuItemClick;
  TrackBarPopupMenu.PopupComponent := Self;
  TrackBarPopupMenu.Popup(X, Y);
end;

procedure TPegtopTrackBar.EditValue;
begin
  PostMessage(Handle, WM_PEGTOPSLIDEBAR_EDITVALUE, 0, 0);
end;

procedure TPegtopTrackBar.CopyValue;
begin
  Clipboard.AsText := PositionToString(FPosition);
end;

procedure TPegtopTrackBar.PasteValue;
begin
  if Clipboard.HasFormat(CF_TEXT) then try
    ApplyPosition(ValueToPosition(StrToFloat(Clipboard.AsText)));
  except
  end;
end;

procedure TPegtopTrackBar.ApplyPosition(const P: Integer);
var
  OldPos: Integer;
begin
  OldPos := FPosition;
  SetPosition(P);
  if FPosition <> OldPos then begin
    if Assigned(FOnScroll) then FOnScroll(Self, pscPosition, FPosition);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TPegtopTrackBar.ChangePosition(const Delta: Integer);
var
  OldPos: Integer;
begin
  OldPos := FPosition;
  SetPosition(FPosition + Delta);
  if FPosition <> OldPos then begin
    if Assigned(FOnScroll) then begin
      if Delta < 0 then
        FOnScroll(Self, pscLineUp, FPosition)
      else
        FOnScroll(Self, pscLineDown, FPosition);
    end;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TPegtopTrackBar.ResetToDefault;
begin
  if FPosition <> FDefaultPosition then begin
    FPosition := FDefaultPosition;
    if Assigned(FOnScroll) then FOnScroll(Self, pscPosition, FPosition);
    if Assigned(FOnChange) then FOnChange(Self);
    Invalidate;
  end;
end;

function TPegtopTrackBar.TransformCaption(const S: String): String;
var
  T: String;
  E: TSize;
  P: Integer;
begin
  Result := S;
  T := PositionToString(FPosition);
  P := Pos('<pos>', AnsiLowerCase(Result));
  if P > 0 then begin
    E := Canvas.TextExtent(T);
    FPositionCaptionRect := Bounds(Canvas.TextWidth(Copy(Result, 1, P - 1)), 0, E.CX, E.CY);
    Result := StringReplace(Result, '<pos>', T ,[rfReplaceAll, rfIgnoreCase]);
  end
  else FPositionCaptionRect := Rect(0, 0, 0, 0);
end;

function TPegtopTrackBar.GetButtonSize: TSize;
begin
  if Orientation = psoHorizontal then begin
    Result.CX := 25;
    Result.CY := 17;
  end
  else begin
    Result.CX := 17;
    Result.CY := 25;
  end;
end;

function TPegtopTrackBar.GetButtonPoint: TPoint;
var
  ButtonSize: TSize;
  LinePoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  LinePoint := GetLinePoint;
  if Orientation = psoHorizontal then begin
    if FMax = FMin then begin
      Result.X := 0;
    end
    else begin
      Result.X := MulDiv(FPosition - FMin, ClientWidth - ButtonSize.CX, FMax - FMin);
    end;
    Result.Y := LinePoint.Y - ButtonSize.CY div 2;
  end
  else begin
    if FMax = FMin then begin
      Result.Y := 0;
    end
    else begin
      Result.Y := MulDiv(FMax - FPosition, ClientHeight - ButtonSize.CY, FMax - FMin);
    end;
    Result.X := LinePoint.X - ButtonSize.CX div 2;
  end;
end;

function TPegtopTrackBar.PointToPosition(const X, Y: Integer): Integer;
var
  ButtonSize: TSize;
begin
  if FMax = FMin then begin
    Result := FMin;
  end
  else begin
    ButtonSize := GetButtonSize;
    if FOrientation = psoHorizontal then
      Result := ((X * (FMax - FMin) div FSmallChange + ((ClientWidth - ButtonSize.CX) div 2)) div (ClientWidth - ButtonSize.CX)) * FSmallChange + FMin
    else
      Result := FMax - ((Y * (FMax - FMin) div FSmallChange + ((ClientHeight - ButtonSize.CY) div 2)) div (ClientHeight - ButtonSize.CY)) * FSmallChange;
    if Result < FMin then Result := FMin else if Result > FMax then Result := FMax;
  end;
end;

procedure TPegtopTrackBar.MenuItemClick(Sender: TObject);
begin
  if Sender is TComponent then begin
    case TComponent(Sender).Tag of
      0:
        ResetToDefault;
      2:
        EditValue;
      3:
        CopyValue;
      4:
        PasteValue;
    end;
  end;
end;

procedure TPegtopTrackBar.SetPosition(V: Integer);
begin
  if V < Min then V := Min
  else if V > Max then V := Max;
  if FPosition <> V then begin
    FPosition := V;
    Invalidate;
  end;
end;

procedure TPegtopTrackBar.SetDefaultPosition(V: Integer);
begin
  if V < Min then V := Min
  else if V > Max then V := Max;
  if FDefaultPosition <> V then begin
    FDefaultPosition := V;
    Invalidate;
  end;
end;

function TPegtopTrackBar.GetValue: Double;
begin
  Result := PositionToValue(FPosition);
end;

procedure TPegtopTrackBar.SetValue(V: Double);
begin
  SetPosition(ValueToPosition(V));
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorTrackBar
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorTrackBar.DrawButtonGrip(const ACanvas: TCanvas; const ARect: TRect; const AState: TPegtopThemeState);
  procedure BlendPixel(const Canvas: TCanvas; const X, Y: Integer; const Color: TColor; const Alpha: Integer);
  var
    C: TColor;
    R, G, B: Integer;
  begin
    C := Canvas.Pixels[X, Y];
    R := (C and $FF) + ((Color and $FF) - (C and $FF)) * Alpha div 256;
    G := (C shr 8 and $FF) + ((Color shr 8 and $FF) - (C shr 8 and $FF)) * Alpha div 256;
    B := (C shr 16 and $FF) + ((Color shr 16 and $FF) - (C shr 16 and $FF)) * Alpha div 256;
    Canvas.Pixels[X, Y] := R or (G shl 8) or (B shl 16);
  end;
var
  C: TColor;
begin
  if Enabled then begin
    C := ColorToRGB(FButtonColor);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := C;
    ACanvas.FillRect(Rect(ARect.Left + 7, ARect.Top + 6, ARect.Right - 7, ARect.Top + 7));
    ACanvas.FillRect(Rect(ARect.Left + 6, ARect.Top + 7, ARect.Right - 6, ARect.Bottom - 7));
    ACanvas.FillRect(Rect(ARect.Left + 7, ARect.Bottom - 7, ARect.Right - 7, ARect.Bottom - 6));
    BlendPixel(ACanvas, ARect.Left + 6, ARect.Top + 6, C, 128);
    BlendPixel(ACanvas, ARect.Right - 7, ARect.Top + 6, C, 128);
    BlendPixel(ACanvas, ARect.Left + 6, ARect.Bottom - 7, C, 128);
    BlendPixel(ACanvas, ARect.Right - 7, ARect.Bottom - 7, C, 128);
  end;
end;

procedure TPegtopColorTrackBar.SetButtonColor(V: TColor);
begin
  if FButtonColor <> V then begin
    FButtonColor := V;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopRangeBar
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopRangeBar.Create(AOwner: TComponent);
const
  MenuCaption: array[0..9] of String = (PegtopRangeBarReset, PegtopRangeBarConstrained, '-',
    PegtopRangeBarEditMin, PegtopRangeBarCopyMin, PegtopRangeBarPasteMin, '-',
    PegtopRangeBarEditMax, PegtopRangeBarCopyMax, PegtopRangeBarPasteMax);
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  if RangeBarPopupMenu = NIL then begin
    RangeBarPopupMenu := TPopupMenu.Create(Application);
    for I := Low(MenuCaption) to High(MenuCaption) do begin
      MenuItem := TMenuItem.Create(RangeBarPopupMenu);
      MenuItem.Caption := MenuCaption[I];
      MenuItem.Tag := I;
      RangeBarPopupMenu.Items.Add(MenuItem);
    end;
  end;
  InitBitmaps;
  FPosition[0] := 0;
  FPosition[1] := 0;
  FDefaultPosition[0] := 0;
  FDefaultPosition[1] := 0;
  FButtonFocus := 0;
  FMouseHover := -1;
  inherited;
  LabelCaption := 'Position: <min> - <max>';
end;

procedure TPegtopRangeBar.Loaded;
begin
  inherited;
  FDefaultPosition[0] := FPosition[0];
  FDefaultPosition[1] := FPosition[1];
end;

procedure TPegtopRangeBar.CMWantSpecialKey(var Msg: TWMKey);
begin
  case Msg.CharCode of
    VK_TAB:
      if (FButtonFocus = 0) xor ((GetKeyState(VK_SHIFT) and 128) <> 0) then Msg.Result := 1 else inherited;
    else
      inherited;
  end;
end;

procedure TPegtopRangeBar.DoEnter;
begin
  if (GetKeyState(VK_TAB) and 128) <> 0 then begin
    if (GetKeyState(VK_SHIFT) and 128) <> 0 then FButtonFocus := 1 else FButtonFocus := 0;
  end;
  inherited;
end;

procedure TPegtopRangeBar.PaintTo(const ACanvas: TCanvas);
var
  ButtonPoint: array[0..1] of TPoint;
  ButtonSize: TSize;
  ButtonRect: TRect;
  State: TPegtopThemeState;
  BitmapIndex: Integer;
begin
  inherited;
  ButtonSize := GetButtonSize;
  if FConstrained then begin
    // draw min button:
    if not Enabled then
      State := ptsDisabled
    else if FMouseDown then
      State := ptsPushed
    else if FMouseHover = 0 then
      State := ptsHot
    else if Focused then
      State := ptsFocused
    else
      State := ptsNormal;
    ButtonPoint[0] := GetButtonPoint(0);
    ButtonPoint[1] := GetButtonPoint(1);
    ButtonRect := Rect(ButtonPoint[0].X, ButtonPoint[0].Y,
      ButtonPoint[1].X + ButtonSize.CX, ButtonPoint[1].Y + ButtonSize.CY);
    DrawButton(ACanvas, ButtonRect, State, Focused);
    if Enabled then begin
      if FOrientation = psoHorizontal then BitmapIndex := 1
      else BitmapIndex := 2;
      ACanvas.CopyMode := cmSrcAnd;
      ACanvas.Draw(ButtonPoint[0].X + (ButtonSize.CX - Bitmaps[BitmapIndex].Width + 1) div 2,
        ButtonPoint[0].Y + (ButtonSize.CY - Bitmaps[BitmapIndex].Height + 1) div 2, Bitmaps[BitmapIndex]);
      if FOrientation = psoHorizontal then BitmapIndex := 0
      else BitmapIndex := 3;
      ACanvas.CopyMode := cmSrcAnd;
      ACanvas.Draw(ButtonPoint[1].X + (ButtonSize.CX - Bitmaps[BitmapIndex].Width) div 2,
        ButtonPoint[1].Y + (ButtonSize.CY - Bitmaps[BitmapIndex].Height + 1) div 2, Bitmaps[BitmapIndex]);
    end;
  end
  else begin
    // draw min button:
    if not Enabled then
      State := ptsDisabled
    else if (FButtonFocus = 0) and FMouseDown then
      State := ptsPushed
    else if FMouseHover = 0 then
      State := ptsHot
    else if (FButtonFocus = 0) and Focused then
      State := ptsFocused
    else
      State := ptsNormal;
    ButtonPoint[0] := GetButtonPoint(0);
    ButtonRect := Bounds(ButtonPoint[0].X, ButtonPoint[0].Y, ButtonSize.CX, ButtonSize.CY);
    DrawButton(ACanvas, ButtonRect, State, (FButtonFocus = 0) and Focused);
    if Enabled then begin
      if FOrientation = psoHorizontal then BitmapIndex := 1
      else BitmapIndex := 2;
      ACanvas.CopyMode := cmSrcAnd;
      ACanvas.Draw(ButtonPoint[0].X + (ButtonSize.CX - Bitmaps[BitmapIndex].Width + 1) div 2,
        ButtonPoint[0].Y + (ButtonSize.CY - Bitmaps[BitmapIndex].Height + 1) div 2, Bitmaps[BitmapIndex]);
    end;
    // draw max button:
    if not Enabled then
      State := ptsDisabled
    else if (FButtonFocus = 1) and FMouseDown then
      State := ptsPushed
    else if FMouseHover = 1 then
      State := ptsHot
    else if (FButtonFocus = 1) and Focused then
      State := ptsFocused
    else
      State := ptsNormal;
    ButtonPoint[1] := GetButtonPoint(1);
    ButtonRect := Bounds(ButtonPoint[1].X, ButtonPoint[1].Y, ButtonSize.CX, ButtonSize.CY);
    DrawButton(ACanvas, ButtonRect, State, (FButtonFocus = 1) and Focused);
    if Enabled then begin
      if FOrientation = psoHorizontal then BitmapIndex := 0
      else BitmapIndex := 3;
      ACanvas.CopyMode := cmSrcAnd;
      ACanvas.Draw(ButtonPoint[1].X + (ButtonSize.CX - Bitmaps[BitmapIndex].Width) div 2,
        ButtonPoint[1].Y + (ButtonSize.CY - Bitmaps[BitmapIndex].Height + 1) div 2, Bitmaps[BitmapIndex]);
    end;
  end;
end;

procedure TPegtopRangeBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_TAB) then begin
    FButtonFocus := FButtonFocus xor 1;
    Invalidate;
  end;
  inherited;
end;

procedure TPegtopRangeBar.ValidatePosition;
var
  I: Integer;
begin
  for I := 0 to 1 do begin
    if FPosition[I] < Min then FPosition[I] := Min
    else if FPosition[I] > Max then FPosition[I] := Max;
    if FDefaultPosition[I] < Min then FDefaultPosition[I] := Min
    else if FDefaultPosition[I] > Max then FDefaultPosition[I] := Max;
  end;
end;

function TPegtopRangeBar.DoStartScroll(const X, Y: Integer): TPoint;
var
  ButtonSize: TSize;
  ButtonPoint: array[0..1] of TPoint;
  NewPos: Integer;
begin
  ButtonSize := GetButtonSize;
  ButtonPoint[0] := GetButtonPoint(0);
  ButtonPoint[1] := GetButtonPoint(1);
  // which button is nearer?
  if Abs(X - (ButtonPoint[1].X + ButtonSize.CX div 2) + Y - (ButtonPoint[1].Y + ButtonSize.CY div 2))
  < Abs(X - (ButtonPoint[0].X + ButtonSize.CX div 2) + Y - (ButtonPoint[0].Y + ButtonSize.CY div 2))
  then
    FButtonFocus := 1
  else
    FButtonFocus := 0;
  if FConstrained then begin
    if (X < ButtonPoint[0].X) or (Y < ButtonPoint[1].Y)
    or (X >= ButtonPoint[1].X + ButtonSize.CX) or (Y >= ButtonPoint[0].Y + ButtonSize.CY) then begin
      // clicked outside the button
      ButtonPoint[FButtonFocus].X := X - (ButtonSize.CX div 2);
      ButtonPoint[FButtonFocus].Y := Y - (ButtonSize.CY div 2);
      NewPos := PointToPosition(FButtonFocus, ButtonPoint[FButtonFocus].X, ButtonPoint[FButtonFocus].Y);
      if NewPos <> FPosition[FButtonFocus] then begin
        FPosition[FButtonFocus xor 1] := FPosition[FButtonFocus xor 1] + NewPos - FPosition[FButtonFocus];
        FPosition[FButtonFocus] := NewPos;
        Invalidate;
        if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[0]);
        if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[1]);
        if Assigned(FOnChange) then FOnChange(Self);
      end;
      ButtonPoint[FButtonFocus] := GetButtonPoint(FButtonFocus);
    end;
  end
  else begin
    if (X < ButtonPoint[FButtonFocus].X) or (Y < ButtonPoint[FButtonFocus].Y)
    or (X >= ButtonPoint[FButtonFocus].X + ButtonSize.CX) or (Y >= ButtonPoint[FButtonFocus].Y + ButtonSize.CY) then begin
      // clicked outside the button
      ButtonPoint[FButtonFocus].X := X - (ButtonSize.CX div 2);
      ButtonPoint[FButtonFocus].Y := Y - (ButtonSize.CY div 2);
      NewPos := PointToPosition(FButtonFocus, ButtonPoint[FButtonFocus].X, ButtonPoint[FButtonFocus].Y);
      if NewPos <> FPosition[FButtonFocus] then begin
        FPosition[FButtonFocus] := NewPos;
        Invalidate;
        if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[FButtonFocus]);
        if Assigned(FOnChange) then FOnChange(Self);
      end;
      ButtonPoint[FButtonFocus] := GetButtonPoint(FButtonFocus);
    end;
  end;
  Result.X := X - ButtonPoint[FButtonFocus].X;
  Result.Y := Y - ButtonPoint[FButtonFocus].Y;
end;

procedure TPegtopRangeBar.DoScroll(const X, Y: Integer);
var
  NewPos: Integer;
begin
  NewPos := PointToPosition(FButtonFocus, X, Y);
  if NewPos <> FPosition[FButtonFocus] then begin
    if FConstrained then begin
      FPosition[FButtonFocus xor 1] := FPosition[FButtonFocus xor 1] + NewPos - FPosition[FButtonFocus];
      FPosition[FButtonFocus] := NewPos;
      Invalidate;
      if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[0]);
      if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[1]);
      if Assigned(FOnChange) then FOnChange(Self);
    end
    else begin
      FPosition[FButtonFocus] := NewPos;
      Invalidate;
      if Assigned(FOnScroll) then FOnScroll(Self, pscTrack, FPosition[FButtonFocus]);
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;

procedure TPegtopRangeBar.DoEndScroll;
begin
  if Assigned(FOnScroll) then FOnScroll(Self, pscEndScroll, FPosition[FButtonFocus]);
  Invalidate;
end;

procedure TPegtopRangeBar.WMEditValue(var Msg: TMessage);
var
  NumEditForm: TPegtopNumEditForm;
  UpperLeft: TPoint;
  MinVal, MaxVal: Double;
begin
  if not (ploDisableEdit in LabelOptions) then begin
    if Msg.WParam in [0, 1] then begin
      FButtonFocus := Msg.WParam;
    end;
    if CanFocus then begin
      SetFocus;
      Invalidate;
    end;
    UpperLeft := ClientToScreen(Point(FCaptionRect.Left, FCaptionRect.Top));
    MinVal := PositionToValue(FMin);
    MaxVal := PositionToValue(FMax);
    case FLabelMode of
      plmPos, plmMul, plmShl, plmBin, plmSqr, plmAdd, plmSub:
        begin
          NumEditForm := TPegtopIntEditForm.CreateNew(Application);
          if MinVal <= MaxVal then begin // could be inversed (plmSub)
            TPegtopIntEdit(NumEditForm.NumEdit).MinValue := Round(MinVal);
            TPegtopIntEdit(NumEditForm.NumEdit).MaxValue := Round(MaxVal);
          end
          else begin
            TPegtopIntEdit(NumEditForm.NumEdit).MinValue := Round(MaxVal);
            TPegtopIntEdit(NumEditForm.NumEdit).MaxValue := Round(MinVal);
          end;
          TPegtopIntEdit(NumEditForm.NumEdit).Value := Round(PositionToValue(FPosition[FButtonFocus]));
        end;
      plmDiv, plmShr, plmInv, plmLog, plmExp:
        begin
          NumEditForm := TPegtopFloatEditForm.CreateNew(Application);
          case FLabelMode of
            plmDiv:
              TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(FLabelParam) / Ln(10) + 0.9999);
            plmShr:
              TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(FLabelParam);
            plmInv:
              if Abs(FMin) > Abs(FMax) then
                TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(Abs(FMin) / FLabelParam) / Ln(10) + 2.4999)
              else
                TPegtopFloatEdit(NumEditForm.NumEdit).Digits := Trunc(Ln(Abs(FMax) / FLabelParam) / Ln(10) + 2.4999);
            plmLog:
              TPegtopFloatEdit(NumEditForm.NumEdit).Digits := 2;
            plmExp:
              TPegtopFloatEdit(NumEditForm.NumEdit).Digits := 0;
          end;
          if MinVal <= MaxVal then begin // could be inversed (plmInv)
            TPegtopFloatEdit(NumEditForm.NumEdit).MinValue := PositionToValue(FMin);
            TPegtopFloatEdit(NumEditForm.NumEdit).MaxValue := PositionToValue(FMax);
          End
          Else Begin
            TPegtopFloatEdit(NumEditForm.NumEdit).MinValue := PositionToValue(FMax);
            TPegtopFloatEdit(NumEditForm.NumEdit).MaxValue := PositionToValue(FMin);
          End;
          TPegtopFloatEdit(NumEditForm.NumEdit).Value := PositionToValue(FPosition[FButtonFocus]);
        end;
      else
        NumEditForm := NIL; // NumEditForm must be initialized
    end;
    if Assigned(NumEditForm) then begin
      NumEditForm.NumEdit.Width := Canvas.TextWidth(StringOfChar('0', NumEditForm.NumEdit.GetMaxLength)) + 8;
      NumEditForm.NumEdit.SelectAll;
      NumEditForm.OnEditChange := NumEditChange;
      NumEditForm.OnClose := NumEditClose;
      NumEditForm.Left := UpperLeft.X + ((FPositionCaptionRect[FButtonFocus].Left + FPositionCaptionRect[FButtonFocus].Right) div 2) - (NumEditForm.NumEdit.Width div 2);
      NumEditForm.Top := UpperLeft.Y + FPositionCaptionRect[FButtonFocus].Bottom - NumEditForm.NumEdit.Height + 4;
      NumEditForm.Show;
    end;
    // SendMessage (ParentWindow, WM_NCACTIVATE, 1, 0);
  end;
end;

procedure TPegtopRangeBar.NumEditChange(Sender: TObject);
begin
  if Sender is TPegtopIntEditForm then
    ApplyPosition(ValueToPosition(TPegtopIntEdit(TPegtopIntEditForm(Sender).NumEdit).Value))
  else if Sender is TPegtopFloatEditForm then
    ApplyPosition(ValueToPosition(TPegtopFloatEdit(TPegtopFloatEditForm(Sender).NumEdit).Value));
end;

procedure TPegtopRangeBar.NumEditClose(Sender: TObject; var Action: TCloseAction);
begin
  if CanFocus then begin
    SetFocus;
    Invalidate;
  end;
end;

procedure TPegtopRangeBar.ExtraMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not (ploDisableEdit in LabelOptions))
  and (ploVisible in LabelOptions)
  and (X >= FCaptionRect.Left + FPositionCaptionRect[0].Left) and (X < FCaptionRect.Left + FPositionCaptionRect[0].Right)
  and (Y >= FCaptionRect.Top + FPositionCaptionRect[0].Top) and (Y < FCaptionRect.Top + FPositionCaptionRect[0].Bottom)
  and HasExtent(FPositionCaptionRect[0]) then begin
    EditValueMin;
  end
  else if (not (ploDisableEdit in LabelOptions))
  and (ploVisible in LabelOptions)
  and (X >= FCaptionRect.Left + FPositionCaptionRect[1].Left) and (X < FCaptionRect.Left + FPositionCaptionRect[1].Right)
  and (Y >= FCaptionRect.Top + FPositionCaptionRect[1].Top) and (Y < FCaptionRect.Top + FPositionCaptionRect[1].Bottom)
  and HasExtent(FPositionCaptionRect[1]) then begin
    EditValueMax;
  end;
  inherited;
end;

procedure TPegtopRangeBar.ExtraMouseMove(Shift: TShiftState; X, Y: Integer);
var
  ButtonSize: TSize;
  ButtonPoint: array[0..1] of TPoint;
  NewHover: Integer;
begin
  NewHover := -1;
  ButtonSize := GetButtonSize;
  ButtonPoint[0] := GetButtonPoint(0);
  ButtonPoint[1] := GetButtonPoint(1);
  if FConstrained then begin
    if (X >= ButtonPoint[0].X) and (Y >= ButtonPoint[0].Y)
    and (X < ButtonPoint[1].X + ButtonSize.CX) and (Y < ButtonPoint[1].Y + ButtonSize.CY) then
      NewHover := 0;
  end
  else begin
    if (X >= ButtonPoint[0].X) and (Y >= ButtonPoint[0].Y)
    and (X < ButtonPoint[0].X + ButtonSize.CX) and (Y < ButtonPoint[0].Y + ButtonSize.CY) then
      NewHover := 0
    else if (X >= ButtonPoint[1].X) and (Y >= ButtonPoint[1].Y)
    and (X < ButtonPoint[1].X + ButtonSize.CX) and (Y < ButtonPoint[1].Y + ButtonSize.CY) then
      NewHover := 1;
  end;
  if FMouseHover <> NewHover then begin
    FMouseHover := NewHover;
    Invalidate;
  end;
  if (not (ploDisableEdit in LabelOptions))
  and (ploVisible in LabelOptions)
  and (X >= FCaptionRect.Left + FPositionCaptionRect[0].Left) and (X < FCaptionRect.Left + FPositionCaptionRect[0].Right)
  and (Y >= FCaptionRect.Top + FPositionCaptionRect[0].Top) and (Y < FCaptionRect.Top + FPositionCaptionRect[0].Bottom)
  and HasExtent(FPositionCaptionRect[0]) then begin
    ChangeCursor(crIBeam);
  end
  else if (not (ploDisableEdit in LabelOptions))
  and (ploVisible in LabelOptions)
  and (X >= FCaptionRect.Left + FPositionCaptionRect[1].Left) and (X < FCaptionRect.Left + FPositionCaptionRect[1].Right)
  and (Y >= FCaptionRect.Top + FPositionCaptionRect[1].Top) and (Y < FCaptionRect.Top + FPositionCaptionRect[1].Bottom)
  and HasExtent(FPositionCaptionRect[1]) then begin
    ChangeCursor(crIBeam);
  end
  else begin
    ChangeCursor(FCursor);
  end;
  inherited;
end;

procedure TPegtopRangeBar.PopupContextMenu(const X, Y: Integer);
var
  I: Integer;
begin
  RangeBarPopupMenu.Items[0].Enabled := (FPosition[0] <> FDefaultPosition[0]) or (FPosition[1] <> FDefaultPosition[1]);
  RangeBarPopupMenu.Items[1].Visible := not (proDisableConstrain in FRangeOptions);
  RangeBarPopupMenu.Items[1].Enabled := FConstrained or (FPosition[0] > FMin) or (FPosition[1] < FMax);
  RangeBarPopupMenu.Items[1].Checked := FConstrained;
  RangeBarPopupMenu.Items[3].Visible := not (ploDisableEdit in LabelOptions);
  RangeBarPopupMenu.Items[3].Enabled := HasExtent(FPositionCaptionRect[0]);
  RangeBarPopupMenu.Items[4].Visible := not (ploDisableCopy in LabelOptions);
  RangeBarPopupMenu.Items[5].Visible := not (ploDisablePaste in LabelOptions);
  RangeBarPopupMenu.Items[5].Enabled := Clipboard.HasFormat(CF_TEXT) and IsValidFloat(Clipboard.AsText);
  RangeBarPopupMenu.Items[7].Visible := not (ploDisableEdit in LabelOptions);
  RangeBarPopupMenu.Items[7].Enabled := HasExtent(FPositionCaptionRect[1]);
  RangeBarPopupMenu.Items[8].Visible := not (ploDisableCopy in LabelOptions);
  RangeBarPopupMenu.Items[9].Visible := not (ploDisablePaste in LabelOptions);
  RangeBarPopupMenu.Items[9].Enabled := Clipboard.HasFormat(CF_TEXT) and IsValidFloat(Clipboard.AsText);
  for I := 0 to RangeBarPopupMenu.Items.Count - 1 do RangeBarPopupMenu.Items[I].OnClick := MenuItemClick;
  RangeBarPopupMenu.PopupComponent := Self;
  RangeBarPopupMenu.Popup(X, Y);
end;

procedure TPegtopRangeBar.EditValueMin;
begin
  PostMessage(Handle, WM_PEGTOPSLIDEBAR_EDITVALUE, 0, 0);
end;

procedure TPegtopRangeBar.EditValueMax;
begin
  PostMessage(Handle, WM_PEGTOPSLIDEBAR_EDITVALUE, 1, 0);
end;

procedure TPegtopRangeBar.CopyValueMin;
begin
  Clipboard.AsText := PositionToString(FPosition[0]);
end;

procedure TPegtopRangeBar.CopyValueMax;
begin
  Clipboard.AsText := PositionToString(FPosition[1]);
end;

procedure TPegtopRangeBar.PasteValueMin;
begin
  if Clipboard.HasFormat(CF_TEXT) then try
    FButtonFocus := 0;
    ApplyPosition(ValueToPosition(StrToFloat(Clipboard.AsText)));
  except
  end;
end;

procedure TPegtopRangeBar.PasteValueMax;
begin
  if Clipboard.HasFormat(CF_TEXT) then try
    FButtonFocus := 1;
    ApplyPosition(ValueToPosition(StrToFloat(Clipboard.AsText)));
  except
  end;
end;

procedure TPegtopRangeBar.ApplyPosition(const P: Integer);
var
  NewPos: Integer;
begin
  NewPos := P;
  if NewPos < Min then NewPos := Min
  else if NewPos > Max then NewPos := Max
  else if (FButtonFocus = 0) and (NewPos > FPosition[1]) then NewPos := FPosition[1]
  else if (FButtonFocus = 1) and (NewPos < FPosition[0]) then NewPos := FPosition[0];
  if FPosition[FButtonFocus] <> NewPos then begin
    FPosition[FButtonFocus] := NewPos;
    Invalidate;
    if Assigned(FOnScroll) then FOnScroll(Self, pscPosition, FPosition[FButtonFocus]);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TPegtopRangeBar.ChangePosition(const Delta: Integer);
var
  NewPos: Integer;
begin
  if FConstrained then begin
    if Delta < 0 then begin
      NewPos := FPosition[0] + Delta;
      if NewPos < Min then NewPos := Min;
      if FPosition[0] <> NewPos then begin
        FPosition[1] := FPosition[1] + NewPos - FPosition[0];
        FPosition[0] := NewPos;
        Invalidate;
        if Assigned(FOnScroll) then begin
          if Delta < 0 then begin
            FOnScroll(Self, pscLineUp, FPosition[0]);
            FOnScroll(Self, pscLineUp, FPosition[1]);
          end
          else begin
            FOnScroll(Self, pscLineDown, FPosition[0]);
            FOnScroll(Self, pscLineDown, FPosition[1]);
          end;
        end;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    end
    else begin
      NewPos := FPosition[1] + Delta;
      if NewPos > Max then NewPos := Max;
      if FPosition[1] <> NewPos then begin
        FPosition[0] := FPosition[0] + NewPos - FPosition[1];
        FPosition[1] := NewPos;
        Invalidate;
        if Assigned(FOnScroll) then begin
          if Delta < 0 then begin
            FOnScroll(Self, pscLineUp, FPosition[0]);
            FOnScroll(Self, pscLineUp, FPosition[1]);
          end
          else begin
            FOnScroll(Self, pscLineDown, FPosition[0]);
            FOnScroll(Self, pscLineDown, FPosition[1]);
          end;
        end;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    end;
  end
  else begin
    NewPos := FPosition[FButtonFocus] + Delta;
    if NewPos < Min then NewPos := Min
    else if NewPos > Max then NewPos := Max
    else if (FButtonFocus = 0) and (NewPos > FPosition[1]) then NewPos := FPosition[1]
    else if (FButtonFocus = 1) and (NewPos < FPosition[0]) then NewPos := FPosition[0];
    if FPosition[FButtonFocus] <> NewPos then begin
      FPosition[FButtonFocus] := NewPos;
      Invalidate;
      if Assigned(FOnScroll) then begin
        if Delta < 0 then
          FOnScroll(Self, pscLineUp, FPosition[FButtonFocus])
        else
          FOnScroll(Self, pscLineDown, FPosition[FButtonFocus]);
      end;
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;

procedure TPegtopRangeBar.ResetToDefault;
begin
  if (FPosition[0] <> FDefaultPosition[0]) or (FPosition[1] <> FDefaultPosition[1]) then begin
    FPosition[0] := FDefaultPosition[0];
    FPosition[1] := FDefaultPosition[1];
    if Assigned(FOnScroll) then FOnScroll(Self, pscPosition, FPosition[0]);
    if Assigned(FOnChange) then FOnChange(Self);
    Invalidate;
  end;
end;

function TPegtopRangeBar.TransformCaption(const S: String): String;
var
  L, T: String;
  E: TSize;
  P: array[0..1] of Integer;
begin
  Result := S;
  L := AnsiLowerCase(Result);
  P[0] := Pos('<min>', L);
  P[1] := Pos('<max>', L);
  if P[0] <= P[1] then begin
    if P[0] > 0 then begin
      T := PositionToString(FPosition[0]);
      E := Canvas.TextExtent(T);
      FPositionCaptionRect[0] := Bounds(Canvas.TextWidth(Copy(Result, 1, P[0] - 1)), 0, E.CX, E.CY);
      Result := StringReplace(Result, '<min>', T ,[rfReplaceAll, rfIgnoreCase]);
    end
    else FPositionCaptionRect[0] := Rect(0, 0, 0, 0);
    P[1] := Pos('<max>', AnsiLowerCase(Result));
    if P[1] > 0 then begin
      T := PositionToString(FPosition[1]);
      E := Canvas.TextExtent(T);
      FPositionCaptionRect[1] := Bounds(Canvas.TextWidth(Copy(Result, 1, P[1] - 1)), 0, E.CX, E.CY);
      Result := StringReplace(Result, '<max>', T ,[rfReplaceAll, rfIgnoreCase]);
    end
    else FPositionCaptionRect[1] := Rect(0, 0, 0, 0);
  end
  else begin
    if P[1] > 0 then begin
      T := PositionToString(FPosition[1]);
      E := Canvas.TextExtent(T);
      FPositionCaptionRect[1] := Bounds(Canvas.TextWidth(Copy(Result, 1, P[1] - 1)), 0, E.CX, E.CY);
      Result := StringReplace(Result, '<max>', T ,[rfReplaceAll, rfIgnoreCase]);
    end
    else FPositionCaptionRect[1] := Rect(0, 0, 0, 0);
    P[0] := Pos('<min>', AnsiLowerCase(Result));
    if P[0] > 0 then begin
      T := PositionToString(FPosition[0]);
      E := Canvas.TextExtent(T);
      FPositionCaptionRect[0] := Bounds(Canvas.TextWidth(Copy(Result, 1, P[0] - 1)), 0, E.CX, E.CY);
      Result := StringReplace(Result, '<min>', T ,[rfReplaceAll, rfIgnoreCase]);
    end
    else FPositionCaptionRect[0] := Rect(0, 0, 0, 0);
  end;
end;

function TPegtopRangeBar.GetButtonSize: TSize;
begin
  if Orientation = psoHorizontal then begin
    Result.CX := 17;
    Result.CY := 17;
  end
  else begin
    Result.CX := 17;
    Result.CY := 17;
  end;
end;

function TPegtopRangeBar.GetButtonPoint(Index: Integer): TPoint;
var
  ButtonSize: TSize;
  LinePoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  LinePoint := GetLinePoint;
  if Orientation = psoHorizontal then begin
    if FMax = FMin then begin
      Result.X := 0;
    end
    else begin
      Result.X := MulDiv(FPosition[Index] - FMin, ClientWidth - 2 * ButtonSize.CX, FMax - FMin) + Index * ButtonSize.CX;
    end;
    Result.Y := LinePoint.Y - ButtonSize.CY div 2;
  end
  else begin
    if FMax = FMin then begin
      Result.Y := 0;
    end
    else begin
      Result.Y := MulDiv(FMax - FPosition[Index], ClientHeight - 2 * ButtonSize.CY, FMax - FMin) + (1 - Index) * ButtonSize.CX;
    end;
    Result.X := LinePoint.X - ButtonSize.CX div 2;
  end;
end;

function TPegtopRangeBar.PointToPosition(const Index, X, Y: Integer): Integer;
var
  ButtonSize: TSize;
begin
  if FMax = FMin then begin
    Result := FMin;
  end
  else begin
    ButtonSize := GetButtonSize;
    if FOrientation = psoHorizontal then
      Result := (((X - Index * ButtonSize.CX) * (FMax - FMin) div FSmallChange + ((ClientWidth - ButtonSize.CX * 2) div 2)) div (ClientWidth - ButtonSize.CX * 2)) * FSmallChange + FMin
    else
      Result := FMax - (((Y - Index * ButtonSize.CY) * (FMax - FMin) div FSmallChange + ((ClientHeight - ButtonSize.CY * 2) div 2)) div (ClientHeight - ButtonSize.CY * 2)) * FSmallChange;
    if FConstrained then begin
      if Index = 1 then begin
        if Result - FPosition[1] + FPosition[0] < FMin then Result := FMin + FPosition[1] - FPosition[0] else if Result > FMax then Result := FMax;
      end
      else begin
        if Result < FMin then Result := FMin else if Result + FPosition[1] - FPosition[0] > FMax then Result := FMax - FPosition[1] + FPosition[0];
      end;
    end
    else begin
      if Index = 1 then begin
        if Result < FPosition[0] then Result := FPosition[0] else if Result > FMax then Result := FMax;
      end
      else begin
        if Result < FMin then Result := FMin else if Result > FPosition[1] then Result := FPosition[1];
      end;
    end;
  end;
end;

procedure TPegtopRangeBar.MenuItemClick(Sender: TObject);
begin
  if Sender is TComponent then begin
    case TComponent(Sender).Tag of
      0:
        ResetToDefault;
      1:
        Constrained := not Constrained;
      3:
        EditValueMin;
      4:
        CopyValueMin;
      5:
        PasteValueMin;
      7:
        EditValueMax;
      8:
        CopyValueMax;
      9:
        PasteValueMax;
    end;
  end;
end;

function TPegtopRangeBar.GetPosition(Index: Integer): Integer;
begin
  Result := FPosition[Index];
end;

procedure TPegtopRangeBar.SetPosition(Index: Integer; V: Integer);
begin
  if V < Min then V := Min
  else if V > Max then V := Max;
  if FPosition[Index] <> V then begin
    FPosition[Index] := V;
    if Index = 1 then begin
      if FPosition[0] > FPosition[1] then FPosition[0] := FPosition[1];
    end
    else begin
      if FPosition[1] < FPosition[0] then FPosition[1] := FPosition[0];
    end;
    Invalidate;
  end;
end;

function TPegtopRangeBar.GetDefaultPosition(Index: Integer): Integer;
begin
  Result := FDefaultPosition[Index];
end;

procedure TPegtopRangeBar.SetDefaultPosition(Index: Integer; V: Integer);
begin
  if V < Min then V := Min
  else if V > Max then V := Max;
  if FDefaultPosition[Index] <> V then begin
    FDefaultPosition[Index] := V;
    if Index = 1 then begin
      if FDefaultPosition[0] > FDefaultPosition[1] then FDefaultPosition[0] := FDefaultPosition[1];
    end
    else begin
      if FDefaultPosition[1] < FDefaultPosition[0] then FDefaultPosition[1] := FDefaultPosition[0];
    end;
  end;
end;

function TPegtopRangeBar.GetValue(Index: Integer): Double;
begin
  Result := PositionToValue(FPosition[Index]);
end;

procedure TPegtopRangeBar.SetValue(Index: Integer; V: Double);
begin
  SetPosition(Index, ValueToPosition(V));
end;

procedure TPegtopRangeBar.SetConstrained(V: Boolean);
begin
  if FConstrained <> V then begin
    FConstrained := V;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopHintWindow
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopHintWindow.WMNCPaint(var Msg: TMessage);
var
  DC: HDC;
  R: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    R := Rect(0, 0, Width, Height);
    Rectangle(DC, R.Left, R.Top, R.Right, R.Bottom);
    // DrawEdge(DC, R, BDR_RAISEDOUTER, BF_RECT);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

initialization
  TrackBarPopupMenu := NIL;
  RangeBarPopupMenu := NIL;
  BitmapsLoaded := False;
  LabelHintWindow := NIL;
  HintWindowClass := TPegtopHintWindow;
finalization
  CloseBitmaps;
end.

