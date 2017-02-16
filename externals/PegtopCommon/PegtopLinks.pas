////////////////////////////////////////////////////////////////////////////////
// File:       PegtopLinks.pas
// Classes:    TPegtopCustomLink, TPegtopLink, TPegtopWebLink
// Version:    1.02
// Date:       29 Oct 2004 1.00
//             20 Apr 2005 1.01 (Layout property added, baseline just.)
//             13 Oct 2005 1.02 (BreakAlignment property added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopLink displayes a clickable link, which can be surrounded by other text
// and can change its appearence based on its state (mouse hover etc.).
// While TPegtopLink can be used to handle anything (OnClick), TPegtopWebLink
// was made to open an URL in the default browser (providing an address).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopLinks;

interface

uses
  Windows, Classes, Messages, Graphics, Controls,
  ShellAPI, StdCtrls;

type
  TPegtopLinkState = (plsNormal, plsVisited, plsFocus, plsHover, plsActive);
  TPegtopLinkLayout = (pllTop, pllMiddle, pllBottom);
  TPegtopLinkElement = (plePrefix, PleSuffix);
  TPegtopLinkElements = set of TPegtopLinkElement;

  TPegtopCustomLink = class(TCustomControl)
  private
    FCaptionPrefix: TCaption;
    FCaptionSuffix: TCaption;
    FAlignment: TAlignment;
    FLayout: TPegtopLinkLayout;
    FBreakAlignment: TPegtopLinkElements;
    FVisited: Boolean;
    FMouseDown: Boolean;
    FState: TPegtopLinkState;
    FLinkFont: TFont;
    FCursor: TCursor;
    FLinkCursor: TCursor;
    FLinkRect: TRect;
    FLinkHover: Boolean;
    FOnStateChange: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnVisit: TNotifyEvent;
    procedure WMSetText(var Msg: TMessage); message WM_SETTEXT;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure LinkFontChange(Sender: TObject);
    procedure ChangeCursor(const NewCursor: TCursor);
    procedure CheckState;
    procedure SetCaptionPrefix(Value: TCaption);
    procedure SetCaptionSuffix(Value: TCaption);
    procedure SetAlignment(Value: TAlignment);
    procedure SetLayout(Value: TPegtopLinkLayout);
    procedure SetBreakAlignment(Value: TPegtopLinkElements);
    procedure SetVisited(Value: Boolean);
    procedure SetLinkFont(Value: TFont);
    procedure SetCursor(Value: TCursor);
    procedure SetLinkCursor(Value: TCursor);
  protected
    procedure Paint; override;
    procedure DoVisit; virtual;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Layout: TPegtopLinkLayout read FLayout write SetLayout default pllTop;
    property BreakAlignment: TPegtopLinkElements read FBreakAlignment write SetBreakAlignment default [];
    property Visited: Boolean read FVisited write SetVisited;
    property LinkFont: TFont read FLinkFont write SetLinkFont;
    property LinkRect: TRect read FLinkRect;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnVisit: TNotifyEvent read FOnVisit write FOnVisit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property State: TPegtopLinkState read FState;
  published
    property CaptionPrefix: TCaption read FCaptionPrefix write SetCaptionPrefix;
    property CaptionSuffix: TCaption read FCaptionSuffix write SetCaptionSuffix;
    property Cursor: TCursor read FCursor write SetCursor;
    property LinkCursor: TCursor read FLinkCursor write SetLinkCursor;
  end;

  TPegtopLink = class(TPegtopCustomLink)
  protected
    procedure DoVisit; override;
  public
    procedure Visit;
    property LinkRect;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BreakAlignment;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Layout;
    property LinkFont;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Visited;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnStateChange;
    property OnVisit;
  end;

  TPegtopWebLink = class(TPegtopLink)
  private
    FAddress: TCaption;
    procedure SetAddress(Value: TCaption);
  protected
    procedure DoVisit; override;
  public
    constructor Create(AOwner: TComponent); override;
    function OpenURL(const URL: String): Boolean;
  published
    property Address: TCaption read FAddress write SetAddress;
  end;

implementation

uses
  Forms;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomLink
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomLink.Create(AOwner: TComponent);
begin
  FLinkFont := TFont.Create;
  FLinkFont.Color := clBlue;
  FLinkFont.Style := [fsUnderline];
  FLinkFont.OnChange := LinkFontChange;
  FLinkCursor := crHandPoint;
  inherited Create(AOwner);
  FCursor := Cursor;
  Width := 121;
  Height := 13;
end;

destructor TPegtopCustomLink.Destroy;
begin
  inherited;
  FLinkFont.Free;
end;

procedure TPegtopCustomLink.Paint;
  function Max(V1, V2: Integer): Integer;
  begin
    if V1 > V2 then Result := V1 else Result := V2;
  end;
var
  LinkSize: TSize;
  PrefixSize: TSize;
  SuffixSize: TSize;
  LinkMetric: TTextMetric;
  PrefixSuffixMetric: TTextMetric;
  CompleteWidth: Integer;
  MaxHeight, MaxAscent, MaxDescent: Integer;
  X, Y, P: Integer;
begin
  inherited;

  // get size / metric of prefix / suffix:
  Canvas.Font.Assign(Font);
  GetTextMetrics(Canvas.Handle, PrefixSuffixMetric);
  PrefixSize := Canvas.TextExtent(FCaptionPrefix);
  SuffixSize := Canvas.TextExtent(FCaptionSuffix);

  // get size / metric of link:
  Canvas.Font.Assign(FLinkFont);
  GetTextMetrics(Canvas.Handle, LinkMetric);
  LinkSize := Canvas.TextExtent(Caption);
  CompleteWidth := PrefixSize.CX + LinkSize.CX + SuffixSize.CX;

  // get max heights:
  MaxHeight := Max(PrefixSuffixMetric.tmHeight, LinkMetric.tmHeight);
  MaxAscent := Max(PrefixSuffixMetric.tmAscent, LinkMetric.tmAscent);
  MaxDescent := Max(PrefixSuffixMetric.tmDescent, LinkMetric.tmDescent);

  // get text position:
  case FAlignment of
    taLeftJustify:  X := 0;
    taCenter:       X := (ClientWidth - CompleteWidth) div 2;
    taRightJustify: X := ClientWidth - CompleteWidth;
    else            X := 0;
  end;
  case FLayout of
    pllTop:    Y := MaxAscent;
    pllMiddle: Y := (ClientHeight - MaxHeight) div 2 + MaxAscent;
    pllBottom: Y := ClientHeight - MaxDescent;
    else       Y := 0;
  end;

  // get link bounds:
  FLinkRect := Bounds(X + PrefixSize.CX, Y - LinkMetric.tmAscent, LinkSize.CX, LinkSize.CY);

  // draw prefix / suffix:
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);
  Windows.SetTextAlign(Canvas.Handle, TA_LEFT or TA_BASELINE);
  if plePrefix in FBreakAlignment then P := 0 else P := X;
  Windows.ExtTextOut(Canvas.Handle, P, Y, 0, NIL,
    PChar(FCaptionPrefix), Length(FCaptionPrefix), NIL);
  if pleSuffix in FBreakAlignment then P := ClientWidth - SuffixSize.CX else P := X + PrefixSize.CX + LinkSize.CX;
  Windows.ExtTextOut(Canvas.Handle, P, Y, 0, NIL,
    PChar(FCaptionSuffix), Length(FCaptionSuffix), NIL);

  // draw link:
  Canvas.Font.Assign(FLinkFont);
  Windows.ExtTextOut(Canvas.Handle, X + PrefixSize.CX, Y, 0, NIL,
    PChar(Caption), Length(Caption), NIL);
end;

procedure TPegtopCustomLink.DoVisit;
begin
  if Assigned(FOnVisit) then FOnVisit(Self);
end;

procedure TPegtopCustomLink.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  Hover: Boolean;
begin
  Hover := (X >= FLinkRect.Left) and (Y >= FLinkRect.Top) and (X < FLinkRect.Right) and (Y < FLinkRect.Bottom);
  if FLinkHover <> Hover then begin
    FLinkHover := Hover;
    CheckState;
    Invalidate;
  end;
end;

procedure TPegtopCustomLink.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  Hover: Boolean;
begin
  Hover := (X >= FLinkRect.Left) and (Y >= FLinkRect.Top) and (X < FLinkRect.Right) and (Y < FLinkRect.Bottom);
  if (Button = mbLeft) and Hover then begin
    FMouseDown := True;
    CheckState;
  end;
end;

procedure TPegtopCustomLink.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  Hover: Boolean;
begin
  Hover := (X >= FLinkRect.Left) and (Y >= FLinkRect.Top) and (X < FLinkRect.Right) and (Y < FLinkRect.Bottom);
  if Hover and FMouseDown then DoVisit;
  FMouseDown := False;
  CheckState;
end;

procedure TPegtopCustomLink.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TPegtopCustomLink.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TPegtopCustomLink.WMSetText(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopCustomLink.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  DoMouseEnter;
end;

procedure TPegtopCustomLink.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  DoMouseLeave;
  if FLinkHover then begin
    FLinkHover := False;
    CheckState;
    Invalidate;
  end;
end;

procedure TPegtopCustomLink.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopCustomLink.LinkFontChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TPegtopCustomLink.ChangeCursor(const NewCursor: TCursor);
begin
  inherited Cursor := NewCursor;
end;

procedure TPegtopCustomLink.CheckState;
var
  NewState: TPegtopLinkState;
begin
  if FLinkHover and FMouseDown then
    NewState := plsActive
  else if FLinkHover then
    NewState := plsHover
  else if Focused then
    NewState := plsFocus
  else if FVisited then
    NewState := plsVisited
  else
    NewState := plsNormal;
  if FState <> NewState then begin
    FState := NewState;
    if FLinkHover then ChangeCursor(FLinkCursor) else ChangeCursor(FCursor);
    if Assigned(FOnStateChange) then FOnStateChange(Self);
  end;
end;

procedure TPegtopCustomLink.SetCaptionPrefix(Value: TCaption);
begin
  if FCaptionPrefix <> Value then begin
    FCaptionPrefix := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomLink.SetCaptionSuffix(Value: TCaption);
begin
  if FCaptionSuffix <> Value then begin
    FCaptionSuffix := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomLink.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomLink.SetLayout(Value: TPegtopLinkLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomLink.SetBreakAlignment(Value: TPegtopLinkElements);
begin
  if FBreakAlignment <> Value then begin
    FBreakAlignment := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomLink.SetVisited(Value: Boolean);
begin
  if FVisited <> Value then begin
    FVisited := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomLink.SetLinkFont(Value: TFont);
begin
  FLinkFont.Assign(Value);
  // FLinkFont.OnChange triggers Invalidate
end;

procedure TPegtopCustomLink.SetCursor(Value: TCursor);
begin
  if FCursor <> Value then begin
    FCursor := Value;
    if not FLinkHover then ChangeCursor(FCursor);
  end;
end;

procedure TPegtopCustomLink.SetLinkCursor(Value: TCursor);
begin
  if FLinkCursor <> Value then begin
    FLinkCursor := Value;
    if FLinkHover then ChangeCursor(FLinkCursor);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopLink
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopLink.Visit;
begin
  DoVisit;
end;

procedure TPegtopLink.DoVisit;
Begin
  inherited;
  Visited := True;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopWebLink
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopWebLink.Create(AOwner: TComponent);
begin
  inherited;
  Address := 'http://www.pegtop.net';
end;

procedure TPegtopWebLink.DoVisit;
Begin
  inherited;
  if (FAddress <> '') and OpenURL(FAddress) then Visited := True;
end;

function TPegtopWebLink.OpenURL(const URL: String): Boolean;
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Result := ShellExecute(Parent.Handle, NIL, PChar(URL), NIL, NIL, SW_SHOWNORMAL) > 32;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TPegtopWebLink.SetAddress(Value: TCaption);
begin
  if FAddress <> Value then begin
    if csDesigning in ComponentState then begin
      if (Caption = Name) or (Caption = FAddress) then Caption := Value;
    end;
    FAddress := Value;
  end;
end;

end.

