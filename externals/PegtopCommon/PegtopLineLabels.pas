////////////////////////////////////////////////////////////////////////////////
// File:       PegtopLineLabels.pas
// Components: TPegtopLineLabel
// Version:    1.00
// Date:       03 Jul 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopLineLabel is a simple control that displays caption (left aligned),
// an optional annex (right aligned), and a line in-between.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
////////////////////////////////////////////////////////////////////////////////

unit PegtopLineLabels;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls;

type
  TPegtopLabelLineStyle = (pllNone, pllSolid, pllDash, pllDot);

  TPegtopLabelLineEvent = procedure (Sender: TObject; Canvas: TCanvas; X1, X2, Y: Integer) of object;

  TPegtopLineLabel = class(TGraphicControl)
  private
    FAnnex: TCaption;
    FLayout: TTextLayout;
    FLineStyle: TPegtopLabelLineStyle;
    FLineColor: TColor;
    FLinePeriod: Integer;
    FLineSeparation: Integer;
    FOnDrawLine: TPegtopLabelLineEvent;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure SetAnnex(Value: TCaption);
    procedure SetLayout(Value: TTextLayout);
    procedure SetLineStyle(Value: TPegtopLabelLineStyle);
    procedure SetLineColor(Value: TColor);
    procedure SetLinePeriod(Value: Integer);
    procedure SetLineSeparation(Value: Integer);
  protected
    procedure Paint; override;
    procedure PaintLine(X1, X2, Y: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Annex: TCaption read FAnnex write SetAnnex;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property LineStyle: TPegtopLabelLineStyle read FLineStyle write SetLineStyle default pllDot;
    property LineColor: TColor read FLineColor write SetLineColor default clGrayText;
    property LinePeriod: Integer read FLinePeriod write SetLinePeriod default 3;
    property LineSeparation: Integer read FLineSeparation write SetLineSeparation default 2;
    property OnDrawLine: TPegtopLabelLineEvent read FOnDrawLine write FOnDrawLine;
    property Align;
    property Anchors;
    property Constraints;
    property Caption;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TPegtopLineLabel
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopLineLabel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 121;
  Height := 13;
  FLineStyle := pllDot;
  FLineColor := clGrayText;
  FLinePeriod := 3;
  FLineSeparation := 2;
end;

procedure TPegtopLineLabel.Paint;
var
  CaptionSize, AnnexSize: TSize;
  CaptionRect, AnnexRect: TRect;
  CaptionTop: Integer;
  Metric: TTextMetric;
  X1, X2, Y: Integer;
begin
  inherited;

  // get size:
  Canvas.Font.Assign(Font);
  GetTextMetrics(Canvas.Handle, Metric);
  CaptionSize := Canvas.TextExtent(Caption);
  AnnexSize := Canvas.TextExtent(FAnnex);
  if CaptionSize.CX > ClientWidth - AnnexSize.CX - 2 then
    CaptionSize.CX := ClientWidth - AnnexSize.CX - 2;

  // assign rects:
  if FLayout = tlTop then CaptionTop := 0
  else if FLayout = tlBottom then CaptionTop := ClientHeight - CaptionSize.CY
  else CaptionTop := (ClientHeight - CaptionSize.CY) div 2;
  CaptionRect := Rect(0, CaptionTop, CaptionSize.CX, CaptionTop + CaptionSize.CY);
  AnnexRect := Rect(ClientWidth - AnnexSize.CX, CaptionTop, ClientWidth, CaptionTop + AnnexSize.CY);

  // draw:
  Canvas.Brush.Style := bsClear;
  if Caption <> '' then
    Windows.DrawText(Canvas.Handle, PChar(Caption), Length(Caption),
      CaptionRect, DT_LEFT or DT_END_ELLIPSIS or DT_EXPANDTABS or DT_NOCLIP);
  if FAnnex <> '' then
    Windows.DrawText(Canvas.Handle, PChar(FAnnex), Length(FAnnex),
      AnnexRect, DT_LEFT or DT_NOPREFIX or DT_EXPANDTABS or DT_NOCLIP);
  if Caption = '' then X1 := 0
  else X1 := CaptionSize.CX + FLineSeparation;
  if FAnnex = '' then X2 := ClientWidth
  else X2 := ClientWidth - AnnexSize.CX - FLineSeparation;
  if X1 < X2 then begin
    Canvas.Pen.Color := FLineColor;
    Y := CaptionTop + Metric.tmHeight - Metric.tmDescent - 1;
    PaintLine(X1, X2, Y);
  end;
end;

procedure TPegtopLineLabel.PaintLine(X1, X2, Y: Integer);
var
  X: Integer;
begin
  case FLineStyle of
    pllSolid:
      begin
        Canvas.MoveTo(X1, Y);
        Canvas.LineTo(X2, Y);
      end;
    pllDash:
      begin
        X := X2 - FLinePeriod;
        while X >= X1 do begin
          Canvas.MoveTo(X, Y);
          Canvas.LineTo(X + FLinePeriod, Y);
          Dec(X, FLinePeriod * 2);
        end;
      end;
    pllDot:
      begin
        X := X2 - 1;
        while X >= X1 do begin
          Canvas.Pixels[X, Y] := FLineColor;
          Dec(X, FLinePeriod);
        end;
      end;
  end;
  if Assigned(FOnDrawLine) then FOnDrawLine(Self, Canvas, X1, X2, Y);
end;

procedure TPegtopLineLabel.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopLineLabel.SetAnnex(Value: TCaption);
begin
  if FAnnex <> Value then begin
    FAnnex := Value;
    Invalidate;
  end;
end;

procedure TPegtopLineLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TPegtopLineLabel.SetLineStyle(Value: TPegtopLabelLineStyle);
begin
  if FLineStyle <> Value then begin
    FLineStyle := Value;
    Invalidate;
  end;
end;

procedure TPegtopLineLabel.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TPegtopLineLabel.SetLinePeriod(Value: Integer);
begin
  if Value < 2 then Value := 2 else if Value > 40 then Value := 40;
  if FLinePeriod <> Value then begin
    FLinePeriod := Value;
    Invalidate;
  end;
end;

procedure TPegtopLineLabel.SetLineSeparation(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 10 then Value := 10;
  if FLineSeparation <> Value then begin
    FLineSeparation := Value;
    Invalidate;
  end;
end;

end.

