////////////////////////////////////////////////////////////////////////////////
// File:       PegtopPanels.pas
// Classes:    TPegtopPanel, TPegtopGradientPanel
// Version:    1.01
// Date:       16 Jun 2004 created 1.00
//             07 Jul 2005 modified 1.01 (OnDefineVector event added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopPanel is a panel, which can be transparent or filled with a custom
// background (unlike OnPaint, OnBackground can be used to paint the "real"
// background of the panel, which is neccessary to make some XP themed controls
// look good).
// TPegtopGradientPanel is a panel with a color gradient background.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopPanels;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls,
  PegtopColorGradients;

type
  TPegtopPaintBackgroundEvent = procedure(Sender: TObject; DC: THandle; var Handled: Boolean) of object;

  TPegtopCustomPanel = class(TCustomPanel)
  private
    procedure WMEraseBkGnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintBackground(const DC: THandle; var Handled: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPegtopPanel = class(TPegtopCustomPanel)
  private
    FOnPaint: TNotifyEvent;
    FOnPaintBackground: TPegtopPaintBackgroundEvent;
    procedure SetOnPaint(Value: TNotifyEvent);
    procedure SetOnPaintBackground(Value: TPegtopPaintBackgroundEvent);
  protected
    procedure PaintBackground(const DC: THandle; var Handled: Boolean); override;
    procedure Paint; override;
  published
    property OnPaint: TNotifyEvent read FOnPaint write SetOnPaint;
    property OnPaintBackground: TPegtopPaintBackgroundEvent read FOnPaintBackground write SetOnPaintBackground;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderStyle;
    property Canvas;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TPegtopVectorEvent = procedure (Sender: TObject; var P1, P2: TPoint) of object;

  TPegtopGradientPanel = class(TPegtopCustomPanel)
  private
    FGradient: TPegtopCustomColorGradient;
    FGradientStyle: TPegtopColorGradientStyle;
    FGradientOptions: TPegtopColorGradientOptions;
    FOnPaint: TNotifyEvent;
    FOnDefineVector: TPegtopVectorEvent;
    procedure GradientChange(Sender: TObject);
    procedure SetGradient(Value: TPegtopCustomColorGradient);
    procedure SetGradientStyle(Value: TPegtopColorGradientStyle);
    procedure SetGradientOptions(Value: TPegtopColorGradientOptions);
    procedure SetOnPaint(Value: TNotifyEvent);
  protected
    procedure PaintBackground(const DC: THandle; var Handled: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Gradient: TPegtopCustomColorGradient read FGradient write SetGradient;
    property GradientStyle: TPegtopColorGradientStyle read FGradientStyle write SetGradientStyle;
    property GradientOptions: TPegtopColorGradientOptions read FGradientOptions write SetGradientOptions;
    property OnPaint: TNotifyEvent read FOnPaint write SetOnPaint;
    property OnDefineVector: TPegtopVectorEvent read FOnDefineVector write FOnDefineVector;
    property Align;
    property Anchors;
    property AutoSize;
    property Canvas;
    property Constraints;
    property DoubleBuffered;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  Graphics;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomPanel
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csOpaque];
end;

procedure TPegtopCustomPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // Params.Style := Params.Style and not WS_CLIPCHILDREN;
  // Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TPegtopCustomPanel.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
var
  Handled: Boolean;
begin
  Handled := False;
  if not (csDestroying in ComponentState) then PaintBackground(Msg.DC, Handled);
  if Handled then Msg.Result := 1
  else inherited;
  // DefWindowProc(Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TPegtopCustomPanel.PaintBackground(const DC: THandle; var Handled: Boolean);
begin
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopPanel
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopPanel.PaintBackground(const DC: THandle; var Handled: Boolean);
begin
  if Assigned(FOnPaintBackground) then FOnPaintBackground(Self, DC, Handled);
end;

procedure TPegtopPanel.Paint;
begin
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TPegtopPanel.SetOnPaint(Value: TNotifyEvent);
begin
  if (TMethod(FOnPaint).Code <> TMethod(Value).Code)
  or (TMethod(FOnPaint).Data <> TMethod(Value).Data) then begin
    FOnPaint := Value;
    Invalidate;
  end;
end;

procedure TPegtopPanel.SetOnPaintBackground(Value: TPegtopPaintBackgroundEvent);
begin
  if (TMethod(FOnPaintBackground).Code <> TMethod(Value).Code)
  or (TMethod(FOnPaintBackground).Data <> TMethod(Value).Data) then begin
    FOnPaintBackground := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopGradientPanel
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopGradientPanel.Create(AOwner: TComponent);
begin
  FGradient := TPegtopColorGradient.Create([clBtnHighlight, clBtnFace, clBtnShadow]);
  TPegtopColorGradient(FGradient).OnChange := GradientChange;
  inherited;
end;

destructor TPegtopGradientPanel.Destroy;
begin
  FGradient.Free;
  inherited;
end;

procedure TPegtopGradientPanel.PaintBackground(const DC: THandle; var Handled: Boolean);
var
  Canvas: TCanvas;
  P1, P2: TPoint;
begin
  P1 := Point(0, 0);
  P2 := Point(ClientWidth - 1, ClientHeight - 1);
  if Assigned(FOnDefineVector) then FOnDefineVector(Self, P1, P2);
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    FGradient.Draw(Canvas, ClientRect, P1, P2,
      FGradientStyle, False, FGradientOptions);
  finally
    Canvas.Free;
  end;
  Handled := True;
end;

procedure TPegtopGradientPanel.Paint;
begin
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TPegtopGradientPanel.GradientChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TPegtopGradientPanel.SetGradient(Value: TPegtopCustomColorGradient);
begin
  FGradient.Assign(Value);
  Invalidate;
end;

procedure TPegtopGradientPanel.SetGradientStyle(Value: TPegtopColorGradientStyle);
begin
  if FGradientStyle <> Value then begin
    FGradientStyle := Value;
    Invalidate;
  end;
end;

procedure TPegtopGradientPanel.SetGradientOptions(Value: TPegtopColorGradientOptions);
begin
  if FGradientOptions <> Value then begin
    FGradientOptions := Value;
    Invalidate;
  end;
end;

procedure TPegtopGradientPanel.SetOnPaint(Value: TNotifyEvent);
begin
  if (TMethod(FOnPaint).Code <> TMethod(Value).Code)
  or (TMethod(FOnPaint).Data <> TMethod(Value).Data) then begin
    FOnPaint := Value;
    Invalidate;
  end;
end;

end.
