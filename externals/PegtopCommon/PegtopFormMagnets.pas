////////////////////////////////////////////////////////////////////////////////
// File:       PegtopFormMagnets.pas
// Classes:    TPegtopFormMagnet, TPegtopMagnetForm
// Version:    1.00
// Date:       16 Jun 2004
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopFormMagnet can be placed onto a form to make it stick to other forms.
// Allies can be set to make it stick to the screen bounds, the work area
// (screen without task bar etc.), other forms and/or some controls you can
// define.
// TPegtopMagnetForm can be used as base for forms with the same behaviour
// without having to use TPegtopFormMagnet (less overhead).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopFormMagnets;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  PegtopWindowHooks;

type
  TPegtopMagnetMode = (pmmOuterBounds, pmmInnerBounds);
  TPegtopMagnetAlly = (pmaScreenBounds, pmaWorkArea, pmaScreenForms, pmaAllyControls, pmaGlobalAllyControls);
  TPegtopMagnetAllies = set of TPegtopMagnetAlly;

  TPegtopFormMagnet = class(TPegtopCustomFormHook)
  private
    FAllies: TPegtopMagnetAllies;
    FAllyControls: TList;
  protected
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Magnet(var X, Y: Integer; W, H: Integer);
    procedure AddAllyControl(Control: TControl; Mode: TPegtopMagnetMode);
    procedure RemoveAllyControl(Control: TControl);
    procedure ClearAllyControls;
  published
    property Allies: TPegtopMagnetAllies read FAllies write FAllies;
    property Active;
  end;

  TPegtopMagnetForm = class(TForm)
  private
    FAllies: TPegtopMagnetAllies;
    FAllyControls: TList;
    procedure WMWindowPosChanging (var Msg: TWMWindowPosMsg); message WM_WINDOWPOSCHANGING;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Magnet(var X, Y: Integer; W, H: Integer);
    procedure AddMagnetAllyControl(Control: TControl; Mode: TPegtopMagnetMode);
    procedure RemoveMagnetAllyControl(Control: TControl);
    procedure ClearMagnetAllyControls;
  published
    property MagnetAllies: TPegtopMagnetAllies read FAllies write FAllies;
  end;

var
  MagnetDistance: Integer;

procedure AddGlobalMagnetAllyControl(Control: TControl; Mode: TPegtopMagnetMode);
procedure RemoveGlobalMagnetAllyControl(Control: TControl);
procedure ClearGlobalMagnetAllyControls;

implementation

type
  TPegtopMagnetAllyItem = class
  private
    FControl: TControl;
    FMode: TPegtopMagnetMode;
  public
    constructor Create(AControl: TControl; AMode: TPegtopMagnetMode);
    property Control: TControl read FControl;
    property Mode: TPegtopMagnetMode read FMode;
  end;

var
  GlobalAllyControls: TList;

var
  WorkAreaValid: Boolean;
  WorkAreaRect: TRect;

////////////////////////////////////////////////////////////////////////////////
// procedural functions
////////////////////////////////////////////////////////////////////////////////

procedure AddGlobalMagnetAllyControl(Control: TControl; Mode: TPegtopMagnetMode);
begin
  GlobalAllyControls.Add(TPegtopMagnetAllyItem.Create(Control, Mode));
end;

procedure RemoveGlobalMagnetAllyControl(Control: TControl);
var
  I: Integer;
begin
  for I := GlobalAllyControls.Count-1 downto 0 do begin
    if TPegtopMagnetAllyItem(GlobalAllyControls[I]).Control = Control then begin
      TPegtopMagnetAllyItem(GlobalAllyControls[I]).Free;
      GlobalAllyControls.Delete(I);
    end;
  end;
end;

procedure ClearGlobalMagnetAllyControls;
var
  I: Integer;
begin
  for I := 0 to GlobalAllyControls.Count-1 do
    TPegtopMagnetAllyItem(GlobalAllyControls[I]).Free;
  GlobalAllyControls.Clear;
end;

procedure MagnetHorizontal(var X, Y: Integer; W, H: Integer; L, T, R, B: Integer; OrgX, OrgY: Integer; var DistH, DistV: Integer);
var
  D: Integer;
begin
  if (OrgY+H >= T) and (OrgY <= B) then begin
    // check against right border:
    D := Abs(OrgX-R);
    if D < DistH then begin
      X := R;
      DistH := D;
      // check against top border:
      D := Abs(OrgY-T);
      if D < DistV then begin
        Y := T;
        DistV := D;
      end;
      // check against bottom border:
      D := Abs((OrgY+H)-B);
      if D < DistV then begin
        Y := B - H;
        DistV := D;
      end;
    end;
    // check against left border:
    D := Abs((OrgX+W)-L);
    if D < DistH then begin
      X := L - W;
      DistH := D;
      // check against top border:
      D := Abs(OrgY-T);
      if D < DistV then begin
        Y := T;
        DistV := D;
      end;
      // check against bottom border:
      D := Abs((OrgY+H)-B);
      if D < DistV then begin
        Y := B - H;
        DistV := D;
      end;
    end;
  end;
end;

procedure MagnetVertical(var X, Y: Integer; W, H: Integer; L, T, R, B: Integer; OrgX, OrgY: Integer; var DistH, DistV: Integer);
var
  D: Integer;
begin
  if (OrgX+W >= L) and (OrgX <= R) then begin
    // check against bottom border:
    D := Abs(OrgY-B);
    if D < DistV then begin
      Y := B;
      DistV := D;
      // check against left border:
      D := Abs(OrgX-L);
      if D < DistH then begin
        X := L;
        DistH := D;
      end;
      // check against right border:
      D := Abs((OrgX+W)-R);
      if D < DistH then begin
        X := R - W;
        DistH := D;
      end;
    end;
    // check against top border:
    D := Abs((OrgY+H)-T);
    if D < DistV then begin
      Y := T - H;
      DistV := D;
      // check against left border:
      D := Abs(OrgX-L);
      if D < DistH then begin
        X := L;
        DistH := D;
      end;
      // check against right border:
      D := Abs((OrgX+W)-R);
      if D < DistH then begin
        X := R - W;
        DistH := D;
      end;
    end;
  end;
end;

procedure MagnetRect(var X, Y: Integer; W, H: Integer; AllyRect: TRect; Mode: TPegtopMagnetMode; OrgX, OrgY: Integer; var DistH, DistV: Integer);
begin
  if Mode = pmmInnerBounds then begin
    MagnetHorizontal(X, Y, W, H, AllyRect.Right, AllyRect.Top, AllyRect.Left, AllyRect.Bottom, OrgX, OrgY, DistH, DistV);
    MagnetVertical(X, Y, W, H, AllyRect.Left, AllyRect.Bottom, AllyRect.Right, AllyRect.Top, OrgX, OrgY, DistH, DistV);
  end
  else begin
    MagnetHorizontal(X, Y, W, H, AllyRect.Left, AllyRect.Top, AllyRect.Right, AllyRect.Bottom, OrgX, OrgY, DistH, DistV);
    MagnetVertical(X, Y, W, H, AllyRect.Left, AllyRect.Top, AllyRect.Right, AllyRect.Bottom, OrgX, OrgY, DistH, DistV);
  end;
end;

procedure MagnetControl(var X, Y: Integer; W, H: Integer; AllyControl: TControl; Mode: TPegtopMagnetMode; OrgX, OrgY: Integer; var DistH, DistV: Integer);
var
  AllyRect: TRect;
begin
  if (AllyControl <> NIL) and AllyControl.Visible then begin
    if AllyControl.Parent <> NIL then
      AllyRect.TopLeft := AllyControl.Parent.ClientToScreen(Point(AllyControl.Left, AllyControl.Top))
    else
      AllyRect.TopLeft := Point(AllyControl.Left, AllyControl.Top);
    AllyRect.Right := AllyRect.Left + AllyControl.Width;
    AllyRect.Bottom := AllyRect.Top + AllyControl.Height;
    MagnetRect(X, Y, W, H, AllyRect, Mode, OrgX, OrgY, DistH, DistV);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopMagnetAllyItem
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopMagnetAllyItem.Create(AControl: TControl; AMode: TPegtopMagnetMode);
begin
  FControl := AControl;
  FMode := AMode;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopFormMagnet
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopFormMagnet.Create(AOwner: TComponent);
begin
  if CountInstances(AOwner) > 0 then raise Exception.Create('Only one PegtopFormMagnet instance allowed on each form.');
  FAllies := [pmaWorkArea, pmaScreenForms, pmaAllyControls, pmaGlobalAllyControls];
  FAllyControls := TList.Create;
  inherited;
end;

destructor TPegtopFormMagnet.Destroy;
begin
  inherited;
  if FAllyControls <> NIL then ClearAllyControls;
  FAllyControls.Free;
end;

procedure TPegtopFormMagnet.HandleMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if (Msg.Msg = WM_WINDOWPOSCHANGING)
  and Form.Visible and (Form.WindowState <> wsMaximized) then begin
    if (TWMWindowPosMsg(Msg).WindowPos.Flags and SWP_NOMOVE) = 0 then begin // form is moved
      if ((TWMWindowPosMsg(Msg).WindowPos.X <> Form.Left)
      or (TWMWindowPosMsg(Msg).WindowPos.Y <> Form.Top))
      and (TWMWindowPosMsg(Msg).WindowPos.CX = Form.Width)
      and (TWMWindowPosMsg(Msg).WindowPos.CY = Form.Height) then begin // form is not resized
        Magnet(TWMWindowPosMsg(Msg).WindowPos.X, TWMWindowPosMsg(Msg).WindowPos.Y,
          Form.Width, Form.Height);
      end;
    end;
    WorkAreaValid := ((TWMWindowPosMsg(Msg).WindowPos.Flags and (SWP_NOMOVE or SWP_NOSIZE)) = 0); // if form was (probably) dropped => get workarea again
  end;
end;

procedure TPegtopFormMagnet.Magnet(var X, Y: Integer; W, H: Integer);
var
  I: Integer;
  Item: TPegtopMagnetAllyItem;
  OrgX, OrgY: Integer;
  DistH, DistV: Integer;
begin
  OrgX := X;
  OrgY := Y;
  DistH := MagnetDistance;
  DistV := MagnetDistance;
  if pmaScreenBounds in FAllies then begin // check screen bounds
    MagnetRect(X, Y, W, H, Rect(0, 0, Screen.Width, Screen.Height), pmmInnerBounds, OrgX, OrgY, DistH, DistV);
  end;
  if pmaWorkArea in FAllies then begin // check bounds of workarea
    if not WorkAreaValid then begin // workarea might have changed since last call
      SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkAreaRect, 0);
    end;
    MagnetRect(X, Y, W, H, WorkAreaRect, pmmInnerBounds, OrgX, OrgY, DistH, DistV);
  end;
  if pmaScreenForms in FAllies then begin // check scren forms
    for I := 0 To Screen.FormCount-1 do begin
      if Screen.Forms[I] <> Form then
        MagnetControl(X, Y, W, H, Screen.Forms[I], pmmOuterBounds, OrgX, OrgY, DistH, DistV);
    end;
  end;
  if pmaAllyControls in FAllies then begin // check forms / controls of list
    for I := 0 To FAllyControls.Count-1 do begin
      Item := TPegtopMagnetAllyItem(FAllyControls[I]);
      if Item.Control <> Form then
        MagnetControl(X, Y, W, H, Item.Control, Item.Mode, OrgX, OrgY, DistH, DistV);
    end;
  end;
  if pmaGlobalAllyControls in FAllies then begin // check forms / controls of global list
    for I := 0 To GlobalAllyControls.Count-1 do begin
      Item := TPegtopMagnetAllyItem(GlobalAllyControls[I]);
      if Item.Control <> Form then
        MagnetControl(X, Y, W, H, Item.Control, Item.Mode, OrgX, OrgY, DistH, DistV);
    end;
  end;
end;

procedure TPegtopFormMagnet.AddAllyControl(Control: TControl; Mode: TPegtopMagnetMode);
begin
  FAllyControls.Add(TPegtopMagnetAllyItem.Create(Control, Mode));
end;

procedure TPegtopFormMagnet.RemoveAllyControl(Control: TControl);
var
  I: Integer;
begin
  for I := FAllyControls.Count-1 downto 0 do begin
    if TPegtopMagnetAllyItem(FAllyControls[I]).Control = Control then begin
      TPegtopMagnetAllyItem(FAllyControls[I]).Free;
      FAllyControls.Delete(I);
    end;
  end;
end;

procedure TPegtopFormMagnet.ClearAllyControls;
var
  I: Integer;
begin
  for I := 0 to FAllyControls.Count-1 do
    TPegtopMagnetAllyItem(FAllyControls[I]).Free;
  FAllyControls.Clear;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopMagnetForm
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopMagnetForm.Create(AOwner: TComponent);
begin
  FAllies := [pmaWorkArea, pmaScreenForms, pmaAllyControls, pmaGlobalAllyControls];
  FAllyControls := TList.Create;
  inherited;
end;

destructor TPegtopMagnetForm.Destroy;
begin
  inherited;
  ClearMagnetAllyControls;
  FAllyControls.Free;
end;

procedure TPegtopMagnetForm.WMWindowPosChanging(var Msg: TWMWindowPosMsg);
begin
  if Visible and (WindowState <> wsMaximized) then begin
    if (Msg.WindowPos.Flags and SWP_NOMOVE) = 0 then begin // form is moved
      if ((Msg.WindowPos.X <> Left) or (Msg.WindowPos.Y <> Top))
      and (Msg.WindowPos.CX = Width) and (Msg.WindowPos.CY = Height) then begin // form is not resized
        Magnet(Msg.WindowPos.X, Msg.WindowPos.Y, Width, Height);
      end;
    end;
    WorkAreaValid := ((Msg.WindowPos.Flags and (SWP_NOMOVE OR SWP_NOSIZE)) = 0); // if form was (probably) dropped => get workarea again
  end;
  inherited;
end;

procedure TPegtopMagnetForm.Magnet(var X, Y: Integer; W, H: Integer);
var
  I: Integer;
  Item: TPegtopMagnetAllyItem;
  OrgX, OrgY: Integer;
  DistH, DistV: Integer;
begin
  OrgX := X;
  OrgY := Y;
  DistH := MagnetDistance;
  DistV := MagnetDistance;
  if pmaScreenBounds in FAllies then begin // check screen bounds
    MagnetRect(X, Y, W, H, Rect(0, 0, Screen.Width, Screen.Height), pmmInnerBounds, OrgX, OrgY, DistH, DistV);
  end;
  if pmaWorkArea in FAllies then begin // check bounds of workarea
    if not WorkAreaValid then begin // workarea might have changed since last call
      SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkAreaRect, 0);
    end;
    MagnetRect(X, Y, W, H, WorkAreaRect, pmmInnerBounds, OrgX, OrgY, DistH, DistV);
  end;
  if pmaScreenForms in FAllies then begin // check scren forms
    for I := 0 To Screen.FormCount-1 do begin
      if Screen.Forms[I] <> Self then
        MagnetControl(X, Y, W, H, Screen.Forms[I], pmmOuterBounds, OrgX, OrgY, DistH, DistV);
    end;
  end;
  if pmaAllyControls in FAllies then begin // check forms / controls of list
    for I := 0 To FAllyControls.Count-1 do begin
      Item := TPegtopMagnetAllyItem(FAllyControls[I]);
      if Item.Control <> Self then
        MagnetControl(X, Y, W, H, Item.Control, Item.Mode, OrgX, OrgY, DistH, DistV);
    end;
  end;
  if pmaGlobalAllyControls in FAllies then begin // check forms / controls of global list
    for I := 0 To GlobalAllyControls.Count-1 do begin
      Item := TPegtopMagnetAllyItem(GlobalAllyControls[I]);
      if Item.Control <> Self then
        MagnetControl(X, Y, W, H, Item.Control, Item.Mode, OrgX, OrgY, DistH, DistV);
    end;
  end;
end;

procedure TPegtopMagnetForm.AddMagnetAllyControl(Control: TControl; Mode: TPegtopMagnetMode);
begin
  FAllyControls.Add(TPegtopMagnetAllyItem.Create(Control, Mode));
end;

procedure TPegtopMagnetForm.RemoveMagnetAllyControl(Control: TControl);
var
  I: Integer;
begin
  for I := FAllyControls.Count-1 downto 0 do begin
    if TPegtopMagnetAllyItem(FAllyControls[I]).Control = Control then begin
      TPegtopMagnetAllyItem(FAllyControls[I]).Free;
      FAllyControls.Delete(I);
    end;
  end;
end;

procedure TPegtopMagnetForm.ClearMagnetAllyControls;
var
  I: Integer;
begin
  for I := 0 to FAllyControls.Count-1 do
    TPegtopMagnetAllyItem(FAllyControls[I]).Free;
  FAllyControls.Clear;
end;

initialization
  WorkAreaValid := False;
  MagnetDistance := 8;
  GlobalAllyControls := TList.Create;
finalization
  ClearGlobalMagnetAllyControls;
  GlobalAllyControls.Free;
end.
