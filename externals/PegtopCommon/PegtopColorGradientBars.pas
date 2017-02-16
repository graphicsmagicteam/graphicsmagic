////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorGradientBars.pas
// Classes:    TPegtopCustomGradientTrackBar, TPegtopColorGradientTrackBar,
//             TPegtopOpacityGradientTrackBar
// Version:    1.02
// History:    1.00 09 Sep 2004 created
//             1.01 19 Jan 2005 OnDrawTrack event handler added
//             1.02 13 Nov 2005 Open / save menu items added
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
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

unit PegtopColorGradientBars;

interface

uses
  Windows, Classes, Messages, Graphics, Forms, Controls, PegtopThemes,
  PegtopColorGradients, PegtopTrackBars, PegtopColorControls;

const
  WM_PEGTOPGRADIENTBAR_EDITKEY = WM_USER + $F;

type
  TPegtopCustomGradientBar = class(TPegtopSlideBar)
  private
    FInternalGradient: TPegtopCustomColorGradient;
    FGradient: TPegtopCustomColorGradient;
    FLook: TPegtopColorControlLook;
    FBarEnabled: Boolean;
    FMinCount: Integer;
    FFocusKey: TPegtopGradientKey;
    FHoverKey: TPegtopGradientKey;
    FRemoveKey: TPegtopGradientKey;
    FOnGradientChange: TNotifyEvent;
    FOnFocusKeyChange: TNotifyEvent;
    FOnFocusKeyModify: TNotifyEvent;
    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure GradientChange(Sender: TObject);
    procedure SetGradient(Value: TPegtopCustomColorGradient);
    procedure SetGradientHook(Value: TPegtopCustomColorGradient);
    procedure SetLook(Value: TPegtopColorControlLook);
    procedure SetBarEnabled(Value: Boolean);
    procedure SetMinCount(Value: Integer);
  protected
    function TrackEnabled: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
    procedure ResetFocus;
    function DoStartScroll(const X, Y: Integer): TPoint; override;
    procedure DoScroll(const X, Y: Integer); override;
    procedure DoEndScroll; override;
    procedure ExtraMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ApplyPosition(const P: Integer); override;
    procedure ChangePosition(const Delta: Integer); override;
    function AddKey: TPegtopGradientKey; virtual; abstract;
    procedure RemoveKey(Key: TPegtopGradientKey); virtual; abstract;
    function GetKeyAtPosition(X, Y: Integer): TPegtopGradientKey;
    function GetCount: Integer; virtual; abstract;
    function GetKey(Index: Integer): TPegtopGradientKey; virtual; abstract;
    function GetKeyIndex(Key: TPegtopGradientKey): Integer; virtual; abstract;
    function GetGradientRect: TRect;
    function GetLinePoint: TPoint; override;
    function GetButtonSize: TSize; override;
    function GetButtonPoint(Key: TPegtopGradientKey): TPoint;
    function PointToPosition(const X, Y: Integer): Integer;
    property Gradient: TPegtopCustomColorGradient read FGradient write SetGradient;
    property GradientHook: TPegtopCustomColorGradient read FGradient write SetGradientHook;
    property Keys[Index: Integer]: TPegtopGradientKey read GetKey;
    property Look: TPegtopColorControlLook read FLook write SetLook default pclRoundedRect;
    property BarEnabled: Boolean read FBarEnabled write SetBarEnabled default True;
    property MinCount: Integer read FMinCount write SetMinCount default 2;
    property OnGradientChange: TNotifyEvent read FOnGradientChange write FOnGradientChange;
    property OnFocusKeyChange: TNotifyEvent read FOnFocusKeyChange write FOnFocusKeyChange;
    property OnFocusKeyModify: TNotifyEvent read FOnFocusKeyModify write FOnFocusKeyModify;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    procedure EditFocusKey;
    procedure ShowContextMenu;
  end;

  TPegtopColorGradientBar = class(TPegtopCustomGradientBar)
  private
    FRemovedColor: TColor;
    procedure WMEditKey(var Msg: TMessage); message WM_PEGTOPGRADIENTBAR_EDITKEY;
    procedure DialogPreview(Sender: TObject; Color: TColor);
    function GetFocusKey: TPegtopColorKey;
  protected
    procedure PaintTo(const ACanvas: TCanvas); override;
    procedure PopupContextMenu(const X, Y: Integer); override;
    procedure ValidatePosition; override;
    function AddKey: TPegtopGradientKey; override;
    procedure RemoveKey(Key: TPegtopGradientKey); override;
    function GetCount: Integer; override;
    function GetKey(Index: Integer): TPegtopGradientKey; override;
    function GetKeyIndex(Key: TPegtopGradientKey): Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property GradientHook;
    property FocusKey: TPegtopColorKey read GetFocusKey;
  published
    property Orientation;
    property Enabled;
    property Visible;
    property Gradient;
    property SmallChange;
    property Look;
    property BarEnabled;
    property MinCount;
    property OnChange;
    property OnScroll;
    property OnGradientChange;
    property OnFocusKeyChange;
    property OnFocusKeyModify;
    property OnDrawTrack;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
  end;

  TPegtopOpacityGradientBar = class(TPegtopCustomGradientBar)
  private
    FRemovedOpacity: Integer;
    procedure WMEditKey(var Msg: TMessage); message WM_PEGTOPGRADIENTBAR_EDITKEY;
    function GetFocusKey: TPegtopOpacityKey;
  protected
    procedure PaintTo(const ACanvas: TCanvas); override;
    procedure PopupContextMenu(const X, Y: Integer); override;
    procedure ValidatePosition; override;
    function AddKey: TPegtopGradientKey; override;
    procedure RemoveKey(Key: TPegtopGradientKey); override;
    function GetCount: Integer; override;
    function GetKey(Index: Integer): TPegtopGradientKey; override;
    function GetKeyIndex(Key: TPegtopGradientKey): Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property GradientHook;
    property FocusKey: TPegtopOpacityKey read GetFocusKey;
  published
    property Orientation;
    property Enabled;
    property Visible;
    property Gradient;
    property SmallChange;
    property Look;
    property BarEnabled;
    property MinCount;
    property OnChange;
    property OnScroll;
    property OnGradientChange;
    property OnFocusKeyChange;
    property OnFocusKeyModify;
    property OnDrawTrack;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
  end;

implementation

uses
  Menus, Dialogs,
  PegtopColorDialogs, PegtopColorUtils, PegtopColorServices,
  PegtopColorGradientFileDialogs;

type
  TPegtopGradientPopupMenu = class(TPopupMenu)
  private
    procedure MenuItemClick(Sender: TObject);
    procedure MenuItemMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
  protected
    function ChangeGradient(GradientBar: TPegtopCustomGradientBar;
      Gradient: TPegtopCustomColorGradient; Command: Integer): Boolean; virtual;
    procedure DoCommand(GradientBar: TPegtopCustomGradientBar; Command: Integer); virtual;
    procedure MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean); virtual;
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPegtopColorGradientPopupMenu = class(TPegtopGradientPopupMenu)
  protected
    function ChangeGradient(GradientBar: TPegtopCustomGradientBar;
      Gradient: TPegtopCustomColorGradient; Command: Integer): Boolean; override;
    procedure MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean); override;
  end;

  TPegtopOpacityGradientPopupMenu = class(TPegtopGradientPopupMenu)
  protected
    function ChangeGradient(GradientBar: TPegtopCustomGradientBar;
      Gradient: TPegtopCustomColorGradient; Command: Integer): Boolean; override;
    procedure MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean); override;
  end;

var
  ColorGradientPopupMenu: TPegtopColorGradientPopupMenu;
  OpacityGradientPopupMenu: TPegtopOpacityGradientPopupMenu;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomGradientBar
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomGradientBar.Create(AOwner: TComponent);
begin
  inherited;
  FInternalGradient := TPegtopColorGradient.Create([clBlack, clWhite]);
  TPegtopColorGradient(FInternalGradient).OnChange := GradientChange;
  FGradient := FInternalGradient;
  Min := 0;
  Max := PegtopColorGradientPositionRange;
  FLook := pclRoundedRect;
  FMinCount := 2;
  FBarEnabled := True;
  ResetFocus;
end;

destructor TPegtopCustomGradientBar.Destroy;
begin
  if (FGradient <> FInternalGradient) then FGradient.RemoveListener(GradientChange);
  FInternalGradient.Free;
  inherited;
end;

function TPegtopCustomGradientBar.CanFocus: Boolean;
begin
  Result := inherited CanFocus and FBarEnabled;
end;

function TPegtopCustomGradientBar.TrackEnabled: Boolean;
begin
  Result := inherited TrackEnabled and FBarEnabled;
end;

procedure TPegtopCustomGradientBar.EditFocusKey;
begin
  PostMessage(Handle, WM_PEGTOPGRADIENTBAR_EDITKEY, 0, 0);
end;

procedure TPegtopCustomGradientBar.CMWantSpecialKey(var Msg: TWMKey);
begin
  case Msg.CharCode of
    VK_LEFT, VK_RIGHT:
      if Orientation = psoVertical then Msg.Result := 1 else inherited;
    VK_UP, VK_DOWN:
      if Orientation = psoHorizontal then Msg.Result := 1 else inherited;
    else
      inherited;
  end;
end;

procedure TPegtopCustomGradientBar.WMContextMenu(var Msg: TWMContextMenu);
begin
  if (Msg.XPos = -1) and (Msg.YPos = -1) then
    ShowContextMenu
  else
    PopupContextMenu(Msg.XPos, Msg.YPos);
  // inherited is not called because we already did popup the context menu
end;

procedure TPegtopCustomGradientBar.ShowContextMenu;
var
  P: TPoint;
begin
  P := ClientToScreen(Point(0, Height));
  PopupContextMenu(P.X, P.Y);
end;

procedure TPegtopCustomGradientBar.DblClick;
begin
  if Enabled and FBarEnabled then EditFocusKey;
  inherited;
end;

procedure TPegtopCustomGradientBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Enabled and FBarEnabled then begin
    if (Key = VK_SPACE) then begin
      EditFocusKey;
    end
    else if ((Key = VK_RIGHT) and (Orientation = psoVertical))
    or ((Key = VK_DOWN) and (Orientation = psoHorizontal)) then begin
      if GetCount > 0 then begin
        if FFocusKey = NIL then
          FFocusKey := Keys[0]
        else
          FFocusKey := Keys[(GetKeyIndex(FFocusKey) + 1) mod GetCount];
        Invalidate;
        if Assigned(FOnFocusKeyChange) then FOnFocusKeyChange(Self);
      end;
    end
    else if ((Key = VK_LEFT) and (Orientation = psoVertical))
    or ((Key = VK_UP) and (Orientation = psoHorizontal)) then begin
      if GetCount > 0 then begin
        if FFocusKey = NIL then
          FFocusKey := Keys[GetCount - 1]
        else
          FFocusKey := Keys[(GetKeyIndex(FFocusKey) + GetCount - 1) mod GetCount];
        Invalidate;
        if Assigned(FOnFocusKeyChange) then FOnFocusKeyChange(Self);
      end;
    end;
  end;
  inherited;
end;

procedure TPegtopCustomGradientBar.ResetFocus;
begin
  if (FFocusKey = NIL) and (GetCount > 0) then begin
    FFocusKey := Keys[0];
  end;
  if Assigned(FOnFocusKeyChange) then FOnFocusKeyChange(Self);
end;

function TPegtopCustomGradientBar.DoStartScroll(const X, Y: Integer): TPoint;
var
  ButtonSize: TSize;
  ButtonPoint: TPoint;
  NewPos: Integer;
  Key: TPegtopGradientKey;
begin
  if Enabled and FBarEnabled then begin
    ButtonSize := GetButtonSize;
    Key := GetKeyAtPosition(X, Y);
    if Key = NIL then begin
      FGradient.BeginUpdate;
      try
        Key := AddKey;
        Key.Position := PointToPosition(X, Y);
      finally
        FGradient.EndUpdate;
      end;
    end;
    if FFocusKey <> Key then begin
      FFocusKey := Key;
      if Assigned(FOnFocusKeyChange) then FOnFocusKeyChange(Self);
    end;
    if FFocusKey <> NIL then begin
      ButtonPoint := GetButtonPoint(FFocusKey);
      if (X < ButtonPoint.X) or (Y < ButtonPoint.Y)
      or (X >= ButtonPoint.X + ButtonSize.CX) or (Y >= ButtonPoint.Y + ButtonSize.CY) then begin
        // clicked outside the button
        ButtonPoint.X := X - (ButtonSize.CX div 2);
        ButtonPoint.Y := Y - (ButtonSize.CY div 2);
        NewPos := PointToPosition(ButtonPoint.X, ButtonPoint.Y);
        if NewPos <> FFocusKey.Position then begin
          FFocusKey.Position := NewPos;
          Invalidate;
          if Assigned(OnScroll) then begin
            OnScroll(Self, pscTrack, NewPos);
            FFocusKey.Position := NewPos;
          end;
          if Assigned(OnChange) then OnChange(Self);
        end;
        ButtonPoint := GetButtonPoint(FFocusKey);
      end;
      Result.X := X - ButtonPoint.X;
      Result.Y := Y - ButtonPoint.Y;
    end
    else begin
      Result.X := X;
      Result.Y := Y;
    end;
  end
  else begin
    Result.X := X;
    Result.Y := Y;
  end;
end;

procedure TPegtopCustomGradientBar.DoScroll(const X, Y: Integer);
var
  NewPos: Integer;
  P: TPoint;
begin
  if Enabled and FBarEnabled and (FFocusKey <> NIL) then begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if (GetCount > FMinCount)
    and (((Orientation = psoHorizontal) and ((P.Y < -8) or (P.Y >= ClientHeight + 8)))
    or ((Orientation = psoVertical) and ((P.X < -8) or (P.X >= ClientWidth + 8)))) then
      FRemoveKey := FFocusKey
    else
      FRemoveKey := NIL;
    FFocusKey.Enabled := FRemoveKey <> FFocusKey;
    NewPos := PointToPosition(X, Y);
    if NewPos <> FFocusKey.Position then begin
      FFocusKey.Position := NewPos;
      Invalidate;
      if Assigned(OnScroll) then begin
        OnScroll(Self, pscTrack, NewPos);
        FFocusKey.Position := NewPos;
      end;
      if Assigned(OnChange) then OnChange(Self);
    end;
  end;
end;

procedure TPegtopCustomGradientBar.DoEndScroll;
var
  P: Integer;
begin
  if Enabled and FBarEnabled and (FFocusKey <> NIL) then begin
    if FRemoveKey <> NIL then begin
      FFocusKey := NIL;
      RemoveKey(FRemoveKey);
      FRemoveKey := NIL;
      if Assigned(FOnFocusKeyChange) then FOnFocusKeyChange(Self);
    end
    else begin
      if Assigned(OnScroll) then begin
        P := FFocusKey.Position;
        OnScroll(Self, pscEndScroll, P);
        FFocusKey.Position := P;
      end;
    end;
    Invalidate;
  end;
end;

procedure TPegtopCustomGradientBar.ExtraMouseMove(Shift: TShiftState; X, Y: Integer);
var
  Key: TPegtopGradientKey;
begin
  if Enabled and FBarEnabled then begin
    Key := GetKeyAtPosition(X, Y);
    if FHoverKey <> Key then begin
      FHoverKey := Key;
      Invalidate;
    end;
    inherited;
  end;
end;

procedure TPegtopCustomGradientBar.ApplyPosition(const P: Integer);
var
  NewPos: Integer;
begin
  if FFocusKey <> NIL then begin
    NewPos := P;
    if NewPos < Min then NewPos := Min
    else if NewPos > Max then NewPos := Max;
    if FFocusKey.Position <> NewPos then begin
      FFocusKey.Position := NewPos;
      Invalidate;
      if Assigned(OnScroll) then begin
        OnScroll(Self, pscPosition, NewPos);
        FFocusKey.Position := NewPos;
      end;
      if Assigned(OnChange) then OnChange(Self);
    end;
  end;
end;

procedure TPegtopCustomGradientBar.ChangePosition(const Delta: Integer);
var
  NewPos: Integer;
begin
  if FFocusKey <> NIL then begin
    NewPos := FFocusKey.Position + Delta;
    if NewPos < Min then NewPos := Min
    else if NewPos > Max then NewPos := Max;
    if FFocusKey.Position <> NewPos then begin
      FFocusKey.Position := NewPos;
      Invalidate;
      if Assigned(OnScroll) then begin
        if Delta < 0 then
          OnScroll(Self, pscLineUp, NewPos)
        else
          OnScroll(Self, pscLineDown, NewPos);
        FFocusKey.Position := NewPos;
      end;
      if Assigned(OnChange) then OnChange(Self);
    end;
  end;
end;

procedure TPegtopCustomGradientBar.GradientChange(Sender: TObject);
begin
  if Sender = FGradient then begin
    if (FRemoveKey <> NIL) and (GetKeyIndex(FRemoveKey) < 0) then begin
      FRemoveKey := NIL;
    end;
    if (FHoverKey <> NIL) and (GetKeyIndex(FHoverKey) < 0) then begin
      // FHoverKey was removed from gradient
      FHoverKey := NIL;
    end;
    if (FFocusKey <> NIL) and (GetKeyIndex(FFocusKey) < 0) then begin
      // FFocusKey was removed from gradient
      FFocusKey := NIL;
      ResetFocus;
    end;
    Invalidate;
    if Assigned(FOnGradientChange) then FOnGradientChange(Self);
  end;
end;

function TPegtopCustomGradientBar.GetGradientRect: TRect;
var
  ButtonSize: TSize;
begin
  ButtonSize := GetButtonSize;
  if Orientation = psoHorizontal then begin
    Result.Left := ButtonSize.CX div 2;
    Result.Top := 1;
    Result.Right := ClientWidth - ButtonSize.CX div 2;
    Result.Bottom := (GetLinePoint.Y - ButtonSize.CY div 2 - 2) - 1;
  end
  else begin
    Result.Left := 1;
    Result.Top := ButtonSize.CY div 2;
    Result.Right := (GetLinePoint.X - ButtonSize.CY div 2 - 2) - 1;
    Result.Bottom := ClientHeight - ButtonSize.CY div 2;
  end;
end;

function TPegtopCustomGradientBar.GetLinePoint: TPoint;
var
  ButtonSize: TSize;
begin
  ButtonSize := GetButtonSize;
  if Orientation = psoHorizontal then begin
    Result.X := ClientWidth div 2;
    Result.Y := ClientHeight - (ButtonSize.CY + 1) div 2;
  end
  else begin
    Result.X := ClientWidth - (ButtonSize.CX + 1) div 2;
    Result.Y := ClientHeight div 2;
  end;
end;

function TPegtopCustomGradientBar.GetButtonSize: TSize;
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

function TPegtopCustomGradientBar.GetButtonPoint(Key: TPegtopGradientKey): TPoint;
var
  ButtonSize: TSize;
  LinePoint: TPoint;
begin
  ButtonSize := GetButtonSize;
  LinePoint := GetLinePoint;
  if Orientation = psoHorizontal then begin
    if Max = Min then begin
      Result.X := 0;
    end
    else begin
      Result.X := MulDiv(Key.Position - Min, ClientWidth - ButtonSize.CX, Max - Min);
    end;
    Result.Y := LinePoint.Y - ButtonSize.CY div 2;
  end
  else begin
    if Max = Min then begin
      Result.Y := 0;
    end
    else begin
      Result.Y := MulDiv(Max - Key.Position, ClientHeight - ButtonSize.CY, Max - Min);
    end;
    Result.X := LinePoint.X - ButtonSize.CX div 2;
  end;
end;

function TPegtopCustomGradientBar.GetKeyAtPosition(X, Y: Integer): TPegtopGradientKey;
var
  ButtonSize: TSize;
  ButtonPoint: TPoint;
  I: Integer;
  Key: TPegtopGradientKey;
begin
  Result := NIL;
  ButtonSize := GetButtonSize;
  if (FFocusKey <> NIL) then begin
    ButtonPoint := GetButtonPoint(FFocusKey);
    if (X >= ButtonPoint.X) and (Y >= ButtonPoint.Y)
    and (X < ButtonPoint.X + ButtonSize.CX) and (Y < ButtonPoint.Y + Buttonsize.CY) then
      Result := FFocusKey;
  end;
  if Result = NIL then begin
    I := GetCount;
    while (Result = NIL) and (I > 0) do begin
      Dec(I);
      Key := Keys[I];
      ButtonPoint := GetButtonPoint(Key);
      if (Key.Enabled) and (X >= ButtonPoint.X) and (Y >= ButtonPoint.Y)
      and (X < ButtonPoint.X + ButtonSize.CX) and (Y < ButtonPoint.Y + Buttonsize.CY) then
        Result := Key;
    end;
  end;
end;

function TPegtopCustomGradientBar.PointToPosition(const X, Y: Integer): Integer;
var
  ButtonSize: TSize;
begin
  if Max = Min then begin
    Result := Min;
  end
  else begin
    ButtonSize := GetButtonSize;
    if Orientation = psoHorizontal then
      Result := ((X * (Max - Min) div SmallChange + ((ClientWidth - ButtonSize.CX) div 2)) div (ClientWidth - ButtonSize.CX)) * SmallChange + Min
    else
      Result := Max - ((Y * (Max - Min) div SmallChange + ((ClientHeight - ButtonSize.CY) div 2)) div (ClientHeight - ButtonSize.CY)) * SmallChange;
    if Result < Min then Result := Min else if Result > Max then Result := Max;
  end;
end;

procedure TPegtopCustomGradientBar.SetGradient(Value: TPegtopCustomColorGradient);
begin
  if FRemoveKey <> NIL then FRemoveKey.Enabled := True;
  FGradient.Assign(Value);
  FFocusKey := NIL;
  FHoverKey := NIL;
  FRemoveKey := NIL;
  ResetFocus;
  Invalidate;
end;

procedure TPegtopCustomGradientBar.SetGradientHook(Value: TPegtopCustomColorGradient);
begin
  if Value = NIL then Value := FInternalGradient;
  if FGradient <> Value then begin
    if FRemoveKey <> NIL then FRemoveKey.Enabled := True;
    if (FGradient <> FInternalGradient) then FGradient.RemoveListener(GradientChange);
    FGradient := Value;
    if (FGradient <> FInternalGradient) then FGradient.AddListener(GradientChange);
    FFocusKey := NIL;
    FHoverKey := NIL;
    FRemoveKey := NIL;
    ResetFocus;
    Invalidate;
  end;
end;

procedure TPegtopCustomGradientBar.SetLook(Value: TPegtopColorControlLook);
begin
  if FLook <> Value then begin
    FLook := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomGradientBar.SetBarEnabled(Value: Boolean);
begin
  if FBarEnabled <> Value then begin
    FBarEnabled := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomGradientBar.SetMinCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FMinCount := Value;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientBar
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientBar.Create(AOwner: TComponent);
begin
  inherited;
  FRemovedColor := $FFFFFF;
end;

procedure TPegtopColorGradientBar.PaintTo(const ACanvas: TCanvas);
var
  ButtonSize: TSize;
  procedure BlendPixel(const X, Y: Integer; const Color: TColor; const Alpha: Integer);
  var
    C: TColor;
    R, G, B: Integer;
  begin
    C := ACanvas.Pixels[X, Y];
    R := (C and $FF) + ((Color and $FF) - (C and $FF)) * Alpha div 256;
    G := (C shr 8 and $FF) + ((Color shr 8 and $FF) - (C shr 8 and $FF)) * Alpha div 256;
    B := (C shr 16 and $FF) + ((Color shr 16 and $FF) - (C shr 16 and $FF)) * Alpha div 256;
    ACanvas.Pixels[X, Y] := R or (G shl 8) or (B shl 16);
  end;
  procedure DrawColorButton(Key: TPegtopGradientKey);
  var
    ButtonPoint: TPoint;
    ButtonRect: TRect;
    State: TPegtopThemeState;
    C: TColor;
  begin
    if Key.Enabled then begin
      if (not Enabled) or (not BarEnabled) then
        State := ptsDisabled
      else if (FFocusKey = Key) and IsMouseDown then
        State := ptsPushed
      else if FHoverKey = Key then
        State := ptsHot
      else if (FFocusKey = Key) and Focused then
        State := ptsFocused
      else
        State := ptsNormal;
      ButtonPoint := GetButtonPoint(Key);
      ButtonRect := Bounds(ButtonPoint.X, ButtonPoint.Y, ButtonSize.CX, ButtonSize.CY);
      // draw arrow:
      if Enabled and BarEnabled then begin
        if (FFocusKey = Key) and Focused then
          ACanvas.Brush.Color := clBlack
        else if FFocusKey = Key then
          ACanvas.Brush.Color := MixColors(TPegtopColor(ColorToRGB(clBlack)), TPegtopColor(ColorToRGB(clBtnFace)), 128).Def
        else
          ACanvas.Brush.Color := clBtnFace;
        ACanvas.Pen.Color := clBlack;
        if Orientation = psoHorizontal then
          ACanvas.Polygon([Point(ButtonPoint.X + 2, ButtonPoint.Y - 1),
            Point(ButtonPoint.X + ButtonSize.CX div 2, ButtonPoint.Y - ButtonSize.CX div 2 + 1),
            Point(ButtonPoint.X + ButtonSize.CX - 3, ButtonPoint.Y - 1)])
        else
          ACanvas.Polygon([Point(ButtonPoint.X - 1, ButtonPoint.Y + 2),
            Point(ButtonPoint.X - ButtonSize.CY div 2 + 1, ButtonPoint.Y + ButtonSize.CY div 2),
            Point(ButtonPoint.X - 1, ButtonPoint.Y + ButtonSize.CY - 3)]);
      end;
      // draw button:
      DrawButton(ACanvas, ButtonRect, State, (FFocusKey = Key) and Focused and Enabled and FBarEnabled);
      // draw color:
      if Enabled and BarEnabled then begin
        C := TPegtopColorKey(Key).Color;
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := C;
        ACanvas.FillRect(Rect(ButtonRect.Left + 6, ButtonRect.Top + 5, ButtonRect.Right - 6, ButtonRect.Top + 6));
        ACanvas.FillRect(Rect(ButtonRect.Left + 5, ButtonRect.Top + 6, ButtonRect.Right - 5, ButtonRect.Bottom - 6));
        ACanvas.FillRect(Rect(ButtonRect.Left + 6, ButtonRect.Bottom - 6, ButtonRect.Right - 6, ButtonRect.Bottom - 5));
        BlendPixel(ButtonRect.Left + 5, ButtonRect.Top + 5, C, 128);
        BlendPixel(ButtonRect.Right - 6, ButtonRect.Top + 5, C, 128);
        BlendPixel(ButtonRect.Left + 5, ButtonRect.Bottom - 6, C, 128);
        BlendPixel(ButtonRect.Right - 6, ButtonRect.Bottom - 6, C, 128);
      end;
    end;
  end;
var
  GradientRect: TRect;
  TempBitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
  I: Integer;
  Key: TPegtopGradientKey;
begin
  inherited;
  GradientRect := GetGradientRect;
  TempBitmap := TBitmap.Create;
  try
    TempBitmap.PixelFormat := pf32bit;
    TempBitmap.Width := GradientRect.Right - GradientRect.Left + 2;
    TempBitmap.Height := GradientRect.Bottom - GradientRect.Top + 2;
    Origin := TempBitmap.ScanLine[0];
    Pitch := Integer(TempBitmap.ScanLine[1]) - Integer(Origin);
    if Orientation = psoHorizontal then begin
      FGradient.DrawDithered32(Origin, Pitch, Rect(1, 1, TempBitmap.Width - 1, TempBitmap.Height - 1),
        Point(1, 1), Point(TempBitmap.Width - 1, 1));
    end
    else begin
      FGradient.DrawDithered32(Origin, Pitch, Rect(1, 1, TempBitmap.Width - 1, TempBitmap.Height - 1),
        Point(1, TempBitmap.Height - 1), Point(1, 1));
    end;
    PegtopDrawBounds32(Origin, Pitch, Rect(0, 0, TempBitmap.Width, TempBitmap.Height),
      Rect(0, 0, TempBitmap.Width, TempBitmap.Height), FLook, clBlack, clBtnFace);
    ACanvas.Draw(GradientRect.Left - 1, GradientRect.Top - 1, TempBitmap);
  finally
    TempBitmap.Free;
  end;
  ButtonSize := GetButtonSize;
  // draw button:
  for I := 0 to GetCount - 1 do begin
    Key := Keys[I];
    if (FFocusKey <> Key) or (not Focused) then
      DrawColorButton(Key);
  end;
  if (FFocusKey <> NIL) and Focused then
    DrawColorButton(FFocusKey);
end;

procedure TPegtopColorGradientBar.PopupContextMenu(const X, Y: Integer);
begin
  if ColorGradientPopupMenu = NIL then ColorGradientPopupMenu := TPegtopColorGradientPopupMenu.Create(Application);
  ColorGradientPopupMenu.PopupComponent := Self;
  ColorGradientPopupMenu.Popup(X, Y);
end;

procedure TPegtopColorGradientBar.WMEditKey(var Msg: TMessage);
var
  ColorDialog: TPegtopColorDialog;
begin
  EndScroll;
  if FFocusKey <> NIL then begin
    Update;
    ColorDialog := TPegtopColorDialog.Create(Self);
    try
      ColorDialog.Color := TPegtopColorKey(FFocusKey).Color;
      ColorDialog.Options := [pcoUserDefinedColors];
      ColorDialog.Look := Look;
      ColorDialog.OnPreview := DialogPreview;
      if ColorDialog.Execute then begin
        TPegtopColorKey(FFocusKey).Color := ColorDialog.Color;
        Invalidate;
        if Assigned(FOnFocusKeyModify) then FOnFocusKeyModify(Self);
      end;
    finally
      ColorDialog.Free;
    end;
  end;
end;

procedure TPegtopColorGradientBar.ValidatePosition;
var
  I: Integer;
begin
  for I := 0 to FGradient.Color.Keys.Count - 1 do begin
    if FGradient.Color.Keys[I].Position < Min then FGradient.Color.Keys[I].Position := Min
    else if FGradient.Color.Keys[I].Position > Max then FGradient.Color.Keys[I].Position := Max;
  end;
end;

function TPegtopColorGradientBar.AddKey: TPegtopGradientKey;
var
  Key: TPegtopColorKey;
begin
  Key := FGradient.Color.Keys.Add;
  if FFocusKey <> NIL then Key.Color := TPegtopColorKey(FFocusKey).Color
  else Key.Color := FRemovedColor;
  Result := Key;
end;

procedure TPegtopColorGradientBar.RemoveKey(Key: TPegtopGradientKey);
begin
  if Key <> NIL then begin
    FRemovedColor := TPegtopColorKey(Key).Color;
    FGradient.Color.Keys.Remove(TPegtopColorKey(Key));
  end;
end;

procedure TPegtopColorGradientBar.DialogPreview(Sender: TObject; Color: TColor);
begin
  if (FFocusKey <> NIL)
  and (TPegtopColorKey(FFocusKey).Color <> Color) then begin
    TPegtopColorKey(FFocusKey).Color := Color;
    Invalidate;
  end;
end;

function TPegtopColorGradientBar.GetCount: Integer;
begin
  Result := FGradient.Color.Keys.Count;
end;

function TPegtopColorGradientBar.GetKey(Index: Integer): TPegtopGradientKey;
begin
  Result := FGradient.Color.Keys[Index];
end;

function TPegtopColorGradientBar.GetKeyIndex(Key: TPegtopGradientKey): Integer;
begin
  Result := FGradient.Color.Keys.IndexOf(TPegtopColorKey(Key));
end;

function TPegtopColorGradientBar.GetFocusKey: TPegtopColorKey;
begin
  Result := TPegtopColorKey(FFocusKey);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopOpacityGradientBar
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopOpacityGradientBar.Create(AOwner: TComponent);
begin
  inherited;
  FRemovedOpacity := 256;
end;

procedure TPegtopOpacityGradientBar.PaintTo(const ACanvas: TCanvas);
var
  ButtonSize: TSize;
  procedure BlendPixel(const X, Y: Integer; const Color: TColor; const Alpha: Integer);
  var
    C: TColor;
    R, G, B: Integer;
  begin
    C := ACanvas.Pixels[X, Y];
    R := (C and $FF) + ((Color and $FF) - (C and $FF)) * Alpha div 256;
    G := (C shr 8 and $FF) + ((Color shr 8 and $FF) - (C shr 8 and $FF)) * Alpha div 256;
    B := (C shr 16 and $FF) + ((Color shr 16 and $FF) - (C shr 16 and $FF)) * Alpha div 256;
    ACanvas.Pixels[X, Y] := R or (G shl 8) or (B shl 16);
  end;
  procedure DrawColorButton(Key: TPegtopGradientKey);
  var
    ButtonPoint: TPoint;
    ButtonRect: TRect;
    State: TPegtopThemeState;
    C: TColor;
  begin
    if Key.Enabled then begin
      if (not Enabled) or (not BarEnabled) then
        State := ptsDisabled
      else if (FFocusKey = Key) and IsMouseDown then
        State := ptsPushed
      else if FHoverKey = Key then
        State := ptsHot
      else if (FFocusKey = Key) and Focused then
        State := ptsFocused
      else
        State := ptsNormal;
      ButtonPoint := GetButtonPoint(Key);
      ButtonRect := Bounds(ButtonPoint.X, ButtonPoint.Y, ButtonSize.CX, ButtonSize.CY);
      // draw arrow:
      if Enabled and BarEnabled then begin
        if (FFocusKey = Key) and Focused then
          ACanvas.Brush.Color := clBlack
        else if FFocusKey = Key then
          ACanvas.Brush.Color := MixColors(TPegtopColor(ColorToRGB(clBlack)), TPegtopColor(ColorToRGB(clBtnFace)), 128).Def
        else
          ACanvas.Brush.Color := clBtnFace;
        ACanvas.Pen.Color := clBlack;
        ACanvas.Polygon([Point(ButtonPoint.X + 2, ButtonPoint.Y - 1),
          Point(ButtonPoint.X + ButtonSize.CX div 2, ButtonPoint.Y - ButtonSize.CX div 2 + 1),
          Point(ButtonPoint.X + ButtonSize.CX - 3, ButtonPoint.Y - 1)]);
      end;
      // draw button:
      DrawButton(ACanvas, ButtonRect, State, (FFocusKey = Key) and Focused and Enabled and FBarEnabled);
      // draw color:
      if Enabled and BarEnabled then begin
        C := $FFFFFF - ((TPegtopOpacityKey(Key).Opacity * 255 div 256) * $10101);
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := C;
        ACanvas.FillRect(Rect(ButtonRect.Left + 6, ButtonRect.Top + 5, ButtonRect.Right - 6, ButtonRect.Top + 6));
        ACanvas.FillRect(Rect(ButtonRect.Left + 5, ButtonRect.Top + 6, ButtonRect.Right - 5, ButtonRect.Bottom - 6));
        ACanvas.FillRect(Rect(ButtonRect.Left + 6, ButtonRect.Bottom - 6, ButtonRect.Right - 6, ButtonRect.Bottom - 5));
        BlendPixel(ButtonRect.Left + 5, ButtonRect.Top + 5, C, 128);
        BlendPixel(ButtonRect.Right - 6, ButtonRect.Top + 5, C, 128);
        BlendPixel(ButtonRect.Left + 5, ButtonRect.Bottom - 6, C, 128);
        BlendPixel(ButtonRect.Right - 6, ButtonRect.Bottom - 6, C, 128);
      end;
    end;
  end;
var
  GradientRect: TRect;
  TempBitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
  I: Integer;
  Key: TPegtopGradientKey;
begin
  inherited;
  GradientRect := GetGradientRect;
  TempBitmap := TBitmap.Create;
  try
    TempBitmap.PixelFormat := pf32bit;
    TempBitmap.Width := GradientRect.Right - GradientRect.Left + 2;
    TempBitmap.Height := GradientRect.Bottom - GradientRect.Top + 2;
    Origin := TempBitmap.ScanLine[0];
    Pitch := Integer(TempBitmap.ScanLine[1]) - Integer(Origin);
    PegtopDrawSolidRect32(Origin, Pitch, Rect(1, 1, TempBitmap.Width - 1, TempBitmap.Height - 1),
      PegtopColor($FFFFFF), 0, NIL, 1, 1, Rect(1, 1, TempBitmap.Width - 1, TempBitmap.Height - 1));
    if Orientation = psoHorizontal then begin
      FGradient.BlendDithered32(Origin, Pitch, Rect(1, 1, TempBitmap.Width - 1, TempBitmap.Height - 1),
        Point(1, 1), Point(TempBitmap.Width - 2, 1));
    end
    else begin
      FGradient.BlendDithered32(Origin, Pitch, Rect(1, 1, TempBitmap.Width - 1, TempBitmap.Height - 1),
        Point(1, TempBitmap.Height - 2), Point(1, 1));
    end;
    PegtopDrawBounds32(Origin, Pitch, Rect(0, 0, TempBitmap.Width, TempBitmap.Height),
      Rect(0, 0, TempBitmap.Width, TempBitmap.Height), FLook, clBlack, clBtnFace);
    ACanvas.Draw(GradientRect.Left - 1, GradientRect.Top - 1, TempBitmap);
  finally
    TempBitmap.Free;
  end;
  ButtonSize := GetButtonSize;
  // draw button:
  for I := 0 to GetCount - 1 do begin
    Key := Keys[I];
    if (FFocusKey <> Key) or (not Focused) then
      DrawColorButton(Key);
  end;
  if (FFocusKey <> NIL) and Focused then
    DrawColorButton(FFocusKey);
end;

procedure TPegtopOpacityGradientBar.PopupContextMenu(const X, Y: Integer);
begin
  if OpacityGradientPopupMenu = NIL then OpacityGradientPopupMenu := TPegtopOpacityGradientPopupMenu.Create(Application);
  OpacityGradientPopupMenu.PopupComponent := Self;
  OpacityGradientPopupMenu.Popup(X, Y);
end;

procedure TPegtopOpacityGradientBar.WMEditKey(var Msg: TMessage);
begin
end;

procedure TPegtopOpacityGradientBar.ValidatePosition;
var
  I: Integer;
begin
  for I := 0 to FGradient.Opacity.Keys.Count - 1 do begin
    if FGradient.Opacity.Keys[I].Position < Min then FGradient.Opacity.Keys[I].Position := Min
    else if FGradient.Opacity.Keys[I].Position > Max then FGradient.Opacity.Keys[I].Position := Max;
  end;
end;

function TPegtopOpacityGradientBar.AddKey: TPegtopGradientKey;
var
  Key: TPegtopOpacityKey;
begin
  Key := FGradient.Opacity.Keys.Add;
  if FFocusKey <> NIL then Key.Opacity := TPegtopOpacityKey(FFocusKey).Opacity
  else Key.Opacity := FRemovedOpacity;
  Result := Key;
end;

procedure TPegtopOpacityGradientBar.RemoveKey(Key: TPegtopGradientKey);
begin
  if Key <> NIL then begin
    FRemovedOpacity := TPegtopOpacityKey(Key).Opacity;
    FGradient.Opacity.Keys.Remove(TPegtopOpacityKey(Key));
  end;
end;

function TPegtopOpacityGradientBar.GetCount: Integer;
begin
  Result := FGradient.Opacity.Keys.Count;
end;

function TPegtopOpacityGradientBar.GetKey(Index: Integer): TPegtopGradientKey;
begin
  Result := FGradient.Opacity.Keys[Index];
end;

function TPegtopOpacityGradientBar.GetKeyIndex(Key: TPegtopGradientKey): Integer;
begin
  Result := FGradient.Opacity.Keys.IndexOf(TPegtopOpacityKey(Key));
end;

function TPegtopOpacityGradientBar.GetFocusKey: TPegtopOpacityKey;
begin
  Result := TPegtopOpacityKey(FFocusKey);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopGradientPopupMenu
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopGradientPopupMenu.Create(AOwner: TComponent);
const
  MenuCaptions: array[0..12] of String = (
    '&Select key color', '&Delete key', '&Unify from key', '-',
    '&Copy', '&Paste', '-',
    '&Blur', 'Distribute keys &evenly', '&Flip keys', '-',
    '&Open...', 'Save &as...'
  );
  UnifyMenuCaptions: array[0..6] of String = (
    '&Hue', '&Saturation', 'Brig&htness', '-', '&Red', '&Green', '&Blue'
  );
  BlurMenuCaptions: array[0..3] of String = (
    '5%', '10%', '25%', '50%'
  );
  function CreateItem(Caption: TCaption; Tag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := Caption;
    Result.Tag := Tag;
    Result.Default := Tag = 0;
    if Result.Caption <> '-' then begin
      Result.OnClick := MenuItemClick;
      Result.OnDrawItem := MenuItemDrawItem;
      Result.OnMeasureItem := MenuItemMeasureItem;
    end;
  end;
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  inherited;
  OwnerDraw := True;
  for I := Low(MenuCaptions) to High(MenuCaptions) do begin
    MenuItem := CreateItem(MenuCaptions[I], I);
    Items.Add(MenuItem);
  end;
  for I := Low(UnifyMenuCaptions) to High(UnifyMenuCaptions) do begin
    MenuItem := CreateItem(UnifyMenuCaptions[I], 100 + I);
    Items[2].Add(MenuItem);
  end;
  for I := Low(BlurMenuCaptions) to High(BlurMenuCaptions) do begin
    MenuItem := CreateItem(BlurMenuCaptions[I], 200 + I);
    Items[7].Add(MenuItem);
  end;
end;

procedure TPegtopGradientPopupMenu.DoPopup(Sender: TObject);
var
  Component: TComponent;
  GradientBar: TPegtopCustomGradientBar;
begin
  Component := TPopupMenu(Sender).PopupComponent;
  if Component is TPegtopCustomGradientBar then begin
    GradientBar := TPegtopCustomGradientBar(Component);
    Items[0].Visible := (GradientBar is TPegtopColorGradientBar) and GradientBar.BarEnabled;
    Items[0].Enabled := GradientBar.FFocusKey <> NIL;
    Items[1].Visible := GradientBar.BarEnabled;
    Items[1].Enabled := (GradientBar.FFocusKey <> NIL) and (GradientBar.GetCount > GradientBar.FMinCount);
    Items[2].Visible := (GradientBar is TPegtopColorGradientBar) and GradientBar.BarEnabled;
    Items[2].Enabled := GradientBar.FFocusKey <> NIL;
    Items[5].Enabled := ColorClipboard.HasColorGradient;
    Items[7].Visible := GradientBar.BarEnabled;
    Items[8].Visible := GradientBar.BarEnabled;
    Items[9].Visible := GradientBar.BarEnabled;
  end;
  inherited;
end;

procedure TPegtopGradientPopupMenu.MenuItemClick(Sender: TObject);
var
  Component: TComponent;
begin
  // since this event handler is not assigned to other objects,
  // we know that Sender is a TMenuItem of a TPopupMenu
  Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
  if Component is TPegtopCustomGradientBar then begin
    DoCommand(TPegtopCustomGradientBar(Component), TComponent(Sender).Tag);
  end;
end;

function TPegtopGradientPopupMenu.ChangeGradient(GradientBar: TPegtopCustomGradientBar;
  Gradient: TPegtopCustomColorGradient; Command: Integer): Boolean;
begin
  Result := False;
end;

procedure TPegtopGradientPopupMenu.DoCommand(GradientBar: TPegtopCustomGradientBar; Command: Integer);
var
  Dialog: TPegtopColorGradientOpenDialog;
begin
  case Command of
    0: GradientBar.EditFocusKey;
    1: GradientBar.RemoveKey(GradientBar.FFocusKey);
    4: ColorClipboard.SetColorGradient(GradientBar.Gradient);
    11:
      begin
        Dialog := TPegtopColorGradientOpenDialog.Create(NIL);
        try
          Dialog.Options := Dialog.Options
          + [ofPathMustExist]
          - [ofAllowMultiSelect, ofOverwritePrompt];
          if Dialog.Execute then begin
            GradientBar.Gradient.LoadFromFile(Dialog.FileName, Dialog.ItemIndex);
          end;
        finally
          Dialog.Free;
        end;
      end;
    12:
      begin
        Dialog := TPegtopColorGradientSaveDialog.Create(NIL);
        try
          Dialog.Options := Dialog.Options
          + [ofOverwritePrompt, ofPathMustExist]
          - [ofAllowMultiSelect];
          if Dialog.Execute then begin
            GradientBar.Gradient.SaveToFile(Dialog.FileName);
          end;
        finally
          Dialog.Free;
        end;
      end;
  end;
  ChangeGradient(GradientBar, GradientBar.Gradient, Command);
end;

procedure TPegtopGradientPopupMenu.MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  CaptionRect: TRect;
begin
  if Sender is TMenuItem then begin
    CaptionRect := Rect(ARect.Left + ARect.Bottom - ARect.Top + 2, ARect.Top + 2, ARect.Right, ARect.Bottom);
    if TMenuItem(Sender).Default then ACanvas.Font.Style := [fsBold];
    ACanvas.FillRect(ARect);
    if not TMenuItem(Sender).Enabled then begin
      if not Selected then begin
        OffsetRect(CaptionRect, 1, 1);
        ACanvas.Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(TMenuItem(Sender).Caption), Length(TMenuItem(Sender).Caption),
          CaptionRect, DT_SINGLELINE or DT_TOP or DT_LEFT);
        OffsetRect(CaptionRect, -1, -1);
      end;
      if Selected and (ColorToRGB(clHighlight) = ColorToRGB(clBtnShadow)) then
        ACanvas.Font.Color := clBtnHighlight
      else
        ACanvas.Font.Color := clBtnShadow;
    end;
    DrawText(ACanvas.Handle, PChar(TMenuItem(Sender).Caption), Length(TMenuItem(Sender).Caption),
      CaptionRect, DT_SINGLELINE or DT_TOP or DT_LEFT);
  end;
end;

procedure TPegtopGradientPopupMenu.MenuItemMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var
  CaptionRect: TRect;
begin
  if Sender is TMenuItem then begin
    DrawText(ACanvas.Handle, PChar(TMenuItem(Sender).Caption), Length(TMenuItem(Sender).Caption),
      CaptionRect, DT_SINGLELINE or DT_TOP or DT_LEFT or DT_CALCRECT);
    Width := Captionrect.Right - CaptionRect.Left + 8 + Height;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientPopupMenu
////////////////////////////////////////////////////////////////////////////////

function TPegtopColorGradientPopupMenu.ChangeGradient(GradientBar: TPegtopCustomGradientBar;
  Gradient: TPegtopCustomColorGradient; Command: Integer): Boolean;
const
  HSBColorComponents: array[0..2] of TPegtopColorComponent = (pccHue, pccSaturation, pccBrightness);
  RGBColorComponents: array[0..2] of TPegtopColorComponent = (pccRed, pccGreen, pccBlue);
  BlurStrength: array[0..3] of Integer = (13, 26, 64, 128);
var
  TempGradient: TPegtopCustomColorGradient;
begin
  Result := False;
  case Command of
    5:        if ColorClipboard.HasColorGradient then begin
                TempGradient := TPegtopColorGradient.Create([]);
                try
                  if ColorClipboard.GetColorGradient(TempGradient) then begin
                    Gradient.Color.Assign(TempGradient.Color);
                    Result := True;
                  end;
                finally
                  TempGradient.Free;
                end;
              end;
    8:        begin
                Gradient.Color.Keys.DistributeEvenly;
                Result := True;
              end;
    9:        begin
                Gradient.Color.Keys.Flip;
                Result := True;
              end;
    100..102: begin
                Gradient.Color.Keys.UnifyColorComponents(TPegtopColorKey(GradientBar.FFocusKey).Color, HSBColorComponents[Command - 100]);
                Result := True;
              end;
    104..106: begin
                Gradient.Color.Keys.UnifyColorComponents(TPegtopColorKey(GradientBar.FFocusKey).Color, RGBColorComponents[Command - 104]);
                Result := True;
              end;
    200..203: begin
                Gradient.Color.Keys.Blur(BlurStrength[Command - 200]);
                Result := True;
              end;
    else      Result := inherited ChangeGradient(GradientBar, Gradient, Command);
  end;
end;

procedure TPegtopColorGradientPopupMenu.MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  Component: TComponent;
  PreviewGradient: TPegtopCustomColorGradient;
begin
  inherited;
  if Sender is TMenuItem then begin
    Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
    if Component is TPegtopCustomGradientBar then begin
      PreviewGradient := TPegtopColorGradient.Create([]);
      try
        PreviewGradient.Assign(TPegtopCustomGradientBar(Component).Gradient);
        if ChangeGradient(TPegtopCustomGradientBar(Component), PreviewGradient, TComponent(Sender).Tag) then begin
          PreviewGradient.Seamless := TPegtopCustomGradientBar(Component).Gradient.Seamless;
          PreviewGradient.Draw(ACanvas,
            Rect(ARect.Left + 3, ARect.Top + 3, ARect.Left + ARect.Bottom - ARect.Top - 3, ARect.Bottom - 3),
            Point(ARect.Left + 3, ARect.Top + 3), Point(ARect.Left + ARect.Bottom - ARect.Top - 3, ARect.Bottom - 3));
          ACanvas.Brush.Style := bsClear;
          ACanvas.Pen.Color := clBlack;
          ACanvas.Rectangle(ARect.Left + 2, ARect.Top + 2, ARect.Left + ARect.Bottom - ARect.Top - 2, ARect.Bottom - 2);
        end;
      finally
        PreviewGradient.Free;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopOpacityGradientPopupMenu
////////////////////////////////////////////////////////////////////////////////

function TPegtopOpacityGradientPopupMenu.ChangeGradient(GradientBar: TPegtopCustomGradientBar;
  Gradient: TPegtopCustomColorGradient; Command: Integer): Boolean;
const
  BlurStrength: array[0..3] of Integer = (13, 26, 64, 128);
var
  TempGradient: TPegtopCustomColorGradient;
begin
  Result := False;
  case Command of
    5:        if ColorClipboard.HasColorGradient then begin
                TempGradient := TPegtopColorGradient.Create([]);
                try
                  if ColorClipboard.GetColorGradient(TempGradient) then begin
                    Gradient.Opacity.Assign(TempGradient.Opacity);
                    Result := True;
                  end;
                finally
                  TempGradient.Free;
                end;
              end;
    8:        begin
                Gradient.Opacity.Keys.DistributeEvenly;
                Result := True;
              end;
    9:        begin
                Gradient.Opacity.Keys.Flip;
                Result := True;
              end;
    200..203: begin
                Gradient.Opacity.Keys.Blur(BlurStrength[Command - 200]);
                Result := True;
              end;
    else      Result := inherited ChangeGradient(GradientBar, Gradient, Command);
  end;
end;

procedure TPegtopOpacityGradientPopupMenu.MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  Component: TComponent;
  PreviewGradient: TPegtopCustomColorGradient;
begin
  inherited;
  if Sender is TMenuItem then begin
    Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
    if Component is TPegtopCustomGradientBar then begin
      PreviewGradient := TPegtopColorGradient.Create([]);
      try
        PreviewGradient.Assign(TPegtopCustomGradientBar(Component).Gradient);
        if ChangeGradient(TPegtopCustomGradientBar(Component), PreviewGradient, TComponent(Sender).Tag) then begin
          PreviewGradient.Seamless := TPegtopCustomGradientBar(Component).Gradient.Seamless;
          PreviewGradient.GrayscaleOpacity(clWhite, clBlack, ACanvas,
            Rect(ARect.Left + 3, ARect.Top + 3, ARect.Left + ARect.Bottom - ARect.Top - 3, ARect.Bottom - 3),
            Point(ARect.Left + 3, ARect.Top + 3), Point(ARect.Left + ARect.Bottom - ARect.Top - 3, ARect.Bottom - 3));
          ACanvas.Brush.Style := bsClear;
          ACanvas.Pen.Color := clBlack;
          ACanvas.Rectangle(ARect.Left + 2, ARect.Top + 2, ARect.Left + ARect.Bottom - ARect.Top - 2, ARect.Bottom - 2);
        end;
      finally
        PreviewGradient.Free;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// initialization
////////////////////////////////////////////////////////////////////////////////

initialization
  ColorGradientPopupMenu := NIL;
  OpacityGradientPopupMenu := NIL;
end.
