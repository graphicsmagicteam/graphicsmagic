////////////////////////////////////////////////////////////////////////////////
// File:       PegtopFireButtons.pas
// Classes:    TPegtopFireButton
// Version:    1.00
// Date:       04 Sep 2003
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopFireButton is a button which fires events periodically when hold down.
// Delay and repeat interval can be adjusted. If ApproximateKeyboard is set to
// True, the button tries to fire with the same rate than your keyboard does
// (deending on the hardware the rate might differ a bit). TPegtopFireButton
// works with mouse and keyboard (hold down the left mouse button or space key).
////////////////////////////////////////////////////////////////////////////////
// Usage:
// OnClick works like normal. Assign an event handler to OnFire to catch the
// periodical events.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopFireButtons;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls;

type
  TPegtopFireState = (pfsSleep, pfsDelay, pfsRepeat);

  TPegtopFireButton = class(TButton)
  private
    FFireState: TPegtopFireState;
    FMouseHold: Boolean;
    FDelayInterval: Cardinal;
    FRepeatInterval: Cardinal;
    FApproximateKeyboard: Boolean;
    FOnFire: TNotifyEvent;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    function GetKeyboardDelayInterval: Cardinal;
    function GetKeyboardRepeatInterval: Cardinal;
    function GetDelayInterval: Cardinal;
    function GetRepeatInterval: Cardinal;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StartFire;
    procedure StopFire;
  published
    property DelayInterval: Cardinal read GetDelayInterval write FDelayInterval default 500;
    property RepeatInterval: Cardinal read GetRepeatInterval write FRepeatInterval default 30;
    property ApproximateKeyboard: Boolean read FApproximateKeyboard write FApproximateKeyboard default False;
    property OnFire: TNotifyEvent read FOnFire write FOnFire;
  end;

implementation

const
  FireEvent = $FE; // :-)

constructor TPegtopFireButton.Create(AOwner: TComponent);
begin
  FDelayInterval := 500;
  FRepeatInterval := 30;
  inherited;
end;

procedure TPegtopFireButton.StartFire;
begin
  if FFireState = pfsSleep then begin
    FFireState := pfsDelay;
    if Assigned(FOnFire) then FOnFire(Self);
    SetTimer(Handle, FireEvent, GetDelayInterval, NIL);
  end;
end;

procedure TPegtopFireButton.StopFire;
begin
  FFireState := pfsSleep;
  KillTimer(Handle, FireEvent);
end;

procedure TPegtopFireButton.WMTimer(var Msg: TWMTimer);
begin
  if Msg.TimerID = FireEvent then begin
    if FFireState = pfsDelay then begin
      KillTimer(Handle, FireEvent);
      SetTimer(Handle, FireEvent, GetRepeatInterval, NIL);
      FFireState := pfsRepeat;
    end;
    if Assigned(FOnFire) then FOnFire(Self);
  end;
  inherited;
end;

procedure TPegtopFireButton.CMMouseEnter(var Msg: TMessage);
begin
  if FMouseHold then StartFire;
  inherited;
end;

procedure TPegtopFireButton.CMMouseLeave(var Msg: TMessage);
begin
  if FMouseHold then StopFire;
  inherited;
end;

procedure TPegtopFireButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    FMouseHold := True;
    StartFire;
  end;
  inherited;
end;

procedure TPegtopFireButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseHold := False;
  StopFire;
  inherited;
end;

procedure TPegtopFireButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then StartFire;
  inherited;
end;

procedure TPegtopFireButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  StopFire;
  inherited;
end;

function TPegtopFireButton.GetKeyboardDelayInterval: Cardinal;
var
  Value: Integer;
begin
  if SystemParametersInfo(SPI_GETKEYBOARDDELAY, 0, @Value, 0) then
    Result := (Value + 1) * 250
  else
    Result := 500;
end;

function TPegtopFireButton.GetKeyboardRepeatInterval: Cardinal;
var
  Value: Integer;
begin
  if SystemParametersInfo(SPI_GETKEYBOARDSPEED, 0, @Value, 0) then
    Result := 1000 div (Value + 2)
  else
    Result := 30;
end;

function TPegtopFireButton.GetDelayInterval: Cardinal;
begin
  if FApproximateKeyboard then Result := GetKeyboardDelayInterval
  else Result := FDelayInterval;
end;

function TPegtopFireButton.GetRepeatInterval: Cardinal;
begin
  if FApproximateKeyboard then Result := GetKeyboardRepeatInterval
  else Result := FRepeatInterval;
end;

end.
