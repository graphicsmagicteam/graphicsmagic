////////////////////////////////////////////////////////////////////////////////
// Components: TPegtopCustomMessageReceiver
//             TPegtopMessageReceiver
// Version:    1.00
// Date:       23 Sep 2003
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopMessageReceiver is a component that catches windows messages without
// being a TWinControl by allocating a windows handle. Other controls can
// be derived from TPegtopCustomMessageReceiver and override HandleMessage.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopMessageReceivers;

interface

uses
  Windows, Messages, Classes;

type
  TPegtopMessageEvent = procedure(Sender: TObject; var Msg: TMessage; var Handled: Boolean) of object;
  TPegtopThreadEvent = procedure(Sender: TObject; ThreadID: Longint) of object;

  TPegtopCustomMessageReceiver = class(TComponent)
  private
    FWindowHandle: THandle;
    FOwnsHandle: Boolean;
    FActive: Boolean;
    procedure WndProc(var Msg: TMessage);
  protected
    procedure AllocateHandle;
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); virtual; abstract;
    property Handle: THandle read FWindowHandle;
    property Active: Boolean read FActive write FActive;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Perform(Msg: TMessage); overload;
    function Perform(Msg: Cardinal; WParam, LParam: Longint): Longint; overload;
  end;

  TPegtopMessageReceiver = class(TPegtopCustomMessageReceiver)
  private
    FOnMessage: TPegtopMessageEvent;
    FOnActivateApp: TPegtopThreadEvent;
    FOnDeactivateApp: TPegtopThreadEvent;
  protected
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Active;
    property OnMessage: TPegtopMessageEvent read FOnMessage write FOnMessage;
    property OnActivateApp: TPegtopThreadEvent read FOnActivateApp write FOnActivateApp;
    property OnDeactivateApp: TPegtopThreadEvent read FOnDeactivateApp write FOnDeactivateApp;
  end;

implementation

uses
  Forms;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomMessageReceiver
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomMessageReceiver.Create(AOwner: TComponent);
begin
  FOwnsHandle := False;
  FWindowHandle := 0;
  FActive := True;
  inherited Create(AOwner);
  AllocateHandle;
end;

destructor TPegtopCustomMessageReceiver.Destroy;
begin
  if FOwnsHandle then DeAllocateHWnd(FWindowHandle);
  inherited;
end;

procedure TPegtopCustomMessageReceiver.AllocateHandle;
begin
  if not FOwnsHandle then begin
    FWindowHandle := AllocateHWnd(WndProc);
    FOwnsHandle := True;
  end;
end;

procedure TPegtopCustomMessageReceiver.WndProc(var Msg: TMessage);
var
  Handled: Boolean;
begin
  if FActive then begin
    Handled := False;
    HandleMessage(Msg, Handled);
    if not Handled then
      Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
  end
  else begin
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
  end;
end;

procedure TPegtopCustomMessageReceiver.Perform(Msg: TMessage);
begin
  WndProc(Msg);
end;

function TPegtopCustomMessageReceiver.Perform(Msg: Cardinal; WParam, LParam: Longint): Longint;
var
  MsgRec: TMessage;
begin
  MsgRec.Msg := Msg;
  MsgRec.WParam := WParam;
  MsgRec.LParam := LParam;
  WndProc(MsgRec);
  Result := MsgRec.Result;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopMessageReceiver
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopMessageReceiver.Create(AOwner: TComponent);
begin
  FOnMessage := NIL;
  FOnActivateApp := NIL;
  FOnDeactivateApp := NIL;
  inherited;
end;

procedure TPegtopMessageReceiver.HandleMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if Msg.Msg = WM_ACTIVATEAPP then begin
    if Msg.WParam = 0 then begin
      // application is deactivated
      if Assigned(FOnDeactivateApp) then FOnDeactivateApp(Self, Msg.LParam);
    end
    else begin
      // application is activated
      if Assigned(FOnActivateApp) then FOnActivateApp(Self, Msg.LParam);
    end;
  end
  else begin
    if Assigned(FOnMessage) then FOnMessage(Self, Msg, Handled);
  end;
end;

end.
